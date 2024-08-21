{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Maid (run, MaidError) where

import Maid.Parser (Task (..), parseTasks)

import Control.Applicative (liftA2)
import Control.Concurrent (forkIO, killThread, newEmptyMVar, newMVar, putMVar, swapMVar, takeMVar, threadDelay, tryTakeMVar)
import Control.Exception (Exception, handle, throw)
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get), StateT, execStateT, modify)
import Data.Bool (bool)
import Data.List (dropWhileEnd, find)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as I
import System.Console.ANSI (hNowSupportsANSI)
import System.Console.GetOpt (ArgDescr (NoArg, ReqArg), ArgOrder (RequireOrder), OptDescr (Option), getOpt, usageInfo)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import System.FSNotify (watchTree, withManager)
import System.FilePath (combine, isDrive, takeDirectory, takeFileName)
import System.IO (hClose, hPutStrLn, stderr, stdout)
import System.IO.Error (catchIOError)
import System.IO.Temp (withSystemTempFile)
import System.Process (proc, waitForProcess, withCreateProcess)

run :: IO ()
run = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (opts, args, []) ->
      handle (bail . show :: MaidError -> IO a) $
        context
          >>= execStateT (mapM_ parseOpt opts)
          >>= setTaskfile
          >>= runReaderT (unMaid $ runArgs args)
    (_, _, err) -> bail $ dropWhileEnd (== '\n') $ concat err
  where
    options =
      [ Option ['h'] ["help"] (NoArg Help) "Display this message"
      , Option ['l'] ["list"] (NoArg List) "List tasks concisely"
      , Option ['n'] ["dry-run"] (NoArg DryRun) "Don't run anything, only display commands"
      , Option ['q'] ["quiet"] (NoArg Quiet) "Don't display anything"
      , Option ['f'] ["taskfile"] (ReqArg Maidfile "FILE") "Use tasks in FILE"
      , Option ['w'] ["watch"] (ReqArg Watch "PATH") "Watch files in PATH. Can be specified multiple times"
      , Option ['r'] ["restart"] (NoArg Restart) "When files change, restart the task instead of waiting"
      ]

    parseOpt :: Flag -> StateT Context IO ()
    parseOpt Help = liftIO $ do
      s <- defaultStyle
      exe <- getProgName

      putStrLn (primary s ++ "Usage: " ++ secondary s ++ exe ++ " [options] [task]\n")
      putStrLn $ usageInfo (primary s ++ "Options:" ++ tertiary s) options
      exitSuccess
    parseOpt List =
      get >>= \ctx -> liftIO $ do
        tasks <- snd . ctxTaskfile <$> setTaskfile ctx
        forM_ tasks $ \task -> do
          I.putStrLn (tName task <> " " <> tDesc task)
        exitSuccess
    parseOpt DryRun = modify $ \c -> c{ctxDryRun = True}
    parseOpt Quiet = modify $ \c -> c{ctxQuiet = True}
    parseOpt (Maidfile file) = do
      tasks <- liftIO $ parseTasks <$> I.readFile file
      when (null tasks) $ throw $ NoTasks file

      modify $ \c -> c{ctxTaskfile = (file, tasks)}
    parseOpt (Watch dir) = modify $ \c -> c{ctxWatch = dir : ctxWatch c}
    parseOpt Restart = modify $ \c -> c{ctxWatchRestart = True}

    context =
      (`fmap` defaultStyle) $ \style ->
        Context
          { ctxTaskfile = ([], [])
          , ctxDryRun = False
          , ctxQuiet = False
          , ctxWatch = []
          , ctxWatchRestart = False
          , ctxStyle = style
          }

    setTaskfile ctx = case ctxTaskfile ctx of
      ([], []) -> do
        tasks <- defaultTaskfiles
        return ctx{ctxTaskfile = tasks}
      _ -> return ctx

    bail e = putErr e >> exitFailure

    runArgs (task : args) = execTask (T.pack task) args
    runArgs [] = listTasks

data Flag
  = Help
  | List
  | DryRun
  | Quiet
  | Maidfile String
  | Watch String
  | Restart

putErr :: String -> IO ()
putErr msg = do
  s <- defaultStyle
  hPutStrLn stderr $ mconcat [err s, "error: ", secondary s, msg]

getTask :: Text -> [Task] -> Task
getTask name =
  fromMaybe (throw $ UnknownTask $ T.unpack name) . find ((. tName) (== name))

execTask :: Text -> [String] -> Maid ()
execTask name args = do
  task <- asks (getTask name . snd . ctxTaskfile)
  watch <- asks ctxWatch
  watchTask watch task args

watchTask :: [FilePath] -> Task -> [String] -> Maid ()
watchTask [] task args = do
  runTask task args
watchTask paths task args = do
  restart <- asks ctxWatchRestart
  exec <- asks $ runReaderT $ unMaid $ runTask task args
  liftIO $ withManager $ \mgr -> do
    action <- bool watchWait watchRestart restart exec
    forM_ paths $ \path ->
      watchTree mgr path (const True) (const action)
    forever (threadDelay maxBound)
  where
    watchWait exec = do
      void exec
      lock <- newMVar ()
      return $ void $ forkIO $ do
        blocked <- isNothing <$> tryTakeMVar lock
        unless blocked (exec >> putMVar lock ())
    watchRestart exec = do
      active <- forkIO exec >>= newMVar
      -- debounced child thread, makes infinite loops less overwhelming
      lock <- newEmptyMVar
      void $ forkIO $ forever $ do
        takeMVar lock
        killThread =<< swapMVar active =<< forkIO exec
        threadDelay (300 * 1000)
      return $ putMVar lock ()

runTask :: Task -> [String] -> Maid ()
runTask task args = do
  displayCommand ("maid " <> tName task)
  runLang (tLang task) args (tCode task)

runLang :: Text -> [String] -> Text -> Maid ()
runLang lang
  | lang `elem` ["sh", "bash"] = runShell
  | lang `elem` ["hs", "haskell"] = runHaskell
  | lang `elem` ["js", "javascript"] = runJavaScript
  | otherwise = throw $ UnknownLanguage $ T.unpack lang

runShell :: [String] -> Text -> Maid ()
runShell args =
  runProc "sh" "maidtask.sh" (("-euv" :) . (: "--" : args))

runHaskell :: [String] -> Text -> Maid ()
runHaskell args =
  runProc "runhaskell" "MaidTask.hs" (: "--" : "-W" : "--" : args)

runJavaScript :: [String] -> Text -> Maid ()
runJavaScript args =
  runProc "node" "maidtask.js" (: "--" : args)

runProc :: String -> String -> (String -> [String]) -> Text -> Maid ()
runProc cmd file args input = do
  dry <- asks ctxDryRun
  unless dry $ liftIO $ withSystemTempFile file $ \p h -> do
    I.hPutStr h input
    hClose h
    withCreateProcess (proc cmd $ args p) $ \_ _ _ ph -> do
      waitForProcess ph >>= handleExit
  where
    handleExit c
      | ExitFailure i <- c = do
          putErr $ "Task failed with exit code " <> show i
          exitWith c
      | otherwise = return ()

displayCommand :: Text -> Maid ()
displayCommand cmd = do
  style <- asks ctxStyle
  quiet <- asks ctxQuiet
  unless quiet $ liftIO $ do
    putStr $ secondary style
    I.putStr cmd
    putStrLn $ tertiary style

listTasks :: Maid ()
listTasks = do
  (file, tasks) <- asks ctxTaskfile
  style <- asks ctxStyle

  liftIO $ do
    putStrLn (primary style ++ "Tasks in " ++ takeFileName file)
    putStrLn ""
    forM_ tasks $ \task -> do
      putStr $ secondary style
      I.putStrLn $ "  " <> tName task
      putStr $ tertiary style
      I.putStrLn $ T.unlines $ map ("    " <>) $ T.lines $ tDesc task

defaultTaskfiles :: IO (String, [Task])
defaultTaskfiles = do
  files <- files <$> getCurrentDirectory
  tasks <- catMaybes <$> mapM maybeTaskfile files
  case tasks of
    t : _ -> return t
    _ -> throw NoTaskfile
  where
    files =
      concatMap ((`map` ["README.md", "CONTRIBUTING.md"]) . combine)
        . takeWhile (not . isDrive)
        . iterate takeDirectory
    maybeTaskfile :: FilePath -> IO (Maybe (String, [Task]))
    maybeTaskfile f =
      read `catchIOError` const (return Nothing)
      where
        read = maybeTask . parseTasks <$> I.readFile f
        maybeTask = flip bool Nothing . Just . (f,) <*> null

data Context = Context
  { ctxTaskfile :: (FilePath, [Task])
  , ctxDryRun :: Bool
  , ctxQuiet :: Bool
  , ctxWatch :: [FilePath]
  , ctxWatchRestart :: Bool
  , ctxStyle :: Style
  }

defaultStyle :: IO Style
defaultStyle = do
  bool empty def <$> (isNothing <$> lookupEnv "NO_COLOR") .&&. hNowSupportsANSI stdout
  where
    def = Style "95;1" "0;1" "" "31;1"
    empty = Style noColor noColor noColor noColor

primary :: Style -> String
primary = toEscape . sPrimary

secondary :: Style -> String
secondary = toEscape . sSecondary

tertiary :: Style -> String
tertiary = toEscape . sTertiary

err :: Style -> String
err = toEscape . sErr

data Style = Style
  { sPrimary :: Color
  , sSecondary :: Color
  , sTertiary :: Color
  , sErr :: Color
  }

toEscape :: Color -> String
toEscape (Color Nothing) = ""
toEscape (Color (Just a)) = "\x1b[" ++ a ++ "m"

noColor :: Color
noColor = Color Nothing

newtype Color = Color (Maybe String)

instance IsString Color where
  fromString = Color . Just

-- | Combine two predicates with and
(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)

data MaidError
  = NoTaskfile
  | NoTasks FilePath
  | UnknownTask String
  | UnknownLanguage String
  | GetOpt String

instance Show MaidError where
  show NoTaskfile = "No taskfile"
  show (NoTasks file) = "No tasks in " <> file
  show (UnknownTask task) = "No such task: " <> task
  show (UnknownLanguage lang) = "No such language: " <> lang
  show (GetOpt err) = err

instance Exception MaidError

newtype Maid a = Maid {unMaid :: ReaderT Context IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)
