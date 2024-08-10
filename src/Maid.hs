{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Maid (run) where

import Maid.Parser (Task (..), parseTasks)

import Control.Applicative (liftA2)
import Control.Exception (Exception (displayException, fromException), SomeException, handle)
import Control.Monad (forM_, unless, when)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (StateT, execStateT, gets, modify)
import Data.Bool (bool)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as I
import System.Console.ANSI (hNowSupportsANSI)
import System.Console.GetOpt (ArgDescr (NoArg), ArgOrder (RequireOrder), OptDescr (Option), getOpt, usageInfo)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode (ExitSuccess), exitFailure, exitSuccess)
import System.IO (hClose, hPutStrLn, stderr, stdout)
import System.IO.Error (catchIOError)
import System.IO.Temp (withSystemTempFile)
import System.Process.Typed (proc, runProcess_)

run :: IO ()
run = handle handleError $ do
  args <- getArgs
  case getOpt RequireOrder options args of
    (opts, args, []) -> do
      ctx <- context
      ctx <- execStateT (mapM_ parseOpt opts) ctx
      runReaderT (unMaid $ runArgs args) ctx
    (_, _, err) -> error $ concat err
  where
    options =
      [ Option ['h'] ["help"] (NoArg Help) "Display this message"
      , Option ['l'] ["list"] (NoArg List) "List tasks concisely"
      , Option ['n'] ["dry-run"] (NoArg DryRun) "Don't run anything, only display commands"
      , Option ['q'] ["quiet"] (NoArg Quiet) "Don't display anything"
      ]

    parseOpt :: Flag -> StateT Context IO ()
    parseOpt Help = liftIO $ do
      s <- defaultStyle
      exe <- getProgName

      putStrLn (primary s ++ "Usage: " ++ secondary s ++ exe ++ " [options] [task]\n")
      putStrLn $ usageInfo (primary s ++ "Options:" ++ tertiary s) options
      exitSuccess
    parseOpt List = do
      tasks <- snd <$> (gets ctxTaskfile >>= liftIO)
      liftIO $ do
        forM_ tasks $ \task -> do
          I.putStrLn (tName task <> " " <> tDesc task)
        exitSuccess
    parseOpt DryRun = modify $ \c -> c{ctxDryRun = True}
    parseOpt Quiet = modify $ \c -> c{ctxQuiet = True}

    context = do
      style <- defaultStyle
      taskfile <- cached defaultTaskfiles
      return $ Context taskfile style False False

    runArgs [] = listTasks
    runArgs (task : args) = runTask (T.pack task) args

    handleError e =
      unless (fromException e == Just ExitSuccess) $ do
        s <- defaultStyle
        hPutStrLn stderr (err s ++ "error: " ++ secondary s ++ displayException (e :: SomeException))
        exitFailure

data Flag = Help | List | DryRun | Quiet

newtype Maid a = Maid {unMaid :: ReaderT Context IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

getTask :: Text -> [Task] -> Task
getTask name =
  fromMaybe (error ("No such task: " ++ T.unpack name)) . find ((. tName) (== name))

runTask :: Text -> [String] -> Maid ()
runTask name args = do
  task <- getTask name . snd <$> (asks ctxTaskfile >>= liftIO)
  displayCommand ("maid " <> name)
  runLang (tLang task) (tCode task) args

runLang :: Text -> Text -> [String] -> Maid ()
runLang lang
  | lang `elem` ["sh", "bash"] = runShell
  | lang `elem` ["hs", "haskell"] = runHaskell
  | lang `elem` ["js", "javascript"] = runJavaScript
  | otherwise = error ("Unsupported language: " ++ T.unpack lang)

runShell :: Text -> [String] -> Maid ()
runShell input args = do
  dry <- asks ctxDryRun
  unless dry $ liftIO $ withSystemTempFile "maidtask.sh" $ \p h -> do
    I.hPutStr h input
    hClose h
    runProcess_ $ proc "sh" ("-euv" : p : "--" : args)

runHaskell :: Text -> [String] -> Maid ()
runHaskell input args = do
  dry <- asks ctxDryRun
  displayCommand "runhaskell Task.hs"
  unless dry $ liftIO $ withSystemTempFile "MaidTask.hs" $ \p h -> do
    I.hPutStr h input
    hClose h
    runProcess_ $ proc "runhaskell" ("--" : "--" : p : args)

runJavaScript :: Text -> [String] -> Maid ()
runJavaScript input args = do
  dry <- asks ctxDryRun
  displayCommand "node task.js"
  unless dry $ liftIO $ withSystemTempFile "maidtask.js" $ \p h -> do
    I.hPutStr h input
    hClose h
    runProcess_ $ proc "node" (p : "--" : args)

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
  (file, tasks) <- asks ctxTaskfile >>= liftIO
  style <- asks ctxStyle

  liftIO $ do
    when (null tasks) $ do
      hPutStrLn stderr "No tasks in current path"
      exitFailure

    putStrLn (primary style ++ "Tasks in " ++ file)
    putStrLn ""
    forM_ tasks $ \task -> do
      putStr $ secondary style
      I.putStrLn $ "  " <> tName task
      putStr $ tertiary style
      I.putStrLn $ T.unlines $ map ("    " <>) $ T.lines $ tDesc task

defaultTaskfiles :: IO (String, [Task])
defaultTaskfiles = do
  tasks <- catMaybes <$> mapM maybeTaskfile ["CONTRIBUTING.md", "README.md"]
  case tasks of
    t : _ -> return t
    _ -> error "No taskfile"
  where
    maybeTaskfile :: FilePath -> IO (Maybe (String, [Task]))
    maybeTaskfile f =
      read `catchIOError` const (return Nothing)
      where
        read = maybeTask . parseTasks <$> I.readFile f
        maybeTask = flip bool Nothing . Just . (f,) <*> null

cached :: IO a -> IO (IO a)
cached body = do
  ref <- newIORef Nothing
  return $ readIORef ref >>= maybe (cache ref) return
  where
    cache ref =
      body >>= \val ->
        writeIORef ref (Just val) >> return val

data Context = Context
  { ctxTaskfile :: IO (FilePath, [Task])
  , ctxStyle :: Style
  , ctxDryRun :: Bool
  , ctxQuiet :: Bool
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
