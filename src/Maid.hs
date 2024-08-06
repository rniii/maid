{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Maid (run) where

import Maid.Parser (Task (..), parseTasks)

import Control.Applicative (liftA2)
import Control.Exception (Exception (displayException, fromException), SomeException, handle)
import Control.Monad (forM_, unless, when)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (StateT, execStateT, modify)
import Data.Bool (bool)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (LazyByteString)
import Data.Char (isSpace)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as I
import System.Console.ANSI (hNowSupportsANSI)
import System.Console.GetOpt (ArgDescr (NoArg), ArgOrder (Permute), OptDescr (Option), getOpt, usageInfo)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode (ExitSuccess), exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdout)
import System.IO.Error (catchIOError)
import System.Process.Typed (byteStringInput, proc, runProcess_, setStdin)

run :: IO ()
run = handle handleError $ do
  args <- getArgs
  case getOpt Permute options args of
    (opts, args, []) -> do
      ctx <- context
      ctx <- execStateT (mapM_ parseOpt opts) ctx
      runReaderT (unMaid $ runArgs args) ctx
    (_, _, err) -> error $ concat err
  where
    options =
      [ Option ['h'] ["help"] (NoArg Help) "Display this message"
      , Option ['n'] ["dry-run"] (NoArg DryRun) "Only output what would be done, don't run anything"
      ]

    parseOpt :: Flag -> StateT Context IO ()
    parseOpt Help = liftIO $ do
      s <- defaultStyle
      exe <- getProgName

      putStrLn (primary s ++ "Usage: " ++ secondary s ++ exe ++ " [options] [task]\n")
      putStrLn $ usageInfo (primary s ++ "Options:" ++ tertiary s) options
      exitSuccess
    parseOpt DryRun = modify $ \c -> c{ctxDryRun = True}

    context = Context defaultTaskfiles <$> defaultStyle <*> return False

    runArgs [] = listTasks
    runArgs (task : args) = runTask (T.pack task) args

    handleError e =
      unless (fromException e == Just ExitSuccess) $ do
        s <- defaultStyle
        hPutStrLn stderr (err s ++ "error: " ++ secondary s ++ displayException (e :: SomeException))
        exitFailure

data Flag = Help | DryRun

newtype Maid a = Maid {unMaid :: ReaderT Context IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

getTask :: Text -> [Task] -> Maybe Task
getTask name = find ((. tName) (== name))

runTask :: Text -> [String] -> Maid ()
runTask name args = do
  task <- fromMaybe (error "No such task") . getTask name . snd <$> (asks ctxTaskfile >>= liftIO)
  style <- asks ctxStyle
  dry <- asks ctxDryRun

  liftIO $ do
    putStr $ secondary style
    I.putStr $ T.strip $ tCode task
    putStrLn $ tertiary style
    unless dry $ case tLang task of
      "sh" -> runShell (B.fromStrict $ E.encodeUtf8 $ tCode task) args
      _ -> error ("Unsupported language: " ++ T.unpack (tLang task))

runShell :: LazyByteString -> [String] -> IO ()
runShell input args =
  runProcess_ $ setStdin (byteStringInput input) $ proc "sh" (["-seu", "--"] ++ args)

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
      I.putStrLn ("  " <> tName task)
      putStr $ tertiary style
      I.putStrLn (T.unlines $ map ("    " <>) $ T.lines $ desc task)
  where
    desc task =
      if T.all isSpace $ tDesc task
        then "[No description]"
        else T.strip $ tDesc task

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
        maybeTask = (\x -> bool (Just x) Nothing) . (f,) <*> null

data Context = Context
  { ctxTaskfile :: IO (FilePath, [Task])
  , ctxStyle :: Style
  , ctxDryRun :: Bool
  }

defaultStyle :: IO Style
defaultStyle = do
  bool empty def <$> (isNothing <$> lookupEnv "NO_COLOR") .&&. hNowSupportsANSI stdout
  where
    def = Style "95;1" "0;1" "" "31;1"
    empty = Style (Color Nothing) (Color Nothing) (Color Nothing) (Color Nothing)

primary :: Style -> String
primary = c . sPrimary

secondary :: Style -> String
secondary = c . sSecondary

tertiary :: Style -> String
tertiary = c . sTertiary

err :: Style -> String
err = c . sErr

data Style = Style
  { sPrimary :: Color
  , sSecondary :: Color
  , sTertiary :: Color
  , sErr :: Color
  }

c :: Color -> String
c (Color Nothing) = ""
c (Color (Just a)) = "\x1b[" ++ a ++ "m"

newtype Color = Color (Maybe String)

instance IsString Color where
  fromString = Color . Just

-- | Combine two predicates with and
(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)
