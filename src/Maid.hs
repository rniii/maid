{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Maid (run, parseMarkdown, Block (..)) where

import Control.Applicative (liftA2)
import Control.Monad (forM_)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (LazyByteString)
import Data.Char (isSpace)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as I
import System.Directory (doesPathExist)
import System.Environment (getArgs)
import System.Process.Typed (byteStringInput, proc, runProcess_, setStdin)

run :: IO ()
run = do
  ctx <- getContext
  getArgs >>= \case
    "help" : _ -> listTasks ctx
    name : args -> runTask ctx (T.pack name) args
    [] -> putStrLn ""

runTask :: Context -> Text -> [String] -> IO ()
runTask ctx name args = do
  I.putStrLn $ T.strip $ tCode task
  case lang of
    "sh" -> runShell (B.fromStrict $ E.encodeUtf8 $ tCode task) args
    _ -> error ("Unsupported language: " ++ T.unpack lang)
  where
    task = fromMaybe (error "No such task") $ find ((. tName) (== name)) $ ctxTasks ctx
    lang =
      if T.all isSpace $ tLang task
        then "sh"
        else T.strip $ tLang task

runShell :: LazyByteString -> [String] -> IO ()
runShell input args =
  runProcess_ (setStdin (byteStringInput input) (proc "sh" ("-seu" : "--" : args)))

listTasks :: Context -> IO ()
listTasks ctx = do
  putStrLn ("Tasks in " <> ctxFile ctx)
  putStrLn ""
  forM_ (ctxTasks ctx) $ \task -> do
    I.putStrLn ("  " <> tName task)
    I.putStrLn (T.unlines $ map ("    " <>) $ T.lines $ desc task)
  where
    desc task =
      if tDesc task == T.empty
        then "[No description]"
        else tDesc task

getContext :: IO Context
getContext = do
  (f, tasks) <- head . catMaybes <$> mapM getTask paths
  return $ Context f tasks
  where
    paths = ["CONTRIBUTING.md", "README.md"]
    getTask f =
      doesPathExist f >>= \e ->
        if e
          then do
            tasks <- findTasks . parseMarkdown <$> I.readFile f
            return $ Just (f, tasks)
          else return Nothing

data Context = Context
  { ctxFile :: String
  , ctxTasks :: [Task]
  }

findTasks :: [Block] -> [Task]
findTasks (Heading h _ : Paragraph p : rest)
  | ["<!--", "maid-tasks", "-->"] == T.words p =
      tasks inner
  where
    inner = takeWhile (\case Heading h' _ | h' <= h -> False; _ -> True) rest
    tasks (Heading _ name : Paragraph desc : Code lang code : rest) =
      Task name desc lang code : tasks rest
    tasks (Heading _ name : Code lang code : rest) =
      Task name "" lang code : tasks rest
    tasks (_ : rest) = tasks rest
    tasks [] = []
findTasks (_ : rest) = findTasks rest
findTasks [] = []

data Task = Task
  { tName :: Text
  , tDesc :: Text
  , tLang :: Text
  , tCode :: Text
  }
  deriving (Show)

-- | Parse markdown into headings, codeblocks and paragraphs
parseMarkdown :: Text -> [Block]
parseMarkdown str =
  parseLines (T.lines str)
  where
    parseLines (line : rest)
      | isBlank line = parseLines rest
      | isHeading line = parseHeading line rest
      | isFenced line = parseFencedCode line rest
      | isIndented line = parseIndentedCode line rest
      | otherwise = parseParagraph line rest
    parseLines [] = []

    parseHeading line rest =
      Heading h (T.stripStart $ T.drop h line) : parseLines rest
      where
        h = T.length $ T.takeWhile (== '#') line
    parseFencedCode line rest =
      Code lang (T.unlines src) : parseLines (tail rest')
      where
        end = T.takeWhile (== T.head line) line
        lang = T.drop (T.length end) line
        (src, rest') = break (end `T.isPrefixOf`) rest
    parseIndentedCode line rest =
      Code "" (T.unlines $ map (T.drop 4) src) : parseLines rest'
      where
        (src, rest') = span (isIndented .||. isBlank) (line : rest)
    parseParagraph line rest =
      Paragraph (T.unlines src) : parseLines rest'
      where
        (src, rest') = break (isHeading .||. isFenced .||. isBlank) (line : rest)
    isBlank = T.all isSpace
    isHeading = ("#" `T.isPrefixOf`)
    isIndented = ("    " `T.isPrefixOf`)
    isFenced = ("```" `T.isPrefixOf`) .||. ("~~~" `T.isPrefixOf`)

-- | Combine two predicates
(.||.) :: Applicative f => f Bool -> f Bool -> f Bool
(.||.) = liftA2 (||)

data Block = Heading Int Text | Code Text Text | Paragraph Text
  deriving (Show)
