{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Maid.Parser (parseTasks, Task (..), parseMarkdown, Block (..), findTaskSection) where

import Control.Applicative (liftA2)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

parseTasks :: Text -> [Task]
parseTasks = findTaskSection . parseMarkdown

findTaskSection :: [Block] -> [Task]
findTaskSection (Heading h _ : Paragraph p : rest)
  | ["<!--", "maid-tasks", "-->"] == T.words p =
      tasks $ takeWhile (\case Heading h' _ | h' <= h -> False; _ -> True) rest
  where
    tasks (Heading _ name : rest) =
      findDesc emptyTask{tName = T.toLower name} rest
    tasks (_ : rest) = tasks rest
    tasks [] = []

    findDesc task (Paragraph desc : rest) =
      findCode task{tDesc = T.strip desc} rest
    findDesc task rest =
      findCode task{tDesc = "[No description]"} rest

    findCode task (Code lang code : rest) =
      task{tLang = getLang lang, tCode = code} : tasks rest
    findCode task (Paragraph _ : rest) =
      findCode task rest
    findCode _ rest = tasks rest
findTaskSection (_ : rest) = findTaskSection rest
findTaskSection [] = []

getLang :: Text -> Text
getLang t | T.all isSpace t = "sh"
getLang t = T.strip t

data Task = Task
  { tName :: Text
  , tDesc :: Text
  , tLang :: Text
  , tCode :: Text
  }
  deriving (Show)

emptyTask :: Task
emptyTask = Task "" "" "" ""

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
      Heading (T.length hashes) (T.stripStart line') : parseLines rest
      where
        (hashes, line') = T.span (== '#') line
    parseFencedCode line rest =
      Code lang (T.unlines src) : parseLines (tail rest')
      where
        (end, lang) = T.span (== T.head line) line
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

-- | Combine two predicates with or
(.||.) :: Applicative f => f Bool -> f Bool -> f Bool
(.||.) = liftA2 (||)

data Block = Heading Int Text | Code Text Text | Paragraph Text
  deriving (Show)
