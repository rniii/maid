{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Maid.Parser (parseTasks, Task (..), parseMarkdown, Block(..), findTaskSection) where

import Control.Applicative (liftA2)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

parseTasks :: Text -> [Task]
parseTasks = findTaskSection . parseMarkdown

findTaskSection :: [Block] -> [Task]
findTaskSection (Heading h _ : Paragraph p : rest)
  | ["<!--", "maid-tasks", "-->"] == T.words p =
      tasks inner
  where
    inner = takeWhile (\case Heading h' _ | h' <= h -> False; _ -> True) rest
    tasks (Heading _ name : Paragraph desc : Code lang code : rest) =
      Task name desc (getLang lang) code : tasks rest
    tasks (Heading _ name : Code lang code : rest) =
      Task name "" (getLang lang) code : tasks rest
    tasks (_ : rest) = tasks rest
    tasks [] = []
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

-- | Combine two predicates with or
(.||.) :: Applicative f => f Bool -> f Bool -> f Bool
(.||.) = liftA2 (||)

data Block = Heading Int Text | Code Text Text | Paragraph Text
  deriving (Show)
