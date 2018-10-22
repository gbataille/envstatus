module EnvStatus.Output.Parse where

import Data.Functor (($>))
import Data.List (delete)
import Text.Parsec (many, parse, try)
import Text.Parsec.Char (anyChar, char, noneOf, oneOf, string)
import Text.Parsec.Combinator (choice, eof, manyTill, notFollowedBy)
import Text.Parsec.Prim ((<|>), (<?>))
import Text.Parsec.String (Parser)

import EnvStatus.Output.Types (Token(..))

parseOutputFormat :: String -> [Token]
parseOutputFormat outputFormat =
  case parsed of
      Right matches -> matches
      Left _ -> []
      where
        parsed = parse (manyTill tokenParser eof) "" outputFormat

tokenParser :: Parser Token
tokenParser = choice [
  commandParser
  , rawParser
  ] <?> "token"

singleOpenCurly :: Parser Char
singleOpenCurly = do
  c <- char '{'
  _ <- notFollowedBy $ char '{'
  return c

rawParser :: Parser Token
rawParser = do
  parsed <- many (try singleOpenCurly <|> noneOf ['{'])
  return $ Raw parsed

commandParser :: Parser Token
commandParser = do
  _ <- char '{'
  _ <- char '{'
  parsed <- manyTill anyChar (try $ string "}}")
  return $ SubCommand parsed

outputFormatParser :: Parser [Token]
outputFormatParser =
  manyTill tokenParser eof

-- Parser of commands ~ words but keeping the quoted parts together
quotedOption :: Parser String
quotedOption = do
  _ <- char '"'
  manyTill anyChar (char '"')


separator :: Parser Char
separator = oneOf [' ', '\t', '\n', '\r']

skipSeparator :: Parser ()
skipSeparator = separator $> ()

word :: Parser String
word = do
  _ <- many separator
  manyTill anyChar (try skipSeparator <|> eof)

commandPart :: Parser String
commandPart = try quotedOption <|> word

parseCommand :: String -> [String]
parseCommand cmd =
  case parsed of
      -- Removes the potential last match "" if the command ended with separators
      Right matches -> delete "" matches
      Left _ -> [""]
      where
        parsed = parse (manyTill commandPart eof) "" cmd
