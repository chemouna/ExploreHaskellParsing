{-# LANGUAGE FlexibleContexts #-}

module Main where

-- import Lib

import Text.Parsec
import Text.Parsec.Combinator
import Data.Char 

import Control.Applicative ((<$), (<*), (*>), (<$>), (<*>))

test p = parse p ""

charAParser:: Parsec String st Char
charAParser = (char 'a')

wordParser :: Parsec String st String
wordParser = many $ noneOf [' ']

secondWordParser :: Parsec String st String
secondWordParser = wordParser *> (char ' ') *> wordParser

twoWordParser :: Parsec String st [String]
twoWordParser = listfy <$> wordParser <*> ((char ' ') *> wordParser)
  where listfy a b = [a, b]

wordsParser :: Parsec String st [String]
wordsParser = (:) <$> wordParser <*> many ((char ' ') *> wordParser)

-- wordsParserAlt:: Parsec String st [String]
-- wordsParserAlt xs = xs `sepBy1` (char ' ')

csvParser :: Parsec String st [[String]]
csvParser = lineParser `endBy` newline <* eof
  where lineParser = cellParser `sepBy` (char ',')
        cellParser = many $ noneOf ",\n"

--

dogCatParser :: Parsec String st String
dogCatParser = (string "dog") <|> (string "cat")

--

camelCatTryParser:: Parsec String st String
camelCatTryParser = try (string "camel") <|> (string "cat")

--

type TNumber = Int

data TOperator = TAdd
               | TSubtract
                 deriving (Eq, Ord, Show)

data TExpression = TNode (TExpression) TOperator (TExpression)
                 | TTerminal TNumber
                   deriving (Show)

numberParser :: Parsec String st TNumber
numberParser = read <$> (many $ oneOf "0123456789")

operatorParser :: Parsec String st TOperator
operatorParser = chooseOp <$> (oneOf "+-")
  where chooseOp '+' = TAdd
        chooseOp '-' = TSubtract


-- try also attoparsec

-- decompose each value in the type to understand whats wrong

-- main :: IO ()
-- main = someFunc



