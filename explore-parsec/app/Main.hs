{-# LANGUAGE FlexibleContexts #-}
module Main where

import Lib

import Text.Parsec
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


-- main :: IO ()
-- main = someFunc

