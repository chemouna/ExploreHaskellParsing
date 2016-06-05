{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ParsecTimeDate where

import Data.Data
import Data.Typeable
import Text.Parsec
import Text.Parsec.String
import Data.Hourglass

-- parsec parser for dates 

test p = parse p ""

data DataIntervalType = Day | Week | Month | Year
  deriving (Eq, Show, Data, Typeable)

data DateInterval = Days Int
                  | Weeks Int
                  | Months Int
                  | Years Int

yesterday :: Parser DateInterval
yesterday = do
  string "yesterday"
  return $ Days (-1)

tomorrow :: Parser DateInterval
tomorrow = do
  string "tomorrow"
  return $ Days 1

today :: Parser DateInterval
today = do
  string "today" <|> string "now"
  return $ Days 0

pRelTime ::Stream s m Char => Config -> ParsecT s st m DateTime
pRelTime Config { now = now' } = do
  offs <- try futureTime
    <|> try passsTime
    <|> try thisminute
    <|> try thishour
    <|> try atnight
    <|> try morning
  return $ now `addInterval` offs

tryToReadInt :: (Stream s m Char, Num a) => String -> ParsecT s st m a
tryToReadInt = str
  if all isDigit string
     then
    else fail $ "Cannot read: " ++ str

futureTime :: Stream s m Char => ParsecT s st m TimeInterval
futureTime = do
  string "in" <|> string "after"
  char ' '
  n <- many1 digit
  tp <- pDateTimeIntervalType
  case tp of
    Minute -> (Minutes . negate) `fmap` tryToReadInt n
    Hour -> (Hours . negate) `fmap` tryToReadInt n 

data Config = Config {
                      now :: DateTime
                     ,startOfWeekDay :: WeekDay 
                     }

parseDateTime :: Config -> String -> Either ParseError DateTime
parseDateTime c s = runParser (pDateTime c) () "" s

