{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ParsecTimeDate where

import Data.Data
import Data.Typeable
import Text.Parsec
import Text.Parsec.String

-- parsec parser for dates 

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
