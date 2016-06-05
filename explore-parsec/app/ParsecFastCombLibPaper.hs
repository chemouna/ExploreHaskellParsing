{-# LANGUAGE FlexibleContexts #-}

module ParsecFastCombLib where

-- module to implement the example in the paper Parsec : A fast combinator parser by Daan Leijen

import Text.Parsec
import qualified Text.Parsec.Combinator as C
import Text.Parsec.String (Parser)

test p = parse p ""

simple :: Parser Char
simple = letter

word :: Parser String
word = many letter

openCloseParser :: Parser Char
openCloseParser = do { char '('
                       ; char ')'
                     }

parens :: Parser ()
parens = do { char '('
             ; parens
             ; char ')'
             ; parens
            }
         <|> return ()


-- (<|>) is predictive 
testOr = String "(a)" <|> String "(b)"

testOrWorks = do { char '('
                 ; char "a" <|> char "b"
                 ; char ")"
                 }
