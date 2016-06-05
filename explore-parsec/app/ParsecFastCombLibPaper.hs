{-# LANGUAGE FlexibleContexts #-}

module ParsecFastCombLib where

-- module to implement the example in the paper Parsec : A fast combinator parser by Daan Leijen

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Char

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
testOr :: Parser String 
testOr = string "(a)" <|> string "(b)"

testOrWorks :: Parser Char
testOrWorks = do { char '('
                 ; char 'a' <|> char 'b'
                 ; char ')'
                 }

testOr2 :: Parser String
testOr2 = try(string "(a)") <|> string "(b)"

testOr3 :: Parser String 
testOr3 = do { try(string "(a")
             ; char ')'
             ; return "(a)"
             }
          <|> string "(b)"
