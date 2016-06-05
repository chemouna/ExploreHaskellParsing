{-# LANGUAGE FlexibleContexts #-}

module ParsecFastCombLib where

-- module to implement the example in the paper Parsec : A fast combinator parser by Daan Leijen

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Expr

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

-- Adding semantics
nesting :: Parser Int
nesting = do { char '('
             ; n <- nesting
             ; char ')'
             ; m <- nesting
             ; return (max (n+1) m)
            }
          <|> return 0

-- sequences and separators
word2 :: Parser String
word2 = many1 letter

sentence :: Parser [String]
sentence = do { words <- sepBy1 word2 separator
                 ; oneOf ".?!"
                 ; return words
                 }

separator :: Parser ()
separator = skipMany1 (space <|> char ',')

-- Better error messages
word3 :: Parser String
word3 = many1 (letter <?> "") <?> "word"

sentence2 :: Parser [String]
sentence2 = do { words <- sepBy1 word3 separator2
                 ; oneOf ".?!" <?> "end of sentence"
                 ; return words
                 }
separator2 :: Parser ()
separator2 = skipMany1 (space <|> char ',' <?> "")

-- expressions
expr :: Parser Integer
expr = buildExpressionParser table factor
         <?> "expression"

table = [[op "*" (*) AssocLeft, op "/" div AssocLeft],
         [op "+" (+) AssocLeft, op "-" (-) AssocLeft]]
        where
          op s f assoc
            = Infix (do{ string s; return f}) assoc

factor = do { char '('
            ; x <- expr
            ; char ')'
            ; return x;
            }
        <|> number
        <?> "simple expression"

number :: Parser Integer
number = do { ds <- many1 digit
            ; return (read ds)
            }
         <?> "number"
