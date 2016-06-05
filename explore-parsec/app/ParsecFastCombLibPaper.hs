{-# LANGUAGE FlexibleContexts #-}

module ParsecFastCombLib where

-- module to implement the example in the paper Parsec : A fast combinator parser by Daan Leijen

import Text.Parsec

test p = parse p ""

simple :: Stream s m Char => ParsecT s u m Char 
simple = letter

word :: Stream s m Char => ParsecT s u m String
word = many letter


