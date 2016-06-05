
module ParsecFastCombLib where

-- module to implement the example in the paper Parsec : A fast combinator parser by Daan Leijen

import Text.Parsec

simple :: Parsec String st Char
simple = letter

