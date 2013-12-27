module Data.Square
       ( Square
       , parserSquare
       ) where

import           Data.Char

import           Text.ParserCombinators.Parsec

type Square = Int


parserSquare :: Parser Square
parserSquare = do
  file <- oneOf ['a' .. 'h']
  rank <- oneOf ['1' .. '8']
  return $ 8 * (digitToInt rank - 1) + (ord file - ord 'a')


