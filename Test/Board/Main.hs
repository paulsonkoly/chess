module Main (main) where

import Test.QuickCheck
import Chess.Board

main :: IO ()
main = do
  quickCheck prop_Board
  quickCheck prop_BoardKingNum
  quickCheck prop_FEN
