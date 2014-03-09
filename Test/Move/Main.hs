module Main (main) where

import Test.QuickCheck
import Chess.Move
import Chess.Board

main :: IO ()
main = do
  quickCheck prop_Board
  quickCheck prop_MoveBoardAfter
  quickCheck prop_numberOfKings
  quickCheck prop_numberOfPieces
  quickCheck prop_zobrist
