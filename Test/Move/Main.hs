module Main (main) where

import Test.QuickCheck
import Chess.Move

main :: IO ()
main = do
  quickCheck prop_MoveBoardAfter
  quickCheck prop_numberOfKings
  quickCheck prop_numberOfPieces
  quickCheck prop_zobrist
