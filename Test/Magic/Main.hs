module Main (main) where

import Test.QuickCheck
import Chess.Magic

main :: IO ()
main = quickCheck prop_slidingAttack
