module Main (main) where

import           Control.Monad
import qualified Control.Monad.State as S

import           Data.Maybe

import           Test.HUnit

import qualified Chess as C

import           Chess.Move
import           Chess.Board
import           Chess.Magic


perft :: Magic -> Magic -> Int -> Board -> Integer
perft bishopMagics rookMagics d = S.evalState (perft' d)
  where perft' d' = if d' == 0
                    then return 1
                    else do
                      let step m = do
                            doMoveM m
                            result <- perft' (d' - 1)
                            undoMoveM m
                            return result
                      ms <- liftM (moves bishopMagics rookMagics) S.get
                      liftM sum $ mapM step ms

initialBoard :: Board
initialBoard = fromJust $ fromFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"


-- from http://chessprogramming.wikispaces.com/Perft+Results
initialPerftResult :: Int -> Integer
initialPerftResult 0  = 1
initialPerftResult 1  = 20
initialPerftResult 2  = 400
initialPerftResult 3  = 8902
initialPerftResult 4  = 1972810
initialPerftResult 5  = 48656091
initialPerftResult 6  = 1190603242
initialPerftResult 7  = 31959018603
initialPerftResult 8  = 84998978956
initialPerftResult 9  = 2439530234167
initialPerftResult 10 = 69352859712417
initialPerftResult 11 = 2097651003696806
initialPerftResult 12 = 62854969236701747
initialPerftResult 13 = 1981066775000396239

testInitialPos :: Magic -> Magic -> Int -> Test
testInitialPos bishopMagics rookMagics n = perft bishopMagics rookMagics n initialBoard ~?= initialPerftResult n

initialTests :: Magic -> Magic -> Test
initialTests bishopMagics rookMagics =
  TestList [ TestLabel ("Perft " ++ show n) $ testInitialPos bishopMagics rookMagics n | n <- [0 .. 13] ]

main :: IO ()
main = do
  let bishopMagics = makeMagic C.Bishop
      rookMagics   = makeMagic C.Rook
  _ <- runTestTT $ initialTests bishopMagics rookMagics
  return ()
