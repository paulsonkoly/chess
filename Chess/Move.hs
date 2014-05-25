module Chess.Move
   ( module M
   -- * properties
   , prop_MoveBoardAfter
   , prop_numberOfPieces
   , prop_numberOfKings
   , prop_zobrist
   , prop_MoveLegalityCheck
   )
   where

import Chess.Board
import qualified Chess.Board as B (calcHash)
import Chess.Move.Execute    as M
import Chess.Move.Generator  as M
import Chess.Move.Move       as M
import Control.Lens
import Data.BitBoard


prop_MoveBoardAfter :: Board -> Bool
prop_MoveBoardAfter b = all prop_Board $ map (`makeMove` b) (moves b)


prop_numberOfPieces :: Board -> Bool
prop_numberOfPieces b = let o = popCount $ occupancy b
                            n = [ popCount $ occupancy $ makeMove m b | m <- moves b ]
                        in all (\x -> x == o || x == o - 1) n


prop_numberOfKings :: Board -> Bool
prop_numberOfKings b = all prop_BoardKingNum [ makeMove m b | m <- moves b ]


prop_zobrist :: Board -> Bool
prop_zobrist b = let b' = (hash .~ B.calcHash b) b
                     bs = [ makeMove m b' | m <- moves b']
                 in all (\b'' -> B.calcHash b'' == b''^.hash) bs


-- | Property that asserts that a move cannot leave the King in check
prop_MoveLegalityCheck :: Board -> Bool
prop_MoveLegalityCheck b = let bs = [ makeMove m b | m <- moves b ]
                           in all (not . flip inCheck (b^.next)) bs
