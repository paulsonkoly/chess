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

import Data.Maybe (catMaybes)

import Chess.Board
import qualified Chess.Board as B (calcHash)
import Chess.Move.Execute    as M
import Chess.Move.Generator  as M
import Chess.Move.Move       as M
import Control.Lens
import Data.BitBoard


------------------------------------------------------------------------------
-- | Asserting that the board is valid after a move
prop_MoveBoardAfter :: Board -> Bool
prop_MoveBoardAfter b = all prop_Board $ map (`makeMove` b) (legalMoves b)


------------------------------------------------------------------------------
-- | Asserting that after a move the number of peices is the same or one less.
prop_numberOfPieces :: Board -> Bool
prop_numberOfPieces b =
  let o = popCount $ occupancy b
      n = [ popCount $ occupancy $ makeMove m b | m <- legalMoves b ]
  in all (\x -> x == o || x == o - 1) n


------------------------------------------------------------------------------
-- | Asserting that after a move the number of Kings is 2.
prop_numberOfKings :: Board -> Bool
prop_numberOfKings b =
  all prop_BoardKingNum [ makeMove m b | m <- legalMoves b ]


------------------------------------------------------------------------------
-- | Asserting that after a move the zobrist is consistent 
prop_zobrist :: Board -> Bool
prop_zobrist b = let b' = (hash .~ B.calcHash b) b
                     bs = [ makeMove m b' | m <- legalMoves b']
                 in all (\b'' -> B.calcHash b'' == b''^.hash) bs


------------------------------------------------------------------------------
-- | Property that asserts that a move cannot leave the King in check
prop_MoveLegalityCheck :: Board -> Bool
prop_MoveLegalityCheck b = let bs = [ makeMove m b | m <- legalMoves b ]
                           in all (not . flip inCheck (b^.next)) bs


------------------------------------------------------------------------------
legalMoves :: Board -> [ Move ]
legalMoves b =
  let leg = mkLegality b
  in catMaybes $ map (legalCheck leg) $ moves b
