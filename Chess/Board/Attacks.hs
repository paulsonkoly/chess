-- | Checks for a `Square` being attacked by one of the players.
--
-- https://chessprogramming.wikispaces.com/Square+Attacked+By
module Chess.Board.Attacks
       ( isAttacked
       , attackedFromBB
       , inCheck
       , inCheckWithNoFriendly
       ) where

import Data.Monoid

import Chess.Board.Board
import Chess.Magic
import Data.BitBoard
import Data.ChessTypes
import qualified Data.ChessTypes as T (opponent)
import Data.Square


------------------------------------------------------------------------------
-- | are any of the given player's pieces attacking the given square?
--
-- Boolean test with sliding piece occupancy testing (magic look up).
isAttacked :: Board -> Colour -> Square -> Bool
isAttacked b c s = isAttackedWithOccupancy b (occupancy b) c s
  

------------------------------------------------------------------------------
-- | is the specified player in check?
inCheck :: Board -> Colour -> Bool
inCheck b c = let kP = head $ toList $ piecesOf b c King
              in  isAttacked b (T.opponent c) kP


------------------------------------------------------------------------------
-- | is the specified player in check with the friendly pieces removed?
inCheckWithNoFriendly :: Board -> Colour -> Bool
inCheckWithNoFriendly b c =
  let occ = piecesByColour b (T.opponent c)
      kP  = head $ toList $ piecesOf b c King 
  in isAttackedWithOccupancy b occ (T.opponent c) kP


------------------------------------------------------------------------------
-- Short circuit the Boolean condition.
isAttackedWithOccupancy :: Board -> BitBoard -> Colour -> Square -> Bool
isAttackedWithOccupancy b occ c s =
  any (/= mempty)
  $ attackList [Pawn, Knight, King, Queen, Bishop, Rook] b occ c s


------------------------------------------------------------------------------
-- | Bitboard with the position of the attacking pieces set. Occupancy can be
-- specified, ie. we can remove pieces from the board.
attackedFromBB :: Board -> BitBoard -> Colour -> Square -> BitBoard
attackedFromBB b occ c s = foldr1 (<>)
  $ attackList [ Queen, Bishop, Rook, Knight, King, Pawn ] b occ c s


------------------------------------------------------------------------------
attackList
  :: [PieceType]
  -> Board
  -> BitBoard
  -> Colour
  -> Square
  -> [BitBoard]
attackList l b occ c s =
  [ attackBitBoard pt s occ c .&. piecesOf b c pt | pt <- l ]


------------------------------------------------------------------------------
attackBitBoard :: PieceType -> Square -> BitBoard -> Colour -> BitBoard
attackBitBoard Bishop pos occ _ =
  {-# SCC attackBitBoardBishop #-} magic Bishop pos occ
attackBitBoard Rook pos occ _   =
  {-# SCC attackBitBoardRook   #-} magic Rook pos occ
attackBitBoard Queen pos occ c  =
  {-# SCC attackBitBoardQueen  #-}
  attackBitBoard Bishop pos occ c <> attackBitBoard Rook pos occ c
attackBitBoard Knight pos _ _   =
  {-# SCC attackBitBoardKnight #-} knightAttackBB pos
attackBitBoard King pos _ _     =
  {-# SCC attackBitBoardKing   #-} kingAttackBB pos
attackBitBoard Pawn pos _ c     =
  {-# SCC attackBitBoardPawn   #-} pawnAttackBB pos (T.opponent c)
