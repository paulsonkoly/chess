{- | Checks for a `Square` being attacked by one of the players.
 -}
module Chess.Board.Attacks
       ( isAttacked
       , attackedFromBB
       , inCheck
       , inCheckWithNoFriendly
       ) where

import           Control.Lens
import           Data.Monoid

import           Data.ChessTypes
import           Chess.Board.Board
import           Chess.Magic
import           Data.BitBoard
import           Data.Square


-- | are any of the given player's pieces attacking the given square?
isAttacked :: Board -> Colour -> Square -> Bool
isAttacked b c s = mempty /= attackedFromBB b (occupancy b) c s


-- | is the specified player in check?
inCheck :: Board -> Colour -> Bool
inCheck b c = let kP = head $ toList $ piecesOf b c King
              in  isAttacked b (opponent' c) kP


-- | is the specified player in check with the friendly pieces removed?
inCheckWithNoFriendly :: Board -> Colour -> Bool
inCheckWithNoFriendly b c =  let kP = head $ toList $ piecesOf b c King
                             in  mempty /= attackedFromBB b (b^.piecesByColour (opponent' c)) (opponent' c) kP



attackedFromBB :: Board -> BitBoard -> Colour -> Square -> BitBoard
attackedFromBB b occ c pos = foldr1 (<>) $ do
  let attackBitBoard Bishop = {-# SCC attackBitBoardBishop #-} magic Bishop pos occ
      attackBitBoard Rook   = {-# SCC attackBitBoardRook   #-} magic Rook pos occ
      attackBitBoard Queen  = {-# SCC attackBitBoardQueen  #-} attackBitBoard Bishop <> attackBitBoard Rook
      attackBitBoard Knight = {-# SCC attackBitBoardKnight #-} knightAttackBB pos
      attackBitBoard King   = {-# SCC attackBitBoardKing   #-} kingAttackBB pos
      attackBitBoard Pawn   = {-# SCC attackBitBoardPawn   #-} pawnAttackBB pos (opponent' c)
  pt <- [ Queen, Bishop, Rook, Knight, King, Pawn ]      
  return $ attackBitBoard pt .&. piecesOf b c pt
