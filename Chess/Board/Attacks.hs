module Chess.Board.Attacks
       ( attackedBy
       , isAttacked
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


-- | the bitboard from where the piece type of the given colour is attacking the specified position
attackedBy :: PieceType -> Board -> BitBoard -> Colour -> Square -> BitBoard
attackedBy Queen b occ c pos  = magic Queen pos occ            .&. piecesOf b c Queen
attackedBy Bishop b occ c pos = magic Bishop pos occ           .&. piecesOf b c Bishop
attackedBy Rook b occ c pos   = magic Rook pos occ             .&. piecesOf b c Rook
attackedBy Knight b _ c pos   = knightAttackBB pos             .&. piecesOf b c Knight
attackedBy King b _ c pos     = kingAttackBB pos               .&. piecesOf b c King
attackedBy Pawn b _ c pos     = pawnAttackBB pos (opponent' c) .&. piecesOf b c Pawn


-- | are any of the given player's pieces attacking the given square?
isAttacked :: Board ->  Colour -> Square -> Bool
isAttacked b = isAttackedWithOccupancy b (occupancy b)


isAttackedWithOccupancy :: Board -> BitBoard -> Colour -> Square -> Bool
isAttackedWithOccupancy b occ c pos = any (/= mempty)
                                      [ attackedBy pt b occ c pos
                                      | pt <- [ Queen, Bishop, Rook, Knight, King, Pawn ]
                                      ]


inCheck :: Board -> Colour -> Bool
inCheck b c = let kP = head $ toList $ piecesOf b c King
              in  isAttacked b (opponent' c) kP


-- | is the king with the given colour attacked with the friendly pieces removed?
inCheckWithNoFriendly :: Board -> Colour -> Bool
inCheckWithNoFriendly b c =  let kP = head $ toList $ piecesOf b c King
                             in  isAttackedWithOccupancy b (b^.piecesByColour (opponent' c)) (opponent' c) kP
