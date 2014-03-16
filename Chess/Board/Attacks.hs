module Chess.Board.Attacks
       ( attacking
       , attackedBy
       , isAttacked
       , inCheck
       , inCheckWithNoFriendly
       ) where

import           Control.Lens
import           Data.Monoid
import           Data.Functor

import qualified Chess as C
import           Chess.Board.Board
import           Chess.Magic
import           Data.BitBoard
import           Data.ChessTypes
import           Data.Square

  
-- | the bitboard & the piece position that the given piece type attacks with the given colour
attacking
  :: C.PieceType   -- ^ piece type
  -> Board         -- ^ board
  -> C.Color       -- ^ attacker's colour
  -> BitBoard      -- ^ occupancy to use
  -> [ (Square, BitBoard) ]
attacking pt b c occ =
  let pcs     = toList $ piecesOf b c pt
      notme   = complement $ b^.piecesByColour c
      att pos = (pos, notme .&. m pos)
      m pos   = case pt of
        C.Queen  -> magic C.Queen pos occ
        C.Rook   -> magic C.Rook pos occ
        C.Bishop -> magic C.Bishop pos occ
        C.Knight -> knightAttackBB pos
        C.King   -> kingAttackBB pos
        C.Pawn   -> undefined
  in case pt of
    C.Pawn -> undefined
    _      -> att <$> pcs


-- | the bitboard from where the piece type of the given colour is attacking the specified position
attackedBy :: C.PieceType -> Board -> BitBoard -> C.Color -> Square -> BitBoard
attackedBy C.Queen b occ c pos  = magic C.Queen pos occ          .&. piecesOf b c C.Queen
attackedBy C.Bishop b occ c pos = magic C.Bishop pos occ         .&. piecesOf b c C.Bishop
attackedBy C.Rook b occ c pos   = magic C.Rook pos occ           .&. piecesOf b c C.Rook
attackedBy C.Knight b _ c pos   = knightAttackBB pos             .&. piecesOf b c C.Knight
attackedBy C.King b _ c pos     = kingAttackBB pos               .&. piecesOf b c C.King
attackedBy C.Pawn b _ c pos     = pawnAttackBB pos (opponent' c) .&. piecesOf b c C.Pawn


-- | are any of the given player's pieces attacking the given square?
isAttacked :: Board ->  C.Color -> Square -> Bool
isAttacked b = isAttackedWithOccupancy b (occupancy b)


isAttackedWithOccupancy :: Board -> BitBoard -> C.Color -> Square -> Bool
isAttackedWithOccupancy b occ c pos = any (/= mempty)
                                      [ attackedBy pt b occ c pos
                                      | pt <- [ C.Queen, C.Bishop, C.Rook, C.Knight, C.King, C.Pawn ]
                                      ]


inCheck :: Board -> C.Color -> Bool
inCheck b c = let kP = head $ toList $ piecesOf b c C.King
              in isAttacked b (opponent' c) kP


-- | is the king with the given colour attacked with the friendly pieces removed?
inCheckWithNoFriendly :: Board -> C.Color -> Bool
inCheckWithNoFriendly b c =  let kP = head $ toList $ piecesOf b c C.King
                             in isAttackedWithOccupancy b (b^.piecesByColour (opponent' c)) (opponent' c) kP



pawnAttackBB :: Square -> C.Color -> BitBoard
pawnAttackBB pos c = let mask  = neighbourFilesBB (file pos)
                     in mconcat [ fromSquare (offset pos $ direction c n) | n <- [7, 9] ] .&. mask
