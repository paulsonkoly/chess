module Chess.Move.Attacks
       ( attacking
       , attackedBy
       , isAttacked
       ) where

import           Control.Lens
import           Data.Monoid
import           Data.Functor

import qualified Chess as C
-- import           Chess.Move.Move
import           Chess.Board
import           Chess.Magic
import           Data.BitBoard
import           Data.ChessTypes
import           Data.Square

  
-- | the bitboard & the piece position that the given piece type attacks with the given colour
attacking :: C.PieceType -> Board -> C.Color -> [ (Square, BitBoard) ]
attacking pt b c =
  let pcs     = toList $ piecesOf b c pt
      notme   = complement $ b^.piecesByColour c
      att pos = (pos, notme .&. m pos)
      m pos   = case pt of
        C.Queen  -> magic C.Queen pos (occupancy b)
        C.Rook   -> magic C.Rook pos (occupancy b)
        C.Bishop -> magic C.Bishop pos (occupancy b)
        C.Knight -> knightAttackBB pos
        C.King   -> kingAttackBB pos
        C.Pawn   -> undefined
  in case pt of
    C.Pawn -> undefined
    _      -> att <$> pcs


-- | the bitboard from where the piece type of the given colour is attacking the specified position
attackedBy :: C.PieceType -> Board -> C.Color -> Square -> BitBoard
attackedBy C.Queen b c pos = magic C.Queen pos (occupancy b) .&. piecesOf b c C.Queen

attackedBy C.Bishop b c pos = magic C.Bishop pos (occupancy b) .&. piecesOf b c C.Bishop

attackedBy C.Rook b c pos = magic C.Rook pos (occupancy b) .&. piecesOf b c C.Rook

attackedBy C.Knight b c pos = knightAttackBB pos .&. piecesOf b c C.Knight

attackedBy C.King b c pos   = kingAttackBB pos  .&. piecesOf b c C.King
-- TODO : en passant
attackedBy C.Pawn b c pos   = let mask  = neighbourFilesBB (file pos) .&. piecesOf b c C.Pawn
                              in (fromSquare (offset pos $ direction c (-7))
                                 .|. fromSquare (offset pos $ direction c (-9)))
                                 .&. mask


-- | are any of the given player's pieces attacking the given square?
isAttacked :: Board -> C.Color -> Square -> Bool
isAttacked b c pos = any (/= mempty)
                     [ attackedBy pt b c pos
                     | pt <- [ C.Queen, C.Bishop, C.Rook, C.Knight, C.King, C.Pawn ]
                     ]

