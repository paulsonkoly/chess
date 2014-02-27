module Data.ChessTypes
       ( Castle(..)
       , pieceValue
       , charToPiece
       , direction
       ) where

import           Data.Char
import qualified Chess as C


data Castle = Short | Long deriving (Show, Eq, Enum)


pieceValue :: C.PieceType -> Int
pieceValue C.Queen  = 90
pieceValue C.Rook   = 50
pieceValue C.Bishop = 30
pieceValue C.Knight = 30
pieceValue C.Pawn   = 10
pieceValue C.King   = 0


charToPiece :: Char -> C.PieceType
charToPiece 'q' = C.Queen
charToPiece 'r' = C.Rook
charToPiece 'b' = C.Bishop
charToPiece 'n' = C.Knight
charToPiece 'p' = C.Pawn
charToPiece 'k' = C.King
charToPiece c   = if c `elem` "QRBNPK" then charToPiece $ toLower c else error "Invalid char in charToPiece"


direction :: C.Color -> Int -> Int
direction C.White = id
direction C.Black = (* (-1))
{-# INLINE direction #-}
