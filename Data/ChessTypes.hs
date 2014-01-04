module Data.ChessTypes
       ( Castle(..)
       , pieceValue
       ) where


import qualified Chess as C


data Castle = Short | Long deriving (Show, Eq, Enum)


pieceValue :: C.PieceType -> Int
pieceValue C.Queen  = 90
pieceValue C.Rook   = 50
pieceValue C.Bishop = 30
pieceValue C.Knight = 30
pieceValue C.Pawn   = 10
pieceValue C.King   = 0


