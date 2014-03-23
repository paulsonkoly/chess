{- | The module provides a zobrist keys from a vector of 781 pseudo
     random bitboards.

 This can be used for a positions zobrist hash calculations. For more info
 see http://chessprogramming.wikispaces.com/Zobrist+Hashing.
-}

module Chess.Zobrist
       ( ZobristKey(..)
       , zobrist   
       ) where

import           Control.Monad.Random
import           Control.Monad.Identity

import           Data.Word
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed ((!))

import           Data.ChessTypes
import           Data.Square


data ZobristKey = ZobristPiece
                  { square     :: Square
                  , pieceColor :: Colour
                  , pieceType  :: PieceType
                  }
                | ZobristSide { side :: Colour }
                | ZobristCastlingRights
                  { whiteRights :: CastlingRights
                  , blackRights :: CastlingRights
                  }
                | ZobristEnPassant { enPassantSquare :: Maybe Square }
                    


zobrist :: ZobristKey -> Word64
zobrist k = zobristVec ! zobristKey k
  where zobristKey (ZobristPiece sq c pt)        = (fromEnum sq * 12) + colVal c * 6 + ptVal pt
        zobristKey (ZobristSide c)               = 12 * 64 + colVal c
        zobristKey (ZobristCastlingRights wc bc) = 12 * 64 + 2 + crVal wc bc
        zobristKey (ZobristEnPassant (Just sq))  = 12 * 64 + 2 + maxCastle * maxCastle + fromEnum (file sq)
        zobristKey (ZobristEnPassant Nothing)    = 12 * 64 + 2 + maxCastle * maxCastle + (1 + fromEnum (maxBound :: File))
        maxCastle = fromEnum (maxBound :: CastlingRights)
        crVal wc bc = maxCastle * fromEnum wc + fromEnum bc
        colVal White = 0
        colVal Black = 1
        ptVal Rook = 0
        ptVal Knight = 1
        ptVal Bishop = 2
        ptVal Queen = 3
        ptVal King = 4
        ptVal Pawn = 5


zobristVec :: V.Vector Word64
zobristVec = runIdentity $ do
  (a, _) <- runRandT (V.replicateM (12 * 64 + 2 + 9 + 8 + 1) getRandom) (mkStdGen 0)
  return a
