{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.ChessTypes
       ( -- * Castling
         CastlingRights
       , Castle(..)
       , toCastleList
       , fromCastle
       , intersect
       -- * Utilities
       , pieceValue
       , charToPiece
       , direction
       ) where

import           Data.Char
import           Data.Monoid
import qualified Chess as C


newtype CastlingRights = CastlingRights (Bool, Bool) deriving (Show, Read, Eq, Bounded)

-- we will define our data type for this
instance Read C.Color where
  readsPrec _ s = case s of
    "White"   -> [ (C.White, "") ]
    "Black"   -> [ (C.Black, "") ]
    _         -> []


-- I have tried the tuple-gen package, but that doesn't define the right instance of
-- Enum (a, b).
instance Enum CastlingRights where
  fromEnum (CastlingRights (False, False)) = 0
  fromEnum (CastlingRights (False, True))  = 1
  fromEnum (CastlingRights (True, False))  = 2
  fromEnum (CastlingRights (True, True))   = 3
  toEnum 0 = CastlingRights (False, False)
  toEnum 1 = CastlingRights (False, True)
  toEnum 2 = CastlingRights (True, False)
  toEnum 3 = CastlingRights (True, True)
  toEnum _ = error "Can't convert to CastlingRights"


instance Monoid CastlingRights where
  mempty = CastlingRights (False, False)
  mappend = binFunc (||)


data Castle = Short | Long deriving (Show, Eq)


toCastleList :: CastlingRights -> [ Castle ]
toCastleList (CastlingRights (True, True)) = [ Short, Long ]
toCastleList (CastlingRights (True, False)) = [ Long ]
toCastleList (CastlingRights (False, True)) = [ Short ]
toCastleList (CastlingRights (False, False)) = []


fromCastle :: Castle -> CastlingRights
fromCastle Short = CastlingRights (False, True)
fromCastle Long  = CastlingRights (True, False)


intersect :: CastlingRights -> CastlingRights -> CastlingRights
intersect = binFunc (&&)


binFunc :: (Bool -> Bool -> Bool) -> CastlingRights -> CastlingRights -> CastlingRights
binFunc f (CastlingRights (a, b)) (CastlingRights (c, d)) = CastlingRights (a `f` c, b `f` d)


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
