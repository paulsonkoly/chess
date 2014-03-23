{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.ChessTypes
       ( -- * Castling
         CastlingRights
       , Castle(..)
       , Colour(..)
       , PieceType(..)
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


newtype CastlingRights = CastlingRights (Bool, Bool) deriving (Show, Read, Eq, Bounded)

data Colour = White | Black deriving (Show, Read, Eq, Enum, Bounded)
data PieceType = Queen | Rook | Bishop | Knight | King | Pawn deriving (Show, Eq)


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


pieceValue :: PieceType -> Int
pieceValue Queen  = 90
pieceValue Rook   = 50
pieceValue Bishop = 30
pieceValue Knight = 30
pieceValue Pawn   = 10
pieceValue King   = 0


charToPiece :: Char -> PieceType
charToPiece 'q' = Queen
charToPiece 'r' = Rook
charToPiece 'b' = Bishop
charToPiece 'n' = Knight
charToPiece 'p' = Pawn
charToPiece 'k' = King
charToPiece c   = if c `elem` "QRBNPK" then charToPiece $ toLower c else error "Invalid char in charToPiece"


direction :: Colour -> Int -> Int
direction White = id
direction Black = (* (-1))
{-# INLINE direction #-}
