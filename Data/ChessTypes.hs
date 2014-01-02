module Data.ChessTypes
       ( Castle(..)
       ) where

data Castle = Short | Long deriving (Show, Eq, Enum)
