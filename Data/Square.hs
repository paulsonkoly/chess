module Data.Square
       ( -- * Types
         Square
       , File
       , Rank
       -- * All elements of a given type domain
       , squares
       , files
       , ranks
       -- Files
       , aFile, bFile, cFile, dFile, eFile, fFile, gFile, hFile
       -- * Ranks
       , firstRank, secondRank, thirdRank, fourthRank, fifthRank, sixthRank, seventhRank, eighthRank
       -- * Conversions
       , file
       , rank
       , toSquare
       -- * Distances
       , hDiff
       , hDist
       , vDiff
       , vDist
       , offset
       , mirror
       , parserSquare
       ) where

import           Data.Char
import           Data.Bits
import           Text.ParserCombinators.Parsec

newtype Square = Square Int deriving (Eq)


instance Bounded Square where
  minBound = Square 0
  maxBound = Square 63


instance Enum Square where
  toEnum i
    | 0 <= i && i <= 63 = Square i
    | otherwise         = error "Square out of range"
  fromEnum (Square i)   = i


instance Show Square where
  show s = [ 'a' .. 'h' ] !! fromEnum (file s) : [ [ '1' .. '8' ] !! fromEnum (rank s) ]


-- This is a bit of hacky Read instance..
instance Read Square where
  readsPrec _ s = case parse parserSquare "" s of
    Left _  -> []
    Right sq -> [ ( sq, "") ]


hDiff :: Square -> Square -> Int
hDiff a b = abs $ fromEnum (rank a) - fromEnum (rank b)


hDist :: Square -> Square -> Int
hDist a b = abs $ hDiff a b


vDiff :: Square -> Square -> Int
vDiff a b = abs $ fromEnum (file a) - fromEnum (file b)


vDist :: Square -> Square -> Int
vDist a b = abs $ vDiff a b


offset :: Square -> Int -> Square
offset (Square a) b = Square $ a + b


mirror :: Square -> Square
mirror sq = toSquare (file sq) (m $ rank sq)
  where m (Rank a) = Rank $ 7- a


newtype File = File Int deriving (Eq)


instance Bounded File where
  minBound = File 0
  maxBound = File 7


instance Enum File where
  toEnum i
    | 0 <= i && i <= 7 = File i
    | otherwise        = error "File out of range"
  fromEnum (File i)    = i


aFile, bFile, cFile, dFile, eFile, fFile, gFile, hFile :: File
aFile = File 0
bFile = File 1
cFile = File 2
dFile = File 3
eFile = File 4
fFile = File 5
gFile = File 6
hFile = File 7


newtype Rank = Rank Int deriving (Eq)


instance Bounded Rank where
  minBound = Rank 0
  maxBound = Rank 7


instance Enum Rank where
  toEnum i
    | 0 <= i && i <= 7 = Rank i
    | otherwise        = error "Rank out of range"
  fromEnum (Rank i)    = i


firstRank, secondRank, thirdRank, fourthRank, fifthRank, sixthRank, seventhRank, eighthRank :: Rank
firstRank   = Rank 0
secondRank  = Rank 1
thirdRank   = Rank 2
fourthRank  = Rank 3
fifthRank   = Rank 4
sixthRank   = Rank 5
seventhRank = Rank 6
eighthRank  = Rank 7


squares :: [ Square ]
squares = [ minBound .. maxBound ]


files :: [ File ]
files = [ minBound .. maxBound ]


ranks :: [ Rank ]
ranks = [ minBound .. maxBound ]


file :: Square -> File
file (Square sq) = File $ sq .&. 7


rank :: Square -> Rank
rank (Square sq) = Rank $ sq `shiftR` 3


toSquare :: File -> Rank -> Square
toSquare (File f) (Rank r) = Square $ r `shiftL` 3 + f


parserSquare :: Parser Square
parserSquare = do
  f <- oneOf ['a' .. 'h']
  r <- oneOf ['1' .. '8']
  return $ Square $ 8 * (digitToInt r - 1) + (ord f - ord 'a')
