module Data.Square
       ( -- * Types
         Square
       , File
       , Rank
       -- * All elements of a given type domain
       , squares
       , files
       , ranks
       -- * Files
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
       -- * utilities
       , mirror
       , parserSquare
       ) where

import           Data.Char
import           Data.Bits
import           Text.ParserCombinators.Parsec
import           Test.QuickCheck hiding ((.&.))


-- | The square represents one of the squares of a chess board
newtype Square = Square Int deriving (Eq)


instance Bounded Square where
  minBound = Square 0
  {-# INLINE minBound #-}
  maxBound = Square 63
  {-# INLINE maxBound #-}


instance Enum Square where
  toEnum i
    | 0 <= i && i <= 63 = Square i
    | otherwise         = error "Square out of range"
  {-# INLINE toEnum #-}
  fromEnum (Square i)   = i
  {-# INLINE fromEnum #-}


-- | like 'a1' or 'h8'
instance Show Square where
  show s = [ 'a' .. 'h' ] !! fromEnum (file s) : [ [ '1' .. '8' ] !! fromEnum (rank s) ]
  {-# INLINE show #-}


-- | Opposite of Show ie parses 'a1' into a Square
instance Read Square where
  readsPrec _ s = case parse parserSquare "" s of
    Left _  -> []
    Right sq -> [ ( sq, "") ]


-- | Arbitrary but no shrink
instance Arbitrary Square where
  arbitrary = arbitraryBoundedEnum
  {-# INLINE arbitrary #-}


-- | Horizonatal difference (can be negative)
hDiff :: Square -> Square -> Int
hDiff a b = fromEnum (file a) - fromEnum (file b)
{-# INLINE hDiff #-}


-- | Horizonatal distance
hDist :: Square -> Square -> Int
hDist a b = abs $ hDiff a b
{-# INLINE hDist #-}


-- | Vertical difference (can be negative)
vDiff :: Square -> Square -> Int
vDiff a b = fromEnum (rank a) - fromEnum (rank b)
{-# INLINE vDiff #-}


-- | Vertical distance
vDist :: Square -> Square -> Int
vDist a b = abs $ vDiff a b
{-# INLINE vDist #-}


-- | Offset a square by an Int (in the enumeration of 'a1' .. 'h1' .. 'a8' .. 'h8'
offset :: Square -> Int -> Square
offset (Square a) b = Square $ a + b
{-# INLINE offset #-}


-- | Horizontal miror. Mirrors on the line between 4th and 5th ranks
mirror :: Square -> Square
mirror sq = toSquare (file sq) (m $ rank sq)
  where m (Rank a) = Rank $ 7- a
{-# INLINE mirror #-}


-- | A File on a Board
newtype File = File Int deriving (Eq, Ord)


instance Bounded File where
  minBound = File 0
  {-# INLINE minBound #-}
  maxBound = File 7
  {-# INLINE maxBound #-}


instance Enum File where
  toEnum i
    | 0 <= i && i <= 7 = File i
    | otherwise        = error "File out of range"
  {-# INLINE toEnum #-}
  fromEnum (File i)    = i
  {-# INLINE fromEnum #-}


aFile, bFile, cFile, dFile, eFile, fFile, gFile, hFile :: File
aFile = File 0
bFile = File 1
cFile = File 2
dFile = File 3
eFile = File 4
fFile = File 5
gFile = File 6
hFile = File 7
{-# INLINE aFile #-}
{-# INLINE bFile #-}
{-# INLINE cFile #-}
{-# INLINE dFile #-}
{-# INLINE eFile #-}
{-# INLINE fFile #-}
{-# INLINE gFile #-}
{-# INLINE hFile #-}


-- | A rank on a chess board
newtype Rank = Rank Int deriving (Eq, Ord)


instance Bounded Rank where
  minBound = Rank 0
  {-# INLINE minBound #-}
  maxBound = Rank 7
  {-# INLINE maxBound #-}


instance Enum Rank where
  toEnum i
    | 0 <= i && i <= 7 = Rank i
    | otherwise        = error "Rank out of range"
  {-# INLINE toEnum #-}
  fromEnum (Rank i)    = i
  {-# INLINE fromEnum #-}


firstRank, secondRank, thirdRank, fourthRank, fifthRank, sixthRank, seventhRank, eighthRank :: Rank
firstRank   = Rank 0
secondRank  = Rank 1
thirdRank   = Rank 2
fourthRank  = Rank 3
fifthRank   = Rank 4
sixthRank   = Rank 5
seventhRank = Rank 6
eighthRank  = Rank 7
{-# INLINE firstRank   #-}
{-# INLINE secondRank  #-}
{-# INLINE thirdRank   #-}
{-# INLINE fourthRank  #-}
{-# INLINE fifthRank   #-}
{-# INLINE sixthRank   #-}
{-# INLINE seventhRank #-}
{-# INLINE eighthRank  #-}


squares :: [ Square ]
squares = [ minBound .. maxBound ]
{-# INLINE squares #-}


files :: [ File ]
files = [ minBound .. maxBound ]
{-# INLINE files #-}


ranks :: [ Rank ]
ranks = [ minBound .. maxBound ]
{-# INLINE ranks #-}


-- | Extracts the file from a square
file :: Square -> File
file (Square sq) = File $ sq .&. 7
{-# INLINE file #-}


-- | Extracts the Rank from a square
rank :: Square -> Rank
rank (Square sq) = Rank $ sq `shiftR` 3
{-# INLINE rank #-}


-- | Constructs a square from a file and a rank
toSquare :: File -> Rank -> Square
toSquare (File f) (Rank r) = Square $ r `shiftL` 3 + f
{-# INLINE toSquare #-}


-- | The parser for 'a1' etc notation
parserSquare :: Parser Square
parserSquare = do
  f <- oneOf ['a' .. 'h']
  r <- oneOf ['1' .. '8']
  return $ Square $ 8 * (digitToInt r - 1) + (ord f - ord 'a')
{-# INLINE parserSquare #-}
