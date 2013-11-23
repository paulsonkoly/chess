module Data.BitBoard
   ( BitBoard(..)
   , setBB
   , andBB
   , orBB
   )
   where


import           Data.Int
import           Data.Bits
import           Data.Monoid


newtype BitBoard = BitBoard Int64


instance Show BitBoard where
   show (BitBoard b) = do
      file <- [ 0 .. 7 ]
      rank <- [ 0 .. 7 ]
      if testBit b (rank * 8 + file) then "X" else "_"


instance Monoid BitBoard where
   mempty  = BitBoard 0
   mappend (BitBoard a) (BitBoard b) = BitBoard $ a .|. b 


setBB :: Int -> Int -> BitBoard
setBB a b = BitBoard $ bit $ a * 8 + b

andBB, orBB :: BitBoard -> BitBoard -> BitBoard

orBB                            = mappend
andBB (BitBoard a) (BitBoard b) = BitBoard $ a .&. b
