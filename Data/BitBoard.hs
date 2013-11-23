module Data.BitBoard
   ( BitBoard(..)
   , setBB
   , andBB
   , orBB
   , xorBB
   , shiftBB
   , toList
   )
   where


import           Data.Int
import           Data.Bits
import           Data.Bits.Extras
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


setBB :: Int -> BitBoard
setBB n = BitBoard $ bit n 

andBB, orBB, xorBB :: BitBoard -> BitBoard -> BitBoard

orBB                            = mappend
andBB (BitBoard a) (BitBoard b) = BitBoard $ a .&. b
xorBB (BitBoard a) (BitBoard b) = BitBoard $ a `xor` b


shiftBB :: Int -> BitBoard -> BitBoard
shiftBB i (BitBoard b) = BitBoard $ shift b i


toList :: BitBoard -> [ Int ]
toList (BitBoard b) = if b == 0
   then []
   else
      let bp = fromIntegral $ trailingZeros b
      in  bp : toList (BitBoard $ b `xor` bit bp)
