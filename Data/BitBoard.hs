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

import           Control.Monad
import           Control.Monad.State
import           Control.Lens
import qualified Chess as C


newtype BitBoard = BitBoard Int64 deriving Show


prettyPrint :: BitBoard -> IO ()
prettyPrint (BitBoard b) = do
   putStrLn "=========="
   forM_ [7, 6 .. 0] $ \rank -> do
      putChar '|'
      forM_ [0 .. 7] $ \file ->
         putChar $ if testBit b (rank * 8 + file) then 'X' else '_'
      putStrLn "|"


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
{-# INLINE shiftBB #-}


toList :: BitBoard -> [ Int ]
toList (BitBoard b) = if b == 0
   then []
   else
      let bp = fromIntegral $ trailingZeros b
      in  bp : toList (BitBoard $ b `xor` bit bp)


knightFiles :: Int -> BitBoard
knightFiles 0 = BitBoard 0x0606060606060606
knightFiles 1 = BitBoard 0x0d0d0d0d0d0d0d0d
knightFiles 2 = BitBoard 0x1b1b1b1b1b1b1b1b
knightFiles 3 = BitBoard 0x3636363636363636
knightFiles 4 = BitBoard 0x6c6c6c6c6c6c6c6c
knightFiles 5 = BitBoard 0xd8d8d8d8d8d8d8d8
knightFiles 6 = BitBoard 0xb0b0b0b0b0b0b0b0
knightFiles 7 = BitBoard 0x6060606060606060
knightFiles _ = error "oops : knightFiles"


knightAttackBB :: Int -> BitBoard
knightAttackBB 28  = BitBoard 44272527353856
knightAttackBB pos = knightFiles (pos .&. 7) `andBB` shiftBB (pos - 28) (knightAttackBB 28)
{-# INLINE knightAttackBB #-}



-- | Sliding attacks, stop at the first occupied position with ray casting
--
-- This can be used for offline Magic Bit Board construction. It should not
-- be used in the engine - unless for testing.
slidingAttackBB
   :: C.PieceType  -- ^ Rook or Bishop 
   -> Int          -- ^ position
   -> BitBoard     -- ^ occupancy
   -> BitBoard     -- ^ resulting board
slidingAttackBB pt pos (BitBoard occ) = fst $ flip execState (mempty, False) $ do
   let
      file = pos .&. 7
      rank = pos `shiftR` 3
      df   = case pt of
         C.Bishop -> bishopDeltas
         C.Rook   -> rookDeltas
         _        -> error "oops : slidingAttackBB"
   forM_ [ 0 .. 3 ] $ \d -> do
      assign _2 True
      forM_ [ 1 .. 7 ] $ \off -> do
         let
            pos'  = pos + off * df d
            hdist = abs $ file - (pos' .&. 7)
            vdist = abs $ rank - (pos' `shiftR` 3)
            accept = case pt of
               C.Bishop -> hdist == vdist
               C.Rook   -> (hdist == 0 && vdist == off) || (vdist == 0 && hdist == off)
               _        -> False
         cont <- use _2
         when (pos' >= 0 && pos' <= 63 && accept && cont) $ do
            _1 <>= setBB pos'
            when (occ .&. bit pos' /= 0) $ assign _2 False
   where
      bishopDeltas 0 = 7
      bishopDeltas 1 = -7
      bishopDeltas 2 = 9
      bishopDeltas 3 = -9
      rookDeltas 0 = 8
      rookDeltas 1 = -8
      rookDeltas 2 = -1
      rookDeltas 3 = 1


bishopAttackBB
   :: Int      -- ^ position
   -> BitBoard -- ^ occupancy
   -> BitBoard -- ^ resulting board
bishopAttackBB = slidingAttackBB C.Bishop


rookAttackBB
   :: Int      -- ^ position
   -> BitBoard -- ^ occupancy
   -> BitBoard -- ^ resulting board
rookAttackBB = slidingAttackBB C.Rook


