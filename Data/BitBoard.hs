{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.BitBoard
   ( BitBoard
   -- * Utilities
   , toList
   , prettyPrint
   -- * Numeric operations
   , mul
   , shiftR'
   , ripple
   -- * Various BitBoard values
   , mempty
   , knightAttackBB
   , rankBB
   , fileBB
   , sparseRandomBB
   , module Data.Bits
   )
   where


import           Data.Word
import           Data.Bits
import           Data.Bits.Extras
import           Data.Monoid
import           Data.Vector.Unboxed.Deriving

import           Control.Monad
import           Control.Monad.Random


newtype BitBoard = BitBoard Word64 deriving (Show, Eq, Bits)


{- | The BitBoard forms a Monoid where mempty is the empty set
     and mappend is the union of two BitBoards. This makes it
     convinient to accumlate bits with combinators from other
     libraries.
-}
instance Monoid BitBoard where
   mempty  = BitBoard 0
   mappend (BitBoard a) (BitBoard b) = BitBoard $ a .|. b


derivingUnbox "BitBoard" [t| BitBoard -> Word64 |] [|(\(BitBoard b) -> b)|] [|BitBoard|]


-- | mulitplication needed in magic index calculations
mul :: BitBoard -> BitBoard -> BitBoard
(BitBoard a) `mul` (BitBoard b) = BitBoard $ a * b
{-# INLINE mul #-}


-- | shift needed in magic index calculations
shiftR' :: BitBoard -> Int -> Int
shiftR' (BitBoard b) s = fromIntegral $ b `shiftR` s
{-# INLINE shiftR' #-}


-- | Generates the nth bitcombination for a specified mask
ripple :: BitBoard -> Int -> BitBoard
ripple _ 0               = mempty
ripple m@(BitBoard bb) n = let (BitBoard nxt) = ripple m (n - 1)
                           in  BitBoard $ (nxt - bb) .&. bb
{-# INLINE ripple #-}


-- | pretty prints a BitBoard
prettyPrint :: BitBoard -> IO ()
prettyPrint (BitBoard b) = do
   putStrLn "=========="
   forM_ [7, 6 .. 0] $ \rank -> do
      putChar '|'
      forM_ [0 .. 7] $ \file ->
         putChar $ if testBit b (rank * 8 + file) then 'X' else '_'
      putStrLn "|"


-- | list of Indices set in a BitBoard
--
-- TODO : this should return list of squares
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


knightAttackBB :: Int -> BitBoard
knightAttackBB 28  = BitBoard 44272527353856
knightAttackBB pos = knightFiles (pos .&. 7) .&. shift (knightAttackBB 28) (pos - 28)
{-# INLINE knightAttackBB #-}


-- | BitBoard with the specified rank (any square of the Rank) set
rankBB :: Int -> BitBoard
rankBB p = BitBoard $ 0x00000000000000ff `shift` (p .&. 0x38)
{-# INLINE rankBB #-}


-- | BitBoard with the specified file (any square of the File) set
fileBB :: Int -> BitBoard
fileBB p = BitBoard $ 0x0101010101010101 `shift` (p .&. 7)
{-# INLINE fileBB #-}


-- | Sparse random bitboard
sparseRandomBB :: (MonadRandom m) => m BitBoard
sparseRandomBB = do 
   a <- getRandom
   b <- getRandom
   c <- getRandom
   return $ BitBoard $ a .&. b .&. c
{-# INLINE sparseRandomBB #-}



