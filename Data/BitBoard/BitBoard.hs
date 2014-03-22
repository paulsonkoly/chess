{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.BitBoard.BitBoard
   ( BitBoard(..)
   -- * Square access
   , toList
   , fromList
   , fromSquare
   -- * Utilities
   , prettyPrint
   -- * Numeric operations
   , mul
   , ripple
   , toInteger
   , module Data.Bits
   )
   where

import           Prelude hiding (toInteger)
import           Data.Word
import           Data.Bits
import           Data.Bits.Extras
import           Data.Monoid

import           Data.Vector.Unboxed.Deriving
import           Test.QuickCheck hiding ((.&.))
import           Control.Monad
import           Data.Square

newtype BitBoard = BitBoard Word64 deriving (Show, Read, Eq, Bits)


{- | The BitBoard forms a Monoid where mempty is the empty set
     and mappend is the union of two BitBoards. This makes it
     convinient to accumlate bits with combinators from other
     libraries.
-}
instance Monoid BitBoard where
   mempty  = BitBoard 0
   {-# INLINE mempty #-}
   mappend (BitBoard a) (BitBoard b) = BitBoard $ a .|. b
   {-# INLINE mappend #-}

derivingUnbox "BitBoard" [t| BitBoard -> Word64 |] [|(\(BitBoard b) -> b)|] [|BitBoard|]


fromSquare :: Square -> BitBoard
fromSquare = BitBoard . bit . fromEnum
{-# INLINE fromSquare #-}


-- | mulitplication needed in magic index calculations
mul :: BitBoard -> BitBoard -> BitBoard
(BitBoard a) `mul` (BitBoard b) = BitBoard $ a * b
{-# INLINE mul #-}


toInteger :: BitBoard -> Integer
toInteger (BitBoard a) = fromIntegral a
{-# INLINE toInteger #-}


-- | Generates the bitcombinations for a specified mask
ripple :: BitBoard -> [ BitBoard ]
ripple m@(BitBoard b) =
   let l = iterate (\(BitBoard a) -> BitBoard $ (a - b) .&. b) m
   in  m : takeWhile (/= m) (tail l)
{-# INLINE ripple #-}


-- | pretty prints a BitBoard
prettyPrint :: BitBoard -> IO ()
prettyPrint b = do
   putStrLn "=========="
   forM_ (reverse ranks) $ \r -> do
      putChar '|'
      forM_ files $ \f ->
         putChar $ if testBit b (fromEnum $ toSquare f r) then 'X' else '_'
      putStrLn "|"


-- | list of Indices set in a BitBoard
toList :: BitBoard -> [ Square ]
toList (BitBoard b) = if b == 0
   then []
   else
      let bp = fromIntegral $ trailingZeros b
      in  toEnum bp : toList (BitBoard $ b `xor` bit bp)
{-# INLINE toList #-}


fromList :: [ Square ] -> BitBoard
fromList = foldr1 (<>) . map (bit . fromEnum)
{-# INLINE fromList #-}


instance Arbitrary BitBoard where
  -- for more complexity we gradually add more bits
  arbitrary = do
    sqs <- listOf arbitrary
    return $ mconcat $ map fromSquare sqs

  -- Take a single square out
  shrink b = [ b .&. complement (fromSquare p) | p <- toList b ]








