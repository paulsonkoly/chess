module Data.BitBoard
       ( module P
       ) where

-- hiding the Bits instance functions that should not be used
-- so we can force the usage of functions that operate on Squares
-- instead of Ints, but it's still convinient to use and, or, xor etc
-- from the Bits instance
import Data.BitBoard.BitBoard as P hiding (isSigned, clearBit, complementBit, setBit, bit)
