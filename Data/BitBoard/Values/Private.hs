module Data.BitBoard.Values.Private
       ( rankBB
       , fileBB
       , neighbourRanksBB'
       , neighbourFilesBB'
       , aheadBB'
       , knightAttackBB'
       , kingAttackBB'
       , pawnAttackBB'
       ) where

import           Data.Monoid
import           Data.BitBoard.BitBoard
import           Data.Square
import           Data.ChessTypes

import qualified Chess as C


fileBB :: File -> BitBoard
fileBB p = BitBoard $ 0x0101010101010101 `shift` fromEnum p
{-# INLINE fileBB #-}


rankBB :: Rank -> BitBoard
rankBB r = BitBoard $ 0x00000000000000ff `shift` (8 * fromEnum r)
{-# INLINE rankBB #-}


neighbourRanksBB' :: Rank -> BitBoard
neighbourRanksBB' r = BitBoard (0x000000ffffffffff `shift` (8 * (fromEnum r - 2)))


neighbourFilesBB' :: File -> BitBoard
neighbourFilesBB' = neighbourFilesBB'' . fromEnum
  where
    neighbourFilesBB'' 0 = BitBoard 0x0707070707070707
    neighbourFilesBB'' 1 = BitBoard 0x0f0f0f0f0f0f0f0f
    neighbourFilesBB'' 2 = BitBoard 0x1f1f1f1f1f1f1f1f
    neighbourFilesBB'' 3 = BitBoard 0x3e3e3e3e3e3e3e3e
    neighbourFilesBB'' 4 = BitBoard 0x7c7c7c7c7c7c7c7c
    neighbourFilesBB'' 5 = BitBoard 0xf8f8f8f8f8f8f8f8
    neighbourFilesBB'' 6 = BitBoard 0xf0f0f0f0f0f0f0f0
    neighbourFilesBB'' 7 = BitBoard 0xe0e0e0e0e0e0e0e0
    neighbourFilesBB''  _ = error "file out of range"


aheadBB' :: Rank -> C.Color -> BitBoard
aheadBB' r C.White = mconcat [ rankBB r' | r' <- [r .. eighthRank] ]
aheadBB' r C.Black = mconcat [ rankBB r' | r' <- [firstRank .. r] ]


e4:: Square
e4 = toSquare eFile fourthRank


knightAttackBB' :: Square -> BitBoard
knightAttackBB' s
  | s ==  e4  = BitBoard 44272527353856
  | otherwise = neighbourFilesBB' (file s) .&. shift (knightAttackBB' e4) (fromEnum s - fromEnum e4)


kingAttackBB' :: Square -> BitBoard
kingAttackBB' s
  | s == e4   = BitBoard 241192927232
  | otherwise = neighbourFilesBB' (file s) .&. shift (kingAttackBB' e4) (fromEnum s - fromEnum e4)


pawnAttackBB' :: Square -> C.Color -> BitBoard
pawnAttackBB' pos c = let mask  = neighbourFilesBB' (file pos)
                      in mconcat [ fromSquare (offset pos $ direction c n) | n <- [7, 9] ] .&. mask
