module Data.BitBoard.Values.Private
       ( rankBB
       , fileBB
       , neighbourRanksBB'
       , neighbourFilesBB'
       , aheadBB'
       , knightAttackBB'
       , kingAttackBB'
       , pawnAttackBB'
       , lineBB'
       , pseudoAttackBB'
       ) where


import           Data.Monoid
import           Data.BitBoard.BitBoard
import           Data.Square
import           Data.ChessTypes


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


aheadBB' :: Rank -> Colour -> BitBoard
aheadBB' r White = mconcat [ rankBB r' | r' <- [r .. eighthRank] ]
aheadBB' r Black = mconcat [ rankBB r' | r' <- [firstRank .. r] ]


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


pawnAttackBB' :: Square -> Colour -> BitBoard
pawnAttackBB' pos c = let mask  = neighbourFilesBB' (file pos)
                      in mconcat [ fromSquare (offset pos $ direction c n) | n <- [7, 9] ] .&. mask


lineBB' :: Square -> Square -> BitBoard
lineBB' a b
  | hDist a b == 0 = mconcat [ fromSquare $ toSquare (file a) r | r <- rankList]
  | vDist a b == 0 = mconcat [ fromSquare $ toSquare f (rank a) | f <- fileList]
  | hDist a b == vDist a b = mconcat $ map fromSquare $ zipWith toSquare fileList rankList
  | otherwise              = mempty
  where fileList = if file a < file b
                   then [ file a .. file b ]
                   else reverse [ file b .. file a ]
        rankList = if rank a < rank b
                   then [ rank a .. rank b ]
                   else reverse [ rank b .. rank a ]


pseudoAttackBB' :: PieceType -> Square -> BitBoard
pseudoAttackBB' Bishop sq = mconcat $ do
  flist <- [ reverse [ aFile .. file sq], [ file sq .. hFile ] ]
  rlist <- [ reverse [ firstRank .. rank sq ], [ rank sq .. eighthRank ] ]
  (f, r) <- zip flist rlist
  return $ fromSquare $ toSquare f r
pseudoAttackBB' Rook sq  = fileBB (file sq) <> rankBB (rank sq)
pseudoAttackBB' Queen sq = pseudoAttackBB' Bishop sq <> pseudoAttackBB' Rook sq
pseudoAttackBB' King _   = mempty
pseudoAttackBB' Knight _ = mempty
pseudoAttackBB' Pawn _   = mempty
              
