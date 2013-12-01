{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.BitBoard
   ( BitBoard
   , toList
   , prettyPrint
   , mul
   , makeMagic
   , masks
   , magics
   , module Data.Bits
   )
   where


import           Data.Word
import           Data.Bits
import           Data.Bits.Extras
import           Data.Monoid
import qualified Data.Vector.Unboxed as V hiding (slice, length)
import qualified Data.Vector.Unboxed.Mutable as V hiding (replicate)
import           Data.Vector.Unboxed.Deriving

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Random
import           Control.Monad.Loops
import           Control.Lens
import qualified Chess as C


import Debug.Trace


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


prettyPrint :: BitBoard -> IO ()
prettyPrint (BitBoard b) = do
   putStrLn "=========="
   forM_ [7, 6 .. 0] $ \rank -> do
      putChar '|'
      forM_ [0 .. 7] $ \file ->
         putChar $ if testBit b (rank * 8 + file) then 'X' else '_'
      putStrLn "|"


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



-- | Sliding attacks, stop at the first occupied position with ray casting
--
-- This can be used for offline Magic Bit Board construction. It should not
-- be used in the engine - unless for testing.
slidingAttackBB
   :: C.PieceType  -- ^ Rook or Bishop
   -> Int          -- ^ position
   -> BitBoard     -- ^ occupancy
   -> BitBoard     -- ^ resulting board
slidingAttackBB pt pos occ = fst $ flip execState (mempty, False) $ do
   let
      file = pos .&. 7
      rank = pos `shiftR` 3
      df   = case pt of
         C.Bishop -> bishopDeltas
         C.Rook   -> rookDeltas
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
         cont <- use _2
         when (pos' >= 0 && pos' <= 63 && accept && cont) $ do
            _1 <>= bit pos'
            when (bit pos' .&. occ /= mempty) $ assign _2 False
   where
      bishopDeltas 0 = 7
      bishopDeltas 1 = -7
      bishopDeltas 2 = 9
      bishopDeltas 3 = -9
      rookDeltas 0 = 8
      rookDeltas 1 = -8
      rookDeltas 2 = -1
      rookDeltas 3 = 1


slidingMaskBB :: C.PieceType -> Int -> BitBoard
slidingMaskBB pt pos =
   let
      rank18   = BitBoard 0xff000000000000ff
      fileAH   = BitBoard 0x8181818181818181
      rankBB p = BitBoard $ 0x00000000000000ff `shift` (p .&. 0x38)
      fileBB p = BitBoard $ 0x0101010101010101 `shift` (p .&. 7)
      edges    = (rank18 .&. complement (rankBB pos)) .|. (fileAH .&. complement (fileBB pos))
   in slidingAttackBB pt pos mempty .&. complement edges


type BBVector = V.Vector BitBoard


data Magic = Magic
   { _masks  :: !BBVector     -- ^ 64 entries of sliding mask on an empty board
   , _shifts :: !(V.Vector Int) -- ^ 64 entries of shifts on the mask bit 
   , _spans  :: !(V.Vector Int) -- ^ 64 entries of the MagicDB span base addresses
   , _magics :: !BBVector     -- ^ 64 entries of magic numbers
   , _dat    :: !BBVector     -- ^ the n entry result data
   }


$(makeLenses ''Magic)


initDB :: C.PieceType -> Magic
initDB pt = Magic
   (V.replicate 64 mempty)
   (V.replicate 64 0)
   (V.replicate 64 0)
   (V.replicate 64 mempty)
   (V.replicate (magicSize pt) mempty)


magicIndex'
   :: Magic    -- ^ magic database
   -> BitBoard -- ^ magic
   -> Int      -- ^ position
   -> BitBoard -- ^ occupancy
   -> Int      -- ^ magic index
magicIndex' m mgc pos occ = ((occ .&. ((m^.masks) V.! pos)) `mul` mgc) `shiftR'` ((m^.shifts) V.! pos)
{-# INLINE magicIndex' #-}


magicIndex
   :: Magic    -- ^ magic database
   -> Int      -- ^ position
   -> BitBoard -- ^ occupancy
   -> Int      -- ^ magic index
magicIndex m pos = magicIndex' m ((m^.magics) V.! pos) pos
{-# INLINE magicIndex #-}


magic
   :: Magic    -- ^ magic database
   -> Int      -- ^ position
   -> BitBoard -- ^ occupancy
   -> BitBoard -- ^ sliding attacks
magic m pos occ = (m^.dat) V.! magicIndex m pos occ
{-# INLINE magic #-}


-- | Generates the nth bitcombination for a specified mask
ripple :: BitBoard -> Int -> BitBoard
ripple _ 0               = BitBoard 0
ripple m@(BitBoard bb) n = let (BitBoard nxt) = ripple m (n - 1)
                           in  BitBoard $ (nxt - bb) .&. bb


-- | Magic^.dat size
magicSize :: C.PieceType -> Int
magicSize pt = sum [ 1 `shiftL` popCount (slidingMaskBB pt pos) | pos <- [0 .. 63]]



-- | Sparse random bitboard
sparseRandomBB :: (MonadRandom m) => m BitBoard
sparseRandomBB = do 
   a <- getRandom
   b <- getRandom
   c <- getRandom
   return $ BitBoard $ a .&. b .&. c
{-   where
      rnd :: (MonadRandom m) => m Word64
      rnd = do
         a <- getRandomR (0              , (1 `shiftL` 16) - 1)
         b <- getRandomR ((1 `shiftL` 16), (1 `shiftL` 32) - 1)
         c <- getRandomR ((1 `shiftL` 32), (1 `shiftL` 48) - 1)
         d <- getRandomR ((1 `shiftL` 48), (1 `shiftL` 64) - 1)
         return $ ((a .|. b .|. c .|. d) :: Word64)
-}


findMagic
   :: (MonadState Magic m, MonadRandom m)
   => BBVector    -- ^ reference vector
   -> BBVector    -- ^ occupancies
   -> BitBoard    -- ^ mask
   -> Int         -- ^ position
   -> Int         -- ^ base
   -> Int         -- ^ size
   -> m Bool
findMagic ref occ mask pos b s = do
   m <- trace ("magic for " ++ (show pos)) get
   iterateUntil id $ do
      dat    %= V.modify (\v -> V.set (V.slice b s v) mempty)

      mgc <- iterateWhile (\r -> popCount (r `mul` mask `shiftR` 56) < 6) sparseRandomBB

      magics %= V.modify (\v -> V.write v pos mgc)

      flip allM [ 0 .. s - 1 ] $ \i -> do
         let
            ref' = ref V.! i
            occ' = occ V.! i
            idx  = magicIndex' m mgc pos occ'
         attack <- liftM ((V.! (b + idx)) . view dat) get
         let res = attack == mempty || ref' == attack
         dat %= V.modify (\v -> V.write v (b + idx) ref')
         return res 


makeMagic' :: (MonadState Magic m, MonadRandom m) => C.PieceType -> m () 
makeMagic' pt =
   forM_ [ 0 .. 63 ] $ \pos -> do
      let 
         mask = slidingMaskBB pt pos
         shft = popCount mask
         size = (1::Int) `shiftL` shft
         occ  = V.generate size $ ripple mask
         ref  = V.map (slidingAttackBB pt pos) occ

      masks  %= V.modify (\v -> V.write v pos mask)
      shifts %= V.modify (\v -> V.write v pos $ 64 - shft)

      base <- do
         base <- liftM ((V.! pos) . (view spans)) get
         when (pos /= 63) $ spans %= V.modify (\v -> V.write v (pos + 1) $ base + size)
         return base
      
      s <- liftM (view spans) get

      findMagic ref occ mask pos base size


makeMagic :: C.PieceType -> Magic
makeMagic pt = evalRand (evalStateT (makeMagic' pt >> get) (initDB pt)) (mkStdGen 0)


{- The magic bit board database

   The database helps quickly determining in run time the sliding pieces
   (Rook, Bishop) attack BitBoards including blocking piece checks. The
   total run time is a 64 entry lookup (per piece type) obtaining the magic
   dataset.
   From there we have the magicNumber which then can be used (involving 64-bit
   multiplication) combined with the current board occupancy BitBoard to obtain
   the magicIndex. This gives the desired BitBoard with the attacked
   positions set (including capturable pieces). The returned position does not
   distinguish between colours, therefore one might need to and the resulting
   BitBoard with the negate of the friendly pieces.

   The per piece span varies on the 64 entry position, for rooks the maximal
   required span address width is 12 for any position. (12 is maximum of
   attackable non-edge squares with a rook). 1 entry is 64 bits, therefore
   the maximal database size is 8 bytes * (2 ^ 12 span width) * 64 entries.
   This is 2Mb however due to the fancy-BitBoard packing (spans are packed
   with no gaps) it is believed that we can go down to ~800Kb.
   For bishops the database is significantly smaller.

   See:
      - the StockFish source code (https://github.com/mcostalba/Stockfish)
      - www.rivalchess.com/magic-bitboards/
      - chessprogramming.wikispaces.com/Magic+Bitboards
 -}

