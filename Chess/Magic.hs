{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}

{- | The magic bit board database

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

     * the StockFish source code <https://github.com/mcostalba/Stockfish>

     * <http://www.rivalchess.com/magic-bitboards>

     * <http://chessprogramming.wikispaces.com/Magic+Bitboards>
 -}

module Chess.Magic
   ( Magic
   , makeMagic
   , magic
   )
   where

import           Control.Monad.State
import           Control.Monad.Loops
import           Control.Monad.Random
import           Control.Lens

import           Data.BitBoard
import qualified Data.Vector.Unboxed as V hiding (slice, length, (!), unsafeSlice)
import qualified Data.Vector.Unboxed.Mutable as V hiding (replicate, slice)

import qualified Chess as C


type BBVector = V.Vector BitBoard


-- | The magic database ( for a Bishop or a Rook )
data Magic = Magic
   { _masks  :: !BBVector       -- ^ 64 entries of sliding mask on an empty board
   , _shifts :: !(V.Vector Int) -- ^ 64 entries of shifts on the mask bit 
   , _spans  :: !(V.Vector Int) -- ^ 64 entries of the MagicDB span base addresses
   , _magics :: !BBVector       -- ^ 64 entries of magic numbers
   , _dat    :: !BBVector       -- ^ the n entry result data
   }


$(makeLenses ''Magic)


-- | Sliding attacks, stop at the first occupied position with ray casting
--
-- This can be used for offline Magic Bit Board construction. It should not
-- be used in the engine - unless for testing. Therefore it's not exported.
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
      rank18   = rankBB 0 .|. rankBB 63
      fileAH   = fileBB 0 .|. fileBB 7
      edges    = (rank18 .&. complement (rankBB pos)) .|. (fileAH .&. complement (fileBB pos))
   in slidingAttackBB pt pos mempty .&. complement edges



initDB :: C.PieceType -> Magic
initDB pt = Magic
   (V.replicate 64 mempty)
   (V.replicate 64 0)
   (V.replicate 64 0)
   (V.replicate 64 mempty)
   (V.replicate (magicSize pt) mempty)


magicIndex
   :: Magic    -- ^ magic database
   -> Int      -- ^ position
   -> BitBoard -- ^ occupancy
   -> Int      -- ^ magic index
magicIndex m pos occ =
   let
      a = (m^.masks)  `V.unsafeIndex` pos
      b = (m^.magics) `V.unsafeIndex` pos
      c = (m^.shifts) `V.unsafeIndex` pos
   in ((a .&. occ) `mul` b) `shiftR'` c
{-# INLINE magicIndex #-}


-- | the sliding attack
magic
   :: Magic    -- ^ magic database
   -> Int      -- ^ position
   -> BitBoard -- ^ occupancy
   -> BitBoard -- ^ sliding attacks
magic m pos occ = (m^.dat) `V.unsafeIndex` (((m^.spans) `V.unsafeIndex` pos) + magicIndex m pos occ)
{-# INLINE magic #-}


-- | Magic^.dat size
magicSize :: C.PieceType -> Int
magicSize pt = sum [ 1 `shiftL` popCount (slidingMaskBB pt pos) | pos <- [0 .. 63]]


makeMagic :: C.PieceType -> Magic
makeMagic pt = evalRand (evalStateT (makeMagic' pt >> get) (initDB pt)) (mkStdGen 0)
   where
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
               base <- liftM ((`V.unsafeIndex` pos) . view spans) get
               when (pos /= 63) $ spans %= V.modify (\v -> V.write v (pos + 1) $ base + size)
               return base
            
            s <- liftM (view spans) get

            findMagic ref occ mask pos base size

      findMagic
         :: (MonadState Magic m, MonadRandom m)
         => BBVector    -- ^ reference vector
         -> BBVector    -- ^ occupancies
         -> BitBoard    -- ^ mask
         -> Int         -- ^ position
         -> Int         -- ^ base
         -> Int         -- ^ size
         -> m Bool
      findMagic ref occ mask pos b s = iterateUntil id $ do

         dat %= V.modify (\v -> V.set (V.unsafeSlice b s v) mempty)

         mgc <- iterateWhile (\r -> popCount (r `mul` mask `shiftR` 56) < 6) sparseRandomBB
         magics %= V.modify (\v -> V.write v pos mgc)

         flip allM [ 0 .. s - 1 ] $ \i -> do
            let
               ref' = ref `V.unsafeIndex` i
               occ' = occ `V.unsafeIndex` i
            idx    <- liftM (\m -> magicIndex m pos occ') get
            attack <- liftM ((`V.unsafeIndex` (b + idx)) . view dat) get
            let res = attack == mempty || ref' == attack
            dat %= V.modify (\v -> V.write v (b + idx) ref')
            return res
