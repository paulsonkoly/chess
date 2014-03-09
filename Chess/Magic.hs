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
       , magic
       -- * properties
       , prop_slidingAttack
       )
       where

import           Control.Monad.State
import           Control.Monad.Loops
import           Control.Monad.Random
import           Control.Lens
import           Data.Monoid
import qualified Test.QuickCheck as Q hiding ((.&.))

import           Data.BitBoard
import qualified Data.Bits as B (bit)
import           Data.Square
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

import qualified Chess as C

type BBVector = V.Vector BitBoard
type IVector  = V.Vector Int


-- | The magic database ( for a Bishop or a Rook )
data Magic = Magic
   { _masks  :: !BBVector       -- ^ 64 entries of sliding mask on an empty board
   , _shifts :: !IVector        -- ^ 64 entries of shifts on the mask bit 
   , _spans  :: !IVector        -- ^ 64 entries of the MagicDB span base addresses
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
   -> Square       -- ^ position
   -> BitBoard     -- ^ occupancy
   -> BitBoard     -- ^ resulting board
slidingAttackBB pt pos occ = fst $ flip execState (mempty, False) $ do
   let
      df   = deltas pt
   forM_ [ 0 .. 3 ] $ \d -> do
      assign _2 True
      forM_ [ 1 .. 7 ] $ \off -> do
         let
            pos'  = pos `offset` (off * df d)
            hdist = hDist pos pos'
            vdist = vDist pos pos'
            accept = case pt of
               C.Bishop -> hdist == vdist
               C.Rook   -> (hdist == 0 && vdist == off) || (vdist == 0 && hdist == off)
               _        -> undefined
         cont <- use _2
         when (fromEnum pos' >= 0 && fromEnum pos' <= 63 && accept && cont) $ do
            _1 <>= fromSquare pos'
            when (fromSquare pos' .&. occ /= mempty) $ assign _2 False
   where
      deltas :: C.PieceType -> Int -> Int
      deltas C.Bishop 0 = 7
      deltas C.Bishop 1 = -7
      deltas C.Bishop 2 = 9
      deltas C.Bishop 3 = -9
      deltas C.Rook   0 = 8
      deltas C.Rook   1 = -8
      deltas C.Rook   2 = -1
      deltas C.Rook   3 = 1
      deltas _        _ = undefined


slidingMaskBB :: C.PieceType -> Square -> BitBoard
slidingMaskBB pt pos =
   let
      rank18   = rankBB firstRank .|. rankBB eighthRank
      fileAH   = fileBB aFile .|. fileBB hFile
      edges    = (rank18 .&. complement (rankBB $ rank pos)) .|. (fileAH .&. complement (fileBB $ file pos))
   in slidingAttackBB pt pos mempty .&. complement edges



initDB :: Magic
initDB = Magic V.empty V.empty V.empty V.empty V.empty


magicIndex
  :: BitBoard -- ^ mask
  -> BitBoard -- ^ magic
  -> Int      -- ^ shift
  -> Int      -- ^ span/base
  -> BitBoard -- ^ occupancy
  -> Int      -- ^ magic index
magicIndex msk mgc shft spn occ = spn + toInt (((msk .&. occ) `mul` mgc) `shiftR` shft)
{-# INLINE magicIndex #-}


bishopMagics :: Magic
bishopMagics = makeMagic C.Bishop
rookMagics :: Magic
rookMagics   = makeMagic C.Rook


magic :: C.PieceType -> Square -> BitBoard -> BitBoard
magic C.Rook  sq occ  = magic' rookMagics sq occ
magic C.Bishop sq occ = magic' bishopMagics sq occ
magic C.Queen sq occ  = magic' rookMagics sq occ .|. magic' bishopMagics sq occ
{-# INLINE magic #-}


-- | the sliding attack
magic'
   :: Magic
   -> Square   -- ^ position
   -> BitBoard -- ^ occupancy
   -> BitBoard -- ^ sliding attacks
magic' m pos occ = let msk  = (m^.masks) `V.unsafeIndex` fromEnum pos
                       mgc  = (m^.magics) `V.unsafeIndex` fromEnum pos
                       shft = (m^.shifts) `V.unsafeIndex` fromEnum pos
                       spn  = (m^.spans) `V.unsafeIndex` fromEnum pos
                    in (m^.dat) `V.unsafeIndex` magicIndex msk mgc shft spn occ
{-# INLINE magic' #-}


-- | The dat size
magicSize :: C.PieceType -> Int
magicSize pt = sum [ B.bit $ popCount (slidingMaskBB pt pos) | pos <- squares ]


-- | Calculates masks
calcMasks :: C.PieceType -> BBVector
calcMasks pt = V.fromList $ map (slidingMaskBB pt) squares


-- | Calculates shifts from the masks
calcShifts :: BBVector -> IVector
calcShifts = V.map (\i -> 64 - popCount i)


-- | Calculates spans from the shifts
calcSpans :: IVector -> IVector
calcSpans = V.scanl' (+) 0 . V.map (B.bit . (64 -))


-- | Calculates dats from masks, shifts, spans and magics
calcDat
  :: C.PieceType -- ^ piece type
  -> BBVector    -- ^ masks
  -> IVector     -- ^ shifts
  -> IVector     -- ^ spans
  -> BBVector    -- ^ magics
  -> BBVector
calcDat pt msks shfts spns mgs = V.create $ do
   v <- VM.new $ magicSize pt
   V.forM_ (V.enumFromN 0 64) $ \pos -> do
     let base = V.unsafeIndex spns pos
         shft = V.unsafeIndex shfts pos
         mgc  = V.unsafeIndex mgs pos
         mask = V.unsafeIndex msks pos
         occ  = V.fromList $ ripple mask
     V.forM_ occ $ \occ' -> do
        let
           ref = slidingAttackBB pt (toEnum pos) occ'
           idx = magicIndex mask mgc shft base occ'
        VM.unsafeWrite v idx ref
   return v


lookForMagic :: C.PieceType -> BBVector -> IVector -> BBVector
lookForMagic pt msks shfts = V.create $ do
  v <- VM.new 64
  V.forM_ (V.enumFromN 0 64) $ \pos -> do
    let occ   = V.fromList $! ripple mask
        ref   = V.map (slidingAttackBB pt $ toEnum pos) $! occ
        mask  = msks V.! pos
        shft  = shfts V.! pos
        dsize = B.bit $ 64 - shft
    d <- VM.new dsize
    search (mkStdGen 0) mask pos dsize occ ref shft v d
  return v
  where
    search gen mask pos dsize occ ref shft v d = do
      (mgc, gen') <- flip runRandT gen $ iterateWhile (\r -> popCount (r `mul` mask `shiftR` 56) < 6) sparseRandomBB
      VM.write v pos mgc
      VM.set d mempty

      found <- flip allM [ 0 .. dsize - 1 ] $ \i -> do
        let occ' = occ V.! i
            ref' = ref V.! i             
            idx  = magicIndex mask mgc shft 0 occ'
            
        attack <- VM.read d idx
        let res = attack == mempty || ref' == attack
        when res $ VM.write d idx ref'
        return res
      unless found $ search gen' mask pos dsize occ ref shft v d

      
generateMagic :: C.PieceType -> BBVector
generateMagic pt = let msks  = calcMasks pt
                       shfts = calcShifts msks
                   in lookForMagic pt msks shfts


makeMagic :: C.PieceType -> Magic
makeMagic pt = execState (makeMagic' >> get) initDB
  where
    makeMagic' :: (MonadState Magic m) => m () 
    makeMagic' = do
      mgs   <- magics <.= preMagics pt
      msks  <- masks  <.= calcMasks pt
      shfts <- shifts <.= calcShifts msks
      spns  <- spans  <.= calcSpans shfts
      dat  .= calcDat pt msks shfts spns mgs


prop_slidingAttack :: BitBoard -> Square -> Q.Property
prop_slidingAttack b sq = Q.forAll (Q.elements [ C.Bishop, C.Rook ])
                          $ \pt -> slidingAttackBB pt sq b == magic pt sq b

