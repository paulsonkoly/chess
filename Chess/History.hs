-- | Relative history heuristics
--
-- This is a relative history heuristics with butterfly penalties. The normal
-- history heuristics are described here: 
-- <https://chessprogramming.wikispaces.com/History+Heuristic>. The relative
-- penalty improvement can be found here:
-- <https://chessprogramming.wikispaces.com/Relative+History+Heuristic>.
module Chess.History
       ( History
       -- * Constructor
       , mkHistory
       -- * Manipulation
       , insert
       -- * Query
       , heuristics
       ) where

import           Data.Bits (shiftL)
import           Data.List (sortBy)
import           Data.Maybe (isJust)
import           Data.Ord
import           Data.Ratio

import           Control.Lens ((&), (+~), (^.), _1, _2)
import qualified Data.Vector as V

import           Chess.Move
import           Data.List.Extras
import           Data.Square


------------------------------------------------------------------------------
-- | History store
newtype History = History (V.Vector (Int, Int))


------------------------------------------------------------------------------
-- | The empty Store
mkHistory :: History
mkHistory =
  let mx1 = 1 + fromEnum (maxBound :: Square)
      mx2 = 1 + fromEnum (maxBound :: Square)
  in History $ V.replicate (mx1 * mx2) (0, 1)


------------------------------------------------------------------------------
-- | Insert move at given depth (should be called on the beta cut). Also
-- updates the buterfly penalties on the moves in the given move list up to
-- the move in the list.
insert :: Int -> Move -> [ PseudoLegalMove ] -> History -> History
insert d m ml (History v) =
  let m'  = mkPseudo m
      ml' = filter (isJust . capturedPiece') $ takeWhile (/= m') ml
  in History $ v `V.unsafeUpd` (updater _2 m' ++ concatMap (updater _1) ml')
  where
    updater l m' = let idx = vidx $ m'
                       p   = v `V.unsafeIndex` idx
                   in if isJust (m^.capturedPiece)
                      then [ (idx, p & l +~ (1 `shiftL` d)) ]
                      else []
                          

------------------------------------------------------------------------------
-- | the new move list with the heuristics applied
heuristics
  :: [ PseudoLegalMove ] -- ^ previous move list
  -> History
  -> [ PseudoLegalMove ]
heuristics ml (History v) = 
  let midx         = findLastIndex (isJust . capturedPiece') ml
      value (a, b) = a % b
      sorting      =
        sortBy (comparing (Down . value . (V.unsafeIndex v) . vidx))
  in case midx of
      Just idx ->
        let (pref, suff) = splitAt (idx + 1) ml
            sorted       = sorting suff
        in pref ++ sorted
      Nothing  -> sorting ml


------------------------------------------------------------------------------
-- store indexing
vidx :: PseudoLegalMove -> Int
vidx m =
  fromEnum (from' m) * fromEnum (maxBound :: Square) + fromEnum (to' m)
