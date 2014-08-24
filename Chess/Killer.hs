-- | Killer move heuristics
--
-- https://chessprogramming.wikispaces.com/Killer+Heuristic.  This is called
-- from the search iteration loop with updates, where a fail high node would
-- cause the update. The store is queried before the iteration and moves are
-- rearranged accordingly.
module Chess.Killer
       ( Killer
       -- * Constructor
       , mkKiller
       -- * Data manipulation
       , insert
       , clearLevel
       -- * Query
       , heuristics
       ) where

------------------------------------------------------------------------------
import           Data.Function (on)
import           Data.List (findIndex, insertBy)

import           Data.Vector ((!), (//))
import qualified Data.Vector as V

import           Chess.Move
import           Data.List.Extras


------------------------------------------------------------------------------
-- The maximum depth we can handle
maxKillerSize :: Int
maxKillerSize = 30


------------------------------------------------------------------------------
-- The maximum entries per level
maxLevelEntries :: Int
maxLevelEntries = 2


------------------------------------------------------------------------------
-- | at a given depth entries are sorted by hit count in reverse order
newtype Killer = Killer (V.Vector [ (Int, Move) ])


------------------------------------------------------------------------------
-- | Creates a killer store
mkKiller :: Killer
mkKiller = Killer $ V.replicate maxKillerSize []


------------------------------------------------------------------------------
-- | inserts a Move to the store
insert
  :: Int   -- ^ distance from start depth ( or in other words real depth )
  -> Move  -- ^ move to insert
  -> Killer
  -> Killer
insert d m k@(Killer v)
  | d >= maxKillerSize = k
  | otherwise          =
    let e   = v ! d
        mix = findIndex ((== m) . snd) e
        nv  = case mix of
          -- e contains m therefore suf /= []
          Just ix -> let (pref, suf) = splitAt ix e
                         (oCnt, _)   = head suf
                     in pref
                        ++ insertBy (compare `on` fst) (oCnt + 1, m) (tail suf)
          Nothing -> (1, m) : if length e >= maxLevelEntries
                              then tail e
                              else e
    in Killer $ v // [ (d, nv) ]


------------------------------------------------------------------------------
-- | Clears the specified depth of the store
clearLevel :: Int -> Killer -> Killer
clearLevel d k@(Killer v)
  | d >= maxKillerSize = k
  | otherwise          = Killer $ v // [ (d, []) ]


------------------------------------------------------------------------------
-- | the new move list with the heuristics applied
heuristics
  :: Int      -- ^ distance from start depth ( or in other words real depth )
  -> [ PseudoLegalMove ] -- ^ previous move list
  ->  Killer
  -> [ PseudoLegalMove ]
heuristics d ms (Killer v) =
  let m   = map (mkPseudo . snd) $ v ! d
      -- the reverse order of the entries nicely matches toFront
      ms' = foldl (flip toFront) ms m
  in if d >= maxKillerSize then ms else ms'
