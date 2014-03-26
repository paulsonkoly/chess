{- | Killer move heuristics

https://chessprogramming.wikispaces.com/Killer+Heuristic.  This is
called from the search iteration loop with updates, where a fail high
node would cause the update. The store is queried before the iteration
and moves are rearranged accordingly.  This should be avoided on
quiscene searches, as a killer move usually supposed to be some quiet
move.

Now it's also used to store PV between different depth of iterative
depening.
-}

module Chess.Killer
       ( Killer
       -- * Constructor
       , mkKiller
       -- * Data manipulation
       , insertKiller
       , insertPVInKiller
       -- * Query
       , killer
       ) where

import qualified Data.Vector as V
import           Data.Vector ((!), (//))
import           Data.List (intersect, (\\))
import           Chess.Move

newtype Killer = Killer (V.Vector [ Move ])


mkKiller :: Killer
mkKiller = Killer $ V.replicate 30 []


insertKiller
  :: Int    -- ^ distance from start depth ( or in other words real depth )
  -> Maybe Move
  -> Killer
  -> Killer
insertKiller d (Just m) (Killer v) = let o = filter (/= m) $ v ! d
                                         t = if length o > 2 then init o else o
                                     in Killer $ v // [ (d, m : t) ]
insertKiller _ Nothing k = k 


-- | killer store from the PV
insertPVInKiller :: [ Move ] -> Killer
insertPVInKiller ml = let Killer k = mkKiller
                      in Killer $ k // zipWith (\a b -> (a, [b])) [0, 1 ..] ml


-- the new move list with the heuristics applied
killer
  :: Int      -- ^ distance from start depth ( or in other words real depth )
  -> [ Move ] -- ^ previous move list
  ->  Killer
  -> [ Move ]
killer d ms (Killer v) = let ixs = filter (>= 0) [ d, d + 2, d - 2 ]
                             m   = concatMap (v !) ixs
                             i   = intersect m ms
                         in  i ++ (ms \\ i)
