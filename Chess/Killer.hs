{- | Killer move heuristics

https://chessprogramming.wikispaces.com/Killer+Heuristic.
This is called from the search iteration loop with updates,
where a fail high node would cause the update. The store is
queried before the iteration and moves are rearranged accordingly.
This should be avoided on quiscene searches, as a killer move
usually supposed to be some quiet move.
-}

module Chess.Killer
       ( Killer
       -- * Constructor
       , mkKiller
       -- * Data manipulation
       , insertKiller
       , bulkInsertKiller
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
  :: Int    -- ^ depth
  -> Maybe Move
  -> Killer
  -> Killer
insertKiller d (Just m) (Killer v) = let o = filter (/= m) $ v ! d
                                         t = if length o > 2 then init o else o
                                     in Killer $ v // [ (d, m : t) ]

insertKiller _ Nothing k = k 


bulkInsertKiller :: [ Move ] -> Killer -> Killer
bulkInsertKiller [] k = k
bulkInsertKiller ml@(m:ms) k = bulkInsertKiller ms $ insertKiller (length ml) (Just m) k


-- the new move list with the heuristics applied
killer
  :: Int      -- ^ depth
  -> [ Move ]
  ->  Killer
  -> [ Move ]
killer d ms (Killer v) = let m2 = if d >= 2 then v ! (d - 2) else []
                             ks = v ! d ++ (v ! (d + 2)) ++ m2
                             i = intersect ks ms
                         in i ++ (ms \\ i)
