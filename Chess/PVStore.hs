-- | Simple module that stores the PV between iterative deepening iterations
module Chess.PVStore
       ( PVStore
       -- * Constructor
       , mkPVStore
       -- * Manipulation
       , insert
       , clearUpTo
       -- * Query
       , heuristics
       ) where

import qualified Data.Vector as V
import           Data.Vector ((!))
import           Data.List hiding (insert)
import           Chess.Move


newtype PVStore = PVStore (V.Vector Move)


-- | The empty PVStore
mkPVStore :: PVStore
mkPVStore = PVStore V.empty


-- | store from the PV
insert :: [ Move ] -> PVStore
insert = PVStore . V.fromList


-- | truncate to given length
clearUpTo :: Int -> PVStore -> PVStore
clearUpTo d (PVStore s) = PVStore $ (V.take $ d - 1) s


-- the new move list with the heuristics applied
heuristics
  :: Int      -- ^ distance from start depth ( or in other words real depth )
  -> [ Move ] -- ^ previous move list
  -> PVStore
  -> [ Move ]
heuristics d ml (PVStore v)
  | V.length v <= d = ml
  | otherwise     = let m  = v ! d
                        mm = elemIndex m ml
                    in case mm of
                      Just ix -> let (f, s) = splitAt ix ml
                                 in m : f ++ tail s
                      Nothing -> ml
