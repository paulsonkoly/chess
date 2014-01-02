{-# LANGUAGE TemplateHaskell #-}
{- | Transpositional cache

  We return hit if all of the 3 conditions are met:
   1. hash of Board matches
   2. Board equals
   3. depth is equal or less than the one stored in cache

  The are no entries with only the depth differing. Therefore inserting the same
  position with different depth updates the previous entry.
  For more info see http://chessprogramming.wikispaces.com/Transposition+Table.
-}
module Chess.TransPosCache
       ( TransPosCache
       , TransPosCacheEntry
       -- * Constructors
       , mkTransPosCache
       -- * Lenses
--       , board
--       , evaluation
--       , depth
       -- * Utils
       , transPosCacheLookUp
       , transPosCacheInsert
--       , withTransPosCache
       ) where

import Prelude hiding (lookup)

import Control.Lens
import Data.Word       
import Data.Cache.LRU
import Chess.Board

data TransPosCacheEntry = TPCE
                          { _board      :: Board
                          , _evaluation :: ! Int
                          , _depth      :: ! Int
                          }


$(makeLenses ''TransPosCacheEntry)


type TransPosCache = LRU Word64 TransPosCacheEntry


mkTransPosCache :: TransPosCache
mkTransPosCache = newLRU $ Just 8192


-- | Just the pair of the modified LRU cache + the entry on hit
transPosCacheLookUp
  :: Board
  -> Int   -- ^ depth
  -> TransPosCache
  -> Maybe (TransPosCache, Int)
transPosCacheLookUp b d cache = let (cache', mval) = lookup (hash b) cache
                                in case mval of
                                  Just val ->
                                    if b == val^.board && val^.depth >= d
                                    then Just (cache', val^.evaluation)
                                    else Nothing
                                  Nothing  -> Nothing


-- | Returns a cache with the entry inserted
transPosCacheInsert
  :: Board
  -> Int   -- ^ depth
  -> Int   -- ^ evaluation
  -> TransPosCache
  -> TransPosCache
transPosCacheInsert b d e = insert (hash b) (TPCE b e d)
