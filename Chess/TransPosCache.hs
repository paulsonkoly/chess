{-# LANGUAGE TemplateHaskell #-}
{- | Transpositional cache

  We return hit if all of the 3 conditions are met:
   1. hash of Board matches
   2. Board equals
   3. depth is equal or less than the one stored in cache

  The are no entries with only the depth differing. Therefore inserting the same
  position with different depth updates the previous entry. Also EXACT entry wins
  over LOWER or UPPER.
  For more info see http://chessprogramming.wikispaces.com/Transposition+Table.
-}
module Chess.TransPosCache
       ( TransPosCache
       , TransPosCacheEntry
       , TransPosCacheEntryType(..)
       -- * Constructors
       , mkTransPosCache
       -- * Lenses
       , board
       , depth
       , result
       , typ
       -- * Utils
       , transPosCacheLookUp
       , transPosCacheInsert
       ) where

import Prelude hiding (lookup)

import Control.Lens
import Data.Word       
import Data.Cache.LRU
import Chess.Board
import Chess.Move


data TransPosCacheEntryType = Exact | Lower | Upper deriving Eq


data TransPosCacheEntry = TPCE
                          { _board      :: Board
                          , _depth      :: ! Int
                          , _result     :: ! SearchResult
                          , _typ        :: ! TransPosCacheEntryType
                          }


$(makeLenses ''TransPosCacheEntry)


type TransPosCache = LRU Word64 TransPosCacheEntry


mkTransPosCache :: TransPosCache
mkTransPosCache = newLRU $ Just $ 4 * 8192


-- | Just the pair of the modified LRU cache + the entry on hit
transPosCacheLookUp
  :: Board
  -> Int   -- ^ depth
  -> TransPosCache
  -> Maybe (TransPosCache, TransPosCacheEntry)
transPosCacheLookUp b d cache = let (cache', mval) = lookup (b^.hash) cache
                                in case mval of
                                  Just val -> if b == val^.board && val^.depth >= d
                                              then Just (cache', val)
                                              else Nothing
                                  Nothing  -> Nothing


-- | Returns a cache with the entry inserted
transPosCacheInsert
  :: Board                  -- ^ board
  -> Int                    -- ^ depth
  -> TransPosCacheEntryType -- ^ type
  -> SearchResult           -- ^ stored result
  -> TransPosCache
  -> TransPosCache
transPosCacheInsert b d t r cache = let mold = transPosCacheLookUp b d cache
                                    in case mold of
                                      Just (_, old) -> if t == Exact && old^.typ /= Exact
                                                       then insert (b^.hash) (TPCE b d r t) cache
                                                       else cache
                                      Nothing       -> insert (b^.hash) (TPCE b d r t) cache
