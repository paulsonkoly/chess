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
--       , transPosCacheDeflate
       ) where

import Chess.Board
import Chess.Move
import Chess.Search.SearchResult
import Control.Lens
import Data.Cache.LRU
import Data.Word
import Prelude                   hiding (lookup)


data TransPosCacheEntryType = Exact | Lower | Upper deriving (Eq, Show)


data TransPosCacheEntry = TPCE
                          { _board      :: Board
                          , _depth      :: ! Int
                          , _result     :: ! SearchResult
                          , _typ        :: ! TransPosCacheEntryType
                          }


$(makeLenses ''TransPosCacheEntry)


type TransPosCache = LRU Word64 TransPosCacheEntry


lruSize :: Maybe Integer
lruSize = Just $ 32 * 8192


mkTransPosCache :: TransPosCache
mkTransPosCache = newLRU lruSize


-- | Either a move recommendation or Nothing on miss or the cache entry with the updated LRU on hit
transPosCacheLookUp
  :: Board
  -> Int   -- ^ depth
  -> TransPosCache
  -> Either (Maybe Move) (TransPosCache, TransPosCacheEntry)
transPosCacheLookUp b d cache = let (cache', mval) = lookup (b^.hash) cache
                                in case mval of
                                  Just val -> if b == val^.board
                                              then if val^.depth >= d
                                                   then Right (cache', val)
                                                   else Left $ first $ val^.result
                                              else Left Nothing
                                  Nothing  -> Left Nothing


-- | Returns a cache with the entry inserted
transPosCacheInsert
  :: Board                  -- ^ board
  -> Int                    -- ^ depth
  -> TransPosCacheEntryType -- ^ type
  -> SearchResult           -- ^ stored result
  -> TransPosCache
  -> TransPosCache
transPosCacheInsert b d t r cache = let eold = transPosCacheLookUp b d cache
                                    in case eold of
                                      Right _ -> cache
                                      Left  _ -> insert (b^.hash) (TPCE b d r t) cache


-- | decreases the depth of each entry by 1
-- transPosCacheDeflate :: TransPosCache -> TransPosCache
-- transPosCacheDeflate = fromList lruSize . (map  (_2 . depth %~ pred)) . toList
