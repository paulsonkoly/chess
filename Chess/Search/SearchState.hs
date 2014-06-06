{-# LANGUAGE TemplateHaskell #-}

module Chess.Search.SearchState
       ( mkSearchState
       , SearchState
         -- * Lenses
       , board
       , aborted
       , maxDepth
       , tpc
       , kill
       , pv
       , tpcHit
       , tpcMiss
       , nCnt
       , clock
       ) where

------------------------------------------------------------------------------

import Control.Concurrent.STM
import Control.Lens
import Data.Time.Clock

import Chess.Board hiding (hash)
import Chess.Killer
import Chess.PVStore
import Chess.TransPosCache (TransPosCache, mkTransPosCache)


------------------------------------------------------------------------------
-- | The state of the 'Search' monad
data SearchState = SearchState
                   { _board    :: Board
                   , _aborted  :: TVar Bool
                   , _maxDepth :: TVar Int                     
                   , _tpc      :: TransPosCache
                   , _kill     :: Killer
                   , _pv       :: PVStore
                   , _tpcHit   :: ! Int
                   , _tpcMiss  :: ! Int
                   , _nCnt     :: ! Int
                   , _clock    :: Maybe UTCTime
                   }


------------------------------------------------------------------------------
-- | Creates a search state with the initialBoard. Use the board Lens to
-- manipulate the position in the SearchState
--
-- The TVar parameters can be changed asynchronously while the 'Search' is
-- running. Transition from pondering to normal search can be achieved with
-- reducing the maximum depth from 'maxBound' to some reasonably small
-- value. The abort should be raised to kill a running 'Search' and should be
-- polled for the bit to be dropped as a signal back mechanism that the abort
-- succeeded.
mkSearchState
  :: TVar Bool -- ^ the signal to toggle to abort a search
  -> TVar Int  -- ^ the maximum depth the search traverses
  -> SearchState
mkSearchState abort maxDepth =
  SearchState
   initialBoard
   abort
   maxDepth
   mkTransPosCache
   mkKiller
   mkPVStore
   0 0 0
   Nothing


$(makeLenses ''SearchState)
