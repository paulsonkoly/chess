{-# LANGUAGE TemplateHaskell #-}

module Chess.Search.SearchState
       ( mkSearchState
       , SearchState
         -- * Lenses
       , board
       , aborted
       , pondering
       , timeStats
       , tpc
       , kill
       , pv
       , hist
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
import Chess.History
import Chess.Killer
import Chess.PVStore
import Chess.TimeControl
import Chess.TransPosCache (TransPosCache, mkTransPosCache)


------------------------------------------------------------------------------
-- | The state of the 'Search' monad
data SearchState = SearchState
                   { _board     :: Board
                   , _aborted   :: TVar Bool
                   , _pondering :: TVar Bool
                   , _timeStats :: TimeStats
                   , _tpc       :: TransPosCache
                   , _kill      :: Killer
                   , _pv        :: PVStore
                   , _hist      :: History
                   , _tpcHit    :: ! Int
                   , _tpcMiss   :: ! Int
                   , _nCnt      :: ! Int
                   , _clock     :: Maybe UTCTime
                   }


------------------------------------------------------------------------------
-- | Creates a search state with the initialBoard. Use the board Lens to
-- manipulate the position in the SearchState
--
-- The TVar parameters can be changed asynchronously while the 'Search' is
-- running. The ponder should be set to false by the interface in response to
-- ponderhit. The abort should be raised to kill a running 'Search' and should
-- be polled for the bit to be dropped as a signal back mechanism that the
-- abort succeeded.
mkSearchState
  :: TVar Bool -- ^ the signal to toggle to abort a search
  -> TVar Bool -- ^ the signal to toggle pondering
  -> SearchState
mkSearchState abort ponder =
  SearchState
   initialBoard
   abort
   ponder
   mkTimeStats
   mkTransPosCache
   mkKiller
   mkPVStore
   mkHistory
   0 0 0
   Nothing


$(makeLenses ''SearchState)
