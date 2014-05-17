{-# LANGUAGE TemplateHaskell #-}

module Chess.Search.SearchState
       ( mkSearchState
       , SearchState
       , Previous(..)
         -- * Lenses
       , board
       , aborted
       , previous
       , depth
       , result
       , tpc
       , kill
       , pv
       , tpcHit
       , tpcMiss
       , nCnt
       ) where

import Chess.Board hiding (hash)
import Chess.Killer
import Chess.PVStore
import Chess.TransPosCache (TransPosCache, mkTransPosCache)
import Chess.Search.SearchResult
import Control.Lens
import Control.Concurrent.STM


data Previous = Previous
                { _depth  :: ! Int          -- ^ the depth that we have successfully searched
                , _result :: SearchResult   -- ^ the last successfull search
                }


$(makeLenses ''Previous)


data SearchState = SearchState
                   { _board    :: Board
                   , _aborted  :: TVar Bool
                   , _previous :: Maybe Previous
                   , _tpc      :: TransPosCache
                   , _kill     :: Killer
                   , _pv       :: PVStore
                   , _tpcHit   :: ! Int
                   , _tpcMiss  :: ! Int
                   , _nCnt     :: ! Int
                   }


-- | Creates a search state with the initialBoard. Use the board Lens to manipulate the position in the SearchState
mkSearchState :: TVar Bool -> SearchState
mkSearchState a = SearchState initialBoard a Nothing mkTransPosCache mkKiller mkPVStore 0 0 0


$(makeLenses ''SearchState)
