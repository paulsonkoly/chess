{-# LANGUAGE TemplateHaskell #-}

module Chess.Search.SearchState
       ( mkSearchState
       , SearchState
       -- * Lenses
       , board   
       , tpc     
       , kill    
       , pv      
       , tpcHit  
       , tpcMiss 
       , nCnt
       ) where

import           Chess.Board
import qualified Chess.Killer as K
import qualified Chess.PVStore as PVS
import qualified Chess.TransPosCache as TPC
import           Control.Lens

data SearchState = SearchState
                   { _board    :: Board
                   , _tpc      :: TPC.TransPosCache
                   , _kill     :: K.Killer
                   , _pv       :: PVS.PVStore
                   , _tpcHit   :: ! Int
                   , _tpcMiss  :: ! Int
                   , _nCnt     :: ! Int                     
                   }


-- | Creates a search state with the initialBoard. Use the board Lens to manipulate the position in the SearchState
mkSearchState :: SearchState
mkSearchState = SearchState initialBoard TPC.mkTransPosCache K.mkKiller PVS.mkPVStore 0 0 0


$(makeLenses ''SearchState)
