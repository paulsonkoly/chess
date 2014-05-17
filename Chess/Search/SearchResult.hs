{-# LANGUAGE TemplateHaskell #-}

module Chess.Search.SearchResult
       ( SearchResult(..)
       , (<@>)
       , (<++>)
       , first
       , second
       , eval
       , moves
       ) where

import           Control.Lens
import           Control.Monad
import           Data.Maybe

import qualified Chess.Move as M

-- | Represents the result of a Search
data SearchResult = SearchResult
                    { _moves :: ! [ M.Move ]
                    , _eval  :: ! Int
                    }


$(makeLenses ''SearchResult)


-- | modifies eval in a SearchResult, in a monadic computation
(<@>) :: (Monad m) => (Int -> Int) -> m (Maybe SearchResult) -> m (Maybe SearchResult)
f <@> m = liftM ((_Just . eval) %~ f) m


-- | Prepends a Move to a SearchResult
(<++>) :: Monad m => M.Move -> m (Maybe SearchResult) -> m (Maybe SearchResult)
x <++> m = liftM (_Just . moves %~ (x:)) m


-- | The first Move of a SearchResult
first :: SearchResult -> Maybe M.Move
first = listToMaybe . view moves


-- | The second Move of a SearchResult
second :: SearchResult -> Maybe M.Move
second sr = case sr^.moves of
  _:m:_ -> Just m
  _     -> Nothing

