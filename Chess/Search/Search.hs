{-# LANGUAGE MultiParamTypeClasses #-}
{- | The monadic interface to the Search algorithm -}
module Chess.Search.Search
  ( Search
  , runSearch
  ) where

import Control.Monad.State
import Chess.Search.SearchState


-- | Type representing a Search action
newtype Search a = Search { runSearch' :: StateT SearchState IO a }

-- | runs a search
runSearch :: Search a -> SearchState -> IO (a, SearchState)
runSearch = runStateT . runSearch'


instance Monad Search where
  return = Search . return
  (Search a) >>= f = Search $ a >>= runSearch' . f


instance MonadIO Search where
  liftIO = Search . liftIO


instance MonadState SearchState Search where
  get = Search get
  put = Search . put
