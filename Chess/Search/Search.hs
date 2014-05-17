{-# LANGUAGE MultiParamTypeClasses #-}
{- | The monadic interface to the Search algorithm -}
module Chess.Search.Search
  ( Search
  , runSearch
  , report
  , abortable
  ) where

import Control.Monad.State
import Chess.Search.SearchState
import Control.Lens (use)
import Control.Concurrent.STM (readTVarIO)


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


report :: String -> Search ()
report = liftIO . putStrLn . ("info " ++)



abortable :: Search (Maybe a) -> Search (Maybe a)
abortable f = do
  abortedtv <- use aborted
  abort     <- liftIO $ readTVarIO abortedtv
  if abort then return Nothing else f
