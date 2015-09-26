{-# LANGUAGE MultiParamTypeClasses #-}
-- | The monadic interface to the Search algorithm
module Chess.Search.Search
  ( Search
  , runSearch
  , report
  , abortable
  , isPondering
  ) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.State

import Control.Lens (use)

import Chess.Search.SearchState


------------------------------------------------------------------------------
-- | Type representing a Search action
newtype Search a = Search { runSearch' :: StateT SearchState IO a }


------------------------------------------------------------------------------
-- | runs a search
runSearch :: Search a -> SearchState -> IO (a, SearchState)
runSearch = runStateT . runSearch'


                                --------------
                                -- Intances --
                                --------------

instance Monad Search where
  return = Search . return
  (Search a) >>= f = Search $ a >>= runSearch' . f


instance Functor Search where
  fmap = liftM


instance Applicative Search where
  pure  = return
  (<*>) = ap


instance MonadIO Search where
  liftIO = Search . liftIO


instance MonadState SearchState Search where
  get = Search get
  put = Search . put


------------------------------------------------------------------------------
-- | reports a string to the UCI interface
report :: String -> Search ()
report = liftIO . putStrLn . ("info " ++)


------------------------------------------------------------------------------
-- | Aborts the search
abortable :: Search (Maybe a) -> Search (Maybe a)
abortable f = do
  abortedtv <- use aborted
  abort     <- liftIO $ readTVarIO abortedtv
  if abort then return Nothing else f


------------------------------------------------------------------------------
-- | Is the search running in pondering mode
--
-- Note, that the interface can asynchronously transition a pondering search
-- to normal search on ponderhit.
isPondering :: Search Bool
isPondering = use pondering >>= liftIO . readTVarIO
