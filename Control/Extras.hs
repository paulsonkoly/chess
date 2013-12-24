module Control.Extras
       ( doOnJust
       ) where

-- | Executes a monadic action on a Just value.
doOnJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
doOnJust = flip (maybe (return ()))
{-# INLINE doOnJust #-}
