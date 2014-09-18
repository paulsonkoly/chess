{- | Some additional list functions we miss from Data.List -}
module Data.List.Extras
       ( toFront
       , findLastIndex
       ) where


import Data.List


------------------------------------------------------------------------------
-- | moves an element to the front of the list if it's in the list if the elem
-- is in the list multiple times, only moves the first occurance
toFront :: (Eq a) => a -> [a] -> [a]
toFront a as = let mix = elemIndex a as
               in case mix of
                 Just ix -> let (f, s) = splitAt ix as
                            in a : f ++ tail s
                 Nothing -> as
{-# INLINE toFront #-}


------------------------------------------------------------------------------
-- | the last index satisfying the predicate. If no element satisfies the 
-- predicate then Nothing.
findLastIndex :: (a -> Bool) -> [a] -> Maybe Int
findLastIndex p = loop Nothing 0
  where
    loop x _ [] = x
    loop r n (x:xs) | p x       = loop (Just n) (n + 1) xs
                    | otherwise = loop r (n + 1) xs
{-# INLINE findLastIndex #-}
