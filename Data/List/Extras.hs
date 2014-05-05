{- | Some additional list functions we miss from Data.List -}
module Data.List.Extras
       ( toFront
       , safeTail
       ) where


import Data.List


-- | moves an element to the front of the list if it's in the list
-- if the elem is in the list multiple times, only moves the first
-- occurance
toFront :: (Eq a) => a -> [a] -> [a]
toFront a as = let mix = elemIndex a as
               in case mix of
                 Just ix -> let (f, s) = splitAt ix as
                            in a : f ++ tail s
                 Nothing -> as
{-# INLINE toFront #-}


-- | same as tail except safeTail [] = []
safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:as) = as
{-# INLINE safeTail #-}
