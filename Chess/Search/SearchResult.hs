{-# LANGUAGE TemplateHaskell #-}

module Chess.Search.SearchResult
       ( SearchResult(..)
       , (<@>)
       , (<++>)
       , first
       , eval
       , moves
       , renderVariation
       ) where

import           Control.Lens
import           Data.Maybe

import qualified Chess.Move as M

-- | Represents the result of a Search
data SearchResult = SearchResult
                    { _moves :: ! [ M.Move ]
                    , _eval  :: ! Int
                    }


$(makeLenses ''SearchResult)

-- | modifies eval in a SearchResult
(<@>) :: (Int -> Int) -> SearchResult -> SearchResult
f <@> sr = (eval %~ f) sr

-- | Prepends a Move to a SearchResult
(<++>) :: M.Move -> SearchResult -> SearchResult
m <++> sr = (moves %~ (m:)) sr


-- | The first Move of a SearchResult
first :: SearchResult -> Maybe M.Move
first = listToMaybe . view moves


-- | UCI string for a SearchResult
renderVariation :: SearchResult -> String
renderVariation sr = show (sr^.eval) ++ " " ++ unwords (map M.renderShortMove (sr^.moves))
