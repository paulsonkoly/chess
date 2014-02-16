{-# LANGUAGE TemplateHaskell #-}

module Chess.SearchResult
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

data SearchResult = SearchResult
                    { _moves :: ! [ M.Move ]
                    , _eval  :: ! Int
                    }


$(makeLenses ''SearchResult)


(<@>) :: (Int -> Int) -> SearchResult -> SearchResult
f <@> sr = (eval %~ f) sr

(<++>) :: M.Move -> SearchResult -> SearchResult
m <++> sr = (moves %~ (m:)) sr

first :: SearchResult -> Maybe M.Move
first = listToMaybe . view moves

renderVariation :: SearchResult -> String
renderVariation sr = show (sr^.eval) ++ " " ++ unwords (map M.renderShortMove (sr^.moves))
