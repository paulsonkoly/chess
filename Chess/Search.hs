{-# LANGUAGE TemplateHaskell #-}

module Chess.Search
       ( GameTree
       -- * Constructor
       , initialGameTree
       , gameTreeFromBoard
       -- * Utils
       , getPath
       , board
       , module Data.Tree.Game_tree.Negascout
       ) where

import           Control.Lens
import           Control.Monad.State

import           Data.Maybe
import           Data.Bits
import           Data.Tree.Game_tree.Game_tree
import           Data.Tree.Game_tree.Negascout
import           Data.Foldable
-- import qualified Data.Cache.LRU as Cache

import qualified Chess as C
import           Chess.Board
import           Chess.Move


data GameTree = GameTree
  { _board        :: ! Board
  , _path         :: ! (Maybe Move)
  , _moves'       :: ! [ Move ]
  , _checkMate'   :: ! Bool
  }


$(makeLenses ''GameTree)


evaluate :: GameTree -> Int
evaluate gt = if gt^.checkMate'
              then direction (gt^.board^.next) 3000
              else let b = gt^.board
                       material = Data.Foldable.sum [ w * (numberOf b C.White pt - numberOf b C.Black pt)
                                                    | (w, pt) <- [ (90, C.Queen), (50, C.Rook), (30, C.Bishop), (30, C.Knight), (10, C.Pawn) ]
                                                    ]
                       mobility = direction (gt^.board^.next) $ length $ gt^.moves'
                       centralSquares = (bit 27) .|. (bit 28) .|. (bit 35) .|. (bit 36)
                       cPawns = popCount ((piecesOf b C.White C.Pawn) .&. centralSquares) - popCount ((piecesOf b C.Black C.Pawn) .&. centralSquares)
                   in material + mobility + cPawns
                                  

initialGameTree :: GameTree
initialGameTree = let ms  = toList $ moves initialBoard
                  in GameTree initialBoard Nothing ms False


gameTreeFromBoard :: Board -> GameTree -> GameTree
gameTreeFromBoard b gt = (board .~ b)
                         $ (checkMate' .~ checkMate b)
                         $ (moves' .~ toList (moves b)) gt

                
instance Game_tree GameTree where
  is_terminal   = view checkMate' -- checkMate (gt^.bishopMagics) (gt^.rookMagics) (gt^.board)
  node_value    = evaluate
  children gt   = do
    m <- gt^.moves'
    let child = execState (doMoveM m) (gt^.board)
    return $ (path .~ Just m) $ gameTreeFromBoard child gt


getPath :: [ GameTree ] -> [ Move ]
getPath = map fromJust . dropWhile isNothing . map (view path)
