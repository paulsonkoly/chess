module Chess.Move
   ( checkMate
   , staleMate
   , module M
   )
   where

import           Control.Lens

import           Chess.Move.Move as M
import           Chess.Move.Attacks as M
import           Chess.Move.ExecMove as M
import           Chess.Move.GenMoves as M

import           Chess.Board


checkMate :: Board -> Bool
checkMate b = inCheck b (b^.next) && null (moves b)


staleMate :: Board -> Bool
staleMate b = not (inCheck b (b^.next)) && null (moves b)

