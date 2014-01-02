module Chess.Evaluation
       ( evaluate
       ) where

import           Control.Lens
import           Data.Bits
import           Data.Foldable
import qualified Data.Sequence as S

import           Data.BitBoard
import           Chess.Board
import           Chess.Move
import qualified Chess as C

import Debug.Trace



weights    = [ (90, C.Queen), (50, C.Rook), (30, C.Bishop), (30, C.Knight), (10, C.Pawn)  ]
diffBB a b = popCount a - popCount b


evaluate :: Board -> Int
evaluate b =
  if checkMate b
  then direction (b^.next) 3000
  else let material = Data.Foldable.sum [ w * (numberOf b C.White pt - numberOf b C.Black pt)
                                        | (w, pt) <- weights
                                        ]
--           mobility = direction (b^.next) $ S.length $ moves b
           cPawns   = diffBB ((piecesOf b C.White C.Pawn) .&. centralSquares)
                             ((piecesOf b C.Black C.Pawn) .&. centralSquares)
           lcPawns  = diffBB ((piecesOf b C.White C.Pawn) .&. largeCentralSquares)
                             ((piecesOf b C.Black C.Pawn) .&. largeCentralSquares)
       in material + 2 * cPawns + lcPawns -- material + mobility + 2 * cPawns + lcPawns
