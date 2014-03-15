module Chess.Move
   ( checkMate
   , staleMate
   , module M
   -- * properties
   , prop_MoveBoardAfter
   , prop_numberOfPieces
   , prop_numberOfKings
   , prop_zobrist
   )
   where

import           Control.Lens

import           Chess.Move.Move as M
import           Chess.Move.Execute as M
import           Chess.Move.Generator as M

import           Chess.Board
import           Data.BitBoard


checkMate :: Board -> Bool
checkMate b = inCheck b (b^.next) && not (anyMove b)


staleMate :: Board -> Bool
staleMate b = not (inCheck b (b^.next)) && not (anyMove b)


prop_MoveBoardAfter :: Board -> Bool
prop_MoveBoardAfter b = all prop_Board $ map (`makeMove` b) (moves b)


prop_numberOfPieces :: Board -> Bool
prop_numberOfPieces b = let o = popCount $ occupancy b
                            n = [ popCount $ occupancy $ makeMove m b | m <- moves b ]
                        in all (\x -> x == o || x == o - 1) n


prop_numberOfKings :: Board -> Bool
prop_numberOfKings b = all prop_BoardKingNum [ makeMove m b | m <- moves b ]


prop_zobrist :: Board -> Bool
prop_zobrist b = let b' = (hash .~ calcHash b) b
                     bs = [ makeMove m b' | m <- moves b']
                 in all (\b'' -> calcHash b'' == b''^.hash) bs
