module Chess.Evaluation
       ( evaluate
       ) where

import           Prelude hiding (concat, sum, elem)

import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.Foldable
import           Data.Monoid

import           Data.BitBoard hiding (toList)
import qualified Data.BitBoard as BB (toList)
import           Data.Square
import           Data.ChessTypes
import           Chess.Board
import           Chess.Move
import           Chess.Magic
import qualified Chess as C


weights    = [ (90, C.Queen), (50, C.Rook), (30, C.Bishop), (30, C.Knight), (10, C.Pawn)  ]
diffBB a b = popCount a - popCount b


evaluate :: Board -> Int
evaluate b
  | checkMate b = direction (b^.next) (-3000)
  | staleMate b = 0
  | otherwise   = let material = sum [ w * (numberOf b C.White pt - numberOf b C.Black pt)
                                     | (w, pt) <- weights
                                     ]
                  in material + sum [ f b C.White - f b C.Black
                                    | f <- [ evaluateRookPosition
--                                           , evaluateKingSafety
--                                         , evaluatePins
                                           , evaluateBishopPosition
                                           , evaluateKnightPosition
                                           , evaluatePawnPosition
                                           ]
                                    ]


{-evaluateKingSafety :: Board -> C.Color -> Int
evaluateKingSafety b col = let kingSq = head $ BB.toList $ piecesOf b col C.King
                               kfQ    = kingSq .&. 4
                               krQ    = kingSq .&. 32
                               quadrant  = mconcat [fileBB i | i <- [kfQ .. kfQ + 3]] .&. mconcat [rankBB i | i <- [krQ .. krQ + 3]]
                               neighbour = kingAttackBB kingSq .|. piecesOf b col C.King
                               attackingKnights = popCount $ quadrant .&. piecesOf b (opponent' col) C.Knight
                               defendingPawns   = popCount $ neighbour .&. piecesOf b col C.Pawn
                               noMyPieces       = removePieces b col
                               rayAttacks       = popCount $ mconcat $ concat
                                                  [ map snd $ attacking pt noMyPieces (opponent' col)
                                                  | pt <- [ C.Queen, C.Rook, C.Bishop ]
                                                  ]
                           in if True -- endGame b || opening b
                              then 0
                              else (defendingPawns - attackingKnights - rayAttacks) `div` 2-}


evaluatePins :: Board -> C.Color -> Int
evaluatePins b col = (-3) * if opening b
                            then 0
                            else sum $ do
                              pt      <- [ C.Queen, C.King ]
                              pinnert <- [ C.Queen, C.Rook, C.Bishop ]
                              guard $ pinnert /= C.Queen || pt == C.King -- otherwise not a real pin
                              pinner  <- BB.toList $ piecesOf b (opponent' col) pinnert
                                   
                              let ocpy   = b^.piecesByColour (opponent' col)
                                  attck  = magic pinnert pinner ocpy
                              return $ popCount $ piecesOf b col pt .&. attck
                        

evaluateRookPosition :: Board -> C.Color -> Int
evaluateRookPosition b col = let mySeventh = if col == C.White then seventhRank else secondRank
                                 onSeventh = popCount $ rankBB mySeventh .&. piecesOf b col C.Rook
                                 onOpen    = sum [ 1
                                                 | r <- BB.toList $ piecesOf b col C.Rook
                                                 , let f = fileBB $ file r
                                                 , f .&. piecesOf b col C.Pawn /= mempty
                                                 ]
                             in 5 * (onOpen + onSeventh)


evaluateBishopPosition :: Board -> C.Color -> Int
evaluateBishopPosition b col = evalFor darkSquares + evalFor lightSquares
  where evalFor fld = let myBishop = fld .&. piecesOf b col C.Bishop
                          enemy    = mconcat [ fld `shift` (direction col 8 * i) | i <- [1 .. 6]]
                          myPawns  = enemy .&. piecesOf b col C.Pawn
                      in  8 - sum [ popCount myPawns | _ <- BB.toList myBishop ]    


evaluateKnightPosition :: Board -> C.Color -> Int
evaluateKnightPosition b col = (-2) * popCount (rimSquares .&. piecesOf b col C.Knight)


opening :: Board -> Bool
opening b = popCount (occupancy b) > 26

  
endGame :: Board -> Bool
endGame b = popCount (occupancy b) < 16


evaluatePawnPosition :: Board -> C.Color -> Int
evaluatePawnPosition b col = let myPawns      = piecesOf b col C.Pawn
                                 blocking c f = length $ filter f [ fileBB (file p) .&. piecesOf b c C.Pawn | p <- BB.toList myPawns ]
                                 passed       = blocking (opponent' col) (==mempty)
                                 double       = blocking col (/=mempty) 
                             in if endGame b
                                then 5 * passed - 2 * double
                                else 2 * popCount (centralSquares .&. myPawns) + popCount (largeCentralSquares .&. myPawns)
