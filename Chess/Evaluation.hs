module Chess.Evaluation
       ( evaluate
       ) where

import           Prelude hiding (concat, sum, elem)

import           Control.Lens
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


evaluate :: Board -> Int
evaluate b = if not (anyMove b)
             then if inCheck b (b^.next)
                  then direction (b^.next) (-3000)
                  else 0
             else let material = sum [ w * (numberOf b White pt - numberOf b Black pt)
                                     | pt <- [ Queen,  Rook, Bishop, Knight, Pawn ]
                                     , let w = pieceValue pt
                                     ]
                  in material + sum [ f b White - f b Black
                                    | f <- [ evaluateRookPosition
                                           , evaluateKingSafety
                                           , evaluateBishopPosition
                                           , evaluateKnightPosition
                                           , evaluatePawnPosition
                                           ]
                                    ]


evaluateKingSafety :: Board -> Colour -> Int
evaluateKingSafety b col = let kingSq           = head $ BB.toList $ piecesOf b col King
                               quadrant         = neighbourFilesBB (file kingSq) .&. neighbourRanksBB (rank kingSq)
                               attackingKnights = popCount $ quadrant .&. piecesOf b (opponent' col) Knight
                               defendingPawns   = popCount $ kingAttackBB kingSq
                                                  .&. piecesOf b col Pawn
                                                  .&. aheadBB (rank kingSq) col
                               rayAttacks       = popCount $! mconcat
                                                  [ piecesOf b (opponent' col) pt .&. magic pt kingSq mempty
                                                  | pt <- [ Queen, Rook, Bishop ]
                                                  ]
                           in defendingPawns - attackingKnights - rayAttacks



evaluateRookPosition :: Board -> Colour -> Int
evaluateRookPosition b col = let mySeventh = if col == White then seventhRank else secondRank
                                 onSeventh = popCount $ rankBB mySeventh .&. piecesOf b col Rook
                                 onOpen    = sum [ 1
                                                 | r <- BB.toList $ piecesOf b col Rook
                                                 , let f = fileBB $ file r
                                                 , f .&. piecesOf b col Pawn /= mempty
                                                 ]
                             in 5 * (onOpen + onSeventh)


evaluateBishopPosition :: Board -> Colour -> Int
evaluateBishopPosition b col = evalFor darkSquares + evalFor lightSquares
  where evalFor fld = sum $ do
          bishopPos <- BB.toList $ fld .&. piecesOf b col Bishop
          let goodSquares = aheadBB (rank bishopPos) col
                            .&. magic Bishop bishopPos (occupancy b)
                            .&. complement (b^.piecesByColour col)
          return $ popCount goodSquares


evaluateKnightPosition :: Board -> Colour -> Int
evaluateKnightPosition b col = (-2) * popCount (rimSquares .&. piecesOf b col Knight)


  
endGame :: Board -> Bool
endGame b = popCount (occupancy b) < 16


evaluatePawnPosition :: Board -> Colour -> Int
evaluatePawnPosition b col = let myPawns      = piecesOf b col Pawn
                                 blocking c f = length $ filter f [ fileBB (file p) .&. piecesOf b c Pawn | p <- BB.toList myPawns ]
                                 passed       = blocking (opponent' col) (==mempty)
                                 double       = blocking col (/=mempty) 
                             in if endGame b
                                then 5 * passed - 2 * double
                                else 2 * popCount (centralSquares .&. myPawns) + popCount (largeCentralSquares .&. myPawns)
