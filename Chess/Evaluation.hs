{-# LANGUAGE MultiWayIf #-}
{- | Chess evalution -}
module Chess.Evaluation
       ( evaluate
       ) where

------------------------------------------------------------------------------
import           Control.Lens ((^.))

import           Chess.Board
import           Chess.Move
import           Data.BitBoard
import           Data.ChessTypes
import qualified Data.ChessTypes as T (opponent)
import           Data.Monoid
import           Data.Square


------------------------------------------------------------------------------
-- | Evaluates the 'Board'
evaluate :: Board -> Int
evaluate b =
  if not (anyMove b)
  then if inCheck b (b^.next)
       then direction (b^.next) (-3000)
       else 0
  else sum [ f b White - f b Black
           | f <- [ evaluateMaterial
                  , evaluateRookPosition
                  , evaluateKingSafety
                  , evaluateCastle
                  , evaluateBishopPosition
                  , evaluateKnightPosition
                  , evaluatePawnPosition
                  ]
           ]


------------------------------------------------------------------------------
-- Evaluate material
evaluateMaterial :: Board -> Colour -> Int
evaluateMaterial b c = sum [ w * numberOf b c pt
                           | pt <- [ Queen,  Rook, Bishop, Knight, Pawn ]
                           , let w = pieceValue pt
                           ]


------------------------------------------------------------------------------
-- number of enemy Knights in the Kings quadrant + number of ray pseudo
-- attacks hitting the King.
evaluateKingSafety :: Board -> Colour -> Int
evaluateKingSafety b c =
  let kingP            = kingByColour c b
      opp              = T.opponent c
      (kingF, kingR)   = (file kingP, rank kingP)
      quadrant         = neighbourFilesBB kingF .&. neighbourRanksBB kingR
      attackingKnights = popCount $ quadrant .&. piecesOf b opp Knight
      rayAttacks       = popCount $ mconcat
                         [ piecesOf b opp pt .&. pseudoAttackBB pt kingP
                         | pt <- [ Queen, Rook, Bishop ]
                         ]
  in negate $ attackingKnights + rayAttacks


------------------------------------------------------------------------------     
-- in case the enemy Queen is still on the Board, this evaluates the Kings
-- castle position, taking into account whether we have already castled (which
-- is worth more), counting the pawns still in the second rank, and scoring
-- down artifical castles that trap the Rook.
evaluateCastle :: Board -> Colour -> Int
evaluateCastle b c =
  let castles          = toCastleList (b^.castleRightsByColour c)
      pawnR            = rankBB $ pawnsRank c
      baseR            = rankBB $ baseRank c
      castleValue cs   = popCount $ castleF cs .&. pawnR .&. piecesOf b c Pawn
      obstacles cs     = popCount $ castleF cs .&. baseR .&. piecesByColour b c
      value cs         = castleValue cs - obstacles cs
      rookTrap Long f  = [ aFile .. f ]
      rookTrap Short f = [ f .. hFile ]
      kFile            = file $ kingByColour c b
      traps cs         = mconcat $ map fileBB $ rookTrap cs kFile
      alreadyCs cs     = castleF cs .&. baseR .&. piecesOf b c King /= mempty
                        && traps cs .&. baseR .&. piecesOf b c Rook == mempty
  in if piecesOf b (T.opponent c) Queen == mempty
     then 0
     else if null castles
          then 3 * if
            | alreadyCs Short -> castleValue Short
            | alreadyCs Long  -> castleValue Long
            | otherwise       -> 0 
          else foldr (max . value) 0 castles


------------------------------------------------------------------------------     
-- Value our Rooks on open files, plus value them even more on the seventh
-- rank
evaluateRookPosition :: Board -> Colour -> Int
evaluateRookPosition b c =
  let seventhR    = pawnsRank (T.opponent c)
      openF' pwns = complement (rankBB seventhR) .&.
                    mconcat [ f | f <- map fileBB files, f .&. pwns == mempty]
      semiOpenF   = openF' $ piecesOf b c Pawn
      openF       = openF' $ pawns b
      countRs bb  = popCount $ bb .&. piecesOf b c Rook
      onSeventh   = countRs $ rankBB seventhR
      onSemiOpen  = countRs semiOpenF
      onOpen      = countRs openF
  in 4 * onSeventh * onSeventh +  4 * onOpen + 3 * onSemiOpen


------------------------------------------------------------------------------     
-- If the bishop is the same colour as most of our pawns it's a bad bishop. A
-- bishop pair is better then a knight and bishop or two knights. Also a
-- bishop blocked behind our own pawns is very bad, especially if those pawns
-- are blocked. Knights are generally better in closed positions whereas
-- bishops are better in open positions. This only matters in the opening or
-- middle game though.
evaluateBishopPosition :: Board -> Colour -> Int
evaluateBishopPosition b c =
  let badBishop' bc = if piecesOf b c Bishop .&. bc /= mempty
                      then popCount $ bc .&. piecesOf b c Pawn
                      else 0
      badBishop     = badBishop' lightSquares + badBishop' darkSquares
      hasPair c'    = piecesOf b c' Bishop .&. lightSquares /= mempty
                      && piecesOf b c' Bishop .&. darkSquares /= mempty
      bishopPair    = hasPair c && not (hasPair (T.opponent c))
      forward       = foldr (<>) mempty [ aheadBB (rank p) c
                                          .&. pseudoAttackBB Bishop p
                                          .&. piecesOf b c Pawn
                                        | p <- toList $ piecesOf b c Bishop
                                        ]
  in (if bishopPair then 6 else 0) - badBishop - 2 * popCount forward


------------------------------------------------------------------------------     
-- Knight on the rim is always dim
evaluateKnightPosition :: Board -> Colour -> Int
evaluateKnightPosition b c =
  (-2) * popCount (rimSquares .&. piecesOf b c Knight)


------------------------------------------------------------------------------
-- Are we playing the endgame?
endGame :: Board -> Bool
endGame b = popCount (occupancy b) < 16


------------------------------------------------------------------------------
-- In end games count the passed or double pawns. In the middle game or opening
-- count the pawns fighting for the centre.
evaluatePawnPosition :: Board -> Colour -> Int
evaluatePawnPosition b c =
  let myPawns       = piecesOf b c Pawn
      aheadSq p     = fileBB (file p) .&. aheadBB (rank p) c `xor` fromSquare p
      blocking f    = filter f [ aheadSq p | p <- toList myPawns ]
      passed' bb    = piecesOf b (T.opponent c) Pawn .&. bb == mempty
      double' bb    = myPawns                       .&. bb /= mempty
      passed        = length $ blocking passed' 
      double        = length $ blocking double'
      centralized   = centralSquares .&. myPawns
      wkCentralized = largeCentralSquares .&. myPawns
  in if endGame b
     then 5 * passed - 2 * double
     else 2 * popCount centralized  + popCount wkCentralized


------------------------------------------------------------------------------
-- First rank from the players pov
baseRank :: Colour -> Rank
baseRank White = firstRank
baseRank Black = eighthRank


------------------------------------------------------------------------------
-- Second rank from the players pov
pawnsRank :: Colour -> Rank
pawnsRank White = secondRank
pawnsRank Black = seventhRank


------------------------------------------------------------------------------
-- a .. c or f .. h files
castleF :: Castle -> BitBoard
castleF Short = mconcat [fileBB r | r <- [ fFile .. hFile ]]
castleF Long  = mconcat [fileBB r | r <- [ aFile .. cFile ]]
