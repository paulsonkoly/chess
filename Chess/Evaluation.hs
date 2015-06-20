{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
-- | Chess evalution
module Chess.Evaluation
       ( evaluate
       -- * Evaluation configuration
       , EvaluationConfig
       , kingSafetyAttackingKnights
       , kingSafetyRayAttacks      
       , castlePawnValue           
       , rookOnSeventh             
       , rookOnOpen                
       , rookOnSemiOpen            
       , bishopPair                
       , bishopBadBishop           
       , bishopBlockingPawns       
       , knightOnRimSquares        
       , pawnEndGamePassed         
       , pawnEndGameDouble         
       , pawnOpeningCentral        
       , pawnOpeningLargeCentral   
       , outpostCentral            
       , outpostLargeCentral       
       ) where

------------------------------------------------------------------------------
import           Control.Monad (guard)

import           Control.Lens
import           Data.Default

import           Chess.Board
import           Chess.Move
import           Data.BitBoard
import           Data.ChessTypes
import qualified Data.ChessTypes as T (opponent)
import           Data.Monoid
import           Data.Square


------------------------------------------------------------------------------
data EvaluationConfig = EvaluationConfig
                        { _kingSafetyAttackingKnights :: ! Int
                        , _kingSafetyRayAttacks       :: ! Int
                        , _castlePawnValue            :: ! Int
                        , _rookOnSeventh              :: ! Int
                        , _rookOnOpen                 :: ! Int
                        , _rookOnSemiOpen             :: ! Int
                        , _bishopPair                 :: ! Int
                        , _bishopBadBishop            :: ! Int
                        , _bishopBlockingPawns        :: ! Int
                        , _knightOnRimSquares         :: ! Int
                        , _pawnEndGamePassed          :: ! Int
                        , _pawnEndGameDouble          :: ! Int
                        , _pawnOpeningCentral         :: ! Int
                        , _pawnOpeningLargeCentral    :: ! Int
                        , _outpostCentral             :: ! Int
                        , _outpostLargeCentral        :: ! Int
                        } deriving (Show, Read, Eq, Ord)


$(makeLenses ''EvaluationConfig)


------------------------------------------------------------------------------
instance Default EvaluationConfig where
  def = EvaluationConfig
        { _kingSafetyAttackingKnights = 1
        , _kingSafetyRayAttacks       = 1
        , _castlePawnValue            = 3
        , _rookOnSeventh              = 4
        , _rookOnOpen                 = 4 
        , _rookOnSemiOpen             = 3
        , _bishopPair                 = 6
        , _bishopBadBishop            = 1
        , _bishopBlockingPawns        = 2
        , _knightOnRimSquares         = 2
        , _pawnEndGamePassed          = 5
        , _pawnEndGameDouble          = 2
        , _pawnOpeningCentral         = 2
        , _pawnOpeningLargeCentral    = 1
        , _outpostCentral             = 2
        , _outpostLargeCentral        = 1
        }


type Evaluation = EvaluationConfig -> Board -> Colour -> Int


------------------------------------------------------------------------------
-- | Evaluates the 'Board'
evaluate :: EvaluationConfig -> Board -> Int
evaluate ec b =
  if not (anyMove b)
  then if inCheck b (b^.next)
       then direction (b^.next) (-3000)
       else 0
  else sum [ f ec b White - f ec b Black
           | f <- [ evaluateMaterial
                  , evaluateRookPosition
                  , evaluateKingSafety
                  , evaluateOutpost
                  , evaluateCastle
                  , evaluateBishopPosition
                  , evaluateKnightPosition
                  , evaluatePawnPosition
                  ]
           ]


------------------------------------------------------------------------------
-- Evaluate material
evaluateMaterial :: Evaluation
evaluateMaterial _ b c =
  sum [ w * numberOf b c pt
      | pt <- [ Queen,  Rook, Bishop, Knight, Pawn ]
      , let w = pieceValue pt
      ]


------------------------------------------------------------------------------
-- number of enemy Knights in the Kings quadrant + number of ray pseudo
-- attacks hitting the King.
evaluateKingSafety :: Evaluation
evaluateKingSafety ec b c =
  let kingP            = kingByColour c b
      opp              = T.opponent c
      (kingF, kingR)   = (file kingP, rank kingP)
      quadrant         =
        largeNeighbourFilesBB kingF .&. neighbourRanksBB kingR
      attackingKnights = popCount $ quadrant .&. piecesOf b opp Knight
      rayAttacks       = popCount $ mconcat
                         [ piecesOf b opp pt .&. pseudoAttackBB pt kingP
                         | pt <- [ Queen, Rook, Bishop ]
                         ]
  in negate
     $ ec^.kingSafetyAttackingKnights * attackingKnights
     + ec^.kingSafetyRayAttacks * rayAttacks


------------------------------------------------------------------------------     
-- in case the enemy Queen is still on the Board, this evaluates the Kings
-- castle position, taking into account whether we have already castled (which
-- is worth more), counting the pawns still in the second rank, and scoring
-- down artifical castles that trap the Rook.
evaluateCastle :: Evaluation
evaluateCastle ec b c =
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
          then ec^.castlePawnValue * if
            | alreadyCs Short -> castleValue Short
            | alreadyCs Long  -> castleValue Long
            | otherwise       -> 0 
          else foldr (max . value) 0 castles


------------------------------------------------------------------------------     
-- Value our Rooks on open files, plus value them even more on the seventh
-- rank
evaluateRookPosition :: Evaluation
evaluateRookPosition ec b c =
  let seventhR    = pawnsRank (T.opponent c)
      rookFiles   = map (fileBB . file) $ toList
                    $ piecesOf b c Rook .&. complement (rankBB seventhR)
      onSemiOpen' = filter (\f -> f .&. piecesOf b c Pawn == mempty) rookFiles
      onOpen'     = filter (\f -> f .&. pawns b == mempty) onSemiOpen'
      onOpen      = length onOpen'
      onSemiOpen  = length onSemiOpen'
      countRs bb  = popCount $ bb .&. piecesOf b c Rook
      onSeventh   = countRs $ rankBB seventhR
  in ec^.rookOnSeventh * onSeventh * onSeventh
     + ec^.rookOnOpen * onOpen
     + ec^.rookOnSemiOpen * onSemiOpen


------------------------------------------------------------------------------     
-- If the bishop is the same colour as most of our pawns it's a bad bishop. A
-- bishop pair is better then a knight and bishop or two knights. Also a
-- bishop blocked behind our own pawns is very bad, especially if those pawns
-- are blocked. Knights are generally better in closed positions whereas
-- bishops are better in open positions. This only matters in the opening or
-- middle game though.
evaluateBishopPosition :: Evaluation
evaluateBishopPosition ec b c =
  let badBishop' bc = if piecesOf b c Bishop .&. bc /= mempty
                      then popCount $ bc .&. piecesOf b c Pawn
                      else 0
      badBishop     = badBishop' lightSquares + badBishop' darkSquares
      hasPair c'    = piecesOf b c' Bishop .&. lightSquares /= mempty
                      && piecesOf b c' Bishop .&. darkSquares /= mempty
      bishopPair'   = hasPair c && not (hasPair (T.opponent c))
      forward       = foldr (<>) mempty [ aheadBB (rank p) c
                                          .&. pseudoAttackBB Bishop p
                                          .&. piecesOf b c Pawn
                                        | p <- toList $ piecesOf b c Bishop
                                        ]
  in (if bishopPair' then ec^.bishopPair else 0)
     - ec^.bishopBadBishop * badBishop
     - ec^.bishopBlockingPawns * popCount forward


------------------------------------------------------------------------------     
-- Knight on the rim is always dim
evaluateKnightPosition :: Evaluation
evaluateKnightPosition ec b c =
  negate $
  ec^.knightOnRimSquares * popCount (rimSquares .&. piecesOf b c Knight)


------------------------------------------------------------------------------
-- Are we playing the endgame?
endGame :: Board -> Bool
endGame b = popCount (occupancy b) < 16


------------------------------------------------------------------------------
-- In end games count the passed or double pawns. In the middle game or
-- opening count the pawns fighting for the centre.
evaluatePawnPosition :: Evaluation
evaluatePawnPosition ec b c =
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
     then ec^.pawnEndGamePassed * passed - ec^.pawnEndGameDouble * double
     else ec^.pawnOpeningCentral * popCount centralized
          + ec^.pawnOpeningLargeCentral * popCount wkCentralized


------------------------------------------------------------------------------
-- Bishop or Knight outpost on the central Squares / large central Squares
evaluateOutpost :: Evaluation
evaluateOutpost ec b c =
  let minorPieces  = mconcat [ piecesOf b c pt | pt <- [ Bishop, Knight ]]
      central      = centralSquares .&. minorPieces
      myFifth      = case c of
                      White -> fifthRank
                      Black -> fourthRank
      largeCentral = largeCentralSquares
                     .&. complement centralSquares
                     .&. aheadBB myFifth c
                     .&. minorPieces
      evalPosts bb = sum $ do
        sq <- toList bb
        let (f, r) = (file sq, rank sq)
            ahead  = aheadBB r c .&. complement (rankBB r)
            fs     = neighbourFilesBB f .&. complement (fileBB f)
        guard $ piecesOf b (T.opponent c) Pawn .&. ahead .&. fs == mempty
        return 3
  in ec^.outpostCentral * evalPosts central
     + ec^.outpostLargeCentral * evalPosts largeCentral


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
