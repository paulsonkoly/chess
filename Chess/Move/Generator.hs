{-# LANGUAGE TemplateHaskell #-}
module Chess.Move.Generator
       ( -- * Legality checks
         PseudoLegalMove
       , Legality
       , mkPseudo
       , mkLegality
       , legalCheck
       , capturedPiece'
       , from'
       , to'
         -- * Generators
       , moves
       , forcingMoves
       , anyMove
       ) where

import           Control.Monad
import qualified Data.Foldable as F
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Set as S

import           Control.Lens hiding (to, from)

import           Chess.Board
import qualified Chess.Board as B (opponent)
import           Chess.Magic
import           Chess.Move.Execute
import           Chess.Move.Move
import           Data.BitBoard
import           Data.ChessTypes
import qualified Data.ChessTypes as T (opponent)
import           Data.Square

                                --------------
                                -- Legality --
                                --------------

------------------------------------------------------------------------------
-- | A move that might leave our king in check
newtype PseudoLegalMove = PseudoLegalMove Move deriving (Show, Eq, Ord)


data Legality = Legality
                { _board     :: Board
                , _inCheck'  :: Bool
                , _pins      :: BitBoard
                , _nubs      :: S.Set PseudoLegalMove
                }


$(makeLenses ''Legality)
                       

------------------------------------------------------------------------------
-- | Move legality checker constructor
mkLegality :: Board -> Legality
mkLegality b =
  let checkers = attackedFromBB b (occupancy b) (b^.B.opponent) (myKingPos b)
  in Legality b (checkers /= mempty) (pinned b) S.empty


------------------------------------------------------------------------------
-- | if the move is legal then Just move and the new Legality
legalCheck :: Legality -> PseudoLegalMove -> (Legality, Maybe Move)
legalCheck l psm@(PseudoLegalMove m) =
  if psm `S.member` (l^.nubs)
  then (l, Nothing)
  else
    let f  = if l^.inCheck'
             then legal (l^.board)
             else ok (l^.board) (l^.pins)
        nl = (nubs %~ S.insert psm) l
    in (nl, if f m then Just m else Nothing)


------------------------------------------------------------------------------
-- | Conversion from Move to PseudoLegalMove
mkPseudo :: Move -> PseudoLegalMove
mkPseudo = PseudoLegalMove


capturedPiece' :: PseudoLegalMove -> Maybe PieceType
capturedPiece' (PseudoLegalMove m) = m^.capturedPiece


from' :: PseudoLegalMove -> Square
from' (PseudoLegalMove m) = m^.from


to' :: PseudoLegalMove -> Square
to' (PseudoLegalMove m) = m^.to


------------------------------------------------------------------------------
-- | Pseudo legal moves
moves :: Board -> [ PseudoLegalMove ]
moves b =
  let checkers = attackedFromBB b (occupancy b) (b^.B.opponent) (myKingPos b)
      cs = simpleChecks b
           ++ discoveredChecks b
           ++ pawnSimpleChecks b
           ++ pawnDiscoveredChecks b
           ++ castleChecks b
      ps = pawnCapturesSorted b
           ++ pawnPromotions b
           ++ capturesSorted b
           ++ pawnEnPassants b
      ts = castleQuiet b
      nq = quietMovesSorted b ++ pawnQuietMovesSorted b
  in map PseudoLegalMove $ if checkers /= mempty
                           then defendCheck b checkers
                           else cs ++ ps ++ ts ++ nq


------------------------------------------------------------------------------
-- | Checks, captures and promotions, or moving out of check
forcingMoves :: Board -> [ PseudoLegalMove ]
forcingMoves b =
  let checkers = attackedFromBB b (occupancy b) (b^.B.opponent) (myKingPos b)
      ms = simpleChecks b
            ++ discoveredChecks b
            ++ pawnSimpleChecks b
            ++ pawnDiscoveredChecks b
            ++ castleChecks b
            ++ pawnCapturesSorted b
            ++ pawnPromotions b
            ++ capturesSorted b
            ++ pawnEnPassants b
  in map PseudoLegalMove $ if checkers /= mempty
                           then defendCheck b checkers
                           else ms


------------------------------------------------------------------------------
-- | Is there a legal move?
anyMove :: Board -> Bool
anyMove b =
  let pins' = pinned b
      checkers = attackedFromBB b (occupancy b) (b^.B.opponent) (myKingPos b)
  in if checkers /= mempty
     then any (legal b) $ defendCheck b checkers
     else or [ any (ok b pins') ms
             | ms <- [ quietMoves b
                     , pawnQuietMoves b
                     , captures b
                     , pawnCaptures b
                     , simpleChecks b
                     , pawnSimpleChecks b
                     , discoveredChecks b
                     , pawnDiscoveredChecks b
                     , pawnEnPassants b
                     , pawnPromotions b
                     ]
             ]


------------------------------------------------------------------------------
-- Legality check (fast). Check if the moving piece is not pinned. King is
-- also handled here.
ok :: Board -> BitBoard -> Move -> Bool
ok b pins' m = case m^.piece of
  King -> legal b m
  _    ->
    (pins' .&. fromSquare (m^.from)) == mempty -- not pinned
    -- or pinned, but staying in line with the King
    || lineBB (myKingPos b) (m^.to) .&. fromSquare (m^.from) /= mempty
    || lineBB (myKingPos b) (m^.from) .&. fromSquare (m^.to) /= mempty


------------------------------------------------------------------------------
-- Legality check (slow). Makes the move and then checks whether our own king
-- is in check
legal :: Board -> Move -> Bool
legal b m = not $ inCheck (makeMoveSimplified m b) (b^.next)


                ---------------------------------------------
                -- Move generation in case we are in check --
                ---------------------------------------------

------------------------------------------------------------------------------
defendCheck :: Board -> BitBoard -> [ Move ]
defendCheck b checkers =
  let numCheckers = popCount checkers
  in case numCheckers of
    1 -> let checker = head $ toList checkers
         in captureChecker b checker ++ blockSlider b checker ++ moveKing b
    2 -> moveKing b
    _ -> error "unexpected number of checkers"


------------------------------------------------------------------------------
-- Captures the checking piece (generates pseudo legal moves)
captureChecker :: Board -> Square -> [ Move ]
captureChecker b checker =
  let pt = fromJust $ pieceAt b checker
  in  sortBy captureHeuristics
      $ map (capturedPiece .~ Just pt) $ captureTo b checker


------------------------------------------------------------------------------
-- Blocks a slider piece
blockSlider :: Board -> Square -> [ Move ]
blockSlider b checker = sortBy moveValHeuristics $ concat $ do
  let endSquares = piecesOf b (b^.next) King .|. fromSquare checker
  t <- toList $ lineBB (myKingPos b) checker .&. complement endSquares
  return $ moveTo b t


------------------------------------------------------------------------------
moveKing :: Board -> [ Move ]
moveKing b = do
  t <- toList $ moveFun b King (myKingPos b) .&. complement (myPieces b)
  return
    $ (capturedPiece .~ pieceAt b t)
    $ defaultMove (myKingPos b) t King (b^.next)


------------------------------------------------------------------------------
-- moves capturing on the given square. This function doesn't set the
-- capturedPiece. This function is used for capturing the checking piece.
captureTo :: Board -> Square -> [ Move ]
captureTo b t = do
  pt <- [ Pawn, Knight, Bishop, Rook, Queen, King ]
  let sqrs = if pt == Pawn
             then pawnAttackBB t (b^.B.opponent)
             else moveFun b pt t
      pcs = sqrs .&. piecesOf b (b^.next) pt
  f <- toList pcs
  return $ defaultMove f t pt (b^.next)


------------------------------------------------------------------------------
-- moving to the specified square (non capture) to block a check
moveTo :: Board -> Square -> [ Move ]
moveTo b t = do
  pt <- [ Pawn, Knight, Bishop, Rook, Queen ]
  let pwn  = filter (\(_, t', _) -> t' == t) $ pawnAdvanceSquares b
      sqrs = if pt == Pawn
             then foldr ((<>) . fromSquare . (\(f, _, _) -> f)) mempty pwn
             else moveFun b pt t
      pcs  = sqrs .&. piecesOf b (b^.next) pt
  f <- toList pcs
  return $ defaultMove f t pt (b^.next)

               
------------------------------------------------------------------------------
-- | enemy king position
eKingPos :: Board -> Square
eKingPos b = kingByColour (b^.B.opponent) b
{-# INLINE eKingPos #-}


------------------------------------------------------------------------------
-- | my king position
myKingPos :: Board -> Square
myKingPos b = kingByColour (b^.next) b
{-# INLINE myKingPos #-}


------------------------------------------------------------------------------
-- | knight, bishop, rook, queen simple checks - not discovered check
simpleChecks :: Board -> [ Move ]
simpleChecks b = do
  pt <- [ Queen, Rook, Bishop, Knight ]

  let kingMoves  = moveFun b pt (eKingPos b) .&. complement (myPieces b)
  f <- toList $ piecesOf b (b^.next) pt
  let pieceMoves = moveFun b pt f
      common = kingMoves .&. pieceMoves

  -- common should be mempty most of the time, so we should
  -- short circuit the calcualtion from this point
  t <- toList common
  guard $ moveValid pt f t
  return
    $ (capturedPiece .~ pieceAt b t)
    $ defaultMove f t pt (b^.next)

  where
    moveValid Knight f t = let hd = hDist f t
                               vd = vDist f t
                           in (hd == 1 && vd == 2) || (hd == 2 && vd == 1)
    moveValid Bishop f t = hDist f t == vDist f t
    moveValid Rook   f t = hDist f t == 0 || vDist f t == 0
    moveValid Queen  f t = moveValid Rook f t || moveValid Bishop f t
    moveValid _      _ _ = error "unexpected piece type"


------------------------------------------------------------------------------
-- | discovered checks (with any piece type except pawns)
discoveredChecks :: Board -> [ Move ]
discoveredChecks b = do
  pt <- [ Queen, Rook, Bishop, Knight, King ]
  let ps = piecesOf b (b^.next) pt .&. discoverer b
  -- ps should be mempty most of the time..
  f <- toList ps
  t <- toList $ moveFun b pt f .&. complement (myPieces b)
  -- make sure that the enemy king is in check..
  guard $ inRayCheck b (occupancy b `xor` fromSquare f `xor` fromSquare t)
  return
    $ (capturedPiece .~ pieceAt b t)
    $ defaultMove f t pt (b^.next)


------------------------------------------------------------------------------
inRayCheck :: Board -> BitBoard -> Bool
inRayCheck b occ = or $ do
  pt <- [ Queen, Rook, Bishop ]
  -- King casting rays ..
  let eKingRay = magic pt (eKingPos b) occ
  return $ mempty /= eKingRay .&. myPiecesOf b pt


------------------------------------------------------------------------------
castleChecks :: Board -> [ Move ]
castleChecks = flip castleMoves True


------------------------------------------------------------------------------
pawnDiscoveredChecks :: Board -> [ Move ]
pawnDiscoveredChecks b =
  pawnMoves b (\f _ -> fromSquare f .&. discoverer b /= mempty)


------------------------------------------------------------------------------
pawnSimpleChecks :: Board -> [ Move ]
pawnSimpleChecks b =
  let kp = [ offset (eKingPos b) (direction (b^.B.opponent) lr)
           | lr <- [ 7, 9]
           ]
  in pawnMoves b (\_ t -> F.any (== t) kp)


------------------------------------------------------------------------------
pawnPromotions :: Board -> [ Move ]
pawnPromotions b =
  let opSecond = rankBB $ case b^.next of
        White -> seventhRank
        Black -> secondRank
  in pawnMoves b (\f _ -> fromSquare f .&. opSecond /= mempty)


------------------------------------------------------------------------------
pawnCaptures :: Board -> [ Move ]
pawnCaptures b =
  pawnMoves b (\_ t -> (fromSquare t .&. opponentsPieces b) /= mempty)


------------------------------------------------------------------------------
pawnCapturesSorted :: Board -> [ Move ]
pawnCapturesSorted = sortBy captureHeuristics . pawnCaptures


------------------------------------------------------------------------------
pawnEnPassants :: Board -> [ Move ]
pawnEnPassants b = do
  (f, t, enp) <- pawnEnPassantSquares b
  return
    $ (enPassantTarget .~ enp)
    $ defaultMove f t Pawn (b^.next)


------------------------------------------------------------------------------
-- pieces that can give discovered checks
discoverer :: Board -> BitBoard
discoverer b = discovererOrPinned b (b^.B.opponent) (b^.next)


------------------------------------------------------------------------------
-- pieces that are pinned
pinned :: Board -> BitBoard
pinned b = discovererOrPinned b (b^.next) (b^.next)


------------------------------------------------------------------------------
discovererOrPinned
  :: Board
  -> Colour -- ^ Colour of the King
  -> Colour -- ^ Colour of the Piece that is pinned or gives discovered check
  -> BitBoard
discovererOrPinned b c1 c2 = mconcat $ do
  pt <- [ Rook, Bishop ]
  let kingPos = kingByColour c1 b
      -- King casting rays ..
      kingRay = pseudoAttackBB pt kingPos -- magic pt kingPos (occupancy b)
      casters = kingRay .&.
                (piecesOf b (T.opponent c1) pt
                 <> piecesOf b (T.opponent c1) Queen)
  caster <- toList casters
  let inBetween = lineBB kingPos caster
                  `xor` fromSquare kingPos
                  `xor` fromSquare caster
  guard (popCount (inBetween .&. occupancy b) < 2)
  return $ inBetween .&. piecesByColour b c2


------------------------------------------------------------------------------
captures :: Board -> [ Move ]
captures b = normMoveGen b (opponentsPieces b)


------------------------------------------------------------------------------
capturesSorted :: Board -> [ Move ]
capturesSorted = sortBy captureHeuristics . captures


------------------------------------------------------------------------------
quietMoves :: Board -> [ Move ]
quietMoves b = normMoveGen b (vacated b)


------------------------------------------------------------------------------
quietMovesSorted :: Board -> [ Move ]
quietMovesSorted = sortBy moveValHeuristics . quietMoves


------------------------------------------------------------------------------
castleQuiet :: Board -> [ Move ]
castleQuiet b = castleMoves b False


------------------------------------------------------------------------------
pawnQuietMoves :: Board -> [ Move ]
pawnQuietMoves b = do
  (f, t, _) <- pawnAdvanceSquares b
  concatMap promote [ defaultMove f t Pawn (b^.next) ]


------------------------------------------------------------------------------
pawnQuietMovesSorted :: Board -> [ Move ]
pawnQuietMovesSorted = sortBy moveValHeuristics . pawnQuietMoves


------------------------------------------------------------------------------
-- captures or quiet moves
normMoveGen :: Board -> BitBoard -> [ Move ]
normMoveGen b filt = do
  pt <- [ Queen, Rook, Bishop, Knight, King ]
  let ps = piecesOf b (b^.next) pt
  f <- toList ps
  t <- toList $ moveFun b pt f .&. filt
  return
    $ (capturedPiece .~ pieceAt b t)
    $ defaultMove f t pt (b^.next)


------------------------------------------------------------------------------
moveFun :: Board -> PieceType -> Square -> BitBoard
moveFun b pt
  | pt == Knight = knightAttackBB
  | pt == King   = kingAttackBB
  | pt == Pawn   = error "moveFun is not implemented for Pawns"
  | otherwise      = \t -> magic pt t (occupancy b)


------------------------------------------------------------------------------
pawnMoves :: Board -> (Square -> Square -> Bool) -> [ Move ]
pawnMoves b fun = do
  (f, t, enp) <- pawnAdvanceSquares b
                 ++ pawnCaptureSquares b
                 ++ pawnEnPassantSquares b
  guard $ fun f t
  map (enPassantTarget .~ enp) $ promote
    $ (capturedPiece .~ pieceAt b t)
    $ defaultMove f t Pawn (b^.next)


------------------------------------------------------------------------------
promote :: Move -> [ Move ]
promote m =
  if fromSquare (m^.to) .&. (rankBB firstRank .|. rankBB eighthRank) /= mempty
  then do
    promo <- [ Queen, Rook, Knight, Bishop ]
    return $ (promotion .~ Just promo) m
  else [ m ]


------------------------------------------------------------------------------
-- | Pawn captures (from, to) including promotions, excluding advances or
-- en Passant
pawnCaptureSquares :: Board -> [ (Square, Square, Maybe Square) ]
pawnCaptureSquares b = do
  let myPawns = myPiecesOf b Pawn
      -- files from which we can left/right capture
      cFiles 7 White = complement $ fileBB aFile
      cFiles 9 White = complement $ fileBB hFile
      cFiles 7 Black = complement $ fileBB hFile
      cFiles 9 Black = complement $ fileBB aFile
      cFiles _ _       = error "Unexpected numbers"
  capture <- [7, 9]  -- left and right capture
  target  <- toList $ opponentsPieces b .&.
             ((myPawns .&. cFiles capture (b^.next))
              `shift` direction (b^.next) capture)
  return (offset target (direction (b^.B.opponent) capture), target, Nothing)


------------------------------------------------------------------------------
-- Pawn advances (from, to) including promotions, excluding captures
pawnAdvanceSquares :: Board -> [ (Square, Square, Maybe Square) ]
pawnAdvanceSquares b = do
  step <- [ 1, 2 ]
  let myPawns'  = myPiecesOf b Pawn
      myPawns   = if step == 1 then myPawns' else myPawns' .&. mySecond
      mySecond  = rankBB $ if b^.next == White
                           then secondRank
                           else seventhRank
      unblocked = complement $ foldl1 (<>)
                  [ occupancy b `shift` direction (b^.next) (-8 * j)
                  | j <- [ 1 .. step ]
                  ]
  pawn <- toList $ myPawns .&. unblocked
  return (pawn, offset pawn $ step * direction (b^.next) 8, Nothing)


------------------------------------------------------------------------------
pawnEnPassantSquares :: Board -> [ (Square, Square, Maybe Square) ]
pawnEnPassantSquares b = case b^.enPassant of
  Just square -> do
    pawn <- toList $ (fromSquare (offset square 1)
                      .|. fromSquare (offset square (-1)))
            .&. largeNeighbourFilesBB (file square)
            .&. myPiecesOf b Pawn
    return (pawn, offset square $ direction (b^.next) 8, Just square)
  Nothing -> []


------------------------------------------------------------------------------
captureHeuristics :: Move -> Move -> Ordering
captureHeuristics =
  comparing (Down . pieceValue . fromJust . view capturedPiece)


------------------------------------------------------------------------------
moveValHeuristics :: Move -> Move -> Ordering
moveValHeuristics = comparing (Down . moveValue)
                    

------------------------------------------------------------------------------
castleMoves :: Board -> Bool -> [ Move ]
castleMoves b chk = do
  side <- toCastleList $ b^.castleRightsByColour (b^.next)
  let kRays     = magic Rook (eKingPos b) (occupancy b)
      (f, t)    = kingCastleMove (b^.next) side
      rt        = toEnum $ (fromEnum f + fromEnum t) `div` 2
      checkSqrs = toList $ checkCastleBB (b^.next) side
  guard $ chk /= (fromSquare rt .&. kRays == mempty)
  guard $ (vacancyCastleBB (b^.next) side .&. occupancy b) == mempty
  guard $ not $ F.any (isAttacked b (b^.B.opponent)) checkSqrs
  return
    $ (castle .~ Just side)
    $ defaultMove f t King $ b^.next


------------------------------------------------------------------------------
vacancyCastleBB :: Colour -> Castle -> BitBoard
vacancyCastleBB White Long  =
  mconcat [ fromSquare sq
          | sq <- [(toSquare bFile firstRank) .. (toSquare dFile firstRank)]
          ]
vacancyCastleBB White Short =
  mconcat [ fromSquare sq
          | sq <- [(toSquare fFile firstRank) .. (toSquare gFile firstRank)]
          ]
vacancyCastleBB Black Long  =
  mconcat [ fromSquare sq
          | sq <- [(toSquare bFile eighthRank) .. (toSquare dFile eighthRank)]
          ]
vacancyCastleBB Black Short =
  mconcat [ fromSquare sq
          | sq <- [(toSquare fFile eighthRank) .. (toSquare gFile eighthRank)]
          ]
{-# INLINE vacancyCastleBB #-}


------------------------------------------------------------------------------
checkCastleBB :: Colour -> Castle -> BitBoard
checkCastleBB White Long  =
  mconcat [ fromSquare sq
          | sq <- [(toSquare cFile firstRank) .. (toSquare eFile firstRank)]
          ]
checkCastleBB White Short =
  mconcat [ fromSquare sq
          | sq <- [(toSquare eFile firstRank) .. (toSquare gFile firstRank)]
          ]
checkCastleBB Black Long  =
  mconcat [ fromSquare sq
          | sq <- [(toSquare cFile eighthRank) .. (toSquare eFile eighthRank)]
          ]
checkCastleBB Black Short =
  mconcat [ fromSquare sq
          | sq <- [(toSquare eFile eighthRank) .. (toSquare gFile eighthRank)]
          ]
{-# INLINE checkCastleBB #-}


------------------------------------------------------------------------------
kingCastleMove :: Colour -> Castle -> (Square, Square)
kingCastleMove White Long  =
  (toSquare eFile firstRank, toSquare cFile firstRank)
kingCastleMove White Short =
  (toSquare eFile firstRank, toSquare gFile firstRank)
kingCastleMove Black Long  =
  (toSquare eFile eighthRank, toSquare cFile eighthRank)
kingCastleMove Black Short =
  (toSquare eFile eighthRank, toSquare gFile eighthRank)
{-# INLINE kingCastleMove #-}
