module Chess.Move.Generator
       ( moves
       , forcingMoves
       , anyMove
       ) where

import           Data.Monoid
import qualified Data.Foldable as F
import           Data.Maybe
import           Data.List
import           Control.Lens hiding (to, from)
import           Control.Monad

import           Data.Square
import           Data.BitBoard
import           Chess.Magic
import           Data.ChessTypes
import           Chess.Move.Move
import           Chess.Move.Execute
import           Chess.Board.Attacks
import           Chess.Board

-- | Legal moves
moves :: Board -> [ Move ]
moves b = let cs = nub (simpleChecks b ++ discoveredChecks b)            -- the nub is not strictly nesecarry, but outherwise we
                   ++ nub (pawnSimpleChecks b ++ pawnDiscoveredChecks b) -- would break perft with double checks.
                   ++ castleChecks b
              ps = pawnCapturesSorted b ++ pawnPromotions b ++ capturesSorted b ++ pawnEnPassants b
              ts = castleQuiet b
              nq = quietMovesSorted b ++ pawnQuietMovesSorted b
              inc = inCheckWithNoFriendly b (b^.next)
          in filter (not . check b (b^.next) inc) $ cs ++ (ps \\ cs) ++ ts ++ (nq \\ cs)


-- | Checks, captures and promotions
forcingMoves :: Board -> [ Move ]
forcingMoves b = let ms = simpleChecks b
                          ++ discoveredChecks b
                          ++ pawnSimpleChecks b
                          ++ pawnDiscoveredChecks b
                          ++ castleChecks b
                          ++ pawnCapturesSorted b
                          ++ pawnPromotions b
                          ++ capturesSorted b
                          ++ pawnEnPassants b
                     inc = inCheckWithNoFriendly b (b^.next)
                 in filter (not . check b (b^.next) inc) ms


-- | Is there a legal move?
anyMove :: Board -> Bool
anyMove b = let ok  = any (legal b)
            in ok (quietMoves b)
               || ok (pawnQuietMoves b)            -- most frequent first
               || ok (captures b)
               || ok (pawnCaptures b)
               || ok (simpleChecks b)
               || ok (pawnSimpleChecks b)
               || ok (discoveredChecks b)
               || ok (pawnDiscoveredChecks b)
               || ok (pawnEnPassants b)
               || ok (pawnPromotions b)


-- | my king position
mKingPos :: Board -> Square
mKingPos b = head $ toList $ piecesOf b (b^.next) King
{-# INLINE mKingPos #-}


-- | enemy king position
eKingPos :: Board -> Square
eKingPos b = head $ toList $ piecesOf b (b^.opponent) King
{-# INLINE eKingPos #-}


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


inRayCheck :: Board -> BitBoard -> Bool
inRayCheck b occ = or $ do
  pt <- [ Queen, Rook, Bishop ]
  -- King casting rays ..
  let eKingRay = magic pt (eKingPos b) occ
  return $ mempty /= eKingRay .&. myPiecesOf b pt


castleChecks :: Board -> [ Move ]
castleChecks = flip castleMoves True


pawnDiscoveredChecks :: Board -> [ Move ]
pawnDiscoveredChecks b = pawnMoves b (\f _ -> fromSquare f .&. discoverer b /= mempty)


pawnSimpleChecks :: Board -> [ Move ]
pawnSimpleChecks b = let kp = [ offset (eKingPos b) (direction (b^.opponent) lr) | lr <- [ 7, 9] ]
                     in  pawnMoves b (\_ t -> F.any (== t) kp)


pawnPromotions :: Board -> [ Move ]
pawnPromotions b = let opSecond = rankBB $ case b^.next of
                         White -> seventhRank
                         Black -> secondRank
                   in pawnMoves b (\f _ -> fromSquare f .&. opSecond /= mempty)


pawnCaptures :: Board -> [ Move ]
pawnCaptures b = pawnMoves b (\_ t -> (fromSquare t .&. opponentsPieces b) /= mempty)



pawnCapturesSorted :: Board -> [ Move ]
pawnCapturesSorted = sortBy heuristics . pawnCaptures
  where heuristics x y = pieceValue (fromJust $ y^.capturedPiece) `compare` pieceValue (fromJust $ x^.capturedPiece)


pawnEnPassants :: Board -> [ Move ]
pawnEnPassants b = do
  (f, t, enp) <- pawnEnPassantSquares b
  return
    $ (enPassantTarget .~ enp)
    $ defaultMove f t Pawn (b^.next)


-- | pieces that can give discovered checks
discoverer :: Board -> BitBoard
discoverer b = discovererOrPinned b (b^.opponent) (b^.next)


-- | pieces that are pinned
pinned :: Board -> BitBoard
pinned b = discovererOrPinned b (b^.next) (b^.next)


discovererOrPinned
  :: Board
  -> Colour -- ^ Colour of the King
  -> Colour -- ^ Colour of the Piece that is pinned or gives discovered check
  -> BitBoard
discovererOrPinned b c1 c2 = mconcat $ do
  pt <- [ Rook, Bishop ]
  let kingPos = head $ toList $ piecesOf b c1 King
      -- King casting rays ..
      kingRay = pseudoAttackBB pt kingPos -- magic pt kingPos (occupancy b)
      casters = kingRay .&. (piecesOf b (opponent' c1) pt <> piecesOf b (opponent' c1) Queen)
  caster <- toList casters
  let inBetween = lineBB kingPos caster `xor` fromSquare kingPos `xor` fromSquare caster
  guard (popCount (inBetween .&. occupancy b) < 2)
  return $ inBetween .&. b^.piecesByColour c2


captures :: Board -> [ Move ]
captures b = normMoveGen b (opponentsPieces b)


capturesSorted :: Board -> [ Move ]
capturesSorted = sortBy heuristics . captures
  where heuristics x y = pieceValue (fromJust $ y^.capturedPiece) `compare` pieceValue (fromJust $ x^.capturedPiece)


quietMoves :: Board -> [ Move ]
quietMoves b = normMoveGen b (vacated b)


quietMovesSorted :: Board -> [ Move ]
quietMovesSorted = sortBy heuristics . quietMoves
  where heuristics x y = moveValue y `compare` moveValue x


castleQuiet :: Board -> [ Move ]
castleQuiet b = castleMoves b False


pawnQuietMoves :: Board -> [ Move ]
pawnQuietMoves b = do
  (f, t, _) <- pawnAdvanceSquares b
  concatMap promote $ return $ defaultMove f t Pawn (b^.next)



pawnQuietMovesSorted :: Board -> [ Move ]
pawnQuietMovesSorted = sortBy heuristics . pawnQuietMoves
  where heuristics x y = moveValue y `compare` moveValue x


-- | captures or quiet moves
normMoveGen :: Board -> BitBoard -> [ Move ]
normMoveGen b filt = do
  pt <- [ Queen, Rook, Bishop, Knight, King ]
  let ps = piecesOf b (b^.next) pt
  f <- toList ps
  t <- toList $ moveFun b pt f .&. filt
  return
    $ (capturedPiece .~ pieceAt b t)
    $ defaultMove f t pt (b^.next)


moveFun :: Board -> PieceType -> Square -> BitBoard
moveFun b pt
  | pt == Knight = knightAttackBB
  | pt == King   = kingAttackBB
  | pt == Pawn   = error "moveFun is not implemented for Pawns"
  | otherwise      = \t -> magic pt t (occupancy b)


pawnMoves :: Board -> (Square -> Square -> Bool) -> [ Move ]
pawnMoves b fun = do
  (f, t, enp) <- pawnAdvanceSquares b ++ pawnCaptureSquares b ++ pawnEnPassantSquares b
  guard $ fun f t
  map (enPassantTarget .~ enp) $ promote
    $ (capturedPiece .~ pieceAt b t)
    $ defaultMove f t Pawn (b^.next)


promote :: Move -> [ Move ]
promote m = if fromSquare (m^.to) .&. (rankBB firstRank .|. rankBB eighthRank) /= mempty
            then do
              promo <- [ Queen, Rook, Knight, Bishop ]
              return $ (promotion .~ Just promo) m
            else [ m ]


-- | Pawn captures (from, to) including promotions, excluding advances or en Passant
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
  target  <- toList $ opponentsPieces b .&. ((myPawns .&. cFiles capture (b^.next)) `shift` direction (b^.next) capture)
  return (offset target (direction (b^.opponent) capture), target, Nothing)


-- | Pawn advances (from, to) including promotions, excluding captures
pawnAdvanceSquares :: Board -> [ (Square, Square, Maybe Square) ]
pawnAdvanceSquares b = do
  step <- [ 1, 2 ]
  let myPawns'  = myPiecesOf b Pawn
      myPawns   = if step == 1 then myPawns' else myPawns' .&. mySecond
      mySecond  = rankBB $ if b^.next == White then secondRank else seventhRank
      unblocked = complement $ foldl1 (<>) [ occupancy b `shift` direction (b^.next) (-8 * j) | j <- [ 1 .. step ] ]
  pawn <- toList $ myPawns .&. unblocked
  return (pawn, offset pawn $ step * direction (b^.next) 8, Nothing)


pawnEnPassantSquares :: Board -> [ (Square, Square, Maybe Square) ]
pawnEnPassantSquares b = case b^.enPassant of
  Just square -> do
    pawn <- toList $ (fromSquare (offset square 1) .|. fromSquare (offset square (-1)))
            .&. neighbourFilesBB (file square)
            .&. myPiecesOf b Pawn
    return (pawn, offset square $ direction (b^.next) 8, Just square)
  Nothing -> []


castleMoves :: Board -> Bool -> [ Move ]
castleMoves b chk = do
  side <- toCastleList $ b^.castleRightsByColour (b^.next)
  let kRays     = magic Rook (eKingPos b) (occupancy b)
      (f, t)    = kingCastleMove (b^.next) side
      rt        = toEnum $ (fromEnum f + fromEnum t) `div` 2
      checkSqrs = toList $ checkCastleBB (b^.next) side
  guard $ chk /= (fromSquare rt .&. kRays == mempty)
  guard $ (vacancyCastleBB (b^.next) side .&. occupancy b) == mempty
  guard $ not $ F.any (isAttacked b (b^.opponent)) checkSqrs
  return $ (castle .~ Just side) $ defaultMove f t King $ b^.next


vacancyCastleBB :: Colour -> Castle -> BitBoard
vacancyCastleBB White Long  = mconcat [ fromSquare sq | sq <- [(toSquare bFile firstRank) .. (toSquare dFile firstRank)]]
vacancyCastleBB White Short = mconcat [ fromSquare sq | sq <- [(toSquare fFile firstRank) .. (toSquare gFile firstRank)]]
vacancyCastleBB Black Long  = mconcat [ fromSquare sq | sq <- [(toSquare bFile eighthRank) .. (toSquare dFile eighthRank)]]
vacancyCastleBB Black Short = mconcat [ fromSquare sq | sq <- [(toSquare fFile eighthRank) .. (toSquare gFile eighthRank)]]
{-# INLINE vacancyCastleBB #-}


checkCastleBB :: Colour -> Castle -> BitBoard
checkCastleBB White Long  = mconcat [ fromSquare sq | sq <- [(toSquare cFile firstRank) .. (toSquare eFile firstRank)]]
checkCastleBB White Short = mconcat [ fromSquare sq | sq <- [(toSquare eFile firstRank) .. (toSquare gFile firstRank)]]
checkCastleBB Black Long  = mconcat [ fromSquare sq | sq <- [(toSquare cFile eighthRank) .. (toSquare eFile eighthRank)]]
checkCastleBB Black Short = mconcat [ fromSquare sq | sq <- [(toSquare eFile eighthRank) .. (toSquare gFile eighthRank)]]
{-# INLINE checkCastleBB #-}


kingCastleMove :: Colour -> Castle -> (Square, Square)
kingCastleMove White Long  = (toSquare eFile firstRank, toSquare cFile firstRank)
kingCastleMove White Short = (toSquare eFile firstRank, toSquare gFile firstRank)
kingCastleMove Black Long  = (toSquare eFile eighthRank, toSquare cFile eighthRank)
kingCastleMove Black Short = (toSquare eFile eighthRank, toSquare gFile eighthRank)
{-# INLINE kingCastleMove #-}


check :: Board -> Colour -> Bool -> Move -> Bool
check b c inc m = (inc || m^.piece == King) && inCheck (makeMoveSimplified m b) c
{-# INLINE check #-}


legal :: Board -> Move -> Bool
legal b m
  | isJust (m^.enPassantTarget) = not $ inCheck (makeMoveSimplified m b) (b^.next)
  | (m^.piece) == King          = not $ inCheck (makeMoveSimplified m b) (b^.next)
                                  -- >,-,-,-,-,-,-,-,-, isAttacked b (opponent' $ b^.next) (m^.to)
                                  -- >| |r| | | |K| | | is not correct as in this position g8
                                  -- >| | | | | | | | | is not attacked, therefore allows f8g8
                                  -- >| | |k| | | | | |
                                  -- >| | | | | | | | |
                                  -- >| | | | | | | | |
                                  -- >| | | | | | | | |
                                  -- >| | | | | | | | |
                                  -- >| | | | | | | | |
                                  -- >'-'-'-'-'-'-'-'-'
  | otherwise                   = let checkers = attackedFromBB b (occupancy b) (b^.opponent) (mKingPos b)
                                      pinCheck = (pinned b .&. fromSquare (m^.from)) == mempty -- not pinned
                                                 -- or pinned, but staying in line with the King
                                                 || lineBB (mKingPos b) (m^.to) .&. fromSquare (m^.from) /= mempty
                                  in case popCount checkers of
                                    0 -> pinCheck
                                    1 -> if m^.capturedPiece == Just Knight || m^.capturedPiece == Just Pawn
                                         then fromSquare (m^.to) == checkers && pinCheck -- Capture the checking piece
                                              -- >,-,-,-,-,-,-,-,-, the pinCheck above is nesecarry as in this position we
                                              -- >| | | | | |k|r|R| capture the checking piece, but we do that with a pinned
                                              -- >| | | | | | |Q| | piece (g8g7)
                                              -- >| | | | | | | | |
                                              -- >| | | | | | | | |
                                              -- >| | |K| | | | | |
                                              -- >| | | | | | | | |
                                              -- >| | | | | | | | |
                                              -- >| | | | | | | | |
                                              -- >'-'-'-'-'-'-'-'-'
                                         else lineBB (mKingPos b) (head $ toList checkers) .&. (fromSquare $ m^.to) /= mempty && pinCheck
                                    2 -> False -- Handled by the King case, only King moves can be legal
                                    _ -> error "the king is attacked by more then 2 pieces"

  
