module Chess.Move.GenMoves
       ( moves
       , forcingMoves
       , anyMove
       , check
       , inCheck
       ) where

import           Data.Monoid
import qualified Data.Foldable as F
import           Data.Maybe
import           Data.List
import           Control.Lens hiding (to, from)
import           Control.Monad

import qualified Chess as C

import           Data.Square
import           Data.BitBoard
import           Chess.Magic
import           Data.ChessTypes
import           Chess.Move.Move
import           Chess.Move.ExecMove
import           Chess.Board.Attacks
import           Chess.Board


-- | Legal moves
moves :: Board -> [ Move ]
moves b = let cs = nub (simpleChecks b ++ discoveredChecks b) -- the nub is not strictly nesecarry, but outherwise we
                   ++ pawnSimpleChecks b                      -- would break perft with double checks.
                   ++ pawnDiscoveredChecks b
                   ++ castleChecks b
              ps = pawnCaptures b ++ pawnPromotions b ++ captures b ++ pawnEnPassants b
              ts = castleQuiet b
              nq = quietMoves b ++ pawnQuietMoves b
          in filter (not . check b (b^.next)) $ cs ++ (ps \\ cs) ++ ts ++ (nq \\ cs)


-- | Checks, captures and promotions
forcingMoves :: Board -> [ Move ]
forcingMoves b = let ms = simpleChecks b
                          ++ discoveredChecks b
                          ++ pawnSimpleChecks b
                          ++ pawnDiscoveredChecks b
                          ++ castleChecks b
                          ++ pawnCaptures b
                          ++ pawnPromotions b
                          ++ captures b
                          ++ pawnEnPassants b
                 in filter (not . check b (b^.next)) ms

-- | Is there a legal move on the Board?
anyMove :: Board -> Bool
anyMove b = F.any (not . null) [ pawnCaptureSquares b , pawnEnPassantSquares b , pawnAdvanceSquares b]
            || F.any (not . null) [ captures b , castleChecks b , castleQuiet b , quietMoves b]


-- | enemy king position
eKingPos :: Board -> Square
eKingPos b = head $ toList $ piecesOf b (b^.opponent) C.King


-- | knight, bishop, rook, queen simple checks - not discovered check
simpleChecks :: Board -> [ Move ]
simpleChecks b = do
  pt <- [ C.Queen, C.Rook, C.Bishop, C.Knight ]
  
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
    moveValid C.Knight f t = let hd = hDist f t
                                 vd = vDist f t
                             in (hd == 1 && vd == 2) || (hd == 2 && vd == 1) 
    moveValid C.Bishop f t = hDist f t == vDist f t
    moveValid C.Rook   f t = hDist f t == 0 || vDist f t == 0
    moveValid C.Queen  f t = moveValid C.Rook f t || moveValid C.Bishop f t
    moveValid _        _ _ = error "unexpected piece type"
    

-- | discovered checks (with any piece type except pawns)
discoveredChecks :: Board -> [ Move ]
discoveredChecks b = do
  pt <- [ C.Queen, C.Rook, C.Bishop, C.Knight, C.King ]
  let ps = piecesOf b (b^.next) pt .&. discoverer b
  -- ps should be mempty most of the time..
  f <- toList ps
  t <- toList $ moveFun b pt f .&. complement (myPieces b)
  return
    $ (capturedPiece .~ pieceAt b t)
    $ defaultMove f t pt (b^.next)


castleChecks :: Board -> [ Move ]
castleChecks = flip castleMoves True


pawnDiscoveredChecks :: Board -> [ Move ]
pawnDiscoveredChecks b = pawnMoves b (\f _ -> fromSquare f .&. discoverer b /= mempty)


pawnSimpleChecks :: Board -> [ Move ]
pawnSimpleChecks b = let kp = [ offset (eKingPos b) (direction (b^.opponent) lr) | lr <- [ 7, 9] ]
                     in  pawnMoves b (\_ t -> F.any (== t) kp)


pawnPromotions :: Board -> [ Move ]
pawnPromotions b = let opSecond = rankBB $ case b^.next of
                         C.White -> seventhRank
                         C.Black -> secondRank
                   in pawnMoves b (\f _ -> fromSquare f .&. opSecond /= mempty)


pawnCaptures :: Board -> [ Move ]
pawnCaptures b = sortBy heuristics $ pawnMoves b (\_ t -> (fromSquare t .&. opponentsPieces b) /= mempty)
  where heuristics x y = pieceValue (fromJust $ y^.capturedPiece) `compare` pieceValue (fromJust $ x^.capturedPiece)

pawnEnPassants :: Board -> [ Move ]
pawnEnPassants b = do
  (f, t, enp) <- pawnEnPassantSquares b
  return
    $ (enPassantTarget .~ enp)
    $ defaultMove f t C.Pawn (b^.next)


-- | pieces that can give discovered checks
discoverer :: Board -> BitBoard
discoverer b = mconcat $ do
  pt <- [ C.Rook, C.Bishop, C.Queen ]
  let
    -- King casting rays ..
    eKingRay = magic pt (eKingPos b) (occupancy b) .&. myPieces b
    -- Now cast rays from my ray casters
    pieceRay = mconcat $ do
      p <- toList $ piecesOf b (b^.next) pt
      return $ magic pt p (occupancy b)
  -- where the 2 rays meet ..
  return $ eKingRay .&. pieceRay


captures :: Board -> [ Move ]
captures b = sortBy heuristics $ normMoveGen b (opponentsPieces b)
  where heuristics x y = pieceValue (fromJust $ y^.capturedPiece) `compare` pieceValue (fromJust $ x^.capturedPiece)


quietMoves :: Board -> [ Move ]
quietMoves b = sortBy heuristics $ normMoveGen b (vacated b)
  where heuristics x y = moveValue y `compare` moveValue x


castleQuiet :: Board -> [ Move ]
castleQuiet b = castleMoves b False


pawnQuietMoves :: Board -> [ Move ]
pawnQuietMoves b = sortBy heuristics $ do
  (f, t, _) <- pawnAdvanceSquares b
  concatMap promote $ return $ defaultMove f t C.Pawn (b^.next)
  where heuristics x y = moveValue y `compare` moveValue x


-- | captures or quiet moves
normMoveGen :: Board -> BitBoard -> [ Move ]
normMoveGen b filt = do
  pt <- [ C.Queen, C.Rook, C.Bishop, C.Knight, C.King ]
  let ps = piecesOf b (b^.next) pt
  f <- toList ps
  t <- toList $ moveFun b pt f .&. filt
  return
    $ (capturedPiece .~ pieceAt b t)
    $ defaultMove f t pt (b^.next)


moveFun :: Board -> C.PieceType -> Square -> BitBoard
moveFun b pt
  | pt == C.Knight = knightAttackBB
  | pt == C.King   = kingAttackBB
  | pt == C.Pawn   = error "moveFun is not implemented for Pawns"
  | otherwise      = \t -> magic pt t (occupancy b)


pawnMoves :: Board -> (Square -> Square -> Bool) -> [ Move ]
pawnMoves b fun = do
  (f, t, enp) <- pawnAdvanceSquares b ++ pawnCaptureSquares b ++ pawnEnPassantSquares b
  guard $ fun f t
  map (enPassantTarget .~ enp) $ promote
    $ (capturedPiece .~ pieceAt b t)
    $ defaultMove f t C.Pawn (b^.next)


promote :: Move -> [ Move ]
promote m = if fromSquare (m^.to) .&. (rankBB firstRank .|. rankBB eighthRank) /= mempty
            then do
              promo <- [ C.Queen, C.Rook, C.Knight, C.Bishop ]
              return $ (promotion .~ Just promo) m
            else [ m ]


-- | Pawn captures (from, to) including promotions, excluding advances or en Passant
pawnCaptureSquares :: Board -> [ (Square, Square, Maybe Square) ]
pawnCaptureSquares b = do
  let myPawns = myPiecesOf b C.Pawn
      -- files from which we can left/right capture
      cFiles 7 C.White = complement $ fileBB aFile
      cFiles 9 C.White = complement $ fileBB hFile
      cFiles 7 C.Black = complement $ fileBB hFile
      cFiles 9 C.Black = complement $ fileBB aFile
      cFiles _ _       = error "Unexpected numbers"
  capture <- [7, 9]  -- left and right capture
  target  <- toList $ opponentsPieces b .&. ((myPawns .&. cFiles capture (b^.next)) `shift` direction (b^.next) capture)
  return (offset target (direction (b^.opponent) capture), target, Nothing)


-- | Pawn advances (from, to) including promotions, excluding captures
pawnAdvanceSquares :: Board -> [ (Square, Square, Maybe Square) ]
pawnAdvanceSquares b = do
  step <- [ 1, 2 ]
  let myPawns'  = myPiecesOf b C.Pawn
      myPawns   = if step == 1 then myPawns' else myPawns' .&. mySecond
      mySecond  = rankBB $ if b^.next == C.White then secondRank else seventhRank
      unblocked = complement $ foldl1 (<>) [ occupancy b `shift` direction (b^.next) (-8 * j) | j <- [ 1 .. step ] ]
  pawn <- toList $ myPawns .&. unblocked
  return (pawn, offset pawn $ step * direction (b^.next) 8, Nothing)
  

pawnEnPassantSquares :: Board -> [ (Square, Square, Maybe Square) ]
pawnEnPassantSquares b = case b^.enPassant of
  Just square -> do
    pawn <- toList $ (fromSquare (offset square 1) .|. fromSquare (offset square (-1)))
            .&. neighbourFilesBB (file square)
            .&. myPiecesOf b C.Pawn
    return (pawn, offset square $ direction (b^.next) 8, Just square)
  Nothing -> []


castleMoves :: Board -> Bool -> [ Move ]
castleMoves b chk = do
  side <- toCastleList $ b^.castleRightsByColour (b^.next)
  let kRays     = magic C.Rook (eKingPos b) (occupancy b)
      (f, t)    = kingCastleMove (b^.next) side
      rt        = toEnum $ (fromEnum f + fromEnum t) `div` 2
      checkSqrs = toList $ checkCastleBB (b^.next) side
  when chk $ guard $ fromSquare rt .&. kRays /= mempty
  guard $ (vacancyCastleBB (b^.next) side .&. occupancy b) == mempty
  guard $ not $ F.any (isAttacked b (b^.opponent)) checkSqrs
  return $ (castle .~ Just side) $ defaultMove f t C.King $ b^.next
  

vacancyCastleBB :: C.Color -> Castle -> BitBoard
vacancyCastleBB C.White Long  = mconcat [ fromSquare sq | sq <- [(toSquare bFile firstRank) .. (toSquare dFile firstRank)]]
vacancyCastleBB C.White Short = mconcat [ fromSquare sq | sq <- [(toSquare fFile firstRank) .. (toSquare gFile firstRank)]]
vacancyCastleBB C.Black Long  = mconcat [ fromSquare sq | sq <- [(toSquare bFile eighthRank) .. (toSquare dFile eighthRank)]]
vacancyCastleBB C.Black Short = mconcat [ fromSquare sq | sq <- [(toSquare fFile eighthRank) .. (toSquare gFile eighthRank)]]
{-# INLINE vacancyCastleBB #-}


checkCastleBB :: C.Color -> Castle -> BitBoard
checkCastleBB C.White Long  = mconcat [ fromSquare sq | sq <- [(toSquare cFile firstRank) .. (toSquare eFile firstRank)]]
checkCastleBB C.White Short = mconcat [ fromSquare sq | sq <- [(toSquare eFile firstRank) .. (toSquare gFile firstRank)]]
checkCastleBB C.Black Long  = mconcat [ fromSquare sq | sq <- [(toSquare cFile eighthRank) .. (toSquare eFile eighthRank)]]
checkCastleBB C.Black Short = mconcat [ fromSquare sq | sq <- [(toSquare eFile eighthRank) .. (toSquare gFile eighthRank)]]
{-# INLINE checkCastleBB #-}


kingCastleMove :: C.Color -> Castle -> (Square, Square)
kingCastleMove C.White Long  = (toSquare eFile firstRank, toSquare cFile firstRank)
kingCastleMove C.White Short = (toSquare eFile firstRank, toSquare gFile firstRank)
kingCastleMove C.Black Long  = (toSquare eFile eighthRank, toSquare cFile eighthRank)
kingCastleMove C.Black Short = (toSquare eFile eighthRank, toSquare gFile eighthRank)
{-# INLINE kingCastleMove #-}


check :: Board -> C.Color -> Move -> Bool
check b c m = inCheck (makeMove m b) c



