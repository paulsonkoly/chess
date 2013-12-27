{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.Move
   ( Move
   -- * Constructor
   , moves
   -- * Lenses
   , from
   , to
   , piece
   , promotion
   , capturedPiece
   , enPassantTarget
   , castle
   -- * Stateful do move
   , doMoveM
   , undoMoveM
   -- * Parser
   , parserMove
   -- * utils
   , direction
   , checkMate
   )
   where

import           Control.Lens hiding (from, to, op, at, (|>))
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence 
import qualified Data.Sequence as S (filter, null)
import           Data.List
import           Data.Functor
import qualified Data.Foldable as F

import           Text.ParserCombinators.Parsec

import qualified Chess as C

import           Chess.Board
import           Chess.Magic
import           Data.BitBoard
import           Data.Square
import           Control.Extras

data Move = Move
            { _from            :: ! Int
            , _to              :: ! Int
            , _piece           :: ! C.PieceType
            , _promotion       :: ! (Maybe C.PieceType)
            , _capturedPiece   :: ! (Maybe C.PieceType)
            , _enPassantTarget :: ! (Maybe Int)
            , _castle          :: ! (Maybe Castle)
            } deriving Show


defaultMove :: Int -> Int -> C.PieceType -> Move
defaultMove f t pt = Move f t pt Nothing Nothing Nothing Nothing


$(makeLenses ''Move)


direction :: C.Color -> Int -> Int
direction C.White = id
direction C.Black = (* (-1))
{-# INLINE direction #-}


isDoubleAdvance :: Move -> Bool
isDoubleAdvance m = m^.piece == C.Pawn && (9 < abs (m^.from - m^.to))
{-# INLINE isDoubleAdvance #-}


fromBB :: Move -> BitBoard
fromBB = bit . (^.from)
{-# INLINE fromBB #-}


toBB :: Move -> BitBoard
toBB = bit . (^.to)
{-# INLINE toBB #-}


putPiece :: BitBoard -> C.Color -> C.PieceType -> Board -> Board 
putPiece bb c pt = (piecesByColour c %~ xor bb) . (piecesByType pt %~ xor bb)
{-# INLINE putPiece #-}


promote :: BitBoard -> C.PieceType -> Board -> Board
promote bb pt = (pawns %~ xor bb) . (piecesByType pt %~ xor bb)
{-# INLINE promote #-}


rookCaslteBB :: C.Color -> Castle -> BitBoard
rookCaslteBB C.White Long  = fromPositionList [ 0,  3  ]
rookCaslteBB C.White Short = fromPositionList [ 5,  7  ]
rookCaslteBB C.Black Long  = fromPositionList [ 56, 59 ]
rookCaslteBB C.Black Short = fromPositionList [ 61, 63 ]
{-# INLINE rookCaslteBB #-}


vacancyCastleBB :: C.Color -> Castle -> BitBoard
vacancyCastleBB C.White Long  = fromPositionList [ 1 .. 3 ]
vacancyCastleBB C.White Short = fromPositionList [ 5, 6 ]
vacancyCastleBB C.Black Long  = fromPositionList [ 57 .. 59 ]
vacancyCastleBB C.Black Short = fromPositionList [ 61, 62 ]
{-# INLINE vacancyCastleBB #-}


checkCastleBB :: C.Color -> Castle -> BitBoard
checkCastleBB C.White Long  = fromPositionList [ 2 .. 4 ]
checkCastleBB C.White Short = fromPositionList [ 4 .. 6 ]
checkCastleBB C.Black Long  = fromPositionList [ 58 .. 60 ]
checkCastleBB C.Black Short = fromPositionList [ 60 .. 62 ]
{-# INLINE checkCastleBB #-}


kingCastleMove :: C.Color -> Castle -> (Int, Int)
kingCastleMove C.White Long  = (4, 2)
kingCastleMove C.White Short = (4, 6)
kingCastleMove C.Black Long  = (60, 58)
kingCastleMove C.Black Short = (60, 62)
{-# INLINE kingCastleMove #-}


castleRights :: Move -> [ Castle ]
castleRights m
  | m^.piece == C.King                               = []
  | (m^.piece == C.Rook) && (((m^.from) .&. 7) == 0) = [ Short ]
  | (m^.piece == C.Rook) && (((m^.from) .&. 7) == 7) = [ Long  ]
  | otherwise                                        = [ Short, Long ]
{-# INLINE castleRights #-}


flipMoveM :: (MonadState Board m) => Move -> m ()
flipMoveM m = do
  let (f, t) = (fromBB m, toBB m)
      ft     = f `xor` t
  nxt <- use next
  modify (putPiece ft nxt (m^.piece))
  doOnJust (m^.capturedPiece)   $ \c -> modify $ putPiece t (opponent' nxt) c
  doOnJust (m^.promotion)       $ \p -> modify $ promote t p
  doOnJust (m^.enPassantTarget) $ \e -> modify $ putPiece (bit e) (opponent' nxt) C.Pawn
  doOnJust (m^.castle)          $ \c -> modify $ putPiece (rookCaslteBB nxt c) nxt C.Rook


-- | makes a @Move@ on a @Board@
doMoveM :: (MonadState Board m) => Move -> m ()
doMoveM m = do
  flipMoveM m
  nxt <- use next
  castleRightsByColour nxt %= (\prev -> (head prev `intersect` castleRights m) : prev)
  next                     %= opponent'
  enPassant                %= ((:) $ if isDoubleAdvance m then Just (m^.to) else Nothing)


-- | Unmakes the @Move@ on a @Board@
undoMoveM :: (MonadState Board m) => Move -> m ()
undoMoveM m = do
  next                     %= opponent'
  flipMoveM m
  enPassant                %= tail
  nxt <- use next
  castleRightsByColour nxt %= tail


check :: Board -> C.Color -> Bool
check b c = let kP = head $ toList $ piecesOf b c C.King
            in isAttacked b (opponent' c) kP


-- | Sequence of legal moves
moves :: Board -> Seq Move
moves b = S.filter (not . check') $ pawnMoves b >< regularMoves b >< castleMoves b
  where check' m = let b' = execState (doMoveM m) b
                   in check b' (b^.next)


checkMate :: Board -> Bool
checkMate b = check b (b^.next) && S.null (moves b)


pawnMoves :: Board -> Seq Move
pawnMoves b = pawnEnPassant b >< F.foldl (><) empty (promotable <$> captures >< advances)
  where promotable m = if m^.to <= 7 || m^.to >= 58
                       then do
                          promo <- singleton C.Queen |> C.Rook |> C.Knight |> C.Bishop
                          return $ (promotion .~ Just promo) m
                       else singleton m
        captures = (\(f, t) -> (capturedPiece .~ pieceAt b t) $ defaultMove f t C.Pawn) <$> pawnCaptures b
        advances = (\(f, t) -> defaultMove f t C.Pawn)                                  <$> pawnAdvances b


-- | Pawn captures (from, to) including promotions, excluding advances or en Passant
pawnCaptures :: Board -> Seq (Int, Int)
pawnCaptures b = do
  let myPawns = myPiecesOf b C.Pawn
      -- files from which we can left/right capture
      cFiles 7 C.White = complement $ fileBB 0
      cFiles 9 C.White = complement $ fileBB 7
      cFiles 7 C.Black = complement $ fileBB 7
      cFiles 9 C.Black = complement $ fileBB 0      
  capture <- singleton 7 |> 9  -- left and right capture
  target  <- toSeq $ opponentsPieces b .&. ((myPawns .&. cFiles capture (b^.next)) `shift` direction (b^.next) capture)
  return (target - direction (b^.next) capture, target)


-- | Pawn advances (from, to) including promotions, excluding captures
pawnAdvances :: Board -> Seq (Int, Int)
pawnAdvances b = do
  step <- singleton 1 |> 2
  let myPawns'  = myPiecesOf b C.Pawn
      myPawns   = if step == 1 then myPawns' else myPawns' .&. mySecond
      mySecond  = rankBB $ 31 - direction (b^.next) 23
      unblocked = complement $ foldl1 (<>) [ occupancy b `shift` direction (b^.next) (-8 * j) | j <- [ 1 .. step ] ]
  pawn <- toSeq $ myPawns .&. unblocked
  return (pawn, pawn + step * direction (b^.next) 8)
  

pawnEnPassant :: Board -> Seq Move
pawnEnPassant b = flip (maybe empty) (join $ listToMaybe $ b^.enPassant) $ \square ->
  let bb = (bit (square + 1) .|. bit (square - 1)) .&. neighbourFilesBB (square .&. 7) .&. myPiecesOf b C.Pawn
  in do
    pawn <- toSeq bb
    return
      $ (enPassantTarget .~ Just square)
      $ defaultMove pawn (square + direction (b^.next) 8) C.Pawn


castleMoves :: Board -> Seq Move
castleMoves b = do
  side <- fromList $ head $ b^.castleRightsByColour (b^.next)
  let (f, t)    = kingCastleMove (b^.next) side
      checkSqrs = toSeq $ checkCastleBB (b^.next) side
  guard $ (vacancyCastleBB (b^.next) side .&. occupancy b) == mempty
  guard $ not $ F.any (isAttacked b (b^.opponent)) checkSqrs
  return $ (castle .~ Just side) $ defaultMove f t C.King


-- | non pawn moves nor castles
regularMoves :: Board -> Seq Move
regularMoves b = do
  pt     <- singleton C.Knight |> C.Bishop |> C.Rook |>  C.Queen |> C.King
  move   <- attacking pt b (b^.next)
  target <- toSeq $ move^._2
  return $ (capturedPiece .~ pieceAt b target) $ defaultMove (move^._1) target pt

bishopMagics :: Magic
bishopMagics = makeMagic C.Bishop
rookMagics :: Magic
rookMagics   = makeMagic C.Rook
  
  
-- | the bitboard & the piece position that the given piece type attacks with the given colour
attacking :: C.PieceType -> Board -> C.Color -> Seq (Int, BitBoard)
attacking pt b colour =
  let pcs     = toSeq $ piecesOf b colour pt
      notme   = complement $ b^.piecesByColour colour
      att pos = (pos, notme .&. m pos)
      m pos   = case pt of
        C.Queen  -> magic bishopMagics pos (occupancy b) .|. magic rookMagics pos (occupancy b)
        C.Rook   -> magic rookMagics pos (occupancy b)
        C.Bishop -> magic bishopMagics pos (occupancy b)
        C.Knight -> knightAttackBB pos
        C.King   -> kingAttackBB pos
        C.Pawn   -> undefined
  in case pt of
    C.Pawn -> undefined
    _      -> att <$> pcs


-- | the bitboard from where the piece type of the given colour is attacking the specified position
attackedBy :: C.PieceType -> Board -> C.Color -> Int -> BitBoard
attackedBy C.Queen b colour pos
  = (magic bishopMagics pos (occupancy b) .|. magic rookMagics pos (occupancy b)) .&. piecesOf b colour C.Queen

attackedBy C.Bishop b colour pos
  = magic bishopMagics pos (occupancy b) .&. piecesOf b colour C.Bishop

attackedBy C.Rook b colour pos
  = magic rookMagics pos (occupancy b) .&. piecesOf b colour C.Rook

attackedBy C.Knight b colour pos = knightAttackBB pos .&. piecesOf b colour C.Knight

attackedBy C.King b colour pos   = kingAttackBB pos  .&. piecesOf b colour C.King
-- TODO : en passant
attackedBy C.Pawn b colour pos   = let mask  = neighbourFilesBB (pos .&. 7) .&. piecesOf b colour C.Pawn
                                   in (bit (pos - direction colour 7) .|. bit (pos - direction colour 9)) .&. mask


-- | are any of the given player's pieces attacking the given square?
-- isAttacked :: Board -> C.Color -> Int -> Bool
isAttacked :: Board -> C.Color -> Int -> Bool
isAttacked b colour pos = any (/= mempty)
                          [ attackedBy pt b colour pos
                          | pt <- [ C.Queen, C.Bishop, C.Rook, C.Knight, C.King, C.Pawn ]
                          ]


-- | On a given board parses an UCI protocol style move notation into Move
parserMove :: Board -> Parser Move
parserMove b = do
  f <- parserSquare
  t <- parserSquare
  promotionCh <- optionMaybe $ oneOf "qrbn"
  let promo = charToPt <$> promotionCh
      enp   = if Just C.Pawn == pieceAt b f && isNothing (pieceAt b t) && abs (f - t) .&. 7 /= 0
              then head $ b^.enPassant
              else Nothing
      cstl  = if Just C.King == pieceAt b f && abs (f - t) == 2
              then Just $ if f - t == 2 then Long else Short
              else Nothing
  return
    $ (promotion .~ promo)
    $ (capturedPiece .~ pieceAt b t)
    $ (enPassantTarget .~ enp)
    $ (castle .~ cstl)
    $ defaultMove f t (fromJust $ pieceAt b f)
  where
    charToPt 'q' = C.Queen
    charToPt 'r' = C.Rook
    charToPt 'b' = C.Bishop
    charToPt 'n' = C.Knight
