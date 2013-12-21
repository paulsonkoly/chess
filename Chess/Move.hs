{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.Move
   ( doMoveM
   , undoMoveM
   , moves
   )
   where

import           Control.Lens hiding (from, to, op, at, (|>))
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence 
import qualified Data.Sequence as S (filter)
import           Data.Functor
import qualified Data.Foldable as F

import qualified Chess as C

import           Chess.Board
import           Chess.Magic
import           Data.BitBoard

data Move = Move
            { _from            :: ! Int
            , _to              :: ! Int
            , _piece           :: ! C.PieceType
            , _promotion       :: ! (Maybe C.PieceType)
            , _capturedPiece   :: ! (Maybe C.PieceType)
            , _enPassantTarget :: ! (Maybe Int)
            } deriving Show


defaultMove :: Int -> Int -> C.PieceType -> Move
defaultMove f t pt = Move f t pt Nothing Nothing Nothing


$(makeLenses ''Move)


opponent :: C.Color -> C.Color
opponent C.Black = C.White
opponent C.White = C.Black
{-# INLINE opponent #-}


direction :: C.Color -> Int -> Int
direction C.White = id
direction C.Black = (* (-1))
{-# INLINE direction #-}


isDoubleAdvance :: Move -> Bool
isDoubleAdvance m = m^.piece == C.Pawn && (8 < abs (m^.from - m^.to))
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


doOnJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
doOnJust = flip (maybe (return ()))
{-# INLINE doOnJust #-}


-- | makes a @Move@ on a @Board@
doMoveM :: (MonadState Board m) => Move -> m ()
doMoveM m = do
  let (f, t) = (fromBB m, toBB m)
      ft     = f `xor` t
  nxt <- use next
  modify (putPiece ft nxt (m^.piece))
  doOnJust (m^.capturedPiece)   $ \c -> modify $ putPiece t (opponent nxt) c
  doOnJust (m^.promotion)       $ \p -> modify $ promote t p
  doOnJust (m^.enPassantTarget) $ \e -> modify $ putPiece (bit e) (opponent nxt) C.Pawn
  next      %= opponent
  enPassant %= ((:) $ if isDoubleAdvance m then Just (m^.to) else Nothing)


-- | Unmakes the @Move@ on a @Board@
undoMoveM :: (MonadState Board m) => Move -> m ()
undoMoveM m = do
  let (f, t) = (fromBB m, toBB m)
      ft     = f `xor` t
  next %= opponent
  nxt <- use next
  modify (putPiece ft nxt (m^.piece))
  doOnJust (m^.capturedPiece)   $ \c -> modify $ putPiece t (opponent nxt) c
  doOnJust (m^.promotion)       $ \p -> modify $ promote f p
  doOnJust (m^.enPassantTarget) $ \e -> modify $ putPiece (bit e) (opponent nxt) C.Pawn
  enPassant %= tail


-- | Sequence of legal moves
moves :: Magic -> Magic -> Board -> Seq Move
moves bishopMagics rookMagics b = S.filter (not . check)
                                  $ pawnMoves b >< regularMoves b bishopMagics rookMagics
  where check m = let bb   = fromBB m `xor` toBB m
                      b'   = putPiece bb (b^.next) (m^.piece) b
                      myKP = head $ toList $ piecesOf b' (b'^.next) C.King
                  in isAttacked b' bishopMagics rookMagics (opponent $ b^.next) myKP



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
  let myPawns = piecesOf b (b^.next) C.Pawn
      -- files from which we can left/right capture
      cFiles cap = complement $ fileBB $ 3 - (8 - cap) * direction (b^.next) 4 
  capture <- singleton 7 |> 9  -- left and right capture
  target  <- toSeq $ opponentsPieces b .&. ((myPawns .&. cFiles capture) `shift` direction (b^.next) capture)
  return (target - direction (b^.next) capture, target)


-- | Pawn advances (from, to) including promotions, excluding captures
pawnAdvances :: Board -> Seq (Int, Int)
pawnAdvances b = do
  step <- singleton 1 |> 2
  let myPawns'  = piecesOf b (b^.next) C.Pawn
      myPawns   = if step == 1 then myPawns' else myPawns' .&. mySecond
      mySecond  = rankBB $ 31 - direction (b^.next) 23
      unblocked = complement $ foldl1 (<>) [ occupancy b `shift` direction (b^.next) (-8 * j) | j <- [ 1 .. step ] ]
  pawn <- toSeq $ myPawns .&. unblocked
  return (pawn, pawn + step * direction (b^.next) 8)
  

pawnEnPassant :: Board -> Seq Move
pawnEnPassant b = case join $ listToMaybe $ b^.enPassant of
  Just square -> let bb = (bit (square + 1) .|. bit (square - 1))
                          .&. neighbourFilesBB (square .&. 7)
                          .&. opponentsPiecesOf b C.Pawn
                 in do
                   pawn <- toSeq bb
                   return
                     $ (capturedPiece   .~ Just C.Pawn)
                     $ (enPassantTarget .~ Just pawn)
                     $ defaultMove square (pawn + direction (b^.next) 8)  C.Pawn
  Nothing -> empty


-- | non pawn moves nor castles
regularMoves :: Board -> Magic -> Magic -> Seq Move
regularMoves b bishopMagics rookMagics = do
  pt     <- singleton C.Knight |> C.Bishop |> C.Rook |>  C.Queen |> C.King
  move   <- attacking pt b bishopMagics rookMagics (b^.next)
  target <- toSeq $ move^._2
  return $ (capturedPiece .~ pieceAt b target) $ defaultMove (move^._1) target pt
  
  
-- | the bitboard & the piece position that the given piece type attacks with the given colour
attacking :: C.PieceType -> Board -> Magic -> Magic -> C.Color -> Seq (Int, BitBoard)
attacking pt b bishopMagics rookMagics colour =
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
attackedBy :: C.PieceType -> Board -> Magic -> Magic -> C.Color -> Int -> BitBoard
attackedBy C.Queen b bishopMagics rookMagics colour pos
  = (magic bishopMagics pos (occupancy b) .|. magic rookMagics pos (occupancy b)) .&. piecesOf b colour C.Queen

attackedBy C.Bishop b bishopMagics _ colour pos
  = magic bishopMagics pos (occupancy b) .&. piecesOf b colour C.Bishop

attackedBy C.Rook b _ rookMagics colour pos
  = magic rookMagics pos (occupancy b) .&. piecesOf b colour C.Rook

attackedBy C.Knight b _ _ colour pos = knightAttackBB pos .&. piecesOf b colour C.Knight

attackedBy C.King b _ _ colour pos   = kingAttackBB pos  .&. piecesOf b colour C.King
-- TODO : en passant
attackedBy C.Pawn b _ _ colour pos   = let mask  = neighbourFilesBB (pos .&. 7) .&. piecesOf b colour C.Pawn
                                       in (bit (pos + direction colour 7) .|. bit (pos + direction colour 9)) .&. mask


-- | are any of the given player's pieces attacking the given square?
-- isAttacked :: Board -> C.Color -> Int -> Bool
isAttacked :: Board -> Magic -> Magic -> C.Color -> Int -> Bool
isAttacked b bishopMagics rookMagics colour pos = any (/= mempty)
                          [ attackedBy pt b bishopMagics rookMagics colour pos
                          | pt <- [ C.Queen, C.Bishop, C.Rook, C.Knight, C.King, C.Pawn ]
                          ]

