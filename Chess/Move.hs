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

import qualified Chess as C

import           Chess.Board
import           Chess.Magic
import           Data.BitBoard


data Move = Move
            { _from          :: Int
            , _to            :: Int
            , _piece         :: C.PieceType
            , _promotion     :: Maybe C.PieceType
            , _capturedPiece :: Maybe C.PieceType
            } deriving Show


$(makeLenses ''Move)


opponent :: C.Color -> C.Color
opponent C.Black = C.White
opponent C.White = C.Black


direction :: C.Color -> Int -> Int
direction C.White = id
direction C.Black = (* (-1))


isEnPassant :: Move -> Bool
isEnPassant m = m^.piece == C.Pawn && (m^.from .&. 7) == (m^.to .&. 7)


isDoubleAdvance :: Move -> Bool
isDoubleAdvance m = m^.piece == C.Pawn && (abs (m^.from - m^.to) > 1)


-- | Makes a @Move@ on a @Board@
doMoveM :: (MonadState Board m) => Move -> m ()
doMoveM m = do
  let fromBB = bit $ m^.from
      toBB   = bit $ m^.to
      ftBB   = fromBB `xor` toBB
  nxt <- use next
  piecesByColour nxt      %= xor ftBB
  piecesByType (m^.piece) %= xor ftBB
  case m^.capturedPiece of
    Just p -> do
      piecesByColour (opponent nxt) %= xor toBB
      piecesByType p %= xor toBB
    Nothing -> return ()
  case m^.promotion of
    Just p -> do
      pawns          %= xor toBB
      piecesByType p %= xor toBB
    Nothing -> return ()
  next      %= opponent
  enPassant %= ((:) $ if isDoubleAdvance m then Just (m^.to) else Nothing)


-- | Unmakes the @Move@ on a @Board@
undoMoveM :: (MonadState Board m) => Move -> m ()
undoMoveM m = do
  let fromBB = bit $ m^.from
      toBB   = bit $ m^.to
      ftBB   = fromBB `xor` toBB
  next %= opponent
  nxt <- use next
  piecesByColour nxt      %= xor ftBB
  piecesByType (m^.piece) %= xor ftBB
  case m^.capturedPiece of
    Just p -> do
      piecesByColour (opponent nxt) %= xor toBB
      piecesByType p %= xor toBB
    Nothing -> return ()
  case m^.promotion of
    Just p -> do
      pawns          %= xor fromBB
      piecesByType p %= xor fromBB
    Nothing -> return ()
  enPassant %= tail


moves :: Magic -> Magic -> Board -> [ Move ]
moves bishopMagics rookMagics b = pawnMoves b ++ regularMoves b bishopMagics rookMagics


pawnMoves :: Board -> [ Move ]
pawnMoves b = concatMap (promotions . setCapture)
              [ Move f t C.Pawn Nothing Nothing | (f, t) <- pawnCaptures b ++ pawnAdvances b ] ++ pawnEnPassant b
  where setCapture m = (capturedPiece .~ pieceAt b (m^.to)) m
        promotions m = if m^.to <= 7 || m^.to >= 58
                       then [ (promotion .~ Just promo) m | promo <- [  C.Queen, C.Rook, C.Knight, C.Bishop ]]
                       else [ m ]


-- | Pawn captures (from, to) including promotions, excluding advances or en Passant
pawnCaptures :: Board -> [ (Int, Int) ]
pawnCaptures b = do
  let myPawns = piecesOf b (b^.next) C.Pawn
      -- files from which we can left/right capture
      cFiles cap = complement $ fileBB $ 3 - (8 - cap) * direction (b^.next) 4 
  capture <- [ 7, 9 ] -- left and right capture
  target  <- toList $ opponentsPieces b .&. ((myPawns .&. cFiles capture) `shift` direction (b^.next) capture)
  return (target - direction (b^.next) capture, target)


-- | Pawn advances (from, to) including promotions, excluding captures
pawnAdvances :: Board -> [ (Int, Int) ]
pawnAdvances b = do
  step <- [ 1, 2 ]
  let myPawns'  = piecesOf b (b^.next) C.Pawn
      myPawns   = if step == 1 then myPawns' else myPawns' .&. mySecond
      mySecond  = rankBB $ 31 - direction (b^.next) 23
      unblocked = complement $ foldl1 (<>) [ occupancy b `shift` direction (b^.next) (-8 * j) | j <- [ 1 .. step ] ]
  pawn <- toList $ myPawns .&. unblocked
  return (pawn, pawn + step * direction (b^.next) 8)
  

pawnEnPassant :: Board -> [ Move ]
pawnEnPassant b =
  case join $ listToMaybe $ b^.enPassant of
      Just square -> let bb = (bit (square + 1) .|. bit (square - 1))
                              .&. neighbourFilesBB (square .&. 7)
                              .&. piecesOf b (b^.next) C.Pawn
                     in [ Move pawn square C.Pawn Nothing $ Just C.Pawn | pawn <- toList bb ]
      Nothing -> []


-- | non pawn moves nor castles
regularMoves :: Board -> Magic -> Magic -> [ Move ]
regularMoves b bishopMagics rookMagics = do
  pt     <- [ C.Knight, C.Bishop, C.Rook, C.Queen, C.King ]
  move   <- attacking pt b bishopMagics rookMagics (b^.next)
  target <- toList $ move^._2
  return $ Move (move^._1) target pt Nothing (pieceAt b target)
  
  
-- | the bitboard & the piece position that the given piece type attacks with the given colour
attacking :: C.PieceType -> Board -> Magic -> Magic -> C.Color -> [ (Int, BitBoard) ]
attacking pt b bishopMagics rookMagics colour =
  let pcs     = toList $ piecesOf b colour pt
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
    _      -> map att pcs


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

