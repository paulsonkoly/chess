module Chess.Move.GenMoves
       ( moves
       , forcingMoves
       , check
       , inCheck
       ) where

import           Data.Functor
import           Data.Monoid
import qualified Data.PQueue.Max as Q
import qualified Data.Foldable as F
import           Control.Lens hiding (to, from)
import           Control.Monad

import qualified Chess as C

import           Data.Square
import           Data.BitBoard
import           Data.ChessTypes
import           Chess.Move.Move
import           Chess.Move.ExecMove
import           Chess.Move.Attacks
import           Chess.Board


type MoveQueue = Q.MaxQueue Move

-- | Sequence of legal moves
moves :: Board -> [ Move ]
moves b = Q.toDescList
          $ Q.filter (not . check b (b^.next)) $ foldl1 Q.union [f b | f <- [ pawnMoves, regularMoves, castleMoves ]]


-- | Sequence of captures
forcingMoves :: Board -> [ Move ]
forcingMoves b = let cptr m = bit (m^.to) .&. occupancy b /= mempty
                     chk  m = check b (b^.opponent) m || inCheck b (b^.next)
                 in filter (\m -> cptr m || chk m) $ moves b


pawnMoves :: Board -> MoveQueue
pawnMoves b = pawnEnPassant b `Q.union` captures `Q.union` advances
  where promotable m = if m^.to <= 7 || m^.to >= 56
                       then do
                          promo <- [ C.Queen, C.Rook, C.Knight, C.Bishop ]
                          return $ (promotion .~ Just promo) m
                       else [ m ]
        conv l = Q.fromList $ concatMap promotable l
        captures = conv $ (\(f, t) -> (capturedPiece .~ pieceAt b t) $ defaultMove f t C.Pawn (b^.next)) <$> pawnCaptures b
        advances = conv $ (\(f, t) -> defaultMove f t C.Pawn (b^.next))                                  <$> pawnAdvances b


-- | Pawn captures (from, to) including promotions, excluding advances or en Passant
pawnCaptures :: Board -> [ (Square, Square) ]
pawnCaptures b = do
  let myPawns = myPiecesOf b C.Pawn
      -- files from which we can left/right capture
      cFiles 7 C.White = complement $ fileBB 0
      cFiles 9 C.White = complement $ fileBB 7
      cFiles 7 C.Black = complement $ fileBB 7
      cFiles 9 C.Black = complement $ fileBB 0      
  capture <- [7, 9]  -- left and right capture
  target  <- toList $ opponentsPieces b .&. ((myPawns .&. cFiles capture (b^.next)) `shift` direction (b^.next) capture)
  return (target - direction (b^.next) capture, target)


-- | Pawn advances (from, to) including promotions, excluding captures
pawnAdvances :: Board -> [ (Square, Square) ]
pawnAdvances b = do
  step <- [ 1, 2 ]
  let myPawns'  = myPiecesOf b C.Pawn
      myPawns   = if step == 1 then myPawns' else myPawns' .&. mySecond
      mySecond  = rankBB $ 31 - direction (b^.next) 23
      unblocked = complement $ foldl1 (<>) [ occupancy b `shift` direction (b^.next) (-8 * j) | j <- [ 1 .. step ] ]
  pawn <- toList $ myPawns .&. unblocked
  return (pawn, pawn + step * direction (b^.next) 8)
  

pawnEnPassant :: Board -> MoveQueue
pawnEnPassant b = Q.fromList $ flip (maybe []) (b^.enPassant) $ \square ->
  let bb = (bit (square + 1) .|. bit (square - 1)) .&. neighbourFilesBB (square .&. 7) .&. myPiecesOf b C.Pawn
  in do
    pawn <- toList bb
    return
      $ (enPassantTarget .~ Just square)
      $ defaultMove pawn (square + direction (b^.next) 8) C.Pawn $ b^.next


castleMoves :: Board -> MoveQueue
castleMoves b = Q.fromList $ do
  side <- b^.castleRightsByColour (b^.next)
  let (f, t)    = kingCastleMove (b^.next) side
      checkSqrs = toSeq $ checkCastleBB (b^.next) side
  guard $ (vacancyCastleBB (b^.next) side .&. occupancy b) == mempty
  guard $ not $ F.any (isAttacked b (b^.opponent)) checkSqrs
  return $ (castle .~ Just side) $ defaultMove f t C.King $ b^.next


-- | non pawn moves nor castles
regularMoves :: Board -> MoveQueue
regularMoves b = Q.fromList $ do
  pt     <- [ C.Knight, C.Bishop, C.Rook,  C.Queen, C.King ]
  move   <- attacking pt b (b^.next)
  target <- toList $ move^._2
  return $ (capturedPiece .~ pieceAt b target) $ defaultMove (move^._1) target pt $ b^.next


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


check :: Board -> C.Color -> Move -> Bool
check b c m = inCheck (makeMove m b) c


inCheck :: Board -> C.Color -> Bool
inCheck b c = let kP = head $ toList $ piecesOf b c C.King
              in isAttacked b (opponent' c) kP

