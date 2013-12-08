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
import           Data.Sequence

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


opp :: C.Color -> C.Color
opp C.Black = C.White
opp C.White = C.Black


-- | Makes a @Move@ on a @Board@
doMoveM :: (MonadState Board m) => Move -> m ()
doMoveM m = do
   let
      f  = bit $ m^.from
      t  = bit $ m^.to
      ft = f `xor` t
   c <- liftM (view next) get
   assign next $ opp c
   piecesByColour c        %= xor ft
   piecesByType (m^.piece) %= xor ft
   when (isJust $ m^.capturedPiece) $ do
      let cp = fromJust $ m^.capturedPiece
      piecesByColour (opp c) %= xor t
      piecesByType   cp      %= xor t


-- | Unmakes the @Move@ on a @Board@
undoMoveM :: (MonadState Board m) => Move -> m ()
undoMoveM = doMoveM 


-- | Generates a (lazy) list of moves with heuristic order of the moving piece
-- in reverse ( pawns first )
moves :: Board -> Magic -> Magic -> Seq Move
moves b bishopMagics rookMagics
   =  pawnMoves b
   >< knightMoves b
   >< bishopMoves b bishopMagics
   >< bishopMoves b bishopMagics
   >< rookMoves b rookMagics
   >< queenMoves b bishopMagics rookMagics
   >< kingMoves b


pawnMoves :: Board -> Seq Move
pawnMoves b
   =  pawnCapturePromotions b
   >< pawnPromotions b
   >< pawnCaptures b
   >< pawnEnPassant b
   >< pawnAdvances2 b
   >< pawnAdvances1 b


pawnPromotions :: Board -> Seq Move
pawnPromotions b = do
   let
      me        = b^.next
      mySeventh = rankBB . flip shiftL 3 $ case me of
         C.White -> 6 
         C.Black -> 1
      myDir     = case me of
         C.White -> id
         C.Black -> (* (-1))
      pawns     = (b^.piecesByColour me) .&. mySeventh
      advance   = pawns `shift` (myDir 8) .&. (complement $ occupancy b)
   promo <- singleton C.Queen |> C.Knight |> C.Rook |> C.Bishop
   target <- toSeq $ advance
   return $ Move (target - (myDir 8)) target C.Pawn (Just promo) Nothing


pawnCapturePromotions :: Board -> Seq Move
pawnCapturePromotions b = do
   let
      me        = b^.next
      mySeventh = rankBB . flip shiftL 3 $ case me of
         C.White -> 6 
         C.Black -> 1
      myDir     = case me of
         C.White -> id
         C.Black -> (* (-1))
      myAShift  = case me of
         C.White -> 9
         C.Black -> -7
      myHShift  = case me of
         C.White -> 7
         C.Black -> -9
      myPawns   = (b^.piecesByColour me) .&. (b^.pawns) .&. mySeventh
      aPawns    = myPawns .&. (fileBB 0)
      hPawns    = myPawns .&. (fileBB 7)
      nahPawns  = myPawns .&. (complement $ (fileBB 0) .&. (fileBB 7))
      capture
         =   aPawns `shift` myAShift .&. opponentsPieces b
         .|. hPawns `shift` myHShift .&. opponentsPieces b
         .|. nahPawns `shift` (myDir 7) .&. opponentsPieces b
         .|. nahPawns `shift` (myDir 9) .&. opponentsPieces b
   promo <- singleton C.Queen |> C.Knight |> C.Rook |> C.Bishop
   target <- toSeq $ capture
   source <- toSeq $ (((bit $ target - (myDir 7)) .|. (bit $ target - (myDir 9))) .&. myPawns)
   return $ Move source target C.Pawn (Just promo) (pieceAt b target)


pawnCaptures :: Board -> Seq Move
pawnCaptures b = do
   let
      me        = b^.next
      mySeventh = rankBB . flip shiftL 3 $ case me of
         C.White -> 6 
         C.Black -> 1
      myDir     = case me of
         C.White -> id
         C.Black -> (* (-1))
      myAShift  = case me of
         C.White -> 9
         C.Black -> -7
      myHShift  = case me of
         C.White -> 7
         C.Black -> -9
      myPawns   = (b^.piecesByColour me) .&. (b^.pawns) .&. (complement mySeventh)
      aPawns    = myPawns .&. (fileBB 0)
      hPawns    = myPawns .&. (fileBB 7)
      nahPawns  = myPawns .&. (complement $ (fileBB 0) .&. (fileBB 7))
      capture   = aPawns `shift` myAShift .&. opponentsPieces b       .|.
                  hPawns `shift` myHShift .&. opponentsPieces b       .|.
                  (nahPawns `shift` (myDir 7) .&. opponentsPieces b)  .|.
                  (nahPawns `shift` (myDir 9) .&. opponentsPieces b)
   target <- toSeq $ capture
   source <- toSeq $ (((bit $ target - (myDir 7)) .|. (bit $ target - (myDir 9))) .&. myPawns)
   return $ Move source target C.Pawn Nothing (pieceAt b target)


pawnAdvances2 :: Board -> Seq Move
pawnAdvances2 b = do
   let
      me        = b^.next
      mySecond  = rankBB . flip shiftL 3 $ case me of
         C.White -> 1 
         C.Black -> 6
      myDir     = case me of
         C.White -> id
         C.Black -> (* (-1))
      unblocked = (complement $ occupancy b) `shift` (myDir (-8)) .&.
                  (complement $ occupancy b) `shift` (myDir (-16)) 
      myPawns   = (b^.piecesByColour me) .&. mySecond .&. unblocked
      advance   = myPawns `shift` (myDir 16)
   target <- toSeq $ advance
   return $ Move (target - (myDir 16)) target C.Pawn Nothing Nothing


pawnAdvances1 :: Board -> Seq Move
pawnAdvances1 b = do
   let
      me        = b^.next
      myDir     = case me of
         C.White -> id
         C.Black -> (* (-1))
      unblocked = (complement $ occupancy b) `shift` (myDir (-8))
      myPawns   = (b^.piecesByColour me) .&. (b^.pawns) .&. unblocked
      advance   = myPawns `shift` (myDir 8)
   target <- toSeq $ advance
   return $ Move (target - (myDir 8)) target C.Pawn Nothing Nothing


pawnEnPassant :: Board -> Seq Move
pawnEnPassant b = empty


knightMoves b = knightCaptures b >< knightQuietMoves b


knightCaptures b = do
   myKnight <- toSeq $ myPiecesOf b C.Knight
   target   <- toSeq $ (knightAttackBB myKnight) .&. (opponentsPieces b)
   return $ Move myKnight target C.Knight Nothing (pieceAt b target) 


knightQuietMoves b = do
   myKnight <- toSeq $ myPiecesOf b C.Knight
   target   <- toSeq $ (knightAttackBB myKnight) .&. (vacated b)
   return $ Move myKnight target C.Knight Nothing Nothing 


bishopMoves b bishopMagics = bishopCaptures b bishopMagics >< bishopQuietMoves b bishopMagics


bishopCaptures b bishopMagics = do
   myBishop <- toSeq $ myPiecesOf b C.Bishop
   target   <- toSeq $ (magic bishopMagics myBishop (occupancy b)) .&. (opponentsPieces b)
   return $ Move myBishop target C.Bishop Nothing (pieceAt b target)


bishopQuietMoves b bishopMagics = do
   myBishop <- toSeq $ myPiecesOf b C.Bishop
   target   <- toSeq $ (magic bishopMagics myBishop (occupancy b)) .&. (vacated b)
   return $ Move myBishop target C.Bishop Nothing Nothing


rookMoves b rookMagics = rookCaptures b rookMagics >< rookQuietMoves b rookMagics


rookCaptures b rookMagics = do
   myRook <- toSeq $ myPiecesOf b C.Rook
   target <- toSeq $ (magic rookMagics myRook (occupancy b)) .&. (opponentsPieces b)
   return $ Move myRook target C.Rook Nothing (pieceAt b target)


rookQuietMoves b rookMagics = do
   myRook <- toSeq $ myPiecesOf b C.Rook
   target <- toSeq $ (magic rookMagics myRook (occupancy b)) .&. (vacated b)
   return $ Move myRook target C.Rook Nothing Nothing


queenMoves b bishopMagics rookMagics
   =  queenCaptures b bishopMagics rookMagics
   >< queenQuietMoves b bishopMagics rookMagics


queenCaptures b bishopMagics rookMagics = do
   myQueen <- toSeq $ myPiecesOf b C.Queen
   target  <- toSeq $ ((magic bishopMagics myQueen (occupancy b)) .|.
                       (magic rookMagics myQueen (occupancy b))) .&. (opponentsPieces b)
   return $ Move myQueen target C.Queen Nothing (pieceAt b target)


queenQuietMoves b bishopMagics rookMagics = do
   myQueen <- toSeq $ myPiecesOf b C.Queen
   target  <- toSeq $ ((magic bishopMagics myQueen (occupancy b)) .|.
                       (magic rookMagics myQueen (occupancy b))) .&. (vacated b)
   return $ Move myQueen target C.Queen Nothing Nothing


kingMoves _ = empty
