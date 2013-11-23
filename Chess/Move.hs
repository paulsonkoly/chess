{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.Move
   ( doMoveM
   , undoMoveM
   , moves
   )
   where


import           Control.Lens hiding (from, to, op, at)
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe

import qualified Chess as C

import           Chess.Board
import           Data.BitBoard


data Move = Move
   { _from          :: Int
   , _to            :: Int
   , _piece         :: C.PieceType
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
      f  = setBB $ view from m
      t  = setBB $ view to   m
      ft = f `xorBB` t
   c <- liftM (view next) get
   assign next $ opp c
   piecesByColour c               %= xorBB ft
   piecesByType   (view piece  m) %= xorBB ft
   when (isJust $ view capturedPiece m) $ do
      let cp = fromJust $ view capturedPiece m
      piecesByColour (opp c) %= xorBB t
      piecesByType   cp      %= xorBB t


-- | Unmakes the @Move@ on a @Board@
undoMoveM :: (MonadState Board m) => Move -> m ()
undoMoveM = doMoveM 


-- | Generates a (lazy) list of moves with heuristic order of the moving piece
-- in reverse ( pawns first )
moves :: Board -> [ Move ]
moves b = concat $ do
   pt <- [ C.Pawn, C.Knight ]
   at <- [ C.Queen, C.Rook, C.Bishop, C.Knight, C.Pawn ]
   return $ captures b pt at


attackBB :: C.PieceType -> C.Color -> BitBoard -> BitBoard
attackBB pt pl bb = case pt of
   C.Pawn ->
      case pl of
         C.White -> shiftBB   7  bb `orBB` shiftBB    9 bb
         C.Black -> shiftBB (-7) bb `orBB` shiftBB (-9) bb
   C.Knight ->
      shiftBB 6    bb `orBB` shiftBB 15    bb `orBB` shiftBB 17    bb `orBB` shiftBB 9    bb `orBB`
      shiftBB (-6) bb `orBB` shiftBB (-15) bb `orBB` shiftBB (-17) bb `orBB` shiftBB (-9) bb
   _ -> error "oops"


-- | Generates a (lazy) list of captures matching the capturing / captured
-- piece type
captures :: Board -> C.PieceType -> C.PieceType -> [ Move ]
captures b pt at = do
   let
      me = view next b
      op = opp me
      ps = view (piecesByColour me) b `andBB` view (piecesByType pt) b
   do
      q <- toList $ view (piecesByColour op) b `andBB` view (piecesByType at) b `andBB` attackBB pt me ps
      o <- toList $ ps `andBB` attackBB pt op (setBB q)
      return $ Move o q pt $ Just at



