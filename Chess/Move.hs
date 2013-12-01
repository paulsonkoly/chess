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
moves :: Board -> [ Move ]
moves b = concat $ do
   pt <- [ C.Pawn, C.Knight ]
   at <- [ C.Queen, C.Rook, C.Bishop, C.Knight, C.Pawn ]
   return $ captures b pt at


attackBB :: C.PieceType -> C.Color -> BitBoard -> BitBoard
attackBB pt pl bb = case pt of
   C.Pawn ->
      case pl of
         C.White -> shift bb   7  .|. shift bb    9
         C.Black -> shift bb (-7) .|. shift bb (-9)
   C.Knight ->
      shift bb 6    .|. shift bb 15    .|. shift bb 17    .|. shift bb 9    .|.
      shift bb (-6) .|. shift bb (-15) .|. shift bb (-17) .|. shift bb (-9) 
   _ -> error "oops"


-- | Generates a (lazy) list of captures matching the capturing / captured
-- piece type
captures :: Board -> C.PieceType -> C.PieceType -> [ Move ]
captures b pt at = do
   let
      me = view next b
      op = opp me
      ps = b^.piecesByColour me .&. b^.piecesByType pt
   do
      q <- toList $ b^.piecesByColour op .&. b^.piecesByType at .&. attackBB pt me ps
      o <- toList $ ps .&. attackBB pt op (bit q)
      return $ Move o q pt $ Just at



