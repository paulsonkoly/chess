{-# LANGUAGE TemplateHaskell #-}
module Chess.Board
   (
   )
   where

import           Control.Monad.State
import           Control.Lens
import           Data.Monoid
import           Control.Applicative

import qualified Chess     as C
import qualified Chess.FEN as C

import           Data.BitBoard

data Board = Board
   { _whitePieces :: BitBoard
   , _blackPieces :: BitBoard
   , _rooks       :: BitBoard
   , _knights     :: BitBoard
   , _bishops     :: BitBoard
   , _queens      :: BitBoard
   , _kings       :: BitBoard
   , _pawns       :: BitBoard
   } deriving Show

$(makeLenses ''Board)


emptyBoard :: Board
emptyBoard = Board mempty mempty mempty mempty mempty mempty mempty mempty


clBToB :: C.Board -> Board
clBToB b = flip execState emptyBoard $
   forM_ [ 0 .. 7 ] $ \file ->
      forM_ [ 0 .. 7 ] $ \rank -> do
         let
            mp  = C.pieceAt file rank b
            sbb = setBB rank file
         case mp of
            Just p  -> do
               case C.clr p of
                  C.Black -> whitePieces <>= sbb
                  C.White -> blackPieces <>= sbb
               case C.piece p of
                  C.Rook   -> rooks      <>= sbb
                  C.Knight -> knights    <>= sbb
                  C.Bishop -> bishops    <>= sbb
                  C.Queen  -> queens     <>= sbb
                  C.King   -> kings      <>= sbb
                  C.Pawn   -> pawns      <>= sbb
            Nothing -> return ()


fromFEN :: String -> Maybe Board
fromFEN s = clBToB <$> C.fromFEN s


