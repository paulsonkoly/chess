{-# LANGUAGE TemplateHaskell  #-}

module Chess.Move.Move
   ( Move
   , Castle(..)
   -- * Constructor
   , defaultMove
   -- * Lenses
   , from
   , to
   , piece
   , colour
   , promotion
   , capturedPiece
   , enPassantTarget
   , castle
   -- * Parser
   , parserMove
   -- * Render
   , renderShortMove
   , moveValue
   ) where

import           Control.Lens hiding (from, to)
import           Text.ParserCombinators.Parsec
import           Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import           Data.Maybe
import           Data.Functor

import qualified Chess as C

import           Chess.Board
import           Data.Square
import           Data.ChessTypes

data Move = Move
            { _from            :: ! Square
            , _to              :: ! Square
            , _piece           :: ! C.PieceType
            , _colour          :: ! C.Color
            , _promotion       :: ! (Maybe C.PieceType)
            , _capturedPiece   :: ! (Maybe C.PieceType)
            , _enPassantTarget :: ! (Maybe Square)
            , _castle          :: ! (Maybe Castle)
            } deriving (Show, Eq)


$(makeLenses ''Move)


defaultMove :: Square -> Square -> C.PieceType -> C.Color -> Move
defaultMove f t pt c = Move f t pt c Nothing Nothing Nothing Nothing



-- | On a given board parses an UCI protocol style move notation into Move
parserMove :: Board -> Parser Move
parserMove b = do
  f <- parserSquare
  t <- parserSquare
  promotionCh <- optionMaybe $ oneOf "qrbn"
  let promo = charToPt <$> promotionCh
      enp   = if Just C.Pawn == pieceAt b f && isNothing (pieceAt b t) && (file f /= file t)
              then b^.enPassant
              else Nothing
      cstl  = if Just C.King == pieceAt b f
              then case hDiff f t of
                2  -> return Long
                (-2) -> return Short
                _  -> Nothing
              else Nothing
  return
    $ (promotion .~ promo)
    $ (capturedPiece .~ pieceAt b t)
    $ (enPassantTarget .~ enp)
    $ (castle .~ cstl)
    $ defaultMove f t (fromJust $ pieceAt b f) $ b^.next
  where
    charToPt 'q' = C.Queen
    charToPt 'r' = C.Rook
    charToPt 'b' = C.Bishop
    charToPt 'n' = C.Knight
    charToPt _   = error "Unexpected char"


renderShortMove :: Move -> String
renderShortMove m = show (m^.from) ++ show (m^.to) ++ showPromotion (m^.promotion)
  where
    showPromotion (Just C.Queen) = "q"
    showPromotion (Just C.Knight) = "n"
    showPromotion (Just C.Rook) = "r"
    showPromotion (Just C.Bishop) = "b"
    showPromotion _ = ""



-- | heuristic value of a Move
--
-- This heuristic controls the order of the move generator.
moveValue :: Move -> Int
moveValue m = positionValue (m^.piece) (m^.colour) (m^.to) - positionValue (m^.piece) (m^.colour) (m^.from)


-- | the number of possible moves from the square
--
-- the penalty for weakening the king position is here for Pawns (for now),
-- by overvalueing the f2, g3, h2 Pawns. Also The central Pawns are overvalued.
--
-- King safety means overvalueing the castle squares
-- 
-- see http://chessprogramming.wikispaces.com/Influence+Quantity+of+Pieces
positionValue :: C.PieceType -> C.Color -> Square -> Int
positionValue C.Pawn C.White sq = V.fromList [ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0
                                             , 3 , 4 , 4 , 4 , 4 , 6 , 6 , 6
                                             , 2 , 3 , 3 , 3 , 3 , 3 , 3 , 2
                                             , 2 , 3 , 3 , 4 , 4 , 3 , 3 , 2
                                             , 2 , 3 , 3 , 4 , 4 , 3 , 3 , 2
                                             , 2 , 3 , 3 , 3 , 3 , 3 , 3 , 2
                                             , 2 , 3 , 3 , 3 , 3 , 3 , 3 , 2
                                             , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0
                                             ] ! fromEnum sq

positionValue C.Pawn C.Black sq = positionValue C.Pawn C.White $ mirror sq


positionValue C.Knight _ sq = V.fromList [ 2 , 3 , 4 , 4 , 4 , 4 , 3 , 2
                                         , 3 , 4 , 6 , 6 , 6 , 6 , 4 , 3
                                         , 4 , 6 , 8 , 8 , 8 , 8 , 6 , 4
                                         , 4 , 6 , 8 , 8 , 8 , 8 , 6 , 4
                                         , 4 , 6 , 8 , 8 , 8 , 8 , 6 , 4
                                         , 4 , 6 , 8 , 8 , 8 , 8 , 6 , 4
                                         , 3 , 4 , 6 , 6 , 6 , 6 , 4 , 3
                                         , 2 , 3 , 4 , 4 , 4 , 4 , 3 , 2
                                         ] ! fromEnum sq


positionValue C.King C.White sq = V.fromList [ 3 , 5 , 10 , 5 , 5 , 5 , 10 , 3 
                                             , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
                                             , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
                                             , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
                                             , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
                                             , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
                                             , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
                                             , 3 , 5 , 5 , 5 , 5 , 5 , 5 , 3
                                             ] ! fromEnum sq

positionValue C.King C.Black sq = positionValue C.King C.White $ mirror sq


positionValue C.Bishop _ sq = V.fromList [ 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 
                                         , 7 , 9 , 9 , 9 , 9 , 9 , 9 , 7 
                                         , 7 , 9 ,11 ,11 ,11 ,11 , 9 , 7
                                         , 7 , 9 ,11 ,13 ,13 ,11 , 9 , 7
                                         , 7 , 9 ,11 ,13 ,13 ,11 , 9 , 7
                                         , 7 , 9 ,11 ,11 ,11 ,11 , 9 , 7
                                         , 7 , 9 , 9 , 9 , 9 , 9 , 9 , 7
                                         , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7
                                         ] ! fromEnum sq

positionValue C.Rook _ _ = 14


positionValue C.Queen _ sq = V.fromList [ 21 , 21 , 21 , 21 , 21 , 21 , 21 , 21
                                        , 21 , 23 , 23 , 23 , 23 , 23 , 23 , 21
                                        , 21 , 23 , 25 , 25 , 25 , 25 , 23 , 21
                                        , 21 , 23 , 25 , 27 , 27 , 25 , 23 , 21
                                        , 21 , 23 , 25 , 27 , 27 , 25 , 23 , 21
                                        , 21 , 23 , 25 , 25 , 25 , 25 , 23 , 21
                                        , 21 , 23 , 23 , 23 , 23 , 23 , 23 , 21
                                        , 21 , 21 , 21 , 21 , 21 , 21 , 21 , 21
                                        ] ! fromEnum sq
                             

