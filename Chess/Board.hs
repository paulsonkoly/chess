{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types      #-}

{-| "Data.BitBoard" representation of the current state of the game
  
   In search it's also used for storing /nodes/. It should support
   fast move and unmove operations. To query for instance the white
   pawns one can do

   > (b^.whitePieces) .&. (b^.pawns)

 -}
module Chess.Board
   ( Board
   , fromFEN
   -- * Board lenses
   , whitePieces
   , blackPieces
   , rooks
   , knights
   , bishops
   , queens
   , kings
   , pawns
   , next
   -- * Lenses by type
   , piecesByColour
   , piecesByType
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
   , _next        :: C.Color
   } deriving Show


$(makeLenses ''Board)


emptyBoard :: Board
emptyBoard = Board mempty mempty mempty mempty mempty mempty mempty mempty C.White


-- | the BitBoard Lens corresponding to the given `colour`
piecesByColour 
   :: C.Color              -- ^ Black / White
   -> Lens' Board BitBoard -- ^ Lens
piecesByColour C.Black = blackPieces
piecesByColour C.White = whitePieces


-- | the BitBoard Lens corresponding to the given PieceType
piecesByType
   :: C.PieceType          -- ^ Rook / Pawn etc.
   -> Lens' Board BitBoard -- ^ Lens
piecesByType C.Pawn   = pawns
piecesByType C.Rook   = rooks
piecesByType C.Knight = knights
piecesByType C.Bishop = bishops
piecesByType C.Queen  = queens
piecesByType C.King   = kings


-- | the chesshs library representation to our BitBoard representation
clBToB :: C.Board -> Board
clBToB b = flip execState emptyBoard $ do
   assign next $ C.turn b
   forM_ [ 0 .. 7 ] $ \file ->
      forM_ [ 0 .. 7 ] $ \rank -> do
         let
            mp  = C.pieceAt file rank b
            sbb = bit $ rank * 8 + file
         case mp of
            Just p  -> do
               piecesByColour (C.clr p)   <>= sbb
               piecesByType   (C.piece p) <>= sbb
            Nothing -> return ()


-- | reads a Board position from a FEN string
fromFEN :: String -> Maybe Board
fromFEN s = clBToB <$> C.fromFEN s


