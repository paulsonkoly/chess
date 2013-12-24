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
   , Castle(..)
   , fromFEN
   , prettyPrint
   , opponent'
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
   , opponent
   , enPassant
   , whiteCastleRights
   , blackCastleRights
   -- * Lenses by type
   , piecesByColour
   , piecesByType
   , castleRightsByColour
   -- * Queries
   , pieceAt
   , occupancy
   , vacated
   , myPieces
   , opponentsPieces
   , piecesOf
   , myPiecesOf
   , opponentsPiecesOf
   )
   where

import           Control.Monad.State
import           Control.Lens
import           Data.Monoid
import           Data.Char
import           Control.Applicative

import qualified Chess     as C
import qualified Chess.FEN as C

import           Data.BitBoard hiding (prettyPrint)
import           Control.Extras


data Castle = Long | Short deriving (Show, Eq)


data Board = Board
   { _whitePieces       :: ! BitBoard
   , _blackPieces       :: ! BitBoard
   , _rooks             :: ! BitBoard
   , _knights           :: ! BitBoard
   , _bishops           :: ! BitBoard
   , _queens            :: ! BitBoard
   , _kings             :: ! BitBoard
   , _pawns             :: ! BitBoard
   , _next              :: ! C.Color
   , _enPassant         :: ! [ Maybe Int ]
   , _whiteCastleRights :: ! [ [ Castle ] ]
   , _blackCastleRights :: ! [ [ Castle ] ]
   } deriving Show


$(makeLenses ''Board)


emptyBoard :: Board
emptyBoard = Board mempty mempty mempty mempty mempty mempty mempty mempty C.White [] [[ Long, Short]] [[ Long, Short ]]


-- | black for white, white for black
opponent' :: C.Color -> C.Color
opponent' C.White = C.Black
opponent' C.Black = C.White
{-# INLINE opponent' #-}


-- | opposite colour of the next lens
opponent :: Lens' Board C.Color
opponent = lens (opponent' . (^.next)) (\s b -> (next.~ opponent' b) s)


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


-- | Castle rights lens corresponding to the given colour
castleRightsByColour
  :: C.Color
  -> Lens' Board [[ Castle ]]
castleRightsByColour C.White = whiteCastleRights
castleRightsByColour C.Black = blackCastleRights


-- | The piece type at the given position
pieceAt :: Board -> Int -> Maybe C.PieceType
pieceAt b pos
   | b^.pawns   .&. p /= mempty = Just C.Pawn
   | b^.knights .&. p /= mempty = Just C.Knight
   | b^.bishops .&. p /= mempty = Just C.Bishop
   | b^.rooks   .&. p /= mempty = Just C.Rook
   | b^.queens  .&. p /= mempty = Just C.Queen
   | b^.kings   .&. p /= mempty = Just C.King
   | otherwise                  = Nothing
   where p = bit pos


-- | the occupancy \Data.BitBoard\
occupancy :: Board -> BitBoard
occupancy b = b^.whitePieces .|. b^.blackPieces


-- | the empty squares \Data.BitBoard\
vacated :: Board -> BitBoard
vacated = complement . occupancy


-- | my pieces
myPieces :: Board -> BitBoard
myPieces b = b^.piecesByColour (b^.next)


-- | opponents pieces
opponentsPieces :: Board -> BitBoard
opponentsPieces b = b^.piecesByColour (b^.opponent)


-- | pieces of a player of a specific type
piecesOf :: Board -> C.Color -> C.PieceType -> BitBoard
piecesOf b colour pt = (b^.piecesByType pt) .&. (b^.piecesByColour colour)


myPiecesOf :: Board -> C.PieceType -> BitBoard
myPiecesOf b = piecesOf b (b^.next)


opponentsPiecesOf :: Board -> C.PieceType -> BitBoard
opponentsPiecesOf b = piecesOf b (b^.opponent)


-- | the chesshs library representation to our BitBoard representation
clBToB :: C.Board -> Board
clBToB b = flip execState emptyBoard $ do
   assign next $ C.turn b
   doOnJust (C.enpassant b) $ \ep -> enPassant .= [ Just (transEP (ep^._2) * 8 + (ep^._1)) ]
   whiteCastleRights .= [ concatMap transCR $ filter isUpper (C.castlingAvail b) ]
   blackCastleRights .= [ concatMap (transCR . toUpper) $ filter isLower (C.castlingAvail b) ]
   forM_ [ 0 .. 7 ] $ \file ->
      forM_ [ 0 .. 7 ] $ \rank -> do
         let
            mp  = C.pieceAt file rank b
            sbb = bit $ rank * 8 + file
         doOnJust mp $ \p -> do
           piecesByColour (C.clr p)   <>= sbb
           piecesByType   (C.piece p) <>= sbb
  where transEP 2 = 3
        transEP 5 = 4
        transEP n = error $ "Unexpected rank when translating en passant : " ++ show n
        transCR 'K' = [ Short ]
        transCR 'Q' = [ Long ]
        transCR _   = []


-- | reads a Board position from a FEN string
fromFEN :: String -> Maybe Board
fromFEN s = clBToB <$> C.fromFEN s


prettyPrint :: Board -> IO ()
prettyPrint b = do
   putStrLn $ "en Passant "  ++ show (b^.enPassant)
     ++ " white castling : " ++ show (b^.whiteCastleRights)
     ++ " black castling : " ++ show (b^.blackCastleRights)
   putStrLn $ take 17 $ cycle ",-"
   forM_ [ 7, 6 .. 0 ] $ \rank -> do
      forM_ [ 0 .. 7 ] $ \file -> do
         putChar '|'
         putChar $ paint file rank $ case pieceAt b (rank * 8 + file) of
            Just C.Pawn   -> 'p'
            Just C.Knight -> 'k'
            Just C.Bishop -> 'b'
            Just C.Rook   -> 'r'
            Just C.Queen  -> 'q'
            Just C.King   -> 'k'
            Nothing       -> ' '
      putStrLn $ "| " ++ show (7 + rank * 8 )
   putStrLn $ take 17 $ cycle "'-"
   where
      paint file rank = if b^.blackPieces .&. bit (rank * 8 + file) /= mempty
         then toUpper
         else id
