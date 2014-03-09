{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types      #-}

{-| "Data.BitBoard" representation of the current state of the game
  
   In search it's also used for storing /nodes/. It should support
   fast move and unmove operations. To query for instance the white
   pawns one can do

   > (b^.whitePieces) .&. (b^.pawns)

 -}
module Chess.Board.Board
   ( Board
   -- * Constructors
   , emptyBoard
   -- * Utilities
   , opponent'
   , prettyPrint
   , fen
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
   , hash
   -- * Lenses by type
   , piecesByColour
   , piecesByType
   , castleRightsByColour
   -- * Queries
   , pieceAt
   , pieceColourAt
   , occupancy
   , vacated
   , myPieces
   , opponentsPieces
   , piecesOf
   , myPiecesOf
   , opponentsPiecesOf
   , numberOf
   )
   where

-- import           Control.Monad.State
-- import           Data.Functor
import           Control.Monad
import           Control.Lens
import           Data.Monoid
import           Data.Char
import           Data.Word
import           Data.Maybe

-- 

import qualified Chess     as C

-- import           Chess.Zobrist
import           Data.Square
import           Data.BitBoard hiding (prettyPrint)
import           Data.ChessTypes


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
   , _enPassant         :: ! (Maybe Square)
   , _whiteCastleRights :: ! CastlingRights
   , _blackCastleRights :: ! CastlingRights
   , _hash              :: Word64
   } deriving (Eq, Show)


emptyBoard :: Board
emptyBoard = Board mempty mempty mempty mempty mempty mempty mempty mempty C.White Nothing mempty mempty 0


$(makeLenses ''Board)



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
  -> Lens' Board CastlingRights
castleRightsByColour C.White = whiteCastleRights
castleRightsByColour C.Black = blackCastleRights



-- | The piece type at the given position
pieceAt :: Board -> Square -> Maybe C.PieceType
pieceAt b pos
   | b^.pawns   .&. p /= mempty = Just C.Pawn
   | b^.knights .&. p /= mempty = Just C.Knight
   | b^.bishops .&. p /= mempty = Just C.Bishop
   | b^.rooks   .&. p /= mempty = Just C.Rook
   | b^.queens  .&. p /= mempty = Just C.Queen
   | b^.kings   .&. p /= mempty = Just C.King
   | otherwise                  = Nothing
   where p = fromSquare pos


-- | The piece colour at a given position
pieceColourAt :: Board -> Square -> Maybe C.Color
pieceColourAt b pos
  | b^.whitePieces .&. p /= mempty = Just C.White
  | b^.blackPieces .&. p /= mempty = Just C.Black
  | otherwise                      = Nothing
  where p = fromSquare pos


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


numberOf :: Board -> C.Color -> C.PieceType -> Int
numberOf b c = popCount . piecesOf b c


paint :: C.PieceType -> C.Color -> Char
paint pt c = let mods = if c == C.White then toUpper else id
             in  mods $ case pt of
               C.Pawn   -> 'p'
               C.Knight -> 'n'
               C.Bishop -> 'b'
               C.Rook   -> 'r'
               C.Queen  -> 'q'
               C.King   -> 'k'


prettyPrint :: Board -> IO ()
prettyPrint b = do
   putStrLn $ "en Passant "  ++ show (b^.enPassant)
     ++ " white castling : " ++ show (b^.whiteCastleRights)
     ++ " black castling : " ++ show (b^.blackCastleRights)
   putStrLn $ take 17 $ cycle ",-"
   forM_ (reverse ranks) $ \r -> do
      forM_ files $ \f -> do
         putChar '|'
         case pieceAt b (toSquare f r) of
           Just pt -> putChar $ paint pt (fromJust $ pieceColourAt b (toSquare f r))
           Nothing -> putChar ' '
      putStrLn "|"
   putStrLn $ take 17 $ cycle "'-"


fen :: Board -> String
fen b = boardPrint (toSquare aFile eighthRank) (0::Int) ++ " " ++ whosNext ++ " " ++ castlings ++ " " ++ enp ++ " 0 0"
  where boardPrint sq acc = let mpt = pieceAt b sq
                            in case mpt of
                              Just pt -> (if acc > 0
                                         then show acc
                                         else "") ++ [ paint pt (fromJust $ pieceColourAt b sq) ] ++ step sq 0
                              Nothing -> step sq (acc + 1)
        step sq acc = if file sq == hFile then
                        (if acc > 0 then show acc else "")
                        ++ if rank sq == firstRank
                           then ""
                           else "/" ++ boardPrint (toSquare aFile (pred $ rank sq)) 0
                      else boardPrint (toSquare (succ $ file sq) (rank sq)) acc
        whosNext  = if (b^.next) == C.White then "w" else "b"
        castlings =     map (toUpper . paintCaslte) (toCastleList $ b^.whiteCastleRights)
                    ++  map            paintCaslte  (toCastleList $ b^.blackCastleRights)
        paintCaslte Short = 'k'
        paintCaslte Long  = 'q'
        enp = case b^.enPassant of
          Just sq -> show $ toSquare (file sq) (if rank sq == fourthRank then thirdRank else sixthRank)
          Nothing -> "-"

