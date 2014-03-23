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
   -- * Others
   , calcHash
   )
   where

import           Control.Monad
import           Control.Lens
import           Data.Monoid
import           Data.Char
import           Data.Word
import           Data.Maybe

import           Data.Square
import           Data.BitBoard hiding (prettyPrint)
import           Data.ChessTypes
import           Chess.Zobrist


data Board = Board
   { _whitePieces       :: ! BitBoard
   , _blackPieces       :: ! BitBoard
   , _rooks             :: ! BitBoard
   , _knights           :: ! BitBoard
   , _bishops           :: ! BitBoard
   , _queens            :: ! BitBoard
   , _kings             :: ! BitBoard
   , _pawns             :: ! BitBoard
   , _next              :: ! Colour
   , _enPassant         :: ! (Maybe Square)
   , _whiteCastleRights :: ! CastlingRights
   , _blackCastleRights :: ! CastlingRights
   , _hash              :: Word64
   } deriving (Eq, Show)


emptyBoard :: Board
emptyBoard = Board mempty mempty mempty mempty mempty mempty mempty mempty White Nothing mempty mempty 0


$(makeLenses ''Board)



-- | black for white, white for black
opponent' :: Colour -> Colour
opponent' White = Black
opponent' Black = White
{-# INLINE opponent' #-}


-- | opposite colour of the next lens
opponent :: Lens' Board Colour
opponent = lens (opponent' . (^.next)) (\s b -> (next.~ opponent' b) s)


-- | the BitBoard Lens corresponding to the given `colour`
piecesByColour 
   :: Colour               -- ^ Black / White
   -> Lens' Board BitBoard -- ^ Lens
piecesByColour Black = blackPieces
piecesByColour White = whitePieces
{-# INLINE piecesByColour #-}

-- | the BitBoard Lens corresponding to the given PieceType
piecesByType
   :: PieceType          -- ^ Rook / Pawn etc.
   -> Lens' Board BitBoard -- ^ Lens
piecesByType Pawn   = pawns
piecesByType Rook   = rooks
piecesByType Knight = knights
piecesByType Bishop = bishops
piecesByType Queen  = queens
piecesByType King   = kings
{-# INLINE piecesByType #-}


-- | Castle rights lens corresponding to the given colour
castleRightsByColour
  :: Colour
  -> Lens' Board CastlingRights
castleRightsByColour White = whiteCastleRights
castleRightsByColour Black = blackCastleRights
{-# INLINE castleRightsByColour #-}


-- | The piece type at the given position
pieceAt :: Board -> Square -> Maybe PieceType
pieceAt b pos
  | occupancy b .&. p == mempty = Nothing
  | b^.pawns    .&. p /= mempty = Just Pawn
  | b^.knights  .&. p /= mempty = Just Knight
  | b^.bishops  .&. p /= mempty = Just Bishop
  | b^.rooks    .&. p /= mempty = Just Rook
  | b^.queens   .&. p /= mempty = Just Queen
  | b^.kings    .&. p /= mempty = Just King
  | otherwise                     = error "inconsistent board"
  where p = fromSquare pos


-- | The piece colour at a given position
pieceColourAt :: Board -> Square -> Maybe Colour
pieceColourAt b pos
  | b^.whitePieces .&. p /= mempty = Just White
  | b^.blackPieces .&. p /= mempty = Just Black
  | otherwise                      = Nothing
  where p = fromSquare pos


-- | the occupancy \Data.BitBoard\
occupancy :: Board -> BitBoard
occupancy b = b^.whitePieces .|. b^.blackPieces
{-# INLINE occupancy #-}


-- | the empty squares \Data.BitBoard\
vacated :: Board -> BitBoard
vacated = complement . occupancy
{-# INLINE vacated #-}


-- | my pieces
myPieces :: Board -> BitBoard
myPieces b = b^.piecesByColour (b^.next)


-- | opponents pieces
opponentsPieces :: Board -> BitBoard
opponentsPieces b = b^.piecesByColour (b^.opponent)


-- | pieces of a player of a specific type
piecesOf :: Board -> Colour -> PieceType -> BitBoard
piecesOf b colour pt = (b^.piecesByType pt) .&. (b^.piecesByColour colour)
{-# INLINE piecesOf #-}


myPiecesOf :: Board -> PieceType -> BitBoard
myPiecesOf b = piecesOf b (b^.next)


opponentsPiecesOf :: Board -> PieceType -> BitBoard
opponentsPiecesOf b = piecesOf b (b^.opponent)


numberOf :: Board -> Colour -> PieceType -> Int
numberOf b c = popCount . piecesOf b c


paint :: PieceType -> Colour -> Char
paint pt c = let mods = if c == White then toUpper else id
             in  mods $ case pt of
               Pawn   -> 'p'
               Knight -> 'n'
               Bishop -> 'b'
               Rook   -> 'r'
               Queen  -> 'q'
               King   -> 'k'


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
                           else '/' : boardPrint (toSquare aFile (pred $ rank sq)) 0
                      else boardPrint (toSquare (succ $ file sq) (rank sq)) acc
        whosNext  = if (b^.next) == White then "w" else "b"
        castlings = let str = map (toUpper . paintCaslte) (toCastleList $ b^.whiteCastleRights)
                              ++ map paintCaslte  (toCastleList $ b^.blackCastleRights)
                    in if str == "" then "-" else str
        paintCaslte Short = 'k'
        paintCaslte Long  = 'q'
        enp = case b^.enPassant of
          Just sq -> show $ toSquare (file sq) (if rank sq == fourthRank then thirdRank else sixthRank)
          Nothing -> "-"


-- | calculates hash from scratch
calcHash :: Board -> Word64
calcHash b = foldr1 xor [ zobrist $ ZobristPiece i (fromJust $ pieceColourAt b i) (fromJust $ pieceAt b i) 
                        | i <- squares
                        , pt <- [ pieceAt b i ], isJust pt
                        , pc <- [ pieceColourAt b i ], isJust pc
                        ]
             `xor` zobrist (ZobristSide $ b^.next)
             `xor` zobrist (ZobristCastlingRights (b^.whiteCastleRights) (b^.blackCastleRights))
             `xor` zobrist (ZobristEnPassant $ b^.enPassant)
