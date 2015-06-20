{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
-- | Representation of the current state of the game
module Chess.Board.Board
   ( Board
   -- * Constructors
   , emptyBoard
   -- * Data manipulation
   , flipPiece
   , removePiece
   -- ** Lenses
   , next
   , Chess.Board.Board.opponent
   , enPassant
   , whiteCastleRights
   , blackCastleRights
   , hash
   , castleRightsByColour     
   -- * Queries
   , pawns
   , piecesByColour     
   , pieceAt
   , pieceColourAt
   , occupancy
   , vacated
   , myPieces
   , opponentsPieces
   , piecesOf
   , kingByColour
   , myPiecesOf
   , opponentsPiecesOf
   , numberOf
   -- * Utilities
   , prettyPrint
   , fen
   , calcHash
   -- * QuickCheck
   , prop_Board
   )
   where

import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.Word

import           Control.Lens

import           Chess.Zobrist
import           Data.BitBoard hiding (prettyPrint)
import           Data.ChessTypes
import qualified Data.ChessTypes as T (opponent)
import           Data.Square

------------------------------------------------------------------------------
data Board = Board
   { _whitePieces       :: ! BitBoard
   , _blackPieces       :: ! BitBoard
   , _rooks'            :: ! BitBoard
   , _knights'          :: ! BitBoard
   , _bishops'          :: ! BitBoard
   , _queens'           :: ! BitBoard
   , _whiteKingPosition :: ! Square
   , _blackKingPosition :: ! Square
   , _pawns'            :: ! BitBoard
   , _next              :: ! Colour
   , _enPassant         :: ! (Maybe Square)
   , _whiteCastleRights :: ! CastlingRights
   , _blackCastleRights :: ! CastlingRights
   , _hash              :: Word64
   } deriving (Eq, Show)


------------------------------------------------------------------------------
-- | A board with no pieces
emptyBoard :: Board
emptyBoard = Board mempty mempty mempty mempty mempty mempty
             (toSquare aFile firstRank) (toSquare hFile eighthRank)
             mempty White Nothing mempty mempty 0


$(makeLenses ''Board)

                           -----------------------
                           -- Data manipulation --
                           -----------------------

------------------------------------------------------------------------------
-- | For Kings a Left Square should be given and this simply sets the King
-- position. For other piece types a Right BitBoard flips the presence of the
-- specified piece type using XOR logic.
flipPiece :: Colour -> PieceType -> Either Square BitBoard -> Board -> Board
flipPiece c King (Left sq) = kingByColour' c .~ sq
flipPiece c pt (Right bb)  =
  (piecesByColour' c %~ (`xor` bb)) . (piecesByType' pt %~ (`xor` bb))
flipPiece _ _ _            = error "flipPiece : unexpected arguments"
{-# INLINE flipPiece #-}


------------------------------------------------------------------------------
-- | Removes a piece (except the king) from a square
removePiece :: Square -> Board -> Board
removePiece s =
  let comp = complement $ fromSquare s
  in foldr1 (.) [ piecesByColour' c %~ (.&. comp)
                | c <- [ Black, White ]
                ]
     . foldr1 (.) [ piecesByType' pt %~ (.&. comp)
                  | pt <-[ Pawn, Rook, Knight, Bishop, Queen]
                  ]


                            ---------------------
                            -- Exported lenses --
                            ---------------------

------------------------------------------------------------------------------
-- | opposite colour of the next lens
opponent :: Lens' Board Colour
opponent = lens (T.opponent . (^.next)) (\s b -> (next.~ T.opponent b) s)


------------------------------------------------------------------------------
-- | Castle rights lens corresponding to the given colour
castleRightsByColour
  :: Colour
  -> Lens' Board CastlingRights
castleRightsByColour White = whiteCastleRights
castleRightsByColour Black = blackCastleRights
{-# INLINE castleRightsByColour #-}

                                -------------
                                -- Queries --
                                -------------

------------------------------------------------------------------------------
-- | BitBoard with all the Pawns
pawns :: Board -> BitBoard
pawns = view pawns'


------------------------------------------------------------------------------
-- the BitBoard Lens of the given `colour` except the King
piecesByColour' :: Colour -> Lens' Board BitBoard
piecesByColour' Black = blackPieces
piecesByColour' White = whitePieces
{-# INLINE piecesByColour' #-}


------------------------------------------------------------------------------
-- the BitBoard Lens of the given type except the King
piecesByType' :: PieceType -> Lens' Board BitBoard
piecesByType' Rook   = rooks'
piecesByType' Knight = knights'
piecesByType' Bishop = bishops'
piecesByType' Queen  = queens'
piecesByType' Pawn   = pawns'
piecesByType' King   = undefined
{-# INLINE piecesByType' #-}


------------------------------------------------------------------------------
-- | the occupancy bitboard for a given colour
piecesByColour :: Board -> Colour -> BitBoard
piecesByColour b c =
  b^.piecesByColour' c .|. fromSquare (b^.kingByColour' c)



------------------------------------------------------------------------------
-- | The piece type at the given position
pieceAt :: Board -> Square -> Maybe PieceType
pieceAt b pos
  | b^.whiteKingPosition == pos = Just King
  | b^.blackKingPosition == pos = Just King
  | occupancy b .&. p == mempty = Nothing
  | b^.pawns'   .&. p /= mempty = Just Pawn
  | b^.knights' .&. p /= mempty = Just Knight
  | b^.bishops' .&. p /= mempty = Just Bishop
  | b^.rooks'   .&. p /= mempty = Just Rook
  | b^.queens'  .&. p /= mempty = Just Queen
  | otherwise                   = error "inconsistent board"
  where p = fromSquare pos


------------------------------------------------------------------------------
-- | The piece colour at a given position
pieceColourAt :: Board -> Square -> Maybe Colour
pieceColourAt b pos
  | b^.whiteKingPosition == pos    = Just White
  | b^.blackKingPosition == pos    = Just Black
  | b^.whitePieces .&. p /= mempty = Just White
  | b^.blackPieces .&. p /= mempty = Just Black
  | otherwise                      = Nothing
  where p = fromSquare pos


------------------------------------------------------------------------------
-- | the occupancy \Data.BitBoard\
occupancy :: Board -> BitBoard
occupancy b = b^.whitePieces
              .|. b^.blackPieces
              .|. fromSquare (b^.whiteKingPosition)
              .|. fromSquare (b^.blackKingPosition)
{-# INLINE occupancy #-}


------------------------------------------------------------------------------
-- | the empty squares \Data.BitBoard\
vacated :: Board -> BitBoard
vacated = complement . occupancy
{-# INLINE vacated #-}


------------------------------------------------------------------------------
-- | my pieces
myPieces :: Board -> BitBoard
myPieces b = piecesByColour b (b^.next)


------------------------------------------------------------------------------
-- | opponents pieces
opponentsPieces :: Board -> BitBoard
opponentsPieces b = piecesByColour b (b^.Chess.Board.Board.opponent)


------------------------------------------------------------------------------
-- | pieces of a player of a specific type
piecesOf :: Board -> Colour -> PieceType -> BitBoard
piecesOf b colour Pawn   = b^.pawns'  .&. b^.piecesByColour' colour
piecesOf b colour Rook   = b^.rooks'  .&. b^.piecesByColour' colour
piecesOf b colour Knight = b^.knights'.&. b^.piecesByColour' colour
piecesOf b colour Bishop = b^.bishops'.&. b^.piecesByColour' colour
piecesOf b colour Queen  = b^.queens' .&. b^.piecesByColour' colour
piecesOf b colour King   = fromSquare $ b^.kingByColour' colour
{-# INLINE piecesOf #-}


------------------------------------------------------------------------------
myPiecesOf :: Board -> PieceType -> BitBoard
myPiecesOf b = piecesOf b (b^.next)


------------------------------------------------------------------------------
opponentsPiecesOf :: Board -> PieceType -> BitBoard
opponentsPiecesOf b = piecesOf b (b^.Chess.Board.Board.opponent)


------------------------------------------------------------------------------
kingByColour' :: Colour -> Lens' Board Square
kingByColour' White = whiteKingPosition
kingByColour' Black = blackKingPosition
{-# INLINE kingByColour' #-}


------------------------------------------------------------------------------
-- | the king's square for a given colour
kingByColour :: Colour -> Board -> Square
kingByColour = view . kingByColour'
{-# INLINE kingByColour #-}

                               ---------------
                               -- Utilities --
                               ---------------

------------------------------------------------------------------------------
numberOf :: Board -> Colour -> PieceType -> Int
numberOf b c = popCount . piecesOf b c


------------------------------------------------------------------------------
paint :: PieceType -> Colour -> Char
paint pt c = let mods = if c == White then toUpper else id
             in  mods $ case pt of
               Pawn   -> 'p'
               Knight -> 'n'
               Bishop -> 'b'
               Rook   -> 'r'
               Queen  -> 'q'
               King   -> 'k'


------------------------------------------------------------------------------
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
           Just pt ->
             putChar $ paint pt (fromJust $ pieceColourAt b (toSquare f r))
           Nothing ->
             putChar ' '
      putStrLn "|"
   putStrLn $ take 17 $ cycle "'-"


------------------------------------------------------------------------------
-- | The FEN string (without the halfmove, fullmove counters)
fen :: Board -> String
fen b = boardPrint (toSquare aFile eighthRank) (0::Int)
        ++ " " ++ whosNext
        ++ " " ++ castlings
        ++ " " ++ enp
  where boardPrint sq acc =
          let mpt = pieceAt b sq
          in case mpt of
            Just pt -> (if acc > 0
                        then show acc
                        else "")
                       ++ [ paint pt (fromJust $ pieceColourAt b sq) ]
                       ++ step sq 0
            Nothing -> step sq (acc + 1)
            
        step sq acc =
          if file sq == hFile then
            (if acc > 0 then show acc else "")
            ++ if rank sq == firstRank
               then ""
               else '/' : boardPrint (toSquare aFile (pred $ rank sq)) 0
          else boardPrint (toSquare (succ $ file sq) (rank sq)) acc
               
        whosNext  = if (b^.next) == White then "w" else "b"
        
        castlings =
          let str = map (toUpper . paintCaslte)
                    (toCastleList $ b^.whiteCastleRights)
                    ++ map paintCaslte  (toCastleList $ b^.blackCastleRights)
          in if str == "" then "-" else str
                                        
        paintCaslte Short = 'k'
        paintCaslte Long  = 'q'
        
        enp = case b^.enPassant of
          Just sq -> show $ toSquare (file sq)
                     (if rank sq == fourthRank then thirdRank else sixthRank)
          Nothing -> "-"


------------------------------------------------------------------------------
-- | calculates hash from scratch
calcHash :: Board -> Word64
calcHash b =
  foldr1 xor [ zobrist $ ZobristPiece i
               (fromJust $ pieceColourAt b i)
               (fromJust $ pieceAt b i) 
             | i <- squares
             , pt <- [ pieceAt b i ], isJust pt
             , pc <- [ pieceColourAt b i ], isJust pc
             ]
  `xor` zobrist (ZobristSide $ b^.next)
  `xor` zobrist (ZobristCastlingRights
                 (b^.whiteCastleRights)
                 (b^.blackCastleRights))
  `xor` zobrist (ZobristEnPassant $ b^.enPassant)


------------------------------------------------------------------------------
-- | Property that asserts that the pieces are consistent. The union of black
-- and white pieces is the same as the union of all piece types.
prop_Board :: Board -> Bool
prop_Board b =
  let byColour = b^.whitePieces .|. b^.blackPieces
      byType   = mconcat [ b^.piecesByType' t
                         | t <- [ Queen, Rook, Knight, Bishop, Pawn]
                         ]
  in byColour == byType
