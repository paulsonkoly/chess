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
   -- * Parsers
   , moveParser
   , moveSansParser  
   -- * Render
   , renderShortMove
   , moveValue
   ) where

import           Control.Monad (liftM)
import           Data.Char (toUpper)
import           Data.Functor
import           Data.Maybe
import           Data.Monoid (mempty)

import           Control.Lens hiding (from, to)
import           Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import           Text.ParserCombinators.Parsec

import           Chess.Board
import           Data.BitBoard
import           Data.ChessTypes
import           Data.Square


------------------------------------------------------------------------------
data Move = Move
            { _from            :: ! Square
            , _to              :: ! Square
            , _piece           :: ! PieceType
            , _colour          :: ! Colour
            , _promotion       :: ! (Maybe PieceType)
            , _capturedPiece   :: ! (Maybe PieceType)
            , _enPassantTarget :: ! (Maybe Square)
            , _castle          :: ! (Maybe Castle)
            } deriving (Show, Eq)                       


$(makeLenses ''Move)


------------------------------------------------------------------------------
defaultMove :: Square -> Square -> PieceType -> Colour -> Move
defaultMove f t pt c = Move f t pt c Nothing Nothing Nothing Nothing


------------------------------------------------------------------------------
-- Move has is unique hash on a given board only.
moveHash :: Move -> Int
moveHash m =
  let promoHash = maybe 0 ((+1) . fromEnum) $ m^.promotion
  in (promoHash `shiftL` 12)
     .|. (fromEnum (m^.from) `shiftL` 6)
     .|. fromEnum (m^.to)


------------------------------------------------------------------------------
-- | Instance to create Sets with moves on a given Board.
instance Ord Move where
  compare a b = (moveHash a) `compare` (moveHash b)


------------------------------------------------------------------------------
-- | On a given board parses an UCI protocol style move notation into Move.
--
-- The input string is in the form of "e2e4" always specifying the square
-- where we move from.
moveParser :: Board -> Parser Move
moveParser b = do
  f <- squareParser
  t <- squareParser
  promo <- liftM (lowerToPromotion <$>) $ optionMaybe $ oneOf "qrbn"
  let enp   = enPassantSquare b f t
      cstl  = if Just King == pieceAt b f
              then case hDiff f t of
                (-2) -> return Short
                2    -> return Long
                _  -> Nothing
              else Nothing
  return
    $ (promotion .~ promo)
    $ (capturedPiece .~ pieceAt b t)
    $ (enPassantTarget .~ enp)
    $ (castle .~ cstl)
    $ defaultMove f t (fromJust $ pieceAt b f) $ b^.next


                              -----------------
                              -- Sans parser --
                              -----------------
  
------------------------------------------------------------------------------
-- | Parses a Sans move notation as specified here:
--
-- https://chessprogramming.wikispaces.com/Extended+Position+Description
moveSansParser :: Board -> Parser Move
moveSansParser b = castleSansParser b <|> otherMoveSansParser b


------------------------------------------------------------------------------
-- Parses a San style castle move
castleSansParser :: Board -> Parser Move
castleSansParser b = do
  cstl <- try (string "O-O-O" >> return Long)
          <|> try (string "O-O" >> return Short)
  let (ff, tf) = case cstl of
        Long  -> (eFile, cFile)
        Short -> (eFile, gFile)
      r = case b^.next of
        White -> firstRank
        Black -> eighthRank
  return
    $ (castle .~ Just cstl)
    $ defaultMove (toSquare ff r) (toSquare tf r) King $ b^.next


------------------------------------------------------------------------------
-- Parses any move but castles from Sans notation
otherMoveSansParser :: Board -> Parser Move
otherMoveSansParser b = do
  pc <- liftM upperToPieceType $ optionMaybe $ oneOf "NBRQK"
  ds <- disambiguityParser
  let (f, t) = disambiguity b pc ds
      enp    = enPassantSquare b f t
  p <- optionMaybe $
       liftM upperToPromotion (try $ char '=' >> oneOf "NBRQ")
  _ <- optionMaybe $ oneOf "#+"
  return
    $ (promotion .~ p)
    $ (capturedPiece .~ pieceAt b t)
    $ (enPassantTarget .~ enp)
    $ defaultMove f t (fromJust $ pieceAt b f) $ b^.next


------------------------------------------------------------------------------
-- San disambiguity with the target Square
data Disambiguity = DSquare Square | DFile File Square | DRank Rank Square


------------------------------------------------------------------------------
-- The disambiguity and the target square parser in san
disambiguityParser :: Parser Disambiguity
disambiguityParser = drParser <|> (try dfParser) <|> dsParser
  where drParser = parser (\(Just r) sq -> DRank r sq) $ Just rankParser          
        dfParser = parser (\(Just f) sq -> DFile f sq) $ Just fileParser
        dsParser = parser (\Nothing sq  -> DSquare sq) Nothing
        parser f sp = do
          sr <- case sp of
            Just p  -> liftM Just p
            Nothing -> return Nothing
          optional $ char 'x'
          sq <- squareParser
          return $ f sr sq


------------------------------------------------------------------------------
-- In san parser this gives the from and to Squares
disambiguity :: Board -> PieceType -> Disambiguity -> (Square, Square)
disambiguity b pt ds =
  let (candidates, t) = case ds of
        DSquare t'  -> (complement mempty, t')
        DFile fl t' -> (fileBB fl, t')
        DRank rn t' -> (rankBB rn, t')
      frBB = whereFromBB pt (b^.next) t b .&. piecesOf b (b^.next) pt
      f = head $ toList $ candidates .&. frBB
  in (f, t)


------------------------------------------------------------------------------
whereFromBB :: PieceType -> Colour -> Square -> Board -> BitBoard
whereFromBB Pawn c sq b   =
  let prevRank     = fromSquare $ offset sq (direction (opponent' c) 8)
      prevPrevRank = fromSquare $ offset sq (direction (opponent' c) 16)
      dbl = if vacated b .&. prevRank /= mempty
               && piecesOf b (b^.next) Pawn .&. prevPrevRank /= mempty
            then prevPrevRank
            else prevRank
  in dbl .|. pawnAttackBB sq (opponent' c)
whereFromBB King _ sq _   = kingAttackBB sq
whereFromBB Knight _ sq _ = knightAttackBB sq
whereFromBB pt _ sq _     = pseudoAttackBB pt sq


------------------------------------------------------------------------------
-- | the opposite of moveParser
renderShortMove :: Move -> String
renderShortMove m = show (m^.from)
                    ++ show (m^.to)
                    ++ showPromotion (m^.promotion)
  where
    showPromotion (Just Queen) = "q"
    showPromotion (Just Knight) = "n"
    showPromotion (Just Rook) = "r"
    showPromotion (Just Bishop) = "b"
    showPromotion _ = ""


------------------------------------------------------------------------------
upperToPromotion :: Char -> PieceType
upperToPromotion 'N' = Knight
upperToPromotion 'B' = Bishop
upperToPromotion 'R' = Rook
upperToPromotion 'Q' = Queen
upperToPromotion _   = error "not promotion character"


------------------------------------------------------------------------------
lowerToPromotion :: Char -> PieceType
lowerToPromotion = upperToPromotion . toUpper


------------------------------------------------------------------------------
upperToPieceType :: Maybe Char -> PieceType
upperToPieceType (Just 'K') = King
upperToPieceType (Just c)   = upperToPromotion c
upperToPieceType Nothing    = Pawn


------------------------------------------------------------------------------
enPassantSquare :: Board -> Square -> Square -> Maybe Square
enPassantSquare b f t =
  if Just Pawn == pieceAt b f && isNothing (pieceAt b t) && (file f /= file t)
  then b^.enPassant
  else Nothing


------------------------------------------------------------------------------
-- | heuristic value of a Move
--
-- This heuristic controls the order of the move generator.
moveValue :: Move -> Int
moveValue m = positionValue (m^.piece) (m^.colour) (m^.to)
              - positionValue (m^.piece) (m^.colour) (m^.from)


------------------------------------------------------------------------------
-- | the number of possible moves from the square
--
-- see http://chessprogramming.wikispaces.com/Influence+Quantity+of+Pieces
positionValue :: PieceType -> Colour -> Square -> Int
positionValue Pawn White sq =
  V.fromList
  [ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0
  , 3 , 4 , 4 , 4 , 4 , 6 , 6 , 6
  , 2 , 3 , 3 , 3 , 3 , 3 , 3 , 2
  , 2 , 3 , 3 , 4 , 4 , 3 , 3 , 2
  , 2 , 3 , 3 , 4 , 4 , 3 , 3 , 2
  , 2 , 3 , 3 , 3 , 3 , 3 , 3 , 2
  , 2 , 3 , 3 , 3 , 3 , 3 , 3 , 2
  , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0
  ] ! fromEnum sq

positionValue Pawn Black sq = positionValue Pawn White $ mirror sq

positionValue Knight _ sq =
  V.fromList
  [ 2 , 3 , 4 , 4 , 4 , 4 , 3 , 2
  , 3 , 4 , 6 , 6 , 6 , 6 , 4 , 3
  , 4 , 6 , 8 , 8 , 8 , 8 , 6 , 4
  , 4 , 6 , 8 , 8 , 8 , 8 , 6 , 4
  , 4 , 6 , 8 , 8 , 8 , 8 , 6 , 4
  , 4 , 6 , 8 , 8 , 8 , 8 , 6 , 4
  , 3 , 4 , 6 , 6 , 6 , 6 , 4 , 3
  , 2 , 3 , 4 , 4 , 4 , 4 , 3 , 2
  ] ! fromEnum sq

positionValue King White sq =
  V.fromList
  [ 3 , 5 , 10 , 5 , 5 , 5 , 10 , 3 
  , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
  , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
  , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
  , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
  , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
  , 5 , 8 , 8 , 8 , 8 , 8 , 8 , 5 
  , 3 , 5 , 5 , 5 , 5 , 5 , 5 , 3
  ] ! fromEnum sq

positionValue King Black sq = positionValue King White $ mirror sq

positionValue Bishop _ sq =
  V.fromList
  [ 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 
  , 7 , 9 , 9 , 9 , 9 , 9 , 9 , 7 
  , 7 , 9 ,11 ,11 ,11 ,11 , 9 , 7
  , 7 , 9 ,11 ,13 ,13 ,11 , 9 , 7
  , 7 , 9 ,11 ,13 ,13 ,11 , 9 , 7
  , 7 , 9 ,11 ,11 ,11 ,11 , 9 , 7
  , 7 , 9 , 9 , 9 , 9 , 9 , 9 , 7
  , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7
  ] ! fromEnum sq

positionValue Rook _ _ = 14

positionValue Queen _ sq =
  V.fromList
  [ 21 , 21 , 21 , 21 , 21 , 21 , 21 , 21
  , 21 , 23 , 23 , 23 , 23 , 23 , 23 , 21
  , 21 , 23 , 25 , 25 , 25 , 25 , 23 , 21
  , 21 , 23 , 25 , 27 , 27 , 25 , 23 , 21
  , 21 , 23 , 25 , 27 , 27 , 25 , 23 , 21
  , 21 , 23 , 25 , 25 , 25 , 25 , 23 , 21
  , 21 , 23 , 23 , 23 , 23 , 23 , 23 , 21
  , 21 , 21 , 21 , 21 , 21 , 21 , 21 , 21
  ] ! fromEnum sq

