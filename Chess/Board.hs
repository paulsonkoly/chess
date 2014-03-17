module Chess.Board
       ( fromFEN
       , initialBoard
       , module B
       -- * Properties
       , prop_Board
       , prop_BoardKingNum
       , prop_FEN
       ) where

import           Control.Lens

import           Text.ParserCombinators.Parsec
import           Data.Maybe

import           Chess.Board.Board as B
import           Chess.Board.Parser as B
import           Chess.Board.Arbitrary as B ()
import           Chess.Board.Attacks as B

import           Data.BitBoard

-- | reads a Board position from a FEN string
fromFEN :: String -> Maybe Board
fromFEN s = case parse parserBoard "" s of
  Left _  -> Nothing
  Right b -> Just b


initialBoard :: Board
initialBoard = fromJust $ fromFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"


prop_Board :: Board -> Bool
prop_Board b = b^.whitePieces .|. b^.blackPieces == b^.kings .|. b^.queens .|. b^.rooks .|. b^.knights .|. b^.bishops .|. b^.pawns


prop_BoardKingNum :: Board -> Bool
prop_BoardKingNum b = popCount (b^.kings) == 2


prop_FEN :: Board -> Bool
prop_FEN b = Just b == fromFEN (fen b)
