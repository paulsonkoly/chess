module Chess.Board
       ( initialBoard
       , module B
       -- * Properties
       , prop_Board
       , prop_BoardKingNum
       , prop_FEN
       ) where

import           Chess.Board.Arbitrary as B ()
import           Chess.Board.Attacks   as B
import           Chess.Board.Board     as B
import           Chess.Board.Parser    as B

import           Control.Lens

import           Data.BitBoard
import           Data.Maybe

-- | The initial position of a chess game.
initialBoard :: Board
initialBoard = fromJust $ fromFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"


-- | Property that asserts that the pieces are consistent. The union
-- of black and white pieces is the same as the union of all piece types.
prop_Board :: Board -> Bool
prop_Board b = b^.whitePieces .|. b^.blackPieces == b^.kings .|. b^.queens .|. b^.rooks .|. b^.knights .|. b^.bishops .|. b^.pawns


-- | Property that asserts that the board has 2 kings.
prop_BoardKingNum :: Board -> Bool
prop_BoardKingNum b = popCount (b^.kings) == 2


-- | Property that asserts that writing a board in fen notaion and then reading it gives the same board back.
prop_FEN :: Board -> Bool
prop_FEN b = Just b == fromFEN (fen b)
