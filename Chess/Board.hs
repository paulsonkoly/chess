-- | The state of the current game.
module Chess.Board
       ( initialBoard
       , module B
       -- * Properties
--       , prop_Board
       , prop_FEN
       ) where

------------------------------------------------------------------------------
import Data.Maybe

import Chess.Board.Arbitrary as B ()
import Chess.Board.Attacks   as B
import Chess.Board.Board     as B
import Chess.Board.Parser    as B


------------------------------------------------------------------------------
-- | The initial position of a chess game.
initialBoard :: Board
initialBoard =
  fromJust $ fromFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"


------------------------------------------------------------------------------
-- | Property that asserts that writing a board in fen notaion and then
-- reading it gives the same board back.
prop_FEN :: Board -> Bool
prop_FEN b = Just b == fromFEN (fen b)
