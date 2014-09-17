-- | The state of the current game.
module Chess.Board
       ( initialBoard
       , module B
       -- * Properties
       , prop_Board
       , prop_FEN
       ) where

------------------------------------------------------------------------------
import Data.Maybe
import Data.Monoid (mconcat)

import Control.Lens ((^.))

import Chess.Board.Arbitrary as B ()
import Chess.Board.Attacks   as B
import Chess.Board.Board     as B
import Chess.Board.Parser    as B
import Data.BitBoard
import Data.ChessTypes


------------------------------------------------------------------------------
-- | The initial position of a chess game.
initialBoard :: Board
initialBoard =
  fromJust $ fromFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"


------------------------------------------------------------------------------
-- | Property that asserts that the pieces are consistent. The union of black
-- and white pieces is the same as the union of all piece types.
prop_Board :: Board -> Bool
prop_Board b =
  let byColour = b^.whitePieces .|. b^.blackPieces
      byType   = mconcat [ b^.(piecesByType t)
                         | t <- [ King, Queen, Rook, Knight, Bishop, Pawn]
                         ]
  in byColour == byType


------------------------------------------------------------------------------
-- | Property that asserts that writing a board in fen notaion and then
-- reading it gives the same board back.
prop_FEN :: Board -> Bool
prop_FEN b = Just b == fromFEN (fen b)
