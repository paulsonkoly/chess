module Main(main) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Chess.Magic
import Chess.Move
import Chess.Board


main :: IO ()
main = defaultMain tests


tests :: [ Test ]
tests =
  [ testGroup "Magic"
    [ testProperty "Sliding attacks are the same as magic lookup" prop_slidingAttack ]
    
  , testGroup "Move"
    [ testProperty "Move Board after" prop_MoveBoardAfter
    , testProperty "Number of kings after a move is 2" prop_numberOfKings
    , testProperty "Piece number is the same or one less after a move" prop_numberOfPieces
    , testProperty "Zobrist is the same after a move as freshly calculated on the modified board" prop_zobrist
    , testProperty "A move cannot leave the King in check" prop_MoveLegalityCheck
    ]
    
  , testGroup "Board"
    [ testProperty "White black pieces are the same as all piecetypes" prop_Board
    , testProperty "Number of kings is 2" prop_BoardKingNum
    , testProperty "converting back and forth to FEN gives the same board back" prop_FEN
    ]
  ]
