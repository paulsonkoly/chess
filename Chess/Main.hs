import Control.Lens
import Data.Maybe

import Chess.Board
import Chess.Move
import Data.BitBoard

main :: IO ()
main = do
   let b = fromJust $ fromFEN "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 3"
   putStrLn "board:"
   print b
   putStrLn "white pawns"
   print $ (view whitePieces b) `andBB` (view pawns b)
   putStrLn "moves"
   print $ moves b
