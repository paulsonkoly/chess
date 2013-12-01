import Control.Lens
import Data.Maybe

import Chess.Board
import Chess.Move
import Chess.Magic
import Data.BitBoard
import qualified Chess as C

main :: IO ()
main = do
   let b = fromJust $ fromFEN "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 3"
   putStrLn "board:"
   print b
   putStrLn "white pawns"
   print $ (b^.whitePieces) .&. (b^.pawns)
   putStrLn "moves"
   print $ moves b
   let bmagic = makeMagic C.Bishop
   putStrLn "squares attacked by c1 Bishop"
   prettyPrint $ magic bmagic 2 $ (b^.whitePieces) .|. (b^.blackPieces) 
   putStrLn "squares attacked by f1 Bishop"
   prettyPrint $ magic bmagic 5 $ (b^.whitePieces) .|. (b^.blackPieces) 
