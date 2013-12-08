import Control.Lens
import Data.Maybe

import Chess.Board
import Chess.Move
import Chess.Magic
import Data.BitBoard hiding (prettyPrint)
import qualified Chess as C

main :: IO ()
main = do
   let b = fromJust $ fromFEN "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 3"
   putStrLn "board:"
   prettyPrint b
   let
      bmagic = makeMagic C.Bishop
      rmagic = makeMagic C.Rook
      movelist = moves b bmagic rmagic
   putStrLn "moves"
   print movelist
