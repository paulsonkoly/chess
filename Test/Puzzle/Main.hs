module Main(main) where

import Chess.Board
import Chess.Move
import Chess.Search
import Control.Applicative ((<$>))
import Control.Concurrent.STM (newTVarIO)
import Control.Lens ((.~))
import Control.Monad (join)
import Data.Maybe (fromJust)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit ((@?=))
import System.IO.Silently

main :: IO ()
main = defaultMain tests


tests :: [ Test ]
tests =
  [ testGroup "Puzzles - from http://www.wtharvey.com/"
    [ testGroup "Zhansaya Abdumalik"
      [ mkTestCase "1r2nb1k/1ppr1ppp/p2p2b1/3NP1B1/P7/1P3B1P/2PR1PP1/4R2K w - - 1 0" "f3g4"
      , mkTestCase "2r2rk1/1b3p2/1q2pPp1/3pP2B/pp2nP2/1N5Q/PPP4P/1KR3R1 w - - 1 0" "h5g6"
      , mkTestCase "5rk1/p4pbp/BpQ3p1/4p3/P7/2P1q1PP/1P2n1PB/3R3K w - - 1 0" "c6f3"
      , mkTestCase "6k1/pp2p2p/3b2p1/7q/3p4/1P1P3P/P1N2rP1/1Q3R1K b - - 0 1" "h5e5"
      , mkTestCase "3r2k1/1b3ppp/p4n2/1P1N4/2r5/8/1B3PPP/2RR2K1 b - - 0 1" "d8d5"
      ]
    , testGroup "Alexander Alekhine"
      [ mkTestCase "2k4r/ppp2p2/2b2B2/7p/6pP/2P1q1bP/PP3N2/R4QK1 b - - 0 1" "g3h2"
      , mkTestCase "3r1b1k/pp4p1/2p1Qp2/5N2/PP2Pp2/2Pq4/5PKP/5R2 b - - 0 1" "f4f3"
      ]
    ]
  ]


    
mkTestCase :: String -> String -> Test
mkTestCase f m = testCase (f ++ " : " ++ m) $ do
  r <- silence $ do
    abortVal <- newTVarIO False
    let position = fromJust $ fromFEN f
        state    = (board .~ position) $ mkSearchState abortVal
    (r, _) <- runSearch (search 4 False) state
    return $ join $ first <$> r
  renderShortMove <$> r @?= Just m
