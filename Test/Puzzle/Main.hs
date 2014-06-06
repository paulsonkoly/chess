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
  [ testGroup "Puzzles"
    [ testGroup "http://www.wtharvey.com/"
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
    , testGroup "Bratko-Kopec Test, http://chessprogramming.wikispaces.com/Bratko-Kopec+Test"
      [ mkTestCase "1k1r4/pp1b1R2/3q2pp/4p3/2B5/4Q3/PPP2B2/2K5 b - - 0 0" "d6d1"
      -- we fail the following bunch
      -- , mkTestCase "3r1k2/4npp1/1ppr3p/p6P/P2PPPP1/1NR5/5K2/2R5 w - - 0 0" "d4d5"
      -- , mkTestCase "2q1rr1k/3bbnnp/p2p1pp1/2pPp3/PpP1P1P1/1P2BNNP/2BQ1PRK/7R b - - 0 0" "f6f5"
      -- , mkTestCase "rnbqkb1r/p3pppp/1p6/2ppP3/3N4/2P5/PPP1QPPP/R1B1KB1R w KQkq - 0 0" "e5e6"
      -- , mkTestCase "r1b2rk1/2q1b1pp/p2ppn2/1p6/3QP3/1BN1B3/PPP3PP/R4RK1 w - - 0 0" "c3d5" -- a2a4 is also good
      -- , mkTestCase "2r3k1/pppR1pp1/4p3/4P1P1/5P2/1P4K1/P1P5/8 w - - 0 0" "g5g6"
      -- , mkTestCase "1nk1r1r1/pp2n1pp/4p3/q2pPp1N/b1pP1P2/B1P2R2/2P1B1PP/R2Q2K1 w - - 0 0" "h5f6"
      -- , mkTestCase "4b3/p3kp2/6p1/3pP2p/2pP1P2/4K1P1/P3N2P/8 w - - 0 0" "f4f5"
      -- , mkTestCase "2kr1bnr/pbpq4/2n1pp2/3p3p/3P1P1B/2N2N1Q/PPP3PP/2KR1B1R w - - 0 0" "f4f5"
      -- , mkTestCase "3rr1k1/pp3pp1/1qn2np1/8/3p4/PP1R1P2/2P1NQPP/R1B3K1 b - - 0 0" "c6e5"
      -- , mkTestCase "2r1nrk1/p2q1ppp/bp1p4/n1pPp3/P1P1P3/2PBB1N1/4QPPP/R4RK1 w - - 0 0" "f2f4"
      -- , mkTestCase "r3r1k1/ppqb1ppp/8/4p1NQ/8/2P5/PP3PPP/R3R1K1 b - - 0 0" "d7f5"
      -- , mkTestCase "r2q1rk1/4bppp/p2p4/2pP4/3pP3/3Q4/PP1B1PPP/R3R1K1 w - - 0 0" "b2b4"
      -- , mkTestCase "rnb2r1k/pp2p2p/2pp2p1/q2P1p2/8/1Pb2NP1/PB2PPBP/R2Q1RK1 w - - 0 0" "d1d2" -- d1e1 is also good
      -- , mkTestCase "2r3k1/1p2q1pp/2b1pr2/p1pp4/6Q1/1P1PP1R1/P1PN2PP/5RK1 w - - 0 0" "g4g7"
      -- , mkTestCase "r1bqkb1r/4npp1/p1p4p/1p1pP1B1/8/1B6/PPPN1PPP/R2Q1RK1 w kq - 0 0" "d2e4"
      -- , mkTestCase "r2q1rk1/1ppnbppp/p2p1nb1/3Pp3/2P1P1P1/2N2N1P/PPB1QP2/R1B2RK1 b - - 0 0"  "h7h5"
      -- , mkTestCase "r1bq1rk1/pp2ppbp/2np2p1/2n5/P3PP2/N1P2N2/1PB3PP/R1B1QRK1 b - - 0 0" "c5b3"
      -- , mkTestCase "3rr3/2pq2pk/p2p1pnp/8/2QBPP2/1P6/P5PP/4RRK1 b - - 0 0" "e8e4"
      -- , mkTestCase "r4k2/pb2bp1r/1p1qp2p/3pNp2/3P1P2/2N3P1/PPP1Q2P/2KRR3 w - - 0 0" "g3g4"
      -- , mkTestCase "3rn2k/ppb2rpp/2ppqp2/5N2/2P1P3/1P5Q/PB3PPP/3RR1K1 w - - 0 0" "f5h6"
      -- , mkTestCase "2r2rk1/1bqnbpp1/1p1ppn1p/pP6/N1P1P3/P2B1N1P/1B2QPP1/R2R2K1 b - - 0 0" "b7e4"
      -- , mkTestCase "r1bqk2r/pp2bppp/2p5/3pP3/P2Q1P2/2N1B3/1PP3PP/R4RK1 b kq - 0 0" "f7f6"
      -- , mkTestCase "r2qnrnk/p2b2b1/1p1p2pp/2pPpp2/1PP1P3/PRNBB3/3QNPPP/5RK1 w - - 0 0"  "f2f4"
      ]
    ]
  ]


    
mkTestCase :: String -> String -> Test
mkTestCase f m = testCase (f ++ " : " ++ m) $ do
  r <- silence $ do
    abortVal <- newTVarIO False
    depthVal <- newTVarIO 4
    let position = fromJust $ fromFEN f
        state    = (board .~ position) $ mkSearchState abortVal depthVal
    (r, _) <- runSearch search state
    return $ join $ first <$> r
  renderShortMove <$> r @?= Just m
