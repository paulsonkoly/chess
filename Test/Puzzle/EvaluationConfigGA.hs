{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Puzzle.EvaluationConfigGA where

import Control.Monad (replicateM)
import System.Random

import Control.Lens
import Control.Monad.Random
import Data.Default
import GA(Entity(..), GAConfig(..))

import Chess.Evaluation
import Test.Puzzle.RunEPD

------------------------------------------------------------------------------
parameterRange :: (Int, Int)
parameterRange = (0, 15)
numberOfParameters :: Int
numberOfParameters = 15


type EvaluationConfigGenome = [ Int ]


------------------------------------------------------------------------------
fromGenome :: EvaluationConfigGenome -> EvaluationConfig
fromGenome g =
  (kingSafetyAttackingKnights .~ g !! 0)
  $ (kingSafetyRayAttacks     .~ g !! 1)
  $ (castlePawnValue          .~ g !! 2)
  $ (rookOnSeventh            .~ g !! 3)
  $ (rookOnOpen               .~ g !! 4)
  $ (rookOnSemiOpen           .~ g !! 5)
  $ (bishopPair               .~ g !! 6)
  $ (bishopBadBishop          .~ g !! 7)
  $ (bishopBlockingPawns      .~ g !! 8)
  $ (knightOnRimSquares       .~ g !! 9)
  $ (pawnEndGamePassed        .~ g !! 10)
  $ (pawnEndGameDouble        .~ g !! 11)
  $ (pawnOpeningCentral       .~ g !! 12)
  $ (pawnOpeningLargeCentral  .~ g !! 13)
  $ (outpostCentral           .~ g !! 14)
  $ (outpostLargeCentral      .~ g !! 15) def
  

------------------------------------------------------------------------------
instance Random EvaluationConfigGenome where
  randomR _ = random

  random g = flip runRand g
             $ replicateM numberOfParameters (getRandomR parameterRange)


------------------------------------------------------------------------------
instance Entity EvaluationConfigGenome Int [ String ] () IO where

  genRandom _ _ = randomIO

  crossover _ _ _ a b = do
    spl <- randomRIO (0, numberOfParameters)
    return $ Just $ take spl a ++ drop spl b

  mutation _ _ _ e = do
    pos <- randomRIO (0, numberOfParameters)
    val <- randomRIO parameterRange
    return $ Just $ take pos e ++ (val : drop (pos + 1) e)

  score fn e = do
    scr <- runTotalScore fn {- minimums -} (const 0) (fromGenome e)
    return $ Just $ 14000 - scr


------------------------------------------------------------------------------
-- The current scores -10. If an entity scores below this abort the STS run.
minimums :: String -> Int
minimums "STS1.epd"  = 342 - 10
minimums "STS2.epd"  = 413 - 10
minimums "STS3.epd"  = 396 - 10
minimums "STS4.epd"  = 377 - 10
minimums "STS5.epd"  = 532 - 10
minimums "STS6.epd"  = 648 - 10
minimums "STS7.epd"  = 348 - 10
minimums "STS8.epd"  = 244 - 10
minimums "STS9.epd"  = 196 - 10
minimums "STS10.epd" = 621 - 10
minimums "STS11.epd" = 244 - 10
minimums "STS12.epd" = 347 - 10
minimums "STS13.epd" = 330 - 10
minimums "STS14.epd" = 440 - 10
minimums _           = 0


------------------------------------------------------------------------------
evaluationConfigGAParams :: GAConfig
evaluationConfigGAParams = GAConfig
                           { getPopSize           = 4
                           , getArchiveSize       = 3
                           , getMaxGenerations    = 20
                           , getCrossoverRate     = 0.8
                           , getMutationRate      = 0.2
                           , getCrossoverParam    = undefined
                           , getMutationParam     = undefined
                           , getWithCheckpointing = False
                           , getRescoreArchive    = False
                           }


