module Main(main) where

import Control.Monad (void)
import Control.Monad.Random (getStdGen)

import Data.Default
import GA (evolveVerbose, Archive)
import Options.Applicative

import Test.Puzzle.EvaluationConfigGA
import Test.Puzzle.RunEPD


------------------------------------------------------------------------------
main :: IO ()
main = do
  conf <- execParser ((info $ commandParser <**> helper) idm)
  case conf of
   HTMLReport fns -> runHTMLReport fns
   Genetic    fns -> do
     g <- getStdGen
     void (evolveVerbose g evaluationConfigGAParams () fns
             :: IO (Archive EvaluationConfigGenome Int))
   Score      fns -> do
     scr <- runTotalScore fns (const 0) def
     putStrLn $ "Total score is : " ++ show scr


------------------------------------------------------------------------------
data Config = HTMLReport [ String ] | Genetic [ String ] | Score [ String ]


fileNamesParser :: Parser [ String ]
fileNamesParser = some $ argument (eitherReader Right) mempty

------------------------------------------------------------------------------
commandParser :: Parser Config
commandParser = subparser (
  command "report"
  ( info (HTMLReport <$> fileNamesParser)
    $ progDesc "runs specified EPDs and writes HTML report")
  <> command "evolve"
  ( info (Genetic <$> fileNamesParser)
    $ progDesc "runs genetic algorithm on the specified EPDs")
  <> command "score"
  ( info (Score <$> fileNamesParser)
    $ progDesc "Scores the current engine"))

  

