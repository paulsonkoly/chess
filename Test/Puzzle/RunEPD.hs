module Test.Puzzle.RunEPD
       ( runHTMLReport
       , runTotalScore
       , runFiles
       , runEPDs
       , runEPD
       ) where

import           Control.Applicative ((<$>))
import           Control.Monad (when, forM_, join, liftM)
import qualified Data.Foldable as F

import           Control.Concurrent.STM (newTVarIO)
import           Control.Lens ((.~), (^.))
import           Control.Parallel.Strategies (rpar, using)
import           Data.Default (def)
import           System.IO.Silently (silence)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Parsec.String (parseFromFile)

import           Chess.Board
import           Chess.Evaluation
import qualified Chess.Search as S
import           Chess.TimeControl
import           Test.Puzzle.EPD
import           Test.Puzzle.EPDResult


------------------------------------------------------------------------------
-- | Runs all STS from the specified files and writes html report into the
-- currect directory
runHTMLReport
  :: [ String ] -- ^ file names
  -> IO ()
runHTMLReport fns = do
  mbEres <- runFiles fns (const 0) True def
  F.forM_ mbEres $ \eres' -> 
    forM_ eres' $ \ (fileName, eres) ->
      writeFile (fileName ++ ".html") $ renderHtml $ report fileName eres


------------------------------------------------------------------------------
-- | Runs all STS from the specified files and returns the total score
runTotalScore
   :: [ String ]      -- ^ file names
   -> (String -> Int) -- ^ minimal required score to continue
   -> EvaluationConfig
   -> IO Int
runTotalScore fns minimums ec = do
  mbEres <- runFiles fns minimums False ec
  let eres = maybe 0 (sum . map stsScore . concatMap snd) mbEres
  return eres
  

------------------------------------------------------------------------------
-- | Runs everything from the given files. Normally returns Just, but if the
-- minimum checks fail it returns Nothing.
runFiles
  :: [ String ]      -- ^ file names
  -> (String -> Int) -- ^ minimal required score to continue
  -> Bool            -- ^ verbose IO
  -> EvaluationConfig
  -> IO (Maybe [ (String, [ EPDResult ]) ])
runFiles [] _ _ _ = return $ Just []
runFiles (fn:fns) minimums verbose ec = do
  res <- parseFromFile epdParser fn
  case res of
   Left err   -> error $ show err
   Right epds -> do
     when verbose $ putStrLn $ "Running " ++ fn
     eres <- runEPDs verbose epds ec
     let tsc = sum $ map stsScore eres
     when verbose $ putStrLn $ "score : " ++ show tsc
     if minimums fn > tsc
       then return Nothing
       else do
          rest <- runFiles fns minimums verbose ec
          return $ ((fn, eres) :) <$> rest


------------------------------------------------------------------------------
-- | Runs a single set of EPDs
runEPDs :: Bool -> [ EPD ] -> EvaluationConfig -> IO [ EPDResult ]
runEPDs verbose epds ec = mapM (runEPD verbose ec) epds `using` rpar


------------------------------------------------------------------------------
-- | Runs a single EPD
runEPD :: Bool -> EvaluationConfig -> EPD -> IO EPDResult
runEPD verbose ec (EPD b ops) = F.foldlM (applyOp verbose b ec) def ops


------------------------------------------------------------------------------
applyOp
  :: Bool
  -> Board
  -> EvaluationConfig
  -> EPDResult
  -> Operation
  -> IO EPDResult
applyOp _ b ec epdr (BestMove ms) = do
  r <- silence $ do
    abortVal  <- newTVarIO False
    ponderVal <- newTVarIO False
    let state = (S.board .~ b) $ S.mkSearchState abortVal ponderVal ec
    liftM fst $ S.runSearch (S.search $ DepthSpecified 4) state
  let m = join $ S.first <$> r
  return $ epdr
               { ourMove  = m
               , bestMove = ms
               , eval     = (^.S.eval) <$> r
               }
  
applyOp verbose b _ epdr (Id ids) = do
  when verbose $ putStrLn $ "  Running " ++ ids
  return $ epdr { board = Just b, ident = ids }
  
applyOp _ _ _ epdr (Comment _ (Left cs)) = return $ epdr { comment = cs }
applyOp _ _ _ epdr (Comment _ (Right l)) =
  let score = case F.find ((== ourMove epdr) . Just . fst) l of
        Just (_, s) -> s
        Nothing     -> 0
  in return $ epdr { stsScore = score }

applyOp _ _ _ epdr _  = return epdr


