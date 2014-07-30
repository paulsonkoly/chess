{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Applicative ((<$>))
import           Control.Concurrent.STM (newTVarIO)
import           Control.Monad (forM_, join, liftM)
import qualified Data.Foldable as F
import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.List (find)
import           Data.Monoid ((<>))
import           Numeric (showHex)
import           System.Environment (getArgs)

import           Control.Lens ((.~), (^.))
import           Control.Parallel.Strategies
import           System.IO.Silently
import           Text.Blaze
import           Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import           Text.Parsec.String (parseFromFile)

import           Chess.Board
import           Chess.Move
import qualified Chess.Search as S
import           Chess.TimeControl
import           Test.Puzzle.EPD

------------------------------------------------------------------------------
data EPDResult = EPDResult
                 { ident    :: String
                 , comment  :: String
                 , ourMove  :: Maybe Move
                 , bestMove :: [ Move ]
                 , board    :: Maybe Board
                 , eval     :: Maybe Int
                 , stsScore :: Int
                 }


startResult :: EPDResult
startResult = EPDResult "" "" Nothing [] Nothing Nothing 0


------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  scr  <- newIORef 0 
  forM_ args $ \fileName -> do
    res <- parseFromFile epdParser fileName
    case res of
      Left err   -> print err
      Right epds -> do
        putStrLn $ "Running " ++ fileName
        eres <- runWithReport epds
        let tsc = sum $ map stsScore eres
        putStrLn $ "score : " ++ show tsc
        modifyIORef scr (+tsc)
        writeFile (fileName ++ ".html") $
          renderHtml $ Main.report fileName $ eres
  tsc <- readIORef scr
  putStrLn $ "Total score : " ++ show tsc


------------------------------------------------------------------------------
runWithReport :: [EPD] -> IO [EPDResult]
runWithReport epds = mapM runEPD epds `using` rpar


------------------------------------------------------------------------------
runEPD :: EPD -> IO EPDResult
runEPD (EPD b ops) = F.foldlM (applyOp b) startResult ops


------------------------------------------------------------------------------
applyOp :: Board -> EPDResult -> Operation -> IO EPDResult
applyOp b epdr (BestMove ms) = do
  r <- silence $ do
    abortVal  <- newTVarIO False
    ponderVal <- newTVarIO False
    let state = (S.board .~ b) $ S.mkSearchState abortVal ponderVal
    liftM fst $ S.runSearch (S.search $ DepthSpecified 4) state
  let m = join $ S.first <$> r
  return $ epdr
               { ourMove  = m
               , bestMove = ms
               , eval     = (^.S.eval) <$> r
               }
  
applyOp b epdr (Id ids) = do
  putStrLn $ "  Running " ++ ids
  return $ epdr { board = Just b, ident = ids }
  
applyOp _ epdr (Comment _ (Left cs)) = return $ epdr { comment = cs }
applyOp _ epdr (Comment _ (Right l)) =
  let score = case find ((== ourMove epdr) . Just . fst) l of
        Just (_, s) -> s
        Nothing     -> 0
  in return $ epdr { stsScore = score }

applyOp _ epdr _  = return epdr


------------------------------------------------------------------------------
report :: String -> [ EPDResult ] -> H.Html
report fn epdrs =
  H.docTypeHtml $ do
    H.head $
      H.title $ H.toHtml $ "EPD results " ++ fn
    H.body $
      H.table ! A.border "1" ! A.align "center" $ do
        H.thead $
          H.tr $ do
            H.td "Identifier"
            H.td "FEN"
            H.td "Our move"
            H.td "STS score"
            H.td "Best move(s)"
            H.td "Our evaluation"            
            H.td "Comment"
        H.tbody $ F.foldr1 (<>) $ map reportEPD epdrs


------------------------------------------------------------------------------
reportEPD :: EPDResult -> H.Html
reportEPD (EPDResult i c mm bm mb me sc) =
  H.tr ! A.style (preEscapedToValue $ scoreCSS sc) $ do
    H.td $ H.toHtml i
    H.td $ H.toHtml $ maybe "No board" fen mb
    H.td $ H.toHtml $ maybe "No move" renderShortMove mm
    H.td $ H.toHtml sc
    H.td $ H.toHtml $ show $ map renderShortMove bm
    H.td $ H.toHtml $ maybe "No evaluation" show me
    H.td $ H.toHtml c


scoreCSS :: Int -> String
scoreCSS sc =
  let a = 25 * sc
      b = max 16 $ a - 50
      s = case sc of
        0 -> "red"
        _ -> '#' : (showHex b . showHex a . showHex b $ "")
  in "background-color:" ++ s ++";"
