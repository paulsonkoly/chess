{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Applicative ((<$>))
import           Control.Concurrent.STM (newTVarIO)
import           Control.Monad (forM_, join, liftM)
import qualified Data.Foldable as F
import           Data.Monoid ((<>))
import           System.Environment (getArgs)

import           Control.Lens ((.~), (^.))
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
                 , success  :: Bool
                 }


startResult :: EPDResult
startResult = EPDResult "" "" Nothing [] Nothing Nothing False


------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \fileName -> do
    res <- parseFromFile epdParser fileName
    case res of
      Left err   -> print err
      Right epds -> do
        putStrLn $ "Running " ++ fileName
        eres <- runWithReport fileName epds 
        writeFile (fileName ++ ".html") $ renderHtml eres


------------------------------------------------------------------------------
runWithReport :: String -> [ EPD ] -> IO H.Html
runWithReport fn epds = liftM (Main.report fn) $ mapM runEPD epds


------------------------------------------------------------------------------
runEPD :: EPD -> IO EPDResult
runEPD (EPD b ops) = F.foldrM (applyOp b) startResult ops


------------------------------------------------------------------------------
applyOp :: Board -> Operation -> EPDResult -> IO EPDResult
applyOp b (BestMove ms) epdr = do
  r <- silence $ do
    abortVal  <- newTVarIO False
    ponderVal <- newTVarIO False
    let state = (S.board .~ b) $ S.mkSearchState abortVal ponderVal
    liftM fst $ S.runSearch (S.search $ DepthSpecified 4) state
  let m = join $ S.first <$> r
  return $ epdr
               { ourMove  = m
               , success  = m `elem` map Just ms
               , bestMove = ms
               , eval     = (^.S.eval) <$> r
               }
  
applyOp b (Id ids) epdr = do
  putStrLn $ "  Running " ++ ids
  return $ epdr { board = Just b, ident = ids }
  
applyOp _ (Comment _ cs) epdr = return $ epdr { comment = cs }

applyOp _ _ epdr = return epdr


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
            H.td "Best move(s)"
            H.td "Our evaluation"
            H.td "Comment"
        H.tbody $ F.foldr1 (<>) $ map reportEPD epdrs


------------------------------------------------------------------------------
reportEPD :: EPDResult -> H.Html
reportEPD (EPDResult i c mm bm mb me s) =
  H.tr ! A.style (preEscapedToValue $ scolour s) $ do
    H.td $ H.toHtml i
    H.td $ H.toHtml $ maybe "No board" fen mb
    H.td $ H.toHtml $ maybe "No move" renderShortMove mm
    H.td $ H.toHtml $ show $ map renderShortMove bm
    H.td $ H.toHtml $ maybe "No evaluation" show me
    H.td $ H.toHtml c
  where
    scolour :: Bool -> String
    scolour True  = "background-color:green;"
    scolour False = "background-color:red;"
