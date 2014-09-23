{-# LANGUAGE OverloadedStrings #-}
module Test.Puzzle.EPDResult
       ( EPDResult(..)
       , report
       ) where

import qualified Data.Foldable as F
import           Data.Monoid ((<>))
import           Numeric (showHex)

import           Data.Default
import           Text.Blaze
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

import           Chess.Board
import           Chess.Move

------------------------------------------------------------------------------
data EPDResult = EPDResult
                 { ident    :: ! String
                 , comment  :: ! String
                 , ourMove  :: ! (Maybe Move)
                 , bestMove :: ! [ Move ]
                 , board    :: ! (Maybe Board)
                 , eval     :: ! (Maybe Int)
                 , stsScore :: ! Int
                 }


instance Default EPDResult where
  def = EPDResult "" "" Nothing [] Nothing Nothing 0


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


------------------------------------------------------------------------------
scoreCSS :: Int -> String
scoreCSS sc =
  let a = 25 * sc
      b = max 16 $ a - 50
      s = case sc of
        0 -> "red"
        _ -> '#' : (showHex b . showHex a . showHex b $ "")
  in "background-color:" ++ s ++";"
