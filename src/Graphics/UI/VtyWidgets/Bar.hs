{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Bar
    (makeHorizontal, makeVertical)
where

import Data.List(intersperse)
import Data.Vector.Vector2(Vector2(..))
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import qualified Graphics.UI.VtyWidgets.Widget as Widget

getHText :: Double -> Double -> Int -> String
getHText low high size = htext
  where
    low' = max 0 . min 1 $ low
    high' = max 0 . min 1 . max low' $ high
    before = ceiling . (* fromIntegral size) $ low'
    after = ceiling . (* fromIntegral size) . (1 -) $ high'
    inside = size - (before + after)
    htext = concat [
      replicate before '=',
      replicate inside '|',
      replicate after  '='
      ]

makeHorizontal :: Vty.Attr -> Int -> Widget.Display (Double, Double)
makeHorizontal attr minWidth =
  Widget.Display (Widget.horizontallyExpanding height minWidth) mkImage
  where
    height = 1
    mkImage (start, end) (Vector2 size _) = TermImage.string attr $ getHText start end size

makeVertical :: Vty.Attr -> Int -> Widget.Display (Double, Double)
makeVertical attr minHeight =
  Widget.Display (Widget.verticallyExpanding width minHeight) mkImage
  where
    width = 1
    mkImage (start, end) (Vector2 _ size) = TermImage.string attr . intersperse '\n' $ getHText start end size
