{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Bar
    (makeHorizontal, makeVertical)
where

import Data.List(intersperse)
import Data.Vector.Vector2(Vector2(..))
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import qualified Graphics.UI.VtyWidgets.Widget as Widget

normalizeRange :: Double -> Double -> (Double, Double)
normalizeRange low high = (low', high')
  where
    low' = max 0 . min 1 $ low
    high' = max 0 . min 1 . max low' $ high

makeHorizontal :: Vty.Attr -> Int -> Widget.Display (Double, Double)
makeHorizontal attr minWidth =
  Widget.Display (Widget.horizontallyExpanding height minWidth) mkImage
  where
    height = 1
    mkImage (start, end) (Vector2 size _) = TermImage.string attr text
      where
        (start', end') = normalizeRange start end
        before = truncate . (* fromIntegral size) $ start'
        after = truncate . (* fromIntegral size) . (1 -) $ end'
        inside = size - (before + after)
        text = concat [
          replicate before '=',
          replicate inside '|',
          replicate after  '='
          ]

makeVertical :: Vty.Attr -> Int -> Widget.Display (Double, Double)
makeVertical attr minHeight =
  Widget.Display (Widget.verticallyExpanding width minHeight) mkImage
  where
    width = 1
    mkImage (start, end) (Vector2 _ size) = TermImage.string attr text
      where
        (start', end') = normalizeRange start end
        before = truncate . (*fromIntegral size) $ start'
        after = truncate . (*fromIntegral size) . (1-) $ end'
        inside = size - (before + after)
        text = intersperse '\n' . concat $ [
          replicate before '=',
          replicate inside '|',
          replicate after  '='
          ]
