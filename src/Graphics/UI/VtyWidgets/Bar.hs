{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Bar
    (makeHorizontal, makeVertical)
where

import Data.Vector.Vector2(Vector2(..))
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange

ranges :: Double -> Double -> Int -> (Int, Int, Int)
ranges low high size = (before, inside, after)
  where
    low' = max 0 . min 1 $ low
    high' = max 0 . min 1 . max low' $ high
    before = ceiling . (* fromIntegral size) $ low'
    after = ceiling . (* fromIntegral size) . (1 -) $ high'
    inside = size - (before + after)

outAttr :: Vty.Attr
outAttr = Vty.def_attr `Vty.with_fore_color` Vty.green

inAttr :: Vty.Attr
inAttr = Vty.def_attr `Vty.with_back_color` Vty.white

makeStrings :: Int -> Int -> Int -> [(Vty.Attr, String)]
makeStrings before inside after =
  concat
  [replicate before (outAttr, "#"),
   replicate inside (inAttr, "#"),
   replicate after (outAttr, "#")]

makeHorizontal :: Int -> Widget.Display (Double, Double)
makeHorizontal minWidth =
  Widget.Display (SizeRange.horizontallyExpanding height minWidth) mkImage
  where
    height = 1
    mkImage (start, end) (Vector2 size _) = TermImage.hstrings $ makeStrings before inside after
      where
        (before, inside, after) = ranges start end size

makeVertical :: Int -> Widget.Display (Double, Double)
makeVertical minHeight =
  Widget.Display (SizeRange.verticallyExpanding width minHeight) mkImage
  where
    width = 1
    mkImage (start, end) (Vector2 _ size) = TermImage.vstrings $ makeStrings before inside after
      where
        (before, inside, after) = ranges start end size