{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Bar
    (makeHorizontal, makeVertical,
     Theme(..), standardTheme)
where

import           Data.Vector.Vector2              (Vector2(..))
import qualified Data.Vector.Vector2              as Vector2
import qualified Graphics.Vty                     as Vty
import           Graphics.UI.VtyWidgets.TermImage (TermImage)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import           Graphics.UI.VtyWidgets.Display   (Display)
import qualified Graphics.UI.VtyWidgets.Display   as Display
import           Graphics.UI.VtyWidgets.SizeRange (SizeRange)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange

data Theme = Theme {
  themeOutAttr :: Vty.Attr,
  themeInAttr :: Vty.Attr
  }

ranges :: Double -> Double -> Int -> (Int, Int, Int)
ranges low high size = (before, inside, after)
  where
    low' = max 0 . min 1 $ low
    high' = max 0 . min 1 . max low' $ high
    before = ceiling . (* fromIntegral size) $ low'
    after = ceiling . (* fromIntegral size) . (1 -) $ high'
    inside = size - (before + after)

makeStrings :: Theme -> Int -> Int -> Int -> [(Vty.Attr, String)]
makeStrings theme before inside after =
  concat
  [replicate before (themeOutAttr theme, " "),
   replicate inside (themeInAttr theme, "#"),
   replicate after (themeOutAttr theme, " ")]

type MakeBar = Theme -> Int -> Display ((->) (Double, Double))

makeDisplay :: (Vector2 Int -> Int) ->
               ([(Vty.Attr, String)] -> TermImage) ->
               (Int -> SizeRange) -> MakeBar
makeDisplay f combine mkSizeRange theme minAxisSize =
  Display.make (mkSizeRange minAxisSize) mkImage
  where
    mkImage size (start, end) = combine $ makeStrings theme before inside after
      where
        (before, inside, after) = ranges start end (f size)

makeHorizontal :: MakeBar
makeHorizontal = makeDisplay Vector2.fst TermImage.hstrings (SizeRange.horizontallyExpanding 1)

makeVertical :: MakeBar
makeVertical = makeDisplay Vector2.snd TermImage.vstrings (SizeRange.verticallyExpanding 1)

standardTheme :: Theme
standardTheme = Theme {
  themeOutAttr = Vty.def_attr `Vty.with_fore_color` Vty.green `Vty.with_back_color` Vty.white,
  themeInAttr = Vty.def_attr `Vty.with_back_color` Vty.green
  }
