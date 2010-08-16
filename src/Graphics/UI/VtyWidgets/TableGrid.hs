{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TableGrid
    (makeColumnView, makeEventMapView, Theme(..), standardTheme)
where

import           Control.Applicative              (pure, liftA2)
import           Control.Arrow                    (first)
import           Data.Monoid                      (mappend)
import           Data.Ord                         (comparing)
import           Data.List                        (sortBy, intercalate)
import           Data.List.Split                  (splitEvery, splitOn)
import           Data.Vector.Vector2              (Vector2(..))
import qualified Data.Vector.Vector2              as Vector2
import           Data.Vector.Rect                 (Rect(..))
import qualified Data.Map                         as Map
import qualified Graphics.Vty                     as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import qualified Graphics.UI.VtyWidgets.Placable  as Placable
import           Graphics.UI.VtyWidgets.EventMap  (EventMap(eventMapGroups))
import qualified Graphics.UI.VtyWidgets.Align     as Align
import qualified Graphics.UI.VtyWidgets.Grid      as Grid
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import           Graphics.UI.VtyWidgets.Display   (Display)
import qualified Graphics.UI.VtyWidgets.TextView  as TextView

data Theme = Theme {
  themeEventMapKeyGroupNameAttr :: Vty.Attr,
  themeEventMapKeyGroupNameWidth :: Int,
  themeEventMapDescriptionAttr :: Vty.Attr,
  themeEventMapDescriptionWidth :: Int
  }

fitToWidth :: Int -> String -> String
fitToWidth width = intercalate "\n" . concatMap (splitEvery width) . splitOn "\n"

makeColumnView :: [(Vty.Attr, Int)] -> [[String]] -> Display a
makeColumnView attrs table =
  Grid.makeView $ (map . zipWith f) attrs table
  where
    f (attr, width) =
      Placable.atPlace modifyMkSizedImage .
      Align.to (Vector2 0 0.5) .
      (Placable.atRequestedSize . SizeRange.atMinSize . Vector2.first . const) width .
      TextView.make attr .
      fitToWidth width
      where
        modifyMkSizedImage mkSizedImage size arg =
          (emptyImage size `mappend`) .
          (`mappend` setAttrImage size) $
          mkSizedImage size arg
        emptyImage size =
          TermImage.rect (Rect (pure 0) size) . const $ (Vty.def_attr, ' ')
        setAttrImage size =
          TermImage.rect (Rect (pure 0) size) . first . const $ attr

makeEventMapView :: Theme -> EventMap e k -> Display a
makeEventMapView theme eventMap =
  makeColumnView [liftA2 (,)
                  themeEventMapKeyGroupNameAttr
                  themeEventMapKeyGroupNameWidth
                  theme,
                  liftA2 (,)
                  themeEventMapDescriptionAttr
                  themeEventMapDescriptionWidth
                  theme] .
  map (\(x, y) -> [x, y]) .
  sortBy (comparing snd) .
  Map.toList . Map.map fst . eventMapGroups $
  eventMap

standardTheme :: Theme
standardTheme = Theme {
  themeEventMapKeyGroupNameWidth = 20,
  themeEventMapKeyGroupNameAttr =
     Vty.def_attr
     `Vty.with_fore_color` Vty.green
     `Vty.with_back_color` Vty.blue,
  themeEventMapDescriptionWidth = 30,
  themeEventMapDescriptionAttr =
    Vty.def_attr
    `Vty.with_fore_color` Vty.red
    `Vty.with_back_color` Vty.blue
    `Vty.with_style` Vty.bold
  }
