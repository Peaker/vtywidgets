{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TableGrid
    (makeColumnView, makeKeymapView)
where

import Control.Applicative(pure)
import Control.Arrow(first)
import Data.Monoid(mappend)
import Data.Ord(comparing)
import Data.List(sortBy, intercalate)
import Data.List.Split(splitEvery, splitOn)
import Data.Vector.Vector2(Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import Data.Vector.Rect(Rect(..))
import qualified Data.Map as Map
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import qualified Graphics.UI.VtyWidgets.Placable as Placable
import Graphics.UI.VtyWidgets.Keymap(Keymap(keymapGroups))
import qualified Graphics.UI.VtyWidgets.Align as Align
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import Graphics.UI.VtyWidgets.Display(Display)
import qualified Graphics.UI.VtyWidgets.TextView as TextView

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

makeKeymapView :: (Vty.Attr, Int) -> (Vty.Attr, Int) -> Keymap k -> Display a
makeKeymapView keyAttrWidth valueAttrWidth keymap =
  makeColumnView [keyAttrWidth, valueAttrWidth] .
  map (\(x, y) -> [x, y]) .
  sortBy (comparing snd) .
  Map.toList . Map.map fst . keymapGroups $
  keymap
