{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TableGrid
    (makeColumnView, makeKeymapView, addKeymapView)
where

import Data.Ord(comparing)
import Data.List(sortBy, intercalate)
import Data.List.Split(splitEvery)
import Data.Vector.Vector2(Vector2(..))
import qualified Data.Map as Map
import qualified Graphics.Vty as Vty
import Graphics.UI.VtyWidgets.Keymap(Keymap(keymapGroups))
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import Graphics.UI.VtyWidgets.Display(Display)
import qualified Graphics.UI.VtyWidgets.TextView as TextView
import qualified Graphics.UI.VtyWidgets.Spacer as Spacer
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Graphics.UI.VtyWidgets.Widget(Widget)
import qualified Graphics.UI.VtyWidgets.Placable as Placable
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import Graphics.UI.VtyWidgets.SizeRange(Size)

makeColumnView :: [Vty.Attr] -> [[String]] -> Display a
makeColumnView attrs table =
  Grid.makeView $ (map . zipWith f) attrs table
  where
    f attr x = Grid.Item (Vector2 0 0.5) (TextView.make attr x)

ljustify :: Int -> a -> [a] -> [a]
ljustify width x xs = xs ++ replicate (width - length xs) x

fitToWidth :: Int -> String -> String
fitToWidth width = intercalate "\n" . splitEvery width . ljustify width ' '

makeFittedColumnView :: [(Vty.Attr, Int)] -> [[String]] -> Display a
makeFittedColumnView attrsWidths table = makeColumnView attrs fittedTable
  where
    fittedTable = (map . zipWith fitToWidth) widths table
    attrs = map fst attrsWidths
    widths = map snd attrsWidths

makeKeymapView :: Keymap k -> (Vty.Attr, Int) -> (Vty.Attr, Int) -> Display a
makeKeymapView keymap keyAttrWidth valueAttrWidth =
  makeFittedColumnView [keyAttrWidth, valueAttrWidth] .
  map (\(x, y) -> [x, y]) .
  sortBy (comparing snd) .
  Map.toList . Map.map fst . keymapGroups $
  keymap

addKeymapView :: Int -> Size -> Widget k -> (Vty.Attr, Int) -> (Vty.Attr, Int) -> Widget k
addKeymapView maxHeight totalSize widget
              keyAttrWidth@(_, keyWidth)
              valueAttrWidth@(_, valueWidth) =
  Widget.atDisplay grid widget
  where
    keymapView = (Placable.atRequestedSize . const) keymapViewRS $
                 makeKeymapView keymap keyAttrWidth valueAttrWidth
    keymapViewRS = SizeRange.fixedSize $ Vector2 (keyWidth + valueWidth) maxHeight
    sizeRows = (map . map) Placable.pRequestedSize $ rows (Widget.toDisplay widget)
    rows disp = [[disp, Spacer.makeHorizontal, keymapView]]
    [[widgetSize, _, _]] = ($ totalSize) . snd . Grid.makeSizes $ sizeRows
    keymap = Widget.keymap widget widgetSize
    grid = Grid.makeView . Grid.simpleRows . rows
