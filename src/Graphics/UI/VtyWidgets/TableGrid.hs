{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TableGrid
    (makeView, makeKeymapView, addKeymapView)
where

import Data.Ord(comparing)
import Data.List(sortBy, intercalate)
import Data.List.Split(splitEvery)
import Data.Vector.Vector2(Vector2(..))
import qualified Data.Map as Map
import Control.Arrow((***))
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

type Make a = Vty.Attr -> Vty.Attr -> a

makeView :: [(String, String)] -> Make (Display a)
makeView table keyAttr valueAttr =
  Grid.makeView
  [ [ Grid.Item (Vector2 0 0.5) (TextView.make attr x)
    | (attr, x) <- [(keyAttr, key),
                    (valueAttr, value)] ]
  | (key, value) <- table ]

ljustify :: Int -> a -> [a] -> [a]
ljustify width x xs = xs ++ replicate (width - length xs) x

fitToWidth :: Int -> String -> String
fitToWidth width = intercalate "\n" . splitEvery width . ljustify width ' '

type MakeKeymap a = Int -> Int -> Make a

makeKeymapView :: Keymap k -> MakeKeymap (Display a)
makeKeymapView keymap keyWidth valueWidth =
  makeView .
  map (fitToWidth keyWidth *** fitToWidth valueWidth) .
  sortBy (comparing snd) .
  Map.toList . Map.map fst . keymapGroups $
  keymap

addKeymapView :: Int -> Size -> Widget k -> MakeKeymap (Widget k)
addKeymapView maxHeight totalSize widget keyWidth valueWidth keyAttr valueAttr =
  Widget.atDisplay grid widget
  where
    keymapView = (Placable.atRequestedSize . const) keymapViewRS $
                 makeKeymapView keymap keyWidth valueWidth keyAttr valueAttr
    keymapViewRS = SizeRange.fixedSize $ Vector2 (keyWidth + valueWidth) maxHeight
    sizeRows = (map . map) Placable.pRequestedSize $ rows (Widget.toDisplay widget)
    rows disp = [[disp, Spacer.makeHorizontal, keymapView]]
    [[widgetSize, _, _]] = ($ totalSize) . snd . Grid.makeSizes $ sizeRows
    keymap = Widget.keymap widget widgetSize
    grid = Grid.makeView . Grid.simpleRows . rows
