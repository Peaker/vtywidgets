{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TableGrid
    (makeColumnView, makeKeymapView)
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
