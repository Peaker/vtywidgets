{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TableGrid
    (makeView, makeKeymapView)
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
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import qualified Graphics.UI.VtyWidgets.TextView as TextView

type MakeView a = Vty.Attr -> Vty.Attr -> Widget.Display a

makeView :: [(String, String)] -> MakeView a
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

makeKeymapView :: Int -> Int -> Keymap k -> MakeView a
makeKeymapView keyWidth valueWidth keymap =
  makeView .
  map (fitToWidth keyWidth *** fitToWidth valueWidth) .
  sortBy (comparing snd) .
  Map.toList . Map.map fst . keymapGroups $
  keymap
