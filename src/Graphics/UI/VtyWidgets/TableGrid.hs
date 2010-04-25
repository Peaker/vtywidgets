{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TableGrid
    (makeView, makeKeymapView)
where

import Data.List(sort)
import qualified Data.Map as Map
import qualified Graphics.Vty as Vty
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))
import Graphics.UI.VtyWidgets.Keymap(Keymap(keymapGroups))
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import qualified Graphics.UI.VtyWidgets.TextView as TextView

makeView :: Vty.Attr -> Vty.Attr -> [(String, String)] -> Widget.Display a
makeView keyAttr valueAttr table =
  Grid.makeView [ [ Grid.Item (Vector2 0 0.5) (TextView.make attr x)
                  | (attr, x) <- [(keyAttr, key),
                                  (valueAttr, value)] ]
                | (key, value) <- table ]

makeKeymapView :: Vty.Attr -> Vty.Attr -> Keymap k -> Widget.Display a
makeKeymapView keyAttr valueAttr keymap =
  makeView keyAttr valueAttr (sort . Map.toList . Map.map fst . keymapGroups $ keymap)
