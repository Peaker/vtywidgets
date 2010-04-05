{-# OPTIONS -O2 -Wall #-}

module Widget(adaptModel, adaptKeymap, WidgetFields(..), Widget) where

import qualified Graphics.Vty as Vty
import Data.Accessor(Accessor, (^.), setVal)
import Data.Word(Word)
import Keymap(Keymap)
import Vector2(Vector2)

adaptKeymap :: Accessor w p -> w -> Keymap p -> Keymap w
adaptKeymap acc wmodel keymap = flip (setVal acc) wmodel `fmap` keymap

adaptModel :: Accessor w p -> Widget p -> Widget w
adaptModel acc widget wmodel = widgetFields {widgetFieldKeymap = adaptKeymap acc wmodel keymap}
  where
    widgetFields = widget (wmodel ^. acc)
    keymap = widgetFieldKeymap widgetFields

data WidgetFields model = WidgetFields {
  widgetFieldImage :: Vty.Image,
  widgetFieldCursor :: Maybe (Vector2 Word),
  widgetFieldKeymap :: Keymap model
  }
type Widget model = model -> WidgetFields model
