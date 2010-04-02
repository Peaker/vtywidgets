{-# OPTIONS -O2 -Wall #-}

module Widget(adaptModel, adaptKeymap, WidgetFields(..), Widget) where

import qualified Graphics.Vty as Vty
import Data.Accessor(Accessor, (^.), setVal)
import Keymap(Keymap)

adaptKeymap :: Accessor w p -> w -> Keymap p -> Keymap w
adaptKeymap acc wmodel keymap = flip (setVal acc) wmodel `fmap` keymap

adaptModel :: Accessor w p -> Widget p -> Widget w
adaptModel acc widget wmodel = WidgetFields image $ adaptKeymap acc wmodel keymap
  where
    WidgetFields image keymap = widget (wmodel ^. acc)

data WidgetFields model = WidgetFields {
  widgetFieldImage :: Vty.Image,
  widgetFieldKeymap :: Keymap model
  }
type Widget model = model -> WidgetFields model
