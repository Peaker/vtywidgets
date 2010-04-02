{-# OPTIONS -O2 -Wall #-}

module Widget(adaptModel, WidgetFields(..), Widget) where

import qualified Graphics.Vty as Vty
import Data.Accessor(Accessor, (^.), setVal)
import Keymap(Keymap)

adaptModel :: Accessor w p -> Widget p -> Widget w
adaptModel acc widget wmodel = WidgetFields image (flip (setVal acc) wmodel `fmap` keymap)
  where
    WidgetFields image keymap = widget (wmodel ^. acc)

data WidgetFields model = WidgetFields {
  widgetFieldImage :: Vty.Image,
  widgetFieldKeymap :: Keymap model
  }
type Widget model = model -> WidgetFields model
