{-# OPTIONS -O2 -Wall #-}

module Widget(WidgetFields(..), Widget, ImageSize,
              adaptModel, adaptKeymap, imageSize, size) where

import Data.Accessor(Accessor, (^.), setVal)
import Keymap(Keymap)
import Vector2(Vector2)
import TermImage(TermImage, boundingRect)

adaptKeymap :: Accessor w p -> w -> Keymap p -> Keymap w
adaptKeymap acc wmodel keymap = flip (setVal acc) wmodel `fmap` keymap

adaptModel :: Accessor w p -> Widget p -> Widget w
adaptModel acc widget wmodel = widgetFields {widgetFieldKeymap = adaptKeymap acc wmodel keymap}
  where
    widgetFields = widget (wmodel ^. acc)
    keymap = widgetFieldKeymap widgetFields

data WidgetFields model = WidgetFields {
  -- The boundingRect topleft is ignored, and the bottom-right is
  -- considered the size
  widgetFieldImage :: TermImage,
  widgetFieldCursor :: Maybe (Vector2 Int),
  widgetFieldKeymap :: Keymap model
  }
type Widget model = model -> WidgetFields model

type ImageSize = Vector2 Int

imageSize :: TermImage -> ImageSize
imageSize = snd . boundingRect

size :: WidgetFields model -> ImageSize
size = imageSize . widgetFieldImage
