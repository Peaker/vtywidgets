{-# OPTIONS -O2 -Wall #-}

module Widget(Widget(..), ImageSize,
              adaptModel, adaptKeymap, imageSize, size) where

import Data.Accessor(Accessor, (^.), setVal)
import Keymap(Keymap)
import Vector2(Vector2)
import TermImage(TermImage, boundingRect)

adaptKeymap :: Accessor w p -> w -> Keymap p -> Keymap w
adaptKeymap acc wmodel keymap = flip (setVal acc) wmodel `fmap` keymap

adaptModel :: Accessor w p -> (p -> Widget p) -> (w -> Widget w)
adaptModel acc pwidget wmodel = widget {widgetKeymap = adaptKeymap acc wmodel keymap}
  where
    widget = pwidget (wmodel ^. acc)
    keymap = widgetKeymap widget

data Widget model = Widget {
  -- The boundingRect topleft is ignored, and the bottom-right is
  -- considered the size
  widgetImage :: TermImage,
  widgetCursor :: Maybe (Vector2 Int),
  widgetKeymap :: Keymap model
  }

type ImageSize = Vector2 Int

imageSize :: TermImage -> ImageSize
imageSize = snd . boundingRect

size :: Widget model -> ImageSize
size = imageSize . widgetImage
