{-# OPTIONS -O2 -Wall #-}

module Widget(Widget(..), ImageSize,
              adaptModel, imageSize, size) where

import Data.Accessor(Accessor, (^.), setVal)
import Keymap(Keymap)
import Vector2(Vector2)
import TermImage(TermImage, boundingRect)

adaptModel :: Accessor w p -> (p -> Widget p) -> (w -> Widget w)
adaptModel acc pwidget wmodel = widget {widgetKeymap = flip (setVal acc) wmodel `fmap` keymap}
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

atWidgetKeymap :: (Keymap a -> Keymap b) -> Widget a -> Widget b
atWidgetKeymap f w = w{widgetKeymap = f (widgetKeymap w)}

instance Functor Widget where
  fmap = atWidgetKeymap . fmap

type ImageSize = Vector2 Int

imageSize :: TermImage -> ImageSize
imageSize = snd . boundingRect

size :: Widget model -> ImageSize
size = imageSize . widgetImage
