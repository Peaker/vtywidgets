{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -O2 -Wall #-}

module Widget(Widget, widgetImage, widgetCursor, widgetKeymap,
              ImageSize, adaptModel, imageSize, size) where

import Data.Accessor(Accessor, (^.), (^:), setVal)
import qualified Data.Accessor.Template as AT
import Keymap(Keymap)
import Vector2(Vector2)
import TermImage(TermImage, boundingRect)

adaptModel :: Accessor w p -> (p -> Widget p) -> (w -> Widget w)
adaptModel acc pwidget wmodel = widgetKeymap ^: fmap flip (setVal acc) wmodel $ widget
  where
    widget = pwidget (wmodel ^. acc)

data Widget model = Widget {
  -- The boundingRect topleft is ignored, and the bottom-right is
  -- considered the size
  widgetImage_ :: TermImage,
  widgetCursor_ :: Maybe (Vector2 Int),
  widgetKeymap_ :: Keymap model
  }
widgetKeymap :: Widget model :-> Keymap model
$(AT.deriveAccessors ''Widget)

atWidgetKeymap :: (Keymap a -> Keymap b) -> Widget a -> Widget b
atWidgetKeymap f w = w{widgetKeymap = f (widgetKeymap w)}

instance Functor Widget where
  fmap f = widgetKeymap ^: fmap f

type ImageSize = Vector2 Int

imageSize :: TermImage -> ImageSize
imageSize = snd . boundingRect

size :: Widget model -> ImageSize
size = imageSize . widgetImage
