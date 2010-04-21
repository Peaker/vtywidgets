{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Widget
    (Widget(..), atCursor, atKeymap, atImage,
     atBoundingRect, rightSpacer,
     ImageSize, imageSize, size,
     adaptModel)
where

import Data.Accessor(Accessor, (^.), setVal)
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import Graphics.UI.VtyWidgets.Vector2(Vector2)
import qualified Graphics.UI.VtyWidgets.Vector2 as Vector2
import Graphics.UI.VtyWidgets.TermImage(TermImage)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.Rect(ClipRect, inClipRect)
import qualified Graphics.UI.VtyWidgets.Rect as Rect

adaptModel :: Accessor w p -> (p -> Widget p) -> w -> Widget w
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

atCursor :: (Maybe (Vector2 Int) -> Maybe (Vector2 Int)) -> Widget a -> Widget a
atCursor f w = w{widgetCursor = f (widgetCursor w)}
atKeymap :: (Keymap a -> Keymap b) -> Widget a -> Widget b
atKeymap f w = w{widgetKeymap = f (widgetKeymap w)}
atImage :: (TermImage -> TermImage) -> Widget a -> Widget a
atImage f w = w{widgetImage = f (widgetImage w)}

type Endo a = a -> a

atBoundingRect :: Endo ClipRect -> Endo (Widget a)
atBoundingRect = atImage . TermImage.inBoundingRect

rightSpacer :: Int -> Endo ClipRect
rightSpacer n = inClipRect . Rect.atBottomRight . Vector2.first $ (+n)

instance Functor Widget where
  fmap = atKeymap . fmap

type ImageSize = Vector2 Int

-- Widget images always start at (0, 0), so the boundingRect topLeft
-- can be ignored...
imageSize :: TermImage -> ImageSize
imageSize = Rect.bottomRight . Rect.unClipRect . TermImage.boundingRect

size :: Widget model -> ImageSize
size = imageSize . widgetImage
