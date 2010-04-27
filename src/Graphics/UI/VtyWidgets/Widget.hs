{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Widget
    (SizeRange(..), Size, fixedSize, makeSizeRange,
     horizontallyExpanding, verticallyExpanding,
     Display(..), atRequestedSize, atImage, atImageArg, expand,
     Widget(..), atDisplay, atKeymap, requestedSize, make, simpleDisplay,
     HasFocus(..), adaptModel)
where

import Data.Accessor(Accessor, (^.), setVal)
import Data.Monoid(Monoid(..))
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))
import Graphics.UI.VtyWidgets.Rect(Rect(..))
import Graphics.UI.VtyWidgets.TermImage(TermImage)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Control.Applicative(pure, liftA2)

adaptModel :: Accessor w p -> (p -> Widget p) -> w -> Widget w
adaptModel acc pwidget w = widget {widgetKeymap = flip (setVal acc) w `fmap` keymap}
  where
    widget = pwidget (w ^. acc)
    keymap = widgetKeymap widget

type Size = Vector2 Int
data SizeRange = SizeRange {
  srMinSize :: Size,
  srMaxSize :: Size
  }
atBothSizes :: Endo Size -> Endo SizeRange
atBothSizes f (SizeRange minSize maxSize) = SizeRange (f minSize) (f maxSize)
fixedSize :: Size -> SizeRange
fixedSize size = SizeRange size size

instance Monoid SizeRange where
  mempty = SizeRange (pure 0) (pure 0)
  SizeRange minSize1 maxSize1 `mappend` SizeRange minSize2 maxSize2 =
    SizeRange (max minSize1 minSize2) (max maxSize1 maxSize2)

makeSizeRange :: Size -> Size -> SizeRange
makeSizeRange minSize maxSize = SizeRange minSize (max minSize maxSize)

horizontallyExpanding :: Int -> Int -> SizeRange
horizontallyExpanding fixedHeight minWidth = SizeRange (Vector2 minWidth fixedHeight)
                                                       (Vector2 (maxBound `div` 2) fixedHeight)
verticallyExpanding :: Int -> Int -> SizeRange
verticallyExpanding fixedWidth minHeight = SizeRange (Vector2 fixedWidth minHeight)
                                                     (Vector2 fixedWidth (maxBound `div` 2))

result :: (b -> c) -> (a -> b) -> a -> c
result = (.)

data Display imgarg = Display {
  displayRequestedSize :: SizeRange,
  displayImage :: imgarg -> Size -> TermImage
  }
atRequestedSize :: Endo SizeRange -> Endo (Display imgarg)
atRequestedSize f d = d{displayRequestedSize = f $ displayRequestedSize d}
atImage :: Endo TermImage -> Endo (Display imgarg)
atImage f d = d{displayImage = (result . result) f $ displayImage d}
atImageArg :: (b -> a) -> Display a -> Display b
atImageArg f d = d{displayImage = displayImage d . f}

makeDisplay :: SizeRange -> (imgarg -> Size -> TermImage) -> Display imgarg
makeDisplay sizeRange mkImage = Display sizeRange mkImage'
  where
    mkImage' imgarg size = TermImage.clip (Rect (pure 0) size) (mkImage imgarg size)

instance Monoid (Display imgarg) where
  mempty = Display mempty mempty
  Display x1 y1 `mappend` Display x2 y2 = Display (x1 `mappend` x2) (y1 `mappend` y2)

expand :: Size -> Endo (Display imgarg)
expand extra = (atRequestedSize . atBothSizes . liftA2 (+)) extra .
               (atImage . TermImage.translate . fmap (`div` 2)) extra

newtype HasFocus = HasFocus { hasFocus :: Bool }
  deriving (Show, Read, Eq, Ord)

data Widget k = Widget {
  -- The boundingRect topleft is ignored, and the bottom-right is
  -- considered the size
  widgetDisplay :: Display HasFocus,
  widgetKeymap :: Keymap k
  }
atDisplay :: Endo (Display HasFocus) -> Endo (Widget k)
atDisplay f w = w{widgetDisplay = f (widgetDisplay w)}
atKeymap :: (Keymap a -> Keymap b) ->
            Widget a -> Widget b
atKeymap f w = w{widgetKeymap = f (widgetKeymap w)}

instance Monoid (Widget k) where
  mempty = Widget mempty mempty
  Widget x1 y1 `mappend` Widget x2 y2 = Widget (x1 `mappend` x2) (y1 `mappend` y2)

simpleDisplay :: Display HasFocus -> Widget k
simpleDisplay display = Widget display mempty

make :: SizeRange -> (HasFocus -> Size -> TermImage) -> Keymap k -> Widget k
make sr f = Widget (makeDisplay sr f)

type Endo a = a -> a

instance Functor Widget where
  fmap = atKeymap . fmap

requestedSize :: Widget k -> SizeRange
requestedSize = displayRequestedSize . widgetDisplay
