{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Widget
    (SizeRange(..), Size, fixedSize, makeSizeRange,
     Display(..), atRequestedSize, atImage, atImageArg, expand,
     Widget(..), atDisplay, atKeymap, requestedSize, make, simpleDisplay,
     HasFocus(..), adaptModel)
where

import Data.Accessor(Accessor, (^.), setVal)
import Data.Monoid(mempty)
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import Graphics.UI.VtyWidgets.Vector2(Vector2)
import Graphics.UI.VtyWidgets.TermImage(TermImage)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Control.Applicative(liftA2)

adaptModel :: Accessor w p -> (p -> Widget p) -> w -> Widget w
adaptModel acc pwidget w = (atKeymap . fmap . fmap) (flip (setVal acc) w) widget
  where
    widget = pwidget (w ^. acc)

type Size = Vector2 Int
data SizeRange = SizeRange {
  srMinSize :: Size,
  srMaxSize :: Size
  }
atBothSizes :: Endo Size -> Endo SizeRange
atBothSizes f (SizeRange minSize maxSize) = SizeRange (f minSize) (f maxSize)
fixedSize :: Size -> SizeRange
fixedSize size = SizeRange size size

makeSizeRange :: Size -> Size -> SizeRange
makeSizeRange minSize maxSize = SizeRange minSize (max minSize maxSize)

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

expand :: Size -> Endo (Display imgarg)
expand extra = (atRequestedSize . atBothSizes . liftA2 (+)) extra .
               (atImage . TermImage.translate . fmap (`div` 2)) extra

newtype HasFocus = HasFocus { hasFocus :: Bool }
  deriving (Show, Read, Eq, Ord)

data Widget k = Widget {
  -- The boundingRect topleft is ignored, and the bottom-right is
  -- considered the size
  widgetDisplay :: Display HasFocus,
  widgetKeymap :: Maybe (Keymap k)
  }
atDisplay :: Endo (Display HasFocus) -> Endo (Widget k)
atDisplay f w = w{widgetDisplay = f (widgetDisplay w)}
atKeymap :: (Maybe (Keymap a) -> Maybe (Keymap b)) ->
            Widget a -> Widget b
atKeymap f w = w{widgetKeymap = f (widgetKeymap w)}

simpleDisplay :: Display HasFocus -> Widget k
simpleDisplay display = Widget display mempty

make :: SizeRange -> (HasFocus -> Size -> TermImage) -> Maybe (Keymap k) -> Widget k
make sr f = Widget (Display sr f)

type Endo a = a -> a

instance Functor Widget where
  fmap = atKeymap . fmap . fmap

requestedSize :: Widget k -> SizeRange
requestedSize = displayRequestedSize . widgetDisplay
