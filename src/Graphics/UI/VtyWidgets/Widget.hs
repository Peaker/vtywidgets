{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Widget
    (Display(..), unDisplay, atRequestedSize, atImage, expand,
     makeDisplay, clipDisplay,
     Widget(..), atDisplay, atKeymap, requestedSize, make, simpleDisplay,
     HasFocus(..))
where

import Data.Monoid(Monoid(..))
import Data.Function.Utils(result)
import Graphics.UI.VtyWidgets.SizeRange(SizeRange(..), Size)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import Graphics.UI.VtyWidgets.Rect(Rect(..))
import Graphics.UI.VtyWidgets.TermImage(TermImage)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Control.Applicative(pure, liftA2)

type Endo a = a -> a

data Display imgarg = Display {
  displayRequestedSize :: SizeRange,
  displayImage :: imgarg -> Size -> TermImage
  }
unDisplay :: Display imgarg -> (SizeRange, imgarg -> Size -> TermImage)
unDisplay (Display rs mkImage) = (rs, mkImage)

atRequestedSize :: Endo SizeRange -> Endo (Display imgarg)
atRequestedSize f d = d{displayRequestedSize = f $ displayRequestedSize d}

atImage :: ((imgarg -> Size -> TermImage) ->
            imgarg' -> Size -> TermImage) ->
           Display imgarg ->
           Display imgarg'
atImage f d = d{displayImage = f . displayImage $ d}

clipDisplay :: Display imgarg -> Display imgarg
clipDisplay = (atImage . result) clip
  where
    clip mkImage size = TermImage.clip (Rect (pure 0) size) (mkImage size)

makeDisplay :: SizeRange -> (imgarg -> Size -> TermImage) -> Display imgarg
makeDisplay = (result . result) clipDisplay Display

instance Monoid (Display imgarg) where
  mempty = Display mempty mempty
  Display x1 y1 `mappend` Display x2 y2 = Display (x1 `mappend` x2) (y1 `mappend` y2)

expand :: Size -> Endo (Display imgarg)
expand extra = (atRequestedSize . SizeRange.atBothSizes . liftA2 (+)) extra .
               (atImage . result . result . TermImage.translate . fmap (`div` 2)) extra

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

instance Functor Widget where
  fmap = atKeymap . fmap

requestedSize :: Widget k -> SizeRange
requestedSize = displayRequestedSize . widgetDisplay
