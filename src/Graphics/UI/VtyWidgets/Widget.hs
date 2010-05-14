{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Widget
    (Widget(..), atDisplay, atKeymap, requestedSize,
     make, simpleDisplay,
     HasFocus(..))
where

import Data.Monoid(Monoid(..))
import Data.Function.Utils(Endo)
import Graphics.UI.VtyWidgets.SizeRange(SizeRange(..), Size)
import qualified Graphics.UI.VtyWidgets.Placable as Placable
import qualified Graphics.UI.VtyWidgets.Display as Display
import Graphics.UI.VtyWidgets.Display(Display)
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import Graphics.UI.VtyWidgets.TermImage(TermImage)

newtype HasFocus = HasFocus { hasFocus :: Bool }
  deriving (Show, Read, Eq, Ord)

data Widget k = Widget {
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

make :: SizeRange -> (Size -> HasFocus -> TermImage) -> Keymap k -> Widget k
make sr f = Widget (Display.make sr f)

instance Functor Widget where
  fmap = atKeymap . fmap

requestedSize :: Widget k -> SizeRange
requestedSize = Placable.placableRequestedSize . widgetDisplay
