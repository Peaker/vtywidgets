{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Widget
    (HasFocus(..), inHasFocus,
     Widget(..), inWidget,
     atDisplay, atKeymap, make, simpleDisplay,
     fromDisplay, toDisplay, keymap)
where

import Control.Arrow(second)
import Control.Applicative(liftA2)
import Data.Monoid(Monoid(..))
import Data.Function.Utils(Endo, result)
import Graphics.UI.VtyWidgets.SizeRange(SizeRange(..), Size)
import qualified Graphics.UI.VtyWidgets.Placable as Placable
import Graphics.UI.VtyWidgets.Placable(Placable(..))
import qualified Graphics.UI.VtyWidgets.Display as Display
import Graphics.UI.VtyWidgets.Display(Display)
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import Graphics.UI.VtyWidgets.TermImage(TermImage)

newtype HasFocus = HasFocus { hasFocus :: Bool }
  deriving (Show, Read, Eq, Ord)
inHasFocus :: Endo Bool -> Endo HasFocus
inHasFocus f = HasFocus . f . hasFocus

newtype Widget k = Widget { unWidget :: Placable (HasFocus -> TermImage, Keymap k) }
inWidget :: (Placable (HasFocus -> TermImage, Keymap k) ->
             Placable (HasFocus -> TermImage, Keymap k')) ->
            Widget k -> Widget k'
inWidget f = Widget . f . unWidget

fromDisplay :: (Size -> Keymap k) -> Display HasFocus -> Widget k
fromDisplay mkKeymap = Widget . Placable.atPlace (flip (liftA2 (,)) mkKeymap)

toDisplay :: Widget k -> Display HasFocus
toDisplay = (Placable.atPlace . result) fst . unWidget

keymap :: Widget k -> Size -> Keymap k
keymap w = snd . (Placable.pPlace . unWidget) w
atDisplay :: Endo (Display HasFocus) -> Endo (Widget k)
atDisplay f w = fromDisplay (keymap w) .
                f .
                toDisplay $ w

atKeymap :: (Keymap a -> Keymap b) ->
            Widget a -> Widget b
atKeymap = inWidget . fmap . second

simpleDisplay :: Display HasFocus -> Widget k
simpleDisplay = fromDisplay mempty

clip :: Endo (Widget k)
clip = atDisplay Display.clip

make :: SizeRange -> (Size -> (HasFocus -> TermImage, Keymap k)) -> Widget k
make sr f = clip (Widget $ Placable sr f)

instance Functor Widget where
  fmap = atKeymap . fmap
