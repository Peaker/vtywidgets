{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Widget
    (HasFocus(..), inHasFocus,
     Widget(..), inWidget, inWidget2, runWidget,
     atDisplay, atMKeymap, atKeymap, atMkImage, make, simpleDisplay,
     fromDisplay, toDisplay, keymap, image, requestedSize,
     strongerKeys, weakerKeys)
where

import Control.Arrow(first, second)
import Control.Applicative(liftA2)
import Data.Monoid(Monoid(..))
import Data.Function.Utils(Endo)
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

newtype Widget k = Widget { unWidget :: Placable (HasFocus -> TermImage, Maybe (Keymap k)) }
inWidget :: (Placable (HasFocus -> TermImage, Maybe (Keymap k)) ->
             Placable (HasFocus -> TermImage, Maybe (Keymap k'))) ->
            Widget k -> Widget k'
inWidget f = Widget . f . unWidget
inWidget2 :: (Placable (HasFocus -> TermImage, Maybe (Keymap k)) ->
              Placable (HasFocus -> TermImage, Maybe (Keymap k')) ->
              Placable (HasFocus -> TermImage, Maybe (Keymap k''))) ->
             Widget k -> Widget k' -> Widget k''
inWidget2 f = inWidget . f . unWidget

instance Monoid (Widget k) where
  mempty = Widget mempty
  mappend = inWidget2 mappend

runWidget :: Widget k -> Size -> (TermImage, Maybe (Keymap k))
runWidget widget size = first ($ HasFocus True) $
                        (Placable.pPlace . unWidget) widget size

fromDisplay :: (Size -> Maybe (Keymap k)) -> Display HasFocus -> Widget k
fromDisplay mkKeymap = Widget . Placable.atPlace (flip (liftA2 (,)) mkKeymap)

toDisplay :: Widget k -> Display HasFocus
toDisplay = fmap fst . unWidget

requestedSize :: Widget k -> SizeRange
requestedSize = Placable.pRequestedSize . unWidget

keymap :: Widget k -> Size -> Maybe (Keymap k)
keymap w = snd . runWidget w

image :: Widget k -> Size -> TermImage
image w = fst . runWidget w

atDisplay :: Endo (Display HasFocus) -> Endo (Widget k)
atDisplay f w = fromDisplay (keymap w) .
                f .
                toDisplay $ w

atMKeymap :: (Maybe (Keymap a) ->
              Maybe (Keymap b)) ->
             Widget a -> Widget b
atMKeymap = inWidget . fmap . second

atKeymap :: (Keymap a -> Keymap b) ->
            Widget a -> Widget b
atKeymap = atMKeymap . fmap

strongerKeys :: Keymap a -> Widget a -> Widget a
strongerKeys = atKeymap . flip mappend

weakerKeys :: Keymap a -> Widget a -> Widget a
weakerKeys = atKeymap . mappend

atMkImage :: ((HasFocus -> TermImage) -> HasFocus -> TermImage) ->
             Widget a -> Widget a
atMkImage = inWidget . fmap . first

simpleDisplay :: Display HasFocus -> Widget k
simpleDisplay = fromDisplay mempty

clip :: Endo (Widget k)
clip = atDisplay Display.clip

make :: SizeRange -> (Size -> (HasFocus -> TermImage, Maybe (Keymap k))) -> Widget k
make sr f = clip . Widget $ Placable sr f

instance Functor Widget where
  fmap = atKeymap . fmap
