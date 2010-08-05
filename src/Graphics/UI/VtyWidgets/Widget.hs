{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Widget
    (HasFocus(..), inHasFocus,
     Widget(..), inWidget, inWidget2, runWidget,
     atDisplay, atMKeymap, takesFocus, noTakeFocus, atKeymap, atMkImage, atMkSizedImage, atSizedImage,
     make, simpleDisplay,
     fromDisplay, toDisplay,
     keymap, image, requestedSize,
     strongerKeys, weakerKeys,
     whenFocused,
     backgroundColorWhenFocused,
     coloredFocusableDisplay)
where

import           Control.Arrow                    (first, second)
import           Control.Applicative              (pure, liftA2)
import           Data.Monoid                      (Monoid(..))
import           Data.Function.Utils              (Endo)
import           Graphics.Vty                     as Vty
import           Graphics.UI.VtyWidgets.SizeRange (SizeRange(..), Size)
import qualified Graphics.UI.VtyWidgets.Placable  as Placable
import           Graphics.UI.VtyWidgets.Placable  (Placable(..))
import qualified Graphics.UI.VtyWidgets.Display   as Display
import           Graphics.UI.VtyWidgets.Display   (Display)
import           Graphics.UI.VtyWidgets.Keymap    (Keymap)
import           Graphics.UI.VtyWidgets.TermImage (TermImage)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import qualified Graphics.UI.VtyWidgets.Align     as Align

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

noTakeFocus :: Endo (Widget a)
noTakeFocus = atMKeymap . const $ Nothing

takesFocus :: Endo (Widget a)
takesFocus = atMKeymap $ maybe (Just mempty) Just

atKeymap :: (Keymap a -> Keymap b) ->
            Widget a -> Widget b
atKeymap = atMKeymap . fmap

strongerKeys :: Keymap a -> Endo (Widget a)
strongerKeys = atKeymap . mappend

weakerKeys :: Keymap a -> Endo (Widget a)
weakerKeys = atKeymap . flip mappend

atMkSizedImage :: Endo (Size -> HasFocus -> TermImage) ->
                  Endo (Widget a)
atMkSizedImage = atDisplay . Placable.atPlace

atSizedImage :: (Size -> Endo TermImage) -> Endo (Widget a)
atSizedImage modifyImage = atMkSizedImage f
  where
    f mkImage size = modifyImage size . mkImage size

atMkImage :: Endo (HasFocus -> TermImage) ->
             Endo (Widget a)
atMkImage = inWidget . fmap . first

whenFocused :: Endo (Size -> TermImage) -> Endo (Widget k)
whenFocused onSizedImage = atMkSizedImage f
  where
    f mkImage size hf@(HasFocus True) = onSizedImage (`mkImage` hf) size
    f mkImage size hf@(HasFocus False) = mkImage size hf

backgroundColorWhenFocused :: Vty.Color -> Endo (Widget k)
backgroundColorWhenFocused c = whenFocused modifyMkImage
  where
    modifyMkImage mkImage size =
      TermImage.backgroundColor c size $
      mkImage size

coloredFocusableDisplay :: Vty.Color -> Display HasFocus -> Widget k
coloredFocusableDisplay c =
  takesFocus .
  (atDisplay . Align.to . pure $ 0) .
  backgroundColorWhenFocused c .
  simpleDisplay

simpleDisplay :: Display HasFocus -> Widget k
simpleDisplay = fromDisplay mempty

clip :: Endo (Widget k)
clip = atDisplay Display.clip

make :: SizeRange -> (Size -> (HasFocus -> TermImage, Maybe (Keymap k))) -> Widget k
make sr f = clip . Widget $ Placable sr f

instance Functor Widget where
  fmap = atKeymap . fmap
