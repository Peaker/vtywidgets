{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Widget
    (HasFocus(..), inHasFocus,
     Widget(..), inWidget, inWidget2, runWidget,
     atHasFocus, atPlacable,
     atMKeymap, atKeymap, atImage, atMkSizedImage, atSizedImage,
     atMkDisplay, atDisplay,
     takesFocus, noTakeFocus,
     make, fromTuple, simpleMkDisplay, simpleDisplay,
     fromDisplay, toDisplay,
     keymap, image, requestedSize,
     strongerKeys, weakerKeys,
     whenFocused,
     backgroundColorWhenFocused,
     coloredFocusableMkDisplay,
     coloredFocusableDisplay)
where

import           Control.Arrow                    (first, second)
import           Control.Applicative              (pure)
import           Data.Monoid                      (Monoid(..))
import           Data.Function.Utils              (Endo, argument, result, inFlip)
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

type InWidget k = HasFocus -> Placable (TermImage, Maybe (Keymap k))
newtype Widget k = Widget { unWidget :: InWidget k }
inWidget :: (InWidget k -> InWidget k') ->
            Widget k -> Widget k'
inWidget f = Widget . f . unWidget
inWidget2 :: (InWidget k -> InWidget k' -> InWidget k'') ->
             Widget k -> Widget k' -> Widget k''
inWidget2 f = inWidget . f . unWidget

instance Monoid (Widget k) where
  mempty = Widget mempty
  mappend = inWidget2 mappend

fromDisplay :: (Size -> Maybe (Keymap k)) -> (HasFocus -> Display ()) -> Widget k
fromDisplay mkKeymap mkDisplay = Widget $ Placable.atPlace mkImageToWidgetTuple . mkDisplay
  where
    mkImageToWidgetTuple mkImage size = (mkImage size (), mkKeymap size)

toDisplay :: Widget k -> HasFocus -> Display a
toDisplay w hf = fmap (const . fst) . ($ hf) . unWidget $ w

atMkDisplay :: Endo (HasFocus -> Display ()) -> Endo (Widget k)
atMkDisplay f w = fromDisplay (keymap w) . f . toDisplay $ w

atDisplay :: Endo (Display ()) -> Endo (Widget k)
atDisplay = atMkDisplay . result

simpleMkDisplay :: (HasFocus -> Display ()) -> Widget k
simpleMkDisplay = fromDisplay . pure $ Nothing

simpleDisplay :: Display () -> Widget k
simpleDisplay = simpleMkDisplay . const

requestedSize :: Widget k -> HasFocus -> SizeRange
requestedSize = (result . result) Placable.pRequestedSize unWidget

runWidget :: Widget k -> Size -> (TermImage, Maybe (Keymap k))
runWidget widget size = Placable.pPlace (unWidget widget $ HasFocus True) size

keymap :: Widget k -> Size -> Maybe (Keymap k)
keymap w = snd . runWidget w

image :: Widget k -> Size -> TermImage
image w = fst . runWidget w

atHasFocus :: Endo Bool -> Endo (Widget a)
atHasFocus = inWidget . argument . inHasFocus

atPlacable :: (Placable (TermImage, Maybe (Keymap k)) ->
               Placable (TermImage, Maybe (Keymap k'))) ->
              Widget k -> Widget k'
atPlacable = inWidget . result

atImage :: Endo TermImage -> Endo (Widget a)
atImage = atPlacable . fmap . first

atMKeymap :: (Maybe (Keymap a) -> Maybe (Keymap b)) ->
             Widget a -> Widget b
atMKeymap = atPlacable . fmap . second

atKeymap :: (Keymap a -> Keymap b) ->
            Widget a -> Widget b
atKeymap = atMKeymap . fmap

instance Functor Widget where
  fmap = atKeymap . fmap

atMkSizedImage :: Endo (Size -> TermImage) ->
                  Endo (Widget a)
atMkSizedImage = atDisplay . Placable.atPlace . inFlip . result

atSizedImage :: (Size -> Endo TermImage) -> Endo (Widget a)
atSizedImage modifyImage = atMkSizedImage f
  where
    f mkImage size = modifyImage size $ mkImage size

noTakeFocus :: Endo (Widget a)
noTakeFocus = atMKeymap . const $ Nothing

takesFocus :: Endo (Widget a)
takesFocus = atMKeymap $ maybe (Just mempty) Just

strongerKeys :: Keymap a -> Endo (Widget a)
strongerKeys = atKeymap . mappend

weakerKeys :: Keymap a -> Endo (Widget a)
weakerKeys = atKeymap . flip mappend

whenFocused :: Endo (Widget k) -> Endo (Widget k)
whenFocused f (Widget mkPlacable) = Widget mkPlacable'
  where
    mkPlacable' hf  = ($hf) . unWidget . m hf . Widget $ mkPlacable
      where
        m (HasFocus True) = f
        m (HasFocus False) = id

backgroundColorWhenFocused :: Vty.Color -> Endo (Widget k)
backgroundColorWhenFocused = whenFocused . atSizedImage . TermImage.backgroundColor

coloredFocusableMkDisplay :: Vty.Color -> (HasFocus -> Display ()) -> Widget k
coloredFocusableMkDisplay c =
  takesFocus .
  (atDisplay . Align.to . pure $ 0) .
  backgroundColorWhenFocused c .
  simpleMkDisplay

coloredFocusableDisplay :: Vty.Color -> Display () -> Widget k
coloredFocusableDisplay c = coloredFocusableMkDisplay c . const

clip :: Endo (Widget k)
clip = atDisplay Display.clip

make :: (HasFocus -> Placable (TermImage, Maybe (Keymap k))) -> Widget k
make = clip . Widget

fromTuple :: (HasFocus -> (SizeRange, Size -> (TermImage, Maybe (Keymap k)))) -> Widget k
fromTuple = (argument . result . uncurry) Placable make
