{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Widget
    (HasFocus(..), inHasFocus,
     Widget(..), inWidget, inWidget2, runWidget,
     atHasFocus, atPlacable,
     Direction(..),
     Event(..), EventMap, KeyMap, fromKeymap,
     atMEventMap, atEventMap,
     atImage, atMkSizedImage, atSizedImage,
     atMkDisplay, atDisplay,
     takesFocus, noTakeFocus,
     make, fromTuple, fromKeymapTuple,
     simpleMkDisplay, simpleDisplay,
     fromDisplay, toDisplay,
     eventMap, image, requestedSize,
     strongerEvents, weakerEvents,
     whenFocused,
     backgroundColorWhenFocused,
     coloredFocusableMkDisplay,
     coloredFocusableDisplay)
where

import           Control.Arrow                    (first, second)
import           Control.Applicative              (pure)
import           Data.Monoid                      (Monoid(..))
import           Data.Function.Utils              (Endo, argument, result, inFlip)
import qualified Graphics.Vty                     as Vty
import           Graphics.UI.VtyWidgets.SizeRange (SizeRange(..), Size)
import qualified Graphics.UI.VtyWidgets.Placable  as Placable
import           Graphics.UI.VtyWidgets.Placable  (Placable(..))
import qualified Graphics.UI.VtyWidgets.Display   as Display
import           Graphics.UI.VtyWidgets.Display   (Display)
import qualified Graphics.UI.VtyWidgets.EventMap  as EventMap
import           Graphics.UI.VtyWidgets.ModKey    (ModKey)
import           Graphics.UI.VtyWidgets.TermImage (TermImage)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import qualified Graphics.UI.VtyWidgets.Align     as Align

-- internal convenience alias
type EM = EventMap.EventMap

data Direction = Left | Right | Top | Bottom
  deriving (Eq, Ord, Read, Show)

data Event = KeyEvent ModKey |
             EnterEvent Direction
  deriving (Eq, Ord, Show)
type EventMap = EM Event
type KeyMap = EM ModKey

newtype HasFocus = HasFocus { hasFocus :: Bool }
  deriving (Show, Read, Eq, Ord)
inHasFocus :: Endo Bool -> Endo HasFocus
inHasFocus f = HasFocus . f . hasFocus

type InWidget k = HasFocus -> Placable (TermImage, Maybe (EventMap k))
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

fromDisplay :: (Size -> Maybe (EventMap k)) -> (HasFocus -> Display ()) -> Widget k
fromDisplay mkEventMap mkDisplay = Widget $ Placable.atPlace mkImageToWidgetTuple . mkDisplay
  where
    mkImageToWidgetTuple mkImage size = (mkImage size (), mkEventMap size)

toDisplay :: Widget k -> HasFocus -> Display a
toDisplay w hf = fmap (const . fst) . ($ hf) . unWidget $ w

atMkDisplay :: Endo (HasFocus -> Display ()) -> Endo (Widget k)
atMkDisplay f w = fromDisplay (eventMap w) . f . toDisplay $ w

atDisplay :: Endo (Display ()) -> Endo (Widget k)
atDisplay = atMkDisplay . result

simpleMkDisplay :: (HasFocus -> Display ()) -> Widget k
simpleMkDisplay = fromDisplay . pure $ Nothing

simpleDisplay :: Display () -> Widget k
simpleDisplay = simpleMkDisplay . const

requestedSize :: Widget k -> HasFocus -> SizeRange
requestedSize = (result . result) Placable.pRequestedSize unWidget

runWidget :: Widget k -> Size -> (TermImage, Maybe (EventMap k))
runWidget widget size = Placable.pPlace (unWidget widget $ HasFocus True) size

eventMap :: Widget k -> Size -> Maybe (EventMap k)
eventMap w = snd . runWidget w

image :: Widget k -> Size -> TermImage
image w = fst . runWidget w

atHasFocus :: Endo Bool -> Endo (Widget a)
atHasFocus = inWidget . argument . inHasFocus

atPlacable :: (Placable (TermImage, Maybe (EventMap k)) ->
               Placable (TermImage, Maybe (EventMap k'))) ->
              Widget k -> Widget k'
atPlacable = inWidget . result

atImage :: Endo TermImage -> Endo (Widget a)
atImage = atPlacable . fmap . first

atMEventMap :: (Maybe (EventMap a) -> Maybe (EventMap b)) ->
             Widget a -> Widget b
atMEventMap = atPlacable . fmap . second

atEventMap :: (EventMap a -> EventMap b) ->
            Widget a -> Widget b
atEventMap = atMEventMap . fmap

instance Functor Widget where
  fmap = atEventMap . fmap

atMkSizedImage :: Endo (Size -> TermImage) ->
                  Endo (Widget a)
atMkSizedImage = atDisplay . Placable.atPlace . inFlip . result

atSizedImage :: (Size -> Endo TermImage) -> Endo (Widget a)
atSizedImage modifyImage = atMkSizedImage f
  where
    f mkImage size = modifyImage size $ mkImage size

noTakeFocus :: Endo (Widget a)
noTakeFocus = atMEventMap . const $ Nothing

takesFocus :: Endo (Widget a)
takesFocus = atMEventMap $ maybe (Just mempty) Just

strongerEvents :: EventMap a -> Endo (Widget a)
strongerEvents = atEventMap . mappend

weakerEvents :: EventMap a -> Endo (Widget a)
weakerEvents = atEventMap . flip mappend

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

make :: (HasFocus -> Placable (TermImage, Maybe (EventMap k))) -> Widget k
make = clip . Widget

fromTuple :: (HasFocus -> (SizeRange, Size -> (TermImage, Maybe (EventMap k)))) -> Widget k
fromTuple = (argument . result . uncurry) Placable make

fromKeymap :: KeyMap k -> EventMap k
fromKeymap = EventMap.mapKeys KeyEvent

fromKeymapTuple :: (HasFocus -> (SizeRange, Size -> (TermImage, Maybe (KeyMap k)))) -> Widget k
fromKeymapTuple = (argument . result . second . result . second . fmap) fromKeymap fromTuple
