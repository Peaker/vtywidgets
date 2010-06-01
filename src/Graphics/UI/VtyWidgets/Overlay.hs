{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Overlay
    (display, widget, widgetAcc, Model(..))
where

import Control.Applicative(liftA2)
import Data.Monoid(mappend)
import Data.Accessor(Accessor, (^.), setVal)
import Graphics.UI.VtyWidgets.Keymap(ModKey, Doc)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.Widget(Widget)
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import qualified Graphics.UI.VtyWidgets.Placable as Placable
import Graphics.UI.VtyWidgets.Display(Display)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange

-- TODO: Grid Item is really all about this, can share code:
translateToCenter :: Display a -> Display a
translateToCenter disp = Placable.atPlace translate disp
  where
    dispMinRS = SizeRange.srMinSize . Placable.pRequestedSize $ disp
    translate mkImage givenSize imgarg = TermImage.translate (halfPos givenSize dispSize) $
                                         mkImage givenSize imgarg
      where
        dispSize = liftA2 min dispMinRS givenSize
    halfPos bigSize smallSize = (`div` 2) `fmap` liftA2 (-) bigSize smallSize

display :: Display a -> Display a -> Display a
display = flip mappend . translateToCenter

newtype Model = Model {
  modelIsOverlaying :: Bool
  }

widget :: (Doc, ModKey) -> (Doc, ModKey) -> Widget k -> (Model -> k) -> Widget k -> Model -> Widget k
widget startShowing stopShowing overlayWidget conv child (Model isOverlaying) =
  if isOverlaying
  then appendKeymap stopShowing False $
       child `mappend` Widget.atDisplay translateToCenter overlayWidget
  else appendKeymap startShowing True $
       child
  where
    appendKeymap docModKey toShow =
      Widget.atKeymap (`mappend` mkKeymap docModKey toShow)
    mkKeymap docModKey toShow =
      uncurry Keymap.simpleton docModKey . conv . Model $ toShow

setter :: w -> Accessor w p -> p -> w
setter w acc p = setVal acc p w

widgetAcc :: Accessor k Model ->
             (Doc, ModKey) -> (Doc, ModKey) -> Widget k -> Widget k -> k -> Widget k
widgetAcc acc startShowing stopShowing overlayWidget child k =
  widget startShowing stopShowing overlayWidget (setter k acc) child (k ^. acc)
