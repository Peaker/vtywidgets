{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Graphics.UI.VtyWidgets.Overlay
    (display, widget, widgetAcc, Model(..), initModel)
where

import Data.Monoid(mappend)
import Data.Record.Label((:->), set, get)
import Data.Vector.Vector2(Vector2(..))
import Data.Function.Utils(Endo)
import Graphics.UI.VtyWidgets.Keymap(ModKey, Doc)
import Graphics.UI.VtyWidgets.Widget(Widget)
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import qualified Graphics.UI.VtyWidgets.Align as Align
import Graphics.UI.VtyWidgets.Display(Display)

translateToCenter :: Endo (Display a)
translateToCenter = Align.to (Vector2 0.5 0.5)

display :: Display a -> Display a -> Display a
display = flip mappend . translateToCenter

newtype Model = Model {
  modelIsOverlaying :: Bool
  }

initModel :: Bool -> Model
initModel = Model

widget :: (Doc, ModKey) -> (Doc, ModKey) -> Widget k -> (Model -> k) -> Widget k -> Model -> Widget k
widget startShowing stopShowing overlayWidget conv child (Model isOverlaying) =
  if isOverlaying
  then appendKeymap stopShowing False $
       child `mappend` Widget.atDisplay translateToCenter overlayWidget
  else appendKeymap startShowing True
       child
  where
    appendKeymap docModKey toShow =
      Widget.atKeymap (`mappend` mkKeymap docModKey toShow)
    mkKeymap docModKey toShow =
      uncurry Keymap.simpleton docModKey . conv . Model $ toShow

widgetAcc :: k :-> Model ->
             (Doc, ModKey) -> (Doc, ModKey) -> Widget k -> Widget k -> k -> Widget k
widgetAcc acc startShowing stopShowing overlayWidget child k =
  widget startShowing stopShowing overlayWidget (flip (set acc) k) child (get acc k)
