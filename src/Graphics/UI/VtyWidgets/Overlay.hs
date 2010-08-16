{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.Overlay
    (display, widget, widgetAcc, Model(..), initModel, eventMapView)
where

import           Data.Binary                      (Binary)
import           Data.Monoid                      (mempty, mappend)
import           Data.Maybe                       (fromMaybe)
import           Data.Record.Label                ((:->), set, get)
import           Data.Vector.Vector2              (Vector2(..))
import           Data.Function.Utils              (Endo)
import           Graphics.UI.VtyWidgets.ModKey    (ModKey(..))
import           Graphics.UI.VtyWidgets.Widget    (Widget)
import qualified Graphics.UI.VtyWidgets.Widget    as Widget
import           Graphics.UI.VtyWidgets.EventMap  (Doc)
import qualified Graphics.UI.VtyWidgets.EventMap  as EventMap
import qualified Graphics.UI.VtyWidgets.TableGrid as TableGrid
import qualified Graphics.UI.VtyWidgets.Align     as Align
import           Graphics.UI.VtyWidgets.Display   (Display)
import           Graphics.UI.VtyWidgets.SizeRange (Size)

translateToCenter :: Endo (Display a)
translateToCenter = Align.to (Vector2 0.5 0.5)

display :: Display a -> Display a -> Display a
display = flip mappend . translateToCenter

newtype Model = Model {
  modelIsOverlaying :: Bool
  }
  deriving (Binary)

initModel :: Bool -> Model
initModel = Model

widget :: (Doc, ModKey) -> (Doc, ModKey) -> Widget k -> (Model -> k) -> Widget k -> Model -> Widget k
widget startShowing stopShowing overlayWidget conv child (Model isOverlaying) =
  if isOverlaying
  then appendEventMap stopShowing False $
       child `mappend` Widget.atDisplay translateToCenter overlayWidget
  else appendEventMap startShowing True
       child
  where
    appendEventMap docModKey toShow =
      Widget.strongerEvents (Widget.fromKeymap $ mkKeyMap docModKey toShow)
    mkKeyMap docModKey toShow =
      uncurry EventMap.simpleton docModKey . conv . Model $ toShow

widgetAcc :: k :-> Model ->
             (Doc, ModKey) -> (Doc, ModKey) -> Widget k -> Widget k -> k -> Widget k
widgetAcc acc startShowing stopShowing overlayWidget child k =
  widget startShowing stopShowing overlayWidget (flip (set acc) k) child (get acc k)

eventMapView :: TableGrid.Theme -> Size -> (Model -> k) -> Model ->
              ModKey -> ModKey ->
              Widget k -> Widget k
eventMapView theme size convert overlayModel showModKey hideModKey w =
  overlayedEventMapWidget
  where
    overlayedEventMapWidget =
      widget (showDoc, showModKey) (hideDoc, hideModKey)
             kv convert w overlayModel
    kv = Widget.simpleDisplay .
         TableGrid.makeEventMapView theme .
         fromMaybe mempty $
         -- We form a cycle here, as we extract the eventMap of the
         -- widget which is built by overlaying the eventMap table of
         -- the same eventMap. This should be Ok because we use
         -- simpleDisplay above which generates an empty eventMap which
         -- doesn't depend on any of this.
         Widget.eventMap overlayedEventMapWidget size
    showDoc = "Keybindings: show"
    hideDoc = "Keybindings: hide"
