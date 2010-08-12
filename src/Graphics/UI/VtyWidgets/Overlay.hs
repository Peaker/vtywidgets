{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.Overlay
    (display, widget, widgetAcc, Model(..), initModel, keymapView)
where

import           Data.Binary                      (Binary)
import           Data.Monoid                      (mempty, mappend)
import           Data.Maybe                       (fromMaybe)
import           Data.Record.Label                ((:->), set, get)
import           Data.Vector.Vector2              (Vector2(..))
import           Data.Function.Utils              (Endo)
import           Graphics.UI.VtyWidgets.Keymap    (ModKey, Doc)
import           Graphics.UI.VtyWidgets.Widget    (Widget)
import qualified Graphics.UI.VtyWidgets.Widget    as Widget
import qualified Graphics.UI.VtyWidgets.Keymap    as Keymap
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
  then appendKeymap stopShowing False $
       child `mappend` Widget.atDisplay translateToCenter overlayWidget
  else appendKeymap startShowing True
       child
  where
    appendKeymap docModKey toShow =
      Widget.strongerKeys (mkKeymap docModKey toShow)
    mkKeymap docModKey toShow =
      uncurry Keymap.simpleton docModKey . conv . Model $ toShow

widgetAcc :: k :-> Model ->
             (Doc, ModKey) -> (Doc, ModKey) -> Widget k -> Widget k -> k -> Widget k
widgetAcc acc startShowing stopShowing overlayWidget child k =
  widget startShowing stopShowing overlayWidget (flip (set acc) k) child (get acc k)

keymapView :: TableGrid.Theme -> Size -> (Model -> k) -> Model ->
              ModKey -> ModKey ->
              Widget k -> Widget k
keymapView theme size convert overlayModel showModKey hideModKey w =
  overlayedKeymapWidget
  where
    overlayedKeymapWidget =
      widget (showDoc, showModKey) (hideDoc, hideModKey)
             kv convert w overlayModel
    kv = Widget.simpleDisplay .
         TableGrid.makeKeymapView theme .
         fromMaybe mempty $
         -- We form a cycle here, as we extract the keymap of the
         -- widget which is built by overlaying the keymap table of
         -- the same keymap. This should be Ok because we use
         -- simpleDisplay above which generates an empty keymap which
         -- doesn't depend on any of this.
         Widget.keymap overlayedKeymapWidget size
    showDoc = "Keybindings: show"
    hideDoc = "Keybindings: hide"
