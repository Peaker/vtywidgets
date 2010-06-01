{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.FocusDelegator
    (make, makeAcc, Model(..), initModel)
where

import Control.Applicative(pure)
import Control.Arrow(first)
import Data.Vector.Rect(Rect(..))
import Data.Vector.Vector2(Vector2)
import Data.Monoid(mappend)
import Data.Accessor(Accessor, setVal, (^.))
import Data.Function.Utils(Endo)
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.Placable as Placable
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.TermImage(TermImage)
import Graphics.UI.VtyWidgets.Widget(Widget)
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import qualified Graphics.UI.VtyWidgets.Widget as Widget

newtype Model = Model {
  focusDelegated :: Bool
  }

initModel :: Bool -> Model
initModel = Model

stopDelegatingKey :: Keymap.ModKey
stopDelegatingKey = ([], Vty.KEsc)

startDelegatingKey :: Keymap.ModKey
startDelegatingKey = ([], Vty.KEnter)

delegatingKeymap :: Keymap Model
delegatingKeymap = Keymap.simpleton "Leave" stopDelegatingKey (Model False)

notDelegatingKeymap :: Keymap Model
notDelegatingKeymap = Keymap.simpleton "Enter" startDelegatingKey (Model True)

notDelegatingImageEndo :: Vector2 Int -> Endo TermImage
notDelegatingImageEndo size =
  (TermImage.inCursor . const) Nothing .
  (`mappend` TermImage.rect (Rect (pure 0) size) (first (`Vty.with_back_color` Vty.blue)))

make :: (Model -> k) -> Widget k -> Model -> Widget k
make conv child (Model isDelegating) =
  if isDelegating
    then Widget.atKeymap (`mappend` fmap conv delegatingKeymap) child
    else (Widget.atKeymap . const) (fmap conv notDelegatingKeymap) .
         (Widget.inWidget . Placable.atPlace) notDelegating $
         child
  where
    notDelegating sizeToPair size = notDelegatingImage size `first` sizeToPair size
    notDelegatingImage size mkImage hf =
      (if Widget.hasFocus hf
       then notDelegatingImageEndo size
       else id) $
      mkImage (Widget.HasFocus False)

setter :: w -> Accessor w p -> p -> w
setter w acc p = setVal acc p w

makeAcc :: Accessor k Model -> Widget k -> k -> Widget k
makeAcc acc child k =
  make (setter k acc) child (k ^. acc)
