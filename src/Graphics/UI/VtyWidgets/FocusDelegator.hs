{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.FocusDelegator
    (make, makeAcc, Model(..), initModel)
where

import Control.Arrow(first)
import Data.Maybe(fromMaybe)
import Data.Monoid(mappend)
import Data.Monoid.Utils(inFirst)
import Data.Accessor(Accessor, setVal, (^.))
import Data.Function.Utils(Endo)
import qualified Graphics.Vty as Vty
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

notDelegatingImageEndo :: Endo TermImage
notDelegatingImageEndo = (TermImage.atEachChar . inFirst) fixChar
  where
    fixChar mChar = Just $
                    (`Vty.with_back_color` Vty.blue) `first` fromMaybe (Vty.def_attr, ' ') mChar

make :: (Model -> k) -> Widget k -> Model -> Widget k
make conv child model =
  case model of
    Model True -> Widget.atKeymap (`mappend` fmap conv delegatingKeymap) child
    Model False -> (Widget.atKeymap . const) (fmap conv notDelegatingKeymap) .
                   Widget.atMkImage notDelegatingImage $
                   child
  where
    notDelegatingImage mkImage hf = (if Widget.hasFocus hf
                                     then notDelegatingImageEndo
                                     else id) $
                                    mkImage (Widget.HasFocus False)

setter :: w -> Accessor w p -> p -> w
setter w acc p = setVal acc p w

makeAcc :: Accessor k Model -> Widget k -> k -> Widget k
makeAcc acc child k =
  make (setter k acc) child (k ^. acc)
