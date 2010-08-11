{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.FocusDelegator
    (make, makeAcc, Model(..), initModel)
where

import           Data.Binary                      (Binary)
import           Data.Record.Label                ((:->), set, get)
import qualified Graphics.Vty                     as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import           Graphics.UI.VtyWidgets.Widget    (Widget)
import           Graphics.UI.VtyWidgets.Keymap    (Keymap)
import qualified Graphics.UI.VtyWidgets.Keymap    as Keymap
import qualified Graphics.UI.VtyWidgets.Widget    as Widget

newtype Model = Model {
  focusDelegated :: Bool
  }
  deriving (Eq, Ord, Read, Show, Binary)

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

make :: (Model -> k) -> Widget k -> Model -> Widget k
make conv child (Model isDelegating) =
  if isDelegating
    then Widget.weakerKeys (conv `fmap` delegatingKeymap) child
    else Widget.whenFocused ((Widget.atImage . TermImage.inCursor . const) Nothing .
                             (Widget.atSizedImage . TermImage.backgroundColor) Vty.blue) .
         Widget.takesFocus .
         (Widget.atKeymap . const) (conv `fmap` notDelegatingKeymap) $
         child

makeAcc :: k :-> Model -> Widget k -> k -> Widget k
makeAcc acc child k =
  make (flip (set acc) k) child (get acc k)
