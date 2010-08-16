{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.FocusDelegator
    (make, makeAcc, Model(..), inModel, initModel,
     Theme(..), standardTheme)
where

import           Data.Function.Utils              (Endo)
import           Data.Binary                      (Binary)
import           Data.Record.Label                ((:->), set, get)
import qualified Graphics.Vty                     as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import           Graphics.UI.VtyWidgets.Widget    (Widget)
import           Graphics.UI.VtyWidgets.ModKey    (ModKey(..))
import           Graphics.UI.VtyWidgets.EventMap  (EventMap)
import qualified Graphics.UI.VtyWidgets.EventMap  as EventMap
import qualified Graphics.UI.VtyWidgets.Widget    as Widget

data Theme = Theme {
  themeBGColor :: Vty.Color
  }

newtype Model = Model {
  focusDelegated :: Bool
  }
  deriving (Eq, Ord, Read, Show, Binary)
inModel :: Endo Bool -> Endo Model
inModel f = Model . f . focusDelegated

initModel :: Bool -> Model
initModel = Model

stopDelegatingKey :: ModKey
stopDelegatingKey = ModKey [] Vty.KEsc

startDelegatingKey :: ModKey
startDelegatingKey = ModKey [] Vty.KEnter

delegatingKeyMap :: EventMap ModKey Model
delegatingKeyMap = EventMap.simpleton "Leave" stopDelegatingKey (Model False)

notDelegatingKeyMap :: EventMap ModKey Model
notDelegatingKeyMap = EventMap.simpleton "Enter" startDelegatingKey (Model True)

make :: Theme -> (Model -> k) -> Widget k -> Model -> Widget k
make theme conv child (Model isDelegating) =
  if isDelegating
    then Widget.weakerEvents (conv `fmap` Widget.fromKeymap delegatingKeyMap) child
    else Widget.whenFocused ((Widget.atImage . TermImage.inCursor . const) Nothing .
                             (Widget.atSizedImage . TermImage.backgroundColor) (themeBGColor theme)) .
         Widget.takesFocus .
         (Widget.atEventMap . const) (conv `fmap` Widget.fromKeymap notDelegatingKeyMap) .
         (Widget.atHasFocus . const) False $
         child

makeAcc :: Theme -> k :-> Model -> Widget k -> k -> Widget k
makeAcc theme acc child k =
  make theme (flip (set acc) k) child (get acc k)

standardTheme :: Theme
standardTheme = Theme {
  themeBGColor = Vty.blue
  }