{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.FocusDelegator
    (make, makeAcc, Model(..), initModel)
where

import Control.Applicative(pure)
import Control.Arrow(first, second)
import Data.Binary(Binary)
import Data.Vector.Rect(Rect(..))
import Data.Vector.Vector2(Vector2)
import Data.Monoid(mempty, mappend)
import Data.Record.Label((:->), set, get)
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
  deriving (Binary)

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
    then Widget.strongerKeys (conv `fmap` delegatingKeymap) child
    else Widget.atMkSizedImage notDelegatingMkImage .
         (Widget.inWidget . Placable.atPlace) nonDelegatingReplace $
         child
  where
    notDelegatingMkImage mkSizedImage size hf =
      notDelegatingImage size (Widget.hasFocus hf) $
      mkSizedImage size hf
    -- When focus isn't on us, we don't tell whether we delegate or
    -- not
    notDelegatingImage  size True = notDelegatingImageEndo size
    notDelegatingImage _size False = id
    nonDelegatingReplace childPlace size = modifyKeymap `second` childPlace size
    modifyKeymap = maybe (Just mempty) (const . Just $ conv `fmap` notDelegatingKeymap)

makeAcc :: k :-> Model -> Widget k -> k -> Widget k
makeAcc acc child k =
  make (flip (set acc) k) child (get acc k)
