{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Completion
    (make, Model(..), initModel)
where

import           Data.Monoid                      (mempty)
import           Data.Binary                      (Binary(..))
import           Data.List                        (isPrefixOf)
import           Data.List.Utils                  (safeIndex)
import           Data.Vector.Vector2              (Vector2(..))
import qualified Graphics.Vty                     as Vty
import           Graphics.UI.VtyWidgets.Widget    (Widget)
import qualified Graphics.UI.VtyWidgets.Widget    as Widget
import qualified Graphics.UI.VtyWidgets.Keymap    as Keymap
import qualified Graphics.UI.VtyWidgets.TextEdit  as TextEdit
import qualified Graphics.UI.VtyWidgets.Box       as Box
import qualified Graphics.UI.VtyWidgets.Spacer    as Spacer
import qualified Graphics.UI.VtyWidgets.TextView  as TextView
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import qualified Graphics.UI.VtyWidgets.Scroll    as Scroll
import           Control.Monad                    (liftM2)

data Model = Model {
  completionTextEdit :: TextEdit.Model,
  completionBox :: Box.Model
  }
  deriving (Show, Read, Eq, Ord)

instance Binary Model where
  get = liftM2 Model get get
  put (Model a b) = put a >> put b

initModel :: String -> Model
initModel s = Model (TextEdit.initModel s) Box.initModel

-- | See TextEdit.make for more documentation
make :: [String] -> Int -> Vty.Color -> Vty.Attr -> String -> Int -> Vty.Attr -> Vty.Attr -> Model -> Widget Model
make options maxCompletions selectedCompletionColor completionAttr emptyString maxLines unfocusedAttr focusedAttr model =
  Box.makeCombined Box.Vertical [
    Widget.strongerKeys (completeKeymap currentCompletion) .
    fmap setTextEditModel $
      TextEdit.make emptyString maxLines unfocusedAttr focusedAttr textEditModel,
    Widget.atDisplay (Spacer.indent 4 . scroller) $
      Box.make Box.Vertical setBoxModel completionTexts boxModel
    ]
  where
    scroller = Scroll.centeredView . SizeRange.fixedSize $ Vector2 maxWidth maxCompletions
    maxWidth = maximum . map length $ activeCompletions
    currentCompletion = index `safeIndex` activeCompletions
    completeKeymap Nothing = mempty
    completeKeymap (Just completionText) =
      Keymap.simpleton ("Complete to " ++ completionText) ([], Vty.KASCII '\t') $
      setTextEditModel (TextEdit.initModel completionText)
    completionTexts = map (Widget.coloredFocusableDisplay selectedCompletionColor .
                           -- TODO: Put cursor at length of string
                           TextView.make completionAttr) $
                      activeCompletions
    activeCompletions = filter (text `isPrefixOf`) options
    Model textEditModel rawBoxModel = model
    boxModel = Box.inModel (max 0 . min (length activeCompletions - 1)) rawBoxModel
    text = TextEdit.textEditText textEditModel
    index = Box.modelCursor boxModel
    setTextEditModel textEditModel' = Model textEditModel' boxModel
    setBoxModel boxModel' = Model textEditModel boxModel'
