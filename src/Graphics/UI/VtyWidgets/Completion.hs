{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Completion
    (Model(..), initModel, make, makeSimple)
where

import           Data.Function.Utils              (Endo)
import           Data.Monoid                      (mempty)
import           Data.Binary                      (Binary(..))
import           Data.List                        (isPrefixOf)
import           Data.List.Utils                  (safeIndex)
import           Data.Vector.Vector2              (Vector2(..))
import qualified Graphics.Vty                     as Vty
import           Graphics.UI.VtyWidgets.Widget    (Widget)
import qualified Graphics.UI.VtyWidgets.Widget    as Widget
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
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

setCursor :: Maybe TermImage.Coordinate -> Endo (Widget a)
setCursor = Widget.atImage . TermImage.inCursor . const

-- | See TextEdit.make for more documentation
make :: [(String, Int)] -> Int -> Vty.Color -> Vty.Attr -> Vty.Attr ->
        String -> Int -> Vty.Attr -> Vty.Attr -> Model -> Widget Model
make options maxCompletions selectedCompletionBGColor
     completionBeforeAttr completionAfterAttr
     emptyString maxLines unfocusedAttr focusedAttr model =
  Box.makeCombined Box.Vertical [
    Widget.strongerKeys (completeKeymap $ fmap fst currentCompletion) .
    fmap setTextEditModel $
      TextEdit.make emptyString maxLines unfocusedAttr focusedAttr textEditModel,
    setCursor Nothing .
    Widget.atDisplay (Spacer.indent 4 . scroller) $
      Box.make Box.Vertical setBoxModel completionTexts boxModel
    ]
  where
    scroller = Scroll.centeredView . SizeRange.fixedSize $ Vector2 (maxWidth+1) maxCompletions
    maxWidth = maximum . (0:) .
               map (length . fst) $ activeCompletions
    currentCompletion = index `safeIndex` activeCompletions
    completeKeymap Nothing = mempty
    completeKeymap (Just completionText) =
      Keymap.simpleton ("Complete to " ++ completionText) ([], Vty.KASCII '\t') $
      setTextEditModel (TextEdit.initModel completionText)
    completionTexts = map makeSingleCompletionView activeCompletions
    makeSingleCompletionView (completion, cursor) =
      (setCursor . Just $ Vector2 cursor 0) .
      Widget.coloredFocusableDisplay selectedCompletionBGColor $
        Box.makeView Box.Horizontal [
          TextView.make completionBeforeAttr before,
          TextView.make completionAfterAttr after
          ]
      where
        (before, after) = splitAt cursor completion
    activeCompletions = filter ((text `isPrefixOf`) . fst) options
    Model textEditModel rawBoxModel = model
    boxModel = Box.inModel (max 0 . min (length activeCompletions - 1)) rawBoxModel
    text = TextEdit.textEditText textEditModel
    index = Box.modelCursor boxModel
    setTextEditModel textEditModel' = Model textEditModel' boxModel
    setBoxModel boxModel' = Model textEditModel boxModel'

makeSimple :: [String] -> Int -> Vty.Color -> Vty.Attr -> Vty.Attr ->
              String -> Int -> Vty.Attr -> Vty.Attr -> Model -> Widget Model
makeSimple options maxCompletions selectedCompletionBGColor
  completionBeforeAttr completionAfterAttr
  emptyString maxLines unfocusedAttr focusedAttr model =
    make cursorOptions maxCompletions selectedCompletionBGColor
         completionBeforeAttr completionAfterAttr
         emptyString maxLines unfocusedAttr focusedAttr model
  where
    cursorOptions = map (flip (,) cursor) options
    cursor = length . TextEdit.textEditText . completionTextEdit $ model
