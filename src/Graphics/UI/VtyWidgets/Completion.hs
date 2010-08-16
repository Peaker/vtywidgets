{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Completion
    (Model(..), initModel, make, makeSimple,
     Theme(..), standardTheme)
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
import qualified Graphics.UI.VtyWidgets.EventMap  as EventMap
import           Graphics.UI.VtyWidgets.ModKey    (ModKey(..))
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

data Theme = Theme {
  themeSelectedBGColor :: Vty.Color,
  themePrefixAttr :: Vty.Attr,
  themeSuffixAttr :: Vty.Attr,
  themeTextEdit :: TextEdit.Theme
  }

-- | See TextEdit.make for more documentation
make :: Theme -> [(String, Int)] -> Int -> String -> Int -> Model -> Widget Model
make theme options maxCompletions emptyString maxLines model =
  Widget.whenFocused addCompletionBox textEdit
  where
    addCompletionBox = Box.makeCombined Box.Vertical . (: [completionsBox])
    textEdit = Widget.strongerEvents (Widget.fromKeymap . completeEventMap $ fmap fst currentCompletion) .
               fmap setTextEditModel $
               TextEdit.make (themeTextEdit theme) emptyString maxLines textEditModel
    completionsBox = setCursor Nothing .
                     Widget.atDisplay (Spacer.indent 4 . scroller) $
                     Box.make Box.Vertical setBoxModel completionTexts boxModel
    scroller = Scroll.centeredView Scroll.standardTheme . SizeRange.fixedSize $ Vector2 (maxWidth+1) maxCompletions
    maxWidth = maximum . (0:) .
               map (length . fst) $ activeCompletions
    currentCompletion = index `safeIndex` activeCompletions
    completeEventMap Nothing = mempty
    completeEventMap (Just completionText) =
      EventMap.simpleton ("Complete to " ++ completionText) (ModKey [] (Vty.KASCII '\t')) $
      setTextEditModel (TextEdit.initModel completionText)
    completionTexts = map makeSingleCompletionView activeCompletions
    makeSingleCompletionView (completion, cursor) =
      (setCursor . Just $ Vector2 cursor 0) .
      Widget.coloredFocusableDisplay (themeSelectedBGColor theme) $
        Box.makeView Box.Horizontal [
          TextView.make (themePrefixAttr theme) prefix,
          TextView.make (themeSuffixAttr theme) suffix
          ]
      where
        (prefix, suffix) = splitAt cursor completion
    activeCompletions = filter ((text `isPrefixOf`) . fst) options
    Model textEditModel rawBoxModel = model
    boxModel = Box.inModel (max 0 . min (length activeCompletions - 1)) rawBoxModel
    text = TextEdit.textEditText textEditModel
    index = Box.modelCursor boxModel
    setTextEditModel textEditModel' = Model textEditModel' boxModel
    setBoxModel boxModel' = Model textEditModel boxModel'

makeSimple :: Theme -> [String] -> Int -> String -> Int -> Model -> Widget Model
makeSimple theme options maxCompletions emptyString maxLines model =
    make theme cursorOptions maxCompletions emptyString maxLines model
  where
    cursorOptions = map (flip (,) cursor) options
    cursor = length . TextEdit.textEditText . completionTextEdit $ model

standardTheme :: Theme
standardTheme = Theme {
  themeSelectedBGColor = Vty.blue,
  themePrefixAttr = fg Vty.green,
  themeSuffixAttr = fg Vty.red,
  themeTextEdit = TextEdit.standardTheme
  }
  where
    fg color = Vty.def_attr `Vty.with_fore_color` color
