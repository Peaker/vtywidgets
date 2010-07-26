{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}

import qualified Graphics.Vty as Vty
import qualified Data.Record.Label as Label
import Data.Record.Label((:->), mkLabels, label)
import Data.Record.Label.Tuple(first, second)
import Data.Monoid(mappend)
import Data.Vector.Vector2(Vector2(..))
import Prelude hiding ((.))
import Control.Category((.))
import Control.Applicative(pure)
import Control.Concurrent.MVar(MVar, newMVar, readMVar, modifyMVar_)
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Keymap(Keymap, ModKey)
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Graphics.UI.VtyWidgets.Widget(Widget)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import qualified Graphics.UI.VtyWidgets.Align as Align
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextView as TextView
import qualified Graphics.UI.VtyWidgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.VtyWidgets.Scroll as Scroll
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Graphics.UI.VtyWidgets.Run as Run
import System.IO(stderr, hSetBuffering, BufferMode(NoBuffering))

nthSet :: Int -> a -> [a] -> [a]
nthSet _ _ [] = error "IndexError in nthSet"
nthSet 0 x' (_:xs) = x' : xs
nthSet n x' (x:xs) = x : nthSet (n-1) x' xs

nth :: Int -> [a] :-> a
nth n = label (!! n) (nthSet n)

data Model = Model {
  _modelGrid :: (FocusDelegator.Model, Grid.Model),
  _modelTextEdits :: [(FocusDelegator.Model, TextEdit.Model)],
  _modelDelegators :: [FocusDelegator.Model]
  }
$(mkLabels [''Model])

modelGrid :: Model :-> (FocusDelegator.Model, Grid.Model)
modelTextEdits :: Model :-> [(FocusDelegator.Model, TextEdit.Model)]
modelDelegators :: Model :-> [FocusDelegator.Model]

initModel :: Model
initModel = Model {
  _modelGrid = (FocusDelegator.initModel True, Grid.initModel),
  _modelTextEdits = map ((,) (FocusDelegator.initModel True) . TextEdit.initModel) ["abc\ndef", "i\nlala", "oopsy daisy", "hehe"],
  _modelDelegators = replicate 2 $ FocusDelegator.initModel False
  }

quitKey :: ModKey
quitKey = ([Vty.MCtrl], Vty.KASCII 'q')

pureModifyMVar_ :: MVar a -> (a -> a) -> IO ()
pureModifyMVar_ mv f = modifyMVar_ mv (return . f)

main :: IO ()
main = do
  hSetBuffering stderr NoBuffering
  mainLoop =<< newMVar initModel
  where
    mainLoop modelMVar = Run.widgetLoopWithOverlay 20 30 . const $ rootWidget
      where
        rootWidget = modelEdit fixKeymap `fmap`
                     readMVar modelMVar
        fixKeymap = (Keymap.simpleton "Quit" quitKey (fail "Quit") `mappend`) .
                    ((pureModifyMVar_ modelMVar . const) `fmap`)

modelEdit :: (Keymap Model -> Keymap k) -> Model -> Widget k
modelEdit fixKeymap model =
  Widget.atKeymap fixKeymap .
  Widget.atDisplay outerGrid $
  innerGrid
  where
    outerGrid innerGridDisp =
      makeGridView (pure 0)
      [ [ TextView.make attr "Title\n-----" ],
        [ innerGridDisp ]
      ]
    delegatedTextView i = FocusDelegator.makeAcc (nth i . modelDelegators)
                          (Widget.simpleDisplay . TextView.make Vty.def_attr $ "static" ++ show i ++ " ") model
    innerGrid =
      Widget.atDisplay (Scroll.centeredView . SizeRange.fixedSize $ Vector2 90 6) $
      makeGrid modelGrid (pure 0) $ map delegatedTextView [0..1] : textEdits
    textEdits =
      [ [ makeTextEdit (nth i . modelTextEdits)
        | y <- [0, 1]
        , let i = y*2 + x ]
      | x <- [0, 1] ]
    attr = Vty.def_attr `Vty.with_fore_color` Vty.yellow
    makeGridView alignment =
      Grid.makeView . (map . map) (Align.to alignment)
    makeTextEdit acc =
      FocusDelegator.makeAcc (first . acc) textEdit model
      where
        textEdit = adaptModel (second . acc) (TextEdit.make "<insert text here>" 5 attr TextEdit.editingAttr) model
    makeGrid acc alignment rows =
      FocusDelegator.makeAcc (first . acc) grid model
      where
        grid = Grid.makeAcc (second . acc)
               ((map . map . Widget.atDisplay) (Align.to alignment) rows)
               model

adaptModel :: w :-> p -> (p -> Widget p) -> w -> Widget w
adaptModel acc pwidget w = Widget.atKeymap (flip (Label.set acc) w `fmap`) (pwidget (Label.get acc w))
