{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}

import qualified Graphics.Vty                          as Vty
import qualified Data.Record.Label                     as Label
import           Data.Record.Label                     ((:->), mkLabels, lens)
import           Data.Record.Label.Tuple               (first, second)
import           Data.Record.Label.List                (nth)
import           Data.Monoid                           (mempty, mappend)
import           Data.Vector.Vector2                   (Vector2(..))
import           Prelude                               hiding ((.))
import           Control.Category                      ((.))
import           Control.Concurrent.MVar               (MVar, newMVar, readMVar, modifyMVar_)
import qualified Graphics.UI.VtyWidgets.Keymap         as Keymap
import           Graphics.UI.VtyWidgets.Keymap         (Keymap, ModKey)
import qualified Graphics.UI.VtyWidgets.Widget         as Widget
import           Graphics.UI.VtyWidgets.Widget         (Widget)
import qualified Graphics.UI.VtyWidgets.SizeRange      as SizeRange
import qualified Graphics.UI.VtyWidgets.Box            as Box
import qualified Graphics.UI.VtyWidgets.Grid           as Grid
import qualified Graphics.UI.VtyWidgets.TextView       as TextView
import qualified Graphics.UI.VtyWidgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.VtyWidgets.Scroll         as Scroll
import qualified Graphics.UI.VtyWidgets.TextEdit       as TextEdit
import qualified Graphics.UI.VtyWidgets.Completion     as Completion
import qualified Graphics.UI.VtyWidgets.TableGrid      as TableGrid
import qualified Graphics.UI.VtyWidgets.Run            as Run
import           System.IO                             (stderr, hSetBuffering, BufferMode(NoBuffering))

type Delegated a = (FocusDelegator.Model, a)

data Model = Model {
  _modelGrid :: Delegated Grid.Model,
  _modelTextEdits :: [Delegated TextEdit.Model],
  _modelDelegators :: [FocusDelegator.Model],
  _modelCompletion :: Delegated Completion.Model
  }
$(mkLabels [''Model])

-- modelGrid :: Model :-> Delegated Grid.Model
-- modelTextEdits :: Model :-> [Delegated TextEdit.Model]
-- modelDelegators :: Model :-> [FocusDelegator.Model]
-- modelCompletion :: Model :-> Delegated Completion.Model

initModel :: Model
initModel = Model {
  _modelGrid = (FocusDelegator.initModel True, Grid.initModel),
  _modelTextEdits = map ((,) (FocusDelegator.initModel True) . TextEdit.initModel) ["abc\ndef", "i\nlala", "oopsy daisy", "hehe"],
  _modelDelegators = replicate 2 $ FocusDelegator.initModel False,
  _modelCompletion = (FocusDelegator.initModel True, Completion.initModel "")
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
    mainLoop modelMVar = Run.widgetLoopWithOverlay TableGrid.standardTheme . const $ rootWidget
      where
        rootWidget = modelEdit fixKeymap `fmap` readMVar modelMVar
        fixKeymap = (mappend . Keymap.simpleton "Quit" quitKey $ fail "Quit") .
                    (fmap $ pureModifyMVar_ modelMVar . const)

modelEdit :: (Keymap Model -> Keymap k) -> Model -> Widget k
modelEdit fixKeymap model =
  Widget.atKeymap fixKeymap .
  Widget.atDisplay outerGrid $
  innerGrid
  where
    outerGrid innerGridDisp =
      Box.makeView Box.Vertical [ TextView.make attr "Title\n-----", innerGridDisp ]
    delegatedTextView i = makeFocusDelegator (nth i . modelDelegators) (staticTextView i) model
    completions = (["hello", "world", "Mr Jones: completion expert"] ++) .
                    map (("Prefix"++) . show) $
                    ([0..9] :: [Int])
    completionEdit acc =
        adaptModel acc
        (Completion.makeSimple Completion.standardTheme completions 5 "<empty>" 1)
        model
    staticTextView i = Widget.simpleDisplay . TextView.make Vty.def_attr $ "static" ++ show i ++ " "
    innerGrid = scrollerAround . makeGrid modelGrid $ map delegatedTextView [0..1] : [mempty, makeCompletionEdit modelCompletion] : textEdits
    scrollerAround = Widget.atDisplay . Scroll.centeredView Scroll.standardTheme . SizeRange.fixedSize $ Vector2 90 12
    textEdits = [ [ makeTextEdit (nth i . modelTextEdits)
                  | y <- [0, 1]
                  , let i = y*2 + x ]
                | x <- [0, 1] ]
    attr = Vty.def_attr `Vty.with_fore_color` Vty.yellow
    makeFocusDelegator = FocusDelegator.makeAcc FocusDelegator.standardTheme
    makeTextEdit acc = makeFocusDelegator (first . acc) (textEdit (second . acc)) model
    makeCompletionEdit acc = makeFocusDelegator (first . acc) (completionEdit (second . acc)) model
    textEdit acc = adaptModel acc (TextEdit.make TextEdit.standardTheme "<insert text here>" 5) model
    makeGrid acc rows = makeFocusDelegator (first . acc) (grid (second . acc) rows) model
    grid acc rows = Grid.makeAcc acc rows model

adaptModel :: w :-> p -> (p -> Widget p) -> w -> Widget w
adaptModel acc pwidget w =
    Widget.atKeymap (flip (Label.setL acc) w `fmap`) .
    pwidget .
    Label.getL acc $
    w
