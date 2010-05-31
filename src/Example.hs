{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Graphics.Vty as Vty
import Data.Accessor(Accessor, accessor, (^.), (^:), setVal)
import qualified Data.Accessor.Template as AT
import Data.Monoid(mempty, mappend)
import Data.Vector.Vector2(Vector2(..))
import Prelude hiding ((.))
import Control.Category((.))
import Control.Monad(forever)
import Control.Applicative(pure)
import Control.Concurrent.MVar(MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad.Trans.State(evalStateT, get, put)
import Control.Monad.Trans(MonadIO, liftIO)
import Graphics.Vty.Utils(withVty)
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Keymap(Keymap, ModKey)
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Graphics.UI.VtyWidgets.Widget(Widget)
import qualified Graphics.UI.VtyWidgets.Display as Display
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import Graphics.UI.VtyWidgets.SizeRange(Size)
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextView as TextView
import qualified Graphics.UI.VtyWidgets.Scroll as Scroll
import qualified Graphics.UI.VtyWidgets.Spacer as Spacer
import qualified Graphics.UI.VtyWidgets.TableGrid as TableGrid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import System.IO(stderr, hSetBuffering, BufferMode(NoBuffering), hPutStrLn)


defaultSize :: Vector2 Int
defaultSize = Vector2 80 25

runWidgetLoop :: (String -> IO ()) -> (Size -> IO (Widget (IO ()))) -> IO ()
runWidgetLoop logMsg makeWidget = do
  withVty $ \vty -> (`evalStateT` defaultSize) . forever $ do
    size <- get
    event <- liftIO $ do
      widget <- makeWidget size
      Vty.update vty . TermImage.render .
        Widget.image widget $ size
      Vty.next_event $ vty
    widget <- liftIO $ do
      logMsg $ "Handling event: " ++ show event
      -- Remake widget because logMsg is allowed to modify it...
      makeWidget size
    case event of
      Vty.EvResize w h -> do
        let size' = Vector2 w h
        put size'
        liftIO . logMsg $ "Resized to: " ++ show size'
      Vty.EvKey key mods -> do
        maybe (return ()) (liftIO . snd . snd) .
          Keymap.lookup (mods, key) . Widget.keymap widget $ size
      _ -> return ()


nthSet :: Int -> a -> [a] -> [a]
nthSet _ _ [] = error "IndexError in nthSet"
nthSet 0 x' (_:xs) = x' : xs
nthSet n x' (x:xs) = x : nthSet (n-1) x' xs

nth :: Int -> Accessor [a] a
nth n = accessor (!! n) (nthSet n)

shortDebugLogLimit :: Int
shortDebugLogLimit = 5

longDebugLogLimit :: Int
longDebugLogLimit = 100

data Model = Model {
  modelGrid_ :: Grid.DelegatedModel,
  modelTextEdits_ :: [TextEdit.DelegatedModel],
  modelDebugLog_ :: [String]
  }
$(AT.deriveAccessors ''Model)

initModel :: Model
initModel = Model {
  modelGrid_ = Grid.initDelegatedModel True,
  modelTextEdits_ = map (TextEdit.initDelegatedModel True) ["abc\ndef", "i\nlala", "oopsy daisy", "hehe"],
  modelDebugLog_ = replicate shortDebugLogLimit ""
  }

quitKey :: ModKey
quitKey = ([Vty.MCtrl], Vty.KASCII 'q')

pureModifyMVar_ :: MonadIO m => MVar a -> (a -> a) -> m ()
pureModifyMVar_ mv f = liftIO $ modifyMVar_ mv (return . f)

main :: IO ()
main = do
  hSetBuffering stderr NoBuffering
  mainLoop =<< newMVar initModel
  where
    mainLoop modelMVar = runWidgetLoop logMsg rootWidget
      where
        logMsg msg = do
          hPutStrLn stderr msg
          pureModifyMVar_ modelMVar (addDebugLog msg)

        addDebugLog msg = modelDebugLog ^: (take longDebugLogLimit . (msg :))

        rootWidget size = modelEdit size fixKeymap `fmap`
                          readMVar modelMVar
        fixKeymap = (Keymap.simpleton "Quit" quitKey (fail "Quit") `mappend`) .
                    ((pureModifyMVar_ modelMVar . const) `fmap`)

shortDebugLog :: Model -> [String]
shortDebugLog model = take shortDebugLogLimit $
                      model ^. modelDebugLog

modelEdit :: Size -> (Keymap Model -> Keymap k) -> Model -> Widget k
modelEdit size fixKeymap model = withKeymap
  where
    withKeymap = TableGrid.addKeymapView 40 size widget (keyAttr, 10) (valueAttr, 30)
    widget = Widget.atDisplay outerGrid innerGrid'
    outerGrid innerGridDisp =
      makeGridView (pure 0)
      [ [ mempty, TextView.make attr "Title\n-----" ],
        [ innerGridDisp, Spacer.makeHorizontal ],
        [ Spacer.makeVertical ],
        [ mempty, mempty,
          TextView.make attr .
          unlines . shortDebugLog $
          model ]
      ]
    innerGrid' = Widget.atKeymap fixKeymap innerGrid
    innerGrid =
      Widget.atDisplay (Scroll.centeredView . SizeRange.fixedSize $ Vector2 40 6) $
      makeGrid (pure 0) modelGrid textEdits
    textEdits =
      [ [ (True, Widget.atDisplay (Display.expand (Vector2 1 0)) .
                 adaptModel (nth i . modelTextEdits)
                 (TextEdit.makeDelegated 5 attr editingAttr) $
                 model)
        | y <- [0, 1]
        , let i = y*2 + x ]
      | x <- [0, 1] ]
    editingAttr = Vty.def_attr `Vty.with_back_color` Vty.blue
    attr = Vty.def_attr `Vty.with_fore_color` Vty.yellow
    keyAttr   = Vty.def_attr
                `Vty.with_fore_color` Vty.green
                `Vty.with_back_color` Vty.blue
    valueAttr = Vty.def_attr
                `Vty.with_fore_color` Vty.red
                `Vty.with_back_color` Vty.blue
                `Vty.with_style` Vty.bold
    makeGridView alignment =
      Grid.makeView . (map . map) (Grid.Item alignment)
    makeGrid alignment acc rows =
      (Grid.makeAccDelegated acc . (map . map) (Grid.Item alignment))
      rows model

adaptModel :: Accessor w p -> (p -> Widget p) -> w -> Widget w
adaptModel acc pwidget w = Widget.atKeymap (flip (setVal acc) w `fmap`) (pwidget (w ^. acc))
