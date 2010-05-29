{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Graphics.Vty as Vty
import Data.Accessor(Accessor, accessor, (^.), setVal)
import qualified Data.Accessor.Template as AT
import Data.Monoid(mempty, mappend)
import Data.Vector.Vector2(Vector2(..))
import Prelude hiding ((.))
import Control.Category((.))
import Control.Monad(forever)
import Control.Arrow(first, second)
import Control.Applicative(pure)
import Control.Monad.Trans.State(StateT, evalStateT, modify, get)
import Control.Monad.Trans(liftIO)
import Graphics.Vty.Utils(withVty)
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Keymap(Keymap, ModKey)
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Graphics.UI.VtyWidgets.Widget(Widget)
import qualified Graphics.UI.VtyWidgets.Display as Display
import qualified Graphics.UI.VtyWidgets.Placable as Placable
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import Graphics.UI.VtyWidgets.SizeRange(Size)
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextView as TextView
import qualified Graphics.UI.VtyWidgets.Scroll as Scroll
import qualified Graphics.UI.VtyWidgets.Spacer as Spacer
import qualified Graphics.UI.VtyWidgets.TableGrid as TableGrid
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.TermImage(TermImage)
import System.IO(stderr, hSetBuffering, BufferMode(NoBuffering), hPutStrLn)

nthSet :: Int -> a -> [a] -> [a]
nthSet _ _ [] = error "IndexError in nthSet"
nthSet 0 x' (_:xs) = x' : xs
nthSet n x' (x:xs) = x : nthSet (n-1) x' xs

nth :: Int -> Accessor [a] a
nth n = accessor (!! n) (nthSet n)

data Model = Model {
  modelGrid_ :: Grid.DelegatedModel,
  modelTextEdits_ :: [TextEdit.DelegatedModel],
  modelLastEvent_ :: String
  }
$(AT.deriveAccessors ''Model)

initModel :: Model
initModel = Model {
  modelGrid_ = Grid.initDelegatedModel True,
  modelTextEdits_ = map (TextEdit.initDelegatedModel True) ["abc\ndef", "i\nlala", "oopsy daisy", "hehe"],
  modelLastEvent_ = ""
  }

quitKey :: ModKey
quitKey = ([Vty.MCtrl], Vty.KASCII 'q')

main :: IO ()
main = do
  hSetBuffering stderr NoBuffering
  withVty $ \vty -> (`evalStateT` (initModel, Vector2 80 25)) . forever $ do
    render vty
    event <- liftIO . Vty.next_event $ vty
    modify . first . setVal modelLastEvent . show $ event
    case event of
      Vty.EvResize w h -> do
        let size' = Vector2 w h
        modify . second . const $ size'
        liftIO . hPutStrLn stderr $ "Resized to: " ++ show size'
      Vty.EvKey key mods -> do
        let k = (mods, key)
        (curModel, size) <- get
        maybe (return ()) (snd . snd) . Keymap.lookup k . snd $ widget size curModel
      _ -> return ()
  where
    render vty = do
      (curModel, size) <- get
      liftIO . Vty.update vty . TermImage.render . fst $ widget size curModel

widget :: Size -> Model -> (TermImage, Keymap (StateT (Model, Size) IO ()))
widget size model = (mkImage (Widget.HasFocus True), km')
  where
    w = Widget.atDisplay outerGrid innerGrid
    (mkImage, km) = (Placable.pPlace . Widget.unWidget) w size
    km' :: Keymap (StateT (Model, Size) IO ())
    km' = Keymap.simpleton "Quit" quitKey (fail "Quit") `mappend`
          ((modify . first . const) `fmap` km)
    outerGrid innerGridDisp =
      makeGridView (pure 0)
      [ [ mempty,TextView.make attr "Title\n-----" ],
        [ innerGridDisp, Spacer.makeHorizontal ],
        [ Spacer.makeVertical ],
        [ mempty, mempty, keymapView km' ],
        [ mempty, mempty, TextView.make attr $ model ^. modelLastEvent ] ]
    innerGrid =
      Widget.atDisplay (Scroll.centeredView . SizeRange.fixedSize $ Vector2 40 6) $
      makeGrid (pure 0) modelGrid textEdits
    keymapView keymap = TableGrid.makeKeymapView 10 30 keymap keyAttr valueAttr
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
