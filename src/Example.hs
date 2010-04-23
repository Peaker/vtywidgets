{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Graphics.Vty as Vty
import Data.Accessor(Accessor, accessor)
import qualified Data.Accessor.Template as AT
import Data.Maybe(fromMaybe)
import Prelude hiding ((.))
import Control.Category((.))
import Control.Monad(forever)
import Control.Arrow(first, second)
import Control.Monad.State(evalStateT, modify, get)
import Control.Monad.Trans(liftIO)
import Graphics.UI.VtyWidgets.VtyWrap(withVty)
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))
import Graphics.UI.VtyWidgets.Widget(Widget(..))
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.TextView as TextView
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import System.IO(stderr, hSetBuffering, BufferMode(NoBuffering), hPutStrLn)

nthSet :: Int -> a -> [a] -> [a]
nthSet _ _ [] = error "IndexError in nthSet"
nthSet 0 x' (_:xs) = x' : xs
nthSet n x' (x:xs) = x : nthSet (n-1) x' xs

nth :: Int -> Accessor [a] a
nth n = accessor (!! n) (nthSet n)

data Model = Model {
  modelOuterGrid_ :: Grid.Model,
  modelInnerGrid_ :: Grid.Model,
  modelTextEdits_ :: [TextEdit.Model]
  }
$(AT.deriveAccessors ''Model)

initModel :: Model
initModel = Model {
  modelOuterGrid_ = Grid.initModel,
  modelInnerGrid_ = Grid.initModel,
  modelTextEdits_ = map TextEdit.initModel ["abc\ndef", "i\nlala", "oopsy daisy", "hehe"]
  }

main :: IO ()
main = do
  hSetBuffering stderr NoBuffering
  withVty $ \vty -> (`evalStateT` (initModel, (Vector2 80 25))) . forever $ do
    (curModel, size) <- get
    let Widget display keymap = widget curModel
    liftIO . Vty.update vty . TermImage.render $ Widget.displayImage display (Widget.HasFocus True) size
    event <- liftIO . Vty.next_event $ vty
    case event of
      Vty.EvResize w h -> do
        let size' = Vector2 w h
        modify . second . const $ size'
        liftIO . hPutStrLn stderr $ "Resized to: " ++ show size'
      Vty.EvKey key mods -> do
        let k = (mods, key)
        -- liftIO . putStrLn $ "Key pressed: " ++ Keymap.showModKey k
        modify . first . const . fromMaybe curModel . fmap (snd . snd) . Keymap.lookup k $ keymap
      _ -> return ()
  where
    makeGrid acc = Grid.makeAcc acc . (map . map) item
    widget model = makeGrid modelOuterGrid [
                     [ Widget.simpleDisplay . TextView.make attr $ "Title\n-----" ],
                     [ makeGrid modelInnerGrid (textEdits model) model ]
                     ] model
    textEdits model = [ [ --Widget.atDisplay (Widget.expand (Vector2 1 0)) .
                          Widget.adaptModel (nth i . modelTextEdits)
                          (TextEdit.make attr editingAttr) $
                          model
                        | y <- [0, 1]
                        , let i = y*2 + x ]
                      | x <- [0, 1] ]
    editingAttr = Vty.def_attr `Vty.with_back_color` Vty.blue
    attr = Vty.def_attr `Vty.with_fore_color` Vty.yellow
    item w = Grid.Item Grid.centered w
