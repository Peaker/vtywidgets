{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Graphics.Vty as Vty
import Data.Accessor(Accessor, accessor, (^.), setVal)
import qualified Data.Accessor.Template as AT
import Data.Maybe(fromMaybe)
import Data.Monoid(mempty)
import Prelude hiding ((.))
import Control.Category((.))
import Control.Monad(forever)
import Control.Arrow(first, second)
import Control.Monad.State(evalStateT, modify, get)
import Control.Monad.Trans(liftIO)
import Graphics.UI.VtyWidgets.VtyWrap(withVty)
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))
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
  modelTextEdits_ :: [TextEdit.Model],
  modelLastEvent_ :: String
  }
$(AT.deriveAccessors ''Model)

initModel :: Model
initModel = Model {
  modelOuterGrid_ = Grid.Model (Grid.Cursor (Vector2 0 1)),
  modelInnerGrid_ = Grid.initModel,
  modelTextEdits_ = map TextEdit.initModel ["abc\ndef", "i\nlala", "oopsy daisy", "hehe"],
  modelLastEvent_ = ""
  }

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
        modify . first $
          \curModel ->
          fromMaybe curModel . fmap (snd . snd) .
          Keymap.lookup (mods, key) . Widget.widgetKeymap . widget $ curModel
      _ -> return ()
  where
    render vty = do
      (curModel, size) <- get
      let image = (Widget.displayImage . Widget.widgetDisplay . widget $ curModel) (Widget.HasFocus True) size
      liftIO . Vty.update vty . TermImage.render $ image
    widget model =
      makeGrid modelOuterGrid [
        [ (False, Widget.simpleDisplay . TextView.make attr $ "Title\n-----"),
          (False, mempty) ],
        [ (True,  makeGrid modelInnerGrid (textEdits model) model),
          (False, Widget.simpleDisplay . TextView.make attr $ model ^. modelLastEvent) ]
        ] model
    textEdits model =
      [ [ (True, Widget.atDisplay (Widget.expand (Vector2 1 0)) .
                 Widget.adaptModel (nth i . modelTextEdits)
                 (TextEdit.make 5 attr editingAttr) $
                 model)
        | y <- [0, 1]
        , let i = y*2 + x ]
      | x <- [0, 1] ]
    editingAttr = Vty.def_attr `Vty.with_back_color` Vty.blue
    attr = Vty.def_attr `Vty.with_fore_color` Vty.yellow
    makeGrid acc = Grid.makeAcc acc . (map . map . uncurry) item
    item = Grid.Item Grid.centered
