{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

import VtyWrap(withVty, emptyBG)
import qualified Keymap as Keymap
import qualified Graphics.Vty as Vty
import Data.Accessor(Accessor, accessor)
import qualified Data.Accessor.Template as AT
-- import qualified Control.Arrow as Arr
import Data.Maybe(fromMaybe)
import Prelude hiding ((.))
import Control.Category((.))
import Control.Monad(forever)
import Control.Monad.State(evalStateT, put, get)
import Control.Monad.Trans(liftIO)
import Widget(Widget(..))
import qualified Widget
import Grid(gridAcc)
import qualified Grid
import TextView(textView)
import TextEdit(textEdit)
import qualified TextEdit
import Vector2(Vector2(..))
import qualified TermImage

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
  withVty $ \vty -> (`evalStateT` initModel) . forever $ do
    curModel <- get
    let Widget image cursor keymap = widget curModel
    liftIO . Vty.update vty $ Vty.Picture (mkCursor cursor) (TermImage.render image) emptyBG
    event <- liftIO . Vty.next_event $ vty
    case event of
      Vty.EvKey key mods -> do
        let k = (mods, key)
        -- liftIO . putStrLn $ "Key pressed: " ++ Keymap.showModKey k
        put . fromMaybe curModel . fmap (snd . snd) . Keymap.lookup k $ keymap
      _ -> return ()
  where
    mkCursor Nothing = Vty.NoCursor
    mkCursor (Just (Vector2 x y)) = Vty.Cursor (fromIntegral x) (fromIntegral y)
    makeGrid acc = gridAcc acc . (map . map) item
    widget :: Model -> Widget Model
    widget model = makeGrid modelOuterGrid [
                     [ fmap (const model) . textView attr $ "Title\n-----" ],
                     [ makeGrid modelInnerGrid (textEdits model) $ model ]
                     ] model
    textEdits model = [ [ Widget.adaptModel (nth i . modelTextEdits)
                          (textEdit attr) $ model
                        | y <- [0, 1]
                        , let i = y*2 + x ]
                      | x <- [0, 1] ]
    attr = Vty.def_attr `Vty.with_fore_color` Vty.yellow
    item w = (Grid.centered, w)
