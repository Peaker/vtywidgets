{-# OPTIONS -O2 -Wall #-}

import VtyWrap(withVty, emptyBG)
import qualified Keymap as Keymap
import qualified Graphics.Vty as Vty
import Graphics.Vty(Vty)
import Data.Accessor.Tuple(first, second)
import Data.Maybe(fromMaybe)
import Prelude hiding ((.))
import Control.Category((.))
import Control.Monad(forever)
import Control.Monad.State(evalStateT, put, get)
import Control.Monad.Trans(liftIO)
import Widget(WidgetFields(WidgetFields), adaptModel)
import Grid(grid)
import qualified Grid
import TextView(textView)
import Vector2(Vector2(..))

main :: IO ()
main = do
  withVty $ \vty -> (`evalStateT` initModel) . forever $ do
    curModel <- get
    let WidgetFields image cursor keymap = widget curModel
    liftIO . Vty.update vty $ Vty.Picture (mkCursor cursor) image emptyBG
    event <- liftIO . Vty.next_event $ vty
    case event of
      Vty.EvKey key mods -> do
        let k = (mods, key)
        -- liftIO . putStrLn $ "Key pressed: " ++ Keymap.showModKey k
        put . fromMaybe curModel . fmap (snd . snd) . Keymap.lookup k $ keymap
      _ -> return ()
  where
    mkCursor Nothing = Vty.NoCursor
    mkCursor (Just (Vector2 x y)) = Vty.Cursor x y
    widget = grid first . (map . map) (item . makeTextView) $
             [[second, first],
              [first, second]]
    makeTextView x = adaptModel (x . second) $ textView (Vty.def_attr `Vty.with_fore_color` Vty.yellow)
    item w = (Grid.centered, w)
    initModel = (Grid.Model (0, 0), ("abc\ndefsdkjflkdlfjasd\n asfdahjsd", "Eliwwwww"))
