{-# OPTIONS -O2 -Wall #-}

import VtyWrap(withVty, pictureOfImage)
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
import Keymap(showModKey)
import Widget(WidgetFields(widgetFieldKeymap, widgetFieldImage), adaptModel)
import Grid(grid)
import qualified Grid
import TextView(textView)

main :: IO ()
main = do
  withVty $ \vty -> (`evalStateT` initModel) . forever $ do
    curModel <- get
    liftIO . Vty.update vty . pictureOfImage . widgetFieldImage . widget $ curModel
    event <- liftIO . Vty.next_event $ vty
    case event of
      Vty.EvKey key mods -> do
        let k = (mods, key)
        liftIO . putStrLn $ "Key pressed: " ++ showModKey k
        put . fromMaybe curModel . fmap (snd . snd) .
          Keymap.lookup k . widgetFieldKeymap . widget $ curModel
      _ -> return ()
  where
    widget = grid first [[item (makeTextView second), item (makeTextView first)],
                         [item (makeTextView first), item (makeTextView second)]]
    makeTextView a = adaptModel (a . second) $ textView Vty.def_attr
    item w = (Grid.centered, w)
    initModel = (Grid.Model, ("hello    hello\nworld of Earth\nGalya Gerovich!", "love"))
