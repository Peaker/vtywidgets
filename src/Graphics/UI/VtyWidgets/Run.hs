{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Run(runWidgetLoop)
where

import Control.Monad(forever)
import Control.Monad.Trans.State(evalStateT, get, put)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Data.Vector.Vector2(Vector2(..))
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Widget(Widget)
import Graphics.UI.VtyWidgets.SizeRange(Size)
import Graphics.Vty.Utils(withVty)
import System.IO(withFile, Handle, IOMode(WriteMode), hPutStrLn, hFlush)

writeLnFlush :: Handle -> String -> IO ()
writeLnFlush h str = do
  hPutStrLn h str
  hFlush h

runWidgetLoop :: (Size -> IO (Widget (IO ()))) -> IO ()
runWidgetLoop makeWidget = do
  withVty $ \vty -> do
    withFile "/tmp/debug.log" WriteMode $ \debugHandle -> do
      let debugLog = liftIO . writeLnFlush debugHandle
      Vty.DisplayRegion width height <- Vty.display_bounds . Vty.terminal $ vty
      let initSize = fmap fromIntegral $ Vector2 width height
      (`evalStateT` initSize) . forever $ do
        liftIO . Vty.update vty . TermImage.render . fst =<< makeWidget'
        event <- liftIO . Vty.next_event $ vty
        debugLog $ "Handling event: " ++ show event
        case event of
          Vty.EvResize w h -> do
            let size' = Vector2 w h
            put size'
            debugLog $ "Resized to: " ++ show size'
          Vty.EvKey key mods -> do
            maybe (return ()) (liftIO . snd . snd) . Keymap.lookup (mods, key) . snd =<< makeWidget'
          _ -> return ()
  where
    makeWidget' = do
      size <- get
      w <- liftIO . makeWidget $ size
      return (Widget.runWidget w size)
