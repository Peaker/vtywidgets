{-# OPTIONS -O2 -Wall #-}

-- TODO: Add these to Vty itself..

module Graphics.Vty.Utils
    (withVty, safeMkVty)
where

import           Data.Monoid       (mempty)
import           Control.Exception (bracket)
import qualified Graphics.Vty      as Vty
import           Graphics.Vty      (Vty)

safeMkVty :: IO Vty
safeMkVty = do
  vty <- Vty.mkVty
  vty `Vty.update` Vty.pic_for_image mempty
  return vty

withVty :: (Vty -> IO a) -> IO a
withVty = bracket safeMkVty Vty.shutdown
