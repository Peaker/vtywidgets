{-# OPTIONS -O2 -Wall #-}

-- TODO: Add these to Vty itself..

module VtyWrap(withVty, safeMkVty, emptyImage, emptyBG, pictureOfImage) where

import qualified Graphics.Vty as Vty
import Graphics.Vty(Vty)
import Control.Exception(bracket)

emptyBG :: Vty.Background
emptyBG = Vty.Background ' ' Vty.def_attr

-- pic_for_image sucks:
pictureOfImage :: Vty.Image -> Vty.Picture
pictureOfImage image = Vty.Picture Vty.NoCursor image emptyBG

safeMkVty :: IO Vty
safeMkVty = do
  vty <- Vty.mkVty
  vty `Vty.update` Vty.pic_for_image emptyImage
  return vty

withVty :: (Vty -> IO a) -> IO a
withVty = bracket safeMkVty Vty.shutdown

emptyImage :: Vty.Image
emptyImage = Vty.horiz_cat []
