{-# OPTIONS -O2 -Wall #-}

import VtyWrap(withVty, pictureOfImage)
import Keymap(Keymap, showModKey)
import qualified Keymap as Keymap
import qualified Graphics.Vty as Vty
import Graphics.Vty(Vty)
import Data.Accessor(Accessor, (^.), setVal)
import Data.Accessor.Tuple(first, second)
import Data.List(transpose)
import Data.Word(Word)
import Data.Monoid(mempty)
import Data.Maybe(fromMaybe)
import Prelude hiding ((.))
import Control.Category((.))
import Control.Monad(forever)
import Control.Monad.State(evalStateT, put, get)
import Control.Monad.Trans(liftIO)

data WidgetFields model = WidgetFields {
  widgetFieldImage :: Vty.Image,
  widgetFieldKeymap :: Keymap model
  }
type Widget model = model -> WidgetFields model

vtyString :: Vty.Attr -> String -> Vty.Image
vtyString attr = Vty.vert_cat . map (Vty.string attr) . lines

textView :: Vty.Attr -> Widget String
textView attr text = WidgetFields image mempty
  where
    image = vtyString attr text

adaptModel :: Accessor w p -> Widget p -> Widget w
adaptModel acc widget wmodel = WidgetFields image (flip (setVal acc) wmodel `fmap` keymap)
  where
    WidgetFields image keymap = widget (wmodel ^. acc)

type Alignment = (Double, Double)
type Size = (Word, Word)

data GridModel = GridModel

centered :: Alignment
centered = (0.5, 0.5)

padImage :: Size -> Alignment -> Vty.Image -> Vty.Image
padImage (width, height) (ax, ay) image =
  Vty.vert_cat [
    alignImage " \n" upAlign,
    Vty.horiz_cat [
      alignImage " " leftAlign,
      image,
      alignImage " " rightAlign
      ],
    alignImage " \n" downAlign
    ]
  where
    alignImage c n = vtyString Vty.def_attr (concat . replicate (fromIntegral n) $ c)
    totalAlignWidth = width - Vty.image_width image
    totalAlignHeight = height - Vty.image_height image
    leftAlign = truncate $ ax * fromIntegral totalAlignWidth
    rightAlign = totalAlignWidth - leftAlign
    upAlign = truncate $ ay * fromIntegral totalAlignHeight
    downAlign = totalAlignHeight - upAlign

grid :: Accessor model GridModel -> [[(Alignment, Widget model)]] -> Widget model
grid acc rows model = WidgetFields image mempty
  where
    GridModel = model ^. acc
    unpaddedChildImages = (map . map) (widgetFieldImage . ($model) . snd) rows
    rowHeights = map maximum . (map . map) Vty.image_height $ unpaddedChildImages
    columnWidths = map maximum . transpose . (map . map) Vty.image_width $ unpaddedChildImages
    paddedChildImages = zipWith padRows rowHeights rows
    padRows height row = zipWith (padChild height) columnWidths row
    padChild height width (alignment, child) = padImage (width, height) alignment (widgetFieldImage . child $ model)
    image = Vty.vert_cat . map Vty.horiz_cat $ paddedChildImages

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
    makeTextView a = adaptModel (a . second) (textView Vty.def_attr)
    item w = (centered, w)
    initModel = (GridModel, ("hello    hello\nworld of Earth\nGalya Gerovich!", "love"))
