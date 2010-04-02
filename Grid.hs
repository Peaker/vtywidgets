{-# OPTIONS -O2 -Wall #-}

module Grid(grid, GridModel(..), centered) where

import qualified Graphics.Vty as Vty
import VtyWrap(vtyString)
import Widget(Widget, WidgetFields(WidgetFields, widgetFieldImage))
import Data.List(transpose)
import Data.Word(Word)
import Data.Accessor(Accessor, (^.))
import Data.Monoid(mempty)

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
