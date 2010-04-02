{-# OPTIONS -O2 -Wall #-}

module Grid(grid, Model(..), centered) where

import qualified Graphics.Vty as Vty
import qualified Keymap
import VtyWrap(vtyString)
import Widget(Widget, WidgetFields(WidgetFields, widgetFieldImage), adaptKeymap)
import Data.List(transpose)
import Data.Word(Word)
import Data.Accessor(Accessor, (^.))
import Data.Monoid(mconcat)

type Alignment = (Double, Double)
type Size = (Word, Word)

type Cursor = (Int, Int)

data Model = Model {
  gridCursor :: Cursor
  }

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

enumerate :: (Enum a, Num a) => [b] -> [(a, b)]
enumerate = zip [0..]

grid :: Accessor model Model -> [[(Alignment, Widget model)]] -> Widget model
grid acc rows model = WidgetFields image keymap
  where
    Model cursor@(cursorX, cursorY) = model ^. acc
    unpaddedChildImages = (map . map) (widgetFieldImage . ($model) . snd) rows
    rowHeights = map maximum . (map . map) Vty.image_height $ unpaddedChildImages
    columnWidths = map maximum . transpose . (map . map) Vty.image_width $ unpaddedChildImages
    paddedChildImages = zipWith padRows rowHeights (enumerate rows)
    padRows height (y, row) = zipWith (padChild height y) columnWidths (enumerate row)

    padChild height y width (x, (alignment, child)) =
      padImage (width, height) alignment $
      if (x, y) == cursor
      then Vty.string Vty.def_attr "***"
      else (widgetFieldImage . child $ model)

    image = Vty.vert_cat . map Vty.horiz_cat $ paddedChildImages
    keymap = adaptKeymap acc model . fmap Model . mconcat . concat $ [
      [ Keymap.singleton "Left" "Move left" ([], Vty.KLeft) (cursorX-1, cursorY)
      | cursorX > 0],
      [ Keymap.singleton "Right" "Move right" ([], Vty.KRight) (cursorX+1, cursorY)
      | cursorX < length (head rows) - 1],
      [ Keymap.singleton "Up" "Move up" ([], Vty.KUp) (cursorX, cursorY-1)
      | cursorY > 0],
      [ Keymap.singleton "Down" "Move down" ([], Vty.KDown) (cursorX, cursorY+1)
      | cursorY < length rows - 1]
      ]
