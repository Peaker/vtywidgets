{-# OPTIONS -O2 -Wall #-}

module Grid(grid, Model(..), centered) where

import qualified Graphics.Vty as Vty
import qualified Keymap
import Keymap(Keymap)
import VtyWrap(vtyString)
import Widget(Widget, WidgetFields(WidgetFields, widgetFieldImage, widgetFieldCursor), adaptKeymap)
import Data.List(transpose, genericLength)
import Data.Word(Word)
import Data.Accessor(Accessor, (^.))
import Data.Monoid(mconcat)
import Control.Monad(msum)
import Control.Applicative(liftA2)
import Vector2(Vector2(..))
  
type Alignment = (Double, Double)
type Cursor = (Word, Word)
type Size = Cursor

data Model = Model {
  gridCursor :: Cursor
  }

centered :: Alignment
centered = (0.5, 0.5)

padImage :: Size -> Alignment -> Vty.Image -> (Vector2 Word, Vty.Image)
padImage (width, height) (ax, ay) image =
  (
    Vector2 leftAlign upAlign,
    Vty.vert_cat [
      alignImage " \n" upAlign,
      Vty.horiz_cat [
        alignImage " " leftAlign,
        image,
        alignImage " " rightAlign
        ],
      alignImage " \n" downAlign
      ]
  )
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

gridKeymap :: Size -> Cursor -> Keymap Cursor
gridKeymap (width, height) (cursorX, cursorY) = mconcat . concat $ [
  [ Keymap.singleton "Left" "Move left" ([], Vty.KLeft) (cursorX-1, cursorY)
  | cursorX > 0],
  [ Keymap.singleton "Right" "Move right" ([], Vty.KRight) (cursorX+1, cursorY)
  | cursorX < width - 1],
  [ Keymap.singleton "Up" "Move up" ([], Vty.KUp) (cursorX, cursorY-1)
  | cursorY > 0],
  [ Keymap.singleton "Down" "Move down" ([], Vty.KDown) (cursorX, cursorY+1)
  | cursorY < height - 1]
  ]

grid :: Accessor model Model -> [[(Alignment, Widget model)]] -> Widget model
grid acc rows model = WidgetFields image vtyCursor keymap
  where
    Model cursor = model ^. acc
    unpaddedChildImages = (map . map) (widgetFieldImage . ($model) . snd) rows
    rowHeights = map maximum . (map . map) Vty.image_height $ unpaddedChildImages
    columnWidths = map maximum . transpose . (map . map) Vty.image_width $ unpaddedChildImages
    ranges :: [Word] -> [(Word, Word)]
    ranges xs = zip (scanl (+) 0 xs) xs

    childImageCursors =
      zipWith childImageCursorsOfRow (ranges rowHeights) (enumerate rows)

    childImageCursorsOfRow (y, height) (yIndex, row) =
      zipWith (childImageCursor y height yIndex) (ranges columnWidths) (enumerate row)

    childImageCursor y height yIndex (x, width) (xIndex, (alignment, child)) =
      (childImage, childCursor)
      where
        childFields = child model
        baseCursorPos = if (xIndex, yIndex) == cursor
                        then Just (liftA2 (+) padSize (Vector2 x y))
                        else Nothing
        (padSize, childImage) = padImage (width, height) alignment . widgetFieldImage $ childFields
        childCursor = (liftA2 . liftA2) (+) baseCursorPos . widgetFieldCursor $ childFields

    image = Vty.vert_cat . map Vty.horiz_cat . (map . map) fst $ childImageCursors
    vtyCursor = msum . map snd . concat $ childImageCursors
    size = (case rows of
               [] -> 0
               (x:_) -> genericLength x,
            genericLength rows)
    keymap = adaptKeymap acc model . fmap Model . gridKeymap size $ cursor
