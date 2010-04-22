{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Grid
    (make, makeAcc,
     Cursor(..), Model(..), Item(..),
     initModel, centered)
where

import qualified Graphics.Vty as Vty
import Data.List(transpose, genericLength)
import Data.Accessor(Accessor, (^.), setVal)
import Data.Monoid(mempty, mappend, mconcat)
import Control.Applicative(liftA2)

import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import Graphics.UI.VtyWidgets.Widget(Widget(..))
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))
import qualified Graphics.UI.VtyWidgets.Vector2 as Vector2
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage

type Alignment = Vector2 Double
newtype Cursor = Cursor (Vector2 Int)
  deriving (Show, Read, Eq, Ord)
type Size = Cursor
data Item model = Item {
  _itemAlignment :: Alignment,
  itemWidget :: Bool -> Widget model
  }

data Model = Model {
  gridModelCursor :: Cursor
  }

initModel :: Model
initModel = Model (Cursor (Vector2 0 0))

centered :: Alignment
centered = Vector2 0.5 0.5

relativeImagePos :: Widget.ImageSize -> Alignment -> Widget.ImageSize -> Widget.ImageSize
relativeImagePos totalSize align imageSize = alignLeftTop
  where
    totalAlign = liftA2 (-) totalSize imageSize
    alignLeftTop = fmap truncate . liftA2 (*) align . fmap fromIntegral $ totalAlign

enumerate :: (Enum a, Num a) => [b] -> [(a, b)]
enumerate = zip [0..]

keymap :: Size -> Cursor -> Keymap Cursor
keymap (Cursor (Vector2 width height))
       (Cursor (Vector2 cursorX cursorY)) =
  fmap Cursor . mconcat . concat $ [
    [ Keymap.singleton "Left" "Move left" ([], Vty.KLeft) (Vector2 (cursorX-1) cursorY)
    | cursorX > 0 ],
    [ Keymap.singleton "Right" "Move right" ([], Vty.KRight) (Vector2 (cursorX+1) cursorY)
    | cursorX < width - 1 ],
    [ Keymap.singleton "Up" "Move up" ([], Vty.KUp) (Vector2 cursorX (cursorY-1))
    | cursorY > 0 ],
    [ Keymap.singleton "Down" "Move down" ([], Vty.KDown) (Vector2 cursorX (cursorY+1))
    | cursorY < height - 1 ]
    ]
length2D :: Integral i => [[a]] -> Vector2 i
length2D [] = Vector2 0 0
length2D l@(x:_) = Vector2 (genericLength x) (genericLength l)

setter :: w -> Accessor w p -> p -> w
setter w acc p = setVal acc p w

ranges :: Num a => [a] -> [(a, a)]
ranges xs = zip (scanl (+) 0 xs) xs

neutralize :: Widget a -> Widget a
neutralize = (Widget.atKeymap . const) mempty .
             (Widget.atImage . TermImage.setCursor) Nothing

make :: (Model -> model) -> [[Item model]] -> Model -> Bool -> Widget model
make conv rows (Model gcursor) hf = Widget gridImage gridKeymap
  where
    -- Feed all of our rows the has_focus boolean, and replace the
    -- cursor/keymap of non-current children with mempty
    childWidgets =
      map childRowWidgets (enumerate rows)
    childRowWidgets (yIndex, row) =
      map (childWidget yIndex) (enumerate row)
    childWidget yIndex (xIndex, Item alignment child) =
      (alignment,
       if Cursor (Vector2 xIndex yIndex) == gcursor
       then child hf
       else neutralize $ child False)

    -- Compute all the row/column sizes:
    computeSizes f = map maximum . (map . map) (f . Widget.size . snd)
    rowHeights = computeSizes Vector2.snd $ childWidgets
    columnWidths = computeSizes Vector2.fst . transpose $ childWidgets

    -- Translate the widget images and cursors to their right
    -- locations:
    translatedWidgets =
      zipWith translatedRowWidgets (ranges rowHeights) childWidgets
    translatedRowWidgets (y, height) row =
      zipWith (translatedWidget y height) (ranges columnWidths) row
    translatedWidget y height (x, width) (alignment, widget) =
      Widget.atImage (TermImage.translate pos) $
      widget
      where
        pos = liftA2 (+) (Vector2 x y) .
              relativeImagePos (Vector2 width height) alignment .
              Widget.size $
              widget

    -- Combine all neutralized, translated children:
    tuplify (Widget image childKeymap) = (image, childKeymap)
    (gridImage, curChildKeymap) = mconcat . map tuplify . concat $ translatedWidgets

    myKeymap = fmap (conv . Model) .
               keymap (Cursor (length2D rows)) $
               gcursor
    gridKeymap = curChildKeymap `mappend` myKeymap

makeAcc :: Accessor model Model -> [[Item model]] -> model -> Bool -> Widget model
makeAcc acc rows model hf = make (setter model acc) rows (model ^. acc) hf
