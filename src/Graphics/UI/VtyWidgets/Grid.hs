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
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Graphics.UI.VtyWidgets.Widget(Widget(..))
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))
import qualified Graphics.UI.VtyWidgets.Vector2 as Vector2
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage

type Alignment = Vector2 Double
newtype Cursor = Cursor (Vector2 Int)
  deriving (Show, Read, Eq, Ord)
type Size = Cursor
data Item k = Item {
  _itemAlignment :: Alignment,
  itemWidget :: Widget k
  }

data Model = Model {
  gridModelCursor :: Cursor
  }

initModel :: Model
initModel = Model (Cursor (Vector2 0 0))

centered :: Alignment
centered = Vector2 0.5 0.5

relativeImagePos :: Widget.Size -> Alignment -> Widget.Size -> Widget.Size
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

-- Replace keymap and image cursor of a widget with mempty/Nothing
neutralize :: Widget a -> Widget a
neutralize = (Widget.atKeymap . const) mempty .
             (Widget.atDisplay . Widget.atImage . TermImage.setCursor) Nothing .
             (Widget.atDisplay . Widget.atImageArg) (const . Widget.HasFocus $ False)

-- Give each min-max range some of the extra budget...
disperse :: Int -> [(Int, Int)] -> [Int]
disperse _     [] = []
disperse extra ((low, high):xs) = result : disperse remaining xs
  where
    result = max low . min high $ low + extra
    remaining = extra - (result - low)

make :: (Model -> k) -> [[Item k]] -> Model -> Widget k
make conv rows (Model gcursor) =
  Widget.make requestedSize mkImage gridKeymap
  where
    requestedSize = Widget.makeSizeRange minSize maxSize
    minSize = Vector2 (sum columnMinWidths) (sum rowMinHeights)
    maxSize = Vector2 (sum columnMaxWidths) (sum rowMaxHeights)

    -- Neutralize non-current children
    childWidgetRows =
      map childRowWidgets (enumerate rows)
    childRowWidgets (yIndex, row) =
      map (childWidget yIndex) (enumerate row)
    childWidget yIndex (xIndex, Item alignment child) =
      (alignment,
       if Cursor (Vector2 xIndex yIndex) == gcursor
       then child
       else neutralize child)

    -- Compute all the row/column sizes:
    computeSizes aggregate f = map aggregate . (map . map) (f . Widget.requestedSize . snd)
    computeSizeRanges f xs = (computeSizes maximum (f . Widget.srMinSize) xs,
                              computeSizes minimum (f . Widget.srMaxSize) xs)
    (rowMinHeights, rowMaxHeights) =
      computeSizeRanges Vector2.snd childWidgetRows
    (columnMinWidths, columnMaxWidths) =
      computeSizeRanges Vector2.fst transposedChildWidgetRows
    rowHeightRanges = zip rowMinHeights rowMaxHeights
    columnWidthRanges = zip columnMinWidths columnMaxWidths

    transposedChildWidgetRows = transpose childWidgetRows

    childrenKeymap = mconcat . map (Widget.widgetKeymap . snd) . concat $ childWidgetRows
    myKeymap = fmap (conv . Model) .
               keymap (Cursor (length2D rows)) $
               gcursor
    gridKeymap = childrenKeymap `mappend` Just myKeymap

    mkImage hf givenSize = gridImage
      where
        (Vector2 extraWidth extraHeight) = liftA2 (-) givenSize minSize
        columnWidths = disperse extraWidth columnWidthRanges
        rowHeights = disperse extraHeight rowHeightRanges
        -- Translate the widget images to their right locations:
        childImages =
          zipWith childRowImages (ranges rowHeights) childWidgetRows
        childRowImages (y, height) row =
          zipWith (childImage y height) (ranges columnWidths) row
        childImage y height (x, width) (alignment, widget) =
          TermImage.translate pos image
          where
            size = Vector2 width height
            image = (Widget.displayImage . Widget.widgetDisplay) widget hf size
            pos = liftA2 (+) (Vector2 x y) .
                  relativeImagePos size alignment .
                  Widget.srMaxSize .
                  Widget.requestedSize $
                  widget

        -- Combine all neutralized, translated children:
        gridImage = mconcat . concat $ childImages

makeAcc :: Accessor k Model -> [[Item k]] -> k -> Widget k
makeAcc acc rows k = make (setter k acc) rows (k ^. acc)
