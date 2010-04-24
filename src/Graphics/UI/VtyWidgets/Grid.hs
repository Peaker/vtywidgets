{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Grid
    (make, makeAcc,
     Cursor(..), Model(..), Item(..),
     initModel, centered)
where

import qualified Graphics.Vty as Vty
import Data.List(transpose)
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

type Endo a = a -> a

type Alignment = Vector2 Double
newtype Cursor = Cursor (Vector2 Int)
  deriving (Show, Read, Eq, Ord)
inCursor :: Endo (Vector2 Int) -> Endo Cursor
inCursor f (Cursor x) = Cursor (f x)

data Item k = Item {
  _itemAlignment :: Alignment,
  itemWantFocus :: Bool,
  itemWidget :: Widget k
  }

atItemWidget :: (Widget k -> Widget k') -> Item k -> Item k'
atItemWidget f item = item{itemWidget = f (itemWidget item)}

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

keymap :: [[Bool]] -> Cursor -> Keymap Cursor
keymap wantFocusRows cursor@(Cursor (Vector2 cursorX cursorY)) = 
  mconcat . concat $ [
    mover "left"  ([], Vty.KLeft)  Vector2.first  (-) (reverse . take cursorX $ curRow),
    mover "right" ([], Vty.KRight) Vector2.first  (+) (drop (cursorX + 1)     $ curRow),
    mover "up"    ([], Vty.KUp)    Vector2.second (-) (reverse . take cursorY $ curColumn),
    mover "down"  ([], Vty.KDown)  Vector2.second (+) (drop (cursorY + 1)     $ curColumn)
    ]
  where
    mover dirName key set f xs =
       [ Keymap.simpleton ("Move " ++ dirName) key ((inCursor . set . f . (+1) . countUnwanters $ xs) cursor)
       | True `elem` xs ]
    curColumn = transpose wantFocusRows !! cursorX
    curRow = wantFocusRows !! cursorY
    countUnwanters = length . takeWhile not

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
    childWidget yIndex (xIndex, item) =
      atItemWidget
      (if Cursor (Vector2 xIndex yIndex) == gcursor
       then id
       else neutralize)
      item

    -- Compute all the row/column sizes:
    computeSizes aggregate f = map aggregate . (map . map) (f . Widget.requestedSize . itemWidget)
    computeSizeRanges f xs = (computeSizes maximum (f . Widget.srMinSize) xs,
                              computeSizes maximum (f . Widget.srMaxSize) xs)
    (rowMinHeights, rowMaxHeights) =
      computeSizeRanges Vector2.snd childWidgetRows
    (columnMinWidths, columnMaxWidths) =
      computeSizeRanges Vector2.fst transposedChildWidgetRows
    rowHeightRanges = zip rowMinHeights rowMaxHeights
    columnWidthRanges = zip columnMinWidths columnMaxWidths

    transposedChildWidgetRows = transpose childWidgetRows

    childrenKeymap = mconcat . map (Widget.widgetKeymap . itemWidget) . concat $ childWidgetRows
    myKeymap = fmap (conv . Model) .
               keymap ((map . map) itemWantFocus childWidgetRows) $
               gcursor
    gridKeymap = childrenKeymap `mappend` myKeymap

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
        childImage y height (x, width) (Item alignment _wantFocus widget) =
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
