{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Grid
    (makeView, make, makeAcc, makeSizes, simpleRows,
     Cursor(..), Model(..), Item(..),
     initModel, centered)
where

import Data.List(transpose)
import Data.Function.Utils(argument)
import Data.Accessor(Accessor, (^.), setVal)
import Data.Monoid(mempty, mappend, mconcat)
import Data.Vector.Vector2(Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import Control.Applicative(liftA2, pure)
import Control.Arrow(second)
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Graphics.UI.VtyWidgets.Widget(Widget(..))
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import Graphics.UI.VtyWidgets.SizeRange(SizeRange(..), Size)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage

type Endo a = a -> a

type Alignment = Vector2 Double
newtype Cursor = Cursor (Vector2 Int)
  deriving (Show, Read, Eq, Ord)
inCursor :: Endo (Vector2 Int) -> Endo Cursor
inCursor f (Cursor x) = Cursor (f x)

data Item w = Item {
  itemAlignment :: Alignment,
  itemChild :: w
  }

atItemChild :: (k -> k') -> Item k -> Item k'
atItemChild f item = item{itemChild = f . itemChild $ item}

data Model = Model {
  gridModelCursor :: Cursor
  }

initModel :: Model
initModel = Model (Cursor (Vector2 0 0))

centered :: Alignment
centered = Vector2 0.5 0.5

relativeImagePos :: Size -> Alignment -> Size -> Size
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
    mover "right" ([], Vty.KRight) Vector2.first  (+) (drop (cursorX + 1)       curRow),
    mover "up"    ([], Vty.KUp)    Vector2.second (-) (reverse . take cursorY $ curColumn),
    mover "down"  ([], Vty.KDown)  Vector2.second (+) (drop (cursorY + 1)       curColumn)
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

-- Give each min-max range some of the extra budget...
disperse :: Int -> [(Int, Int)] -> [Int]
disperse _     [] = []
disperse extra ((low, high):xs) = r : disperse remaining xs
  where
    r = max low . min high $ low + extra
    remaining = low + extra - r

simpleRows :: [[Widget.Display a]] -> [[Item (Widget.Display a)]]
simpleRows = (map . map) (Item (pure 0))

makeSizes :: [[SizeRange]] -> (SizeRange, Size -> [[Size]])
makeSizes rows = (requestedSize, mkSizes)
  where
    requestedSize = SizeRange.make minSize maxSize
    minSize = Vector2 (sum columnMinWidths) (sum rowMinHeights)
    maxSize = Vector2 (sum columnMaxWidths) (sum rowMaxHeights)
    -- Compute all the row/column sizes:
    computeSizes aggregate f = map aggregate . (map . map) f
    computeSizeRanges f xs = (computeSizes maximum (f . SizeRange.srMinSize) xs,
                              computeSizes maximum (f . SizeRange.srMaxSize) xs)

    (rowMinHeights, rowMaxHeights) =
      computeSizeRanges Vector2.snd rows
    (columnMinWidths, columnMaxWidths) =
      computeSizeRanges Vector2.fst . transpose $ rows

    rowHeightRanges = zip rowMinHeights rowMaxHeights
    columnWidthRanges = zip columnMinWidths columnMaxWidths
    mkSizes givenSize = map (Vector2.zip columnWidths . repeat) rowHeights
      where
        Vector2 extraWidth extraHeight = liftA2 (-) givenSize minSize
        columnWidths = disperse extraWidth columnWidthRanges
        rowHeights = disperse extraHeight rowHeightRanges

makeView :: [[Item (Widget.Display a)]] -> Widget.Display a
makeView rows = Widget.makeDisplay requestedSize mkImage
  where
    (requestedSize, mkSizes) = makeSizes . (map . map) (Widget.placableRequestedSize . itemChild) $ rows
    mkImage givenSize imgarg = gridImage
      where
        sizes = mkSizes givenSize
        positions = zipWith Vector2.zip
                    (map (scanl (+) 0 . map Vector2.fst) sizes)
                    (transpose . map (scanl (+) 0 . map Vector2.snd) . transpose $ sizes)
        posSizes = zipWith zip positions sizes
        -- Translate the widget images to their right locations:
        translatedImages = (zipWith . zipWith) translateImage posSizes rows
        translateImage (basePos, size) (Item alignment display) =
          TermImage.translate pos image
          where
            image = Widget.placablePlace display size imgarg
            pos = liftA2 (+) basePos .
                  relativeImagePos size alignment .
                  SizeRange.srMaxSize .
                  Widget.placableRequestedSize $
                  display

        -- Combine all translated images:
        gridImage = mconcat . concat $ translatedImages

itemWidget :: Item (Bool, Widget k) -> Widget k
itemWidget = snd . itemChild

itemWantFocus :: Item (Bool, Widget k) -> Bool
itemWantFocus = fst . itemChild

-- Replace keymap and image cursor of a widget with mempty/Nothing
neutralize :: Widget a -> Widget a
neutralize = (Widget.atKeymap . const) mempty .
             (Widget.atDisplay . Widget.atImage . TermImage.setCursor) Nothing .
             (Widget.atDisplay . fmap . argument) (const . Widget.HasFocus $ False)

make :: (Model -> k) -> [[Item (Bool, Widget k)]] -> Model -> Widget k
make conv rows (Model gcursor) =
  Widget (makeView ((map . map . atItemChild) (Widget.widgetDisplay . snd) childWidgetRows)) gridKeymap
  where
    -- Neutralize non-current children
    childWidgetRows =
      map childRowWidgets (enumerate rows)
    childRowWidgets (yIndex, row) =
      map (childWidget yIndex) (enumerate row)
    childWidget yIndex (xIndex, item) =
      (atItemChild . second)
      (if Cursor (Vector2 xIndex yIndex) == gcursor
       then id
       else neutralize)
      item

    childrenKeymap = mconcat . map (Widget.widgetKeymap . itemWidget) . concat $ childWidgetRows
    myKeymap = fmap (conv . Model) .
               keymap ((map . map) itemWantFocus childWidgetRows) $
               gcursor
    gridKeymap = childrenKeymap `mappend` myKeymap


makeAcc :: Accessor k Model -> [[Item (Bool, Widget k)]] -> k -> Widget k
makeAcc acc rows k = make (setter k acc) rows (k ^. acc)
