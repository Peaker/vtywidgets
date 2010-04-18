{-# OPTIONS -O2 -Wall #-}

module Grid(grid, gridAcc,
            Cursor(..), Model(..), Item(..),
            initModel, centered) where

import qualified Graphics.Vty as Vty
import qualified Keymap
import Keymap(Keymap)
import Widget(Widget(..))
import qualified Widget
import Data.List(transpose, genericLength)
import Data.Maybe(fromMaybe)
import Data.Accessor(Accessor, (^.), setVal)
import Data.Monoid(mempty, mappend, mconcat, First(First))
import Control.Applicative(liftA2)
import Vector2(Vector2(..))
import qualified Vector2
import qualified TermImage
  
type Alignment = Vector2 Double
newtype Cursor = Cursor (Vector2 Int)
  deriving (Show, Read, Eq, Ord)
type Size = Cursor
data Item model = Item {
  _itemAlignment :: Alignment,
  itemWidget :: Widget model
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

grid :: (Model -> model) -> [[Item model]] -> Model -> Widget model
grid conv rows (Model gcursor) = Widget gridImage gridCursor gridKeymap
  where
    unpaddedChildImages = (map . map) (widgetImage . itemWidget) rows
    rowHeights = map maximum . (map . map) (Vector2.snd . Widget.imageSize) $ unpaddedChildImages
    columnWidths = map maximum . transpose . (map . map) (Vector2.fst . Widget.imageSize) $ unpaddedChildImages
    ranges xs = zip (scanl (+) 0 xs) xs
    size = Cursor (length2D rows)
    myKeymap = fmap (conv . Model) . keymap size $ gcursor
    gridKeymap = childKeymap `mappend` myKeymap
    childKeymap = fromMaybe mempty . fmap (widgetKeymap . snd) $ curGridElement
    gridCursor = uncurry moveCursor =<< curGridElement
    pos `moveCursor` childFields = liftA2 (+) pos `fmap` widgetCursor childFields

    (gridImage, First curGridElement) =
      mconcat . concat $
      zipWith gridRowElements (ranges rowHeights) (enumerate rows)

    gridRowElements (y, height) (yIndex, row) =
      zipWith (gridElement y height yIndex) (ranges columnWidths) (enumerate row)

    gridElement y height yIndex (x, width) (xIndex, Item alignment child) =
      (childImage, curChild)

      where
        gpos = Cursor (Vector2 xIndex yIndex)
        basePos = Vector2 x y
        pos = liftA2 (+) padSize basePos
        curChild = if gpos == gcursor
                   then First . Just $ (pos, child)
                   else First $ Nothing
        padSize = relativeImagePos (Vector2 width height) alignment .
                  Widget.size $
                  child
        childImage = TermImage.translate pos . widgetImage $ child

gridAcc :: Accessor model Model -> [[Item model]] -> model -> Widget model
gridAcc acc rows model = grid (setter model acc) rows (model ^. acc)
