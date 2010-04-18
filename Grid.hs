{-# OPTIONS -O2 -Wall #-}

module Grid(Cursor(..), grid, Model(..), centered) where

import qualified Graphics.Vty as Vty
import qualified Keymap
import Keymap(Keymap)
import Widget(Widget(..))
import qualified Widget
import Data.List(transpose, genericLength)
import Data.Maybe(fromMaybe)
import Data.Accessor(Accessor, (^.))
import Data.Monoid(mempty, mappend, mconcat, First(First))
import Control.Applicative(liftA2)
import Vector2(Vector2(..))
import qualified Vector2
import qualified TermImage
  
type Alignment = Vector2 Double
newtype Cursor = Cursor (Vector2 Int)
  deriving (Show, Read, Eq, Ord)
type Size = Cursor

data Model = Model {
  gridModelCursor :: Cursor
  }

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

grid :: Accessor model Model -> [[(Alignment, (model -> Widget model))]] -> model -> Widget model
grid acc rows model = Widget gridImage gridCursor gridKeymap
  where
    Model gcursor = model ^. acc
    unpaddedChildImages = (map . map) (widgetImage . ($model) . snd) rows
    rowHeights = map maximum . (map . map) (Vector2.snd . Widget.imageSize) $ unpaddedChildImages
    columnWidths = map maximum . transpose . (map . map) (Vector2.fst . Widget.imageSize) $ unpaddedChildImages
    ranges xs = zip (scanl (+) 0 xs) xs
    size = Cursor (length2D rows)
    myKeymap = Widget.adaptKeymap acc model . fmap Model . keymap size $ gcursor
    gridKeymap = childKeymap `mappend` myKeymap
    childKeymap = fromMaybe mempty . fmap (widgetKeymap . snd) $ curGridElement
    gridCursor = uncurry moveCursor =<< curGridElement
    pos `moveCursor` childFields = liftA2 (+) pos `fmap` widgetCursor childFields

    (gridImage, First curGridElement) =
      mconcat . concat $
      zipWith gridRowElements (ranges rowHeights) (enumerate rows)

    gridRowElements (y, height) (yIndex, row) =
      zipWith (gridElement y height yIndex) (ranges columnWidths) (enumerate row)

    gridElement y height yIndex (x, width) (xIndex, (alignment, child)) =
      (childImage, curChild)

      where
        gpos = Cursor (Vector2 xIndex yIndex)
        basePos = Vector2 x y
        pos = liftA2 (+) padSize basePos
        childFields = child model
        curChild = if gpos == gcursor
                   then First . Just $ (pos, childFields)
                   else First $ Nothing
        padSize = relativeImagePos (Vector2 width height) alignment .
                  Widget.size $
                  childFields
        childImage = TermImage.translate pos . widgetImage $ childFields
