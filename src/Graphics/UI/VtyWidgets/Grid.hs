{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.UI.VtyWidgets.Grid
    (makeView, make, makeAcc, makeSizes, simpleRows,
     Cursor(..), Model(..), Item(..),
     initModel, centered)
where

import Data.Function.Utils(Endo, result, (~>))
import Data.List(transpose)
import Data.Accessor(Accessor, (^.), setVal)
import Data.Monoid(mempty, mappend, mconcat)
import Data.Maybe(fromMaybe)
import Data.Vector.Vector2(Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import Control.Applicative(liftA2, pure)
import Control.Arrow((***), second)
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Graphics.UI.VtyWidgets.Widget(Widget(..))
import qualified Graphics.UI.VtyWidgets.Placable as Placable
import Graphics.UI.VtyWidgets.Placable(Placable(..))
import qualified Graphics.UI.VtyWidgets.Display as Display
import Graphics.UI.VtyWidgets.Display(Display)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import Graphics.UI.VtyWidgets.SizeRange(SizeRange(..), Size)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.TermImage(TermImage, Coordinate)

-- Item:

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

instance Functor Item where
  fmap = atItemChild

centered :: Alignment
centered = Vector2 0.5 0.5

-- Model:

data Model = Model {
  gridModelCursor :: Cursor
  }

initModel :: Model
initModel = Model (Cursor (Vector2 0 0))

--- Size computations:

-- Give each min-max range some of the extra budget...
disperse :: Int -> [(Int, Int)] -> [Int]
disperse _     [] = []
disperse extra ((low, high):xs) = r : disperse remaining xs
  where
    r = max low . min high $ low + extra
    remaining = low + extra - r

makeSizes :: [[SizeRange]] -> (SizeRange, Size -> [[Size]])
makeSizes rows = (requestedSize, mkSizes)
  where
    requestedSize = SizeRange.make minSize maxSize
    minSize = Vector2 (sum columnMinWidths) (sum rowMinHeights)
    maxSize = Vector2 (sum columnMaxWidths) (sum rowMaxHeights)
    -- Compute all the row/column sizes:
    computeSizes f = map maximum . (map . map) f
    computeSizeRanges f xs =
      (computeSizes (f . SizeRange.srMinSize) xs,
       computeSizes (f . SizeRange.srMaxSize) xs)

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

--- Placables:

type Placement = (Coordinate, Size)

makePlacements :: [[SizeRange]] -> (SizeRange, Size -> [[Placement]])
makePlacements = (result . second . result) placements makeSizes
  where
    placements sizes = zipWith zip positions sizes
      where
        positions = zipWith Vector2.zip
                    (map (scanl (+) 0 . map Vector2.fst) sizes)
                    (transpose . map (scanl (+) 0 . map Vector2.snd) . transpose $ sizes)
    
--- Images:

relativeImagePos :: Size -> Alignment -> Size -> Size
relativeImagePos totalSize align imageSize = alignLeftTop
  where
    totalAlign = liftA2 (-) totalSize imageSize
    alignLeftTop = fmap truncate . liftA2 (*) align . fmap fromIntegral $ totalAlign

translateImage :: Item (Placement, (Size, TermImage)) -> TermImage
translateImage (Item alignment ((basePos, size), (imgSize, image))) =
  TermImage.translate pos image
      where
        pos = liftA2 (+) basePos $
              relativeImagePos size alignment imgSize

mapu :: (a -> b -> c) -> [(a, b)] -> [c]
mapu = map . uncurry

combineImages :: [[Item ((Placement, (Size, TermImage)))]] -> TermImage
combineImages = mconcat . map translateImage . concat

--- Displays:

feedPlacable :: Placement -> Placable a -> (Placement, (Size, a))
feedPlacable pl@(_, size) placable = (pl, unPlacable placable)
  where
    unPlacable (Placable rs place) = (imgSize, place size)
      where
        -- Give the cell the minimum between available space and
        -- his maximum requested space (in each axis):
        imgSize = liftA2 min size . SizeRange.srMaxSize $ rs

makeView :: [[Item (Display a)]] -> Display a
makeView rows = Display.make requestedSize mkImage
  where
    (requestedSize, mkPlacements) =
      makePlacements .
      (map . map) (Placable.pRequestedSize . itemChild) $
      rows
    mkImage givenSize imgarg =
      combineImages .
      (zipWith . zipWith) (fmap . feedPlacable) (mkPlacements givenSize) .
      -- Penetrate [[Item (Placable (..))]] and feed the arg
      (map . map . fmap . fmap) ($ imgarg) $
      rows

--- Widgets:

enumerate :: (Enum a, Num a) => [b] -> [(a, b)]
enumerate = zip [0..]

enumerate2 :: (Enum a, Num a) => [[b]] -> [[((a, a), b)]]
enumerate2 xss = mapu row (enumerate xss)
  where
    row rowIndex items = mapu (add rowIndex) (enumerate items)
    add rowIndex columnIndex item = ((columnIndex, rowIndex), item)

mkNavKeymap :: [[Bool]] -> Cursor -> Keymap Cursor
mkNavKeymap wantFocusRows cursor@(Cursor (Vector2 cursorX cursorY)) = 
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

make :: forall k. (Model -> k) -> [[Item (Bool, Widget k)]] -> Model -> Widget k
make conv rows (Model gcursor) = Widget.make requestedSize mkImageKeymap
  where
    wantFocusRows = (map . map) (fst . itemChild) rows
    navKeymap = fmap (conv . Model) .
                mkNavKeymap wantFocusRows $
                gcursor

    widgetRows = (map . map . fmap) (Widget.unWidget . snd) rows
    
    -- TODO: Reduce duplication with makeView
    (requestedSize, mkPlacements) =
      makePlacements . (map . map) (Placable.pRequestedSize . itemChild) $ widgetRows
    mkImageKeymap givenSize = (mkImage, keymap)
      where
        -- Get rid of the Placable, and put the Placable and actual
        -- Size with each item:
        placementWidgetRows :: [[Item (Placement, (Size, (Widget.HasFocus -> TermImage, Keymap k)))]]
        placementWidgetRows = (zipWith . zipWith) (fmap . feedPlacable)
                              (mkPlacements givenSize) widgetRows
        
        -- Disable the cursor and HasFocus of inactive children, and
        -- replace their keymap with a Nothing. Only the active child gets
        -- a Just around his keymap:

        childWidgetRows :: [[Item (Placement, (Size, (Widget.HasFocus -> TermImage, Maybe (Keymap k))))]]
        childWidgetRows = (map . mapu) childWidget . enumerate2 $ placementWidgetRows
        childWidget :: (Int, Int) ->
                       Item (Placement, (Size, (Widget.HasFocus -> TermImage, Keymap k))) ->
                       Item (Placement, (Size, (Widget.HasFocus -> TermImage, Maybe (Keymap k))))
        childWidget index =
          (fmap . second . second)
          (if gcursor == Cursor (uncurry Vector2 index)
           then curChild
           else unCurChild)

        curChild :: (Widget.HasFocus -> TermImage, Keymap k) ->
                    (Widget.HasFocus -> TermImage, Maybe (Keymap k))
        curChild = second Just -- Keymap

        unCurChild :: (Widget.HasFocus -> TermImage, Keymap k) ->
                      (Widget.HasFocus -> TermImage, Maybe (Keymap k))
        unCurChild = ((Widget.inHasFocus . const) False ~>
                      (TermImage.inCursor . const) Nothing) ***
                     const Nothing

        mkImage hf = combineImages .
                     -- Get the TermImage:
                     (map . map . fmap . second . second) (($hf) . fst) $
                     childWidgetRows
    
        childKeymap = fromMaybe mempty .
                      mconcat .
                      -- Get the Maybe-wrapped keymaps
                      map (snd . snd . snd . itemChild) .
                      concat $
                      childWidgetRows
        keymap = childKeymap `mappend` navKeymap

--- Convenience

simpleRows :: [[Display a]] -> [[Item (Display a)]]
simpleRows = (map . map) (Item (pure 0))

setter :: w -> Accessor w p -> p -> w
setter w acc p = setVal acc p w

makeAcc :: Accessor k Model -> [[Item (Bool, Widget k)]] -> k -> Widget k
makeAcc acc rows k = make (setter k acc) rows (k ^. acc)
