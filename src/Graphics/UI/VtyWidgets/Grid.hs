{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.Grid
    (makeView, make, makeAcc, makeSizes,
     Model(..), inModel, initModel)
where

import Data.Binary(Binary)
import Data.Function.Utils(result, (~>))
import Data.List(find, transpose)
import Data.List.Utils(safeIndex)
import Data.Record.Label((:->), set, get)
import Data.Monoid(mempty, mappend, mconcat)
import Data.Maybe(maybeToList, fromMaybe, isJust)
import Data.Vector.Vector2(Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import Control.Monad(msum)
import Control.Applicative(pure, liftA2)
import Control.Arrow((***), first, second)
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Keymap(Keymap, ModKey)
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

newtype Model = Model {
  modelCursor :: Vector2 Int
  }
  deriving (Show, Read, Eq, Ord, Binary)
inModel :: (Vector2 Int -> Vector2 Int) -> Model -> Model
inModel f (Model cursor) = Model (f cursor)

initModel :: Model
initModel = Model (pure 0)

length2d [] = pure 0
length2d xs@(x:_) = Vector2 (length x) (length xs)

capCursor :: Vector2 Int -> Vector2 Int -> Vector2 Int
capCursor size = fmap (max 0) . liftA2 min (fmap (subtract 1) size)

enumerate :: (Enum a, Num a) => [b] -> [(a, b)]
enumerate = zip [0..]

enumerate2d :: (Enum a, Num a) => [[b]] -> [[(Vector2 a, b)]]
enumerate2d xss = mapu row (enumerate xss)
  where
    row rowIndex = mapu (add rowIndex) . enumerate
    add rowIndex columnIndex = (,) (Vector2 columnIndex rowIndex)

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
    minSize = fmap sum $ Vector2 columnMinWidths rowMinHeights
    maxSize = fmap sum $ Vector2 columnMaxWidths rowMaxHeights
    -- Compute all the row/column sizes:
    computeSizes f = map (maximum . map f)
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

mapu :: (a -> b -> c) -> [(a, b)] -> [c]
mapu = map . uncurry

combineImages :: Size -> [[(Placement, TermImage)]] -> TermImage
combineImages size =
  mconcat .
  mapu TermImage.translate .
  (map . first) fst .       -- Get rid of the (, Size)
  concatMap (takeWhile columnFits) .
  takeWhile rowFits
  where
    rowFits [] = False
    rowFits (((pos, _), _):_) = Vector2.snd pos < Vector2.snd size
    columnFits ((pos, _), _) = Vector2.fst pos < Vector2.fst size

--- Displays:

feedPlacable :: Placement -> Placable a -> (Placement, a)
feedPlacable pl@(_, size) placable = (pl, Placable.pPlace placable size)

makeView :: [[Display a]] -> Display a
makeView rows = Display.make requestedSize mkImage
  where
    (requestedSize, mkPlacements) =
      makePlacements .
      (map . map) Placable.pRequestedSize $
      rows
    mkImage givenSize imgarg =
      combineImages givenSize .
      (zipWith . zipWith) feedPlacable (mkPlacements givenSize) .
      -- Penetrate [[Placable (..)]] and feed the arg
      (map . map . fmap) ($ imgarg) $
      rows

--- Widgets:

kLeft :: ModKey
kLeft  = ([], Vty.KLeft)
kRight :: ModKey
kRight = ([], Vty.KRight)
kUp :: ModKey
kUp    = ([], Vty.KUp)
kDown :: ModKey
kDown  = ([], Vty.KDown)

-- | length2d assumes the given list has a square shape
length2d :: [[a]] -> Vector2 Int
mkNavKeymap :: [[Bool]] -> Vector2 Int -> Keymap (Vector2 Int)
mkNavKeymap []            _ = mempty
mkNavKeymap [[]]          _ = mempty
mkNavKeymap wantFocusRows cursor@(Vector2 cursorX cursorY) =
  mconcat . concat $ [
    movement "left"  kLeft  leftOfCursor,
    movement "right" kRight rightOfCursor,
    movement "up"    kUp    aboveCursor,
    movement "down"  kDown  belowCursor
    ]
  where
    movement dirName key pos
                  = maybeToList . fmap (Keymap.simpleton ("Move " ++ dirName) key) $ pos
    size          = length2d wantFocusRows
    Vector2 cappedX cappedY
                  = capCursor size cursor
    leftOfCursor  = fmap (`Vector2` cappedY) . findMove . reverse . take cursorX $ curRow
    aboveCursor   = fmap (cappedX `Vector2`) . findMove . reverse . take cursorY $ curColumn
    rightOfCursor = fmap (`Vector2` cappedY) . findMove . drop (cursorX+1) $ curRow
    belowCursor   = fmap (cappedX `Vector2`) . findMove . drop (cursorY+1) $ curColumn
    findMove      = fmap fst . find snd
    curRow        = enumerate $ wantFocusRows !! cappedY
    curColumn     = enumerate $ transpose wantFocusRows !! cappedX

make :: (Model -> k) -> [[Widget k]] -> Model -> Widget k
make conv rows model = Widget.make requestedSize mkImageKeymap
  where
    gcursor@(Vector2 gx gy) = modelCursor model
    widgetRows = (map . map) Widget.unWidget rows
    
    -- TODO: Reduce duplication with makeView
    (requestedSize, mkPlacements) =
      makePlacements . (map . map) Placable.pRequestedSize $ widgetRows
    mkImageKeymap givenSize = (mkImage, keymap)
      where
        -- Get rid of the Placable, and put the Placement and actual
        -- Size with each item:
        placementWidgetRows = (zipWith . zipWith) feedPlacable
                              (mkPlacements givenSize) widgetRows

        wantFocusRows = (map . map) (isJust . snd . snd) placementWidgetRows
        navKeymap = fmap (conv . Model) .
                    mkNavKeymap wantFocusRows $
                    gcursor

        -- Disable the cursor and HasFocus of inactive children, and
        -- replace their keymap with a Nothing. Only the active child gets
        -- a Just around his keymap:

        childWidgetRows = (map . mapu) childWidget . enumerate2d $ placementWidgetRows
        childWidget index =
          second
          (if gcursor == index
           then curChild
           else unCurChild)

        curChild = second Just -- Keymap

        unCurChild = ((Widget.inHasFocus . const) False ~>
                      (TermImage.inCursor . const) Nothing) ***
                     const Nothing

        mkImage hf = combineImages givenSize .
                     -- Get the TermImage:
                     (map . map . second) (($hf) . fst) $
                     childWidgetRows

        childKeymap = fromMaybe mempty $ snd . snd =<< safeIndex gx =<< safeIndex gy childWidgetRows
        keymap =
          -- Use msum to figure out whether at least one of the
          -- children has a Just keymap, and not a Nothing. If all are
          -- Nothing, we want to also remain Nothing, thus the use of
          -- (fmap . const) on the result of msum:
          (fmap . const) (fromMaybe mempty childKeymap `mappend` navKeymap) .
          msum . map (snd . snd) . concat $
          placementWidgetRows

--- Convenience

makeAcc :: k :-> Model -> [[Widget k]] -> k -> Widget k
makeAcc acc rows k = make (flip (set acc) k) rows (get acc k)
