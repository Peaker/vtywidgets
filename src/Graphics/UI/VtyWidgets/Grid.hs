{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.Grid
    (makeView, make, makeAcc, getCursor, makeSizes,
     DelegatedModel, initDelegatedModel, makeDelegated, makeAccDelegated,
     Model(..), initModel)
where

import Data.Binary(Binary)
import Data.Function.Utils(result, (~>))
import Data.List(transpose)
import Data.Record.Label((:->), set, get)
import Data.Monoid(mempty, mappend, mconcat)
import Data.Maybe(fromMaybe)
import Data.Vector.Vector2(Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import Control.Applicative(pure, liftA2)
import Control.Arrow((***), first, second)
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Graphics.UI.VtyWidgets.Widget(Widget(..))
import qualified Graphics.UI.VtyWidgets.Placable as Placable
import Graphics.UI.VtyWidgets.Placable(Placable(..))
import qualified Graphics.UI.VtyWidgets.Display as Display
import qualified Graphics.UI.VtyWidgets.FocusDelegator as FocusDelegator
import Graphics.UI.VtyWidgets.Display(Display)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import Graphics.UI.VtyWidgets.SizeRange(SizeRange(..), Size)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.TermImage(TermImage, Coordinate)

-- Model:

newtype Model = Model {
  modelCursor :: Vector2 Int
  }
  deriving (Show, Read, Eq, Ord, Binary)

initModel :: Model
initModel = Model (pure 0)

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
makeView [] = mempty
makeView [[]] = mempty
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

enumerate :: (Enum a, Num a) => [b] -> [(a, b)]
enumerate = zip [0..]

enumerate2 :: (Enum a, Num a) => [[b]] -> [[((a, a), b)]]
enumerate2 xss = mapu row (enumerate xss)
  where
    row rowIndex = mapu (add rowIndex) . enumerate
    add rowIndex columnIndex = (,) (columnIndex, rowIndex)

mkNavKeymap :: [[Bool]] -> Vector2 Int -> Keymap (Vector2 Int)
mkNavKeymap wantFocusRows cursor@(Vector2 cursorX cursorY) =
  mconcat . concat $ [
    mover "left"  ([], Vty.KLeft)  Vector2.first  (-) (reverse . take cursorX $ curRow),
    mover "right" ([], Vty.KRight) Vector2.first  (+) (drop (cursorX + 1)       curRow),
    mover "up"    ([], Vty.KUp)    Vector2.second (-) (reverse . take cursorY $ curColumn),
    mover "down"  ([], Vty.KDown)  Vector2.second (+) (drop (cursorY + 1)       curColumn)
    ]
  where
    mover dirName key axis sign xs =
       [ Keymap.simpleton ("Move " ++ dirName) key ((axis . flip sign . (+1) . countUnwanters $ xs) cursor)
       | True `elem` xs ]
    curColumn = transpose wantFocusRows !! cursorX
    curRow = wantFocusRows !! cursorY
    countUnwanters = length . takeWhile not

-- | length2d assumes the given list has a square shape
length2d :: [[a]] -> Vector2 Int
length2d [] = pure 0
length2d xs@(x:_) = Vector2 (length x) (length xs)

getCursor :: [[(Bool, Widget k)]] -> Model -> Vector2 Int
getCursor rows =
  fmap (max 0) .
  liftA2 min (fmap (subtract 1) $ length2d rows) .
  modelCursor

make :: (Model -> k) -> [[(Bool, Widget k)]] -> Model -> Widget k
make _ [] _ = mempty
make _ [[]] _ = mempty
make conv rows model = Widget.make requestedSize mkImageKeymap
  where
    gcursor@(Vector2 gx gy) = getCursor rows model
    wantFocusRows = (map . map) fst rows
    navKeymap = fmap (conv . Model) .
                mkNavKeymap wantFocusRows $
                gcursor

    widgetRows = (map . map) (Widget.unWidget . snd) rows
    
    -- TODO: Reduce duplication with makeView
    (requestedSize, mkPlacements) =
      makePlacements . (map . map) Placable.pRequestedSize $ widgetRows
    mkImageKeymap givenSize = (mkImage, keymap)
      where
        -- Get rid of the Placable, and put the Placement and actual
        -- Size with each item:
        placementWidgetRows = (zipWith . zipWith) feedPlacable
                              (mkPlacements givenSize) widgetRows
        
        -- Disable the cursor and HasFocus of inactive children, and
        -- replace their keymap with a Nothing. Only the active child gets
        -- a Just around his keymap:

        childWidgetRows = (map . mapu) childWidget . enumerate2 $ placementWidgetRows
        childWidget index =
          second
          (if gcursor == uncurry Vector2 index
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

        childKeymap = fromMaybe mempty . snd . snd $ childWidgetRows !! gy !! gx
        keymap = childKeymap `mappend` navKeymap

--- Convenience

makeAcc :: k :-> Model -> [[(Bool, Widget k)]] -> k -> Widget k
makeAcc acc rows k = make (flip (set acc) k) rows (get acc k)

type DelegatedModel = (FocusDelegator.Model, Model)

initDelegatedModel :: Bool -> DelegatedModel
initDelegatedModel = flip (,) initModel . FocusDelegator.initModel

makeDelegated :: (DelegatedModel -> k) -> [[(Bool, Widget k)]] -> DelegatedModel -> Widget k
makeDelegated conv rows (fdm, m) = focusDelegator
  where
    focusDelegator = FocusDelegator.make (\fdm' -> conv (fdm', m)) grid fdm
    grid = make (\m' -> conv (fdm, m')) rows m

makeAccDelegated :: k :-> DelegatedModel -> [[(Bool, Widget k)]] -> k -> Widget k
makeAccDelegated acc rows k = makeDelegated (flip (set acc) k) rows (get acc k)
