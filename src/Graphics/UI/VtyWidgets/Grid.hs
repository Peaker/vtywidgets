{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.Grid
    (makeSizes, makeView, make, makeAcc, makeCombined,
     Model(..), inModel, initModel)
where

import           Data.Binary                      (Binary)
import           Data.Function.Utils              (argument, result)
import           Data.List                        (find, transpose)
import           Data.List.Utils                  (safeIndex)
import           Data.Record.Label                ((:->), set, get)
import           Data.Monoid                      (mappend, mconcat)
import           Data.Maybe                       (isJust)
import           Data.Vector.Vector2              (Vector2(..))
import qualified Data.Vector.Vector2              as Vector2
import           Control.Applicative              (pure, liftA2)
import           Control.Arrow                    (first, second)
import qualified Graphics.Vty                     as Vty
import qualified Graphics.UI.VtyWidgets.EventMap  as EventMap
import           Graphics.UI.VtyWidgets.ModKey    (ModKey(..))
import qualified Graphics.UI.VtyWidgets.Widget    as Widget
import           Graphics.UI.VtyWidgets.Widget    (Widget(..))
import qualified Graphics.UI.VtyWidgets.Placable  as Placable
import           Graphics.UI.VtyWidgets.Placable  (Placable(..))
import qualified Graphics.UI.VtyWidgets.Display   as Display
import           Graphics.UI.VtyWidgets.Display   (Display)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import           Graphics.UI.VtyWidgets.SizeRange (SizeRange(..), Size)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import           Graphics.UI.VtyWidgets.TermImage (TermImage, Coordinate)

newtype Model = Model {
  modelCursor :: Vector2 Int
  }
  deriving (Show, Read, Eq, Ord, Binary)
inModel :: (Vector2 Int -> Vector2 Int) -> Model -> Model
inModel f (Model cursor) = Model (f cursor)

initModel :: Model
initModel = Model (pure 0)

-- | length2d assumes the given list has a square shape
length2d :: [[a]] -> Vector2 Int
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

makePlacements :: [[Placable r]] -> (SizeRange, Size -> [[Placement]])
makePlacements = (result . second . result) placements makeSizes . (map . map) Placable.pRequestedSize
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
    (requestedSize, mkPlacements) = makePlacements $ rows
    mkImage givenSize imgarg =
      combineImages givenSize .
      (zipWith . zipWith) feedPlacable (mkPlacements givenSize) .
      -- Penetrate [[Placable (..)]] and feed the arg
      (map . map . fmap) ($ imgarg) $
      rows

--- Widgets:

kLeft   :: ModKey
kLeft    = ModKey [] Vty.KLeft
kRight  :: ModKey
kRight   = ModKey [] Vty.KRight
kUp     :: ModKey
kUp      = ModKey [] Vty.KUp
kDown   :: ModKey
kDown    = ModKey [] Vty.KDown
kPgDown :: ModKey
kPgDown  = ModKey [] Vty.KPageDown
kPgUp   :: ModKey
kPgUp    = ModKey [] Vty.KPageUp
kHome   :: ModKey
kHome    = ModKey [] Vty.KHome
kEnd    :: ModKey
kEnd     = ModKey [] Vty.KEnd

mkNavMEventMap :: [[Bool]] -> Model -> Maybe (Widget.EventMap Model)
mkNavMEventMap []            _ = Nothing
mkNavMEventMap [[]]          _ = Nothing
mkNavMEventMap wantFocusRows (Model cursor@(Vector2 cursorX cursorY)) =
  fmap Widget.fromKeymap . mconcat $ [
    movement "left"      kLeft   leftOfCursor,
    movement "right"     kRight  rightOfCursor,
    movement "up"        kUp     aboveCursor,
    movement "down"      kDown   belowCursor,
    movement "top"       kPgUp   topCursor,
    movement "bottom"    kPgDown bottomCursor,
    movement "leftmost"  kHome   leftMostCursor,
    movement "rightmost" kEnd    rightMostCursor
    ]
  where
    Vector2 cappedX cappedY = capCursor size cursor
    movement dirName key pos = (EventMap.simpleton ("Move " ++ dirName) key . Model) `fmap` pos
    size = length2d wantFocusRows
    x = fmap (cappedX `Vector2`) . findMove
    y = fmap (`Vector2` cappedY) . findMove
    leftOfCursor    = y . reverse . take cursorX $ curRow
    aboveCursor     = x . reverse . take cursorY $ curColumn
    rightOfCursor   = y . drop (cursorX+1) $ curRow
    belowCursor     = x . drop (cursorY+1) $ curColumn
    topCursor       = x . take (min 1 cursorY) $ curColumn
    leftMostCursor  = y . take (min 1 cursorX) $ curRow
    bottomCursor    = x . take 1 . reverse . drop (cursorY+1) $ curColumn
    rightMostCursor = y . take 1 . reverse . drop (cursorX+1) $ curRow
    findMove      = fmap fst . find snd
    curRow        = enumerate $ wantFocusRows !! cappedY
    curColumn     = enumerate $ transpose wantFocusRows !! cappedX

make :: (Model -> k) -> [[Widget k]] -> Model -> Widget k
make conv rows model@(Model gcursor@(Vector2 gx gy)) =
  Widget.fromTuple $ makeTuple
  where
    makeTuple hf = (requestedSize, mkImageEventMap)
      where
        neutralize f = (map . map) (onEach f) . enumerate2d
        onEach f (index, item)
          | index == gcursor = item
          | otherwise        = f item
        -- TODO: Enumerate immediately so I can fix hf...
        placableRows =
          (map . map) (($ hf) . Widget.unWidget) .
          neutralize (Widget.inWidget . argument . const $ Widget.HasFocus False) $
          rows
        -- TODO: Reduce duplication with makeView
        (requestedSize, mkPlacements) = makePlacements placableRows
        mkImageEventMap givenSize = (image, mEventMap)
          where
            -- Get rid of the Placable, and put the Placement and actual
            -- Size with each item:
            placedItems = (zipWith . zipWith) feedPlacable
                          (mkPlacements givenSize) placableRows
            wantFocusRows = (map . map) (isJust . snd . snd) placedItems
            uncursor = (TermImage.inCursor . const) Nothing
            image = combineImages givenSize .
                    neutralize (second uncursor) .
                    (map . map . second) fst $
                    placedItems
            navMEventMap = (fmap . fmap) conv $ mkNavMEventMap wantFocusRows model
            mEventMap = (`mappend` navMEventMap) $
                      snd . snd =<<
                      safeIndex gx =<<
                      safeIndex gy placedItems

--- Convenience

makeAcc :: k :-> Model -> [[Widget k]] -> k -> Widget k
makeAcc acc rows k = make (flip (set acc) k) rows (get acc k)

-- | A combined grid sends its key inputs to all of the children
makeCombined :: [[Widget k]] -> Widget k
makeCombined rows =
  Widget.fromDisplay (mconcat . map Widget.eventMap . concat $ rows) $
  \hf -> makeView . (map . map) (($ hf) . Widget.toDisplay) $ rows
