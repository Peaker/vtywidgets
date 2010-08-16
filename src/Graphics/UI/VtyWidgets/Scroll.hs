{-# OPTIONS -O2 -Wall #-}
module Graphics.UI.VtyWidgets.Scroll
    (centeredView, Theme(..), standardTheme)
where

import           Control.Applicative              (pure, liftA2)
import           Control.Arrow                    (first)
import           Data.Functor.Identity            (Identity(..))
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      (First(..), mempty)
import           Data.Vector.Rect                 (Coordinate, Rect(..))
import qualified Data.Vector.Vector2              as Vector2
import           Data.Vector.Vector2              (Vector2(..))
import qualified Graphics.UI.VtyWidgets.Display   as Display
import           Graphics.UI.VtyWidgets.Display   (Display)
import           Graphics.UI.VtyWidgets.Placable  (Placable(..))
import qualified Graphics.UI.VtyWidgets.Placable  as Placable
import           Graphics.UI.VtyWidgets.SizeRange (SizeRange, Size)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import qualified Graphics.UI.VtyWidgets.Grid      as Grid
import qualified Graphics.UI.VtyWidgets.Bar       as Bar
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import           Graphics.UI.VtyWidgets.TermImage (TermImage)

data Theme = Theme {
  themeHBar :: Bar.Theme,
  themeVBar :: Bar.Theme
  }

-- move small rect into the big one, if it is out of it...
moveIn :: Rect -> Rect -> Rect
moveIn big small = Rect smalltl' smallbr'
  where
    Rect bigtl bigbr = big
    Rect smalltl smallbr = small
    smallSize = liftA2 (-) smallbr smalltl
    maxtl = liftA2 (-) bigbr smallSize
    smalltl' = liftA2 max bigtl .
               liftA2 min maxtl $
               smalltl
    smallbr' = liftA2 min bigbr .
               liftA2 (+) smalltl' $
               smallSize

centerRect :: Coordinate -> Rect -> Rect
centerRect pos (Rect tl br) = Rect tl' br'
  where
    size = liftA2 (-) br tl
    halfSize = fmap (`div` 2) size
    tl' = liftA2 (-) pos halfSize
    br' = liftA2 (+) tl' size

centered :: Rect -> Rect -> TermImage -> (Rect, TermImage)
centered showRange imageRect image = (imageRect', image')
  where
    focalPoint = fromMaybe (pure 0) . getFirst . TermImage.tiCursor $ image
    imageRect'@(Rect tl' _) = moveIn imageRect . centerRect focalPoint $ showRange
    image' = TermImage.translate (fmap negate tl') .
             TermImage.clip imageRect' $
             image

makeCenteredImage :: Size -> Size -> TermImage -> (Rect, TermImage)
makeCenteredImage showSize imageSize = centered (Rect (pure 0) showSize) (Rect (pure 0) imageSize)

-- Terminology:
--
--   -----HBar-----
-- : /````````````\
-- V |            |
-- B |   Scroll   |
-- a |   display  |
-- r |            |
-- : \____________/

sizedCenteredView :: Theme -> SizeRange
                     -- ^ Size range of the scroller itself, including bars
                     -> Display f
                     -- ^ The display to scroll through
                     -> Display f
sizedCenteredView theme sizeRange' (Placable sizeRange mkImage) =
  Display.make sizeRange' mkGridImage
  where
    mkGridImage givenSize = fImage'
      where
        -- Just use the maximum size for the scrollable:
        scrollSize = SizeRange.srMaxSize sizeRange
        bigEnough = liftA2 (>=) givenSize scrollSize
        barsNeeded = Vector2 True True /= bigEnough

        hbar = Bar.makeHorizontal (themeHBar theme) 3
        vbar = Bar.makeVertical (themeVBar theme) 3

        fImage = mkImage scrollSize
        fImage' = if barsNeeded
                  then Placable.pPlace (Grid.makeView rows) givenSize
                  else fImage
        getSSize rs = sSize
          where
            [[_, _],
             [_, sSize]] = ($ givenSize) . snd . Grid.makeSizes .
                           (map . map) Placable.pRequestedSize $ rs
        -- Worst-case, we'll have both bars:
        -- Check which scroll bars we still need:
        wcSize = getSSize [[ mempty, hbar ],
                           [ vbar, mempty ]]
        Vector2 hbarNeeded vbarNeeded = liftA2 (<) wcSize scrollSize

        makeBar m range = Placable.atPlace (Identity . ($range)) $ m 3
        conditionalMakeBar p = if p then makeBar else mempty
        rows = [[ mempty,
                  conditionalMakeBar hbarNeeded (Bar.makeHorizontal $ themeHBar theme) hrange ],
                [ conditionalMakeBar vbarNeeded (Bar.makeVertical $ themeVBar theme)   vrange,
                  Display.make (SizeRange.expanding 0 0) (const . const $ scrollImage) ]]
          where
            sSize = getSSize rows
            fPair = fmap (first toRange . makeCenteredImage sSize scrollSize) fImage
            toRange (Rect leftTop rightBottom) = (mkRange Vector2.fst, mkRange Vector2.snd)
              where
                mkRange dim = (dimRange leftTop, dimRange rightBottom)
                  where
                    fiDim = fromIntegral . dim
                    dimRange = (/fiDim scrollSize) . fiDim

centeredView :: Theme ->
                SizeRange
                -- ^ Size range of the scroller itself, including bars
                -> Display a
                -- ^ The display to scroll through
                -> Display a
centeredView theme requestedSize display =
  sizedCenteredView theme requestedSize' display
  where
    requestedSize' = SizeRange.atBothSizes
                     (liftA2 min . fmap (+1) $ maxSize)
                     requestedSize
    maxSize = SizeRange.srMaxSize . Placable.pRequestedSize $ display

standardTheme :: Theme
standardTheme = Theme Bar.standardTheme Bar.standardTheme
