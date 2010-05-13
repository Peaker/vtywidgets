{-# OPTIONS -O2 -Wall #-}
module Graphics.UI.VtyWidgets.Scroll
    (makeView)
where

import Control.Applicative(pure, liftA2)
import Data.Function.Utils(argument)
import Data.Maybe(fromMaybe)
import Data.Monoid(First(..), mempty)
import Graphics.UI.VtyWidgets.Rect(Coordinate, Rect(..))
import qualified Data.Vector.Vector2 as Vector2
import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.VtyWidgets.Widget(Display)
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Graphics.UI.VtyWidgets.SizeRange(SizeRange, Size)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import qualified Graphics.UI.VtyWidgets.Bar as Bar
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.TermImage(TermImage)

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
makeView :: SizeRange
            -- ^ Size range of the scroller itself, including bars
            -> Display a
            -- ^ The display to scroll through
            -> Display a
makeView sizeRange' (Widget.Placable sizeRange mkImage) =
  Widget.makeDisplay sizeRange' mkGridImage
  where
    mkGridImage givenSize imgarg = image'
      where
        -- Just use the maximum size for the scrollable:
        scrollSize = SizeRange.srMaxSize sizeRange
        bigEnough = liftA2 (>=) givenSize scrollSize
        barsNeeded = Vector2 True True /= bigEnough

        hbar = Bar.makeHorizontal 3
        vbar = Bar.makeVertical 3

        image = mkImage scrollSize imgarg
        image' = if barsNeeded
                 then Widget.placablePlace (Grid.makeView . Grid.simpleRows $ rows) givenSize imgarg
                 else image
        getSSize rs = sSize
          where
            [[_, _],
             [_, sSize]] = ($ givenSize) . snd . Grid.makeSizes .
                           (map . map) Widget.placableRequestedSize $ rs
        -- Worst-case, we'll have both bars:
        -- Check which scroll bars we still need:
        wcSize = getSSize [[ mempty, hbar ],
                           [ vbar, mempty ]]
        Vector2 hbarNeeded vbarNeeded = liftA2 (<) wcSize scrollSize

        makeBar m range = (fmap . argument) (const range) $ m 3
        conditionalMakeBar p = if p then makeBar else mempty
        rows = [[ mempty,
                  conditionalMakeBar hbarNeeded Bar.makeHorizontal hrange ],
                [ conditionalMakeBar vbarNeeded Bar.makeVertical   vrange,
                  Widget.makeDisplay (SizeRange.expanding 0 0) ((const . const) scrollImage) ]]
          where
            sSize = getSSize rows
            (Rect leftTop rightBottom, scrollImage) =
              makeCenteredImage sSize scrollSize image
            mkRange dim = (dimRange leftTop, dimRange rightBottom)
              where
                fiDim = fromIntegral . dim
                dimRange = (/fiDim scrollSize) . fiDim
            hrange = mkRange Vector2.fst
            vrange = mkRange Vector2.snd
