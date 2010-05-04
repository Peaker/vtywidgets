{-# OPTIONS -O2 -Wall #-}
module Graphics.UI.VtyWidgets.Scroll
    (makeView)
where

import Control.Applicative(pure, liftA2)
import Data.Maybe(fromMaybe)
import Data.Monoid(First(..))
import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.VtyWidgets.Rect(Rect(..), unExpandingRect)
import Graphics.UI.VtyWidgets.Widget(Display)
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.TermImage(TermImage)

moveIn :: Rect -> Rect -> Rect
moveIn big small = Rect smalltl' smallbr'
  where
    Rect bigtl bigbr = big
    Rect smalltl smallbr = small
    smallSize = liftA2 (-) smallbr smalltl
    smalltl' = liftA2 max bigtl
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

centered :: Rect -> TermImage -> TermImage
centered showRange image = image'
  where
    imageRect = unExpandingRect . TermImage.boundingRect $ image
    focalPoint = fromMaybe (pure 0) . getFirst . TermImage.tiCursor $ image
    Rect tl' _ = moveIn imageRect . centerRect focalPoint $ showRange
    image' = TermImage.translate (fmap negate tl') image

makeView :: Widget.SizeRange -> Display a -> Display a
makeView sizeRange disp =
  Widget.makeDisplay sizeRange mkImage
  where
    mkImage imgArg size =
      centered (Rect (pure 0) size) .
      Widget.displayImage disp imgArg .
      Widget.srMaxSize .
      Widget.displayRequestedSize $
      disp
