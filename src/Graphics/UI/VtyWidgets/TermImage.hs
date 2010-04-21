{-# OPTIONS -Wall -O2 #-}

module Graphics.UI.VtyWidgets.TermImage
    (TermChar, TermImage, render, string, stringSize,
     -- re-exports:
     translate,
     Coordinate, ClipRect(..), unClipRect, inClipRect, inClipRect2, inTopLeft, inBottomRight,
     boundingRect, inBoundingRect)
where

import Data.Maybe(fromMaybe)
import Data.List(foldl')
import Data.List.Split(splitOn)
import Data.List.Utils(safeIndex)
import Data.Monoid(First(First, getFirst))
import Control.Applicative(pure)
import qualified Graphics.Vty as Vty
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))
import Graphics.UI.VtyWidgets.Image(Image, mkImage,
                                    translate,
                                    Coordinate, ClipRect(..),
                                    unClipRect, inClipRect, inClipRect2,
                                    inTopLeft, inBottomRight,
                                    boundingRect, inBoundingRect)
import qualified Graphics.UI.VtyWidgets.Image as Image

type TermChar = (Vty.Attr, Char)
type TermImage = Image (First TermChar)

render :: TermImage -> Vty.Image
render image =
  Vty.vert_cat
  [ Vty.horiz_cat
    [ uncurry Vty.char . fromMaybe (Vty.def_attr, ' ') . getFirst . f $ Vector2 x y
    | x <- [0..r-1] ]
  | y <- [0..b-1]
  ]
  where
    ClipRect _ (Vector2 r b) = Image.boundingRect image
    f = Image.pick image

stringParse :: String -> (Vector2 Int, [String])
stringParse chars = (Vector2 w h, ls)
  where
    ls = splitOn "\n" chars
    w = fromIntegral (foldl' max 0 . map length $ ls)
    h = fromIntegral (length ls)

string :: Vty.Attr -> String -> TermImage
string attr chars = mkImage (ClipRect (pure 0) (Vector2 w h)) func
  where
    func (Vector2 x y) = if 0 <= x && x < w &&
                            0 <= y && y < h
                         then First . fmap ((,) attr) $ safeIndex y ls >>= safeIndex x
                         else First $ Nothing
    (Vector2 w h, ls) = stringParse chars

stringSize :: String -> Vector2 Int
stringSize = fst . stringParse
