{-# OPTIONS -Wall -O2 #-}

module Graphics.UI.VtyWidgets.TermImage
    (TermChar, TermImage(..),
     atImage, atCursor, setCursor,
     render, string, stringSize,
     translate, boundingRect, atBoundingRect)
where

import Data.Maybe(fromMaybe)
import Data.List(foldl')
import Data.List.Split(splitOn)
import Data.List.Utils(safeIndex)
import Data.Monoid(Monoid(..), First(First, getFirst))
import Control.Applicative(pure, liftA2)
import qualified Graphics.Vty as Vty
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))
import qualified Graphics.UI.VtyWidgets.Vector2 as Vector2
import Graphics.UI.VtyWidgets.Rect(ExpandingRect(..), Rect(..), Coordinate)
import Graphics.UI.VtyWidgets.Image(Image)
import qualified Graphics.UI.VtyWidgets.Image as Image

type Endo a = a -> a

type TermChar = (Vty.Attr, Char)
data TermImage = TermImage {
  tiImage :: Image (First TermChar),
  tiCursor :: First (Vector2 Int)
  }
atImage :: Endo (Image (First TermChar)) -> Endo TermImage
atImage f ti = ti{tiImage = f (tiImage ti)}
atCursor :: Endo (First (Vector2 Int)) -> Endo TermImage
atCursor f ti = ti{tiCursor = f (tiCursor ti)}

setCursor :: Maybe (Vector2 Int) -> Endo TermImage
setCursor = atCursor . const . First

instance Monoid TermImage where
  mempty = TermImage mempty mempty
  TermImage img1 cursor1 `mappend` TermImage img2 cursor2 =
    TermImage (img1 `mappend` img2) (cursor1 `mappend` cursor2)

inFirst :: (Maybe a -> Maybe b) -> First a -> First b
inFirst f = First . f . getFirst

fmapFirst :: (a -> b) -> First a -> First b
fmapFirst = inFirst . fmap

translate :: Coordinate -> TermImage -> TermImage
translate c = (atCursor . fmapFirst) (liftA2 (+) c) . atImage (Image.translate c)

boundingRect :: TermImage -> ExpandingRect
boundingRect = Image.boundingRect . tiImage

atBoundingRect :: Endo ExpandingRect -> Endo TermImage
atBoundingRect = atImage . Image.atBoundingRect

render :: TermImage -> Vty.Picture
render (TermImage image (First mCursor)) =
  Vty.Picture cursor img bg
  where
    cursor = maybe Vty.NoCursor (Vector2.uncurry Vty.Cursor . fmap fromIntegral) mCursor
    img = Vty.vert_cat $
          replicate (min t b) (Vty.char Vty.def_attr ' ') ++
          [ Vty.horiz_cat $
            Vty.string Vty.def_attr (replicate (min l r) ' ') :
            [ uncurry Vty.char . fromMaybe (Vty.def_attr, ' ') . getFirst . f $ Vector2 x y
            | x <- [l..r] ] -- we don't need (r, b), it's an
                            -- inclusive/exclusive range, but we want
                            -- to avoid arithmetic on these, as they
                            -- may be minBound/maxBound
          | y <- [t..b] ]
    ExpandingRect (Rect (Vector2 l t) (Vector2 r b)) = Image.boundingRect image
    f = Image.pick image
    bg = Vty.Background ' ' Vty.def_attr

make :: Rect -> (Coordinate -> First TermChar) -> TermImage
make r f = TermImage {
  tiImage = Image.make (ExpandingRect r) f,
  tiCursor = First Nothing
  }

stringParse :: String -> (Vector2 Int, [String])
stringParse chars = (Vector2 w h, ls)
  where
    ls = splitOn "\n" chars
    w = fromIntegral (foldl' max 0 . map length $ ls)
    h = fromIntegral (length ls)

string :: Vty.Attr -> String -> TermImage
string attr chars = make (Rect (pure 0) (Vector2 w h)) func
  where
    func (Vector2 x y) = if 0 <= x && x < w &&
                            0 <= y && y < h
                         then First . fmap ((,) attr) $ safeIndex y ls >>= safeIndex x
                         else First $ Nothing
    (Vector2 w h, ls) = stringParse chars

stringSize :: String -> Vector2 Int
stringSize = fst . stringParse
