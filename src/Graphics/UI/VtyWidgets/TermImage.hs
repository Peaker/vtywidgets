{-# OPTIONS -Wall -O2 #-}

module Graphics.UI.VtyWidgets.TermImage
    (TermChar, TermImage(..),
     atImage, atCursor, setCursor,
     render,
     string, stringSize, hstrings, vstrings,
     clip, translate,
     boundingRect, atBoundingRect)
where

import Data.Maybe(fromMaybe)
import Data.List(foldl', groupBy)
import Data.List.Split(splitOn)
import Data.List.Utils(safeIndex)
import Data.Function(on)
import Data.Monoid(Monoid(..), First(First, getFirst))
import Data.Vector.Vector2(Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import Control.Applicative(pure, liftA2)
import qualified Graphics.Vty as Vty
import Graphics.UI.VtyWidgets.Rect(ExpandingRect(..), Rect(..), Coordinate)
import Graphics.UI.VtyWidgets.Image(Image)
import qualified Graphics.UI.VtyWidgets.Image as Image
import Graphics.UI.VtyWidgets.TMap(TMap)
import qualified Graphics.UI.VtyWidgets.TMap as TMap

type Endo a = a -> a

type TermChar = First (Vty.Attr, Char)
data TermImage = TermImage {
  tiImage :: Image TermChar,
  tiCursor :: First Coordinate
  }
atImage :: Endo (Image TermChar) -> Endo TermImage
atImage f ti = ti{tiImage = f (tiImage ti)}
atCursor :: Endo (First Coordinate) -> Endo TermImage
atCursor f ti = ti{tiCursor = f (tiCursor ti)}

setCursor :: Maybe Coordinate -> Endo TermImage
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
translate c =
  (atCursor . fmapFirst . liftA2 (+) $ c) .
  (atImage . Image.translate $ c)

boundingRect :: TermImage -> ExpandingRect
boundingRect = Image.boundingRect . tiImage

atBoundingRect :: Endo ExpandingRect -> Endo TermImage
atBoundingRect = atImage . Image.atBoundingRect

groupFst :: Eq a => [(a, b)] -> [(a, [b])]
groupFst = map extractFst . groupBy ((==) `on` fst)
  where
    extractFst [] = error "groupBy returned an empty group"
    extractFst grp = (fst . head $ grp, map snd grp)

render :: TermImage -> Vty.Picture
render (TermImage image (First mCursor)) =
  Vty.Picture cursor img bg
  where
    cursor = maybe Vty.NoCursor (Vector2.uncurry Vty.Cursor . fmap fromIntegral) mCursor
    img = Vty.vert_cat $
          replicate (min t b) (Vty.char Vty.def_attr ' ') ++
          [ Vty.horiz_cat $
            Vty.string Vty.def_attr (replicate (min l r) ' ') :
            (map (uncurry Vty.string) . groupFst)
            [ fromMaybe (Vty.def_attr, ' ') . getFirst . f $ Vector2 x y
            | x <- [l..r] ]
          | y <- [t..b] ]
          -- we don't need (r, b), but we still iterate them, it's an
          -- inclusive/exclusive range, but we want to avoid
          -- arithmetic on these, as they may be minBound/maxBound

    ExpandingRect (Rect (Vector2 l t) (Vector2 r b)) = Image.boundingRect image
    f = Image.pick image
    bg = Vty.Background ' ' Vty.def_attr

clip :: Rect -> TermImage -> TermImage
clip = atImage . Image.clip

make :: Rect -> TMap Coordinate TermChar -> TermImage
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
string attr chars = make (Rect (pure 0) (Vector2 w h)) m
  where
    m = foldr (.) id addItems mempty
    addItems = [ TMap.override (Vector2 x y) . First . fmap ((,) attr) $ safeIndex y ls >>= safeIndex x
               | x <- [0..w-1]
               , y <- [0..h-1] ]
    (Vector2 w h, ls) = stringParse chars

stringSize :: String -> Vector2 Int
stringSize = fst . stringParse

combineStrings :: (Endo Int -> Endo (Vector2 Int)) -> [(Vty.Attr, String)] -> TermImage
combineStrings toIgnore = foldr combine mempty
  where
    combine (attr, str) rest =
      string attr str `mappend`
      (translate . (toIgnore . const) 0 . stringSize) str rest

hstrings :: [(Vty.Attr, String)] -> TermImage
hstrings = combineStrings Vector2.second

vstrings :: [(Vty.Attr, String)] -> TermImage
vstrings = combineStrings Vector2.first
