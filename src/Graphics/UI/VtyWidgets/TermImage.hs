{-# OPTIONS -Wall -O2 #-}

module Graphics.UI.VtyWidgets.TermImage
    (TermChar, TermImage, tiCursor, inCursor, render,
     string, stringParse, stringSize, hstrings, vstrings,
     clip, translate, rect,
     boundingRect,
     -- re-export:
     Coordinate)
where

import           Data.List           (foldl')
import           Data.List.Split     (splitOn)
import           Data.List.Utils     (safeIndex, groupFst)
import           Data.Function.Utils (Endo, argument)
import           Data.Monoid         (Monoid(..), First(First, getFirst))
import           Data.Monoid.Utils   (inFirst)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import           Data.Array.Utils    (marrayAt)
import           Data.Array          ((!))
import           Data.Array.ST       (runSTArray, newArray)
import           Data.DList          (DList)
import qualified Data.DList          as DList
import qualified Data.Vector.Rect    as Rect
import           Data.Vector.Rect    (ExpandingRect(..), Rect(..), Coordinate)
import           Control.Monad       (forM_)
import           Control.Applicative (pure, liftA2)
import qualified Graphics.Vty        as Vty

type Pixels = Coordinate -> Rect -> DList (Coordinate, Endo TermChar)

type TermChar = (Vty.Attr, Char)
data TermImage = TermImage {
  tiBoundingRect :: !ExpandingRect,
  -- ^ A Rect big enough to at least cover the entire image
  tiPixels :: Pixels,
  -- ^ An action to update an array of TermChars
  tiCursor :: !(First Coordinate)
  -- ^ The position the cursor is at
  }
atBoundingRect :: Endo ExpandingRect -> Endo TermImage
atBoundingRect f ti = ti{tiBoundingRect = f (tiBoundingRect ti)}
atPixels :: Endo Pixels -> Endo TermImage
atPixels f ti = ti{tiPixels = f (tiPixels ti)}
atCursor :: Endo (First Coordinate) -> Endo TermImage
atCursor f ti = ti{tiCursor = f (tiCursor ti)}

expandBoundingRectToCursor :: Endo TermImage
expandBoundingRectToCursor ti =
  maybe ti expandToCursor . getFirst . tiCursor $ ti
  where
    expandToCursor cursor =
      atBoundingRect (cursorRect `mappend`) ti
      where
        cursorRect = ExpandingRect $ Rect cursor ((+1) `fmap` cursor)

inCursor :: Endo (Maybe Coordinate) -> Endo TermImage
inCursor f = expandBoundingRectToCursor . (atCursor . inFirst) f

boundingRect :: TermImage -> Rect
boundingRect = Rect.unExpandingRect . tiBoundingRect

instance Monoid TermImage where
  mempty = TermImage mempty mempty mempty
  TermImage bRect1 pixels1 cursor1 `mappend`
    TermImage bRect2 pixels2 cursor2 =
      TermImage (bRect1 `mappend` bRect2)
                (pixels1 `mappend` pixels2)
                (cursor1 `mappend` cursor2)

rect :: Rect -> Endo TermChar -> TermImage
rect r f = make r pixels Nothing
  where
    pixels translation area =
      DList.fromList .
      map (flip (,) f) .
      Rect.enum .
      Rect.clip area .
      Rect.translate translation $
      r

translate :: Coordinate -> TermImage -> TermImage
translate c =
  (atBoundingRect . Rect.inExpandingRect) (Rect.translate c) .
  (atCursor . fmap . liftA2 (+)) c .
  (atPixels . argument . liftA2 (+)) c

render :: TermImage -> Vty.Picture
render (TermImage eBoundingRect pixels (First mCursor)) =
  Vty.Picture cursor img bg
  where
    cursor = maybe Vty.NoCursor makeCursor mCursor
    makeCursor v = if v `Rect.inside` bRect
                   then Vector2.uncurry Vty.Cursor . fmap fromIntegral $ v
                   else Vty.NoCursor
    imageArray = runSTArray $ do
      -- TODO: Let's stop using Int with maxBound/minBound, it's
      -- terrible. Vector2 r b includes 1 more than we need in each
      -- axis
      array <- newArray (tl, br) (Vty.def_attr, ' ')
      forM_ (DList.toList . pixels (Vector2 0 0) $ bRect) $
            uncurry (marrayAt array)
      return array
    img = Vty.vert_cat $
          replicate (min t b) (Vty.char Vty.def_attr ' ') ++
          [ Vty.horiz_cat $
            Vty.string Vty.def_attr (replicate (min l r) ' ') :
            (map (uncurry Vty.string) . groupFst)
            [ imageArray ! Vector2 x y
            | x <- takeWhile (<r) [l..] ]
          | y <- takeWhile (<b) [t..] ]

    bRect = (Rect.atTopLeft . fmap) (max 0) .
            Rect.unExpandingRect $ eBoundingRect
    Rect tl@(Vector2 l t) br@(Vector2 r b) = bRect
    bg = Vty.Background ' ' Vty.def_attr

clip :: Rect -> TermImage -> TermImage
clip r = (atBoundingRect . Rect.inExpandingRect) (Rect.clip r) .
         atPixels clipPixels
  where
    clipPixels pixels translation area =
      pixels translation . Rect.clip (Rect.translate translation r) $ area

stringParse :: String -> (Vector2 Int, [String])
stringParse chars = (Vector2 w h, ls)
  where
    ls = splitOn "\n" chars
    w = fromIntegral (foldl' max 0 . map length $ ls)
    h = fromIntegral (length ls)

make :: Rect -> Pixels -> Maybe Coordinate -> TermImage
make r m c = TermImage (ExpandingRect r) m (First c)

string :: Vty.Attr -> String -> TermImage
string attr chars = make rangeRect pixels Nothing
  where
    pixels translation area =
      DList.fromList .
      concatMap (makePixel translation) .
      Rect.enum .
      Rect.clip area .
      Rect.translate translation $
      rangeRect
    makePixel translation v =
      maybe [] (return . (,) v . const . (,) attr) (safeIndex column =<< safeIndex row ls)
        where
          Vector2 column row = liftA2 (-) v translation
    rangeRect = Rect (pure 0) size
    (size, ls) = stringParse chars

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
