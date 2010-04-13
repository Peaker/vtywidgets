{-# OPTIONS -Wall -O2 #-}

module TermImage(TermChar, TermImage, render, string, stringSize,
                 -- re-exports:
                 translate, boundingRect) where

import qualified Graphics.Vty as Vty
import Data.Maybe(fromMaybe, listToMaybe)
import Data.Monoid(First(First, getFirst))
import Data.Word(Word)
import Control.Applicative(pure)
import Image(Image, mkImage, translate, boundingRect)
import qualified Image
import Vector2(Vector2(..))
import qualified Vector2

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
    (_, Vector2 r b) = Image.boundingRect image
    f = Image.pick image

safeIndex :: Integral ix => ix -> [a] -> Maybe a
safeIndex n = listToMaybe . drop (fromIntegral n)

stringParse :: String -> (Vector2 Word, [String])
stringParse chars = (Vector2 w h, ls)
  where
    ls = lines chars
    w = fromIntegral (maximum . map length $ ls)
    h = fromIntegral (length ls)

string :: Vty.Attr -> String -> TermImage
string attr chars = mkImage (pure 0, Vector2 w h) func
  where
    func (Vector2 x y) = if 0 <= x && x < w &&
                            0 <= y && y < h
                         then First . fmap ((,) attr) $ safeIndex y ls >>= safeIndex x
                         else First $ Nothing
    ls = lines chars
    w = fromIntegral (maximum . map length $ ls)
    h = fromIntegral (length ls)

stringSize :: String -> Vector2 Word
stringSize = fst . stringParse
