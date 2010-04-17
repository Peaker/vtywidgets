{-# OPTIONS -O2 -Wall #-}

module LineEdit(lineEdit, Model(..)) where

import Data.Char(chr)
import Data.Monoid(mconcat)
import qualified Data.Map as Map
import qualified Graphics.Vty as Vty
import qualified TermImage
import Vector2(Vector2(..))
import qualified Keymap
import Widget(Widget, WidgetFields(WidgetFields))
import Control.Arrow(first)

type Cursor = Int

data Model = Model {
  lineEditCursor :: Cursor,
  lineEditText :: String
  }

lineEdit :: Vty.Attr -> Widget Model
lineEdit attr (Model cursorXw text) =
  WidgetFields (TermImage.string attr text)
               (Just (Vector2 cursorXw 0))
               keymap
  where
    cursorX = fromIntegral cursorXw
    width = length text
    keymap =
      fmap (uncurry Model . first fromIntegral) . mconcat . concat $ [
        [ Keymap.singleton "Left" "Move left" ([], Vty.KLeft) (cursorX-1, text)
        | cursorX > 0 ],
        [ Keymap.singleton "Right" "Move right" ([], Vty.KRight) ((cursorX+1, text))
        | cursorX < width ],
        [ Keymap.singleton "Backspace" "Delete backwards" ([], Vty.KBS)
          (cursorX-1, take (cursorX-1) text ++ drop cursorX text)
        | cursorX > 0 ],
        [ Keymap.singleton "Delete" "Delete forward" ([], Vty.KDel)
          (cursorX, before ++ drop 1 after)
        | cursorX < width ],
        [ Keymap.fromGroups [ ("Alphabet", ("Insert", Map.fromList insertKeys)) ] ]
        ]
    insertKeys =
      [ (([], Vty.KASCII l), (cursorX+1, before ++ l:after))
      | x <- [0..127]
      , let l = chr x ]
    (before, after) = splitAt cursorX text