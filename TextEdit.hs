{-# OPTIONS -O2 -Wall #-}

module TextEdit(textEdit, Model(..)) where

import Data.Char(chr)
import Data.List(intercalate)
import Data.Monoid(mconcat)
import qualified Data.Map as Map
import qualified Graphics.Vty as Vty
import qualified TermImage
import Vector2(Vector2(..))
import qualified Keymap
import Widget(Widget, WidgetFields(WidgetFields))
import Control.Arrow(first)
import Data.List.Split(splitOn)

type Cursor = Int

data Model = Model {
  textEditCursor :: Cursor,
  textEditText :: String
  }

splitLines :: String -> [String]
splitLines = splitOn "\n"

unsplitLines :: [String] -> String
unsplitLines = intercalate "\n"

inSplitLines :: ([String] -> [String]) -> String -> String
inSplitLines f = unsplitLines . f . splitLines

textEdit :: Vty.Attr -> Widget Model
textEdit attr (Model cursor text) =
  WidgetFields (TermImage.string attr (inSplitLines (map (++ " ")) $ text))
               (Just (Vector2 cursorX cursorY))
               keymap
  where
    (before, after) = splitAt cursor text

    width = length text

    -- ls = splitLines text
    -- height = length ls

    linesBefore = splitLines before
    linesAfter = splitLines after
    curLineBefore = last linesBefore
    curLineAfter = head linesAfter
    cursorX = length curLineBefore
    cursorY = length linesBefore - 1

    moveAbsolute a = (a, text)
    moveRelative d = moveAbsolute (cursor + d)
    homeKeys = [([], Vty.KHome), ([Vty.MCtrl], Vty.KASCII 'a')]
    endKeys = [([], Vty.KEnd), ([Vty.MCtrl], Vty.KASCII 'e')]
    homeKeymap doc p model' =
      [ Keymap.singletonKeys "Home" doc [(mk, model') | mk <- homeKeys]
      | p ]
    endKeymap doc p model' =
      [ Keymap.singletonKeys "End" doc [(mk, model') | mk <- endKeys]
      | p ]

    keymap =
      fmap (uncurry Model . first fromIntegral) . mconcat . concat $ [
        [ Keymap.singleton "Left" "Move left" ([], Vty.KLeft) $ moveRelative (-1)
        | cursor > 0 ],
        [ Keymap.singleton "Right" "Move right" ([], Vty.KRight) $ moveRelative 1
        | cursor < width ],

        homeKeymap "Move to beginning of line" (cursorX > 0) $ moveRelative (-cursorX),
        endKeymap "Move to end of line" (length curLineAfter > 0) $ moveRelative (length curLineAfter),

        homeKeymap "Move to beginning of text" (cursorX == 0 && cursor > 0) $ moveAbsolute 0,
        endKeymap "Move to end of text" (null curLineAfter && cursor < width) $ moveAbsolute width,

        [ Keymap.singleton "Backspace" "Delete backwards" ([], Vty.KBS)
          (cursor-1, take (cursor-1) text ++ drop cursor text)
        | cursor > 0 ],
        [ Keymap.singleton "Delete" "Delete forward" ([], Vty.KDel)
          (cursor, before ++ drop 1 after)
        | cursor < width ],
        [ Keymap.fromGroups [ ("Alphabet", ("Insert", Map.fromList insertKeys)) ] ]
        ]
    insert l = (cursor + length l, concat [before, l, after])
    insertKeys =
      [ (([], Vty.KASCII l), insert [l])
      | x <- [0x20..0x7F]
      , let l = chr x ] ++
      [ (([], Vty.KEnter), insert "\n") ]
