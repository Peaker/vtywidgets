{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TextEdit
    (textEdit, Model(..), initModel)
where

import Data.Char(chr)
import Data.List(intercalate)
import Data.Monoid(mconcat)
import qualified Data.Map as Map
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Widget(Widget(..))
import Control.Arrow(first)
import Data.List.Split(splitOn)

type Cursor = Int

data Model = Model {
  textEditCursor :: Cursor,
  textEditText :: String
  }

initModel :: String -> Model
initModel s = Model (length s) s

splitLines :: String -> [String]
splitLines = splitOn "\n"

unsplitLines :: [String] -> String
unsplitLines = intercalate "\n"

inSplitLines :: ([String] -> [String]) -> String -> String
inSplitLines f = unsplitLines . f . splitLines

textEdit :: Vty.Attr -> Model -> Widget Model
textEdit attr (Model cursor text) =
  Widget (TermImage.string attr (inSplitLines (map (++ " ")) $ text))
         (Just $ Vector2 cursorX cursorY)
         keymap
  where
    (before, after) = splitAt cursor text

    width = length text

    height = length (splitLines text)

    linesBefore = reverse (splitLines before)
    linesAfter = splitLines after
    prevLine = linesBefore !! 1
    curLineBefore = head linesBefore
    curLineAfter = head linesAfter
    cursorX = length curLineBefore
    cursorY = length linesBefore - 1

    ifList p x = [ x | p ]

    moveAbsolute a = (max 0 . min (length text) $ a, text)
    moveRelative d = moveAbsolute (cursor + d)
    backspace = (cursor-1, take (cursor-1) text ++ drop cursor text)
    delete = (cursor, before ++ drop 1 after)

    homeKeys = [([], Vty.KHome), ([Vty.MCtrl], Vty.KASCII 'a')]
    endKeys = [([], Vty.KEnd), ([Vty.MCtrl], Vty.KASCII 'e')]
    homeKeymap doc model' = Keymap.singletonKeys "Home" doc [(mk, model') | mk <- homeKeys]
    endKeymap doc model' = Keymap.singletonKeys "End" doc [(mk, model') | mk <- endKeys]

    keymap =
      fmap (uncurry Model . first fromIntegral) . mconcat . concat $ [
        ifList (cursor > 0) .
        Keymap.singleton "Left" "Move left" ([], Vty.KLeft) $
        moveRelative (-1),

        ifList (cursor < width) .
        Keymap.singleton "Right" "Move right" ([], Vty.KRight) $
        moveRelative 1,

        ifList (cursorY > 0) .
        Keymap.singleton "Up" "Move up" ([], Vty.KUp) $
        moveRelative (- cursorX - 1 - length (drop cursorX prevLine)),

        ifList (cursorY < height-1) .
        Keymap.singleton "Down" "Move down" ([], Vty.KDown) $
        moveRelative (length curLineAfter + 1 + cursorX),

        ifList (cursorX > 0) .
        homeKeymap "Move to beginning of line" $
        moveRelative (-cursorX),

        ifList (length curLineAfter > 0) .
        endKeymap "Move to end of line" $
        moveRelative (length curLineAfter),

        ifList (cursorX == 0 && cursor > 0) .
        homeKeymap "Move to beginning of text" $
        moveAbsolute 0,

        ifList (null curLineAfter && cursor < width) .
        endKeymap "Move to end of text" $
        moveAbsolute width,

        ifList (cursor > 0) .
        Keymap.singleton "Backspace" "Delete backwards" ([], Vty.KBS) $
        backspace,

        ifList (cursor < width) .
        Keymap.singleton "Delete" "Delete forward" ([], Vty.KDel) $
        delete,
        
        [ Keymap.fromGroups [ ("Alphabet", ("Insert", Map.fromList insertKeys)) ] ]

        ]
    insert l = (cursor + length l, concat [before, l, after])
    insertKeys =
      [ (([], Vty.KASCII l), insert [l])
      | x <- [0x20..0x7F]
      , let l = chr x ] ++
      [ (([], Vty.KEnter), insert "\n") ]
