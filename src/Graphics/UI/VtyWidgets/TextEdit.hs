{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TextEdit
    (make, Model(..), initModel)
where

import Data.Char(chr)
import Data.Monoid(mconcat)
import qualified Data.Map as Map
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Widget(Widget(..))
import qualified Graphics.UI.VtyWidgets.Widget as Widget
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

-- Note: maxLines prevents the *user* from exceeding it, not the given
-- text...
make :: Int -> Vty.Attr -> Vty.Attr -> Model -> Widget Model
make maxLines unfocusedAttr focusedAttr (Model cursor text) =
  Widget.make requestedSize mkImage keymap
  where
    attr True = focusedAttr
    attr False = unfocusedAttr
    requestedSize = Widget.fixedSize (Vector2 width height)
    mkImage (Widget.HasFocus hf) _givenSize =
      (TermImage.setCursor . Just) (Vector2 cursorX cursorY) .
      TermImage.string (attr hf) $
      text

    (before, after) = splitAt cursor text
    textLength = length text
    textLines = splitLines text
    width = maximum . map length $ textLines
    height = length textLines

    linesBefore = reverse (splitLines before)
    linesAfter = splitLines after
    prevLine = linesBefore !! 1
    nextLine = linesAfter !! 1
    curLineBefore = head linesBefore
    curLineAfter = head linesAfter
    cursorX = length curLineBefore
    cursorY = length linesBefore - 1

    ifList p x = [ x | p ]

    moveAbsolute a = (max 0 . min (length text) $ a, text)
    moveRelative d = moveAbsolute (cursor + d)
    backspace = (cursor-1, take (cursor-1) text ++ drop cursor text)
    delete n = (cursor, before ++ drop n after)

    homeKeys = [([], Vty.KHome), ([Vty.MCtrl], Vty.KASCII 'a')]
    endKeys = [([], Vty.KEnd), ([Vty.MCtrl], Vty.KASCII 'e')]
    homeKeymap doc k' = Keymap.singletonKeys "Home" doc [(mk, k') | mk <- homeKeys]
    endKeymap doc k' = Keymap.singletonKeys "End" doc [(mk, k') | mk <- endKeys]

    keymap =
      fmap (uncurry Model . first fromIntegral) . mconcat . concat $ [
        ifList (cursor > 0) .
        Keymap.singleton "Left" "Move left" ([], Vty.KLeft) $
        moveRelative (-1),

        ifList (cursor < textLength) .
        Keymap.singleton "Right" "Move right" ([], Vty.KRight) $
        moveRelative 1,

        ifList (cursorY > 0) .
        Keymap.singleton "Up" "Move up" ([], Vty.KUp) $
        moveRelative (- cursorX - 1 - length (drop cursorX prevLine)),

        ifList (cursorY < height-1) .
        Keymap.singleton "Down" "Move down" ([], Vty.KDown) $
        moveRelative (length curLineAfter + 1 + min cursorX (length nextLine)),

        ifList (cursorX > 0) .
        homeKeymap "Move to beginning of line" $
        moveRelative (-cursorX),

        ifList (not . null $ curLineAfter) .
        endKeymap "Move to end of line" $
        moveRelative (length curLineAfter),

        ifList (cursorX == 0 && cursor > 0) .
        homeKeymap "Move to beginning of text" $
        moveAbsolute 0,

        ifList (null curLineAfter && cursor < textLength) .
        endKeymap "Move to end of text" $
        moveAbsolute textLength,

        ifList (cursor > 0) .
        Keymap.singleton "Backspace" "Delete backwards" ([], Vty.KBS) $
        backspace,

        ifList (cursor < textLength) .
        Keymap.singleton "Delete" "Delete forward" ([], Vty.KDel) $
        delete 1,

        ifList (not . null $ curLineAfter) .
        Keymap.singleton "Ctrl+K" "Delete rest of line" ([Vty.MCtrl], Vty.KASCII 'k') $
        delete (length curLineAfter),

        [ Keymap.fromGroups [ ("Alphabet", ("Insert", Map.fromList insertKeys)) ] ]

        ]
    insert l = if (length . splitLines) text' <= max height maxLines
               then (cursor', text')
               else (cursor, text)
      where
        cursor' = cursor + length l
        text' = concat [before, l, after]
    insertKeys =
      [ (([], Vty.KASCII l), insert [l])
      | x <- [0x20..0x7F]
      , let l = chr x ] ++
      [ (([], Vty.KEnter), insert "\n") ]
