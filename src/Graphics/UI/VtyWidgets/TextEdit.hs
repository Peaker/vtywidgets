{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TextEdit
    (make, Model(..), initModel)
where

import Data.Char(chr)
import Data.Monoid(mconcat)
import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap
import Graphics.UI.VtyWidgets.Widget(Widget(..))
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Control.Arrow(first)
import Data.List.Split(splitOn)
import Data.Char(isSpace)

type Cursor = Int

data Model = Model {
  textEditCursor :: Cursor,
  textEditText :: String
  }

initModel :: String -> Model
initModel s = Model (length s) s

splitLines :: String -> [String]
splitLines = splitOn "\n"

tillEndOfWord :: String -> String
tillEndOfWord xs = spaces ++ nonSpaces
  where
    spaces = takeWhile isSpace xs
    nonSpaces = takeWhile (not . isSpace) . dropWhile isSpace $ xs

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
    backDelete n = (cursor-n, take (cursor-n) text ++ drop cursor text)
    delete n = (cursor, before ++ drop n after)

    backDeleteWord = backDelete . length . tillEndOfWord . reverse $ before
    deleteWord = delete . length . tillEndOfWord $ after

    ctrlCharK k = [([Vty.MCtrl], Vty.KASCII k)]
    altCharK k = [ ([m], Vty.KASCII k) | m <- [Vty.MAlt, Vty.MMeta] ]
    simpleK k = [([], k)]
    charK k = simpleK (Vty.KASCII k)

    homeKeys = simpleK Vty.KHome ++ ctrlCharK 'a'
    endKeys = simpleK Vty.KEnd ++ ctrlCharK 'e'

    multiKey doc keys value =
      mconcat . map (flip (Keymap.simpleton doc) value) $ keys

    homeKeymap doc = multiKey doc homeKeys
    endKeymap doc = multiKey doc endKeys

    keymap =
      fmap (uncurry Model . first fromIntegral) . mconcat . concat $ [
        ifList (cursor > 0) .
        multiKey "Move left" (simpleK Vty.KLeft) $
        moveRelative (-1),

        ifList (cursor < textLength) .
        multiKey "Move right" (simpleK Vty.KRight) $
        moveRelative 1,

        ifList (cursorY > 0) .
        multiKey "Move up" (simpleK Vty.KUp) $
        moveRelative (- cursorX - 1 - length (drop cursorX prevLine)),

        ifList (cursorY < height-1) .
        multiKey "Move down" (simpleK Vty.KDown) $
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
        multiKey "Delete backwards" (simpleK Vty.KBS ++ ctrlCharK 'h') $
        backDelete 1,

        ifList (cursor > 0) .
        multiKey "Delete word backwards" (ctrlCharK 'w') $
        backDeleteWord,

        ifList (cursor < textLength) .
        multiKey "Delete forward" (simpleK Vty.KDel ++ ctrlCharK 'd') $
        delete 1,

        ifList (cursor < textLength) .
        multiKey "Delete word forward" (altCharK 'd') $
        deleteWord,

        ifList (not . null $ curLineAfter) .
        multiKey "Delete rest of line" (ctrlCharK 'k') $
        delete (length curLineAfter),

        [ Keymap.fromGroupLists [ ("Alphabet", ("Insert", insertKeys)) ] ]

        ]
    insert l = if (length . splitLines) text' <= max height maxLines
               then (cursor', text')
               else (cursor, text)
      where
        cursor' = cursor + length l
        text' = concat [before, l, after]
    insertKeys =
      [ (k, insert [l])
      | x <- [0x20..0x7F]
      , let l = chr x
      , k <- charK l ]
      ++
      [ (k, insert "\n")
      | k <- simpleK Vty.KEnter ]
