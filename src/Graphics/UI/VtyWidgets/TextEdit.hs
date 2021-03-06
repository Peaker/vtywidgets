{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TextEdit
    (Model(..), initModel, make, lineEdit,
     Theme(..), standardTheme)
where

import           Data.Binary                      (Binary(..))
import           Data.Char                        (chr, isSpace)
import           Data.Monoid                      (mconcat)
import           Data.List.Split                  (splitOn)
import           Data.Vector.Vector2              (Vector2(..))
import qualified Data.Vector.Vector2              as Vector2
import           Control.Applicative              (pure, liftA2)
import           Control.Arrow                    (first)
import           Control.Monad                    (liftM2)
import qualified Graphics.Vty                     as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import qualified Graphics.UI.VtyWidgets.Keymap    as Keymap
import           Graphics.UI.VtyWidgets.Widget    (Widget)
import qualified Graphics.UI.VtyWidgets.Widget    as Widget
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange

type Cursor = Int

data Model = Model {
  textEditCursor :: Cursor,
  textEditText :: String
  }
  deriving (Show, Read, Eq, Ord)

instance Binary Model where
  get = liftM2 Model get get
  put (Model cursor text) = put cursor >> put text

initModel :: String -> Model
initModel s = Model (length s) s

splitLines :: String -> [String]
splitLines = splitOn "\n"

tillEndOfWord :: String -> String
tillEndOfWord xs = spaces ++ nonSpaces
  where
    spaces = takeWhile isSpace xs
    nonSpaces = takeWhile (not . isSpace) . dropWhile isSpace $ xs

data Theme = Theme {
  themeFocusedAttr :: Vty.Attr,
  themeUnfocusedAttr :: Vty.Attr
  }

-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
make :: Theme -> String -> Int -> Model -> Widget Model
make theme emptyString maxLines (Model cursor text) =
  Widget.fromTuple makeTuple
  where
    makeTuple (Widget.HasFocus hf) = (requestedSize, const (image (mkAttr hf) text, keymap))
    mkAttr True = themeFocusedAttr theme
    mkAttr False = themeUnfocusedAttr theme
    emptyStringSize = if null text
                      then fst . TermImage.stringParse $ emptyString
                      else pure 0
    requestedSize = SizeRange.fixedSize (Vector2.first (+1) $ liftA2 max emptyStringSize (Vector2 width height))
    image attr [] = TermImage.string attr emptyString
    image attr t  = setCursor . TermImage.string attr $ t

    setCursor = TermImage.inCursor . const . Just $ Vector2 cursorX cursorY
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

    -- TODO: Use keyGroup/fromKeyGroup
    multiKey doc keys value =
      mconcat . map (flip (Keymap.simpleton doc) value) $ keys

    homeKeymap doc = multiKey doc homeKeys
    endKeymap doc = multiKey doc endKeys

    keymap =
      Just .
      fmap (uncurry Model . first fromIntegral) . mconcat . concat $ [
        [ multiKey "Move left" (simpleK Vty.KLeft) $
          moveRelative (-1)
        | cursor > 0 ],

        [ multiKey "Move right" (simpleK Vty.KRight) $
          moveRelative 1
        | cursor < textLength ],

        [ multiKey "Move up" (simpleK Vty.KUp) $
          moveRelative (- cursorX - 1 - length (drop cursorX prevLine))
        | cursorY > 0 ],

        [ multiKey "Move down" (simpleK Vty.KDown) $
          moveRelative (length curLineAfter + 1 + min cursorX (length nextLine))
        | cursorY < height-1 ],

        [ homeKeymap "Move to beginning of line" $
          moveRelative (-cursorX)
        | cursorX > 0 ],

        [ endKeymap "Move to end of line" $
          moveRelative (length curLineAfter)
        | not . null $ curLineAfter ],

        [ homeKeymap "Move to beginning of text" $
          moveAbsolute 0
        | cursorX == 0 && cursor > 0 ],

        [ endKeymap "Move to end of text" $
          moveAbsolute textLength
        | null curLineAfter && cursor < textLength ],

        [ multiKey "Delete backwards" (simpleK Vty.KBS ++ ctrlCharK 'h') $
          backDelete 1
        | cursor > 0 ],

        [ multiKey "Delete word backwards" (ctrlCharK 'w')
          backDeleteWord
        | cursor > 0 ],

        let swapPoint = min (textLength - 2) (cursor - 1)
            (beforeSwap, x:y:afterSwap) = splitAt swapPoint text
            swapLetters = (min textLength (cursor + 1),
                           beforeSwap ++ y:x:afterSwap)
        in

        [ multiKey "Swap letters" (ctrlCharK 't')
          swapLetters
        | cursor > 0 && textLength >= 2 ],

        [ multiKey "Delete forward" (simpleK Vty.KDel ++ ctrlCharK 'd') $
          delete 1
        | cursor < textLength ],

        [ multiKey "Delete word forward" (altCharK 'd')
          deleteWord
        | cursor < textLength ],

        [ multiKey "Delete rest of line" (ctrlCharK 'k') $
          delete (length curLineAfter)
        | not . null $ curLineAfter ],

        [ multiKey "Delete newline" (ctrlCharK 'k') $
          delete 1
        | null curLineAfter && cursor < textLength ],

        [ multiKey "Delete till beginning of line" (ctrlCharK 'u') $
          backDelete (length curLineBefore)
        | not . null $ curLineBefore ],

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

lineEdit :: Theme -> String -> Model -> Widget Model
lineEdit theme emptyString = make theme emptyString 1

standardTheme :: Theme
standardTheme = Theme {
  themeFocusedAttr   = Vty.def_attr `Vty.with_fore_color` Vty.yellow `Vty.with_back_color` Vty.blue,
  themeUnfocusedAttr = Vty.def_attr `Vty.with_fore_color` Vty.yellow
  }
