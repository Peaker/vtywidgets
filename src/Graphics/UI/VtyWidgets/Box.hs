{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.Box
    (makeView, make, makeAcc, makeSizes,
     Orientation(..), Model(..), initModel)
where

import Prelude hiding ((.))
import Control.Category((.))
import Control.Arrow(second)
import Data.Binary(Binary)
import Data.Function.Utils(result)
import Data.Vector.Vector2(Vector2(..))
import Data.Record.Label((:->), label)
import Graphics.UI.VtyWidgets.Display(Display)
import Graphics.UI.VtyWidgets.Widget(Widget)
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import Graphics.UI.VtyWidgets.SizeRange(SizeRange(..), Size)

newtype Model = Model {
  modelCursor :: Int
  }
  deriving (Show, Read, Eq, Ord, Binary)

initModel :: Model
initModel = Model 0

data Orientation = Horizontal | Vertical

pack :: Orientation -> [a] -> [[a]]
pack Horizontal = (: [])
pack Vertical = map (: [])

extract :: String -> [a] -> a
extract _   [x] = x
extract msg _   = error msg

unpack :: Orientation -> [[a]] -> [a]
unpack Horizontal = extract "Grid row listing of a horizontal box contained more than one row"
unpack Vertical = map . extract $ "Grid row listing of a vertical box contained rows with more than one column"

toGridModel :: Orientation -> Model -> Grid.Model
toGridModel Horizontal = Grid.Model . (`Vector2` 0) . modelCursor
toGridModel Vertical = Grid.Model . (0 `Vector2`) . modelCursor

fromGridModel :: Orientation -> Grid.Model -> Model
fromGridModel Horizontal (Grid.Model (Vector2 x 0)) = Model x
fromGridModel Horizontal (Grid.Model (Vector2 _ _)) = error "Grid model of horizontal box with non-zero y in cursor"
fromGridModel Vertical (Grid.Model (Vector2 0 y)) = Model y
fromGridModel Vertical (Grid.Model (Vector2 _ _)) = error "Grid model of vertical box with non-zero x in cursor"

modelLabel :: Orientation -> Model :-> Grid.Model
modelLabel o = label (toGridModel o) (const . fromGridModel o)

makeSizes :: Orientation -> [SizeRange] -> (SizeRange, Size -> [Size])
makeSizes o = (second . result) (unpack o) . Grid.makeSizes . pack o

makeView :: Orientation -> [Display a] -> Display a
makeView o = Grid.makeView . pack o

make :: Orientation -> (Model -> k) -> [Widget k] -> Model -> Widget k
make o conv rows model = Grid.make (conv . fromGridModel o) (pack o rows) (toGridModel o model)

makeAcc :: Orientation -> k :-> Model -> [Widget k] -> k -> Widget k
makeAcc o acc rows = Grid.makeAcc (modelLabel o . acc) (pack o rows)
