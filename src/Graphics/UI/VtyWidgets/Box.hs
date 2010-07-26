{-# OPTIONS -O2 -Wall #-}
-- {-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.Box
    (makeView, make, makeAcc, makeSizes,
     Model(..), inModel, initModel)
where

import Data.Vector.Vector2(Vector2(..))
import qualified Graphics.UI.VtyWidgets.Grid as Grid
import Graphics.UI.VtyWidgets.SizeRange(SizeRange(..), Size)

newtype Model = Model {
  modelCursor :: Int
  }
  deriving (Show, Read, Eq, Ord, Binary)
inModel :: (Int -> Int) -> Model -> Model
inModel f (Model cursor) = Model (f cursor)

initModel :: Model
initModel = Model 0

data Orientation = Horizontal | Vertical

packForGrid :: Orientation -> [a] -> [[a]]
packForGrid Horizontal = (: [])
packForGrid Vertical = map (: [])

toGrid :: Orientation -> Model -> Grid.Model
toGrid Horizontal = Grid.Model . (`Vector2` 0) . modelCursor
toGrid Vertical = Grid.Model . (0 `Vector2`) . modelCursor

fromGrid :: Orientation -> Grid.Model -> Model
fromGrid Horizontal (Grid.Model (Vector2 x 0)) = Model x
fromGrid Horizontal (Grid.Model (Vector2 _ _)) = error "Grid model of horizontal box with non-zero y in cursor"
fromGrid Vertical (Grid.Model (Vector2 0 y)) = Model y
fromGrid Vertical (Grid.Model (Vector2 _ _)) = error "Grid model of vertical box with non-zero x in cursor"

makeSizes :: Orientation -> [SizeRange] -> (SizeRange, Size -> [Size])
makeSizes o rows = Grid.makeSizes (packForGrid o rows)
