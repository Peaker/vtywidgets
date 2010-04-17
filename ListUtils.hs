{-# OPTIONS -Wall -O2 #-}

module ListUtils(safeIndex) where

import Data.Maybe(listToMaybe)

safeIndex :: Integral ix => ix -> [a] -> Maybe a
safeIndex n = listToMaybe . drop (fromIntegral n)
