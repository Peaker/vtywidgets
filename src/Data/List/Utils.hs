{-# OPTIONS -Wall -O2 #-}

module Data.List.Utils(safeIndex, groupFst, removeAt) where

import Data.Maybe    (listToMaybe)
import Data.List     (groupBy)
import Data.Function (on)

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

safeIndex :: Integral ix => ix -> [a] -> Maybe a
safeIndex n = listToMaybe . drop (fromIntegral n)

groupFst :: Eq a => [(a, b)] -> [(a, [b])]
groupFst = map extractFst . groupBy ((==) `on` fst)
  where
    extractFst [] = error "groupBy returned an empty group"
    extractFst grp = (fst . head $ grp, map snd grp)
