{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Data.Record.Label.List(nth) where

import Data.Record.Label ((:->), lens)

nthSet :: Int -> a -> [a] -> [a]
nthSet _ _ [] = error "IndexError in nthSet"
nthSet 0 x' (_:xs) = x' : xs
nthSet n x' (x:xs) = x : nthSet (n-1) x' xs

nth :: Int -> [a] :-> a
nth n = lens (!! n) (nthSet n)
