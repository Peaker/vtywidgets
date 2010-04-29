{-# OPTIONS -Wall -O2 #-}

module Data.Function.Utils(result, argument) where

result :: (b -> c) -> (a -> b) -> a -> c
result = (.)

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)
