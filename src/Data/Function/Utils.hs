{-# OPTIONS -Wall -O2 #-}

module Data.Function.Utils(result, argument, inFlip) where

result :: (b -> c) -> (a -> b) -> a -> c
result = (.)

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

inFlip :: ((b' -> a' -> c') -> a -> b -> c) -> (a' -> b' -> c') -> b -> a -> c
inFlip f = flip . f . flip
