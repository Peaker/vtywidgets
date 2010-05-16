{-# OPTIONS -Wall -O2 #-}

module Data.Function.Utils(Endo, Endo2, result, argument, inFlip, (~>)) where

type Endo a = a -> a
type Endo2 a = a -> a -> a

result :: (b -> c) -> (a -> b) -> a -> c
result = (.)

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

infixr 2 ~>
(~>) :: (a' -> a) -> (b -> b') -> (a -> b) -> a' -> b'
arg ~> res = result res . argument arg

inFlip :: ((b' -> a' -> c') -> a -> b -> c) -> (a' -> b' -> c') -> b -> a -> c
inFlip f = flip . f . flip
