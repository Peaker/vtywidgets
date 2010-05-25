{-# OPTIONS -Wall -O2 #-}

module Data.Monoid.Utils(inFirst) where

import Data.Monoid(First(..))

instance Functor First where
  fmap = inFirst . fmap

inFirst :: (Maybe a -> Maybe b) -> First a -> First b
inFirst f = First . f . getFirst
