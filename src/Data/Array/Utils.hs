{-# OPTIONS -Wall -O2 #-}

module Data.Array.Utils(marrayAt) where

import Control.Monad(liftM)
import Data.Array(Ix)
import Data.Array.Base(MArray(..), readArray, writeArray)

marrayAt :: (Monad m, MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
marrayAt array index func = writeArray array index =<< liftM func (readArray array index)
