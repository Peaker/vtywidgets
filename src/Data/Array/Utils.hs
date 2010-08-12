{-# OPTIONS -Wall -O2 #-}

module Data.Array.Utils(modifyArrayAt) where

import Control.Monad   (liftM)
import Data.Array      (Ix)
import Data.Array.Base (MArray(..), readArray, writeArray)

modifyArrayAt :: (Monad m, MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArrayAt array index func = writeArray array index =<< liftM func (readArray array index)
