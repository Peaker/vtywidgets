{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.TMap
    (TMap, override, lookup, mapKeys)
where

import Prelude hiding (lookup)
import Control.Applicative(Applicative(..))
import Data.Monoid(Monoid(..))
import Data.Map(Map)
import Data.Maybe(fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map

data TMap k v = TMap {
  tmapDefaultVal :: v,
  tmapMap :: Map k v
  }
  deriving (Show, Read)

atMap :: (Map k v -> Map k' v) ->
         TMap k v -> TMap k' v
atMap f t = t{tmapMap = f . tmapMap $ t}

instance Ord k => Functor (TMap k) where
  fmap f (TMap dv m) = TMap (f dv) (fmap f m)

instance Ord k => Applicative (TMap k) where
  pure d = TMap { tmapDefaultVal = d, tmapMap = mempty }
  TMap fdv fm <*> TMap xdv xm =
    TMap dv (Map.fromList (map makeItem allKeys))
      where
        makeItem k = (k, (fromMaybe fdv . Map.lookup k $ fm)
                         (fromMaybe xdv . Map.lookup k $ xm))
        allKeys = Set.toList $ Map.keysSet fm `mappend` Map.keysSet xm
        dv = fdv xdv

instance (Ord k, Monoid v) => Monoid (TMap k v) where
  mempty = TMap mempty mempty
  TMap adv am `mappend` TMap bdv bm = TMap (adv `mappend` bdv) (Map.unionWith mappend am bm)

override :: Ord k => k -> v -> TMap k v -> TMap k v
override k v = atMap (Map.insert k v)

lookup :: Ord k => k -> TMap k v -> v
lookup k (TMap dv m) = fromMaybe dv . Map.lookup k $ m

mapKeys :: (Ord k, Ord k') => (k -> k') -> TMap k v -> TMap k' v
mapKeys = atMap . Map.mapKeys
