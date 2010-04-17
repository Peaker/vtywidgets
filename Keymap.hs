{-# OPTIONS -Wall -O2 #-}

module Keymap
    (Keymap(keymapGroups),
     Doc, KeyGroupName, ModKey, showModKey,
     lookup, make, fromGroups, singleton, singletonKeys, simpleton)
where

import qualified Graphics.Vty as Vty
import Prelude hiding (lookup)
import Data.Monoid(Monoid(..))
import qualified Data.Map as Map
import Data.Map(Map)

type KeyGroupName = String
type ModKey = ([Vty.Modifier], Vty.Key)
type Doc = String

data Keymap a = Keymap {
    keymapGroups :: Map KeyGroupName (Doc, Map ModKey a)
  , keymapCache :: Map ModKey (KeyGroupName, (Doc, a))
  }

instance Functor Keymap where
  fmap f (Keymap groups cache) =
    Keymap ((fmap . fmap . fmap) f groups)
           ((fmap . fmap . fmap) f cache)

instance Monoid (Keymap a) where
  mempty = make mempty
  x `mappend` y = make $ keymapGroups x `mappend` keymapGroups y

lookup :: ModKey -> Keymap a -> Maybe (KeyGroupName, (Doc, a))
lookup modkey = Map.lookup modkey . keymapCache

make :: Map KeyGroupName (Doc, Map ModKey a) -> Keymap a
make handlers = Keymap handlers mkCache
  where
    mkCache = mconcat . Map.elems $ Map.mapWithKey putIntoMap handlers
    putIntoMap kgn (doc, modKeyToA) =
      Map.map (\x -> (kgn, (doc, x))) modKeyToA

fromGroups :: [(KeyGroupName, (Doc, Map ModKey a))] -> Keymap a
fromGroups = make . Map.fromList

singletonKeys :: KeyGroupName -> Doc -> [(ModKey, a)] -> Keymap a
singletonKeys keyGroupName doc keys =
  make . Map.singleton keyGroupName $ (doc, Map.fromList keys)

singleton :: KeyGroupName -> Doc -> ModKey -> a -> Keymap a
singleton keyGroupName doc key a =
  singletonKeys keyGroupName doc [(key, a)]

simpleton :: ModKey -> Doc -> a -> Keymap a
simpleton key doc = singleton (showModKey key) doc key

showKey :: Vty.Key -> String
showKey key = case key of
  Vty.KEsc        -> "Escape"
  Vty.KFun n      -> "F" ++ show n
  Vty.KBackTab    -> "BackTab"
  Vty.KPrtScr     -> "PrintScreen"
  Vty.KPause      -> "Pause"
  Vty.KASCII ' '  -> "Space"
  Vty.KASCII '\t' -> "Tab"
  Vty.KASCII x    -> [x]
  Vty.KBS         -> "Backspace"
  Vty.KIns        -> "Insert"
  Vty.KHome       -> "Home"
  Vty.KPageUp     -> "PageUp"
  Vty.KDel        -> "Delete"
  Vty.KEnd        -> "End"
  Vty.KPageDown   -> "PageDown"
  Vty.KNP5        -> "NP5"
  Vty.KUp         -> "Up"
  Vty.KMenu       -> "Menu"
  Vty.KLeft       -> "Left"
  Vty.KDown       -> "Down"
  Vty.KRight      -> "Right"
  Vty.KEnter      -> "Enter"

showModKey :: ModKey -> String
showModKey (mods, key) = unwords (modsStr ++ [showKey key])
  where
    modsStr = map (drop 1 . show) $ mods
