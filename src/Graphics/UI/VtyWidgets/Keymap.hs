{-# OPTIONS -Wall -O2 #-}

module Graphics.UI.VtyWidgets.Keymap
    (Keymap(keymapGroups),
     Doc, KeyGroupName, ModKey, showModKey,
     overlap, lookup,
     make, fromGroups, fromGroupLists,
     singleton, simpleton,
     KeyGroup, simpletonGroup, fromGroup,
     removeKey, removeKeys,
     removeGroup, removeGroups)
where

import qualified Graphics.Vty  as Vty
import           Prelude       hiding (lookup)
import           Control.Arrow (second)
import           Data.Monoid   (Monoid(..))
import qualified Data.Map      as Map
import           Data.Map      (Map)

type KeyGroupName = String
type ModKey = ([Vty.Modifier], Vty.Key)
type Doc = String

type KeyGroup = (KeyGroupName, [ModKey])

simpletonGroup :: ModKey -> KeyGroup
simpletonGroup key = (showModKey key, [key])

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
  strong `mappend` weak = make $ keymapGroups strong `mappend` unshadowedWeakGroups
    where
      unshadowedWeakGroups = Map.filter (unshadowed . snd) $ keymapGroups weak
      unshadowed modKeys = all (`Map.notMember` keymapCache strong) . Map.keys $ modKeys

-- | Combine keymaps similarly to mappend, but instead of fully
-- | removing overlapped groups, only removes the specifically
-- | overlapped keys from those groups
overlap :: Keymap a -> Keymap a -> Keymap a
overlap strong weak = make $ keymapGroups strong `mappend` cleanGroups (keymapGroups weak)
  where
    cleanGroups =
      Map.filter (not . Map.null . snd) .
      (Map.map . second . filterKeys) (`Map.notMember` keymapCache strong)

filterKeys :: Ord k => (k -> Bool) -> Map k a -> Map k a
filterKeys f = Map.filterWithKey (const . f)

removeKey :: ModKey -> Keymap a -> Keymap a
removeKey key keymap =
  case mbGroupName of
    Nothing -> keymap
    Just groupName -> make . Map.alter alterGroup groupName . keymapGroups $ keymap
  where
    alterGroup Nothing = error "Keymap's cache incompatible with groups"
    alterGroup (Just (doc, modKeys)) = (,) doc `fmap` removeOrNothing
      where
        removeOrNothing = if Map.null removed
                          then Nothing
                          else Just removed
        removed = key `Map.delete` modKeys
    mbGroupName = fmap fst $ key `Map.lookup` keymapCache keymap

compose :: [a -> a] -> a -> a
compose = foldr (.) id

removeKeys :: [ModKey] -> Keymap a -> Keymap a
removeKeys = compose . map removeKey

removeGroup :: KeyGroup -> Keymap a -> Keymap a
removeGroup = removeKeys . snd

removeGroups :: [KeyGroup] -> Keymap a -> Keymap a
removeGroups = compose . map removeGroup

lookup :: ModKey -> Keymap a -> Maybe (KeyGroupName, (Doc, a))
lookup modkey = Map.lookup modkey . keymapCache

make :: Map KeyGroupName (Doc, Map ModKey a) -> Keymap a
make handlers = Keymap handlers mkCache
  where
    mkCache = mconcat . Map.elems $ Map.mapWithKey putIntoMap handlers
    putIntoMap kgn (doc, modKeyToA) =
      Map.map (\x -> (kgn, (doc, x))) modKeyToA

fromGroupLists :: [(KeyGroupName, (Doc, [(ModKey, a)]))] -> Keymap a
fromGroupLists = fromGroups . (map . second . second) Map.fromList

fromGroups :: [(KeyGroupName, (Doc, Map ModKey a))] -> Keymap a
fromGroups = make . Map.fromList

fromGroup :: KeyGroup -> Doc -> a -> Keymap a
fromGroup (keyGroupName, keys) doc a =
  make . Map.singleton keyGroupName $ (doc, Map.fromList . map (flip (,) a) $ keys)

singleton :: KeyGroupName -> Doc -> ModKey -> a -> Keymap a
singleton keyGroupName doc key a =
  fromGroup (keyGroupName, [key]) doc a

simpleton :: Doc -> ModKey -> a -> Keymap a
simpleton doc key = singleton (showModKey key) doc key

showKey :: Vty.Key -> String
showKey key = case key of
  Vty.KEsc        -> "Escape"
  Vty.KFun n      -> 'F' : show n
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
    modsStr = map (drop 1 . show) mods
