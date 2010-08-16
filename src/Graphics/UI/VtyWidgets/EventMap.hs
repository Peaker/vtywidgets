{-# OPTIONS -Wall -O2 #-}

module Graphics.UI.VtyWidgets.EventMap
    (EventMap(eventMapGroups), Doc,
     GroupName, Group, simpletonGroup,
     mapKeys, overlap, lookup,
     make, fromGroups, fromGroupLists,
     singleton, simpleton,
     fromGroup, fromEventGroups,
     removeEvent, removeEvents,
     removeGroup, removeGroups)
where

import           Prelude       hiding (lookup)
import           Control.Arrow (second)
import           Data.Monoid   (Monoid(..))
import qualified Data.Map      as Map
import           Data.Map      (Map)

type GroupName = String
type Doc = String

type Group e = (GroupName, [e])

data EventMap e a = EventMap {
    eventMapGroups :: Map GroupName (Doc, Map e a)
  , eventMapCache :: Map e (GroupName, (Doc, a))
  }
  deriving (Eq, Ord, Show)

instance Functor (EventMap e) where
  fmap f (EventMap groups cache) =
    EventMap ((fmap . fmap . fmap) f groups)
             ((fmap . fmap . fmap) f cache)

instance Ord e => Monoid (EventMap e a) where
  mempty = make mempty
  strong `mappend` weak =
    make $ eventMapGroups strong `mappend` unshadowedWeakGroups
    where
      unshadowedWeakGroups =
        Map.filter (unshadowed . snd) $ eventMapGroups weak
      unshadowed events =
        all (`Map.notMember` eventMapCache strong) .
        Map.keys $ events

mapKeys :: Ord e' => (e -> e') -> EventMap e a -> EventMap e' a
mapKeys f (EventMap groups cache) =
  EventMap ((Map.map . second . Map.mapKeys) f groups)
           (Map.mapKeys f cache)

-- | Combine eventMaps similarly to mappend, but instead of fully
-- | removing overlapped groups, only removes the specifically
-- | overlapped events from those groups
overlap :: Ord e => EventMap e a -> EventMap e a -> EventMap e a
overlap strong weak =
  make $ eventMapGroups strong `mappend`
         cleanGroups (eventMapGroups weak)
  where
    cleanGroups =
      Map.filter (not . Map.null . snd) .
      (Map.map . second . filterKeys)
      (`Map.notMember` eventMapCache strong)

filterKeys :: Ord k => (k -> Bool) -> Map k a -> Map k a
filterKeys f = Map.filterWithKey (const . f)

removeEvent :: Ord e => e -> EventMap e a -> EventMap e a
removeEvent event eventMap =
  case mbGroupName of
    Nothing -> eventMap
    Just groupName ->
      make . Map.alter alterGroup groupName . eventMapGroups $
      eventMap
  where
    alterGroup Nothing =
      error "EventMap's cache incompatible with groups"
    alterGroup (Just (doc, events)) =
      (,) doc `fmap` removeOrNothing
      where
        removeOrNothing = if Map.null removed
                          then Nothing
                          else Just removed
        removed = event `Map.delete` events
    mbGroupName = fmap fst $
                  event `Map.lookup` eventMapCache eventMap

compose :: [a -> a] -> a -> a
compose = foldr (.) id

removeEvents :: Ord e => [e] -> EventMap e a -> EventMap e a
removeEvents = compose . map removeEvent

removeGroup :: Ord e =>
               Group e -> EventMap e a -> EventMap e a
removeGroup = removeEvents . snd

removeGroups :: Ord e =>
                [Group e] -> EventMap e a -> EventMap e a
removeGroups = compose . map removeGroup

lookup :: Ord e =>
          e -> EventMap e a -> Maybe (GroupName, (Doc, a))
lookup e = Map.lookup e . eventMapCache

make :: Ord e => Map GroupName (Doc, Map e a) -> EventMap e a
make handlers = EventMap handlers mkCache
  where
    mkCache = mconcat . Map.elems $
              Map.mapWithKey putIntoMap handlers
    putIntoMap kgn (doc, eventToA) =
      Map.map (\x -> (kgn, (doc, x))) eventToA

fromGroupLists :: Ord e =>
                  [(GroupName, (Doc, [(e, a)]))] ->
                  EventMap e a
fromGroupLists = fromGroups . (map . second . second) Map.fromList

fromGroups :: Ord e =>
              [(GroupName, (Doc, Map e a))] -> EventMap e a
fromGroups = make . Map.fromList

fromGroup :: Ord e => Group e -> Doc -> a -> EventMap e a
fromGroup (groupName, events) doc a =
  make . Map.singleton groupName $
  (doc, Map.fromList . map (flip (,) a) $ events)

fromEventGroups :: Ord e => [Group e] -> Doc -> a -> EventMap e a
fromEventGroups = mconcat . map fromGroup

singleton :: Ord e =>
             GroupName -> Doc -> e -> a -> EventMap e a
singleton groupName doc event a =
  fromGroup (groupName, [event]) doc a

simpletonGroup :: Show e => e -> Group e
simpletonGroup e = (show e, [e])

simpleton :: (Show e, Ord e) => Doc -> e -> a -> EventMap e a
simpleton doc event = singleton (show event) doc event
