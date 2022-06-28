{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , ScopedTypeVariables
           , PatternSignatures #-}

{-
    Map.hs
        Copyright 2008 Matthew Sackman <matthew@wellquite.org>

    This file is part of Session Types for Haskell.

    Session Types for Haskell is free software: you can redistribute it
    and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation, either version 3 of
    the License, or (at your option) any later version.

    Session Types for Haskell is distributed in the hope that it will
    be useful, but WITHOUT ANY WARRANTY; without even the implied
    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Session Types for Haskell.
    If not, see <http://www.gnu.org/licenses/>.
-}

-- | Heterogeneous maps at the type level. Obviously equality is done
-- on types and not values. Duplicate keys are not permitted, as you'd expect.

module Control.Concurrent.Session.Base.Map
    ( TyMap (..)
    , emptyMap
    , MapInsert (..)
    , MapLookup (..)
    , MapUpdate (..)
    , MapSize (..)
    , tyMapKeys
    , MapDelete (..)
    , MapSelectToList (..)
    ) where

import Control.Concurrent.Session.Base.Number
import Control.Concurrent.Session.Base.List
import Control.Concurrent.Session.Base.Bool

data TyMap keyToIdx idxToValue = TM keyToIdx idxToValue
                                 deriving (Show)

emptyMap :: TyMap Nil Nil
emptyMap = TM nil nil

-- | Insert into a map. Remember, the values are irrelevant, it's only
-- the types that matter. Inserting a key that already exists is not
-- permitted.
class MapInsert m1 key val m2 | m1 key val -> m2 where
    mapInsert :: key -> val -> m1 -> m2

instance ( TyListMember keyToIdx key res
         , MapInsert' res (TyMap keyToIdx idxToValue) key val (TyMap keyToIdx' idxToValue')
         , MapDelete (TyMap keyToIdx' idxToValue') key (TyMap keyToIdx idxToValue)
         ) =>
 MapInsert (TyMap keyToIdx idxToValue) key val (TyMap keyToIdx' idxToValue') where
     mapInsert key val m1@(TM keyToIdx _)
         = mapInsert' res key val m1
           where
             res = isTyListMember key keyToIdx

class MapInsert' bool m1 key val m2 | bool m1 key val -> m2 where
    mapInsert' :: bool -> key -> val -> m1 -> m2
{-
instance ( TyListUpdateVar idxToValue idx value idxToValue'
         , TyListElem keyToIdx key idx
         ) =>
    MapInsert' True (TyMap keyToIdx idxToValue) key value (TyMap keyToIdx idxToValue') where
        mapInsert' _ key value (TM keyToIdx idxToValue)
            = TM keyToIdx idxToValue'
              where
                idx = tyListElem keyToIdx key
                idxToValue' = tyListUpdateVar idxToValue idx value
-}

instance ( TyListLength keyToIdx newIdx
         , TyListReverse keyToIdx keyToIdxRev
         , TyListReverse (Cons key keyToIdxRev) keyToIdx'
         , TyListUpdateVar idxToValue newIdx value idxToValue'
         , TyList keyToIdxRev
         , MapDelete (TyMap keyToIdx' idxToValue') key (TyMap keyToIdx idxToValue)
         ) =>
    MapInsert' False (TyMap keyToIdx idxToValue) key value (TyMap keyToIdx' idxToValue') where
        mapInsert' _ key value (TM keyToIdx idxToValue)
            = TM keyToIdx' idxToValue'
              where
                keyToIdx' = tyListReverse . cons key . tyListReverse $ keyToIdx
                idxToValue' = tyListUpdateVar idxToValue (tyListLength keyToIdx) value

{-
instance ( TyListReverse keyToIdx keyToIdxRev
         , TyListReverse idxToValue idxToValueRev
         , TyListReverse (Cons key keyToIdxRev) keyToIdx'
         , TyListReverse (Cons val idxToValueRev) idxToValue'
         , TyList keyToIdxRev
         , TyList idxToValueRev
         , MapDelete (TyMap keyToIdx' idxToValue') key (TyMap keyToIdx idxToValue)
         ) =>
    MapInsert (TyMap keyToIdx idxToValue) key val (TyMap keyToIdx' idxToValue') where
    mapInsert = undefined
-}
-- | lookup in a map. Will call fail in Monad if it's not there.
class MapLookup mp key val | mp key -> val where
    mapLookup :: mp -> key -> val
    mapUpdateValue :: mp -> key -> val -> mp

instance ( TyListElem keyToIdx key idx
         , TyListIndex idxToValue idx val
         ) =>
    MapLookup (TyMap keyToIdx idxToValue) key val where
        mapLookup (TM keyToIdx idxToValue) key
            = tyListIndex idxToValue $ tyListElem keyToIdx key
        mapUpdateValue (TM keyToIdx idxToValue) key val
            = TM keyToIdx . tyListUpdate idxToValue (tyListElem keyToIdx key) $ val

-- | Update a map. The key must already be in the map. The value is
-- the type of the value, if you see what I mean and so obviously,
-- updating the map means changing the type of the value.
class MapUpdate mp key val' mp' | mp key val' -> mp' where
    mapUpdate :: mp -> key -> val' -> mp'

instance ( TyListUpdateVar idxToValue idx val' idxToValue'
         , TyListElem keyToIdx key idx
         , MapLookup (TyMap keyToIdx idxToValue') key val'
         ) => 
    MapUpdate (TyMap keyToIdx idxToValue) key val' (TyMap keyToIdx idxToValue') where
        mapUpdate (TM keyToIdx idxToValue) key val'
            = TM keyToIdx . tyListUpdateVar idxToValue (tyListElem keyToIdx key) $ val'

-- | Find the size of a map.
class MapSize mp size | mp -> size where
    mapSize :: mp -> size

instance (TyListLength keyToIdx len) =>
    MapSize (TyMap keyToIdx idxToValue) len where
        mapSize (TM keyToIdx _) = tyListLength keyToIdx

tyMapKeys :: TyMap keyToIdx idxToValue -> keyToIdx
tyMapKeys (TM keyToIdx _) = keyToIdx

class MapDelete mp key mp' | mp key -> mp' where
    mapDelete :: mp -> key -> mp'

instance ( TyListElem keyToIdx key idx
         , TyListTake idx keyToIdx keyToIdxPrefix
         , TyListTake idx idxToValue idxToValuePrefix
         , TyListDrop idxP keyToIdx keyToIdxSuffix
         , TyListDrop idxP idxToValue idxToValueSuffix
         , Succ idx idxP
         , Pred idxP idx
         , TyListAppend keyToIdxPrefix keyToIdxSuffix keyToIdx'
         , TyListAppend idxToValuePrefix idxToValueSuffix idxToValue'
         ) =>
    MapDelete (TyMap keyToIdx idxToValue) key (TyMap keyToIdx' idxToValue') where
        mapDelete (TM keyToIdx idxToValue) key
            = TM (tyListAppend keyToIdxPrefix keyToIdxSuffix) (tyListAppend idxToValuePrefix idxToValueSuffix)
            where
              idx = tyListElem keyToIdx key
              idxP = tySucc idx
              keyToIdxPrefix = tyListTake idx keyToIdx
              keyToIdxSuffix = tyListDrop idxP keyToIdx
              idxToValuePrefix = tyListTake idx idxToValue
              idxToValueSuffix = tyListDrop idxP idxToValue

class MapSelectToList mp keys lst | mp keys -> lst where
    mapSelectToList :: mp -> keys -> lst

instance MapSelectToList mp Nil Nil where
    mapSelectToList _ _ = nil

instance ( MapSelectToList mp keys vals
         , MapLookup mp key val
         , TyList vals
         ) =>
    MapSelectToList mp (Cons key keys) (Cons val vals) where
        mapSelectToList mp keys = cons val $ mapSelectToList mp keys'
            where
              val = mapLookup mp key
              key = tyHead keys
              keys' = tyTail keys
