{-# LANGUAGE KindSignatures
           , GADTs
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , OverlappingInstances #-}

{-
    List.hs
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


-- | Heterogeneous lists. This has been done many times, in many
-- different ways. Explicit constructors are hidden deliberately.

module Control.Concurrent.Session.Base.List
    ( Nil ()
    , Cons ()
    , TyListLength (..)
    , nil
    , cons
    , modifyCons
    , tyHead
    , tyTail
    , TyList ()
    , TyListIndex (..)
    , TyListUpdateVar (..)
    , TyListTake (..)
    , TyListDrop (..)
    , TyListAppend (..)
    , TyListReverse (..)
    , TyListElem (..)
    , TyListMember (..)
    , TyListConsSet (..)
    , TyListToSet (..)
    , TyListSortNums (..)
    , TyListDelete (..)
    , TySubList (..)
    , TyListToList (..)
    , TyListZip (..)
    ) where

import Control.Concurrent.Session.Base.Number
import Control.Concurrent.Session.Base.Bool

data Nil :: * where
            Nil :: Nil
data Cons :: * -> * -> * where
             Cons :: t -> n -> Cons t n

-- | Find the length of a list.
class TyListLength list length | list -> length where
    tyListLength :: list -> length
instance TyListLength Nil (D0 E) where
    tyListLength Nil = (D0 E)
instance (TyListLength n len, Succ len len') => TyListLength (Cons t n) len' where
    tyListLength (Cons _ nxt) = tySucc . tyListLength $ nxt

instance Show Nil where
    show Nil = "Nil"

instance (TyListLength n l, Succ l l', Show n, Show t) => Show (Cons t n) where
    show (Cons val nxt) = "Cons " ++ (show val) ++ " (" ++ (show nxt) ++ ")"

nil :: Nil
nil = Nil

cons :: (TyList n) => t -> n -> (Cons t n)
cons t n = Cons t n

modifyCons :: (TyList n1, TyList n2) => (t1 -> t2) -> (n1 -> n2) -> (Cons t1 n1) -> (Cons t2 n2)
modifyCons f g (Cons t n) = cons (f t) (g n)

class TyList l
instance TyList Nil
instance (TyList nxt) => TyList (Cons val nxt)

tyHead :: (Cons t n) -> t
tyHead (Cons t _) = t

tyTail :: (Cons t n) -> n
tyTail (Cons _ n) = n

-- | Index or update a list. When updating, the type of the new value
-- must be the same as the type of the old value.
class TyListIndex lst idx res | lst idx -> res where
    tyListIndex :: lst -> idx -> res
    tyListUpdate :: lst -> idx -> res -> lst

instance TyListIndex (Cons res nxt) (D0 E) res where
    tyListIndex (Cons val _) _ = val
    tyListUpdate (Cons _ nxt) _ val = (Cons val nxt)

instance ( TyListIndex nxt idx' res
         , Pred idx idx'
         , SmallerThanBool idx' len True
         , TyListLength nxt len) =>
    TyListIndex (Cons val nxt) idx res where
        tyListIndex (Cons _ nxt) idx = tyListIndex nxt (tyPred idx)
        tyListUpdate (Cons val nxt) idx val'
            = Cons val (tyListUpdate nxt (tyPred idx) val')

-- | Update a list but allow the type of the new value to be different
-- from the type of the old value.
class TyListUpdateVar lst1 idx val lst2 | lst1 idx val -> lst2 where
    tyListUpdateVar :: lst1 -> idx -> val -> lst2

instance ( TyListTake idx lst1 prefix
         , TyListDrop idxP lst1 suffix
         , Succ idx idxP
         , TyListAppend prefix (Cons val suffix) lst2
         ) =>
    TyListUpdateVar lst1 idx val lst2 where
        tyListUpdateVar lst1 idx val
            = tyListAppend prefix (Cons val suffix)
            where
              prefix = tyListTake idx lst1
              idxP = tySucc idx
              suffix = tyListDrop idxP lst1

-- | Append two lists together. Mirrors the "Prelude" function '(++)'.
class TyListAppend a b c | a b -> c where
    tyListAppend :: a -> b -> c

instance TyListAppend Nil b b where
    tyListAppend _ b = b

instance (TyListAppend nxt b nxt') =>
    TyListAppend (Cons val nxt) b (Cons val nxt') where
        tyListAppend (Cons val nxt) b
            = Cons val $ tyListAppend nxt b

-- | Drop from the head of a list. Mirrors the "Prelude" function 'drop'.
class TyListDrop cnt lst res | cnt lst -> res where
    tyListDrop :: cnt -> lst -> res

instance TyListDrop (D0 E) (Cons val nxt) (Cons val nxt) where
    tyListDrop _ lst = lst

instance TyListDrop cnt Nil Nil where
    tyListDrop _ lst = lst

instance ( TyListDrop cnt' nxt lst
         , Pred cnt cnt'
         ) =>
    TyListDrop cnt (Cons val nxt) lst where
        tyListDrop cnt (Cons _ nxt) = tyListDrop (tyPred cnt) nxt

-- | Take from the head of a list. Mirrors the "Prelude" function 'take'.
class TyListTake cnt lst res | cnt lst -> res where
    tyListTake :: cnt -> lst -> res

instance TyListTake (D0 E) Nil Nil where
    tyListTake _ _ = nil

instance TyListTake (D0 E) (Cons val nxt) Nil where
    tyListTake _ _ = nil

instance ( TyListTake cnt' nxt nxt'
         , Pred (D1 r) cnt'
         ) =>
    TyListTake (D1 r) (Cons val nxt) (Cons val nxt') where
        tyListTake cnt (Cons val nxt)
            = Cons val (tyListTake (tyPred cnt) nxt)

instance ( TyListTake cnt' nxt nxt'
         , Pred (D2 r) cnt'
         ) =>
    TyListTake (D2 r) (Cons val nxt) (Cons val nxt') where
        tyListTake cnt (Cons val nxt)
            = Cons val (tyListTake (tyPred cnt) nxt)

instance ( TyListTake cnt' nxt nxt'
         , Pred (D3 r) cnt'
         ) =>
    TyListTake (D3 r) (Cons val nxt) (Cons val nxt') where
        tyListTake cnt (Cons val nxt)
            = Cons val (tyListTake (tyPred cnt) nxt)

instance ( TyListTake cnt' nxt nxt'
         , Pred (D4 r) cnt'
         ) =>
    TyListTake (D4 r) (Cons val nxt) (Cons val nxt') where
        tyListTake cnt (Cons val nxt)
            = Cons val (tyListTake (tyPred cnt) nxt)

instance ( TyListTake cnt' nxt nxt'
         , Pred (D5 r) cnt'
         ) =>
    TyListTake (D5 r) (Cons val nxt) (Cons val nxt') where
        tyListTake cnt (Cons val nxt)
            = Cons val (tyListTake (tyPred cnt) nxt)

instance ( TyListTake cnt' nxt nxt'
         , Pred (D6 r) cnt'
         ) =>
    TyListTake (D6 r) (Cons val nxt) (Cons val nxt') where
        tyListTake cnt (Cons val nxt)
            = Cons val (tyListTake (tyPred cnt) nxt)

instance ( TyListTake cnt' nxt nxt'
         , Pred (D7 r) cnt'
         ) =>
    TyListTake (D7 r) (Cons val nxt) (Cons val nxt') where
        tyListTake cnt (Cons val nxt)
            = Cons val (tyListTake (tyPred cnt) nxt)

instance ( TyListTake cnt' nxt nxt'
         , Pred (D8 r) cnt'
         ) =>
    TyListTake (D8 r) (Cons val nxt) (Cons val nxt') where
        tyListTake cnt (Cons val nxt)
            = Cons val (tyListTake (tyPred cnt) nxt)

instance ( TyListTake cnt' nxt nxt'
         , Pred (D9 r) cnt'
         ) =>
    TyListTake (D9 r) (Cons val nxt) (Cons val nxt') where
        tyListTake cnt (Cons val nxt)
            = Cons val (tyListTake (tyPred cnt) nxt)


class TyListElem lst val idx | lst val -> idx where
    tyListElem :: lst -> val -> idx

instance (TyListElem' lst (D0 E) val idx) =>
    TyListElem lst val idx where
        tyListElem lst val = tyListElem' lst (D0 E) val

class TyListElem' lst acc val idx | lst acc val -> idx where
    tyListElem' :: lst -> acc -> val -> idx

instance TyListElem' (Cons val nxt) idx val idx where
    tyListElem' _ idx _ = idx

instance ( Succ acc acc'
         , TyListElem' nxt acc' val idx
         ) =>
    TyListElem' (Cons val' nxt) acc val idx where
        tyListElem' (Cons _ nxt) acc val = tyListElem' nxt (tySucc acc) val

-- | Reverse a list.
class TyListReverse m n | m -> n where
    tyListReverse :: m -> n
instance (TyListReverse' m Nil n) => TyListReverse m n where
    tyListReverse lst = tyListReverse' lst nil

class TyListReverse' m a n | m a -> n where
    tyListReverse' :: m -> a -> n
instance TyListReverse' Nil acc acc where
    tyListReverse' _ = id
instance (TyListReverse' nxt (Cons v acc) n, TyList acc) =>
    TyListReverse' (Cons v nxt) acc n where
        tyListReverse' (Cons v nxt) acc = tyListReverse' nxt (cons v acc)

class TyListDelete lst idx lst' | lst idx -> lst' where
    tyListDelete :: idx -> lst -> lst'
instance ( TyListTake idx lst prefix
         , Succ idx idxS
         , TyListDrop idxS lst suffix
         , TyListAppend prefix suffix lst'
         ) =>
    TyListDelete lst idx lst' where
        tyListDelete idx lst = tyListAppend prefix suffix
            where
              prefix = tyListTake idx lst
              idxS = tySucc idx
              suffix = tyListDrop idxS lst

class TyListMember lst val res | lst val -> res where
    isTyListMember :: val -> lst -> res
instance TyListMember Nil val False where
    isTyListMember _ _ = FF
instance TyListMember (Cons val nxt) val True where
    isTyListMember _ _ = TT
instance (TyListMember nxt val res) =>
    TyListMember (Cons val' nxt) val res where
        isTyListMember val (Cons _ nxt) = isTyListMember val nxt

class TyListConsSet e set set' | e set -> set' where
    tyListConsSet :: e -> set -> set'
instance (TyListMember set e res, TyListConsSet' res e set set') =>
    TyListConsSet e set set' where
        tyListConsSet elem lst
            = tyListConsSet' (isTyListMember elem lst) elem lst

class TyListConsSet' bool e set set' | bool e set -> set' where
    tyListConsSet' :: bool -> e -> set -> set'
instance (TyList set) => TyListConsSet' False e set (Cons e set) where
    tyListConsSet' _ elem lst = cons elem lst
instance TyListConsSet' True e set set where
    tyListConsSet' _ _ lst = lst

class TyListToSet lst set | lst -> set where
    tyListToSet :: lst -> set
instance TyListToSet Nil Nil where
    tyListToSet = id
instance (TyListToSet nxt set, TyListConsSet v set set') =>
    TyListToSet (Cons v nxt) set' where
        tyListToSet (Cons v nxt) = tyListConsSet v . tyListToSet $ nxt

class TyListSortNums lstA lstB | lstA -> lstB where
    tyListSortNums :: lstA -> lstB
instance TyListSortNums Nil Nil where
    tyListSortNums _ = nil
instance ( TyNum num
         , TyListSortNums nxt lst'
         , Insert num val lst' lst''
         ) =>
    TyListSortNums (Cons (num, val) nxt) lst'' where
        tyListSortNums (Cons (num, val) nxt) = insert num val (tyListSortNums nxt)

class Insert num val lstA lstB | num val lstA -> lstB where
    insert :: num -> val -> lstA -> lstB
instance Insert num val Nil (Cons (num, val) Nil) where
    insert num val _ = cons (num, val) nil
instance ( SmallerThanBool num num' isSmaller
         , Insert' isSmaller num val (Cons (num', val') nxt) lstB
         ) =>
    Insert num val (Cons (num', val') nxt) lstB where
        insert num val lst@(Cons (num', _) _) = insert' isSmaller num val lst
            where
              isSmaller = isSmallerThan num num'

class Insert' isSmaller num val lstA lstB | isSmaller num val lstA -> lstB where
    insert' :: isSmaller -> num -> val -> lstA -> lstB
instance (TyList lstA) =>
    Insert' True num val lstA (Cons (num, val) lstA) where
        insert' TT num val lst = cons (num, val) lst
instance ( Insert num val nxt lstB
         , TyList lstB
         ) =>
    Insert' False num val (Cons (num', val') nxt) (Cons (num', val') lstB) where
        insert' FF num val (Cons (num', val') nxt) = cons (num', val') $ insert num val nxt

class TySubList smaller bigger result | smaller bigger -> result where
    isTySubList :: smaller -> bigger -> result

instance TySubList Nil b True where
    isTySubList _ _ = TT

instance ( TyListMember b a resMember
         , TySubList as b resSubList
         , And resMember resSubList res
         ) =>
    TySubList (Cons a as) b res where
        isTySubList (Cons a as) b = tyAnd (isTyListMember a b) (isTySubList as b)

class TyListToList lst res | lst -> res where
    tyListToList :: lst -> res

instance TyListToList Nil [res] where
    tyListToList _ = []

instance (TyListToList nxt [res]) =>
    TyListToList (Cons res nxt) [res] where
        tyListToList (Cons res nxt) = res : tyListToList nxt

class TyListZip a b c | a b -> c where
    tyListZip :: a -> b -> c

instance TyListZip Nil Nil Nil where
    tyListZip _ _ = nil

instance ( TyListZip as bs cs
         , TyList cs
         ) =>
    TyListZip (Cons a as) (Cons b bs) (Cons (a,b) cs) where
        tyListZip (Cons a as) (Cons b bs) = cons (a, b) $ tyListZip as bs
