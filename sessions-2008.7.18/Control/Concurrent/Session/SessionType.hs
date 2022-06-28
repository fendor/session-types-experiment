{-# LANGUAGE KindSignatures
           , GADTs
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , OverlappingInstances
           , FlexibleContexts
           , TypeFamilies #-}

{-
    SessionType.hs
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

-- | This module is concerned with allowing you to describe a session
-- type.  A session type is treated as a table or 2D array, where each
-- row represents a particular session type function which can refer,
-- by index, to the other rows.
--
-- Basically, what you have here is the ability to describe a
-- program at the type level.
--
-- Just look at "Control.Concurrent.Session.Tests" for examples

module Control.Concurrent.Session.SessionType where

import Control.Concurrent.Session.Base.List
import Control.Concurrent.Session.Base.Bool
import Control.Concurrent.Session.Base.Number

data End = End deriving Show

end :: Cons End Nil
end = cons End nil

sendPid :: ( TyListSortNums lst lst'
           ) =>
    lst -> SendPid False lst'
sendPid = SendPid FF . tyListSortNums

recvPid :: ( TyListSortNums lst lst'
           ) =>
    lst -> RecvPid False lst'
recvPid = RecvPid FF . tyListSortNums

data SendPid inverted lst = SendPid inverted lst
                            deriving (Show)

data RecvPid inverted lst = RecvPid inverted lst
                            deriving (Show)

sendSession :: idx -> SendSession False idx
sendSession = SendSession FF

recvSession :: idx -> RecvSession False idx
recvSession = RecvSession FF

data SendSession inverted idx
    = SendSession inverted idx
      deriving (Show)

data RecvSession inverted idx
    = RecvSession inverted idx
      deriving (Show)

data Send t = Send t
data Recv t = Recv t
data Jump l = Jump l
            deriving (Show)

jump :: (TyNum n) => n -> Cons (Jump n) Nil
jump l = cons (Jump l) nil

data Select :: * -> * where
               Select :: lstOfLabels -> Select lstOfLabels

select :: (SListOfJumps (Cons val nxt)) => (Cons val nxt) -> Cons (Select (Cons val nxt)) Nil
select lol = cons (Select lol) nil

data Offer :: * -> * where
              Offer :: lstOfLabels -> Offer lstOfLabels

offer :: (SListOfJumps (Cons val nxt)) => (Cons val nxt) -> Cons (Offer (Cons val nxt)) Nil
offer lol = cons (Offer lol) nil

class Dual a b | a -> b, b -> a where
    type DualT a
    dual :: a -> b
instance Dual End End where
    type DualT End = End
    dual End = End
instance Dual (Jump l) (Jump l) where
    type DualT (Jump l) = (Jump l)
    dual (Jump l) = Jump l
instance Dual (Send t) (Recv t) where
    type DualT (Send t) = (Recv t)
    dual (Send t) = Recv t
instance Dual (Recv t) (Send t) where
    type DualT (Recv t) = (Send t)
    dual (Recv t) = Send t
instance (Not inverted inverted') => Dual (SendPid inverted lst) (RecvPid inverted' lst) where
    type DualT (SendPid inverted lst) = (RecvPid (NotT inverted) lst)
    dual (SendPid inverted lst) = RecvPid (tyNot inverted) lst
instance (Not inverted inverted') => Dual (RecvPid inverted lst) (SendPid inverted' lst) where
    type DualT (RecvPid inverted lst) = (SendPid (NotT inverted) lst)
    dual (RecvPid inverted lst) = SendPid (tyNot inverted) lst
instance Dual (Select lst) (Offer lst) where
    type DualT (Select lst) = (Offer lst)
    dual (Select lst) = Offer lst
instance Dual (Offer lst) (Select lst) where
    type DualT (Offer lst) = (Select lst)
    dual (Offer lst) = Select lst
instance (Not inverted inverted') => Dual (SendSession inverted idx) (RecvSession inverted' idx) where
    type DualT (SendSession inverted idx) = RecvSession (NotT inverted) idx
    dual (SendSession inverted idx) = RecvSession (tyNot inverted) idx
instance (Not inverted inverted') => Dual (RecvSession inverted idx) (SendSession inverted' idx) where
    type DualT (RecvSession inverted idx) = SendSession (NotT inverted) idx
    dual (RecvSession inverted idx) = SendSession (tyNot inverted) idx

instance Dual Nil Nil where
    type DualT Nil = Nil
    dual = id
instance (TyList nxt, TyList nxt', Dual val val', Dual nxt nxt') =>
    Dual (Cons val nxt) (Cons val' nxt') where
        type DualT (Cons val nxt) = (Cons (DualT val) (DualT nxt))
        dual = modifyCons dual dual

class SListOfJumps lst
instance SListOfJumps Nil
instance (SListOfJumps nxt, TyNum val) => SListOfJumps (Cons (Cons (Jump val) Nil) nxt)

class SListOfSessionTypes lstOfLists
instance SListOfSessionTypes Nil
instance (SValidSessionType val, SListOfSessionTypes nxt) => SListOfSessionTypes (Cons val nxt)

class SNonTerminal a
instance SNonTerminal (Send t)
instance SNonTerminal (Recv t)
instance SNonTerminal (SendPid inverted t)
instance SNonTerminal (RecvPid inverted t)
instance (SValidSessionType idx) => SNonTerminal (SendSession inverted idx)
instance (SValidSessionType idx) => SNonTerminal (RecvSession inverted idx)

class STerminal a
instance STerminal End
instance (TyNum l) => STerminal (Jump l)
instance (SListOfJumps (Cons val nxt)) => STerminal (Select (Cons val nxt))
instance (SListOfJumps (Cons val nxt)) => STerminal (Offer (Cons val nxt))

class SValidSessionType lst
instance (STerminal a) => SValidSessionType (Cons a Nil)
instance (SValidSessionType nxt, SNonTerminal val) => SValidSessionType (Cons val nxt)

infixr 5 ~>
(~>) :: (TyList nxt, SNonTerminal a, SValidSessionType nxt) => a -> nxt -> (Cons a nxt)
(~>) = cons

infixr 5 ~|~
(~|~) :: (TyNum target, TyList nxt) => target -> nxt -> Cons (Cons (Jump target) Nil) nxt
(~|~) = cons . jump

class SNoJumpsBeyond s idx
instance SNoJumpsBeyond End idx
instance (SmallerThanBool l idx True) => SNoJumpsBeyond (Jump l) idx
instance SNoJumpsBeyond (Send t) idx
instance SNoJumpsBeyond (Recv t) idx
instance (SNoJumpsBeyond lol idx) => SNoJumpsBeyond (Select lol) idx
instance (SNoJumpsBeyond lol idx) => SNoJumpsBeyond (Offer lol) idx
instance ( MakeListOfJumps lol lol'
         , SNoJumpsBeyond lol' idx) => 
    SNoJumpsBeyond (SendPid inverted lol) idx
instance ( MakeListOfJumps lol lol'
         , SNoJumpsBeyond lol' idx) =>
    SNoJumpsBeyond (RecvPid inverted lol) idx
instance (SNoJumpsBeyond l idx) => SNoJumpsBeyond (SendSession inverted l) idx
instance (SNoJumpsBeyond l idx) => SNoJumpsBeyond (RecvSession inverted l) idx
instance SNoJumpsBeyond Nil idx
instance (SNoJumpsBeyond val idx, SNoJumpsBeyond nxt idx) => SNoJumpsBeyond (Cons val nxt) idx

class MakeListOfJumps x y | x -> y where
    makeListOfJumps :: x -> y
instance MakeListOfJumps Nil Nil where
    makeListOfJumps _ = nil
instance ( TyNum num
         , MakeListOfJumps nxt nxt'
         , TyList nxt
         , TyList nxt'
         ) =>
    MakeListOfJumps (Cons (num, invert) nxt) (Cons (Cons (Jump num) Nil) nxt') where
        makeListOfJumps lst = cons (jump num) (makeListOfJumps lst')
            where
              (num, _) = tyHead lst
              lst' = tyTail lst

class SWellFormedConfig idxA idxB ss
instance ( SListOfSessionTypes ss
         , TyListLength ss len
         , SNoJumpsBeyond ss len
         , SmallerThanBool idxA len True
         , TyListIndex ss idxA st
         , TyListLength st len'
         , SmallerThanBool idxB len' True
         ) =>
    SWellFormedConfig idxA idxB ss

testWellformed :: (SWellFormedConfig idxA idxB ss) => ss -> idxA -> idxB -> Bool
testWellformed _ _ _ = True

data Choice :: * -> * where
               Choice :: lstOfLabels -> Choice lstOfLabels

type family Outgoing frag
type instance Outgoing (Cons (Send (sp,t)) nxt) = Cons t (Outgoing nxt)
type instance Outgoing (Cons (Recv (sp,t)) nxt)      = Outgoing nxt
type instance Outgoing (Cons (Select loj) Nil)  = Cons (Choice loj) Nil
type instance Outgoing (Cons (Offer loj) Nil)   = Cons (Choice loj) Nil
type instance Outgoing (Cons (Jump l) Nil)      = Cons (Jump l) Nil
type instance Outgoing (Cons End Nil)           = Cons End Nil

class Expand prog frag expanded | prog frag -> expanded where
    type ExpandT prog frag
instance (Expand prog nxt nxt') => Expand prog (Cons (Send t) nxt) (Cons (Send t) nxt') where
    type ExpandT prog (Cons (Send t) nxt) = Cons (Send t) (ExpandT prog nxt)
instance (Expand prog nxt nxt') => Expand prog (Cons (Recv t) nxt) (Cons (Recv t) nxt') where
    type ExpandT prog (Cons (Recv t) nxt) = Cons (Recv t) (ExpandT prog nxt)
instance Expand prog (Cons (Select loj) Nil) (Cons (Select loj) Nil) where
    type ExpandT prog (Cons (Select loj) Nil) = Cons (Select loj) Nil
instance Expand prog (Cons (Offer loj) Nil) (Cons (Offer loj) Nil) where
    type ExpandT prog (Cons (Offer loj) Nil) = Cons (Offer loj) Nil
instance Expand prog (Cons (Jump l) Nil) (Cons (Jump l) Nil) where
    type ExpandT prog (Cons (Jump l) Nil) = Cons (Jump l) Nil
instance Expand prog (Cons End Nil) (Cons End Nil) where
    type ExpandT prog (Cons End Nil) = Cons End Nil
