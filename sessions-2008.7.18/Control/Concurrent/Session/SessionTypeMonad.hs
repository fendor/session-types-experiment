{-# LANGUAGE KindSignatures
           , GADTs
           , MultiParamTypeClasses
           , UndecidableInstances
           , FunctionalDependencies
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts #-}

{-
    SessionTypeMonad.hs
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

module Control.Concurrent.Session.SessionTypeMonad where

import Control.Concurrent.Session.Base.Bool
import Control.Concurrent.Session.Base.Number
import Control.Concurrent.Session.Base.List
import Control.Concurrent.Session.Base.SMonad
import Control.Concurrent.Session.Types
import Control.Concurrent.Session.SessionType hiding (jump, end, select, offer, (~|~), sendPid, recvPid, sendSession, recvSession, Dual(..))

data TypeState :: * -> * -> * -> * -> * where
                  TypeState :: nxtLabel -> declareable -> useable -> st ->
                               TypeState nxtLabel declareable useable st

newtype SessionType f t r = SessionType { buildSessionType :: f -> (r, t) }

instance SMonad SessionType where
    a ~>> b   = SessionType $ \f -> let (_, f') = buildSessionType a f
                                    in buildSessionType b f'
    a ~>>= b  = SessionType $ \f -> let (r, f') = buildSessionType a f
                                    in buildSessionType (b r) f'
    sreturn r = SessionType $ \f -> (r, f)

newLabel :: ( Succ nxtLabel nxtLabel'
            , TyListConsSet nxtLabel declareable declareable'
            , TyListConsSet nxtLabel useable useable'
            ) =>
            SessionType (TypeState nxtLabel declareable useable st)
                        (TypeState nxtLabel' declareable' useable' st) nxtLabel
newLabel = SessionType $ \(TypeState nxtLabel declareable useable st) ->
                          (nxtLabel, (TypeState (tySucc nxtLabel) (tyListConsSet nxtLabel declareable) (tyListConsSet nxtLabel useable) st))

declareLabel :: ( TyListMember declareable label True
                , TyListElem declareable label idx
                , TyListDelete declareable idx declareable'
                , TyList st
                ) =>
    label -> (SessionType (TypeState nxtLabel declareable' useable (Cons (label, Nil) st))
                          (TypeState nxtLabel' declareable'' useable' st') a) ->
    SessionType (TypeState nxtLabel declareable useable st)
                (TypeState nxtLabel' declareable'' useable' st') a
declareLabel label f
    = SessionType $ \(TypeState nxtLabel declareable useable st) ->
                    let idx = tyListElem declareable label
                        declareable' = tyListDelete idx declareable
                    in buildSessionType f (TypeState nxtLabel declareable' useable (cons (label, nil) st))

(.=) :: ( TyListMember declareable label True
        , TyListElem declareable label idx
        , TyListDelete declareable idx declareable'
        , TyList st
        ) =>
    label -> (SessionType (TypeState nxtLabel declareable' useable (Cons (label, Nil) st))
                          (TypeState nxtLabel' declareable'' useable' st') a) ->
    SessionType (TypeState nxtLabel declareable useable st)
                (TypeState nxtLabel' declareable'' useable' st') a
(.=) = declareLabel
infixl 2 .=

send :: ( TyList f
        , TyList fs
        ) =>
    t -> SessionType (TypeState nxtLabel declareable useable (Cons (label, f) fs))
                     (TypeState nxtLabel declareable useable (Cons (label, (Cons (Send (SpecialNormal, t)) f)) fs)) ()
send t = SessionType $ \(TypeState nxtLabel declareable useable st) ->
                        ((), (TypeState nxtLabel declareable useable (modifyCons (\(label, f) -> (label, cons (Send (undefined, t)) f)) id st)))

recv :: ( TyList f
        , TyList fs
        ) =>
    t -> SessionType (TypeState nxtLabel declareable useable (Cons (label, f) fs))
                     (TypeState nxtLabel declareable useable (Cons (label, (Cons (Recv (SpecialNormal, t)) f)) fs)) ()
recv t = SessionType $ \(TypeState nxtLabel declareable useable st) ->
                        ((), (TypeState nxtLabel declareable useable (modifyCons (\(label, f) -> (label, cons (Recv (undefined, t)) f)) id st)))

sendPid :: ( TyList f
           , TyList fs
           , TyListSortNums lst lst'
           ) =>
    lst -> SessionType (TypeState nxtLabel declareable useable (Cons (label, f) fs))
                       (TypeState nxtLabel declareable useable (Cons (label, (Cons (SendPid False lst') f)) fs)) ()
sendPid lst = SessionType $ \(TypeState nxtLabel declareable useable st) ->
                             ((), (TypeState nxtLabel declareable useable (modifyCons (\(label, f) -> (label, cons (SendPid FF (tyListSortNums lst)) f)) id st)))

recvPid :: ( TyList f
           , TyList fs
           , TyListSortNums lst lst'
           ) =>
    lst -> SessionType (TypeState nxtLabel declareable useable (Cons (label, f) fs))
                       (TypeState nxtLabel declareable useable (Cons (label, (Cons (RecvPid False lst') f)) fs)) ()
recvPid lst = SessionType $ \(TypeState nxtLabel declareable useable st) ->
                             ((), (TypeState nxtLabel declareable useable (modifyCons (\(label, f) -> (label, cons (RecvPid FF (tyListSortNums lst)) f)) id st)))

sendSession :: ( TyList f
               , TyList fs
               , TyList fs'
               , TyListReverse frag fragRev
               , TyListTake (D1 E) fragRev (Cons (label, fragHead) Nil)
               , TyListDrop (D1 E) fragRev fragTailRev
               , TyListReverse fragTailRev fragTail
               , TyListAppend fragTail fs fs'
               ) =>
    SessionType (TypeState nxtLabel declareable useable (Cons (label, Nil) Nil))
                (TypeState nxtLabel' declareable' useable' frag) res ->
    SessionType (TypeState nxtLabel declareable useable (Cons (label, f) fs))
                (TypeState nxtLabel' declareable' useable' (Cons (label, (Cons (SendSession False fragHead) f)) fs')) res
sendSession fragST = SessionType $ \(TypeState nxtLabel declareable useable st) ->
                     let (label, f) = tyHead st
                         stTail = tyTail st
                         (res, (TypeState nxtLabel' declareable' useable' frag))
                             = buildSessionType fragST (TypeState nxtLabel declareable useable (cons (label, nil) nil))
                         fragRev = tyListReverse frag
                         (_, fragHead) = tyHead $ tyListTake (D1 E) fragRev
                         fragTailRev = tyListDrop (D1 E) fragRev
                         fragTail = tyListReverse fragTailRev
                         fs' = cons (label, cons (SendSession FF fragHead) f) $ tyListAppend fragTail stTail
                     in (res, (TypeState nxtLabel' declareable' useable' fs'))

recvSession :: ( TyList f
               , TyList fs
               , TyList fs'
               , TyListReverse frag fragRev
               , TyListTake (D1 E) fragRev (Cons (label, fragHead) Nil)
               , TyListDrop (D1 E) fragRev fragTailRev
               , TyListReverse fragTailRev fragTail
               , TyListAppend fragTail fs fs'
               ) =>
    SessionType (TypeState nxtLabel declareable useable (Cons (label, Nil) Nil))
                (TypeState nxtLabel' declareable' useable' frag) res ->
    SessionType (TypeState nxtLabel declareable useable (Cons (label, f) fs))
                (TypeState nxtLabel' declareable' useable' (Cons (label, (Cons (RecvSession False fragHead) f)) fs')) res
recvSession fragST = SessionType $ \(TypeState nxtLabel declareable useable st) ->
                     let (label, f) = tyHead st
                         stTail = tyTail st
                         (res, (TypeState nxtLabel' declareable' useable' frag))
                             = buildSessionType fragST (TypeState nxtLabel declareable useable (cons (label, nil) nil))
                         fragRev = tyListReverse frag
                         (_, fragHead) = tyHead $ tyListTake (D1 E) fragRev
                         fragTailRev = tyListDrop (D1 E) fragRev
                         fragTail = tyListReverse fragTailRev
                         fs' = cons (label, cons (RecvSession FF fragHead) f) $ tyListAppend fragTail stTail
                     in (res, (TypeState nxtLabel' declareable' useable' fs'))

end :: ( TyListReverse (Cons End f) f'
       , TyList f
       , TyList fs
       ) =>
    SessionType (TypeState nxtLabel declareable useable (Cons (label, f) fs))
                (TypeState nxtLabel declareable useable (Cons (label, f') fs)) ()
end = SessionType $ \(TypeState nxtLabel declareable useable st) ->
                     ((), (TypeState nxtLabel declareable useable (modifyCons (\(label, f) -> (label, tyListReverse (cons End f))) id st)))

jump :: ( TyListReverse (Cons (Jump jt) f) f'
        , TyList f
        , TyList fs
        , TyListMember useable jt True
        ) =>
    jt -> SessionType (TypeState nxtLabel declareable useable (Cons (label, f) fs))
                      (TypeState nxtLabel declareable useable (Cons (label, f') fs)) ()
jump jt = SessionType $ \(TypeState nxtLabel declareable useable st) ->
                         ((), (TypeState nxtLabel declareable useable (modifyCons (\(label, f) -> (label, tyListReverse (cons (Jump jt) f))) id st)))

data BranchesList :: * -> * -> * -> * -> * -> * where
                     BLNil :: BranchesList Nil Nil z z z
                     BLCons :: (SessionType (TypeState nxtLabel declareable useable st)
                                            (TypeState nxtLabel' declareable' useable' st') ( (resLst -> Cons res resLst)
                                                                                            , (labs -> Cons (Cons (Jump nxtLabel) Nil) labs))) ->
                               (BranchesList resLst labs (TypeState nxtLabel' declareable' useable' st') to finalTo) ->
                               (BranchesList (Cons res resLst) (Cons (Cons (Jump nxtLabel) Nil) labs) (TypeState nxtLabel declareable useable st)
                                                 ((TypeState nxtLabel' declareable' useable' st'), to) finalTo)

(~|~) :: ( Succ label nxtLabel
         , TyListConsSet label declareable declareable'
         , TyListElem declareable' label idx
         , TyListDelete declareable' idx declareable''
         , TyListConsSet label useable useable'
         , TyList st
         , TyListMember declareable' label True
         , TyList labs
         , TyList resLst
         ) =>
    (SessionType (TypeState nxtLabel declareable'' useable' (Cons (label, Nil) st))
                 (TypeState nxtLabel' declareable''' useable'' st') res) ->
    (BranchesList resLst labs (TypeState nxtLabel' declareable''' useable'' st') to finalTo) ->
    (BranchesList (Cons res resLst) (Cons (Cons (Jump label) Nil) labs) (TypeState label declareable useable st) ((TypeState nxtLabel' declareable''' useable'' st'), to) finalTo)
(~|~) st lst = BLCons (newLabel ~>>= \l -> declareLabel l st ~>>= \r -> sreturn (cons r, (cons (cons (Jump l) nil)))) lst
infixr 5 ~|~

class BuildBranches bl st where
    buildBranches :: bl -> st
instance BuildBranches (BranchesList Nil Nil (TypeState nxtLabel declareLabel useable st)
                                             (TypeState nxtLabel declareLabel useable st)
                                             (TypeState nxtLabel declareLabel useable st))
    (SessionType (TypeState nxtLabel declareLabel useable st) (TypeState nxtLabel declareLabel useable st) (Nil, Nil)) where
        buildBranches (BLNil) = sreturn (nil, nil)
        buildBranches _ = error "Monstrously impossible"
instance (BuildBranches (BranchesList resLst labs (TypeState nxtLabel' declareable' useable' st') to finalTo)
                        (SessionType (TypeState nxtLabel' declareable' useable' st') finalTo (resLst, labs))
         ) =>
    BuildBranches (BranchesList (Cons res resLst) (Cons (Cons (Jump nxtLabel) Nil) labs)
                                (TypeState nxtLabel declareable useable st)
                                ((TypeState nxtLabel' declareable' useable' st'), to)
                                finalTo)
                  (SessionType (TypeState nxtLabel declareable useable st) finalTo ((Cons res resLst), (Cons (Cons (Jump nxtLabel) Nil) labs))) where
        buildBranches (BLCons stm lst) = stm ~>>= \(fR, fL) ->
                                         buildBranches lst ~>>= \(res, labs) ->
                                         sreturn (fR res, fL labs)
        buildBranches _ = error "Equally monstrously impossible"

select :: forall f f' fs listOfRes listOfJumps label nxtLabel declareable useable to finalTo .
          ( TyListReverse (Cons (Select listOfJumps) f) f'
          , TyList f
          , TyList fs
          , BuildBranches (BranchesList listOfRes listOfJumps (TypeState nxtLabel declareable useable (Cons (label, f') fs)) to finalTo)
                          (SessionType (TypeState nxtLabel declareable useable (Cons (label, f') fs)) finalTo (listOfRes, listOfJumps))
          ) =>
    (BranchesList listOfRes listOfJumps (TypeState nxtLabel declareable useable (Cons (label, f') fs)) to finalTo) ->
    (SessionType (TypeState nxtLabel declareable useable (Cons (label, f) fs)) finalTo listOfRes)
select bl = SessionType $ \(TypeState nxtLabel declareable useable st) ->
                           let ((listOfRes, listOfJumps), ts)
                                   = buildSessionType (buildBranches bl) (TypeState nxtLabel declareable useable
                                                                          ((modifyCons (\(label, f) -> (label, tyListReverse (cons (Select listOfJumps) f)))
                                                                                      id st) :: Cons (label, f') fs))
                           in (listOfRes, ts)

offer :: forall f f' fs listOfRes listOfJumps label nxtLabel declareable useable to finalTo .
          ( TyListReverse (Cons (Offer listOfJumps) f) f'
          , TyList f
          , TyList fs
          , BuildBranches (BranchesList listOfRes listOfJumps (TypeState nxtLabel declareable useable (Cons (label, f') fs)) to finalTo)
                          (SessionType (TypeState nxtLabel declareable useable (Cons (label, f') fs)) finalTo (listOfRes, listOfJumps))
          ) =>
    (BranchesList listOfRes listOfJumps (TypeState nxtLabel declareable useable (Cons (label, f') fs)) to finalTo) ->
    (SessionType (TypeState nxtLabel declareable useable (Cons (label, f) fs)) finalTo listOfRes)
offer bl = SessionType $ \(TypeState nxtLabel declareable useable st) ->
                           let ((listOfRes, listOfJumps), ts)
                                   = buildSessionType (buildBranches bl) (TypeState nxtLabel declareable useable
                                                                          ((modifyCons (\(label, f) -> (label, tyListReverse (cons (Offer listOfJumps) f)))
                                                                                      id st) :: Cons (label, f') fs))
                           in (listOfRes, ts)

currentLabel :: SessionType (TypeState nxtLabel declareable useable (Cons (label, f) fs)) (TypeState nxtLabel declareable useable (Cons (label, f) fs)) label
currentLabel = SessionType $ \ts@(TypeState _ _ _ st) -> (fst . tyHead $ st, ts)

makeSessionType :: ( TyListSortNums st st'
                   , TyListSnd st' st''
                   ) =>
                   SessionType (TypeState (D0 E) Nil Nil Nil) (TypeState nxtLabel Nil useable st) res -> (st'', res)
makeSessionType a = let (res, (TypeState _ _ _ st)) = (buildSessionType a) (TypeState (D0 E) nil nil nil)
                    in (tyListSnd . tyListSortNums $ st, res)

class TyListSnd lstA lstB | lstA -> lstB where
    tyListSnd :: lstA -> lstB
instance TyListSnd Nil Nil where
    tyListSnd _ = nil
instance ( TyListSnd nxt nxt'
         , TyList nxt
         , TyList nxt'
         ) =>
    TyListSnd (Cons (a, b) nxt) (Cons b nxt') where
        tyListSnd = modifyCons snd tyListSnd

dual :: True
dual = TT

notDual :: False
notDual = FF
