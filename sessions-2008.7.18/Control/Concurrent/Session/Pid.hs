{-# LANGUAGE KindSignatures
           , GADTs
           , ScopedTypeVariables
           , PatternSignatures
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , TypeFamilies
           , FlexibleContexts
           #-}

{-
    Pid.hs
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

-- | Defines what a 'Pid' is and provides functionality to create new
-- sessions / channels to a given pid. Obviously this is /safe/ in
-- some way - in particular, a Pid carries about with it the set of
-- Session Types it is willing to use. This means that you can't try
-- to start any old Session Type with any given Pid. However, it
-- doesn't mean that given an acceptable Session Type, the other
-- thread will ever actually get around to agreeing to create the new
-- session / channel with you.

module Control.Concurrent.Session.Pid
    ( makePid
    , rootPid
    , iPidToPid
    , myPid
    , BuildPidTyMap (..)
    , BuildInvertedSessionsSet (..)
    , CreateSession (..)
    , PidEq (..)
    , MultiReceiveList (MultiReceiveNil)
    , (~|||~)
    , MultiReceive (..)
    , PlainMultiReceive (..)
    , CombinedMultiRecv (..)
    ) where

import Control.Concurrent.Session.Base.Bool
import Control.Concurrent.Session.Base.Number
import Control.Concurrent.Session.Base.Map
import Control.Concurrent.Session.Base.List
import Control.Concurrent.Session.SessionType
import Control.Concurrent.Session.Types
import Control.Concurrent.Session.Runtime
import Control.Concurrent
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

makePid :: InternalPid prog prog' invertedSessionsO sessionsToIdxO idxsToPairStructsO ->
           invertedSessionsN -> TyMap sessionsToIdxN idxsToPairStructsN ->
           (InternalPid prog prog' invertedSessionsO sessionsToIdxO idxsToPairStructsO,
            InternalPid prog prog' invertedSessionsN sessionsToIdxN idxsToPairStructsN)
makePid (IPid orig@(Pid _ _) (p:ps)) _ childTM = ((IPid orig ps), child)
    where
      child = IPid (Pid p childTM) [x:p | x <- [0..]]
makePid (IPid _ []) _ _ = error "Out of pids. Interesting."

rootPid :: ( Dual prog prog'
           , DualT prog ~ prog'
           ) =>
           TyMap sessionsToIdx idxsToPairStructs -> invertedSessions -> prog ->
           InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs
rootPid tm _ _ = IPid (Pid [0] tm) [[x,0] | x <- [0..]]

myPid :: InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) from from (Pid prog prog' invertedSessions sessionsToIdx idxsToPairStructs)
myPid = InterleavedChain $ \p x -> return (iPidToPid p, x, p)

class BuildPidTyMap prog stlst tymap | prog stlst -> tymap where
    type BuildPidTyMapT prog stlst
    buildPidTyMap :: prog -> stlst -> IO tymap

instance (BuildPidTyMap' prog stlst (TyMap Nil Nil) tymap) =>
    BuildPidTyMap prog stlst tymap where
        type BuildPidTyMapT prog stlst = BuildPidTyMapT' prog stlst (TyMap Nil Nil)
        buildPidTyMap prog stlst = buildPidTyMap' prog stlst emptyMap

class BuildPidTyMap' prog stlist tymap1 tymap2 | prog stlist tymap1 -> tymap2 where
    type BuildPidTyMapT' prog stlist tymap1
    buildPidTyMap' :: prog -> stlist -> tymap1 -> IO tymap2

instance BuildPidTyMap' prog Nil acc acc where
    type BuildPidTyMapT' prog Nil acc = acc
    buildPidTyMap' _ _ m = return m
-- this instance reverses the stList in the keys of the map. So if the stList is sorted then this will be reverse . sorted
instance ( BuildPidTyMap' prog nxt
                          (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue')
         , MapInsert (TyMap keyToIdx' idxToValue') init
                     (MVar (Map (RawPid, RawPid)
                                (MVar (PairStruct init prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil))))))
                     (TyMap keyToIdx'' idxToValue'')
         , TyList nxt
         ) => 
    BuildPidTyMap' prog (Cons (init, False) nxt) (TyMap keyToIdx idxToValue)
                   (TyMap keyToIdx'' idxToValue'') where
        type BuildPidTyMapT' prog (Cons (init, False) nxt) (TyMap keyToIdx idxToValue)
                      -- so we stick on the front of what we receive incoming. Thus the first elem ends up at the end of the map. Matching below.
            = BuildPidTyMapT' prog nxt (TyMap (Cons init keyToIdx) -- HERE LIES TROUBLE! -- the use of DualT to make the inverse may be dangerous
                                              (Cons ((MVar (Map (RawPid, RawPid) -- plus this is cheating as I'm using knowledge of how mapInsert works
                                                                (MVar (PairStruct init prog (DualT prog) -- to avoid rewriting lists in type families
                                                                       ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))))))
                                                    idxToValue))
        buildPidTyMap' prog lst m
            = do { m' <- buildPidTyMap' prog nxt m
                 ; mvar <- newMVar Map.empty
                 ; return $ f mvar m' -- inserting adds at the end. So the first elem will be last in the map
                 }
            where
              (init, FF) = tyHead lst
              nxt = tyTail lst
              f :: (MVar (Map (RawPid, RawPid)
                              (MVar (PairStruct init prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))))) ->
                   TyMap keyToIdx' idxToValue' ->
                   TyMap keyToIdx'' idxToValue''
              f mvar = mapInsert init mvar

instance ( BuildPidTyMap' prog nxt (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue')
         , TyList nxt
         ) =>
    BuildPidTyMap' prog (Cons (init, True) nxt) (TyMap keyToIdx idxToValue)
                       (TyMap keyToIdx' idxToValue') where
        type BuildPidTyMapT' prog (Cons (init, True) nxt) (TyMap keyToIdx idxToValue) = BuildPidTyMapT' prog nxt (TyMap keyToIdx idxToValue)
        buildPidTyMap' prog lst m = buildPidTyMap' prog (tyTail lst) m

class BuildInvertedSessionsSet stlist set | stlist -> set where
    type BuildInvertedSessionsSetT stlist
    buildInvertedSessionsSet :: stlist -> set
instance BuildInvertedSessionsSet Nil Nil where
    type BuildInvertedSessionsSetT Nil = Nil
    buildInvertedSessionsSet _ = nil
instance ( BuildInvertedSessionsSet nxt set
         ) =>
    BuildInvertedSessionsSet (Cons (init, False) nxt) set where
        type BuildInvertedSessionsSetT (Cons (init, False) nxt) = BuildInvertedSessionsSetT nxt
        buildInvertedSessionsSet lst = buildInvertedSessionsSet (tyTail lst)
instance ( BuildInvertedSessionsSet nxt set
         , TyList set
         ) =>
    BuildInvertedSessionsSet (Cons (init, True) nxt) (Cons init set) where
        type BuildInvertedSessionsSetT (Cons (init, True) nxt) = Cons init (BuildInvertedSessionsSetT nxt)
        buildInvertedSessionsSet lst = cons (fst . tyHead $ lst) . buildInvertedSessionsSet . tyTail $ lst

instance ( Expand prog nxt nxt'
         , ExpandPid prog (SendPid invert idxs) expandedSendPid
         ) =>
    Expand prog (Cons (SendPid invert idxs) nxt) (Cons expandedSendPid nxt') where
        type ExpandT prog (Cons (SendPid invert idxs) nxt) = Cons (ExpandPidT prog (SendPid invert idxs)) (ExpandT prog nxt)
instance ( Expand prog nxt nxt'
         , ExpandPid prog (RecvPid invert idxs) expandedRecvPid
         ) =>
    Expand prog (Cons (RecvPid invert idxs) nxt) (Cons expandedRecvPid nxt') where
        type ExpandT prog (Cons (RecvPid invert idxs) nxt) = Cons (ExpandPidT prog (RecvPid invert idxs)) (ExpandT prog nxt)

class ExpandPid prog pid expanded | prog pid -> expanded where
    type ExpandPidT prog pid

instance ( Dual prog prog'
         , BuildInvertedSessionsSet idxs invertedSessions
         , BuildPidTyMap prog idxs (TyMap sessionsToIdx idxsToPairStructs)
         ) =>
    ExpandPid prog (SendPid False idxs) (Send (SpecialPid, (Pid prog prog' invertedSessions sessionsToIdx idxsToPairStructs))) where
        type ExpandPidT prog (SendPid False idxs)
            = Send ( SpecialPid
                   , (Pid prog (DualT prog) (BuildInvertedSessionsSetT idxs) (LHS (BuildPidTyMapT prog idxs)) (RHS (BuildPidTyMapT prog idxs)))
                   )

instance ( Dual prog prog'
         , BuildInvertedSessionsSet idxs invertedSessions
         , BuildPidTyMap prog' idxs (TyMap sessionsToIdx idxsToPairStructs)
         ) =>
    ExpandPid prog (SendPid True idxs) (Send (SpecialPid, (Pid prog' prog invertedSessions sessionsToIdx idxsToPairStructs))) where
        type ExpandPidT prog (SendPid True idxs)
            = Send ( SpecialPid
                   , (Pid (DualT prog) prog (BuildInvertedSessionsSetT idxs) (LHS (BuildPidTyMapT (DualT prog) idxs)) (RHS (BuildPidTyMapT (DualT prog) idxs)))
                   )

instance ( Dual prog prog'
         , BuildInvertedSessionsSet idxs invertedSessions
         , BuildPidTyMap prog idxs (TyMap sessionsToIdx idxsToPairStructs)
         ) =>
    ExpandPid prog (RecvPid False idxs) (Recv (SpecialPid, (Pid prog prog' invertedSessions sessionsToIdx idxsToPairStructs))) where
        type ExpandPidT prog (RecvPid False idxs)
            = Recv ( SpecialPid
                   , (Pid prog (DualT prog) (BuildInvertedSessionsSetT idxs) (LHS (BuildPidTyMapT prog idxs)) (RHS (BuildPidTyMapT prog idxs)))
                   )

instance ( Dual prog prog'
         , BuildInvertedSessionsSet idxs invertedSessions
         , BuildPidTyMap prog' idxs (TyMap sessionsToIdx idxsToPairStructs)
         ) =>
    ExpandPid prog (RecvPid True idxs) (Recv (SpecialPid, (Pid prog' prog invertedSessions sessionsToIdx idxsToPairStructs))) where
        type ExpandPidT prog (RecvPid True idxs)
            = Recv ( SpecialPid
                   , (Pid (DualT prog) prog (BuildInvertedSessionsSetT idxs) (LHS (BuildPidTyMapT (DualT prog) idxs)) (RHS (BuildPidTyMapT (DualT prog) idxs)))
                   )

type family LHS thing
type instance LHS (TyMap sessionsToIdx idxsToPairStructs) = sessionsToIdx
type family RHS thing
type instance RHS (TyMap sessionsToIdx idxsToPairStructs) = idxsToPairStructs

-- TyMap :: (init, InvertBool) -> MVar (Map (RawPid, RawPid) (MVar PairStruct init prog progOut progIn (fromO, fromI)))

-- | Provides the ability to make a new session / channel with the
-- given Pid.  Supply the index to the Session Type, whether or not
-- you're locally inverting (dualing) the Session Type, and the Pid,
-- and so long as the Pid supports the dual of your local Session
-- Type, this will block until the Pid gets around to servicing you.
-- Thus this is a synchronous operation and both Pids must know of
-- each other to create a new session / channel between them.
class CreateSession invert init prog prog'
                    sessionsToIdxMe sessionsToIdxThem idxsToPairStructsMe idxsToPairStructsThem
                    keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe' idxOfThem invertedSessionsMe invertedSessionsThem where
    createSession :: init -> invert -> Pid prog prog' invertedSessionsThem sessionsToIdxThem idxsToPairStructsThem ->
                     InterleavedChain (InternalPid prog prog' invertedSessionsMe sessionsToIdxMe idxsToPairStructsMe)
                                      (TyMap keyToIdxMe idxToValueMe) (TyMap keyToIdxMe' idxToValueMe') idxOfThem

instance forall init prog prog' fromO fromI progOut progIn
                sessionsToIdxMe sessionsToIdxThem idxsToPairStructsMe idxsToPairStructsThem
                keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe' idxOfThem current current' invertedSessionsMe invertedSessionsThem .
    ( ProgramToMVarsOutgoingT prog prog ~ progOut
    , ProgramToMVarsOutgoingT prog' prog' ~ progIn
    , SWellFormedConfig init (D0 E) prog
    , SWellFormedConfig init (D0 E) prog'
    , TyListIndex progOut init (MVar (ProgramCell (Cell fromO)))
    , TyListIndex progIn init (MVar (ProgramCell (Cell fromI)))
    , TyListIndex prog init current'
    , Expand prog current' current
    , MapLookup (TyMap sessionsToIdxMe idxsToPairStructsMe) init
                    (MVar (Map (RawPid, RawPid) (MVar (PairStruct init prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil))))))
    , TyListMember invertedSessionsThem init True
    , MapSize (TyMap keyToIdxMe idxToValueMe) idxOfThem
    , MapInsert (TyMap keyToIdxMe idxToValueMe) idxOfThem
                    (SessionState prog prog' (current, fromO, fromI)) (TyMap keyToIdxMe' idxToValueMe')
    ) =>
    CreateSession False init prog prog'
                  sessionsToIdxMe sessionsToIdxThem idxsToPairStructsMe idxsToPairStructsThem
                  keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe' idxOfThem invertedSessionsMe invertedSessionsThem where
                      createSession init FF (Pid remotePid _) =
                          InterleavedChain $
                              \ipid@(IPid (Pid localPid localSTMap) _) mp ->
                                  do { let pidFuncMapMVar :: MVar (Map (RawPid, RawPid)
                                                                       (MVar (PairStruct init prog prog'
                                                                              ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))))
                                               = mapLookup localSTMap init
                                     ; pidFuncMap <- takeMVar pidFuncMapMVar
                                     ; emptyMVar :: MVar (TyMap keyToIdxMe' idxToValueMe') <- newEmptyMVar
                                     ; psMVar :: MVar (PairStruct init prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))
                                              <- case Map.lookup (localPid, remotePid) pidFuncMap of
                                                   Nothing
                                                       -> do { empty <- newEmptyMVar
                                                             ; putMVar pidFuncMapMVar (Map.insert (localPid, remotePid) empty pidFuncMap)
                                                             ; return empty
                                                             }
                                                   (Just mv)
                                                       -> do { putMVar pidFuncMapMVar pidFuncMap
                                                             ; return mv
                                                             }
                                     ; let idxOfThem :: idxOfThem = mapSize mp
                                           ps :: PairStruct init prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil))
                                              = PS localPid (f idxOfThem mp emptyMVar)
                                     ; putMVar psMVar ps
                                     ; mp' <- takeMVar emptyMVar
                                     ; return (idxOfThem, mp', ipid)
                                     }
                                  where
                                    f :: idxOfThem -> (TyMap keyToIdxMe idxToValueMe) ->
                                         MVar (TyMap keyToIdxMe' idxToValueMe') ->
                                         SessionState prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)) ->
                                         IO ()
                                    f idxOfThem mp mv localST
                                        = do { ((), localST') <- runSessionChain sjump localST
                                             ; putMVar mv (mapInsert idxOfThem localST' mp)
                                             }

instance forall init prog prog' fromO fromI progOut progIn
                sessionsToIdxMe sessionsToIdxThem idxsToPairStructsMe idxsToPairStructsThem
                keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe' idxOfThem current current' currentUX currentUX' invertedSessionsMe invertedSessionsThem .
    ( ProgramToMVarsOutgoingT prog prog ~ progOut
    , ProgramToMVarsOutgoingT prog' prog' ~ progIn
    , ProgramToMVarsOutgoing prog prog progOut
    , ProgramToMVarsOutgoing prog' prog' progIn
    , SWellFormedConfig init (D0 E) prog
    , SWellFormedConfig init (D0 E) prog'
    , TyListIndex progOut init (MVar (ProgramCell (Cell fromO)))
    , TyListIndex progIn init (MVar (ProgramCell (Cell fromI)))
    , TyListIndex prog init currentUX
    , Expand prog currentUX current
    , TyListIndex prog' init currentUX'
    , Expand prog' currentUX' current'
    , MapLookup (TyMap sessionsToIdxThem idxsToPairStructsThem) init
                    (MVar (Map (RawPid, RawPid) (MVar (PairStruct init prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil))))))
    , TyListMember invertedSessionsMe init True
    , MapSize (TyMap keyToIdxMe idxToValueMe) idxOfThem
    , MapInsert (TyMap keyToIdxMe idxToValueMe) idxOfThem
                    (SessionState prog' prog (current', fromI, fromO)) (TyMap keyToIdxMe' idxToValueMe')
    ) =>
    CreateSession True init prog prog'
                  sessionsToIdxMe sessionsToIdxThem idxsToPairStructsMe idxsToPairStructsThem
                  keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe' idxOfThem invertedSessionsMe invertedSessionsThem where
                      createSession init TT (Pid remotePid remoteSTMap) =
                          InterleavedChain $
                              \ipid@(IPid (Pid localPid _) _) mp ->
                                  do { let pidFuncMapMVar :: MVar (Map (RawPid, RawPid)
                                                                       (MVar (PairStruct init prog prog'
                                                                              ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))))
                                               = mapLookup remoteSTMap init
                                     ; pidFuncMap <- takeMVar pidFuncMapMVar
                                     ; mvarsOut <- programToMVarsOutgoing prog prog
                                     ; mvarsIn <- programToMVarsOutgoing prog' prog'
                                     ; aNotify <- newEmptyMVar
                                     ; bNotify <- newEmptyMVar
                                     ; let (theirST :: SessionState prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))
                                               =  SessionState mvarsOut mvarsIn undefined aNotify undefined bNotify undefined
                                           (myST :: SessionState prog' prog ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))
                                               = SessionState mvarsIn mvarsOut undefined bNotify undefined aNotify undefined
                                           idxOfThem :: idxOfThem = mapSize mp
                                     ; case Map.lookup (remotePid, localPid) pidFuncMap of
                                         Nothing
                                             -> do { newEmptyMVar <- newEmptyMVar
                                                   ; putMVar pidFuncMapMVar (Map.insert (remotePid, localPid) newEmptyMVar pidFuncMap)
                                                   ; ps <- takeMVar newEmptyMVar
                                                   ; modifyMVar_ pidFuncMapMVar (return . Map.delete (remotePid, localPid))
                                                   ; fun ps theirST
                                                   }
                                         (Just fullMVar)
                                             -> do { ps <- takeMVar fullMVar
                                                   ; putMVar pidFuncMapMVar (Map.delete (remotePid, localPid) pidFuncMap)
                                                   ; fun ps theirST
                                                   }
                                     ; ((), myST') <- runSessionChain sjump myST
                                     ; return (idxOfThem, mapInsert idxOfThem myST' mp, ipid)
                                     }
                                  where
                                    prog = undefined::prog
                                    prog' = undefined::prog'
                                    fun :: PairStruct init prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil))  ->
                                           SessionState prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil))  -> IO ()
                                    fun (PS _ f) theirST = f theirST


data MultiReceiveList :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> * where
                     MultiReceiveNil :: MultiReceiveList Nil prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' res
                     MultiReceiveCons :: (ch, InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs)
                                                                 (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') res) ->
                                         MultiReceiveList chs prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' res ->
                                         MultiReceiveList (Cons ch chs) prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' res

class RecvOrOffer cur inc cur' inc'
instance RecvOrOffer (Recv (sp, t)) t cur' inc'
instance RecvOrOffer (Offer lst) (Choice lst) Nil Nil

(~|||~) :: ( MapLookup (TyMap keyToIdx idxToValue) ch (SessionState progS progS' ((Cons cur nxt), fromO, (Cons inc nxt')))
           , RecvOrOffer cur inc nxt nxt'
           ) =>
           (ch, InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') res) ->
           MultiReceiveList chs prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' res ->
           MultiReceiveList (Cons ch chs) prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' res
(~|||~) (ch, func) nxt = MultiReceiveCons (ch, func) nxt
infixr 5 ~|||~

-- COMBINED
class CombinedMultiRecv chs prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' idxs result where
    combinedMultiRecv :: (MultiReceiveList chs prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' result) ->
                         idxs -> [InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs)
                                                   (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') result
                                 ] ->
                         InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') result

instance
    ( MapSelectToList (TyMap keyToIdx idxToValue) idxs sessions
    , TyListToList sessions [SessionState prog'' prog''' from]
    , TyListLength chs len
    , SmallerThanBool (D0 E) len True
    , SetIncomingNotify (TyMap keyToIdx idxToValue) chs
    , TypeNumberToInt len
    ) =>
    CombinedMultiRecv chs prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' idxs result where
      combinedMultiRecv tuples idxs funcs =
          InterleavedChain $ \ipid mp ->
              do { notifyChan <- newChan
                 ; let sessions :: sessions = mapSelectToList mp idxs
                       plain = zip (tyListToList sessions) funcs
                 ; chMaybe <- setIncomingNotify notifyChan mp 0 chs
                 ; case chMaybe of
                     Nothing ->
                         do { chMaybe' <- setIncomingNotifyPlain notifyChan chsCount plain -- so the plain starts after the multi
                            ; case chMaybe' of
                                Nothing -> blockOnNotifyChanCombined chs chsCount ipid notifyChan mp tuples plain
                                (Just idx) -> do { unsetIncomingNotify mp chs chsCount
                                                 ; unsetIncomingNotifyPlain idx plain
                                                 ; runInterleavedChain (snd $ plain !! idx) ipid mp
                                                 }
                            }
                     (Just idx) ->
                         do { unsetIncomingNotify mp chs idx
                            ; runInterleavedChain (walkMultiReceives idx tuples) ipid mp
                            }
                 }
              where
                chs = undefined :: chs
                chsCount = tyNumToInt . tyListLength $ chs

blockOnNotifyChanCombined
    :: (SetIncomingNotify (TyMap keyToIdx idxToValue) idxs) =>
       idxs -> Int -> InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs ->
       Chan Int -> TyMap keyToIdx idxToValue ->
       MultiReceiveList chs prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' res ->
       [( SessionState prog'' prog''' from
        , InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs)
          (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') res
        )] ->
       IO (res, TyMap keyToIdx' idxToValue',
           InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs)
blockOnNotifyChanCombined chs chsCount ipid chan mp multiFunctions plainFunctions
    = do { idx <- readChan chan
         ; unsetIncomingNotify mp chs chsCount
         ; unsetIncomingNotifyPlain plainLength plainFunctions
         ; case idx < chsCount of
             True -> runInterleavedChain (walkMultiReceives idx multiFunctions) ipid mp
             _ -> runInterleavedChain (snd $ plainFunctions !! (idx - chsCount)) ipid mp
         }
    where
      plainLength = length plainFunctions

-- PLAIN
class PlainMultiReceive idxs pid mp mp' result  where
    plainMultiReceive :: idxs ->
                         [InterleavedChain pid mp mp' result] ->
                         InterleavedChain pid mp mp' result

instance forall prog prog' prog'' prog''' idxs funcs keyToIdx idxToValue keyToIdx' idxToValue' functions sessions from invertedSessions sessionsToIdx idxsToPairStructs result .
    ( MapSelectToList (TyMap keyToIdx idxToValue) idxs sessions
    , TyListToList sessions [SessionState prog'' prog''' from]
    , MultiReceive [( SessionState prog'' prog''' from
                    , InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') result
                    )]
                       (InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') result)
    ) =>
    PlainMultiReceive idxs (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') result where
        plainMultiReceive idxs funcs =
            InterleavedChain $ \ipid mp ->
                let sessions :: sessions = mapSelectToList mp idxs
                    functions = zip (tyListToList sessions) funcs
                in runInterleavedChain (multiReceive functions) ipid mp

-- RICH
class MultiReceive lst chain | lst -> chain where
    multiReceive :: lst -> chain

instance forall prog prog' prog'' prog''' from invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' result .
    MultiReceive
    [( SessionState prog'' prog''' from
     , InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') result
     )]
    (InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') result) where
        multiReceive functions =
            InterleavedChain $ \ipid mp ->
                do { notifyChan <- newChan
                   ; chMaybe <- setIncomingNotifyPlain notifyChan 0 functions
                   ; case chMaybe of
                       (Just idx) -> do { unsetIncomingNotifyPlain idx functions
                                        ; runInterleavedChain (snd $ (functions !! idx)) ipid mp
                                        }
                       Nothing -> blockOnNotifyChanPlain ipid notifyChan mp functions
                   }

setIncomingNotifyPlain ::
    Chan Int -> Int ->
    [( SessionState prog'' prog''' from
     , InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') result
     )] ->
    IO (Maybe Int)
setIncomingNotifyPlain _ _ [] = return Nothing
setIncomingNotifyPlain chan n (((SessionState _ _ _ _ _ inNotifyMVar incoming),_):rest)
    = do { empty <- isEmptyMVar incoming
         ; when empty $ putMVar inNotifyMVar (writeChan chan n)
         ; empty <- isEmptyMVar incoming
         ; if empty
           then setIncomingNotifyPlain chan (n+1) rest
           else return $ Just n
         }

unsetIncomingNotifyPlain ::
    Int -> 
    [( SessionState prog'' prog''' from
     , InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') rest
     )] ->
    IO ()
unsetIncomingNotifyPlain _ [] = return ()
unsetIncomingNotifyPlain 0 _  = return ()
unsetIncomingNotifyPlain n (((SessionState _ _ _ _ _ inNotifyMVar _),_):rest)
    = do { tryTakeMVar inNotifyMVar
         ; unsetIncomingNotifyPlain (n-1) rest
         }

blockOnNotifyChanPlain ::
    InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs ->
    Chan Int -> TyMap keyToIdx idxToValue ->
    [( SessionState prog'' prog''' from
     , InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs)
       (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') result
     )] ->
    IO (result, TyMap keyToIdx' idxToValue', InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs)
blockOnNotifyChanPlain ipid chan mp functions
    = do { idx <- readChan chan
         ; unsetIncomingNotifyPlain (length functions) functions
         ; runInterleavedChain (snd $ functions !! idx) ipid mp
         }

instance forall chs len keyToIdx idxToValue prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx' idxToValue' res .
                ( TyListLength chs len
                , SmallerThanBool (D0 E) len True
                , SetIncomingNotify (TyMap keyToIdx idxToValue) chs
                , TypeNumberToInt len
                ) =>
    MultiReceive (MultiReceiveList chs prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' res) (InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') res) where
        multiReceive functions =
            InterleavedChain $ \ipid mp ->
                do { notifyChan <- newChan
                   ; chMaybe <- setIncomingNotify notifyChan mp 0 chs
                   ; case chMaybe of
                       (Just idx) -> do { unsetIncomingNotify mp chs idx
                                        ; runInterleavedChain (walkMultiReceives idx functions) ipid mp
                                        }
                       Nothing -> blockOnNotifyChanMulti chs chsCount ipid notifyChan mp functions
                   }
                where
                  chs = undefined :: chs
                  chsCount = tyNumToInt . tyListLength $ chs

blockOnNotifyChanMulti
    :: (SetIncomingNotify (TyMap keyToIdx idxToValue) idxs) =>
       idxs -> Int -> InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs ->
       Chan Int -> TyMap keyToIdx idxToValue ->
       MultiReceiveList chs prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' res ->
       IO (res, TyMap keyToIdx' idxToValue',
           InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs)
blockOnNotifyChanMulti chs chsCount ipid chan mp functions
    = do { idx <- readChan chan
         ; unsetIncomingNotify mp chs chsCount
         ; runInterleavedChain (walkMultiReceives idx functions) ipid mp
         }

class SetIncomingNotify mp idxs where
    setIncomingNotify :: Chan Int -> mp -> Int -> idxs -> IO (Maybe Int)
    unsetIncomingNotify :: mp -> idxs -> Int -> IO ()
instance SetIncomingNotify mp Nil where
    setIncomingNotify _ _ _ _ = return Nothing
    unsetIncomingNotify _ _ _ = return ()
instance forall keyToIdx idxToValue idx prog prog' current fromO fromI nxt .
         ( MapLookup (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (current, fromO, fromI))
         , SetIncomingNotify (TyMap keyToIdx idxToValue) nxt
         , TypeNumberToInt idx
         ) =>
    SetIncomingNotify (TyMap keyToIdx idxToValue) (Cons idx nxt) where
        setIncomingNotify chan mp acc idxs
            = do { let st = mapLookup mp idx
                 ; isEmpty <- setIncomingNotify' chan st
                 ; if isEmpty
                   then setIncomingNotify chan mp (acc+1) idxs'
                   else return . return $ acc
                 }
              where
                idxs' = tyTail idxs
                idx = tyHead idxs
                setIncomingNotify' :: Chan Int -> SessionState prog prog' (current, fromO, fromI) -> IO Bool
                setIncomingNotify' chan (SessionState _ _ _ _ _ inNotifyMVar incoming)
                    = do { empty <- isEmptyMVar incoming -- 0. Test first. The producer may be a long way ahead
                         ; when empty $ do { putMVar inNotifyMVar (writeChan chan acc)
                                           }
                         ; isEmptyMVar incoming
                         }
        unsetIncomingNotify _ _ (-1)
            = return ()
        unsetIncomingNotify mp idxs count
            = do { let st = mapLookup mp idx
                 ; unsetIncomingNotify' st
                 ; unsetIncomingNotify mp idxs' (count - 1)
                 }
            where
              idx = tyHead idxs
              idxs' = tyTail idxs
              unsetIncomingNotify' :: SessionState prog prog' (current, fromO, fromI) -> IO ()
              unsetIncomingNotify' (SessionState _ _ _ _ _ inNotifyMVar _)
                  = do { tryTakeMVar inNotifyMVar
                       ; return ()
                       }

class WalkMultiReceives chs prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' res where
    walkMultiReceives :: Int -> MultiReceiveList chs prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' res ->
                         InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') res

instance WalkMultiReceives chs prog prog' invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue keyToIdx' idxToValue' res where
        walkMultiReceives 0 (MultiReceiveCons (_, func) _)
            = func
        walkMultiReceives n (MultiReceiveCons _ nxt) = walkMultiReceives (n - 1) nxt
        walkMultiReceives _ _ = error "The Truly Impossible Happened."
