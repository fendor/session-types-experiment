{-# LANGUAGE FlexibleContexts
           , ScopedTypeVariables
           , PatternSignatures
           , TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances #-}

{-
    Interleaving.hs
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


-- | A single thread should be able to have multiple concurrent
-- conversations with other threads. So this module places
-- 'InterleavedChain' as an instance of 'SMonad', and equipts it with
-- the capability to manage and modify a mapping of channels to other
-- threads. Note there is some danger in here. Deadlocks can start to
-- appear if you're silly or deliberately mean. However, this is
-- starting to get towards the Actor / Erlang model of message
-- passing.

module Control.Concurrent.Session.Interleaving
    ( withChannel
    , withChannelRec
    , MapChannelsRec (..)
    , Fork(..)
    , runInterleaved
    , sjumpCh
    , ssendCh
    , srecvCh
    , sofferCh
    , sselectCh
    , scloseCh
    , withThenClose
    , createSessionThenClose
    , forkThenClose
    , sendChannel
    , recvChannel
    , (<!>)
    , (<?>)
    ) where

import Control.Concurrent.Session.Base.Bool
import Control.Concurrent.Session.Base.Number
import Control.Concurrent.Session.Base.List
import Control.Concurrent.Session.Base.Map
import Control.Concurrent.Session.Base.SMonad
import Control.Concurrent.Session.SessionType
import Control.Concurrent.Session.Types
import Control.Concurrent.Session.Runtime
import Control.Concurrent.Session.Pid
import Control.Concurrent
import Control.Monad

-- | Perform the given actions on the given channel. Note that the
-- value emitted by the actions will be passed out.
withChannel :: ( MapLookup (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (current, fromO, fromI))
               , MapUpdate (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (current', toO, toI)) (TyMap keyToIdx idxToValue')
               , MapDelete (TyMap keyToIdx idxToValue) idx (TyMap keyToIdx'' idxToValue'')
               , MapDelete (TyMap keyToIdx idxToValue') idx (TyMap keyToIdx'' idxToValue'')
               ) =>
    idx -> SessionChain prog prog' (current, fromO, fromI) (current', toO, toI) res ->
    InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx idxToValue') res
withChannel idx chain
    = InterleavedChain $
      \p mp -> do { let st = mapLookup mp idx
                  ; (res, st') <- runSessionChain chain st
                  ; let mp' = mapUpdate mp idx st'
                  ; return (res, mp', p)
                  }

withChannelRec :: ( MapLookup (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (current, fromO, fromI))
                  ) =>
    idx -> SessionChain prog prog' (current, fromO, fromI) (current, fromO, fromI) res ->
    InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx idxToValue) res
withChannelRec idx chain
    = InterleavedChain $
      \p mp -> do { let st = mapLookup mp idx
                  ; (res, st') <- runSessionChain chain st
                  ; let mp' = mapUpdateValue mp idx st'
                  ; return (res, mp', p)
                  }

class MapChannelsRec idxs prog prog' current from to res keyToIdx idxToValue pid where
    mapChannelsRec :: SessionChain prog prog' (current, from, to) (current, from, to) res -> idxs ->
                      InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx idxToValue) [res]

instance MapChannelsRec Nil prog prog' current from to res keyToIdx idxToValue pid where
    mapChannelsRec _ _ = sreturn []

instance ( MapChannelsRec idxs prog prog' current from to res keyToIdx idxToValue pid
         , MapLookup (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (current, from, to))
         ) =>
    MapChannelsRec (Cons idx idxs) prog prog' current from to res keyToIdx idxToValue pid where
        mapChannelsRec f idxs = withChannelRec idx f ~>>= \res ->
                                mapChannelsRec f idxs' ~>>= \ress ->
                                sreturn (res : ress)
            where
              idx = tyHead idxs
              idxs' = tyTail idxs

-- | Think of this as /spawn/ or /fork/ - it creates a child thread
-- which must be prepared to communicate with you. You get a channel
-- set up to the child which is emitted by this function. The child is
-- first told about the channel back to you and your Pid. The child
-- can go off and do what ever it wants, including creating additional
-- channels. The child starts off knowing with only one open channel
-- which is to the parent.
class Fork invert init sessionsList prog prog'
           sessionsToIdxThem idxsToPairStructsThem sessionsToIdxMe idxsToPairStructsMe
           fromO fromI progOut progIn keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe'
           keyToIdxChild' idxToValueChild' keyToIdxChild'' idxToValueChild'' idxOfChild
           current current' currentUX currentUX' invertedSessionsMe invertedSessionsThem where
    fork :: init -> invert -> sessionsList ->
            ((D0 E) -> Pid prog prog' invertedSessionsMe sessionsToIdxMe idxsToPairStructsMe ->
             InterleavedChain (InternalPid prog prog' invertedSessionsThem sessionsToIdxThem idxsToPairStructsThem)
                              (TyMap keyToIdxChild' idxToValueChild') (TyMap keyToIdxChild'' idxToValueChild'')
                              ()) ->
            InterleavedChain (InternalPid prog prog' invertedSessionsMe sessionsToIdxMe idxsToPairStructsMe)
                             (TyMap keyToIdxMe idxToValueMe) (TyMap keyToIdxMe' idxToValueMe')
                             (idxOfChild, Pid prog prog' invertedSessionsThem sessionsToIdxThem idxsToPairStructsThem)

instance forall prog prog' progOut progIn init
    fromO fromI sessionsList sessionsListSorted sessionsListSortedRev current current' currentUX currentUX'
    keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe'
    keyToIdxChild' idxToValueChild' keyToIdxChild'' idxToValueChild''
    idxOfChild sessionsToIdxMe idxsToPairStructsMe sessionsToIdxThem idxsToPairStructsThem invertedSessionsMe invertedSessionsThem .
    ( ProgramToMVarsOutgoingT prog prog ~ progOut
    , ProgramToMVarsOutgoingT prog' prog' ~ progIn
    , ProgramToMVarsOutgoing prog prog progOut
    , ProgramToMVarsOutgoing prog' prog' progIn
    , SWellFormedConfig init (D0 E) prog
    , SWellFormedConfig init (D0 E) prog'
    , TyListIndex progOut init (MVar (ProgramCell (Cell fromO)))
    , TyListIndex progIn init (MVar (ProgramCell (Cell fromI)))
    , TyListIndex prog init currentUX
    , TyListIndex prog' init currentUX'
    , Expand prog currentUX current
    , Expand prog' currentUX' current'
    , BuildPidTyMap prog sessionsListSorted (TyMap sessionsToIdxThem idxsToPairStructsThem)
    , BuildInvertedSessionsSet sessionsListSorted invertedSessionsThem
    , TyListSortNums sessionsList sessionsListSortedRev
    , TyListReverse sessionsListSortedRev sessionsListSorted
    , MapSize (TyMap keyToIdxMe idxToValueMe) idxOfChild
    , MapInsert (TyMap keyToIdxMe idxToValueMe) idxOfChild
                    (SessionState prog prog' (current, fromO, fromI)) (TyMap keyToIdxMe' idxToValueMe')
    , MapInsert (TyMap Nil Nil) (D0 E)
                    (SessionState prog' prog (current', fromI, fromO)) (TyMap keyToIdxChild' idxToValueChild')
    ) =>
    Fork False init sessionsList prog prog'
         sessionsToIdxThem idxsToPairStructsThem sessionsToIdxMe idxsToPairStructsMe
         fromO fromI progOut progIn keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe'
         keyToIdxChild' idxToValueChild' keyToIdxChild'' idxToValueChild'' idxOfChild
         current current' currentUX currentUX' invertedSessionsMe invertedSessionsThem where
             fork _ FF sessionsList child =
                 InterleavedChain $
                     \localPid@(IPid localPPid@(Pid _ _) _) mp ->
                         do { mvarsOut <- programToMVarsOutgoing prog prog
                            ; mvarsIn <- programToMVarsOutgoing prog' prog'
                            ; aNotify <- newEmptyMVar
                            ; bNotify <- newEmptyMVar
                            ; ((), (childST :: SessionState prog' prog (current', fromI, fromO)))
                                <- runSessionChain (sjump :: SessionChain prog' prog
                                                    ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)) (current', fromI, fromO) ())
                                                    (SessionState mvarsIn mvarsOut undefined aNotify undefined bNotify undefined)
                            ; childSessions <- buildPidTyMap prog sortedSessions
                            ; let childMap :: TyMap keyToIdxChild' idxToValueChild' = mapInsert (D0 E) childST emptyMap
                                  (localPid', childPid) = makePid localPid invertedSessions childSessions
                            ; forkIO $ runInterleavedChain (child (D0 E) localPPid) childPid childMap >> return ()
                            ; ((), (myST :: SessionState prog prog' (current, fromO, fromI)))
                                <- runSessionChain (sjump :: SessionChain prog prog'
                                                    ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)) (current, fromO, fromI) ())
                                                    (SessionState mvarsOut mvarsIn undefined bNotify undefined aNotify undefined)
                            ; let idxOfChild :: idxOfChild = mapSize mp
                            ; return ((idxOfChild, iPidToPid childPid), mapInsert idxOfChild myST mp, localPid')
                            }
                         where
                           prog = undefined::prog
                           prog' = undefined::prog'
                           sortedSessions = tyListReverse . tyListSortNums $ sessionsList
                           invertedSessions = buildInvertedSessionsSet sortedSessions

instance forall prog prog' progOut progIn init
    fromO fromI sessionsList sessionsListSorted sessionsListSortedRev current current' currentUX currentUX'
    keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe'
    keyToIdxChild' idxToValueChild' keyToIdxChild'' idxToValueChild''
    idxOfChild sessionsToIdxMe idxsToPairStructsMe sessionsToIdxThem idxsToPairStructsThem invertedSessionsMe invertedSessionsThem .
    ( ProgramToMVarsOutgoingT prog prog ~ progOut
    , ProgramToMVarsOutgoingT prog' prog' ~ progIn
    , ProgramToMVarsOutgoing prog prog progOut
    , ProgramToMVarsOutgoing prog' prog' progIn
    , SWellFormedConfig init (D0 E) prog
    , SWellFormedConfig init (D0 E) prog'
    , TyListIndex progOut init (MVar (ProgramCell (Cell fromO)))
    , TyListIndex progIn init (MVar (ProgramCell (Cell fromI)))
    , TyListIndex prog init currentUX
    , TyListIndex prog' init currentUX'
    , Expand prog currentUX current
    , Expand prog' currentUX' current'
    , BuildPidTyMap prog sessionsListSorted (TyMap sessionsToIdxThem idxsToPairStructsThem)
    , BuildInvertedSessionsSet sessionsListSorted invertedSessionsThem
    , TyListSortNums sessionsList sessionsListSortedRev
    , TyListReverse sessionsListSortedRev sessionsListSorted
    , MapSize (TyMap keyToIdxMe idxToValueMe) idxOfChild
    , MapInsert (TyMap keyToIdxMe idxToValueMe) idxOfChild
                    (SessionState prog' prog (current', fromI, fromO)) (TyMap keyToIdxMe' idxToValueMe')
    , MapInsert (TyMap Nil Nil) (D0 E)
                    (SessionState prog prog' (current, fromO, fromI)) (TyMap keyToIdxChild' idxToValueChild')
    ) =>
    Fork True init sessionsList prog prog'
         sessionsToIdxThem idxsToPairStructsThem sessionsToIdxMe idxsToPairStructsMe
         fromO fromI progOut progIn keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe'
         keyToIdxChild' idxToValueChild' keyToIdxChild'' idxToValueChild'' idxOfChild
         current current' currentUX currentUX' invertedSessionsMe invertedSessionsThem where
             fork _ TT sessionsList child =
                 InterleavedChain $
                     \localPid@(IPid localPPid@(Pid _ _) _) mp ->
                         do { mvarsOut <- programToMVarsOutgoing prog prog
                            ; mvarsIn <- programToMVarsOutgoing prog' prog'
                            ; aNotify <- newEmptyMVar
                            ; bNotify <- newEmptyMVar
                            ; ((), (childST :: SessionState prog prog' (current, fromO, fromI)))
                                <- runSessionChain (sjump :: SessionChain prog prog'
                                                    ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)) (current, fromO, fromI) ())
                                                    (SessionState mvarsOut mvarsIn undefined aNotify undefined bNotify undefined)
                            ; childSessions <- buildPidTyMap prog sortedSessions
                            ; let childMap :: TyMap keyToIdxChild' idxToValueChild' = mapInsert (D0 E) childST emptyMap
                                  (localPid', childPid) = makePid localPid invertedSessions childSessions
                            ; forkIO $ runInterleavedChain (child (D0 E) localPPid) childPid childMap >> return ()
                            ; ((), (myST :: SessionState prog' prog (current', fromI, fromO)))
                                <- runSessionChain (sjump :: SessionChain prog' prog
                                                    ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)) (current', fromI, fromO) ())
                                                    (SessionState mvarsIn mvarsOut undefined bNotify undefined aNotify undefined)
                            ; let idxOfChild :: idxOfChild = mapSize mp
                            ; return ((idxOfChild, iPidToPid childPid), mapInsert idxOfChild myST mp, localPid')
                            }
                         where
                           prog = undefined::prog
                           prog' = undefined::prog'
                           sortedSessions = tyListReverse . tyListSortNums $ sessionsList
                           invertedSessions = buildInvertedSessionsSet sortedSessions

-- | Run the root. Use this to start up a family from a single root.
runInterleaved :: forall prog prog' sessionsList sessionsListSorted sessionsListSortedRev invertedSessions sessionsToIdx idxsToPairStructs keyToIdx idxToValue res .
    ( BuildPidTyMap prog sessionsListSorted (TyMap sessionsToIdx idxsToPairStructs)
    , TyListSortNums sessionsList sessionsListSortedRev
    , TyListReverse sessionsListSortedRev sessionsListSorted
    , BuildInvertedSessionsSet sessionsListSorted invertedSessions
    , Dual prog prog'
    , DualT prog ~ prog'
    ) =>
    sessionsList -> prog ->
    InterleavedChain (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs)
                         (TyMap Nil Nil) (TyMap keyToIdx idxToValue) res ->
    IO res
runInterleaved sessions _ ic
    = do { tyMap <- buildPidTyMap prog sortedSessions
         ; runInterleavedChain ic (rootPid tyMap invertedSessions prog) emptyMap >>= \(res, _, _) -> return res
         }
    where
      prog = undefined::prog
      sortedSessions = tyListReverse . tyListSortNums $ sessions
      invertedSessions = buildInvertedSessionsSet sortedSessions

-- | Convenience combination of 'withChannel' and 'sjump'
sjumpCh :: ( ProgramToMVarsOutgoingT prog prog ~ progOut
           , ProgramToMVarsOutgoingT prog' prog' ~ progIn
           , SWellFormedConfig l (D0 E) prog
           , SWellFormedConfig l (D0 E) prog'
           , TyListIndex progOut l (MVar (ProgramCell (Cell outgoing)))
           , TyListIndex progIn l (MVar (ProgramCell (Cell incoming)))
           , TyListIndex prog l current'
           , Expand prog current' current
           , MapLookup (TyMap keyToIdx idxToValue) idx (SessionState prog prog' ((Cons (Jump l) Nil), (Cons (Jump l) Nil), (Cons (Jump l) Nil)))
           , MapUpdate (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (current, outgoing, incoming)) (TyMap keyToIdx idxToValue')
           , MapDelete (TyMap keyToIdx idxToValue) idx (TyMap keyToIdx'' idxToValue'')
           , MapDelete (TyMap keyToIdx idxToValue') idx (TyMap keyToIdx'' idxToValue'')
           ) =>
    idx -> InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx idxToValue') ()
sjumpCh ch = withChannel ch sjump

-- | Convenience combination of 'withChannel' and 'ssend'
ssendCh :: forall prog prog' sp t t' idx keyToIdx idxToValue nxt nxt' incoming idxToValue' keyToIdx'' idxToValue''
                  pid .
           ( MapLookup (TyMap keyToIdx idxToValue) idx (SessionState prog prog' ((Cons (Send (sp, t)) nxt), (Cons t nxt'), incoming))
           , MapUpdate (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (nxt, nxt', incoming)) (TyMap keyToIdx idxToValue')
           , MapDelete (TyMap keyToIdx idxToValue) idx (TyMap keyToIdx'' idxToValue'')
           , MapDelete (TyMap keyToIdx idxToValue') idx (TyMap keyToIdx'' idxToValue'')
           , CompatibleTypes sp t' t
           ) =>
    idx -> t' -> InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx idxToValue') ()
ssendCh ch t' = withChannel ch ((ssend t') :: SessionChain prog prog' (Cons (Send (sp, t)) nxt, Cons t nxt', incoming) (nxt, nxt', incoming) ())

(<!>) :: forall prog prog' sp t t' idx keyToIdx idxToValue nxt nxt' incoming idxToValue' keyToIdx'' idxToValue''
                  pid .
         ( MapLookup (TyMap keyToIdx idxToValue) idx (SessionState prog prog' ((Cons (Send (sp, t)) nxt), (Cons t nxt'), incoming))
         , MapUpdate (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (nxt, nxt', incoming)) (TyMap keyToIdx idxToValue')
         , MapDelete (TyMap keyToIdx idxToValue) idx (TyMap keyToIdx'' idxToValue'')
         , MapDelete (TyMap keyToIdx idxToValue') idx (TyMap keyToIdx'' idxToValue'')
         , CompatibleTypes sp t' t
         ) =>
    idx -> t' -> InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx idxToValue') ()
(<!>) = ssendCh

-- | Convenience combination of 'withChannel' and 'srecv'
srecvCh :: ( MapLookup (TyMap keyToIdx idxToValue) idx (SessionState prog prog' ((Cons (Recv (sp, t)) nxt), outgoing, (Cons t nxt')))
           , MapUpdate (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (nxt, outgoing, nxt')) (TyMap keyToIdx idxToValue')
           , MapDelete (TyMap keyToIdx idxToValue) idx (TyMap keyToIdx'' idxToValue'')
           , MapDelete (TyMap keyToIdx idxToValue') idx (TyMap keyToIdx'' idxToValue'')
           ) =>
    idx -> InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx idxToValue') t
srecvCh ch = withChannel ch (srecv)

(<?>) :: ( MapLookup (TyMap keyToIdx idxToValue) idx (SessionState prog prog' ((Cons (Recv (sp, t)) nxt), outgoing, (Cons t nxt')))
         , MapUpdate (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (nxt, outgoing, nxt')) (TyMap keyToIdx idxToValue')
         , MapDelete (TyMap keyToIdx idxToValue) idx (TyMap keyToIdx'' idxToValue'')
         , MapDelete (TyMap keyToIdx idxToValue') idx (TyMap keyToIdx'' idxToValue'')
         ) =>
    idx -> InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx idxToValue') t
(<?>) = srecvCh

-- | Convenience combination of 'withChannel' and 'soffer'
sofferCh :: ( MapLookup (TyMap keyToIdx idxToValue) idx (SessionState prog prog' ((Cons (Offer jumps) Nil), Cons (Choice jumps) Nil, Cons (Choice jumps) Nil))
            , MapUpdate (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (current, outgoing, incoming)) (TyMap keyToIdx idxToValue')
            , MapDelete (TyMap keyToIdx idxToValue) idx (TyMap keyToIdx'' idxToValue'')
            , MapDelete (TyMap keyToIdx idxToValue') idx (TyMap keyToIdx'' idxToValue'')
            ) =>
    idx -> OfferImpls jumps prog prog' (current, outgoing, incoming) finalResult ->
    InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx idxToValue') finalResult
sofferCh ch offerImpls = withChannel ch (soffer offerImpls)

-- | Convenience combination of 'withChannel' and 'sselectCh'
sselectCh :: ( ProgramToMVarsOutgoingT prog prog ~ progOut
             , ProgramToMVarsOutgoingT prog' prog' ~ progIn
             , MapLookup (TyMap keyToIdx idxToValue) idx (SessionState prog prog' ((Cons (Select jumps) Nil), Cons (Choice jumps) Nil, Cons (Choice jumps) Nil))
             , MapUpdate (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (current, outgoing, incoming)) (TyMap keyToIdx idxToValue')
             , TyListLength jumps len
             , SmallerThanBool label len True
             , TypeNumberToInt label
             , TyListIndex jumps label (Cons (Jump jumpTarget) Nil)
             , SWellFormedConfig jumpTarget (D0 E) prog
             , SWellFormedConfig jumpTarget (D0 E) prog'
             , TyListIndex progOut jumpTarget (MVar (ProgramCell (Cell outgoing)))
             , TyListIndex progIn jumpTarget (MVar (ProgramCell (Cell incoming)))
             , TyListIndex prog jumpTarget currentUX
             , Expand prog currentUX current
             , MapDelete (TyMap keyToIdx idxToValue) idx (TyMap keyToIdx'' idxToValue'')
             , MapDelete (TyMap keyToIdx idxToValue') idx (TyMap keyToIdx'' idxToValue'')
             ) =>
    idx -> label -> InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx idxToValue') ()
sselectCh ch b = withChannel ch (sselect b)

scloseCh :: ( MapDelete (TyMap keyToIdx idxToValue) ch (TyMap keyToIdx' idxToValue')
            , MapLookup (TyMap keyToIdx idxToValue) ch (SessionState prog prog' (Cons End Nil, outgoing, incoming))
            ) =>
    ch -> InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue') ()
scloseCh ch = InterleavedChain $
              \localPid mp -> return ((), mapDelete mp ch, localPid)

withThenClose ch f = withChannel ch f ~>>= \r -> scloseCh ch ~>> sreturn r
createSessionThenClose idx dualSess pid f = createSession idx dualSess pid ~>>= \ch -> withThenClose ch f
forkThenClose idx dualSess childSess child f = fork idx dualSess childSess child ~>>= \(ch, pid) -> withThenClose ch f ~>>= \r -> sreturn (r, pid)

sendChannel :: ( MapLookup (TyMap keyToIdx idxToValue) chToSend (SessionState prog prog' (current, outgoing, incoming))
               , MapLookup (TyMap keyToIdx idxToValue) chOnWhichToSend (SessionState prog'' prog''' (Cons (Send (SpecialSession, (SessionState prog prog' (current, outgoing, incoming)))) nxt, Cons (SessionState prog prog' (current, outgoing, incoming)) nxt', incoming'))
               , MapUpdate (TyMap keyToIdx idxToValue) chOnWhichToSend (SessionState prog'' prog''' (nxt, nxt', incoming')) (TyMap keyToIdx idxToValue')
               , MapDelete (TyMap keyToIdx idxToValue') chToSend (TyMap keyToIdx' idxToValue'')
               ) =>
    chToSend -> chOnWhichToSend ->
    InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue'') ()
sendChannel chToSend chOnWhichToSend
    = InterleavedChain $
      \p mp -> do { let stToSend = mapLookup mp chToSend
                        stOnWhichToSend = mapLookup mp chOnWhichToSend
                  ; ((), stOnWhichToSend') <- runSessionChain (ssend stToSend) stOnWhichToSend
                  ; let mp' = mapUpdate mp chOnWhichToSend stOnWhichToSend'
                        mp'' = mapDelete mp' chToSend
                  ; return ((), mp'', p)
                  }

recvChannel :: ( MapLookup (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (Cons (Recv (SpecialSession, (SessionState prog'' prog''' (current', outgoing', incoming')))) nxt, outgoing, Cons (SessionState prog'' prog''' (current', outgoing', incoming')) nxt'))
               , MapUpdate (TyMap keyToIdx idxToValue) idx (SessionState prog prog' (nxt, outgoing, nxt')) (TyMap keyToIdx idxToValue')
               , MapSize (TyMap keyToIdx idxToValue') idx'
               , MapInsert (TyMap keyToIdx idxToValue') idx' (SessionState prog'' prog''' (current', outgoing', incoming')) (TyMap keyToIdx' idxToValue'')
               ) =>
    idx -> InterleavedChain pid (TyMap keyToIdx idxToValue) (TyMap keyToIdx' idxToValue'') idx'
recvChannel idx
    = InterleavedChain f
    where
      f p mp = do { let stOnWhichToRecv = mapLookup mp idx
                  ; (stToRecv, stOnWhichToRecv') <- runSessionChain srecv stOnWhichToRecv
                  ; let mp' = mapUpdate mp idx stOnWhichToRecv'
                        idx' = mapSize mp'
                        mp'' = mapInsert idx' stToRecv mp'
                  ; return (idx', mp'', p)
                  }
