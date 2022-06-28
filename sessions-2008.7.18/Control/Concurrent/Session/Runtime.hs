{-# LANGUAGE KindSignatures
           , ScopedTypeVariables
           , GADTs
           , MultiParamTypeClasses
           , FunctionalDependencies
           , UndecidableInstances
           , FlexibleInstances
           , FlexibleContexts
           , TypeFamilies
           , PatternSignatures #-}

{-
    Runtime.hs
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

-- | Having actually described a session type, you'll now want to
-- implement it! Use the methods of 'SMonad' to chain functions
-- together.

module Control.Concurrent.Session.Runtime
    ( OfferImpls (OfferImplsNil)
    , (~||~)
    , sjump
    , ssend
    , CompatibleTypes (..)
    , srecv
    , srecvTest
    , srecvTestTimeOut
    , soffer
    , sselect
    , run
    , carefullySwapToNextCell
    ) where

import Control.Concurrent.Session.Base.Bool
import Control.Concurrent.Session.Base.List
import Control.Concurrent.Session.Base.Number
import Control.Concurrent.Session.Base.Map
import Control.Concurrent.Session.Base.SMonad
import Control.Concurrent.Session.SessionType
import Control.Concurrent.Session.Types
import Control.Concurrent
import Control.Monad

-- | Use OfferImpls to construct the implementations of the branches
-- of an offer. Really, it's just a slightly fancy list.
data OfferImpls :: * -> * -> * -> * -> * -> * where
                   OfferImplsNil :: OfferImpls Nil prog prog' finalState finalResult
                   OfferCons :: (SessionChain prog prog' (Cons (Jump l) Nil, Cons (Jump l) Nil, Cons (Jump l) Nil) finalState finalResult) ->
                            OfferImpls jumps prog prog' finalState finalResult ->
                            OfferImpls (Cons (Cons (Jump l) Nil) jumps) prog prog' finalState finalResult

-- | Use to construct OfferImpls. This function automatically adds the
-- necessary 'sjump' to the start of each branch implementation.
(~||~) :: forall prog prog' progOut progIn outgoing incoming finalState finalResult jumps l current currentUX .
          ( (ProgramToMVarsOutgoingT prog prog) ~ progOut
          , (ProgramToMVarsOutgoingT prog' prog') ~ progIn
          , ProgramToMVarsOutgoing prog prog progOut
          , ProgramToMVarsOutgoing prog' prog' progIn
          , SWellFormedConfig l (D0 E) prog
          , SWellFormedConfig l (D0 E) prog'
          , TyListIndex progOut l (MVar (ProgramCell (Cell outgoing)))
          , TyListIndex progIn l (MVar (ProgramCell (Cell incoming)))
          , TyListIndex prog l currentUX
          , Expand prog currentUX current
          ) =>
          (SessionChain prog prog' (current, outgoing, incoming) finalState finalResult) ->
          (OfferImpls jumps prog prog' finalState finalResult) ->
          (OfferImpls (Cons (Cons (Jump l) Nil) jumps) prog prog' finalState finalResult)
(~||~) chain nxt = OfferCons chain' nxt
    where
      chain' :: SessionChain prog prog' ((Cons (Jump l) Nil), (Cons (Jump l) Nil), (Cons (Jump l) Nil)) finalState finalResult
      chain' = sjump ~>> chain
infixr 5 ~||~

class WalkOfferImpls prog prog' finalState finalResult where
    walkOfferImpls :: Int -> OfferImpls jumps prog prog' finalState finalResult -> SessionChain prog prog' from finalState finalResult
instance forall prog prog' finalState finalResult .
    WalkOfferImpls prog prog' finalState finalResult where
        walkOfferImpls 0 (OfferCons chain _) = SessionChain f
            where
              f :: forall from .
                   SessionState prog prog' from ->
                   IO (finalResult, SessionState prog prog' finalState)
              f (SessionState outgoingProg incomingProg _ outNotify _ inNotify _)
                  = runSessionChain chain (SessionState outgoingProg incomingProg undefined outNotify undefined inNotify undefined)
        walkOfferImpls n (OfferCons _ rest) = walkOfferImpls (n - 1) rest
        walkOfferImpls _ _ = error "The Truly Impossible Happened."

instance ( Expand prog nxt nxt'
         , ExpandSession prog (SendSession invert frag) expandedSendSession
         ) =>
    Expand prog (Cons (SendSession invert frag) nxt) (Cons expandedSendSession nxt') where
        type ExpandT prog (Cons (SendSession invert frag) nxt) = Cons (ExpandSessionT prog (SendSession invert frag)) (ExpandT prog nxt)

instance ( Expand prog nxt nxt'
         , ExpandSession prog (RecvSession invert frag) expandedRecvSession
         ) =>
    Expand prog (Cons (RecvSession invert frag) nxt) (Cons expandedRecvSession nxt') where
        type ExpandT prog (Cons (RecvSession invert frag) nxt) = Cons (ExpandSessionT prog (RecvSession invert frag)) (ExpandT prog nxt)

class ExpandSession prog session expanded | prog session -> expanded where
    type ExpandSessionT prog session

instance ( Dual prog prog'
         , Expand prog frag current
         , Outgoing current ~ outgoing
         , Dual frag frag'
         , Expand prog frag' current'
         , Outgoing current' ~ incoming
         ) =>
    ExpandSession prog (SendSession False frag) (Send (SpecialSession, (SessionState prog prog' (current, outgoing, incoming)))) where
        type ExpandSessionT prog (SendSession False frag)
            = Send ( SpecialSession
                   , (SessionState prog (DualT prog) ((ExpandT prog frag), (Outgoing (ExpandT prog frag)), (Outgoing (ExpandT prog (DualT frag)))))
                   )

instance ( Dual prog prog'
         , Expand prog' frag current
         , Outgoing current ~ outgoing
         , Dual frag frag'
         , Expand prog' frag' current'
         , Outgoing current' ~ incoming
         ) =>
    ExpandSession prog (SendSession True frag) (Send (SpecialSession, (SessionState prog' prog (current, outgoing, incoming)))) where
        type ExpandSessionT prog (SendSession True frag)
            = Send ( SpecialSession
                   , (SessionState (DualT prog) prog ((ExpandT (DualT prog) frag), (Outgoing (ExpandT (DualT prog) frag)), (Outgoing (ExpandT (DualT prog) (DualT frag)))))
                   )

instance ( Dual prog prog'
         , Expand prog frag current
         , Outgoing current ~ outgoing
         , Dual frag frag'
         , Expand prog frag' current'
         , Outgoing current' ~ incoming
         ) =>
    ExpandSession prog (RecvSession False frag) (Recv (SpecialSession, (SessionState prog prog' (current, outgoing, incoming)))) where
        type ExpandSessionT prog (RecvSession False frag)
            = Recv ( SpecialSession
                   , (SessionState prog (DualT prog) ((ExpandT prog frag), (Outgoing (ExpandT prog frag)), (Outgoing (ExpandT prog (DualT frag)))))
                   )

instance ( Dual prog prog'
         , Expand prog' frag current
         , Outgoing current ~ outgoing
         , Dual frag frag'
         , Expand prog' frag' current'
         , Outgoing current' ~ incoming
         ) =>
    ExpandSession prog (RecvSession True frag) (Recv (SpecialSession, (SessionState prog' prog (current, outgoing, incoming)))) where
        type ExpandSessionT prog (RecvSession True frag)
            = Recv ( SpecialSession
                   , (SessionState (DualT prog) prog ((ExpandT (DualT prog) frag), (Outgoing (ExpandT (DualT prog) frag)), (Outgoing (ExpandT (DualT prog) (DualT frag)))))
                   )

carefullySwapToNextCell :: MVar (ProgramCell a) -> IO (ProgramCell a)
carefullySwapToNextCell programCellMVar
    = do { maybeProgramCell <- tryTakeMVar programCellMVar
         ; case maybeProgramCell of
                        -- if it's already full then no one else will grab it but us, so safe
                        -- if it's empty, then must be careful, as could fill up in mean time
             Nothing -> do { emptyProgramCell <- newEmptyMVar
                           ; emptyProgramCellMVar <- newEmptyMVar
                           ; let cell = (ProgramCell emptyProgramCell emptyProgramCellMVar)
                           ; didPut <- tryPutMVar programCellMVar  cell
                           ; if didPut
                             then return cell
                             else takeMVar programCellMVar
                           }
             (Just cell) -> return cell
         }

-- | Perform a jump. Now you may think that you should indicate where
-- you want to jump to. But of course, that's actually specified by
-- the session type so you don't have to specify it at all in the
-- implementation.
sjump :: forall l prog prog' progOut progIn outgoing incoming current currentUX .
         ( (ProgramToMVarsOutgoingT prog prog) ~ progOut
         , (ProgramToMVarsOutgoingT prog' prog') ~ progIn
         , SWellFormedConfig l (D0 E) prog
         , SWellFormedConfig l (D0 E) prog'
         , TyListIndex progOut l (MVar (ProgramCell (Cell outgoing)))
         , TyListIndex progIn l (MVar (ProgramCell (Cell incoming)))
         , TyListIndex prog l currentUX
         , Expand prog currentUX current
         ) =>
        (SessionChain prog prog') ((Cons (Jump l) Nil), (Cons (Jump l) Nil), (Cons (Jump l) Nil)) (current, outgoing, incoming) ()
sjump = SessionChain f
    where
      f :: SessionState prog prog' (Cons (Jump l) Nil, (Cons (Jump l) Nil), (Cons (Jump l) Nil)) ->
           IO ((), SessionState prog prog' (current, outgoing, incoming))
      f (SessionState outgoingProg incomingProg _ outNotify _ inNotify _)
          = do { (ProgramCell outgoing outProgCellMVar') <- carefullySwapToNextCell outProgCellMVar
               ; (ProgramCell incoming inProgCellMVar') <- carefullySwapToNextCell inProgCellMVar
               ; let outgoingProg' = tyListUpdate outgoingProg (undefined :: l) outProgCellMVar'
               ; let incomingProg' = tyListUpdate incomingProg (undefined :: l) inProgCellMVar'
               ; return ((), (SessionState outgoingProg' incomingProg' (undefined::current) outNotify outgoing inNotify incoming))
               }
          where
            outProgCellMVar = tyListIndex outgoingProg (undefined :: l)
            inProgCellMVar = tyListIndex incomingProg (undefined :: l)

-- | Send a value to the other party. Of course, the value must be of
-- the correct type indicated in the session type.
ssend :: forall t t' sp prog prog' cur nxtCur nxtOut incoming .
         ( CompatibleTypes sp t' t
         , NextSend cur (Send (sp, t)) nxtCur
         ) =>
         t' -> SessionChain prog prog' (cur, Cons t nxtOut, incoming) (nxtCur, nxtOut, incoming) ()
ssend t = SessionChain f
            where
              f :: SessionState prog prog' (cur, (Cons t nxt'), incoming) ->
                   IO ((), SessionState prog prog' (nxtCur, nxt', incoming))
              f (SessionState outgoingProg incomingProg current outNotify outMVar inNotify inMVar)
                  = do { hole <- newEmptyMVar
                       ; outFunc <- tryTakeMVar outNotify
                       ; putMVar outMVar (Cell (convert (undefined::sp) t) hole)
                       ; case outFunc of
                           Nothing -> return ()
                           (Just f) -> f
                       ; return ((), (SessionState outgoingProg incomingProg (removeNextSend current) outNotify hole inNotify inMVar))
                       }

class NextSend cur send edited | cur -> send, cur -> edited where
    removeNextSend :: cur -> edited
instance NextSend (Cons (Send (sp, t)) nxt)  (Send (sp, t)) nxt where
    removeNextSend = tyTail
instance ( NextSend nxt send edited
         , TyList edited
         ) =>
    NextSend (Cons (Recv (sp, t)) nxt) send (Cons (Recv (sp, t)) edited) where
        removeNextSend cur = cons (tyHead cur) . removeNextSend . tyTail $ cur

-- a can be more general than b
class CompatibleTypes special a b | special a -> b, special b -> a where
    convert :: special -> a -> b

instance ( TySubList invertedSessionsB invertedSessionsA True
         , TySubList sessionsToIdxB sessionsToIdxA True
         , ReducePairStructs (TyMap sessionsToIdxA idxsToPairStructsA) (TyMap sessionsToIdxB idxsToPairStructsB) (TyMap Nil Nil) (TyMap sessionsToIdxB idxsToPairStructsB)
         ) =>
    CompatibleTypes SpecialPid
                    (Pid prog prog' invertedSessionsA sessionsToIdxA idxsToPairStructsA)
                    (Pid prog prog' invertedSessionsB sessionsToIdxB idxsToPairStructsB) where
        convert _ (Pid raw tm) = Pid raw tm'
            where
              tm' = reducePairStructs tm (undefined::TyMap sessionsToIdxB idxsToPairStructsB) emptyMap

class ReducePairStructs orig smaller acc result | orig smaller acc -> result where
    reducePairStructs :: orig -> smaller -> acc -> result

instance ReducePairStructs orig (TyMap Nil Nil) acc acc where
    reducePairStructs _ _ acc = acc

instance ( ReducePairStructs (TyMap origKeys origVals) (TyMap keys vals') (TyMap accKeys' accVals') result
         , MapInsert (TyMap accKeys accVals) key val (TyMap accKeys' accVals')
         , MapLookup (TyMap origKeys origVals) key val
         , MapLookup (TyMap (Cons key keys) vals) key val
         , MapDelete (TyMap (Cons key keys) vals) key (TyMap keys vals')
         , TyListElem origKeys key idx
         , TyListIndex origKeys idx key
         ) =>
    ReducePairStructs (TyMap origKeys origVals) (TyMap (Cons key keys) vals) (TyMap accKeys accVals) result where
        reducePairStructs orig smaller acc
            = reducePairStructs orig (mapDelete smaller key) acc'
              where
                origKeys = tyMapKeys orig
                key' = tyHead . tyMapKeys $ smaller
                key = tyListIndex origKeys (tyListElem origKeys key')
                acc' = mapInsert key (mapLookup orig key) acc

instance CompatibleTypes SpecialNormal t t where
    convert _ t = t

instance CompatibleTypes SpecialSession t t where
    convert _ t = t

-- | Recieve a value from the other party. This will block as
-- necessary. The type of the value received is specified by the
-- session type. No magic coercion needed.
srecv :: forall sp t prog prog' nxt nxt' outgoing .
         (SessionChain prog prog') ((Cons (Recv (sp, t)) nxt), outgoing, (Cons t nxt')) (nxt, outgoing, nxt') t
srecv = SessionChain f
    where
      f :: SessionState prog prog' ((Cons (Recv (sp, t)) nxt), outgoing, (Cons t nxt')) ->
           IO (t, SessionState prog prog' (nxt, outgoing, nxt'))
      f (SessionState outgoingProg incomingProg current outNotify outMVar inNotify inMVar)
          = do { (Cell t nxt') <- takeMVar inMVar
               ; return (t, (SessionState outgoingProg incomingProg (tyTail current) outNotify outMVar inNotify nxt'))
               }

srecvTest :: SessionChain prog prog' (Cons (Recv (sp, t)) nxt, outgoing, (Cons t nxt')) (Cons (Recv (sp, t)) nxt, outgoing, (Cons t nxt')) Bool
srecvTest = SessionChain f
    where
      f :: SessionState prog prog' ((Cons (Recv (sp, t)) nxt), outgoing, (Cons t nxt')) ->
           IO (Bool, SessionState prog prog' ((Cons (Recv (sp, t)) nxt), outgoing, (Cons t nxt')))
      f ss@(SessionState _ _ _ _ _ _ inMVar)
          = do { canRecv <- liftM not . isEmptyMVar $ inMVar
               ; return (canRecv, ss)
               }

srecvTestTimeOut :: forall prog prog' sp t nxt nxt' outgoing .
    Int -> SessionChain prog prog' (Cons (Recv (sp, t)) nxt, outgoing, (Cons t nxt')) (Cons (Recv (sp, t)) nxt, outgoing, (Cons t nxt')) Bool
srecvTestTimeOut delay = SessionChain f
    where
      f :: SessionState prog prog' ((Cons (Recv (sp, t)) nxt), outgoing, (Cons t nxt')) ->
           IO (Bool, SessionState prog prog' ((Cons (Recv (sp, t)) nxt), outgoing, (Cons t nxt')))
      f ss@(SessionState _ _ _ _ _ inNotify inMVar)
          = do { empty <- isEmptyMVar inMVar -- 0. It's possible the producer is miles ahead
               ; if empty
                 then -- 1. The producer is either behind us or with us
                      do { empty' <-
                               do { chan <- newChan
                                  ; forkIO (threadDelay delay >> writeChan chan ())
                                  ; putMVar inNotify (writeChan chan ())
                                  ; empty'' <- isEmptyMVar inMVar -- 2. producer could be ahead
                                  ; if empty''
                                    then do { readChan chan
                                            ; isEmptyMVar inMVar
                                            }
                                    else return empty''
                                  }
                         ; return (not empty', ss)
                         }
                 else return (not empty, ss)
               }

-- | Offer a number of branches. This is basically an external choice
-- - the other party uses 'sselect' to decide which branch to take.
-- Use OfferImpls in order to construct the list of implementations of
-- branches. Note that every implementation must result in the same
-- final state and emit the same value.
soffer :: forall current outgoing incoming finalResult prog prog' jumps .
          OfferImpls jumps prog prog' (current, outgoing, incoming) finalResult
          -> (SessionChain prog prog') (Cons (Offer jumps) Nil, Cons (Choice jumps) Nil, Cons (Choice jumps) Nil) (current, outgoing, incoming) finalResult
soffer implementations = SessionChain f
    where
      f :: SessionState prog prog' (Cons (Offer jumps) Nil, Cons (Choice jumps) Nil, Cons (Choice jumps) Nil) ->
           IO (finalResult, SessionState prog prog' (current, outgoing, incoming))
      f (SessionState outgoingProg incomingProg _ outNotify _ inNotify inMVar)
          = do { (SelectCell n) <- takeMVar inMVar
               ; runSessionChain (walkOfferImpls n implementations)
                                 (SessionState outgoingProg incomingProg undefined outNotify undefined inNotify undefined)
               }

-- | Select which branch we're taking at a branch point. Use a type
-- number ("Control.Concurrent.Session.Number") to indicate the branch
-- to take.
sselect :: forall prog prog' progOut progIn label jumps outgoing incoming current currentUX len jumpTarget .
           ( ProgramToMVarsOutgoingT prog prog ~ progOut
           , ProgramToMVarsOutgoingT prog' prog' ~ progIn
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
           ) =>
           label -> (SessionChain prog prog') (Cons (Select jumps) Nil, Cons (Choice jumps) Nil, Cons (Choice jumps) Nil) (current, outgoing, incoming) ()
sselect label = SessionChain f
    where
      f :: SessionState prog prog' (Cons (Select jumps) Nil, Cons (Choice jumps) Nil, Cons (Choice jumps) Nil) ->
           IO ((), SessionState prog prog' (current, outgoing, incoming))
      f (SessionState outgoingProg incomingProg _ outNotify outMVar inNotify _)
          = do { outFunc <- tryTakeMVar outNotify
               ; putMVar outMVar (SelectCell (tyNumToInt label))
               ; case outFunc of
                   Nothing -> return ()
                   (Just f) -> f
               ; (ProgramCell outgoing outProgCellMVar') <- carefullySwapToNextCell outProgCellMVar
               ; (ProgramCell incoming inProgCellMVar') <- carefullySwapToNextCell inProgCellMVar
               ; let outgoingProg' = tyListUpdate outgoingProg (undefined :: jumpTarget) outProgCellMVar'
               ; let incomingProg' = tyListUpdate incomingProg (undefined :: jumpTarget) inProgCellMVar'
               ; return ((), (SessionState outgoingProg' incomingProg' (undefined::current) outNotify outgoing inNotify incoming))
               }
          where
            outProgCellMVar = tyListIndex outgoingProg (undefined :: jumpTarget)
            inProgCellMVar = tyListIndex incomingProg (undefined :: jumpTarget)

-- | Run! Provide a program and a start point within that program
-- (which is automatically 'sjump'ed to), the two implementations
-- which must be duals of each other, run them, have them communicate,
-- wait until they both finish and die and then return the results
-- from both of them.
run :: forall prog prog' progOut progIn init fromO fromI toO toI res res' currentUX currentUX' current current' toCur toCur' .
       ( ProgramToMVarsOutgoing prog prog progOut
       , ProgramToMVarsOutgoing prog' prog' progIn
       , ProgramToMVarsOutgoingT prog prog ~ progOut
       , ProgramToMVarsOutgoingT prog' prog' ~ progIn
       , SWellFormedConfig init (D0 E) prog
       , SWellFormedConfig init (D0 E) prog'
       , TyListIndex progOut init (MVar (ProgramCell (Cell fromO)))
       , TyListIndex progIn init (MVar (ProgramCell (Cell fromI)))
       , DualT prog ~ prog'
       , Dual prog prog'
       , TyListIndex prog init currentUX
       , Expand prog currentUX current
       , TyListIndex prog' init currentUX'
       , Expand prog' currentUX' current'
       ) => prog -> init ->
       SessionChain prog prog' (current, fromO, fromI) (toCur, toO, toI) res ->
       SessionChain prog' prog (current', fromI, fromO) (toCur', toI, toO) res' ->
       IO (res, res')
run _ _ chain1 chain2
    = do { mvarsOut <- programToMVarsOutgoing prog prog
         ; mvarsIn <- programToMVarsOutgoing prog' prog'
         ; aDone <- newEmptyMVar
         ; bDone <- newEmptyMVar
         ; aNotify <- newEmptyMVar
         ; bNotify <- newEmptyMVar
         ; forkIO $ runSessionChain chain1' (SessionState mvarsOut mvarsIn undefined aNotify undefined bNotify undefined)
                      >>= putMVar aDone . fst
         ; forkIO $ runSessionChain chain2' (SessionState mvarsIn mvarsOut undefined bNotify undefined aNotify undefined)
                      >>= putMVar bDone . fst
         ; aRes <- takeMVar aDone
         ; bRes <- takeMVar bDone
         ; return (aRes, bRes)
         }
    where
      chain1' :: SessionChain prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)) (toCur, toO, toI) res
      chain1'  = sjump ~>> chain1
      chain2' :: SessionChain prog' prog ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)) (toCur', toI, toO) res'
      chain2'  = sjump ~>> chain2
      prog = undefined::prog
      prog' = undefined::prog'
