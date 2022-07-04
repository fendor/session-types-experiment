{-# LANGUAGE KindSignatures
           , ScopedTypeVariables
           , GADTs
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , PatternSignatures
           , TypeFamilies
           , FunctionalDependencies
           , UndecidableInstances #-}

{-
    Socket.hs
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

module Control.Concurrent.Session.Network.Socket
    ( runOverNetwork
    , CreateSessionOverNetwork (..)
    )
    where

import Control.Concurrent.Session.Base.Number
import Control.Concurrent.Session.Base.Bool
import Control.Concurrent.Session.Base.List
import Control.Concurrent.Session.Base.Map
import Control.Concurrent.Session.Base.SMonad
import Control.Concurrent.Session.SessionType
import Control.Concurrent.Session.Types
import Control.Concurrent.Session.Runtime

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent
import System.IO
import Data.Binary
import Data.Binary.Put
import Data.Int
import qualified Data.ByteString.Lazy as BSL
import Control.Monad
import Network

{- convenience so that we don't have to deal with SessionState with its state type -}
data SessionStateShort :: * -> * -> * where
                     SessionStateShort :: ( (ProgramToMVarsOutgoingT prog prog) ~ progOut
                                          , (ProgramToMVarsOutgoingT prog' prog') ~ progIn
                                          ) =>
                                          progOut -> progIn ->
                                          MVar (IO ()) ->
                                          MVar (IO ()) ->
                                          SessionStateShort prog prog'

{- The idea is that we build a list of functions where each function corresponds to
   the session type in the fragment from the prog "list". That way, when we need to jump
   we just pull the relevant elem out of the list and invoke it.
-}
newtype NetworkRunner prog prog' to result = NR (Handle -> [NetworkRunner prog prog' to result] -> SessionStateShort prog prog' ->
                                                 IO (result, SessionState prog prog' to))

{- And here we actually build the NetworkRunner list -}
class BuildNetworkRunner prog prog' to result progList curIdx where
    buildNetworkRunner :: curIdx -> progList -> [NetworkRunner prog prog' to result]

instance BuildNetworkRunner prog prog' to result Nil idx where
    buildNetworkRunner _ _ = []

{- recursive case and is the only point at which we rely on NetworkRuntime.
   We effectively build in the logic for the jump here too.
-}
instance forall prog prog' to result frag progList idx idxSucc progOut progIn outgoing incoming current currentUX .
    ( Succ idx idxSucc
    , BuildNetworkRunner prog prog' to result progList idxSucc
    , (ProgramToMVarsOutgoingT prog prog) ~ progOut
    , (ProgramToMVarsOutgoingT prog' prog') ~ progIn
    , TyListIndex progOut idx (MVar (ProgramCell (Cell outgoing)))
    , TyListIndex progIn idx (MVar (ProgramCell (Cell incoming)))
    , TyListIndex prog idx currentUX
    , Expand prog currentUX current
    , NetworkRuntime prog prog' (current, outgoing, incoming) to result
    ) =>
    BuildNetworkRunner prog prog' to result (Cons frag progList) idx where
        buildNetworkRunner idx progList
            = (NR f) : buildNetworkRunner (tySucc idx) (tyTail progList)
              where
                f :: Handle -> [NetworkRunner prog prog' to result] -> SessionStateShort prog prog' ->
                     IO (result, SessionState prog prog' to)
                f hdl nr (SessionStateShort outgoingProg incomingProg outNotify inNotify)
                    = do { (ProgramCell outgoing outProgCellMVar') <- carefullySwapToNextCell outProgCellMVar
                         ; (ProgramCell incoming inProgCellMVar') <- carefullySwapToNextCell inProgCellMVar
                         ; let outgoingProg' = tyListUpdate outgoingProg idx outProgCellMVar'
                         ; let incomingProg' = tyListUpdate incomingProg idx inProgCellMVar'
                         ; runSessionChain (runNetwork hdl nr)
                           (SessionState outgoingProg' incomingProg' (undefined::current) outNotify outgoing inNotify incoming)
                         }
                    where
                      outProgCellMVar = tyListIndex outgoingProg idx
                      inProgCellMVar = tyListIndex incomingProg idx

{- Now how to actually work on each fragment -}
class NetworkRuntime prog prog' from to result where
    runNetwork :: Handle -> [NetworkRunner prog prog' to result] -> SessionChain prog prog' from to result

instance forall prog prog' nxtC nxtO incoming nxtC' nxtO' incoming' result sp t t' .
    ( NetworkRuntime prog prog' (nxtC, nxtO, incoming) (nxtC', nxtO', incoming') result
    , Binary t'
    , CompatibleTypes sp t' t
    ) =>
    NetworkRuntime prog prog' ((Cons (Send (sp, t)) nxtC), (Cons t nxtO), incoming) (nxtC', nxtO', incoming') result where
        runNetwork hdl runner
            = sliftIO (receiveBin hdl) ~>>= \(t'::t') ->
              ssend t' ~>>
              runNetwork hdl runner

instance ( NetworkRuntime prog prog' (nxtC, outgoing, nxtI) (nxtC', outgoing', nxtI') result
         , Binary t
         ) =>
    NetworkRuntime prog prog' ((Cons (Recv (sp, t)) nxtC), outgoing, (Cons t nxtI)) (nxtC', outgoing', nxtI') result where
        runNetwork hdl runner
            = srecv ~>>= \t ->
              sliftIO (sendBin hdl t) ~>>
              runNetwork hdl runner

instance NetworkRuntime prog prog' ((Cons End Nil), (Cons End Nil), (Cons End Nil)) (current, outgoing, incoming) () where
    runNetwork hdl _
        = sliftIO (hClose hdl) ~>> (SessionChain f)
          where
            f :: SessionState prog prog' (Cons End Nil, Cons End Nil, Cons End Nil) ->
                 IO ((), SessionState prog prog' (current, outgoing, incoming))
            f (SessionState outgoingProg incomingProg _ outNotify _ inNotify _)
                = return ((), SessionState outgoingProg incomingProg undefined outNotify undefined inNotify undefined)

{- in this case, it's just a matter of selecting the right elem from the list as
   the logic for the jump itself is within the function that is the list elem
-}
instance forall prog prog' l result to .
    ( TypeNumberToInt l
    ) =>
    NetworkRuntime prog prog' ((Cons (Jump l) Nil), (Cons (Jump l) Nil), (Cons (Jump l) Nil))
                              to result where
        runNetwork hdl runner
            = SessionChain f
              where
                f :: SessionState prog prog' (Cons (Jump l) Nil, Cons (Jump l) Nil, Cons (Jump l) Nil) ->
                     IO (result, SessionState prog prog' to)
                f (SessionState outgoingProg incomingProg _ outNotify _ inNotify _)
                    = run hdl runner (SessionStateShort outgoingProg incomingProg outNotify inNotify)
                (NR run) = runner !! (tyNumToInt (undefined::l))

{- For Choice, we build a normal haskell list of all the possible targets -
   basically stolen from the runners list.
   Then it's just a matter of indexing that list with the branch label int.
-}
instance forall prog prog' to result jumps .
         ( JumpsToList prog prog' to result jumps
                           [SessionChain prog prog' ((Cons (Offer jumps) Nil), (Cons (Choice jumps) Nil), (Cons (Choice jumps) Nil))
                                         to result]
         ) =>
    NetworkRuntime prog prog' ((Cons (Offer jumps) Nil), (Cons (Choice jumps) Nil), (Cons (Choice jumps) Nil))
                              to result where
        runNetwork hdl runner
            = (SessionChain f) ~>>= \(n, lst) -> lst !! n
            where
              f :: SessionState prog prog' ((Cons (Offer jumps) Nil), (Cons (Choice jumps) Nil), (Cons (Choice jumps) Nil)) ->
                   IO ( ( Int
                        , [SessionChain prog prog' ((Cons (Offer jumps) Nil), (Cons (Choice jumps) Nil), (Cons (Choice jumps) Nil))
                                        to result]
                        )
                      , (SessionState prog prog' ((Cons (Offer jumps) Nil), (Cons (Choice jumps) Nil), (Cons (Choice jumps) Nil)))
                      )
              f st@(SessionState _ _ _ _ _ _ inMVar)
                  = do { (SelectCell n) <- takeMVar inMVar
                       ; sendBin hdl n
                       ; return ( (n, toJumpsList hdl runner (undefined :: jumps))
                                , st
                                )
                       }

instance forall prog prog' to result jumps .
         ( JumpsToList prog prog' to result jumps
                           [SessionChain prog prog' ((Cons (Select jumps) Nil), (Cons (Choice jumps) Nil), (Cons (Choice jumps) Nil))
                                         to result]
         ) =>
    NetworkRuntime prog prog' ((Cons (Select jumps) Nil), (Cons (Choice jumps) Nil), (Cons (Choice jumps) Nil))
                              to result where
        runNetwork hdl runner
            = (SessionChain f) ~>>= \(n, lst) -> lst !! n
            where
              f :: SessionState prog prog' ((Cons (Select jumps) Nil), (Cons (Choice jumps) Nil), (Cons (Choice jumps) Nil)) ->
                   IO ( ( Int
                        , [SessionChain prog prog' ((Cons (Select jumps) Nil), (Cons (Choice jumps) Nil), (Cons (Choice jumps) Nil))
                                        to result]
                        )
                      , (SessionState prog prog' ((Cons (Select jumps) Nil), (Cons (Choice jumps) Nil), (Cons (Choice jumps) Nil)))
                      )
              f st@(SessionState _ _ _ _ outMVar _ _)
                  = do { n <- receiveBin hdl
                       ; putMVar outMVar (SelectCell n)
                       ; return ( (n, toJumpsList hdl runner (undefined :: jumps))
                                , st
                                )
                       }

{- This is basically just some sort of filter -}
class JumpsToList prog prog' to result jumpsList list | prog prog' to result jumpsList -> list where
    toJumpsList :: Handle -> [NetworkRunner prog prog' to result] -> jumpsList -> list

instance JumpsToList prog prog' to result Nil [a] where
    toJumpsList _ _ _ = []

instance forall prog prog' from to result t jumps' .
    ( JumpsToList prog prog' to result jumps' [SessionChain prog prog' from to result]
    , NetworkRuntime prog prog' (Cons (Jump t) Nil, Cons (Jump t) Nil, Cons (Jump t) Nil) to result
    , TypeNumberToInt t
    ) =>
    JumpsToList prog prog' to result (Cons (Cons (Jump t) Nil) jumps') [SessionChain prog prog' from to result] where
        toJumpsList hdl runner jumpsList
            = (SessionChain g) : (toJumpsList hdl runner (tyTail jumpsList))
              where
                (NR f) = runner !! (tyNumToInt (undefined::t))
                g :: SessionState prog prog' from ->
                     IO (result, SessionState prog prog' to)
                g (SessionState outgoingProg incomingProg _ outNotify _ inNotify _)
                    = f hdl runner (SessionStateShort outgoingProg incomingProg outNotify inNotify)

sendBin :: (Binary a) => Handle -> a -> IO ()
sendBin hdl bin = do { BSL.hPut hdl encoded
                     ; hFlush hdl
                     }
    where
      binBS = encode bin
      (len16::Int16) = fromIntegral (BSL.length binBS)
      encoded = runPut $ do { put len16
                            ; putLazyByteString binBS
                            }

receiveBin :: (Binary a) => Handle -> IO a
receiveBin hdl
    = do { (len::Int16) <- liftM decode $ BSL.hGet hdl 2
         ; bs <- BSL.hGet hdl (fromIntegral len)
         ; return $ decode bs
         }

{- So long as we can build the list of runners for the prog and so
   long as we can start, we should be ok. We could actually just index
   the list directly and avoid the NetworkRuntime constraint, but it
   wouldn't make much difference.
-}
runOverNetwork :: forall prog prog' progOut progIn init fromO fromI toO toI res' currentUX currentUX' current current' toCur toCur' .
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
                  , BuildNetworkRunner prog' prog (toCur', toI, toO) () prog' (D0 E)
                  , NetworkRuntime prog' prog (current', fromI, fromO) (toCur', toI, toO) ()
                  ) =>
    prog -> init ->
    SessionChain prog prog' (current, fromO, fromI) (toCur, toO, toI) res' ->
    Maybe HostName -> PortID ->
    IO res'
runOverNetwork prog init chain1 (Just name) port
    = do { hdl <- connectTo name port
         ; let (runners :: [NetworkRunner prog' prog (toCur', toI, toO) ()]) = buildNetworkRunner (D0 E) (dual prog)
               (chain2::(SessionChain prog' prog (current', fromI, fromO) (toCur', toI, toO) ())) = runNetwork hdl runners
         ; liftM fst $ run prog init chain1 chain2
         }
runOverNetwork prog init chain1 Nothing port
    = do { sock <- listenOn port
         ; (hdl, host, port) <- accept sock
         ; putStrLn $ "Accepted connection from " ++ host ++ ":" ++ (show port)
         ; sClose sock
         ; let (runners :: [NetworkRunner prog' prog (toCur', toI, toO) ()]) = buildNetworkRunner (D0 E) (dual prog)
               (chain2::(SessionChain prog' prog (current', fromI, fromO) (toCur', toI, toO) ())) = runNetwork hdl runners
         ; liftM fst $ run prog init chain1 chain2
         }

class CreateSessionOverNetwork invert init prog prog' invertedSessionsMe sessionsToIdxMe idxsToPairStructsMe
                               keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe' idxOfThem where
    createSessionOverNetwork :: init -> invert -> Handle ->
                                InterleavedChain (InternalPid prog prog' invertedSessionsMe sessionsToIdxMe idxsToPairStructsMe)
                                                 (TyMap keyToIdxMe idxToValueMe) (TyMap keyToIdxMe' idxToValueMe') idxOfThem

instance forall init prog prog' invertedSessionsMe sessionsToIdxMe idxsToPairStructsMe
                keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe' idxOfThem
                progOut progIn fromO fromI current currentUX current' currentUX' toCur toI toO .
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
    , Dual prog prog'
    , DualT prog ~ prog'
    , TyListIndex prog' init currentUX'
    , Expand prog' currentUX' current'
    , MapSize (TyMap keyToIdxMe idxToValueMe) idxOfThem
    , MapInsert (TyMap keyToIdxMe idxToValueMe) idxOfThem
                    (SessionState prog prog' (current, fromO, fromI)) (TyMap keyToIdxMe' idxToValueMe')
    , BuildNetworkRunner prog' prog (toCur, toI, toO) () prog' init
    , NetworkRuntime prog' prog (current', fromI, fromO) (toCur, toI, toO) ()
    , MapLookup (TyMap sessionsToIdxMe idxsToPairStructsMe) init
                    (MVar (Map (RawPid, RawPid) (MVar (PairStruct init prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil))))))
    ) =>
    CreateSessionOverNetwork False init prog prog'
                  invertedSessionsMe sessionsToIdxMe idxsToPairStructsMe
                  keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe' idxOfThem where
        createSessionOverNetwork init FF hdl =
            InterleavedChain $
                \ipid@(IPid (Pid _ _) _) mp ->
                    do { mvarsOut <- programToMVarsOutgoing prog prog
                       ; mvarsIn <- programToMVarsOutgoing prog' prog'
                       ; aNotify <- newEmptyMVar
                       ; bNotify <- newEmptyMVar
                       ; let (theirST :: SessionState prog' prog ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))
                                 =  SessionState mvarsIn mvarsOut undefined bNotify undefined aNotify undefined
                             (myST :: SessionState prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))
                                 = SessionState mvarsOut mvarsIn undefined aNotify undefined bNotify undefined
                             idxOfThem :: idxOfThem = mapSize mp
                             (runners :: [NetworkRunner prog' prog (toCur, toI, toO) ()]) = buildNetworkRunner init prog'
                             (chain::(SessionChain prog' prog (current', fromI, fromO) (toCur, toI, toO) ())) = runNetwork hdl runners
                       ; ((), myST') <- runSessionChain sjump myST
                       ; forkIO $ runSessionChain (sjump ~>> chain) theirST >> return ()
                       ; return (idxOfThem, mapInsert idxOfThem myST' mp, ipid)
                       }
                    where
                      prog = undefined::prog
                      prog' = undefined::prog'

instance forall init prog prog' invertedSessionsMe sessionsToIdxMe idxsToPairStructsMe
                keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe' idxOfThem
                progOut progIn fromO fromI current currentUX current' currentUX' toCur toI toO .
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
    , Dual prog prog'
    , DualT prog ~ prog'
    , TyListIndex prog' init currentUX'
    , Expand prog' currentUX' current'
    , MapSize (TyMap keyToIdxMe idxToValueMe) idxOfThem
    , MapInsert (TyMap keyToIdxMe idxToValueMe) idxOfThem
                    (SessionState prog' prog (current', fromI, fromO)) (TyMap keyToIdxMe' idxToValueMe')
    , BuildNetworkRunner prog prog' (toCur, toO, toI) () prog init
    , NetworkRuntime prog prog' (current, fromO, fromI) (toCur, toO, toI) ()
    , TyListMember invertedSessionsMe init True
    ) =>
    CreateSessionOverNetwork True init prog prog'
                  invertedSessionsMe sessionsToIdxMe idxsToPairStructsMe
                  keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe' idxOfThem where
        createSessionOverNetwork init TT hdl =
            InterleavedChain $
                \ipid@(IPid (Pid _ _) _) mp ->
                    do { mvarsOut <- programToMVarsOutgoing prog prog
                       ; mvarsIn <- programToMVarsOutgoing prog' prog'
                       ; aNotify <- newEmptyMVar
                       ; bNotify <- newEmptyMVar
                       ; let (theirST :: SessionState prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))
                                 =  SessionState mvarsOut mvarsIn undefined aNotify undefined bNotify undefined
                             (myST :: SessionState prog' prog ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))
                                 = SessionState mvarsIn mvarsOut undefined bNotify undefined aNotify undefined
                             idxOfThem :: idxOfThem = mapSize mp
                             (runners :: [NetworkRunner prog prog' (toCur, toO, toI) ()]) = buildNetworkRunner init prog
                             (chain::(SessionChain prog prog' (current, fromO, fromI) (toCur, toO, toI) ())) = runNetwork hdl runners
                       ; ((), myST') <- runSessionChain sjump myST
                       ; forkIO $ runSessionChain (sjump ~>> chain) theirST >> return ()
                       ; return (idxOfThem, mapInsert idxOfThem myST' mp, ipid)
                       }
                    where
                      prog = undefined::prog
                      prog' = undefined::prog'
