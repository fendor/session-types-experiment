{-# LANGUAGE KindSignatures
           , ScopedTypeVariables
           , GADTs
           , MultiParamTypeClasses
           , FunctionalDependencies
           , UndecidableInstances
           , FlexibleInstances
           , TypeFamilies
           , EmptyDataDecls #-}

{-
    Types.hs
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

module Control.Concurrent.Session.Types where

import Control.Concurrent.Session.Base.SMonad
import Control.Concurrent.Session.Base.List
import Control.Concurrent.Session.Base.Map
import Control.Concurrent.Session.SessionType
import Control.Concurrent

data Cell :: * -> * where
             Cell :: val -> MVar (Cell nxt) -> Cell (Cons val nxt)
             SelectCell :: Int -> Cell (Cons (Choice jumps) Nil)

data ProgramCell :: * -> * where
                    ProgramCell :: MVar a -> MVar (ProgramCell a) -> ProgramCell a

class ProgramToMVarsOutgoing progRef prog mvars | progRef prog -> mvars where
    type ProgramToMVarsOutgoingT progRef prog
    programToMVarsOutgoing :: progRef -> prog -> IO mvars
instance ProgramToMVarsOutgoing ref Nil Nil where
    type ProgramToMVarsOutgoingT ref Nil = Nil
    programToMVarsOutgoing _ p = return p
instance ( ProgramToMVarsOutgoing ref nxt nxt'
         , TyList nxt
         , TyList nxt'
         , Expand ref val val''
         , (Outgoing val'') ~ val'
         ) =>
    ProgramToMVarsOutgoing ref (Cons val nxt) (Cons (MVar (ProgramCell (Cell val'))) nxt') where
        type ProgramToMVarsOutgoingT ref (Cons val nxt) = (Cons (MVar (ProgramCell (Cell (Outgoing (ExpandT ref val))))) (ProgramToMVarsOutgoingT ref nxt))
        programToMVarsOutgoing ref v
            = do { hole <- newEmptyMVar
                 ; rest <- programToMVarsOutgoing ref nxt
                 ; return $ cons hole rest
                 }
            where
              nxt = tyTail v

data SessionState :: * -> * -> * -> * where
                     SessionState :: ( (ProgramToMVarsOutgoingT prog prog) ~ progOut
                                     , (ProgramToMVarsOutgoingT prog' prog') ~ progIn
                                     ) =>
                                     progOut -> progIn ->
                                     current ->
                                     MVar (IO ()) ->
                                     (MVar (Cell currentOutgoing)) ->
                                     MVar (IO ()) ->
                                     (MVar (Cell currentIncoming)) ->
                                     SessionState prog prog' (current, currentOutgoing, currentIncoming)

-- | The representation of a computation that performs work using
-- session types. Again, really quite similar to a more-parameterized
-- State monad.
newtype SessionChain prog prog' from to res
    = SessionChain { runSessionChain :: (SessionState prog prog' from) ->
                                        IO (res, SessionState prog prog' to)
                   }

instance SMonad (SessionChain prog prog') where
        f ~>> g   = SessionChain $
                    \x ->
                        do { (_, y) <- runSessionChain f x
                           ; runSessionChain g y
                           }
        f ~>>= g  = SessionChain $
                    \x ->
                        do { (a, y) <- runSessionChain f x
                           ; runSessionChain (g a) y
                           }
        sreturn a = SessionChain $ \x -> return (a, x)

instance SMonadIO (SessionChain prog prog') where
        sliftIO f = SessionChain $
                    \x ->
                        do { a <- f
                           ; return (a, x)
                           }

type RawPid = [Int]

-- | A process ID. This is a tiny bit like ThreadId but rather heavily annotated.
data Pid :: * -> * -> * -> * -> * -> * where
            Pid :: RawPid -> TyMap sessionsToIdx idxsToPairStructs ->
                   Pid prog prog' invertedSessions sessionsToIdx idxsToPairStructs
data InternalPid :: * -> * -> * -> * -> * -> * where
                    IPid :: Pid prog prog' invertedSessions sessionsToIdx idxsToPairStructs -> [RawPid] ->
                            InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs

pidToRawPid :: Pid prog prog' invertedSessions sessionsToIdx idxsToPairStructs -> RawPid
pidToRawPid (Pid p _) = p

iPidToPid :: InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs ->
             Pid prog prog' invertedSessions sessionsToIdx idxsToPairStructs
iPidToPid (IPid p _) = p

instance Show (Pid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) where
    show = (:) '<' . (:) '.' . foldr (\c a -> shows c ('.':a)) ">" . reverse . pidToRawPid

instance Eq (Pid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) where
    (==) a b = (==) (pidToRawPid a) (pidToRawPid b)

instance Ord (Pid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) where
    compare a b = compare (pidToRawPid a) (pidToRawPid b)

instance Eq (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) where
    (==) a b = (==) (iPidToPid a) (iPidToPid b)

instance Ord (InternalPid prog prog' invertedSessions sessionsToIdx idxsToPairStructs) where
    compare a b = compare (iPidToPid a) (iPidToPid b)

-- | Provides a way to compare two Pids. Of course, if the Pids have
-- different type params, then they are definitely different, but it's
-- still convenient to be able to do something like (==) on them.
class PidEq a b where
    (=~=) :: a -> b -> Bool
instance PidEq (Pid progA progA' invertedSessionsA sessionsToIdxA idxsToPairStructsA) (Pid progB progB' invertedSessionsB sessionsToIdxB idxsToPairStructsB) where
    (=~=) a b = (==) (pidToRawPid a) (pidToRawPid b)

newtype InterleavedChain internalPid from to res
    = InterleavedChain { runInterleavedChain :: internalPid ->
                                                from ->
                                                IO (res, to, internalPid)
                       }

instance SMonad (InterleavedChain internalPid) where
        f ~>> g   = InterleavedChain $
                    \p x -> do { (_, y, p') <- runInterleavedChain f p x
                               ; runInterleavedChain g p' y
                               }
        f ~>>= g  = InterleavedChain $
                    \p x -> do { (a, y, p') <- runInterleavedChain f p x
                               ; runInterleavedChain (g a) p' y
                               }
        sreturn a = InterleavedChain $
                    \p x -> return (a, x, p)

instance SMonadIO (InterleavedChain internalPid) where
    sliftIO f = InterleavedChain $
                \p x -> do { a <- f
                           ; return (a, x, p)
                           }

data SpecialSession
data SpecialPid
data SpecialNormal

data PairStruct :: * -> * -> * -> * -> * where
                   PS ::RawPid ->
                        (SessionState prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)) -> IO ()) ->
                        PairStruct init prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil))

instance Eq (PairStruct init prog prog' start) where
    (==) (PS x _) (PS y _) = x == y
instance Ord (PairStruct init prog prog' start) where
    compare (PS x _) (PS y _) = compare x y
