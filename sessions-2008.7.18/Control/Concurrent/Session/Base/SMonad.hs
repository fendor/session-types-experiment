{-
    SMonad.hs
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

{-# LANGUAGE KindSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

-- | Super magic Monads.

module Control.Concurrent.Session.Base.SMonad where

import Control.Monad.State

newtype SChain m x y a = SChain { runSChain :: x -> m (a, y) }

-- | An extension of the typical Monad such that you track additional
-- @from@ and @to@ parameters. Thus you can think of this like 'State'
-- where the type of the 'State' varies.
class SMonad (m :: * -> * -> * -> *) where
    (~>>) :: m x y a -> m y z b -> m x z b
    (~>>=) :: m x y a -> (a -> m y z b) -> m x z b
    sreturn :: a -> m x x a

infixl 1 ~>>
infixl 1 ~>>=

instance (Monad m) => SMonad (SChain m) where
    f ~>> g   = SChain $ \x -> do { (_, y) <- runSChain f x
                                  ; runSChain g y
                                  }
    f ~>>= g  = SChain $ \x -> do { (a, y) <- runSChain f x
                                  ; runSChain (g a) y
                                  }
    sreturn a = SChain $ \x -> return (a, x)

instance (Monad m) => Monad (SChain m x x) where
    m >> n   = SChain $ \x -> do { ~(_, y) <- runSChain m x
                                 ; runSChain n y
                                 }
    m >>= k  = SChain $ \x -> do { ~(a, y) <- runSChain m x
                                 ; runSChain (k a) y
                                 }
    return a = SChain $ \x -> return (a, x)
    fail str = SChain $ \_ -> fail str

newtype SStateT s m x y a = SStateT { runSStateT :: s -> m x y (a, s) }

instance (SMonad m) => SMonad (SStateT s m) where
    f ~>> g   = SStateT $ \s -> runSStateT f s ~>>= \(_, s') ->
                                runSStateT g s'
    f ~>>= g  = SStateT $ \s -> runSStateT f s ~>>= \(a, s') ->
                                runSStateT (g a) s'
    sreturn a = SStateT $ \s -> sreturn (a, s)

class SMonadTrans t where
    slift :: (SMonad m) => m x y a -> t m x y a

instance SMonadTrans (SStateT s) where
    slift f = SStateT $ \s -> f ~>>= \a -> sreturn (a, s)

class (SMonad m) => SMonadIO m where
    sliftIO :: IO a -> m x x a

instance (MonadIO m) => SMonadIO (SChain m) where
    sliftIO f = SChain $ \x -> do { a <- liftIO f
                                  ; return (a, x)
                                  }

class (SMonad m) => SMonadState s m | m -> s where
  sget :: m x x s
  sput :: s -> m x x ()

instance (SMonad m) => SMonadState s (SStateT s m) where
    sget = SStateT $ \s -> sreturn (s, s)
    sput s = SStateT $ \_ -> sreturn ((), s)

ssequence_ :: (SMonad m) => [m x x a] -> m x x ()
ssequence_ [] = sreturn ()
ssequence_ (f:fs) = f ~>> ssequence_ fs

ssequence :: (SMonad m) => [m x x a] -> m x x [a]
ssequence [] = sreturn []
ssequence (f:fs) = f ~>>= \r ->
                   ssequence fs ~>>= \rs ->
                   sreturn (r:rs)

sjoin :: (SMonad m) => m x y (m y z b) -> m x z b
sjoin f = f ~>>= id

smapM :: (SMonad m) => (a -> m x x b) -> [a] -> m x x [b]
smapM f lst = ssequence . map f $ lst

smapM_ :: (SMonad m) => (a -> m x x b) -> [a] -> m x x ()
smapM_ f lst = ssequence_ . map f $ lst
