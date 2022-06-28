{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , GADTs
           , KindSignatures #-}

{-
    Number.hs
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

-- | Type level Integers. These are all base 10.

module Control.Concurrent.Session.Base.Number
    ( E (..)
    , D0 (..)
    , D1 (..)
    , D2 (..)
    , D3 (..)
    , D4 (..)
    , D5 (..)
    , D6 (..)
    , D7 (..)
    , D8 (..)
    , D9 (..)
    , Succ (..)
    , Pred (..)
    , Add (..)
    , SmallerThan
    , SmallerThanBool (..)
    , TyNum
    , TypeNumberToInt (..)
    ) where

import Control.Concurrent.Session.Base.Bool

{-
This module provides basic arithmetic for numbers at the type level.
-}

data E = E deriving (Show)
data D0 n = D0 n deriving (Show)
data D1 n = D1 n deriving (Show)
data D2 n = D2 n deriving (Show)
data D3 n = D3 n deriving (Show)
data D4 n = D4 n deriving (Show)
data D5 n = D5 n deriving (Show)
data D6 n = D6 n deriving (Show)
data D7 n = D7 n deriving (Show)
data D8 n = D8 n deriving (Show)
data D9 n = D9 n deriving (Show)

class Reverse x y | x -> y where
    tyReverse :: x -> y
instance (Reverse' x E y) => Reverse x y where
    tyReverse x = tyReverse' x E

class Reverse' x a z | x a -> z where
    tyReverse' :: x -> a -> z
instance Reverse' E a a where
    tyReverse' _ a = a
instance (Reverse' n (D0 a) r) => Reverse' (D0 n) a r where
    tyReverse' (D0 n) a = tyReverse' n (D0 a)
instance (Reverse' n (D1 a) r) => Reverse' (D1 n) a r where
    tyReverse' (D1 n) a = tyReverse' n (D1 a)
instance (Reverse' n (D2 a) r) => Reverse' (D2 n) a r where
    tyReverse' (D2 n) a = tyReverse' n (D2 a)
instance (Reverse' n (D3 a) r) => Reverse' (D3 n) a r where
    tyReverse' (D3 n) a = tyReverse' n (D3 a)
instance (Reverse' n (D4 a) r) => Reverse' (D4 n) a r where
    tyReverse' (D4 n) a = tyReverse' n (D4 a)
instance (Reverse' n (D5 a) r) => Reverse' (D5 n) a r where
    tyReverse' (D5 n) a = tyReverse' n (D5 a)
instance (Reverse' n (D6 a) r) => Reverse' (D6 n) a r where
    tyReverse' (D6 n) a = tyReverse' n (D6 a)
instance (Reverse' n (D7 a) r) => Reverse' (D7 n) a r where
    tyReverse' (D7 n) a = tyReverse' n (D7 a)
instance (Reverse' n (D8 a) r) => Reverse' (D8 n) a r where
    tyReverse' (D8 n) a = tyReverse' n (D8 a)
instance (Reverse' n (D9 a) r) => Reverse' (D9 n) a r where
    tyReverse' (D9 n) a = tyReverse' n (D9 a)

class IncrementRightToLeft x y | x -> y where
    tyIncrementRightToLeft :: x -> y
instance IncrementRightToLeft E (D1 E) where
    tyIncrementRightToLeft _ = (D1 E)
instance IncrementRightToLeft (D0 a) (D1 a) where
    tyIncrementRightToLeft (D0 a) = (D1 a)
instance IncrementRightToLeft (D1 a) (D2 a) where
    tyIncrementRightToLeft (D1 a) = (D2 a)
instance IncrementRightToLeft (D2 a) (D3 a) where
    tyIncrementRightToLeft (D2 a) = (D3 a)
instance IncrementRightToLeft (D3 a) (D4 a) where
    tyIncrementRightToLeft (D3 a) = (D4 a)
instance IncrementRightToLeft (D4 a) (D5 a) where
    tyIncrementRightToLeft (D4 a) = (D5 a)
instance IncrementRightToLeft (D5 a) (D6 a) where
    tyIncrementRightToLeft (D5 a) = (D6 a)
instance IncrementRightToLeft (D6 a) (D7 a) where
    tyIncrementRightToLeft (D6 a) = (D7 a)
instance IncrementRightToLeft (D7 a) (D8 a) where
    tyIncrementRightToLeft (D7 a) = (D8 a)
instance IncrementRightToLeft (D8 a) (D9 a) where
    tyIncrementRightToLeft (D8 a) = (D9 a)
instance (IncrementRightToLeft a b) => IncrementRightToLeft (D9 a) (D0 b) where
    tyIncrementRightToLeft (D9 a) = D0 (tyIncrementRightToLeft a)

class Succ x y | x -> y where
    tySucc :: x -> y
instance (Reverse a a', IncrementRightToLeft a' b', Reverse b' b) => Succ a b where
    tySucc = tyReverse . tyIncrementRightToLeft . tyReverse

class DecrementRightToLeft x y | x -> y where
    tyDecrementRightToLeft :: x -> y
instance DecrementRightToLeft (D9 a) (D8 a) where
    tyDecrementRightToLeft (D9 a) = (D8 a)
instance DecrementRightToLeft (D8 a) (D7 a) where
    tyDecrementRightToLeft (D8 a) = (D7 a)
instance DecrementRightToLeft (D7 a) (D6 a) where
    tyDecrementRightToLeft (D7 a) = (D6 a)
instance DecrementRightToLeft (D6 a) (D5 a) where
    tyDecrementRightToLeft (D6 a) = (D5 a)
instance DecrementRightToLeft (D5 a) (D4 a) where
    tyDecrementRightToLeft (D5 a) = (D4 a)
instance DecrementRightToLeft (D4 a) (D3 a) where
    tyDecrementRightToLeft (D4 a) = (D3 a)
instance DecrementRightToLeft (D3 a) (D2 a) where
    tyDecrementRightToLeft (D3 a) = (D2 a)
instance DecrementRightToLeft (D2 a) (D1 a) where
    tyDecrementRightToLeft (D2 a) = (D1 a)
instance DecrementRightToLeft (D1 a) (D0 a) where
    tyDecrementRightToLeft (D1 a) = (D0 a)
instance (DecrementRightToLeft a b) => DecrementRightToLeft (D0 a) (D9 b) where
    tyDecrementRightToLeft (D0 a) = D9 (tyDecrementRightToLeft a)

class StripLeadingZeros x y | x -> y where
    tyStripLeadingZeros :: x -> y
instance StripLeadingZeros (D0 E) (D0 E) where
    tyStripLeadingZeros = id
instance StripLeadingZeros (D1 a) (D1 a) where
    tyStripLeadingZeros = id
instance StripLeadingZeros (D2 a) (D2 a) where
    tyStripLeadingZeros = id
instance StripLeadingZeros (D3 a) (D3 a) where
    tyStripLeadingZeros = id
instance StripLeadingZeros (D4 a) (D4 a) where
    tyStripLeadingZeros = id
instance StripLeadingZeros (D5 a) (D5 a) where
    tyStripLeadingZeros = id
instance StripLeadingZeros (D6 a) (D6 a) where
    tyStripLeadingZeros = id
instance StripLeadingZeros (D7 a) (D7 a) where
    tyStripLeadingZeros = id
instance StripLeadingZeros (D8 a) (D8 a) where
    tyStripLeadingZeros = id
instance StripLeadingZeros (D9 a) (D9 a) where
    tyStripLeadingZeros = id
instance (StripLeadingZeros a b) => StripLeadingZeros (D0 a) b where
    tyStripLeadingZeros (D0 a) = tyStripLeadingZeros a

class Pred x y | x -> y where
    tyPred :: x -> y
instance ( Reverse a a'
         , DecrementRightToLeft a' b'
         , Reverse b' b''
         , StripLeadingZeros b'' b)
    => Pred a b where
        tyPred = tyStripLeadingZeros . tyReverse . tyDecrementRightToLeft . tyReverse

class Add m n s | m n -> s where
    tyAdd :: m -> n -> s
instance Add (D0 E) (D0 E) (D0 E) where
    tyAdd _ _ = D0 E
instance (Pred m m') => Add m (D0 E) m where
    tyAdd m (D0 E) = m
instance (Pred n n') => Add (D0 E) n n where
    tyAdd (D0 E) n = n
instance (Add m' n' s, Pred m m', Pred n n', Succ s s', Succ s' s'') => Add m n s'' where
    tyAdd m n = tySucc . tySucc $ tyAdd (tyPred m) (tyPred n)

class SmallerThan x y -- x < y
instance (Pred y y') => SmallerThan (D0 E) y
instance (Pred x x', Pred y y', SmallerThan x' y') => SmallerThan x y

class SmallerThanBool x y res | x y -> res where
    isSmallerThan :: x -> y -> res
instance ( Pred' x x' resX
         , Pred' y y' resY
         , SmallerThanBool' x' y' resX resY res
         ) =>
    SmallerThanBool x y res where
        isSmallerThan x y = isSmallerThan' x' y' resX resY
            where
              (x', resX) = tyPred' x
              (y', resY) = tyPred' y

class SmallerThanBool' x y resX resY res | x y resX resY -> res where
    isSmallerThan' :: x -> y -> resX -> resY -> res
instance ( SmallerThanBool x y res
         ) =>
    SmallerThanBool' x y True True res where
        isSmallerThan' x y TT TT = isSmallerThan x y
instance SmallerThanBool' x y False True True where
    isSmallerThan' _ _ FF TT = TT
instance SmallerThanBool' x y True False False where
    isSmallerThan' _ _ TT FF = FF
instance SmallerThanBool' x y False False False where
    isSmallerThan' _ _ FF FF = FF

class Pred' x y res | x -> y res where
    tyPred' :: x -> (y, res)
-- instance Pred' (D0 E) y False where
--     tyPred' _ = (undefined, FF)
-- instance (Pred (D1 a) x') => Pred' (D1 a) x' True where
--     tyPred' x = (tyPred x, TT)
-- instance (Pred (D2 a) x') => Pred' (D2 a) x' True where
--     tyPred' x = (tyPred x, TT)
-- instance (Pred (D3 a) x') => Pred' (D3 a) x' True where
--     tyPred' x = (tyPred x, TT)
-- instance (Pred (D4 a) x') => Pred' (D4 a) x' True where
--     tyPred' x = (tyPred x, TT)
-- instance (Pred (D5 a) x') => Pred' (D5 a) x' True where
--     tyPred' x = (tyPred x, TT)
-- instance (Pred (D6 a) x') => Pred' (D6 a) x' True where
--     tyPred' x = (tyPred x, TT)
-- instance (Pred (D7 a) x') => Pred' (D7 a) x' True where
--     tyPred' x = (tyPred x, TT)
-- instance (Pred (D8 a) x') => Pred' (D8 a) x' True where
--     tyPred' x = (tyPred x, TT)
-- instance (Pred (D9 a) x') => Pred' (D9 a) x' True where
--     tyPred' x = (tyPred x, TT)

class TyNum n
instance TyNum (D0 E)
instance TyNum (D1 E)
instance TyNum (D2 E)
instance TyNum (D3 E)
instance TyNum (D4 E)
instance TyNum (D5 E)
instance TyNum (D6 E)
instance TyNum (D7 E)
instance TyNum (D8 E)
instance TyNum (D9 E)
instance (TyNum n) => TyNum (D0 n)
instance (TyNum n) => TyNum (D1 n)
instance (TyNum n) => TyNum (D2 n)
instance (TyNum n) => TyNum (D3 n)
instance (TyNum n) => TyNum (D4 n)
instance (TyNum n) => TyNum (D5 n)
instance (TyNum n) => TyNum (D6 n)
instance (TyNum n) => TyNum (D7 n)
instance (TyNum n) => TyNum (D8 n)
instance (TyNum n) => TyNum (D9 n)


-- | Convert a type-level number to an Int. Of course, we can only go this way...
class TypeNumberToInt ty where
    tyNumToInt :: ty -> Int
instance TypeNumberToInt (D0 E) where
    tyNumToInt _ = 0
instance (Pred a b, TypeNumberToInt b) => TypeNumberToInt a where
    tyNumToInt n = 1 + (tyNumToInt (tyPred n))
