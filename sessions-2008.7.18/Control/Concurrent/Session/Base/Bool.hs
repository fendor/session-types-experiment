{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , KindSignatures
           , TypeFamilies #-}

{-
    Bool.hs
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

module Control.Concurrent.Session.Base.Bool where

infixr 1 :$
infixr 9 :.
type (:$) (a :: * -> *) b = a b
type (:.) (a :: (* -> *)) (b :: (* -> *)) (c :: *) = a (b c)

data True = TT deriving (Show)
data False = FF deriving (Show)

class And x y z | x y -> z where
    tyAnd :: x -> y -> z
instance And True True True where
    tyAnd TT TT = TT
instance And True False False where
    tyAnd TT FF = FF
instance And False y False where
    tyAnd FF _ = FF

class Or x y z | x y -> z where
    tyOr :: x -> y -> z
instance Or True y True where
    tyOr TT _ = TT
instance Or False True True where
    tyOr FF TT = TT
instance Or False False False where
    tyOr FF FF = FF

class If c x y z | c x y -> z where
    tyIf :: c -> x -> y -> z
instance If True x y x where
    tyIf TT x _ = x
instance If False x y y where
    tyIf FF _ y = y

class Not x y | x -> y where
    type NotT x
    tyNot :: x -> y
instance Not True False where
    type NotT True = False
    tyNot TT = FF
instance Not False True where
    type NotT False = True
    tyNot FF = TT
