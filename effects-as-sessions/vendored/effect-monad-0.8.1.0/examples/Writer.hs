{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, RebindableSyntax,
             FlexibleInstances, ConstraintKinds, FlexibleContexts,
             TypeFamilies, ScopedTypeVariables #-}
module Writer where

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.Writer

import Data.Monoid as M

var_x = Var::(Var "x")
var_y = Var::(Var "y")

test :: Writer '["x" :-> M.Sum Int, "y" :-> String] ()
test = do
   put var_x (42::M.Sum Int)
   put var_y "hello"
   put var_x (58::M.Sum Int)
   put var_y " world"

--test' :: forall a . (Monoid a, Num a) => a -> Writer '["x" :-> a, "y" :-> String] ()
test' (n::a) = do
   put var_x (42::a)
   put var_y "hello"
   put var_x (n::a)
   put var_y " world"

{-- Polymorphism test -}
test2 :: (IsMap f, Unionable f '["y" :-> String]) =>
         (Int -> Writer f t) -> Writer (Union f '["y" :-> String]) ()
test2 f = do
   f 3
   put var_y ". hi"

{-- Subeffecting test -}
test3 :: Writer '["x" :-> M.Sum Int, "y" :-> String, "z" :-> M.Sum Int] ()
test3 = sub (test2 test')

foo2 :: (IsMap f, Unionable f '["x" :-> M.Sum Int, "y" :-> t], Num a) =>
       (a -> Writer f t) -> Writer (Union f '["x" :-> M.Sum Int, "y" :-> t]) ()
foo2 f = do
    y <- f 3
    put var_x (42::M.Sum Int)
    put var_y y
