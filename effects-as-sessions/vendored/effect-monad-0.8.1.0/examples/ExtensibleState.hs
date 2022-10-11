{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module ExtensibleState where

import Prelude hiding (Monad(..), MonadFail(..))
import Control.Effect.Parameterised
import Control.Effect.Parameterised.ExtensibleState
import GHC.TypeLits
import Data.Type.Map

-- Example map
exMap :: Map '["x" ':-> Int, "flag" ':-> Bool]
exMap = Ext (Var @"x") (42 :: Int)
      $ Ext (Var @"flag") False
        Empty

increment :: (Update "x" Int m) => State (Map m) (Map m) ()
increment = do
   (n :: Int) <- get (Var @"x")
   put (Var @"x") (n+1)

example ::
   forall m.
   ( Update "x" Int m
   , Get "flag" Bool m
   , Put "flag" Int m m
   , Put "x" String m m
   ) =>
   State
      (Map m)
      (Map m)
      ()
example = do
   flag :: Bool <- get (Var @"flag")
   increment
   (n :: Int) <- get (Var @"x")
   put @"flag" @Int @m @m (Var @"flag") n
   put @"x" @String @m @m (Var @"x") "test"


-- go :: ((), Map '["x" ':-> String, "flag" ':-> Int])
go = runState example exMap

example2 :: (Get "flag" Bool m, Update "x" Int m, Put "y" Int m m) => State (Map m) (Map m) ()
example2 = do
   flag <- get (Var @"flag")
   if flag
     then modify (Var @"x") (\(x :: Int) -> x + 1)
     else put (Var @"y") (42 :: Int)
