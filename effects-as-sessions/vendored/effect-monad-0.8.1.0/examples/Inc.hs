{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Inc where

import           Control.Effect
import           Control.Effect.State
import           GHC.TypeLits
import           Prelude               hiding (log, Monad(..), (>>))

varX :: Var "x"
varX = Var

inc :: State '["x" :-> Int :! 'RW] ()
inc =
    get varX >>= (put varX . (+1))

-- No instance for (Control.Effect.State.Nubable '["x" :-> (Int :! 'W)] '[])
-- inc2 =
--     inc >>=
--     \_ ->
--          inc
