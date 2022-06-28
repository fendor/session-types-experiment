{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

module WriteOnceWriter where

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.WriteOnceWriter

foo = do put 42
         put "hello"
         return ()
