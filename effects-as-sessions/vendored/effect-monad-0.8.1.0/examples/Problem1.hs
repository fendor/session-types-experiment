{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Problem1 where

import Control.Monad.State.Lazy

appendBuffer x = do buff <- get
                    put (buff ++ x)

hello :: Monad m => StateT String (StateT String m) ()
hello = do name <- get
           lift $ appendBuffer $ "hello " ++ name

incSC :: (Monad m) => StateT Int m ()
incSC = do x <- get
           put (x + 1)

writeS :: (Monad m) => [a] -> StateT [a] m ()
writeS y = do x <- get
              put (x ++ y)

write :: (Monad m) => [a] -> StateT [a] (StateT Int m) ()
write x = do  writeS x
              lift $ incSC

hellow = do write "hello"
            write " "
            write "world"

runHellow = runStateT (runStateT hellow "") 0

-- prog2 :: (Monad m) => StateT Int (StateT String (StateT Int (StateT String m))) ()

hellowCount = do hellow
                 lift $ lift $ incSC

runHellowCount = runStateT (runStateT (runStateT hellowCount "") 0) 1
