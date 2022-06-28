\ignore{

> {-# LANGUAGE MagicHash #-}
> module Control.Concurrent.SimpleSession.UChan (
>   UChan, newUChan, unsafeReadUChan, unsafeWriteUChan
> ) where
> 
> import GHC.Exts
> import Control.Concurrent.SimpleSession.TChan

}

On top of |TChan| we have implemented |UChan|, an untyped,
synchronous channel:

> newtype UChan

\ignore{

>   = CC (TChan Int)
> 
> unUChan (CC c) = unsafeCoerce# c
> 
> newUChan         = newTChan >>= return . CC
> unsafeWriteUChan = writeTChan . unUChan
> unsafeReadUChan  = readTChan . unUChan

}

Like |TChan|, |UChan| has three operations:

> newUChan         :: IO UChan
> unsafeWriteUChan :: UChan -> a -> IO ()
> unsafeReadUChan  :: UChan -> IO a

Note that since |UChan| is willing to send or receive a value of \emph{any}
type, it's unsafe unless we find some other way to restrict it.

