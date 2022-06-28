
\ignore{

> module Control.Concurrent.SimpleSession.TChan (
>   TChan, newTChan, writeTChan, readTChan
> ) where
> 
> import Control.Concurrent.Chan.Synchronous

}

A |TChan a| is a monomorphic, synchronous channel that can transmit
values of type |a|.

> type TChan a

\ignore{

>   = Chan a
> 
> newTChan = newChan
> 
> writeTChan = writeChan
> 
> readTChan = readChan

}

|TChan| has three operations:

> newTChan   :: IO (TChan a)
> writeTChan :: TChan a -> a -> IO ()
> readTChan  :: TChan a -> IO a
