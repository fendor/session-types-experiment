\section{Take $n$: Multiple Channels}
\label{sec:positional}

\ignore{

> {-# LANGUAGE TypeOperators,
>              EmptyDataDecls,
>              Rank2Types #-}
>
> module Control.Concurrent.SimpleSession.Positional (
>   module Control.Concurrent.SimpleSession.SessionTypes,
>   module Control.Monad.Indexed,
>   Session, Cap, Channel,
>   io,
>   send, recv, close, sel1, sel2, offer,
>   send_cap, recv_cap,
>   enter, zero, suc,
>   dig, swap, forkSession,
>   Rendezvous, newRendezvous,
>   accept, request, runSession
> ) where
>
> import Control.Concurrent (forkIO)
>
> import Control.Concurrent.SimpleSession.UChan
> import Control.Concurrent.SimpleSession.TChan
> import Control.Monad.Indexed
> import Control.Concurrent.SimpleSession.SessionTypes
> import Control.Monad (ap)
>
> newtype Rendezvous r = Rendezvous (TChan UChan)
>
> newRendezvous :: IO (Rendezvous r)
> newRendezvous  = newTChan >>= return . Rendezvous
> 
> recv  :: Channel t -> Session (Cap t e (a :?: r), x) (Cap t e r, x) a
> close :: Channel t -> Session (Cap t e Eps, x) x ()
> sel1  :: Channel t -> Session (Cap t e (r :+: s), x) (Cap t e r, x) ()
> sel2  :: Channel t -> Session (Cap t e (r :+: s), x) (Cap t e s, x) ()
> offer :: Channel t -> Session (Cap t e r, x) u a -> Session (Cap t e s, x) u a -> Session (Cap t e (r:&:s), x) u a
> enter :: Channel t -> Session (Cap t e (Rec r), x) (Cap t (r, e) r, x) ()
> zero  :: Channel t -> Session (Cap t (r, e) (Var Z), x) (Cap t (r, e) r, x) ()
> suc   :: Session (Cap t (r, e) (Var (S v)), x) (Cap t e (Var v), x) ()
>
> _cast = Session . unSession
> recv (Channel c) = Session (unsafeReadUChan c)
> sel1 c  = _cast (send c True)
> sel2 c  = _cast (send c False)
> offer c l r = _cast (recv c) >>>= \choice ->
>               if choice
>                 then _cast l
>                 else _cast r
> close _ = _cast (ireturn ())
> enter _ = _cast (ireturn ())
> zero  _ = _cast (ireturn ())
> suc     = _cast (ireturn ())

}

Rather than limit ourselves to one implicit channel at a time, it might
be more flexible to work with several channels at once.  To extend |Session| to
handle multiple channels, our first step is to separate the channel
itself from the capability to use it for a particular session:

> newtype Channel t = Channel UChan
> data Cap t e r

The parameter |t| is a unique tag that ties a given channel to the
capability to use it.  A |Channel t| is an actual value at run time,
while the corresponding |Cap t e r| is relevant only during type-checking.
We allow |Channel t| to be aliased freely because
a channel is unusable without its capability, and we treat capabilities
linearly.  As before, the capability also contains a session type
environment |e| and a session type |r| that is closed in |e|.

We now index |Session| by a \emph{stack} of capabilities, while
underneath the hood, it is just the |IO| monad.  |Session| is no longer
responsible for maintaining the run-time representation of channels, but
instead it keeps track of the compile-time representation of
capabilities.

> newtype Session s s' a = Session { unSession :: IO a }
>
> instance IxFunctor Session where
>   imap f = Session . fmap f . unSession
> instance IxPointed Session where
>   ireturn = Session . return
> instance IxApplicative Session where
>   iap (Session f) (Session x) = Session (f `ap` x)
> instance IxMonad Session where
>   ibind k m = Session (unSession m >>= unSession . k)
>
> io :: IO a -> Session s s a
> io  = Session

< instance IxMonad Session where
<   ret      = Session . return
<   m >>>= k = Session (unSession m >>= unSession . k)

\par
A |Session| computation now carries a stack of capability types, and
communication operations manipulate only the top capability on the
stack, leaving the rest of the stack unchanged.
The |send| operation takes a channel
as an argument rather than obtaining it implicitly, and the tag |t| on
the channel must match the tag in the capability.

> send :: Channel t -> a ->
>         Session (Cap t e (a :!: r), x)
>                 (Cap t e r, x) ()
> send (Channel c) a = Session (unsafeWriteUChan c a)

In the type above, |Cap t e (a :!: r)| is the capability on the top
of the stack before the |send|, and |Cap t e r| is the capability
after the |send|.  Type variable |x| represents the rest of the
capability stack, which is unaffected by this operation.

The implementations of the remaining operations are similarly unsurprising.
Each differs from the previous section only in obtaining a channel
explicitly from its argument rather than implicitly from the indexed
monad.  Their types may be found in Figure~\ref{fig:operationtypes}.
Note that |close| now has the effect of popping the capability for the
closed channel from the top of the stack.

\paragraph{Stack manipulation.}

Channel operations act on the top of the capability stack.  Because the
capability for the particular channel we wish to use may not be on the
top of the stack, we may need to use other capabilities than the top
one.  The |dig| combinator suffices to
select any capability on the stack.  Given a |Session| computation that
transforms a stack |x| to a stack |x'|, |dig| lifts it to a computation
that transforms |(r, x)| to |(r, x')| for any |r|; thus, $n$
applications of |dig| will select the $n$th capability on the stack.
Note that |dig| has no run-time effect, but merely unwraps and rewraps 
a |Session| to change the phantom type parameters.

> dig  :: Session x x' a -> Session (r, x) (r, x') a
> dig   = Session . unSession

In combination with |swap|, we may generate any desired stack permutation.
Since |swap| exchanges the top two capabilities on the stack, |dig| and
|swap| may be combined to exchange any two adjacent capabilities.

> swap :: Session (r, (s, x)) (s, (r, x)) ()
> swap  = Session (return ())

\par
One reason we may want to rearrange the stack is to support |forkSession|,
which runs a |Session| computation in a new thread, giving to it
the entire \emph{visible} stack.  Thus, to partition the stack
between the current thread and a new thread, we use |dig| and |swap|
until all the capabilities for the new thread are below all the
capabilities for the current thread.  Then we call |forkSession|
under sufficiently many |dig|s so that it takes only the desired capabilities
with it.

> forkSession :: Session x () () -> Session x () ()
> forkSession (Session c)
>              = Session (forkIO c >> return ())

For example, to keep the top two capabilities on the stack
for the current thread and assign the rest to a new thread |m|, we
would use |dig (dig (forkSession m))|.

\paragraph{Making a connection.}

In the implicit channel case, each |accept| or |request| starts
a single |Session| computation that runs to completion.
Because we now have multiple channels, we may need
to use |accept| and |request| to start new communication
sessions during an ongoing |Session| computation.
Given a |Rendezvous| and a continuation
of matching session type, |accept|
creates a new channel/capability pair.  It calls the continuation with
the channel, pushing the corresponding capability on the top of its
stack.  The \mbox{rank-2} type in |accept| ensures that the new |Channel t|
and |Cap t () r| cannot be used with any other capability or channel.
In \Section\ref{sec:discussion} we discuss an alternate formulation that
does not require higher-rank polymorphism, but this version here seems
more elegant.

> accept :: Rendezvous r ->
>           (forall t. Channel t ->
>             Session (Cap t () r, x) y a) ->
>           Session x y a
> accept (Rendezvous c) f = Session (do
>   nc <- newUChan
>   writeTChan c nc
>   unSession (f (Channel nc)))

The |request| function behaves similarly, but as before, it
uses the dual session type.

> request :: Dual r r' =>
>            Rendezvous r ->
>            (forall t. Channel t ->
>              Session (Cap t () r', x) y a) ->
>            Session x y a
> request (Rendezvous c) f = Session (do
>   nc <- readTChan c
>   unSession (f (Channel nc)))

We may start a |Session| computation from within the IO monad.  The type
of |runSession| ensures that the computation both begins and ends with
no capabilities in the stack.

> runSession :: Session () () a -> IO a
> runSession  = unSession

\paragraph{Sending capabilities.}

Now that we have multiple channels, we might wonder whether we can send
capabilities themselves over a channel.  Certainly, but since we do
not allow direct access to capabilities, this requires a specialized
pair of functions.

> send_cap :: Channel t ->
>             Session (Cap t e (Cap t' e' r' :!: r),
>                      (Cap t' e' r', x))
>                     (Cap t e r, x) ()
> send_cap (Channel c)
>           = Session (unsafeWriteUChan c ())
>
> recv_cap :: Channel t ->
>             Session (Cap t e (Cap t' e' r' :?: r), x)
>                     (Cap t e r, (Cap t' e' r', x)) ()
> recv_cap (Channel c) = Session (unsafeReadUChan c)

Observe that because capabilities have no run-time existence, the actual
value sent over the channel is |()|.  This provides synchronization so
that the receiving process does not perform channel operations
with the capability before the sending process has finished its part.
The phantom type parameters to |Session| change to reflect the
transmission of the capability.

%include PositionalExample.lhs

