\section{Take 1: One Implicit Channel}
\label{sec:implicit}

\ignore{

> {-# LANGUAGE TypeOperators,
>              EmptyDataDecls,
>              MultiParamTypeClasses,
>              FunctionalDependencies,
>              FlexibleInstances,
>              FlexibleContexts,
>              UndecidableInstances #-}
>
> module Control.Concurrent.SimpleSession.Implicit (
>   module Control.Concurrent.SimpleSession.SessionTypes,
>   module Control.Monad.Indexed,
>   Session, Cap,
>   io,
>   send, recv, close, sel1, sel2, offer,
>   enter, zero, suc, Pop(pop),
>   Rendezvous, newRendezvous,
>   accept, request
> ) where
> 
> import Control.Concurrent.SimpleSession.TChan
> import Control.Concurrent.SimpleSession.UChan
> import Control.Monad.Indexed
> import Control.Concurrent.SimpleSession.SessionTypes
> import Control.Monad (ap)

}

Encoding protocols in Haskell is not enough.  We cannot merely provide
channels parameterized by session types and call it a day.  For example,
consider a hypothetical |send| operation:

< send :: Channel (a :!: r) -> a -> IO (Channel r)

While this |send| returns the correct channel for the rest of the
session, it fails to prevent reuse of the |a :!: r| channel,
which would violate the protocol.  One way to avoid this problem is to
require that channels (or at least their sessions) be treated linearly.
In this section, we show how this is done for
processes having access to only one channel, which is left implicit in the
environment; in the next section, we implement multiple
concurrent channels.

We assume a substrate of synchronous channels in both typed and
untyped varieties:

< writeTChan       :: TChan a -> a -> IO ()
< readTChan        :: TChan a -> IO a
<
< unsafeWriteUChan :: UChan -> a -> IO ()
< unsafeReadUChan  :: UChan -> IO a

These channels have dynamic semantics similar to Concurrent ML's
\citep{Reppy1991CML} synchronous channels.  While |TChan|s transmit
only a single type, |UChan|s are indiscriminating about what
they send and receive.  In our implementation, they use |unsafeCoerce#|,
which can lead to undefined
behavior if sent and received types differ.  We must somehow impose our
own type discipline.

We define an abstract type |Session s s' a|, which represents a computation that
evolves a session from state |s| to state |s'| while producing a value of
type |a|.  |Session|'s constructor is not
exported to client code, so that clients of the library
cannot arbitrarily modify the session state.
|Session| is implemented as the composition of the IO monad with
a reader monad carrying a untyped channel.

> newtype Session s s' a =
>   Session { unSession :: UChan -> IO a }

The phantom parameters |s| and |s'| must track more information than
just the current session.  We define a type constructor |Cap| to hold
not only the current session |r|, but another type |e|, which represents
a session type environment:

> data Cap e r

The type |Cap e r| represents the capability to run the protocol |r|.
The session type environment |e| provides context for any
free variables |Var v| in |r|; that is, |r| must be closed in |e|.
We discuss |e| in more detail when we
explain recursion, and the other operations merely thread it through.

We can now give |send| a type and definition that will work:

> send  :: a -> Session (Cap e (a :!: r)) (Cap e r) ()
> send x = Session (\c -> unsafeWriteUChan c x)

Given an |a|, |send| evolves the session from
|a :!: r| to |r|.
In its implementation, |unsafeWriteUChan|
indiscriminately transmits values of any type over an untyped channel.
Thus, if we fail to ensure that the receiving process expects a value of
type |a|, things can go very wrong.  In \Section\ref{sec:theory}, we
argue that this cannot happen.

Predictably, |recv| requires the capability to receive an |a|, which it
then produces:

> recv  :: Session (Cap e (a :?: r)) (Cap e r) a
> recv   = Session unsafeReadUChan

We use |close| to discard an exhausted capability, replacing it
with |()|.
In this implementation, |close| is a run-time no-op.

> close :: Session (Cap e Eps) () ()
> close  = Session (\_ -> return ())

\paragraph{Composing computations.}

We also need a way to compose |Session| computations.  Composing a
session from state $s_1$ to $s_2$ with a session from state $t_1$ to
$t_2$ should be permitted only if $s_2 = t_1$.  This is precisely the
situation that \emph{indexed monads} capture.

%include IxMonad.lhs

The |IxMonad| instance for |Session| is then straightforward.  It
threads the implicit channel through and runs the underlying
computations in the |IO| monad.

> instance IxFunctor Session where
>   imap f m = Session (fmap f . unSession m)
>
> instance IxPointed Session where
>   ireturn x = Session (const $ return x)
>
> instance IxApplicative Session where
>   iap f x = Session (\r -> unSession f r `ap` unSession x r)
>
> instance IxMonad Session where
>   ibind k m = Session (\r -> unSession m r >>= \a ->
>                              unSession (k a) r)

< instance IxMonad Session where
<   ret a    = Session (\_ -> return a)
<   m >>>= k = Session (\c -> do a <- unSession m c
<                                unSession (k a) c)

We use |io| to lift an arbitrary |IO| computation into |Session|:

> io    :: IO a -> Session s s a
> io m   = Session (\_ -> m)

Because of |io|, this implementation is actually not linear but affine:
an |IO| action may raise an exception and terminate the |Session|
computation.  Provided that exceptions cannot be caught within a
|Session|, this does not jeopardize safety in the sense that any
messages received will still have the expected representation.  Some
formulations of session types guarantee that a session, once initiated,
will run to completion, but this seems unrealistic for real-world
programs.  Handling exceptions from within a session remains an open
problem.

\paragraph{Alternation.}

The session actions |sel1|, |sel2|, and |offer| implement alternation.
Action |sel1| selects the left side of an ``internal choice'',
thereby replacing a session |r :+: s| with the session |r|; |sel2|
selects the right side.  On the other side of the channel, |offer| combines a
|Session| computation for |r| with a computation for |s| into a
computation that can handle |r :&: s|.  Dynamically, |sel1| sends |True|
over the channel, whereas |sel2| sends |False|, and |offer| dispatches
on the boolean value received.

> sel1  :: Session (Cap e (r :+: s)) (Cap e r) ()
> sel1   = Session (\c -> unsafeWriteUChan c True)
> 
> sel2  :: Session (Cap e (r :+: s)) (Cap e s) ()
> sel2   = Session (\c -> unsafeWriteUChan c False)
> 
> offer :: Session (Cap e r) u a ->
>          Session (Cap e s) u a ->
>          Session (Cap e (r :&: s)) u a
> offer (Session m1) (Session m2)
>        = Session (\c -> do b <- unsafeReadUChan c
>                            if b then m1 c else m2 c)

\paragraph{Recursion.}

Session actions |enter|, |zero|, and |suc| implement recursion.
Consider the recursive session type

< Request :!: Rec ((Response :?: Var Z) :&: Eps)

from above.  After sending a |Request|, we need some way to enter the
body of the |Rec|, and upon reaching |Var Z|, we need some way to repeat
the body of the |Rec|.  We accomplish the former with |enter|, which
strips the |Rec| constructor from |r| and pushes |r| onto the stack |e|:

> enter :: Session (Cap e (Rec r)) (Cap (r, e) r) ()
> enter  = Session (\_ -> return ())

In |e|, we maintain a stack of session types for the body of each enclosing
|Rec|, representing an environment that closes over |r|.  Upon
encountering a variable occurence |Var |$n$, where $n$ is a Peano
numeral, we restore the
$n$th session type from the stack and return the stack to its former
state, using $n$ expressed with |zero| and |suc|:

> zero  :: Session (Cap (r, e) (Var Z))
>                  (Cap (r, e) r) ()
> zero   = Session (\_ -> return ())
>
> suc   :: Session (Cap (r, e) (Var (S v)))
>                  (Cap e (Var v)) ()
> suc    = Session (\_ -> return ())

For example, if the current session is |Var (S (S Z))|, then the operation

< suc >>> suc >>> zero

pops two elements from the stack and
replaces the current session with the body of the third enclosing |Rec|.

It is worth remarking that this duplication of type and code to pop the
stack is not strictly necessary.  If we explicitly
write |suc >>> suc >>> zero|, Haskell's type checker can infer |S (S Z)|.  If,
on the other hand, the type is already known, then a type class can do
the work:\footnote{Note that the definition of the method |pop| is the
same for both instances of |Pop|, which suggests that it could
be provided as a default method.  This would introduce a subtle bug,
however, as it would enable defining new instances of |Pop| with
arbitrary effect.}

> class Pop s s' | s -> s' where pop :: Session s s' ()
> 
> instance Pop (Cap (r, e) (Var Z)) (Cap (r, e) r)
>   where pop = Session (\_ -> return ())
>
> instance Pop (Cap e (Var v)) (Cap e' r') =>
>          Pop (Cap (r, e) (Var (S v))) (Cap e' r')
>   where pop = Session (\_ -> return ())

\paragraph{Putting it all together.}

Finally, we need a way to connect and run sessions.

A |Rendezvous| is a synchronization object that connects the types of
two processes at compile time, and then enables their connection by a
channel at run time.  The |Rendezvous| carries a phantom parameter
indicating the protocol to be spoken on the shared implicit channel,
and is represented by a
homogeneous, typed channel on which the untyped channel for a particular
session will later be exchanged.  Creating a |Rendezvous| is as simple
as creating a new typed channel and wrapping it.

> newtype Rendezvous r = Rendezvous (TChan UChan)
> 
> newRendezvous :: IO (Rendezvous r)
> newRendezvous  = newTChan >>= return . Rendezvous

\par
To accept a connection request, we need a |Rendezvous| object,
and a |Session| computation whose starting session type matches that of
the |Rendezvous|.  The computation must deplete and close its channel.
At run time, |accept| creates a new untyped channel on which
the communication will take place and sends it over the |Rendezvous|
channel.  It then runs the session computation on the new channel.

> accept :: Rendezvous r ->
>           Session (Cap () r) () a -> IO a
> accept (Rendezvous c) (Session f) = do
>   nc <- newUChan
>   writeTChan c nc
>   f nc

\par
To request a connection, the session type of the |Session| computation
must be dual to that of the given |Rendezvous|.  At run time,
|request| receives a new, untyped channel from |accept| over the
|Rendezvous| channel and then runs the computation using the channel.

> request :: Dual r r' => Rendezvous r ->
>            Session (Cap () r') () a -> IO a
> request (Rendezvous c) (Session f)
>          = readTChan c >>= f

%include ImplicitExample.lhs
