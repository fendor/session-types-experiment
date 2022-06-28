\subsection{Implicit Channel Examples}
\label{sec:examples:implicit}
\ignore{

> {-# OPTIONS -F -pgmF ixdopp #-}
> module Control.Concurrent.SimpleSession.Examples.Implicit where
>
> import Control.Concurrent (forkIO)
>
> import Control.Concurrent.SimpleSession.Implicit

}

In these examples, we use |ixdo| notation for indexed monads,
analogous to |do| notation for monads.  This syntax is implemented by a
preprocessor.

\paragraph{A print server.}

As an example, we implement a simple print server.
The client side of the print server protocol is:
\begin{enumerate}
  \item Choose either to finish or to continue.
  \item Send a string.
  \item Go to step 1.
\end{enumerate}

We first implement the server.

> server = enter >>>= \_ -> loop where
>   loop = offer close
>                (ixdo
>                   s <- recv
>                   io (putStrLn s)
>                   zero
>                   loop)

GHC's type checker can infer that |server|'s
session type is |Rec (Eps :&: (String :?: Var Z))|.

%include fig-operationtypes.tex

The client reads user input, which it sends to the server for printing.
When the user tells the client to quit, it sends one more string to the
server, tells the server to quit, and closes the channel.

> client = enter >>>= \_ -> loop 0 where
>   loop count = ixdo
>     s <- io getLine
>     case s of
>       "q" -> ixdo
>                sel2
>                send (show count ++ " lines sent")
>                zero; sel1; close
>       _   -> ixdo
>                sel2; send s
>                zero; loop (count + 1)

GHC infers the session type
|Rec (Eps :+: (String :!: Var Z))| for
|client|, which is clearly dual to the type inferred for |server| above.

We run a session by creating a new |Rendezvous|, having the server
accept in a new thread, and having the client request in the main thread.

> runPrintSession = do
>   rv <- newRendezvous
>   forkIO (accept rv server)
>   request rv client

\paragraph{An example of subtyping.}
\label{sec:subtyping}

Our implementation provides a form of protocol subtyping.
Consider a reimplementation of Gay and Hole's \citeyearpar{r:gay99}
arithmetic server, which provides two services, addition and negation:

> server1 = offer
>   (ixdo a <- recv
>         b <- recv
>         send (a + b)
>         close)
>   (ixdo a <- recv
>         send (-a)
>         close)

The full protocol for |server1| is inferred:

< (Integer :?: Integer :?: Integer :!: Eps) :&:
< (Integer :?: Integer :!: Eps)

A second server implements only the negation service:

> server2 = offer
>   close
>   (ixdo a <- recv
>         send (-a)
>         close)

Its protocol is inferred as well:

< Eps :&: (Integer :?: Integer :!: Eps)

A particular client may avail itself of only
one of the offered services:

> client' x = ixdo sel2; send x; y <- recv; close; ireturn y

The client's protocol is inferred as |r :+: (a :!: b :?: Eps)|, which
unifies with the duals of both servers' protocols.  Without the
functional dependencies in |Dual|, however, attempting to connect the
client with |server2| leads the type checker to complain that there
is no instance of |Dual| for |r| and |Eps|; connecting |client| with
|server1| also fails to type check.  The functional dependency nudges
the type checker towards attempting to unify |r| with the corresponding
part of either server's type, which then succeeds.  As a result, the
|client| may be composed with both servers in the same program and
never notices the difference.

