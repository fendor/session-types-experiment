\section{Session Types in Haskell}
\label{sec:session}

\ignore{

> {-# LANGUAGE TypeOperators,
>              EmptyDataDecls,
>              MultiParamTypeClasses,
>              FunctionalDependencies,
>              UndecidableInstances #-}
> module Control.Concurrent.SimpleSession.SessionTypes (
>   Z, S,
>   Eps, (:!:), (:?:), (:+:), (:&:), Rec, Var,
>   Dual
> ) where
> 
> infixr 3 :!:, :?:
> infix 2 :+:, :&:
>
> data Z
> data S n

}

The central idea of session types \citep{r:gay99} is to parameterize a
channel with some type that represents a protocol, which the type system
then enforces.  In Haskell, we may encode a protocol using ordinary
datatypes:

> data (:!:) a r
> data (:?:) a r
> data Eps

These datatypes require no constructors because they will have no
run-time representation.

If |a| is any type, and |r| is a protocol, then
we interpret |a :!: r| as the protocol, ``first send an |a|, and
then continue with |r|.''  Similarly, we interpret |a :?: r| as
the protocol, ``receive
an |a|, and then continue with |r|.''  The type |Eps| represents the empty
protocol of a depleted channel that is not yet closed.

For example, the type |Int :!: Bool :?: Eps| represents the protocol,
``send an |Int|, receive a |Bool|, and close the
channel.''\thinspace\footnote{The type constructors |(:!:)| and
|(:?:)| are declared right associative and with higher precedence than
|(:+:)| and |(:&:)|.}

If the process on one end of a channel speaks a particular protocol,
its correspondant at the other end of the channel must be prepared to
understand it.  For example, if one process speaks |Int :!: Bool :?: Eps|,
the other process must implement the dual protocol
|Int :?: Bool :!: Eps|.  We encode the duality relation using a type
class with multiple parameters and functional dependencies
\citep{Jones1997Type,Jones2000Type}.

> class Dual r s | r -> s, s -> r

The functional dependencies indicate that duality
is bijective, which helps Haskell to infer
protocols and enables a form of subtyping.  Sending and receiving are
dual:  if |r| is dual to |s|, then |a :!: r| is dual to |a :?: s|.  The
empty session is dual to itself.

> instance Dual r s => Dual (a :!: r) (a :?: s)
> instance Dual r s => Dual (a :?: r) (a :!: s)
> instance Dual Eps Eps

\par
Our session types also represent alternation and recursion.  If |r|
and |s| are protocols, then |r :+: s| represents an active choice
between following |r| or |s|.  The type |r :&: s| represents
an offer to follow either |r| or |s|, as chosen by the other
process.

> data (:+:) r s
> data (:&:) r s

The two alternation operators are dual:

> instance (Dual r1 s1, Dual r2 s2) =>
>                    Dual (r1 :+: r2) (s1 :&: s2)
> instance (Dual r1 s1, Dual r2 s2) =>
>                    Dual (r1 :&: r2) (s1 :+: s2)

\par
Recursion turns out to be slightly more difficult.  It is tempting to
use a fixed-point combinator, but this would require constructing a
type of kind $\star \to \star$ for any desired loop body, which
is not generally possible.  We need
some other way for a recursive type to refer to itself, so we represent
this binding using de~Bruijn indices.

> data Rec r
> data Var v
> 
> instance Dual r s => Dual (Rec r) (Rec s)
> instance Dual (Var v) (Var v)

The type |Rec r| adds a binding for |r| inside |r|; that is, it
implicitly defines a variable bound to the whole of |r| that can be used
\emph{within} |r|.  We use |Var v| to refer to the variable bound by
the |v|th |Rec|, counting outward, where |v| is a Peano numeral
written with type constructors |Z| and |S|
(\emph{e.g.,} |Z| or |S (S Z)|).  For example, the protocol

< Request :!: Rec (Response :?: (Var Z :&: Eps))

says to send a request and then be prepared to receive one or more
responses.  By contrast, a process implementing the protocol

< Request :!: Rec ((Response :?: Var Z) :&: Eps)

must send a request and be prepared to accept any number of responses.

