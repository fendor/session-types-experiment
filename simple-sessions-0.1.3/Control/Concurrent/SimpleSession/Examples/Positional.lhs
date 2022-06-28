\ignore{

> {-# OPTIONS -F -pgmF ixdopp #-}
> {-# LANGUAGE TypeOperators #-}
> module Control.Concurrent.SimpleSession.Examples.Positional where
>
> import Control.Concurrent (threadDelay)
> import Random
>
> import Control.Concurrent.SimpleSession.Positional

}

\subsection{An Example with Multiple Channels}
\label{sec:example:multiple}

As an example, we give an implementation of the Sutherland-Hodgman
\citeyearpar{Sutherland1974Reentrant} reentrant polygon clipping algorithm,
which takes a plane and a series of points representing the
vertices of a polygon,
and produces vertices for the polygon restricted to one side of the plane.
\Citet{Shivers2006Continuations}
present a stream transducer implementation, which we follow.  Each
transducer takes one plane to clip by, and two |Rendezvous| objects for
the same protocol.  It connects on both, and then receives original points
on one channel and sends clipped points on the other.

We assume that we have types |Plane| and |Point|, a predicate
|above| that indicates whether a given point is on the visible side
of a given plane, and a partial function |intersection| that computes
where the line segment between two points intersects a plane.

GHC infers all the types in this example.

< type SendList a = Rec (Eps :+: (a :!: Var Z))
<
< clipper :: Plane -> Rendezvous (SendList Point)
<            -> Rendezvous (SendList Point)
<            -> Session x x ()
< clipper plane inrv outrv =
<   accept outrv $ \oc ->
<   request inrv $ \ic -> ixdo
<   let shutdown = ixdo close ic; sel1 oc; close oc
<       put pt   = dig $ ixdo
<         sel2 oc; send oc pt; zero oc
<       -- Attempt to get a point; pass it to yes, or
<       -- call no if there are no more:
<       get no yes = offer ic no $ ixdo
<         pt <- recv ic; zero ic; yes pt
<       -- If the line crosses the plane, send the intersection point:
<       putCross line =
<         maybe (ireturn ()) put (line `intersect` plane)
<       putIfVisible pt   =
<         if pt `above` plane then put pt else ireturn ()
<   dig (enter oc)
<   enter ic
<   get shutdown $ \pt0 ->
<     let loop pt = ixdo
<           putIfVisible pt
<           get (putcross (pt, pt0) >>>= \_ -> shutdown)
<               (\pt' -> ixdo putcross (pt,pt')
<                             loop pt')
<      in loop pt0

\par
We use |sendlist| to send a list of points to the first
transducer in the pipeline, and we use |recvlist| to accumulate
points produced by the last transducer.

< sendlist :: [a] -> Rendezvous (SendList a)
<          -> Session x x ()
< sendlist xs rv = accept rv start where
<   start oc = enter oc >>>= \_ -> loop xs where
<     loop []     = ixdo sel1 oc; close oc
<     loop (x:xs) = ixdo sel2 oc; send oc x
<                        zero oc; loop xs
<
< recvlist :: Rendezvous (SendList a) -> Session x x [a]
< recvlist rv = request rv start where
<   start ic = enter ic >>>= \_ -> loop [] where
<     loop acc = offer ic
<       (close ic >>>= \_ -> ireturn (reverse acc))
<       (recv ic >>>= \x -> zero ic >>>= \_ -> loop (x : acc))

\par
Given a list of planes and a list of points, |clipMany| starts a |clipper|
for each plane in a separate thread.  It starts |sendlist| a new thread,
giving it the list of points and connecting it to the first |clipper|.
It then runs |recvlist| in the main thread to gather up the result.

< clipMany :: [Plane] -> [Point] -> IO [Point]
< clipMany planes points = runSession $ ixdo
<     rv <- io newRendezvous
<     forkSession (sendlist points rv)
<     let loop []     rv = recvlist rv
<         loop (p:ps) rv = ixdo
<           rv' <- io newRendezvous
<           forkSession (clipper p rv rv')
<           loop ps rv'
<     loop planes rv

\ignore{

< bench n m = do
<   g1 <- getStdRandom split
<   g2 <- getStdRandom split
<   let groupPoints (x:y:z:r) = Point x y z   : groupPoints r
<       groupPlanes (a:b:c:r) = Plane a b c 0 : groupPlanes r
<       points = groupPoints [ 10 * (x - 0.5) | x <- randoms g1 ]
<       planes = groupPlanes [ 10 * (x - 0.5) | x <- randoms g2 ]
<   points <- clipMany (take m planes) (take n points)
<   print (length points)
<
< data Point = Point !Double !Double !Double
< data Plane = Plane !Double !Double !Double !Double
<
< instance Show Point where
<   showsPrec _ (Point x y z) =
<     ('(':) . shows x . (", "++) . shows y . (", "++) . shows z . (')':)
<
< instance Show Plane where
<   showsPrec _ (Plane a b c d) =
<     shows a . ("x + "++) . shows b . ("y + "++) .
<     shows c . ("z + "++) . shows d . (" = 0"++)
< 
< above :: Point -> Plane -> Bool
< above (Point x y z) (Plane a b c d)
<        = (a * x + b * y + c * z + d) / sqrt (a * a + b * b + c * c) > 0
< 
< intersect :: (Point, Point) -> Plane -> Maybe Point
< intersect (p1@(Point x1 y1 z1), p2@(Point x2 y2 z2)) plane@(Plane a b c d)
<            = if above p1 plane == above p2 plane
<              then Nothing
<              else Just (Point x y z) where
<                x = x1 + (x2 - x1) * t
<                y = y1 + (y2 - y1) * t
<                z = z1 + (z2 - z1) * t
<                t = (a * x1 + b * y1 + c * z1 + d) /
<                    (a * (x1 - x2) + b * (y1 - y2) + c * (z1 - z2))

> newPrinter = do
>   spec <- newRendezvous
>   let printer = ixdo
>         clet c = accept spec
>         offer c
>           (recv c >>>= io . putStrLn >>>= \_ -> close c >>>= \_ -> printer)
>           (close c)
>   let say s    = runSession $
>                    clet c = request spec in
>                      ixdo sel1 c; send c s; close c
>   let shutdown = runSession $
>                    clet c = request spec in
>                      sel2 c >>>= \_ -> close c
>   runSession (forkSession printer)
>   return (say, shutdown)
>
> logger say c = enter c >>>= \_ -> loop where
>   loop =
>     offer c
>       (ixdo
>          msg <- recv c
>          io (say ("logger: " ++ msg))
>          zero c
>          loop)
>       (ixdo
>          io (say "logger: exiting")
>          send c ()
>          close c)
>
> echoServer espec lspec = ixdo
>   clet lc = request lspec
>   enter lc
>   let loop = ixdo
>         clet ec = accept espec
>         offer ec
>           (ixdo
>            swap
>            sel1 lc; send lc "echo server forking"
>            zero lc
>            dig (forkSession (recv ec >>>= send ec >>>= \_ -> close ec))
>            loop)
>           (ixdo
>            dig $ ixdo
>              sel1 lc; send lc "echo server exiting"
>              zero lc
>              sel2 lc; recv lc
>              close lc
>            send ec ()
>            close ec)
>   loop
>
> client delay say espec = ixdo
>   clet c = request espec
>   s <- io getLine
>   case s of
>     "q" -> ixdo
>       sel2 c
>       recv c
>       close c
>     _   -> ixdo
>       forkSession $ ixdo
>         sel1 c
>         io (threadDelay $ round $ 1000000 * delay)
>         send c s
>         str <- recv c
>         io (say str)
>         close c
>       client delay say espec
>
> go delay = do
>   espec <- newRendezvous
>   lspec <- newRendezvous
>   (say, shutdown) <- newPrinter
>   runSession $
>     forkSession (accept lspec $ logger say) >>>= \_ ->
>     forkSession (echoServer espec lspec) >>>= \_ ->
>     client delay say espec
>   shutdown

}
