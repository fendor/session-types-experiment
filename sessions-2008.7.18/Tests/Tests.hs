{-# LANGUAGE NoMonomorphismRestriction #-}

{-
    Tests.hs
        Copyright 2008 Matthew Sackman <matthew@wellquite.org>

    This file is part of Session Types for Haskell.

    Session Types for Haskell is free software: you can redistribute it
    and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation, either version 3 of
    the License, or (at your option) any later version.

    Session Types for Haskell is distributed in the hope that it will
    be useful, but WITHOUT ANY WARRANTY; without even the implied
    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Session Types for Haskell.
    If not, see <http://www.gnu.org/licenses/>.
-}

module Tests.Tests where

import Control.Concurrent.Session
import Control.Concurrent

import System.IO

testCombinedMultiReceive aDelay bDelay cDelay = runInterleaved nil st master
    where
      (st, (a, b))
          = makeSessionType $
            newLabel ~>>= \a ->
            newLabel ~>>= \b ->
            a .= send int ~>> jump a ~>>
            b .= send double ~>> jump b ~>>
            sreturn (a,b)
                where
                  int = undefined :: Int
                  double = undefined :: Double

      rec ch = withChannel ch (srecv ~>>= (sliftIO . print) ~>> sjump)

      master = fork a dual nil (childA aDelay) ~>>= \(chA, _) ->
               fork a dual nil (childA bDelay) ~>>= \(chB, _) ->
               fork b dual nil (childB cDelay) ~>>= \(chC, _) ->
               let plainChs = cons chA $ cons chB nil
                   plainFuns = [rec chA, rec chB]
                   richLst = (chC, rec chC) ~|||~ MultiReceiveNil
               in
                 combinedMultiRecv richLst plainChs plainFuns

      childA delay pch _
          = sliftIO (threadDelay delay) ~>>
            withChannel pch (ssend delay ~>> sjump)

      childB delay pch _
          = sliftIO (threadDelay delay) ~>>
            withChannel pch (ssend (sqrt . fromIntegral $ delay) ~>> sjump)

testPlainMultiReceive aDelay bDelay = runInterleaved nil st master
    where
      (st, a)
          = makeSessionType $
            newLabel ~>>= \a ->
            a .= send int ~>> jump a ~>>
            sreturn a
                where
                  int = undefined :: Int

      rec ch = withChannel ch (srecv ~>>= (sliftIO . print) ~>> sjump)

      master = fork a dual nil (child aDelay) ~>>= \(ch, _) ->
               let chs = cons ch nil
                   funs = [rec ch]
               in
                 fork a dual nil (child bDelay) ~>>= \(ch', _) ->
                 let chs' = cons ch' chs
                     funs' = (rec ch') : funs
                 in plainMultiReceive chs' funs'

      child delay pch _
          = sliftIO (threadDelay delay) ~>>
            withChannel pch (ssend delay ~>> sjump)

testSubtypingPids = runInterleaved nil st master
    where
      (st, (a, b, c, d))
          = makeSessionType $
            newLabel ~>>= \a ->
            newLabel ~>>= \b ->
            newLabel ~>>= \c ->
            newLabel ~>>= \d ->
            a .= sendPid (cons (c, notDual) nil) ~>> end ~>>
            b .= sendPid (cons (c, dual) nil) ~>> end ~>>
            c .= send int ~>> end ~>>
            d .= send char ~>> end ~>>
            sreturn (a, b, c, d)
          where
            int = undefined :: Int
            char = undefined :: Char

      childSessions = cons (c, dual) $ cons (c, notDual) $ cons (d, notDual) $ cons (d, dual) nil

      master -- childA and childB are declared with much greater session types than necessary...
         = fork a notDual childSessions childA ~>>= \(childChA, childPidA) ->
           fork b notDual childSessions childB ~>>= \(childChB, childPidB) ->
           withChannel childChA (ssend childPidB) ~>>
           withChannel childChB (ssend childPidA) ~>>
           sreturn ()

      childA parentCh parentPid
          = withChannel parentCh srecv ~>>= \pid ->
            createSession c dual pid ~>>= \ch -> -- ... but the session type restricts them and they're thus limited here ...
            withChannel ch srecv ~>>= sliftIO . print
--          ~>> createSession d dual pid ~>>= \ch' -> sreturn () -- (won't work as d isn't in the sendPid session type)

      childB parentCh parentPid
          = withChannel parentCh srecv ~>>= \pid ->
            createSession c notDual pid ~>>= \ch -> -- ... and here.
            withChannel ch (ssend 42)
--          ~>> createSession d notDual pid ~>>= \ch' -> sreturn () -- (won't work as d isn't in the sendPid session type)

testHigherOrderSessionTypes = runInterleaved nil st master
    where
      (st, (a, b))
          = makeSessionType $
            newLabel ~>>= \a ->
            newLabel ~>>= \b ->
            a .= sendSession (recv (undefined :: Int) ~>> end) ~>> end ~>>
            b .= send (undefined :: Int) ~>> recv (undefined :: Int) ~>> end ~>>
            sreturn (a, b)

      master = fork b notDual nil childA ~>>= master'
      -- master' used to avoid monomorphism restriction on lambdas
      master' (chA, _) = fork a notDual nil childB ~>>= \(chB, _) ->
                         withChannel chA (ssend 41) ~>>
                         sendChannel chA chB

      childA chP _ = withChannel chP (srecv ~>>= ssend . (+) 1)

      childB chP _ = recvChannel chP ~>>= \chA ->
                     withChannel chA (srecv ~>>= sliftIO . print)

testRealTimeout delayA delayB = runInterleaved nil st master
    where
      (st, a)
          = makeSessionType $
            newLabel ~>>= \a ->
            a .= send int ~>> end ~>>
            sreturn a
          where
            int = undefined :: Int

      master = fork a dual nil child ~>>= \(ch, _) ->
               withChannel ch (srecvTestTimeOut delayA) ~>>= \result ->
               (sliftIO . putStrLn $ if result
                                       then ("Channel readable within " ++ (show delayA) ++ " microseconds.")
                                       else ("Channel not readable within " ++ (show delayA) ++ " microseconds.")) ~>>
               withChannel ch srecv ~>>=
               sliftIO . print

      child ch _
          = (sliftIO . threadDelay $ delayB) ~>>
            withChannel ch (ssend delayB)

testMultiRecvAsTimeout delay pulse = runInterleaved nil st master
    where
      (st, (a, b))
          = makeSessionType $
            newLabel ~>>= \a ->
            newLabel ~>>= \b ->
            a .= send () ~>> jump a ~>>
            b .= send (undefined :: Int) ~>> end ~>>
            sreturn (a, b)

      master
         = fork a dual nil (clock pulse) ~>>= \(clkCh, _) ->
           fork b dual nil (child delay) ~>>= \(childCh, _) ->
           multiReceive ( (clkCh, withChannel clkCh (srecv ~>> sjump) ~>> (sliftIO . print $ "TIMEOUT") ~>> withChannel childCh srecv)
                          ~|||~
                          (childCh, (sliftIO . print $ "Child") ~>> withChannel childCh srecv)
                          ~|||~
                          MultiReceiveNil
                        )

      clock pulse ch pid = sliftIO (threadDelay pulse) ~>>
                           withChannel ch (ssend () ~>> sjump) ~>>
                           clock pulse ch pid

      child delay ch pid = sliftIO (threadDelay delay) ~>>
                           withChannel ch (ssend delay)

-- note with this one that child' uses select so this shows how the multireceive
-- construct allows you to mix together channels next doing a Recv or an Offer.
testMultiRecv aDelay bDelay cDelay = runInterleaved nil st master
    where
      (st, (x, y))
          = makeSessionType $
            newLabel ~>>= \x ->
            x .= send (undefined :: Int) ~>> end ~>>
            newLabel ~>>= \y ->
            y .= select ( (send (undefined :: Int) ~>> end)
                          ~|~ BLNil
                        ) ~>>
            sreturn (x, y)

      master
         = fork x dual nil (child aDelay) ~>>= \(aCh, _) ->
           fork x dual nil (child bDelay) ~>>= \(bCh, _) ->
           fork y dual nil (child' cDelay) ~>>= \(cCh, _) ->
           receiveABC ("A", aCh) ("B", bCh) ("C", cCh)

      receive str ch
          = withChannel ch (receiveInner str)
      receiveInner str
          = srecv ~>>=
            sliftIO . print .
            (++) ("Master received from child " ++ str ++ ": ") . show

      receiveAB (a, aCh) (b, bCh)
          = multiReceive ( (aCh, receive a aCh ~>> receive b bCh) ~|||~
                           (bCh, receive b bCh ~>> receive a aCh) ~|||~ MultiReceiveNil
                         )

      receiveXC (a, aCh) (c, cCh)
          = multiReceive ( (aCh, receive a aCh ~>> withChannel cCh ( soffer ((receiveInner c) ~||~ OfferImplsNil) )) ~|||~
                           (cCh, withChannel cCh (soffer ((receiveInner c) ~||~ OfferImplsNil ))  ~>> receive a aCh) ~|||~ MultiReceiveNil
                         )

      receiveABC (a, aCh) (b, bCh) (c, cCh)
          = multiReceive ( (aCh, receive a aCh ~>> receiveXC (b, bCh) (c, cCh)) ~|||~
                           (bCh, receive b bCh ~>> receiveXC (a, aCh) (c, cCh)) ~|||~
                           (cCh, (withChannel cCh (soffer ((receiveInner c) ~||~ OfferImplsNil )) ~>> receiveAB (a, aCh) (b, bCh)))
                           ~|||~ MultiReceiveNil
                         )

      child delay parentCh _
         = (sliftIO . threadDelay $ delay) ~>>
           withChannel parentCh (ssend delay)

      child' delay parentCh ppid
         = (sliftIO . threadDelay $ delay) ~>>
           withChannel parentCh (sselect (D0 E) ~>> ssend delay)

testCreateAndInterleaving = runInterleaved (cons (b, dual) nil) st master
    where
       (st, (a, b))
           = makeSessionType $
             newLabel ~>>= \a ->
             newLabel ~>>= \b ->
             a .= sendPid (cons (b, notDual) nil) ~>> end ~>>
             b .= send (undefined :: Int) ~>> end ~>>
             sreturn (a, b)

       master
          = fork a dual (cons (b, notDual) nil) child ~>>= \(childCh, childPid) ->
	    withChannel childCh srecv ~>>= \childPid' ->
            createSession b dual childPid' ~>>= \chA ->
            createSession b dual childPid ~>>= \chB ->
            withChannel chA (srecv ~>>= sliftIO . putStrLn . (++) "Received on chA: " . show) ~>>
            withChannel chB (srecv ~>>= sliftIO . putStrLn . (++) "Received on chB: " . show) ~>>
            sreturn ()

       child parentCh parentPid
          = myPid ~>>= \me ->
            withChannel parentCh (ssend me) ~>>
            createSession b notDual parentPid ~>>= \chA ->
            createSession b notDual parentPid ~>>= \chB ->
            withChannel chA (ssend 10) ~>>
            withChannel chB (ssend 20)

testFork = runInterleaved nil st master
    where
      (st, (a, b))
          = makeSessionType $
            newLabel ~>>= \a ->
            newLabel ~>>= \b ->
            a .= send int ~>> send int ~>> recv bool ~>> end ~>>
            b .= send bool ~>> recv bool ~>> end ~>>
            sreturn (a, b)
          where
            int = undefined :: Int
            bool = undefined :: Bool

      childA parentCh parentPid
          = (sliftIO . putStrLn $ "ChildA is alive!") ~>>
            withChannel parentCh srecv ~>>= \x ->
            withChannel parentCh srecv ~>>= \y ->
            withChannel parentCh (ssend (x == y))

      childB parentCh parentPid
          = (sliftIO . putStrLn $ "ChildB is alive!") ~>>
            withChannel parentCh (ssend True ~>>
                                  srecv ~>>=
                                  sliftIO . putStrLn . (++) "ChildB received a Bool: " . show
                                 )

      master
          = fork a notDual nil childA ~>>= \(aCh, aPid) ->
            fork b dual nil childB ~>>= \(bCh, bPid) ->
            withChannel aCh (ssend 5) ~>>
            withChannel bCh srecv ~>>= \boolB ->
            (if boolB
             then withChannel aCh (ssend 6 ~>> srecv)
	     else withChannel aCh (ssend 5 ~>> srecv)
            ) ~>>= \boolA ->
            withChannel bCh (ssend (boolA && boolB)) ~>>
            (sliftIO . putStrLn $ "Master has finished. Bye bye.")

testRealChannels num
    = runInterleaved nil spec master
    where
      (spec, ( aSessions
             , bSessions
             , a
             , b
             , c
             ))
          = makeSessionType $
            newLabel ~>>= \a ->
            newLabel ~>>= \b ->
            newLabel ~>>= \c ->
            a .= sendPid (cons (c, notDual) nil) ~>> send bool ~>> end ~>>
            b .= sendPid (cons (c, dual) nil) ~>> send bool ~>> end ~>>
            c .= send string ~>> recv int ~>> select ( (send int ~>> end)
                                                       ~|~
                                                       (sendPid (cons (c, notDual) nil) ~>> end)
                                                       ~|~
                                                       BLNil
                                                     ) ~>>
            sreturn ( (cons (c, dual) nil)
                    , (cons (c, notDual) nil)
                    , a
                    , b
                    , c
                    )
          where
            int = undefined :: Int
            bool = undefined :: Bool
            string = undefined :: String

      master
          = fork a notDual aSessions childA ~>>= \(aCh, aPid) ->
            fork b notDual bSessions childB ~>>= \(bCh, bPid) ->
            withChannel aCh (ssend bPid) ~>>
            withChannel bCh (ssend aPid) ~>>
            sliftIO (threadDelay num) ~>>
            withChannel aCh (ssend True) ~>>
            sreturn (aPid, bPid)

      childA mCh mPid = sliftIO (putStrLn "Child A alive") ~>>
                        withChannel mCh srecv ~>>= \bPid ->
                        createSession c dual bPid ~>>= \bCh ->
                        myPid ~>>= \me ->
                        multiReceive ( (bCh, bThenM mCh bCh me)
                                       ~|||~
                                       (mCh, mThenB mCh bCh me)
                                       ~|||~
                                       MultiReceiveNil
                                     )
                            where
                              mThenB mCh bCh me = (dealWithM mCh) ~>> (dealWithB bCh me)
                              bThenM mCh bCh me = (dealWithB bCh me) ~>> (dealWithM mCh)
                              dealWithM mCh
                                  = withChannel mCh srecv ~>>=
                                    sliftIO . putStrLn . (++) "Got a bool from master: " . show
                              dealWithB bCh me
                                  = withChannel bCh (srecv ~>> ssend num ~>>
                                                     soffer (
                                                             (srecv ~>>= sliftIO . print)
                                                             ~||~
                                                             (srecv ~>>= \them ->
                                                              sliftIO (if (them =~= me)
                                                                       then putStrLn $ "EQ: " ++ (show them) ++ " " ++ (show me)
                                                                       else putStrLn $ "NEQ: " ++ (show them) ++ " " ++ (show me)
                                                                      )
                                                             )
                                                             ~||~
                                                             OfferImplsNil
                                                            )
                                                    )

      childB mCh mPid = sliftIO (putStrLn "Child B alive") ~>>
                        withChannel mCh srecv ~>>= \aPid ->
                        createSession c notDual aPid ~>>= \aCh ->
                        myPid ~>>= \me ->
                        withChannel aCh (ssend "foo" ~>> srecv) ~>>= \num ->
                        withChannel aCh (if even num
                                         then sselect (D0 E) ~>> ssend (2*num)
                                         else sselect (D1 E) ~>> ssend me
                                        )
                   
testCalculator = run calculatorSpec a calculatorServer calculatorClient
    where
      (calculatorSpec, a)
          = makeSessionType $
            newLabel ~>>= \a ->
            a .= offer ( bin a ~|~
                         bin a ~|~
                         bin a ~|~
                         uni a ~|~
                         end ~|~
                         BLNil
                       ) ~>>
            sreturn a
                where
                  int = undefined :: Int
                  bin t = recv int ~>> recv int ~>> send int ~>> jump t
                  uni t = recv int ~>> send int ~>> jump t

      calculatorServer
          = soffer ( bin (+) ~||~
                     bin (-) ~||~
                     bin (*) ~||~
                     (srecv ~>>=
                      ssend . negate ~>>
                      sjump ~>>
                      calculatorServer) ~||~
                     sreturn () ~||~
                     OfferImplsNil
                   )
            where
              bin f = (srecv ~>>= \x ->
                       srecv ~>>= \y ->
                       ssend (f x y) ~>>
                       sjump ~>>
                       calculatorServer
                      )

      calculatorClient = sjoin (sliftIO doMenu)
          where
            doMenu
                = do { putStrLn "Menu:\n 1. Add\n 2. Subtract\n 3. Multiply\n 4. Negate\n 5. Quit\n Enter your choice:"
                     ; l <- hGetLine stdin
                     ; case read l of
                         1 -> return $ sselect (D0 E) ~>> two
                         2 -> return $ sselect (D1 E) ~>> two
                         3 -> return $ sselect (D2 E) ~>> two
                         4 -> return $ sselect (D3 E) ~>> one
                         5 -> return $ sselect (D4 E)
                         _ -> doMenu
                     }
            fetchInt :: IO Int
            fetchInt
                = do { putStrLn "Enter an Int"
                     ; l <- hGetLine stdin
                     ; return . read $ l
                     }
            two = sliftIO fetchInt ~>>=
                  ssend ~>> one
            one = sliftIO fetchInt ~>>=
                  ssend ~>>
                  srecv ~>>= sliftIO . print ~>>
                  sjump ~>> sjoin (sliftIO doMenu)
