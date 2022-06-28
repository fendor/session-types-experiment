{-# LANGUAGE PatternSignatures, BangPatterns #-}

{-
    Speed.hs
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

module Main where

import Control.Concurrent.Session
import System.Environment
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

(st, a) = makeSessionType $
          newLabel ~>>= \a ->
          a .= send int ~>> jump a ~>>
          sreturn a
    where
      int = undefined :: Int

senderST !target !count
    | target == count = sreturn ()
    | otherwise = (ssend $! count) ~>> sjump ~>> (senderST target $! (count + 1))

receiverST !target !count
    | target == count = sreturn ()
    | otherwise = srecv ~>> sjump ~>> (receiverST target $! (count + 1))

mainST :: IO ()
mainST = do { (_:targetStr:_) <- getArgs
            ; let (target :: Int) = read targetStr
            ; run st a (senderST target 0) (receiverST target 0)
            ; return ()
            }

senderChan chan !target !count
    | target == count = return ()
    | otherwise = do { writeChan chan $! count
                     ; senderChan chan target $! (count + 1)
                     }

receiverChan chan !target !count
    | target == count = return ()
    | otherwise = do { readChan chan
                     ; receiverChan chan target $! (count + 1)
                     }

mainChan :: IO ()
mainChan = do { (_:targetStr:_) <- getArgs
              ; let (target :: Int) = read targetStr
              ; chan <- newChan
              ; doneA <- newEmptyMVar
              ; doneB <- newEmptyMVar
              ; forkIO (senderChan chan target 0 >> putMVar doneA ())
              ; forkIO (receiverChan chan target 0 >> putMVar doneB ())
              ; takeMVar doneA
              ; takeMVar doneB
              ; return ()
              }

data STOrChan = Session | Chan
                deriving (Read, Show, Eq)

main :: IO ()
main = do { (stOrChan:_) <- getArgs
          ; case ((read stOrChan)::STOrChan) of
              Session -> mainST
              Chan -> mainChan
          }

{-

$ time ./Speed Chan 1000000 +RTS -N4 -RTS
real    0m5.573s

$ time ./Speed Chan 1000000 +RTS -N2 -RTS
real    0m6.190s

$ time ./Speed Chan 1000000 +RTS -N1 -RTS
real    0m0.564s


$ time ./Speed Session 1000000 +RTS -N4 -RTS
real    0m3.634s

$ time ./Speed Session 1000000 +RTS -N2 -RTS
real    0m2.540s

$ time ./Speed Session 1000000 +RTS -N1 -RTS
real    0m1.350s

-}