{-
    TestNetwork.hs
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

import Network
import Control.Concurrent.Session
import System.Environment

main = do { (connectStr:other) <- getArgs
          ; let connectBool = read connectStr
          ; case (connectBool, other) of
              (True, (address:port:count:_)) -> connectToHost (read count) (PortNumber $ fromIntegral ((read port)::Int)) address
              (False, (port:_)) -> acceptFromHost (PortNumber $ fromIntegral ((read port)::Int))
          }

connectToHost :: Int -> PortID -> String -> IO ()
connectToHost count port host = runOverNetwork st l (f count) (Just host) port
    where
      (st, l) = makeSessionType $
                newLabel ~>>= \l ->
                l .= select ( (send (undefined :: Int) ~>> jump l)
                              ~|~
                              (send (undefined :: Bool) ~>> end)
                              ~|~ BLNil) ~>>
                sreturn l
      f 0 = sselect (D1 E) ~>>
            ssend True
      f n = sselect (D0 E) ~>>
            ssend n ~>>
            sjump ~>>
            f (n - 1)

acceptFromHost :: PortID -> IO ()
acceptFromHost port = runOverNetwork st l f Nothing port
    where
      (st, l) = makeSessionType $
                newLabel ~>>= \l ->
                l .= offer ( (recv (undefined :: Int) ~>> jump l)
                             ~|~
                             (recv (undefined :: Bool) ~>> end)
                             ~|~ BLNil) ~>>
                sreturn l
      f = soffer ( (srecv ~>> sjump ~>> f)
                   ~||~
                   (srecv ~>>= sliftIO . print)
                   ~||~ OfferImplsNil)
