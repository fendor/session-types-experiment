{-
    Queens.hs
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

{-# LANGUAGE NoMonomorphismRestriction #-}
module Tests.Queens where

import Control.Concurrent.Session

(st, ( childSessions
     , rootSessions
     , commPids
     , commCoords
     , commResult
     ))
    = makeSessionType $ newLabel ~>>= \a ->
                        newLabel ~>>= \b ->
                        newLabel ~>>= \c ->
                        let queenFragList = (cons (a, dual) $ cons (b, notDual) $ cons (b, dual) $ cons (c, dual) nil) in
                        a .= (select ( (sendAPid a queenFragList)
                                       ~|~
                                       (sendAPid a queenFragList)
                                       ~|~
                                       end
                                       ~|~
                                       BLNil
                                     )
                             ) ~>>
                        b .= (send (int, int) ~>> end) ~>>
                        c .= (recv bool ~>> end) ~>>
                        sreturn ( queenFragList
                                , (cons (a, notDual) $ cons (c, notDual) nil)
                                , a
                                , b
                                , c
                                )
    where
      sendAPid a lst = (sendPid lst ~>> jump a)
      int = undefined :: Int
      bool = undefined :: Bool

makeQueens [] pids
    = sreturn pids
makeQueens (coord:coords) pids
   = forkThenClose commCoords notDual childSessions queen (ssend coord) ~>>= \((), pid) ->
     makeQueens coords (pid:pids)
    where
      queen rootCoordCh rootPid
          = withThenClose rootCoordCh srecv ~>>= \(x, y) ->

            createSessionThenClose commPids dual rootPid (receivePids [] [])  ~>>= \(beforePids, afterPids) ->

            (smapM_
             (\p -> createSessionThenClose commCoords notDual p (ssend (x, y))) afterPids) ~>>

            (smapM
             (\p -> createSessionThenClose commCoords dual p srecv) beforePids) ~>>= \beforeCoords ->

            createSessionThenClose commResult dual rootPid (ssend . or . map (detectCollision (x, y)) $ beforeCoords)
            where
              receivePids beforePids afterPids
                  = soffer ( (srecv ~>>= \beforePid ->
                              sjump ~>> receivePids (beforePid:beforePids) afterPids
                             ) ~||~
                             (srecv ~>>= \afterPid ->
                              sjump ~>> receivePids beforePids (afterPid:afterPids)
                             ) ~||~
                             (sreturn (afterPids, reverse beforePids))
                             ~||~ OfferImplsNil
                           )

              detectCollision (x, y) (x', y') = x == x'
                                             || y == y'
                                             || (abs (x' - x)) == (abs (y' - y))

sendPids 0 [] = sselect (D2 E) ~>> sreturn ()
sendPids 0 (pid:pids) = sselect (D1 E) ~>>
                        ssend pid ~>>
                        sjump ~>>
                        sendPids 0 pids
sendPids n (pid:pids) = sselect (D0 E) ~>>
                        ssend pid ~>>
                        sjump ~>>
                        sendPids (n-1) pids

distributePids 0 _ = sreturn ()
distributePids n (pid:pids)
    = createSessionThenClose commPids notDual pid (sendPids (n - 1) pids) ~>>
      distributePids (n-1) (pids ++ [pid])

gatherResults [] = sreturn False
gatherResults (pid:pids)
    = createSessionThenClose commResult notDual pid srecv ~>>= \collision ->
      gatherResults pids ~>>= \collisions ->
      sreturn (collision || collisions)

hasCollision lst = runInterleaved rootSessions st
                   (makeQueens lst [] ~>>= \pids ->
                    distributePids (length lst) pids ~>>
                    gatherResults pids
                   )
--hasCollision [(1,1), (2,7), (3,4), (4,6), (5,8), (6,2), (7,5), (8,3)] ==> False

main = hasCollision [(1,1), (2,7), (3,4), (4,6), (5,8), (6,2), (7,5), (8,3)] >>= print
