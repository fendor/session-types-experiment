{-# LANGUAGE NoMonomorphismRestriction #-}

{-
    Session.hs
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

module System.Session where

import Control.Concurrent.Session
import System.IO

lineSessionRaw
    = newLabel ~>>= \l ->
      l .= select ( (send string ~>> jump l) ~|~
                    end ~|~ BLNil
                  ) ~>>
      sreturn l
          where
            string = undefined::String

makeLineOutputChannel hdl lineSessionFragment = fork lineSessionFragment notDual nil child
    where
      child parentCh _ = withChannel parentCh f
          where
            f = soffer ( (srecv ~>>= sliftIO . hPutStrLn hdl ~>> f)
                         ~||~
                         (sreturn ())
                         ~||~ OfferImplsNil
                       )

makeLineInputChannel hdl lineSessionFragment = fork lineSessionFragment dual nil child
    where
      child parentCh _ = withChannel parentCh f
          where
            f = sliftIO (hIsClosed hdl) ~>>= \closed ->
                if closed
                  then sselect (D1 E)
                  else sselect (D0 E) ~>>
                       sliftIO (hGetLine hdl) ~>>= \line ->
                       ssend line ~>>
                       f

stdoutLineOutputChannel = makeLineOutputChannel stdout
stdinLineInputChannel = makeLineInputChannel stdin
