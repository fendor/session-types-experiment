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

-- | This is just a convenience module that reexports everything
-- you're expected to need.
module Control.Concurrent.Session

    ( True (..)
    , False (..)

    , OfferImpls (OfferImplsNil)
    , (~||~)
    , sjump
    , soffer
    , sselect
    , ssend
    , srecv
    , srecvTest
    , srecvTestTimeOut
    , run

    , End ()
    , Send (..)
    , Recv (..)
    , sendPid
    , recvPid
    , sendSession
    , recvSession
    , Jump ()
    , Select ()
    , Offer ()
    , jump
    , end
    , select
    , offer
    , (~>)
    , (~|~)
    , SWellFormedConfig
    , testWellformed
    , SessionChain

    , (.=)
    , dual
    , notDual
    , newLabel
    , send
    , recv
    , makeSessionType
    , currentLabel
    , BranchesList (BLNil)

    , Cons ()
    , cons
    , Nil ()
    , nil

    , E (..)
    , D0 (..)
    , D1 (..)
    , D2 (..)
    , D3 (..)
    , D4 (..)
    , D5 (..)
    , D6 (..)
    , D7 (..)
    , D8 (..)
    , D9 (..)

    , module Control.Concurrent.Session.Base.SMonad

    , emptyMap

    , Pid ()
    , InterleavedChain ()
    , CreateSession (..)
    , myPid
    , PidEq (..)
    , MultiReceiveList (MultiReceiveNil)
    , (~|||~)
    , MultiReceive (..)
    , PlainMultiReceive (..)
    , CombinedMultiRecv (..)

    , module Control.Concurrent.Session.Interleaving

    , module Control.Concurrent.Session.Network.Socket

    ) where

import Control.Concurrent.Session.Base.Number
import Control.Concurrent.Session.Base.Bool
import Control.Concurrent.Session.Base.List
import Control.Concurrent.Session.Base.Map
import Control.Concurrent.Session.Base.SMonad
import Control.Concurrent.Session.SessionType hiding (jump, end, select, offer, (~|~), sendPid, recvPid, sendSession, recvSession, Dual(..))
import Control.Concurrent.Session.SessionTypeMonad
import Control.Concurrent.Session.Types
import Control.Concurrent.Session.Runtime
import Control.Concurrent.Session.Pid
import Control.Concurrent.Session.Interleaving
import Control.Concurrent.Session.Network.Socket
