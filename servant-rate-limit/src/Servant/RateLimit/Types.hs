--------------------------------------------------------------------------------
-- Rate Limiting Middleware for Servant                                       --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.RateLimit.Types (
    -- * Servant combinator
    RateLimit,

    -- * Rate-limiting strategies
    FixedWindow,
    SlidingWindow,
    HasRateLimitStrategy(..),

    -- * Rate-limiting policies
    IPAddressPolicy,
    HasRateLimitPolicy(..),

    -- * Re-exports
    module Data.Time.TypeLevel
) where

--------------------------------------------------------------------------------

import GHC.TypeLits

import Data.ByteString.Char8 as C8
import Data.Kind
import Data.Proxy
import qualified Data.Time.Units as Units
import Data.Time.TypeLevel

import Network.Wai
import Network.Wai.RateLimit.Backend
import Network.Wai.RateLimit.Strategy

--------------------------------------------------------------------------------

-- | A type-level description for the parameters of the `fixedWindow` strategy.
data FixedWindow (dur :: TimePeriod) (capacity :: Nat)

-- | A type-level description for the parameters of the `slidingWindow`
-- strategy.
data SlidingWindow (dur :: TimePeriod) (capacity :: Nat)

-- | A class of types which are type-level descriptions of rate-limiting
-- strategies.
class HasRateLimitStrategy strategy where
    -- | `strategyValue` @backend getKey@ is a function which, given a
    -- @backend@ and a function @getKey@ used to compute the key using which
    -- the client should be identified, returns a rate-limiting `Strategy`.
    strategyValue :: Backend key -> (Request -> IO key) -> Strategy

instance
    (KnownDuration dur, KnownNat capacity, Units.TimeUnit (DurationUnit dur))
    => HasRateLimitStrategy (FixedWindow dur capacity)
    where

    strategyValue backend getKey = fixedWindow
        backend
        (Units.convertUnit $ durationVal @dur)
        (fromInteger $ natVal (Proxy :: Proxy capacity))
        getKey

instance
    (KnownDuration dur, KnownNat capacity, Units.TimeUnit (DurationUnit dur))
    => HasRateLimitStrategy (SlidingWindow dur capacity)
    where

    strategyValue backend getKey = slidingWindow
        backend
        (Units.convertUnit $ durationVal @dur)
        (fromInteger $ natVal (Proxy :: Proxy capacity))
        getKey

--------------------------------------------------------------------------------

-- | A simple rate-limiting policy which applies a rate-limiting strategy
-- based on the client's IP address. This policy is useful mainly for testing
-- purposes. For production use, you should implement your own policy based
-- on e.g. the current user, API key, etc. The @prefix@ parameter may be set
-- to the empty string if all API endpoints count towards the same rate limit,
-- but can be set to other values to have different rate limits for different
-- sets of endpoints.
data IPAddressPolicy (prefix :: Symbol)

-- | A class of types which are type-level descriptions of rate-limiting
-- policies.
class HasRateLimitPolicy policy where
    type RateLimitPolicyKey policy :: Type

    -- | `policyGetIdentifier` @request@ computes the key that should be
    -- used by the backend to identify the client to which the rate
    -- limiting policy should be applied to. This could be as simple
    -- as retrieving the IP address of the client from @request@
    -- (as is the case with `IPAddressPolicy`) or retrieving data from
    -- the @request@ vault. The computation runs in `IO` to allow policies
    -- to perform arbitrary effects.
    policyGetIdentifier :: Request -> IO (RateLimitPolicyKey policy)

instance KnownSymbol prefix => HasRateLimitPolicy (IPAddressPolicy prefix) where
    type RateLimitPolicyKey (IPAddressPolicy prefix) = ByteString

    policyGetIdentifier =
        pure . (C8.pack (symbolVal (Proxy :: Proxy prefix)) <>) .
        C8.pack . show . remoteHost

--------------------------------------------------------------------------------

-- | A generalised rate limiting combinator which combines type-level
-- descriptions of a rate-limiting strategy, such as `FixedWindow`, with a
-- type-level description of a rate-limiting policy, such as `IPAddressPolicy`.
data RateLimit strategy policy

--------------------------------------------------------------------------------
