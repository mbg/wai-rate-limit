--------------------------------------------------------------------------------
-- Rate Limiting Middleware for Servant                                       --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Servant.RateLimit.Types (
    -- * Servant combinator
    RateLimit,

    -- * Rate-limiting strategies
    FixedWindow,
    SlidingWindow,

    -- * Rate-limiting policies
    IPAddressPolicy,

    -- * Re-exports
    module Data.Time.TypeLevel
) where

--------------------------------------------------------------------------------

import GHC.TypeLits

import Data.Time.TypeLevel

--------------------------------------------------------------------------------

-- | A type-level description for the parameters of the `fixedWindow` strategy.
data FixedWindow (dur :: TimePeriod) (capacity :: Nat)

-- | A type-level description for the parameters of the `slidingWindow`
-- strategy.
data SlidingWindow (dur :: TimePeriod) (capacity :: Nat)

--------------------------------------------------------------------------------

-- | A simple rate-limiting policy which applies a rate-limiting strategy
-- based on the client's IP address. This policy is useful mainly for testing
-- purposes. For production use, you should implement your own policy based
-- on e.g. the current user, API key, etc. The @prefix@ parameter may be set
-- to the empty string if all API endpoints count towards the same rate limit,
-- but can be set to other values to have different rate limits for different
-- sets of endpoints.
data IPAddressPolicy (prefix :: Symbol)

--------------------------------------------------------------------------------

-- | A generalised rate limiting combinator which combines type-level
-- descriptions of a rate-limiting strategy, such as `FixedWindow`, with a
-- type-level description of a rate-limiting policy, such as `IPAddressPolicy`.
data RateLimit strategy policy

--------------------------------------------------------------------------------
