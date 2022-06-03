--------------------------------------------------------------------------------
-- Rate Limiting Middleware for Servant                                       --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.RateLimit.Server (
    HasRateLimitStrategy(..),
    HasRateLimitPolicy(..)
) where

--------------------------------------------------------------------------------

import GHC.TypeLits

import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString.Char8 as C8
import Data.Kind
import Data.Proxy
import qualified Data.Time.Units as Units

import Network.Wai
import Network.Wai.RateLimit.Backend
import Network.Wai.RateLimit.Strategy

import Servant
import Servant.RateLimit.Types
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO

--------------------------------------------------------------------------------

-- | A class of types which are type-level descriptions of rate-limiting
-- strategies.
class HasRateLimitStrategy (ctx :: [Type]) strategy where
    -- | `strategyValue` @context backend getKey@ is a function which, given a
    -- @backend@ and a function @getKey@ used to compute the key using which
    -- the client should be identified, returns a rate-limiting `Strategy`.
    strategyValue ::
        Context ctx -> Backend key -> (Request -> IO key) -> Strategy

instance
    (KnownDuration dur, KnownNat capacity, Units.TimeUnit (DurationUnit dur))
    => HasRateLimitStrategy ctx (FixedWindow dur capacity)
    where

    strategyValue _ backend getKey = fixedWindow
        backend
        (Units.convertUnit $ durationVal @dur)
        (fromInteger $ natVal (Proxy :: Proxy capacity))
        getKey

instance
    (KnownDuration dur, KnownNat capacity, Units.TimeUnit (DurationUnit dur))
    => HasRateLimitStrategy ctx (SlidingWindow dur capacity)
    where

    strategyValue _ backend getKey = slidingWindow
        backend
        (Units.convertUnit $ durationVal @dur)
        (fromInteger $ natVal (Proxy :: Proxy capacity))
        getKey

-- | A class of types which are type-level descriptions of rate-limiting
-- policies.
class HasRateLimitPolicy (ctx :: [Type]) policy where
    type RateLimitPolicyKey ctx policy :: Type

    -- | `policyGetIdentifier` @context request@ computes the key that should
    -- be used by the backend to identify the client to which the rate
    -- limiting policy should be applied to. This could be as simple
    -- as retrieving the IP address of the client from @request@
    -- (as is the case with `IPAddressPolicy`) or retrieving data from
    -- the @request@ vault. The computation runs in `IO` to allow policies
    -- to perform arbitrary effects.
    policyGetIdentifier :: Context ctx -> Request -> IO (RateLimitPolicyKey ctx policy)

instance KnownSymbol prefix => HasRateLimitPolicy ctx (IPAddressPolicy prefix) where
    type RateLimitPolicyKey ctx (IPAddressPolicy prefix) = ByteString

    policyGetIdentifier _ =
        pure . (C8.pack (symbolVal (Proxy :: Proxy prefix)) <>) .
        C8.pack . show . remoteHost

instance
    ( HasServer api ctx
    , HasContextEntry ctx (Backend key)
    , HasRateLimitStrategy ctx strategy
    , HasRateLimitPolicy ctx policy
    , key ~ RateLimitPolicyKey ctx policy
    ) => HasServer (RateLimit strategy policy :> api) ctx
    where

    type ServerT (RateLimit strategy policy :> api) m = ServerT api m

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy :: Proxy api) pc nt s

    route _ context subserver = do
        -- retrieve the backend from the Servant context
        let backend = getContextEntry context

        -- retrieve the rate-limiting policy used to identify clients
        let policy = policyGetIdentifier @ctx @policy context

        -- retrieve the rate-limiting strategy used to limit access
        let strategy = strategyValue @ctx @strategy @key context backend policy

        let rateCheck = withRequest $ \req -> do
                -- apply the rate-limiting strategy to the request
                allowRequest <- liftIO $ strategyOnRequest strategy req

                -- fail if the rate limit has been exceeded
                unless allowRequest $ delayedFailFatal $ ServerError{
                    errHTTPCode = 429,
                    errReasonPhrase = "Rate limit exceeded",
                    errBody = "",
                    errHeaders = []
                }

        -- add the check for whether the rate limit has been exceeded to the
        -- server and return it
        route (Proxy :: Proxy api) context $
            subserver `addAcceptCheck` rateCheck

--------------------------------------------------------------------------------
