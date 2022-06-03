--------------------------------------------------------------------------------
-- Rate Limiting Middleware for Servant                                       --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.RateLimit.Server where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.IO.Class

import Network.Wai.RateLimit.Backend
import Network.Wai.RateLimit.Strategy

import Servant
import Servant.RateLimit.Types
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO

--------------------------------------------------------------------------------

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
