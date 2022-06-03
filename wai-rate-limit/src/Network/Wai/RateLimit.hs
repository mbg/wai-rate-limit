--------------------------------------------------------------------------------
-- Rate Limiting Middleware for WAI                                           --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Implements WAI 'Middleware' for rate limiting.
module Network.Wai.RateLimit (
    rateLimiting
) where

--------------------------------------------------------------------------------

import Network.HTTP.Types
import Network.Wai
import Network.Wai.RateLimit.Strategy

--------------------------------------------------------------------------------

-- | 'rateLimiting' @strategy@ is a WAI 'Middleware' which limits requests
-- according to the configuration represented by @strategy@.
rateLimiting :: Strategy -> Middleware
rateLimiting MkStrategy{..} app req sendResponse = do
    -- check that the client has not exceeded
    allowRequest <- strategyOnRequest req

    if allowRequest
    -- if not: process the request normally
    then app req sendResponse
    -- otherwise: return a 429 response with an appropriate message
    else sendResponse $ responseLBS status429 [] "Rate limit exceeded"

--------------------------------------------------------------------------------
