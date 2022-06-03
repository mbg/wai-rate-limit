--------------------------------------------------------------------------------
-- Rate Limiting Middleware for Servant                                       --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines an instance of `HasOpenApi` for the `RateLimit` combinator.
module Servant.RateLimit.OpenApi () where

--------------------------------------------------------------------------------

import Data.Function
import Data.OpenApi
import Data.Proxy

import Servant.API
import Servant.OpenApi
import Servant.RateLimit

--------------------------------------------------------------------------------

add429response :: OpenApi -> OpenApi
add429response =
    setResponseWith (const mkResponse) 429 (pure $ mkResponse mempty)
    where
        mkResponse res = res{
            _responseDescription = "Rate limited"
        }

instance HasOpenApi api => HasOpenApi (RateLimit strategy policy :> api) where
    toOpenApi _ = toOpenApi (Proxy :: Proxy api)
        & add429response

--------------------------------------------------------------------------------
