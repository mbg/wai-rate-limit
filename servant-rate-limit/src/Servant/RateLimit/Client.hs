--------------------------------------------------------------------------------
-- Rate Limiting Middleware for Servant                                       --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.RateLimit.Client where

--------------------------------------------------------------------------------

import Servant
import Servant.Client
import Servant.RateLimit.Types

--------------------------------------------------------------------------------

instance HasClient m api => HasClient m (RateLimit st p :> api) where
    type Client m (RateLimit st p :> api) = Client m api

    hoistClientMonad mp _ = hoistClientMonad mp (Proxy :: Proxy api)

    clientWithRoute mp _ = clientWithRoute mp (Proxy :: Proxy api)

--------------------------------------------------------------------------------
