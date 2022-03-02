--------------------------------------------------------------------------------
-- Rate Limiting Middleware for Servant                                       --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | This library implements Servant combinators for applying rate limiting
-- strategies from "Network.Wai.RateLimit.Strategy". Different strategies may
-- be applied to different parts of the API. For example:
-- @
-- import Servant.RateLimit
--
-- type TestAPI
--     = RateLimit (FixedWindow 2 50) (IPAddressPolicy "fixed:") :>
--       "fixed-window" :>
--       Get '[JSON] String
--  :<|> RateLimit (SlidingWindow 2 50) (IPAddressPolicy "sliding:") :>
--       "sliding-window" :>
--       Get '[JSON] String
--  :<|> "unrestricted" :>
--       Get '[JSON] String
-- @
module Servant.RateLimit (
    module RateLimit
) where

--------------------------------------------------------------------------------

import Servant.RateLimit.Types as RateLimit

--------------------------------------------------------------------------------
