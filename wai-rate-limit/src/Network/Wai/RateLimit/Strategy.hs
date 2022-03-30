--------------------------------------------------------------------------------
-- Rate Limiting Middleware for WAI                                           --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Exports functions which implement rate-limiting strategies.
module Network.Wai.RateLimit.Strategy (
    Strategy(..),
    fixedWindow,
    slidingWindow
) where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Time.Units

import Network.Wai
import Network.Wai.RateLimit.Backend

--------------------------------------------------------------------------------

-- | Represents rate limiting strategies.
newtype Strategy = MkStrategy {
    -- | 'strategyOnRequest' @request@ is a computation which determines
    -- whether the request should be allowed or not, based on the rate
    -- limiting strategy.
    strategyOnRequest :: Request -> IO Bool
}

-- | `windowStrategy` implements a general window-based rate limiting strategy.
windowStrategy
    :: Backend key -- ^ The storage backend to use.
    -> Second -- ^ The number of seconds after which recorded usage expires.
    -> Integer -- ^ How much capacity each key should have.
    -> (Request -> IO key) -- ^ A function which computes a key for the
                           -- request.
    -> (Integer -> Bool) -- ^ A predicate which determines whether the expiry
                         -- timer should be reset.
    -> Request -- ^ The request to apply the stragey to, used for deriving
               -- the key.
    -> IO Bool
windowStrategy MkBackend{..} seconds capacity getKey cond req = do
    -- get a key to identify the usage bucket for the request: this is
    -- up the application and may be comprised of e.g. the IP of the client
    -- or a unique user id, followed by e.g. a timestamp
    key <- getKey req

    -- get usage for the key and increment it by 1
    used <- backendIncAndGetUsage key 1

    -- we got back the current usage: check whether it is within the
    -- acceptable limit and, if so, add to the expiry timer
    if used <= capacity
    then do
        when (cond used) $ void $ backendExpireIn key (toInteger seconds)
        pure True
    else pure False

-- | 'fixedWindow' @seconds limit@ is a 'Strategy' which limits the number
-- of requests made by a client to @limit@ within a window of @seconds@.
fixedWindow
    :: Backend key
    -> Second
    -> Integer
    -> (Request -> IO key)
    -> Strategy
fixedWindow backend seconds capacity getKey = MkStrategy{
    strategyOnRequest =
        let cond = (==) 1
        in windowStrategy backend seconds capacity getKey cond
}

-- | 'slidingWindow' @seconds limit@ is a 'Strategy' which limits the number
-- of requests made by a client to @limit@ within a sliding window of
-- @seconds@. That is, for every successful request, the window is extended by
-- @seconds@ so that a "break" of @seconds@ is required after @limit@-many
-- requests have been made in a period during which the timeout has never
-- been exceeded.
slidingWindow
    :: Backend key
    -> Second
    -> Integer
    -> (Request -> IO key)
    -> Strategy
slidingWindow backend seconds capacity getKey = MkStrategy{
    strategyOnRequest =
        let cond = const True
        in windowStrategy backend seconds capacity getKey cond
}

--------------------------------------------------------------------------------
