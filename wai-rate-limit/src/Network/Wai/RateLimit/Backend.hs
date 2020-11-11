--------------------------------------------------------------------------------
-- Rate Limiting Middleware for WAI                                           --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Network.Wai.RateLimit.Backend (
    Backend(..)
) where

--------------------------------------------------------------------------------

-- | Represents storage backends for the rate limiting middleware.
data Backend key err = MkBackend {
    -- | 'backendGetUsage' @key@ is a computation which gets the usage 
    -- associated with @key@.
    backendGetUsage :: key -> IO (Either err Integer),
    -- | 'backendIncAndGetUsage' @key usage@ is a computation which increments  
    -- the usage associated with @key@ by @usage@ and returns the result.
    backendIncAndGetUsage :: key -> Integer -> IO (Either err Integer),
    -- | 'backendExpireIn' @key seconds@ is a computation which makes @key@
    -- expire in @seconds@ from now.
    backendExpireIn :: key -> Integer -> IO (Either err ())
}

--------------------------------------------------------------------------------
