--------------------------------------------------------------------------------
-- Rate Limiting Middleware for WAI                                           --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Network.Wai.RateLimit.Backend (
    BackendError(..),
    Backend(..)
) where

--------------------------------------------------------------------------------

import Control.Exception

--------------------------------------------------------------------------------

-- | Represents a base type for exceptions that occur in `Backend`s.
data BackendError = forall e . Exception e => BackendError e

instance Show BackendError where
    showsPrec p (BackendError e) = showsPrec p e

instance Exception BackendError

-- | Represents storage backends for the rate limiting middleware.
data Backend key = MkBackend {
    -- | `backendGetUsage` @key@ is a computation which gets the usage
    -- associated with @key@. This computation may raise a `BackendError`
    -- exception if an error occurs while trying to retrieve the @key@'s
    -- usage from the backend.
    backendGetUsage :: key -> IO Integer,
    -- | `backendIncAndGetUsage` @key usage@ is a computation which increments
    -- the usage associated with @key@ by @usage@ and returns the result.
    -- This computation may raise a `BaackendError` exception if an error
    -- occurs while trying to increment and retrieve the usage associated
    -- with @key@.
    backendIncAndGetUsage :: key -> Integer -> IO Integer,
    -- | `backendExpireIn` @key seconds@ is a computation which makes @key@
    -- expire in @seconds@ from now. This computation may raise a
    -- `BackendError` exception if an error occurs while trying to set @key@
    -- to expire.
    backendExpireIn :: key -> Integer -> IO ()
}

--------------------------------------------------------------------------------
