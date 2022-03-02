--------------------------------------------------------------------------------
-- Rate Limiting Middleware for WAI                                           --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Network.Wai.RateLimit.Redis (
    RedisBackendError(..),
    redisBackend
) where

--------------------------------------------------------------------------------

import Control.Exception

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Database.Redis as Redis

import Network.Wai.RateLimit.Backend

--------------------------------------------------------------------------------

-- | Represents reasons why requests made to the Redis backend have failed.
data RedisBackendError
    = RedisBackendReply Reply
    | RedisBackendTxAborted
    | RedisBackendTxError String
    deriving (Eq, Show)

instance Exception RedisBackendError

-- | 'redisBackend' @connection@ constructs a rate limiting 'Backend' for the
-- given redis @connection@.
redisBackend :: Connection -> Backend BS.ByteString
redisBackend conn = MkBackend{
    backendGetUsage = \key -> runRedis conn (get key) >>= \case
        Left err -> throw $ BackendError $ RedisBackendReply err
        Right mVal -> case mVal >>= \val -> C8.readInteger val of
            -- the key does not exist or is not a valid integer:
            -- no previous usage
            Nothing -> pure 0
            -- the key exists: check that nothing follows the integer and then
            -- return the current usage
            Just (n, xs) | BS.null xs -> pure n
                         | otherwise -> pure 0,
    backendIncAndGetUsage = \key val -> do
        -- increment the value of the key by the specified amount
        result <- runRedis conn $ incrby key val

        case result of
            Left err -> throw $ BackendError $ RedisBackendReply err
            Right n -> pure n,
    backendExpireIn = \key seconds ->
        -- update the key to expire in the specified number of seconds
        runRedis conn (expire key seconds) >>= \case
            Left err -> throw $ BackendError $ RedisBackendReply err
            Right _ -> pure ()
}

--------------------------------------------------------------------------------
