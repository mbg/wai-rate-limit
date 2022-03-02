--------------------------------------------------------------------------------
-- Rate Limiting Middleware for WAI                                           --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Main ( main ) where

--------------------------------------------------------------------------------

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as C8

import Database.Redis

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Network.Wai.Handler.Warp

import Network.Wai.RateLimit
import Network.Wai.RateLimit.Backend
import Network.Wai.RateLimit.Strategy
import Network.Wai.RateLimit.Redis

import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------

-- | 'rateLimitedSession' makes 50 requests to the server (which should be the
-- rate limit), all of which should result in a 200 status code. It then makes
-- another request which should result in a 429 status code.
rateLimitedSession :: Session ()
rateLimitedSession = do
    let req = SRequest defaultRequest ""

    forM_ [0..49] $ const $ do
        res <- srequest req
        assertStatus 200 res

    res <- srequest req
    assertStatus 429 res

    pure ()

-- | 'expirySession' makes 50 requests to the server, which should hit the
-- rate limit. It then sleeps for three seconds and makes another request,
-- which should succeed.
expirySession :: Session ()
expirySession = do
    let req = SRequest defaultRequest ""

    -- make 50 requests
    forM_ [0..49] $ const $ do
        res <- srequest req
        assertStatus 200 res

    -- sleep for 3 seconds
    liftIO $ threadDelay (3 * 1000 * 1000)

    -- make another request
    res <- srequest req
    assertStatus 200 res

    pure ()

windowTest :: Strategy -> Session () -> Assertion
windowTest strategy tests =
    runSession tests $ rateLimiting strategy $ \req respond -> do
        respond $ responseLBS status200 [] "OK"

getKey :: C8.ByteString -> Request -> IO C8.ByteString
getKey prefix = pure . (<>) prefix . C8.pack . show . remoteHost

-- | 'redisTests' @backend@ constructs the tests using @backend@.
redisTests :: Backend C8.ByteString -> TestTree
redisTests backend = testGroup "Redis tests"
    [ testCase "slidingWindow" $
        let strategy = slidingWindow backend 29 50 (getKey "sliding:")
        in windowTest strategy rateLimitedSession
    , testCase "slidingWindow (reset)" $
        let strategy = slidingWindow backend 2 50 (getKey "sliding-reset:")
        in windowTest strategy expirySession
    , testCase "fixedWindow" $
        let strategy = fixedWindow backend 29 50 (getKey "fixed:")
        in windowTest strategy rateLimitedSession
    , testCase "fixedWindow (reset)" $
        let strategy = fixedWindow backend 2 50 (getKey "fixed-reset:")
        in windowTest strategy expirySession
    ]

-- | 'main' is the main entry point for the test suite.
main :: IO ()
main = do
    -- connect to the Redis server and construct a backend for the connection
    backend <- redisBackend <$> checkedConnect defaultConnectInfo

    -- run the tests with the Redis backend
    defaultMain $ redisTests backend


--------------------------------------------------------------------------------
