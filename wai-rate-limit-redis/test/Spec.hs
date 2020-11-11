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

import Database.Redis

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Network.Wai.Handler.Warp

import Network.Wai.RateLimit
import Network.Wai.RateLimit.Strategy
import Network.Wai.RateLimit.Redis

--------------------------------------------------------------------------------

tests :: Session ()
tests = do 
    let req = SRequest defaultRequest ""

    forM_ [0..49] $ const $ do
        res <- srequest req
        assertStatus 200 res

    res <- srequest req 
    assertStatus 429 res 

    pure ()

-- | 'main' is the main entry point for the test suite.
main :: IO ()
main = do 
    conn <- checkedConnect defaultConnectInfo

    let backend = redisBackend conn 
    let strategy = slidingWindow backend 29 50 (\req -> pure "key")

    runSession tests $ rateLimiting strategy $ \req respond -> do 
        respond $ responseLBS status200 [] "OK"

--------------------------------------------------------------------------------
