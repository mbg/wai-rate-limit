--------------------------------------------------------------------------------
-- Rate Limiting Middleware for Servant                                       --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE UndecidableInstances #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString (ByteString)

import Database.Redis

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp as Warp
import Network.Wai.RateLimit.Redis

import Servant
import Servant.Client
import Servant.Server
import Servant.RateLimit
import Servant.RateLimit.Client
import Servant.RateLimit.Server

import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------

newtype ApiKey = MkApiKey { getApiKey :: ByteString }

data ApiKeyPolicy

instance HasContextEntry ctx ApiKey => HasRateLimitPolicy ctx ApiKeyPolicy where
    type RateLimitPolicyKey ctx ApiKeyPolicy = ByteString

    policyGetIdentifier ctx req = pure $ getApiKey $ getContextEntry ctx

--------------------------------------------------------------------------------

-- | The API we use for out tests, which has endpoints for the different
-- rate limiting strategies as well as an unrestricted endpoint.
type TestAPI
    = RateLimit (FixedWindow ('Second 2) 50) (IPAddressPolicy "fixed:") :>
      "fixed-window" :>
      Get '[JSON] String
 :<|> RateLimit (SlidingWindow ('Second 2) 50) (IPAddressPolicy "sliding:") :>
      "sliding-window" :>
      Get '[JSON] String
 :<|> RateLimit (FixedWindow ('Second 2) 50) ApiKeyPolicy :>
      "fixed-window-api-key" :>
      Get '[JSON] String
 :<|> "unrestricted" :>
      Get '[JSON] String

-- | `testApi` is a `Proxy` for `TestAPI`, as required by Servant.
testApi :: Proxy TestAPI
testApi = Proxy

-- | `server` implements a Servant `Server` for `TestAPI`.
server :: Server TestAPI
server =
    pure "Fixed window" :<|>
    pure "Sliding window" :<|>
    pure "Fixed window (API key)" :<|>
    pure "Unrestricted"

getFixedWindow :: ClientM String
getSlidingWindow :: ClientM String
getFixedWindowApiKey :: ClientM String
getUnrestricted :: ClientM String
getFixedWindow :<|>
    getSlidingWindow :<|>
    getFixedWindowApiKey :<|>
    getUnrestricted = client testApi

--------------------------------------------------------------------------------

-- | `appTestCase` @app name handler@ is a wrapper around `testCase` which
-- constructs a `TestTree` named @name@, but for which we run a web server
-- running @app@. The test @handler@ is given a Servant `ClientEnv` which is
-- configured to communicate with the web server.
appTestCase :: Application -> TestName -> (ClientEnv -> Assertion) -> TestTree
appTestCase app name test =
    testCase name $
    Warp.testWithApplication (pure app) $ \port -> do

        manager <- newManager defaultManagerSettings

        let baseUrl = BaseUrl Http "localhost" port ""
        let env = mkClientEnv manager baseUrl{ baseUrlPort = port }

        test env

-- | `assertSuccess` @result@ fails if @result@ indicates an error.
assertSuccess :: Either ClientError a -> Assertion
assertSuccess (Left err) = assertFailure (show err)
assertSuccess _ = pure ()

-- | `assertFailed` @result@ fails if @result@ indicates success.
assertFailed :: Either ClientError a -> Assertion
assertFailed (Right _) =
    assertFailure "Excepted the request to fail, but it succeeded"
assertFailed _ = pure ()

rateLimitedSession :: ClientM a -> ClientEnv -> Assertion
rateLimitedSession endpoint env = do
    forM_ [0..49] $ const $ do
        res <- runClientM endpoint env
        assertSuccess res

    res <- runClientM endpoint env
    assertFailed res

expirySession :: ClientM a -> ClientEnv -> Assertion
expirySession endpoint env = do
    -- make 50 requests
    forM_ [0..49] $ const $ do
        res <- runClientM endpoint env
        assertSuccess res

    -- sleep for 3 seconds
    liftIO $ threadDelay (3 * 1000 * 1000)

    -- make another request
    res <- runClientM endpoint env
    assertSuccess res

tests :: Application -> TestTree
tests app = testGroup "Servant.RateLimiting"
    [ appTestCase app "Fixed window: gets rate limited" $
        rateLimitedSession getFixedWindow
    , appTestCase app "Fixed window: rate limit resets" $
        expirySession getFixedWindow
    , appTestCase app "Sliding window: gets rate limited" $
        rateLimitedSession getSlidingWindow
    , appTestCase app "Sliding window: rate limit resets" $
        expirySession getSlidingWindow
    , appTestCase app "Fixed window (API key): gets rate limited" $
        rateLimitedSession getFixedWindowApiKey
    ]

-- | `main` is the main entry point for the test suite in which we initialise
-- a Redis connection and the Servant server before running the tests.
main :: IO ()
main = do
    -- connect to the Redis server and construct a backend for the connection
    backend <- redisBackend <$> checkedConnect defaultConnectInfo

    -- stick the Redis backend into the Servant context so that we can access
    -- it when we try to apply rate limiting policies
    -- also include an arbitrary API key for the custom rate limiting policy
    -- tests
    let ctx = backend :. MkApiKey "X-API-Test" :. EmptyContext

    -- construct the Servant application using the context
    let app = serveWithContext testApi ctx server

    -- run the tests
    defaultMain (tests app)

--------------------------------------------------------------------------------
