# Rate limiting as WAI middleware

![MIT](https://img.shields.io/github/license/mbg/wai-rate-limit)
![CI](https://github.com/mbg/wai-rate-limit/workflows/CI/badge.svg?branch=main)
![stackage-nightly](https://github.com/mbg/wai-rate-limit/workflows/stackage-nightly/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/wai-rate-limit)](https://hackage.haskell.org/package/wai-rate-limit)

This repository contains WAI middleware for rate limiting. The main library is `wai-rate-limit` which provides the WAI middleware as well as implementations of different rate limiting strategies.

To limit dependencies introduced by `wai-rate-limit`, storage backends are split up into their own packages:

- A Redis backend is provided by `wai-rate-limit-redis` [![Hackage](https://img.shields.io/hackage/v/wai-rate-limit-redis)](https://hackage.haskell.org/package/wai-rate-limit-redis)

## Usage

### Sliding Window

The following example demonstrates how to use the middleware with a sliding window strategy and a Redis backend. The resulting middleware will limit requests to 50 requests per sliding window of 29 seconds based on keys derived from the client's IP address. In other words, if we receive a request, we check whether the limit of 50 has been exceed for the client based on their IP and if not, the request is allowed, the request count is increased, and the window is extended by 29 seconds. If the limit is exceeded, the client will always have to wait 29 seconds before making another request.

```haskell
import qualified Data.ByteString.Char8 as C8

import Database.Redis as Redis

import Network.Wai.RateLimit
import Network.Wai.RateLimit.Strategy
import Network.Wai.RateLimit.Redis

middleware :: Redis.Connection -> Middleware
middleware conn = rateLimiting strategy
    where backend = redisBackend conn
          getKey = pure . C8.pack . show . remoteHost
          strategy = slidingWindow backend 29 50 getKey
```

The behaviour described above can be changed by altering the parameters to `slidingWindow` accordingly. In particular, for e.g. REST APIs, you may wish to use e.g. API keys or other user identifiers in place of IP addresses.

### Fixed Window

The following example demonstrates how to use the middleware with a fixed window strategy and a Redis backend. The resulting middleware will limit requests to 50 requests per window of 29 seconds based on keys derived from the client's IP address.

```haskell
import qualified Data.ByteString.Char8 as C8

import Database.Redis as Redis

import Network.Wai.RateLimit
import Network.Wai.RateLimit.Strategy
import Network.Wai.RateLimit.Redis

middleware :: Redis.Connection -> Middleware
middleware conn = rateLimiting strategy
    where backend = redisBackend conn
          getKey = pure . C8.pack . show . remoteHost
          strategy = fixedWindow backend 29 50 getKey
```

The behaviour described above can be changed by altering the parameters to `fixedWindow` accordingly. In particular, for e.g. REST APIs, you may wish to use e.g. API keys or other user identifiers in place of IP addresses.

### Custom strategies

In addition to the provided strategies, you can implement your own `Strategy` values or customise existing ones. The `Strategy` type is currently defines as follows, so a custom strategy is essentially a function `Request -> IO Bool` which should return `True` if the request should proceed or `False` if it should be rejected:

```haskell
-- | Represents rate limiting strategies.
data Strategy = MkStrategy {
    -- | 'strategyOnRequest' @request@ is a computation which determines
    -- whether the request should be allowed or not, based on the rate
    -- limiting strategy.
    strategyOnRequest :: Request -> IO Bool
}
```

Modifying existing strategies makes it relatively easy to e.g. selectively apply rate limiting to some paths:

```haskell
import qualified Data.ByteString.Char8 as C8

import Database.Redis as Redis

import Network.Wai.RateLimit
import Network.Wai.RateLimit.Strategy
import Network.Wai.RateLimit.Redis

middleware :: Redis.Connection -> Middleware
middleware conn = rateLimiting strategy{ strategyOnRequest = customHandler }
    where backend = redisBackend conn
          getKey = pure . C8.pack . show . remoteHost
          strategy = fixedWindow backend 29 50 getKey
          customHandler req =
              if rawPathInfo req == "/index.html"
              then pure True -- always allow access to /index.html
              else strategyOnRequest strategy req
```
