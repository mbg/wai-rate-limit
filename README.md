# Rate limiting as WAI middleware

![MIT](https://img.shields.io/github/license/mbg/wai-rate-limit)
![CI](https://github.com/mbg/wai-rate-limit/workflows/CI/badge.svg?branch=master)

This repository contains WAI middleware for rate limiting. The main library is `wai-rate-limit` which provides the WAI middleware as well as implementations of different rate limiting strategies. 

To limit dependencies introduced by `wai-rate-limit`, storage backends are split up into their own packages:

- A Redis backend is provided by `wai-rate-limit-redis`. 

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
