---
title: "Caching"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Caching strategies in Go: in-memory caching, distributed caching with Redis, cache patterns"
weight: 1000070
tags: ["golang", "caching", "redis", "performance", "distributed-cache", "production"]
---

## Why Caching Matters

Caching reduces database load, improves response times, and scales applications by storing frequently accessed data in fast storage (memory, Redis). Without caching, every request hits the database causing slow responses, high latency, and poor user experience under load. Understanding caching patterns prevents performance bottlenecks, reduces infrastructure costs, and enables horizontal scaling.

**Core benefits**:

- **Performance**: 100-1000x faster than database queries
- **Scalability**: Reduces database load (serves more users)
- **Cost reduction**: Less database resources needed
- **Availability**: Serves cached data even if database slow

**Problem**: Standard library provides sync.Map for concurrent access but no TTL (time-to-live), no eviction policies, no distributed caching. Manual implementation leads to memory leaks and stale data.

**Solution**: Start with sync.Map for basic in-memory caching to understand fundamentals, identify limitations (no TTL, no eviction), then use production libraries (go-cache for in-memory with TTL, Redis for distributed) for comprehensive caching.

## Standard Library: sync.Map

Go's sync.Map provides thread-safe map for concurrent access.

**Pattern from standard library**:

```go
package main

import (
    "fmt"
    // => Standard library for formatted output
    "sync"
    // => Standard library for concurrency primitives
    // => sync.Map is concurrent-safe map
    "time"
    // => Standard library for time operations
)

var cache sync.Map
// => Global cache (thread-safe)
// => No initialization needed
// => Optimized for append-once, read-many pattern

func getUser(id int) string {
    // => Gets user from cache or database
    // => Returns user data

    // Check cache first
    if value, ok := cache.Load(id);ok {
        // => cache.Load(key) returns (value, found)
        // => value is interface{} (type assertion needed)
        // => found is true if key exists

        fmt.Println("Cache hit for user", id)
        // => Data found in cache (fast path)

        return value.(string)
        // => Type assertion to string
        // => Panics if wrong type (production: check type)
    }

    // Cache miss: fetch from database
    fmt.Println("Cache miss for user", id)
    // => Data not in cache (slow path)

    user := fetchFromDatabase(id)
    // => Simulates database query
    // => user is "User-1", "User-2", etc.

    cache.Store(id, user)
    // => Store in cache for future requests
    // => cache.Store(key, value) is thread-safe
    // => No expiration (cached forever)

    return user
}

func fetchFromDatabase(id int) string {
    // => Simulates slow database query
    // => Production: actual database call

    time.Sleep(100 * time.Millisecond)
    // => Simulates 100ms database latency
    // => Real databases: 10-100ms typical

    return fmt.Sprintf("User-%d", id)
    // => Returns user data
}

func main() {
    // First request: cache miss
    fmt.Println(getUser(1))
    // => Output: Cache miss for user 1
    // => Output: User-1
    // => 100ms delay (database query)

    // Second request: cache hit
    fmt.Println(getUser(1))
    // => Output: Cache hit for user 1
    // => Output: User-1
    // => < 1ms (from memory)

    // Concurrent access (safe with sync.Map)
    var wg sync.WaitGroup
    for i := 0; i < 10; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()
            getUser(id)
            // => Multiple goroutines access cache safely
            // => No data races
        }(i % 3)  // Access users 0, 1, 2 repeatedly
    }
    wg.Wait()
}
```

**Deleting from cache**:

```go
package main

import (
    "sync"
)

var cache sync.Map

func invalidateUser(id int) {
    // => Removes user from cache
    // => Called when user updated in database

    cache.Delete(id)
    // => Delete(key) removes entry
    // => Thread-safe operation
    // => Next getUser() will fetch from database
}

func updateUser(id int, newData string) {
    // => Updates user in database and cache

    updateDatabase(id, newData)
    // => Update database first

    invalidateUser(id)
    // => Remove from cache (or update cache directly)
    // => Next request will fetch updated data
}

func updateDatabase(id int, newData string) {
    // => Simulates database update
    // Production: actual database UPDATE query
}
```

**Limitations for production caching**:

- No TTL (time-to-live) - entries cached forever
- No eviction policies (memory leaks possible)
- No cache size limits (unbounded growth)
- No statistics (cache hit rate, miss rate)
- No distributed caching (single-process only)
- No automatic expiration (manual deletion required)

## Production Framework: In-Memory Cache with TTL

go-cache provides in-memory caching with TTL and eviction.

**Adding go-cache**:

```bash
go get github.com/patrickmn/go-cache
# => Installs in-memory cache library
# => Supports TTL, eviction, cleanup
```

**Pattern: Cache with TTL**:

```go
package main

import (
    "fmt"
    "time"

    "github.com/patrickmn/go-cache"
    // => In-memory cache library
    // => Thread-safe, supports TTL
)

var c *cache.Cache
// => Global cache instance

func init() {
    // => Initializes cache on package load

    c = cache.New(5*time.Minute, 10*time.Minute)
    // => New(defaultTTL, cleanupInterval)
    // => defaultTTL: 5 minutes (entries expire after 5min)
    // => cleanupInterval: 10 minutes (cleanup expired entries every 10min)
    // => Thread-safe (multiple goroutines safe)
}

func getUser(id int) (string, error) {
    // => Gets user from cache or database
    // => Returns user data or error

    key := fmt.Sprintf("user:%d", id)
    // => key is "user:1", "user:2", etc.
    // => Namespace keys to avoid collisions

    // Check cache
    if value, found := c.Get(key); found {
        // => c.Get(key) returns (value, found)
        // => value is interface{} (type assertion needed)

        fmt.Println("Cache hit for", key)
        return value.(string), nil
        // => Type assertion to string
        // => Cache hit (fast path)
    }

    // Cache miss
    fmt.Println("Cache miss for", key)
    user := fetchFromDatabase(id)

    c.Set(key, user, cache.DefaultExpiration)
    // => Set(key, value, duration)
    // => cache.DefaultExpiration uses 5 minutes (from New())
    // => Alternative: specific duration (1*time.Hour)
    // => cache.NoExpiration for no TTL

    return user, nil
}

func getUserWithCustomTTL(id int, ttl time.Duration) (string, error) {
    // => Gets user with custom TTL
    // => Different data has different freshness requirements

    key := fmt.Sprintf("user:%d", id)

    if value, found := c.Get(key); found {
        return value.(string), nil
    }

    user := fetchFromDatabase(id)

    c.Set(key, user, ttl)
    // => Custom TTL (e.g., 1 hour for frequently accessed data)

    return user, nil
}

func invalidateUser(id int) {
    // => Removes user from cache
    // => Called when user updated

    key := fmt.Sprintf("user:%d", id)
    c.Delete(key)
    // => Delete(key) removes entry immediately
    // => Next request will miss cache
}

func getCacheStats() {
    // => Gets cache statistics

    itemCount := c.ItemCount()
    // => Number of items in cache

    fmt.Printf("Cache items: %d\n", itemCount)
    // => Output: Cache items: 42
}

func fetchFromDatabase(id int) string {
    time.Sleep(100 * time.Millisecond)
    return fmt.Sprintf("User-%d", id)
}

func main() {
    // First request: cache miss
    user, _ := getUser(1)
    fmt.Println(user)
    // => Output: Cache miss for user:1
    // => Output: User-1

    // Second request: cache hit
    user, _ = getUser(1)
    fmt.Println(user)
    // => Output: Cache hit for user:1
    // => Output: User-1

    // Wait for TTL expiration
    time.Sleep(6 * time.Minute)
    // => Entry expired after 5 minutes

    // Request after expiration: cache miss
    user, _ = getUser(1)
    // => Output: Cache miss for user:1
    // => Fetches from database again

    getCacheStats()
}
```

**Pattern: Cache-Aside (Lazy Loading)**:

```go
package main

import (
    "fmt"
    "time"

    "github.com/patrickmn/go-cache"
)

var c *cache.Cache

func init() {
    c = cache.New(5*time.Minute, 10*time.Minute)
}

func getUser(id int) (string, error) {
    // => Cache-Aside pattern (most common)
    // => Application manages cache loading

    key := fmt.Sprintf("user:%d", id)

    // 1. Check cache first
    if value, found := c.Get(key); found {
        return value.(string), nil
        // => CACHE HIT: return immediately
    }

    // 2. Cache miss: fetch from database
    user := fetchFromDatabase(id)

    // 3. Update cache for next request
    c.Set(key, user, cache.DefaultExpiration)

    // 4. Return data
    return user, nil
    // => CACHE MISS: fetch, cache, return
}
```

**Pattern: Write-Through Cache**:

```go
package main

import (
    "fmt"
    "time"

    "github.com/patrickmn/go-cache"
)

var c *cache.Cache

func updateUser(id int, newData string) error {
    // => Write-Through pattern
    // => Updates database AND cache together

    key := fmt.Sprintf("user:%d", id)

    // 1. Update database first
    if err := updateDatabase(id, newData); err != nil {
        // => Database update failed

        return err
        // => Don't update cache if database fails
    }

    // 2. Update cache (keep in sync)
    c.Set(key, newData, cache.DefaultExpiration)
    // => Cache now consistent with database

    return nil
}

func updateDatabase(id int, newData string) error {
    // => Simulates database UPDATE
    // Production: actual SQL UPDATE
    time.Sleep(50 * time.Millisecond)
    return nil
}
```

## Production Framework: Distributed Cache with Redis

Redis provides distributed caching across multiple servers.

**Adding go-redis**:

```bash
go get github.com/redis/go-redis/v9
# => Installs Redis client library
# => v9 supports Redis 7.0+
```

**Pattern: Redis Cache**:

```go
package main

import (
    "context"
    "fmt"
    "time"

    "github.com/redis/go-redis/v9"
    // => Redis client library
    // => Thread-safe, connection pooling
)

var rdb *redis.Client
// => Global Redis client

func init() {
    // => Initializes Redis connection

    rdb = redis.NewClient(&redis.Options{
        Addr:     "localhost:6379",
        // => Redis server address
        // => Production: load from environment variable

        Password: "",
        // => Redis password (empty if no auth)
        // => Production: load from secure config

        DB:       0,
        // => Database number (0-15)
        // => Different databases for different apps

        PoolSize: 10,
        // => Connection pool size
        // => More connections = more concurrent requests
    })
    // => Creates Redis client with connection pool
    // => Thread-safe (reuse across goroutines)
}

func getUser(ctx context.Context, id int) (string, error) {
    // => Gets user from Redis or database
    // => ctx for timeout and cancellation

    key := fmt.Sprintf("user:%d", id)

    // Check Redis cache
    value, err := rdb.Get(ctx, key).Result()
    // => rdb.Get(ctx, key) returns StringCmd
    // => .Result() returns (string, error)
    // => err is redis.Nil if key doesn't exist

    if err == redis.Nil {
        // => Cache miss (key not found)

        fmt.Println("Cache miss for", key)

        user := fetchFromDatabase(id)

        // Store in Redis with TTL
        err := rdb.Set(ctx, key, user, 5*time.Minute).Err()
        // => Set(ctx, key, value, expiration)
        // => Expires after 5 minutes
        // => .Err() returns error or nil

        if err != nil {
            // => Redis SET failed (log but continue)
            fmt.Println("Redis set error:", err)
        }

        return user, nil
    } else if err != nil {
        // => Redis connection error

        fmt.Println("Redis error:", err)
        // => Log error, fallback to database

        return fetchFromDatabase(id), nil
        // => Graceful degradation (continue without cache)
    }

    // Cache hit
    fmt.Println("Cache hit for", key)
    return value, nil
}

func invalidateUser(ctx context.Context, id int) error {
    // => Removes user from Redis
    // => Called when user updated

    key := fmt.Sprintf("user:%d", id)

    err := rdb.Del(ctx, key).Err()
    // => Del(ctx, keys...) deletes keys
    // => Returns error if deletion fails

    return err
}

func main() {
    ctx := context.Background()
    // => Background context for operations

    // First request: cache miss
    user, _ := getUser(ctx, 1)
    fmt.Println(user)
    // => Output: Cache miss for user:1
    // => Output: User-1

    // Second request: cache hit
    user, _ = getUser(ctx, 1)
    fmt.Println(user)
    // => Output: Cache hit for user:1
    // => Output: User-1

    // Invalidate cache
    invalidateUser(ctx, 1)
    fmt.Println("Cache invalidated")

    // Request after invalidation: cache miss
    user, _ = getUser(ctx, 1)
    // => Output: Cache miss for user:1
}

func fetchFromDatabase(id int) string {
    time.Sleep(100 * time.Millisecond)
    return fmt.Sprintf("User-%d", id)
}
```

**Pattern: Cache Warming**:

```go
package main

import (
    "context"
    "fmt"

    "github.com/redis/go-redis/v9"
)

func warmCache(ctx context.Context) error {
    // => Pre-loads frequently accessed data into cache
    // => Called on application startup

    popularUserIDs := []int{1, 2, 3, 10, 42}
    // => Most accessed users (from analytics)
    // => Production: query database for popular IDs

    for _, id := range popularUserIDs {
        user := fetchFromDatabase(id)
        // => Fetch from database

        key := fmt.Sprintf("user:%d", id)
        rdb.Set(ctx, key, user, 5*time.Minute)
        // => Pre-populate cache
        // => First user requests will be cache hits
    }

    fmt.Println("Cache warmed")
    return nil
}
```

## Trade-offs: When to Use Each

**Comparison table**:

| Approach      | Scope          | Persistence | Use Case                            |
| ------------- | -------------- | ----------- | ----------------------------------- |
| **sync.Map**  | Single process | In-memory   | Development, testing                |
| **go-cache**  | Single process | In-memory   | Single-server apps, session storage |
| **Redis**     | Distributed    | Persistent  | Multi-server apps, microservices    |
| **Memcached** | Distributed    | In-memory   | High-throughput caching             |

**When to use sync.Map**:

- Development and testing (simple, no dependencies)
- Single process (no distributed caching needed)
- Short-lived data (no TTL required)
- Append-once, read-many pattern

**When to use go-cache**:

- Single-server applications (no distribution needed)
- Session storage (in-memory sessions)
- Temporary data with TTL (automatic expiration)
- Low latency requirements (<1ms)

**When to use Redis**:

- Multi-server applications (shared cache)
- Microservices (distributed cache)
- Persistent caching (survives restarts)
- Advanced features (sorted sets, pub/sub, transactions)
- High availability (Redis Cluster, Redis Sentinel)

**When to use Memcached**:

- High-throughput caching (10K+ req/sec)
- Simple key-value storage (no complex data structures)
- Lower memory footprint than Redis

## Production Best Practices

**Set appropriate TTLs based on data freshness**:

```go
// GOOD: different TTLs for different data
c.Set("user:profile:1", profile, 1*time.Hour)      // Rarely changes
c.Set("user:session:1", session, 15*time.Minute)   // Short-lived
c.Set("product:inventory:1", inventory, 1*time.Minute)  // Frequently updated

// BAD: same TTL for all data
c.Set("key", value, cache.DefaultExpiration)  // Ignores data freshness requirements
```

**Handle cache stampede (thundering herd)**:

```go
// GOOD: use singleflight to prevent cache stampede
import "golang.org/x/sync/singleflight"

var g singleflight.Group

func getUser(id int) (string, error) {
    key := fmt.Sprintf("user:%d", id)

    // Check cache
    if value, found := c.Get(key); found {
        return value.(string), nil
    }

    // singleflight ensures only one database query for concurrent requests
    value, err, _ := g.Do(key, func() (interface{}, error) {
        // => Only one goroutine executes this function
        // => Other goroutines wait for result
        user := fetchFromDatabase(id)
        c.Set(key, user, cache.DefaultExpiration)
        return user, nil
    })

    return value.(string), err
}

// BAD: cache stampede on expiration
// => 100 concurrent requests all miss cache
// => 100 database queries executed simultaneously
```

**Implement cache fallback on errors**:

```go
// GOOD: graceful degradation
func getUser(ctx context.Context, id int) (string, error) {
    key := fmt.Sprintf("user:%d", id)

    // Try Redis
    value, err := rdb.Get(ctx, key).Result()
    if err == redis.Nil {
        // Cache miss (expected)
        return fetchFromDatabase(id), nil
    } else if err != nil {
        // Redis error (log and fallback)
        fmt.Println("Redis error:", err)
        return fetchFromDatabase(id), nil  // Graceful degradation
    }

    return value, nil
}

// BAD: fail on cache errors
if err != nil {
    return "", err  // Application fails if Redis down
}
```

**Monitor cache hit rate**:

```go
// Track cache performance
type CacheStats struct {
    Hits   int64
    Misses int64
}

func (s *CacheStats) HitRate() float64 {
    total := s.Hits + s.Misses
    if total == 0 {
        return 0
    }
    return float64(s.Hits) / float64(total)
}

// Log cache hit rate periodically
// => Target: 80%+ hit rate for production caches
```

## Summary

Caching improves performance and scalability by storing frequently accessed data in fast storage. Standard library provides sync.Map for concurrent access but no TTL, eviction, or distribution. Production systems use go-cache for in-memory caching with TTL and automatic cleanup, Redis for distributed caching across multiple servers. Use cache-aside for lazy loading, write-through for consistency, and singleflight to prevent cache stampede. Monitor cache hit rate (target 80%+) and implement graceful degradation on cache errors.

**Key takeaways**:

- sync.Map provides thread-safe map but no TTL or eviction
- go-cache adds TTL and automatic cleanup for single-process caching
- Redis enables distributed caching across multiple servers
- Cache-aside (lazy loading) is most common pattern
- Use singleflight to prevent cache stampede (thundering herd)
- Set TTLs based on data freshness requirements
- Implement graceful degradation on cache errors
- Monitor cache hit rate (target 80%+ for production)
