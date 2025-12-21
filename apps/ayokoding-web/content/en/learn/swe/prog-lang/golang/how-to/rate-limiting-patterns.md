---
title: "How to Implement Rate Limiting"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000240
description: "Practical techniques for implementing rate limiting in Go using token bucket, leaky bucket, and golang.org/x/time/rate"
---

## Problem

APIs need protection from excessive requests. Rate limiting prevents abuse and ensures fair resource allocation.

## Solution

### 1. Using golang.org/x/time/rate

```go
import "golang.org/x/time/rate"

func main() {
    limiter := rate.NewLimiter(10, 20)  // 10 req/sec, burst of 20

    http.HandleFunc("/api", func(w http.ResponseWriter, r *http.Request) {
        if !limiter.Allow() {
            http.Error(w, "Rate limit exceeded", http.StatusTooManyRequests)
            return
        }

        w.Write([]byte("Request processed"))
    })

    http.ListenAndServe(":8080", nil)
}
```

### 2. Per-User Rate Limiting

```go
type RateLimiter struct {
    mu       sync.Mutex
    limiters map[string]*rate.Limiter
}

func NewRateLimiter() *RateLimiter {
    return &RateLimiter{
        limiters: make(map[string]*rate.Limiter),
    }
}

func (rl *RateLimiter) getLimiter(key string) *rate.Limiter {
    rl.mu.Lock()
    defer rl.mu.Unlock()

    limiter, exists := rl.limiters[key]
    if !exists {
        limiter = rate.NewLimiter(1, 5)  // 1 req/sec, burst 5
        rl.limiters[key] = limiter
    }

    return limiter
}

func (rl *RateLimiter) Allow(key string) bool {
    return rl.getLimiter(key).Allow()
}
```

### 3. Middleware Integration

```go
func rateLimitMiddleware(rl *RateLimiter) Middleware {
    return func(next http.Handler) http.Handler {
        return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            userID := getUserID(r)

            if !rl.Allow(userID) {
                http.Error(w, "Rate limit exceeded", http.StatusTooManyRequests)
                return
            }

            next.ServeHTTP(w, r)
        })
    }
}
```

## How It Works

### Token Bucket Algorithm

`golang.org/x/time/rate` implements the token bucket algorithm:

1. **Bucket Capacity**: Maximum burst size (number of tokens)
2. **Refill Rate**: Tokens added per second
3. **Token Consumption**: Each request consumes one token
4. **Allow/Deny**: Request allowed if token available, denied otherwise
5. **Refill**: Tokens automatically refill at specified rate

**Parameters**:

- `rate.NewLimiter(r, b)` creates limiter with rate `r` tokens/second and burst `b`
- `limiter.Allow()` returns true if token available (non-blocking)
- `limiter.Wait(ctx)` blocks until token available (blocking with timeout)

### Limiter State Management

Limiter tracks internal state:

```go
type Limiter struct {
    limit   Rate       // Refill rate (tokens/second)
    burst   int        // Maximum tokens (bucket size)
    tokens  float64    // Current tokens available
    last    time.Time  // Last token update time
}
```

When `Allow()` called:

1. **Calculate elapsed time** since last call
2. **Add new tokens**: `elapsed * rate`
3. **Cap at burst size**: `min(tokens, burst)`
4. **Check availability**: If tokens >= 1, consume 1 and return true
5. **Update state**: Store new token count and timestamp

### Per-User Tracking

Map-based limiter storage:

```go
limiters map[string]*rate.Limiter
```

- **Key**: User identifier (IP, user ID, API key)
- **Value**: Individual rate limiter per user
- **Lookup**: O(1) average case with mutex protection
- **Memory**: Grows with unique users (requires cleanup)

### Cleanup Strategy

Limiters accumulate in memory:

**Problem**: Old users' limiters never removed
**Solution**: Periodic cleanup or LRU eviction

```go
// Cleanup limiters not used in last hour
func (rl *RateLimiter) cleanup() {
    rl.mu.Lock()
    defer rl.mu.Unlock()

    threshold := time.Now().Add(-1 * time.Hour)
    for key, limiter := range rl.limiters {
        if limiter.LastUsed().Before(threshold) {
            delete(rl.limiters, key)
        }
    }
}
```

### Distributed Rate Limiting

For multi-server deployments, use centralized storage:

- **Redis**: INCR + EXPIRE for simple counters
- **Redis + Lua**: Atomic token bucket implementation
- **Database**: Shared state across servers (higher latency)

## Variations

### 1. IP-Based Rate Limiting

Limit by client IP address:

```go
func getClientIP(r *http.Request) string {
    // Check X-Forwarded-For header (behind proxy)
    forwarded := r.Header.Get("X-Forwarded-For")
    if forwarded != "" {
        // Use first IP (original client)
        return strings.Split(forwarded, ",")[0]
    }

    // Direct connection
    ip, _, _ := net.SplitHostPort(r.RemoteAddr)
    return ip
}

func ipRateLimitMiddleware(rl *RateLimiter) Middleware {
    return func(next http.Handler) http.Handler {
        return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            ip := getClientIP(r)

            if !rl.Allow(ip) {
                http.Error(w, "Rate limit exceeded", http.StatusTooManyRequests)
                return
            }

            next.ServeHTTP(w, r)
        })
    }
}
```

**Trade-offs**: Simple but can penalize users behind shared IPs (NAT, proxies).

### 2. Sliding Window with Redis

Implement sliding window counter using Redis:

```go
import "github.com/redis/go-redis/v9"

type RedisRateLimiter struct {
    client *redis.Client
    limit  int
    window time.Duration
}

func (rl *RedisRateLimiter) Allow(ctx context.Context, key string) (bool, error) {
    now := time.Now()
    windowStart := now.Add(-rl.window)

    pipe := rl.client.Pipeline()

    // Remove old entries
    pipe.ZRemRangeByScore(ctx, key, "0", fmt.Sprintf("%d", windowStart.Unix()))

    // Count current requests
    pipe.ZCard(ctx, key)

    // Add current request
    pipe.ZAdd(ctx, key, redis.Z{
        Score:  float64(now.Unix()),
        Member: now.UnixNano(),
    })

    // Set expiry
    pipe.Expire(ctx, key, rl.window)

    results, err := pipe.Exec(ctx)
    if err != nil {
        return false, err
    }

    count := results[1].(*redis.IntCmd).Val()
    return count < int64(rl.limit), nil
}
```

**Trade-offs**: Accurate sliding window but requires Redis dependency.

### 3. Hierarchical Rate Limiting

Apply different limits at multiple levels:

```go
type HierarchicalLimiter struct {
    globalLimiter *rate.Limiter          // All requests
    userLimiters  map[string]*rate.Limiter  // Per user
    ipLimiters    map[string]*rate.Limiter  // Per IP
    mu            sync.Mutex
}

func (hl *HierarchicalLimiter) Allow(userID, ip string) bool {
    // Global limit (most restrictive)
    if !hl.globalLimiter.Allow() {
        return false
    }

    // Per-IP limit
    hl.mu.Lock()
    ipLimiter := hl.getOrCreateLimiter(hl.ipLimiters, ip, 10, 20)
    hl.mu.Unlock()

    if !ipLimiter.Allow() {
        return false
    }

    // Per-user limit (least restrictive)
    hl.mu.Lock()
    userLimiter := hl.getOrCreateLimiter(hl.userLimiters, userID, 100, 200)
    hl.mu.Unlock()

    return userLimiter.Allow()
}
```

**Trade-offs**: Fine-grained control but more complex logic and memory usage.

### 4. Adaptive Rate Limiting

Adjust limits based on system load:

```go
type AdaptiveLimiter struct {
    baseRate     float64
    currentRate  float64
    mu           sync.RWMutex
    limiter      *rate.Limiter
}

func (al *AdaptiveLimiter) UpdateRate(cpuUsage float64) {
    al.mu.Lock()
    defer al.mu.Unlock()

    // Reduce rate when CPU high
    if cpuUsage > 0.8 {
        al.currentRate = al.baseRate * 0.5
    } else if cpuUsage > 0.6 {
        al.currentRate = al.baseRate * 0.75
    } else {
        al.currentRate = al.baseRate
    }

    // Update limiter
    al.limiter.SetLimit(rate.Limit(al.currentRate))
}
```

**Trade-offs**: Protects system under load but variable performance for users.

### 5. Rate Limit Headers

Include rate limit info in HTTP headers:

```go
func rateLimitWithHeaders(rl *RateLimiter) Middleware {
    return func(next http.Handler) http.Handler {
        return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            userID := getUserID(r)
            limiter := rl.getLimiter(userID)

            // Reserve a token
            reservation := limiter.Reserve()
            if !reservation.OK() {
                http.Error(w, "Rate limit exceeded", http.StatusTooManyRequests)
                return
            }

            // Calculate remaining
            remaining := int(limiter.Tokens())
            resetTime := time.Now().Add(reservation.Delay())

            // Set headers
            w.Header().Set("X-RateLimit-Limit", "100")
            w.Header().Set("X-RateLimit-Remaining", fmt.Sprintf("%d", remaining))
            w.Header().Set("X-RateLimit-Reset", fmt.Sprintf("%d", resetTime.Unix()))

            next.ServeHTTP(w, r)
        })
    }
}
```

**Trade-offs**: Better user experience but adds header overhead.

## Common Pitfalls

### 1. Not Protecting Limiter Map Access

**Problem**: Concurrent map access causes panics:

```go
// Bad: Race condition
type RateLimiter struct {
    limiters map[string]*rate.Limiter  // No mutex!
}

func (rl *RateLimiter) Allow(key string) bool {
    limiter := rl.limiters[key]  // Concurrent read - RACE!
    if limiter == nil {
        limiter = rate.NewLimiter(1, 5)
        rl.limiters[key] = limiter  // Concurrent write - RACE!
    }
    return limiter.Allow()
}
```

**Solution**: Always protect map with mutex:

```go
// Good: Mutex protects map
type RateLimiter struct {
    mu       sync.Mutex
    limiters map[string]*rate.Limiter
}

func (rl *RateLimiter) Allow(key string) bool {
    rl.mu.Lock()
    limiter, exists := rl.limiters[key]
    if !exists {
        limiter = rate.NewLimiter(1, 5)
        rl.limiters[key] = limiter
    }
    rl.mu.Unlock()

    return limiter.Allow()
}
```

### 2. Memory Leak from Unlimited Limiters

**Problem**: Limiters accumulate indefinitely:

```go
// Bad: Never cleans up old limiters
func (rl *RateLimiter) Allow(key string) bool {
    limiter := rl.getLimiter(key)
    return limiter.Allow()
}
// Memory grows forever as unique keys accumulate
```

**Solution**: Implement periodic cleanup or use LRU cache:

```go
// Good: Periodic cleanup
func (rl *RateLimiter) startCleanup(interval time.Duration) {
    ticker := time.NewTicker(interval)
    go func() {
        for range ticker.C {
            rl.cleanup()
        }
    }()
}

func (rl *RateLimiter) cleanup() {
    rl.mu.Lock()
    defer rl.mu.Unlock()

    cutoff := time.Now().Add(-1 * time.Hour)
    for key, limiter := range rl.limiters {
        // Check if limiter unused (would need tracking)
        if isUnused(limiter, cutoff) {
            delete(rl.limiters, key)
        }
    }
}
```

### 3. Wrong Rate Units

**Problem**: Confusing rate units leads to incorrect limits:

```go
// Bad: rate.NewLimiter(60, 100) is NOT 60 requests per minute!
limiter := rate.NewLimiter(60, 100)  // 60 req/sec, not per minute
```

**Solution**: Use rate.Every for clarity:

```go
// Good: Explicit time unit
limiter := rate.NewLimiter(rate.Every(time.Minute/60), 100)  // 60/min
limiter := rate.NewLimiter(rate.Every(time.Second), 10)      // 10/sec
limiter := rate.NewLimiter(1, 5)                             // 1/sec, burst 5
```

### 4. Using Allow() in Loops

**Problem**: Checking rate limit in tight loop wastes tokens:

```go
// Bad: Consumes multiple tokens unnecessarily
for i := 0; i < 100; i++ {
    if limiter.Allow() {  // Consumes token even if not used!
        processItem(i)
    }
}
```

**Solution**: Use Reserve() or check once before loop:

```go
// Good: Check availability without consuming
for i := 0; i < 100; i++ {
    reservation := limiter.Reserve()
    if !reservation.OK() {
        break
    }
    processItem(i)
}
```

### 5. Ignoring Burst Parameter

**Problem**: Burst too low causes rejection of legitimate traffic spikes:

```go
// Bad: Burst = 1 means no tolerance for bursts
limiter := rate.NewLimiter(10, 1)  // Rejects bursts immediately
```

**Solution**: Set burst to accommodate expected traffic spikes:

```go
// Good: Burst allows temporary spikes
limiter := rate.NewLimiter(10, 50)  // 10/sec sustained, 50 burst
// Allows 50 requests instantly, then 10/sec after
```

### 6. Not Handling Distributed Systems

**Problem**: Per-server limiting ineffective in multi-server setup:

```go
// Bad: Each server has own limiter (100 req/sec * 10 servers = 1000 req/sec total!)
limiter := rate.NewLimiter(100, 200)
```

**Solution**: Use centralized rate limiting with Redis:

```go
// Good: Shared rate limit across servers
func checkRateLimit(ctx context.Context, client *redis.Client, key string) (bool, error) {
    count, err := client.Incr(ctx, key).Result()
    if err != nil {
        return false, err
    }

    if count == 1 {
        client.Expire(ctx, key, time.Second)
    }

    return count <= 100, nil  // 100 req/sec across all servers
}
```

## Related Patterns

**Related Tutorial**: See [Advanced Tutorial - Concurrency](../tutorials/advanced.md#concurrency) for concurrency fundamentals.

**Related How-To**: See [Implement Middleware](./implement-middleware.md) for middleware integration, [Use Context Effectively](./use-context-effectively.md) for context-based limiting.

**Related Cookbook**: See Cookbook recipes "Rate Limiting", "Token Bucket", "Redis Rate Limiting" for ready-to-use implementations.
