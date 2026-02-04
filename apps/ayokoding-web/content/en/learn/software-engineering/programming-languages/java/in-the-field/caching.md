---
title: "Caching"
date: 2026-02-04T10:00:00+07:00
draft: false
description: Comprehensive guide to caching in Java from manual implementation to distributed caching with Redis and Spring Cache abstraction
weight: 10000026
tags: ["java", "caching", "redis", "caffeine", "spring-cache", "performance"]
---

## Why Caching Matters

Caching stores frequently accessed data in fast-access storage to avoid expensive recomputation or retrieval. Well-designed caching dramatically improves application performance and reduces load on backend systems.

**Core Benefits**:

- **Performance**: Reduce response time by avoiding expensive operations
- **Scalability**: Handle more requests with same infrastructure
- **Cost efficiency**: Reduce database load and computational costs
- **Reliability**: Serve cached data when backend is slow or unavailable
- **User experience**: Faster responses improve satisfaction

**Problem**: Repeatedly fetching same data from databases or computing same results wastes time and resources. Backend systems become bottlenecks under load.

**Solution**: Cache frequently accessed data with appropriate eviction policies, TTLs, and invalidation strategies.

## Caching Trade-offs

Understand fundamental trade-offs before implementing caching.

**Benefits vs Costs**:

| Benefit                 | Cost                      |
| ----------------------- | ------------------------- |
| Faster data access      | Memory consumption        |
| Reduced backend load    | Stale data risk           |
| Improved scalability    | Complexity (invalidation) |
| Better user experience  | Cache warming overhead    |
| Backend fault tolerance | Consistency challenges    |

**When to cache**:

- Data is read frequently (high read-to-write ratio)
- Data is expensive to compute or retrieve
- Data has acceptable staleness tolerance
- Memory is available for cache storage

**When NOT to cache**:

- Data changes constantly (write-heavy)
- Data must be real-time accurate
- Memory is constrained
- Cache overhead exceeds benefit

## Manual Caching (Standard Library)

Java standard library provides basic data structures for simple caching.

### ConcurrentHashMap as Simple Cache

ConcurrentHashMap provides thread-safe in-memory caching.

**Pattern**:

```java
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class UserService {
    private final Map<String, User> cache = new ConcurrentHashMap<>();

    public User getUser(String userId) {
        // Check cache first
        User cached = cache.get(userId);
        if (cached != null) {
            return cached;
        }

        // Cache miss: fetch from database
        User user = fetchFromDatabase(userId);

        // Store in cache
        cache.put(userId, user);

        return user;
    }

    private User fetchFromDatabase(String userId) {
        // Expensive database query
        System.out.println("Fetching user " + userId + " from database");
        // Simulate DB query
        return new User(userId, "User " + userId);
    }

    public static void main(String[] args) {
        UserService service = new UserService();

        // First call: cache miss
        User user1 = service.getUser("user-123");
        System.out.println("Got: " + user1.getName());

        // Second call: cache hit (no database query)
        User user2 = service.getUser("user-123");
        System.out.println("Got: " + user2.getName());
    }
}

class User {
    private final String id;
    private final String name;

    public User(String id, String name) {
        this.id = id;
        this.name = name;
    }

    public String getId() { return id; }
    public String getName() { return name; }
}
```

**ConcurrentHashMap characteristics**:

- Thread-safe (multiple threads can access safely)
- Fast read operations
- No automatic eviction (unbounded growth)
- No TTL support
- No memory management

### WeakHashMap for Auto-Cleanup

WeakHashMap automatically removes entries when keys are garbage collected.

**Pattern**:

```java
import java.util.Map;
import java.util.WeakHashMap;

public class ImageCache {
    // Keys are weak references - GC can remove entries
    private final Map<ImageKey, BufferedImage> cache = new WeakHashMap<>();

    public BufferedImage loadImage(ImageKey key) {
        BufferedImage cached = cache.get(key);
        if (cached != null) {
            return cached;
        }

        BufferedImage image = loadFromDisk(key);
        cache.put(key, image);
        return image;
    }

    private BufferedImage loadFromDisk(ImageKey key) {
        System.out.println("Loading image: " + key.getPath());
        // Load from disk
        return null; // Placeholder
    }

    public static void main(String[] args) {
        ImageCache cache = new ImageCache();

        ImageKey key1 = new ImageKey("/images/logo.png");
        BufferedImage img1 = cache.loadImage(key1);

        // If key1 becomes unreachable and GC runs:
        // - Entry is automatically removed from cache
        // - Memory is freed

        key1 = null; // Key becomes unreachable
        System.gc(); // Suggest GC (not guaranteed to run immediately)
    }
}

class ImageKey {
    private final String path;

    public ImageKey(String path) {
        this.path = path;
    }

    public String getPath() { return path; }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ImageKey)) return false;
        ImageKey imageKey = (ImageKey) o;
        return path.equals(imageKey.path);
    }

    @Override
    public int hashCode() {
        return path.hashCode();
    }
}
```

**WeakHashMap characteristics**:

- Automatic memory management (weak references)
- Entries removed when keys are GC'd
- Not predictable eviction timing
- Use for memory-sensitive caches
- Not suitable for guaranteed caching

### LinkedHashMap for LRU Eviction

LinkedHashMap supports Least Recently Used (LRU) eviction policy.

**Pattern**:

```java
import java.util.LinkedHashMap;
import java.util.Map;

public class LRUCache<K, V> extends LinkedHashMap<K, V> {
    private final int maxSize;

    public LRUCache(int maxSize) {
        // accessOrder=true: LRU ordering (access-based, not insertion-based)
        super(16, 0.75f, true);
        this.maxSize = maxSize;
    }

    @Override
    protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
        // Remove oldest entry when size exceeds max
        return size() > maxSize;
    }

    public static void main(String[] args) {
        LRUCache<String, String> cache = new LRUCache<>(3);

        // Add entries
        cache.put("key1", "value1");
        cache.put("key2", "value2");
        cache.put("key3", "value3");

        System.out.println("Cache: " + cache);
        // {key1=value1, key2=value2, key3=value3}

        // Access key1 (moves to end)
        cache.get("key1");
        System.out.println("After accessing key1: " + cache);
        // {key2=value2, key3=value3, key1=value1}

        // Add key4 (evicts key2 - least recently used)
        cache.put("key4", "value4");
        System.out.println("After adding key4: " + cache);
        // {key3=value3, key1=value1, key4=value4}
    }
}
```

**Thread-safe wrapper**:

```java
import java.util.Collections;
import java.util.Map;

public class ThreadSafeLRUCache<K, V> {
    private final Map<K, V> cache;

    public ThreadSafeLRUCache(int maxSize) {
        // Synchronize access to LRU cache
        this.cache = Collections.synchronizedMap(new LRUCache<>(maxSize));
    }

    public V get(K key) {
        synchronized (cache) {
            return cache.get(key);
        }
    }

    public void put(K key, V value) {
        synchronized (cache) {
            cache.put(key, value);
        }
    }
}
```

### Why Manual Caching Doesn't Scale

**Limitations of standard library caching**:

1. **No TTL support**: Cannot expire entries after time period
2. **No eviction policies**: Limited to size-based LRU with manual implementation
3. **No statistics**: Cannot measure hit rate, miss rate, eviction count
4. **No loading function**: Must manually check, load, and store
5. **Limited thread safety**: Requires manual synchronization or careful design
6. **No async operations**: Blocking cache operations
7. **No memory size limits**: Only entry count limits (not byte size)

**Before**: Manual ConcurrentHashMap with unbounded growth
**After**: Caffeine cache with TTL, size limits, and statistics

## In-Memory Caching

Production-ready caching libraries provide advanced features.

### Caffeine (High-Performance)

Caffeine is the modern, high-performance caching library for Java 8+.

**Maven dependency**:

```xml
<dependency>
    <groupId>com.github.ben-manes.caffeine</groupId>
    <artifactId>caffeine</artifactId>
    <version>3.1.8</version>
</dependency>
```

**Basic usage**:

```java
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import java.time.Duration;

public class CaffeineBasicExample {
    private final Cache<String, User> cache = Caffeine.newBuilder()
        .maximumSize(10_000)                     // Max 10,000 entries
        .expireAfterWrite(Duration.ofMinutes(5)) // TTL: 5 minutes
        .build();

    public User getUser(String userId) {
        // Get or compute
        return cache.get(userId, key -> {
            System.out.println("Cache miss for: " + userId);
            return fetchFromDatabase(userId);
        });
    }

    public void invalidate(String userId) {
        cache.invalidate(userId);
    }

    private User fetchFromDatabase(String userId) {
        // Expensive database query
        return new User(userId, "User " + userId);
    }

    public static void main(String[] args) {
        CaffeineBasicExample example = new CaffeineBasicExample();

        // First call: cache miss
        User user1 = example.getUser("user-123");
        System.out.println("Got: " + user1.getName());

        // Second call: cache hit
        User user2 = example.getUser("user-123");
        System.out.println("Got: " + user2.getName());

        // Invalidate
        example.invalidate("user-123");

        // Third call: cache miss (after invalidation)
        User user3 = example.getUser("user-123");
        System.out.println("Got: " + user3.getName());
    }
}
```

### Loading Cache

LoadingCache automatically loads values on cache miss.

**Pattern**:

```java
import com.github.benmanes.caffeine.cache.LoadingCache;
import com.github.benmanes.caffeine.cache.Caffeine;
import java.time.Duration;

public class LoadingCacheExample {
    private final LoadingCache<String, User> cache = Caffeine.newBuilder()
        .maximumSize(10_000)
        .expireAfterWrite(Duration.ofMinutes(5))
        .build(key -> fetchFromDatabase(key)); // Loading function

    public User getUser(String userId) {
        // Automatically loads on miss using loading function
        return cache.get(userId);
    }

    public Map<String, User> getUsers(Set<String> userIds) {
        // Bulk load
        return cache.getAll(userIds);
    }

    private User fetchFromDatabase(String userId) {
        System.out.println("Fetching user: " + userId);
        return new User(userId, "User " + userId);
    }

    public static void main(String[] args) {
        LoadingCacheExample example = new LoadingCacheExample();

        // Single get
        User user1 = example.getUser("user-123");
        System.out.println("Got: " + user1.getName());

        // Bulk get
        Set<String> ids = Set.of("user-456", "user-789", "user-123");
        Map<String, User> users = example.getUsers(ids);
        System.out.println("Got " + users.size() + " users");
    }
}
```

### Async Cache

AsyncCache provides non-blocking cache operations.

**Pattern**:

```java
import com.github.benmanes.caffeine.cache.AsyncCache;
import com.github.benmanes.caffeine.cache.Caffeine;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class AsyncCacheExample {
    private final ExecutorService executor = Executors.newFixedThreadPool(4);

    private final AsyncCache<String, User> cache = Caffeine.newBuilder()
        .maximumSize(10_000)
        .expireAfterWrite(Duration.ofMinutes(5))
        .executor(executor)
        .buildAsync();

    public CompletableFuture<User> getUserAsync(String userId) {
        return cache.get(userId, (key, executor) ->
            CompletableFuture.supplyAsync(() -> fetchFromDatabase(key), executor)
        );
    }

    private User fetchFromDatabase(String userId) {
        System.out.println("Async fetch for: " + userId);
        // Simulate slow DB query
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        return new User(userId, "User " + userId);
    }

    public static void main(String[] args) {
        AsyncCacheExample example = new AsyncCacheExample();

        // Non-blocking cache access
        CompletableFuture<User> future = example.getUserAsync("user-123");

        future.thenAccept(user -> {
            System.out.println("Async result: " + user.getName());
        });

        System.out.println("Main thread continues...");

        // Wait for completion
        future.join();
        example.executor.shutdown();
    }
}
```

### Eviction Policies

Caffeine supports multiple eviction policies.

**Size-based eviction**:

```java
Cache<String, User> cache = Caffeine.newBuilder()
    .maximumSize(1000) // Max 1000 entries
    .build();
```

**Weight-based eviction** (custom size calculation):

```java
Cache<String, User> cache = Caffeine.newBuilder()
    .maximumWeight(10_000_000) // Max 10MB
    .weigher((String key, User value) -> {
        // Calculate memory size
        return key.length() + value.getEstimatedSize();
    })
    .build();
```

**Time-based eviction**:

```java
// Expire after write (absolute TTL)
Cache<String, User> cache1 = Caffeine.newBuilder()
    .expireAfterWrite(Duration.ofMinutes(5))
    .build();

// Expire after access (idle timeout)
Cache<String, User> cache2 = Caffeine.newBuilder()
    .expireAfterAccess(Duration.ofMinutes(10))
    .build();

// Custom expiry (per-entry TTL)
Cache<String, User> cache3 = Caffeine.newBuilder()
    .expireAfter(new Expiry<String, User>() {
        @Override
        public long expireAfterCreate(String key, User value, long currentTime) {
            // Premium users: 10 minutes
            // Regular users: 5 minutes
            return value.isPremium() ?
                TimeUnit.MINUTES.toNanos(10) :
                TimeUnit.MINUTES.toNanos(5);
        }

        @Override
        public long expireAfterUpdate(String key, User value, long currentTime, long currentDuration) {
            return currentDuration; // Keep existing TTL
        }

        @Override
        public long expireAfterRead(String key, User value, long currentTime, long currentDuration) {
            return currentDuration; // Don't refresh on read
        }
    })
    .build();
```

**Reference-based eviction** (soft or weak references):

```java
// Weak references (entries removed when keys are GC'd)
Cache<String, User> weakCache = Caffeine.newBuilder()
    .weakKeys()
    .weakValues()
    .build();

// Soft references (entries removed when memory is low)
Cache<String, User> softCache = Caffeine.newBuilder()
    .softValues()
    .build();
```

### Cache Statistics

Monitor cache performance with built-in statistics.

**Pattern**:

```java
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.stats.CacheStats;

public class CacheStatsExample {
    private final Cache<String, User> cache = Caffeine.newBuilder()
        .maximumSize(1000)
        .recordStats() // Enable statistics
        .build();

    public User getUser(String userId) {
        return cache.get(userId, this::fetchFromDatabase);
    }

    public void printStats() {
        CacheStats stats = cache.stats();

        System.out.println("Cache Statistics:");
        System.out.println("  Hit count: " + stats.hitCount());
        System.out.println("  Miss count: " + stats.missCount());
        System.out.println("  Hit rate: " + stats.hitRate() * 100 + "%");
        System.out.println("  Load success count: " + stats.loadSuccessCount());
        System.out.println("  Load failure count: " + stats.loadFailureCount());
        System.out.println("  Average load penalty: " + stats.averageLoadPenalty() + "ns");
        System.out.println("  Eviction count: " + stats.evictionCount());
    }

    private User fetchFromDatabase(String userId) {
        return new User(userId, "User " + userId);
    }

    public static void main(String[] args) {
        CacheStatsExample example = new CacheStatsExample();

        // Generate cache activity
        example.getUser("user-1");
        example.getUser("user-1"); // Hit
        example.getUser("user-2");
        example.getUser("user-1"); // Hit
        example.getUser("user-3");

        example.printStats();
    }
}
```

**Interpreting statistics**:

- **High hit rate (>80%)**: Cache is effective
- **Low hit rate (<50%)**: Cache may not be helping, or data not cacheable
- **High eviction count**: Cache size too small, increase maximum size
- **High load penalty**: Data fetching is slow, caching is valuable

### Guava Cache (Legacy)

Guava Cache is the predecessor to Caffeine, still widely used.

**Maven dependency**:

```xml
<dependency>
    <groupId>com.google.guava</groupId>
    <artifactId>guava</artifactId>
    <version>33.0.0-jre</version>
</dependency>
```

**Pattern**:

```java
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import java.util.concurrent.TimeUnit;

public class GuavaCacheExample {
    private final Cache<String, User> cache = CacheBuilder.newBuilder()
        .maximumSize(10_000)
        .expireAfterWrite(5, TimeUnit.MINUTES)
        .recordStats()
        .build();

    public User getUser(String userId) {
        try {
            return cache.get(userId, () -> fetchFromDatabase(userId));
        } catch (Exception e) {
            throw new RuntimeException("Failed to load user", e);
        }
    }

    private User fetchFromDatabase(String userId) {
        return new User(userId, "User " + userId);
    }
}
```

### Caffeine vs Guava Trade-offs

| Feature             | Caffeine             | Guava Cache      |
| ------------------- | -------------------- | ---------------- |
| **Performance**     | Faster (2-5x)        | Slower           |
| **Memory overhead** | Lower                | Higher           |
| **Java version**    | Java 8+              | Java 8+          |
| **Async support**   | Yes (AsyncCache)     | No               |
| **API style**       | Functional (lambdas) | Callback-based   |
| **Active dev**      | Active               | Maintenance mode |
| **Maturity**        | Mature (since 2015)  | Very mature      |
| **Eviction**        | Window TinyLFU       | LRU-based        |
| **Recommendation**  | Use for new projects | Legacy projects  |

**Migration from Guava to Caffeine** is straightforward:

```java
// Guava
Cache<String, User> guavaCache = CacheBuilder.newBuilder()
    .maximumSize(1000)
    .expireAfterWrite(5, TimeUnit.MINUTES)
    .build();

// Caffeine (equivalent)
Cache<String, User> caffeineCache = Caffeine.newBuilder()
    .maximumSize(1000)
    .expireAfterWrite(Duration.ofMinutes(5))
    .build();
```

## Distributed Caching

Distributed caches share data across multiple application instances.

### Redis (Key-Value Store)

Redis is the most popular distributed cache and data structure server.

**Maven dependency (Lettuce client)**:

```xml
<dependency>
    <groupId>io.lettuce</groupId>
    <artifactId>lettuce-core</artifactId>
    <version>6.3.1.RELEASE</version>
</dependency>
```

**Basic pattern**:

```java
import io.lettuce.core.RedisClient;
import io.lettuce.core.RedisURI;
import io.lettuce.core.api.StatefulRedisConnection;
import io.lettuce.core.api.sync.RedisCommands;

public class RedisExample {
    private final RedisClient client;
    private final StatefulRedisConnection<String, String> connection;
    private final RedisCommands<String, String> commands;

    public RedisExample() {
        RedisURI uri = RedisURI.builder()
            .withHost("localhost")
            .withPort(6379)
            .build();

        this.client = RedisClient.create(uri);
        this.connection = client.connect();
        this.commands = connection.sync();
    }

    public void setUser(String userId, String userData) {
        // Store with 5-minute TTL
        commands.setex("user:" + userId, 300, userData);
    }

    public String getUser(String userId) {
        return commands.get("user:" + userId);
    }

    public void deleteUser(String userId) {
        commands.del("user:" + userId);
    }

    public boolean exists(String userId) {
        return commands.exists("user:" + userId) > 0;
    }

    public void close() {
        connection.close();
        client.shutdown();
    }

    public static void main(String[] args) {
        RedisExample redis = new RedisExample();

        // Set value
        redis.setUser("user-123", "{\"name\":\"Alice\",\"email\":\"alice@example.com\"}");

        // Get value
        String userData = redis.getUser("user-123");
        System.out.println("Retrieved: " + userData);

        // Check existence
        System.out.println("Exists: " + redis.exists("user-123"));

        // Delete
        redis.deleteUser("user-123");
        System.out.println("After delete: " + redis.exists("user-123"));

        redis.close();
    }
}
```

### Redis Data Types

Redis supports multiple data types beyond simple strings.

**String operations**:

```java
// Simple get/set
commands.set("key", "value");
String value = commands.get("key");

// Increment counter
commands.incr("counter");
commands.incrby("counter", 10);

// Multiple keys
commands.mset("key1", "value1", "key2", "value2");
Map<String, String> values = commands.mget("key1", "key2");
```

**Hash operations** (for objects):

```java
// Store user as hash
commands.hset("user:123", "name", "Alice");
commands.hset("user:123", "email", "alice@example.com");
commands.hset("user:123", "age", "30");

// Get single field
String name = commands.hget("user:123", "name");

// Get all fields
Map<String, String> user = commands.hgetall("user:123");

// Set multiple fields
Map<String, String> userData = Map.of(
    "name", "Bob",
    "email", "bob@example.com"
);
commands.hmset("user:456", userData);
```

**List operations** (for queues):

```java
// Push to list
commands.lpush("tasks", "task1", "task2", "task3");

// Pop from list
String task = commands.rpop("tasks");

// Get range
List<String> tasks = commands.lrange("tasks", 0, -1);

// List length
Long length = commands.llen("tasks");
```

**Set operations** (for unique collections):

```java
// Add members
commands.sadd("tags", "java", "redis", "cache");

// Check membership
Boolean exists = commands.sismember("tags", "java");

// Get all members
Set<String> tags = commands.smembers("tags");

// Set operations
commands.sadd("set1", "a", "b", "c");
commands.sadd("set2", "b", "c", "d");
Set<String> union = commands.sunion("set1", "set2");        // {a, b, c, d}
Set<String> intersection = commands.sinter("set1", "set2"); // {b, c}
Set<String> difference = commands.sdiff("set1", "set2");    // {a}
```

**Sorted set operations** (for rankings):

```java
// Add members with scores
commands.zadd("leaderboard", 100, "player1");
commands.zadd("leaderboard", 200, "player2");
commands.zadd("leaderboard", 150, "player3");

// Get range by rank (highest scores first)
List<String> top3 = commands.zrevrange("leaderboard", 0, 2);
// [player2, player3, player1]

// Get range by score
List<String> between100And200 = commands.zrangebyscore("leaderboard", 100, 200);

// Get rank
Long rank = commands.zrevrank("leaderboard", "player1");

// Increment score
commands.zincrby("leaderboard", 50, "player1");
```

### Jedis vs Lettuce Clients

Two popular Redis clients for Java.

**Jedis** (simpler, synchronous):

```xml
<dependency>
    <groupId>redis.clients</groupId>
    <artifactId>jedis</artifactId>
    <version>5.1.0</version>
</dependency>
```

```java
import redis.clients.jedis.Jedis;

public class JedisExample {
    public static void main(String[] args) {
        try (Jedis jedis = new Jedis("localhost", 6379)) {
            jedis.set("key", "value");
            String value = jedis.get("key");
            System.out.println(value);
        }
    }
}
```

**Lettuce** (async, reactive):

```java
import io.lettuce.core.RedisClient;
import io.lettuce.core.api.async.RedisAsyncCommands;
import java.util.concurrent.CompletableFuture;

public class LettuceAsyncExample {
    public static void main(String[] args) {
        RedisClient client = RedisClient.create("redis://localhost:6379");
        StatefulRedisConnection<String, String> connection = client.connect();
        RedisAsyncCommands<String, String> async = connection.async();

        // Async operations
        CompletableFuture<String> setFuture = async.set("key", "value")
            .toCompletableFuture();
        CompletableFuture<String> getFuture = async.get("key")
            .toCompletableFuture();

        getFuture.thenAccept(value -> {
            System.out.println("Value: " + value);
        });

        setFuture.join();
        getFuture.join();

        connection.close();
        client.shutdown();
    }
}
```

**Comparison**:

| Feature              | Jedis                 | Lettuce                 |
| -------------------- | --------------------- | ----------------------- |
| **API style**        | Synchronous           | Sync + Async + Reactive |
| **Connection model** | Single-threaded       | Thread-safe             |
| **Performance**      | Good                  | Better (async)          |
| **Complexity**       | Simpler               | More complex            |
| **Reactive support** | No                    | Yes (Reactor)           |
| **Recommendation**   | Simple sync use cases | Modern async apps       |

### Redis Cluster and Sentinel

Production Redis deployments use clustering or sentinel for high availability.

**Redis Cluster** (sharding):

```java
import io.lettuce.core.cluster.RedisClusterClient;
import io.lettuce.core.cluster.api.StatefulRedisClusterConnection;
import io.lettuce.core.cluster.api.sync.RedisAdvancedClusterCommands;

public class RedisClusterExample {
    public static void main(String[] args) {
        // Connect to cluster nodes
        RedisClusterClient client = RedisClusterClient.create(
            "redis://node1:6379,redis://node2:6379,redis://node3:6379"
        );

        StatefulRedisClusterConnection<String, String> connection = client.connect();
        RedisAdvancedClusterCommands<String, String> commands = connection.sync();

        // Operations automatically routed to correct node
        commands.set("key1", "value1");
        String value = commands.get("key1");

        connection.close();
        client.shutdown();
    }
}
```

**Redis Sentinel** (high availability):

```java
import io.lettuce.core.RedisClient;
import io.lettuce.core.RedisURI;

public class RedisSentinelExample {
    public static void main(String[] args) {
        RedisURI uri = RedisURI.builder()
            .withSentinel("sentinel1", 26379)
            .withSentinel("sentinel2", 26379)
            .withSentinel("sentinel3", 26379)
            .withSentinelMasterId("mymaster")
            .build();

        RedisClient client = RedisClient.create(uri);
        // Automatically connects to current master
        // Fails over to new master if current fails

        client.shutdown();
    }
}
```

### Memcached (Simple Distributed Cache)

Memcached is a simpler alternative to Redis for basic caching.

**Maven dependency (Spymemcached)**:

```xml
<dependency>
    <groupId>net.spy</groupId>
    <artifactId>spymemcached</artifactId>
    <version>2.12.3</version>
</dependency>
```

**Pattern**:

```java
import net.spy.memcached.MemcachedClient;
import java.net.InetSocketAddress;

public class MemcachedExample {
    private final MemcachedClient client;

    public MemcachedExample() throws Exception {
        this.client = new MemcachedClient(
            new InetSocketAddress("localhost", 11211)
        );
    }

    public void setUser(String userId, User user) {
        // Store with 5-minute TTL (300 seconds)
        client.set("user:" + userId, 300, user);
    }

    public User getUser(String userId) {
        return (User) client.get("user:" + userId);
    }

    public void deleteUser(String userId) {
        client.delete("user:" + userId);
    }

    public void close() {
        client.shutdown();
    }

    public static void main(String[] args) throws Exception {
        MemcachedExample memcached = new MemcachedExample();

        User user = new User("user-123", "Alice");
        memcached.setUser("user-123", user);

        User retrieved = memcached.getUser("user-123");
        System.out.println("Retrieved: " + retrieved.getName());

        memcached.close();
    }
}
```

### Redis vs Memcached Comparison

| Feature            | Redis                     | Memcached         |
| ------------------ | ------------------------- | ----------------- |
| **Data types**     | Many (string, hash, list) | String only       |
| **Persistence**    | Yes (RDB, AOF)            | No                |
| **Replication**    | Yes (master-replica)      | No                |
| **Clustering**     | Yes (Redis Cluster)       | Client-side       |
| **Max value size** | 512MB                     | 1MB               |
| **Performance**    | Excellent                 | Slightly faster   |
| **Complexity**     | More features, complex    | Simple            |
| **Use cases**      | Cache + data structures   | Simple caching    |
| **Recommendation** | Most use cases            | Pure caching only |

**When to use Memcached**: Only when you need absolute simplest caching (no persistence, no data structures, no replication). Redis handles most use cases better.

## Cache Strategies

Choose caching strategy based on application requirements.

### Cache-Aside (Lazy Loading)

Application manages cache explicitly. Most common pattern.

**Pattern**:

```java
public class CacheAsideExample {
    private final Cache<String, User> cache = Caffeine.newBuilder()
        .maximumSize(1000)
        .build();

    public User getUser(String userId) {
        // 1. Check cache
        User cached = cache.getIfPresent(userId);
        if (cached != null) {
            return cached;
        }

        // 2. Cache miss: load from database
        User user = loadFromDatabase(userId);

        // 3. Store in cache
        cache.put(userId, user);

        return user;
    }

    public void updateUser(User user) {
        // Update database
        saveToDatabase(user);

        // Invalidate cache
        cache.invalidate(user.getId());
    }

    private User loadFromDatabase(String userId) {
        System.out.println("Loading from database: " + userId);
        return new User(userId, "User " + userId);
    }

    private void saveToDatabase(User user) {
        System.out.println("Saving to database: " + user.getId());
    }
}
```

**Characteristics**:

- Application controls caching logic
- Cache only populated on read (lazy)
- Cache misses load from database
- Writes invalidate cache
- Most flexible pattern

### Read-Through

Cache loads data automatically on miss (transparent to application).

**Pattern**:

```java
public class ReadThroughExample {
    private final LoadingCache<String, User> cache = Caffeine.newBuilder()
        .maximumSize(1000)
        .build(key -> loadFromDatabase(key)); // Loader function

    public User getUser(String userId) {
        // Cache automatically loads on miss
        return cache.get(userId);
    }

    public void updateUser(User user) {
        saveToDatabase(user);
        cache.invalidate(user.getId());
    }

    private User loadFromDatabase(String userId) {
        System.out.println("Read-through loading: " + userId);
        return new User(userId, "User " + userId);
    }

    private void saveToDatabase(User user) {
        System.out.println("Saving to database: " + user.getId());
    }
}
```

**Characteristics**:

- Cache handles loading transparently
- Application doesn't manage cache misses
- Simpler application code
- LoadingCache handles concurrency (single load per key)

### Write-Through

Writes go to cache and database together (synchronously).

**Pattern**:

```java
public class WriteThroughExample {
    private final Cache<String, User> cache = Caffeine.newBuilder()
        .maximumSize(1000)
        .build();

    public User getUser(String userId) {
        return cache.get(userId, key -> loadFromDatabase(key));
    }

    public void updateUser(User user) {
        // Write to database first
        saveToDatabase(user);

        // Then update cache
        cache.put(user.getId(), user);
    }

    private User loadFromDatabase(String userId) {
        return new User(userId, "User " + userId);
    }

    private void saveToDatabase(User user) {
        System.out.println("Write-through: saving " + user.getId());
    }
}
```

**Characteristics**:

- Cache always consistent with database
- Higher write latency (two operations)
- Reduces cache misses after writes
- Data always cached after write

### Write-Behind (Write-Back)

Writes go to cache immediately, database asynchronously.

**Pattern**:

```java
import java.util.concurrent.*;

public class WriteBehindExample {
    private final Cache<String, User> cache = Caffeine.newBuilder()
        .maximumSize(1000)
        .build();

    private final BlockingQueue<User> writeQueue = new LinkedBlockingQueue<>();
    private final ExecutorService writeExecutor = Executors.newSingleThreadExecutor();

    public WriteBehindExample() {
        // Start background writer
        writeExecutor.submit(this::processWrites);
    }

    public User getUser(String userId) {
        return cache.get(userId, key -> loadFromDatabase(key));
    }

    public void updateUser(User user) {
        // Write to cache immediately
        cache.put(user.getId(), user);

        // Queue for async database write
        writeQueue.offer(user);
    }

    private void processWrites() {
        while (!Thread.currentThread().isInterrupted()) {
            try {
                User user = writeQueue.take();
                saveToDatabase(user);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    private User loadFromDatabase(String userId) {
        return new User(userId, "User " + userId);
    }

    private void saveToDatabase(User user) {
        System.out.println("Write-behind: async saving " + user.getId());
    }

    public void shutdown() {
        writeExecutor.shutdown();
    }
}
```

**Characteristics**:

- Fastest write performance (async)
- Risk of data loss (writes pending in queue)
- Requires batching and retry logic
- Complexity vs performance trade-off

### Refresh-Ahead

Proactively refresh cache before expiry.

**Pattern**:

```java
public class RefreshAheadExample {
    private final LoadingCache<String, User> cache = Caffeine.newBuilder()
        .maximumSize(1000)
        .expireAfterWrite(Duration.ofMinutes(5))
        .refreshAfterWrite(Duration.ofMinutes(4)) // Refresh 1 min before expiry
        .build(key -> loadFromDatabase(key));

    public User getUser(String userId) {
        // Refreshes in background if TTL approaching
        return cache.get(userId);
    }

    private User loadFromDatabase(String userId) {
        System.out.println("Loading/refreshing: " + userId);
        return new User(userId, "User " + userId);
    }
}
```

**Characteristics**:

- Prevents cache miss latency spikes
- Background refresh before expiry
- Always serves cached data (never blocks)
- Higher background load

**Comparison matrix**:

| Strategy          | Read Latency | Write Latency | Consistency | Complexity |
| ----------------- | ------------ | ------------- | ----------- | ---------- |
| **Cache-Aside**   | High (miss)  | Low           | Eventual    | Low        |
| **Read-Through**  | High (miss)  | Low           | Eventual    | Low        |
| **Write-Through** | Medium       | High          | Strong      | Medium     |
| **Write-Behind**  | Medium       | Low           | Weak        | High       |
| **Refresh-Ahead** | Low          | Low           | Eventual    | Medium     |

## Spring Cache Abstraction

Spring Framework provides caching abstraction for declarative caching.

**Maven dependencies**:

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-cache</artifactId>
    <version>3.2.2</version>
</dependency>
<dependency>
    <groupId>com.github.ben-manes.caffeine</groupId>
    <artifactId>caffeine</artifactId>
    <version>3.1.8</version>
</dependency>
```

**Enable caching**:

```java
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;

@SpringBootApplication
@EnableCaching
public class Application {
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
```

### @Cacheable Annotation

Cache method results automatically.

**Pattern**:

```java
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

@Service
public class UserService {

    @Cacheable(value = "users", key = "#userId")
    public User getUser(String userId) {
        System.out.println("Fetching user from database: " + userId);
        return loadFromDatabase(userId);
    }

    @Cacheable(value = "users", key = "#userId", unless = "#result == null")
    public User getUserOrNull(String userId) {
        // Don't cache null results
        return loadFromDatabase(userId);
    }

    @Cacheable(value = "users", key = "#userId", condition = "#userId.length() > 0")
    public User getUserConditional(String userId) {
        // Only cache if userId is not empty
        return loadFromDatabase(userId);
    }

    private User loadFromDatabase(String userId) {
        return new User(userId, "User " + userId);
    }
}
```

**Attributes**:

- **value**: Cache name
- **key**: Cache key expression (SpEL)
- **condition**: Cache only if condition true (before method execution)
- **unless**: Don't cache if condition true (after method execution)

### @CacheEvict Annotation

Remove entries from cache.

**Pattern**:

```java
import org.springframework.cache.annotation.CacheEvict;

@Service
public class UserService {

    @CacheEvict(value = "users", key = "#userId")
    public void updateUser(String userId, User user) {
        System.out.println("Updating user: " + userId);
        saveToDatabase(user);
    }

    @CacheEvict(value = "users", allEntries = true)
    public void updateAllUsers() {
        System.out.println("Clearing all users cache");
    }

    @CacheEvict(value = "users", key = "#userId", beforeInvocation = true)
    public void deleteUser(String userId) {
        // Evict before method execution (even if method throws exception)
        deleteFromDatabase(userId);
    }

    private void saveToDatabase(User user) {
        // Database save
    }

    private void deleteFromDatabase(String userId) {
        // Database delete
    }
}
```

### @CachePut Annotation

Always execute method and update cache.

**Pattern**:

```java
import org.springframework.cache.annotation.CachePut;

@Service
public class UserService {

    @CachePut(value = "users", key = "#user.id")
    public User saveUser(User user) {
        System.out.println("Saving user: " + user.getId());
        saveToDatabase(user);
        return user; // Return value stored in cache
    }

    @CachePut(value = "users", key = "#result.id", condition = "#result != null")
    public User createUser(String name, String email) {
        User user = new User(UUID.randomUUID().toString(), name);
        saveToDatabase(user);
        return user;
    }

    private void saveToDatabase(User user) {
        // Database save
    }
}
```

### @Caching Annotation

Combine multiple cache annotations.

**Pattern**:

```java
import org.springframework.cache.annotation.Caching;

@Service
public class UserService {

    @Caching(
        cacheable = {
            @Cacheable(value = "users", key = "#userId")
        },
        evict = {
            @CacheEvict(value = "userList", allEntries = true),
            @CacheEvict(value = "userStats", allEntries = true)
        }
    )
    public User getUserAndInvalidateLists(String userId) {
        return loadFromDatabase(userId);
    }

    private User loadFromDatabase(String userId) {
        return new User(userId, "User " + userId);
    }
}
```

### CacheManager Configuration

Configure cache provider and settings.

**Caffeine configuration**:

```java
import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import java.time.Duration;

@Configuration
public class CacheConfig {

    @Bean
    public CacheManager cacheManager() {
        CaffeineCacheManager cacheManager = new CaffeineCacheManager("users", "products");
        cacheManager.setCaffeine(Caffeine.newBuilder()
            .maximumSize(1000)
            .expireAfterWrite(Duration.ofMinutes(5))
            .recordStats()
        );
        return cacheManager;
    }
}
```

**Multiple cache providers**:

```java
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.concurrent.ConcurrentMapCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

@Configuration
@EnableCaching
public class MultipleCacheConfig {

    @Bean
    @Primary
    public CacheManager defaultCacheManager() {
        return new CaffeineCacheManager("users", "products");
    }

    @Bean
    public CacheManager simpleCacheManager() {
        return new ConcurrentMapCacheManager("temp");
    }
}
```

### Cache Key Generation

Customize cache key generation.

**Default key generation** (all parameters):

```java
@Cacheable("users")
public User getUser(String userId, boolean includeDetails) {
    // Key: SimpleKey[userId, includeDetails]
    return loadFromDatabase(userId, includeDetails);
}
```

**Custom key with SpEL**:

```java
@Cacheable(value = "users", key = "#userId + '-' + #includeDetails")
public User getUser(String userId, boolean includeDetails) {
    // Key: "userId-true"
    return loadFromDatabase(userId, includeDetails);
}
```

**Custom KeyGenerator**:

```java
import org.springframework.cache.interceptor.KeyGenerator;
import java.lang.reflect.Method;

@Configuration
public class CacheConfig {

    @Bean
    public KeyGenerator customKeyGenerator() {
        return (target, method, params) -> {
            StringBuilder key = new StringBuilder(method.getName());
            for (Object param : params) {
                key.append("-").append(param);
            }
            return key.toString();
        };
    }
}

@Cacheable(value = "users", keyGenerator = "customKeyGenerator")
public User getUser(String userId) {
    return loadFromDatabase(userId);
}
```

### Conditional Caching

Cache based on runtime conditions.

**Pattern**:

```java
@Service
public class UserService {

    // Cache only premium users
    @Cacheable(value = "users", key = "#userId", condition = "#result.isPremium()")
    public User getUser(String userId) {
        return loadFromDatabase(userId);
    }

    // Don't cache if user is admin
    @Cacheable(value = "users", key = "#userId", unless = "#result.isAdmin()")
    public User getUserUnlessAdmin(String userId) {
        return loadFromDatabase(userId);
    }

    // Cache only if result has data
    @Cacheable(value = "reports", unless = "#result.isEmpty()")
    public List<Report> getReports(String userId) {
        return loadReports(userId);
    }

    private User loadFromDatabase(String userId) {
        return new User(userId, "User " + userId);
    }

    private List<Report> loadReports(String userId) {
        return List.of();
    }
}
```

## Cache Invalidation Patterns

Keeping cache consistent with underlying data.

### Time-to-Live (TTL)

Expire entries after fixed duration.

**Pattern**:

```java
Cache<String, User> cache = Caffeine.newBuilder()
    .expireAfterWrite(Duration.ofMinutes(5)) // Absolute TTL
    .build();
```

**When to use**:

- Data changes infrequently
- Staleness acceptable for short period
- Simple invalidation strategy
- Low complexity

### Event-Based Invalidation

Invalidate cache when data changes.

**Pattern**:

```java
@Service
public class UserService {
    private final Cache<String, User> cache;
    private final ApplicationEventPublisher eventPublisher;

    public void updateUser(User user) {
        saveToDatabase(user);

        // Invalidate cache
        cache.invalidate(user.getId());

        // Publish event for other caches
        eventPublisher.publishEvent(new UserUpdatedEvent(user.getId()));
    }

    @EventListener
    public void onUserUpdated(UserUpdatedEvent event) {
        cache.invalidate(event.getUserId());
    }
}
```

### Cache Stampede Prevention

Prevent multiple threads from loading same data simultaneously.

**Problem**:

```
Time: 0s   - 100 requests arrive for same key (cache miss)
Time: 0-2s - 100 threads all query database simultaneously (stampede)
Time: 2s   - All 100 results stored in cache (waste)
```

**Solution with Caffeine**:

```java
LoadingCache<String, User> cache = Caffeine.newBuilder()
    .maximumSize(1000)
    .build(key -> {
        // Caffeine guarantees single execution per key
        System.out.println("Loading key: " + key);
        return expensiveLoad(key);
    });

// 100 concurrent requests for same key
// Only 1 actually loads, others wait for result
ExecutorService executor = Executors.newFixedThreadPool(100);
for (int i = 0; i < 100; i++) {
    executor.submit(() -> {
        User user = cache.get("user-123"); // All get same loaded result
    });
}
```

**Solution with manual locking** (if not using LoadingCache):

```java
public class StampedePreventionCache {
    private final Map<String, User> cache = new ConcurrentHashMap<>();
    private final Map<String, CompletableFuture<User>> loadingKeys = new ConcurrentHashMap<>();

    public User get(String userId) {
        // Check cache
        User cached = cache.get(userId);
        if (cached != null) {
            return cached;
        }

        // Check if already loading
        CompletableFuture<User> loadingFuture = loadingKeys.computeIfAbsent(userId, key -> {
            // Only first thread creates future
            return CompletableFuture.supplyAsync(() -> loadFromDatabase(key));
        });

        try {
            // All threads wait on same future
            User user = loadingFuture.join();
            cache.put(userId, user);
            return user;
        } finally {
            loadingKeys.remove(userId);
        }
    }

    private User loadFromDatabase(String userId) {
        System.out.println("Loading from database: " + userId);
        return new User(userId, "User " + userId);
    }
}
```

### Write Invalidation Strategies

Patterns for invalidating cache on writes.

**Invalidate on write**:

```java
public void updateUser(User user) {
    saveToDatabase(user);
    cache.invalidate(user.getId()); // Simple invalidation
}
```

**Update cache on write**:

```java
public void updateUser(User user) {
    saveToDatabase(user);
    cache.put(user.getId(), user); // Write-through
}
```

**Invalidate related caches**:

```java
public void updateUser(User user) {
    saveToDatabase(user);

    // Invalidate user cache
    userCache.invalidate(user.getId());

    // Invalidate related caches
    userListCache.invalidateAll();
    userStatCache.invalidate("total");
    organizationCache.invalidate(user.getOrganizationId());
}
```

**Bulk invalidation**:

```java
public void bulkUpdate(List<User> users) {
    saveBulkToDatabase(users);

    // Invalidate specific keys
    Set<String> userIds = users.stream()
        .map(User::getId)
        .collect(Collectors.toSet());
    userCache.invalidateAll(userIds);

    // Invalidate all related caches
    userListCache.invalidateAll();
}
```

## Best Practices

### Cache Only Immutable or Stable Data

Mutable cached objects cause inconsistencies.

**Problem**:

```java
// BAD: Caching mutable object
User user = cache.get("user-123");
user.setName("New Name"); // Mutates cached object!
// Now cache contains modified object without database update
```

**Solution**:

```java
// GOOD: Cache immutable objects
public record User(String id, String name, String email) {}

// Or: Copy on retrieval
User user = cache.get("user-123");
User copy = user.copy(); // Defensive copy
copy.setName("New Name"); // Only affects copy
```

### Set Appropriate TTLs

Match TTL to data characteristics.

**Guidelines**:

- **Fast-changing data**: Short TTL (1-5 minutes) or no caching
- **Moderately changing**: Medium TTL (5-30 minutes)
- **Rarely changing**: Long TTL (30 minutes - 24 hours)
- **Static data**: Very long TTL (24 hours+) or permanent

**Examples**:

```java
// User profile (changes occasionally)
Cache<String, User> userCache = Caffeine.newBuilder()
    .expireAfterWrite(Duration.ofMinutes(15))
    .build();

// Product catalog (changes rarely)
Cache<String, Product> productCache = Caffeine.newBuilder()
    .expireAfterWrite(Duration.ofHours(1))
    .build();

// Session data (time-sensitive)
Cache<String, Session> sessionCache = Caffeine.newBuilder()
    .expireAfterAccess(Duration.ofMinutes(30))
    .build();

// Configuration (very stable)
Cache<String, Config> configCache = Caffeine.newBuilder()
    .expireAfterWrite(Duration.ofHours(24))
    .build();
```

### Monitor Hit/Miss Ratios

Track cache effectiveness with metrics.

**Pattern**:

```java
Cache<String, User> cache = Caffeine.newBuilder()
    .maximumSize(1000)
    .recordStats()
    .build();

// Periodically check stats
public void logCacheStats() {
    CacheStats stats = cache.stats();

    double hitRate = stats.hitRate();
    double missRate = stats.missRate();

    System.out.println("Hit rate: " + (hitRate * 100) + "%");
    System.out.println("Miss rate: " + (missRate * 100) + "%");
    System.out.println("Eviction count: " + stats.evictionCount());

    // Alert if hit rate too low
    if (hitRate < 0.5) {
        System.err.println("WARNING: Cache hit rate below 50%!");
        // Investigate: TTL too short? Cache size too small?
    }
}
```

**Target hit rates**:

- **80%+**: Excellent caching effectiveness
- **50-80%**: Good, may need tuning
- **<50%**: Poor, investigate cache size or TTL

### Handle Cache Failures Gracefully

Cache should enhance performance, not create dependencies.

**Pattern**:

```java
public class ResilientCacheService {
    private final Cache<String, User> cache;

    public User getUser(String userId) {
        try {
            // Try cache first
            return cache.get(userId, key -> loadFromDatabase(key));
        } catch (Exception e) {
            // Cache failure: fallback to database
            System.err.println("Cache error, falling back to database: " + e.getMessage());
            return loadFromDatabase(userId);
        }
    }

    private User loadFromDatabase(String userId) {
        // Database is source of truth
        return new User(userId, "User " + userId);
    }
}
```

**Distributed cache resilience**:

```java
public class RedisResilientService {
    private final RedisCommands<String, String> redis;
    private final Cache<String, User> localCache;

    public User getUser(String userId) {
        // Try local cache first
        User local = localCache.getIfPresent(userId);
        if (local != null) {
            return local;
        }

        // Try Redis
        try {
            String json = redis.get("user:" + userId);
            if (json != null) {
                User user = parseUser(json);
                localCache.put(userId, user);
                return user;
            }
        } catch (Exception e) {
            System.err.println("Redis error, falling back to database: " + e.getMessage());
        }

        // Fallback to database
        User user = loadFromDatabase(userId);
        localCache.put(userId, user);

        // Try to update Redis (best effort)
        try {
            redis.setex("user:" + userId, 300, serializeUser(user));
        } catch (Exception e) {
            // Ignore Redis write failure
        }

        return user;
    }

    private User loadFromDatabase(String userId) {
        return new User(userId, "User " + userId);
    }

    private User parseUser(String json) {
        // JSON deserialization
        return null;
    }

    private String serializeUser(User user) {
        // JSON serialization
        return "";
    }
}
```

### Avoid Caching User-Specific Data in Shared Caches

User-specific data in shared caches causes security and privacy issues.

**Problem**:

```java
// BAD: User-specific data in shared cache
@Cacheable("users") // Shared cache
public User getCurrentUser() {
    // Returns different user based on authentication context
    return securityContext.getCurrentUser();
}
// User A might get User B's data from cache!
```

**Solution**:

```java
// GOOD: Include user ID in cache key
@Cacheable(value = "users", key = "#userId")
public User getUser(String userId) {
    return loadFromDatabase(userId);
}

// GOOD: User-specific caches
@Cacheable(value = "user-preferences", key = "#userId")
public UserPreferences getPreferences(String userId) {
    return loadPreferences(userId);
}
```

### Use Cache Warming for Critical Data

Pre-populate cache with frequently accessed data on startup.

**Pattern**:

```java
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
public class CacheWarmer {
    private final UserService userService;
    private final Cache<String, User> cache;

    @EventListener(ApplicationReadyEvent.class)
    public void warmCache() {
        System.out.println("Warming cache...");

        // Load critical users
        List<String> criticalUserIds = List.of("admin", "system", "default");
        for (String userId : criticalUserIds) {
            try {
                User user = userService.getUser(userId);
                cache.put(userId, user);
            } catch (Exception e) {
                System.err.println("Failed to warm cache for user: " + userId);
            }
        }

        System.out.println("Cache warming completed");
    }
}
```

## Related Content

- [Performance](/en/learn/software-engineering/programming-languages/java/in-the-field/performance) - Performance optimization patterns, profiling, JVM tuning
- [Concurrency and Parallelism](/en/learn/software-engineering/programming-languages/java/in-the-field/concurrency-and-parallelism) - Thread-safe caching patterns, ConcurrentHashMap usage
- [NoSQL Databases](/en/learn/software-engineering/programming-languages/java/in-the-field/nosql-databases) - Redis beyond caching (pub/sub, streams, data structures)
- [Working with SQL Databases](/en/learn/software-engineering/programming-languages/java/in-the-field/sql-database) - Query result caching strategies

---

**Last Updated**: 2026-02-04
**Java Version**: 17+ (baseline), 21+ (recommended)
