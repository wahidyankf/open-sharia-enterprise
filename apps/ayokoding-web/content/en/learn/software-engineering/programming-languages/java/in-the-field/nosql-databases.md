---
title: "Working with NoSQL Databases"
date: 2026-02-04T00:00:00+07:00
draft: false
description: Comprehensive guide to NoSQL database integration in Java covering MongoDB, Redis, and Cassandra with native drivers and Spring Data abstractions
weight: 10000028
tags: ["java", "nosql", "mongodb", "redis", "cassandra", "spring-data", "databases"]
---

## Why NoSQL Databases Matter

NoSQL databases provide alternatives to relational databases optimized for different data models and scaling patterns. They trade traditional ACID guarantees for flexibility, horizontal scalability, and performance.

**Core Benefits**:

- **Flexible schema**: Adapt data model without migrations
- **Horizontal scalability**: Add nodes to increase capacity
- **High performance**: Optimized for specific access patterns
- **Specialized data models**: Documents, key-value, wide-column, graphs
- **High availability**: Built-in replication and fault tolerance

**Problem**: Relational databases struggle with massive scale, flexible schemas, and certain access patterns (e.g., caching, time-series).

**Solution**: Choose appropriate NoSQL database type based on data model and access patterns.

## NoSQL vs SQL Trade-offs

| Aspect                | SQL                         | NoSQL                                     |
| --------------------- | --------------------------- | ----------------------------------------- |
| **Schema**            | Fixed (enforced)            | Flexible (schema-less or schema-optional) |
| **Scaling**           | Vertical (bigger hardware)  | Horizontal (more nodes)                   |
| **Transactions**      | ACID (strong consistency)   | BASE (eventual consistency)               |
| **Joins**             | Complex joins supported     | Limited or no joins (denormalize)         |
| **Query flexibility** | SQL (very flexible)         | Varies by database type                   |
| **Use cases**         | Financial, traditional apps | Big data, real-time, flexible schema      |

**Key insight**: SQL and NoSQL are complementary - many applications use both (polyglot persistence).

## CAP Theorem

CAP theorem states distributed systems can provide only 2 of 3 guarantees:

- **C**onsistency: All nodes see same data at same time
- **A**vailability: System responds to requests (no downtime)
- **P**artition tolerance: System works despite network splits

**NoSQL choices**:

- **CP (Consistency + Partition tolerance)**: MongoDB, HBase, Redis Cluster
- **AP (Availability + Partition tolerance)**: Cassandra, DynamoDB, Riak

**Trade-off**: Choose consistency (CP) for financial data, availability (AP) for social media feeds.

## NoSQL Database Types

### Document Stores

**Model**: JSON-like documents with nested structure

**Examples**: MongoDB, CouchDB

**Use cases**: Content management, user profiles, catalogs

**Strengths**:

- Flexible schema (add fields without migrations)
- Rich queries (secondary indexes, aggregation)
- Natural mapping to objects

**Weaknesses**:

- No joins (denormalize data)
- Document size limits (16MB in MongoDB)
- Eventual consistency (configurable)

### Key-Value Stores

**Model**: Simple key → value mapping

**Examples**: Redis, DynamoDB, Riak

**Use cases**: Caching, session storage, real-time analytics

**Strengths**:

- Extremely fast (O(1) lookups)
- Simple API (GET/SET)
- Data structures (Redis: lists, sets, sorted sets)

**Weaknesses**:

- No queries (only key-based access)
- Limited transactions
- Value is opaque (no partial updates)

### Wide-Column Stores

**Model**: Rows with dynamic columns (column families)

**Examples**: Cassandra, HBase, ScyllaDB

**Use cases**: Time-series, IoT sensors, event logging

**Strengths**:

- Write-optimized (append-only)
- Linear scalability (petabyte scale)
- Time-series efficient

**Weaknesses**:

- Complex data modeling
- Limited query flexibility
- Eventual consistency

### Graph Databases

**Model**: Nodes and edges (relationships)

**Examples**: Neo4j, Amazon Neptune

**Use cases**: Social networks, recommendation engines, fraud detection

**Strengths**:

- Relationship queries (shortest path, pattern matching)
- Cypher query language
- ACID transactions

**Weaknesses**:

- Scaling challenges
- Limited to graph problems
- Specialized expertise required

## NoSQL Type Selection Matrix

| Data Model            | Access Pattern        | Choose        |
| --------------------- | --------------------- | ------------- |
| Flexible documents    | Rich queries          | **MongoDB**   |
| Simple key-value      | Fast cache/sessions   | **Redis**     |
| Time-series/events    | High write throughput | **Cassandra** |
| Complex relationships | Graph traversal       | **Neo4j**     |

## MongoDB (Document Store)

MongoDB stores JSON-like documents with flexible schema and rich query capabilities.

### Why Use MongoDB

**Strengths**:

- Flexible schema (add fields without migrations)
- Rich queries (filters, projections, aggregation)
- Horizontal scaling (sharding)
- ACID transactions (replica sets)
- Mature ecosystem (Atlas cloud, Compass GUI)

**Weaknesses**:

- No joins (denormalize or use `$lookup`)
- 16MB document size limit
- Memory-intensive (indexes in RAM)

**Use when**: Schema evolves frequently, need rich queries, documents are self-contained units.

### MongoDB Java Driver

**Maven dependency**:

```xml
<dependency>
    <groupId>org.mongodb</groupId>
    <artifactId>mongodb-driver-sync</artifactId>
    <version>5.2.1</version>
</dependency>
```

**Connection pattern**:

```java
import com.mongodb.client.*;
import org.bson.Document;

String connectionString = "mongodb://localhost:27017";
try (MongoClient mongoClient = MongoClients.create(connectionString)) {
    MongoDatabase database = mongoClient.getDatabase("myapp");
    MongoCollection<Document> collection = database.getCollection("users");

    // Perform operations
}
```

### CRUD Operations

**Insert document**:

```java
Document user = new Document("name", "Alice")
    .append("email", "alice@example.com")
    .append("age", 30)
    .append("tags", Arrays.asList("developer", "java"));

collection.insertOne(user);
System.out.println("Inserted with ID: " + user.getObjectId("_id"));
```

**Find documents**:

```java
// Find all
for (Document doc : collection.find()) {
    System.out.println(doc.toJson());
}

// Find with filter
Document filter = new Document("age", new Document("$gte", 25));
for (Document doc : collection.find(filter)) {
    System.out.println(doc.getString("name"));
}

// Find one
Document found = collection.find(Filters.eq("email", "alice@example.com"))
    .first();

if (found != null) {
    System.out.println("Found user: " + found.getString("name"));
}
```

**Update document**:

```java
import com.mongodb.client.model.Updates;
import com.mongodb.client.result.UpdateResult;

// Update one
UpdateResult result = collection.updateOne(
    Filters.eq("email", "alice@example.com"),
    Updates.combine(
        Updates.set("age", 31),
        Updates.addToSet("tags", "mongodb")
    )
);

System.out.println("Modified " + result.getModifiedCount() + " document(s)");

// Update many
collection.updateMany(
    Filters.lt("age", 18),
    Updates.set("status", "minor")
);
```

**Delete document**:

```java
import com.mongodb.client.result.DeleteResult;

// Delete one
DeleteResult result = collection.deleteOne(
    Filters.eq("email", "alice@example.com")
);

System.out.println("Deleted " + result.getDeletedCount() + " document(s)");

// Delete many
collection.deleteMany(Filters.eq("status", "inactive"));
```

### Query Filters and Projections

**Complex filters**:

```java
import com.mongodb.client.model.Filters;

// Comparison operators
collection.find(Filters.and(
    Filters.gte("age", 25),
    Filters.lt("age", 40)
));

// Logical operators
collection.find(Filters.or(
    Filters.eq("status", "active"),
    Filters.exists("premium", true)
));

// Array operators
collection.find(Filters.in("tags", "java", "python"));
collection.find(Filters.all("tags", Arrays.asList("java", "developer")));

// Text search
collection.createIndex(new Document("description", "text"));
collection.find(Filters.text("mongodb tutorial"));
```

**Projections** (select specific fields):

```java
import com.mongodb.client.model.Projections;

// Include specific fields
collection.find()
    .projection(Projections.fields(
        Projections.include("name", "email"),
        Projections.excludeId()
    ))
    .forEach(doc -> System.out.println(doc.toJson()));

// Exclude specific fields
collection.find()
    .projection(Projections.exclude("password", "ssn"))
    .forEach(doc -> System.out.println(doc.toJson()));
```

### Aggregation Pipeline

Aggregation pipeline processes documents through stages (filter → transform → group).

**Pattern**:

```java
import com.mongodb.client.model.Aggregates;
import com.mongodb.client.model.Accumulators;
import com.mongodb.client.model.Sorts;

// Count users by country, sorted descending
List<Document> pipeline = Arrays.asList(
    Aggregates.match(Filters.eq("status", "active")),
    Aggregates.group("$country", Accumulators.sum("count", 1)),
    Aggregates.sort(Sorts.descending("count")),
    Aggregates.limit(10)
);

collection.aggregate(pipeline)
    .forEach(doc -> {
        System.out.println(doc.getString("_id") + ": " + doc.getInteger("count"));
    });
```

**Average age by department**:

```java
List<Document> pipeline = Arrays.asList(
    Aggregates.group("$department",
        Accumulators.avg("avgAge", "$age"),
        Accumulators.sum("count", 1)
    )
);

collection.aggregate(pipeline)
    .forEach(doc -> {
        System.out.printf("%s: %.1f avg age (%d users)%n",
            doc.getString("_id"),
            doc.getDouble("avgAge"),
            doc.getInteger("count"));
    });
```

### Indexing Strategies

**Create indexes** for frequently queried fields:

```java
import com.mongodb.client.model.Indexes;
import com.mongodb.client.model.IndexOptions;

// Single field index
collection.createIndex(Indexes.ascending("email"));

// Compound index (multiple fields)
collection.createIndex(Indexes.compoundIndex(
    Indexes.ascending("country"),
    Indexes.descending("age")
));

// Unique index
collection.createIndex(
    Indexes.ascending("username"),
    new IndexOptions().unique(true)
);

// Text index for search
collection.createIndex(Indexes.text("description"));

// TTL index (auto-delete old documents)
collection.createIndex(
    Indexes.ascending("createdAt"),
    new IndexOptions().expireAfter(30L, TimeUnit.DAYS)
);
```

**Performance**:

- Without index: O(n) collection scan
- With index: O(log n) B-tree lookup

**Trade-off**: Indexes speed up reads but slow down writes (must update index).

## Redis (Key-Value Store)

Redis is an in-memory data structure store supporting strings, hashes, lists, sets, and sorted sets.

### Why Use Redis

**Strengths**:

- Extremely fast (in-memory, ~100k ops/sec)
- Rich data structures (not just key-value)
- Pub/Sub messaging
- Persistence options (RDB snapshots, AOF logs)
- Atomic operations

**Weaknesses**:

- Limited by RAM (must fit in memory)
- Single-threaded (CPU-bound for complex operations)
- No query language (key-based access only)

**Use when**: Need caching, session storage, real-time leaderboards, rate limiting, pub/sub messaging.

### Redis Java Clients

**Jedis** (simple, synchronous):

```xml
<dependency>
    <groupId>redis.clients</groupId>
    <artifactId>jedis</artifactId>
    <version>5.2.0</version>
</dependency>
```

**Lettuce** (async, thread-safe):

```xml
<dependency>
    <groupId>io.lettuce</groupId>
    <artifactId>lettuce-core</artifactId>
    <version>6.4.0.RELEASE</version>
</dependency>
```

**Recommendation**: Use Lettuce for production (thread-safe, reactive support).

### String Operations (Jedis)

```java
import redis.clients.jedis.*;

try (Jedis jedis = new Jedis("localhost", 6379)) {
    // Set/Get
    jedis.set("user:1000:name", "Alice");
    String name = jedis.get("user:1000:name");
    System.out.println("Name: " + name);  // Output: Name: Alice

    // Set with expiration (TTL)
    jedis.setex("session:abc123", 3600, "user-data");  // Expires in 1 hour

    // Increment counter
    jedis.incr("page:views");
    Long views = jedis.incrBy("page:views", 5);

    // Check existence
    boolean exists = jedis.exists("user:1000:name");

    // Delete
    jedis.del("session:abc123");
}
```

### Hash Operations (Object Storage)

Hashes map field names to values (like Java Map).

```java
// Store user as hash
Map<String, String> user = Map.of(
    "name", "Alice",
    "email", "alice@example.com",
    "age", "30"
);

jedis.hset("user:1000", user);

// Get all fields
Map<String, String> userData = jedis.hgetAll("user:1000");
System.out.println(userData);

// Get specific field
String email = jedis.hget("user:1000", "email");

// Increment numeric field
jedis.hincrBy("user:1000", "loginCount", 1);

// Get multiple fields
List<String> values = jedis.hmget("user:1000", "name", "email");
```

### List Operations (Queues/Stacks)

Lists are ordered collections (doubly-linked lists).

```java
// Push to list (queue)
jedis.rpush("tasks", "task1", "task2", "task3");

// Pop from list (FIFO queue)
String task = jedis.lpop("tasks");  // Returns "task1"

// Stack (LIFO)
jedis.rpush("stack", "item1", "item2");
String top = jedis.rpop("stack");  // Returns "item2"

// Get range
List<String> allTasks = jedis.lrange("tasks", 0, -1);

// List length
Long length = jedis.llen("tasks");

// Blocking pop (wait for items)
List<String> item = jedis.blpop(5, "tasks");  // Wait 5 seconds
```

**Use cases**: Task queues, activity feeds, recent items.

### Set Operations (Unique Collections)

Sets store unique unordered values.

```java
// Add members
jedis.sadd("user:1000:tags", "developer", "java", "mongodb");

// Check membership
boolean isMember = jedis.sismember("user:1000:tags", "java");

// Get all members
Set<String> tags = jedis.smembers("user:1000:tags");

// Remove member
jedis.srem("user:1000:tags", "mongodb");

// Set operations
jedis.sadd("user:1000:skills", "java", "python");
jedis.sadd("user:2000:skills", "python", "go");

// Intersection (common skills)
Set<String> common = jedis.sinter("user:1000:skills", "user:2000:skills");

// Union (all skills)
Set<String> all = jedis.sunion("user:1000:skills", "user:2000:skills");

// Difference
Set<String> unique = jedis.sdiff("user:1000:skills", "user:2000:skills");
```

**Use cases**: Tags, relationships, uniqueness constraints.

### Sorted Set Operations (Leaderboards)

Sorted sets store members with scores for ranking.

```java
// Add members with scores
jedis.zadd("leaderboard", 100, "Alice");
jedis.zadd("leaderboard", 95, "Bob");
jedis.zadd("leaderboard", 120, "Charlie");

// Get top 3 (descending)
List<String> top3 = jedis.zrevrange("leaderboard", 0, 2);
// Output: [Charlie, Alice, Bob]

// Get range with scores
List<Tuple> topWithScores = jedis.zrevrangeWithScores("leaderboard", 0, 2);
for (Tuple t : topWithScores) {
    System.out.println(t.getElement() + ": " + t.getScore());
}

// Get rank (0-based)
Long rank = jedis.zrevrank("leaderboard", "Alice");  // Returns 1 (second place)

// Increment score
jedis.zincrby("leaderboard", 10, "Bob");

// Get score
Double score = jedis.zscore("leaderboard", "Alice");

// Count in range
Long count = jedis.zcount("leaderboard", 90, 110);
```

**Use cases**: Leaderboards, priority queues, time-series data.

### Pub/Sub Messaging

Redis supports publish/subscribe messaging pattern.

**Publisher**:

```java
try (Jedis jedis = new Jedis("localhost", 6379)) {
    jedis.publish("notifications", "New order received");
    jedis.publish("notifications", "Payment processed");
}
```

**Subscriber**:

```java
import redis.clients.jedis.JedisPubSub;

JedisPubSub subscriber = new JedisPubSub() {
    @Override
    public void onMessage(String channel, String message) {
        System.out.println("Received on " + channel + ": " + message);
    }

    @Override
    public void onSubscribe(String channel, int subscribedChannels) {
        System.out.println("Subscribed to " + channel);
    }
};

try (Jedis jedis = new Jedis("localhost", 6379)) {
    jedis.subscribe(subscriber, "notifications");  // Blocks
}
```

**Pattern matching**:

```java
// Subscribe to pattern
jedis.psubscribe(subscriber, "user:*:notifications");
```

### Redis Transactions

Redis transactions execute commands atomically with MULTI/EXEC.

```java
Transaction tx = jedis.multi();

tx.set("key1", "value1");
tx.incr("counter");
tx.hset("user:1000", "status", "active");

List<Object> results = tx.exec();  // Execute atomically
```

**Watch** (optimistic locking):

```java
String key = "balance";

jedis.watch(key);  // Watch for changes

Integer balance = Integer.parseInt(jedis.get(key));
if (balance < 100) {
    jedis.unwatch();
    System.out.println("Insufficient balance");
    return;
}

Transaction tx = jedis.multi();
tx.decrBy(key, 100);
List<Object> result = tx.exec();  // Fails if key changed

if (result == null) {
    System.out.println("Transaction failed (key modified)");
}
```

### Lua Scripting

Execute complex operations atomically with Lua scripts.

```java
String script =
    "local current = redis.call('get', KEYS[1]) " +
    "if current and tonumber(current) > tonumber(ARGV[1]) then " +
    "    redis.call('decrby', KEYS[1], ARGV[1]) " +
    "    return 1 " +
    "else " +
    "    return 0 " +
    "end";

Object result = jedis.eval(script, 1, "balance", "100");

if (result.equals(1L)) {
    System.out.println("Deducted 100 from balance");
} else {
    System.out.println("Insufficient balance");
}
```

### Redis Persistence

**RDB (Snapshots)**:

- Periodic point-in-time snapshots
- Fast recovery, compact files
- Data loss possible between snapshots

**AOF (Append-Only File)**:

- Logs every write operation
- Minimal data loss (configurable)
- Larger files, slower recovery

**Configuration** (redis.conf):

```
# RDB
save 900 1        # Save after 900s if 1 key changed
save 300 10       # Save after 300s if 10 keys changed

# AOF
appendonly yes
appendfsync everysec   # Sync every second
```

**Recommendation**: Use both RDB + AOF for best durability.

### Redis Cluster vs Sentinel

**Redis Sentinel** (high availability):

- Automatic failover (promote replica on master failure)
- Monitoring and notifications
- Single master + multiple replicas

**Redis Cluster** (horizontal scaling):

- Automatic sharding across nodes
- Multi-master architecture
- 16384 hash slots distributed across nodes

**Choose Sentinel when**: Need high availability, single master sufficient

**Choose Cluster when**: Dataset exceeds single server RAM, need write scalability

## Cassandra (Wide-Column Store)

Cassandra is a distributed wide-column store optimized for write-heavy workloads and linear scalability.

### Why Use Cassandra

**Strengths**:

- Linear scalability (add nodes, increase throughput)
- High write throughput (append-only log)
- No single point of failure (masterless architecture)
- Tunable consistency (ONE, QUORUM, ALL)
- Time-series efficient

**Weaknesses**:

- Complex data modeling (denormalize for queries)
- Limited query flexibility (no joins, limited WHERE)
- Eventual consistency (by default)
- Learning curve (different from SQL)

**Use when**: High write volume, linear scalability needed, time-series data, IoT sensors.

### DataStax Java Driver

**Maven dependency**:

```xml
<dependency>
    <groupId>com.datastax.oss</groupId>
    <artifactId>java-driver-core</artifactId>
    <version>4.18.1</version>
</dependency>
```

**Connection pattern**:

```java
import com.datastax.oss.driver.api.core.CqlSession;
import com.datastax.oss.driver.api.core.cql.*;

try (CqlSession session = CqlSession.builder()
        .withKeyspace("myapp")
        .build()) {

    // Execute queries
}
```

### CQL (Cassandra Query Language)

CQL looks like SQL but has different semantics.

**Create keyspace** (database):

```java
session.execute(
    "CREATE KEYSPACE IF NOT EXISTS myapp " +
    "WITH replication = {'class': 'SimpleStrategy', 'replication_factor': 3}"
);
```

**Create table**:

```java
session.execute(
    "CREATE TABLE IF NOT EXISTS users (" +
    "    id uuid PRIMARY KEY," +
    "    name text," +
    "    email text," +
    "    created_at timestamp" +
    ")"
);

// Index for secondary queries
session.execute("CREATE INDEX ON users (email)");
```

### Partition Keys and Clustering Columns

**Partition key** determines data distribution across nodes.

**Clustering columns** determine sort order within partition.

**Pattern**:

```java
// Time-series table
session.execute(
    "CREATE TABLE sensor_data (" +
    "    sensor_id text," +          // Partition key
    "    timestamp timestamp," +     // Clustering column
    "    temperature double," +
    "    PRIMARY KEY (sensor_id, timestamp)" +
    ") WITH CLUSTERING ORDER BY (timestamp DESC)"
);
```

**Key insight**: All queries MUST include partition key for efficiency.

### CRUD Operations

**Insert data**:

```java
PreparedStatement prepared = session.prepare(
    "INSERT INTO users (id, name, email, created_at) VALUES (?, ?, ?, ?)"
);

BoundStatement bound = prepared.bind(
    UUID.randomUUID(),
    "Alice",
    "alice@example.com",
    Instant.now()
);

session.execute(bound);
```

**Query data**:

```java
// Query by partition key
ResultSet rs = session.execute(
    "SELECT * FROM users WHERE id = ?",
    UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
);

for (Row row : rs) {
    System.out.println(row.getString("name") + ": " + row.getString("email"));
}

// Query with clustering column
rs = session.execute(
    "SELECT * FROM sensor_data WHERE sensor_id = ? AND timestamp > ?",
    "sensor-001",
    Instant.now().minus(1, ChronoUnit.HOURS)
);
```

**Update data**:

```java
session.execute(
    "UPDATE users SET email = ? WHERE id = ?",
    "newemail@example.com",
    userId
);

// Counter column (atomic increment)
session.execute(
    "UPDATE page_views SET count = count + 1 WHERE page_id = ?",
    "home"
);
```

**Delete data**:

```java
// Delete row
session.execute("DELETE FROM users WHERE id = ?", userId);

// Delete column
session.execute("DELETE email FROM users WHERE id = ?", userId);

// TTL (time-to-live) delete
session.execute(
    "INSERT INTO sessions (id, data) VALUES (?, ?) USING TTL 3600",
    sessionId,
    sessionData
);
```

### Tunable Consistency

Cassandra allows per-query consistency levels.

**Consistency levels**:

- **ONE**: Single replica (fastest, least consistent)
- **QUORUM**: Majority of replicas (balanced)
- **ALL**: All replicas (slowest, most consistent)
- **LOCAL_QUORUM**: Quorum in local datacenter

**Pattern**:

```java
SimpleStatement statement = SimpleStatement.builder(
    "SELECT * FROM users WHERE id = ?"
)
.setConsistencyLevel(ConsistencyLevel.QUORUM)
.build();

ResultSet rs = session.execute(statement.bind(userId));
```

**Trade-off**: Higher consistency → slower queries, lower availability

**Recommendation**: Use QUORUM for balanced consistency/performance.

### Write Path and Read Path

**Write path** (why writes are fast):

1. Write to commit log (sequential disk write)
2. Write to memtable (in-memory)
3. Return success immediately
4. Background flush to SSTable (sorted string table)

**Read path**:

1. Check memtable
2. Check row cache
3. Check bloom filters (avoid disk reads)
4. Read SSTables, merge results

**Key insight**: Writes never read disk (append-only), reads may hit multiple SSTables.

### When to Use Cassandra

**Use Cassandra for**:

- Time-series data (sensor readings, logs)
- High write throughput (millions/sec per node)
- Linear scalability (petabyte scale)
- Multi-datacenter replication
- Always-on availability (no single point of failure)

**Avoid Cassandra for**:

- Complex JOINs (not supported)
- Ad-hoc queries (limited WHERE clauses)
- Strong consistency requirements (ACID transactions)
- Small datasets (operational overhead)

## Spring Data NoSQL

Spring Data provides consistent abstractions over NoSQL databases.

### Spring Data MongoDB

**Maven dependency**:

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-mongodb</artifactId>
</dependency>
```

**Entity mapping**:

```java
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.annotation.Id;

@Document(collection = "users")
public class User {
    @Id
    private String id;
    private String name;
    private String email;
    private List<String> tags;

    // Getters/setters
}
```

**Repository interface**:

```java
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;

public interface UserRepository extends MongoRepository<User, String> {
    // Query derivation from method name
    List<User> findByName(String name);
    List<User> findByEmailContaining(String domain);
    List<User> findByTagsContaining(String tag);

    // Custom query
    @Query("{ 'age' : { $gte: ?0, $lte: ?1 } }")
    List<User> findByAgeBetween(int minAge, int maxAge);
}
```

**MongoTemplate** (lower-level access):

```java
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Criteria;

@Service
public class UserService {
    @Autowired
    private MongoTemplate mongoTemplate;

    public List<User> findActiveUsers() {
        Query query = new Query();
        query.addCriteria(Criteria.where("status").is("active"));
        return mongoTemplate.find(query, User.class);
    }
}
```

### Spring Data Redis

**Maven dependency**:

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-redis</artifactId>
</dependency>
```

**Entity mapping**:

```java
import org.springframework.data.redis.core.RedisHash;
import org.springframework.data.annotation.Id;

@RedisHash("users")
public class User {
    @Id
    private String id;
    private String name;
    private String email;

    // Getters/setters
}
```

**Repository interface**:

```java
import org.springframework.data.repository.CrudRepository;

public interface UserRepository extends CrudRepository<User, String> {
    // Inherits save(), findById(), findAll(), delete()
}
```

**RedisTemplate** (for data structures):

```java
import org.springframework.data.redis.core.RedisTemplate;

@Service
public class CacheService {
    @Autowired
    private RedisTemplate<String, String> redisTemplate;

    public void cacheValue(String key, String value, Duration ttl) {
        redisTemplate.opsForValue().set(key, value, ttl);
    }

    public void addToSet(String key, String value) {
        redisTemplate.opsForSet().add(key, value);
    }

    public void incrementCounter(String key) {
        redisTemplate.opsForValue().increment(key);
    }
}
```

### Spring Data Cassandra

**Maven dependency**:

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-cassandra</artifactId>
</dependency>
```

**Entity mapping**:

```java
import org.springframework.data.cassandra.core.mapping.Table;
import org.springframework.data.cassandra.core.mapping.PrimaryKey;

@Table("users")
public class User {
    @PrimaryKey
    private UUID id;
    private String name;
    private String email;

    // Getters/setters
}
```

**Repository interface**:

```java
import org.springframework.data.cassandra.repository.CassandraRepository;

public interface UserRepository extends CassandraRepository<User, UUID> {
    List<User> findByEmail(String email);
}
```

## Data Modeling Patterns

### Document Modeling (Embedding vs Referencing)

**Embedding** (denormalize):

```java
// User document with embedded addresses
{
    "_id": ObjectId("..."),
    "name": "Alice",
    "addresses": [
        { "type": "home", "street": "123 Main St", "city": "NYC" },
        { "type": "work", "street": "456 Office Blvd", "city": "SF" }
    ]
}
```

**Referencing** (normalize):

```java
// User document
{ "_id": ObjectId("user1"), "name": "Alice" }

// Address documents
{ "_id": ObjectId("addr1"), "userId": ObjectId("user1"), "type": "home", ... }
{ "_id": ObjectId("addr2"), "userId": ObjectId("user1"), "type": "work", ... }
```

**When to embed**:

- One-to-few relationships
- Data accessed together
- No independent access needed

**When to reference**:

- One-to-many or many-to-many
- Large subdocuments (approaching 16MB limit)
- Independent access patterns

### Denormalization Strategies

**Problem**: NoSQL databases have no joins - must denormalize data.

**Pattern**: Duplicate data to avoid lookups.

**Example** (e-commerce orders):

```java
// Order document with denormalized user data
{
    "_id": ObjectId("order1"),
    "orderId": "ORD-123",
    "user": {                    // Denormalized user data
        "userId": ObjectId("user1"),
        "name": "Alice",
        "email": "alice@example.com"
    },
    "items": [
        {
            "productId": ObjectId("prod1"),
            "name": "Widget",    // Denormalized product name
            "price": 29.99,      // Denormalized price (at time of order)
            "quantity": 2
        }
    ],
    "total": 59.98,
    "status": "shipped"
}
```

**Trade-off**: Faster reads (no joins), but must handle stale data (user changes email).

### Time-Series Data Patterns

**Cassandra pattern** (bucketing by time):

```java
CREATE TABLE sensor_readings (
    sensor_id text,
    bucket text,              // "2025-02-04" (daily bucket)
    timestamp timestamp,
    temperature double,
    PRIMARY KEY ((sensor_id, bucket), timestamp)
) WITH CLUSTERING ORDER BY (timestamp DESC);
```

**Query recent data**:

```java
String bucket = LocalDate.now().toString();
session.execute(
    "SELECT * FROM sensor_readings WHERE sensor_id = ? AND bucket = ? LIMIT 100",
    "sensor-001",
    bucket
);
```

**MongoDB pattern** (time-series collections)\*\*:

```java
db.createCollection("sensor_data", {
    timeseries: {
        timeField: "timestamp",
        metaField: "sensorId",
        granularity: "seconds"
    }
});
```

### Aggregation Patterns

**MongoDB aggregation** (pre-aggregate for performance):

```java
// Real-time aggregation (slower)
collection.aggregate(Arrays.asList(
    Aggregates.match(Filters.eq("status", "active")),
    Aggregates.group("$country", Accumulators.sum("count", 1))
));

// Pre-aggregated collection (faster)
// Background job: Aggregate hourly/daily into summary collection
{
    "_id": "2025-02-04:USA",
    "date": "2025-02-04",
    "country": "USA",
    "activeUsers": 12500,
    "newUsers": 150
}
```

**Pattern**: Pre-aggregate data in background jobs for fast dashboard queries.

## Best Practices

### 1. Choose Appropriate NoSQL Type

Match database to data model and access pattern.

**Before**: Using MongoDB for simple caching
**After**: Use Redis for caching (10x faster)

### 2. Understand Consistency Models

NoSQL databases often use eventual consistency.

**Pattern**: Design application to handle stale data.

```java
// Redis cache with fallback to database
String cached = jedis.get("user:" + userId);
if (cached == null) {
    User user = database.findById(userId);
    jedis.setex("user:" + userId, 300, serialize(user));
    return user;
}
return deserialize(cached);
```

### 3. Index Frequently Queried Fields

Add indexes for performance.

**MongoDB**:

```java
collection.createIndex(Indexes.ascending("email"));
collection.createIndex(Indexes.compound(
    Indexes.ascending("country"),
    Indexes.descending("createdAt")
));
```

**Cassandra**: Design schema so partition key matches query pattern.

### 4. Monitor Database Performance

Track slow queries and resource usage.

**MongoDB profiler**:

```java
database.runCommand(new Document("profile", 1).append("slowms", 100));
```

**Redis**: Use `MONITOR` command (development only).

**Cassandra**: Use `nodetool` for cluster health.

### 5. Plan for Schema Evolution

NoSQL schemas evolve over time.

**Pattern**: Version documents.

```java
{
    "_id": ObjectId("..."),
    "_version": 2,           // Schema version
    "name": "Alice",
    "email": "alice@example.com",
    "phoneNumbers": [...]    // New field in v2
}
```

**Handle multiple versions** in code:

```java
if (doc.getInteger("_version") == 1) {
    // Migrate to v2 on read
    doc.put("phoneNumbers", new ArrayList<>());
    doc.put("_version", 2);
    collection.replaceOne(Filters.eq("_id", doc.getObjectId("_id")), doc);
}
```

### 6. Use Connection Pooling

Configure connection pools for production.

**MongoDB**:

```java
MongoClientSettings settings = MongoClientSettings.builder()
    .applyConnectionString(new ConnectionString(connectionString))
    .applyToConnectionPoolSettings(builder ->
        builder.maxSize(20)
               .minSize(5)
               .maxWaitTime(30, TimeUnit.SECONDS))
    .build();
```

**Redis** (Jedis pool):

```java
JedisPoolConfig poolConfig = new JedisPoolConfig();
poolConfig.setMaxTotal(20);
poolConfig.setMaxIdle(10);
poolConfig.setMinIdle(5);

JedisPool pool = new JedisPool(poolConfig, "localhost", 6379);

try (Jedis jedis = pool.getResource()) {
    // Use connection
}
```

### 7. Backup and Disaster Recovery

Plan for data loss scenarios.

**MongoDB**: Use `mongodump`/`mongorestore` or continuous backups (Atlas).

**Redis**: Configure RDB + AOF persistence, backup RDB files.

**Cassandra**: Use `nodetool snapshot` for backups.

## Related Content

### Core Java Topics

- **[Working with SQL Databases](/en/learn/software-engineering/programming-languages/java/in-the-field/sql-database)** - SQL vs NoSQL comparison
- **[Caching Strategies](/en/learn/software-engineering/programming-languages/java/in-the-field/caching)** - Redis caching patterns
- **[Performance Optimization](/en/learn/software-engineering/programming-languages/java/in-the-field/performance)** - Indexing and optimization

### External Resources

**MongoDB**:

- [MongoDB Java Driver](https://mongodb.github.io/mongo-java-driver/) - Official driver documentation
- [MongoDB University](https://university.mongodb.com/) - Free courses
- [MongoDB Atlas](https://www.mongodb.com/cloud/atlas) - Cloud-hosted MongoDB

**Redis**:

- [Redis Documentation](https://redis.io/documentation) - Official Redis docs
- [Jedis GitHub](https://github.com/redis/jedis) - Jedis client
- [Lettuce Documentation](https://lettuce.io/) - Lettuce client (async)

**Cassandra**:

- [DataStax Java Driver](https://docs.datastax.com/en/developer/java-driver/latest/) - Official driver
- [Cassandra Academy](https://www.datastax.com/dev) - Free DataStax courses
- [Apache Cassandra](https://cassandra.apache.org/) - Official project site

**Spring Data**:

- [Spring Data MongoDB](https://spring.io/projects/spring-data-mongodb) - Spring integration
- [Spring Data Redis](https://spring.io/projects/spring-data-redis) - Spring Redis integration
- [Spring Data Cassandra](https://spring.io/projects/spring-data-cassandra) - Spring Cassandra integration

---

**Last Updated**: 2026-02-04
**Java Version**: 17+ (baseline), 21+ (recommended)
**Library Versions**: MongoDB Driver 5.2.1, Jedis 5.2.0, Lettuce 6.4.0, Cassandra Driver 4.18.1
