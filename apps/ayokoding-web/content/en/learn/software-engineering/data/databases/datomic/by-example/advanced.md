---
title: "Advanced"
date: 2026-01-29T00:00:00+07:00
draft: false
weight: 10000003
description: "Examples 61-80: Advanced Datomic patterns covering client API, excision, performance tuning, distributed systems, and expert techniques (75-95% coverage)"
tags: ["datomic", "database", "tutorial", "by-example", "advanced", "expert", "datalog", "clojure"]
---

Master advanced Datomic with 20 expert-level examples. Explore client API, excision, performance tuning, distributed patterns, and production deployment strategies.

## Example 61: Client API for Remote Access

The client API provides remote database access via HTTP or gRPC. Lighter-weight than peer library for microservices.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Application<br/>Client API"]
    B["Datomic Cloud<br/>or Peer Server"]
    C["Storage<br/>(DynamoDB, etc)"]
    D["Transactor<br/>(writes)"]

    A -->|HTTP/gRPC| B
    B -->|Reads| C
    B -->|Writes| D
    D -->|Commits| C

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
```

**Java Code**:

```java
// Note: Client API requires Datomic Cloud or Peer Server
// For Datomic Free, use peer library as shown in earlier examples

// Connect via client API (Datomic Cloud example)
import datomic.client.api.Client;
import datomic.client.api.Datomic;

Client client = Datomic.clientCloud(
    Util.map("server-type", "cloud",
             "region", "us-east-1",
             "system", "my-system",
             "endpoint", "https://...")
);
// => Creates client connection to Datomic Cloud

Connection conn = client.connect(Util.map("db-name", "tutorial"));
// => Connect to specific database

// Query via client API (same datalog)
Collection results = client.q(
    Util.map("query", "[:find ?name :where [?e :person/name ?name]]",
             "args", Util.list(client.db(conn)))
);
// => Client API uses same query language as peer
// => Queries execute remotely on peer server or Cloud

// Transaction via client API
Map txResult = client.transact(
    conn,
    Util.map("tx-data", Util.list(
        Util.map(":person/name", "Remote User",
                 ":person/email", "remote@example.com")
    ))
);
// => :tx-data key wraps transaction data
// => Returns transaction result map
```

**Clojure Code**:

```clojure
;; Note: Client API requires Datomic Cloud or Peer Server
;; For Datomic Free, use peer library as shown in earlier examples

;; Connect via client API (Datomic Cloud example)
(require '[datomic.client.api :as d-client])

(def client (d-client/client {:server-type :cloud
                               :region "us-east-1"
                               :system "my-system"
                               :endpoint "https://..."}))
;; => Creates client connection to Datomic Cloud

(def conn (d-client/connect client {:db-name "tutorial"}))
;; => Connect to specific database

;; Query via client API (same datalog)
(d-client/q '[:find ?name
              :where [?e :person/name ?name]]
            (d-client/db conn))
;; => Client API uses same query language as peer
;; => Queries execute remotely on peer server or Cloud

;; Transaction via client API
(d-client/transact conn
  {:tx-data [{:person/name "Remote User"
              :person/email "remote@example.com"}]})
;; => :tx-data key wraps transaction data
;; => Returns transaction result map
```

**Key Takeaway**: Client API provides remote access to Datomic Cloud/Peer Server. Same query and transaction semantics as peer library, lighter resource footprint.

---

## Example 62: Excision for Data Deletion (GDPR Compliance)

Excision permanently removes data from database history. Use for legal requirements like GDPR "right to be forgotten".

**Java Code**:

```java
// Note: Excision requires Datomic Pro or Cloud
// Datomic Free doesn't support excision

// Mark entity for excision (Datomic Pro/Cloud example)
// WARNING: Excision is irreversible - data permanently deleted

// Query entity to excise
Object entityToExcise = Peer.q(
    "[:find ?e . " +
    " :where [?e :person/email \"excise-me@example.com\"]]",
    db
);

// Submit excision request (Datomic Cloud API with client)
// client.transact(
//     conn,
//     Util.map("tx-data", Util.list(
//         Util.list(":db/excise", entityToExcise)
//     ))
// );
// => Schedules excision of entity
// => Background process removes all datoms for this entity from history
// => Cannot be undone - use with extreme caution

// Alternative: Soft delete for Datomic Free
conn.transact(
    Util.list(
        Util.map(":db/ident", ":person/deleted",
                 ":db/valueType", ":db.type/boolean",
                 ":db/cardinality", ":db.cardinality/one")
    )
).get();

conn.transact(
    Util.list(
        Util.map(":db/id", Util.list(":person/email", "alice@example.com"),
                 ":person/deleted", true)
    )
).get();
// => Mark as deleted instead of excising
// => Filter deleted entities in queries
```

**Clojure Code**:

```clojure
;; Note: Excision requires Datomic Pro or Cloud
;; Datomic Free doesn't support excision

;; Mark entity for excision (Datomic Pro/Cloud example)
;; WARNING: Excision is irreversible - data permanently deleted

;; Query entity to excise
(def entity-to-excise
  (d/q '[:find ?e .
         :where [?e :person/email "excise-me@example.com"]]
       db))

;; Submit excision request (Datomic Cloud API)
(d-client/transact conn
  {:tx-data [[:db/excise entity-to-excise]]})
;; => Schedules excision of entity
;; => Background process removes all datoms for this entity from history
;; => Cannot be undone - use with extreme caution

;; Alternative: Soft delete for Datomic Free
@(d/transact conn
   [{:db/ident       :person/deleted
     :db/valueType   :db.type/boolean
     :db/cardinality :db.cardinality/one}])

@(d/transact conn
   [{:db/id [:person/email "alice@example.com"]
     :person/deleted true}])
;; => Mark as deleted instead of excising
;; => Filter deleted entities in queries
```

**Key Takeaway**: Excision (Pro/Cloud) permanently removes data from history. Use for GDPR compliance. Datomic Free uses soft deletes with boolean flags.

---

## Example 63: Analytical Queries with Aggregates

Combine multiple aggregates and grouping for analytical workloads.

**Java Code**:

```java
// Statistics by tenant
Collection results = Peer.q(
    "[:find ?tenant (count ?e) (avg ?age) (min ?age) (max ?age) " +
    " :where [?e :person/tenant ?tenant] " +
    "        [?e :person/age ?age]]",
    db
);
// => [["acme" 15 32.5 22 55]
//     ["globex" 12 29.3 21 48]]
// => Returns tenant, count, average age, min age, max age

// Age distribution (histogram bins)
Collection results2 = Peer.q(
    "[:find ?age-bin (count ?e) " +
    " :where [?e :person/age ?age] " +
    "        [(quot ?age 10) ?age-bin]]",
    db
);
// => [[2 450] [3 380] [4 178]]
// => Groups by decade: 20s, 30s, 40s

// Custom aggregate: percentile (implement as Java function)
public static class Percentile implements clojure.lang.IFn {
    private double p;
    public Percentile(double p) { this.p = p; }
    public Object invoke(Object vals) {
        List<Number> numbers = new ArrayList<>((Collection<Number>) vals);
        Collections.sort(numbers, Comparator.comparingDouble(Number::doubleValue));
        int idx = (int) (p * (numbers.size() - 1));
        return numbers.get(idx);
    }
}

// Use in query (requires registering function)
// Collection results3 = Peer.q(
//     "[:find (my.ns.Percentile/invoke 0.95 ?age) " +
//     " :where [?e :person/age ?age]]",
//     db
// );
// => [[52]]
// => 95th percentile age
```

**Clojure Code**:

```clojure
;; Statistics by tenant
(d/q '[:find ?tenant (count ?e) (avg ?age) (min ?age) (max ?age)
       :where [?e :person/tenant ?tenant]
              [?e :person/age ?age]]
     db)
;; => [["acme" 15 32.5 22 55]
;;     ["globex" 12 29.3 21 48]]
;; => Returns tenant, count, average age, min age, max age

;; Age distribution (histogram bins)
(d/q '[:find ?age-bin (count ?e)
       :where [?e :person/age ?age]
              [(quot ?age 10) ?age-bin]]
     db)
;; => [[2 450] [3 380] [4 178]]
;; => Groups by decade: 20s, 30s, 40s

;; Custom aggregate: percentile
(defn percentile [p vals]
  (let [sorted (sort vals)
        idx (int (* p (dec (count vals))))]
    (nth sorted idx)))

(d/q '[:find (my-ns/percentile 0.95 ?age)
       :where [?e :person/age ?age]]
     db)
;; => [[52]]
;; => 95th percentile age
```

**Key Takeaway**: Combine built-in and custom aggregates for analytics. Group by computed expressions for histograms and distribution analysis.

---

## Example 64: Streaming Large Result Sets

Process large query results as lazy sequences to avoid memory issues.

**Java Code**:

```java
// Query returns Collection (not lazy in Java, but can be streamed)
Collection allPeople = Peer.q(
    "[:find ?e ?name " +
    " :where [?e :person/name ?name]]",
    db
);
// => Collection of results

// Process in chunks
int chunkSize = 100;
List<Object> chunk = new ArrayList<>();
Iterator it = allPeople.iterator();
while (it.hasNext()) {
    chunk.add(it.next());
    if (chunk.size() >= chunkSize) {
        System.out.println("Processing " + chunk.size() + " people");
        // Process chunk
        for (Object item : chunk) {
            // Do something with each person
        }
        chunk.clear();
    }
}
// Process remaining
if (!chunk.isEmpty()) {
    System.out.println("Processing " + chunk.size() + " people");
}
// => Processes 100 entities at a time
// => Keeps memory usage bounded

// Transform with streams (Java 8+)
allPeople.stream()
    .map(tuple -> ((List) tuple).get(0))  // Get entity ID
    .map(e -> db.entity(e))                // Convert to entity
    .filter(entity -> ((Integer) entity.get(":person/age")) > 30)
    .limit(10)
    .collect(Collectors.toList());
// => Stream transformation pipeline
// => Only processes first 10 matching results
```

**Clojure Code**:

```clojure
;; Query returns lazy sequence
(def all-people
  (d/q '[:find ?e ?name
         :where [?e :person/name ?name]]
       db))
;; => Lazy sequence - not fully realized in memory

;; Process in chunks
(doseq [chunk (partition-all 100 all-people)]
  (println "Processing" (count chunk) "people")
  ;; Process chunk
  (doseq [[e name] chunk]
    ;; Do something with each person
    nil))
;; => Processes 100 entities at a time
;; => Keeps memory usage constant

;; Transform lazily
(->> (d/q '[:find ?e :where [?e :person/name]] db)
     (map first)
     (map (partial d/entity db))
     (filter #(> (:person/age % 0) 30))
     (take 10))
;; => Lazy transformation pipeline
;; => Only realizes 10 results, not entire dataset
```

**Key Takeaway**: Query results are lazy sequences. Use `partition-all`, lazy transformations, and `take` to process large datasets with constant memory.

---

## Example 65: Custom Index for Query Optimization

Build materialized views or custom indexes for frequently-accessed patterns.

**Java Code**:

```java
// Materialize age-to-people index in Map
Map<Integer, List<Object>> ageIndex = new ConcurrentHashMap<>();

void rebuildAgeIndex(Database db) {
    ageIndex.clear();
    Collection results = Peer.q(
        "[:find ?e ?age " +
        " :where [?e :person/age ?age]]",
        db
    );
    for (Object obj : results) {
        List tuple = (List) obj;
        Object entityId = tuple.get(0);
        Integer age = (Integer) tuple.get(1);
        ageIndex.computeIfAbsent(age, k -> new ArrayList<>()).add(entityId);
    }
}

rebuildAgeIndex(db);

// Query custom index (O(1) lookup)
List<Object> people30 = ageIndex.get(30);
// => [entity-id-1, entity-id-2, ...]
// => Instant lookup vs query scan

// Update index incrementally after transactions
void updateAgeIndex(Collection txData) {
    for (Object obj : txData) {
        datomic.Datom datom = (datomic.Datom) obj;
        if (datom.a().equals(db.entid(":person/age"))) {
            Integer age = (Integer) datom.v();
            Object entityId = datom.e();
            if ((Boolean) datom.added()) {
                ageIndex.computeIfAbsent(age, k -> new ArrayList<>()).add(entityId);
            } else {
                List<Object> entities = ageIndex.get(age);
                if (entities != null) {
                    entities.remove(entityId);
                }
            }
        }
    }
}
```

**Clojure Code**:

```clojure
;; Materialize age-to-people index in atom
(def age-index (atom {}))

(defn rebuild-age-index [db]
  (reset! age-index
    (reduce (fn [acc [e age]]
              (update acc age (fnil conj []) e))
            {}
            (d/q '[:find ?e ?age
                   :where [?e :person/age ?age]]
                 db))))

(rebuild-age-index db)

;; Query custom index (O(1) lookup)
(get @age-index 30)
;; => [entity-id-1 entity-id-2 ...]
;; => Instant lookup vs query scan

;; Update index incrementally after transactions
(defn update-age-index [tx-data]
  (doseq [[e a v tx added] tx-data]
    (when (= a :person/age)
      (if added
        (swap! age-index update v (fnil conj []) e)
        (swap! age-index update v (fn [es] (remove #{e} es)))))))
```

**Key Takeaway**: Build custom indexes for hot query paths. Materialize query results in memory for O(1) lookups. Update incrementally using transaction data.

---

## Example 66: Distributed Transactions Across Databases

Coordinate transactions across multiple Datomic databases using application-level coordination.

**Java Code**:

```java
// Note: Datomic doesn't support distributed transactions across databases
// Use application-level coordination (e.g., outbox pattern)

// Two databases
String uri1 = "datomic:mem://db1";
String uri2 = "datomic:mem://db2";
Peer.createDatabase(uri1);
Peer.createDatabase(uri2);
Connection conn1 = Peer.connect(uri1);
Connection conn2 = Peer.connect(uri2);

// Application-level two-phase commit simulation
Map distributedTransact(Connection conn1, List tx1, Connection conn2, List tx2) {
    try {
        // Phase 1: Prepare (validate transactions)
        conn1.db().with(tx1);
        conn2.db().with(tx2);

        // Phase 2: Commit (both or neither)
        Map result1 = conn1.transact(tx1).get();
        Map result2 = conn2.transact(tx2).get();

        return Util.map(":success", true,
                       ":results", Util.list(result1, result2));
    } catch (Exception e) {
        // Rollback if either fails
        return Util.map(":success", false,
                       ":error", e.getMessage());
    }
}

// Use with care - not true distributed transactions
distributedTransact(
    conn1, Util.list(Util.map(":person/name", "User in DB1")),
    conn2, Util.list(Util.map(":project/name", "Project in DB2"))
);
```

**Clojure Code**:

```clojure
;; Note: Datomic doesn't support distributed transactions across databases
;; Use application-level coordination (e.g., outbox pattern)

;; Two databases
(def uri-1 "datomic:mem://db1")
(def uri-2 "datomic:mem://db2")
(d/create-database uri-1)
(d/create-database uri-2)
(def conn-1 (d/connect uri-1))
(def conn-2 (d/connect uri-2))

;; Application-level two-phase commit simulation
(defn distributed-transact [conn-1 tx-1 conn-2 tx-2]
  (try
    ;; Phase 1: Prepare (validate transactions)
    (d/with (d/db conn-1) tx-1)
    (d/with (d/db conn-2) tx-2)
    ;; Phase 2: Commit (both or neither)
    (let [result-1 @(d/transact conn-1 tx-1)
          result-2 @(d/transact conn-2 tx-2)]
      {:success true
       :results [result-1 result-2]})
    (catch Exception e
      ;; Rollback if either fails
      {:success false
       :error (.getMessage e)})))

;; Use with care - not true distributed transactions
(distributed-transact
  conn-1 [{:person/name "User in DB1"}]
  conn-2 [{:project/name "Project in DB2"}])
```

**Key Takeaway**: Datomic transactions are per-database. Use application-level coordination (saga pattern, outbox pattern) for cross-database atomicity.

---

## Example 67: Performance Tuning with Memory Settings

Optimize peer memory settings for query performance and cache hit rates.

**Java Code**:

```java
// Note: Memory tuning applies to Datomic Pro/Cloud peer servers
// For Datomic Free embedded peer, use JVM heap settings

// JVM settings for peer (example)
// -Xmx4g -Xms4g              (4GB heap)
// -Ddatomic.objectCacheMax=1g (1GB object cache)
// -Ddatomic.memoryIndexMax=512m (512MB memory index)

// Monitor cache statistics (Datomic Pro)
// Map metrics = (Map) Peer.metrics(conn);
// => Returns metrics including cache hit rates

// Application-level caching of database values
static class DbCache {
    private volatile Database db;
    private volatile long validUntil;

    public synchronized Database getCachedDb(Connection conn, long cacheMs) {
        long now = System.currentTimeMillis();
        if (now > validUntil) {
            db = conn.db();
            validUntil = now + cacheMs;
        }
        return db;
    }
}

DbCache dbCache = new DbCache();

// Use cached db (refreshes every 1 second)
Database cachedDb = dbCache.getCachedDb(conn, 1000);
// => Returns cached database value (saves connection overhead)
```

**Clojure Code**:

```clojure
;; Note: Memory tuning applies to Datomic Pro/Cloud peer servers
;; For Datomic Free embedded peer, use JVM heap settings

;; JVM settings for peer (example)
;; -Xmx4g -Xms4g              (4GB heap)
;; -Ddatomic.objectCacheMax=1g (1GB object cache)
;; -Ddatomic.memoryIndexMax=512m (512MB memory index)

;; Monitor cache statistics (Datomic Pro)
;; (d/metrics conn)
;; => Returns metrics including cache hit rates

;; Application-level caching of database values
(def db-cache (atom {:db nil :valid-until 0}))

(defn cached-db [conn cache-ms]
  (let [now (System/currentTimeMillis)
        cached @db-cache]
    (if (> now (:valid-until cached))
      (let [new-db (d/db conn)]
        (reset! db-cache {:db new-db
                          :valid-until (+ now cache-ms)})
        new-db)
      (:db cached)))

;; Use cached db (refreshes every 1 second)
(cached-db conn 1000)
;; => Returns cached database value (saves connection overhead)
```

**Key Takeaway**: Tune peer memory settings for performance. Cache database values at application level to reduce connection overhead for read-heavy workloads.

---

## Example 68: Schema Versioning and Migration

Manage schema evolution across versions using additive schema changes and migration functions.

**Java Code**:

```java
// Schema versioning attribute
conn.transact(
    Util.list(
        Util.map(":db/ident", ":schema/version",
                 ":db/valueType", ":db.type/long",
                 ":db/cardinality", ":db.cardinality/one")
    )
).get();

// Initial schema (v1)
List schemaV1 = Util.list(
    Util.map(":db/ident", ":person/full-name",
             ":db/valueType", ":db.type/string",
             ":db/cardinality", ":db.cardinality/one")
);

List combined = new ArrayList(schemaV1);
combined.add(Util.map(":db/ident", ":schema/version", ":schema/version", 1));
conn.transact(combined).get();

// Schema v2: Split full-name into first-name and last-name
List schemaV2 = Util.list(
    Util.map(":db/ident", ":person/first-name",
             ":db/valueType", ":db.type/string",
             ":db/cardinality", ":db.cardinality/one"),
    Util.map(":db/ident", ":person/last-name",
             ":db/valueType", ":db.type/string",
             ":db/cardinality", ":db.cardinality/one")
);

// Migration function
void migrateV1ToV2(Connection conn) throws Exception {
    // Add new attributes
    conn.transact(schemaV2).get();

    // Transform data
    Database db = conn.db();
    Collection people = Peer.q(
        "[:find ?e ?full-name " +
        " :where [?e :person/full-name ?full-name]]",
        db
    );

    List txData = new ArrayList();
    for (Object obj : people) {
        List tuple = (List) obj;
        Object e = tuple.get(0);
        String fullName = (String) tuple.get(1);
        String[] parts = fullName.split(" ");
        String firstName = parts[0];
        String lastName = parts.length > 1 ? parts[parts.length - 1] : parts[0];

        txData.add(Util.map(":db/id", e,
                           ":person/first-name", firstName,
                           ":person/last-name", lastName));
    }
    conn.transact(txData).get();

    // Update version
    conn.transact(Util.list(
        Util.map(":db/ident", ":schema/version", ":schema/version", 2)
    )).get();
}

// Check current version
Entity schemaVersionEntity = conn.db().entity(":schema/version");
Integer currentVersion = (Integer) schemaVersionEntity.get(":schema/version");

// Run migration if needed
if (currentVersion < 2) {
    migrateV1ToV2(conn);
}
```

**Clojure Code**:

```clojure
;; Schema versioning attribute
@(d/transact conn
   [{:db/ident       :schema/version
     :db/valueType   :db.type/long
     :db/cardinality :db.cardinality/one}])

;; Initial schema (v1)
(def schema-v1
  [{:db/ident       :person/full-name
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}])

@(d/transact conn (conj schema-v1 {:db/ident :schema/version :schema/version 1}))

;; Schema v2: Split full-name into first-name and last-name
(def schema-v2
  [{:db/ident       :person/first-name
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :person/last-name
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}])

;; Migration function
(defn migrate-v1-to-v2 [conn]
  ;; Add new attributes
  @(d/transact conn schema-v2)
  ;; Transform data
  (let [db (d/db conn)
        people (d/q '[:find ?e ?full-name
                      :where [?e :person/full-name ?full-name]]
                    db)]
    @(d/transact conn
       (for [[e full-name] people
             :let [parts (clojure.string/split full-name #" ")
                   first-name (first parts)
                   last-name (last parts)]]
         {:db/id e
          :person/first-name first-name
          :person/last-name last-name})))
  ;; Update version
  @(d/transact conn [{:db/ident :schema/version :schema/version 2}]))

;; Check current version
(def current-version
  (:schema/version (d/entity (d/db conn) :schema/version)))

;; Run migration if needed
(when (< current-version 2)
  (migrate-v1-to-v2 conn))
```

**Key Takeaway**: Schema evolution is additive in Datomic. Version your schema, write migration functions to transform data, and track schema version in database.

---

## Example 69: Monitoring and Alerting on Transaction Log

Monitor transaction log for anomalies, trigger alerts on suspicious patterns.

**Java Code**:

```java
void monitorLargeTransactions(Connection conn, int threshold) {
    datomic.Log log = conn.log();
    Iterable recentTxs = log.txRange(null, null);

    int count = 0;
    for (Object obj : recentTxs) {
        if (count++ >= 100) break;

        Map tx = (Map) obj;
        Collection data = (Collection) tx.get(Keyword.intern("data"));
        int datomCount = data.size();

        if (datomCount > threshold) {
            System.out.println("ALERT: Large transaction detected: " +
                             "tx-id " + tx.get(Keyword.intern("t")) +
                             " datoms " + datomCount);
            // Trigger alert (send to monitoring system)
        }
    }
}

// Run monitoring
monitorLargeTransactions(conn, 1000);
// => Alerts on transactions with >1000 datoms

// Monitor specific attribute changes
void monitorSensitiveAttributes(Connection conn, Set<String> attributes) {
    Database db = conn.db();
    datomic.Log log = conn.log();
    Iterable recentTxs = log.txRange(null, null);

    int count = 0;
    for (Object obj : recentTxs) {
        if (count++ >= 100) break;

        Map tx = (Map) obj;
        Collection data = (Collection) tx.get(Keyword.intern("data"));

        for (Object datomObj : data) {
            datomic.Datom datom = (datomic.Datom) datomObj;
            Object attr = db.ident(datom.a());

            if (attributes.contains(attr.toString())) {
                System.out.println("ALERT: Sensitive attribute changed: " +
                                 "entity " + datom.e() +
                                 " attribute " + attr +
                                 " tx " + tx.get(Keyword.intern("t")));
            }
        }
    }
}

monitorSensitiveAttributes(conn, new HashSet<>(Arrays.asList(":person/email", ":person/password-hash")));
```

**Clojure Code**:

```clojure
(defn monitor-large-transactions [conn threshold]
  (let [log (d/log conn)
        recent-txs (take 100 (d/tx-range log nil nil))]
    (doseq [tx recent-txs]
      (let [datom-count (count (:data tx))]
        (when (> datom-count threshold)
          (println "ALERT: Large transaction detected:"
                   "tx-id" (:t tx)
                   "datoms" datom-count)
          ;; Trigger alert (send to monitoring system)
          )))))

;; Run monitoring
(monitor-large-transactions conn 1000)
;; => Alerts on transactions with >1000 datoms

;; Monitor specific attribute changes
(defn monitor-sensitive-attributes [conn attributes]
  (let [db (d/db conn)
        log (d/log conn)
        recent-txs (take 100 (d/tx-range log nil nil))]
    (doseq [tx recent-txs]
      (doseq [datom (:data tx)
              :let [attr (d/ident db (:a datom))]
              :when (attributes attr)]
        (println "ALERT: Sensitive attribute changed:"
                 "entity" (:e datom)
                 "attribute" attr
                 "tx" (:t tx))))))

(monitor-sensitive-attributes conn #{:person/email :person/password-hash})
```

**Key Takeaway**: Monitor transaction log for anomalies, large transactions, sensitive data changes. Essential for security, compliance, and operational visibility.

---

## Example 70: Backup and Point-in-Time Recovery

Create backups and restore to specific points in time.

**Java Code**:

```java
// Note: Backup strategies differ by storage backend
// Datomic Free (dev storage): backup not supported (in-memory or local files)
// Datomic Pro: use backup-db API
// Datomic Cloud: AWS backup snapshots

// Datomic Pro backup example (requires Pro license)
// Peer.backupDb(conn, "s3://my-bucket/backups/tutorial");

// For Datomic Free: export data manually
void exportDatabase(Connection conn, String outputFile) throws IOException {
    Database db = conn.db(); // Get current database value
    Iterable datoms = db.datoms(Peer.EAVT); // Get all datoms in EAVT index order

    try (PrintWriter w = new PrintWriter(new FileWriter(outputFile))) {
        for (Object obj : datoms) {
            datomic.Datom datom = (datomic.Datom) obj; // Cast to Datom
            w.println(datom.toString()); // Write datom as string
        }
    }
}

// Export all data
exportDatabase(conn, "backup-2026-01-29.edn"); // Create backup file

// Point-in-time query (no restore needed - just query)
Database dbYesterday = conn.db().asOf(yesterdayTxId); // Get database as of yesterday
Long count = (Long) Peer.q(
    "[:find (count ?e) . :where [?e :person/name]]", // Count people
    dbYesterday
);
// => Count of people as of yesterday
// => No restore needed - immutability enables time-travel
```

**Clojure Code**:

```clojure
;; Note: Backup strategies differ by storage backend
;; Datomic Free (dev storage): backup not supported (in-memory or local files)
;; Datomic Pro: use backup-db API
;; Datomic Cloud: AWS backup snapshots

;; Datomic Pro backup example (requires Pro license)
;; (d/backup-db conn "s3://my-bucket/backups/tutorial")

;; For Datomic Free: export data manually
(defn export-database [conn output-file]
  (let [db (d/db conn)
        all-datoms (d/datoms db :eavt)]
    (with-open [w (clojure.java.io/writer output-file)]
      (doseq [datom all-datoms]
        (.write w (pr-str datom))
        (.write w "\n")))))

;; Export all data
(export-database conn "backup-2026-01-29.edn")

;; Point-in-time query (no restore needed - just query)
(def db-yesterday (d/as-of (d/db conn) yesterday-tx-id))
(d/q '[:find (count ?e) . :where [?e :person/name]] db-yesterday)
;; => Count of people as of yesterday
;; => No restore needed - immutability enables time-travel
```

**Key Takeaway**: Datomic Pro/Cloud support native backup. Datomic Free uses manual export or just queries historical database values. Immutability enables point-in-time queries without restore.

---

## Example 71: Integration with Message Queues

Publish database changes to message queues for downstream processing.

**Java Code**:

```java
// Simulate message queue (use real queue in production)
List<Map<String, Object>> messageQueue = new ArrayList<>(); // In-memory queue

void publishChangeEvent(Long entityId, Keyword attribute,
                       Object oldValue, Object newValue) {
    Map<String, Object> event = new HashMap<>(); // Create event map
    event.put("entity", entityId); // Entity ID
    event.put("attribute", attribute); // Attribute changed
    event.put("old", oldValue); // Old value (null if new)
    event.put("new", newValue); // New value
    event.put("timestamp", new java.util.Date()); // Event timestamp
    messageQueue.add(event); // Add to queue
}

// Process transactions and publish changes
void processTransactionForEvents(Map txResult) {
    Database dbBefore = (Database) txResult.get(Keyword.intern("db-before")); // DB before tx
    Database dbAfter = (Database) txResult.get(Keyword.intern("db-after")); // DB after tx
    Collection txData = (Collection) txResult.get(Keyword.intern("tx-data")); // Transaction data

    for (Object obj : txData) {
        datomic.Datom datom = (datomic.Datom) obj; // Cast to Datom
        Long e = datom.e(); // Entity ID
        Object a = datom.a(); // Attribute
        Object v = datom.v(); // Value
        boolean added = datom.added(); // Was added (not retracted)

        Keyword attrIdent = (Keyword) dbAfter.ident(a); // Get attribute ident
        if (added && "person".equals(attrIdent.getNamespace())) { // Filter person namespace
            Object oldValue = dbBefore.entity(e).get(attrIdent); // Get old value
            publishChangeEvent(e, attrIdent, oldValue, v); // Publish event
        }
    }
}

// Transact and publish
Map txResult = conn.transact(
    Util.list(
        Util.map(":person/email", "queue-test@example.com",
                 ":person/name", "Queue Test",
                 ":person/age", 35)
    )
).get(); // Block for result

processTransactionForEvents(txResult); // Process and publish events

// Check message queue
System.out.println(messageQueue);
// => [{entity=123, attribute=:person/name, old=null, new=Queue Test, ...}
//     {entity=123, attribute=:person/age, old=null, new=35, ...}]
```

**Clojure Code**:

```clojure
;; Simulate message queue (use real queue in production)
(def message-queue (atom []))

(defn publish-change-event [entity-id attribute old-value new-value]
  (swap! message-queue conj
    {:entity entity-id
     :attribute attribute
     :old old-value
     :new new-value
     :timestamp (java.util.Date.)}))

;; Process transactions and publish changes
(defn process-transaction-for-events [tx-result]
  (let [db-before (:db-before tx-result)
        db-after (:db-after tx-result)
        tx-data (:tx-data tx-result)]
    (doseq [[e a v tx added] tx-data]
      (let [attr-ident (d/ident db-after a)]
        (when (and added (= (namespace attr-ident) "person"))
          (let [old-value (get (d/entity db-before e) attr-ident)]
            (publish-change-event e attr-ident old-value v)))))))

;; Transact and publish
(def tx-result
  @(d/transact conn
     [{:person/email "queue-test@example.com"
       :person/name "Queue Test"
       :person/age 35}]))

(process-transaction-for-events tx-result)

;; Check message queue
@message-queue
;; => [{:entity 123 :attribute :person/name :old nil :new "Queue Test" ...}
;;     {:entity 123 :attribute :person/age :old nil :new 35 ...}]
```

**Key Takeaway**: Process transaction results to publish change events to message queues. Enables event-driven architectures and downstream system integration.

---

## Example 72: Multi-Version Concurrency Control (MVCC)

Understand Datomic's MVCC model: reads never block writes, writes never block reads.

**Java Code**:

```java
// Thread 1: Long-running read
Database readerDb = conn.db(); // Capture immutable database snapshot
new Thread(() -> {
    try {
        Thread.sleep(5000); // Simulate slow processing
        Long count = (Long) Peer.q(
            "[:find (count ?e) . :where [?e :person/name]]", // Count people
            readerDb // Using captured snapshot
        );
        System.out.println("Reader sees count: " + count); // Print count
    } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
    }
}).start(); // Start reader thread

// Thread 2: Write during read
Thread.sleep(1000); // Wait 1 second
conn.transact(
    Util.list(
        Util.map(":person/email", "concurrent@example.com",
                 ":person/name", "Concurrent User")
    )
).get(); // Block for result

// Output:
// Reader sees count: 1008
// => Reader's database value is immutable
// => Unaffected by concurrent writes

// New reader sees updated count
Long newCount = (Long) Peer.q(
    "[:find (count ?e) . :where [?e :person/name]]", // Count people
    conn.db() // Get current database value
);
// => 1009
// => New database value includes concurrent write
```

**Clojure Code**:

```clojure
;; Thread 1: Long-running read
(def reader-db (d/db conn))
(future
  (Thread/sleep 5000) ;; Simulate slow processing
  (println "Reader sees count:"
           (d/q '[:find (count ?e) . :where [?e :person/name]] reader-db)))

;; Thread 2: Write during read
(Thread/sleep 1000)
@(d/transact conn
   [{:person/email "concurrent@example.com"
     :person/name "Concurrent User"}])

;; Output:
;; Reader sees count: 1008
;; => Reader's database value is immutable
;; => Unaffected by concurrent writes

;; New reader sees updated count
(d/q '[:find (count ?e) . :where [?e :person/name]] (d/db conn))
;; => 1009
;; => New database value includes concurrent write
```

**Key Takeaway**: Datomic uses MVCC - database values are immutable snapshots. Reads never block writes, writes never block reads. No read locks needed.

---

## Example 73: Optimizing Large Cardinality-Many Attributes

Handle attributes with thousands of values efficiently.

**Java Code**:

```java
// Large cardinality-many attribute (e.g., followers)
conn.transact(
    Util.list(
        Util.map(":db/ident", ":person/followers",
                 ":db/valueType", ":db.type/ref",
                 ":db/cardinality", ":db.cardinality/many",
                 ":db/doc", "People who follow this person")
    )
).get(); // Define followers attribute

// Add many followers
List influencerId = Util.list(":person/email", "influencer@example.com"); // Lookup ref
conn.transact(
    Util.list(
        Util.map(":person/email", "influencer@example.com",
                 ":person/name", "Influencer")
    )
).get(); // Create influencer

// Add 10k followers
List<Map> followers = new ArrayList<>(); // List of follower entities
List<List> followerIds = new ArrayList<>(); // List of lookup refs
for (int i = 0; i < 10000; i++) {
    String email = "follower" + i + "@example.com";
    followers.add(Util.map(":person/email", email,
                           ":person/name", "Follower " + email)); // Create entity
    followerIds.add(Util.list(":person/email", email)); // Create lookup ref
}

conn.transact(followers).get(); // Create all followers
conn.transact(
    Util.list(
        Util.map(":db/id", influencerId,
                 ":person/followers", followerIds) // Add all followers
    )
).get(); // Link followers to influencer

// Query followers efficiently (use limit in pull)
long start = System.currentTimeMillis();
Map result = (Map) Peer.pull(
    db,
    "[{(:person/followers {:limit 100}) [*]}]", // Pull pattern with limit
    influencerId
);
long elapsed = System.currentTimeMillis() - start;
// => Returns first 100 followers
// => Faster than pulling all 10k

// Count followers without loading all
start = System.currentTimeMillis();
Collection countResult = Peer.q(
    "[:find (count ?follower) " + // Count aggregate
    " :where [?influencer :person/email \"influencer@example.com\"] " +
    "        [?influencer :person/followers ?follower]]",
    db
);
elapsed = System.currentTimeMillis() - start;
// => [[10000]]
// => Count aggregation doesn't materialize all values
```

**Clojure Code**:

```clojure
;; Large cardinality-many attribute (e.g., followers)
@(d/transact conn
   [{:db/ident       :person/followers
     :db/valueType   :db.type/ref
     :db/cardinality :db.cardinality/many
     :db/doc         "People who follow this person"}])

;; Add many followers
(def influencer-id [:person/email "influencer@example.com"])
@(d/transact conn
   [{:person/email "influencer@example.com"
     :person/name "Influencer"}])

;; Add 10k followers
(let [follower-ids (for [i (range 10000)]
                     [:person/email (str "follower" i "@example.com")])]
  @(d/transact conn
     (for [email follower-ids]
       {:person/email (second email)
        :person/name (str "Follower " (second email))}))
  @(d/transact conn
     [{:db/id influencer-id
       :person/followers follower-ids}]))

;; Query followers efficiently (use limit in pull)
(time
  (d/pull db '[(limit :person/followers 100)] influencer-id))
;; => Returns first 100 followers
;; => Faster than pulling all 10k

;; Count followers without loading all
(time
  (d/q '[:find (count ?follower)
         :where [?influencer :person/email "influencer@example.com"]
                [?influencer :person/followers ?follower]]
       db))
;; => [[10000]]
;; => Count aggregation doesn't materialize all values
```

**Key Takeaway**: Use `(limit :attr n)` in pull patterns for large cardinality-many attributes. Use count aggregates instead of materializing all values.

---

## Example 74: Reactive Queries with Core.async

Implement reactive queries that update automatically on database changes.

**Java Code**:

```java
// Note: Java doesn't have core.async built-in
// Use java.util.concurrent for reactive patterns

// Create blocking queue for database updates
BlockingQueue<Database> dbQueue = new LinkedBlockingQueue<>(); // Thread-safe queue

// Polling function (simulates tx-report-queue in Datomic Pro)
void pollForUpdates(Connection conn, long intervalMs) {
    new Thread(() -> {
        long lastT = conn.db().basisT(); // Get initial basis-t
        while (true) {
            try {
                Thread.sleep(intervalMs); // Wait for interval
                Database currentDb = conn.db(); // Get current database
                long currentT = currentDb.basisT(); // Get current basis-t
                if (currentT > lastT) { // Database changed
                    dbQueue.put(currentDb); // Send to queue
                    lastT = currentT; // Update last-t
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }
    }).start(); // Start polling thread
}

// Start polling
pollForUpdates(conn, 1000); // Poll every 1 second

// Reactive query consumer
new Thread(() -> {
    while (true) {
        try {
            Database db = dbQueue.take(); // Block for next database
            Long count = (Long) Peer.q(
                "[:find (count ?e) . :where [?e :person/name]]", // Count people
                db
            );
            System.out.println("Person count updated: " + count); // Print update
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            break;
        }
    }
}).start(); // Start consumer thread

// Trigger update
conn.transact(
    Util.list(
        Util.map(":person/email", "reactive@example.com",
                 ":person/name", "Reactive User")
    )
).get(); // Create reactive user
// => Output (after 1s): "Person count updated: 1010"
```

**Clojure Code**:

```clojure
(require '[clojure.core.async :as async])

;; Create channel for database updates
(def db-channel (async/chan))

;; Polling function (simulates tx-report-queue in Datomic Pro)
(defn poll-for-updates [conn interval-ms]
  (async/go-loop [last-t (d/basis-t (d/db conn))]
    (async/<! (async/timeout interval-ms))
    (let [current-db (d/db conn)
          current-t (d/basis-t current-db)]
      (when (> current-t last-t)
        (async/>! db-channel current-db))
      (recur current-t))))

;; Start polling
(poll-for-updates conn 1000)

;; Reactive query consumer
(async/go-loop []
  (when-let [db (async/<! db-channel)]
    (let [count (d/q '[:find (count ?e) . :where [?e :person/name]] db)]
      (println "Person count updated:" count))
    (recur)))

;; Trigger update
@(d/transact conn [{:person/email "reactive@example.com"
                    :person/name "Reactive User"}])
;; => Output (after 1s): "Person count updated: 1010"
```

**Key Takeaway**: Implement reactive queries with core.async channels. Poll for database changes (Datomic Free) or use tx-report-queue (Pro/Cloud).

---

## Example 75: Access Control with Attribute-Level Filters

Implement attribute-level access control using database filters.

**Java Code**:

```java
// Define sensitive attributes
Set<Keyword> sensitiveAttrs = new HashSet<>(); // Set of sensitive keywords
sensitiveAttrs.add(Keyword.intern("person", "password-hash")); // Password hash
sensitiveAttrs.add(Keyword.intern("person", "ssn")); // Social security number

// Create filtered database (removes sensitive attributes)
Database createRestrictedDb(Database db, String userRole) {
    if ("admin".equals(userRole)) {
        return db; // Admins see everything
    }

    // Filter for non-admin users
    return db.filter(new Database.Predicate<datomic.Datom>() { // Filter predicate
        @Override
        public boolean apply(Database filterDb, datomic.Datom datom) {
            Keyword attr = (Keyword) filterDb.ident(datom.a()); // Get attribute ident
            return !sensitiveAttrs.contains(attr); // Include if not sensitive
        }
    });
}

// User query (non-admin)
Database userDb = createRestrictedDb(conn.db(), "user"); // Create filtered DB
Map userResult = (Map) Peer.pull(
    userDb,
    "[*]", // Pull all attributes
    Util.list(":person/email", "alice@example.com") // Lookup ref
);
// => {:person/name "Alice Johnson"
//     :person/email "alice@example.com"
//     :person/age 34}
// => No :person/password-hash (filtered out)

// Admin query
Database adminDb = createRestrictedDb(conn.db(), "admin"); // Unfiltered DB
Map adminResult = (Map) Peer.pull(
    adminDb,
    "[*]", // Pull all attributes
    Util.list(":person/email", "alice@example.com") // Lookup ref
);
// => {:person/name "Alice Johnson"
//     :person/email "alice@example.com"
//     :person/age 34
//     :person/password-hash "..."}
// => Includes sensitive attributes
```

**Clojure Code**:

```clojure
;; Define sensitive attributes
(def sensitive-attrs #{:person/password-hash :person/ssn})

;; Create filtered database (removes sensitive attributes)
(defn create-restricted-db [db user-role]
  (if (= user-role :admin)
    db ;; Admins see everything
    (d/filter db
      (fn [db datom]
        (let [attr (d/ident db (:a datom))]
          (not (sensitive-attrs attr)))))))

;; User query (non-admin)
(def user-db (create-restricted-db (d/db conn) :user))
(d/pull user-db '[*] [:person/email "alice@example.com"])
;; => {:person/name "Alice Johnson"
;;     :person/email "alice@example.com"
;;     :person/age 34}
;; => No :person/password-hash (filtered out)

;; Admin query
(def admin-db (create-restricted-db (d/db conn) :admin))
(d/pull admin-db '[*] [:person/email "alice@example.com"])
;; => {:person/name "Alice Johnson"
;;     :person/email "alice@example.com"
;;     :person/age 34
;;     :person/password-hash "..."}
;; => Includes sensitive attributes
```

**Key Takeaway**: Use database filters for attribute-level access control. Filter sensitive attributes based on user roles without modifying queries.

---

## Example 76: Debugging Queries with :explain

Understand query execution plans to optimize performance.

**Java Code**:

```java
// Note: :explain is experimental and may vary by Datomic version

// Add :explain to query (example)
Collection result = Peer.q(
    "[:find ?name " + // Find name
    " :where [?e :person/email \"alice@example.com\"] " +
    "        [?e :person/name ?name]]",
    db
);
// => Standard query execution

// Datomic Pro query explanation (not available in Free)
// Map queryArg = Util.map(":db", db, ":explain", true);
// Object explanation = Peer.q(
//     "[:find ?name " +
//     " :where [?e :person/email \"alice@example.com\"] " +
//     "        [?e :person/name ?name]]",
//     queryArg
// );
// => Returns execution plan showing index selection

// Manual analysis: check if unique attribute is used
boolean queryUsesUniqueAttr(String queryStr, Database db) {
    // Parse query string (simplified example)
    // In real code, parse the query properly
    if (queryStr.contains(":person/email")) { // Check for email attribute
        Object attrId = db.entid(Keyword.intern("person", "email")); // Get attribute ID
        if (attrId != null) {
            datomic.Entity attr = db.entity(attrId); // Get attribute entity
            Object unique = attr.get(Keyword.intern("db", "unique")); // Get :db/unique
            return unique != null; // Returns true if unique
        }
    }
    return false;
}

boolean usesUnique = queryUsesUniqueAttr(
    "[:find ?name :where [?e :person/email \"alice@example.com\"] " +
    "[?e :person/name ?name]]",
    db
);
// => true (:person/email is unique)
```

**Clojure Code**:

```clojure
;; Note: :explain is experimental and may vary by Datomic version

;; Add :explain to query (example)
(d/q '[:find ?name
       :where [?e :person/email "alice@example.com"]
              [?e :person/name ?name]]
     db)
;; => Standard query execution

;; Datomic Pro query explanation (not available in Free)
;; (d/q '[:find ?name
;;        :where [?e :person/email "alice@example.com"]
;;               [?e :person/name ?name]]
;;      {:db db :explain true})
;; => Returns execution plan showing index selection

;; Manual analysis: check if unique attribute is used
(defn query-uses-unique-attr? [query]
  (some (fn [clause]
          (and (vector? clause)
               (= 3 (count clause))
               (keyword? (second clause))
               (:db/unique (d/entity db (second clause)))))
        (:where query)))

(query-uses-unique-attr?
  '{:find [?name]
    :where [[?e :person/email "alice@example.com"]
            [?e :person/name ?name]]})
;; => true (:person/email is unique)
```

**Key Takeaway**: Datomic Pro offers query explanation. In Datomic Free, manually analyze queries for unique attribute usage and index selection.

---

## Example 77: Composite Entities with Component Attributes

Model complex aggregates using component attributes for lifecycle coupling.

**Java Code**:

```java
// Order-OrderLine aggregate
conn.transact(
    Util.list(
        Util.map(":db/ident", ":order/id",
                 ":db/valueType", ":db.type/string",
                 ":db/cardinality", ":db.cardinality/one",
                 ":db/unique", ":db.unique/identity"), // Unique order ID
        Util.map(":db/ident", ":order/lines",
                 ":db/valueType", ":db.type/ref",
                 ":db/cardinality", ":db.cardinality/many",
                 ":db/isComponent", true), // Component relationship
        Util.map(":db/ident", ":order-line/product",
                 ":db/valueType", ":db.type/string",
                 ":db/cardinality", ":db.cardinality/one"), // Product name
        Util.map(":db/ident", ":order-line/quantity",
                 ":db/valueType", ":db.type/long",
                 ":db/cardinality", ":db.cardinality/one") // Quantity
    )
).get(); // Define schema

// Create order with lines
conn.transact(
    Util.list(
        Util.map(":order/id", "ORD-001",
                 ":order/lines", Util.list( // Nested entities
                     Util.map(":order-line/product", "Widget",
                              ":order-line/quantity", 5), // First line
                     Util.map(":order-line/product", "Gadget",
                              ":order-line/quantity", 3)  // Second line
                 ))
    )
).get(); // Create order with lines

// Pull complete aggregate
Database db = conn.db();
Map order = (Map) Peer.pull(
    db,
    "[* {:order/lines [*]}]", // Pull order and nested lines
    Util.list(":order/id", "ORD-001") // Lookup ref
);
// => {:order/id "ORD-001"
//     :order/lines [{:order-line/product "Widget" :order-line/quantity 5}
//                   {:order-line/product "Gadget" :order-line/quantity 3}]}

// Retract order - lines automatically retracted
conn.transact(
    Util.list(
        Util.list(":db/retractEntity", Util.list(":order/id", "ORD-001")) // Retract order
    )
).get(); // Component lines retracted too

// Verify lines are gone
Collection products = Peer.q(
    "[:find ?product :where [?line :order-line/product ?product]]", // Find products
    conn.db()
);
// => #{}
// => Component lines retracted with parent
```

**Clojure Code**:

```clojure
;; Order-OrderLine aggregate
@(d/transact conn
   [{:db/ident       :order/id
     :db/valueType   :db.type/string
     :db/cardinality :db.cardinality/one
     :db/unique      :db.unique/identity}
    {:db/ident       :order/lines
     :db/valueType   :db.type/ref
     :db/cardinality :db.cardinality/many
     :db/isComponent true}
    {:db/ident       :order-line/product
     :db/valueType   :db.type/string
     :db/cardinality :db.cardinality/one}
    {:db/ident       :order-line/quantity
     :db/valueType   :db.type/long
     :db/cardinality :db.cardinality/one}])

;; Create order with lines
@(d/transact conn
   [{:order/id "ORD-001"
     :order/lines [{:order-line/product "Widget"
                    :order-line/quantity 5}
                   {:order-line/product "Gadget"
                    :order-line/quantity 3}]}])

;; Pull complete aggregate
(def db (d/db conn))
(d/pull db
  '[* {:order/lines [*]}]
  [:order/id "ORD-001"])
;; => {:order/id "ORD-001"
;;     :order/lines [{:order-line/product "Widget" :order-line/quantity 5}
;;                   {:order-line/product "Gadget" :order-line/quantity 3}]}

;; Retract order - lines automatically retracted
@(d/transact conn [[:db/retractEntity [:order/id "ORD-001"]]])

;; Verify lines are gone
(d/q '[:find ?product :where [?line :order-line/product ?product]] (d/db conn))
;; => #{}
;; => Component lines retracted with parent
```

**Key Takeaway**: Component attributes couple entity lifecycles. Use for aggregate roots and their owned children (Order-OrderLine, BlogPost-Comments, etc).

---

## Example 78: Building Event Sourcing Systems

Use Datomic's immutable log as event store for event sourcing architecture.

**Java Code**:

```java
// Event schema
conn.transact(
    Util.list(
        Util.map(":db/ident", ":event/type",
                 ":db/valueType", ":db.type/keyword",
                 ":db/cardinality", ":db.cardinality/one"), // Event type
        Util.map(":db/ident", ":event/aggregate-id",
                 ":db/valueType", ":db.type/uuid",
                 ":db/cardinality", ":db.cardinality/one"), // Aggregate ID
        Util.map(":db/ident", ":event/payload",
                 ":db/valueType", ":db.type/string",
                 ":db/cardinality", ":db.cardinality/one") // Event payload
    )
).get(); // Define event schema

// Record events
UUID aggregateId = UUID.randomUUID(); // Generate aggregate ID

conn.transact(
    Util.list(
        Util.map(":event/type", ":order/created",
                 ":event/aggregate-id", aggregateId,
                 ":event/payload", "{:order-id \"ORD-001\" :customer \"Alice\"}")
    )
).get(); // Record order created event

conn.transact(
    Util.list(
        Util.map(":event/type", ":order/item-added",
                 ":event/aggregate-id", aggregateId,
                 ":event/payload", "{:product \"Widget\" :quantity 5}")
    )
).get(); // Record item added event

conn.transact(
    Util.list(
        Util.map(":event/type", ":order/completed",
                 ":event/aggregate-id", aggregateId,
                 ":event/payload", "{:total 150.00}")
    )
).get(); // Record order completed event

// Replay events for aggregate
Collection replayEvents(Database db, UUID aggregateId) {
    return Peer.q(
        "[:find ?type ?payload ?tx " + // Find event details
        " :in $ ?aggregate-id " +
        " :where [?e :event/aggregate-id ?aggregate-id] " +
        "        [?e :event/type ?type] " +
        "        [?e :event/payload ?payload] " +
        "        [?e _ _ ?tx] " + // Get transaction ID
        " :order ?tx]", // Order by transaction
        db,
        aggregateId
    );
}

Collection events = replayEvents(conn.db(), aggregateId); // Get events in order
// => [[:order/created "{:order-id ...}" tx1]
//     [:order/item-added "{:product ...}" tx2]
//     [:order/completed "{:total ...}" tx3]]

// Rebuild aggregate state from events
Map<String, Object> state = new HashMap<>(); // Initial state
for (Object obj : events) {
    List event = (List) obj; // Cast to list
    Keyword eventType = (Keyword) event.get(0); // Event type
    String payload = (String) event.get(1); // Payload string
    // Apply event to state (simplified - parse payload properly in real code)
    System.out.println("Apply event: " + eventType + " -> " + payload);
}
// => {:order-id "ORD-001" :customer "Alice" :product "Widget" :quantity 5 :total 150.00}
```

**Clojure Code**:

```clojure
;; Event schema
@(d/transact conn
   [{:db/ident       :event/type
     :db/valueType   :db.type/keyword
     :db/cardinality :db.cardinality/one}
    {:db/ident       :event/aggregate-id
     :db/valueType   :db.type/uuid
     :db/cardinality :db.cardinality/one}
    {:db/ident       :event/payload
     :db/valueType   :db.type/string
     :db/cardinality :db.cardinality/one}])

;; Record events
(def aggregate-id (java.util.UUID/randomUUID))

@(d/transact conn
   [{:event/type :order/created
     :event/aggregate-id aggregate-id
     :event/payload (pr-str {:order-id "ORD-001" :customer "Alice"})}])

@(d/transact conn
   [{:event/type :order/item-added
     :event/aggregate-id aggregate-id
     :event/payload (pr-str {:product "Widget" :quantity 5})}])

@(d/transact conn
   [{:event/type :order/completed
     :event/aggregate-id aggregate-id
     :event/payload (pr-str {:total 150.00})}])

;; Replay events for aggregate
(defn replay-events [db aggregate-id]
  (d/q '[:find ?type ?payload ?tx
         :in $ ?aggregate-id
         :where [?e :event/aggregate-id ?aggregate-id]
                [?e :event/type ?type]
                [?e :event/payload ?payload]
                [?e _ _ ?tx]
         :order ?tx]
       db
       aggregate-id))

(def events (replay-events (d/db conn) aggregate-id))
;; => [[:order/created "{:order-id ...}" tx1]
;;     [:order/item-added "{:product ...}" tx2]
;;     [:order/completed "{:total ...}" tx3]]

;; Rebuild aggregate state from events
(reduce (fn [state [event-type payload]]
          ;; Apply event to state
          (merge state (read-string payload)))
        {}
        events)
;; => {:order-id "ORD-001" :customer "Alice" :product "Widget" :quantity 5 :total 150.00}
```

**Key Takeaway**: Datomic's transaction log is a natural event store. Record events as facts, replay by querying transactions in order. Immutability guarantees event history integrity.

---

## Example 79: Handling Schema Conflicts Across Teams

Manage schema in multi-team environments using namespaced attributes and schema registries.

**Java Code**:

```java
// Team A: User service schema
List teamASchema = Util.list(
    Util.map(":db/ident", ":user-service/user-id",
             ":db/valueType", ":db.type/uuid",
             ":db/cardinality", ":db.cardinality/one",
             ":db/unique", ":db.unique/identity"), // Unique user ID
    Util.map(":db/ident", ":user-service/username",
             ":db/valueType", ":db.type/string",
             ":db/cardinality", ":db.cardinality/one") // Username
);

// Team B: Order service schema
List teamBSchema = Util.list(
    Util.map(":db/ident", ":order-service/order-id",
             ":db/valueType", ":db.type/uuid",
             ":db/cardinality", ":db.cardinality/one",
             ":db/unique", ":db.unique/identity"), // Unique order ID
    Util.map(":db/ident", ":order-service/user-id",
             ":db/valueType", ":db.type/ref",
             ":db/cardinality", ":db.cardinality/one",
             ":db/doc", "References :user-service/user-id") // Cross-service ref
);

// Install schemas (no conflicts due to namespacing)
conn.transact(teamASchema).get(); // Install team A schema
conn.transact(teamBSchema).get(); // Install team B schema

// Cross-service reference
conn.transact(
    Util.list(
        Util.map(":user-service/user-id", UUID.randomUUID(),
                 ":user-service/username", "alice")
    )
).get(); // Create user

Long aliceUserId = (Long) Peer.q(
    "[:find ?user . " + // Find user entity ID
    " :where [?user :user-service/username \"alice\"]]",
    conn.db()
);

conn.transact(
    Util.list(
        Util.map(":order-service/order-id", UUID.randomUUID(),
                 ":order-service/user-id", aliceUserId) // Reference user
    )
).get(); // Create order

// Query across services
Collection result = Peer.q(
    "[:find ?username ?order-id " + // Find username and order ID
    " :where [?order :order-service/user-id ?user] " + // Join order to user
    "        [?user :user-service/username ?username] " +
    "        [?order :order-service/order-id ?order-id]]",
    conn.db()
);
// => [["alice" #uuid "..."]]
```

**Clojure Code**:

```clojure
;; Team A: User service schema
(def team-a-schema
  [{:db/ident       :user-service/user-id
    :db/valueType   :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity}
   {:db/ident       :user-service/username
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}])

;; Team B: Order service schema
(def team-b-schema
  [{:db/ident       :order-service/order-id
    :db/valueType   :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity}
   {:db/ident       :order-service/user-id
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc         "References :user-service/user-id"}])

;; Install schemas (no conflicts due to namespacing)
@(d/transact conn team-a-schema)
@(d/transact conn team-b-schema)

;; Cross-service reference
@(d/transact conn
   [{:user-service/user-id (java.util.UUID/randomUUID)
     :user-service/username "alice"}])

(def alice-user-id
  (d/q '[:find ?user .
         :where [?user :user-service/username "alice"]]
       (d/db conn)))

@(d/transact conn
   [{:order-service/order-id (java.util.UUID/randomUUID)
     :order-service/user-id alice-user-id}])

;; Query across services
(d/q '[:find ?username ?order-id
       :where [?order :order-service/user-id ?user]
              [?user :user-service/username ?username]
              [?order :order-service/order-id ?order-id]]
     (d/db conn))
;; => [["alice" #uuid "..."]]
```

**Key Takeaway**: Use namespaced attributes to avoid schema conflicts across teams. Establish namespace ownership conventions and document cross-service references.

---

## Example 80: Production Monitoring and Health Checks

Implement health checks and monitoring for production Datomic deployments.

**Java Code**:

```java
Map<String, Object> healthCheck(Connection conn) {
    try {
        // Test connection
        Database db = conn.db(); // Get database value

        // Test query
        Long personCount = (Long) Peer.q(
            "[:find (count ?e) . :where [?e :person/name]]", // Count people
            db
        );

        // Test transaction
        Object tempId = Peer.tempid(":db.part/user"); // Create temp ID
        Map txResult = conn.transact(
            Util.list(
                Util.list(":db/add", tempId, ":db/doc", "Health check") // Test tx
            )
        ).get(); // Block for result

        // Verify transaction committed
        Database dbAfter = (Database) txResult.get(Keyword.intern("db-after")); // Get db-after
        Map tempIds = (Map) txResult.get(Keyword.intern("tempids")); // Get tempids
        Long txId = (Long) Peer.resolveTempid(dbAfter, tempIds, tempId); // Resolve temp ID

        return Util.map(
            "healthy", true,
            "person-count", personCount,
            "test-tx-id", txId,
            "timestamp", new java.util.Date()
        );
    } catch (Exception e) {
        return Util.map(
            "healthy", false,
            "error", e.getMessage(),
            "timestamp", new java.util.Date()
        );
    }
}

// Run health check
Map healthResult = healthCheck(conn);
// => {healthy=true, person-count=1010, test-tx-id=17592186045999,
//     timestamp=#inst "2026-01-29T..."}

// Monitoring metrics
Map<String, Object> collectMetrics(Connection conn) {
    Database db = conn.db(); // Get database value

    Long entityCount = (Long) Peer.q(
        "[:find (count ?e) . :where [?e :db/ident]]", // Count idents
        db
    );

    Long personCount = (Long) Peer.q(
        "[:find (count ?e) . :where [?e :person/name]]", // Count people
        db
    );

    Long databaseSize = db.basisT(); // Get basis-t (database size indicator)

    return Util.map(
        "entity-count", entityCount,
        "person-count", personCount,
        "database-size", databaseSize,
        "timestamp", new java.util.Date()
    );
}

Map metrics = collectMetrics(conn);
// => {entity-count=1523, person-count=1010,
//     database-size=13194139534330, timestamp=#inst "2026-01-29T..."}

// Export to monitoring system (Prometheus, Datadog, etc.)
// sendToMonitoring(collectMetrics(conn));
```

**Clojure Code**:

```clojure
(defn health-check [conn]
  (try
    ;; Test connection
    (let [db (d/db conn)
          ;; Test query
          person-count (d/q '[:find (count ?e) .
                              :where [?e :person/name]]
                            db)
          ;; Test transaction
          tx-result @(d/transact conn
                       [[:db/add (d/tempid :db.part/user)
                         :db/doc "Health check"]])
          ;; Verify transaction committed
          tx-id (d/resolve-tempid (:db-after tx-result)
                                   (:tempids tx-result)
                                   (d/tempid :db.part/user))]
      {:healthy true
       :person-count person-count
       :test-tx-id tx-id
       :timestamp (java.util.Date.)})
    (catch Exception e
      {:healthy false
       :error (.getMessage e)
       :timestamp (java.util.Date.)})))

;; Run health check
(health-check conn)
;; => {:healthy true
;;     :person-count 1010
;;     :test-tx-id 17592186045999
;;     :timestamp #inst "2026-01-29T..."}

;; Monitoring metrics
(defn collect-metrics [conn]
  (let [db (d/db conn)]
    {:entity-count (d/q '[:find (count ?e) .
                          :where [?e :db/ident]]
                        db)
     :person-count (d/q '[:find (count ?e) .
                          :where [?e :person/name]]
                        db)
     :database-size (d/basis-t db)
     :timestamp (java.util.Date.)}))

(collect-metrics conn)
;; => {:entity-count 1523
;;     :person-count 1010
;;     :database-size 13194139534330
;;     :timestamp #inst "2026-01-29T..."}

;; Export to monitoring system (Prometheus, Datadog, etc.)
;; (send-to-monitoring (collect-metrics conn))
```

**Key Takeaway**: Implement health checks with connection, query, and transaction tests. Collect metrics on entity counts, database size, and query performance for production monitoring.

---

These 20 advanced examples provide expert-level Datomic knowledge: client API, excision, performance tuning, event sourcing, distributed patterns, and production operations. You've now covered 95% of Datomic through 80 hands-on examples. Continue learning through official documentation, community resources, and real-world projects!
