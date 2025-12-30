---
title: "Advanced"
weight: 10000003
date: 2025-12-30T03:00:00+07:00
draft: false
description: "Examples 55-80: Advanced macros, component architecture, web dev, performance (75-95% coverage)"
---

This section covers advanced Clojure techniques from examples 55-80, achieving 75-95% topic coverage.

## Example 55: Advanced Macros - Code Walking

Macros can recursively transform nested code structures.

```mermaid
%% Macro code walking process
graph TD
    A[Code Form] --> B{Is Seq?}
    B -->|Yes| C[Map Walk Over Elements]
    B -->|No| D{Is Vector?}
    D -->|Yes| E[Vec Map Walk]
    D -->|No| F{Is Map?}
    F -->|Yes| G[Map Walk K/V Pairs]
    F -->|No| H[Transform Leaf Node]
    C --> I[Recursive Walk]
    E --> I
    G --> I
    H --> J[Return Transformed]
    I --> J

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style D fill:#DE8F05,color:#000
    style F fill:#DE8F05,color:#000
    style H fill:#029E73,color:#fff
    style J fill:#CC78BC,color:#000
```

```clojure
(defmacro debug-all [& forms]
  `(do
     ~@(map (fn [form]
              `(let [result# ~form]
                 (println '~form "=>" result#)
                 result#))
            forms)))

(debug-all
  (+ 1 2)
  (* 3 4)
  (/ 10 2))
;; => (+ 1 2) => 3
;; => (* 3 4) => 12
;; => (/ 10 2) => 5

;; Recursive code walker
(defn walk-expr [form transform]
  (cond
    (seq? form) (map #(walk-expr % transform) form)
    (vector? form) (vec (map #(walk-expr % transform) form))
    (map? form) (into {} (map (fn [[k v]] [k (walk-expr v transform)]) form))
    :else (transform form)))

(walk-expr '(+ 1 (* 2 3)) #(if (number? %) (* % 10) %))
;; => (+ 10 (* 20 30))
```

**Key Takeaway**: Code walkers enable deep transformation of arbitrarily nested code structures.

## Example 56: Macro Debugging with macroexpand

Debug macros by expanding to see generated code.

```mermaid
%% Macro expansion process
graph LR
    A[Macro Form] --> B[macroexpand-1]
    B --> C[First Level Expansion]
    C --> D{More Macros?}
    D -->|Yes| E[macroexpand]
    D -->|No| F[Final Code]
    E --> G[Recursive Expansion]
    G --> F

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#029E73,color:#fff
    style E fill:#DE8F05,color:#000
    style F fill:#CC78BC,color:#000
```

```clojure
(defmacro when-valid [test & body]
  `(if ~test
     (do ~@body)
     (println "Validation failed")))

;; Expand once
(macroexpand-1 '(when-valid (pos? 5) (println "Valid")))
;; => (if (pos? 5) (do (println "Valid")) (println "Validation failed"))

;; Expand all nested macros
(macroexpand '(when-valid (pos? 5) (println "Valid")))

;; Use macroexpand-1 to see single level expansion
(defmacro unless [test & body]
  `(when-valid (not ~test) ~@body))

(macroexpand-1 '(unless false (println "OK")))
;; => (when-valid (not false) (println "OK"))

(macroexpand '(unless false (println "OK")))
;; => (if (not false) (do (println "OK")) (println "Validation failed"))
```

**Key Takeaway**: macroexpand/macroexpand-1 reveal generated code for debugging macro behavior.

## Example 57: Reader Conditionals for Multiplatform

Write portable code targeting Clojure and ClojureScript.

```clojure
;; .cljc file (Clojure common)
(ns myapp.utils)

(defn current-time []
  #?(:clj  (System/currentTimeMillis)        ;; => JVM implementation
     :cljs (.getTime (js/Date.))))           ;; => JS implementation

(defn log [message]
  #?(:clj  (println message)                 ;; => JVM println
     :cljs (.log js/console message)))       ;; => JS console.log

;; Reader conditional splice
(defn process-data [data]
  [data
   #?@(:clj  [(str "JVM: " data)]
       :cljs [(str "JS: " data)])])          ;; => Splices into vector

;; Feature expressions
#?(:clj (import 'java.util.Date)
   :cljs (def Date js/Date))
```

**Key Takeaway**: Reader conditionals enable shared code with platform-specific implementations.

## Example 58: Type Hints for Performance

Add type hints to eliminate reflection for performance.

```mermaid
%% Type hint performance impact
graph TD
    A[Method Call] --> B{Type Hinted?}
    B -->|No| C[Runtime Reflection]
    B -->|Yes| D[Direct Method Call]
    C --> E[Slow: Class Lookup]
    E --> F[Slow: Method Search]
    F --> G[Finally Execute]
    D --> H[Fast: Direct Invoke]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#CA9161,color:#000
    style D fill:#029E73,color:#fff
    style G fill:#CA9161,color:#000
    style H fill:#029E73,color:#fff
```

```clojure
;; Without type hints (reflection warning)
(defn slow-add [a b]
  (.add a b))                                ;; => Reflection on .add

;; With type hints (no reflection)
(defn fast-add [^java.math.BigDecimal a ^java.math.BigDecimal b]
  (.add a b))                                ;; => Direct method call

;; Return type hint
(defn compute ^long []
  (+ 1 2))                                   ;; => Returns primitive long

;; Array type hints
(defn sum-array ^long [^longs arr]           ;; => long[] array
  (aget arr 0))                              ;; => Fast array access

;; Set compiler warnings
(set! *warn-on-reflection* true)             ;; => Enable reflection warnings

;; Measure performance
(time (dotimes [_ 1000000] (fast-add (bigdec 1) (bigdec 2))))
```

**Key Takeaway**: Type hints eliminate reflection overhead for significant performance improvements.

## Example 59: Stateful Transducers

Transducers can maintain state across transformation steps.

```mermaid
%% Stateful transducer flow
sequenceDiagram
    participant C as Collection
    participant T as Transducer
    participant V as Volatile State
    participant R as Reducing Function

    C->>T: Input 1
    T->>V: Read State
    V-->>T: Previous Value
    T->>V: Update State
    T->>R: Transform & Emit
    C->>T: Input 2
    T->>V: Read State
    V-->>T: Updated Value
    T->>V: Update State
    T->>R: Transform & Emit

    Note over T,V: State persists across steps
```

```clojure
(defn dedupe-consecutive []
  (fn [rf]
    (let [prev (volatile! ::none)]           ;; => Volatile for transducer state
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [p @prev]
           (vreset! prev input)
           (if (= p input)
             result                          ;; => Skip duplicate
             (rf result input))))))))

(into [] (dedupe-consecutive) [1 1 2 2 2 3 3 1])
;; => [1 2 3 1] (consecutive duplicates removed)

;; Running average transducer
(defn running-average []
  (fn [rf]
    (let [sum (volatile! 0)
          count (volatile! 0)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (vswap! sum + input)
         (vswap! count inc)
         (rf result (/ @sum @count)))))))

(into [] (running-average) [1 2 3 4 5])
;; => [1 3/2 2 5/2 3] (running averages)
```

**Key Takeaway**: Volatile refs enable efficient mutable state within transducers.

## Example 60: Reducers with Fork-Join

Leverage reducers for parallel processing on large datasets.

```mermaid
%% Fork-join parallel processing
graph TD
    A[Large Collection] --> B[Split into Chunks]
    B --> C[Chunk 1]
    B --> D[Chunk 2]
    B --> E[Chunk 3]
    B --> F[Chunk 4]
    C --> G[Process in Parallel]
    D --> G
    E --> G
    F --> G
    G --> H[Combine Results]
    H --> I[Final Result]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style G fill:#029E73,color:#fff
    style H fill:#CC78BC,color:#000
    style I fill:#029E73,color:#fff
```

```clojure
(require '[clojure.core.reducers :as r])

;; Transform large collection in parallel
(defn parallel-process [n]
  (->> (range n)
       vec                                   ;; => Must be vector for fold
       (r/map inc)
       (r/filter even?)
       (r/fold +)))                          ;; => Fork-join parallel reduce

(time (parallel-process 10000000))           ;; => Faster with fold

;; Custom combiner
(defn parallel-max [coll]
  (r/fold
    max                                      ;; => Combine function (associative)
    (fn ([acc x] (max acc x)))               ;; => Reduce function
    coll))

(parallel-max (vec (shuffle (range 1000000)))) ;; => 999999

;; Control parallelism
(r/fold 512                                  ;; => Chunk size
        +                                    ;; => Combine
        (fn [acc x] (+ acc x))               ;; => Reduce
        (vec (range 1000000)))
```

**Key Takeaway**: Reducers enable automatic parallelization with fork-join for CPU-bound operations.

## Example 61: Protocols for Polymorphism

Define protocols for extensible polymorphic operations.

```mermaid
%% Protocol polymorphism
graph TD
    A[Protocol Definition] --> B[Serializable]
    B --> C[serialize method]
    B --> D[deserialize method]
    E[String Type] --> F[extend-protocol]
    G[Vector Type] --> F
    H[Date Type] --> I[extend-type]
    F --> C
    F --> D
    I --> C
    I --> D

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style F fill:#029E73,color:#fff
    style I fill:#029E73,color:#fff
```

```clojure
(defprotocol Serializable
  (serialize [this])
  (deserialize [this data]))

;; Extend to existing types
(extend-protocol Serializable
  java.lang.String
  (serialize [s] (.getBytes s))
  (deserialize [_ data] (String. data))

  clojure.lang.PersistentVector
  (serialize [v] (pr-str v))
  (deserialize [_ data] (read-string data)))

(serialize "Hello")                          ;; => byte array
(serialize [1 2 3])                          ;; => "[1 2 3]"

;; Inline extension
(extend-type java.util.Date
  Serializable
  (serialize [d] (.getTime d))
  (deserialize [_ data] (java.util.Date. data)))

;; Check protocol implementation
(satisfies? Serializable "text")             ;; => true
(satisfies? Serializable 42)                 ;; => false
```

**Key Takeaway**: Protocols enable extensible polymorphism for existing and new types.

## Example 62: Multimethods with Hierarchies

Define custom type hierarchies for multimethod dispatch.

```mermaid
%% Type hierarchy for multimethods
graph TD
    A[::animal] --> B[::dog]
    A --> C[::cat]
    A --> D[::parrot]
    E[::bird] --> D

    F[speak multimethod] --> G{Dispatch on :type}
    G -->|::dog| H[Woof!]
    G -->|::cat| I[Meow!]
    G -->|::parrot| J[Inherits ::animal]
    J --> K[Some sound]

    style A fill:#0173B2,color:#fff
    style E fill:#0173B2,color:#fff
    style F fill:#DE8F05,color:#000
    style G fill:#029E73,color:#fff
```

```clojure
;; Define hierarchy
(derive ::dog ::animal)
(derive ::cat ::animal)
(derive ::parrot ::animal)
(derive ::parrot ::bird)

;; Multimethod using hierarchy
(defmulti speak (fn [animal] (:type animal)))

(defmethod speak ::dog [_] "Woof!")
(defmethod speak ::cat [_] "Meow!")
(defmethod speak ::animal [_] "Some sound")  ;; => Default for animals

(speak {:type ::dog})                        ;; => "Woof!"
(speak {:type ::parrot})                     ;; => "Some sound" (via ::animal)

;; Check relationships
(isa? ::dog ::animal)                        ;; => true
(isa? ::parrot ::bird)                       ;; => true

;; Get parents/ancestors
(parents ::parrot)                           ;; => #{::animal ::bird}
(ancestors ::dog)                            ;; => #{::animal}
```

**Key Takeaway**: Hierarchies enable rich inheritance relationships for multimethod dispatch.

## Example 63: Component Architecture

Structure applications using component lifecycle management.

```mermaid
%% Component dependency graph
graph TD
    A[System] --> B[Database Component]
    A --> C[Web Server Component]
    C -.depends on.-> B

    B --> D[start: connect]
    B --> E[stop: disconnect]
    C --> F[start: bind port]
    C --> G[stop: unbind]

    H[Start System] --> D
    D --> F
    I[Stop System] --> G
    G --> E

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#DE8F05,color:#000
    style D fill:#029E73,color:#fff
    style F fill:#029E73,color:#fff
```

```clojure
(require '[com.stuartsierra.component :as component])

(defrecord Database [host port connection]
  component/Lifecycle
  (start [this]
    (println "Starting database connection")
    (assoc this :connection {:host host :port port}))
  (stop [this]
    (println "Stopping database connection")
    (assoc this :connection nil)))

(defrecord WebServer [port database handler]
  component/Lifecycle
  (start [this]
    (println "Starting web server on port" port)
    (assoc this :handler {:port port :db (:connection database)}))
  (stop [this]
    (println "Stopping web server")
    (assoc this :handler nil)))

;; Build system
(defn create-system []
  (component/system-map
    :database (map->Database {:host "localhost" :port 5432})
    :web-server (component/using
                  (map->WebServer {:port 8080})
                  {:database :database})))     ;; => Dependency injection

(def system (create-system))
(alter-var-root #'system component/start-system)
;; => Starting database connection, Starting web server on port 8080

(alter-var-root #'system component/stop-system)
;; => Stopping web server, Stopping database connection
```

**Key Takeaway**: Component pattern provides dependency injection and lifecycle management for applications.

## Example 64: Mount for State Management

Alternative to Component using global state with lifecycle.

```clojure
(require '[mount.core :refer [defstate start stop]])

(defstate database
  :start (do
           (println "Connecting to database")
           {:connection "db-conn"})
  :stop (do
          (println "Closing database")
          nil))

(defstate web-server
  :start (do
           (println "Starting server with" database)
           {:server "running" :db database})
  :stop (do
          (println "Stopping server")
          nil))

;; Start all states
(start)                                      ;; => Connects DB, starts server

;; Access state
@web-server                                  ;; => {:server "running" :db {...}}

;; Stop all states
(stop)                                       ;; => Stops server, closes DB

;; Start specific states
(start #'database)                           ;; => Only database
```

**Key Takeaway**: Mount provides simpler state management than Component with global state vars.

## Example 65: Ring Middleware

Build HTTP middleware for request/response transformation.

```mermaid
%% Ring middleware stack
sequenceDiagram
    participant R as Request
    participant L as wrap-logging
    participant A as wrap-auth
    participant H as Handler
    participant Res as Response

    R->>L: Incoming Request
    L->>L: Log request URI
    L->>A: Forward request
    A->>A: Check authorization
    A->>H: Authorized request
    H->>H: Process request
    H-->>A: Generate response
    A-->>L: Pass response
    L->>L: Log response status
    L-->>Res: Final response

    Note over L,A: Middleware applied bottom-up
```

```clojure
(require '[ring.adapter.jetty :refer [run-jetty]])

(defn wrap-logging [handler]
  (fn [request]
    (println "Request:" (:uri request))
    (let [response (handler request)]
      (println "Response:" (:status response))
      response)))

(defn wrap-auth [handler]
  (fn [request]
    (if (get-in request [:headers "authorization"])
      (handler request)
      {:status 401 :body "Unauthorized"})))

;; Base handler
(defn app [request]
  {:status 200 :body "Hello, World!"})

;; Compose middleware (bottom-up application)
(def wrapped-app
  (-> app
      wrap-auth                              ;; => Applied first (inner)
      wrap-logging))                         ;; => Applied second (outer)

;; (run-jetty wrapped-app {:port 8080})
```

**Key Takeaway**: Ring middleware wraps handlers for cross-cutting concerns like auth and logging.

## Example 66: Compojure Routing

Define routes with Compojure DSL for web applications.

```clojure
(require '[compojure.core :refer [defroutes GET POST]]
         '[compojure.route :as route])

(defroutes app-routes
  (GET "/" [] "Home page")                   ;; => Route pattern

  (GET "/users/:id" [id]                     ;; => Path parameter
    (str "User ID: " id))

  (GET "/search" [q limit]                   ;; => Query parameters
    (str "Search: " q " (limit: " limit ")"))

  (POST "/users" [name email]                ;; => POST route
    {:status 201
     :body (str "Created user: " name)})

  (route/not-found "Not found"))             ;; => 404 handler

;; Request/response format
;; Request: {:uri "/users/123" :request-method :get}
;; Response: {:status 200 :body "..." :headers {...}}
```

**Key Takeaway**: Compojure provides concise DSL for HTTP routing with parameter extraction.

## Example 67: HTTP Client with clj-http

Make HTTP requests using clj-http library.

```clojure
(require '[clj-http.client :as http])

;; GET request
(let [response (http/get "https://api.example.com/users/1")]
  (println (:status response))               ;; => 200
  (println (:body response)))                ;; => JSON string

;; GET with query parameters
(http/get "https://api.example.com/search"
          {:query-params {:q "clojure" :limit 10}})

;; POST with JSON
(http/post "https://api.example.com/users"
           {:content-type :json
            :body (json/write-str {:name "Alice" :email "alice@example.com"})})

;; Automatic JSON parsing
(http/get "https://api.example.com/users/1"
          {:as :json})                       ;; => Parses response as JSON

;; Headers and authentication
(http/get "https://api.example.com/private"
          {:headers {"Authorization" "Bearer TOKEN"}
           :throw-exceptions false})         ;; => Don't throw on 4xx/5xx
```

**Key Takeaway**: clj-http simplifies HTTP requests with automatic JSON handling and configuration.

## Example 68: Database Access with next.jdbc

Access relational databases using next.jdbc.

```clojure
(require '[next.jdbc :as jdbc]
         '[next.jdbc.sql :as sql])

(def db {:dbtype "postgresql"
         :dbname "myapp"
         :host "localhost"
         :user "postgres"
         :password "secret"})

;; Query
(sql/query db ["SELECT * FROM users WHERE id = ?" 1])
;; => [{:users/id 1 :users/name "Alice" :users/email "..."}]

;; Insert
(sql/insert! db :users {:name "Bob" :email "bob@example.com"})
;; => {:users/id 2 :users/name "Bob" :users/email "bob@example.com"}

;; Update
(sql/update! db :users {:email "newemail@example.com"} ["id = ?" 1])

;; Delete
(sql/delete! db :users ["id = ?" 2])

;; Transaction
(jdbc/with-transaction [tx db]
  (sql/insert! tx :users {:name "Charlie"})
  (sql/insert! tx :orders {:user_id 3 :amount 100}))
;; => All-or-nothing commit
```

**Key Takeaway**: next.jdbc provides modern JDBC wrapper with transactions and named parameters.

## Example 69: Spec Generative Testing

Generate test data automatically from specs.

```mermaid
%% Spec generative testing flow
graph TD
    A[Spec Definition] --> B[s/def specs]
    B --> C[s/gen Generator]
    C --> D[gen/sample]
    D --> E[Random Valid Data]

    A --> F[s/fdef Function Spec]
    F --> G[stest/check]
    G --> H[Generate Inputs]
    H --> I[Run Function]
    I --> J[Validate Output]
    J --> K{Valid?}
    K -->|Yes| L[Test Passes]
    K -->|No| M[Counterexample Found]

    style A fill:#0173B2,color:#fff
    style C fill:#DE8F05,color:#000
    style G fill:#DE8F05,color:#000
    style L fill:#029E73,color:#fff
    style M fill:#CC78BC,color:#000
```

```clojure
(require '[clojure.spec.alpha :as s]
         '[clojure.spec.gen.alpha :as gen]
         '[clojure.spec.test.alpha :as stest])

(s/def ::age (s/and int? #(<= 0 % 120)))
(s/def ::name (s/and string? #(< 0 (count %) 50)))
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))

(s/def ::user (s/keys :req [::name ::age ::email]))

;; Generate sample data
(gen/sample (s/gen ::age))                   ;; => [0 1 0 0 2 ...]
(gen/sample (s/gen ::user))
;; => [{::name "a" ::age 0 ::email "a@b.c"} ...]

;; Property-based testing
(s/fdef create-user
  :args (s/cat :name ::name :age ::age :email ::email)
  :ret ::user)

(defn create-user [name age email]
  {::name name ::age age ::email email})

(stest/check `create-user {:num-tests 100})  ;; => Run 100 generated tests
```

**Key Takeaway**: Spec generators enable automatic property-based testing from specifications.

## Example 70: test.check for Property Testing

Write generative property-based tests.

```clojure
(require '[clojure.test.check :as tc]
         '[clojure.test.check.generators :as gen]
         '[clojure.test.check.properties :as prop])

;; Property: reverse twice equals original
(def reverse-property
  (prop/for-all [v (gen/vector gen/int)]
    (= v (reverse (reverse v)))))

(tc/quick-check 100 reverse-property)        ;; => Run 100 random tests

;; Property: sort is idempotent
(def sort-property
  (prop/for-all [v (gen/vector gen/int)]
    (= (sort v) (sort (sort v)))))

(tc/quick-check 100 sort-property)

;; Custom generators
(def email-gen
  (gen/fmap
    (fn [[name domain]] (str name "@" domain ".com"))
    (gen/tuple
      (gen/not-empty gen/string-alphanumeric)
      (gen/not-empty gen/string-alphanumeric))))

(gen/sample email-gen)
;; => ["a@b.com" "c@d.com" ...]
```

**Key Takeaway**: test.check enables property-based testing with custom generators.

## Example 71: Performance Profiling

Profile code to identify performance bottlenecks.

```mermaid
%% Performance profiling workflow
graph TD
    A[Code to Profile] --> B[crit/bench]
    B --> C[JVM Warmup]
    C --> D[Multiple Iterations]
    D --> E[Statistical Analysis]
    E --> F[Timing Report]

    A --> G[time macro]
    G --> H[Single Execution]
    H --> I[Elapsed Time]

    A --> J[Memory Profiling]
    J --> K[Before Memory]
    K --> L[Execute Function]
    L --> M[After Memory]
    M --> N[Memory Delta]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style F fill:#029E73,color:#fff
    style I fill:#029E73,color:#fff
    style N fill:#029E73,color:#fff
```

```clojure
(require '[criterium.core :as crit])

;; Benchmark function
(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(crit/bench (fib 20))                        ;; => Detailed timing statistics

;; Quick benchmark
(crit/quick-bench (reduce + (range 10000)))

;; Profile with time
(time (reduce + (range 1000000)))            ;; => "Elapsed time: X msecs"

;; Memory profiling
(defn measure-memory [f]
  (let [runtime (Runtime/getRuntime)
        before (.totalMemory runtime)]
    (f)
    (let [after (.totalMemory runtime)]
      (- after before))))

(measure-memory #(vec (range 1000000)))      ;; => Memory used in bytes
```

**Key Takeaway**: Criterium provides accurate benchmarking accounting for JVM warmup and GC.

## Example 72: Memoization for Performance

Cache function results for repeated calls with same arguments.

```mermaid
%% Memoization cache flow
graph TD
    A[Function Call] --> B{In Cache?}
    B -->|Yes| C[Return Cached Result]
    B -->|No| D[Compute Result]
    D --> E[Store in Cache]
    E --> F[Return Result]

    G[First Call fib 35] --> D
    D --> H[~5 seconds]
    H --> E
    I[Second Call fib 35] --> B
    B --> C
    C --> J[Instant: 0ms]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#029E73,color:#fff
    style D fill:#CA9161,color:#000
    style F fill:#029E73,color:#fff
```

```clojure
(defn slow-fib [n]
  (if (<= n 1)
    n
    (+ (slow-fib (- n 1)) (slow-fib (- n 2)))))

(time (slow-fib 35))                         ;; => ~5 seconds

;; Memoized version
(def fast-fib (memoize slow-fib))

(time (fast-fib 35))                         ;; => ~5 seconds first time
(time (fast-fib 35))                         ;; => Instant (cached)

;; Custom memoization with limited cache
(defn memoize-limited [f limit]
  (let [cache (atom {})]
    (fn [& args]
      (if-let [result (get @cache args)]
        result
        (let [result (apply f args)]
          (swap! cache (fn [c]
                        (if (>= (count c) limit)
                          {args result}      ;; => Evict old entries
                          (assoc c args result))))
          result)))))

(def limited-fib (memoize-limited slow-fib 10))
```

**Key Takeaway**: Memoization trades memory for speed caching expensive computation results.

## Example 73: AOT Compilation

Ahead-of-time compile for faster startup and deployment.

```clojure
;; project.clj
{:aot [myapp.core]                           ;; => Compile this namespace
 :main myapp.core                            ;; => Main entry point
 :uberjar-name "myapp-standalone.jar"}

;; myapp/core.clj
(ns myapp.core
  (:gen-class))                              ;; => Required for -main

(defn -main [& args]
  (println "Starting application...")
  (println "Args:" args))

;; Compile
;; lein compile
;; lein uberjar

;; Run
;; java -jar target/myapp-standalone.jar arg1 arg2
```

**Key Takeaway**: AOT compilation produces standalone JARs with faster startup times.

## Example 74: Logging with timbre

Structured logging with timbre library.

```clojure
(require '[taoensso.timbre :as log])

;; Configure logging
(log/set-level! :info)                       ;; => Set global level

;; Log messages
(log/info "Application started")             ;; => [INFO] Application started
(log/warn "Low memory" {:free-mb 50})        ;; => [WARN] Low memory {...}
(log/error "Connection failed" (Exception. "Timeout"))

;; Structured logging
(log/info {:event :user-login
           :user-id 123
           :ip "192.168.1.1"})

;; Conditional logging
(log/spy (* 2 3))                            ;; => Logs and returns (* 2 3) => 6

;; Custom appenders
(log/merge-config!
  {:appenders
   {:file {:enabled? true
           :fn (fn [data]
                 (spit "app.log"
                       (str (:output_ data) "\n")
                       :append true))}}})
```

**Key Takeaway**: Timbre provides flexible logging with structured data and custom appenders.

## Example 75: JSON and EDN Parsing

Parse and generate JSON and EDN data formats.

```clojure
(require '[clojure.data.json :as json]
         '[clojure.edn :as edn])

;; JSON
(def json-str "{\"name\":\"Alice\",\"age\":30}")
(json/read-str json-str)                     ;; => {"name" "Alice" "age" 30}
(json/read-str json-str :key-fn keyword)     ;; => {:name "Alice" :age 30}

(json/write-str {:name "Bob" :age 25})       ;; => "{\"name\":\"Bob\",\"age\":25}"

;; EDN (Clojure data format)
(def edn-str "{:name \"Alice\" :age 30 :roles #{:admin :user}}")
(edn/read-string edn-str)                    ;; => {:name "Alice" :age 30 :roles #{:admin :user}}

(pr-str {:name "Bob" :age 25})               ;; => "{:name \"Bob\", :age 25}"

;; EDN supports more types than JSON
(pr-str {:date #inst "2025-12-30"
         :uuid #uuid "550e8400-e29b-41d4-a716-446655440000"
         :tags #{:clojure :lisp}})
```

**Key Takeaway**: EDN preserves Clojure types better than JSON for Clojure-to-Clojure communication.

## Example 76: Building Uberjars

Package application with all dependencies into standalone JAR.

```clojure
;; deps.edn approach
{:aliases
 {:uberjar
  {:replace-deps {com.github.seancorfield/depstar {:mvn/version "2.1.303"}}
   :exec-fn hf.depstar/uberjar
   :exec-args {:jar "target/myapp.jar"
               :main-class myapp.core
               :aot true}}}}

;; Build uberjar
;; clj -X:uberjar

;; Run
;; java -jar target/myapp.jar

;; leiningen approach (project.clj)
{:main myapp.core
 :aot [myapp.core]
 :uberjar-name "myapp-standalone.jar"}

;; lein uberjar
```

**Key Takeaway**: Uberjars bundle application and dependencies for simple deployment.

## Example 77: Environment Configuration

Manage environment-specific configuration.

```clojure
(require '[environ.core :refer [env]])

;; Read from environment variables
(def db-url (env :database-url))             ;; => From DATABASE_URL env var
(def port (Integer/parseInt (env :port "8080"))) ;; => Default 8080

;; .lein-env for development
;; {:database-url "jdbc:postgresql://localhost/dev"
;;  :port "3000"}

;; Production env vars
;; export DATABASE_URL=jdbc:postgresql://prod-host/db
;; export PORT=8080

;; Config map pattern
(defn load-config []
  {:database {:url (env :database-url)
              :user (env :db-user)
              :password (env :db-password)}
   :server {:port (Integer/parseInt (env :port "8080"))
            :host (env :host "0.0.0.0")}
   :logging {:level (keyword (env :log-level "info"))}})

(def config (load-config))
```

**Key Takeaway**: Environment variables enable configuration without code changes across environments.

## Example 78: Production Deployment Checklist

Best practices for deploying Clojure applications to production.

```mermaid
%% Production deployment pipeline
graph TD
    A[Development] --> B[AOT Compilation]
    B --> C[Build Uberjar]
    C --> D[JVM Tuning]
    D --> E[Configure Logging]
    E --> F[Health Checks]
    F --> G[Graceful Shutdown]
    G --> H[Error Handling]
    H --> I[Connection Pooling]
    I --> J[Deploy to Production]
    J --> K[Monitor & Alert]

    style A fill:#0173B2,color:#fff
    style C fill:#DE8F05,color:#000
    style F fill:#029E73,color:#fff
    style J fill:#029E73,color:#fff
    style K fill:#CC78BC,color:#000
```

```clojure
;; 1. AOT compilation for faster startup
;;    :aot [myapp.core] in project.clj

;; 2. Uberjar for standalone deployment
;;    lein uberjar or clj -X:uberjar

;; 3. JVM tuning
;;    java -Xmx2g -server -jar myapp.jar

;; 4. Logging configuration
(log/set-level! (keyword (env :log-level "info")))
(log/merge-config!
  {:appenders {:file {:enabled? true}}})

;; 5. Health check endpoint
(GET "/health" []
  {:status 200 :body "OK"})

;; 6. Graceful shutdown
(defn shutdown-hook []
  (println "Shutting down...")
  (stop))                                    ;; => Stop components

(.addShutdownHook (Runtime/getRuntime)
                  (Thread. shutdown-hook))

;; 7. Error handling and monitoring
(defn wrap-error-handling [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception e
        (log/error e "Request failed")
        {:status 500 :body "Internal error"}))))

;; 8. Database connection pooling
;; Use HikariCP with next.jdbc
```

**Key Takeaway**: Production deployment requires AOT compilation, proper JVM tuning, logging, and error handling.

## Example 79: ClojureScript Basics

Write frontend code in ClojureScript compiling to JavaScript.

```clojure
;; src/myapp/core.cljs
(ns myapp.core
  (:require [reagent.core :as r]))

;; Reagent component (React wrapper)
(defn counter []
  (let [count (r/atom 0)]                    ;; => Reagent atom (reactive)
    (fn []
      [:div
       [:h1 "Count: " @count]
       [:button {:on-click #(swap! count inc)} "Increment"]])))

(defn mount-app []
  (r/render [counter]
            (.getElementById js/document "app")))

(mount-app)

;; JavaScript interop
(.log js/console "Hello from ClojureScript")
(set! (.-title js/document) "My App")
```

**Key Takeaway**: ClojureScript brings Clojure to browser with React integration via Reagent.

## Example 80: Best Practices - Immutability and Pure Functions

Embrace functional programming with immutability and purity.

```clojure
;; Pure functions (no side effects)
(defn calculate-total [items]
  (reduce + (map :price items)))             ;; => Deterministic, no mutations

;; Immutable updates
(def user {:name "Alice" :age 30})

(assoc user :email "alice@example.com")      ;; => New map, original unchanged
(update user :age inc)                       ;; => New map with age+1

;; Threading for readability
(->> users
     (filter :active)
     (map :email)
     (take 10))                              ;; => Thread-last macro

(-> request
    (assoc :user current-user)
    (update :headers merge auth-headers))    ;; => Thread-first macro

;; Avoid mutations
(defn bad-add! [coll item]
  (.add coll item))                          ;; => Mutates collection!

(defn good-add [coll item]
  (conj coll item))                          ;; => Returns new collection

;; Persistent data structures
(def v [1 2 3])
(def v2 (conj v 4))                          ;; => [1 2 3 4] (shares structure)
(identical? (pop v2) v)                      ;; => false (different objects)
(= (pop v2) v)                               ;; => true (equal values)
```

**Key Takeaway**: Immutability and pure functions eliminate entire categories of bugs and enable safe concurrency.

## Summary

Advanced Clojure (examples 55-80) covers expert-level techniques: advanced macros with code walking, multiplatform development with reader conditionals, performance optimization through type hints and reducers, component architecture for application structure, web development with Ring/Compojure, database access, property-based testing, profiling and optimization, deployment best practices, ClojureScript for frontend development, and functional programming principles. Master these techniques to write production-grade Clojure systems operating at 95% language coverage.
