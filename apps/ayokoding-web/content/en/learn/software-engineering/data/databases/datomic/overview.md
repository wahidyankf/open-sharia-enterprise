---
title: Overview
weight: 100000
date: 2026-01-29T00:00:00+07:00
draft: false
description: Learn Datomic, the immutable database with time-travel queries and datalog
---

**Datomic is an immutable database** that stores facts as data, enabling time-travel queries, built-in auditing, and a flexible datalog query language. It treats the database as an accumulation of facts over time rather than a mutable store of current state.

## What Is Datomic

Datomic is a distributed database system designed around immutability, separation of reads from writes, and powerful query capabilities through datalog. Created by Rich Hickey (creator of Clojure) and first released in 2012, Datomic rethinks database architecture from first principles.

Key characteristics:

- **Immutability** - Facts are never deleted or modified, only accumulated over time
- **Time-Travel Queries** - Query the database at any point in its history
- **Datalog Query Language** - Declarative, composable queries based on logic programming
- **ACID Transactions** - Strong consistency guarantees with serializable isolation
- **Separation of Concerns** - Reads, writes, and storage are independent services

## What You'll Learn

Through our Datomic tutorials, you'll master:

### Database Fundamentals

- Schema definition: Attributes, entity types, cardinality, value types
- Transactions: Assert facts, retract facts, transaction functions
- Datalog queries: Pattern matching, joins, logic variables, find specs
- Entity API: Navigation through entity relationships as maps
- Pull API: Declarative data fetching with recursive patterns

### Production Patterns

- Time queries: `as-of`, `since`, `history` for temporal navigation
- Transaction metadata: Adding context to every database change
- Schema evolution: Adding attributes, retracting schema, migration patterns
- Optimistic concurrency: Compare-and-swap semantics with `:db/cas`
- Query optimization: Index selection, query planning, parameterization

### Advanced Features

- Data structures: Lists, maps, sets as attribute values
- Rules: Recursive rules, logic programming patterns
- Aggregates: Custom aggregation functions, grouping
- Transaction functions: Database functions that run inside transactions
- Excision: Removing data when legally required (GDPR compliance)

### Administration

- Peer library: Embedded database access within applications
- Client API: Remote access via HTTP or gRPC
- Backup and restore: Point-in-time database snapshots
- Performance tuning: Memory settings, index management
- Storage services: DynamoDB, PostgreSQL, Cassandra backends

## Learning Paths

### By-Example Tutorial (Code-First)

Learn Datomic through **80 annotated examples in both Java and Clojure** covering 95% of the database - ideal for experienced developers who prefer learning through working code rather than narrative explanations.

- **[Datomic By-Example](/en/learn/software-engineering/data/databases/datomic/by-example)** - Start here for rapid, hands-on learning

What you'll get:

- Self-contained, copy-paste-runnable examples using Datomic Free
- Heavy annotations showing query results, database states, and behaviors
- Progressive complexity: Beginner (30 examples) → Intermediate (30 examples) → Advanced (20 examples)
- Production-ready patterns and best practices
- Mermaid diagrams for complex concepts

## Getting Started

Start your Datomic learning journey with these foundational tutorials:

1. **[Initial Setup](/en/learn/software-engineering/data/databases/datomic/initial-setup)** - Install Datomic Free, configure Java/Clojure environment, connect to database
2. **[Quick Start](/en/learn/software-engineering/data/databases/datomic/quick-start)** - Your first transactions, basic datalog queries, essential patterns

These foundational tutorials (0-30% coverage) prepare you for comprehensive by-example learning in both Java and Clojure.

## Prerequisites and Getting Started

### Prerequisites

**For Java developers:**

- Java 8+ installed (Datomic runs on the JVM)
- Maven or Gradle for dependency management
- Familiarity with Java collections and generics
- IDE with Java support (IntelliJ IDEA, Eclipse, VS Code)

**For Clojure developers:**

- Java 8+ installed (Datomic runs on the JVM)
- Leiningen or Clojure CLI tools for project setup
- Basic Clojure knowledge (let, defn, vectors, maps) or willingness to learn
- Familiarity with REPL-driven development

No prior Datomic or database experience required - our tutorials start from fundamentals and progress to advanced topics. All examples are provided in both Java and Clojure so you can learn in your preferred language.

### Quick Start

Get Datomic Free running locally:

**Java Setup (Maven):**

```xml
<!-- Add to pom.xml -->
<dependency>
  <groupId>com.datomic</groupId>
  <artifactId>datomic-free</artifactId>
  <version>0.9.5697</version>
</dependency>
```

**Java Setup (Gradle):**

```gradle
// Add to build.gradle
dependencies {
    implementation 'com.datomic:datomic-free:0.9.5697'
}
```

**Clojure Setup:**

```clojure
;; Add to deps.edn dependencies
{:deps {com.datomic/datomic-free {:mvn/version "0.9.5697"}}}

;; Or add to project.clj
[com.datomic/datomic-free "0.9.5697"]
```

**Java: Connect to Database:**

```java
import datomic.Peer;
import datomic.Connection;

// Create in-memory database
String uri = "datomic:mem://tutorial";
Peer.createDatabase(uri);
Connection conn = Peer.connect(uri);

// You're ready to run examples
```

**Clojure: Connect to Database:**

```clojure
(require '[datomic.api :as d])

;; Create in-memory database
(def uri "datomic:mem://tutorial")
(d/create-database uri)
(def conn (d/connect uri))

;; You're ready to run examples
```

Now you're ready to follow along with our by-example tutorials in your preferred language.

## Why Datomic

### When to Choose Datomic

Datomic excels in scenarios requiring:

- **Audit trails** - Every fact is timestamped and preserved forever
- **Temporal queries** - Business logic needs access to historical data states
- **Event sourcing** - Application architecture built around immutable events
- **Flexible schemas** - Schema evolves incrementally without migrations
- **Complex queries** - Datalog enables recursive, multi-way joins naturally
- **Consistency guarantees** - ACID transactions with serializable isolation

### Datomic vs Other Databases

- **vs PostgreSQL** - Datomic provides time-travel queries and immutability; PostgreSQL offers mature ecosystem and wider adoption
- **vs MongoDB** - Datomic guarantees ACID transactions and strong consistency; MongoDB provides horizontal scaling and simpler deployment
- **vs EventStore** - Datomic offers flexible queries over events; EventStore specializes in event sourcing with projection support
- **vs Traditional SQL** - Datomic stores facts immutably with time; SQL databases mutate state in place
- **vs Cassandra** - Datomic provides strong consistency and time queries; Cassandra offers eventual consistency with massive scale

### Datomic Editions

- **Datomic Free** - Free edition for development and evaluation (single peer, limited storage)
- **Datomic Pro** - Production-ready with horizontal scaling, multiple storage backends
- **Datomic Cloud** - Managed service on AWS with pay-as-you-go pricing

Our tutorials use Datomic Free for universal accessibility. Patterns transfer directly to Pro and Cloud editions.

## Next Steps

Start your Datomic journey:

1. **[Datomic By-Example Overview](/en/learn/software-engineering/data/databases/datomic/by-example/overview)** - Understand the by-example approach
2. **[Beginner Examples](/en/learn/software-engineering/data/databases/datomic/by-example/beginner)** - Master fundamentals (Examples 1-30)
3. **[Intermediate Examples](/en/learn/software-engineering/data/databases/datomic/by-example/intermediate)** - Production patterns (Examples 31-60)
4. **[Advanced Examples](/en/learn/software-engineering/data/databases/datomic/by-example/advanced)** - Expert mastery (Examples 61-80)

Prefer narrative learning? Traditional beginner, intermediate, and advanced tutorials coming soon.

## Community and Resources

- [Official Datomic Documentation](https://docs.datomic.com/)
- [Datomic Forum](https://forum.datomic.com/)
- [Day of Datomic Videos](https://www.youtube.com/playlist?list=PLZdCLR02grLrju9ntDh3RGPpWSWBvjwXg)
- [Learn Datalog Today](http://www.learndatalogtoday.org/) - Interactive datalog tutorial
- [Datomic Blog](https://blog.datomic.com/)
- [Datomic on GitHub](https://github.com/Datomic) - Examples and tools
