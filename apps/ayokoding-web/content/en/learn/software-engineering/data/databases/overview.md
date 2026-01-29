---
title: Overview
weight: 10000
date: 2025-12-23T00:00:00+07:00
draft: false
description: Master SQL fundamentals, PostgreSQL, and Datomic immutable databases
---

Databases are specialized systems for storing, organizing, and retrieving data efficiently. This section covers relational databases (SQL, PostgreSQL) and immutable databases (Datomic).

## What You'll Learn

- **SQL** - The standard language for relational database querying and manipulation
- **PostgreSQL** - Advanced open-source relational database with powerful features
- **Datomic** - Immutable database with time-travel queries and datalog
- **Database Design** - Schema design, normalization, and indexing strategies
- **Query Optimization** - Performance tuning and query analysis
- **Transactions** - ACID properties, isolation levels, and concurrency control

## Available Databases

### SQL - Universal Database Language

**[SQL](/en/learn/software-engineering/data/databases/sql)** provides the foundation for working with relational databases:

- **Query Fundamentals** - SELECT, WHERE, JOIN for data retrieval
- **Data Manipulation** - INSERT, UPDATE, DELETE operations
- **Schema Definition** - CREATE TABLE, indexes, constraints
- **Aggregation** - GROUP BY, HAVING, aggregate functions
- **Advanced Queries** - Subqueries, window functions, CTEs
- **Transactions** - BEGIN, COMMIT, ROLLBACK

### PostgreSQL - Production Database System

**[PostgreSQL](/en/learn/software-engineering/data/databases/postgresql)** extends SQL with advanced features:

- **Advanced Data Types** - JSON, arrays, ranges, custom types
- **Performance Features** - Parallel queries, partitioning, materialized views
- **Full-Text Search** - Built-in text search capabilities
- **Extensions** - PostGIS for geospatial, pg_trgm for fuzzy matching
- **Replication** - Streaming replication, logical replication
- **Administration** - Backup strategies, performance tuning, monitoring

### Datomic - Immutable Database

**[Datomic](/en/learn/software-engineering/data/databases/datomic)** provides immutable data storage with powerful temporal features:

- **Immutability** - Facts are never deleted, only accumulated over time
- **Time-Travel Queries** - Query database at any point in history with `as-of`, `since`, `history`
- **Datalog Query Language** - Declarative, composable queries based on logic programming
- **ACID Transactions** - Strong consistency with serializable isolation
- **Flexible Schema** - Schema evolves incrementally without migrations
- **Audit Trail** - Every fact timestamped and preserved forever

## Learning Path

### For Relational Databases

1. **Start with SQL fundamentals** - Learn the universal language of relational databases
2. **Explore PostgreSQL** - Apply SQL knowledge to a production database with advanced features

### For Immutable Databases

1. **Start with Datomic** - Learn immutable data storage, datalog queries, and time-travel

All databases provide **By Example** tutorials:

- **Beginner** - Core concepts and basic operations (0-40% coverage)
- **Intermediate** - Production patterns and optimization (40-75% coverage)
- **Advanced** - Expert techniques and administration (75-95% coverage)

## Database Operations

### Relational Databases (SQL, PostgreSQL)

- **Schema Design** - Normalization, relationships, constraints
- **Query Optimization** - Indexes, EXPLAIN plans, query tuning
- **Transactions** - ACID guarantees, isolation levels, deadlocks
- **Backup & Recovery** - pg_dump, WAL archiving, point-in-time recovery
- **Security** - User management, role-based access, SSL connections
- **Monitoring** - pg_stat views, performance metrics, logging

### Immutable Databases (Datomic)

- **Schema Evolution** - Additive schema changes without migrations
- **Datalog Queries** - Pattern matching, joins, rules, aggregates
- **Time Queries** - as-of, since, history for temporal navigation
- **Transaction Functions** - Database functions executing inside transactions
- **Audit Trails** - Transaction metadata and change tracking
- **Performance** - Index selection, caching, query optimization

## Getting Started

### Relational Databases

Begin with **[SQL](/en/learn/software-engineering/data/databases/sql)** to build a strong foundation in relational database concepts. Then explore **[PostgreSQL](/en/learn/software-engineering/data/databases/postgresql)** to learn production database features and administration.

### Immutable Databases

Start with **[Datomic](/en/learn/software-engineering/data/databases/datomic)** to learn immutable data storage, datalog queries, and time-travel capabilities.

### Choosing Your Database

- **Use SQL/PostgreSQL** for traditional CRUD applications, complex queries, mature ecosystem
- **Use Datomic** for audit trails, event sourcing, temporal queries, immutable data requirements

All paths include practical, annotated examples you can run immediately.
