---
title: Overview
weight: 1000000
date: 2025-12-29T08:08:50+07:00
draft: false
description: Learn PostgreSQL, the powerful open-source relational database
---

**PostgreSQL is a powerful, open-source relational database** known for its robustness, extensibility, and standards compliance. It excels at handling complex queries, supporting advanced data types (JSON, arrays, ranges), and providing enterprise-grade features like full-text search, partitioning, and logical replication.

## What Is PostgreSQL

PostgreSQL (often called "Postgres") is an object-relational database management system (ORDBMS) that emphasizes extensibility and SQL compliance. Originally developed at UC Berkeley in 1986, it has evolved into one of the most advanced open-source databases, trusted by organizations worldwide for mission-critical applications.

Key characteristics:

- **ACID Compliance** - Atomic, Consistent, Isolated, Durable transactions guarantee data integrity
- **Extensibility** - Custom data types, operators, functions, and extensions (PostGIS, TimescaleDB)
- **Advanced Data Types** - JSON/JSONB, arrays, ranges, geometric types, full-text search
- **Concurrency** - Multi-Version Concurrency Control (MVCC) enables high read/write throughput
- **Standards Compliance** - Extensive SQL standard support with advanced features

## What You'll Learn

Through our PostgreSQL tutorials, you'll master:

### Database Fundamentals

- SQL basics: SELECT, INSERT, UPDATE, DELETE, WHERE, ORDER BY, LIMIT
- Data types: Numeric, text, temporal, boolean, UUID, NULL handling
- Schema design: Tables, primary keys, foreign keys, constraints, indexes
- Joins: INNER, LEFT, RIGHT, FULL OUTER, self joins

### Production Patterns

- Advanced queries: CTEs, window functions, recursive queries, set operations
- Indexes: B-tree, GIN, GiST, partial, expression, covering indexes
- Transactions: ACID properties, isolation levels, savepoints, deadlock handling
- JSON support: JSONB operators, indexing, querying nested data

### Advanced Features

- Full-text search: tsvector, tsquery, ranking, highlighting
- Partitioning: Range and list partitioning for scalability
- Replication: Logical replication for high availability
- Performance: Query optimization, EXPLAIN ANALYZE, VACUUM, statistics

### Administration

- User management: Roles, permissions, row-level security
- Backup/restore: pg_dump, pg_restore, point-in-time recovery
- Monitoring: pg_stat views, connection pooling, WAL tuning
- Security: Authentication, encryption, RLS policies

## Learning Paths

### By-Example Tutorial (Code-First)

Learn PostgreSQL through **85 annotated SQL examples** covering 95% of the database - ideal for experienced developers who prefer learning through working code rather than narrative explanations.

- **[PostgreSQL By-Example](/en/learn/software-engineering/data/databases/postgresql/tutorials/by-example)** - Start here for rapid, hands-on learning

What you'll get:

- Self-contained, copy-paste-runnable examples in Docker containers
- Heavy annotations showing query results, table states, and behaviors
- Progressive complexity: Beginner (30 examples) → Intermediate (30 examples) → Advanced (25 examples)
- Production-ready patterns and best practices
- Mermaid diagrams for complex concepts

## Prerequisites and Getting Started

### Prerequisites

- Basic SQL knowledge (SELECT, INSERT, UPDATE, DELETE) or willingness to learn through examples
- Docker installed and running (for PostgreSQL container)
- A terminal or SQL client (psql, pgAdmin, DBeaver, etc.)

No prior PostgreSQL experience required - our tutorials start from fundamentals and progress to advanced topics.

### Quick Start

Get PostgreSQL running in Docker:

```bash
# Create PostgreSQL 16 container
docker run --name postgres-tutorial \
  -e POSTGRES_PASSWORD=password \
  -p 5432:5432 \
  -d postgres:16

# Connect to PostgreSQL
docker exec -it postgres-tutorial psql -U postgres
```

Now you're ready to follow along with our by-example tutorials.

## Why PostgreSQL

### When to Choose PostgreSQL

PostgreSQL excels in scenarios requiring:

- **Complex queries and analytics** - Advanced SQL features, window functions, CTEs
- **Data integrity** - ACID compliance, constraints, foreign keys, transactions
- **Flexible data models** - JSON/JSONB for semi-structured data alongside relational tables
- **Extensibility** - Custom types, functions, operators, and rich extension ecosystem
- **Scalability** - Partitioning, replication, connection pooling for large datasets
- **Open-source freedom** - No licensing costs, community-driven development

### PostgreSQL vs Other Databases

- **vs MySQL** - PostgreSQL offers better SQL compliance, advanced features (CTEs, window functions), and extensibility
- **vs MongoDB** - PostgreSQL's JSONB provides NoSQL flexibility with ACID guarantees and SQL querying
- **vs Oracle** - PostgreSQL delivers enterprise features (partitioning, replication, full-text search) without licensing costs
- **vs SQLite** - PostgreSQL handles concurrent writes, network access, and production workloads better

## Next Steps

Start your PostgreSQL journey:

1. **[PostgreSQL By-Example Overview](/en/learn/software-engineering/data/databases/postgresql/tutorials/by-example/overview)** - Understand the by-example approach
2. **[Beginner Examples](/en/learn/software-engineering/data/databases/postgresql/tutorials/by-example/beginner)** - Master fundamentals (Examples 1-30)
3. **[Intermediate Examples](/en/learn/software-engineering/data/databases/postgresql/tutorials/by-example/intermediate)** - Production patterns (Examples 31-60)
4. **[Advanced Examples](/en/learn/software-engineering/data/databases/postgresql/tutorials/by-example/advanced)** - Expert mastery (Examples 61-85)

Prefer narrative learning? Traditional beginner, intermediate, and advanced tutorials coming soon.

## Community and Resources

- [Official PostgreSQL Documentation](https://www.postgresql.org/docs/)
- [PostgreSQL Wiki](https://wiki.postgresql.org/)
- [Planet PostgreSQL](https://planet.postgresql.org/) - Blog aggregator
- [PostgreSQL Mailing Lists](https://www.postgresql.org/list/)
- [Stack Overflow PostgreSQL Tag](https://stackoverflow.com/questions/tagged/postgresql)
