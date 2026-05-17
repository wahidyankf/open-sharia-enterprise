---
title: "Overview"
date: 2026-03-27T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn Clojure Migratus through annotated code examples covering database migration patterns - ideal for experienced developers building production Clojure applications"
tags: ["clojure-migratus", "tutorial", "by-example", "examples", "code-first", "migratus", "database", "migrations"]
---

## What is Clojure Migratus By Example?

**Clojure Migratus By Example** is a code-first tutorial series teaching experienced Clojure developers how to manage database schema evolution using Migratus. Through heavily annotated, self-contained examples, you will achieve 40% coverage of Migratus patterns at the beginner level—from configuration and SQL migration files to running, rolling back, and inspecting pending migrations.

This tutorial assumes you are an experienced developer familiar with Clojure, deps.edn, next.jdbc, and relational databases. If you are new to Clojure, start with foundational Clojure tutorials first.

## Why By Example?

**Philosophy**: Show the code first, run it second, understand through direct interaction.

Traditional tutorials explain concepts then show code. By-example tutorials reverse this: every example is a working, runnable code snippet with inline annotations showing exactly what happens at each step—config maps, SQL file contents, migration states, and REPL output.

**Target Audience**: Experienced developers who:

- Already know Clojure fundamentals (namespaces, maps, deps.edn)
- Understand relational databases and SQL DDL
- Prefer learning through working code rather than narrative explanations
- Want comprehensive reference material covering production migration patterns

**Not For**: Developers new to Clojure or SQL. This tutorial moves quickly and assumes foundational knowledge.

## What Does Coverage Mean?

**Coverage** means the depth and breadth of Migratus features needed for production work, not toy examples.

### Included in Beginner Coverage (0-40%)

- **Configuration**: Config map structure, :store :database, :migration-dir, JDBC URIs
- **Migration Files**: Naming conventions, up/down SQL pairs, file placement
- **Core Operations**: migrate, up, down, create, pending-list
- **Common DDL Patterns**: CREATE TABLE, ADD COLUMN, ADD INDEX, foreign keys, constraints
- **Schema Tracking**: schema_migrations table structure and behavior
- **Data Seeding**: INSERT statements in migration files
- **Safety Guards**: IF NOT EXISTS, IF EXISTS, cascade behavior
- **deps.edn Integration**: Declaring the Migratus dependency

### Excluded from Beginner Coverage

- **Multiple Stores**: In-memory, filesystem stores (rarely used in production)
- **Custom Reporters**: Progress callbacks and custom logging hooks
- **Advanced Transactions**: Per-migration transaction control flags
- **Migration Scripting**: Clojure-based (non-SQL) migration files
- **Programmatic Discovery**: Dynamic migration directory resolution

## Tutorial Structure

### Examples Across One Level

**Beginner** (Examples 1-30): 0-40% coverage

- Configuration and file naming (Examples 1-5)
- Core API operations (Examples 6-10)
- Table and column DDL (Examples 11-16)
- Connection and type patterns (Examples 17-23)
- Structural patterns and safety (Examples 24-30)

## Five-Part Example Format

Every example follows a **mandatory five-part structure**:

### Part 1: Brief Explanation (2-3 sentences)

Answers what the concept is, why it matters in production code, and when to use it.

### Part 2: Mermaid Diagram (when appropriate)

Included when data flow or relationships are non-obvious. Uses the color-blind-friendly palette:

- Blue `#0173B2`, Orange `#DE8F05`, Teal `#029E73`, Purple `#CC78BC`, Brown `#CA9161`

### Part 3: Heavily Annotated Code

Every significant line has an inline comment. Clojure annotations use `; =>` notation. SQL annotations use `-- =>` notation.

```clojure
(def config
  {:store         :database         ; => Use the database store (SQL-based)
   :migration-dir "migrations"      ; => Relative to classpath root (resources/)
   :db            {:connection-uri uri}}) ; => next.jdbc-compatible JDBC URI map
```

### Part 4: Key Takeaway (1-2 sentences)

Distills the core insight and when to apply it in production.

### Part 5: Why It Matters (50-100 words)

Explains the production relevance, common pitfalls, and consequences of ignoring the pattern.

## Self-Containment Rules

Each example must be copy-paste-runnable within its chapter scope:

- Full config map or SQL file content shown
- No references to code outside the example
- Clojure REPL snippets include all required `require` calls
- SQL files shown in full (no ellipsis)

## Code Annotation Philosophy

Every example uses educational annotations to show exactly what happens:

```clojure
(migratus/migrate config)  ; => Runs all pending migrations in order
                           ; => Reads files from resources/migrations/
                           ; => SQL: SELECT id FROM schema_migrations
                           ; => Output: Migrating 001-create-users
```

Annotations show:

- **Config values** and what each key controls
- **File system effects** (which files are read/created)
- **SQL executed** by Migratus internally
- **Return values** and REPL output
- **Common gotchas** and sequencing constraints

## Quality Standards

Every example in this tutorial meets these standards:

- **Self-contained**: Copy-paste-runnable within chapter scope
- **Annotated**: Every significant line has an inline comment (1.0-2.25 ratio per example)
- **Production-relevant**: Real-world patterns based on actual Clojure project usage
- **Accessible**: Color-blind-friendly diagrams, clear structure

## Next Steps

Ready to start? Begin with [Beginner Examples (1-30)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner) to learn Migratus from configuration through advanced DDL patterns.

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: Migratus Config Map](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-1-migratus-config-map)
- [Example 2: First Migration Pair (up.sql / down.sql)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-2-first-migration-pair-upsql--downsql)
- [Example 3: Migration File Naming Convention](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-3-migration-file-naming-convention)
- [Example 4: :store :database Configuration](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-4-store-database-configuration)
- [Example 5: :migration-dir Setting](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-5-migration-dir-setting)
- [Example 6: Running All Migrations (migratus/migrate)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-6-running-all-migrations-migratusmigrate)
- [Example 7: Running Single Migration Up (migratus/up)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-7-running-single-migration-up-migratusup)
- [Example 8: Rolling Back Single Migration (migratus/down)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-8-rolling-back-single-migration-migratusdown)
- [Example 9: Creating New Migration (migratus/create)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-9-creating-new-migration-migratuscreate)
- [Example 10: Checking Pending Migrations (migratus/pending-list)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-10-checking-pending-migrations-migratuspending-list)
- [Example 11: Creating Tables](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-11-creating-tables)
- [Example 12: Adding Columns](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-12-adding-columns)
- [Example 13: Adding Indexes](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-13-adding-indexes)
- [Example 14: Adding Foreign Keys](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-14-adding-foreign-keys)
- [Example 15: Adding Unique Constraints](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-15-adding-unique-constraints)
- [Example 16: schema_migrations Table Structure](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-16-schema_migrations-table-structure)
- [Example 17: JDBC Connection String Setup](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-17-jdbc-connection-string-setup)
- [Example 18: NOT NULL with Default Values](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-18-not-null-with-default-values)
- [Example 19: UUID Primary Keys (PostgreSQL)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-19-uuid-primary-keys-postgresql)
- [Example 20: Timestamp Columns with Defaults](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-20-timestamp-columns-with-defaults)
- [Example 21: Enum Types via SQL](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-21-enum-types-via-sql)
- [Example 22: CHECK Constraints](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-22-check-constraints)
- [Example 23: Composite Indexes](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-23-composite-indexes)
- [Example 24: Junction Tables (Many-to-Many)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-24-junction-tables-many-to-many)
- [Example 25: Seed Data in Migrations](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-25-seed-data-in-migrations)
- [Example 26: Multiple Statements in One File](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-26-multiple-statements-in-one-file)
- [Example 27: IF NOT EXISTS Guards](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-27-if-not-exists-guards)
- [Example 28: Cascade Delete Foreign Keys](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-28-cascade-delete-foreign-keys)
- [Example 29: Dropping Tables/Columns Safely](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-29-dropping-tablescolumns-safely)
- [Example 30: deps.edn Dependency Declaration](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/beginner#example-30-depsedn-dependency-declaration)

### Intermediate (Examples 31–60)

- [Example 31: Clojure-Based Migrations (.clj Files)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-31-clojure-based-migrations-clj-files)
- [Example 32: Transaction Wrapping in Migrations](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-32-transaction-wrapping-in-migrations)
- [Example 33: Rollback Command (migratus/rollback)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-33-rollback-command-migratusrollback)
- [Example 34: Reset Command (migratus/reset)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-34-reset-command-migratusreset)
- [Example 35: Pending Migrations List](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-35-pending-migrations-list)
- [Example 36: Custom Migration Store](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-36-custom-migration-store)
- [Example 37: Data Migration with INSERT...SELECT](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-37-data-migration-with-insertselect)
- [Example 38: Seed Data Pattern](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-38-seed-data-pattern)
- [Example 39: Foreign Key with ON UPDATE CASCADE](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-39-foreign-key-with-on-update-cascade)
- [Example 40: Composite Primary Keys](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-40-composite-primary-keys)
- [Example 41: Partial Indexes](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-41-partial-indexes)
- [Example 42: Full-Text Search Indexes](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-42-full-text-search-indexes)
- [Example 43: Creating Views](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-43-creating-views)
- [Example 44: Creating Materialized Views](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-44-creating-materialized-views)
- [Example 45: Trigger Functions](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-45-trigger-functions)
- [Example 46: Stored Procedures](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-46-stored-procedures)
- [Example 47: Conditional Migration Logic](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-47-conditional-migration-logic)
- [Example 48: Batch Data Migration Pattern](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-48-batch-data-migration-pattern)
- [Example 49: Migration Testing with clojure.test](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-49-migration-testing-with-clojuretest)
- [Example 50: Test Database Setup with Testcontainers](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-50-test-database-setup-with-testcontainers)
- [Example 51: JSON/JSONB Columns](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-51-jsonjsonb-columns)
- [Example 52: Array Columns (PostgreSQL)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-52-array-columns-postgresql)
- [Example 53: GIN Index for JSONB](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-53-gin-index-for-jsonb)
- [Example 54: Table Partitioning](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-54-table-partitioning)
- [Example 55: Generated Columns](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-55-generated-columns)
- [Example 56: Connection Pooling with HikariCP](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-56-connection-pooling-with-hikaricp)
- [Example 57: Migration with next.jdbc](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-57-migration-with-nextjdbc)
- [Example 58: Multi-Database Support](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-58-multi-database-support)
- [Example 59: Migration Error Handling](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-59-migration-error-handling)
- [Example 60: Pedestal Integration Pattern](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/intermediate#example-60-pedestal-integration-pattern)

### Advanced (Examples 61–85)

- [Example 61: Custom Migration Protocol](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-61-custom-migration-protocol)
- [Example 62: Zero-Downtime Column Addition](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-62-zero-downtime-column-addition)
- [Example 63: Zero-Downtime Column Removal (3-Phase)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-63-zero-downtime-column-removal-3-phase)
- [Example 64: Zero-Downtime Table Rename](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-64-zero-downtime-table-rename)
- [Example 65: Large Table Migration with Batched Updates](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-65-large-table-migration-with-batched-updates)
- [Example 66: Online Index Creation (CONCURRENTLY)](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-66-online-index-creation-concurrently)
- [Example 67: Data Backfill Pattern](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-67-data-backfill-pattern)
- [Example 68: Migratus in CI/CD Pipeline](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-68-migratus-in-cicd-pipeline)
- [Example 69: Migration Monitoring with Metrics](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-69-migration-monitoring-with-metrics)
- [Example 70: Migration Rollback Testing](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-70-migration-rollback-testing)
- [Example 71: Blue-Green Deployment Migrations](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-71-blue-green-deployment-migrations)
- [Example 72: Feature Flag Migration Pattern](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-72-feature-flag-migration-pattern)
- [Example 73: Multi-Tenant Schema Migration](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-73-multi-tenant-schema-migration)
- [Example 74: Migration with pgcrypto Encryption](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-74-migration-with-pgcrypto-encryption)
- [Example 75: Audit Trail Table Migration](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-75-audit-trail-table-migration)
- [Example 76: Soft Delete Schema Pattern](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-76-soft-delete-schema-pattern)
- [Example 77: REPL-Driven Migration Development](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-77-repl-driven-migration-development)
- [Example 78: Migration with HugSQL Integration](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-78-migration-with-hugsql-integration)
- [Example 79: Schema Drift Detection](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-79-schema-drift-detection)
- [Example 80: Migration Dependency Graph](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-80-migration-dependency-graph)
- [Example 81: Migration Squashing Pattern](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-81-migration-squashing-pattern)
- [Example 82: Migration Performance Benchmarking](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-82-migration-performance-benchmarking)
- [Example 83: Stored Procedures in Advanced Patterns](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-83-stored-procedures-in-advanced-patterns)
- [Example 84: Production Migration Checklist](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-84-production-migration-checklist)
- [Example 85: Migration Observability and Alerting](/en/learn/software-engineering/data/tools/clojure-migratus/by-example/advanced#example-85-migration-observability-and-alerting)
