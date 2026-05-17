---
title: "Overview"
date: 2026-03-27T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn TypeScript Effect SQL through 80 annotated code examples covering 95% of the library - ideal for experienced developers building production database migration layers"
tags:
  [
    "typescript",
    "effect",
    "effect-sql",
    "tutorial",
    "by-example",
    "database",
    "migrations",
    "pg",
    "sqlite",
    "code-first",
  ]
---

## What is TypeScript Effect SQL By Example?

**TypeScript Effect SQL By Example** is a code-first tutorial series teaching experienced TypeScript developers how to write database migrations using `@effect/sql`, `@effect/sql-pg`, and `@effect/sql-sqlite-node`. Through 80 heavily annotated, self-contained examples, you will achieve 95% coverage of Effect SQL migration patterns—from basic table creation to advanced constraint design, multi-statement migrations, and effect-aware error handling.

This tutorial assumes you are an experienced developer familiar with TypeScript, the Effect library's `Effect.gen` pattern, and relational databases. If you are new to Effect, start with the Effect fundamentals tutorials first.

## Why By Example?

**Philosophy**: Show the code first, run it second, understand through direct interaction.

Traditional tutorials explain concepts then show code. By-example tutorials reverse this: every example is a working, annotated code snippet with inline comments showing exactly what happens at each step—SQL executed, Effect service resolution, migration registry patterns, and common pitfalls.

**Target Audience**: Experienced developers who:

- Already know TypeScript and the Effect ecosystem
- Understand relational databases and SQL DDL
- Prefer learning through working code rather than narrative explanations
- Want comprehensive reference material covering 95% of production migration patterns

**Not For**: Developers new to TypeScript or the Effect library. This tutorial moves quickly and assumes foundational knowledge of both.

## What Does 95% Coverage Mean?

**95% coverage** means depth and breadth of Effect SQL migration features needed for production work, not toy examples.

### Included in 95% Coverage

- **Migration Structure**: `Effect.gen` pattern, `SqlClient` service, SQL template literals
- **Registry Patterns**: `index.ts` exports, `fromRecord`, `fromFileSystem` loaders
- **Migration Runner**: `PgMigrator`, `SqliteMigrator`, `Layer.build`, `effect_sql_migrations` table
- **Table DDL**: CREATE TABLE, ALTER TABLE, DROP TABLE, IF NOT EXISTS guards
- **Column Types**: UUID, VARCHAR, INTEGER, DECIMAL, BOOLEAN, DATE, TIMESTAMPTZ, BYTEA, TEXT
- **Constraints**: PRIMARY KEY, NOT NULL, DEFAULT, UNIQUE, CHECK, FOREIGN KEY, CASCADE
- **Indexes**: Single-column, composite, partial, covering indexes
- **Associations**: Foreign keys, junction tables, many-to-many relationships
- **Client Setup**: `PgClient.layer`, `SqliteClient.layer`, connection configuration
- **Error Handling**: Effect error channels, `catchAll`, `mapError`, migration failure recovery
- **Advanced Patterns**: Enum types via SQL, seed data, conditional DDL, idempotent migrations

### Excluded from 95% (the remaining 5%)

- **Adapter Internals**: Connection pool mechanics, prepared statement caching internals
- **Rare Edge Cases**: Obscure PostgreSQL-specific feature combinations not used in typical production code
- **Legacy Patterns**: Deprecated Effect v2 migration approaches
- **Advanced Database**: Exotic PostgreSQL extensions (PostGIS, TimescaleDB-specific DDL)

## Tutorial Structure

### 80 Examples Across Three Levels

**Sequential numbering**: Examples 1-80 (unified reference system)

**Distribution**:

- **Beginner** (Examples 1-30): 0-40% coverage - Basic migrations, table creation, indexes, constraints, client setup
- **Intermediate** (Examples 31-60): 40-75% coverage - ALTER TABLE, complex constraints, transactions, multi-statement migrations
- **Advanced** (Examples 61-80): 75-95% coverage - Dynamic migrations, custom error handling, testing patterns, performance

## Five-Part Example Format

Every example follows a **mandatory five-part structure**:

### Part 1: Brief Explanation (2-3 sentences)

Answers what this concept or pattern is, why it matters in production code, and when you should use it.

### Part 2: Mermaid Diagram (when appropriate)

Included when data flow between the migrator and database is non-obvious, or when the migration lifecycle has multiple stages worth visualizing.

**Diagram requirements**:

- Use color-blind friendly palette: Blue `#0173B2`, Orange `#DE8F05`, Teal `#029E73`, Purple `#CC78BC`, Brown `#CA9161`
- Vertical orientation (mobile-first)
- Comment syntax: `%%` (NOT `%%{ }%%`)

### Part 3: Heavily Annotated Code

**Core requirement**: Every significant line must have an inline comment using `// =>` notation for TypeScript and `-- =>` for SQL.

```typescript
// => Resolves SqlClient from the Effect context
const sql = yield * SqlClient.SqlClient;

// => Executes the SQL template literal as a single statement
yield *
  sql`
  CREATE TABLE IF NOT EXISTS users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid()
    -- => UUID column; gen_random_uuid() requires pgcrypto or PostgreSQL 13+
  )
`;
```

### Part 4: Key Takeaway (1-2 sentences)

Distills the core insight to its essence and highlights when to apply this in production.

### Part 5: Why It Matters (50-100 words)

Explains the production context: what breaks without this pattern, what alternatives exist, and why Effect SQL's approach differs from other migration tools.

## Self-Containment Rules

**Critical requirement**: Examples must be copy-paste-runnable within their chapter scope.

Every beginner example is completely standalone with full imports, complete migration functions, and no references to previous examples.

## How to Use This Tutorial

### Prerequisites

Before starting, ensure you have:

- Node.js 20+ with TypeScript 5+
- `@effect/sql`, `@effect/sql-pg` or `@effect/sql-sqlite-node` installed
- PostgreSQL 15+ or SQLite running
- Basic Effect library knowledge (`Effect.gen`, `Layer`, service pattern)
- Basic SQL knowledge (DDL statements, relational concepts)

### Learning Path

**For experienced TypeScript developers new to Effect SQL**:

1. Skim beginner examples (1-30) — review fundamentals quickly
2. Deep dive intermediate (31-60) — master production patterns
3. Reference advanced (61-80) — learn optimization and edge cases

**For developers switching from other migration tools** (Flyway, Liquibase, Knex, Drizzle):

1. Read overview to understand Effect SQL philosophy
2. Jump to Example 13 — see how `Layer.build` differs from CLI-driven migrations
3. Reference beginner for Effect SQL-specific syntax as needed

**For quick reference**:

- Use example numbers as reference (e.g., "See Example 13 for Layer.build")
- Search for specific patterns (Ctrl+F for "CASCADE", "composite index", etc.)
- Copy-paste examples as starting points for your own migrations

### Coverage Progression

As you progress through examples, you achieve cumulative coverage:

- **After Beginner** (Example 30): 40% — Can write all standard table creation migrations
- **After Intermediate** (Example 60): 75% — Can handle most production migration scenarios
- **After Advanced** (Example 80): 95% — Expert-level Effect SQL migration mastery

## Next Steps

Ready to start? Choose your path:

- **New to Effect SQL**: Start with [Beginner Examples (1-30)](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner)

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: First Effect SQL Migration](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-1-first-effect-sql-migration)
- [Example 2: @effect/sql and @effect/sql-pg Packages](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-2-effectsql-and-effectsql-pg-packages)
- [Example 3: SqlClient Service](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-3-sqlclient-service)
- [Example 4: Effect.gen Migration Pattern](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-4-effectgen-migration-pattern)
- [Example 5: SQL Template Literals](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-5-sql-template-literals)
- [Example 6: Migration Registry (index.ts exports)](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-6-migration-registry-indexts-exports)
- [Example 7: Migration Layer Setup](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-7-migration-layer-setup)
- [Example 8: Creating Tables](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-8-creating-tables)
- [Example 9: Adding Columns](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-9-adding-columns)
- [Example 10: Adding Indexes](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-10-adding-indexes)
- [Example 11: Adding Foreign Keys](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-11-adding-foreign-keys)
- [Example 12: Adding Unique Constraints](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-12-adding-unique-constraints)
- [Example 13: Running Migrations (Layer.build)](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-13-running-migrations-layerbuild)
- [Example 14: effect_sql_migrations Table](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-14-effect_sql_migrations-table)
- [Example 15: NOT NULL Constraints with Defaults](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-15-not-null-constraints-with-defaults)
- [Example 16: UUID Primary Keys](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-16-uuid-primary-keys)
- [Example 17: Timestamp Columns with Defaults](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-17-timestamp-columns-with-defaults)
- [Example 18: Multiple Statements in One Migration](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-18-multiple-statements-in-one-migration)
- [Example 19: PgClient Setup](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-19-pgclient-setup)
- [Example 20: SqliteClient Setup (@effect/sql-sqlite-node)](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-20-sqliteclient-setup-effectsql-sqlite-node)
- [Example 21: Enum Types via SQL](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-21-enum-types-via-sql)
- [Example 22: CHECK Constraints](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-22-check-constraints)
- [Example 23: Composite Indexes](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-23-composite-indexes)
- [Example 24: Junction Tables (Many-to-Many)](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-24-junction-tables-many-to-many)
- [Example 25: Seed Data in Migrations](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-25-seed-data-in-migrations)
- [Example 26: IF NOT EXISTS Guards](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-26-if-not-exists-guards)
- [Example 27: Dropping Tables/Columns Safely](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-27-dropping-tablescolumns-safely)
- [Example 28: Cascade Delete Foreign Keys](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-28-cascade-delete-foreign-keys)
- [Example 29: Migration Ordering Convention](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-29-migration-ordering-convention)
- [Example 30: Migration with Effect Error Handling](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/beginner#example-30-migration-with-effect-error-handling)

### Intermediate (Examples 31–60)

- [Example 31: Multi-Database Support (Postgres + SQLite)](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-31-multi-database-support-postgres--sqlite)
- [Example 32: Database-Specific SQL Branching](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-32-database-specific-sql-branching)
- [Example 33: Transactions in Migrations](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-33-transactions-in-migrations)
- [Example 34: Effect Error Handling in Migrations (Effect.catchAll)](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-34-effect-error-handling-in-migrations-effectcatchall)
- [Example 35: Conditional Migrations with Effect.if](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-35-conditional-migrations-with-effectif)
- [Example 36: Data Migration with INSERT...SELECT](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-36-data-migration-with-insertselect)
- [Example 37: Seed Data Pattern](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-37-seed-data-pattern)
- [Example 38: Foreign Key with ON UPDATE CASCADE](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-38-foreign-key-with-on-update-cascade)
- [Example 39: Composite Primary Keys](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-39-composite-primary-keys)
- [Example 40: Partial Indexes](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-40-partial-indexes)
- [Example 41: Full-Text Search Indexes](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-41-full-text-search-indexes)
- [Example 42: Creating Views](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-42-creating-views)
- [Example 43: Creating Materialized Views](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-43-creating-materialized-views)
- [Example 44: Trigger Functions](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-44-trigger-functions)
- [Example 45: Stored Procedures](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-45-stored-procedures)
- [Example 46: Custom Migration Runner](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-46-custom-migration-runner)
- [Example 47: Layer Composition for Migrations](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-47-layer-composition-for-migrations)
- [Example 48: Migration Testing with Effect TestContext](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-48-migration-testing-with-effect-testcontext)
- [Example 49: Test Database Setup Pattern](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-49-test-database-setup-pattern)
- [Example 50: JSON/JSONB Columns](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-50-jsonjsonb-columns)
- [Example 51: Array Columns (PostgreSQL)](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-51-array-columns-postgresql)
- [Example 52: GIN Index for JSONB](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-52-gin-index-for-jsonb)
- [Example 53: Table Partitioning](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-53-table-partitioning)
- [Example 54: Generated Columns](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-54-generated-columns)
- [Example 55: Batch Data Migration with Effect.forEach](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-55-batch-data-migration-with-effectforeach)
- [Example 56: Migration Dependencies and Ordering](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-56-migration-dependencies-and-ordering)
- [Example 57: Effect Schema Integration for Validation](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-57-effect-schema-integration-for-validation)
- [Example 58: Connection Pool Configuration](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-58-connection-pool-configuration)
- [Example 59: Migration Retry with Effect.retry](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-59-migration-retry-with-effectretry)
- [Example 60: Migration Logging with Effect.log](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/intermediate#example-60-migration-logging-with-effectlog)

### Advanced (Examples 61–85)

- [Example 61: Custom Migration Provider](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-61-custom-migration-provider)
- [Example 62: Effect Stream for Large Data Migrations](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-62-effect-stream-for-large-data-migrations)
- [Example 63: Zero-Downtime Column Addition](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-63-zero-downtime-column-addition)
- [Example 64: Zero-Downtime Column Removal (3-Phase)](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-64-zero-downtime-column-removal-3-phase)
- [Example 65: Zero-Downtime Table Rename](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-65-zero-downtime-table-rename)
- [Example 66: Large Table Migration with Batched Updates](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-66-large-table-migration-with-batched-updates)
- [Example 67: Online Index Creation (CONCURRENTLY)](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-67-online-index-creation-concurrently)
- [Example 68: Data Backfill Pattern](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-68-data-backfill-pattern)
- [Example 69: Migration in CI/CD Pipeline](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-69-migration-in-cicd-pipeline)
- [Example 70: Migration Monitoring with Effect Metrics](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-70-migration-monitoring-with-effect-metrics)
- [Example 71: Migration Rollback Testing](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-71-migration-rollback-testing)
- [Example 72: Blue-Green Deployment Migrations](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-72-blue-green-deployment-migrations)
- [Example 73: Feature Flag Migration Pattern](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-73-feature-flag-migration-pattern)
- [Example 74: Multi-Tenant Schema Migration](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-74-multi-tenant-schema-migration)
- [Example 75: Migration with pgcrypto Encryption](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-75-migration-with-pgcrypto-encryption)
- [Example 76: Audit Trail Table Migration](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-76-audit-trail-table-migration)
- [Example 77: Soft Delete Schema Pattern](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-77-soft-delete-schema-pattern)
- [Example 78: Schema Drift Detection](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-78-schema-drift-detection)
- [Example 79: Migration Dependency Graph with Effect Layer](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-79-migration-dependency-graph-with-effect-layer)
- [Example 80: Effect HTTP Server Integration](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-80-effect-http-server-integration)
- [Example 81: Migration Performance Benchmarking](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-81-migration-performance-benchmarking)
- [Example 82: Custom Error Types for Migrations](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-82-custom-error-types-for-migrations)
- [Example 83: Migration Squashing Pattern](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-83-migration-squashing-pattern)
- [Example 84: Production Migration Checklist](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-84-production-migration-checklist)
- [Example 85: Migration Observability Dashboard](/en/learn/software-engineering/data/tools/typescript-effect-sql/by-example/advanced#example-85-migration-observability-dashboard)
