---
title: "Overview"
date: 2026-03-27T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn Rust SQLx migrations through 85 annotated code examples covering 95% of migration patterns - ideal for experienced developers managing database schema evolution"
tags: ["rust-sqlx", "tutorial", "by-example", "migrations", "code-first", "sqlx", "database", "postgresql", "sqlite"]
---

## What is Rust SQLx Migrations By Example?

**Rust SQLx Migrations By Example** is a code-first tutorial series teaching experienced Rust developers how to manage database schema evolution using SQLx's migrate feature. Through 85 heavily annotated, self-contained examples across three levels (beginner, intermediate, advanced), you will achieve 95% coverage of SQLx migration patterns—from writing your first SQL migration file to embedded migrations, programmatic execution, and advanced constraint patterns.

This tutorial focuses specifically on the migrate feature of SQLx. It assumes familiarity with Rust, async/await, and basic SQL. If you are new to Rust async programming, start with foundational Rust tutorials first.

## Why By Example?

**Philosophy**: Show the code first, run it second, understand through direct interaction.

Traditional tutorials explain concepts then show code. By-example tutorials reverse this: every example is a working, runnable code snippet with inline annotations showing exactly what happens at each step—migration file contents, SQL executed, schema states produced, and common pitfalls.

**Target Audience**: Experienced developers who:

- Already know Rust fundamentals including async/await and error handling
- Understand relational databases and SQL
- Prefer learning through working code rather than narrative explanations
- Want comprehensive reference material covering 95% of production migration patterns

**Not For**: Developers new to Rust or SQL. This tutorial moves quickly and assumes foundational knowledge.

## What Does 95% Coverage Mean?

**95% coverage** means depth and breadth of SQLx migration features needed for production work, not toy examples.

### Included in 95% Coverage

- **Migration Files**: SQL file structure, naming conventions, sequential numbering
- **CLI Commands**: `sqlx migrate add`, `sqlx migrate run`, `sqlx migrate info`, `sqlx migrate revert`
- **Embedded Migrations**: `migrate!()` macro, `include_str!()`, compile-time SQL embedding
- **Connection Pools**: `PgPool`, `SqlitePool`, `AnyPool` setup and configuration
- **Table Operations**: CREATE TABLE, ALTER TABLE, DROP TABLE with safety guards
- **Column Constraints**: NOT NULL, DEFAULT values, UNIQUE, CHECK constraints
- **Data Types**: UUIDs, timestamps, enums (PostgreSQL), numeric types
- **Indexes**: Single-column, composite, conditional indexes
- **Foreign Keys**: REFERENCES, ON DELETE CASCADE, ON UPDATE behaviors
- **Junction Tables**: Many-to-many relationship schemas
- **Data Migrations**: Seed data, backfill scripts embedded in migrations
- **Reversibility**: Up/down migration pairs for rollback capability
- **\_sqlx_migrations Table**: How SQLx tracks applied migrations internally

### Excluded from 95% (the remaining 5%)

- **SQLx Internals**: Connection pool mechanics, driver implementation details
- **Rare Constraints**: Partial indexes, expression indexes, exclusion constraints
- **Database-Specific**: Exotic PostgreSQL features outside standard migration use cases
- **Custom Migrators**: Writing custom MigratorTrait implementations
- **Advanced DDL**: Materialized views, stored procedures, triggers

## Tutorial Structure

### 85 Examples Across Three Levels

**Sequential numbering**: Examples 1-85

**Distribution**:

- **Beginner** (Examples 1-30): 0-40% coverage - Migration files, CLI commands, embedded migrations, connection setup, common constraint patterns
- **Intermediate** (Examples 31-60): 40-75% coverage - Typed queries, compile-time verification, multi-database support, batch migrations, testing patterns
- **Advanced** (Examples 61-85): 75-95% coverage - Custom migration sources, programmatic migrators, concurrent index creation, blue-green deployments, multi-tenant schemas

**Rationale**: 85 examples across three progressive levels provide thorough coverage of the migrate feature, giving you everything needed to manage schema evolution from first migration to production-grade deployment strategies.

## Five-Part Example Format

Every example follows a **mandatory five-part structure**:

### Part 1: Brief Explanation (2-3 sentences)

**Answers**:

- What is this concept or pattern?
- Why does it matter in production code?
- When should you use it?

**Example**:

> ### Example 12: Embedded Migrations with migrate!() Macro
>
> The `migrate!()` macro embeds SQL migration files directly into the compiled binary at build time, eliminating the need to ship separate migration files with your application. This is the recommended approach for production deployments where you want a self-contained executable.

### Part 2: Mermaid Diagram (when appropriate)

**Included when** (roughly 40% of examples):

- Migration execution flow has non-obvious steps
- Schema relationships between tables require illustration
- The `_sqlx_migrations` tracking mechanism needs visualization
- CLI command flow involves multiple stages

**Skipped when**:

- Simple SQL DDL statements with clear linear semantics
- Single-constraint examples
- Trivial file structure examples

**Diagram requirements**:

- Use color-blind friendly palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
- Vertical orientation (mobile-first)
- Clear labels on all nodes and edges
- Comment syntax: `%%` (NOT `%%{ }%%`)

### Part 3: Heavily Annotated Code

**Core requirement**: Every significant line must have an inline comment

**Comment annotations use `-- =>` for SQL and `// =>` for Rust**:

```sql
-- Create the users table with a UUID primary key
CREATE TABLE IF NOT EXISTS users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(), -- => UUID auto-generated by PostgreSQL
    username TEXT NOT NULL UNIQUE,                 -- => username must be present and unique
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()  -- => timestamp set automatically on insert
);
-- => SQL executed: CREATE TABLE users (...)
-- => _sqlx_migrations updated: version recorded as applied
```

```rust
// Embed all migrations from the migrations/ directory at compile time
let migrator = sqlx::migrate!("./migrations"); // => Scans ./migrations/ at compile time
                                               // => Embeds SQL files into binary
migrator.run(&pool).await?;                    // => Applies any unapplied migrations in order
                                               // => Updates _sqlx_migrations table
```

**Required annotations**:

- **SQL semantics**: Show what each constraint or column definition enforces
- **Migration state**: Document what the \_sqlx_migrations table records
- **Side effects**: Document schema changes produced
- **Expected outputs**: Show CLI output with `-- =>` prefix
- **Error cases**: Document when migrations fail and why

### Part 4: Key Takeaway (1-2 sentences)

**Purpose**: Distill the core insight to its essence

**Must highlight**:

- The most important pattern or concept
- When to apply this in production
- Common pitfalls to avoid

### Part 5: Why It Matters (50-100 words)

**Purpose**: Connect the example to real-world production scenarios

**Must cover**:

- Production relevance of the pattern
- Consequences of not following the pattern
- How it fits into a larger migration strategy

## Self-Containment Rules

**Critical requirement**: Examples must be copy-paste-runnable within their chapter scope.

**Beginner Level Self-Containment**

Each example is completely standalone:

- Full SQL file contents shown
- All necessary Rust code included
- No references to previous examples
- Runnable with `cargo run` or `sqlx` CLI given a database URL

**Example structure**:

```sql
-- migrations/0001_create_users.sql
CREATE TABLE IF NOT EXISTS users (    -- => Creates users table if absent
    id BIGSERIAL PRIMARY KEY,         -- => Auto-incrementing integer primary key
    username TEXT NOT NULL            -- => Required text column
);
```

```rust
// src/main.rs
use sqlx::PgPool;

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let pool = PgPool::connect("postgres://user:pass@localhost/mydb").await?;
    // => Connects to PostgreSQL, returns connection pool
    sqlx::migrate!("./migrations").run(&pool).await?;
    // => Applies all unapplied migrations in version order
    Ok(())
}
```

## How to Use This Tutorial

### Prerequisites

Before starting, ensure you have:

- Rust 1.75+ with Cargo installed
- PostgreSQL or SQLite running (examples note which database applies)
- Basic Rust knowledge (structs, enums, async/await, error handling)
- Basic SQL knowledge (CREATE TABLE, ALTER TABLE, INSERT)
- SQLx CLI installed: `cargo install sqlx-cli`

### Running Examples

SQL migration examples run directly with the SQLx CLI:

```bash
# Apply migrations
sqlx migrate run --database-url postgres://user:pass@localhost/mydb

# Check status
sqlx migrate info --database-url postgres://user:pass@localhost/mydb

# Revert last migration
sqlx migrate revert --database-url postgres://user:pass@localhost/mydb
```

Rust code examples run with Cargo:

```bash
DATABASE_URL=postgres://user:pass@localhost/mydb cargo run
```

### Learning Path

**For Rust developers new to SQLx migrations**:

1. Work through examples 1-10 for CLI and file fundamentals
2. Study examples 11-20 for programmatic migration and connection setup
3. Apply examples 21-30 for constraint and schema patterns
4. Progress to examples 31-60 for typed queries, compile-time checks, and testing
5. Master examples 61-85 for production deployment strategies and advanced patterns

**For developers migrating from other tools**:

1. Read the overview to understand SQLx migration philosophy
2. Jump to Example 12 for embedded migrations (key differentiator from other tools)
3. Reference beginner examples for SQL pattern specifics
4. Study intermediate examples 31-60 for SQLx-specific compile-time verification
5. Review advanced examples 61-85 for production deployment patterns

**For quick reference**:

- Use example numbers as reference (for example, "See Example 12 for embedded migrations")
- Search for specific patterns using Ctrl+F for terms like "UUID", "CASCADE", "index"
- Copy-paste examples as starting points for your migration files

### Coverage Progression

As you progress through examples, you achieve cumulative coverage:

- **After Example 10**: Can create and run basic migrations with the CLI
- **After Example 20**: Can embed migrations in Rust binaries with multiple pool types
- **After Example 30**: Solid foundation covering 40% of production migration patterns
- **After Example 45**: Can use typed queries, compile-time verification, and multi-database setups
- **After Example 60**: Intermediate mastery covering 75% of production patterns including testing and batch migrations
- **After Example 75**: Can handle concurrent indexes, blue-green deployments, and multi-tenant schemas
- **After Example 85**: Expert-level SQLx migration mastery covering 95% of production patterns

## Example Numbering System

**Sequential numbering**: Examples 1-85 across three tutorial levels

**Why sequential?**

- Creates a unified reference system ("See Example 12")
- Clear progression from CLI fundamentals to production deployment patterns
- Easy to track coverage percentage

**Beginner**: Examples 1-30 (0-40% coverage)
**Intermediate**: Examples 31-60 (40-75% coverage)
**Advanced**: Examples 61-85 (75-95% coverage)

## Code Annotation Philosophy

Every example uses **educational annotations** to show exactly what happens:

```sql
-- Variable annotation (SQL column definition)
username VARCHAR(50) NOT NULL UNIQUE, -- => text, required, must be unique across table
                                      -- => violation raises unique_violation (SQLSTATE 23505)

-- Migration tracking
-- => After migration runs: _sqlx_migrations row inserted
-- => version: 20240101120000, checksum: sha384(...), applied_at: now()
```

```rust
// Pool creation
let pool = PgPool::connect(database_url).await?; // => Returns PgPool (connection pool)
                                                  // => Establishes initial connections to PostgreSQL
sqlx::migrate!("./migrations").run(&pool).await?; // => Scans ./migrations/ at build time
                                                   // => Applies unapplied migrations in version order
```

Annotations show:

- **SQL semantics** enforced by each constraint
- **Migration tracking** via \_sqlx_migrations
- **Return types** and their meanings
- **Common gotchas** such as version ordering and checksum validation

## Quality Standards

Every example in this tutorial meets these standards:

- **Self-contained**: Copy-paste-runnable within chapter scope
- **Annotated**: Every significant line has an inline comment
- **Production-relevant**: Real-world patterns, not toy examples
- **Accessible**: Color-blind friendly diagrams, clear structure
- **Database-accurate**: SQL examples tested against PostgreSQL and SQLite behavior

## Next Steps

Ready to start? Begin with [Beginner Examples (1-30)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner) to build a complete foundation in SQLx migrations, then continue to [Intermediate Examples (31-60)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate) and [Advanced Examples (61-85)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced).

## Feedback and Contributions

Found an issue? Have a suggestion? This tutorial is part of the ayokoding-web learning platform. Check the repository for contribution guidelines.

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: First SQL Migration File](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-1-first-sql-migration-file)
- [Example 2: sqlx migrate add Command](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-2-sqlx-migrate-add-command)
- [Example 3: Migration File Structure (.up.sql / .down.sql)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-3-migration-file-structure-upsql--downsql)
- [Example 4: Running Migrations (sqlx migrate run)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-4-running-migrations-sqlx-migrate-run)
- [Example 5: Creating Tables](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-5-creating-tables)
- [Example 6: Adding Columns](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-6-adding-columns)
- [Example 7: Adding Indexes](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-7-adding-indexes)
- [Example 8: Adding Foreign Keys](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-8-adding-foreign-keys)
- [Example 9: Adding Unique Constraints](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-9-adding-unique-constraints)
- [Example 10: Checking Migration Status (sqlx migrate info)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-10-checking-migration-status-sqlx-migrate-info)
- [Example 11: Reversible Migrations](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-11-reversible-migrations)
- [Example 12: Embedded Migrations with migrate!() Macro](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-12-embedded-migrations-with-migrate-macro)
- [Example 13: AnyPool Setup for Multi-Database](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-13-anypool-setup-for-multi-database)
- [Example 14: PgPool Connection Setup](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-14-pgpool-connection-setup)
- [Example 15: SqlitePool Connection Setup](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-15-sqlitepool-connection-setup)
- [Example 16: include_str!() for Embedding SQL](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-16-include_str-for-embedding-sql)
- [Example 17: Sequential Migration Numbering](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-17-sequential-migration-numbering)
- [Example 18: NOT NULL Constraints with Defaults](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-18-not-null-constraints-with-defaults)
- [Example 19: UUID Primary Keys](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-19-uuid-primary-keys)
- [Example 20: Timestamp Columns with Defaults](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-20-timestamp-columns-with-defaults)
- [Example 21: Enum Types (PostgreSQL CREATE TYPE)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-21-enum-types-postgresql-create-type)
- [Example 22: CHECK Constraints](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-22-check-constraints)
- [Example 23: Composite Indexes](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-23-composite-indexes)
- [Example 24: Junction Tables (Many-to-Many)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-24-junction-tables-many-to-many)
- [Example 25: Seed Data in Migrations](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-25-seed-data-in-migrations)
- [Example 26: Multiple Statements in One Migration](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-26-multiple-statements-in-one-migration)
- [Example 27: IF NOT EXISTS Guards](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-27-if-not-exists-guards)
- [Example 28: Dropping Tables/Columns Safely](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-28-dropping-tablescolumns-safely)
- [Example 29: Cascade Delete Foreign Keys](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-29-cascade-delete-foreign-keys)
- [Example 30: \_sqlx_migrations Table Structure](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/beginner#example-30-_sqlx_migrations-table-structure)

### Intermediate (Examples 31–60)

- [Example 31: Transactions in Migrations](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-31-transactions-in-migrations)
- [Example 32: Multi-Database Migrations (Postgres + SQLite)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-32-multi-database-migrations-postgres--sqlite)
- [Example 33: Database-Specific SQL Branching](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-33-database-specific-sql-branching)
- [Example 34: Custom Enum Types with FromRow](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-34-custom-enum-types-with-fromrow)
- [Example 35: JSON/JSONB Columns](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-35-jsonjsonb-columns)
- [Example 36: Array Columns (PostgreSQL)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-36-array-columns-postgresql)
- [Example 37: Foreign Keys with ON UPDATE CASCADE](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-37-foreign-keys-with-on-update-cascade)
- [Example 38: Composite Primary Keys](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-38-composite-primary-keys)
- [Example 39: Partial Indexes](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-39-partial-indexes)
- [Example 40: Full-Text Search Indexes](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-40-full-text-search-indexes)
- [Example 41: Conditional Column Addition (IF NOT EXISTS Pattern)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-41-conditional-column-addition-if-not-exists-pattern)
- [Example 42: Seed Data with INSERT...ON CONFLICT](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-42-seed-data-with-inserton-conflict)
- [Example 43: Migration Ordering and Dependencies](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-43-migration-ordering-and-dependencies)
- [Example 44: sqlx-cli Commands Reference](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-44-sqlx-cli-commands-reference)
- [Example 45: Offline Mode (sqlx prepare)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-45-offline-mode-sqlx-prepare)
- [Example 46: SQLX_OFFLINE Environment Variable](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-46-sqlx_offline-environment-variable)
- [Example 47: Migration Testing with sqlx::test](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-47-migration-testing-with-sqlxtest)
- [Example 48: Test Database Setup Pattern](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-48-test-database-setup-pattern)
- [Example 49: Creating Views in Migrations](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-49-creating-views-in-migrations)
- [Example 50: Creating Materialized Views](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-50-creating-materialized-views)
- [Example 51: Trigger Functions (PostgreSQL)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-51-trigger-functions-postgresql)
- [Example 52: Stored Procedures](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-52-stored-procedures)
- [Example 53: Table Partitioning](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-53-table-partitioning)
- [Example 54: Generated/Computed Columns](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-54-generatedcomputed-columns)
- [Example 55: GIN Index for JSONB](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-55-gin-index-for-jsonb)
- [Example 56: Expression Indexes](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-56-expression-indexes)
- [Example 57: Batch Data Migration Pattern](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-57-batch-data-migration-pattern)
- [Example 58: Migration with Environment-Specific Logic](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-58-migration-with-environment-specific-logic)
- [Example 59: Error Handling in Programmatic Migrations](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-59-error-handling-in-programmatic-migrations)
- [Example 60: Migration Logging and Observability](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/intermediate#example-60-migration-logging-and-observability)

### Advanced (Examples 61–85)

- [Example 61: Programmatic Migration with Migrator](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-61-programmatic-migration-with-migrator)
- [Example 62: Custom Migration Source](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-62-custom-migration-source)
- [Example 63: Zero-Downtime Column Addition](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-63-zero-downtime-column-addition)
- [Example 64: Zero-Downtime Column Removal (3-Phase)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-64-zero-downtime-column-removal-3-phase)
- [Example 65: Zero-Downtime Table Rename](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-65-zero-downtime-table-rename)
- [Example 66: Large Table Migration with Batched Updates](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-66-large-table-migration-with-batched-updates)
- [Example 67: Online Index Creation (CONCURRENTLY)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-67-online-index-creation-concurrently)
- [Example 68: Data Backfill Migration Pattern](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-68-data-backfill-migration-pattern)
- [Example 69: Migration in CI/CD Pipeline](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-69-migration-in-cicd-pipeline)
- [Example 70: Compile-Time Verification (sqlx prepare)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-70-compile-time-verification-sqlx-prepare)
- [Example 71: Migration Rollback Testing](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-71-migration-rollback-testing)
- [Example 72: Blue-Green Deployment Migrations](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-72-blue-green-deployment-migrations)
- [Example 73: Feature Flag Migration Pattern](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-73-feature-flag-migration-pattern)
- [Example 74: Multi-Tenant Schema Migration](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-74-multi-tenant-schema-migration)
- [Example 75: Migration with pgcrypto Encryption](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-75-migration-with-pgcrypto-encryption)
- [Example 76: Audit Trail Table Migration](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-76-audit-trail-table-migration)
- [Example 77: Soft Delete Schema Pattern](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-77-soft-delete-schema-pattern)
- [Example 78: Migration Performance Benchmarking](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-78-migration-performance-benchmarking)
- [Example 79: Schema Drift Detection](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-79-schema-drift-detection)
- [Example 80: Migration Dependency Graph](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-80-migration-dependency-graph)
- [Example 81: Axum Integration Pattern (Startup Migrations)](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-81-axum-integration-pattern-startup-migrations)
- [Example 82: Connection Pool Tuning for Migrations](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-82-connection-pool-tuning-for-migrations)
- [Example 83: Migration with Custom Error Types](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-83-migration-with-custom-error-types)
- [Example 84: Production Migration Checklist Pattern](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-84-production-migration-checklist-pattern)
- [Example 85: Migration Monitoring and Alerting](/en/learn/software-engineering/data/tools/rust-sqlx/by-example/advanced#example-85-migration-monitoring-and-alerting)
