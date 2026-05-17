---
title: "Overview"
date: 2026-03-27T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn Go Goose through 85 annotated code examples covering 95% of the migration tool - ideal for experienced developers building production database migration pipelines"
tags: ["golang", "goose", "tutorial", "by-example", "database", "migrations", "code-first", "sql", "postgresql"]
---

## What is Go Goose By Example?

**Go Goose By Example** is a code-first tutorial series teaching experienced Go developers how to build production-ready database migration pipelines using the Goose migration tool. Through 85 heavily annotated, self-contained examples, you'll achieve 95% coverage of Goose patterns—from basic SQL migration files to advanced embedded migrations, programmatic execution, and version management.

This tutorial assumes you're an experienced developer familiar with Go, SQL, and relational databases. If you're new to Go, start with foundational Go tutorials first.

## Why By Example?

**Philosophy**: Show the code first, run it second, understand through direct interaction.

Traditional tutorials explain concepts then show code. By-example tutorials reverse this: every example is a working, runnable code snippet with inline annotations showing exactly what happens at each step—SQL executed, migration states, file structures, and common pitfalls.

**Target Audience**: Experienced developers who:

- Already know Go fundamentals and SQL
- Understand relational databases and schema design
- Prefer learning through working code rather than narrative explanations
- Want comprehensive reference material covering 95% of production migration patterns

**Not For**: Developers new to Go or databases. This tutorial moves quickly and assumes foundational knowledge.

## What Does 95% Coverage Mean?

**95% coverage** means depth and breadth of Goose features needed for production work, not toy examples.

### Included in 95% Coverage

- **SQL Migration Files**: Up/Down directives, naming conventions, sequential versioning
- **CLI Usage**: goose up, goose down, goose status, goose redo, goose version
- **Schema Operations**: CREATE TABLE, ALTER TABLE, DROP TABLE, indexes, constraints
- **Data Types**: UUID primary keys, TIMESTAMPTZ columns, DECIMAL fields, BOOLEAN flags
- **Constraints**: NOT NULL, UNIQUE, CHECK, FOREIGN KEY, ON DELETE CASCADE
- **Indexes**: Single-column, composite, partial, and unique indexes
- **Embedded Migrations**: Go embed.FS, goose.NewProvider(), programmatic execution
- **Context-Aware Execution**: Context-aware Up, UpByOne, UpTo, Down, Reset operations
- **Version Management**: goose_db_version table, version tracking, selective rollback
- **Data Migrations**: Seed data, bulk inserts, transformations within migrations
- **Guard Patterns**: IF NOT EXISTS, IF EXISTS for idempotent migrations
- **PostgreSQL Patterns**: ENUM types, gen_random_uuid(), composite indexes, sequences
- **Transaction Handling**: goose No-Transaction annotation for DDL outside transactions
- **Migration Composition**: Multi-statement migrations, conditional logic

### Excluded from 95% (the remaining 5%)

- **Goose Internals**: Migration engine implementation details, lock mechanics
- **Rare Dialects**: MySQL/MariaDB-specific syntax beyond standard SQL
- **Legacy Features**: Goose v1/v2 API patterns
- **Custom Migration Sources**: Implementing custom goose.MigrationSource
- **Extreme Edge Cases**: Race conditions, concurrent migration scenarios

## Tutorial Structure

### 85 Examples Across Three Levels

**Sequential numbering**: Examples 1-85 (unified reference system)

**Distribution**:

- **Beginner** (Examples 1-30): 0-40% coverage - SQL migration files, CLI usage, basic schema operations, embedded migrations, programmatic execution
- **Intermediate** (Examples 31-60): 40-75% coverage - Advanced schema patterns, version management, transactions, data migrations, multi-environment strategies
- **Advanced** (Examples 61-85): 75-95% coverage - Custom providers, integration patterns, testing migrations, performance optimization, complex rollback strategies

**Rationale**: 85 examples provide granular progression from basic SQL files to expert mastery without overwhelming maintenance burden.

## Five-Part Example Format

Every example follows a **mandatory five-part structure**:

### Part 1: Brief Explanation (2-3 sentences)

**Answers**:

- What is this concept/pattern?
- Why does it matter in production code?
- When should you use it?

**Example**:

> ### Example 18: Embedding Migrations with Go embed.FS
>
> Go's embed package allows you to bundle SQL migration files directly into your binary at compile time, eliminating deployment dependencies on external migration directories. Combined with goose.NewProvider(), embedded migrations enable hermetic deployments where the binary contains everything needed to bring the database to the correct schema version.

### Part 2: Mermaid Diagram (when appropriate)

**Included when** (~40% of examples):

- Migration execution flow involves multiple stages
- Schema relationships between tables are non-obvious
- Embed.FS file system hierarchy needs visualization
- Version table state transitions require illustration
- Rollback sequences need step-by-step depiction

**Skipped when**:

- Simple single-file SQL migrations
- Basic CLI commands with clear linear flow
- Trivial ALTER TABLE statements

**Diagram requirements**:

- Use color-blind friendly palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
- Vertical orientation (mobile-first)
- Clear labels on all nodes and edges
- Comment syntax: `%%` (NOT `%%{ }%%`)

### Part 3: Heavily Annotated Code

**Core requirement**: Every significant line must have an inline comment

**Comment annotations use `-- =>` notation for SQL and `// =>` for Go**:

```sql
-- +goose Up
CREATE TABLE users (                        -- => Begin table definition
    id UUID NOT NULL PRIMARY KEY            -- => UUID primary key, non-null
        DEFAULT gen_random_uuid(),          -- => PostgreSQL generates UUID automatically
    username VARCHAR(50) NOT NULL UNIQUE    -- => Max 50 chars, enforces uniqueness
);                                          -- => Table created with constraints

-- +goose Down
DROP TABLE IF EXISTS users;                 -- => Safely removes table; IF EXISTS prevents errors
```

```go
provider, err := goose.NewProvider(         // => Constructs migration provider
    goose.DialectPostgres,                  // => Targets PostgreSQL dialect
    sqlDB,                                  // => *sql.DB handle from database/sql
    migrationsFS,                           // => embed.FS containing .sql files
    goose.WithVerbose(false),               // => Suppresses migration log output
)                                           // => Returns *goose.Provider, error
_, err = provider.Up(context.Background())  // => Executes all pending migrations
                                            // => Returns []MigrationResult, error
```

**Required annotations**:

- **SQL statements**: Show what each clause does and why
- **Go setup code**: Show types, method signatures, return values
- **Migration states**: Document what version the database reaches
- **Side effects**: Document schema changes, index creation, data inserts
- **Expected outputs**: Show goose CLI output with `-- => Output:` prefix
- **Error cases**: Document when errors occur and how to handle them

### Part 4: Key Takeaway (1-2 sentences)

**Purpose**: Distill the core insight to its essence

**Must highlight**:

- The most important pattern or concept
- When to apply this in production
- Common pitfalls to avoid

**Example**:

> **Key Takeaway**: Always use `goose.WithVerbose(false)` in production programmatic migrations and handle the returned `[]MigrationResult` to log which migrations ran, enabling observability without cluttering logs.

### Part 5: Why It Matters (50-100 words)

**Purpose**: Connect the example to production impact

**Example**:

> **Why It Matters**: In production deployments, embedded migrations eliminate the need to ship migration files alongside your binary or manage file paths across environments. When combined with `provider.Up(ctx)`, your application can self-migrate on startup with full context cancellation support. This pattern is used in `apps/a-demo-be-golang-gin` and ensures every deployed instance of the binary can bring its database schema to the correct version without external tooling or deployment scripts.

## Self-Containment Rules

**Critical requirement**: Examples must be copy-paste-runnable within their chapter scope.

### Beginner Level Self-Containment

**Rule**: Each example is completely standalone

**Requirements**:

- Full SQL migration file or complete Go function
- All necessary imports shown
- Helper code defined in-place
- No references to previous examples
- Runnable with `goose` CLI or standard `go run`

### Intermediate Level Self-Containment

**Rule**: Examples assume beginner concepts but include all necessary code

**Allowed assumptions**:

- Reader knows basic SQL migration syntax
- Reader understands goose Up/Down directives
- Reader can run goose CLI commands

### Advanced Level Self-Containment

**Rule**: Examples assume beginner + intermediate knowledge but remain runnable

**Allowed assumptions**:

- Reader knows embedded migrations and goose.NewProvider()
- Reader understands goose_db_version table
- Reader can navigate Goose documentation for context

## How to Use This Tutorial

### Prerequisites

Before starting, ensure you have:

- Go 1.21+ installed
- PostgreSQL running (or SQLite for local development)
- Goose CLI installed (`go install github.com/pressly/goose/v3/cmd/goose@latest`)
- Basic SQL knowledge (CREATE TABLE, ALTER TABLE, indexes)

### Running SQL Examples

All SQL migration files follow this pattern:

```bash
# Apply migrations from a directory
goose -dir ./db/migrations postgres "host=localhost user=postgres dbname=myapp" up

# Check status
goose -dir ./db/migrations postgres "host=localhost user=postgres dbname=myapp" status

# Roll back one migration
goose -dir ./db/migrations postgres "host=localhost user=postgres dbname=myapp" down
```

### Learning Path

**For experienced Go developers new to Goose**:

1. Skim beginner examples (1-30) - Review migration fundamentals quickly
2. Deep dive intermediate (31-60) - Master production patterns
3. Reference advanced (61-85) - Learn complex strategies and edge cases

**For developers switching from Flyway/Liquibase**:

1. Read overview to understand Goose philosophy
2. Jump to Examples 18-20 (embedded migrations) - See Go-idiomatic approach
3. Reference beginner for SQL syntax as needed
4. Use advanced for provider customization

**For quick reference**:

- Use example numbers as reference (e.g., "See Example 18 for embed.FS setup")
- Search for specific patterns (Ctrl+F for "foreign key", "UUID", etc.)
- Copy-paste examples as starting points for your migrations

### Coverage Progression

As you progress through examples, you'll achieve cumulative coverage:

- **After Beginner** (Example 30): 40% - Can write and run basic SQL migrations programmatically
- **After Intermediate** (Example 60): 75% - Can handle most production migration scenarios
- **After Advanced** (Example 85): 95% - Expert-level Goose mastery

## Example Numbering System

**Sequential numbering**: Examples 1-85 across all three levels

**Why sequential?**

- Creates unified reference system ("See Example 18")
- Clear progression from fundamentals to mastery
- Easy to track coverage percentage

**Beginner**: Examples 1-30 (0-40% coverage)
**Intermediate**: Examples 31-60 (40-75% coverage)
**Advanced**: Examples 61-85 (75-95% coverage)

## Code Annotation Philosophy

Every example uses **educational annotations** to show exactly what happens:

```sql
-- +goose Up
-- => Goose reads this directive to find the "apply" block
CREATE TABLE products (
    id          UUID        NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
    -- => UUID primary key; gen_random_uuid() requires pgcrypto or PostgreSQL 13+
    name        VARCHAR(255) NOT NULL,
    -- => Required product name; VARCHAR(255) allows up to 255 characters
    price       DECIMAL(10,2) NOT NULL,
    -- => Monetary value; DECIMAL(10,2) = up to 8 digits before decimal, 2 after
    created_at  TIMESTAMPTZ NOT NULL DEFAULT NOW()
    -- => Timezone-aware timestamp; DEFAULT NOW() auto-populated on INSERT
);
-- => Creates products table with 4 columns and 1 auto-generated primary key

-- +goose Down
DROP TABLE IF EXISTS products;
-- => Removes products table; IF EXISTS prevents error if already dropped
```

Annotations show:

- **SQL clause meanings** and their production implications
- **Type choices** (why UUID vs SERIAL, why TIMESTAMPTZ vs TIMESTAMP)
- **Constraint effects** on insert/update behavior
- **Goose directives** and their roles in migration execution
- **Go setup patterns** and their types/return values

## Quality Standards

Every example in this tutorial meets these standards:

- **Self-contained**: Copy-paste-runnable within chapter scope
- **Annotated**: Every significant line has inline comment
- **Tested**: All code examples verified working
- **Production-relevant**: Real-world patterns, not toy examples
- **Accessible**: Color-blind friendly diagrams, clear structure

## Next Steps

Ready to start? Choose your path:

- **New to Goose**: Start with [Beginner Examples (1-30)](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner)
- **Know Goose CLI, want programmatic**: Jump to Example 18 in [Beginner Examples (1-30)](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner)

## Feedback and Contributions

Found an issue? Have a suggestion? This tutorial is part of the ayokoding-web learning platform. Check the repository for contribution guidelines.

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: First SQL Migration File (goose Up/Down Directives)](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-1-first-sql-migration-file-goose-updown-directives)
- [Example 2: Running Migrations with the Goose CLI](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-2-running-migrations-with-the-goose-cli)
- [Example 3: Creating a Users Table](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-3-creating-a-users-table)
- [Example 4: Adding Columns with ALTER TABLE](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-4-adding-columns-with-alter-table)
- [Example 5: Adding Indexes](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-5-adding-indexes)
- [Example 6: Adding Unique Constraints](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-6-adding-unique-constraints)
- [Example 7: Dropping Columns Safely](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-7-dropping-columns-safely)
- [Example 8: Migration File Naming Convention](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-8-migration-file-naming-convention)
- [Example 9: Checking Migration Status](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-9-checking-migration-status)
- [Example 10: Rolling Back a Migration](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-10-rolling-back-a-migration)
- [Example 11: Rolling Back to a Specific Version](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-11-rolling-back-to-a-specific-version)
- [Example 12: Redo the Last Migration](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-12-redo-the-last-migration)
- [Example 13: Creating Tables with Foreign Keys](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-13-creating-tables-with-foreign-keys)
- [Example 14: Adding NOT NULL Constraints with Defaults](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-14-adding-not-null-constraints-with-defaults)
- [Example 15: Creating Enum Types in PostgreSQL](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-15-creating-enum-types-in-postgresql)
- [Example 16: Seed Data in Migrations](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-16-seed-data-in-migrations)
- [Example 17: Multiple Statements in One Migration](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-17-multiple-statements-in-one-migration)
- [Example 18: Embedding Migrations with Go embed.FS](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-18-embedding-migrations-with-go-embedfs)
- [Example 19: goose.NewProvider() Setup](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-19-goosenewprovider-setup)
- [Example 20: Running Embedded Migrations Programmatically](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-20-running-embedded-migrations-programmatically)
- [Example 21: Context-Aware Migration Execution](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-21-context-aware-migration-execution)
- [Example 22: Single-Step Migration (Up by One)](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-22-single-step-migration-up-by-one)
- [Example 23: Creating Composite Indexes](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-23-creating-composite-indexes)
- [Example 24: Adding CHECK Constraints](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-24-adding-check-constraints)
- [Example 25: Creating Junction Tables for Many-to-Many](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-25-creating-junction-tables-for-many-to-many)
- [Example 26: Timestamp Columns with Defaults](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-26-timestamp-columns-with-defaults)
- [Example 27: UUID Primary Keys](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-27-uuid-primary-keys)
- [Example 28: Migration with IF NOT EXISTS Guards](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-28-migration-with-if-not-exists-guards)
- [Example 29: Dropping Tables Safely](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-29-dropping-tables-safely)
- [Example 30: The goose_db_version Table](/en/learn/software-engineering/data/tools/golang-goose/by-example/beginner#example-30-the-goose_db_version-table)

### Intermediate (Examples 31–60)

- [Example 31: Go-Based Migrations (goose.AddMigrationNoTxContext)](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-31-go-based-migrations-gooseaddmigrationnotxcontext)
- [Example 32: Go Migration with Database Queries](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-32-go-migration-with-database-queries)
- [Example 33: Go Migration with Data Transformation](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-33-go-migration-with-data-transformation)
- [Example 34: Transaction Control in SQL Migrations (+goose NO TRANSACTION)](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-34-transaction-control-in-sql-migrations-goose-no-transaction)
- [Example 35: Explicit Transaction Wrapping in Go Migrations](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-35-explicit-transaction-wrapping-in-go-migrations)
- [Example 36: Rollback Strategies for Complex Migrations](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-36-rollback-strategies-for-complex-migrations)
- [Example 37: Version Pinning (goose up-to VERSION)](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-37-version-pinning-goose-up-to-version)
- [Example 38: Migration Down-to Specific Version](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-38-migration-down-to-specific-version)
- [Example 39: Goose Provider with Custom Options](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-39-goose-provider-with-custom-options)
- [Example 40: Multi-Dialect Support (PostgreSQL + SQLite)](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-40-multi-dialect-support-postgresql--sqlite)
- [Example 41: Dialect-Specific SQL in Migrations](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-41-dialect-specific-sql-in-migrations)
- [Example 42: Migration Locking for Concurrent Safety](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-42-migration-locking-for-concurrent-safety)
- [Example 43: Creating Partial Indexes](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-43-creating-partial-indexes)
- [Example 44: Full-Text Search Indexes (PostgreSQL)](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-44-full-text-search-indexes-postgresql)
- [Example 45: Creating Views](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-45-creating-views)
- [Example 46: Creating Materialized Views](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-46-creating-materialized-views)
- [Example 47: Trigger Functions](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-47-trigger-functions)
- [Example 48: Stored Procedures in Migrations](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-48-stored-procedures-in-migrations)
- [Example 49: Conditional Migration Logic](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-49-conditional-migration-logic)
- [Example 50: Batch Data Migration Pattern](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-50-batch-data-migration-pattern)
- [Example 51: Migration Testing with testcontainers-go](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-51-migration-testing-with-testcontainers-go)
- [Example 52: Migration Validation in Tests](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-52-migration-validation-in-tests)
- [Example 53: Foreign Key with ON UPDATE CASCADE](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-53-foreign-key-with-on-update-cascade)
- [Example 54: Composite Primary Keys](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-54-composite-primary-keys)
- [Example 55: Table Partitioning (PostgreSQL)](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-55-table-partitioning-postgresql)
- [Example 56: Adding Generated Columns](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-56-adding-generated-columns)
- [Example 57: JSON/JSONB Columns](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-57-jsonjsonb-columns)
- [Example 58: Array Columns (PostgreSQL)](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-58-array-columns-postgresql)
- [Example 59: GIN Index for JSONB](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-59-gin-index-for-jsonb)
- [Example 60: Migration with Environment Variables](/en/learn/software-engineering/data/tools/golang-goose/by-example/intermediate#example-60-migration-with-environment-variables)

### Advanced (Examples 61–85)

- [Example 61: Custom goose.Provider with AllowMissing](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-61-custom-gooseprovider-with-allowmissing)
- [Example 62: Migration Hooks (SetGlobalMigrationOptions)](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-62-migration-hooks-setglobalmigrationoptions)
- [Example 63: Zero-Downtime Column Addition](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-63-zero-downtime-column-addition)
- [Example 64: Zero-Downtime Column Removal (3-Phase)](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-64-zero-downtime-column-removal-3-phase)
- [Example 65: Zero-Downtime Table Rename](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-65-zero-downtime-table-rename)
- [Example 66: Large Table Migration with Chunked Offset Batching](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-66-large-table-migration-with-chunked-offset-batching)
- [Example 67: Online Index Creation (CONCURRENTLY)](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-67-online-index-creation-concurrently)
- [Example 68: Data Backfill Migration Pattern (Source Transformation)](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-68-data-backfill-migration-pattern-source-transformation)
- [Example 69: Migration in CI/CD Pipeline](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-69-migration-in-cicd-pipeline)
- [Example 70: Dry-Run Mode (goose status before apply)](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-70-dry-run-mode-goose-status-before-apply)
- [Example 71: Migration Version Gap Handling](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-71-migration-version-gap-handling)
- [Example 72: Hybrid SQL + Go Migration Workflow](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-72-hybrid-sql--go-migration-workflow)
- [Example 73: Migration with Custom Logger](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-73-migration-with-custom-logger)
- [Example 74: Schema Drift Detection Pattern](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-74-schema-drift-detection-pattern)
- [Example 75: Migration Rollback Testing](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-75-migration-rollback-testing)
- [Example 76: Blue-Green Deployment Migrations](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-76-blue-green-deployment-migrations)
- [Example 77: Feature Flag Migration Pattern](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-77-feature-flag-migration-pattern)
- [Example 78: Migration Performance Benchmarking](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-78-migration-performance-benchmarking)
- [Example 79: Multi-Tenant Schema Migration](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-79-multi-tenant-schema-migration)
- [Example 80: Migration with Encryption (pgcrypto)](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-80-migration-with-encryption-pgcrypto)
- [Example 81: Audit Trail Table Migration](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-81-audit-trail-table-migration)
- [Example 82: Soft Delete Schema Pattern](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-82-soft-delete-schema-pattern)
- [Example 83: Migration Dependency Graph](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-83-migration-dependency-graph)
- [Example 84: Production Migration Checklist Pattern](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-84-production-migration-checklist-pattern)
- [Example 85: Migration Monitoring with Prometheus Metrics](/en/learn/software-engineering/data/tools/golang-goose/by-example/advanced#example-85-migration-monitoring-with-prometheus-metrics)
