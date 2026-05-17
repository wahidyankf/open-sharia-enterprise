---
title: "Overview"
date: 2026-03-27T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn F# DbUp through 75+ annotated examples covering 95% of database migration patterns - ideal for experienced developers managing PostgreSQL schema evolution with SQL scripts"
tags:
  ["fsharp", "dbup", "tutorial", "by-example", "database", "migrations", "postgresql", "sql", "schema", "code-first"]
---

## What is F# DbUp By Example?

**F# DbUp By Example** is a code-first tutorial series teaching experienced developers how to manage PostgreSQL schema evolution using DbUp from F#. Through 75+ heavily annotated, self-contained examples, you achieve 95% coverage of DbUp patterns—from writing your first SQL migration script to advanced deployment strategies, idempotency guards, and assembly-embedded script discovery.

This tutorial assumes you are an experienced developer familiar with F#, PostgreSQL, and relational database concepts. If you are new to F#, start with foundational F# tutorials first.

## Why By Example?

**Philosophy**: Show the code first, run it second, understand through direct interaction.

Traditional tutorials explain concepts then show code. By-example tutorials reverse this: every example is a working, runnable code snippet with inline annotations showing exactly what happens at each step—SQL executed, DbUp journal state, migration results, and common pitfalls.

**Target Audience**: Experienced developers who:

- Already know F# fundamentals (modules, pipelines, computation expressions)
- Understand relational databases and SQL DDL
- Prefer learning through working code rather than narrative explanations
- Want comprehensive reference material covering 95% of production migration patterns

**Not For**: Developers new to F# or databases. This tutorial moves quickly and assumes foundational knowledge.

## What Does 95% Coverage Mean?

**95% coverage** means the depth and breadth of DbUp features needed for production work, not toy examples.

### Included in 95% Coverage

- **Script Authoring**: SQL DDL conventions, naming patterns, sequential numbering, IF NOT EXISTS guards
- **DeployChanges Builder**: PostgresqlDatabase, WithScriptsEmbeddedInAssembly, LogToConsole, Build, PerformUpgrade
- **Journal Table**: SchemaVersions tracking, idempotency guarantees, migration history queries
- **Connection Setup**: NpgsqlConnection in F#, connection string patterns, PostgreSQL-specific types
- **Schema Operations**: CREATE TABLE, ALTER TABLE, DROP with safety guards, column types, constraints
- **Indexes**: Single-column, composite, unique, partial indexes
- **Constraints**: Foreign keys, CHECK constraints, UNIQUE constraints, NOT NULL with defaults
- **Data Types**: UUID primary keys, TIMESTAMPTZ defaults, DECIMAL precision, BYTEA, BOOLEAN, ENUM via CHECK
- **Relationships**: One-to-many, many-to-many junction tables, cascade behavior
- **Data Migrations**: Seed data scripts, backfill patterns, safe column renames
- **Assembly Integration**: Script discovery from embedded resources, ordering guarantees
- **Error Handling**: Checking Successful property, ErrorScript details, rollback patterns
- **Advanced Patterns**: Multiple script sources, filtered scripts, pre-deployment scripts

### Excluded from 95% (the remaining 5%)

- **Rare Adapters**: MySQL, SQLite, SQL Server specific behaviors outside core patterns
- **Custom Journal**: Implementing ISchemaVersionJournal from scratch
- **Internal Mechanics**: DbUp source connection pooling, adapter internals
- **Legacy API**: Deprecated pre-4.x DbUp builder patterns

## Tutorial Structure

### 75+ Examples Across Three Levels

**Sequential numbering**: Examples 1-75+ (unified reference system)

**Distribution**:

- **Beginner** (Examples 1-30): 0-40% coverage — Script authoring, DeployChanges builder, PostgreSQL setup, basic DDL patterns, schema operations
- **Intermediate** (Examples 31-60): 40-75% coverage — Advanced DDL, data migrations, multiple script sources, error handling, deployment strategies
- **Advanced** (Examples 61-75+): 75-95% coverage — Custom filters, journal queries, CI/CD integration, idempotency patterns, multi-schema deployments

**Rationale**: This distribution mirrors real production adoption: most teams need beginner and intermediate patterns daily; advanced patterns arise for complex multi-tenant or CI/CD scenarios.

## Five-Part Example Format

Every example follows a mandatory five-part structure:

### Part 1: Brief Explanation (2-3 sentences)

Answers:

- What is this concept or pattern?
- Why does it matter in production migrations?
- When should you use it?

**Example**:

> ### Example 7: Console Logging with WithConsoleLogger
>
> WithConsoleLogger attaches a console sink to the DbUp upgrade engine, printing each script name and execution status during migration runs. Visibility into which scripts executed—and in what order—is essential for debugging migration failures in CI/CD pipelines and local development.

### Part 2: Mermaid Diagram (when appropriate)

**Included when** (~35% of examples):

- DbUp execution flow involves multiple stages
- Relationships between SQL files and the journal table are non-obvious
- Assembly embedding and script discovery need illustration
- Error handling branches require visualization

**Skipped when**:

- Simple single-file SQL DDL operations
- Linear ALTER TABLE statements
- Trivial index additions

**Diagram requirements**:

- Use color-blind friendly palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
- Vertical orientation (mobile-first)
- Clear labels on all nodes and edges
- Comment syntax: `%%` (NOT `%%{ }%%`)

### Part 3: Heavily Annotated Code

**Core requirement**: Every significant line must have an inline comment.

**Comment annotations use `-- =>` for SQL and `// =>` for F#**:

```fsharp
let result =
    DeployChanges.To                        // => Entry point for the fluent builder
        .PostgresqlDatabase(connStr)        // => Targets PostgreSQL with Npgsql driver
        .WithScriptsEmbeddedInAssembly(asm) // => Discovers *.sql embedded resources
        .LogToConsole()                     // => Prints script names and results to stdout
        .Build()                            // => Returns UpgradeEngine instance
        .PerformUpgrade()                   // => Executes pending scripts; returns DatabaseUpgradeResult
// => result.Successful is true when all scripts ran without error
// => result.Scripts contains list of ScriptName strings that were executed
```

**Required annotations**:

- **Builder steps**: Show what each fluent call configures
- **SQL results**: Document which columns, constraints, or indexes the statement creates
- **DbUp state**: Show journal table changes after execution
- **Error cases**: Document Successful/ErrorScript properties and when they occur
- **Expected outputs**: Show console output with `-- =>` prefix in SQL examples

### Part 4: Key Takeaway (1-2 sentences)

**Purpose**: Distill the core insight to its essence.

**Must highlight**:

- The most important pattern or concept
- When to apply this in production
- Common pitfalls to avoid

**Example**:

> **Key Takeaway**: Always check `result.Successful` before proceeding with application startup; on failure, `result.Error.Message` gives the exact SQL error and `result.ErrorScript` identifies the offending script.

### Part 5: Why It Matters (50-100 words)

**Purpose**: Contextualize the example within production concerns.

Covers:

- Production impact of ignoring this pattern
- How it prevents common migration failures
- Relationship to broader database reliability practices

## Self-Containment Rules

**Critical requirement**: Examples must be copy-paste-runnable within their chapter scope.

### Beginner Level Self-Containment

**Rule**: Each SQL example is completely standalone; each F# snippet is runnable with the stated dependencies.

**Requirements**:

- Complete SQL DDL statements with no external table references (or explicit dependency noted)
- Full F# snippets including open statements and let bindings
- No references to previous examples
- Runnable against a live PostgreSQL instance with DbUp 4.x NuGet packages

### Intermediate and Advanced Level Self-Containment

**Rule**: Examples assume beginner concepts but include all necessary code.

**Allowed assumptions**:

- Reader understands DeployChanges builder and PerformUpgrade from beginner examples
- Reader can create a PostgreSQL connection string from environment variables
- Reader knows F# module syntax and basic pattern matching

## How to Use This Tutorial

### Prerequisites

Before starting, ensure you have:

- .NET 8+ SDK installed
- PostgreSQL 14+ running (local or Docker)
- Basic F# knowledge (modules, functions, pipelines)
- Basic SQL knowledge (DDL: CREATE, ALTER, DROP)
- DbUp NuGet package: `dbup-postgresql` (4.x or 5.x)

### Running Examples

SQL examples run directly against PostgreSQL:

```bash
psql $DATABASE_URL -f 001-create-users.sql
```

F# examples run as part of your application startup or test setup:

```fsharp
// Add to project file: <PackageReference Include="dbup-postgresql" Version="5.*" />
open DbUp

let connStr = System.Environment.GetEnvironmentVariable("DATABASE_URL")
let result =
    DeployChanges.To
        .PostgresqlDatabase(connStr)
        .WithScriptsEmbeddedInAssembly(System.Reflection.Assembly.GetExecutingAssembly())
        .LogToConsole()
        .Build()
        .PerformUpgrade()
```

### Learning Path

**For F# developers adopting DbUp**:

1. Work through beginner examples (1-30) — learn script authoring and builder setup
2. Deep dive intermediate (31-60) — master complex DDL and data migration patterns
3. Reference advanced (61-75+) — learn CI/CD integration and deployment strategies

**For developers migrating from Flyway or Liquibase**:

1. Read this overview to understand DbUp philosophy (SQL-first, no XML/YAML)
2. Jump to intermediate examples (31-60) to see how DbUp handles common scenarios
3. Use beginner examples as syntax reference for PostgreSQL-specific DDL

### Coverage Progression

As you progress through examples, you achieve cumulative coverage:

- **After Beginner** (Example 30): 40% — Can manage basic schema evolution for production tables
- **After Intermediate** (Example 60): 75% — Can handle most production migration scenarios
- **After Advanced** (Example 75+): 95% — Expert-level DbUp mastery for complex deployments

## Code Annotation Philosophy

Every example uses **educational annotations** to show exactly what happens:

```sql
-- Example 1: Creates the users table with UUID primary key
CREATE TABLE users (
    -- => UUID type requires the pgcrypto extension or PostgreSQL 13+ gen_random_uuid()
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    -- => VARCHAR without length limit stores up to 1 GB; add CHECK constraint for practical limits
    username VARCHAR NOT NULL,
    -- => TIMESTAMPTZ stores timezone-aware instants; prefer over TIMESTAMP for distributed systems
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
-- => After execution: users table exists in schemaversions journal as 001-create-users.sql
```

Annotations show:

- **Column type choices** and their tradeoffs
- **Default value behavior** and when defaults apply
- **Constraint enforcement** and what violations look like
- **DbUp journal state** after script execution
- **Common gotchas** and safe alternatives

## Quality Standards

Every example in this tutorial meets these standards:

- **Self-contained**: Copy-paste-runnable within chapter scope
- **Annotated**: Every significant line has an inline comment using `-- =>` (SQL) or `// =>` (F#)
- **Production-relevant**: Real-world patterns drawn from actual F#/PostgreSQL projects
- **Accessible**: Color-blind friendly diagrams, clear structure

## Next Steps

Ready to start? Begin with:

- [Beginner Examples (1-30)](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner) — Script authoring and DeployChanges builder fundamentals

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: First DbUp Migration Script (SQL File)](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-1-first-dbup-migration-script-sql-file)
- [Example 2: DeployChanges Builder Setup](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-2-deploychanges-builder-setup)
- [Example 3: PostgresqlDatabase Target](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-3-postgresqldatabase-target)
- [Example 4: Embedded Resource Scripts](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-4-embedded-resource-scripts)
- [Example 5: Script Naming Conventions (Sequential Numbering)](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-5-script-naming-conventions-sequential-numbering)
- [Example 6: SchemaVersions Journal Table](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-6-schemaversions-journal-table)
- [Example 7: Console Logging with WithConsoleLogger](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-7-console-logging-with-withconsolelogger)
- [Example 8: Creating Tables](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-8-creating-tables)
- [Example 9: Adding Columns](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-9-adding-columns)
- [Example 10: Adding Indexes](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-10-adding-indexes)
- [Example 11: Adding Foreign Keys](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-11-adding-foreign-keys)
- [Example 12: Adding Unique Constraints](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-12-adding-unique-constraints)
- [Example 13: Adding NOT NULL with Defaults](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-13-adding-not-null-with-defaults)
- [Example 14: Running Migrations with PerformUpgrade](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-14-running-migrations-with-performupgrade)
- [Example 15: Checking Migration Results (Successful/ErrorScript)](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-15-checking-migration-results-successfulerrorscript)
- [Example 16: NpgsqlConnection Setup in F](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-16-npgsqlconnection-setup-in-f)
- [Example 17: Multiple Scripts in Sequence](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-17-multiple-scripts-in-sequence)
- [Example 18: Dropping Columns Safely](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-18-dropping-columns-safely)
- [Example 19: Dropping Tables Safely](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-19-dropping-tables-safely)
- [Example 20: UUID Primary Keys in PostgreSQL](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-20-uuid-primary-keys-in-postgresql)
- [Example 21: Timestamp Columns with Defaults](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-21-timestamp-columns-with-defaults)
- [Example 22: Enum Types via SQL](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-22-enum-types-via-sql)
- [Example 23: CHECK Constraints](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-23-check-constraints)
- [Example 24: Composite Indexes](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-24-composite-indexes)
- [Example 25: Junction Tables (Many-to-Many)](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-25-junction-tables-many-to-many)
- [Example 26: Seed Data in Migration Scripts](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-26-seed-data-in-migration-scripts)
- [Example 27: IF NOT EXISTS Guards](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-27-if-not-exists-guards)
- [Example 28: Cascade Delete Foreign Keys](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-28-cascade-delete-foreign-keys)
- [Example 29: Script Discovery from Assembly](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-29-script-discovery-from-assembly)
- [Example 30: Migration Execution Order](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/beginner#example-30-migration-execution-order)

### Intermediate (Examples 31–60)

- [Example 31: Script Filtering with IScriptFilter](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-31-script-filtering-with-iscriptfilter)
- [Example 32: Custom Journal Table Name](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-32-custom-journal-table-name)
- [Example 33: Transaction Per Script Strategy](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-33-transaction-per-script-strategy)
- [Example 34: Single Transaction Strategy](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-34-single-transaction-strategy)
- [Example 35: No Transaction Strategy](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-35-no-transaction-strategy)
- [Example 36: Variables in SQL Scripts](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-36-variables-in-sql-scripts)
- [Example 37: Script Preprocessing](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-37-script-preprocessing)
- [Example 38: Always-Run Scripts](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-38-always-run-scripts)
- [Example 39: Script Naming Groups and Ordering](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-39-script-naming-groups-and-ordering)
- [Example 40: F# Type-Safe Migration Wrapper](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-40-f-type-safe-migration-wrapper)
- [Example 41: Data Migration with INSERT...SELECT](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-41-data-migration-with-insertselect)
- [Example 42: Seed Data Migration Pattern](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-42-seed-data-migration-pattern)
- [Example 43: Foreign Key with ON UPDATE CASCADE](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-43-foreign-key-with-on-update-cascade)
- [Example 44: Composite Primary Keys](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-44-composite-primary-keys)
- [Example 45: Partial Indexes](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-45-partial-indexes)
- [Example 46: Full-Text Search Indexes (PostgreSQL)](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-46-full-text-search-indexes-postgresql)
- [Example 47: Creating Views](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-47-creating-views)
- [Example 48: Creating Materialized Views](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-48-creating-materialized-views)
- [Example 49: Trigger Functions](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-49-trigger-functions)
- [Example 50: Stored Procedures](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-50-stored-procedures)
- [Example 51: Conditional Migration Logic](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-51-conditional-migration-logic)
- [Example 52: Batch Data Migration Pattern](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-52-batch-data-migration-pattern)
- [Example 53: Migration Testing with xUnit](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-53-migration-testing-with-xunit)
- [Example 54: Test Database Setup with Testcontainers](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-54-test-database-setup-with-testcontainers)
- [Example 55: JSON/JSONB Columns](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-55-jsonjsonb-columns)
- [Example 56: Array Columns (PostgreSQL)](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-56-array-columns-postgresql)
- [Example 57: GIN Index for JSONB](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-57-gin-index-for-jsonb)
- [Example 58: Table Partitioning](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-58-table-partitioning)
- [Example 59: Generated Columns](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-59-generated-columns)
- [Example 60: Migration Error Handling and Recovery](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/intermediate#example-60-migration-error-handling-and-recovery)

### Advanced (Examples 61–85)

- [Example 61: Custom IScriptProvider](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-61-custom-iscriptprovider)
- [Example 62: Custom IScriptExecutor](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-62-custom-iscriptexecutor)
- [Example 63: Custom IJournal Implementation](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-63-custom-ijournal-implementation)
- [Example 64: Zero-Downtime Column Addition](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-64-zero-downtime-column-addition)
- [Example 65: Zero-Downtime Column Removal (3-Phase)](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-65-zero-downtime-column-removal-3-phase)
- [Example 66: Large Table Migration with Batched Updates](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-66-large-table-migration-with-batched-updates)
- [Example 67: Online Index Creation (CONCURRENTLY)](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-67-online-index-creation-concurrently)
- [Example 68: Data Backfill Pattern](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-68-data-backfill-pattern)
- [Example 69: DbUp in CI/CD Pipeline](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-69-dbup-in-cicd-pipeline)
- [Example 70: Dry-Run with LogScriptOutput](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-70-dry-run-with-logscriptoutput)
- [Example 71: Multi-Database Migrations](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-71-multi-database-migrations)
- [Example 72: Schema Comparison Pattern](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-72-schema-comparison-pattern)
- [Example 73: Migration Rollback Testing](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-73-migration-rollback-testing)
- [Example 74: Blue-Green Deployment Migrations](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-74-blue-green-deployment-migrations)
- [Example 75: Feature Flag Migration Pattern](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-75-feature-flag-migration-pattern)
- [Example 76: DbUp with EF Core Hybrid](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-76-dbup-with-ef-core-hybrid)
- [Example 77: Multi-Tenant Schema Migration](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-77-multi-tenant-schema-migration)
- [Example 78: Migration with pgcrypto](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-78-migration-with-pgcrypto)
- [Example 79: Audit Trail Table](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-79-audit-trail-table)
- [Example 80: Soft Delete Schema Pattern](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-80-soft-delete-schema-pattern)
- [Example 81: Custom Preprocessor](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-81-custom-preprocessor)
- [Example 82: Migration Performance Benchmarking](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-82-migration-performance-benchmarking)
- [Example 83: Schema Drift Detection](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-83-schema-drift-detection)
- [Example 84: Production Migration Checklist](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-84-production-migration-checklist)
- [Example 85: Migration Monitoring and Alerting](/en/learn/software-engineering/data/tools/fsharp-dbup/by-example/advanced#example-85-migration-monitoring-and-alerting)
