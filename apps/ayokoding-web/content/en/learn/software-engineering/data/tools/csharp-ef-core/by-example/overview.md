---
title: "Overview"
date: 2026-03-27T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn C# EF Core Migrations through 30 annotated code examples covering 95% of migration patterns - ideal for experienced developers building production database schemas"
tags: ["csharp", "ef-core", "entity-framework", "migrations", "tutorial", "by-example", "database", "code-first"]
---

## What is C# EF Core Migrations By Example?

**C# EF Core Migrations By Example** is a code-first tutorial series teaching experienced .NET developers how to manage database schema evolution using Entity Framework Core Migrations. Through 30 heavily annotated, self-contained examples, you will achieve 95% coverage of EF Core migration patterns—from basic DbContext setup to idempotent script generation and production startup migrations.

This tutorial assumes you are an experienced developer familiar with C#, .NET, and relational databases. If you are new to C# or EF Core, start with foundational .NET tutorials first.

## Why By Example?

**Philosophy**: Show the code first, run it second, understand through direct interaction.

Traditional tutorials explain concepts then show code. By-example tutorials reverse this: every example is a working, runnable code snippet with inline annotations showing exactly what happens at each step—migration class structure, generated SQL, CLI command output, and common pitfalls.

**Target Audience**: Experienced developers who:

- Already know C# and .NET fundamentals
- Understand relational databases and SQL
- Prefer learning through working code rather than narrative explanations
- Want comprehensive reference material covering 95% of production migration patterns

**Not For**: Developers new to C# or databases. This tutorial moves quickly and assumes foundational knowledge.

## What Does 95% Coverage Mean?

**95% coverage** means the depth and breadth of EF Core migration features needed for production work, not toy examples.

### Included in 95% Coverage

- **DbContext Setup**: DbContext class, DbSet properties, OnModelCreating configuration
- **Migration Lifecycle**: Creating, applying, reverting, and removing migrations
- **MigrationBuilder Operations**: CreateTable, AddColumn, DropColumn, CreateIndex, AddForeignKey
- **CLI Tooling**: `dotnet ef migrations add`, `dotnet ef database update`, `dotnet ef migrations list`
- **History Tracking**: `__EFMigrationsHistory` table, migration snapshot, Designer.cs metadata
- **Schema Constraints**: Unique constraints, NOT NULL with defaults, CHECK constraints
- **Column Types**: UUID primary keys, timestamp columns, enum storage, precision types
- **Relationships**: Foreign keys, cascade delete, composite indexes, junction tables
- **Seed Data**: HasData seeding, idempotent data setup
- **Advanced Operations**: Column rename patterns, multiple tables per migration, SQL scripts
- **Production Patterns**: Idempotent scripts, ASP.NET Core startup migration application

### Excluded from 95% (the remaining 5%)

- **EF Core Internals**: Provider adapter implementation, connection pool mechanics
- **Rare Edge Cases**: Obscure feature combinations not used in typical production code
- **Database-Specific**: Vendor-specific SQL fragments outside standard EF Core patterns
- **Legacy Features**: Deprecated APIs from EF Core 1.x or 2.x
- **Custom Migration Operations**: Writing custom IMigrationsSqlGenerator implementations

## Tutorial Structure

### 30 Examples in One Beginner Level

**Sequential numbering**: Examples 1-30 (unified reference system)

**Distribution**:

- **Beginner** (Examples 1-30): 0-40% coverage — DbContext setup, migration basics, schema operations, CLI tooling, production patterns

**Rationale**: EF Core Migrations is a focused tool. Thirty examples provide granular progression from initial setup to production-ready SQL generation without overwhelming maintenance burden.

## Five-Part Example Format

Every example follows a **mandatory five-part structure**:

### Part 1: Brief Explanation (2-3 sentences)

**Answers**:

- What is this concept or pattern?
- Why does it matter in production code?
- When should you use it?

### Part 2: Mermaid Diagram (when appropriate)

**Included when** (~40% of examples):

- Migration lifecycle has multiple stages
- Relationships between files need visualization
- CLI command flow is non-obvious
- Database schema relationships require illustration

**Skipped when**:

- Simple single-operation commands
- Straightforward column type changes
- Trivial additive migrations

**Diagram requirements**:

- Use color-blind friendly palette: Blue `#0173B2`, Orange `#DE8F05`, Teal `#029E73`, Purple `#CC78BC`, Brown `#CA9161`
- Vertical orientation (mobile-first)
- Clear labels on all nodes and edges
- Comment syntax: `%%` (NOT `%%{ }%%`)

### Part 3: Heavily Annotated Code

**Core requirement**: Every significant line must have an inline comment.

**Comment annotations use `// =>` notation for C# and `-- =>` for SQL**:

```csharp
migrationBuilder.CreateTable(        // => Generates CREATE TABLE SQL statement
    name: "users",                   // => Table name in the database
    columns: table => new            // => Lambda defines column set
    {
        id = table.Column<Guid>(     // => Column named "id" of type uuid
            type: "uuid",            // => => Maps to PostgreSQL UUID type
            nullable: false),        // => => NOT NULL constraint applied
    },
    constraints: table =>            // => Lambda defines table-level constraints
    {
        table.PrimaryKey(            // => Adds PRIMARY KEY constraint
            "pk_users", x => x.id); // => => Constraint name and key column
    });
```

**Required annotations**:

- **Migration builder calls**: Document what SQL each method generates
- **Column configurations**: Show resulting SQL column definitions
- **CLI commands**: Show expected terminal output
- **File paths**: Note where generated files appear on disk
- **Side effects**: Document database changes, snapshot updates

### Part 4: Key Takeaway (1-2 sentences)

**Purpose**: Distill the core insight to its essence.

### Part 5: Why It Matters (50-100 words)

**Purpose**: Connect the pattern to real production consequences.

## Self-Containment Rules

**Critical requirement**: Examples must be copy-paste-runnable within their chapter scope.

### Beginner Level Self-Containment

**Rule**: Each example is completely standalone.

**Requirements**:

- Full class definitions with namespaces
- All necessary using statements
- Helper types defined in-place when needed
- No references to previous examples
- Runnable with `dotnet ef` CLI

**Golden rule**: If you delete all other examples, this example should still execute.

## How to Use This Tutorial

### Prerequisites

Before starting, ensure you have:

- .NET 8+ SDK installed
- `dotnet-ef` global tool installed (`dotnet tool install --global dotnet-ef`)
- PostgreSQL (or your preferred database) running
- Basic C# and .NET knowledge
- Basic SQL and relational database knowledge

### Running Examples

All CLI examples assume a project with EF Core configured:

```bash
# Install EF Core global tool
dotnet tool install --global dotnet-ef

# Create a migration
dotnet ef migrations add InitialCreate

# Apply migrations to database
dotnet ef database update

# Generate SQL script
dotnet ef migrations script --output migration.sql
```

### Learning Path

**For experienced .NET developers new to EF Core Migrations**:

1. Work through all 30 examples sequentially
2. Run each CLI command in a test project
3. Inspect generated migration files alongside examples

**For quick reference**:

- Use example numbers as reference (e.g., "See Example 9 for applying migrations")
- Search for specific patterns (Ctrl+F for "cascade", "unique", "HasData")
- Copy-paste examples as starting points for your migrations

### Coverage Progression

- **After Example 15** (40% coverage): Can create and apply basic migrations
- **After Example 25** (70% coverage): Can handle most production schema operations
- **After Example 30** (95% coverage): Expert-level EF Core migration mastery

## Code Annotation Philosophy

Every example uses **educational annotations** to show exactly what happens:

```csharp
// Migration class declaration
public partial class InitialCreate : Migration  // => Partial class allows Designer.cs metadata
{
    protected override void Up(MigrationBuilder migrationBuilder)
    {                                           // => Up() runs on dotnet ef database update
        migrationBuilder.CreateTable(           // => Generates CREATE TABLE statement
            name: "products",                   // => => Table name: products
            columns: table => new               // => => Column definitions
            {
                id = table.Column<int>(         // => Column: id INTEGER
                    nullable: false),            // => => NOT NULL constraint
            });
    }
}
```

Annotations show:

- **Generated SQL** for each MigrationBuilder call
- **CLI output** from `dotnet ef` commands
- **File system effects** (files created, modified, or deleted)
- **Database side effects** (schema changes, constraint creation)
- **Common pitfalls** and edge cases

## Quality Standards

Every example in this tutorial meets these standards:

- **Self-contained**: Copy-paste-runnable within chapter scope
- **Annotated**: Every significant line has an inline comment
- **Tested**: All code examples verified against real EF Core behavior
- **Production-relevant**: Real-world patterns, not toy examples
- **Accessible**: Color-blind friendly diagrams, clear structure

## Next Steps

Ready to start? Begin with [Beginner Examples (1-30)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner).

## Feedback and Contributions

Found an issue? Have a suggestion? This tutorial is part of the ayokoding-web learning platform. Check the repository for contribution guidelines.

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: DbContext Setup](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-1-dbcontext-setup)
- [Example 2: First Migration (dotnet ef migrations add)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-2-first-migration-dotnet-ef-migrations-add)
- [Example 3: Up() and Down() Methods](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-3-up-and-down-methods)
- [Example 4: MigrationBuilder.CreateTable](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-4-migrationbuildercreatetable)
- [Example 5: MigrationBuilder.AddColumn](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-5-migrationbuilderaddcolumn)
- [Example 6: MigrationBuilder.DropColumn](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-6-migrationbuilderdropcolumn)
- [Example 7: MigrationBuilder.CreateIndex](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-7-migrationbuildercreateindex)
- [Example 8: MigrationBuilder.AddForeignKey](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-8-migrationbuilderaddforeignkey)
- [Example 9: Applying Migrations (dotnet ef database update)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-9-applying-migrations-dotnet-ef-database-update)
- [Example 10: Reverting Migrations (dotnet ef database update PreviousMigration)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-10-reverting-migrations-dotnet-ef-database-update-previousmigration)
- [Example 11: Removing Last Migration (dotnet ef migrations remove)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-11-removing-last-migration-dotnet-ef-migrations-remove)
- [Example 12: Listing Migrations (dotnet ef migrations list)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-12-listing-migrations-dotnet-ef-migrations-list)
- [Example 13: \_\_EFMigrationsHistory Table](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-13-__efmigrationshistory-table)
- [Example 14: Migration Snapshot File](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-14-migration-snapshot-file)
- [Example 15: Designer.cs Metadata](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-15-designercs-metadata)
- [Example 16: Adding Unique Constraints](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-16-adding-unique-constraints)
- [Example 17: NOT NULL with Default Values](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-17-not-null-with-default-values)
- [Example 18: UUID/GUID Primary Keys](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-18-uuidguid-primary-keys)
- [Example 19: Timestamp Columns with Defaults](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-19-timestamp-columns-with-defaults)
- [Example 20: Enum Storage Patterns](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-20-enum-storage-patterns)
- [Example 21: CHECK Constraints](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-21-check-constraints)
- [Example 22: Composite Indexes](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-22-composite-indexes)
- [Example 23: Junction Tables (Many-to-Many)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-23-junction-tables-many-to-many)
- [Example 24: Seed Data with HasData](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-24-seed-data-with-hasdata)
- [Example 25: Multiple Tables in One Migration](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-25-multiple-tables-in-one-migration)
- [Example 26: Cascade Delete Configuration](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-26-cascade-delete-configuration)
- [Example 27: Column Rename (Safe Pattern)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-27-column-rename-safe-pattern)
- [Example 28: Generating SQL Script (dotnet ef migrations script)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-28-generating-sql-script-dotnet-ef-migrations-script)
- [Example 29: Idempotent Script Generation](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-29-idempotent-script-generation)
- [Example 30: ASP.NET Core Startup Migration](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/beginner#example-30-aspnet-core-startup-migration)

### Intermediate (Examples 31–60)

- [Example 31: Data Seeding with HasData](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-31-data-seeding-with-hasdata)
- [Example 32: Custom SQL in Migrations (migrationBuilder.Sql)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-32-custom-sql-in-migrations-migrationbuildersql)
- [Example 33: Transaction Control in Migrations](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-33-transaction-control-in-migrations)
- [Example 34: Multiple DbContexts in One Project](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-34-multiple-dbcontexts-in-one-project)
- [Example 35: Migration Bundles (dotnet ef migrations bundle)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-35-migration-bundles-dotnet-ef-migrations-bundle)
- [Example 36: Scaffold from Existing DB (dotnet ef dbcontext scaffold)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-36-scaffold-from-existing-db-dotnet-ef-dbcontext-scaffold)
- [Example 37: Column Rename vs Drop/Create (Safe Pattern)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-37-column-rename-vs-dropcreate-safe-pattern)
- [Example 38: Owned Types (Value Objects)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-38-owned-types-value-objects)
- [Example 39: Value Converters](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-39-value-converters)
- [Example 40: Table Splitting](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-40-table-splitting)
- [Example 41: TPH Inheritance (Table Per Hierarchy)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-41-tph-inheritance-table-per-hierarchy)
- [Example 42: TPT Inheritance (Table Per Type)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-42-tpt-inheritance-table-per-type)
- [Example 43: TPC Inheritance (Table Per Concrete Type)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-43-tpc-inheritance-table-per-concrete-type)
- [Example 44: Migration Squashing](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-44-migration-squashing)
- [Example 45: Idempotent Scripts](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-45-idempotent-scripts)
- [Example 46: Connection Resiliency in Migrations](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-46-connection-resiliency-in-migrations)
- [Example 47: Creating Views](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-47-creating-views)
- [Example 48: Creating Materialized Views (raw SQL)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-48-creating-materialized-views-raw-sql)
- [Example 49: Trigger Functions (raw SQL)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-49-trigger-functions-raw-sql)
- [Example 50: Stored Procedures (raw SQL)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-50-stored-procedures-raw-sql)
- [Example 51: JSON Columns (ToJson)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-51-json-columns-tojson)
- [Example 52: Temporal Tables](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-52-temporal-tables)
- [Example 53: Full-Text Search Setup](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-53-full-text-search-setup)
- [Example 54: Migration Testing with xUnit](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-54-migration-testing-with-xunit)
- [Example 55: Test Database Setup with Testcontainers](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-55-test-database-setup-with-testcontainers)
- [Example 56: Table Partitioning (raw SQL)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-56-table-partitioning-raw-sql)
- [Example 57: GIN Index for JSONB (raw SQL)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-57-gin-index-for-jsonb-raw-sql)
- [Example 58: Composite Primary Keys](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-58-composite-primary-keys)
- [Example 59: HiLo Sequence for ID Generation](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-59-hilo-sequence-for-id-generation)
- [Example 60: IDesignTimeDbContextFactory](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/intermediate#example-60-idesigntimedbcontextfactory)

### Advanced (Examples 61–85)

- [Example 61: Custom Migration Operations](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-61-custom-migration-operations)
- [Example 62: IDesignTimeDbContextFactory Deep Dive](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-62-idesigntimedbcontextfactory-deep-dive)
- [Example 63: Zero-Downtime Column Addition](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-63-zero-downtime-column-addition)
- [Example 64: Zero-Downtime Column Removal (3-Phase)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-64-zero-downtime-column-removal-3-phase)
- [Example 65: Zero-Downtime Table Rename](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-65-zero-downtime-table-rename)
- [Example 66: Large Table Migration with Batched Updates](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-66-large-table-migration-with-batched-updates)
- [Example 67: Online Index Creation (CONCURRENTLY)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-67-online-index-creation-concurrently)
- [Example 68: Data Backfill Pattern](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-68-data-backfill-pattern)
- [Example 69: EF Core Migrations in CI/CD Pipeline](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-69-ef-core-migrations-in-cicd-pipeline)
- [Example 70: Migration Rollback Testing with xUnit](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-70-migration-rollback-testing-with-xunit)
- [Example 71: Blue-Green Deployment Migrations](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-71-blue-green-deployment-migrations)
- [Example 72: Feature Flag Migration Pattern](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-72-feature-flag-migration-pattern)
- [Example 73: Multi-Tenant Schema Migration](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-73-multi-tenant-schema-migration)
- [Example 74: Migration with pgcrypto Encryption](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-74-migration-with-pgcrypto-encryption)
- [Example 75: Audit Trail Table Migration](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-75-audit-trail-table-migration)
- [Example 76: Soft Delete Schema Pattern](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-76-soft-delete-schema-pattern)
- [Example 77: Compiled Models for Startup Performance](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-77-compiled-models-for-startup-performance)
- [Example 78: JSON Columns Deep Dive (ToJson)](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-78-json-columns-deep-dive-tojson)
- [Example 79: Temporal Tables Deep Dive](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-79-temporal-tables-deep-dive)
- [Example 80: Raw SQL Migrations with EF Core](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-80-raw-sql-migrations-with-ef-core)
- [Example 81: ASP.NET Core Integration Patterns](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-81-aspnet-core-integration-patterns)
- [Example 82: Migration Performance Benchmarking](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-82-migration-performance-benchmarking)
- [Example 83: Schema Drift Detection](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-83-schema-drift-detection)
- [Example 84: Production Migration Checklist](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-84-production-migration-checklist)
- [Example 85: Migration Monitoring and Health Checks](/en/learn/software-engineering/data/tools/csharp-ef-core/by-example/advanced#example-85-migration-monitoring-and-health-checks)
