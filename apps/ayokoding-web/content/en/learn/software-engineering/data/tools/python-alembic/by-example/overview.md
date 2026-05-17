---
title: "Overview"
date: 2026-03-27T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn Python Alembic through 30 annotated code examples covering 95% of migration patterns - ideal for experienced developers managing database schema evolution"
tags:
  [
    "python-alembic",
    "tutorial",
    "by-example",
    "examples",
    "code-first",
    "alembic",
    "database",
    "migrations",
    "sqlalchemy",
  ]
---

## What is Python Alembic By Example?

**Python Alembic By Example** is a code-first tutorial series teaching experienced developers how to manage database schema evolution using Alembic, the industry-standard migration tool for SQLAlchemy-based Python applications. Through 30 heavily annotated, self-contained examples, you will achieve 95% coverage of Alembic patterns—from basic initialization and revision creation to autogenerate, advanced column types, and data migrations.

This tutorial assumes you are an experienced developer familiar with Python, SQLAlchemy, and relational databases. If you are new to SQLAlchemy, study that first before working through these examples.

## Why By Example?

**Philosophy**: Show the code first, run it second, understand through direct interaction.

Traditional tutorials explain concepts then show code. By-example tutorials reverse this: every example is a working, runnable code snippet with inline annotations showing exactly what happens at each step—migration file structure, SQL statements emitted, version table state, and common pitfalls.

**Target Audience**: Experienced developers who:

- Already know Python fundamentals and SQLAlchemy models
- Understand relational databases and SQL DDL statements
- Prefer learning through working code rather than narrative explanations
- Want comprehensive reference material covering 95% of production migration patterns

**Not For**: Developers new to Python or databases. This tutorial moves quickly and assumes foundational knowledge.

## What Does 95% Coverage Mean?

**95% coverage** means the depth and breadth of Alembic features needed for production work, not toy examples.

### Included in 95% Coverage

- **Initialization**: `alembic init`, alembic.ini configuration, env.py structure
- **Revision Management**: Creating revisions, upgrade/downgrade functions, revision messages, dependencies
- **Table Operations**: create_table, drop_table with full column definitions
- **Column Operations**: add_column, drop_column, alter_column with nullable/default changes
- **Constraint Operations**: create_index, drop_index, create_unique_constraint, create_check_constraint, create_foreign_key
- **CLI Commands**: upgrade head, downgrade -1, current, history, show
- **Autogenerate**: SQLAlchemy metadata integration, `--autogenerate` flag, detecting schema drift
- **Advanced Column Types**: UUID, timestamp with defaults, Enum, Numeric
- **Data Migrations**: op.bulk_insert, op.execute for seed data
- **Complex Migrations**: Multiple operations per revision, composite indexes, junction tables
- **Version Tracking**: alembic_version table structure, base and head concepts

### Excluded from 95% (the remaining 5%)

- **Adapter Internals**: Alembic autogenerate comparator plugin development
- **Rare Edge Cases**: Multi-database setups with branch merging
- **Legacy Patterns**: Alembic 0.x API differences
- **Non-PostgreSQL Specifics**: MySQL/SQLite dialect edge cases not applicable to PostgreSQL

## Tutorial Structure

### 30 Examples Across One Level

**Distribution**:

- **Beginner** (Examples 1-30): 0-100% coverage — initialization, CLI commands, revision structure, all core operations, autogenerate, advanced types, data migrations

**Rationale**: Alembic is a focused tool with a well-defined surface area. 30 examples provide complete coverage without artificial distribution across multiple files.

## Four-Part Example Format

Every example follows a **mandatory five-part structure**:

### Part 1: Brief Explanation (2-3 sentences)

**Answers**:

- What is this concept/pattern?
- Why does it matter in production migrations?
- When should you use it?

### Part 2: Mermaid Diagram (when appropriate)

**Included when** (~30% of examples):

- The relationship between files or concepts is non-obvious
- Migration execution flow has multiple stages
- Version chain structure requires visualization

**Skipped when**:

- Simple single-operation migrations with clear linear flow
- CLI commands with obvious outputs
- Isolated column operations

**Diagram requirements**:

- Use color-blind friendly palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
- Vertical orientation (mobile-first)
- Clear labels on all nodes and edges
- Comment syntax: `%%` (NOT `%%{ }%%`)

### Part 3: Heavily Annotated Code

**Core requirement**: Every significant line must have an inline comment.

**Comment annotations use `# =>` notation for Python and `-- =>` for SQL**:

```python
revision: str = "abc123"           # => revision ID; unique identifier for this migration
down_revision: str | None = None   # => parent revision; None means this is the first migration
                                   # => forms a linked list: current -> parent -> ... -> None
```

**Required annotations**:

- **Variable states**: Show values and what they represent
- **SQL executed**: Document what DDL statement Alembic emits
- **Side effects**: Document database mutations and version table changes
- **Expected outputs**: Show CLI output with `# => Output:` prefix
- **Error cases**: Document when errors occur and how to handle them

### Part 4: Key Takeaway (1-2 sentences)

**Purpose**: Distill the core insight to its essence.

**Must highlight**:

- The most important pattern or concept
- When to apply this in production
- Common pitfalls to avoid

### Part 5: Why It Matters (50-100 words)

**Purpose**: Production context explaining real consequences of understanding or misunderstanding this example.

## Self-Containment Rules

**Critical requirement**: Examples must be copy-paste-runnable within their scope.

**Requirements**:

- Full file content shown (not snippets)
- All necessary imports included
- No references to previous examples for required context
- CLI commands shown in full with expected output

## How to Use This Tutorial

### Prerequisites

Before starting, ensure you have:

- Python 3.11+ installed
- PostgreSQL (or another supported database) running
- Basic Python knowledge (modules, functions, type hints)
- Basic SQLAlchemy knowledge (declarative models, Column types)
- Basic database knowledge (SQL DDL, relational concepts)

### Running Examples

All migration examples assume a standard Alembic project structure:

```bash
# Install Alembic and SQLAlchemy
pip install alembic sqlalchemy psycopg2-binary

# Initialize a new project (Example 1)
alembic init alembic

# Run migrations
alembic upgrade head

# Check current version
alembic current
```

### Learning Path

**For Python developers new to Alembic**:

1. Work through Examples 1-15 to understand initialization, revision structure, and basic CLI commands
2. Study Examples 16-25 to master SQLAlchemy integration and advanced column types
3. Complete Examples 26-30 for data migrations and complex real-world patterns

**For developers migrating from another tool** (Flyway, Liquibase, Django migrations):

1. Read Example 1-3 to understand Alembic's file layout
2. Jump to Example 11-14 for CLI command equivalents
3. Study Example 17 for the autogenerate workflow unique to Alembic

**For quick reference**:

- Use example numbers as reference (e.g., "See Example 22 for UUID columns")
- Search for specific patterns (Ctrl+F for "autogenerate", "create_foreign_key", etc.)
- Copy-paste examples as starting points for your revision files

### Coverage Progression

As you progress through examples, you will achieve cumulative coverage:

- **After Example 15**: 50% — Can initialize Alembic, create revisions, run basic DDL operations, and use the CLI
- **After Example 25**: 80% — Can handle autogenerate, advanced column types, constraints, and real-world schemas
- **After Example 30**: 95% — Expert-level Alembic mastery for production use

## Code Annotation Philosophy

Every example uses **educational annotations** to show exactly what happens:

```python
# Migration header
revision: str = "001"              # => this revision's unique ID
down_revision: str | None = None   # => no parent: this is the base migration

def upgrade() -> None:
    op.create_table(               # => emits CREATE TABLE DDL
        "users",                   # => table name in database
        sa.Column("id", sa.Integer, primary_key=True),
        # => id INTEGER PRIMARY KEY
        sa.Column("name", sa.String(100), nullable=False),
        # => name VARCHAR(100) NOT NULL
    )
    # => alembic_version row updated: rev = "001"
```

Annotations show:

- **Variable states** after operations
- **SQL statements** emitted by Alembic
- **Version table changes** after upgrade/downgrade
- **Return values** and their types
- **Common gotchas** and edge cases

## Quality Standards

Every example in this tutorial meets these standards:

- **Self-contained**: Copy-paste-runnable within chapter scope
- **Annotated**: Every significant line has an inline comment
- **Production-relevant**: Real-world patterns, not toy examples
- **Accessible**: Color-blind friendly diagrams, clear structure

## Next Steps

Ready to start? Begin with [Beginner Examples (1-30)](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner) to build complete Alembic mastery.

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: Initializing Alembic](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-1-initializing-alembic)
- [Example 2: alembic.ini Configuration](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-2-alembicini-configuration)
- [Example 3: env.py Structure and Purpose](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-3-envpy-structure-and-purpose)
- [Example 4: Creating a First Revision](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-4-creating-a-first-revision)
- [Example 5: upgrade() and downgrade() Functions](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-5-upgrade-and-downgrade-functions)
- [Example 6: op.create_table](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-6-opcreate_table)
- [Example 7: op.add_column](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-7-opadd_column)
- [Example 8: op.drop_column](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-8-opdrop_column)
- [Example 9: op.create_index](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-9-opcreate_index)
- [Example 10: op.create_foreign_key](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-10-opcreate_foreign_key)
- [Example 11: Running Migrations](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-11-running-migrations)
- [Example 12: Downgrading](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-12-downgrading)
- [Example 13: Checking Current Version](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-13-checking-current-version)
- [Example 14: Viewing History](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-14-viewing-history)
- [Example 15: Revision with Message](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-15-revision-with-message)
- [Example 16: SQLAlchemy Metadata Integration](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-16-sqlalchemy-metadata-integration)
- [Example 17: Autogenerate Basics](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-17-autogenerate-basics)
- [Example 18: op.alter_column](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-18-opalter_column)
- [Example 19: op.create_unique_constraint](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-19-opcreate_unique_constraint)
- [Example 20: op.create_check_constraint](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-20-opcreate_check_constraint)
- [Example 21: Adding NOT NULL with server_default](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-21-adding-not-null-with-server_default)
- [Example 22: UUID Columns with PostgreSQL](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-22-uuid-columns-with-postgresql)
- [Example 23: Timestamp Columns with Defaults](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-23-timestamp-columns-with-defaults)
- [Example 24: Enum Columns](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-24-enum-columns)
- [Example 25: Composite Indexes](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-25-composite-indexes)
- [Example 26: Junction Tables](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-26-junction-tables)
- [Example 27: Seed Data with op.bulk_insert](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-27-seed-data-with-opbulk_insert)
- [Example 28: Multiple Operations in One Revision](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-28-multiple-operations-in-one-revision)
- [Example 29: alembic_version Table Structure](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-29-alembic_version-table-structure)
- [Example 30: Downgrade to Base](/en/learn/software-engineering/data/tools/python-alembic/by-example/beginner#example-30-downgrade-to-base)

### Intermediate (Examples 31–60)

- [Example 31: Autogenerate with --autogenerate](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-31-autogenerate-with---autogenerate)
- [Example 32: Autogenerate Limitations and Manual Fixes](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-32-autogenerate-limitations-and-manual-fixes)
- [Example 33: Batch Operations for SQLite (op.batch_alter_table)](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-33-batch-operations-for-sqlite-opbatch_alter_table)
- [Example 34: Data Migration with op.execute](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-34-data-migration-with-opexecute)
- [Example 35: Bulk Insert (op.bulk_insert)](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-35-bulk-insert-opbulk_insert)
- [Example 36: Branching Revisions](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-36-branching-revisions)
- [Example 37: Multiple Heads Detection](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-37-multiple-heads-detection)
- [Example 38: Merging Branches (alembic merge)](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-38-merging-branches-alembic-merge)
- [Example 39: Offline Mode (--sql)](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-39-offline-mode---sql)
- [Example 40: Environment-Specific Configuration](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-40-environment-specific-configuration)
- [Example 41: Custom Migration Templates (script.py.mako)](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-41-custom-migration-templates-scriptpymako)
- [Example 42: Enum Type Creation and Alteration](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-42-enum-type-creation-and-alteration)
- [Example 43: JSON/JSONB Columns](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-43-jsonjsonb-columns)
- [Example 44: Array Columns (PostgreSQL)](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-44-array-columns-postgresql)
- [Example 45: Partial Indexes](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-45-partial-indexes)
- [Example 46: Full-Text Search Indexes](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-46-full-text-search-indexes)
- [Example 47: Creating Views](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-47-creating-views)
- [Example 48: Creating Materialized Views](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-48-creating-materialized-views)
- [Example 49: Trigger Functions](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-49-trigger-functions)
- [Example 50: Stored Procedures](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-50-stored-procedures)
- [Example 51: Conditional Migration Logic](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-51-conditional-migration-logic)
- [Example 52: Batch Data Migration Pattern](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-52-batch-data-migration-pattern)
- [Example 53: Migration Testing with pytest](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-53-migration-testing-with-pytest)
- [Example 54: Test Database Setup with testcontainers](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-54-test-database-setup-with-testcontainers)
- [Example 55: Table Partitioning](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-55-table-partitioning)
- [Example 56: Generated/Computed Columns](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-56-generatedcomputed-columns)
- [Example 57: GIN Index for JSONB](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-57-gin-index-for-jsonb)
- [Example 58: Composite Primary Keys](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-58-composite-primary-keys)
- [Example 59: Migration Dependencies and Ordering](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-59-migration-dependencies-and-ordering)
- [Example 60: Alembic Stamps (alembic stamp)](/en/learn/software-engineering/data/tools/python-alembic/by-example/intermediate#example-60-alembic-stamps-alembic-stamp)

### Advanced (Examples 61–85)

- [Example 61: Custom Migration Operations (MigrateOperation)](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-61-custom-migration-operations-migrateoperation)
- [Example 62: Alembic with Async Engines (asyncio)](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-62-alembic-with-async-engines-asyncio)
- [Example 63: Zero-Downtime Column Addition](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-63-zero-downtime-column-addition)
- [Example 64: Zero-Downtime Column Removal (3-Phase)](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-64-zero-downtime-column-removal-3-phase)
- [Example 65: Zero-Downtime Table Rename](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-65-zero-downtime-table-rename)
- [Example 66: Large Table Migration with Batched Updates](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-66-large-table-migration-with-batched-updates)
- [Example 67: Online Index Creation (CONCURRENTLY)](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-67-online-index-creation-concurrently)
- [Example 68: Data Backfill Pattern](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-68-data-backfill-pattern)
- [Example 69: Alembic in CI/CD Pipeline](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-69-alembic-in-cicd-pipeline)
- [Example 70: Migration Testing with pytest](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-70-migration-testing-with-pytest)
- [Example 71: Migration Rollback Testing](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-71-migration-rollback-testing)
- [Example 72: Multi-Database Migrations (multidb Template)](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-72-multi-database-migrations-multidb-template)
- [Example 73: Schema-Level Migrations](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-73-schema-level-migrations)
- [Example 74: Custom Comparators for Autogenerate](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-74-custom-comparators-for-autogenerate)
- [Example 75: Version Locations (Multiple Directories)](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-75-version-locations-multiple-directories)
- [Example 76: Post-Migration Hooks](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-76-post-migration-hooks)
- [Example 77: Migration Squashing Pattern](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-77-migration-squashing-pattern)
- [Example 78: Blue-Green Deployment Migrations](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-78-blue-green-deployment-migrations)
- [Example 79: Feature Flag Migration Pattern](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-79-feature-flag-migration-pattern)
- [Example 80: Multi-Tenant Schema Migration](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-80-multi-tenant-schema-migration)
- [Example 81: Migration with pgcrypto Encryption](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-81-migration-with-pgcrypto-encryption)
- [Example 82: Audit Trail Table Migration](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-82-audit-trail-table-migration)
- [Example 83: Soft Delete Schema Pattern](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-83-soft-delete-schema-pattern)
- [Example 84: Production Migration Checklist Pattern](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-84-production-migration-checklist-pattern)
- [Example 85: Migration Monitoring with FastAPI Integration](/en/learn/software-engineering/data/tools/python-alembic/by-example/advanced#example-85-migration-monitoring-with-fastapi-integration)
