---
title: "Overview"
date: 2026-03-27T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn Java Liquibase through 30+ annotated code examples covering database schema migration patterns - ideal for experienced developers shipping reliable schema changes to production"
tags:
  ["java-liquibase", "tutorial", "by-example", "examples", "code-first", "database-migration", "changelog", "changeset"]
---

## What is Java Liquibase By Example?

**Java Liquibase By Example** is a code-first tutorial series teaching experienced Java developers how to manage database schema migrations reliably using Liquibase. Through 30 heavily annotated, self-contained examples, you will achieve deep coverage of Liquibase patterns—from writing your first changeset to managing rollbacks, Spring Boot auto-configuration, Maven/Gradle plugin integration, and understanding internal tracking tables.

This tutorial assumes you are an experienced developer familiar with Java, relational databases, and SQL. If you are new to relational databases or SQL, start with foundational database tutorials first.

## Why By Example?

**Philosophy**: Show the migration first, run it second, understand through direct interaction.

Traditional tutorials explain concepts then show code. By-example tutorials reverse this: every example is a working, runnable snippet with inline annotations showing exactly what happens at each step—changeset execution order, SQL generated, rollback behavior, and tracking table updates.

**Target Audience**: Experienced developers who:

- Already know Java and SQL fundamentals
- Understand relational databases and schema design
- Prefer learning through working code rather than narrative explanations
- Want comprehensive reference material covering production-grade migration patterns

**Not For**: Developers new to databases or SQL. This tutorial moves quickly and assumes foundational knowledge.

## What Does This Tutorial Cover?

**Beginner (Examples 1-30)** covers database migration fundamentals needed for production work with real teams and CI/CD pipelines.

### Included in Beginner Coverage

- **Changelog Formats**: YAML, XML, and SQL-formatted changelogs; master changelog with include/includeAll
- **Core Change Types**: createTable, addColumn, dropColumn, createIndex, addForeignKeyConstraint
- **Constraint Management**: addNotNullConstraint, addUniqueConstraint, addDefaultValue
- **Schema Evolution**: modifyDataType, renameColumn, renameTable
- **Rollbacks**: rollbackCount, rollbackToTag, SQL rollback blocks, tag command
- **Operational Commands**: update, status, tag, generateChangeLog
- **Build Integration**: Spring Boot auto-configuration, Maven plugin, Gradle plugin
- **Internal Tables**: DATABASECHANGELOG and DATABASECHANGELOGLOCK structure and behavior

### Excluded from Beginner Coverage

- **Advanced Diffing**: generateChangeLog from existing schema, diff between databases
- **Custom Extensions**: Writing custom Change and Precondition classes
- **Complex Contexts and Labels**: Multi-environment conditional execution
- **Liquibase Hub and Pro Features**: Remote change tracking, drift detection
- **Database-Specific Change Types**: Vendor extensions beyond standard SQL

## Tutorial Structure

### 30 Examples in One Level

**Sequential numbering**: Examples 1-30 (unified reference system)

**Distribution**:

- **Changelog Basics** (Examples 1-5): Master changelog, SQL formatted changeset, author/ID conventions, createTable, addColumn
- **Schema Changes** (Examples 6-13): dropColumn, createIndex, addForeignKeyConstraint, running update, rollbackCount, rollbackToTag, tag command, status command
- **Changelog Formats** (Examples 14-17): XML, YAML, SQL formatted, include/includeAll
- **Constraint and Type Changes** (Examples 18-23): addNotNullConstraint, addUniqueConstraint, modifyDataType, renameColumn, renameTable, addDefaultValue
- **Build Tool Integration** (Examples 24-26): Spring Boot auto-config, Maven plugin, Gradle plugin
- **Structural Patterns** (Examples 27-30): Multiple changesets per file, rollback SQL blocks, DATABASECHANGELOG table, DATABASECHANGELOGLOCK table

## Five-Part Example Format

Every example follows a **mandatory five-part structure**:

### Part 1: Brief Explanation (2-3 sentences)

Answers what this concept is, why it matters in production, and when to use it.

### Part 2: Mermaid Diagram (when appropriate)

Included for examples where execution flow, table relationships, or command sequences benefit from visualization. Skipped for straightforward single-operation examples.

**Diagram requirements**:

- Color-blind friendly palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
- Vertical orientation (mobile-first)
- Clear labels on all nodes and edges
- Comment syntax: `%%` (NOT `%%{ }%%`)

### Part 3: Heavily Annotated Code

**Core requirement**: Every significant line has an inline comment showing what happens.

**Comment annotation notation**:

- `-- =>` for SQL files
- `# =>` for YAML files
- `<!-- => -->` for XML files (inline comment approximation shown as `<!-- ... -->`)

```sql
-- liquibase formatted sql
-- changeset author:001-create-products dbms:postgresql
-- => Header declares this file as Liquibase-managed SQL
-- => changeset id "001-create-products" with author "author" runs exactly once
-- => dbms:postgresql restricts execution to PostgreSQL only

CREATE TABLE products (
    id   UUID NOT NULL DEFAULT gen_random_uuid(),
    -- => gen_random_uuid() generates a v4 UUID; requires pgcrypto or PostgreSQL 13+
    name VARCHAR(255) NOT NULL
    -- => NOT NULL enforced at database level; Liquibase does not add application-level validation
);
-- rollback DROP TABLE products;
-- => rollback block tells Liquibase how to undo this changeset
-- => executed when running liquibase rollback or rollbackCount
```

### Part 4: Key Takeaway (1-2 sentences)

Distills the core insight—the most important pattern and when to apply it in production.

### Part 5: Why It Matters (50-100 words)

Explains production relevance, common pitfalls, and real-world impact of the concept.

## Self-Containment Rules

Every example is self-contained and runnable given a Liquibase installation and a running PostgreSQL database. Examples reference only standard Liquibase change types and core SQL. Examples do not depend on code from other examples.

## Code Annotation Philosophy

Every example uses **educational annotations** to show exactly what happens:

```yaml
databaseChangeLog: # => Root key for YAML changelog format
  - includeAll: # => Scans a directory and includes all changelog files found
      path: db/changelog/changes/
      # => path is relative to the classpath root (src/main/resources/)
      # => files are sorted alphabetically and executed in order
      # => use numeric prefixes (001-, 002-) to control execution order
```

Annotations show:

- **Command execution semantics**: What Liquibase does at each step
- **Database side effects**: Tables created, columns added, constraints applied
- **SQL generated**: What DDL Liquibase executes behind the scenes
- **Tracking table updates**: When DATABASECHANGELOG rows are inserted
- **Rollback behavior**: What happens on undo
- **Common gotchas**: Idempotency, ordering, lock contention

## Quality Standards

Every example in this tutorial meets these standards:

- **Self-contained**: Runnable with a Liquibase installation and PostgreSQL
- **Annotated**: Every significant line has an inline comment
- **Production-relevant**: Real-world migration patterns matching actual project usage
- **Accessible**: Color-blind friendly diagrams, clear structure

## Next Steps

Ready to start?

- **New to Liquibase**: Start with [Beginner Examples (1-30)](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner)

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: Master Changelog File (YAML Format)](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-1-master-changelog-file-yaml-format)
- [Example 2: First SQL Formatted Changeset](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-2-first-sql-formatted-changeset)
- [Example 3: Changeset Author and ID Convention](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-3-changeset-author-and-id-convention)
- [Example 4: createTable Change Type (YAML Changelog)](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-4-createtable-change-type-yaml-changelog)
- [Example 5: addColumn Change Type](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-5-addcolumn-change-type)
- [Example 6: dropColumn Change Type](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-6-dropcolumn-change-type)
- [Example 7: createIndex Change Type](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-7-createindex-change-type)
- [Example 8: addForeignKeyConstraint Change Type](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-8-addforeignkeyconstraint-change-type)
- [Example 9: Running Liquibase Update](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-9-running-liquibase-update)
- [Example 10: Rollback with rollbackCount](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-10-rollback-with-rollbackcount)
- [Example 11: Rollback to Tag](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-11-rollback-to-tag)
- [Example 12: Tag Command](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-12-tag-command)
- [Example 13: Liquibase Status Command](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-13-liquibase-status-command)
- [Example 14: XML Changelog Format](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-14-xml-changelog-format)
- [Example 15: YAML Changelog Format](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-15-yaml-changelog-format)
- [Example 16: SQL Changelog with `-- liquibase formatted sql`](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-16-sql-changelog-with----liquibase-formatted-sql)
- [Example 17: Including Changelogs (include/includeAll)](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-17-including-changelogs-includeincludeall)
- [Example 18: addNotNullConstraint](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-18-addnotnullconstraint)
- [Example 19: addUniqueConstraint](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-19-adduniqueconstraint)
- [Example 20: modifyDataType](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-20-modifydatatype)
- [Example 21: renameColumn](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-21-renamecolumn)
- [Example 22: renameTable](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-22-renametable)
- [Example 23: addDefaultValue](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-23-adddefaultvalue)
- [Example 24: Spring Boot Auto-Configuration](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-24-spring-boot-auto-configuration)
- [Example 25: Liquibase Maven Plugin](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-25-liquibase-maven-plugin)
- [Example 26: Liquibase Gradle Plugin](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-26-liquibase-gradle-plugin)
- [Example 27: Multiple Changesets in One File](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-27-multiple-changesets-in-one-file)
- [Example 28: Rollback SQL Block](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-28-rollback-sql-block)
- [Example 29: DATABASECHANGELOG Table Structure](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-29-databasechangelog-table-structure)
- [Example 30: DATABASECHANGELOGLOCK Table](/en/learn/software-engineering/data/tools/java-liquibase/by-example/beginner#example-30-databasechangeloglock-table)

### Intermediate (Examples 31–60)

- [Example 31: Contexts for Environment-Specific Changes](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-31-contexts-for-environment-specific-changes)
- [Example 32: Labels for Change Categorization](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-32-labels-for-change-categorization)
- [Example 33: Preconditions (tableExists, columnExists)](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-33-preconditions-tableexists-columnexists)
- [Example 34: Precondition onFail/onError Strategies](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-34-precondition-onfailonerror-strategies)
- [Example 35: Data Migration with insert Change Type](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-35-data-migration-with-insert-change-type)
- [Example 36: Data Migration with update Change Type](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-36-data-migration-with-update-change-type)
- [Example 37: Data Migration with delete Change Type](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-37-data-migration-with-delete-change-type)
- [Example 38: loadData from CSV](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-38-loaddata-from-csv)
- [Example 39: Custom SQL Change Type](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-39-custom-sql-change-type)
- [Example 40: Rollback Strategies for Data Changes](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-40-rollback-strategies-for-data-changes)
- [Example 41: Changelog Parameters and Property Substitution](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-41-changelog-parameters-and-property-substitution)
- [Example 42: changeLogSync Command](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-42-changelogsync-command)
- [Example 43: diff Command](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-43-diff-command)
- [Example 44: diffChangeLog Command](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-44-diffchangelog-command)
- [Example 45: generateChangeLog Command](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-45-generatechangelog-command)
- [Example 46: Multiple Schema Support](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-46-multiple-schema-support)
- [Example 47: Creating Views](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-47-creating-views)
- [Example 48: Creating Materialized Views](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-48-creating-materialized-views)
- [Example 49: Trigger Creation](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-49-trigger-creation)
- [Example 50: Stored Procedure Creation](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-50-stored-procedure-creation)
- [Example 51: Conditional Execution with dbms Attribute](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-51-conditional-execution-with-dbms-attribute)
- [Example 52: Batch Data Migration Pattern](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-52-batch-data-migration-pattern)
- [Example 53: Migration Testing with Spring Boot Test](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-53-migration-testing-with-spring-boot-test)
- [Example 54: Test Database Setup with Testcontainers](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-54-test-database-setup-with-testcontainers)
- [Example 55: JSON/JSONB Columns](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-55-jsonjsonb-columns)
- [Example 56: Array Columns](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-56-array-columns)
- [Example 57: Full-Text Search Indexes](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-57-full-text-search-indexes)
- [Example 58: Table Partitioning](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-58-table-partitioning)
- [Example 59: Composite Primary Keys](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-59-composite-primary-keys)
- [Example 60: Changelog Validation and Best Practices](/en/learn/software-engineering/data/tools/java-liquibase/by-example/intermediate#example-60-changelog-validation-and-best-practices)

### Advanced (Examples 61–85)

- [Example 61: Custom Change Class (AbstractChange)](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-61-custom-change-class-abstractchange)
- [Example 62: Liquibase Extensions](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-62-liquibase-extensions)
- [Example 63: Zero-Downtime Column Addition](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-63-zero-downtime-column-addition)
- [Example 64: Zero-Downtime Column Removal (3-Phase)](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-64-zero-downtime-column-removal-3-phase)
- [Example 65: Zero-Downtime Table Rename](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-65-zero-downtime-table-rename)
- [Example 66: Large Table Migration with Batched Updates](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-66-large-table-migration-with-batched-updates)
- [Example 67: Online Index Creation (CONCURRENTLY)](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-67-online-index-creation-concurrently)
- [Example 68: Data Backfill Pattern](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-68-data-backfill-pattern)
- [Example 69: Liquibase in CI/CD Pipeline](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-69-liquibase-in-cicd-pipeline)
- [Example 70: Migration Rollback Testing](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-70-migration-rollback-testing)
- [Example 71: Changelog Locking Deep Dive](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-71-changelog-locking-deep-dive)
- [Example 72: Blue-Green Deployment Migrations](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-72-blue-green-deployment-migrations)
- [Example 73: Feature Flag Migration Pattern](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-73-feature-flag-migration-pattern)
- [Example 74: Multi-Tenant Migrations](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-74-multi-tenant-migrations)
- [Example 75: Migration with pgcrypto Encryption](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-75-migration-with-pgcrypto-encryption)
- [Example 76: Audit Trail Table Migration](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-76-audit-trail-table-migration)
- [Example 77: Soft Delete Schema Pattern](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-77-soft-delete-schema-pattern)
- [Example 78: Migration Performance Benchmarking](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-78-migration-performance-benchmarking)
- [Example 79: Schema Drift Detection](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-79-schema-drift-detection)
- [Example 80: Migration Dependency Graph](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-80-migration-dependency-graph)
- [Example 81: Spring Boot Integration Patterns](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-81-spring-boot-integration-patterns)
- [Example 82: Vert.x Integration Pattern](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-82-vertx-integration-pattern)
- [Example 83: Migration Squashing Pattern](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-83-migration-squashing-pattern)
- [Example 84: Production Migration Checklist](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-84-production-migration-checklist)
- [Example 85: Migration Monitoring and Alerting](/en/learn/software-engineering/data/tools/java-liquibase/by-example/advanced#example-85-migration-monitoring-and-alerting)
