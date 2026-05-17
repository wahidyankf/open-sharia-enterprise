---
title: "Overview"
date: 2025-12-29T17:29:25+07:00
draft: false
weight: 10000000
description: "Learn Elixir Ecto through 85+ annotated code examples covering 95% of the library - ideal for experienced developers building production data access layers"
tags: ["elixir-ecto", "tutorial", "by-example", "examples", "code-first", "ecto", "database", "orm"]
---

## What is Elixir Ecto By Example?

**Elixir Ecto By Example** is a code-first tutorial series teaching experienced Elixir developers how to build production-ready data access layers using Ecto. Through 85 heavily annotated, self-contained examples, you'll achieve 95% coverage of Ecto patterns—from basic CRUD operations to advanced dynamic queries, custom types, and performance optimization.

This tutorial assumes you're an experienced developer familiar with Elixir, pattern matching, and relational databases. If you're new to Elixir, start with foundational Elixir tutorials first.

## Why By Example?

**Philosophy**: Show the code first, run it second, understand through direct interaction.

Traditional tutorials explain concepts then show code. By-example tutorials reverse this: every example is a working, runnable code snippet with inline annotations showing exactly what happens at each step—changeset states, SQL queries executed, results returned, and common pitfalls.

**Target Audience**: Experienced developers who:

- Already know Elixir fundamentals and pattern matching
- Understand relational databases and SQL
- Prefer learning through working code rather than narrative explanations
- Want comprehensive reference material covering 95% of production patterns

**Not For**: Developers new to Elixir or databases. This tutorial moves quickly and assumes foundational knowledge.

## What Does 95% Coverage Mean?

**95% coverage** means depth and breadth of Ecto features needed for production work, not toy examples.

### Included in 95% Coverage

- **Repository Patterns**: Repo operations (insert, update, delete, get, all), batch operations, upserts, transactions
- **Schemas**: Schema definition, embedded schemas, field types, primary keys, source naming
- **Changesets**: Validation, casting, constraints, associations, nested changesets
- **Queries**: Ecto.Query DSL (from, where, select, join, order_by, group_by), query composition
- **Associations**: belongs_to, has_one, has_many, many_to_many, preloading strategies
- **Transactions**: Repo.transaction, Multi operations, rollback, isolation levels
- **Aggregations**: count, sum, avg, min, max, group_by with aggregates
- **Joins**: Inner joins, left joins, right joins, full joins, lateral joins
- **Dynamic Queries**: Building queries programmatically, conditional filters, search patterns
- **Migrations**: Creating tables, altering schemas, indexes, constraints, data migrations
- **Custom Types**: Implementing Ecto.Type, custom field types, parameterized types
- **Embedded Schemas**: embeds_one, embeds_many, JSON fields
- **Advanced Patterns**: Subqueries, CTEs, window functions, fragments, query hints
- **Performance**: N+1 prevention, batch loading, query optimization, explain plans

### Excluded from 95% (the remaining 5%)

- **Framework Internals**: Ecto adapter implementation details, connection pool mechanics
- **Rare Edge Cases**: Obscure feature combinations not used in typical production code
- **Database-Specific**: Vendor-specific features outside standard SQL (unless commonly used)
- **Legacy Features**: Deprecated APIs from Ecto 1.x or 2.x
- **Advanced Database**: Exotic window functions, recursive CTEs beyond standard use cases

## Tutorial Structure

### 85 Examples Across Three Levels

**Sequential numbering**: Examples 1-85 (unified reference system)

**Distribution**:

- **Beginner** (Examples 1-30): 0-40% coverage - Repository basics, schemas, changesets, basic queries, simple associations
- **Intermediate** (Examples 31-60): 40-75% coverage - Advanced queries, complex associations, transactions, aggregations, migrations
- **Advanced** (Examples 61-85): 75-95% coverage - Dynamic queries, custom types, subqueries, CTEs, performance optimization

**Rationale**: 85 examples provide granular progression from CRUD operations to expert mastery without overwhelming maintenance burden.

## Four-Part Example Format

Every example follows a **mandatory five-part structure**:

### Part 1: Brief Explanation (2-3 sentences)

**Answers**:

- What is this concept/pattern?
- Why does it matter in production code?
- When should you use it?

**Example**:

> ### Example 12: has_many Association with Preloading
>
> The has_many association maps a single entity to multiple related entities, commonly used for parent-child relationships like User → Posts or Order → OrderItems. Preloading associated data prevents N+1 query problems by fetching all related records in a single additional query, crucial for production performance.

### Part 2: Mermaid Diagram (when appropriate)

**Included when** (~40% of examples):

- Data flow between Repo and database is non-obvious
- Schema relationships involve multiple tables
- Query execution flow has multiple stages
- N+1 problems or lazy loading behavior requires illustration
- Transaction boundaries need visualization

**Skipped when**:

- Simple CRUD operations with clear linear flow
- Single-table queries without joins
- Trivial changeset validations

**Diagram requirements**:

- Use color-blind friendly palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
- Vertical orientation (mobile-first)
- Clear labels on all nodes and edges
- Comment syntax: `%%` (NOT `%%{ }%%`)

### Part 3: Heavily Annotated Code

**Core requirement**: Every significant line must have an inline comment

**Comment annotations use `# =>` notation**:

```elixir
user = %User{name: "Alice", age: 30}  # => user is struct (not persisted)
{:ok, saved} = Repo.insert(user)      # => saved is persisted user with id=1
updated = Ecto.Changeset.change(saved, age: 31)
                                      # => updated is changeset with change: %{age: 31}
{:ok, result} = Repo.update(updated)  # => result.age is 31 (database updated)
IO.inspect(result)                    # => Output: %User{id: 1, name: "Alice", age: 31}
```

**Required annotations**:

- **Struct/changeset states**: Show values and persistence status
- **Query results**: Document what data is returned
- **SQL executed**: Show generated SQL when relevant
- **Side effects**: Document database mutations, transactions
- **Expected outputs**: Show IEx output with `=> Output:` prefix
- **Error cases**: Document when errors occur and how to handle

**Code organization**:

- Include full module definitions and aliases
- Define schemas and helper functions in-place for self-containment
- Use descriptive variable names
- Format code with `mix format`

### Part 4: Key Takeaway (1-2 sentences)

**Purpose**: Distill the core insight to its essence

**Must highlight**:

- The most important pattern or concept
- When to apply this in production
- Common pitfalls to avoid

**Example**:

```markdown
**Key Takeaway**: Always use Repo.preload/2 or join-based preloading when accessing associations to prevent N+1 queries, and prefer :all strategy for has_many when you need all associated records loaded.
```

## Self-Containment Rules

**Critical requirement**: Examples must be copy-paste-runnable within their chapter scope.

### Beginner Level Self-Containment

**Rule**: Each example is completely standalone

**Requirements**:

- Full module definition with schema
- All necessary aliases and imports
- Helper functions defined in-place
- No references to previous examples
- Runnable in IEx with proper setup

**Example structure**:

```elixir
defmodule User do
  use Ecto.Schema

  schema "users" do
    field :name, :string          # => field definition with type
    field :age, :integer          # => integer field
    timestamps()                  # => inserted_at and updated_at
  end
end

# Usage
user = %User{name: "Bob", age: 25}  # => struct creation
{:ok, saved} = Repo.insert(user)    # => persisted to database
```

### Intermediate Level Self-Containment

**Rule**: Examples assume beginner concepts but include all necessary code

**Allowed assumptions**:

- Reader knows basic Ecto.Schema and Repo operations
- Reader understands pattern matching and pipe operator
- Reader can run IEx commands

**Requirements**:

- Full schema definitions and associations
- Can reference beginner concepts ("as we saw with basic queries")
- Must be runnable without referring to previous examples
- Include migration snippets if schema structure is non-obvious

### Advanced Level Self-Containment

**Rule**: Examples assume beginner + intermediate knowledge but remain runnable

**Allowed assumptions**:

- Reader knows query composition and association preloading
- Reader understands Multi and transaction patterns
- Reader can navigate Ecto documentation for context

**Requirements**:

- Full runnable code with schema and query definitions
- Can reference patterns by name ("using the dynamic query pattern")
- Include all necessary imports and custom types
- Provide complete example even if building on earlier concepts

### Cross-Reference Guidelines

**Acceptable cross-references**:

```markdown
This builds on the Multi pattern from Example 45, but here's the complete code including the transaction setup...
```

**Unacceptable cross-references**:

```markdown
Use the `validate_user` function from Example 12 (code not shown).
```

**Golden rule**: If you delete all other examples, this example should still run in IEx.

## How to Use This Tutorial

### Prerequisites

Before starting, ensure you have:

- Elixir 1.14+ installed
- PostgreSQL (or your preferred database) running
- Basic Elixir knowledge (modules, functions, pattern matching)
- Basic database knowledge (SQL, relational concepts)

### Running Examples

All examples are designed to run in IEx:

```bash
# Start IEx with your project
iex -S mix

# Run example code directly
iex> user = %User{name: "Alice", age: 30}
iex> {:ok, saved} = Repo.insert(user)
```

### Learning Path

**For experienced Elixir developers new to Ecto**:

1. Skim beginner examples (1-30) - Review fundamentals quickly
2. Deep dive intermediate (31-60) - Master production patterns
3. Reference advanced (61-85) - Learn optimization and edge cases

**For developers switching from other ORMs**:

1. Read overview to understand Ecto philosophy
2. Jump to intermediate examples (31-60) - See how Ecto differs
3. Reference beginner for Ecto-specific syntax as needed
4. Use advanced for performance optimization

**For quick reference**:

- Use example numbers as reference (e.g., "See Example 42 for Multi operations")
- Search for specific patterns (Ctrl+F for "upsert", "subquery", etc.)
- Copy-paste examples as starting points for your code

### Coverage Progression

As you progress through examples, you'll achieve cumulative coverage:

- **After Beginner** (Example 30): 40% - Can build basic CRUD applications
- **After Intermediate** (Example 60): 75% - Can handle most production scenarios
- **After Advanced** (Example 85): 95% - Expert-level Ecto mastery

## Example Numbering System

**Sequential numbering**: Examples 1-85 across all three levels

**Why sequential?**

- Creates unified reference system ("See Example 42")
- Clear progression from fundamentals to mastery
- Easy to track coverage percentage

**Beginner**: Examples 1-30 (0-40% coverage)
**Intermediate**: Examples 31-60 (40-75% coverage)
**Advanced**: Examples 61-85 (75-95% coverage)

## Code Annotation Philosophy

Every example uses **educational annotations** to show exactly what happens:

```elixir
# Variable assignment with type
user = %User{name: "Alice"}           # => user is struct (type: User)

# Changeset creation
changeset = User.changeset(user, %{age: 30})
                                      # => changeset valid: true, changes: %{age: 30}

# Database operation
{:ok, saved} = Repo.insert(changeset) # => saved.id is 1 (persisted)

# Query execution
users = Repo.all(User)                # => users is [%User{id: 1, name: "Alice", age: 30}]
                                      # => SQL: SELECT * FROM users
```

Annotations show:

- **Struct/changeset states** after operations
- **Database side effects** (inserts, updates, deletes)
- **SQL queries** executed
- **Return values** and their types
- **Common gotchas** and edge cases

## Quality Standards

Every example in this tutorial meets these standards:

- **Self-contained**: Copy-paste-runnable within chapter scope
- **Annotated**: Every significant line has inline comment
- **Tested**: All code examples verified working
- **Production-relevant**: Real-world patterns, not toy examples
- **Accessible**: Color-blind friendly diagrams, clear structure

## Next Steps

Ready to start? Choose your path:

- **New to Ecto**: Start with [Beginner Examples (1-30)](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner)
- **Experienced with other ORMs**: Jump to [Intermediate Examples (31-60)](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate)
- **Performance optimization**: Skip to [Advanced Examples (61-85)](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced)

## Feedback and Contributions

Found an issue? Have a suggestion? This tutorial is part of the ayokoding-web learning platform. Check the repository for contribution guidelines.

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: Defining a Basic Schema](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-1-defining-a-basic-schema)
- [Example 2: Inserting Data with Repo.insert/1](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-2-inserting-data-with-repoinsert1)
- [Example 3: Querying with Repo.all/1](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-3-querying-with-repoall1)
- [Example 4: Fetching a Single Record with Repo.get/2](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-4-fetching-a-single-record-with-repoget2)
- [Example 5: Raising on Not Found with Repo.get!/2](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-5-raising-on-not-found-with-repoget2)
- [Example 6: Filtering with Repo.get_by/2](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-6-filtering-with-repoget_by2)
- [Example 7: Updating Records with Repo.update/1](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-7-updating-records-with-repoupdate1)
- [Example 8: Deleting Records with Repo.delete/1](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-8-deleting-records-with-repodelete1)
- [Example 9: Creating a Changeset with cast/3](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-9-creating-a-changeset-with-cast3)
- [Example 10: Validating Required Fields](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-10-validating-required-fields)
- [Example 11: Format Validation with validate_format/3](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-11-format-validation-with-validate_format3)
- [Example 12: Length Validation with validate_length/3](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-12-length-validation-with-validate_length3)
- [Example 13: Number Validation with validate_number/3](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-13-number-validation-with-validate_number3)
- [Example 14: Unique Constraint with unique_constraint/2](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-14-unique-constraint-with-unique_constraint2)
- [Example 15: Basic Query with from/2](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-15-basic-query-with-from2)
- [Example 16: Selecting Specific Fields](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-16-selecting-specific-fields)
- [Example 17: Ordering Results with order_by](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-17-ordering-results-with-order_by)
- [Example 18: Limiting Results with limit](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-18-limiting-results-with-limit)
- [Example 19: Offsetting Results with offset](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-19-offsetting-results-with-offset)
- [Example 20: Counting Records with Repo.aggregate/3](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-20-counting-records-with-repoaggregate3)
- [Example 21: Defining belongs_to Association](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-21-defining-belongs_to-association)
- [Example 22: Defining has_many Association](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-22-defining-has_many-association)
- [Example 23: Preloading Associations with Repo.preload/2](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-23-preloading-associations-with-repopreload2)
- [Example 24: Preloading in Query with preload](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-24-preloading-in-query-with-preload)
- [Example 25: Updating with Ecto.Changeset.change/2](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-25-updating-with-ectochangesetchange2)
- [Example 26: Inserting with Repo.insert!/1](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-26-inserting-with-repoinsert1)
- [Example 27: Deleting All Records with Repo.delete_all/1](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-27-deleting-all-records-with-repodelete_all1)
- [Example 28: Updating All Records with Repo.update_all/2](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-28-updating-all-records-with-repoupdate_all2)
- [Example 29: Fetching One Record with Repo.one/1](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-29-fetching-one-record-with-repoone1)
- [Example 30: Upserting with Repo.insert/2 and on_conflict](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner#example-30-upserting-with-repoinsert2-and-on_conflict)

### Intermediate (Examples 31–60)

- [Example 31: Join Query with join/5](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-31-join-query-with-join5)
- [Example 32: Left Join with left_join/5](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-32-left-join-with-left_join5)
- [Example 33: Group By with Aggregates](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-33-group-by-with-aggregates)
- [Example 34: Having Clause for Filtered Aggregates](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-34-having-clause-for-filtered-aggregates)
- [Example 35: Transactions with Repo.transaction/1](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-35-transactions-with-repotransaction1)
- [Example 36: Rolling Back Transactions with Repo.rollback/1](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-36-rolling-back-transactions-with-reporollback1)
- [Example 37: Ecto.Multi for Composable Transactions](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-37-ectomulti-for-composable-transactions)
- [Example 38: Conditional Multi Operations](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-38-conditional-multi-operations)
- [Example 39: Migration Basics - Creating Tables](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-39-migration-basics---creating-tables)
- [Example 40: Migration - Adding Columns](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-40-migration---adding-columns)
- [Example 41: Migration - Adding Indexes](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-41-migration---adding-indexes)
- [Example 42: Migration - Adding Foreign Keys](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-42-migration---adding-foreign-keys)
- [Example 43: Embedded Schemas with embeds_one](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-43-embedded-schemas-with-embeds_one)
- [Example 44: Embedded Schemas with embeds_many](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-44-embedded-schemas-with-embeds_many)
- [Example 45: Composite Primary Keys](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-45-composite-primary-keys)
- [Example 46: Custom Primary Key Types](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-46-custom-primary-key-types)
- [Example 47: many_to_many Associations](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-47-many_to_many-associations)
- [Example 48: Putting Associations with put_assoc/3](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-48-putting-associations-with-put_assoc3)
- [Example 49: Casting Associations with cast_assoc/3](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-49-casting-associations-with-cast_assoc3)
- [Example 50: Subqueries for Complex Filtering](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-50-subqueries-for-complex-filtering)
- [Example 51: Fragment for Raw SQL Expressions](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-51-fragment-for-raw-sql-expressions)
- [Example 52: Distinct Queries with distinct/2](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-52-distinct-queries-with-distinct2)
- [Example 53: Lock Queries with lock/2](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-53-lock-queries-with-lock2)
- [Example 54: Select Merge for Field Updates](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-54-select-merge-for-field-updates)
- [Example 55: Windows Functions with over/2](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-55-windows-functions-with-over2)
- [Example 56: Common Table Expressions (CTEs) with with_cte/3](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-56-common-table-expressions-ctes-with-with_cte3)
- [Example 57: Batch Insert with insert_all/3](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-57-batch-insert-with-insert_all3)
- [Example 58: Returning Inserted Data with returning Option](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-58-returning-inserted-data-with-returning-option)
- [Example 59: Schemaless Queries for Flexibility](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-59-schemaless-queries-for-flexibility)
- [Example 60: Query Prefixes for Multi-Tenancy](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate#example-60-query-prefixes-for-multi-tenancy)

### Advanced (Examples 61–85)

- [Example 61: Building Dynamic Queries](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-61-building-dynamic-queries)
- [Example 62: Dynamic Order By](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-62-dynamic-order-by)
- [Example 63: Implementing Custom Ecto.Type](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-63-implementing-custom-ectotype)
- [Example 64: Parameterized Types](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-64-parameterized-types)
- [Example 65: Optimistic Locking with :version](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-65-optimistic-locking-with-version)
- [Example 66: Association Preloading Strategies](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-66-association-preloading-strategies)
- [Example 67: Preventing N+1 Queries with Dataloader](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-67-preventing-n1-queries-with-dataloader)
- [Example 68: Lazy vs Eager Loading](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-68-lazy-vs-eager-loading)
- [Example 69: Repo.stream for Large Result Sets](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-69-repostream-for-large-result-sets)
- [Example 70: Preparing Queries for Performance](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-70-preparing-queries-for-performance)
- [Example 71: Using Indexes Effectively](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-71-using-indexes-effectively)
- [Example 72: Analyzing Query Performance with EXPLAIN](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-72-analyzing-query-performance-with-explain)
- [Example 73: Transactions with Savepoints](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-73-transactions-with-savepoints)
- [Example 74: Schema-less Changesets for Validation](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-74-schema-less-changesets-for-validation)
- [Example 75: Custom Changeset Validators](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-75-custom-changeset-validators)
- [Example 76: Unsafe Fragments and SQL Injection Prevention](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-76-unsafe-fragments-and-sql-injection-prevention)
- [Example 77: Polymorphic Associations with Type Field](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-77-polymorphic-associations-with-type-field)
- [Example 78: Using Ecto.Query.API for Type Casting](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-78-using-ectoqueryapi-for-type-casting)
- [Example 79: Repo.exists? for Existence Checks](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-79-repoexists-for-existence-checks)
- [Example 80: Aggregates in Subqueries](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-80-aggregates-in-subqueries)
- [Example 81: Using Repo.in_transaction? for Context Awareness](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-81-using-repoin_transaction-for-context-awareness)
- [Example 82: Conditional Updates with Repo.update_all and Expressions](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-82-conditional-updates-with-repoupdate_all-and-expressions)
- [Example 83: Repo Callbacks with Ecto.Repo.Callbacks](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-83-repo-callbacks-with-ectorepocallbacks)
- [Example 84: Schema Reflection with **schema**](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-84-schema-reflection-with-schema)
- [Example 85: Production Best Practices Checklist](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced#example-85-production-best-practices-checklist)
