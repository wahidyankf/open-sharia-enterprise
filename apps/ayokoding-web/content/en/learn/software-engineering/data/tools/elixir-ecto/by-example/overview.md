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
