---
title: Overview
weight: 100000
date: 2025-12-23T00:00:00+07:00
draft: false
description: Comprehensive Elixir Ecto database wrapper overview covering schemas, queries, changesets, associations, transactions, and production patterns with 85 annotated examples
---

Ecto is Elixir's database wrapper and query generator providing a composable API for database interactions with strong compile-time guarantees.

## Getting Started

Before diving into Ecto development, get up and running:

1. **[Initial Setup](/en/learn/software-engineering/data/tools/elixir-ecto/initial-setup)** - Install Elixir, Ecto, PostgreSQL, configure database connection
2. **[Quick Start](/en/learn/software-engineering/data/tools/elixir-ecto/quick-start)** - Your first schema, basic queries, essential patterns

These foundational tutorials (0-30% coverage) prepare you for comprehensive Ecto learning.

## What You'll Learn

- **Schemas** - Define database tables as Elixir structs
- **Queries** - Compose type-safe database queries with Ecto.Query
- **Changesets** - Validate and transform data before database operations
- **Associations** - Model relationships between tables (belongs_to, has_many, many_to_many)
- **Migrations** - Version control database schema changes
- **Transactions** - Ensure data consistency with Ecto.Multi
- **Production Patterns** - Connection pooling, prepared statements, query optimization

## Why Ecto

Ecto provides compile-time safety and composable queries that eliminate entire categories of runtime errors common in other database libraries. Its changeset system separates data validation from schema definition, enabling reusable validation pipelines across different contexts (registration, profile update, admin override).

### When to Choose Ecto

Ecto excels in scenarios requiring:

- **Data integrity** — Changesets validate and transform data before it reaches the database, catching errors at the application boundary
- **Composable queries** — Ecto.Query builds queries as data structures that compose naturally with Elixir's pipe operator
- **Concurrent workloads** — Built on Elixir/OTP, Ecto's connection pool handles thousands of concurrent database operations
- **Schema evolution** — Reversible migrations with rollback support and version tracking

### Ecto vs Other Database Libraries

- **vs ActiveRecord (Ruby)** — Ecto separates schemas from changesets and queries, avoiding the "fat model" problem. ActiveRecord couples validation, persistence, and querying into one object
- **vs SQLAlchemy (Python)** — Ecto provides compile-time query validation while SQLAlchemy validates at runtime. Ecto's changeset system has no direct equivalent in SQLAlchemy
- **vs Spring Data JPA (Java)** — Ecto embraces explicit queries and functional composition while Spring Data JPA relies on method naming conventions and annotation-driven configuration
- **vs Prisma (TypeScript)** — Ecto integrates deeply with Elixir's type system and OTP patterns while Prisma generates a type-safe client from schema files

## Next Steps

Start your Ecto journey:

1. **[Ecto By-Example Overview](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/overview)** — Understand the by-example approach
2. **[Beginner Examples](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/beginner)** — Master fundamentals
3. **[Intermediate Examples](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/intermediate)** — Production patterns
4. **[Advanced Examples](/en/learn/software-engineering/data/tools/elixir-ecto/by-example/advanced)** — Expert mastery
