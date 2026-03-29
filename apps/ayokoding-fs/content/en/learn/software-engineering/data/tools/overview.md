---
title: Overview
weight: 100000
date: 2025-12-23T00:00:00+07:00
draft: false
description: Master data access patterns with Spring Data JPA and Elixir Ecto ORM frameworks
---

Data tools provide practical frameworks for accessing and manipulating database data from application code. This section covers object-relational mapping (ORM) tools that bridge the gap between your application's object model and relational databases.

## What You'll Learn

- **Spring Data JPA** - Java persistence with JPA and Hibernate
- **Elixir Ecto** - Elixir database wrapper and query DSL

## Available Tools

### Spring Data JPA - Java Database Access

**[Spring Data JPA](/en/learn/software-engineering/data/tools/spring-data-jpa)** provides elegant database access for Java applications:

- **Entity Mapping** - Map Java classes to database tables with annotations
- **Repository Pattern** - Interface-based data access with automatic implementation
- **Query Methods** - Generate SQL from method names automatically
- **JPQL Queries** - Write database-agnostic queries in Java
- **Transaction Management** - Declarative transaction control with @Transactional
- **Spring Boot Integration** - Auto-configuration and minimal setup

### Elixir Ecto - Functional Database Access

**[Elixir Ecto](/en/learn/software-engineering/data/tools/elixir-ecto)** brings functional programming patterns to database access:

- **Schema Definitions** - Define data structures with changesets for validation
- **Composable Queries** - Build queries functionally with Ecto.Query
- **Migrations** - Version control your database schema
- **Multi-Database Support** - PostgreSQL, MySQL, SQLite adapters
- **Associations** - Define relationships between schemas
- **Repo Pattern** - Centralized database operations

## Learning Approach

Each tool provides **By Example** tutorials with practical code:

- **Beginner** - Core concepts, basic CRUD operations, query fundamentals
- **Intermediate** - Complex queries, relationships, transactions
- **Advanced** - Performance optimization, custom queries, advanced patterns

All examples are annotated and immediately runnable.

## Choosing Your Tool

**Use Spring Data JPA when**:

- Building Java applications with Spring Boot
- Need JPA standard compliance
- Working with complex object hierarchies
- Want automatic SQL generation from method names
- Require enterprise features (caching, auditing)

**Use Elixir Ecto when**:

- Building Elixir/Phoenix applications
- Prefer explicit, composable queries
- Want functional programming patterns
- Need changesets for data validation
- Require strong compile-time guarantees

## Getting Started

Choose the tool matching your language ecosystem:

- **Java developers** → [Spring Data JPA](/en/learn/software-engineering/data/tools/spring-data-jpa)
- **Elixir developers** → [Elixir Ecto](/en/learn/software-engineering/data/tools/elixir-ecto)

Both tools assume basic SQL knowledge. Complete the [SQL](/en/learn/software-engineering/data/databases/sql) tutorial first if you're new to relational databases.
