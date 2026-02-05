---
title: "Ecto Patterns"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000018
description: "Database access patterns with Ecto for production Elixir applications"
tags: ["elixir", "ecto", "database", "postgresql", "orm"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/phoenix-channels"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/rest-api-design"
---

**Building database-backed Elixir applications?** This guide teaches Ecto patterns through the OTP-First progression, starting with raw SQL via Postgrex to understand database access challenges before introducing Ecto's schema-based abstractions.

## Why Ecto Matters

Most production applications need persistent data storage:

- **Web applications** - User accounts, content management, transaction history
- **Financial systems** - Transaction records, account balances, audit logs
- **E-commerce platforms** - Product catalogs, orders, inventory tracking
- **API backends** - Resource persistence, caching, session storage

Elixir provides two approaches:

1. **Raw SQL drivers** - Postgrex for PostgreSQL (maximum control, manual everything)
2. **Ecto library** - Schema-based data layer with query DSL (production standard)

**Our approach**: Start with raw Postgrex to understand SQL composition challenges, then see how Ecto solves them with schemas, changesets, and transactions.

## OTP Primitives - Raw SQL with Postgrex

### Basic Database Connection

Let's query PostgreSQL using raw SQL:

```elixir
# Raw PostgreSQL queries with Postgrex
# Add to mix.exs: {:postgrex, "~> 0.17"}

# Start connection
{:ok, pid} = Postgrex.start_link(
  hostname: "localhost",                         # => Database host
  username: "postgres",                          # => Database user
  password: "postgres",                          # => Database password
  database: "myapp_dev"                          # => Database name
)
# => pid: Connection process
# => Returns: {:ok, pid}

# Simple query
{:ok, result} = Postgrex.query(pid, "SELECT * FROM users WHERE id = $1", [1])
# => $1: Parameterized query (SQL injection safe)
# => [1]: Parameters
# => Returns: {:ok, %Postgrex.Result{}}

result.rows                                      # => [[1, "Alice", "alice@example.com"]]
                                                 # => List of row tuples
result.columns                                   # => ["id", "name", "email"]
                                                 # => Column names
result.num_rows                                  # => 1
```

### Manual CRUD Operations

Implementing create, read, update, delete manually:

```elixir
defmodule UserRepository do
  # Create user
  def create(conn, name, email) do
    sql = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
                                                 # => RETURNING: Get inserted row
    case Postgrex.query(conn, sql, [name, email]) do
      {:ok, %{rows: [[id, name, email]]}} ->
        {:ok, %{id: id, name: name, email: email}}
                                                 # => Manual map construction
                                                 # => No struct validation

      {:error, %Postgrex.Error{} = error} ->
        {:error, error.postgres.message}         # => Extract error message
    end
  end

  # Read user by ID
  def get(conn, id) do
    sql = "SELECT id, name, email FROM users WHERE id = $1"
    case Postgrex.query(conn, sql, [id]) do
      {:ok, %{rows: [[id, name, email]]}} ->
        {:ok, %{id: id, name: name, email: email}}
                                                 # => Manual map construction

      {:ok, %{rows: []}} ->
        {:error, :not_found}                     # => No rows returned

      {:error, error} ->
        {:error, error}
    end
  end

  # Update user
  def update(conn, id, name, email) do
    sql = "UPDATE users SET name = $2, email = $3 WHERE id = $1 RETURNING id, name, email"
    case Postgrex.query(conn, sql, [id, name, email]) do
      {:ok, %{rows: [[id, name, email]]}} ->
        {:ok, %{id: id, name: name, email: email}}

      {:ok, %{rows: []}} ->
        {:error, :not_found}

      {:error, error} ->
        {:error, error}
    end
  end

  # Delete user
  def delete(conn, id) do
    sql = "DELETE FROM users WHERE id = $1"
    case Postgrex.query(conn, sql, [id]) do
      {:ok, %{num_rows: 1}} ->
        :ok                                      # => Deleted successfully

      {:ok, %{num_rows: 0}} ->
        {:error, :not_found}                     # => No matching row

      {:error, error} ->
        {:error, error}
    end
  end
end
```

**Usage**:

```elixir
{:ok, conn} = Postgrex.start_link(...)

# Create
{:ok, user} = UserRepository.create(conn, "Alice", "alice@example.com")
# => user: %{id: 1, name: "Alice", email: "alice@example.com"}

# Read
{:ok, user} = UserRepository.get(conn, 1)
# => user: %{id: 1, name: "Alice", email: "alice@example.com"}

# Update
{:ok, user} = UserRepository.update(conn, 1, "Alice Smith", "alice.smith@example.com")
# => user: %{id: 1, name: "Alice Smith", email: "alice.smith@example.com"}

# Delete
:ok = UserRepository.delete(conn, 1)
# => Row deleted
```

### Limitations of Raw SQL

This manual approach has serious production issues:

**1. No Query Composition**

```elixir
# Cannot compose queries dynamically
def find_users(conn, filters) do
  # Need to build SQL string manually
  base_sql = "SELECT * FROM users WHERE 1=1"

  {sql, params} = Enum.reduce(filters, {base_sql, []}, fn
    {:name, name}, {sql, params} ->
      {sql <> " AND name = $#{length(params) + 1}", params ++ [name]}
                                                 # => Manual parameter numbering
                                                 # => SQL string concatenation
                                                 # => Error-prone

    {:email, email}, {sql, params} ->
      {sql <> " AND email = $#{length(params) + 1}", params ++ [email]}
  end)

  Postgrex.query(conn, sql, params)
  # => Brittle, hard to maintain
  # => No type safety
end
```

**2. No Changesets or Validation**

```elixir
# No built-in validation
def create(conn, name, email) do
  # Must validate manually
  cond do
    String.length(name) < 3 ->
      {:error, "Name too short"}               # => Manual validation logic

    !String.contains?(email, "@") ->
      {:error, "Invalid email"}                # => String-based checks

    true ->
      # Only then insert
      sql = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
      Postgrex.query(conn, sql, [name, email])
  end
  # => Validation scattered across code
  # => No reusable validation rules
end
```

**3. Manual Migrations**

```elixir
# No migration framework
# Must write SQL files manually:
# migrations/001_create_users.sql
"""
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  email VARCHAR(255) NOT NULL UNIQUE,
  inserted_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);
"""
# => No rollback mechanism
# => No version tracking
# => Manual execution
```

**4. No Relationship Handling**

```elixir
# Must manually join tables
def get_user_with_posts(conn, user_id) do
  sql = """
  SELECT u.id, u.name, u.email, p.id, p.title, p.body
  FROM users u
  LEFT JOIN posts p ON p.user_id = u.id
  WHERE u.id = $1
  """
  # => Manual JOIN writing
  # => Manual result parsing

  {:ok, result} = Postgrex.query(conn, sql, [user_id])

  # Must parse rows manually
  Enum.reduce(result.rows, %{}, fn [user_id, name, email, post_id, title, body], acc ->
    # => Complex nested map construction
    # => Handle NULL values manually
    # => Error-prone
  end)
end
```

**5. No Transaction Support**

```elixir
# Manual transaction handling
def transfer_funds(conn, from_id, to_id, amount) do
  Postgrex.query(conn, "BEGIN", [])              # => Start transaction
  # => No automatic rollback

  case Postgrex.query(conn, "UPDATE accounts SET balance = balance - $1 WHERE id = $2", [amount, from_id]) do
    {:ok, _} ->
      case Postgrex.query(conn, "UPDATE accounts SET balance = balance + $1 WHERE id = $2", [amount, to_id]) do
        {:ok, _} ->
          Postgrex.query(conn, "COMMIT", [])     # => Commit on success
          :ok

        {:error, _} ->
          Postgrex.query(conn, "ROLLBACK", [])   # => Rollback on error
          {:error, :transfer_failed}
      end

    {:error, _} ->
      Postgrex.query(conn, "ROLLBACK", [])
      {:error, :insufficient_balance}
  end
  # => Verbose error handling
  # => Easy to forget rollback
end
```

### Production Disaster Scenarios

**Scenario 1: SQL Injection**

```elixir
# Vulnerable to SQL injection
def find_by_email(conn, email) do
  # WRONG: String interpolation
  sql = "SELECT * FROM users WHERE email = '#{email}'"
                                                 # => SQL injection vulnerability
                                                 # => email: "'; DROP TABLE users; --"
  Postgrex.query(conn, sql, [])
  # => Database destroyed
end

# Must use parameterized queries
def find_by_email(conn, email) do
  sql = "SELECT * FROM users WHERE email = $1"  # => Safe parameterization
  Postgrex.query(conn, sql, [email])
end
```

**Scenario 2: N+1 Query Problem**

```elixir
# Fetching users and posts separately
def get_all_users_with_posts(conn) do
  {:ok, result} = Postgrex.query(conn, "SELECT * FROM users", [])

  Enum.map(result.rows, fn [user_id, name, email] ->
    # N+1: Separate query per user
    {:ok, posts_result} = Postgrex.query(conn, "SELECT * FROM posts WHERE user_id = $1", [user_id])
                                                 # => If 100 users: 1 + 100 queries
                                                 # => Database overload
    %{id: user_id, name: name, email: email, posts: posts_result.rows}
  end)
end
# => Should use JOIN instead
```

**Scenario 3: Failed Transaction Cleanup**

```elixir
# Forgot to rollback on error
def create_order(conn, user_id, items) do
  Postgrex.query(conn, "BEGIN", [])

  {:ok, %{rows: [[order_id]]}} = Postgrex.query(
    conn,
    "INSERT INTO orders (user_id) VALUES ($1) RETURNING id",
    [user_id]
  )

  Enum.each(items, fn item ->
    Postgrex.query(conn, "INSERT INTO order_items (order_id, product_id, quantity) VALUES ($1, $2, $3)", [order_id, item.product_id, item.quantity])
    # => If this fails, transaction left open
    # => No automatic rollback
    # => Database locks held
  end)

  Postgrex.query(conn, "COMMIT", [])
end
```

## Ecto - Production Database Layer

### Setting Up Ecto

Ecto provides schemas, changesets, migrations, and query DSL:

```elixir
# mix.exs dependencies
defp deps do
  [
    {:ecto_sql, "~> 3.10"},                      # => Ecto SQL adapter
    {:postgrex, "~> 0.17"}                       # => PostgreSQL driver
  ]
end

# config/config.exs
config :myapp, MyApp.Repo,
  database: "myapp_dev",
  username: "postgres",
  password: "postgres",
  hostname: "localhost"
# => Centralized configuration

config :myapp, ecto_repos: [MyApp.Repo]          # => List of repositories

# lib/myapp/repo.ex
defmodule MyApp.Repo do
  use Ecto.Repo,
    otp_app: :myapp,                             # => Application name
    adapter: Ecto.Adapters.Postgres              # => Database adapter
end
# => Repository module
# => Provides query interface

# lib/myapp/application.ex
def start(_type, _args) do
  children = [
    MyApp.Repo                                   # => Start Repo as supervised child
  ]

  Supervisor.start_link(children, strategy: :one_for_one)
end
```

### Defining Schemas

Schemas map database tables to Elixir structs:

```elixir
# lib/myapp/accounts/user.ex
defmodule MyApp.Accounts.User do
  use Ecto.Schema                                # => Schema behavior
  import Ecto.Changeset                          # => Changeset functions

  schema "users" do                              # => Table name: "users"
    field :name, :string                         # => Column: name (VARCHAR)
    field :email, :string                        # => Column: email (VARCHAR)

    timestamps()                                 # => inserted_at, updated_at
  end
  # => Defines struct %User{id: ..., name: ..., email: ...}

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email])              # => Allow these fields to change
                                                 # => attrs: %{name: "Alice", email: "..."}
    |> validate_required([:name, :email])        # => Both required
    |> validate_length(:name, min: 3)            # => Name >= 3 characters
    |> validate_format(:email, ~r/@/)            # => Email contains @
    |> unique_constraint(:email)                 # => Email unique in database
  end
  # => Changeset: Data validation and transformation pipeline
end
```

### Basic CRUD with Ecto

Ecto provides clean query API:

```elixir
# Create
changeset = User.changeset(%User{}, %{name: "Alice", email: "alice@example.com"})
# => changeset: Ecto.Changeset struct
# => Contains: changes, errors, validations

case MyApp.Repo.insert(changeset) do
  {:ok, user} ->
    user                                         # => %User{id: 1, name: "Alice", ...}
                                                 # => Type: %User{}

  {:error, changeset} ->
    changeset.errors                             # => [email: {"has already been taken", []}]
                                                 # => Validation errors
end

# Read
user = MyApp.Repo.get(User, 1)                   # => Get by primary key
# => user: %User{id: 1, name: "Alice", ...}
# => Returns: struct or nil

user = MyApp.Repo.get_by(User, email: "alice@example.com")
# => Get by field
# => Returns: struct or nil

# Update
changeset = User.changeset(user, %{name: "Alice Smith"})
{:ok, updated_user} = MyApp.Repo.update(changeset)
# => updated_user: %User{id: 1, name: "Alice Smith", ...}
# => Validations run automatically

# Delete
{:ok, deleted_user} = MyApp.Repo.delete(user)
# => deleted_user: %User{id: 1, ...}
# => Row removed from database
```

### Query DSL

Ecto provides composable query syntax:

```elixir
import Ecto.Query                                # => Query macros

# Simple query
query = from u in User,                          # => u: Binding for User
        where: u.name == "Alice",                # => Filter condition
        select: u                                # => Select entire struct

MyApp.Repo.all(query)                            # => [%User{name: "Alice", ...}]
                                                 # => List of structs

# Composable queries
base_query = from u in User                      # => Base query

query = base_query
        |> where([u], u.name == "Alice")         # => Add WHERE clause
        |> order_by([u], asc: u.name)            # => Add ORDER BY
        |> limit(10)                             # => Add LIMIT

MyApp.Repo.all(query)                            # => Compose dynamically
                                                 # => Type-safe

# Dynamic filtering
def find_users(filters) do
  query = from u in User

  query = if name = filters[:name] do
    where(query, [u], u.name == ^name)           # => ^ pin operator: Interpolate value
  else
    query
  end

  query = if email = filters[:email] do
    where(query, [u], u.email == ^email)
  else
    query
  end

  MyApp.Repo.all(query)
  # => Builds WHERE clause conditionally
  # => Safe interpolation
end
```

### Relationships

Ecto handles associations declaratively:

```elixir
# User has many posts
defmodule MyApp.Content.Post do
  use Ecto.Schema

  schema "posts" do
    field :title, :string
    field :body, :text

    belongs_to :user, MyApp.Accounts.User        # => Foreign key: user_id
                                                 # => Type: integer

    timestamps()
  end
end

# User schema with association
defmodule MyApp.Accounts.User do
  use Ecto.Schema

  schema "users" do
    field :name, :string
    field :email, :string

    has_many :posts, MyApp.Content.Post          # => One-to-many relationship
                                                 # => Accessor: user.posts

    timestamps()
  end
end

# Preload associations (avoid N+1)
user = MyApp.Repo.get(User, 1)
       |> MyApp.Repo.preload(:posts)             # => Load posts in single query
# => user.posts: [%Post{}, %Post{}, ...]
# => One query: SELECT * FROM posts WHERE user_id = 1

# Preload in query
query = from u in User,
        where: u.id == 1,
        preload: [:posts]                        # => JOIN or separate query

user = MyApp.Repo.one(query)
# => user.posts loaded
```

### Transactions with Ecto.Multi

Ecto.Multi provides atomic multi-operation transactions:

```elixir
# Example: Create donation record and update balance
defmodule MyApp.Finance do
  import Ecto.Query
  alias Ecto.Multi
  alias MyApp.Repo
  alias MyApp.Finance.{Account, Donation}

  def record_donation(donor_id, recipient_id, amount) do
    Multi.new()                                  # => Start transaction pipeline
    |> Multi.run(:donor_account, fn repo, _changes ->
      # Fetch donor account
      case repo.get(Account, donor_id) do
        nil -> {:error, :donor_not_found}
        account -> {:ok, account}                # => Pass to next operation
      end
    end)
    |> Multi.run(:check_balance, fn _repo, %{donor_account: account} ->
      # Check sufficient balance
      if account.balance >= amount do
        {:ok, account}
      else
        {:error, :insufficient_balance}          # => Abort transaction
      end
    end)
    |> Multi.update(:deduct_balance, fn %{donor_account: account} ->
      # Deduct from donor
      Account.changeset(account, %{balance: account.balance - amount})
    end)
    |> Multi.run(:recipient_account, fn repo, _changes ->
      # Fetch recipient account
      case repo.get(Account, recipient_id) do
        nil -> {:error, :recipient_not_found}
        account -> {:ok, account}
      end
    end)
    |> Multi.update(:add_balance, fn %{recipient_account: account} ->
      # Add to recipient
      Account.changeset(account, %{balance: account.balance + amount})
    end)
    |> Multi.insert(:donation, fn %{donor_account: donor, recipient_account: recipient} ->
      # Create donation record
      Donation.changeset(%Donation{}, %{
        donor_id: donor.id,
        recipient_id: recipient.id,
        amount: amount
      })
    end)
    |> Repo.transaction()                        # => Execute atomically
    # => Returns: {:ok, %{donor_account: ..., donation: ...}}
    # => Or: {:error, :check_balance, :insufficient_balance, %{donor_account: ...}}
  end
end
```

**Usage**:

```elixir
case Finance.record_donation(donor_id, recipient_id, 1000) do
  {:ok, %{donation: donation}} ->
    # All operations succeeded
    # - Donor balance deducted
    # - Recipient balance increased
    # - Donation record created
    donation                                     # => %Donation{amount: 1000, ...}

  {:error, :check_balance, :insufficient_balance, _changes} ->
    # Transaction rolled back
    # No changes to database
    {:error, "Insufficient funds"}

  {:error, failed_operation, error, _changes} ->
    # Transaction rolled back
    {:error, "Failed at #{failed_operation}: #{inspect(error)}"}
end
```

## Trade-offs: Raw SQL vs Ecto

| Aspect                   | Raw SQL (Postgrex)            | Ecto                          |
| ------------------------ | ----------------------------- | ----------------------------- |
| **Query Composition**    | Manual string concatenation   | Composable query DSL          |
| **Validation**           | Manual checks                 | Changesets with validations   |
| **Type Safety**          | None (maps/tuples)            | Schemas (structs)             |
| **Migrations**           | Manual SQL files              | Mix tasks with rollback       |
| **Relationships**        | Manual JOINs                  | Declarative associations      |
| **Transactions**         | Manual BEGIN/COMMIT/ROLLBACK  | Ecto.Multi (atomic pipelines) |
| **N+1 Prevention**       | Manual optimization           | Preload with single query     |
| **Learning Curve**       | SQL knowledge only            | Ecto DSL + changesets         |
| **Flexibility**          | Maximum (any SQL)             | Limited to Ecto query syntax  |
| **Production Readiness** | Requires extensive validation | Battle-tested abstractions    |
| **Recommended Use**      | Learning, custom queries      | Production applications       |

**Recommendation**: Use Ecto for production applications. Raw SQL appropriate for:

- Learning SQL fundamentals
- Complex custom queries (use `Repo.query` for raw SQL when needed)
- Database-specific features not supported by Ecto

## Best Practices

### 1. Use Changesets for All Data Changes

```elixir
# Bad: Direct struct manipulation
user = %User{name: "Alice", email: "invalid"}
MyApp.Repo.insert(user)                          # => No validation

# Good: Always use changesets
changeset = User.changeset(%User{}, %{name: "Alice", email: "invalid"})
case MyApp.Repo.insert(changeset) do
  {:ok, user} -> user
  {:error, changeset} ->
    # => changeset.errors: [email: {"invalid format", []}]
end
```

### 2. Preload Associations to Avoid N+1

```elixir
# Bad: N+1 queries
users = MyApp.Repo.all(User)
Enum.map(users, fn user ->
  posts = MyApp.Repo.all(from p in Post, where: p.user_id == ^user.id)
  # => Separate query per user
end)

# Good: Preload in single query
users = MyApp.Repo.all(User)
        |> MyApp.Repo.preload(:posts)            # => One additional query
```

### 3. Use Ecto.Multi for Complex Transactions

```elixir
# Good: Atomic multi-step operations
Multi.new()
|> Multi.insert(:user, user_changeset)
|> Multi.insert(:account, fn %{user: user} ->
  Account.changeset(%Account{user_id: user.id}, account_attrs)
end)
|> Multi.run(:send_email, fn _repo, %{user: user} ->
  # Side effect with transaction safety
  Email.send_welcome(user)
end)
|> Repo.transaction()
# => All or nothing
```

### 4. Use Constraints for Database-Level Validation

```elixir
# Migration
create unique_index(:users, [:email])            # => Database constraint

# Changeset
def changeset(user, attrs) do
  user
  |> cast(attrs, [:email])
  |> unique_constraint(:email)                   # => Maps to database constraint
                                                 # => Returns friendly error
end
```

### 5. Use Indexes for Performance

```elixir
# Migration: Add indexes for frequently queried columns
create index(:posts, [:user_id])                 # => Speed up user_id lookups
create index(:posts, [:inserted_at])             # => Speed up date queries
```

### 6. Use Repo.transaction for Multi-Query Operations

```elixir
Repo.transaction(fn ->
  user = Repo.insert!(user_changeset)            # => ! raises on error
  account = Repo.insert!(account_changeset)
  Repo.insert!(profile_changeset)
  # All succeed or all rolled back
  {user, account}
end)
```

## When to Use Ecto

**Use Ecto when**:

- Building web applications with database persistence
- Need schema validation and changesets
- Want composable queries
- Require transaction support
- Working with relationships between entities
- Need migration management

**Consider raw SQL when**:

- Writing complex analytical queries (reporting, aggregations)
- Optimizing critical query paths
- Using database-specific features
- Prototyping or learning SQL

**Use both**: Ecto allows raw SQL via `Repo.query` when needed:

```elixir
# Use Ecto for most operations
users = Repo.all(User)

# Use raw SQL for complex query
{:ok, result} = Repo.query("SELECT * FROM users WHERE tsv @@ plainto_tsquery($1)", ["search term"])
# => Full-text search with PostgreSQL-specific syntax
```

## Next Steps

**Completed**: Ecto patterns for database access

**Continue learning**:

- [Phoenix Framework](/en/learn/software-engineering/programming-languages/elixir/in-the-field/phoenix-framework) - Web framework with Ecto integration
- [REST API Design](/en/learn/software-engineering/programming-languages/elixir/in-the-field/rest-api-design) - RESTful APIs with Ecto resources
- [Testing Strategies](/en/learn/software-engineering/programming-languages/elixir/in-the-field/testing-strategies) - Testing Ecto schemas and queries

**Related patterns**:

- [Application Structure](/en/learn/software-engineering/programming-languages/elixir/in-the-field/application-structure) - Where Ecto fits in application architecture

**Quick reference**:

- [Overview](/en/learn/software-engineering/programming-languages/elixir/in-the-field/overview) - All 36 In-the-Field guides

---

**Summary**: Ecto provides production-ready database access through schemas, changesets, query DSL, and transactions. Start with raw Postgrex to understand SQL challenges, then adopt Ecto for validation, relationships, and atomic operations. Use Ecto.Multi for complex multi-step transactions ensuring data consistency.
