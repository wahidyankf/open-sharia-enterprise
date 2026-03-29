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
                                                 # => conn: Database connection process
                                                 # => name: User name string
                                                 # => email: User email string
    sql = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
                                                 # => $1, $2: Parameterized placeholders
                                                 # => RETURNING: Get inserted row back
                                                 # => Prevents second SELECT query
    case Postgrex.query(conn, sql, [name, email]) do
                                                 # => Execute INSERT with parameters
                                                 # => [name, email]: Bind to $1, $2
                                                 # => Returns {:ok, result} or {:error, error}
      {:ok, %{rows: [[id, name, email]]}} ->
                                                 # => Success: Extract inserted row
                                                 # => rows: List of tuples [[1, "Alice", "alice@..."]]
                                                 # => Pattern match extracts id, name, email
        {:ok, %{id: id, name: name, email: email}}
                                                 # => Manual map construction
                                                 # => No struct validation
                                                 # => Type: {:ok, map()}

      {:error, %Postgrex.Error{} = error} ->
                                                 # => Database error (constraint violation, etc.)
                                                 # => error.postgres: PostgreSQL error details
        {:error, error.postgres.message}         # => Extract human-readable message
                                                 # => Type: {:error, String.t()}
    end
  end

  # Read user by ID
  def get(conn, id) do
                                                 # => conn: Database connection
                                                 # => id: User ID to lookup
    sql = "SELECT id, name, email FROM users WHERE id = $1"
                                                 # => $1: Parameterized ID placeholder
                                                 # => Prevents SQL injection
    case Postgrex.query(conn, sql, [id]) do
                                                 # => Execute SELECT with id parameter
      {:ok, %{rows: [[id, name, email]]}} ->
                                                 # => Success: One row found
                                                 # => Pattern match extracts fields
        {:ok, %{id: id, name: name, email: email}}
                                                 # => Manual map construction
                                                 # => Type: {:ok, map()}

      {:ok, %{rows: []}} ->
                                                 # => Success but no matching row
                                                 # => Empty result set
        {:error, :not_found}                     # => Return not_found error
                                                 # => Type: {:error, :not_found}

      {:error, error} ->
                                                 # => Database error (connection, syntax, etc.)
        {:error, error}                          # => Pass through error
                                                 # => Type: {:error, Postgrex.Error.t()}
    end
  end

  # Update user
  def update(conn, id, name, email) do
                                                 # => conn: Database connection
                                                 # => id: User ID to update
                                                 # => name: New name value
                                                 # => email: New email value
    sql = "UPDATE users SET name = $2, email = $3 WHERE id = $1 RETURNING id, name, email"
                                                 # => $1: id, $2: name, $3: email
                                                 # => RETURNING: Get updated row
    case Postgrex.query(conn, sql, [id, name, email]) do
                                                 # => Execute UPDATE with 3 parameters
                                                 # => Order matters: [id, name, email]
      {:ok, %{rows: [[id, name, email]]}} ->
                                                 # => Success: Row updated
                                                 # => Extract updated fields
        {:ok, %{id: id, name: name, email: email}}
                                                 # => Return updated user map
                                                 # => Type: {:ok, map()}

      {:ok, %{rows: []}} ->
                                                 # => Success but no matching row
                                                 # => ID doesn't exist
        {:error, :not_found}                     # => Return not_found error
                                                 # => Type: {:error, :not_found}

      {:error, error} ->
                                                 # => Database error
        {:error, error}                          # => Pass through error
    end
  end

  # Delete user
  def delete(conn, id) do
                                                 # => conn: Database connection
                                                 # => id: User ID to delete
    sql = "DELETE FROM users WHERE id = $1"
                                                 # => $1: id placeholder
                                                 # => No RETURNING clause needed
    case Postgrex.query(conn, sql, [id]) do
                                                 # => Execute DELETE with id parameter
      {:ok, %{num_rows: 1}} ->
                                                 # => Success: One row deleted
                                                 # => num_rows: Count of affected rows
        :ok                                      # => Simple success atom
                                                 # => Type: :ok

      {:ok, %{num_rows: 0}} ->
                                                 # => Success but no row deleted
                                                 # => ID doesn't exist
        {:error, :not_found}                     # => Return not_found error
                                                 # => Type: {:error, :not_found}

      {:error, error} ->
                                                 # => Database error
        {:error, error}                          # => Pass through error
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
  base_sql = "SELECT * FROM users WHERE 1=1"   # => Base query with always-true condition
                                                 # => Allows appending AND clauses
                                                 # => Type: String.t()

  {sql, params} = Enum.reduce(filters, {base_sql, []}, fn
                                                 # => Iterate over filter list
                                                 # => Accumulator: {sql_string, params_list}
    {:name, name}, {sql, params} ->
      {sql <> " AND name = $#{length(params) + 1}", params ++ [name]}
                                                 # => Append name filter to SQL
                                                 # => Manual parameter numbering ($1, $2, etc.)
                                                 # => SQL string concatenation
                                                 # => Error-prone: Off-by-one errors possible
                                                 # => Returns: {updated_sql, updated_params}

    {:email, email}, {sql, params} ->
      {sql <> " AND email = $#{length(params) + 1}", params ++ [email]}
                                                 # => Append email filter to SQL
                                                 # => Must track parameter position manually
                                                 # => Returns: {updated_sql, updated_params}
  end)                                           # => Final: {complete_sql, all_params}

  Postgrex.query(conn, sql, params)
  # => Execute dynamically built query
  # => Brittle: Parameter numbering fragile
  # => Hard to maintain: String manipulation
  # => No type safety: Params are any()
  # => No SQL injection protection if interpolated
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
                                                 # => Hard-coded length check
                                                 # => No validation pipeline

    !String.contains?(email, "@") ->
      {:error, "Invalid email"}                # => String-based checks
                                                 # => Naive email validation
                                                 # => No format regex

    true ->
      # Only then insert
      sql = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
                                                 # => SQL INSERT statement
                                                 # => $1, $2: Parameterized values
                                                 # => RETURNING: Get inserted row
      Postgrex.query(conn, sql, [name, email])
                                                 # => Execute insert
                                                 # => No pre-validation at database layer
                                                 # => Returns {:ok, result} or {:error, error}
  end
  # => Validation scattered across code
  # => No reusable validation rules
  # => No validation composition
  # => Error messages not standardized
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
                                                 # => Locks acquired on affected rows
  # => No automatic rollback on error
  # => Must manually track transaction state

  case Postgrex.query(conn, "UPDATE accounts SET balance = balance - $1 WHERE id = $2", [amount, from_id]) do
                                                 # => Deduct from source account
                                                 # => $1: amount to transfer
                                                 # => $2: source account ID
    {:ok, _} ->
      case Postgrex.query(conn, "UPDATE accounts SET balance = balance + $1 WHERE id = $2", [amount, to_id]) do
                                                 # => Add to destination account
                                                 # => $1: amount to transfer
                                                 # => $2: destination account ID
        {:ok, _} ->
          Postgrex.query(conn, "COMMIT", [])     # => Commit on success
                                                 # => Both updates applied atomically
          :ok                                    # => Return success

        {:error, _} ->
          Postgrex.query(conn, "ROLLBACK", [])   # => Rollback on error
                                                 # => First update reverted
          {:error, :transfer_failed}             # => Return error

      end

    {:error, _} ->
      Postgrex.query(conn, "ROLLBACK", [])       # => Rollback first operation failure
                                                 # => No changes applied
      {:error, :insufficient_balance}            # => Return insufficient funds error
  end
  # => Verbose error handling
  # => Easy to forget rollback in error paths
  # => Nested case statements hard to maintain
  # => Transaction state tracked manually
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

  def changeset(user, attrs) do                  # => Validation pipeline function
                                                 # => user: %User{} struct or empty struct
                                                 # => attrs: Map of changes to apply
                                                 # => Type: (User.t(), map()) -> Ecto.Changeset.t()
    user                                         # => Start with user struct
                                                 # => Base for changeset operations
    |> cast(attrs, [:name, :email])              # => Cast attrs map to struct fields
                                                 # => Only :name, :email allowed to change
                                                 # => Filters unknown fields
                                                 # => attrs: %{name: "Alice", email: "..."}
                                                 # => Returns: Ecto.Changeset.t()
    |> validate_required([:name, :email])        # => Check both fields present
                                                 # => Adds error if missing
                                                 # => Both required (not null)
                                                 # => Returns: Ecto.Changeset.t()
    |> validate_length(:name, min: 3)            # => Name minimum 3 characters
                                                 # => Adds error if shorter
                                                 # => Name >= 3 characters
                                                 # => Returns: Ecto.Changeset.t()
    |> validate_format(:email, ~r/@/)            # => Email must contain @
                                                 # => Regex pattern match
                                                 # => Simple email validation
                                                 # => Returns: Ecto.Changeset.t()
    |> unique_constraint(:email)                 # => Email unique in database
                                                 # => Maps to database constraint
                                                 # => Checked on insert/update
                                                 # => Returns: Ecto.Changeset.t()
  end                                            # => Final changeset with all validations
  # => Changeset: Data validation and transformation pipeline
  # => Type: Ecto.Changeset.t()
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
user = MyApp.Repo.get(User, 1)                   # => Get by primary key (id = 1)
                                                 # => User: Schema module
                                                 # => 1: Primary key value
# => user: %User{id: 1, name: "Alice", ...}     # => Returns struct if found
# => Returns: struct or nil                     # => nil if not found
                                                 # => Type: User.t() | nil

user = MyApp.Repo.get_by(User, email: "alice@example.com")
                                                 # => Get by any field (not just id)
                                                 # => User: Schema module
                                                 # => email: Query condition
# => Get by field                               # => Filters by email column
# => Returns: struct or nil                     # => First matching record
                                                 # => Type: User.t() | nil

# Update
changeset = User.changeset(user, %{name: "Alice Smith"})
                                                 # => Create changeset with changes
                                                 # => user: Existing user struct
                                                 # => %{name: "Alice Smith"}: Changes to apply
                                                 # => Runs all validations
                                                 # => Type: Ecto.Changeset.t()
{:ok, updated_user} = MyApp.Repo.update(changeset)
                                                 # => Execute UPDATE query
                                                 # => changeset: Validated changes
# => updated_user: %User{id: 1, name: "Alice Smith", ...}
                                                 # => Success: Returns updated struct
# => Validations run automatically             # => Changeset validates before update
                                                 # => Type: {:ok, User.t()} | {:error, Ecto.Changeset.t()}

# Delete
{:ok, deleted_user} = MyApp.Repo.delete(user)    # => Execute DELETE query
                                                 # => user: Struct to delete
                                                 # => Must have primary key set
# => deleted_user: %User{id: 1, ...}            # => Success: Returns deleted struct
# => Row removed from database                  # => Physically removed from table
                                                 # => Type: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
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
  query = from u in User                         # => Base query from User schema
                                                 # => u: Query binding variable

  query = if name = filters[:name] do
                                                 # => Check if name filter provided
                                                 # => name: Filter value if present
    where(query, [u], u.name == ^name)           # => Add WHERE clause to query
                                                 # => ^ pin operator: Interpolate value safely
                                                 # => Returns: Updated query
  else
    query                                        # => No name filter, return unchanged query
  end

  query = if email = filters[:email] do
                                                 # => Check if email filter provided
    where(query, [u], u.email == ^email)         # => Add email WHERE clause
                                                 # => Compose with previous query
                                                 # => Returns: Updated query
  else
    query                                        # => No email filter, return unchanged
  end

  MyApp.Repo.all(query)
  # => Execute composed query
  # => Builds WHERE clause conditionally
  # => Safe interpolation with ^ operator
  # => Type-safe: Ecto validates field types
  # => Returns: [%User{}, ...] or []
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
user = MyApp.Repo.get(User, 1)                   # => Fetch user by id
                                                 # => user: %User{} struct
                                                 # => user.posts: Not loaded (Ecto.Association.NotLoaded)
       |> MyApp.Repo.preload(:posts)             # => Load associated posts
                                                 # => Executes: SELECT * FROM posts WHERE user_id = 1
                                                 # => Avoids N+1 query problem
                                                 # => Load posts in single query
                                                 # => Type: User.t() with posts loaded
# => user.posts: [%Post{}, %Post{}, ...]        # => Loaded posts list
                                                 # => Type: [Post.t()]
# => One query: SELECT * FROM posts WHERE user_id = 1
                                                 # => Efficient: Two total queries (user + posts)
                                                 # => Not N+1: Doesn't query per post

# Preload in query
query = from u in User,                          # => Build query on User schema
                                                 # => u: Query binding variable
        where: u.id == 1,                        # => Filter by user id
                                                 # => WHERE clause condition
        preload: [:posts]                        # => Eager load posts association
                                                 # => JOIN or separate query depending on adapter
                                                 # => Loaded in same Repo call
                                                 # => Type: Ecto.Query.t()

user = MyApp.Repo.one(query)                     # => Execute query, return single result
                                                 # => one: Expects 0 or 1 result
                                                 # => Raises if multiple results
# => user.posts loaded                          # => Posts already available
                                                 # => No additional query needed
                                                 # => Type: User.t() | nil
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
