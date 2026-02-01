---
title: "Advanced"
weight: 11000003
date: 2025-12-25T16:18:56+07:00
draft: false
description: Master advanced Elixir Phoenix patterns through 30 annotated examples covering database optimization, performance, deployment, and resilience
tags:
  - phoenix
  - elixir
  - web-framework
  - tutorial
  - by-example
  - advanced
  - performance
  - deployment
  - resilience
---

## Group 9: Database Advanced

### Example 51: Transactions and Concurrency with Ecto.Multi

Execute multiple database operations atomically. If any fails, all rollback.

```mermaid
%% Ecto.Multi transaction flow
graph TD
    A[Multi.new] --> B[Multi.update :debit]
    B --> C[Multi.update :credit]
    C --> D[Multi.insert :log]
    D --> E[Repo.transaction]
    E --> F{All succeed?}
    F -->|Yes| G[Commit all changes]
    F -->|No| H[Rollback everything]
    G --> I[Return {:ok, results}]
    H --> J[Return {:error, failed_op}]

    style A fill:#0173B2,color:#fff
    style E fill:#DE8F05,color:#000
    style G fill:#029E73,color:#fff
    style H fill:#CA9161,color:#000
```

```elixir
defmodule MyApp.Transfers do                          # => Defines Transfers service module
                                                       # => Groups money transfer operations
  def transfer_funds(from_account, to_account, amount) do  # => Public function: transfer_funds/3
                                                             # => Params: from_account, to_account, amount
    result =                                          # => Binds result to Multi pipeline return
                                                       # => Will be {:ok, map} or {:error, ...} tuple
      Ecto.Multi.new()                                # => Creates empty Multi struct
                                                       # => Multi accumulates operations to run in transaction
      |> Ecto.Multi.update(                           # => Pipes Multi into update operation
        :debit,                                       # => Named operation key :debit
                                                       # => Can reference this result later as %{debit: ...}
        Ecto.Changeset.change(from_account, balance: from_account.balance - amount)
                                                       # => Creates changeset subtracting amount from balance
                                                       # => E.g., balance: 1000 - 50 = 950
      )                                               # => Returns Multi with :debit operation added
                                                       # => Multi now has 1 operation queued
      |> Ecto.Multi.update(                           # => Adds second update operation
        :credit,                                      # => Named operation key :credit
                                                       # => Independent of :debit (parallel semantics)
        Ecto.Changeset.change(to_account, balance: to_account.balance + amount)
                                                       # => Creates changeset adding amount to balance
                                                       # => E.g., balance: 500 + 50 = 550
      )                                               # => Returns Multi with :debit and :credit queued
                                                       # => Multi now has 2 operations
      |> Ecto.Multi.insert(:transaction_log, %TransactionLog{
                                                       # => Adds insert operation for audit log
                                                       # => Named :transaction_log
        from_id: from_account.id,                     # => Sets from_id field to source account ID
                                                       # => E.g., from_id: 123
        to_id: to_account.id,                         # => Sets to_id field to destination account ID
                                                       # => E.g., to_id: 456
        amount: amount                                # => Sets amount field to transferred amount
                                                       # => E.g., amount: 50
      })                                              # => Returns Multi with 3 operations total
                                                       # => All operations ready to execute atomically
      |> MyApp.Repo.transaction()                     # => Executes all operations in database transaction
                                                       # => Returns {:ok, %{debit: ..., credit: ..., transaction_log: ...}} on success
                                                       # => Returns {:error, failed_operation, changeset, changes_so_far} on failure
                                                       # => Database commits all or rolls back all

    case result do                                    # => Pattern match on transaction result
                                                       # => Handles success and failure cases
      {:ok, %{debit: from, credit: to, transaction_log: log}} ->  # => Success pattern match
                                                                    # => Destructures map with all 3 results
                                                                    # => from is updated from_account struct
                                                                    # => to is updated to_account struct
                                                                    # => log is inserted TransactionLog struct
        {:ok, from, to, log}                          # => Returns success tuple with all results
                                                       # => Caller receives all updated structs
                                                       # => Database changes are committed permanently

      {:error, :debit, changeset, _changes} ->        # => Failure pattern: :debit operation failed
                                                       # => changeset contains validation errors
                                                       # => _changes is empty map (no operations succeeded yet)
        {:error, "Debit failed", changeset}           # => Returns error tuple with message
                                                       # => Transaction rolled back before :credit ran
                                                       # => Database unchanged (atomicity guaranteed)

      {:error, failed_op, changeset, _changes} ->     # => Catch-all failure pattern for any operation
                                                       # => failed_op is operation name (:credit or :transaction_log)
                                                       # => changeset has error details
        {:error, "Operation failed: #{failed_op}", changeset}  # => Returns error with operation name
                                                                 # => E.g., "Operation failed: credit"
                                                                 # => All changes discarded, database rolled back
    end                                               # => End case statement
  end                                                 # => End transfer_funds/3 function

  # Dependency between operations - sequential execution with references
  def complex_transaction do                          # => Public function: complex_transaction/0
                                                       # => No parameters, demonstrates dependent operations
    Ecto.Multi.new()                                  # => Creates empty Multi struct
                                                       # => Starting point for dependent operation chain
    |> Ecto.Multi.insert(:user, User.create_changeset(%{email: "user@example.com"}))
                                                       # => First operation: insert user
                                                       # => Named :user (required for later references)
                                                       # => Creates user with email "user@example.com"
                                                       # => Returns Multi with :user operation queued
    |> Ecto.Multi.insert(:profile, fn %{user: user} ->  # => Second operation: insert profile (DEPENDENT)
                                                         # => Anonymous function receives %{user: user} from previous operations
                                                         # => user is the inserted User struct from :user operation
                                                         # => Sequential dependency: profile needs user.id
      Profile.create_changeset(user)                  # => Creates profile changeset for inserted user
                                                       # => Uses user struct to set foreign key
                                                       # => E.g., profile.user_id = user.id
    end)                                              # => Returns Multi with :user and :profile queued
                                                       # => Multi now has 2 operations, :profile depends on :user
    |> Ecto.Multi.insert(:settings, fn %{user: user} ->  # => Third operation: insert settings (DEPENDENT)
                                                          # => Also receives %{user: user} from accumulated results
                                                          # => user is same inserted User struct
      Settings.default_changeset(user)                # => Creates default settings for user
                                                       # => Uses user struct to set foreign key
                                                       # => E.g., settings.user_id = user.id
    end)                                              # => Returns Multi with all 3 operations queued
                                                       # => Execution order: :user, then :profile, then :settings
    |> MyApp.Repo.transaction()                       # => Executes operations in order within transaction
                                                       # => If :user fails, :profile and :settings never run
                                                       # => If :profile fails, :user is rolled back, :settings never runs
                                                       # => Returns {:ok, %{user: ..., profile: ..., settings: ...}} on success
                                                       # => Returns {:error, operation_name, changeset, partial_results} on failure
  end                                                 # => End complex_transaction/0 function
end                                                   # => End MyApp.Transfers module
```

**Key Takeaway**: Ecto.Multi ensures all-or-nothing execution. Operations reference previous results with fn. Rollback happens automatically on any failure. Perfect for transfers, account creation, multi-step operations.

**Why It Matters**: Ecto provides type-safe database interactions with compile-time query validation. This prevents SQL injection and catches query errors before deployment.

### Example 52: Database Constraints and Error Handling

Handle database constraint violations (unique, foreign key, etc.) gracefully in changesets.

```mermaid
%% Constraint violation handling
graph TD
    A[Insert user] --> B[Database]
    B --> C{Constraint violated?}
    C -->|UNIQUE email| D[unique_constraint catches]
    C -->|FK organization_id| E[assoc_constraint catches]
    C -->|No violation| F[Insert succeeds]
    D --> G[Return changeset with error]
    E --> G
    F --> H[Return {:ok, user}]

    style A fill:#0173B2,color:#fff
    style C fill:#DE8F05,color:#000
    style F fill:#029E73,color:#fff
    style G fill:#CA9161,color:#000
```

```elixir
defmodule MyApp.Accounts.User do                      # => Defines User schema module
                                                       # => Maps Elixir struct to "users" database table
  schema "users" do                                   # => Defines schema for "users" table
                                                       # => Auto-generates struct with id, inserted_at, updated_at
    field :email, :string                             # => Declares email field, type :string (VARCHAR in DB)
                                                       # => Will be in User struct as user.email
    field :username, :string                          # => Declares username field, type :string
                                                       # => Will be in User struct as user.username
  end                                                 # => End schema definition

  def registration_changeset(user, attrs) do          # => Public function: registration_changeset/2
                                                       # => Params: user struct, attrs map
                                                       # => Returns changeset for user registration
    user                                              # => Starts with User struct (empty or existing)
                                                       # => E.g., %User{email: nil, username: nil}
    |> cast(attrs, [:email, :username])               # => Casts attrs map to changeset with allowed fields
                                                       # => Only :email and :username are whitelisted
                                                       # => E.g., attrs = %{email: "alice@example.com", username: "alice"}
                                                       # => Returns changeset with changes: %{email: "alice@example.com", username: "alice"}
    |> unique_constraint(:email)                      # => Adds constraint check for unique email
                                                       # => Does NOT query database yet (lazy)
                                                       # => If Repo.insert finds duplicate email, catches constraint error
                                                       # => Converts PostgreSQL UNIQUE violation to changeset error
                                                       # => Error: {:email, {"has already been taken", [constraint: :unique]}}
    |> unique_constraint(:username)                   # => Adds constraint check for unique username
                                                       # => If Repo.insert finds duplicate username, catches error
                                                       # => Prevents "ERROR: duplicate key value violates unique constraint"
                                                       # => Converts to user-friendly changeset error
    |> assoc_constraint(:organization)                # => Validates foreign key :organization_id exists in organizations table
                                                       # => If organization_id points to non-existent record, catches FK violation
                                                       # => Converts "violates foreign key constraint" to changeset error
                                                       # => Error: {:organization, {"does not exist", [constraint: :assoc]}}
  end                                                 # => Returns changeset with all constraints registered
                                                       # => Constraints only validated when Repo.insert/update called
end                                                   # => End MyApp.Accounts.User module

# In your service
defmodule MyApp.Accounts do                           # => Defines Accounts context module
                                                       # => Groups user registration and management operations
  def register_user(attrs) do                         # => Public function: register_user/1
                                                       # => Param: attrs map with email, username
                                                       # => Returns {:ok, user} or {:error, message}
    case %User{}                                      # => Creates empty User struct
                                                       # => %User{id: nil, email: nil, username: nil}
         |> User.registration_changeset(attrs)        # => Builds changeset with validations
                                                       # => Adds constraints for email, username, organization
                                                       # => Returns changeset ready for insert
         |> Repo.insert() do                          # => Attempts database INSERT
                                                       # => If successful: returns {:ok, %User{id: 123, email: "...", ...}}
                                                       # => If constraint violated: returns {:error, %Changeset{errors: [...]}}
                                                       # => Constraint checks happen at database level
      {:ok, user} ->                                  # => Pattern match on success case
                                                       # => user is persisted User struct with database ID
                                                       # => E.g., %User{id: 123, email: "alice@example.com", username: "alice"}
        {:ok, user}                                   # => Returns success tuple with user struct
                                                       # => Caller receives complete user record

      {:error, %Changeset{} = changeset} ->           # => Pattern match on failure case
                                                       # => changeset contains validation/constraint errors
                                                       # => E.g., changeset.errors = [{:email, {"has already been taken", ...}}]
        # Check for constraint violations
        error_fields = Enum.map(changeset.errors, fn {field, {msg, _}} -> {field, msg} end)
                                                       # => Maps errors to {field, message} tuples
                                                       # => E.g., [{:email, "has already been taken"}, {:username, "..."}]
                                                       # => Extracts field name and error message, discards metadata

        if Enum.any?(error_fields, fn {field, _} -> field == :email end) do
                                                       # => Checks if :email field has error
                                                       # => Returns true if duplicate email constraint violated
                                                       # => Returns false if email is valid
          {:error, "Email already registered"}        # => Returns specific error for duplicate email
                                                       # => User-friendly message for UI display
        else                                          # => All other errors (username duplicate, FK violation, etc.)
          {:error, "Registration failed"}             # => Returns generic error message
                                                       # => Could inspect changeset.errors for specific details
        end                                           # => End if statement
    end                                               # => End case statement
  end                                                 # => End register_user/1 function
end                                                   # => End MyApp.Accounts module
```

**Key Takeaway**: unique_constraint/2 catches database uniqueness violations. assoc_constraint/2 catches foreign key errors. Changesets provide user-friendly error messages without SQL errors exposed.

**Why It Matters**: Changesets centralize validation logic and provide user-friendly error messages. This pattern ensures data integrity and improves user experience with clear feedback.

### Example 53: Polymorphic Associations with many_to_many :through

Model flexible relationships where the same entity can have many different types of related entities.

```mermaid
%% Many-to-many through join table
erDiagram
    POST ||--o{ POST_TAG : has
    TAG ||--o{ POST_TAG : has
    COMMENT ||--o{ COMMENT_TAG : has
    TAG ||--o{ COMMENT_TAG : has

    POST {
        int id PK
        string title
    }
    TAG {
        int id PK
        string name
    }
    POST_TAG {
        int post_id FK
        int tag_id FK
    }
    COMMENT {
        int id PK
        string body
    }
    COMMENT_TAG {
        int comment_id FK
        int tag_id FK
    }
```

```elixir
defmodule MyApp.Content.Post do                       # => Defines Post schema module
                                                       # => Maps to "posts" database table
  schema "posts" do                                   # => Defines schema for "posts" table
                                                       # => Auto-generates id, inserted_at, updated_at
    field :title, :string                             # => Declares title field, type :string
                                                       # => Will be accessible as post.title
    many_to_many(:tags, MyApp.Tagging.Tag, join_through: "post_tags")
                                                       # => Declares many-to-many relationship with Tag
                                                       # => join_through: "post_tags" - uses post_tags join table
                                                       # => Post can have many Tags, Tag can have many Posts
                                                       # => Accessible as post.tags (list of Tag structs)
    many_to_many(:attachments, MyApp.Attachments.Attachment, join_through: "post_attachments")
                                                       # => Second many-to-many relationship with Attachment
                                                       # => Uses post_attachments join table
                                                       # => Demonstrates polymorphic-like pattern (Post has Tags AND Attachments)
  end                                                 # => End schema definition
end                                                   # => End MyApp.Content.Post module

defmodule MyApp.Content.Comment do                    # => Defines Comment schema module
                                                       # => Maps to "comments" database table
  schema "comments" do                                # => Defines schema for "comments" table
                                                       # => Auto-generates id, inserted_at, updated_at
    field :body, :string                              # => Declares body field for comment text
                                                       # => Will be accessible as comment.body
    many_to_many(:tags, MyApp.Tagging.Tag, join_through: "comment_tags")
                                                       # => Declares many-to-many relationship with SAME Tag module
                                                       # => Uses comment_tags join table (different from post_tags)
                                                       # => Comment can have many Tags, Tag can belong to Posts AND Comments
                                                       # => Polymorphic pattern: Tag shared across multiple entities
  end                                                 # => End schema definition
end                                                   # => End MyApp.Content.Comment module

# Migration for join table
def change do                                         # => Migration function to create join table
                                                       # => Runs when executing mix ecto.migrate
  create table(:post_tags) do                         # => Creates post_tags join table
                                                       # => Auto-generates id, inserted_at, updated_at
    add :post_id, references(:posts, on_delete: :delete_all)
                                                       # => Adds post_id foreign key column
                                                       # => references(:posts) - points to posts.id
                                                       # => on_delete: :delete_all - when post deleted, delete all post_tags rows
                                                       # => Ensures referential integrity (CASCADE DELETE)
    add :tag_id, references(:tags, on_delete: :delete_all)
                                                       # => Adds tag_id foreign key column
                                                       # => references(:tags) - points to tags.id
                                                       # => on_delete: :delete_all - when tag deleted, delete all post_tags rows
                                                       # => Prevents orphaned join records
    timestamps()                                      # => Adds inserted_at and updated_at timestamp columns
                                                       # => Tracks when join record was created/updated
  end                                                 # => End table creation

  create unique_index(:post_tags, [:post_id, :tag_id])
                                                       # => Creates unique composite index on (post_id, tag_id)
                                                       # => Prevents duplicate Post-Tag associations
                                                       # => E.g., can't link same Post to same Tag twice
                                                       # => Also improves query performance for lookups
end                                                   # => End migration function

# Usage
post = Post                                           # => Starts with Post module alias
  |> Repo.preload(:tags)                              # => Loads associated tags from database
                                                       # => Executes JOIN query: SELECT tags.* FROM tags JOIN post_tags ON...
                                                       # => Returns post with post.tags populated (list of Tag structs)
                                                       # => E.g., post.tags = [%Tag{id: 1, name: "elixir"}, %Tag{id: 2, name: "web"}]
  |> Ecto.Changeset.change()                          # => Creates changeset from preloaded post
                                                       # => Prepares post struct for updating associations
                                                       # => Returns changeset with no changes yet
  |> put_assoc(:tags, tags)                           # => Replaces post's tags with new tags list
                                                       # => tags variable contains list of Tag structs
                                                       # => Ecto will DELETE old post_tags rows and INSERT new ones
                                                       # => E.g., tags = [%Tag{id: 3}, %Tag{id: 4}]
  |> Repo.update()                                    # => Persists association changes to database
                                                       # => Executes: DELETE FROM post_tags WHERE post_id = X
                                                       # =>            INSERT INTO post_tags (post_id, tag_id) VALUES (X, 3), (X, 4)
                                                       # => Returns {:ok, updated_post} with new associations

# Query posts with specific tag
posts = from p in Post,                               # => Starts Ecto query for Post table
                                                       # => p is alias for posts table
  join: t in assoc(p, :tags),                         # => INNER JOIN with tags through post_tags join table
                                                       # => assoc(p, :tags) generates: JOIN post_tags ON post_tags.post_id = p.id
                                                       # =>                            JOIN tags ON tags.id = post_tags.tag_id
                                                       # => t is alias for tags table
  where: t.slug == "featured"                         # => Filters for tag with slug = "featured"
                                                       # => WHERE tags.slug = 'featured'
                                                       # => Returns only posts that have the "featured" tag
                                                       # => Result: list of Post structs [%Post{...}, ...]
```

**Key Takeaway**: many_to_many/3 with join_through creates flexible relationships. Use put_assoc/3 to update related records. Query across relationships with join.

**Why It Matters**: Query composition enables complex database operations. Understanding Ecto queries is essential for application performance.

### Example 54: Multi-Tenancy with Ecto Query Prefix

Isolate tenant data at the query level. Each query automatically scopes to tenant.

```elixir
defmodule MyApp.Accounts do                           # => Defines Accounts context for multi-tenancy
                                                       # => Groups tenant-scoped user operations
  def get_user(user_id, tenant_id) do                 # => Public function: get_user/2
                                                       # => Params: user_id, tenant_id for isolation
    from(u in User, where: u.id == ^user_id and u.tenant_id == ^tenant_id)
                                                       # => Builds query filtering by BOTH user_id AND tenant_id
                                                       # => Prevents cross-tenant data leakage
                                                       # => WHERE users.id = ? AND users.tenant_id = ?
    |> Repo.one()                                     # => Executes query, returns single user or nil
                                                       # => Result: %User{} if found, nil if not found or wrong tenant
  end                                                 # => End get_user/2 function

  def create_user(attrs, tenant_id) do                # => Public function: create_user/2
                                                       # => Params: attrs map, tenant_id for isolation
    %User{}                                           # => Creates empty User struct
                                                       # => %User{id: nil, tenant_id: nil, ...}
    |> User.changeset(attrs |> Map.put("tenant_id", tenant_id))
                                                       # => Adds tenant_id to attrs before validation
                                                       # => E.g., %{"name" => "Alice", "tenant_id" => 123}
                                                       # => Ensures user always belongs to correct tenant
    |> Repo.insert()                                  # => Inserts user into database
                                                       # => Returns {:ok, %User{}} or {:error, changeset}
  end                                                 # => End create_user/2 function
end                                                   # => End MyApp.Accounts module

# Or use query prefix for schema-per-tenant
defmodule MyApp.Repo do                               # => Alternative approach: schema-level isolation
                                                       # => Each tenant has separate PostgreSQL schema
  def for_tenant(tenant_id) do                        # => Public function: for_tenant/1
                                                       # => Switches Repo to use tenant-specific schema
    # All queries run with prefix filter
    Repo.put_dynamic_repo({__MODULE__, {tenant_id}})  # => Sets dynamic repo prefix for current process
                                                       # => All subsequent queries in this process use "tenant_#{tenant_id}" schema
                                                       # => E.g., tenant_id=123 → queries use "tenant_123" schema
  end                                                 # => End for_tenant/1 function
end                                                   # => End MyApp.Repo module

# Query scoped to tenant
User                                                  # => Starts with User schema
|> where([u], u.tenant_id == ^tenant_id)              # => Filters by tenant_id column
                                                       # => WHERE users.tenant_id = ?
                                                       # => Ensures only this tenant's users returned
|> Repo.all()                                         # => Executes query, returns list of users
                                                       # => Result: [%User{tenant_id: 123}, %User{tenant_id: 123}]
                                                       # => Never returns users from other tenants

# Or with dynamic query prefix
User                                                  # => Starts with User schema
|> Repo.all(prefix: "tenant_#{tenant_id}")            # => Queries from tenant-specific schema
                                                       # => E.g., SELECT * FROM tenant_123.users
                                                       # => Complete schema isolation (tenant_123, tenant_456, etc.)
                                                       # => Each tenant has independent tables
```

**Key Takeaway**: Always filter by tenant_id in queries. Use scopes (functions that return queries) to prevent tenant leaks. Consider separate schemas per tenant for complete isolation.

**Why It Matters**: Schemas define data structure and types for validation and persistence. This provides a single source of truth for your domain models.

### Example 55: PostgreSQL Advanced Features in Ecto

Leverage PostgreSQL-specific features: JSONB, arrays, full-text search, custom types.

```elixir
defmodule MyApp.Blog.Post do                          # => Defines Post schema with PostgreSQL features
                                                       # => Demonstrates JSONB, arrays, full-text search
  schema "posts" do                                   # => Maps to "posts" table
                                                       # => Auto-generates id, inserted_at, updated_at
    field :title, :string                             # => Standard string field (VARCHAR)
                                                       # => Will be accessible as post.title
    field :metadata, :map              # => JSONB in PostgreSQL
                                                       # => Stores arbitrary JSON data efficiently
                                                       # => E.g., %{"status" => "draft", "tags" => ["elixir"]}
    field :tags, {:array, :string}     # => Array type
                                                       # => PostgreSQL native array: text[]
                                                       # => E.g., ["elixir", "phoenix", "web"]
    field :search_vector, :string      # => Full-text search
                                                       # => Actually stored as tsvector type
                                                       # => Preprocessed text for efficient searching
  end                                                 # => End schema definition
end                                                   # => End MyApp.Blog.Post module

# Migration
def change do                                         # => Migration function to create posts table
                                                       # => Runs when executing mix ecto.migrate
  create table(:posts) do                             # => Creates posts table
                                                       # => Auto-adds id primary key
    add :title, :string                               # => Adds title column (VARCHAR)
                                                       # => Standard text field
    add :metadata, :jsonb, default: "{}"              # => Adds metadata column (JSONB type)
                                                       # => Default: empty JSON object {}
                                                       # => Stores structured data without predefined schema
    add :tags, {:array, :string}, default: []         # => Adds tags column (text[] array)
                                                       # => Default: empty array []
                                                       # => Native PostgreSQL array, not JSON
    add :search_vector, :tsvector                     # => Adds search_vector column (tsvector type)
                                                       # => Optimized for full-text search
                                                       # => Stores preprocessed, indexed text

    timestamps()                                      # => Adds inserted_at, updated_at columns
                                                       # => Auto-managed by Ecto
  end                                                 # => End table creation

  # GIN index for JSONB performance
  create index(:posts, ["(metadata)"], using: :gin)   # => Creates GIN index on metadata column
                                                       # => GIN (Generalized Inverted Index) for JSONB
                                                       # => Enables fast queries on JSON keys/values
                                                       # => E.g., WHERE metadata->>'status' = 'published'
  # GIN index for full-text search
  create index(:posts, [:search_vector], using: :gin) # => Creates GIN index on search_vector
                                                       # => Enables fast full-text search
                                                       # => Required for efficient @@ operator queries
end                                                   # => End migration function

# Query JSONB
posts = from p in Post,                               # => Starts Ecto query for Post
                                                       # => p is alias for posts table
  where: fragment("? ->> ? = ?", p.metadata, "status", "published")
                                                       # => JSONB query: metadata->>'status' = 'published'
                                                       # => ->> extracts JSON field as text
                                                       # => Finds posts where metadata has status="published"
                                                       # => Result: posts with matching JSON property

# Array operations
posts = from p in Post,                               # => Query for posts with specific tag
                                                       # => Uses PostgreSQL array operators
  where: fragment("? @> ?", p.tags, ^["elixir"])      # => Array contains operator @>
                                                       # => Checks if tags array contains ["elixir"]
                                                       # => E.g., ["elixir", "phoenix"] @> ["elixir"] = true
                                                       # => Result: posts tagged with "elixir"

# Full-text search
results = from p in Post,                             # => Full-text search query
                                                       # => Uses PostgreSQL tsvector and tsquery
  where: fragment("to_tsvector('english', ?) @@ plainto_tsquery('english', ?)",
    p.title, ^search_term),                           # => to_tsvector converts title to searchable form
                                                       # => plainto_tsquery converts search term to query
                                                       # => @@ matches tsvector against tsquery
                                                       # => E.g., search_term="phoenix framework" finds titles with those words
  select: p                                           # => Returns matching Post structs
                                                       # => Result: posts matching search term
```

**Key Takeaway**: Use :map for JSONB, {:array, :string} for arrays. Full-text search with tsvector. Use fragment/2 for database-specific SQL. Index JSONB and tsvector for performance.

**Why It Matters**: This Phoenix pattern is fundamental for building production web applications. Understanding this concept enables you to create robust, maintainable, and scalable applications.

## Group 10: Performance

### Example 56: Query Optimization - N+1 Prevention

Load related data efficiently to avoid N+1 queries where one query results in N additional queries.

```mermaid
%% N+1 problem vs preload solution
graph TD
    A[❌ N+1 Problem] --> B[Query 1: SELECT * FROM posts]
    B --> C[Query 2: SELECT * FROM authors WHERE id = 1]
    B --> D[Query 3: SELECT * FROM authors WHERE id = 2]
    B --> E[Query 4: SELECT * FROM authors WHERE id = 3]
    B --> F[... N queries]

    G[✅ Preload Solution] --> H[Query 1: SELECT * FROM posts]
    H --> I[Query 2: SELECT * FROM authors WHERE id IN]
    I --> J[Total: 2 queries]

    style A fill:#CA9161,color:#000
    style G fill:#029E73,color:#fff
    style J fill:#029E73,color:#fff
```

```elixir
# ❌ N+1 Problem - 1 query + N queries
posts = Post |> Repo.all()                            # => SELECT * FROM posts (1 query)
for post <- posts do
  author = Author |> where([a], a.id == ^post.author_id) |> Repo.one()
  # => SELECT * FROM authors WHERE id = ? (N queries!)
end
# Total: 1 + N queries (if 100 posts = 101 queries!)

# ✅ Solution 1: Preload
posts = Post
  |> preload(:author)                                 # => Eager load authors
  |> Repo.all()
# => 2 queries total: SELECT posts, SELECT authors WHERE id IN (...)

# ✅ Solution 2: Join (for aggregations)
posts = from p in Post,
  join: a in assoc(p, :author),                       # => SQL JOIN
  select: {p, a}
# => 1 query: SELECT posts.*, authors.* FROM posts JOIN authors

# ✅ Solution 3: Preload with nested associations
posts = Post
  |> preload([comments: :author])  # => Loads comments and their authors
  |> Repo.all()

# Query with EXPLAIN to see execution plan
results = Repo.all(from p in Post, preload: :author)
IO.inspect(Repo.explain(:all, Post))
```

**Key Takeaway**: Use preload/1 to eager-load associations. Use join for aggregations and filtering. Always check your queries with EXPLAIN. Avoid fetching in loops.

**Why It Matters**: Associations model relationships between data entities. Understanding Ecto associations enables efficient data access patterns.

### Example 57: Caching Strategies

Cache expensive operations to reduce database load and improve response time.

```elixir
defmodule MyApp.CacheServer do                        # => Defines in-memory cache GenServer
                                                       # => Stores key-value pairs with TTL
  use GenServer                                       # => Imports GenServer behavior
                                                       # => Provides start_link, init, handle_call, etc.

  def start_link(_opts) do                            # => Public function: start_link/1
                                                       # => Called by supervisor to start cache process
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
                                                       # => Starts GenServer with empty map as initial state
                                                       # => Registers process with module name
                                                       # => Returns {:ok, pid}
  end                                                 # => End start_link/1 function

  def get_cache(key) do                               # => Public API: get_cache/1
                                                       # => Synchronous retrieval of cached value
    GenServer.call(__MODULE__, {:get, key})           # => Sends synchronous message to cache process
                                                       # => Blocks caller until response received
                                                       # => Returns {:ok, value} or :not_found
  end                                                 # => End get_cache/1 function

  def set_cache(key, value, ttl_ms) do                # => Public API: set_cache/3
                                                       # => Asynchronous cache write with TTL
    GenServer.cast(__MODULE__, {:set, key, value, ttl_ms})
                                                       # => Sends async message (fire-and-forget)
                                                       # => Returns :ok immediately without blocking
                                                       # => Value stored with expiration time
  end                                                 # => End set_cache/3 function

  @impl true                                          # => Implements GenServer callback
  def init(state) do                                  # => Initializes GenServer state
                                                       # => Called when process starts
    {:ok, state}                                      # => Returns {:ok, initial_state}
                                                       # => State is empty map %{}
  end                                                 # => End init/1 callback

  @impl true                                          # => Implements GenServer callback
  def handle_call({:get, key}, _from, state) do       # => Handles synchronous get requests
                                                       # => Pattern matches {:get, key} message
    case Map.get(state, key) do                       # => Retrieves value from state map
                                                       # => Result: {value, expires_at} or nil
      {value, expires_at} when expires_at > System.monotonic_time(:millisecond) ->
                                                       # => Pattern match: value exists AND not expired
                                                       # => Compares expiration time with current time
        {:reply, {:ok, value}, state}                 # => Cache hit, not expired
                                                       # => Returns value to caller, keeps state unchanged

      _ ->                                            # => Catch-all: cache miss or expired
                                                       # => Either key doesn't exist OR past expiration
        {:reply, :not_found, state}                   # => Cache miss or expired
                                                       # => Returns :not_found to caller
    end                                               # => End case statement
  end  # => Synchronous call, blocks caller until reply

  @impl true                                          # => Implements GenServer callback
  def handle_cast({:set, key, value, ttl_ms}, state) do
                                                       # => Handles asynchronous set requests
                                                       # => Pattern matches {:set, key, value, ttl_ms} message
    expires_at = System.monotonic_time(:millisecond) + ttl_ms  # => Calculate expiry
                                                       # => Current monotonic time + TTL in milliseconds
                                                       # => E.g., current=1000, ttl=5000 → expires_at=6000
    {:noreply, Map.put(state, key, {value, expires_at})}
                                                       # => Updates state map with new cache entry
                                                       # => Stores tuple {value, expires_at} as value
                                                       # => Returns {:noreply, new_state}
  end  # => Asynchronous, returns immediately without blocking
end                                                   # => End MyApp.CacheServer module

# Or use Cachex library
defmodule MyApp.Blog do                               # => Example using Cachex library
                                                       # => Production-ready caching solution
  def get_popular_posts do                            # => Public function: get_popular_posts/0
                                                       # => Caches expensive database query
    case Cachex.get(:blog_cache, "popular_posts") do  # => Checks cache first
                                                       # => :blog_cache is cache name (configured in app)
                                                       # => "popular_posts" is cache key
      {:ok, nil} ->                                   # => Cache miss: no cached value
                                                       # => Need to fetch from database
        posts = Post |> where([p], p.likes > 100) |> Repo.all()
                                                       # => Expensive database query
                                                       # => SELECT * FROM posts WHERE likes > 100
                                                       # => Returns list of popular posts
        Cachex.put(:blog_cache, "popular_posts", posts, ttl: :timer.minutes(60))
                                                       # => Stores result in cache with 60-minute TTL
                                                       # => Future requests served from cache
                                                       # => TTL: cache expires after 1 hour
        posts                                         # => Returns posts to caller
                                                       # => First request: slow (database query)

      {:ok, posts} ->                                 # => Cache hit: value exists in cache
                                                       # => posts already fetched from cache
        posts                                         # => Returns cached posts immediately
                                                       # => Subsequent requests: fast (no database query)
    end                                               # => End case statement
  end                                                 # => End get_popular_posts/0 function
end                                                   # => End MyApp.Blog module
```

**Key Takeaway**: Cache expensive queries with TTL (time-to-live). Invalidate cache when data changes. Use Cachex for distributed caching. Cache at controller or service layer.

**Why It Matters**: Controllers implement the request-response pattern that forms the backbone of web applications. Understanding Phoenix controllers enables proper separation of concerns and clean HTTP interface design.

### Example 58: Background Jobs with Oban

Execute long-running tasks asynchronously. Schedule recurring jobs.

```mermaid
%% Oban job processing flow
graph TD
    A[User Registration] --> B[Insert Oban Job]
    B --> C[oban_jobs table]
    C --> D[Oban Worker Pool]
    D --> E[EmailWorker.perform]
    E --> F{Success?}
    F -->|Yes| G[Mark completed]
    F -->|No| H[Retry with backoff]
    H --> D

    style A fill:#0173B2,color:#fff
    style C fill:#DE8F05,color:#000
    style G fill:#029E73,color:#fff
    style H fill:#CC78BC,color:#000
```

```elixir
# lib/my_app/workers/email_worker.ex
defmodule MyApp.Workers.EmailWorker do
  use Oban.Worker, queue: :default

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"user_id" => user_id}}) do
    user = MyApp.Accounts.get_user!(user_id)          # => Load user
    MyApp.Mailer.send_welcome_email(user)             # => Send email
    :ok                                               # => Mark job complete
  end  # => Return :ok for success, {:error, reason} to retry
end

# In your controller/action
defmodule MyAppWeb.UserController do
  def create(conn, %{"user" => user_params}) do
    case MyApp.Accounts.create_user(user_params) do
      {:ok, user} ->
        # Queue background job
        %{"user_id" => user.id}
        |> MyApp.Workers.EmailWorker.new()            # => Build job struct
        |> Oban.insert()                              # => Insert into oban_jobs table

        json(conn, user)                              # => Respond immediately

      {:error, changeset} ->
        error_response(conn, changeset)
    end
  end
end

# Schedule recurring jobs
defmodule MyApp.Application do
  def start(_type, _args) do
    children = [
      # ... other children
      Oban,
      # Schedule daily cleanup at 2 AM
      {Oban.Cron, crontab: [
        {"0 2 * * *", MyApp.Workers.CleanupWorker}
      ]}
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: MyApp.Supervisor)
  end
end
```

**Key Takeaway**: Create Worker modules implementing Oban.Worker. Queue jobs asynchronously. Implement retry logic. Use Oban for background processing and cron jobs.

**Why It Matters**: Process-based architecture enables horizontal scaling and fault isolation. Understanding OTP processes is key to building highly available systems.

### Example 59: Phoenix LiveDashboard for Metrics

Monitor your application in real-time with LiveDashboard. View requests, Ecto stats, processes.

```elixir
# mix.exs
defp deps do                                          # => Dependency list for LiveDashboard
                                                       # => Requires 3 libraries for monitoring
  [
    {:phoenix_live_dashboard, "~> 0.7"},              # => Main dashboard library
                                                       # => Provides web UI for metrics/monitoring
                                                       # => Version ~> 0.7 (compatible with 0.7.x)
    {:telemetry_metrics, "~> 0.6"},                   # => Metrics aggregation library
                                                       # => Collects and processes telemetry events
                                                       # => Required by LiveDashboard
    {:telemetry_poller, "~> 1.0"}                     # => Periodic metric collection
                                                       # => Polls VM metrics (memory, processes, etc.)
                                                       # => Runs collection tasks on interval
  ]
end

# lib/my_app_web/telemetry.ex
defmodule MyAppWeb.Telemetry do                       # => Defines Telemetry supervisor module
                                                       # => Manages metric collection processes
  use Supervisor                                      # => Imports Supervisor behavior
                                                       # => Provides supervision functions

  def start_link(arg) do                              # => Public function: start_link/1
                                                       # => Called by application supervisor
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
                                                       # => Starts supervisor process
                                                       # => Registers with module name
                                                       # => Returns {:ok, pid}
  end                                                 # => End start_link/1 function

  @impl true                                          # => Implements Supervisor callback
  def init(_arg) do                                   # => Initializes supervisor
                                                       # => Defines child processes to supervise
    children = [                                      # => List of child specifications
                                                       # => Each child runs independently
      # Telemetry poller periodically collects metrics
      {:telemetry_poller, handlers: handle_metrics()},
                                                       # => Starts telemetry_poller process
                                                       # => Calls handle_metrics() to get metric definitions
                                                       # => Polls metrics on configured interval
      {Phoenix.LiveDashboard.TelemetryListener, []}   # => Starts LiveDashboard listener
                                                       # => Listens for telemetry events
                                                       # => Updates dashboard UI in real-time
    ]                                                 # => End children list

    Supervisor.init(children, strategy: :one_for_one) # => Initializes supervisor with children
                                                       # => Strategy :one_for_one: restart only failed child
                                                       # => If poller crashes, restart poller (not listener)
  end                                                 # => End init/1 callback

  defp handle_metrics do                              # => Private function: handle_metrics/0
                                                       # => Returns list of metrics to collect
    [
      # VM metrics
      {:process_count, unit: {:byte, :kilobyte}},     # => Metric: number of Erlang processes
                                                       # => Unit conversion: bytes to kilobytes
                                                       # => Monitors process count over time
      {:memory, unit: {:byte, :kilobyte}},            # => Metric: total VM memory usage
                                                       # => Converts bytes to KB for display
                                                       # => Tracks memory consumption
      # Ecto metrics
      {MyApp.Repo, [:repo, :adapter, :status], :ok},  # => Metric: database connection status
                                                       # => Event: MyApp.Repo emits [:repo, :adapter, :status]
                                                       # => Tracks connection health
      {MyApp.Repo, [:repo, :adapter, :connection_error], :ok}
                                                       # => Metric: database connection errors
                                                       # => Event: emitted when connection fails
                                                       # => Monitors database issues
    ]                                                 # => End metrics list
  end                                                 # => End handle_metrics/0 function
end                                                   # => End MyAppWeb.Telemetry module

# router.ex
import Phoenix.LiveDashboard.Router                   # => Imports LiveDashboard routing macros
                                                       # => Provides live_dashboard/2 macro

scope "/" do                                          # => Defines root scope
                                                       # => Routes accessible from /
  pipe_through :browser                               # => Uses browser pipeline
                                                       # => Applies session, CSRF, cookies
  live_dashboard "/dashboard"                         # => Mounts LiveDashboard at /dashboard route
                                                       # => Creates LiveView route
                                                       # => Access at http://localhost:4000/dashboard
                                                       # => Shows metrics, processes, Ecto stats in UI
end                                                   # => End scope
```

**Key Takeaway**: Phoenix LiveDashboard shows real-time metrics. Monitor request performance, database connections, memory usage, processes. Access at /dashboard.

**Why It Matters**: Process-based architecture enables horizontal scaling and fault isolation. Understanding OTP processes is key to building highly available systems.

### Example 60: Custom Metrics with Telemetry

Emit custom metrics to track business logic and application behavior.

```elixir
defmodule MyApp.Blog do
  def create_post(attrs) do
    start_time = System.monotonic_time()

    case %Post{}
         |> Post.changeset(attrs)
         |> Repo.insert() do
      {:ok, post} ->
        duration = System.monotonic_time() - start_time

        :telemetry.execute(
          [:blog, :post, :created],
          %{duration: duration},
          %{post_id: post.id, user_id: attrs["user_id"]}
        )

        {:ok, post}

      {:error, changeset} ->
        {:error, changeset}
    end
  end
end

# Listen to events
defmodule MyApp.TelemetryHandler do
  def attach_handlers do
    :telemetry.attach(
      "blog-post-created",
      [:blog, :post, :created],
      &__MODULE__.handle_post_created/4,
      nil
    )
  end

  def handle_post_created(_event, measurements, metadata, _config) do
    IO.inspect({measurements, metadata})
    # Send to monitoring service, log, increment counter, etc.
  end
end

# In your application startup
MyApp.TelemetryHandler.attach_handlers()
```

**Key Takeaway**: Use :telemetry.execute/3 to emit metrics. Attach handlers with :telemetry.attach/4. Track custom business metrics for monitoring and alerting.

**Why It Matters**: This Phoenix pattern is fundamental for building production web applications. Understanding this concept enables you to create robust, maintainable, and scalable applications.

## Group 11: Production Deployment

### Example 61: Mix Releases for Production

Build self-contained release that runs without Elixir/Erlang installed. Configure runtime settings.

```elixir
# mix.exs
def project do                                        # => Project configuration function
                                                       # => Defines app metadata and release settings
  [
    app: :my_app,                                     # => Application name (atom)
                                                       # => Used for release binary name
    version: "0.1.0",                                 # => Current version string
                                                       # => Follows semantic versioning
    releases: [                                       # => Release configuration map
                                                       # => Defines production release settings
      prod: [                                         # => Release name: "prod"
                                                       # => Can have multiple releases (prod, staging, etc.)
        include_executables_for: [:unix],             # => Include Unix executables (start, stop scripts)
                                                       # => Targets Linux/macOS platforms
                                                       # => Excludes Windows executables
        steps: [:assemble, :tar]                      # => Release build steps
                                                       # => :assemble - builds release structure
                                                       # => :tar - creates .tar.gz archive for deployment
      ]
    ]
  ]
end

# config/runtime.exs - Loaded at runtime, not compile time
import Config                                         # => Imports Config macros
                                                       # => Enables config/2 function

config :my_app, MyAppWeb.Endpoint,                    # => Configures Phoenix Endpoint
                                                       # => Settings applied when release starts
  http: [ip: {0, 0, 0, 0}, port: String.to_integer(System.get_env("PORT") || "4000")],
                                                       # => HTTP server configuration
  # => Bind to all interfaces (0.0.0.0) for Docker
                                                       # => ip: {0,0,0,0} accepts connections from any IP
                                                       # => port: reads PORT env var or defaults to 4000
                                                       # => E.g., PORT=8080 → server listens on 8080
  secret_key_base: System.fetch_env!("SECRET_KEY_BASE"),  # => Read from env var
                                                       # => Fetches SECRET_KEY_BASE environment variable
                                                       # => Crashes if not set (fetch_env! requires it)
                                                       # => Used for session encryption
  url: [host: System.get_env("PHX_HOST", "localhost"), port: 443, scheme: "https"]
                                                       # => URL configuration for generated URLs
  # => HTTPS for production
                                                       # => host: reads PHX_HOST or defaults to "localhost"
                                                       # => port: 443 (standard HTTPS port)
                                                       # => scheme: "https" (forces HTTPS URLs)

if config_env() == :prod do                           # => Production-only configuration
                                                       # => Only applies when MIX_ENV=prod
  config :my_app, MyApp.Repo,                         # => Configures Ecto Repo for production
                                                       # => Database connection settings
    url: System.fetch_env!("DATABASE_URL"),           # => Reads DATABASE_URL from environment
                                                       # => E.g., "ecto://user:pass@host/db"
                                                       # => Crashes if DATABASE_URL not set
    ssl: true,                                        # => Enables SSL for database connection
                                                       # => Required for most cloud databases
    socket_options: [:inet6]                          # => Enables IPv6 support
                                                       # => Allows IPv6 database connections
end                                                   # => End production config block

# Build release
# mix release                                         # => Command: builds default release
                                                       # => Creates _build/prod/rel/my_app/ directory
                                                       # => Packages app, dependencies, ERTS runtime

# Or with custom name
# mix release prod                                    # => Command: builds "prod" named release
                                                       # => Uses prod release configuration from mix.exs
                                                       # => Creates tarball for deployment

# Run release
# _build/prod/rel/my_app/bin/my_app start             # => Command: starts release as daemon
                                                       # => Runs in background
                                                       # => Logs to _build/prod/rel/my_app/log/

# Start in foreground
# _build/prod/rel/my_app/bin/my_app foreground        # => Command: starts release in foreground
                                                       # => Logs to stdout/stderr
                                                       # => Useful for Docker containers
                                                       # => Ctrl+C stops the application
```

**Key Takeaway**: Mix.Release builds independent package. config/runtime.exs loads at runtime. Set environment variables for secrets. Releases don't require Elixir installation.

**Why It Matters**: Runtime configuration enables environment-specific settings. This pattern allows the same release to run in different environments without rebuilding.

### Example 62: Docker Containerization with Multi-Stage Build

Build optimized Docker image with minimal size and security.

```mermaid
%% Docker multi-stage build
graph TD
    A[Stage 1: Builder] --> B[elixir:1.14-alpine]
    B --> C[Install build deps]
    C --> D[Copy source code]
    D --> E[mix deps.get]
    E --> F[MIX_ENV=prod mix release]

    G[Stage 2: Runtime] --> H[alpine:latest]
    H --> I[Install runtime deps]
    I --> J[Copy release from builder]
    J --> K[Final image 50MB]

    style A fill:#0173B2,color:#fff
    style F fill:#DE8F05,color:#000
    style G fill:#029E73,color:#fff
    style K fill:#029E73,color:#fff
```

```dockerfile
# Dockerfile - Multi-stage build
FROM elixir:1.14-alpine AS builder                   # => Stage 1: Builder image
                                                       # => Uses Elixir 1.14 on Alpine Linux (minimal)
                                                       # => AS builder: names this stage for later reference

WORKDIR /app                                          # => Sets working directory to /app
                                                       # => All subsequent commands run from /app

# Install build dependencies
RUN apk add --no-cache build-base git                 # => Installs build tools
                                                       # => build-base: gcc, make, etc. for native deps
                                                       # => git: for git-based dependencies
                                                       # => --no-cache: doesn't store package index (smaller image)

# Copy source
COPY mix.exs mix.lock ./                              # => Copies dependency files first
                                                       # => Enables Docker layer caching
                                                       # => If mix.exs unchanged, deps layer cached
RUN mix local.hex --force && mix local.rebar --force  # => Installs Hex package manager
                                                       # => Installs Rebar build tool
                                                       # => --force: non-interactive installation
RUN mix deps.get                                      # => Downloads dependencies
                                                       # => Fetches from Hex and Git sources
                                                       # => Cached if mix.exs/mix.lock unchanged

COPY . .                                              # => Copies entire application code
                                                       # => Runs AFTER deps.get for better caching
                                                       # => Invalidates cache only when code changes

# Compile release
RUN MIX_ENV=prod mix release                          # => Builds production release
                                                       # => MIX_ENV=prod: production optimizations
                                                       # => Creates self-contained package
                                                       # => Output: _build/prod/rel/my_app/

# Runtime stage
FROM alpine:latest                                    # => Stage 2: Runtime image
                                                       # => Minimal Alpine Linux (~5MB base)
                                                       # => No Elixir, no build tools (small image)

WORKDIR /app                                          # => Sets working directory
                                                       # => Same as builder for consistency

# Install runtime dependencies
RUN apk add --no-cache openssl                        # => Installs OpenSSL library
                                                       # => Required for HTTPS/SSL connections
                                                       # => Only runtime dependency needed

# Copy release from builder
COPY --from=builder /app/_build/prod/rel/my_app ./    # => Copies compiled release from builder stage
                                                       # => --from=builder: references first stage
                                                       # => Only copies final binary (not source)
                                                       # => Dramatically reduces image size (50MB vs 500MB+)

EXPOSE 4000                                           # => Documents that container listens on port 4000
                                                       # => Doesn't actually publish port (use docker run -p)
                                                       # => Informational for users

# Health check
HEALTHCHECK --interval=10s --timeout=3s --start-period=40s --retries=3 \
                                                       # => Configures container health check
                                                       # => --interval=10s: check every 10 seconds
                                                       # => --timeout=3s: fail if check takes >3s
                                                       # => --start-period=40s: grace period on startup
                                                       # => --retries=3: mark unhealthy after 3 failures
  CMD ["./bin/my_app", "eval", "MyApp.ready?"]        # => Health check command
                                                       # => Evaluates Elixir expression MyApp.ready?
                                                       # => Should return true when app ready
                                                       # => Kubernetes uses this for liveness/readiness

CMD ["./bin/my_app", "start"]                         # => Default command when container starts
                                                       # => Starts Phoenix application
                                                       # => Runs in foreground (required for Docker)
```

**Key Takeaway**: Multi-stage builds keep image small. Builder stage compiles, runtime stage runs. Use Alpine Linux for minimal footprint. Include health checks.

**Why It Matters**: Health checks enable load balancer integration and monitoring. This is essential for zero-downtime deployments and auto-scaling.

### Example 63: Health Checks for Kubernetes

Implement liveness and readiness endpoints for orchestration systems to manage pod lifecycle.

```mermaid
%% Health check flow in Kubernetes
graph TD
    A[Kubernetes] --> B{Liveness Probe}
    B -->|GET /health/live| C[Health endpoint]
    C --> D{App alive?}
    D -->|Yes 200| E[Keep running]
    D -->|No 5xx| F[Restart pod]

    A --> G{Readiness Probe}
    G -->|GET /health/ready| H[Health endpoint]
    H --> I{DB connected?}
    I -->|Yes 200| J[Send traffic]
    I -->|No 503| K[Remove from load balancer]

    style A fill:#0173B2,color:#fff
    style E fill:#029E73,color:#fff
    style F fill:#CA9161,color:#000
    style J fill:#029E73,color:#fff
    style K fill:#CC78BC,color:#000
```

```elixir
# lib/my_app_web/controllers/health_controller.ex
defmodule MyAppWeb.HealthController do
  use MyAppWeb, :controller

  def readiness(conn, _params) do
    # Check if app is ready to serve traffic
    case check_database() do
      :ok ->
        json(conn, %{status: "ok"})                   # => 200 OK, ready

      :error ->
        conn
        |> put_status(:service_unavailable)           # => 503 Service Unavailable
        |> json(%{status: "error", reason: "database_unavailable"})
        # => Kubernetes removes from load balancer
    end
  end  # => Checked every 5 seconds

  def liveness(conn, _params) do
    # Check if app is alive (should restart if not)
    json(conn, %{status: "ok"})                       # => Always returns 200
  end  # => If this fails, Kubernetes restarts pod

  defp check_database do
    case Ecto.Adapters.SQL.query(MyApp.Repo, "SELECT 1", []) do
      {:ok, _} -> :ok
      {:error, _} -> :error
    end
  end
end

# router.ex
scope "/health", MyAppWeb do
  get "/ready", HealthController, :readiness
  get "/live", HealthController, :liveness
end

# Kubernetes deployment yaml
# livenessProbe:
#   httpGet:
#     path: /health/live
#     port: 4000
#   initialDelaySeconds: 30
#   periodSeconds: 10
# readinessProbe:
#   httpGet:
#     path: /health/ready
#     port: 4000
#   initialDelaySeconds: 5
#   periodSeconds: 5
```

**Key Takeaway**: Readiness probe indicates if app can handle traffic. Liveness probe indicates if app needs restart. Health endpoints check critical dependencies (database, cache).

**Why It Matters**: Endpoints configure the HTTP entry point for your application. Understanding endpoint configuration is essential for performance tuning and security.

### Example 64: Graceful Shutdown

Handle shutdown signals gracefully, completing in-flight requests before terminating.

```elixir
# lib/my_app/application.ex
defmodule MyApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      MyAppWeb.Telemetry,
      MyApp.Repo,
      {Phoenix.PubSub, name: MyApp.PubSub},
      MyAppWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    MyAppWeb.Endpoint.config_change(changed, removed)
  end

  @impl true
  def prep_stop(_state) do
    # Called before shutdown
    # Drain in-flight requests
    IO.puts("Shutting down gracefully...")           # => Log shutdown
    :ok
  end  # => Application.stop/1 waits for this to return
end

# In endpoint config
config :my_app, MyAppWeb.Endpoint,
  # Graceful shutdown timeout (milliseconds)
  shutdown: 25_000,
  # Give existing connections time to close
  drain_on_stop: true
```

**Key Takeaway**: prep_stop/1 gives app chance to drain requests. Set shutdown timeout. Complete in-flight work before terminating. Important for zero-downtime deployments.

**Why It Matters**: Deployment releases enable zero-downtime updates and easy rollbacks. Understanding OTP releases is essential for production Phoenix applications.

### Example 65: Environment Configuration Management

Separate configuration by environment. Use runtime configuration for secrets.

```elixir
# config/config.exs - Common to all environments
import Config                                         # => Imports Config module
                                                       # => Provides config/2 and config/3 macros

config :logger, :console,                             # => Configures Logger console backend
                                                       # => Applies to all environments (dev, test, prod)
  format: "$time $metadata[$level] $message\n"        # => Log format template
                                                       # => $time: timestamp, $level: info/warn/error
                                                       # => $message: log message, $metadata: custom data

# config/dev.exs
import Config                                         # => Development environment config
                                                       # => Only loaded when MIX_ENV=dev

config :my_app, MyApp.Repo,                           # => Ecto Repo configuration for dev
                                                       # => Local PostgreSQL database
  username: "postgres",                               # => Database username
                                                       # => Default PostgreSQL user
  password: "postgres",                               # => Database password
                                                       # => Hardcoded OK for dev (never in prod!)
  hostname: "localhost",                              # => Database host
                                                       # => Connects to local PostgreSQL
  port: 5432,                                         # => PostgreSQL default port
                                                       # => Standard port number
  database: "my_app_dev"                              # => Database name
                                                       # => Separate database for development

config :my_app, MyAppWeb.Endpoint,                    # => Phoenix Endpoint configuration
                                                       # => Dev-specific settings for fast iteration
  debug_errors: true,                                 # => Shows detailed error pages
                                                       # => Displays stack traces in browser
                                                       # => NEVER enable in production!
  check_origin: false,                                # => Disables WebSocket origin checking
                                                       # => Allows connections from any origin in dev
                                                       # => Required for LiveView development
  watchers: [esbuild: {Esbuild, :install_and_run, []}]
                                                       # => File watchers for asset compilation
                                                       # => Auto-rebuilds JS/CSS on file changes
                                                       # => esbuild watches and compiles assets

# config/test.exs
import Config                                         # => Test environment configuration
                                                       # => Only loaded when MIX_ENV=test

config :my_app, MyApp.Repo,                           # => Ecto Repo configuration for tests
                                                       # => Separate database for test isolation
  username: "postgres",                               # => Database username
                                                       # => Same as dev for simplicity
  password: "postgres",                               # => Database password
                                                       # => Same as dev for simplicity
  database: "my_app_test",                            # => Test database name
                                                       # => Separate from dev database
  pool: Ecto.Adapters.SQL.Sandbox                     # => Connection pool for tests
                                                       # => Sandbox: wraps each test in transaction
                                                       # => Auto-rollback after each test
                                                       # => Ensures test isolation and cleanup

# config/runtime.exs - Loaded at runtime
import Config                                         # => Runtime configuration
                                                       # => Loaded when release starts (NOT at compile time)
                                                       # => Enables environment-specific configuration

database_url = System.get_env("DATABASE_URL") || "ecto://postgres:postgres@localhost/my_app_prod"
                                                       # => Reads DATABASE_URL from environment
                                                       # => Falls back to default for local prod
                                                       # => E.g., "ecto://user:pass@host:5432/dbname"
                                                       # => Production: set by deployment platform

config :my_app, MyApp.Repo, url: database_url         # => Configures Repo with database URL
                                                       # => Parses URL into connection params
                                                       # => Overrides compile-time config

secret_key_base = System.fetch_env!("SECRET_KEY_BASE")
                                                       # => Reads SECRET_KEY_BASE from environment
                                                       # => Crashes if not set (fetch_env! requires it)
                                                       # => Used for encrypting sessions/cookies
                                                       # => Generate with: mix phx.gen.secret

config :my_app, MyAppWeb.Endpoint,                    # => Endpoint runtime configuration
                                                       # => Settings loaded when app starts
  secret_key_base: secret_key_base,                   # => Sets secret from environment
                                                       # => Required for session encryption
  url: [host: System.get_env("PHX_HOST", "localhost"), port: 443, scheme: "https"]
                                                       # => URL configuration for generated links
                                                       # => host: reads PHX_HOST or defaults to localhost
                                                       # => port: 443 (HTTPS), scheme: "https"
                                                       # => Affects url_for/3 and route helpers
```

**Key Takeaway**: config/ files configure at compile time. config/runtime.exs loads at runtime (for secrets). Use environment variables for production secrets. Never commit secrets to git.

**Why It Matters**: This Phoenix pattern is fundamental for building production web applications. Understanding this concept enables you to create robust, maintainable, and scalable applications.

## Group 12: Resilience & Observability

### Example 66: Error Tracking and Structured Logging

Send errors to Sentry. Log with context for debugging.

```elixir
# mix.exs
defp deps do
  [
    {:sentry, "~> 10.0"},
    {:logger_backends, "~> 1.1"},
    {:jason, "~> 1.4"}
  ]
end

# config/config.exs
config :sentry,
  dsn: System.get_env("SENTRY_DSN"),
  environment_name: Mix.env(),
  enable_in_test: false

# lib/my_app_web/router.ex
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  # Sentry captures errors
  use Sentry.PlugContext
end

# Structured logging
defmodule MyApp.Blog do                               # => Defines Blog context module
                                                       # => Demonstrates structured logging patterns
  require Logger                                      # => Imports Logger macros
                                                       # => Enables compile-time logging optimization

  def create_post(attrs) do                           # => Public function: create_post/1
                                                       # => Param: attrs map with post data
    Logger.info("Creating post", %{user_id: attrs["user_id"], title: attrs["title"]})
                                                       # => Logs informational message with structured metadata
                                                       # => First arg: string message
                                                       # => Second arg: metadata map with context
                                                       # => E.g., [info] Creating post user_id=123 title="Hello"
                                                       # => Structured data parseable by log aggregators

    case MyApp.Repo.insert(changeset) do              # => Attempts to insert post into database
                                                       # => Returns {:ok, post} or {:error, changeset}
      {:ok, post} ->                                  # => Success case: post created
                                                       # => post is inserted struct with ID
        Logger.info("Post created successfully", %{post_id: post.id})
                                                       # => Logs success with post ID
                                                       # => Metadata includes database-generated ID
                                                       # => Enables tracking specific post through logs
        {:ok, post}                                   # => Returns success tuple to caller
                                                       # => Caller receives created post struct

      {:error, changeset} ->                          # => Failure case: validation errors
                                                       # => changeset contains error details
        Logger.warning("Post creation failed", %{     # => Logs warning (not error - expected case)
                                                       # => Warning level: operation failed but handled
          errors: changeset.errors,                   # => Includes validation errors
                                                       # => E.g., [{:title, {"is required", []}}]
          user_id: attrs["user_id"]                   # => Includes user context for debugging
                                                       # => Helps correlate failures to users
        })
        {:error, changeset}                           # => Returns error tuple to caller
                                                       # => Caller can display validation errors
    end                                               # => End case statement
  rescue                                              # => Rescue clause catches unexpected exceptions
                                                       # => Only for crashes, not {:error, ...} returns
    e ->                                              # => Binds exception to variable e
                                                       # => e is exception struct (e.g., ArithmeticError)
      Logger.error("Post creation crashed",           # => Logs error (unexpected crash)
                                                       # => Error level: requires investigation
        error: inspect(e),                            # => Logs exception details
                                                       # => inspect/1 converts to readable string
                                                       # => E.g., %ArithmeticError{message: "divide by zero"}
        stacktrace: Exception.format_stacktrace(__STACKTRACE__)
                                                       # => Logs formatted stack trace
                                                       # => __STACKTRACE__: built-in macro with call stack
                                                       # => Shows file, line, function for each frame
      )
      {:error, "Internal error"}                      # => Returns generic error to caller
                                                       # => Doesn't expose exception details to user
                                                       # => Full details in logs/Sentry
  end                                                 # => End rescue clause
end                                                   # => End MyApp.Blog module
```

**Key Takeaway**: Sentry captures production errors. Structured logging adds context. Use Logger.info/warn/error with metadata maps. Include request IDs for tracing.

**Why It Matters**: Contexts provide bounded modules for organizing business logic. This pattern enables clean API boundaries between different parts of your application and improves maintainability.

### Example 67: Rate Limiting with Token Bucket

Prevent abuse by limiting requests per IP or user. Token bucket algorithm refills over time.

```mermaid
%% Rate limiting state machine
stateDiagram-v2
    [*] --> CheckBucket: Request arrives
    CheckBucket --> HasTokens: Tokens available
    CheckBucket --> NoTokens: Bucket empty
    HasTokens --> ConsumeToken: Take 1 token
    ConsumeToken --> AllowRequest: Process request
    NoTokens --> RejectRequest: 429 Too Many Requests
    AllowRequest --> [*]
    RejectRequest --> [*]

    note right of CheckBucket: Bucket refills over time
    note right of NoTokens: Client must wait
```

```elixir
defmodule MyApp.RateLimiter do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def allow_request?(key, max_requests, time_window_ms) do
    GenServer.call(__MODULE__, {:check, key, max_requests, time_window_ms})
  end

  @impl true
  def init(_opts) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:check, key, max_requests, time_window_ms}, _from, state) do
    now = System.monotonic_time(:millisecond)

    {requests, state} = case Map.get(state, key) do
      {count, reset_time} when reset_time > now ->
        {count, state}                                # => Window still active

      _ ->
        # Reset bucket
        reset_time = now + time_window_ms             # => New window starts
        {0, Map.put(state, key, {0, reset_time})}     # => Reset counter
    end  # => Token bucket refills

    if requests < max_requests do
      {count, reset_time} = Map.get(state, key)
      new_state = Map.put(state, key, {count + 1, reset_time})  # => Consume token
      {:reply, :ok, new_state}                        # => Allow request

    else
      {:reply, :rate_limited, state}                  # => Reject request
    end  # => Returns 429 Too Many Requests
  end
end

# Plug for rate limiting
defmodule MyAppWeb.Plugs.RateLimit do
  def init(opts), do: opts

  def call(conn, opts) do
    max_requests = opts[:max_requests] || 100
    time_window = opts[:time_window] || 60_000

    key = conn.remote_ip |> Tuple.to_list() |> Enum.join(".")

    case MyApp.RateLimiter.allow_request?(key, max_requests, time_window) do
      :ok -> conn
      :rate_limited ->
        conn
        |> put_status(:too_many_requests)
        |> json(%{error: "Rate limit exceeded"})
        |> halt()
    end
  end
end
```

**Key Takeaway**: Rate limiting prevents abuse. Token bucket algorithm is fair and flexible. Apply per IP or per user. Return 429 Too Many Requests when limited.

**Why It Matters**: Event-driven patterns decouple components and enable scalable architectures. Understanding events is key to building maintainable Phoenix applications.

### Example 68: Distributed Phoenix Clustering

Connect multiple Phoenix instances for distributed state and fault tolerance.

```mermaid
%% Distributed Phoenix cluster
graph TD
    A[Load Balancer] --> B[Phoenix Node 1]
    A --> C[Phoenix Node 2]
    A --> D[Phoenix Node 3]

    B <-->|Distributed Erlang| C
    C <-->|Distributed Erlang| D
    B <-->|Distributed Erlang| D

    B --> E[PubSub broadcasts to all nodes]
    C --> E
    D --> E

    E --> F[PostgreSQL]

    style A fill:#0173B2,color:#fff
    style B fill:#029E73,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#029E73,color:#fff
    style F fill:#DE8F05,color:#000
```

```elixir
# mix.exs
defp deps do
  [
    {:libcluster, "~> 3.3"},
    {:observer_cli, "~> 1.7"}
  ]
end

# config/config.exs
config :libcluster,
  topologies: [
    example: [
      strategy: Cluster.Strategy.Kubernetes.DNS,      # => Auto-discover in K8s
      config: [
        service: "my_app-headless",                   # => Headless service name
        namespace: "default"                          # => K8s namespace
      ]
    ]
  ]  # => Nodes connect automatically via DNS

# Or with fixed IPs
config :libcluster,
  topologies: [
    fixed: [
      strategy: Cluster.Strategy.Static,
      config: [
        nodes: [:"app1@10.0.0.1", :"app2@10.0.0.2", :"app3@10.0.0.3"]
      ]
    ]
  ]

# lib/my_app/cluster.ex
defmodule MyApp.Cluster do
  def start_link(opts) do
    Supervisor.start_link(
      [{:libcluster, Cluster.Strategy.Kubernetes.DNS, [topologies: topologies()]}],
      opts
    )
  end

  defp topologies do
    Application.get_env(:libcluster, :topologies)
  end
end

# Distributed PubSub across nodes
broadcast_all = fn event, data ->
  MyApp.PubSub
  |> Phoenix.PubSub.broadcast("topic", {:event, event, data})
  # => Sends to ALL nodes in cluster
end  # => Every connected LiveView receives update

# List connected nodes
Node.list()
```

**Key Takeaway**: libcluster connects nodes automatically. Distribute PubSub across cluster. All nodes share state. Provides fault tolerance.

**Why It Matters**: PubSub enables event-driven communication between processes. This pattern powers real-time broadcasts and decoupled system architecture.

### Example 69: WebSocket Load Balancing with Sticky Sessions

Route WebSocket connections to same server. Use load balancer affinity.

```nginx
# nginx.conf - Sticky sessions by IP
upstream my_app {                                     # => Defines upstream server group "my_app"
                                                       # => Load balancer distributes requests across these
  server app1:4000;                                   # => Backend server 1 at app1:4000
                                                       # => Phoenix node 1
  server app2:4000;                                   # => Backend server 2 at app2:4000
                                                       # => Phoenix node 2
  server app3:4000;                                   # => Backend server 3 at app3:4000
                                                       # => Phoenix node 3

  hash $remote_addr consistent;  # => Sticky by IP
                                                       # => Routes same IP to same backend server
                                                       # => consistent: uses consistent hashing
                                                       # => Required for WebSocket persistence
}                                                     # => End upstream block

server {                                              # => Defines HTTP server block
                                                       # => Handles incoming requests
  listen 80;                                          # => Listens on port 80 (HTTP)
                                                       # => Standard HTTP port
  server_name my_app.com;                             # => Server domain name
                                                       # => Only processes requests for my_app.com

  location / {                                        # => Matches all paths (/)
                                                       # => Applies to all routes
    proxy_pass http://my_app;                         # => Proxies requests to my_app upstream
                                                       # => Load balances across app1/app2/app3
    proxy_http_version 1.1;                           # => Uses HTTP/1.1 protocol
                                                       # => Required for WebSocket upgrades

    # WebSocket headers
    proxy_set_header Upgrade $http_upgrade;           # => Forwards Upgrade header
                                                       # => Enables WebSocket protocol upgrade
                                                       # => $http_upgrade: request's Upgrade value
    proxy_set_header Connection "upgrade";            # => Sets Connection header to "upgrade"
                                                       # => Required for WebSocket handshake
                                                       # => Tells backend to upgrade connection

    # Preserve client IP
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                                                       # => Forwards client IP address
                                                       # => Appends to X-Forwarded-For chain
                                                       # => Phoenix reads from this header
    proxy_set_header X-Real-IP $remote_addr;          # => Sets original client IP
                                                       # => $remote_addr: nginx's client IP variable
                                                       # => Phoenix uses for rate limiting/logging
  }                                                   # => End location block
}                                                     # => End server block

# HAProxy.cfg - Also supports sticky sessions
backend phoenix_nodes                                 # => Defines HAProxy backend group
                                                       # => Alternative to nginx for sticky sessions
  balance roundrobin                                  # => Load balancing algorithm: round robin
                                                       # => Distributes requests evenly (with cookie affinity)
  cookie SERVERID insert indirect nocache             # => Cookie-based sticky sessions
                                                       # => insert: HAProxy adds SERVERID cookie
                                                       # => indirect: doesn't forward cookie to backend
                                                       # => nocache: prevents caching of Set-Cookie

  server node1 app1:4000 check cookie node1           # => Backend server 1
                                                       # => check: health checks enabled
                                                       # => cookie node1: assigns cookie value "node1"
                                                       # => Clients get SERVERID=node1 cookie
  server node2 app2:4000 check cookie node2           # => Backend server 2
                                                       # => Clients routed here get SERVERID=node2
  server node3 app3:4000 check cookie node3           # => Backend server 3
                                                       # => HAProxy routes based on cookie value
                                                       # => WebSockets stay on same server
```

**Key Takeaway**: WebSockets require persistent connections to same server. Use sticky sessions (by IP or cookie). Load balancer must preserve connection. Forward X-Forwarded-For headers.

**Why It Matters**: Phoenix sockets enable efficient bidirectional communication for real-time features. This is essential for chat, notifications, and collaborative editing applications.

### Example 70: Blue-Green Deployment for Zero-Downtime Releases

Run two production environments. Switch traffic after verifying new version works.

```mermaid
%% Blue-green deployment flow
graph TD
    A[Current: Blue v1.0] --> B[Deploy Green v1.1]
    B --> C[Test Green in isolation]
    C --> D{Tests pass?}
    D -->|Yes| E[Switch traffic to Green]
    D -->|No| F[Keep Blue, fix Green]
    E --> G[Green is now production]
    G --> H[Blue becomes standby]
    H --> I[Rollback available]

    style A fill:#0173B2,color:#fff
    style C fill:#DE8F05,color:#000
    style E fill:#029E73,color:#fff
    style F fill:#CA9161,color:#000
    style I fill:#CC78BC,color:#000
```

```bash
# Blue-Green deployment with two releases
#!/bin/bash                                           # => Bash script for blue-green deployment
                                                       # => Zero-downtime deployment strategy

# Current version (blue)
BLUE_VERSION=$(aws ecs describe-services --cluster prod --services my-app | \
  jq -r '.services[0].taskDefinition' | \
  grep -oP 'my-app:\K[^:]+')                          # => Gets current running version from AWS ECS
                                                       # => Queries ECS service task definition
                                                       # => Extracts version number from task definition
                                                       # => E.g., "my-app:v1.2.3" → BLUE_VERSION="v1.2.3"

# New version (green)
GREEN_VERSION=$(git describe --tags --always)        # => Gets new version from Git tags
                                                       # => Uses latest Git tag or commit SHA
                                                       # => E.g., "v1.3.0" or "abc123def"

# Build and deploy green version
mix release                                           # => Builds Elixir production release
                                                       # => Creates _build/prod/rel/my_app/
                                                       # => Self-contained package

docker build -t my-app:$GREEN_VERSION .               # => Builds Docker image with new version tag
                                                       # => Uses Dockerfile in current directory
                                                       # => Tags image with GREEN_VERSION
                                                       # => E.g., my-app:v1.3.0
docker tag my-app:$GREEN_VERSION my-app:green         # => Tags image with "green" alias
                                                       # => Creates second tag for same image
                                                       # => my-app:green always points to latest deployment
docker push my-app:green                              # => Pushes image to Docker registry
                                                       # => Uploads to ECR/DockerHub
                                                       # => Makes available to ECS cluster

# Register new task definition
aws ecs register-task-definition \
  --family my-app \
  --container-definitions "[{\"image\": \"my-app:$GREEN_VERSION\", ...}]"
                                                       # => Registers new ECS task definition
                                                       # => Task definition: container specs (image, CPU, memory)
                                                       # => Creates new revision with green version
                                                       # => E.g., my-app:47 (revision number increments)

# Update service to use green
aws ecs update-service \
  --cluster prod \
  --service my-app \
  --task-definition my-app:$GREEN_VERSION             # => Updates ECS service to use new task definition
                                                       # => Triggers deployment of green version
                                                       # => ECS starts new tasks with green containers
                                                       # => Blue tasks still running (zero downtime)

# Wait for deployment
aws ecs wait services-stable \
  --cluster prod \
  --services my-app                                   # => Blocks until deployment completes
                                                       # => Waits for all green tasks to be healthy
                                                       # => Waits for old blue tasks to drain
                                                       # => Timeout after 10 minutes if not stable

# Test green version
curl http://green.my-app.com/health/ready             # => Tests green version health endpoint
                                                       # => Verifies application is responding
                                                       # => Checks database connectivity
                                                       # => Returns HTTP 200 if ready

if [ $? -eq 0 ]; then                                 # => Checks if curl succeeded (exit code 0)
                                                       # => $? is exit code of previous command
  # Switch traffic from blue to green
  aws route53 change-resource-record-sets \
    --hosted-zone-id Z123 \
    --change-batch "[{\"Action\": \"UPSERT\", \"ResourceRecordSet\": {\"Name\": \"my-app.com\", \"Type\": \"A\", \"TTL\": 60, \"ResourceRecords\": [{\"Value\": \"green-alb.aws.com\"}]}}]"
                                                       # => Updates DNS to point to green load balancer
                                                       # => UPSERT: creates or updates DNS record
                                                       # => Changes my-app.com A record to green ALB
                                                       # => TTL 60s: DNS caches for 1 minute
                                                       # => Traffic gradually shifts to green

  echo "Deployment successful!"                       # => Logs success message
                                                       # => Green version now serving production traffic
else
  # Rollback to blue
  aws ecs update-service \
    --cluster prod \
    --service my-app \
    --task-definition my-app:$BLUE_VERSION            # => Rolls back to blue version
                                                       # => Updates service to use previous task definition
                                                       # => Keeps blue running, stops green
                                                       # => Fast rollback without re-deployment
fi                                                    # => End if statement
```

**Key Takeaway**: Blue-green keeps current version running while deploying new version. Switch traffic only after verification. Can rollback instantly. Zero-downtime deployments.

**Why It Matters**: Deployment releases enable zero-downtime updates and easy rollbacks. Understanding OTP releases is essential for production Phoenix applications.

### Example 71: Custom Ecto Types for Domain Logic

Create custom Ecto types to encapsulate domain logic and validation.

```elixir
# lib/my_app/ecto_types/money.ex
defmodule MyApp.EctoTypes.Money do
  use Ecto.Type

  def type, do: :map                                  # => Stored as JSONB in PostgreSQL

  # Cast from user input
  def cast(%{"amount" => amount, "currency" => currency}) when is_number(amount) do
    {:ok, %{amount: amount, currency: currency}}      # => Valid money struct
  end
  def cast(_), do: :error                             # => Invalid format

  # Load from database
  def load(%{"amount" => amount, "currency" => currency}) do
    {:ok, %{amount: Decimal.new(amount), currency: currency}}  # => Convert to Decimal
  end

  # Dump to database
  def dump(%{amount: amount, currency: currency}) do
    {:ok, %{"amount" => Decimal.to_float(amount), "currency" => currency}}
    # => Store as JSON: {"amount": 19.99, "currency": "USD"}
  end
  def dump(_), do: :error
end

# Usage in schema
defmodule MyApp.Shop.Product do                       # => Defines Product schema using custom Money type
                                                       # => Demonstrates domain logic encapsulation
  use Ecto.Schema                                     # => Imports Ecto.Schema macros
                                                       # => Provides schema/2, field/3, etc.

  schema "products" do                                # => Maps to "products" table
                                                       # => Auto-generates id, inserted_at, updated_at
    field :name, :string                              # => Standard string field for product name
                                                       # => Will be accessible as product.name
    field :price, MyApp.EctoTypes.Money                # => Custom type
                                                       # => Uses Money Ecto type defined above
                                                       # => Handles validation, casting, db conversion
                                                       # => Accessible as product.price (map with amount/currency)
    timestamps()                                      # => Adds inserted_at, updated_at fields
                                                       # => Auto-managed by Ecto
  end                                                 # => End schema definition

  def changeset(product, attrs) do                    # => Public function: changeset/2
                                                       # => Validates and casts product attributes
    product                                           # => Starts with Product struct
                                                       # => Either new %Product{} or existing from database
    |> cast(attrs, [:name, :price])                   # => Cast price with custom type
                                                       # => Casts attrs to allowed fields
                                                       # => price calls Money.cast/1 automatically
                                                       # => Returns changeset
    |> validate_required([:name, :price])             # => Validates required fields
                                                       # => Adds errors if name or price missing
    |> validate_price()                               # => Domain validation
                                                       # => Custom business rule validation
  end                                                 # => End changeset/2 function

  defp validate_price(changeset) do                   # => Private function: validate_price/1
                                                       # => Domain-specific validation logic
    case get_change(changeset, :price) do             # => Gets changed price value
                                                       # => Returns nil if price unchanged
                                                       # => Returns money map if changed
      %{amount: amount} when amount < 0 ->            # => Pattern match: negative price
                                                       # => Guard: amount < 0
        add_error(changeset, :price, "must be positive")  # => Business rule
                                                       # => Adds validation error
                                                       # => Error: {:price, {"must be positive", []}}
                                                       # => Prevents negative prices

      _ ->                                            # => Catch-all: price valid or unchanged
                                                       # => Either positive amount or nil
        changeset                                     # => Returns unchanged changeset
                                                       # => No validation errors
    end                                               # => End case statement
  end                                                 # => End validate_price/1 function
end                                                   # => End MyApp.Shop.Product module

# Create product
%Product{}                                            # => Creates empty Product struct
                                                       # => %Product{name: nil, price: nil}
|> Product.changeset(%{                               # => Builds changeset with attributes
                                                       # => Validates and casts input
  name: "Widget",                                     # => Product name
                                                       # => Simple string value
  price: %{"amount" => 19.99, "currency" => "USD"}    # => Input format
                                                       # => Plain map (user input format)
                                                       # => Money.cast/1 converts to internal format
})
|> Repo.insert()                                      # => Inserts product into database
# => Stores: {"amount": 19.99, "currency": "USD"} in database
                                                       # => Money.dump/1 converts to JSONB
                                                       # => Database column stores JSON
                                                       # => Returns {:ok, %Product{}} on success
```

**Key Takeaway**: Custom Ecto types encapsulate domain logic. Implement cast/1, load/1, dump/1, and type/0. Use Decimal for money to avoid floating-point errors.

**Why It Matters**: Ecto provides type-safe database interactions with compile-time query validation. This prevents SQL injection and catches query errors before deployment.

### Example 72: Database Fragments for Advanced Queries

Use Ecto fragments for PostgreSQL-specific features and complex SQL.

```elixir
# Full-text search with tsvector
defmodule MyApp.Blog do
  import Ecto.Query

  def search_posts(search_term) do
    from p in Post,
      where: fragment(
        "to_tsvector('english', ?) @@ plainto_tsquery('english', ?)",
        p.title,                                      # => Search in title
        ^search_term                                  # => User's search query
      ),
      order_by: fragment(
        "ts_rank(to_tsvector('english', ?), plainto_tsquery('english', ?)) DESC",
        p.title,
        ^search_term                                  # => Rank by relevance
      ),
      select: %{
        post: p,
        rank: fragment("ts_rank(to_tsvector('english', ?), plainto_tsquery('english', ?))",
          p.title, ^search_term)                      # => Include rank in results
      }
    |> Repo.all()
    # => SELECT posts.*, ts_rank(...) FROM posts WHERE to_tsvector(...) @@ plainto_tsquery(...)
  end

  # JSONB queries
  def posts_by_metadata(key, value) do
    from p in Post,
      where: fragment("? ->> ? = ?", p.metadata, ^key, ^value)
      # => SELECT * FROM posts WHERE metadata->>'status' = 'draft'
    |> Repo.all()
  end

  # Array contains
  def posts_with_tag(tag) do
    from p in Post,
      where: fragment("? @> ?::jsonb", p.tags, ^[tag])  # => Array contains tag
    |> Repo.all()
    # => SELECT * FROM posts WHERE tags @> '["elixir"]'::jsonb
  end

  # Window functions
  def posts_with_rank do                              # => Public function: posts_with_rank/0
                                                       # => Uses PostgreSQL window functions
    from p in Post,                                   # => Starts query from Post table
                                                       # => p is alias for posts
      select: %{                                      # => Selects map with post and ranking fields
                                                       # => Returns list of maps, not Post structs
        post: p,                                      # => Includes full post struct
                                                       # => All post fields available
        row_number: fragment("ROW_NUMBER() OVER (ORDER BY ? DESC)", p.inserted_at),
                                                       # => Window function: assigns sequential numbers
        # => Assign sequential number
                                                       # => ROW_NUMBER(): 1, 2, 3... (no ties)
                                                       # => OVER: defines window (all rows)
                                                       # => ORDER BY inserted_at DESC: newest first
                                                       # => E.g., newest post = 1, second newest = 2
        rank: fragment("RANK() OVER (ORDER BY ? DESC)", p.view_count)
                                                       # => Window function: ranks by view count
        # => Rank by view count (ties get same rank)
                                                       # => RANK(): ties get same rank, next skips
                                                       # => E.g., 1000 views = rank 1, 1000 views = rank 1, 500 views = rank 3
                                                       # => ORDER BY view_count DESC: most views first
      }                                               # => End select map
    |> Repo.all()                                     # => Executes query
                                                       # => Returns list of maps with post, row_number, rank
  end                                                 # => End posts_with_rank/0 function

  # Common Table Expressions (CTEs)
  def posts_with_comment_count do                     # => Public function: posts_with_comment_count/0
                                                       # => Demonstrates CTE (WITH clause) usage
    comments_cte = from c in Comment,                 # => Defines CTE: counts comments per post
                                                       # => c is alias for comments table
      group_by: c.post_id,                            # => Groups comments by post_id
                                                       # => Aggregates all comments for each post
      select: %{post_id: c.post_id, count: count(c.id)}
                                                       # => Selects post_id and comment count
                                                       # => count(c.id): number of comments
                                                       # => Result: %{post_id: 1, count: 5}, %{post_id: 2, count: 3}

    from p in Post,                                   # => Main query from posts table
                                                       # => Joins with CTE result
      left_join: c in subquery(comments_cte), on: c.post_id == p.id,
                                                       # => LEFT JOIN with CTE subquery
                                                       # => subquery: executes comments_cte inline
                                                       # => LEFT JOIN: includes posts with 0 comments
                                                       # => ON: matches post ID
      select: %{post: p, comment_count: coalesce(c.count, 0)}
                                                       # => Selects post and comment count
                                                       # => coalesce(c.count, 0): returns 0 if NULL
                                                       # => Handles posts with no comments (LEFT JOIN NULL)
    |> Repo.all()                                     # => Executes query
    # => WITH comments AS (...) SELECT posts.*, COALESCE(comments.count, 0)
                                                       # => Generates SQL with CTE
                                                       # => Result: [%{post: %Post{}, comment_count: 5}, ...]
  end                                                 # => End posts_with_comment_count/0 function
end                                                   # => End module
```

**Key Takeaway**: Use fragment/1 for database-specific SQL. Supports full-text search, JSONB queries, window functions, and CTEs. Always use ^pinned parameters to prevent SQL injection.

**Why It Matters**: JSON API patterns enable integration with mobile apps and external services. Phoenix makes building RESTful APIs straightforward with proper content negotiation.

### Example 73: Query Profiling with Telemetry

Measure query performance to identify slow queries in production.

```elixir
# lib/my_app/telemetry.ex
defmodule MyApp.Telemetry do
  require Logger

  def attach_handlers do
    :telemetry.attach(
      "my-app-ecto-query",                            # => Handler ID
      [:my_app, :repo, :query],                       # => Event name
      &__MODULE__.handle_query/4,                     # => Handler function
      nil
    )
  end

  def handle_query(_event, measurements, metadata, _config) do
    query_time = measurements.total_time              # => Time in native units
    query_time_ms = System.convert_time_unit(query_time, :native, :millisecond)

    if query_time_ms > 100 do                         # => Slow query threshold
      Logger.warning("Slow query detected",
        query: inspect(metadata.query),               # => SQL query
        params: inspect(metadata.params),             # => Query parameters
        time_ms: query_time_ms,                       # => Execution time
        source: metadata.source                       # => Table name
      )
      # => Log: Slow query detected query="SELECT ..." time_ms=250
    end

    # Send to monitoring service
    if query_time_ms > 1000 do                        # => Very slow (>1s)
      MyApp.Monitoring.report_slow_query(%{
        query: metadata.query,
        time_ms: query_time_ms,
        type: metadata.type                           # => :ecto_sql_query
      })
    end
  end
end

# In application.ex
def start(_type, _args) do
  MyApp.Telemetry.attach_handlers()                  # => Attach on startup

  children = [
    MyApp.Repo,
    MyAppWeb.Endpoint
  ]

  Supervisor.start_link(children, strategy: :one_for_one)
end

# Query all telemetry events
defmodule MyApp.QueryStats do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    :telemetry.attach_many(
      "query-stats",
      [
        [:my_app, :repo, :query],                     # => Ecto queries
        [:phoenix, :router_dispatch, :stop],          # => HTTP requests
        [:phoenix, :endpoint, :stop]                  # => Endpoint timing
      ],
      &__MODULE__.handle_event/4,
      %{stats: %{}}
    )

    {:ok, %{query_count: 0, total_time: 0}}
  end

  def handle_event([:my_app, :repo, :query], measurements, _metadata, state) do
    state = %{
      query_count: state.query_count + 1,             # => Increment counter
      total_time: state.total_time + measurements.total_time
    }
    {:ok, state}
  end

  def get_stats do
    GenServer.call(__MODULE__, :get_stats)            # => Retrieve stats
  end

  def handle_call(:get_stats, _from, state) do
    avg_time = if state.query_count > 0 do
      state.total_time / state.query_count            # => Average query time
    else
      0
    end

    {:reply, %{
      query_count: state.query_count,
      avg_time_ms: System.convert_time_unit(avg_time, :native, :millisecond)
    }, state}
  end
end
```

**Key Takeaway**: Attach telemetry handlers to [:repo, :query] events. Log slow queries (>100ms). Track query count and average time. Use for production monitoring and optimization.

**Why It Matters**: Telemetry enables observability and performance monitoring. This is critical for debugging production issues and understanding application behavior.

### Example 74: Advanced LiveView Performance Optimization

Optimize LiveView rendering with targeted updates and efficient assigns.

```elixir
defmodule MyAppWeb.DashboardLive do
  use Phoenix.LiveView

  # Temporary assigns - not tracked for diff
  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(1000, self(), :tick)       # => Update every second
    end

    {:ok,
     socket
     |> assign(:time, DateTime.utc_now())             # => Tracked assign
     |> assign(:posts, load_posts())
     |> assign_new(:expensive_data, fn -> load_expensive_data() end)}
     # => Only computed on first mount, cached after
  end

  # Use temporary_assigns to avoid diffing large data
  def handle_info(:tick, socket) do
    {:noreply,
     socket
     |> assign(:time, DateTime.utc_now())             # => Only updates time
     |> push_event("time-update", %{time: DateTime.to_string(socket.assigns.time)})}
     # => Send JS event instead of re-rendering
  end

  # Optimize list rendering with streams
  def handle_event("load_more", _params, socket) do
    new_posts = load_more_posts(socket.assigns.last_id)

    {:noreply, stream_insert(socket, :posts, new_posts)}  # => Append to stream
    # => Only new items rendered, existing items unchanged
  end

  # Debounce user input
  def handle_event("search", %{"query" => query}, socket) do
    Process.send_after(self(), {:search, query}, 300)  # => Debounce 300ms
    {:noreply, assign(socket, :search_query, query)}
  end

  def handle_info({:search, query}, socket) do
    if socket.assigns.search_query == query do       # => Check still current
      results = search_posts(query)                   # => Execute search
      {:noreply, assign(socket, :search_results, results)}
    else
      {:noreply, socket}                              # => User typed more, skip
    end
  end

  # Use update/3 for atomic updates
  def handle_event("increment_likes", %{"post_id" => id}, socket) do
    {:noreply,
     update(socket, :posts, fn posts ->
       Enum.map(posts, fn post ->
         if post.id == id do
           %{post | likes: post.likes + 1}            # => Update only this post
         else
           post                                       # => Keep unchanged
         end
       end)
     end)}
  end

  # Render only changed sections
  def render(assigns) do                              # => Render callback: generates HTML
                                                       # => Called whenever assigns change
    ~H"""                                             # => HEEx template (HTML + Elixir)
                                                       # => Compiled to efficient diff tracking
    <div>
      <.header time={@time} />                        # => Separate component
                                                       # => Function component: header/1
                                                       # => Only re-renders when @time changes
                                                       # => Passes time={@time} as assign

      <div id="posts" phx-update="stream">            # => Stream container for efficient list rendering
                                                       # => phx-update="stream": only patches changed items
                                                       # => No full list re-render on updates
        <%= for {dom_id, post} <- @streams.posts do %>
                                                       # => Iterates over stream items
                                                       # => dom_id: unique DOM ID per item
                                                       # => post: actual post struct
          <.post_card id={dom_id} post={post} />      # => Component per post
                                                       # => id={dom_id}: stable DOM ID for diffing
                                                       # => Only changed posts re-render
        <% end %>
      </div>

      <.footer />                                     # => Static component
                                                       # => Never re-renders (no dynamic assigns)
    </div>
    """
  end                                                 # => End render/1 function

  # Function components for granular updates
  def header(assigns) do                              # => Function component: header/1
                                                       # => Isolated template for header section
    ~H"""                                             # => HEEx template
                                                       # => LiveView diffs only this section
    <header>
      <time><%= @time %></time>                       # => Only this re-renders
                                                       # => When @time changes, only <time> updates
                                                       # => Rest of page unchanged (efficient)
    </header>
    """
  end                                                 # => End header/1 component
end                                                   # => End MyAppWeb.DashboardLive module
```

**Key Takeaway**: Use assign_new/3 for expensive one-time computation. Prefer streams over lists for collections. Debounce rapid events. Use push_event for client-side updates. Break templates into small function components.

**Why It Matters**: Templates with HEEx enable component-based UI development with compile-time validation. This catches HTML errors during compilation rather than runtime, improving reliability.

### Example 75: Production Debugging with Observer and LiveDashboard

Debug production issues with Observer, LiveDashboard, and remote IEx.

```elixir
# Connect to remote production node
# On local machine:
# iex --name debug@127.0.0.1 --cookie production_cookie

# In IEx session:
Node.connect(:"my_app@production-server.com")        # => Connect to prod node
Node.list()                                           # => ["my_app@production-server.com"]

# Inspect running processes
Process.list()                                        # => All Erlang processes
  |> Enum.filter(fn pid ->
    case Process.info(pid, :current_function) do
      {:current_function, {module, _fun, _arity}} ->
        String.starts_with?(to_string(module), "Elixir.MyApp")
      _ -> false
    end
  end)
  |> Enum.map(fn pid ->
    {pid, Process.info(pid, [:memory, :message_queue_len, :current_function])}
  end)
  # => [{#PID<0.123.0>, [memory: 12345, message_queue_len: 0, ...]}, ...]

# Find process by name
pid = Process.whereis(MyApp.Worker)                   # => #PID<0.456.0>
Process.info(pid)                                     # => Full process info

# Check process mailbox
Process.info(pid, :message_queue_len)                 # => {message_queue_len, 1000}
# => If high, process is backed up

# Inspect state of GenServer
:sys.get_state(pid)                                   # => Current state map
# => %{pending: [], processing: 10, errors: []}

# Trace function calls
:dbg.tracer()                                         # => Start tracer
:dbg.p(:all, :c)                                      # => Trace all processes
:dbg.tp(MyApp.Blog, :create_post, 2, [])              # => Trace function
# => Prints every call to MyApp.Blog.create_post/2

# LiveDashboard metrics
defmodule MyAppWeb.Telemetry do
  def metrics do
    [
      # VM Metrics
      last_value("vm.memory.total", unit: {:byte, :megabyte}),
      last_value("vm.total_run_queue_lengths.total"),  # => Scheduler queue
      last_value("vm.total_run_queue_lengths.cpu"),

      # Database Metrics
      summary("my_app.repo.query.total_time",
        unit: {:native, :millisecond},
        tags: [:source, :command]                     # => Group by table/command
      ),
      counter("my_app.repo.query.count"),             # => Total queries

      # Phoenix Metrics
      summary("phoenix.router_dispatch.stop.duration",
        tags: [:route],
        unit: {:native, :millisecond}
      ),
      counter("phoenix.router_dispatch.stop.count",
        tags: [:route, :status]                       # => Requests by route/status
      ),

      # Custom Business Metrics
      counter("my_app.orders.created.count"),         # => Business events
      last_value("my_app.users.active.count"),
      distribution("my_app.checkout.duration",
        buckets: [100, 500, 1000, 5000]               # => Histogram buckets
      )
    ]
  end
end

# Memory leak detection
defmodule MyApp.MemoryMonitor do
  use GenServer
  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    :timer.send_interval(60_000, :check_memory)       # => Check every minute
    {:ok, %{baseline: get_memory()}}
  end

  def handle_info(:check_memory, state) do
    current = get_memory()
    diff = current - state.baseline

    if diff > 100_000_000 do                          # => >100MB increase
      Logger.warning("Memory leak detected",
        baseline_mb: div(state.baseline, 1_000_000),
        current_mb: div(current, 1_000_000),
        diff_mb: div(diff, 1_000_000)
      )

      # Get top memory consumers
      top_processes = Process.list()
        |> Enum.map(fn pid ->
          {pid, Process.info(pid, :memory)}
        end)
        |> Enum.sort_by(fn {_pid, {:memory, mem}} -> mem end, :desc)
        |> Enum.take(10)                              # => Top 10 processes

      Logger.warning("Top memory consumers", processes: top_processes)
    end

    {:noreply, state}
  end

  defp get_memory do
    :erlang.memory(:total)                            # => Total VM memory
  end
end
```

**Key Takeaway**: Use remote IEx to connect to production. Inspect process state with :sys.get_state/1. Monitor memory leaks. Use LiveDashboard for real-time metrics. Trace function calls with :dbg.

**Why It Matters**: Process-based architecture enables horizontal scaling and fault isolation. Understanding OTP processes is key to building highly available systems.

### Example 76: Security Best Practices

Implement security headers, CSRF protection, and input sanitization.

```elixir
# Security headers in endpoint
defmodule MyAppWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :my_app

  plug Plug.Static, at: "/", from: :my_app

  # Security headers
  plug :put_secure_headers

  defp put_secure_headers(conn, _opts) do
    conn
    |> put_resp_header("x-frame-options", "DENY")     # => Prevent clickjacking
    |> put_resp_header("x-content-type-options", "nosniff")  # => Prevent MIME sniffing
    |> put_resp_header("x-xss-protection", "1; mode=block")  # => XSS protection
    |> put_resp_header("strict-transport-security",
         "max-age=31536000; includeSubDomains")       # => Force HTTPS
    |> put_resp_header("content-security-policy",
         "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'")
         # => CSP policy
    |> put_resp_header("referrer-policy", "strict-origin-when-cross-origin")
  end

  plug Plug.RequestId                                 # => Add request ID header
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Jason

  plug Plug.MethodOverride                            # => Support _method param
  plug Plug.Head                                      # => Handle HEAD requests
  plug Plug.Session, @session_options                 # => Encrypted sessions

  plug MyAppWeb.Router
end

# CSRF protection (automatic in Phoenix)
defmodule MyAppWeb.Router do
  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {MyAppWeb.Layouts, :root}
    plug :protect_from_forgery                        # => CSRF token validation
    plug :put_secure_browser_headers
  end

  # API doesn't use CSRF (use JWT instead)
  pipeline :api do
    plug :accepts, ["json"]
    # No CSRF protection for stateless API
  end
end

# Input sanitization for user content
defmodule MyApp.Content do
  @doc "Sanitize HTML to prevent XSS"
  def sanitize_html(html) do
    HtmlSanitizeEx.basic_html(html)                   # => Strip dangerous tags
    # => Allows: <p>, <br>, <strong>, <em>, <a>, etc.
    # => Removes: <script>, <iframe>, onclick, etc.
  end

  def sanitize_user_post(params) do
    params
    |> Map.update("body", "", &sanitize_html/1)       # => Clean HTML content
    |> Map.update("title", "", &String.trim/1)        # => Trim whitespace
  end
end

# SQL injection prevention (automatic with Ecto)
# ❌ NEVER do this:
def search_posts_unsafe(term) do
  query = "SELECT * FROM posts WHERE title LIKE '%#{term}%'"  # => SQL INJECTION!
  Ecto.Adapters.SQL.query!(Repo, query, [])
end

# ✅ Always use parameterized queries:
def search_posts_safe(term) do
  from p in Post,
    where: like(p.title, ^"%#{term}%")                # => Parameter binding
  |> Repo.all()
  # => Generates: SELECT * FROM posts WHERE title LIKE $1
  # => Binds: ["%search%"]
end

# Rate limiting (prevent brute force)
defmodule MyAppWeb.Plugs.RateLimitLogin do
  def init(opts), do: opts

  def call(conn, _opts) do
    ip = get_ip(conn)                                 # => Client IP
    key = "login_attempts:#{ip}"

    case MyApp.Cache.get(key) do
      nil ->
        MyApp.Cache.set(key, 1, ttl: 300)             # => First attempt, 5min window
        conn

      attempts when attempts < 5 ->
        MyApp.Cache.incr(key)                         # => Increment counter
        conn

      _ ->
        conn
        |> put_status(:too_many_requests)             # => 429
        |> Phoenix.Controller.json(%{error: "Too many login attempts"})
        |> halt()                                     # => Block request
    end
  end

  defp get_ip(conn) do
    conn.remote_ip |> Tuple.to_list() |> Enum.join(".")
  end
end

# Secure password hashing (automatic with phx.gen.auth)
defmodule MyApp.Accounts.User do
  def registration_changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :password])
    |> validate_email()
    |> validate_password()
    |> hash_password()                                # => Bcrypt with salt
  end

  defp hash_password(changeset) do
    password = get_change(changeset, :password)

    if password && changeset.valid? do
      changeset
      |> put_change(:password_hash, Bcrypt.hash_pwd_salt(password))
      |> delete_change(:password)                     # => Don't store plaintext
    else
      changeset
    end
  end
end
```

**Key Takeaway**: Set security headers (CSP, HSTS, X-Frame-Options). CSRF tokens automatic in Phoenix. Sanitize user HTML with HtmlSanitizeEx. Always use Ecto for queries (prevents SQL injection). Rate limit login attempts.

**Why It Matters**: Ecto provides type-safe database interactions with compile-time query validation. This prevents SQL injection and catches query errors before deployment.

### Example 77: WebSocket Connection Pooling

Optimize WebSocket connections with connection pooling and load distribution.

```elixir
# Channel with connection limits
defmodule MyAppWeb.UserSocket do
  use Phoenix.Socket

  # Limit concurrent connections per user
  def connect(%{"token" => token}, socket, _connect_info) do
    case verify_token(token) do
      {:ok, user_id} ->
        # Check connection limit
        case check_connection_limit(user_id) do
          :ok ->
            {:ok, assign(socket, :user_id, user_id)}  # => Allow connection

          {:error, :limit_exceeded} ->
            :error                                    # => Reject connection
        end

      {:error, _} ->
        :error
    end
  end

  defp check_connection_limit(user_id) do
    count = Phoenix.Tracker.list(MyApp.Presence, "user:#{user_id}")
      |> Enum.count()                                 # => Count current connections

    if count < 5 do                                   # => Max 5 connections per user
      :ok
    else
      {:error, :limit_exceeded}
    end
  end

  def id(socket), do: "user_socket:#{socket.assigns.user_id}"

  channel "room:*", MyAppWeb.RoomChannel
  channel "user:*", MyAppWeb.UserChannel
end

# Connection pooling for external services
defmodule MyApp.External.APIClient do                # => Defines HTTP client with connection pooling
                                                       # => Reuses connections for better performance
  use GenServer                                       # => Imports GenServer behavior
                                                       # => Manages connection pool lifecycle

  def start_link(opts) do                             # => Public function: start_link/1
                                                       # => Called by supervisor to start process
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
                                                       # => Starts GenServer
                                                       # => Registers with module name
                                                       # => Returns {:ok, pid}
  end                                                 # => End start_link/1 function

  def init(_opts) do                                  # => Initializes GenServer
                                                       # => Sets up HTTP connection pool
    # Create connection pool
    {:ok, pool} = :hackney_pool.child_spec(:api_pool, [
                                                       # => Creates Hackney connection pool
                                                       # => Pool name: :api_pool
      timeout: 15_000,                                # => Connection timeout
                                                       # => Max 15 seconds to establish connection
                                                       # => Prevents hanging on slow servers
      max_connections: 100                            # => Pool size
                                                       # => Maximum 100 concurrent connections
                                                       # => Reuses idle connections
    ])                                                # => Returns pool child_spec

    {:ok, %{pool: pool}}                              # => Returns GenServer initial state
                                                       # => Stores pool reference
  end                                                 # => End init/1 callback

  def get(path) do                                    # => Public API: get/1
                                                       # => Makes HTTP GET request using pool
    url = "https://api.example.com" <> path           # => Builds full URL
                                                       # => E.g., path="/users" → "https://api.example.com/users"
    headers = [{"Authorization", "Bearer #{api_token()}"}]
                                                       # => Sets request headers
                                                       # => Authorization header with Bearer token
                                                       # => api_token() fetches from config/env

    case :hackney.get(url, headers, "", [pool: :api_pool]) do
                                                       # => Makes HTTP GET request
                                                       # => Uses :api_pool for connection reuse
                                                       # => Returns {:ok, status, headers, client_ref} or {:error, reason}
      {:ok, 200, _headers, client_ref} ->             # => Success case: HTTP 200 OK
                                                       # => client_ref: reference to read response body
        {:ok, body} = :hackney.body(client_ref)      # => Read response
                                                       # => Reads full response body
                                                       # => body is binary string (JSON)
        Jason.decode(body)                            # => Parse JSON
                                                       # => Converts JSON string to Elixir map
                                                       # => Returns {:ok, map} or {:error, reason}

      {:ok, status, _headers, _client_ref} ->         # => Non-200 status code
                                                       # => E.g., 404, 500, etc.
        {:error, {:http_error, status}}               # => Returns error tuple with status
                                                       # => E.g., {:error, {:http_error, 404}}

      {:error, reason} ->                             # => Network/connection error
                                                       # => E.g., timeout, connection refused
        {:error, reason}                              # => Returns error reason
                                                       # => E.g., {:error, :timeout}
    end                                               # => End case statement
  end                                                 # => End get/1 function
end                                                   # => End MyApp.External.APIClient module

# Distribute WebSocket connections across nodes
defmodule MyAppWeb.Presence do
  use Phoenix.Presence,
    otp_app: :my_app,
    pubsub_server: MyApp.PubSub

  # Track connections across cluster
  def track_connection(socket) do
    track(socket, "connections", socket.id, %{
      node: node(),                                   # => Which Erlang node
      joined_at: System.system_time(:second)
    })
  end

  # Get connection count across all nodes
  def connection_count do
    list("connections")                               # => All tracked connections
    |> Enum.count()
  end

  # Get connections per node
  def connections_by_node do
    list("connections")
    |> Enum.group_by(fn {_id, %{metas: [meta | _]}} ->
      meta.node                                       # => Group by node name
    end)
    |> Enum.map(fn {node, conns} ->
      {node, Enum.count(conns)}                       # => {node, count}
    end)
    |> Enum.into(%{})
  end
end
```

**Key Takeaway**: Limit connections per user to prevent abuse. Use Hackney pool for HTTP connection pooling. Track WebSocket connections across cluster with Presence. Distribute load across multiple nodes.

**Why It Matters**: Phoenix sockets enable efficient bidirectional communication for real-time features. This is essential for chat, notifications, and collaborative editing applications.

### Example 78: GraphQL API with Absinthe

Build GraphQL API for flexible client queries.

```elixir
# mix.exs
defp deps do
  [
    {:absinthe, "~> 1.7"},                            # => GraphQL implementation
    {:absinthe_plug, "~> 1.5"}                        # => Phoenix integration
  ]
end

# GraphQL schema
defmodule MyAppWeb.Schema do
  use Absinthe.Schema
  import_types MyAppWeb.Schema.ContentTypes

  query do
    @desc "Get all posts"
    field :posts, list_of(:post) do
      arg :limit, :integer, default_value: 10        # => Optional limit
      arg :offset, :integer, default_value: 0
      resolve &MyAppWeb.Resolvers.Content.list_posts/3
    end

    @desc "Get post by ID"
    field :post, :post do
      arg :id, non_null(:id)                          # => Required ID
      resolve &MyAppWeb.Resolvers.Content.get_post/3
    end

    @desc "Search posts"
    field :search_posts, list_of(:post) do
      arg :query, non_null(:string)
      resolve &MyAppWeb.Resolvers.Content.search_posts/3
    end
  end

  mutation do
    @desc "Create a post"
    field :create_post, :post do
      arg :title, non_null(:string)
      arg :body, non_null(:string)
      arg :tags, list_of(:string)
      resolve &MyAppWeb.Resolvers.Content.create_post/3
    end

    @desc "Update a post"
    field :update_post, :post do
      arg :id, non_null(:id)
      arg :title, :string                             # => Optional fields
      arg :body, :string
      resolve &MyAppWeb.Resolvers.Content.update_post/3
    end
  end

  subscription do
    @desc "Subscribe to new posts"
    field :post_created, :post do
      config fn _args, _context ->
        {:ok, topic: "posts"}                         # => PubSub topic
      end

      trigger :create_post, topic: fn _post ->
        ["posts"]                                     # => Trigger on mutation
      end
    end
  end
end

# Object types
defmodule MyAppWeb.Schema.ContentTypes do
  use Absinthe.Schema.Notation

  object :post do
    field :id, :id                                    # => Post ID
    field :title, :string
    field :body, :string
    field :inserted_at, :string
    field :author, :user, resolve: &get_author/3     # => Nested resolver
    field :comments, list_of(:comment) do
      resolve &get_comments/3                         # => Load comments
    end
  end

  object :user do
    field :id, :id
    field :name, :string
    field :email, :string
  end

  object :comment do
    field :id, :id
    field :body, :string
    field :author, :user
  end

  defp get_author(post, _args, _resolution) do
    {:ok, MyApp.Accounts.get_user!(post.author_id)}   # => Load author
  end

  defp get_comments(post, _args, _resolution) do
    comments = MyApp.Blog.list_comments(post.id)
    {:ok, comments}                                   # => Load comments
  end
end

# Resolvers
defmodule MyAppWeb.Resolvers.Content do
  def list_posts(_parent, args, _resolution) do
    posts = MyApp.Blog.list_posts(args)               # => Pass limit/offset
    {:ok, posts}
  end

  def get_post(_parent, %{id: id}, _resolution) do
    case MyApp.Blog.get_post(id) do
      nil -> {:error, "Post not found"}
      post -> {:ok, post}
    end
  end

  def create_post(_parent, args, %{context: %{current_user: user}}) do
    case MyApp.Blog.create_post(Map.put(args, :author_id, user.id)) do
      {:ok, post} ->
        Absinthe.Subscription.publish(
          MyAppWeb.Endpoint,
          post,
          post_created: "posts"                       # => Trigger subscription
        )
        {:ok, post}

      {:error, changeset} ->
        {:error, changeset}                           # => Return errors
    end
  end
end

# Router
defmodule MyAppWeb.Router do                          # => Defines Phoenix router
                                                       # => Configures GraphQL endpoints
  scope "/api" do                                     # => API scope under /api path
                                                       # => All routes prefixed with /api
    pipe_through :api                                 # => Uses API pipeline
                                                       # => Accepts JSON, no session/CSRF

    forward "/graphql", Absinthe.Plug,                # => GraphQL query endpoint
                                                       # => POST requests to /api/graphql
      schema: MyAppWeb.Schema                         # => GraphQL endpoint
                                                       # => Uses schema defined above
                                                       # => Handles queries, mutations, subscriptions

    forward "/graphiql", Absinthe.Plug.GraphiQL,      # => GraphQL IDE endpoint
                                                       # => Browser-based query explorer
      schema: MyAppWeb.Schema,                        # => Uses same schema
                                                       # => Access at /api/graphiql in browser
      interface: :playground                          # => GraphQL IDE
                                                       # => Interactive query editor
                                                       # => Shows schema documentation
                                                       # => Only enable in dev/staging!
  end                                                 # => End /api scope
end                                                   # => End router

# Example queries:
# query {                                             # => GraphQL query operation
#   posts(limit: 5) {                                 # => Fetches up to 5 posts
#     id                                              # => Request post ID
#     title                                           # => Request post title
#     author { name }                                 # => Nested query: author's name
#     comments { body }                               # => Nested query: all comment bodies
#   }                                                 # => Client chooses exact fields needed
# }                                                   # => No over-fetching or under-fetching
#
# mutation {                                          # => GraphQL mutation operation
#   createPost(title: "Hello", body: "World") {       # => Creates new post with arguments
#     id                                              # => Returns created post ID
#     title                                           # => Returns created post title
#   }                                                 # => Client specifies return fields
# }                                                   # => Mutation triggers subscription
```

**Key Takeaway**: Absinthe provides GraphQL for Phoenix. Define schema with queries, mutations, subscriptions. Clients request only needed fields. Use resolvers to load data. GraphiQL provides interactive API explorer.

**Why It Matters**: Schemas define data structure and types for validation and persistence. This provides a single source of truth for your domain models.

### Example 79: Event Sourcing Pattern

Implement event sourcing to store state changes as immutable events.

```elixir
# Event schema
defmodule MyApp.Events.Event do
  use Ecto.Schema

  schema "events" do
    field :aggregate_id, :string                      # => Entity ID (e.g., order_id)
    field :aggregate_type, :string                    # => Entity type (e.g., "Order")
    field :event_type, :string                        # => Event name
    field :payload, :map                              # => Event data (JSONB)
    field :version, :integer                          # => Event version
    field :inserted_at, :utc_datetime
  end
end

# Event store
defmodule MyApp.EventStore do
  alias MyApp.Events.Event
  alias MyApp.Repo

  def append_event(aggregate_id, aggregate_type, event_type, payload) do
    # Get current version
    current_version = get_current_version(aggregate_id)

    event = %Event{
      aggregate_id: aggregate_id,
      aggregate_type: aggregate_type,
      event_type: event_type,
      payload: payload,
      version: current_version + 1,                   # => Increment version
      inserted_at: DateTime.utc_now()
    }

    case Repo.insert(event) do
      {:ok, event} ->
        # Publish event for projections
        Phoenix.PubSub.broadcast(
          MyApp.PubSub,
          "events:#{aggregate_type}",
          {:event_appended, event}
        )
        {:ok, event}

      {:error, changeset} ->
        {:error, changeset}
    end
  end

  def get_events(aggregate_id) do
    from e in Event,
      where: e.aggregate_id == ^aggregate_id,
      order_by: [asc: e.version]                      # => Chronological order
    |> Repo.all()
  end

  defp get_current_version(aggregate_id) do
    from e in Event,
      where: e.aggregate_id == ^aggregate_id,
      select: max(e.version)
    |> Repo.one()
    |> case do
      nil -> 0                                        # => No events yet
      version -> version
    end
  end
end

# Order aggregate
defmodule MyApp.Orders.Order do                       # => Defines Order aggregate (domain model)
                                                       # => State rebuilt from events, not stored directly
  defstruct id: nil,                                  # => Order ID field
            items: [],                                # => List of order items
            status: :draft,                           # => Order status (:draft, :placed, :shipped)
            total: 0,                                 # => Total order amount
            version: 0                                # => Event version counter

  # Apply event to state
  def apply_event(order, %Event{event_type: "OrderCreated", payload: payload}) do
                                                       # => Handles OrderCreated event
                                                       # => Pattern matches event_type
    %{order |                                         # => Updates order struct
      id: payload["id"],                              # => Sets order ID from event
      items: [],                                      # => Starts with empty items list
      status: :draft,                                 # => Initial status is :draft
      total: 0                                        # => Initial total is 0
    }                                                 # => Returns updated order
  end                                                 # => End OrderCreated handler

  def apply_event(order, %Event{event_type: "ItemAdded", payload: payload}) do
                                                       # => Handles ItemAdded event
                                                       # => Adds item to order
    item = payload["item"]                            # => Extracts item from payload
                                                       # => E.g., %{"name" => "Widget", "price" => 10}
    %{order |                                         # => Updates order struct
      items: [item | order.items],                    # => Prepends item to items list
                                                       # => E.g., [new_item | existing_items]
      total: order.total + item["price"]              # => Update total
                                                       # => Adds item price to running total
                                                       # => E.g., total: 20 + 10 = 30
    }                                                 # => Returns updated order
  end                                                 # => End ItemAdded handler

  def apply_event(order, %Event{event_type: "OrderPlaced"}) do
                                                       # => Handles OrderPlaced event
                                                       # => Transitions order to placed status
    %{order | status: :placed}                        # => Change status
                                                       # => Updates status field to :placed
                                                       # => Order can no longer be modified
  end                                                 # => End OrderPlaced handler

  def apply_event(order, %Event{event_type: "OrderShipped", payload: payload}) do
                                                       # => Handles OrderShipped event
                                                       # => Records shipment details
    %{order |                                         # => Updates order struct
      status: :shipped,                               # => Changes status to :shipped
                                                       # => Final order status
      tracking_number: payload["tracking_number"]     # => Adds tracking number from payload
                                                       # => E.g., "TRACK123456"
    }                                                 # => Returns updated order
  end                                                 # => End OrderShipped handler

  # Rebuild state from events
  def from_events(events) do                          # => Public function: from_events/1
                                                       # => Replays events to rebuild order state
    Enum.reduce(events, %__MODULE__{}, &apply_event(&2, &1))
                                                       # => Reduces events list into order struct
                                                       # => Starts with empty %Order{}
                                                       # => Applies each event in order
    # => Replay all events to rebuild current state
                                                       # => E.g., [OrderCreated, ItemAdded, ItemAdded, OrderPlaced]
                                                       # => Builds final order: %Order{id: 1, items: [..], status: :placed, total: 50}
  end                                                 # => End from_events/1 function
end                                                   # => End MyApp.Orders.Order module

# Command handler
defmodule MyApp.Orders.Commands do
  alias MyApp.EventStore
  alias MyApp.Orders.Order

  def create_order(order_id) do
    EventStore.append_event(
      order_id,
      "Order",
      "OrderCreated",
      %{"id" => order_id}                             # => Event payload
    )
  end

  def add_item(order_id, item) do
    # Load current state
    events = EventStore.get_events(order_id)
    order = Order.from_events(events)                 # => Rebuild from events

    # Validate command
    if order.status == :draft do
      EventStore.append_event(
        order_id,
        "Order",
        "ItemAdded",
        %{"item" => item}                             # => Record event
      )
    else
      {:error, "Cannot add items to placed order"}
    end
  end

  def place_order(order_id) do
    events = EventStore.get_events(order_id)
    order = Order.from_events(events)

    if order.total > 0 do
      EventStore.append_event(
        order_id,
        "Order",
        "OrderPlaced",
        %{}                                           # => No additional data
      )
    else
      {:error, "Order must have items"}
    end
  end
end

# Read model projection (materialized view)
defmodule MyApp.Orders.Projection do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(_) do
    Phoenix.PubSub.subscribe(MyApp.PubSub, "events:Order")
    {:ok, %{}}
  end

  def handle_info({:event_appended, event}, state) do
    # Update read model based on event
    update_read_model(event)                          # => Update database view
    {:noreply, state}
  end

  defp update_read_model(%{event_type: "OrderPlaced", aggregate_id: order_id}) do
    # Update orders table for fast queries
    events = EventStore.get_events(order_id)
    order = Order.from_events(events)

    # Save snapshot for performance
    MyApp.Repo.insert_or_update(%MyApp.Orders.OrderSnapshot{
      id: order_id,
      items: order.items,
      total: order.total,
      status: order.status
    })
  end
end
```

**Key Takeaway**: Store all state changes as immutable events. Rebuild state by replaying events. Use projections for fast queries. Events provide complete audit trail. Supports time travel and debugging.

**Why It Matters**: Event-driven patterns decouple components and enable scalable architectures. Understanding events is key to building maintainable Phoenix applications.

### Example 80: Advanced Testing Strategies

Implement property-based testing and contract testing.

```elixir
# Property-based testing with StreamData
defmodule MyApp.BlogTest do                           # => Test module for Blog context
                                                       # => Uses property-based testing
  use ExUnit.Case                                     # => Imports ExUnit test macros
                                                       # => Provides test/2, assert/1, etc.
  use ExUnitProperties                                # => Property testing
                                                       # => Imports property/2, check/2, generators

  property "post title is always trimmed" do          # => Property test: invariant over many inputs
                                                       # => Tests 100 random inputs by default
    check all title <- string(:alphanumeric, min_length: 1, max_length: 100) do
                                                       # => Generator: random alphanumeric strings
                                                       # => min_length: 1, max_length: 100
                                                       # => Runs test with 100 different titles
      # Generate random titles
      params = %{"title" => "  #{title}  "}          # => Add whitespace
                                                       # => Surrounds title with spaces
                                                       # => E.g., "  HelloWorld  "
      changeset = Post.changeset(%Post{}, params)     # => Creates changeset
                                                       # => Validates and casts params

      if changeset.valid? do                          # => Only check if changeset valid
                                                       # => Skips invalid inputs
        trimmed = Ecto.Changeset.get_change(changeset, :title)
                                                       # => Gets :title change from changeset
                                                       # => Should be trimmed value
        assert String.trim(title) == trimmed          # => Property: always trimmed
                                                       # => Invariant: whitespace removed
                                                       # => Fails if any input violates property
      end                                             # => End if
    end                                               # => End check
  end                                                 # => End property test

  property "creating and deleting post is idempotent" do
                                                       # => Property test: operation reversibility
                                                       # => Tests create → delete → verify cycle
    check all title <- string(:printable) do          # => Generator: random printable strings
                                                       # => Includes special chars, unicode
      # Create post
      {:ok, post} = MyApp.Blog.create_post(%{title: title, body: "test"})
                                                       # => Creates post with random title
                                                       # => Should succeed for all inputs

      # Delete post
      {:ok, _} = MyApp.Blog.delete_post(post.id)      # => Deletes created post
                                                       # => Should succeed

      # Verify deleted
      assert MyApp.Blog.get_post(post.id) == nil      # => Property: delete works
                                                       # => Post no longer exists
                                                       # => Invariant: create+delete = nothing
    end                                               # => End check
  end                                                 # => End property test
end                                                   # => End MyApp.BlogTest module

# Contract testing for APIs
defmodule MyApp.API.ContractTest do
  use ExUnit.Case

  @api_contract %{
    "POST /api/posts" => %{
      request: %{
        "title" => "string",
        "body" => "string"
      },
      response: %{
        status: 201,
        body: %{
          "id" => "integer",
          "title" => "string",
          "body" => "string",
          "inserted_at" => "string"
        }
      }
    },
    "GET /api/posts/:id" => %{
      response: %{
        status: 200,
        body: %{
          "id" => "integer",
          "title" => "string"
        }
      }
    }
  }

  test "API contract compliance" do
    for {endpoint, contract} <- @api_contract do
      # Test request/response matches contract
      verify_contract(endpoint, contract)             # => Verify API shape
    end
  end

  defp verify_contract("POST /api/posts", contract) do
    conn = build_conn()
      |> post("/api/posts", contract.request)

    assert conn.status == contract.response.status
    body = Jason.decode!(conn.resp_body)

    # Verify response structure
    for {field, type} <- contract.response.body do
      assert Map.has_key?(body, field)                # => Field exists
      assert type_matches?(body[field], type)         # => Type correct
    end
  end

  defp type_matches?(value, "string"), do: is_binary(value)
  defp type_matches?(value, "integer"), do: is_integer(value)
end

# Integration tests with setup
defmodule MyAppWeb.PostLiveTest do                    # => LiveView integration test module
                                                       # => Tests full user interactions
  use MyAppWeb.ConnCase, async: true                  # => Uses conn test case
                                                       # => async: true enables parallel test execution
                                                       # => Faster test suite (isolated database)
  import Phoenix.LiveViewTest                         # => Imports LiveView test helpers
                                                       # => Provides live/2, render_click/2, etc.

  setup do                                            # => Setup callback: runs before each test
                                                       # => Creates test data
    # Setup test data
    user = insert(:user)                              # => Factory
                                                       # => Creates test user with factory
                                                       # => Uses ExMachina or similar
    posts = insert_list(3, :post, author: user)       # => Create 3 posts
                                                       # => Creates 3 posts linked to user
                                                       # => insert_list/3: factory helper

    {:ok, user: user, posts: posts}                   # => Return to test
                                                       # => Makes user and posts available in test context
                                                       # => Tests receive as %{user: user, posts: posts}
  end                                                 # => End setup callback

  test "displays posts", %{conn: conn, posts: posts} do
                                                       # => Test function
                                                       # => Receives conn and posts from setup
    {:ok, view, html} = live(conn, "/posts")          # => Mounts LiveView at /posts
                                                       # => view: LiveView test process
                                                       # => html: initial rendered HTML

    # Assert all posts shown
    for post <- posts do                              # => Iterates over test posts
                                                       # => Checks each post rendered
      assert html =~ post.title                       # => Each title present
                                                       # => =~ checks string contains title
                                                       # => Fails if any post missing
    end                                               # => End for loop

    # Test interaction
    view                                              # => LiveView test process
    |> element("button", "Load More")                 # => Finds <button>Load More</button>
                                                       # => Selects element to interact with
    |> render_click()                                 # => Click button
                                                       # => Triggers phx-click event
                                                       # => Simulates user click

    assert has_element?(view, "[data-role=post]", count: 6)  # => 6 posts now
                                                       # => Checks for 6 elements with data-role="post"
                                                       # => Verifies "Load More" added 3 more posts
  end                                                 # => End test

  test "creates post", %{conn: conn} do               # => Test post creation flow
                                                       # => Receives conn from setup
    {:ok, view, _html} = live(conn, "/posts/new")     # => Mounts LiveView at /posts/new
                                                       # => Opens post creation form

    # Fill form
    view                                              # => LiveView test process
    |> form("#post-form", post: %{title: "", body: ""})
                                                       # => Selects form by ID
                                                       # => Sets form values (empty for validation test)
    |> render_change()                                # => Trigger validation
                                                       # => Simulates phx-change event
                                                       # => Triggers LiveView validation

    assert has_element?(view, ".error")               # => Shows errors
                                                       # => Checks for error message elements
                                                       # => Validates required field errors shown

    # Submit valid form
    view                                              # => LiveView test process
    |> form("#post-form", post: %{title: "Test", body: "Content"})
                                                       # => Fills form with valid data
                                                       # => title: "Test", body: "Content"
    |> render_submit()                                # => Submit form
                                                       # => Triggers phx-submit event
                                                       # => Creates post in database

    assert_redirected(view, "/posts/1")               # => Redirects to show
                                                       # => Verifies redirect to post show page
                                                       # => Post ID is 1 (first post in test)
  end                                                 # => End test
end                                                   # => End MyAppWeb.PostLiveTest module

# Performance testing
defmodule MyApp.PerformanceTest do                    # => Performance test module
                                                       # => Validates performance requirements
  use ExUnit.Case                                     # => Imports ExUnit test macros
                                                       # => Provides test/2, assert/1, etc.

  @tag :performance                                   # => Tags test as performance test
                                                       # => Can run selectively: mix test --only performance
  test "list posts performs under 100ms" do           # => Performance test: query speed
                                                       # => Validates 100ms requirement
    # Create test data
    insert_list(100, :post)                           # => Creates 100 test posts
                                                       # => Realistic data volume

    {time_us, _result} = :timer.tc(fn ->              # => Measures execution time
                                                       # => :timer.tc/1 returns {microseconds, result}
      MyApp.Blog.list_posts()                         # => Measure execution time
                                                       # => Function being benchmarked
    end)                                              # => Returns {time_in_microseconds, function_result}

    time_ms = time_us / 1000                          # => Converts microseconds to milliseconds
                                                       # => 1000 μs = 1 ms
    assert time_ms < 100, "Query took #{time_ms}ms, expected < 100ms"
                                                       # => Fails if query takes ≥100ms
                                                       # => Error message shows actual time
  end                                                 # => End performance test

  @tag :performance                                   # => Tags as performance test
  test "N+1 query prevention" do                      # => Tests for N+1 query problem
                                                       # => Validates preloading works
    posts = insert_list(10, :post)                    # => Creates 10 test posts
                                                       # => Base data for N+1 test
    Enum.each(posts, fn post ->                       # => Iterates over posts
                                                       # => Creates comments for each
      insert_list(5, :comment, post: post)            # => Creates 5 comments per post
                                                       # => Total: 50 comments
    end)                                              # => End each

    # Count queries
    query_count = count_queries(fn ->                 # => Counts database queries executed
                                                       # => Uses telemetry to track
      MyApp.Blog.list_posts_with_comments()           # => Preload comments
                                                       # => Should use 2 queries (posts + comments)
                                                       # => BAD: 11 queries (1 + 10 for each post)
    end)                                              # => Returns total query count

    assert query_count == 2, "Expected 2 queries, got #{query_count}"
                                                       # => Fails if more than 2 queries
    # => 1 for posts, 1 for comments (not 11!)
                                                       # => Verifies preload prevents N+1
  end                                                 # => End N+1 test

  defp count_queries(fun) do                          # => Helper: counts queries during execution
                                                       # => Uses telemetry to intercept queries
    ref = make_ref()                                  # => Creates unique reference
                                                       # => Used for telemetry handler ID
    :telemetry.attach(                                # => Attaches telemetry handler
                                                       # => Intercepts query events
      "query-counter-#{ref}",                         # => Handler ID (unique per test)
                                                       # => E.g., "query-counter-#Reference<0.1.2.3>"
      [:my_app, :repo, :query],                       # => Event to listen for
                                                       # => Emitted by Ecto on each query
      fn _event, _measurements, _metadata, acc ->     # => Handler function
                                                       # => Called for each query
        send(self(), {:query, acc + 1})               # => Sends message with incremented count
                                                       # => acc starts at 0, increments each query
      end,
      0                                               # => Initial accumulator value
                                                       # => Starting count is 0
    )

    fun.()                                            # => Executes function being measured
                                                       # => Triggers database queries

    count = receive_queries(0)                        # => Collects query count from messages
                                                       # => Starts with count 0
    :telemetry.detach("query-counter-#{ref}")         # => Removes telemetry handler
                                                       # => Cleanup to prevent leaks
    count                                             # => Returns total query count
  end                                                 # => End count_queries/1 helper

  defp receive_queries(count) do                      # => Helper: receives query count messages
                                                       # => Recursively collects counts
    receive do                                        # => Waits for messages
      {:query, new_count} -> receive_queries(new_count)
                                                       # => Receives updated count
                                                       # => Recursively waits for more
    after
      100 -> count                                    # => No more queries
                                                       # => Timeout after 100ms
                                                       # => Returns final count
    end                                               # => End receive
  end                                                 # => End receive_queries/1 helper
end                                                   # => End MyApp.PerformanceTest module
```

**Key Takeaway**: Property-based testing validates invariants across random inputs. Contract testing ensures API stability. Integration tests verify full user flows. Performance tests catch regressions. Use async: true for parallel test execution.

**Why It Matters**: This Phoenix pattern is fundamental for building production web applications. Understanding this concept enables you to create robust, maintainable, and scalable applications.
