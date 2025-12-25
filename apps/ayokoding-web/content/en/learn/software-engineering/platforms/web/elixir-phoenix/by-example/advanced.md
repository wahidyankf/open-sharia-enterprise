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
defmodule MyApp.Transfers do
  def transfer_funds(from_account, to_account, amount) do
    result =
      Ecto.Multi.new()                                # => Start transaction pipeline
      |> Ecto.Multi.update(
        :debit,                                       # => Named operation
        Ecto.Changeset.change(from_account, balance: from_account.balance - amount)
      )  # => Deduct from source account
      |> Ecto.Multi.update(
        :credit,                                      # => Second operation
        Ecto.Changeset.change(to_account, balance: to_account.balance + amount)
      )  # => Add to destination account
      |> Ecto.Multi.insert(:transaction_log, %TransactionLog{
        from_id: from_account.id,
        to_id: to_account.id,
        amount: amount
      })  # => Record transaction history
      |> MyApp.Repo.transaction()                     # => Execute all or rollback

    case result do
      {:ok, %{debit: from, credit: to, transaction_log: log}} ->
        {:ok, from, to, log}                          # => All 3 operations committed
        # => Database changes persisted atomically

      {:error, :debit, changeset, _changes} ->
        {:error, "Debit failed", changeset}           # => Rolled back before credit
        # => No partial updates in database

      {:error, failed_op, changeset, _changes} ->
        {:error, "Operation failed: #{failed_op}", changeset}
        # => All changes discarded, database unchanged
    end
  end

  # Dependency between operations
  def complex_transaction do
    Ecto.Multi.new()
    |> Ecto.Multi.insert(:user, User.create_changeset(%{email: "user@example.com"}))
    |> Ecto.Multi.insert(:profile, fn %{user: user} ->
      Profile.create_changeset(user)
    end)
    |> Ecto.Multi.insert(:settings, fn %{user: user} ->
      Settings.default_changeset(user)
    end)
    |> MyApp.Repo.transaction()
  end
end
```

**Key Takeaway**: Ecto.Multi ensures all-or-nothing execution. Operations reference previous results with fn. Rollback happens automatically on any failure. Perfect for transfers, account creation, multi-step operations.

### Example 52: Database Constraints and Error Handling

Handle database constraint violations (unique, foreign key, etc.) gracefully in changesets.

```mermaid
%% Constraint violation handling
graph LR
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
defmodule MyApp.Accounts.User do
  schema "users" do
    field :email, :string
    field :username, :string
  end

  def registration_changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :username])
    |> unique_constraint(:email)                      # => Catch duplicate email
    |> unique_constraint(:username)                   # => Catch duplicate username
    |> assoc_constraint(:organization)                # => Validate FK exists
  end  # => Converts DB errors to changeset errors
end

# In your service
defmodule MyApp.Accounts do
  def register_user(attrs) do
    case %User{}
         |> User.registration_changeset(attrs)
         |> Repo.insert() do
      {:ok, user} ->
        {:ok, user}

      {:error, %Changeset{} = changeset} ->
        # Check for constraint violations
        error_fields = Enum.map(changeset.errors, fn {field, {msg, _}} -> {field, msg} end)

        if Enum.any?(error_fields, fn {field, _} -> field == :email end) do
          {:error, "Email already registered"}
        else
          {:error, "Registration failed"}
        end
    end
  end
end
```

**Key Takeaway**: unique_constraint/2 catches database uniqueness violations. assoc_constraint/2 catches foreign key errors. Changesets provide user-friendly error messages without SQL errors exposed.

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
defmodule MyApp.Content.Post do
  schema "posts" do
    field :title, :string
    many_to_many(:tags, MyApp.Tagging.Tag, join_through: "post_tags")
    many_to_many(:attachments, MyApp.Attachments.Attachment, join_through: "post_attachments")
  end
end

defmodule MyApp.Content.Comment do
  schema "comments" do
    field :body, :string
    many_to_many(:tags, MyApp.Tagging.Tag, join_through: "comment_tags")
  end
end

# Migration for join table
def change do
  create table(:post_tags) do
    add :post_id, references(:posts, on_delete: :delete_all)
    add :tag_id, references(:tags, on_delete: :delete_all)
    timestamps()
  end

  create unique_index(:post_tags, [:post_id, :tag_id])
end

# Usage
post = Post
  |> Repo.preload(:tags)
  |> Ecto.Changeset.change()
  |> put_assoc(:tags, tags)
  |> Repo.update()

# Query posts with specific tag
posts = from p in Post,
  join: t in assoc(p, :tags),
  where: t.slug == "featured"
```

**Key Takeaway**: many_to_many/3 with join_through creates flexible relationships. Use put_assoc/3 to update related records. Query across relationships with join.

### Example 54: Multi-Tenancy with Ecto Query Prefix

Isolate tenant data at the query level. Each query automatically scopes to tenant.

```elixir
defmodule MyApp.Accounts do
  def get_user(user_id, tenant_id) do
    from(u in User, where: u.id == ^user_id and u.tenant_id == ^tenant_id)
    |> Repo.one()
  end

  def create_user(attrs, tenant_id) do
    %User{}
    |> User.changeset(attrs |> Map.put("tenant_id", tenant_id))
    |> Repo.insert()
  end
end

# Or use query prefix for schema-per-tenant
defmodule MyApp.Repo do
  def for_tenant(tenant_id) do
    # All queries run with prefix filter
    Repo.put_dynamic_repo({__MODULE__, {tenant_id}})
  end
end

# Query scoped to tenant
User
|> where([u], u.tenant_id == ^tenant_id)
|> Repo.all()

# Or with dynamic query prefix
User
|> Repo.all(prefix: "tenant_#{tenant_id}")
```

**Key Takeaway**: Always filter by tenant_id in queries. Use scopes (functions that return queries) to prevent tenant leaks. Consider separate schemas per tenant for complete isolation.

### Example 55: PostgreSQL Advanced Features in Ecto

Leverage PostgreSQL-specific features: JSONB, arrays, full-text search, custom types.

```elixir
defmodule MyApp.Blog.Post do
  schema "posts" do
    field :title, :string
    field :metadata, :map              # => JSONB in PostgreSQL
    field :tags, {:array, :string}     # => Array type
    field :search_vector, :string      # => Full-text search
  end
end

# Migration
def change do
  create table(:posts) do
    add :title, :string
    add :metadata, :jsonb, default: "{}"
    add :tags, {:array, :string}, default: []
    add :search_vector, :tsvector

    timestamps()
  end

  # GIN index for JSONB performance
  create index(:posts, ["(metadata)"], using: :gin)
  # GIN index for full-text search
  create index(:posts, [:search_vector], using: :gin)
end

# Query JSONB
posts = from p in Post,
  where: fragment("? ->> ? = ?", p.metadata, "status", "published")

# Array operations
posts = from p in Post,
  where: fragment("? @> ?", p.tags, ^["elixir"])

# Full-text search
results = from p in Post,
  where: fragment("to_tsvector('english', ?) @@ plainto_tsquery('english', ?)",
    p.title, ^search_term),
  select: p
```

**Key Takeaway**: Use :map for JSONB, {:array, :string} for arrays. Full-text search with tsvector. Use fragment/2 for database-specific SQL. Index JSONB and tsvector for performance.

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

### Example 57: Caching Strategies

Cache expensive operations to reduce database load and improve response time.

```elixir
defmodule MyApp.CacheServer do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def get_cache(key) do
    GenServer.call(__MODULE__, {:get, key})
  end

  def set_cache(key, value, ttl_ms) do
    GenServer.cast(__MODULE__, {:set, key, value, ttl_ms})
  end

  @impl true
  def init(state) do
    {:ok, state}
  end

  @impl true
  def handle_call({:get, key}, _from, state) do
    case Map.get(state, key) do
      {value, expires_at} when expires_at > System.monotonic_time(:millisecond) ->
        {:reply, {:ok, value}, state}                 # => Cache hit, not expired

      _ ->
        {:reply, :not_found, state}                   # => Cache miss or expired
    end
  end  # => Synchronous call, blocks caller until reply

  @impl true
  def handle_cast({:set, key, value, ttl_ms}, state) do
    expires_at = System.monotonic_time(:millisecond) + ttl_ms  # => Calculate expiry
    {:noreply, Map.put(state, key, {value, expires_at})}
  end  # => Asynchronous, returns immediately without blocking
end

# Or use Cachex library
defmodule MyApp.Blog do
  def get_popular_posts do
    case Cachex.get(:blog_cache, "popular_posts") do
      {:ok, nil} ->
        posts = Post |> where([p], p.likes > 100) |> Repo.all()
        Cachex.put(:blog_cache, "popular_posts", posts, ttl: :timer.minutes(60))
        posts

      {:ok, posts} ->
        posts
    end
  end
end
```

**Key Takeaway**: Cache expensive queries with TTL (time-to-live). Invalidate cache when data changes. Use Cachex for distributed caching. Cache at controller or service layer.

### Example 58: Background Jobs with Oban

Execute long-running tasks asynchronously. Schedule recurring jobs.

```mermaid
%% Oban job processing flow
graph LR
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

### Example 59: Phoenix LiveDashboard for Metrics

Monitor your application in real-time with LiveDashboard. View requests, Ecto stats, processes.

```elixir
# mix.exs
defp deps do
  [
    {:phoenix_live_dashboard, "~> 0.7"},
    {:telemetry_metrics, "~> 0.6"},
    {:telemetry_poller, "~> 1.0"}
  ]
end

# lib/my_app_web/telemetry.ex
defmodule MyAppWeb.Telemetry do
  use Supervisor

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    children = [
      # Telemetry poller periodically collects metrics
      {:telemetry_poller, handlers: handle_metrics()},
      {Phoenix.LiveDashboard.TelemetryListener, []}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp handle_metrics do
    [
      # VM metrics
      {:process_count, unit: {:byte, :kilobyte}},
      {:memory, unit: {:byte, :kilobyte}},
      # Ecto metrics
      {MyApp.Repo, [:repo, :adapter, :status], :ok},
      {MyApp.Repo, [:repo, :adapter, :connection_error], :ok}
    ]
  end
end

# router.ex
import Phoenix.LiveDashboard.Router

scope "/" do
  pipe_through :browser
  live_dashboard "/dashboard"
end
```

**Key Takeaway**: Phoenix LiveDashboard shows real-time metrics. Monitor request performance, database connections, memory usage, processes. Access at /dashboard.

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

## Group 11: Production Deployment

### Example 61: Mix Releases for Production

Build self-contained release that runs without Elixir/Erlang installed. Configure runtime settings.

```elixir
# mix.exs
def project do
  [
    app: :my_app,
    version: "0.1.0",
    releases: [
      prod: [
        include_executables_for: [:unix],
        steps: [:assemble, :tar]
      ]
    ]
  ]
end

# config/runtime.exs - Loaded at runtime, not compile time
import Config

config :my_app, MyAppWeb.Endpoint,
  http: [ip: {0, 0, 0, 0}, port: String.to_integer(System.get_env("PORT") || "4000")],
  # => Bind to all interfaces (0.0.0.0) for Docker
  secret_key_base: System.fetch_env!("SECRET_KEY_BASE"),  # => Read from env var
  url: [host: System.get_env("PHX_HOST", "localhost"), port: 443, scheme: "https"]
  # => HTTPS for production

if config_env() == :prod do
  config :my_app, MyApp.Repo,
    url: System.fetch_env!("DATABASE_URL"),
    ssl: true,
    socket_options: [:inet6]
end

# Build release
# mix release

# Or with custom name
# mix release prod

# Run release
# _build/prod/rel/my_app/bin/my_app start

# Start in foreground
# _build/prod/rel/my_app/bin/my_app foreground
```

**Key Takeaway**: Mix.Release builds independent package. config/runtime.exs loads at runtime. Set environment variables for secrets. Releases don't require Elixir installation.

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
FROM elixir:1.14-alpine AS builder

WORKDIR /app

# Install build dependencies
RUN apk add --no-cache build-base git

# Copy source
COPY mix.exs mix.lock ./
RUN mix local.hex --force && mix local.rebar --force
RUN mix deps.get

COPY . .

# Compile release
RUN MIX_ENV=prod mix release

# Runtime stage
FROM alpine:latest

WORKDIR /app

# Install runtime dependencies
RUN apk add --no-cache openssl

# Copy release from builder
COPY --from=builder /app/_build/prod/rel/my_app ./

EXPOSE 4000

# Health check
HEALTHCHECK --interval=10s --timeout=3s --start-period=40s --retries=3 \
  CMD ["./bin/my_app", "eval", "MyApp.ready?"]

CMD ["./bin/my_app", "start"]
```

**Key Takeaway**: Multi-stage builds keep image small. Builder stage compiles, runtime stage runs. Use Alpine Linux for minimal footprint. Include health checks.

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

### Example 65: Environment Configuration Management

Separate configuration by environment. Use runtime configuration for secrets.

```elixir
# config/config.exs - Common to all environments
import Config

config :logger, :console,
  format: "$time $metadata[$level] $message\n"

# config/dev.exs
import Config

config :my_app, MyApp.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  port: 5432,
  database: "my_app_dev"

config :my_app, MyAppWeb.Endpoint,
  debug_errors: true,
  check_origin: false,
  watchers: [esbuild: {Esbuild, :install_and_run, []}]

# config/test.exs
import Config

config :my_app, MyApp.Repo,
  username: "postgres",
  password: "postgres",
  database: "my_app_test",
  pool: Ecto.Adapters.SQL.Sandbox

# config/runtime.exs - Loaded at runtime
import Config

database_url = System.get_env("DATABASE_URL") || "ecto://postgres:postgres@localhost/my_app_prod"

config :my_app, MyApp.Repo, url: database_url

secret_key_base = System.fetch_env!("SECRET_KEY_BASE")

config :my_app, MyAppWeb.Endpoint,
  secret_key_base: secret_key_base,
  url: [host: System.get_env("PHX_HOST", "localhost"), port: 443, scheme: "https"]
```

**Key Takeaway**: config/ files configure at compile time. config/runtime.exs loads at runtime (for secrets). Use environment variables for production secrets. Never commit secrets to git.

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
defmodule MyApp.Blog do
  require Logger

  def create_post(attrs) do
    Logger.info("Creating post", %{user_id: attrs["user_id"], title: attrs["title"]})

    case MyApp.Repo.insert(changeset) do
      {:ok, post} ->
        Logger.info("Post created successfully", %{post_id: post.id})
        {:ok, post}

      {:error, changeset} ->
        Logger.warning("Post creation failed", %{
          errors: changeset.errors,
          user_id: attrs["user_id"]
        })
        {:error, changeset}
    end
  rescue
    e ->
      Logger.error("Post creation crashed",
        error: inspect(e),
        stacktrace: Exception.format_stacktrace(__STACKTRACE__)
      )
      {:error, "Internal error"}
  end
end
```

**Key Takeaway**: Sentry captures production errors. Structured logging adds context. Use Logger.info/warn/error with metadata maps. Include request IDs for tracing.

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

### Example 69: WebSocket Load Balancing with Sticky Sessions

Route WebSocket connections to same server. Use load balancer affinity.

```elixir
# nginx.conf - Sticky sessions by IP
upstream my_app {
  server app1:4000;
  server app2:4000;
  server app3:4000;

  hash $remote_addr consistent;  # => Sticky by IP
}

server {
  listen 80;
  server_name my_app.com;

  location / {
    proxy_pass http://my_app;
    proxy_http_version 1.1;

    # WebSocket headers
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";

    # Preserve client IP
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Real-IP $remote_addr;
  }
}

# HAProxy.cfg - Also supports sticky sessions
backend phoenix_nodes
  balance roundrobin
  cookie SERVERID insert indirect nocache

  server node1 app1:4000 check cookie node1
  server node2 app2:4000 check cookie node2
  server node3 app3:4000 check cookie node3
```

**Key Takeaway**: WebSockets require persistent connections to same server. Use sticky sessions (by IP or cookie). Load balancer must preserve connection. Forward X-Forwarded-For headers.

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
#!/bin/bash

# Current version (blue)
BLUE_VERSION=$(aws ecs describe-services --cluster prod --services my-app | \
  jq -r '.services[0].taskDefinition' | \
  grep -oP 'my-app:\K[^:]+')

# New version (green)
GREEN_VERSION=$(git describe --tags --always)

# Build and deploy green version
mix release

docker build -t my-app:$GREEN_VERSION .
docker tag my-app:$GREEN_VERSION my-app:green
docker push my-app:green

# Register new task definition
aws ecs register-task-definition \
  --family my-app \
  --container-definitions "[{\"image\": \"my-app:$GREEN_VERSION\", ...}]"

# Update service to use green
aws ecs update-service \
  --cluster prod \
  --service my-app \
  --task-definition my-app:$GREEN_VERSION

# Wait for deployment
aws ecs wait services-stable \
  --cluster prod \
  --services my-app

# Test green version
curl http://green.my-app.com/health/ready

if [ $? -eq 0 ]; then
  # Switch traffic from blue to green
  aws route53 change-resource-record-sets \
    --hosted-zone-id Z123 \
    --change-batch "[{\"Action\": \"UPSERT\", \"ResourceRecordSet\": {\"Name\": \"my-app.com\", \"Type\": \"A\", \"TTL\": 60, \"ResourceRecords\": [{\"Value\": \"green-alb.aws.com\"}]}}]"

  echo "Deployment successful!"
else
  # Rollback to blue
  aws ecs update-service \
    --cluster prod \
    --service my-app \
    --task-definition my-app:$BLUE_VERSION
fi
```

**Key Takeaway**: Blue-green keeps current version running while deploying new version. Switch traffic only after verification. Can rollback instantly. Zero-downtime deployments.

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
defmodule MyApp.Shop.Product do
  use Ecto.Schema

  schema "products" do
    field :name, :string
    field :price, MyApp.EctoTypes.Money                # => Custom type
    timestamps()
  end

  def changeset(product, attrs) do
    product
    |> cast(attrs, [:name, :price])                   # => Cast price with custom type
    |> validate_required([:name, :price])
    |> validate_price()                               # => Domain validation
  end

  defp validate_price(changeset) do
    case get_change(changeset, :price) do
      %{amount: amount} when amount < 0 ->
        add_error(changeset, :price, "must be positive")  # => Business rule

      _ ->
        changeset
    end
  end
end

# Create product
%Product{}
|> Product.changeset(%{
  name: "Widget",
  price: %{"amount" => 19.99, "currency" => "USD"}    # => Input format
})
|> Repo.insert()
# => Stores: {"amount": 19.99, "currency": "USD"} in database
```

**Key Takeaway**: Custom Ecto types encapsulate domain logic. Implement cast/1, load/1, dump/1, and type/0. Use Decimal for money to avoid floating-point errors.

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
  def posts_with_rank do
    from p in Post,
      select: %{
        post: p,
        row_number: fragment("ROW_NUMBER() OVER (ORDER BY ? DESC)", p.inserted_at),
        # => Assign sequential number
        rank: fragment("RANK() OVER (ORDER BY ? DESC)", p.view_count)
        # => Rank by view count (ties get same rank)
      }
    |> Repo.all()
  end

  # Common Table Expressions (CTEs)
  def posts_with_comment_count do
    comments_cte = from c in Comment,
      group_by: c.post_id,
      select: %{post_id: c.post_id, count: count(c.id)}

    from p in Post,
      left_join: c in subquery(comments_cte), on: c.post_id == p.id,
      select: %{post: p, comment_count: coalesce(c.count, 0)}
    |> Repo.all()
    # => WITH comments AS (...) SELECT posts.*, COALESCE(comments.count, 0)
  end
end
```

**Key Takeaway**: Use fragment/1 for database-specific SQL. Supports full-text search, JSONB queries, window functions, and CTEs. Always use ^pinned parameters to prevent SQL injection.

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
  def render(assigns) do
    ~H"""
    <div>
      <.header time={@time} />                        # => Separate component

      <div id="posts" phx-update="stream">
        <%= for {dom_id, post} <- @streams.posts do %>
          <.post_card id={dom_id} post={post} />      # => Component per post
        <% end %>
      </div>

      <.footer />                                     # => Static component
    </div>
    """
  end

  # Function components for granular updates
  def header(assigns) do
    ~H"""
    <header>
      <time><%= @time %></time>                       # => Only this re-renders
    </header>
    """
  end
end
```

**Key Takeaway**: Use assign_new/3 for expensive one-time computation. Prefer streams over lists for collections. Debounce rapid events. Use push_event for client-side updates. Break templates into small function components.

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
defmodule MyApp.External.APIClient do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    # Create connection pool
    {:ok, pool} = :hackney_pool.child_spec(:api_pool, [
      timeout: 15_000,                                # => Connection timeout
      max_connections: 100                            # => Pool size
    ])

    {:ok, %{pool: pool}}
  end

  def get(path) do
    url = "https://api.example.com" <> path
    headers = [{"Authorization", "Bearer #{api_token()}"}]

    case :hackney.get(url, headers, "", [pool: :api_pool]) do
      {:ok, 200, _headers, client_ref} ->
        {:ok, body} = :hackney.body(client_ref)      # => Read response
        Jason.decode(body)                            # => Parse JSON

      {:ok, status, _headers, _client_ref} ->
        {:error, {:http_error, status}}

      {:error, reason} ->
        {:error, reason}
    end
  end
end

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
defmodule MyAppWeb.Router do
  scope "/api" do
    pipe_through :api

    forward "/graphql", Absinthe.Plug,
      schema: MyAppWeb.Schema                         # => GraphQL endpoint

    forward "/graphiql", Absinthe.Plug.GraphiQL,
      schema: MyAppWeb.Schema,
      interface: :playground                          # => GraphQL IDE
  end
end

# Example queries:
# query {
#   posts(limit: 5) {
#     id
#     title
#     author { name }
#     comments { body }
#   }
# }
#
# mutation {
#   createPost(title: "Hello", body: "World") {
#     id
#     title
#   }
# }
```

**Key Takeaway**: Absinthe provides GraphQL for Phoenix. Define schema with queries, mutations, subscriptions. Clients request only needed fields. Use resolvers to load data. GraphiQL provides interactive API explorer.

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
defmodule MyApp.Orders.Order do
  defstruct id: nil,
            items: [],
            status: :draft,
            total: 0,
            version: 0

  # Apply event to state
  def apply_event(order, %Event{event_type: "OrderCreated", payload: payload}) do
    %{order |
      id: payload["id"],
      items: [],
      status: :draft,
      total: 0
    }
  end

  def apply_event(order, %Event{event_type: "ItemAdded", payload: payload}) do
    item = payload["item"]
    %{order |
      items: [item | order.items],
      total: order.total + item["price"]              # => Update total
    }
  end

  def apply_event(order, %Event{event_type: "OrderPlaced"}) do
    %{order | status: :placed}                        # => Change status
  end

  def apply_event(order, %Event{event_type: "OrderShipped", payload: payload}) do
    %{order |
      status: :shipped,
      tracking_number: payload["tracking_number"]
    }
  end

  # Rebuild state from events
  def from_events(events) do
    Enum.reduce(events, %__MODULE__{}, &apply_event(&2, &1))
    # => Replay all events to rebuild current state
  end
end

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

### Example 80: Advanced Testing Strategies

Implement property-based testing and contract testing.

```elixir
# Property-based testing with StreamData
defmodule MyApp.BlogTest do
  use ExUnit.Case
  use ExUnitProperties                                # => Property testing

  property "post title is always trimmed" do
    check all title <- string(:alphanumeric, min_length: 1, max_length: 100) do
      # Generate random titles
      params = %{"title" => "  #{title}  "}          # => Add whitespace
      changeset = Post.changeset(%Post{}, params)

      if changeset.valid? do
        trimmed = Ecto.Changeset.get_change(changeset, :title)
        assert String.trim(title) == trimmed          # => Property: always trimmed
      end
    end
  end

  property "creating and deleting post is idempotent" do
    check all title <- string(:printable) do
      # Create post
      {:ok, post} = MyApp.Blog.create_post(%{title: title, body: "test"})

      # Delete post
      {:ok, _} = MyApp.Blog.delete_post(post.id)

      # Verify deleted
      assert MyApp.Blog.get_post(post.id) == nil      # => Property: delete works
    end
  end
end

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
defmodule MyAppWeb.PostLiveTest do
  use MyAppWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  setup do
    # Setup test data
    user = insert(:user)                              # => Factory
    posts = insert_list(3, :post, author: user)       # => Create 3 posts

    {:ok, user: user, posts: posts}                   # => Return to test
  end

  test "displays posts", %{conn: conn, posts: posts} do
    {:ok, view, html} = live(conn, "/posts")

    # Assert all posts shown
    for post <- posts do
      assert html =~ post.title                       # => Each title present
    end

    # Test interaction
    view
    |> element("button", "Load More")
    |> render_click()                                 # => Click button

    assert has_element?(view, "[data-role=post]", count: 6)  # => 6 posts now
  end

  test "creates post", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/posts/new")

    # Fill form
    view
    |> form("#post-form", post: %{title: "", body: ""})
    |> render_change()                                # => Trigger validation

    assert has_element?(view, ".error")               # => Shows errors

    # Submit valid form
    view
    |> form("#post-form", post: %{title: "Test", body: "Content"})
    |> render_submit()                                # => Submit form

    assert_redirected(view, "/posts/1")               # => Redirects to show
  end
end

# Performance testing
defmodule MyApp.PerformanceTest do
  use ExUnit.Case

  @tag :performance
  test "list posts performs under 100ms" do
    # Create test data
    insert_list(100, :post)

    {time_us, _result} = :timer.tc(fn ->
      MyApp.Blog.list_posts()                         # => Measure execution time
    end)

    time_ms = time_us / 1000
    assert time_ms < 100, "Query took #{time_ms}ms, expected < 100ms"
  end

  @tag :performance
  test "N+1 query prevention" do
    posts = insert_list(10, :post)
    Enum.each(posts, fn post ->
      insert_list(5, :comment, post: post)
    end)

    # Count queries
    query_count = count_queries(fn ->
      MyApp.Blog.list_posts_with_comments()           # => Preload comments
    end)

    assert query_count == 2, "Expected 2 queries, got #{query_count}"
    # => 1 for posts, 1 for comments (not 11!)
  end

  defp count_queries(fun) do
    ref = make_ref()
    :telemetry.attach(
      "query-counter-#{ref}",
      [:my_app, :repo, :query],
      fn _event, _measurements, _metadata, acc ->
        send(self(), {:query, acc + 1})
      end,
      0
    )

    fun.()

    count = receive_queries(0)
    :telemetry.detach("query-counter-#{ref}")
    count
  end

  defp receive_queries(count) do
    receive do
      {:query, new_count} -> receive_queries(new_count)
    after
      100 -> count                                    # => No more queries
    end
  end
end
```

**Key Takeaway**: Property-based testing validates invariants across random inputs. Contract testing ensures API stability. Integration tests verify full user flows. Performance tests catch regressions. Use async: true for parallel test execution.
