---
title: "Advanced"
weight: 11000003
date: 2025-12-24T00:00:00+07:00
draft: false
description: Master advanced Elixir Phoenix patterns through 20 annotated examples covering database optimization, performance, deployment, and resilience
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

### Example 41: Transactions and Concurrency with Ecto.Multi

Execute multiple database operations atomically. If any fails, all rollback.

```elixir
defmodule MyApp.Transfers do
  def transfer_funds(from_account, to_account, amount) do
    result =
      Ecto.Multi.new()
      |> Ecto.Multi.update(
        :debit,
        Ecto.Changeset.change(from_account, balance: from_account.balance - amount)
      )
      |> Ecto.Multi.update(
        :credit,
        Ecto.Changeset.change(to_account, balance: to_account.balance + amount)
      )
      |> Ecto.Multi.insert(:transaction_log, %TransactionLog{
        from_id: from_account.id,
        to_id: to_account.id,
        amount: amount
      })
      |> MyApp.Repo.transaction()

    case result do
      {:ok, %{debit: from, credit: to, transaction_log: log}} ->
        {:ok, from, to, log}  # => All succeeded together

      {:error, :debit, changeset, _changes} ->
        {:error, "Debit failed", changeset}  # => Transaction rolled back

      {:error, failed_op, changeset, _changes} ->
        {:error, "Operation failed: #{failed_op}", changeset}
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

### Example 42: Database Constraints and Error Handling

Handle database constraint violations (unique, foreign key, etc.) gracefully in changesets.

```elixir
defmodule MyApp.Accounts.User do
  schema "users" do
    field :email, :string
    field :username, :string
  end

  def registration_changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :username])
    |> unique_constraint(:email)     # => Handle database UNIQUE constraint
    |> unique_constraint(:username)
    |> assoc_constraint(:organization)  # => Handle foreign key
  end
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

### Example 43: Polymorphic Associations with many_to_many :through

Model flexible relationships where the same entity can have many different types of related entities.

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

### Example 44: Multi-Tenancy with Ecto Query Prefix

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

### Example 45: PostgreSQL Advanced Features in Ecto

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

### Example 46: Query Optimization - N+1 Prevention

Load related data efficiently to avoid N+1 queries where one query results in N additional queries.

```elixir
# ❌ N+1 Problem - 1 query + N queries
posts = Post |> Repo.all()
for post <- posts do
  author = Author |> where([a], a.id == ^post.author_id) |> Repo.one()
end
# Total: 1 + N queries

# ✅ Solution 1: Preload
posts = Post
  |> preload(:author)  # => Loads all authors in single query
  |> Repo.all()

# ✅ Solution 2: Join (for aggregations)
posts = from p in Post,
  join: a in assoc(p, :author),
  select: {p, a}

# ✅ Solution 3: Preload with nested associations
posts = Post
  |> preload([comments: :author])  # => Loads comments and their authors
  |> Repo.all()

# Query with EXPLAIN to see execution plan
results = Repo.all(from p in Post, preload: :author)
IO.inspect(Repo.explain(:all, Post))
```

**Key Takeaway**: Use preload/1 to eager-load associations. Use join for aggregations and filtering. Always check your queries with EXPLAIN. Avoid fetching in loops.

### Example 47: Caching Strategies

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
        {:reply, {:ok, value}, state}

      _ ->
        {:reply, :not_found, state}
    end
  end

  @impl true
  def handle_cast({:set, key, value, ttl_ms}, state) do
    expires_at = System.monotonic_time(:millisecond) + ttl_ms
    {:noreply, Map.put(state, key, {value, expires_at})}
  end
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

### Example 48: Background Jobs with Oban

Execute long-running tasks asynchronously. Schedule recurring jobs.

```elixir
# lib/my_app/workers/email_worker.ex
defmodule MyApp.Workers.EmailWorker do
  use Oban.Worker, queue: :default

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"user_id" => user_id}}) do
    user = MyApp.Accounts.get_user!(user_id)
    MyApp.Mailer.send_welcome_email(user)
    :ok  # => Job succeeded
  end
end

# In your controller/action
defmodule MyAppWeb.UserController do
  def create(conn, %{"user" => user_params}) do
    case MyApp.Accounts.create_user(user_params) do
      {:ok, user} ->
        # Queue background job
        %{"user_id" => user.id}
        |> MyApp.Workers.EmailWorker.new()
        |> Oban.insert()

        json(conn, user)

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

### Example 49: Phoenix LiveDashboard for Metrics

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

### Example 50: Custom Metrics with Telemetry

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

### Example 51: Mix Releases for Production

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
  secret_key_base: System.fetch_env!("SECRET_KEY_BASE"),
  url: [host: System.get_env("PHX_HOST", "localhost"), port: 443, scheme: "https"]

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

### Example 52: Docker Containerization with Multi-Stage Build

Build optimized Docker image with minimal size and security.

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

### Example 53: Health Checks for Kubernetes

Implement liveness and readiness endpoints for orchestration systems to manage pod lifecycle.

```elixir
# lib/my_app_web/controllers/health_controller.ex
defmodule MyAppWeb.HealthController do
  use MyAppWeb, :controller

  def readiness(conn, _params) do
    # Check if app is ready to serve traffic
    case check_database() do
      :ok ->
        json(conn, %{status: "ok"})

      :error ->
        conn
        |> put_status(:service_unavailable)
        |> json(%{status: "error", reason: "database_unavailable"})
    end
  end

  def liveness(conn, _params) do
    # Check if app is alive (should restart if not)
    json(conn, %{status: "ok"})
  end

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

### Example 54: Graceful Shutdown

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
    IO.puts("Shutting down gracefully...")
    :ok
  end
end

# In endpoint config
config :my_app, MyAppWeb.Endpoint,
  # Graceful shutdown timeout (milliseconds)
  shutdown: 25_000,
  # Give existing connections time to close
  drain_on_stop: true
```

**Key Takeaway**: prep_stop/1 gives app chance to drain requests. Set shutdown timeout. Complete in-flight work before terminating. Important for zero-downtime deployments.

### Example 55: Environment Configuration Management

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

### Example 56: Error Tracking and Structured Logging

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

### Example 57: Rate Limiting with Token Bucket

Prevent abuse by limiting requests per IP or user. Token bucket algorithm refills over time.

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
        {count, state}

      _ ->
        # Reset bucket
        reset_time = now + time_window_ms
        {0, Map.put(state, key, {0, reset_time})}
    end

    if requests < max_requests do
      {count, reset_time} = Map.get(state, key)
      new_state = Map.put(state, key, {count + 1, reset_time})
      {:reply, :ok, new_state}
    else
      {:reply, :rate_limited, state}
    end
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

### Example 58: Distributed Phoenix Clustering

Connect multiple Phoenix instances for distributed state and fault tolerance.

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
      strategy: Cluster.Strategy.Kubernetes.DNS,
      config: [
        service: "my_app-headless",
        namespace: "default"
      ]
    ]
  ]

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
end

# List connected nodes
Node.list()
```

**Key Takeaway**: libcluster connects nodes automatically. Distribute PubSub across cluster. All nodes share state. Provides fault tolerance.

### Example 59: WebSocket Load Balancing with Sticky Sessions

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

### Example 60: Blue-Green Deployment for Zero-Downtime Releases

Run two production environments. Switch traffic after verifying new version works.

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
