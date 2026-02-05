---
title: "Application Structure"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000006
description: "From manual application start to Mix Application behavior with supervision trees and config management"
tags: ["elixir", "application", "otp", "supervision", "mix", "config"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/supervisor-trees"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/otp-behaviors"
---

**How do you structure production Elixir applications?** This guide teaches the progression from manual application startup through OTP Application behavior to Mix-managed applications with supervision trees, configuration, and dependency ordering.

## Why It Matters

Application structure determines how your system starts, manages dependencies, and handles configuration. Production systems need:

- **Ordered startup** - Dependencies start before dependents (database before web server)
- **Supervision trees** - Automatic process restart on failure
- **Configuration management** - Environment-specific settings (dev, test, prod)
- **Graceful shutdown** - Clean resource cleanup on termination
- **Dependency coordination** - Multiple apps working together (umbrella projects)

Real-world scenarios requiring structured applications:

- **Financial services** - Database connection pools, payment processors, audit logging
- **E-commerce platforms** - Inventory systems, payment gateways, notification services
- **API backends** - Database, cache, HTTP server with proper startup order
- **Data pipelines** - Source connections, transformation workers, destination writers
- **Microservices** - Multiple coordinated services with shared configuration

Production question: Should you start processes manually, use Application behavior, or structure as Mix application? The answer depends on your supervision and configuration requirements.

## Standard Library - Manual Application Start

Elixir's standard library provides Application module for manual application lifecycle management.

### Application.start/2 - Manual Start

```elixir
# Starting application manually
{:ok, pid} = Application.start(:logger)      # => Starts Logger application
                                             # => Returns supervisor PID
                                             # => Type: {:ok, pid()}
                                             # => No supervision tree management

Application.start(:postgrex)                 # => Start database driver
                                             # => Must start dependencies first
                                             # => Manual ordering required
```

Manual start requires explicit dependency ordering, no automatic management.

### Application Callbacks - Minimal Structure

```elixir
# Basic application module
defmodule MyApp do
  use Application                            # => Imports Application behavior
                                             # => Requires start/2 and stop/1

  def start(_type, _args) do
    children = [
      Worker.Server                          # => List of child processes
                                             # => Type: [module() | {module(), term()}]
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
                                             # => Supervision strategy
                                             # => :one_for_one restarts failed child only

    Supervisor.start_link(children, opts)    # => Starts supervision tree
                                             # => Returns {:ok, pid}
                                             # => Type: {:ok, pid()} | {:error, term()}
  end

  def stop(_state) do
    :ok                                      # => Cleanup on shutdown
                                             # => Type: :ok
  end
end
```

Requires implementing `start/2` and `stop/1` callbacks. No configuration management.

### Complete Example - Manual Financial Service

```elixir
# Financial calculation service with manual start
defmodule FinanceApp do
  use Application

  def start(_type, _args) do
    children = [
      {Task.Supervisor, name: FinanceApp.TaskSupervisor}
                                             # => Task supervisor for calculations
                                             # => Type: {module(), keyword()}
    ]

    opts = [strategy: :one_for_one, name: FinanceApp.Supervisor]

    Supervisor.start_link(children, opts)
  end

  def stop(_state) do
    IO.puts("FinanceApp stopped")            # => Cleanup notification
    :ok
  end
end

# Manual start in iex
Application.start(FinanceApp)                # => Must start manually
                                             # => No automatic dependency handling
                                             # => No config management

# Usage
task = Task.Supervisor.async(
  FinanceApp.TaskSupervisor,
  fn -> calculate_invoice_total(items) end
)                                            # => Spawn supervised calculation task

result = Task.await(task)                    # => Wait for result
                                             # => Type: number()
```

Works for simple cases but lacks production features: no configuration, manual dependency ordering, no automatic start.

## Limitations of Manual Start

### No Supervision Tree Management

Manual start doesn't integrate with OTP supervision:

```elixir
# Problem: No automatic restart
Application.start(:my_app)                   # => Starts once
                                             # => If supervisor crashes, no restart
                                             # => No integration with system supervision
```

OTP expects applications to be supervised, manual start bypasses this.

### Manual Dependency Ordering

Must start applications in correct order:

```elixir
# Problem: Manual dependency chain
Application.start(:logger)                   # => Start logger first
Application.start(:postgrex)                 # => Then database driver
Application.start(:ecto)                     # => Then Ecto
Application.start(:my_app)                   # => Finally your app
                                             # => Fragile, error-prone
                                             # => Missing one breaks system
```

Forget one dependency, application fails to start.

### No Configuration Management

No built-in environment-specific configuration:

```elixir
# Problem: Hardcoded values
def start(_type, _args) do
  children = [
    {DatabasePool, host: "localhost", port: 5432}
                                             # => Hardcoded connection details
                                             # => Same for dev, test, prod
                                             # => No secrets management
  ]
  # ...
end
```

Production needs different settings per environment.

### No Application Environment

No standard way to store application configuration:

```elixir
# Problem: Custom config storage
def get_config do
  case System.get_env("DATABASE_URL") do     # => Manual environment variable reading
    nil -> raise "DATABASE_URL not set"      # => Error handling required
    url -> url                               # => No standardized approach
  end
end
```

Every application implements configuration differently.

## Production Framework - Mix Application

Mix provides application management with supervision, configuration, and dependency resolution.

### mix.exs - Application Definition

```elixir
# Define Mix application
defmodule FinanceApp.MixProject do
  use Mix.Project                            # => Mix project behavior

  def project do
    [
      app: :finance_app,                     # => Application name
                                             # => Type: atom()
      version: "0.1.0",                      # => Semantic version
      elixir: "~> 1.14",                     # => Elixir version requirement
      start_permanent: Mix.env() == :prod,   # => Permanent in production
                                             # => Supervisor restarts on failure
      deps: deps()                           # => Dependency list
    ]
  end

  def application do
    [
      extra_applications: [:logger],         # => Include Logger
                                             # => Type: [atom()]
      mod: {FinanceApp.Application, []}      # => Application callback module
                                             # => [] is init args
    ]
  end

  defp deps do
    [
      {:ecto_sql, "~> 3.10"},                # => Database library
      {:postgrex, ">= 0.0.0"},               # => PostgreSQL driver
      {:decimal, "~> 2.0"}                   # => Precise financial calculations
    ]                                        # => Type: [{atom(), String.t()}]
                                             # => Mix handles dependency ordering
  end
end
```

Mix automatically starts applications in dependency order.

### Application Module with Supervision

```elixir
# Application with supervision tree
defmodule FinanceApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      FinanceApp.Repo,                       # => Ecto repository
                                             # => Database connection pool
      {Task.Supervisor, name: FinanceApp.TaskSupervisor},
                                             # => Task supervisor for jobs
      {Registry, keys: :unique, name: FinanceApp.Registry},
                                             # => Process registry
      FinanceApp.InvoiceProcessor,           # => Invoice worker
      {FinanceApp.PaymentGateway, interval: 5000}
                                             # => Payment polling worker
                                             # => interval: Configuration
    ]                                        # => Type: [supervisor_child_spec()]

    opts = [strategy: :one_for_one, name: FinanceApp.Supervisor]

    Supervisor.start_link(children, opts)
  end

  @impl true
  def stop(_state) do
    FinanceApp.Repo.disconnect_all()         # => Close database connections
    :ok
  end
end
```

Supervision tree automatically restarts failed children.

### Configuration Management

```elixir
# config/config.exs - Base configuration
import Config

config :finance_app,
  currency_precision: 2,                     # => Decimal precision for money
                                             # => Type: non_neg_integer()
  vat_rate: Decimal.new("0.21")              # => 21% VAT
                                             # => Type: Decimal.t()

config :finance_app, FinanceApp.Repo,
  database: "finance_dev",                   # => Development database
  username: "postgres",                      # => Default credentials
  password: "postgres",
  hostname: "localhost",
  pool_size: 10                              # => Connection pool
                                             # => Type: pos_integer()

# Import environment-specific config
import_config "#{config_env()}.exs"          # => Loads dev.exs, test.exs, or prod.exs
                                             # => Overrides base config
```

```elixir
# config/prod.exs - Production overrides
import Config

config :finance_app, FinanceApp.Repo,
  url: System.get_env("DATABASE_URL"),       # => Production connection string
                                             # => Type: String.t() | nil
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "15"),
                                             # => Production pool size
                                             # => Type: pos_integer()
  ssl: true,                                 # => Require SSL
  ssl_opts: [
    verify: :verify_peer,                    # => Verify certificate
    cacerts: :public_key.cacerts_get()       # => System CA certificates
  ]

config :logger, level: :info                 # => Production log level
                                             # => Type: :debug | :info | :warn | :error
```

```elixir
# config/runtime.exs - Runtime configuration
import Config

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
    raise "DATABASE_URL not available"       # => Fail fast if missing
                                             # => Type: String.t()

  config :finance_app, FinanceApp.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
    ssl: true

  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
    raise "SECRET_KEY_BASE not available"
                                             # => Runtime secret
                                             # => Type: String.t()

  config :finance_app,
    secret_key_base: secret_key_base
end
```

Runtime config loads at application start, reads environment variables.

### Reading Configuration

```elixir
# Access application configuration
defmodule FinanceApp.Invoice do
  def calculate_total(items) do
    precision = Application.get_env(:finance_app, :currency_precision)
                                             # => Reads config value
                                             # => Returns 2
                                             # => Type: term()

    vat_rate = Application.get_env(:finance_app, :vat_rate)
                                             # => Returns Decimal.new("0.21")
                                             # => Type: term()

    subtotal = Enum.reduce(items, Decimal.new(0), fn item, acc ->
      Decimal.add(acc, Decimal.mult(item.price, item.quantity))
    end)                                     # => Sum line items
                                             # => Type: Decimal.t()

    tax = Decimal.mult(subtotal, vat_rate)   # => Calculate VAT
    total = Decimal.add(subtotal, tax)       # => Add tax to subtotal

    Decimal.round(total, precision)          # => Round to configured precision
                                             # => Type: Decimal.t()
  end
end
```

Configuration available throughout application via `Application.get_env/2`.

### Complete Example - Financial Application

```elixir
# Full production financial application

# mix.exs
defmodule FinanceApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :finance_app,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {FinanceApp.Application, []}
    ]
  end

  defp deps do
    [
      {:ecto_sql, "~> 3.10"},
      {:postgrex, ">= 0.0.0"},
      {:decimal, "~> 2.0"},
      {:phoenix_pubsub, "~> 2.1"}            # => PubSub for events
    ]
  end
end

# lib/finance_app/application.ex
defmodule FinanceApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      FinanceApp.Repo,                       # => Database
      {Phoenix.PubSub, name: FinanceApp.PubSub},
                                             # => Event bus
      {Registry, keys: :unique, name: FinanceApp.Registry},
                                             # => Process registry
      {Task.Supervisor, name: FinanceApp.TaskSupervisor},
                                             # => Background jobs
      FinanceApp.InvoiceWorker,              # => Invoice processor
      FinanceApp.PaymentWorker               # => Payment processor
    ]

    opts = [strategy: :one_for_one, name: FinanceApp.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def stop(_state) do
    FinanceApp.Repo.disconnect_all()
    :ok
  end
end

# lib/finance_app/invoice_worker.ex
defmodule FinanceApp.InvoiceWorker do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    schedule_work()                          # => Schedule first tick
    {:ok, %{}}
  end

  @impl true
  def handle_info(:work, state) do
    process_pending_invoices()               # => Process batch
    schedule_work()                          # => Schedule next tick
    {:noreply, state}
  end

  defp schedule_work do
    interval = Application.get_env(:finance_app, :invoice_interval, 60_000)
                                             # => Config with default
                                             # => Type: pos_integer()
    Process.send_after(self(), :work, interval)
  end

  defp process_pending_invoices do
    invoices = FinanceApp.Repo.all(FinanceApp.Invoice.pending())
                                             # => Query pending invoices
                                             # => Type: [FinanceApp.Invoice.t()]

    Enum.each(invoices, fn invoice ->
      Task.Supervisor.start_child(
        FinanceApp.TaskSupervisor,
        fn -> process_invoice(invoice) end
      )                                      # => Spawn supervised task per invoice
    end)
  end

  defp process_invoice(invoice) do
    total = FinanceApp.Invoice.calculate_total(invoice.items)
                                             # => Calculate total with VAT

    FinanceApp.Repo.update!(invoice, %{total: total, status: :calculated})
                                             # => Update database

    Phoenix.PubSub.broadcast(
      FinanceApp.PubSub,
      "invoices",
      {:invoice_calculated, invoice.id}
    )                                        # => Broadcast event
  end
end

# config/config.exs
import Config

config :finance_app,
  currency_precision: 2,
  vat_rate: Decimal.new("0.21"),
  invoice_interval: 60_000                   # => 1 minute

config :finance_app, FinanceApp.Repo,
  database: "finance_dev",
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  pool_size: 10

import_config "#{config_env()}.exs"

# config/prod.exs
import Config

config :finance_app,
  invoice_interval: 300_000                  # => 5 minutes in production

config :finance_app, FinanceApp.Repo,
  url: System.get_env("DATABASE_URL"),
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "20"),
  ssl: true

config :logger, level: :info

# Start application
# mix run --no-halt                          # => Starts with supervision
                                             # => Loads config automatically
                                             # => Handles dependencies
```

Full production setup with database, PubSub, workers, configuration, and supervision.

## Trade-offs

| Approach                     | Complexity | Config | Supervision | Use Case                    |
| ---------------------------- | ---------- | ------ | ----------- | --------------------------- |
| Manual `Application.start/2` | Low        | None   | Manual      | Simple scripts, experiments |
| Application behavior         | Medium     | Manual | Basic       | Small apps, libraries       |
| Mix application              | High       | Full   | Complete    | Production systems          |

**Manual start**: Quick for scripts, no production features.

**Application behavior**: Adds supervision, still manual config.

**Mix application**: Full production features, standard tooling.

## Best Practices

### Define Clear Supervision Strategy

Choose appropriate supervisor strategy:

```elixir
# :one_for_one - Independent children
children = [
  Worker1,                                   # => Restart only failed child
  Worker2,                                   # => Others unaffected
  Worker3
]
opts = [strategy: :one_for_one]

# :one_for_all - Dependent children
children = [
  Database,                                  # => If one fails, restart all
  Cache,                                     # => Ensures clean state
  ApiServer
]
opts = [strategy: :one_for_all]

# :rest_for_one - Sequential dependencies
children = [
  Database,                                  # => If Database fails, restart all
  Cache,                                     # => If Cache fails, restart Cache and ApiServer
  ApiServer                                  # => If ApiServer fails, restart only ApiServer
]
opts = [strategy: :rest_for_one]
```

Match strategy to failure requirements.

### Use Runtime Configuration for Secrets

Never hardcode secrets in config files:

```elixir
# config/runtime.exs - Runtime secrets
import Config

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
    raise "DATABASE_URL not available"

  config :finance_app, FinanceApp.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")
end
```

Read from environment at runtime, not compile time.

### Structure Config by Environment

Organize config files clearly:

```
config/
├── config.exs          # Base config, common settings
├── dev.exs             # Development overrides
├── test.exs            # Test overrides (fast settings)
├── prod.exs            # Production overrides
└── runtime.exs         # Runtime config (secrets, env vars)
```

Base config for defaults, environment-specific for overrides.

### Handle Graceful Shutdown

Clean up resources in `stop/1`:

```elixir
def stop(_state) do
  # Close database connections
  FinanceApp.Repo.disconnect_all()

  # Drain message queues
  GenServer.call(FinanceApp.Worker, :drain)

  # Flush logs
  Logger.flush()

  :ok
end
```

Ensure clean shutdown, no data loss.

### Use Umbrella Apps for Complex Systems

Structure large systems as multiple applications:

```
finance_system/
├── apps/
│   ├── finance_core/          # Core business logic
│   ├── finance_web/           # Web interface (Phoenix)
│   ├── finance_worker/        # Background jobs
│   └── finance_api/           # External API
├── config/
└── mix.exs
```

Each app has own supervision tree, configuration, dependencies.

### Document Supervision Tree

Add comments explaining supervision strategy:

```elixir
def start(_type, _args) do
  children = [
    # Database - Must start first
    FinanceApp.Repo,

    # PubSub - Used by all workers
    {Phoenix.PubSub, name: FinanceApp.PubSub},

    # Registry - Process lookup
    {Registry, keys: :unique, name: FinanceApp.Registry},

    # Workers - Can restart independently
    FinanceApp.InvoiceWorker,
    FinanceApp.PaymentWorker
  ]

  # one_for_one: Workers independent, can fail without affecting others
  opts = [strategy: :one_for_one, name: FinanceApp.Supervisor]
  Supervisor.start_link(children, opts)
end
```

Clarify supervision decisions for maintainers.

## References

**OTP Documentation**:

- [Application](https://hexdocs.pm/elixir/Application.html) - Application behavior
- [Supervisor](https://hexdocs.pm/elixir/Supervisor.html) - Supervision trees
- [Mix.Project](https://hexdocs.pm/mix/Mix.Project.html) - Mix project structure

**Configuration**:

- [Config](https://hexdocs.pm/elixir/Config.html) - Configuration module
- [Mix Config](https://hexdocs.pm/mix/Mix.Config.html) - Config file format

**Mix Guide**:

- [Mix Introduction](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html) - Mix basics
- [Umbrella Projects](https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html) - Multi-app projects
