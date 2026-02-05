---
title: "Logging Observability"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000028
description: "Logging and observability strategies for production Elixir applications using Logger and Telemetry"
tags: ["elixir", "logging", "observability", "telemetry", "opentelemetry", "metrics", "tracing"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/configuration-management"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/error-handling-resilience"
---

**Building observable Elixir applications?** This guide teaches logging and observability through the OTP-First progression, starting with the Logger module for basic logging to understand foundational patterns before introducing Telemetry for metrics and OpenTelemetry for distributed tracing.

## Why Logging and Observability Matter

Every production application needs comprehensive observability:

- **Financial systems** - Donation processing latency, payment success rates, transaction audit logs
- **Healthcare platforms** - Patient data access logs, API response times, system health metrics
- **E-commerce** - Order processing duration, inventory update frequency, checkout conversion metrics
- **SaaS applications** - User activity tracking, feature usage metrics, error rate monitoring

Elixir provides three observability approaches:

1. **Logger (Standard Library)** - Basic logging with configurable backends and metadata
2. **Telemetry** - Event-based metrics and monitoring without external dependencies
3. **OpenTelemetry** - Distributed tracing and metrics aggregation for production systems

**Our approach**: Start with Logger to understand basic logging patterns, recognize limitations with structured logging and metrics, then introduce Telemetry for event-based monitoring and OpenTelemetry for distributed systems.

## OTP Primitives - Logger Module

### Basic Logging with Logger

Let's start with Logger's fundamental logging patterns:

```elixir
# Basic Logger usage
defmodule DonationService do
  # => Module for donation processing
  # => Handles validation and transaction creation

  require Logger
  # => Imports: Logger.debug, info, warn, error
  # => Compile-time macro transformation

  def process_donation(donation) do
    # => Public function: Process donation
    # => Parameter: donation struct

    Logger.info("Processing donation")
    # => Logs: "Processing donation"
    # => Level: :info
    # => No structured data

    case validate_donation(donation) do
      # => Pattern match validation result
      # => Calls private validation function

      {:ok, validated} ->
        # => Success path: Validation passed
        # => validated: Validated donation struct

        Logger.info("Donation validated: #{donation.id}")
        # => String interpolation for context
        # => donation.id embedded in message

        create_transaction(validated)
        # => Proceeds with transaction
        # => Returns transaction result

      {:error, reason} ->
        # => Error path: Validation failed
        # => reason: Error reason term

        Logger.error("Donation validation failed: #{inspect(reason)}")
        # => inspect/1: Converts term to readable string
        # => Logs error with reason

        {:error, reason}
        # => Returns error tuple
        # => Type: {:error, term()}
    end
  end

  defp validate_donation(donation) do
    # => Private validation function
    # => Returns {:ok, donation} or {:error, reason}
    # Implementation details...
  end

  defp create_transaction(donation) do
    # => Private transaction creation
    # => Returns transaction result
    # Implementation details...
  end
end
```

### Logger Metadata for Context

Add structured metadata to log entries:

```elixir
# Logger metadata for context
defmodule PaymentProcessor do
  # => Module for payment processing
  # => Handles card charges with metadata

  require Logger
  # => Import Logger macros

  def charge_card(payment_id, amount, user_id) do
    # => Public function: Charge card
    # => Parameters: payment_id, amount, user_id

    Logger.metadata(payment_id: payment_id, user_id: user_id)
    # => Sets metadata for current process
    # => Available in all subsequent logs
    # => Type: keyword list

    Logger.info("Charging card", amount: amount)
    # => Logs with metadata
    # => Output includes payment_id, user_id, amount
    # => Format depends on backend configuration

    case process_charge(payment_id, amount) do
      # => Pattern match charge result
      # => Calls private charge function

      {:ok, transaction_id} ->
        # => Success path: Charge succeeded
        # => transaction_id: Payment transaction identifier

        Logger.info("Card charged successfully",
          transaction_id: transaction_id
        )
        # => Additional metadata for this log
        # => Merged with process metadata

        {:ok, transaction_id}
        # => Return success tuple
        # => Type: {:ok, String.t()}

      {:error, :insufficient_funds} ->
        # => Error path: Not enough funds
        # => Recoverable error case

        Logger.warn("Insufficient funds for payment")
        # => Warning level for recoverable errors
        # => Still includes process metadata

        {:error, :insufficient_funds}
        # => Return error atom
        # => Type: {:error, :insufficient_funds}

      {:error, reason} ->
        # => Error path: Unexpected failure
        # => Catch-all error case

        Logger.error("Payment processing failed",
          reason: inspect(reason)
        )
        # => Error level for unexpected failures
        # => inspect/1 converts reason to string

        {:error, reason}
        # => Return error tuple
        # => Type: {:error, term()}
    end
  after
    # => Always executed after function body
    # => Cleanup block for metadata

    Logger.metadata(payment_id: nil, user_id: nil)
    # => Clears metadata after operation
    # => Prevents metadata leak to subsequent operations
  end

  defp process_charge(_payment_id, _amount) do
    # => Private charge processing
    # => Returns {:ok, txn_id} or {:error, reason}
    # Implementation details...
    {:ok, "txn_123abc"}
    # => Mock successful charge result
  end
end
```

### Configuring Logger Backends

Configure logging output and formatting:

```elixir
# config/config.exs - Logger configuration
import Config
# => Import Config macros
# => Enables config/2 function

config :logger,
  # => Global Logger configuration
  # => Applies to all backends

  level: :info
  # => Minimum log level: :debug, :info, :warn, :error
  # => Filters logs below this level
  # => Production default: :info

config :logger, :console,
  # => Console backend configuration
  # => Built-in backend for stdout/stderr

  format: "$time $metadata[$level] $message\n"
  # => Log format template
  # => $time: Timestamp
  # => $metadata: Process metadata
  # => $level: Log level
  # => $message: Log message

  metadata: [:request_id, :user_id, :payment_id]
  # => Metadata keys to include
  # => Only specified keys printed
  # => Empty list: no metadata
  # => Type: list(atom())

# Example log output:
# 12:34:56.789 request_id=abc user_id=123[info] Processing donation
```

### Logger for Audit Logs

Create immutable audit trails:

```elixir
# Audit logging pattern
defmodule AuditLogger do
  # => Module for audit trail logging
  # => Immutable record of user actions

  require Logger
  # => Import Logger macros

  def log_user_action(user_id, action, resource, metadata \\ []) do
    # => Public function: Log user action
    # => Parameters: user_id, action, resource, optional metadata
    # => Default metadata: empty list

    Logger.info("User action recorded",
      user_id: user_id,
      # => User performing action
      # => Type: integer() or string()

      action: action,
      # => Action type: :create, :update, :delete
      # => Type: atom()

      resource: resource,
      # => Affected resource identifier
      # => Format: "resource_type:id"

      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      # => ISO 8601 timestamp
      # => UTC for consistency
      # => Example: "2026-02-05T12:34:56Z"

      metadata: metadata
      # => Additional context
      # => Type: keyword list
      # => Flexible key-value pairs
    )
  end
end

# Usage example
AuditLogger.log_user_action(
  123,
  # => user_id
  # => Integer identifier
  :create,
  # => action
  # => Atom representing operation
  "donation:456",
  # => resource identifier
  # => String with type:id format
  amount: 1000, currency: "USD"
  # => Additional metadata
  # => Keyword list of context
)
# => Logs: User action recorded user_id=123 action=create resource=donation:456...
# => Immutable audit trail entry
```

## Limitations of Logger Alone

### Problem 1: No Structured Logging

Logger metadata is limited:

```elixir
# Logger metadata limitations
Logger.info("Payment processed",
  # => Log payment completion
  # => Metadata not JSON-serialized

  amount: 1000,
  # => Simple value works
  # => Integer metadata supported

  payment_details: %{card: "visa", last4: "4242"}
  # => Map requires inspect/1
  # => Output: payment_details=%{card: "visa", last4: "4242"}
  # => Not parseable by log aggregators
  # => Loses structure in log files
)

# Need structured logging for:
# - JSON output for log aggregators (ELK, Datadog)
# - Queryable fields in monitoring systems
# - Consistent schema across services
# - Machine-readable log format
```

### Problem 2: No Metrics Collection

Logger doesn't track metrics:

```elixir
# No built-in metrics
Logger.info("Donation processed", amount: 1000)
# => Logs event but doesn't aggregate
# => Can't calculate: average donation, total volume, rate
# => Need manual parsing of logs
# => No real-time metrics dashboard
# => Type: single log entry

# Need metrics for:
# - Request latency percentiles (p50, p95, p99)
# - Error rates and success rates
# - Resource utilization (memory, processes)
# - Business metrics (donations/hour, conversion rate)
# - Alerting on threshold breaches
```

### Problem 3: No Distributed Tracing

Can't trace requests across services:

```elixir
# No trace correlation
# Service A logs: request_id=abc
Logger.info("API request received", request_id: "abc")
# => Log in Service A
# => request_id: "abc"
# => No automatic propagation

# Service B logs: Different context, no correlation
Logger.info("Database query executed")
# => Log in Service B
# => No trace_id linking services
# => Can't reconstruct request path
# => Manual correlation required
# => Lost context across service boundaries

# Need distributed tracing for:
# - Request path visualization
# - Cross-service latency analysis
# - Dependency mapping
# - Performance bottleneck identification
# - End-to-end transaction monitoring
```

## Production Solution - Telemetry

### Installing Telemetry

Add Telemetry for event-based metrics:

```elixir
# mix.exs - Add Telemetry dependency
defp deps do
  # => Dependencies function
  # => Returns list of package tuples

  [
    {:telemetry, "~> 1.0"},
    # => Core telemetry library
    # => Event emission and handling
    # => Required for all telemetry features

    {:telemetry_metrics, "~> 0.6"},
    # => Metric aggregation
    # => Counter, sum, last_value, summary, distribution
    # => Defines metric types

    {:telemetry_poller, "~> 1.0"}
    # => Periodic measurements
    # => Memory, process count, scheduler utilization
    # => Automatic VM stats collection
  ]
end
```

### Emitting Telemetry Events

Instrument code with telemetry events:

```elixir
# Emitting telemetry events
defmodule DonationService do
  def process_donation(donation) do
    start_time = System.monotonic_time()
    # => Monotonic time for duration calculation
    # => Not affected by system clock changes

    metadata = %{
      # => Event metadata

      donation_id: donation.id,
      # => Donation identifier

      user_id: donation.user_id,
      # => User identifier

      amount: donation.amount
      # => Donation amount
    }

    result = do_process_donation(donation)
    # => Perform actual processing
    # => Returns {:ok, result} or {:error, reason}

    duration = System.monotonic_time() - start_time
    # => Calculate operation duration
    # => Type: integer (native time unit)

    :telemetry.execute(
      [:donation, :processed],
      # => Event name as list of atoms
      # => Hierarchical naming

      %{duration: duration, count: 1},
      # => Measurements map
      # => Numeric values for metrics

      metadata
      # => Metadata map
      # => Contextual information
    )

    result
    # => Return original result
  end

  defp do_process_donation(_donation) do
    # => Actual processing logic
    # Implementation details...
    {:ok, %{id: "don_123"}}
  end
end
```

### Attaching Telemetry Handlers

Define metric handlers:

```elixir
# lib/my_app/telemetry.ex - Telemetry handler setup
defmodule MyApp.Telemetry do
  use Supervisor
  # => Supervisor behavior for handler supervision

  import Telemetry.Metrics
  # => Imports: counter, sum, last_value, summary, distribution

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
    # => Starts supervisor
    # => Registers with module name
  end

  def init(_arg) do
    children = [
      {:telemetry_poller, measurements: periodic_measurements(), period: 10_000}
      # => Polls measurements every 10 seconds
      # => Memory, process count, scheduler stats
    ]

    Supervisor.init(children, strategy: :one_for_one)
    # => One-for-one supervision
    # => Restart failed handlers independently
  end

  def metrics do
    [
      # Donation processing metrics
      counter("donation.processed.count"),
      # => Counts donation events
      # => Incremented by measurement.count

      summary("donation.processed.duration",
        # => Duration percentiles

        unit: {:native, :millisecond}
        # => Converts native time to milliseconds
      ),

      distribution("donation.processed.duration",
        # => Duration histogram

        buckets: [100, 200, 500, 1000, 2000, 5000]
        # => Latency buckets in milliseconds
      ),

      sum("donation.amount.total",
        # => Sum of all donation amounts

        measurement: :amount,
        # => Uses amount from metadata

        tags: [:currency]
        # => Group by currency
      ),

      last_value("vm.memory.total",
        # => Current memory usage

        unit: {:byte, :megabyte}
        # => Converts bytes to megabytes
      )
    ]
  end

  defp periodic_measurements do
    [
      {__MODULE__, :measure_memory, []}
      # => Calls measure_memory/0 periodically
    ]
  end

  def measure_memory do
    # => Custom measurement function

    :telemetry.execute(
      [:vm, :memory],
      # => Event name

      %{total: :erlang.memory(:total), processes: :erlang.memory(:processes)}
      # => Memory measurements
      # => :erlang.memory/1 returns bytes
    )
  end
end
```

### Visualizing Metrics with Telemetry UI

Add live dashboard for metrics:

```elixir
# mix.exs - Add Phoenix LiveDashboard
defp deps do
  [
    {:phoenix_live_dashboard, "~> 0.7"}
    # => Web UI for telemetry metrics
  ]
end

# lib/my_app_web/router.ex - Mount dashboard
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  import Phoenix.LiveDashboard.Router
  # => Imports live_dashboard routes

  scope "/" do
    pipe_through :browser
    # => Browser pipeline

    live_dashboard "/dashboard",
      # => Mounts at /dashboard

      metrics: MyApp.Telemetry.metrics()
      # => Displays defined metrics
      # => Real-time updates
  end
end
```

## Production Solution - OpenTelemetry

### Installing OpenTelemetry

Add OpenTelemetry for distributed tracing:

```elixir
# mix.exs - Add OpenTelemetry dependencies
defp deps do
  # => Dependencies function
  # => OpenTelemetry stack packages

  [
    {:opentelemetry, "~> 1.0"},
    # => Core OpenTelemetry library
    # => Span creation and tracing

    {:opentelemetry_exporter, "~> 1.0"},
    # => OTLP exporter (Jaeger, Tempo, etc.)
    # => Sends traces to collector

    {:opentelemetry_phoenix, "~> 1.0"},
    # => Phoenix instrumentation
    # => Automatic HTTP request tracing

    {:opentelemetry_ecto, "~> 1.0"}
    # => Ecto query tracing
    # => Database query spans
  ]
end
```

### Configuring OpenTelemetry

Configure tracing and export:

```elixir
# config/runtime.exs - OpenTelemetry configuration
import Config

config :opentelemetry,
  # => Global OpenTelemetry config

  service_name: "my_app",
  # => Service identifier in traces

  traces_exporter: :otlp,
  # => Export format: OTLP (OpenTelemetry Protocol)

  resource: [
    # => Resource attributes

    {:service, :name, "my_app"},
    # => Service name

    {:service, :version, "1.0.0"},
    # => Service version

    {:deployment, :environment, config_env()}
    # => Environment: :dev, :test, :prod
  ]

config :opentelemetry_exporter,
  # => Exporter configuration

  otlp_endpoint: "http://localhost:4318"
  # => OTLP receiver endpoint
  # => Jaeger, Tempo, or custom collector
```

### Manual Span Creation

Create custom spans for operations:

```elixir
# Manual span creation
defmodule DonationService do
  require OpenTelemetry.Tracer, as: Tracer
  # => Imports span macros

  def process_donation(donation) do
    Tracer.with_span "process_donation" do
      # => Creates span for this operation
      # => Automatically closed after block

      Tracer.set_attributes([
        # => Add span attributes

        {"donation.id", donation.id},
        # => Donation identifier

        {"user.id", donation.user_id},
        # => User identifier

        {"amount", donation.amount}
        # => Donation amount
      ])

      validate_result = validate_donation(donation)
      # => Nested operation (creates child span if instrumented)

      case validate_result do
        {:ok, validated} ->
          Tracer.add_event("donation_validated", %{})
          # => Records event in span
          # => Timestamp automatically added

          create_transaction(validated)
          # => Another nested operation

        {:error, reason} ->
          Tracer.set_status(:error, inspect(reason))
          # => Marks span as error
          # => Includes error description

          Tracer.record_exception(reason)
          # => Records exception details
          # => Stack trace if available

          {:error, reason}
      end
    end
  end

  defp validate_donation(donation) do
    Tracer.with_span "validate_donation" do
      # => Child span under process_donation
      # => Automatic parent-child relationship

      # Validation logic...
      {:ok, donation}
    end
  end

  defp create_transaction(donation) do
    Tracer.with_span "create_transaction" do
      # => Another child span
      # Implementation details...
      {:ok, %{id: "txn_123"}}
    end
  end
end
```

### Automatic Phoenix Instrumentation

Phoenix requests automatically traced:

```elixir
# lib/my_app/application.ex - Enable Phoenix instrumentation
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    OpentelemetryPhoenix.setup()
    # => Instruments Phoenix router, controllers, views
    # => Creates spans for: HTTP requests, controller actions, view rendering

    OpentelemetryEcto.setup([:my_app, :repo])
    # => Instruments Ecto queries
    # => Creates spans for: SELECT, INSERT, UPDATE, DELETE
    # => Includes query text and parameters

    children = [
      MyApp.Repo,
      MyAppWeb.Endpoint
      # Other children...
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

# Automatic tracing creates span hierarchy:
# HTTP GET /donations/123
#   ├─ DonationController.show
#   │   ├─ Ecto SELECT donations WHERE id = $1
#   │   └─ DonationView.render
#   └─ HTTP Response 200
```

### Distributed Trace Context

Propagate trace context across services:

```elixir
# Distributed trace propagation
defmodule PaymentService do
  require OpenTelemetry.Tracer, as: Tracer

  def charge_payment(payment_id, amount) do
    Tracer.with_span "charge_payment" do
      # => Span in Service A

      # Call external service B
      response = HTTPoison.post(
        "http://payment-gateway/charge",
        # => External service URL

        Jason.encode!(%{payment_id: payment_id, amount: amount}),
        # => Request body

        [
          {"content-type", "application/json"},
          {"traceparent", get_trace_header()}
          # => Injects trace context
          # => W3C Trace Context format
          # => Links Service B span to Service A span
        ]
      )

      case response do
        {:ok, %{status_code: 200}} ->
          {:ok, :charged}

        {:error, reason} ->
          Tracer.set_status(:error, inspect(reason))
          {:error, reason}
      end
    end
  end

  defp get_trace_header do
    # => Extracts current trace context
    # => Format: 00-trace_id-span_id-flags
    OpenTelemetry.Tracer.current_span_ctx()
    |> OpenTelemetry.Propagator.text_map_inject()
    |> Map.get("traceparent")
  end
end

# Trace spans across services:
# Service A: charge_payment (trace_id: abc123)
#   └─ HTTP POST to Service B (propagates trace_id: abc123)
#       └─ Service B: process_charge (same trace_id: abc123)
#           └─ Database query
```

## Trade-offs: Logger vs Telemetry vs OpenTelemetry

| Aspect              | Logger (stdlib)    | Telemetry          | OpenTelemetry           |
| ------------------- | ------------------ | ------------------ | ----------------------- |
| **Complexity**      | Low                | Medium             | High                    |
| **Learning Curve**  | 1 hour             | 4-8 hours          | 2-3 days                |
| **Dependencies**    | None (stdlib)      | 1-3 libraries      | 5+ libraries            |
| **Structured Logs** | Limited (metadata) | Event-based        | Full structured tracing |
| **Metrics**         | None               | Built-in           | Built-in                |
| **Tracing**         | None               | None               | Distributed tracing     |
| **Performance**     | Minimal overhead   | Low overhead       | Medium overhead         |
| **Production Use**  | Simple apps        | Most apps          | Microservices           |
| **Visualization**   | Log files          | LiveDashboard      | Jaeger/Tempo/Grafana    |
| **Best For**        | Basic logging      | Single-app metrics | Distributed systems     |

## Best Practices

### Use Logger for Basic Logging

```elixir
# Good: Logger for simple logging
Logger.info("User logged in", user_id: user.id)
# => Quick debugging and audit logs
# => No external dependencies
# => Adequate for most cases
```

### Use Telemetry for Metrics

```elixir
# Good: Telemetry for business metrics
:telemetry.execute(
  [:checkout, :completed],
  %{duration: duration, amount: amount},
  %{user_id: user.id, product_id: product.id}
)
# => Track latency, throughput, business KPIs
# => Low overhead, no external services
```

### Use OpenTelemetry for Distributed Tracing

```elixir
# Good: OpenTelemetry for multi-service tracing
Tracer.with_span "process_order" do
  # => Trace across: API gateway, order service, payment service, inventory
  # => Visualize request path
  # => Identify bottlenecks
end
```

### Combine All Three

```elixir
# Production pattern: Logger + Telemetry + OpenTelemetry
defmodule OrderService do
  require Logger
  require OpenTelemetry.Tracer, as: Tracer

  def create_order(order) do
    Tracer.with_span "create_order" do
      # => OpenTelemetry: Distributed trace

      Logger.info("Creating order", order_id: order.id)
      # => Logger: Audit log

      start_time = System.monotonic_time()
      result = do_create_order(order)
      duration = System.monotonic_time() - start_time

      :telemetry.execute(
        [:order, :created],
        %{duration: duration, count: 1},
        %{order_id: order.id, user_id: order.user_id}
      )
      # => Telemetry: Business metrics

      result
    end
  end

  defp do_create_order(_order) do
    # Implementation...
    {:ok, %{id: "order_123"}}
  end
end
```

### Structure Log Metadata Consistently

```elixir
# Good: Consistent metadata keys
Logger.metadata(
  request_id: request_id,
  # => UUID for request correlation

  user_id: user_id,
  # => User identifier

  session_id: session_id
  # => Session identifier
)

# Avoid: Inconsistent naming
Logger.metadata(reqId: x, userId: y, sessionID: z)
# => Mixed naming conventions
# => Harder to query logs
```

### Set Appropriate Log Levels

```elixir
# Good: Appropriate log levels
Logger.debug("Request payload: #{inspect(payload)}")
# => Debug: Verbose information (disabled in prod)

Logger.info("User logged in", user_id: user.id)
# => Info: Normal operations

Logger.warn("Rate limit approaching", usage: 95)
# => Warn: Potential issues

Logger.error("Payment failed", error: reason)
# => Error: Operation failures

# Avoid: Everything as info
Logger.info("Payment failed")
# => Should be :error level
```

### Use Sampling for High-Volume Traces

```elixir
# config/runtime.exs - Trace sampling
config :opentelemetry,
  traces_sampler: {:parent_based, %{root: {:trace_id_ratio_based, 0.1}}}
  # => Sample 10% of traces
  # => Reduces overhead for high-throughput services
  # => Parent-based: Always sample if parent sampled
```

## Financial System Example

Complete logging and observability for donation processing:

```elixir
defmodule CharityPlatform.DonationService do
  require Logger
  require OpenTelemetry.Tracer, as: Tracer

  def process_donation(donation) do
    # Distributed trace span
    Tracer.with_span "process_donation" do
      Tracer.set_attributes([
        {"donation.id", donation.id},
        {"donation.amount", donation.amount},
        {"donation.currency", donation.currency},
        {"user.id", donation.user_id}
      ])

      # Audit log
      Logger.metadata(
        donation_id: donation.id,
        user_id: donation.user_id
      )

      Logger.info("Processing donation",
        amount: donation.amount,
        currency: donation.currency
      )

      # Measure performance
      start_time = System.monotonic_time()

      result =
        with {:ok, validated} <- validate_donation(donation),
             {:ok, payment} <- charge_payment(validated),
             {:ok, receipt} <- create_receipt(payment) do
          {:ok, receipt}
        else
          {:error, reason} = error ->
            Logger.error("Donation processing failed", reason: inspect(reason))
            Tracer.set_status(:error, inspect(reason))
            error
        end

      duration = System.monotonic_time() - start_time

      # Emit metrics
      :telemetry.execute(
        [:donation, :processed],
        %{duration: duration, count: 1, amount: donation.amount},
        %{
          status: if(match?({:ok, _}, result), do: "success", else: "failure"),
          currency: donation.currency
        }
      )

      result
    end
  end

  defp validate_donation(donation) do
    Tracer.with_span "validate_donation" do
      # Validation logic...
      {:ok, donation}
    end
  end

  defp charge_payment(donation) do
    Tracer.with_span "charge_payment" do
      # Payment processing...
      {:ok, %{transaction_id: "txn_123"}}
    end
  end

  defp create_receipt(payment) do
    Tracer.with_span "create_receipt" do
      # Receipt generation...
      {:ok, %{receipt_id: "receipt_456"}}
    end
  end
end

# Observability output:
# 1. Logger: Audit logs in files/stdout
# 2. Telemetry: Metrics in LiveDashboard
#    - donation.processed.count: 1234 donations/hour
#    - donation.processed.duration p95: 234ms
#    - donation.amount.total USD: $12,345
# 3. OpenTelemetry: Distributed traces in Jaeger
#    - Trace: process_donation (245ms)
#      ├─ validate_donation (12ms)
#      ├─ charge_payment (198ms)
#      │  └─ HTTP POST to payment gateway (195ms)
#      └─ create_receipt (35ms)
```

## Summary

**Start with Logger** for basic logging and audit trails. It's built-in, simple, and sufficient for most use cases.

**Add Telemetry** when you need metrics, monitoring, and business KPIs. Use it for single-application observability.

**Adopt OpenTelemetry** when building distributed systems that require trace correlation across multiple services.

**Production pattern**: Combine all three for comprehensive observability: Logger for audit logs, Telemetry for metrics, OpenTelemetry for distributed tracing.

**Next steps**: Explore [Error Handling Resilience](/en/learn/software-engineering/programming-languages/elixir/in-the-field/error-handling-resilience) for handling failures gracefully, or [Performance Optimization](/en/learn/software-engineering/programming-languages/elixir/in-the-field/performance-optimization) for profiling and optimization techniques.
