---
title: "Monitoring"
date: 2025-12-21T18:35:00+07:00
draft: false
description: "Implement comprehensive monitoring and logging in Elixir using Telemetry, Logger, metrics collection, and observability for production systems."
weight: 1000023
tags: ["elixir", "monitoring", "logging", "telemetry", "observability", "production", "how-to"]
---

**Need production observability?** Use Logger, Telemetry, and metrics for comprehensive monitoring.

## Prerequisites

- Production Elixir application
- Understanding of processes and supervision
- Completed [Advanced Tutorial](/en/learn/software-engineering/programming-languages/elixir/tutorials/advanced)

## Problem

Production applications need visibility into performance, errors, resource usage, and business metrics. You need structured logging for debugging, real-time metrics for monitoring, and telemetry for understanding system behavior without impacting performance.

**Challenges:**

- Collecting metrics without performance degradation
- Correlating logs across distributed processes
- Monitoring system health in real-time
- Tracking business metrics and SLAs
- Debugging production issues without access to the machine

## Solution

Use **Telemetry** for instrumentation, **Logger** for structured logging, and external services (Prometheus, Datadog, New Relic) for aggregation and visualization.

## How It Works

### 1. Structured Logging with Logger

Basic logging:

```elixir
require Logger

defmodule MyApp.UserService do
  def create_user(params) do
    Logger.info("Creating user", params: sanitize(params))

    with {:ok, validated} <- validate(params),
         {:ok, user} <- insert_user(validated) do
      Logger.info("User created successfully",
        user_id: user.id,
        email: user.email,
        duration_ms: calculate_duration()
      )
      {:ok, user}
    else
      {:error, :validation} = error ->
        Logger.warning("User validation failed",
          errors: inspect(params),
          reason: "invalid_data"
        )
        error

      {:error, reason} = error ->
        Logger.error("User creation failed",
          reason: inspect(reason),
          params: sanitize(params),
          stacktrace: Process.info(self(), :current_stacktrace)
        )
        error
    end
  end
end
```

Configure Logger:

```elixir
config :logger,
  backends: [:console],
  level: :info,
  compile_time_purge_matching: [
    [level_lower_than: :info]  # Remove debug logs in prod
  ]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, :user_id, :module, :function, :file, :line],
  colors: [enabled: false]  # Disable in production

config :logger, :console,
  format: {MyApp.LogFormatter, :format},
  metadata: :all
```

Custom JSON formatter:

```elixir
defmodule MyApp.LogFormatter do
  def format(level, message, timestamp, metadata) do
    %{
      timestamp: format_timestamp(timestamp),
      level: level,
      message: to_string(message),
      metadata: Map.new(metadata)
    }
    |> Jason.encode!()
    |> Kernel.<>("\n")
  end

  defp format_timestamp({date, {h, m, s, ms}}) do
    with {:ok, datetime} <- NaiveDateTime.new(date, {h, m, s, ms * 1000}),
         {:ok, datetime} <- DateTime.from_naive(datetime, "Etc/UTC") do
      DateTime.to_iso8601(datetime)
    end
  end
end
```

### 2. Telemetry Events

Instrument your code:

```elixir
defmodule MyApp.Accounts do
  def create_user(params) do
    start_time = System.monotonic_time()

    result = do_create_user(params)

    duration = System.monotonic_time() - start_time

    :telemetry.execute(
      [:my_app, :accounts, :create_user],
      %{duration: duration},
      %{result: elem(result, 0), user_id: extract_user_id(result)}
    )

    result
  end
end
```

Attach handlers:

```elixir
defmodule MyApp.Telemetry do
  require Logger

  def setup do
    events = [
      [:my_app, :accounts, :create_user],
      [:my_app, :api, :request],
      [:phoenix, :endpoint, :stop],
      [:my_app, :repo, :query]
    ]

    :telemetry.attach_many(
      "my-app-handler",
      events,
      &handle_event/4,
      nil
    )
  end

  def handle_event([:my_app, :accounts, :create_user], measurements, metadata, _config) do
    Logger.info("User creation",
      duration_ms: measurements.duration / 1_000_000,
      result: metadata.result,
      user_id: metadata.user_id
    )

    # Send to metrics system
    MyApp.Metrics.histogram("user.create.duration", measurements.duration)
  end

  def handle_event([:phoenix, :endpoint, :stop], measurements, metadata, _config) do
    Logger.info("HTTP request completed",
      method: metadata.conn.method,
      path: metadata.conn.request_path,
      status: metadata.conn.status,
      duration_ms: measurements.duration / 1_000_000
    )
  end

  def handle_event([:my_app, :repo, :query], measurements, metadata, _config) do
    if measurements.total_time > 100_000_000 do  # > 100ms
      Logger.warning("Slow query detected",
        query: metadata.query,
        duration_ms: measurements.total_time / 1_000_000,
        source: metadata.source
      )
    end
  end
end
```

Start telemetry in application:

```elixir
def start(_type, _args) do
  MyApp.Telemetry.setup()

  children = [
    # ... other children
  ]

  Supervisor.start_link(children, strategy: :one_for_one)
end
```

### 3. Prometheus Metrics

Add dependencies:

```elixir
{:telemetry_metrics, "~> 0.6"},
{:telemetry_poller, "~> 1.0"},
{:telemetry_metrics_prometheus, "~> 1.1"}
```

Define metrics:

```elixir
defmodule MyApp.Metrics do
  use TelemetryMetricsPrometheus

  def metrics do
    [
      # Counters
      counter("my_app.accounts.create_user.count",
        event_name: [:my_app, :accounts, :create_user],
        tags: [:result]
      ),

      # Distributions (histograms)
      distribution("my_app.accounts.create_user.duration",
        event_name: [:my_app, :accounts, :create_user],
        measurement: :duration,
        unit: {:native, :millisecond},
        tags: [:result],
        buckets: [10, 100, 500, 1000, 5000]
      ),

      # HTTP request metrics
      counter("http.requests.total",
        event_name: [:phoenix, :endpoint, :stop],
        tags: [:method, :status]
      ),

      distribution("http.request.duration",
        event_name: [:phoenix, :endpoint, :stop],
        measurement: :duration,
        unit: {:native, :millisecond},
        tags: [:method, :route],
        buckets: [10, 50, 100, 200, 500, 1000]
      ),

      # Database metrics
      counter("db.queries.total",
        event_name: [:my_app, :repo, :query],
        tags: [:source, :result]
      ),

      distribution("db.query.duration",
        event_name: [:my_app, :repo, :query],
        measurement: :total_time,
        unit: {:native, :millisecond},
        tags: [:source],
        buckets: [1, 5, 10, 50, 100, 500]
      ),

      # System metrics
      last_value("vm.memory.total",
        event_name: [:vm, :memory],
        measurement: :total,
        unit: :byte
      ),

      last_value("vm.total_run_queue_lengths.total",
        event_name: [:vm, :total_run_queue_lengths],
        measurement: :total
      )
    ]
  end

  def setup do
    TelemetryMetricsPrometheus.init(
      metrics: metrics(),
      port: 9568,
      name: :my_app_metrics
    )
  end
end
```

Expose metrics endpoint:

```elixir
scope "/metrics" do
  pipe_through :api
  get "/", MyAppWeb.MetricsController, :index
end

defmodule MyAppWeb.MetricsController do
  use MyAppWeb, :controller

  def index(conn, _params) do
    metrics = TelemetryMetricsPrometheus.scrape(:my_app_metrics)
    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(200, metrics)
  end
end
```

### 4. Phoenix LiveDashboard

Add to dependencies:

```elixir
{:phoenix_live_dashboard, "~> 0.8"}
```

Configure:

```elixir
import Phoenix.LiveDashboard.Router

scope "/" do
  pipe_through :browser

  live_dashboard "/dashboard",
    metrics: MyApp.Telemetry,
    additional_pages: [
      route_name: MyCustomPage
    ]
end
```

Define telemetry module:

```elixir
defmodule MyApp.Telemetry do
  import Telemetry.Metrics

  def metrics do
    [
      # Phoenix metrics
      summary("phoenix.endpoint.stop.duration",
        unit: {:native, :millisecond}
      ),
      summary("phoenix.router_dispatch.stop.duration",
        tags: [:route],
        unit: {:native, :millisecond}
      ),

      # Database metrics
      summary("my_app.repo.query.total_time", unit: {:native, :millisecond}),
      summary("my_app.repo.query.decode_time", unit: {:native, :millisecond}),
      summary("my_app.repo.query.query_time", unit: {:native, :millisecond}),
      summary("my_app.repo.query.queue_time", unit: {:native, :millisecond}),
      summary("my_app.repo.query.idle_time", unit: {:native, :millisecond}),

      # VM metrics
      summary("vm.memory.total", unit: {:byte, :kilobyte}),
      summary("vm.total_run_queue_lengths.total"),
      summary("vm.total_run_queue_lengths.cpu"),
      summary("vm.total_run_queue_lengths.io")
    ]
  end
end
```

### 5. Request ID Tracking

Implement request ID middleware:

```elixir
defmodule MyAppWeb.Plugs.RequestID do
  import Plug.Conn
  require Logger

  def init(opts), do: opts

  def call(conn, _opts) do
    request_id = get_request_id(conn)

    conn
    |> put_resp_header("x-request-id", request_id)
    |> Logger.metadata(request_id: request_id)
  end

  defp get_request_id(conn) do
    case get_req_header(conn, "x-request-id") do
      [request_id] -> request_id
      [] -> generate_request_id()
    end
  end

  defp generate_request_id do
    binary = <<
      System.system_time(:nanosecond)::64,
      :erlang.phash2({node(), self()})::32,
      :erlang.unique_integer()::32
    >>

    Base.url_encode64(binary)
  end
end

plug MyAppWeb.Plugs.RequestID
```

### 6. Error Tracking with Sentry

Add Sentry:

```elixir
{:sentry, "~> 10.0"}
```

Configure:

```elixir
config :sentry,
  dsn: System.get_env("SENTRY_DSN"),
  environment_name: System.get_env("ENV", "production"),
  enable_source_code_context: true,
  root_source_code_paths: [File.cwd!()],
  tags: %{
    env: System.get_env("ENV", "production"),
    version: Application.spec(:my_app, :vsn)
  }

config :logger,
  backends: [:console, Sentry.LoggerBackend]
```

Use in code:

```elixir
try do
  risky_operation()
rescue
  exception ->
    Sentry.capture_exception(exception,
      stacktrace: __STACKTRACE__,
      extra: %{user_id: user_id, context: additional_context}
    )
    reraise exception, __STACKTRACE__
end
```

### 7. Application Performance Monitoring (APM)

New Relic integration:

```elixir
{:new_relic_agent, "~> 1.27"}
```

```elixir
config :new_relic_agent,
  app_name: "MyApp",
  license_key: System.get_env("NEW_RELIC_LICENSE_KEY")

config :new_relic_agent,
  automatic_attributes: [:request_id, :user_id]
```

Custom transactions:

```elixir
defmodule MyApp.BackgroundJob do
  use NewRelic.Transaction

  @transaction_name "BackgroundJob/process"

  def perform(data) do
    NewRelic.start_transaction(@transaction_name)

    result = process_data(data)

    NewRelic.stop_transaction()
    result
  end
end
```

### 8. Health Checks

Comprehensive health check:

```elixir
defmodule MyAppWeb.HealthController do
  use MyAppWeb, :controller
  require Logger

  def index(conn, _params) do
    checks = [
      database: check_database(),
      redis: check_redis(),
      external_api: check_external_api()
    ]

    all_healthy = Enum.all?(checks, fn {_name, status} -> status == :ok end)

    status_code = if all_healthy, do: 200, else: 503

    response = %{
      status: if(all_healthy, do: "healthy", else: "unhealthy"),
      checks: format_checks(checks),
      timestamp: DateTime.utc_now()
    }

    conn
    |> put_status(status_code)
    |> json(response)
  end

  defp check_database do
    case Ecto.Adapters.SQL.query(MyApp.Repo, "SELECT 1", [], timeout: 5_000) do
      {:ok, _} -> :ok
      {:error, _} -> :error
    end
  catch
    :exit, _ -> :error
  end

  defp check_redis do
    case Redix.command(:redix, ["PING"]) do
      {:ok, "PONG"} -> :ok
      _ -> :error
    end
  catch
    :exit, _ -> :error
  end

  defp check_external_api do
    case HTTPoison.get("https://api.example.com/health", [], recv_timeout: 5_000) do
      {:ok, %{status_code: 200}} -> :ok
      _ -> :error
    end
  catch
    :exit, _ -> :error
  end

  defp format_checks(checks) do
    Enum.map(checks, fn {name, status} ->
      %{
        name: name,
        status: if(status == :ok, do: "healthy", else: "unhealthy")
      }
    end)
  end
end
```

### 9. Custom Metrics

Business metrics:

```elixir
defmodule MyApp.Metrics.Business do
  def track_signup(user) do
    :telemetry.execute(
      [:my_app, :business, :signup],
      %{count: 1},
      %{
        plan: user.plan,
        referral_source: user.referral_source,
        country: user.country
      }
    )
  end

  def track_purchase(order) do
    :telemetry.execute(
      [:my_app, :business, :purchase],
      %{
        amount: order.total,
        count: 1
      },
      %{
        payment_method: order.payment_method,
        currency: order.currency,
        user_tier: order.user.tier
      }
    )
  end

  def track_feature_usage(feature_name, user) do
    :telemetry.execute(
      [:my_app, :features, :usage],
      %{count: 1},
      %{
        feature: feature_name,
        user_plan: user.plan,
        user_tenure_days: calculate_tenure(user)
      }
    )
  end
end
```

## Variations

### StatsD Integration

```elixir
{:statix, "~> 1.4"}

config :statix,
  host: System.get_env("STATSD_HOST", "localhost"),
  port: String.to_integer(System.get_env("STATSD_PORT", "8125"))

Statix.increment("user.signup", 1, tags: ["plan:premium"])
Statix.histogram("api.request.duration", duration_ms)
Statix.gauge("queue.size", queue_length)
```

### CloudWatch Integration

```elixir
{:ex_aws, "~> 2.0"},
{:ex_aws_cloudwatch, "~> 2.0"}

defmodule MyApp.Metrics.CloudWatch do
  def put_metric(namespace, metric_name, value, unit \\ "Count") do
    ExAws.Cloudwatch.put_metric_data([
      %{
        metric_name: metric_name,
        value: value,
        unit: unit,
        timestamp: DateTime.utc_now()
      }
    ], namespace)
    |> ExAws.request()
  end
end
```

## Advanced Patterns

### 1. Distributed Tracing

```elixir
{:opentelemetry, "~> 1.0"},
{:opentelemetry_phoenix, "~> 1.0"},
{:opentelemetry_ecto, "~> 1.0"}

config :opentelemetry,
  span_processor: :batch,
  traces_exporter: :otlp

config :opentelemetry_exporter,
  otlp_endpoint: System.get_env("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4318")
```

### 2. Log Aggregation

Centralized logging with structured JSON:

```elixir
config :logger, :console,
  format: {MyApp.LogFormatter, :format},
  metadata: :all

Logger.metadata(
  deployment: "production",
  region: "us-east-1",
  instance_id: System.get_env("INSTANCE_ID")
)
```

### 3. Alerting

```elixir
defmodule MyApp.Alerts do
  require Logger

  def check_error_rate do
    rate = calculate_error_rate()

    if rate > 0.05 do  # > 5% error rate
      Logger.error("High error rate detected",
        error_rate: rate,
        alert: true,
        severity: "critical"
      )

      send_pagerduty_alert("High error rate: #{rate}")
    end
  end
end
```

## Use Cases

**Debugging:**

- Request tracing across microservices
- Identifying slow queries
- Understanding error patterns

**Performance:**

- Response time distributions
- Resource utilization trends
- Bottleneck identification

**Business:**

- User signup funnels
- Revenue metrics
- Feature adoption rates

## Troubleshooting

### Missing Metrics

```elixir
:telemetry.list_handlers([:my_app, :accounts, :create_user])

:telemetry.list_handlers(:all)
```

### High Memory Usage from Logs

```elixir
config :logger, level: :info

config :logger, :console, format: "$message\n"
```

## Best Practices

1. **Use structured logging:** Key-value pairs, not interpolated strings
2. **Tag metrics appropriately:** Enable filtering and grouping
3. **Set reasonable retention:** 30 days for logs, 1 year for metrics
4. **Monitor the monitors:** Alert on monitoring system failures
5. **Sanitize sensitive data:** Never log passwords, tokens, PII
6. **Use sampling for high-volume events:** Don't log every request
7. **Set up dashboards:** Visualize key metrics
8. **Define SLOs:** Service Level Objectives for uptime, latency

## Common Pitfalls

1. **Logging too much:** Disk fills up, costs increase
2. **Not logging enough:** Can't debug production issues
3. **Ignoring metrics:** Problems go undetected
4. **Over-alerting:** Alert fatigue reduces response
5. **No request correlation:** Can't trace issues across services

## Related Resources

- [Deployment Guide](/en/learn/software-engineering/programming-languages/elixir/how-to/deployment)
- [Debugging Guide](/en/learn/software-engineering/programming-languages/elixir/how-to/debugging)
- [Performance Guide](/en/learn/software-engineering/programming-languages/elixir/how-to/performance)
- [Telemetry Documentation](https://hexdocs.pm/telemetry/)
- [Phoenix LiveDashboard](https://hexdocs.pm/phoenix_live_dashboard/)
