---
title: "Configuration and Environment Management"
date: 2025-12-21T17:40:00+07:00
draft: false
description: "Manage Elixir application configuration with runtime config, environment variables, and secrets for development, staging, and production."
weight: 1000012
tags: ["elixir", "configuration", "environment", "secrets", "devops", "how-to"]
---

# Configuration and Environment Management

**Need environment-specific configuration?** Elixir provides compile-time and runtime config with environment variable support, enabling flexible configuration management across development, staging, and production environments.

## Prerequisites

- Understanding of Mix projects
- Basic knowledge of environment variables
- Familiarity with deployment concepts

## Problem

Applications need different configurations for development, testing, staging, and production. Managing secrets, environment-specific settings, and runtime configuration is challenging.

**Challenges:**

- Separating compile-time and runtime configuration
- Managing secrets securely
- Supporting multiple environments
- Handling environment variables
- Avoiding hard-coded configuration

## Solution

Use **Config** module with compile-time config files and runtime.exs for environment-specific and runtime configuration.

## How It Works

### 1. Compile-Time Configuration

```elixir
# config/config.exs
import Config

# Application-wide configuration
config :my_app,
  ecto_repos: [MyApp.Repo],
  generators: [timestamp_type: :utc_datetime]

# Logger configuration
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment-specific config
import_config "#{config_env()}.exs"
```

**Development config:**

```elixir
# config/dev.exs
import Config

config :my_app, MyApp.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "my_app_dev",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

config :my_app, MyAppWeb.Endpoint,
  http: [port: 4000],
  debug_errors: true,
  code_reloader: true,
  check_origin: false,
  watchers: [
    esbuild: {Esbuild, :install_and_run, [:default, ~w(--sourcemap=inline --watch)]}
  ]

config :logger, :console, format: "[$level] $message\n"

config :phoenix, :stacktrace_depth, 20
config :phoenix, :plug_init_mode, :runtime
```

**Test config:**

```elixir
# config/test.exs
import Config

config :my_app, MyApp.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "my_app_test#{System.get_env("MIX_TEST_PARTITION")}",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10

config :my_app, MyAppWeb.Endpoint,
  http: [port: 4002],
  server: false

config :logger, level: :warning

config :phoenix, :plug_init_mode, :runtime
```

**Production config:**

```elixir
# config/prod.exs
import Config

# Don't include passwords or secrets here
config :my_app, MyAppWeb.Endpoint,
  cache_static_manifest: "priv/static/cache_manifest.json"

config :logger, level: :info

# Runtime configuration should go in config/runtime.exs
```

### 2. Runtime Configuration

```elixir
# config/runtime.exs
import Config

# This file is executed during runtime, not compile time
# Perfect for reading environment variables

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
      raise """
      environment variable DATABASE_URL is missing.
      For example: ecto://USER:PASS@HOST/DATABASE
      """

  maybe_ipv6 = if System.get_env("ECTO_IPV6") in ~w(true 1), do: [:inet6], else: []

  config :my_app, MyApp.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
    socket_options: maybe_ipv6

  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      You can generate one by calling: mix phx.gen.secret
      """

  host = System.get_env("PHX_HOST") || "example.com"
  port = String.to_integer(System.get_env("PORT") || "4000")

  config :my_app, MyAppWeb.Endpoint,
    url: [host: host, port: 443, scheme: "https"],
    http: [
      ip: {0, 0, 0, 0, 0, 0, 0, 0},
      port: port
    ],
    secret_key_base: secret_key_base

  # External service API keys
  config :my_app, :stripe,
    api_key: System.get_env("STRIPE_API_KEY"),
    webhook_secret: System.get_env("STRIPE_WEBHOOK_SECRET")

  config :my_app, :aws,
    access_key_id: System.get_env("AWS_ACCESS_KEY_ID"),
    secret_access_key: System.get_env("AWS_SECRET_ACCESS_KEY"),
    region: System.get_env("AWS_REGION") || "us-east-1"
end
```

### 3. Environment Variables

```elixir
# Reading environment variables
database_url = System.get_env("DATABASE_URL")

# With default value
port = System.get_env("PORT", "4000")

# Fetch! (raises if missing)
secret = System.fetch_env!("SECRET_KEY_BASE")

# Type conversion
pool_size = System.get_env("POOL_SIZE", "10") |> String.to_integer()
enable_feature = System.get_env("ENABLE_FEATURE") in ["true", "1"]

# Multiple fallbacks
api_key =
  System.get_env("API_KEY") ||
  System.get_env("LEGACY_API_KEY") ||
  raise "API key not configured"
```

**Using with ExUnit:**

```elixir
# test/test_helper.exs
System.put_env("DATABASE_URL", "ecto://postgres:postgres@localhost/my_app_test")
System.put_env("SECRET_KEY_BASE", "test_secret_key_base_at_least_64_bytes_long_test")

ExUnit.start()
```

## Advanced Patterns

### 1. Application Environment

```elixir
# Setting configuration
config :my_app, :settings,
  timeout: 5000,
  retries: 3,
  api_url: "https://api.example.com"

# Reading configuration
defmodule MyApp.Client do
  @timeout Application.compile_env(:my_app, [:settings, :timeout])
  @retries Application.compile_env(:my_app, [:settings, :retries])

  def fetch_data do
    api_url = Application.fetch_env!(:my_app, [:settings, :api_url])

    with {:ok, response} <- HTTPoison.get(api_url, [], timeout: @timeout) do
      {:ok, response.body}
    end
  end
end

# Runtime config access
def get_setting(key) do
  Application.get_env(:my_app, :settings)
  |> Keyword.get(key)
end
```

### 2. Feature Flags

```elixir
# config/config.exs
config :my_app, :features,
  new_dashboard: false,
  experimental_api: false,
  beta_features: false

# config/runtime.exs
if config_env() == :prod do
  config :my_app, :features,
    new_dashboard: System.get_env("FEATURE_NEW_DASHBOARD") == "true",
    experimental_api: System.get_env("FEATURE_EXPERIMENTAL_API") == "true",
    beta_features: System.get_env("FEATURE_BETA") == "true"
end

# Usage
defmodule MyAppWeb.DashboardLive do
  def mount(_params, _session, socket) do
    if Application.get_env(:my_app, :features)[:new_dashboard] do
      {:ok, assign(socket, :view, :new_dashboard)}
    else
      {:ok, assign(socket, :view, :legacy_dashboard)}
    end
  end
end
```

### 3. Multi-Tenant Configuration

```elixir
# config/runtime.exs
config :my_app, :tenants, [
  %{
    id: "tenant_a",
    database: System.get_env("TENANT_A_DATABASE"),
    api_key: System.get_env("TENANT_A_API_KEY")
  },
  %{
    id: "tenant_b",
    database: System.get_env("TENANT_B_DATABASE"),
    api_key: System.get_env("TENANT_B_API_KEY")
  }
]

# Usage
defmodule MyApp.Tenants do
  def get_config(tenant_id) do
    Application.get_env(:my_app, :tenants)
    |> Enum.find(&(&1.id == tenant_id))
  end

  def get_repo(tenant_id) do
    config = get_config(tenant_id)
    # Return appropriate repo based on config
  end
end
```

### 4. Configuration Providers

```elixir
# Custom configuration provider
defmodule MyApp.ConfigProvider do
  @behaviour Config.Provider

  def init(path) when is_binary(path) do
    path
  end

  def load(config, path) do
    # Load configuration from external source
    # (e.g., AWS Secrets Manager, Vault, etc.)
    external_config = load_from_source(path)

    Config.Reader.merge(config, external_config)
  end

  defp load_from_source(path) do
    # Implementation for loading config
    [
      my_app: [
        secret_key_base: fetch_secret("secret_key_base"),
        database_url: fetch_secret("database_url")
      ]
    ]
  end

  defp fetch_secret(key) do
    # Fetch from external secret manager
  end
end

# In releases configuration (mix.exs)
def project do
  [
    releases: [
      my_app: [
        config_providers: [
          {MyApp.ConfigProvider, "/etc/my_app/secrets.json"}
        ]
      ]
    ]
  ]
end
```

## Real-World Examples

### 1. Database Configuration

```elixir
# config/runtime.exs
if config_env() == :prod do
  # Parse DATABASE_URL
  database_url = System.get_env("DATABASE_URL") || raise "DATABASE_URL not set"

  config :my_app, MyApp.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
    ssl: System.get_env("DATABASE_SSL") in ["true", "1"],
    queue_target: 500,
    queue_interval: 1000,
    timeout: 15_000,
    connect_timeout: 5_000,
    handshake_timeout: 5_000,
    pool_timeout: 5_000

  # Read replica configuration
  if replica_url = System.get_env("DATABASE_REPLICA_URL") do
    config :my_app, MyApp.ReadRepo,
      url: replica_url,
      pool_size: String.to_integer(System.get_env("READ_POOL_SIZE") || "5"),
      ssl: true
  end
end
```

### 2. External Services Configuration

```elixir
# config/runtime.exs
config :my_app, :services,
  stripe: %{
    api_key: System.get_env("STRIPE_API_KEY"),
    webhook_secret: System.get_env("STRIPE_WEBHOOK_SECRET"),
    enabled: System.get_env("STRIPE_ENABLED") in ["true", "1"]
  },
  sendgrid: %{
    api_key: System.get_env("SENDGRID_API_KEY"),
    from_email: System.get_env("FROM_EMAIL") || "noreply@example.com"
  },
  s3: %{
    bucket: System.get_env("S3_BUCKET"),
    region: System.get_env("AWS_REGION") || "us-east-1",
    access_key_id: System.get_env("AWS_ACCESS_KEY_ID"),
    secret_access_key: System.get_env("AWS_SECRET_ACCESS_KEY")
  },
  redis: %{
    url: System.get_env("REDIS_URL") || "redis://localhost:6379",
    pool_size: String.to_integer(System.get_env("REDIS_POOL_SIZE") || "10")
  }

# Usage module
defmodule MyApp.Services do
  def stripe_config do
    Application.get_env(:my_app, :services)[:stripe]
  end

  def stripe_enabled? do
    stripe_config()[:enabled]
  end

  def sendgrid_config do
    Application.get_env(:my_app, :services)[:sendgrid]
  end
end
```

### 3. Logging Configuration

```elixir
# config/runtime.exs
log_level =
  case System.get_env("LOG_LEVEL") do
    "debug" -> :debug
    "info" -> :info
    "warn" -> :warning
    "error" -> :error
    _ -> if config_env() == :prod, do: :info, else: :debug
  end

config :logger, level: log_level

# Structured logging
config :logger, :console,
  format: {MyApp.LogFormatter, :format},
  metadata: [:request_id, :user_id, :tenant_id]

# External logging services
if config_env() == :prod do
  if sentry_dsn = System.get_env("SENTRY_DSN") do
    config :sentry,
      dsn: sentry_dsn,
      environment_name: config_env(),
      enable_source_code_context: true,
      root_source_code_path: File.cwd!(),
      tags: %{
        env: to_string(config_env())
      }
  end

  if logflare_api_key = System.get_env("LOGFLARE_API_KEY") do
    config :logflare,
      api_key: logflare_api_key,
      source_id: System.get_env("LOGFLARE_SOURCE_ID")
  end
end
```

### 4. Phoenix Endpoint Configuration

```elixir
# config/runtime.exs
if config_env() == :prod do
  host = System.get_env("PHX_HOST") || raise "PHX_HOST not set"
  port = String.to_integer(System.get_env("PORT") || "4000")

  # Force SSL configuration
  force_ssl = System.get_env("FORCE_SSL") in ["true", "1"]

  config :my_app, MyAppWeb.Endpoint,
    url: [host: host, port: 443, scheme: "https"],
    http: [
      ip: {0, 0, 0, 0, 0, 0, 0, 0},
      port: port,
      protocol_options: [max_header_value_length: 8192]
    ],
    secret_key_base: System.fetch_env!("SECRET_KEY_BASE"),
    server: true,
    force_ssl: if(force_ssl, do: [rewrite_on: [:x_forwarded_proto]], else: false)

  # Session configuration
  config :my_app, MyAppWeb.Endpoint,
    live_view: [signing_salt: System.fetch_env!("LIVE_VIEW_SALT")],
    session_options: [
      key: "_my_app_key",
      signing_salt: System.fetch_env!("SESSION_SALT"),
      same_site: "Lax",
      secure: force_ssl,
      max_age: 86400 * 30  # 30 days
    ]

  # CORS configuration
  if allowed_origins = System.get_env("ALLOWED_ORIGINS") do
    origins = String.split(allowed_origins, ",")
    config :cors_plug, origin: origins
  end
end
```

## Configuration Validation

```elixir
# lib/my_app/config.ex
defmodule MyApp.Config do
  @required_env_vars [
    "DATABASE_URL",
    "SECRET_KEY_BASE",
    "PHX_HOST"
  ]

  @optional_env_vars [
    "PORT",
    "POOL_SIZE",
    "LOG_LEVEL"
  ]

  def validate! do
    missing = Enum.reject(@required_env_vars, &System.get_env/1)

    unless Enum.empty?(missing) do
      raise """
      Missing required environment variables:
      #{Enum.join(missing, "\n")}
      """
    end

    :ok
  end

  def required_vars, do: @required_env_vars
  def optional_vars, do: @optional_env_vars
end

# In application.ex
def start(_type, _args) do
  MyApp.Config.validate!()

  # ... rest of start function
end
```

## Common Pitfalls

### 1. Hard-Coding Secrets

**Problem:**

```elixir
# BAD - secrets in code
config :my_app, :stripe,
  api_key: "sk_live_abc123..."  # Never do this!
```

**Solution:**

```elixir
# GOOD - secrets from environment
config :my_app, :stripe,
  api_key: System.get_env("STRIPE_API_KEY")
```

### 2. Compile-Time Environment Variables

**Problem:**

```elixir
# config/prod.exs - BAD
# This reads at compile time, not runtime
config :my_app, :api_url, System.get_env("API_URL")
```

**Solution:**

```elixir
# config/runtime.exs - GOOD
# This reads at runtime
if config_env() == :prod do
  config :my_app, :api_url, System.get_env("API_URL")
end
```

### 3. Missing Environment Variables

**Problem:**

```elixir
# Silently falls back to nil
database_url = System.get_env("DATABASE_URL")
```

**Solution:**

```elixir
# Explicit error if missing
database_url =
  System.get_env("DATABASE_URL") ||
    raise "DATABASE_URL environment variable is required"

# Or use fetch_env!
database_url = System.fetch_env!("DATABASE_URL")
```

## Configuration Best Practices

1. **Use runtime.exs for production** - Read environment variables at runtime
2. **Never commit secrets** - Use environment variables for sensitive data
3. **Validate required config** - Fail fast if required configuration is missing
4. **Provide defaults** - Use sensible defaults for optional configuration
5. **Document environment variables** - List all required and optional variables in README
6. **Use different configs per environment** - Separate dev/test/prod configuration
7. **Keep config DRY** - Extract common configuration patterns
8. **Version configuration files** - Track config changes in version control (except secrets)

## Environment Variable Documentation

Create a `.env.example` file:

```bash
# .env.example
# Copy to .env and fill in values

# Database
DATABASE_URL=ecto://postgres:postgres@localhost/my_app_dev
POOL_SIZE=10

# Application
SECRET_KEY_BASE=
PHX_HOST=localhost
PORT=4000

# External Services
STRIPE_API_KEY=
STRIPE_WEBHOOK_SECRET=
SENDGRID_API_KEY=

# AWS
AWS_ACCESS_KEY_ID=
AWS_SECRET_ACCESS_KEY=
AWS_REGION=us-east-1
S3_BUCKET=

# Feature Flags
FEATURE_NEW_DASHBOARD=false
FEATURE_EXPERIMENTAL_API=false

# Logging
LOG_LEVEL=info
SENTRY_DSN=
```

## Testing Configuration

```elixir
# test/my_app/config_test.exs
defmodule MyApp.ConfigTest do
  use ExUnit.Case

  test "validates required environment variables" do
    # Save current env
    original_db_url = System.get_env("DATABASE_URL")

    # Remove required var
    System.delete_env("DATABASE_URL")

    assert_raise RuntimeError, ~r/DATABASE_URL/, fn ->
      MyApp.Config.validate!()
    end

    # Restore
    if original_db_url do
      System.put_env("DATABASE_URL", original_db_url)
    end
  end

  test "loads stripe configuration" do
    config = MyApp.Services.stripe_config()
    assert is_binary(config[:api_key])
    assert is_boolean(config[:enabled])
  end
end
```

## Related Resources

- [Mix Guide](/en/learn/swe/prog-lang/elixir/how-to/mix)
- [Deployment Guide](/en/learn/swe/prog-lang/elixir/how-to/deployment)
- [Testing Guide](/en/learn/swe/prog-lang/elixir/how-to/testing)
- [Best Practices](/en/learn/swe/prog-lang/elixir/explanation/best-practices)
