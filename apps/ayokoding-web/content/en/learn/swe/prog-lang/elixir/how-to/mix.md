---
title: "Mix Project Management"
date: 2025-12-21T17:35:00+07:00
draft: false
description: "Manage Elixir projects with Mix build tool covering dependencies, tasks, releases, and project organization for development and production."
weight: 1000011
tags:
  [
    "elixir",
    "mix",
    "build-tool",
    "dependencies",
    "project-management",
    "how-to",
  ]
---

# Mix Project Management

**Need to manage Elixir projects effectively?** Mix is Elixir's build tool for dependencies, compilation, testing, and releases, providing a complete project lifecycle management solution.

## Prerequisites

- Basic Elixir knowledge
- Understanding of command-line tools
- Familiarity with project structure concepts

## Problem

Managing Elixir projects requires handling dependencies, running tests, building releases, and organizing code. Manual management is error-prone and doesn't scale.

**Challenges:**

- Organizing project structure and dependencies
- Running tests and maintaining code quality
- Building production releases
- Creating custom automation tasks
- Managing different environments

## Solution

Use **Mix** for complete project lifecycle management from creation to deployment.

## How It Works

### 1. Create New Project

```bash
# Regular application
mix new my_app
cd my_app

# Supervised application (with supervision tree)
mix new my_app --sup

# Library (no supervisor)
mix new my_library --module MyLibrary

# Umbrella project (monorepo)
mix new my_umbrella --umbrella
```

**Generated structure:**

```
my_app/
├── lib/
│   └── my_app.ex
├── test/
│   ├── my_app_test.exs
│   └── test_helper.exs
├── mix.exs
├── .formatter.exs
├── .gitignore
└── README.md
```

### 2. Project Configuration (mix.exs)

```elixir
defmodule MyApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :my_app,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),

      # Documentation
      name: "MyApp",
      source_url: "https://github.com/user/my_app",
      docs: [
        main: "MyApp",
        extras: ["README.md"]
      ],

      # Testing
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {MyApp.Application, []}
    ]
  end

  defp deps do
    [
      {:phoenix, "~> 1.7.0"},
      {:ecto_sql, "~> 3.10"},
      {:postgrex, ">= 0.0.0"},
      {:jason, "~> 1.4"},

      # Dev/Test only
      {:ex_doc, "~> 0.29", only: :dev, runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false}
    ]
  end

  defp aliases do
    [
      setup: ["deps.get", "ecto.setup"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
      quality: ["format --check-formatted", "credo --strict", "dialyzer"]
    ]
  end
end
```

### 3. Dependency Management

```bash
# Install dependencies
mix deps.get

# Update dependencies
mix deps.update --all
mix deps.update phoenix ecto  # Specific packages

# Check outdated dependencies
mix hex.outdated

# View dependency tree
mix deps.tree

# Clean dependencies
mix deps.clean --all
mix deps.clean --unused

# Compile dependencies
mix deps.compile
```

**Dependency options:**

```elixir
defp deps do
  [
    # Hex package with version constraint
    {:plug, "~> 1.14"},

    # Git repository
    {:plug, git: "https://github.com/elixir-plug/plug.git", tag: "v1.14.0"},
    {:plug, github: "elixir-plug/plug", branch: "main"},

    # Local path
    {:my_lib, path: "../my_lib"},

    # Environment-specific
    {:ex_doc, "~> 0.29", only: :dev, runtime: false},
    {:credo, "~> 1.7", only: [:dev, :test], runtime: false},

    # Optional dependency
    {:jason, "~> 1.4", optional: true},

    # Override dependency
    {:plug, "~> 1.14", override: true}
  ]
end
```

### 4. Common Mix Commands

```bash
# Compilation
mix compile                    # Compile project
mix compile --force           # Force recompilation
mix clean                     # Clean build artifacts

# Testing
mix test                      # Run all tests
mix test test/my_test.exs     # Run specific test file
mix test test/my_test.exs:42  # Run test at line 42
mix test --trace              # Run with detailed trace
mix test --cover              # Run with coverage

# Code Quality
mix format                    # Format code
mix format --check-formatted  # Check if formatted
mix credo                     # Static code analysis
mix dialyzer                  # Type checking

# Interactive Console
mix run -e 'IO.puts("Hello")' # Run expression
iex -S mix                    # Start IEx with project loaded

# Documentation
mix docs                      # Generate HTML docs
mix hex.docs open             # Open published docs

# Releases
mix release                   # Build production release
mix release --overwrite       # Overwrite existing release
```

## Advanced Patterns

### 1. Custom Mix Tasks

```elixir
# lib/mix/tasks/hello.ex
defmodule Mix.Tasks.Hello do
  use Mix.Task

  @shortdoc "Prints hello message"

  @moduledoc """
  This task prints a hello message.

  ## Usage

      mix hello
      mix hello World
  """

  def run(args) do
    # Ensure application started
    Mix.Task.run("app.start")

    name = List.first(args) || "World"
    Mix.shell().info("Hello, #{name}!")
  end
end

# Run with: mix hello Alice
```

**Complex task with options:**

```elixir
defmodule Mix.Tasks.Deploy do
  use Mix.Task

  @shortdoc "Deploy application"

  def run(args) do
    {opts, _, _} = OptionParser.parse(args,
      switches: [environment: :string, version: :string, dry_run: :boolean],
      aliases: [e: :environment, v: :version, d: :dry_run]
    )

    environment = opts[:environment] || "production"
    version = opts[:version] || get_version()
    dry_run = opts[:dry_run] || false

    Mix.shell().info("Deploying version #{version} to #{environment}")

    if dry_run do
      Mix.shell().info("[DRY RUN] Would deploy now")
    else
      do_deploy(environment, version)
    end
  end

  defp get_version do
    Mix.Project.config()[:version]
  end

  defp do_deploy(environment, version) do
    # Actual deployment logic
    :ok
  end
end
```

### 2. Mix Aliases

```elixir
defp aliases do
  [
    # Database
    setup: ["deps.get", "ecto.setup", "assets.setup"],
    "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
    "ecto.reset": ["ecto.drop", "ecto.setup"],

    # Testing
    test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
    "test.watch": ["test.watch --stale"],

    # Code Quality
    quality: [
      "format --check-formatted",
      "credo --strict",
      "dialyzer",
      "test --cover"
    ],

    # CI/CD
    ci: [
      "deps.get --only test",
      "compile --warnings-as-errors",
      "format --check-formatted",
      "credo --strict",
      "test --cover"
    ],

    # Development
    dev: ["setup", "phx.server"],

    # Assets
    "assets.setup": ["cmd --cd assets npm install"],
    "assets.build": ["cmd --cd assets npm run build"],
    "assets.deploy": [
      "cmd --cd assets npm run build",
      "esbuild default --minify",
      "phx.digest"
    ]
  ]
end
```

### 3. Environment-Specific Configuration

```elixir
# config/config.exs
import Config

config :my_app,
  ecto_repos: [MyApp.Repo]

config :logger, level: :info

# Import environment specific config
import_config "#{config_env()}.exs"
```

```elixir
# config/dev.exs
import Config

config :my_app, MyApp.Repo,
  username: "postgres",
  password: "postgres",
  database: "my_app_dev",
  hostname: "localhost",
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

config :logger, level: :debug
```

```elixir
# config/prod.exs
import Config

config :logger, level: :info

# Runtime configuration
# config/runtime.exs
import Config

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
      raise """
      environment variable DATABASE_URL is missing.
      For example: ecto://USER:PASS@HOST/DATABASE
      """

  config :my_app, MyApp.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")

  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      """

  config :my_app, MyAppWeb.Endpoint,
    http: [port: String.to_integer(System.get_env("PORT") || "4000")],
    secret_key_base: secret_key_base
end
```

### 4. Umbrella Projects

```bash
# Create umbrella
mix new my_umbrella --umbrella
cd my_umbrella

# Create apps within umbrella
cd apps
mix new web_app --sup
mix new data_layer --sup
mix new business_logic --sup
```

**Structure:**

```
my_umbrella/
├── apps/
│   ├── web_app/
│   ├── data_layer/
│   └── business_logic/
├── config/
└── mix.exs
```

**Dependencies between apps:**

```elixir
# apps/web_app/mix.exs
defp deps do
  [
    {:business_logic, in_umbrella: true},
    {:phoenix, "~> 1.7"}
  ]
end

# apps/business_logic/mix.exs
defp deps do
  [
    {:data_layer, in_umbrella: true}
  ]
end
```

**Umbrella commands:**

```bash
# Run command in all apps
mix test

# Run command in specific app
mix cmd --app web_app mix test

# Compile specific app
mix compile --app data_layer

# Get dependencies for all apps
mix deps.get
```

## Production Releases

### 1. Mix Release Configuration

```elixir
# mix.exs
def project do
  [
    releases: [
      my_app: [
        include_executables_for: [:unix],
        applications: [runtime_tools: :permanent],
        steps: [:assemble, :tar]
      ],
      my_app_minimal: [
        include_erts: false,
        include_executables_for: [:unix]
      ]
    ]
  ]
end
```

### 2. Building Releases

```bash
# Build release
MIX_ENV=prod mix release

# Build specific release
MIX_ENV=prod mix release my_app_minimal

# Build with overwrite
MIX_ENV=prod mix release --overwrite

# Release location
_build/prod/rel/my_app/
```

### 3. Running Releases

```bash
# Start release
_build/prod/rel/my_app/bin/my_app start

# Start in daemon mode
_build/prod/rel/my_app/bin/my_app daemon

# Remote console
_build/prod/rel/my_app/bin/my_app remote

# Run migrations
_build/prod/rel/my_app/bin/my_app eval "MyApp.Release.migrate()"

# Custom command
_build/prod/rel/my_app/bin/my_app rpc "Application.get_env(:my_app, :version)"
```

### 4. Release Commands

```elixir
# lib/my_app/release.ex
defmodule MyApp.Release do
  @app :my_app

  def migrate do
    load_app()

    for repo <- repos() do
      {:ok, _, _} = Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :up, all: true))
    end
  end

  def rollback(repo, version) do
    load_app()
    {:ok, _, _} = Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :down, to: version))
  end

  defp repos do
    Application.fetch_env!(@app, :ecto_repos)
  end

  defp load_app do
    Application.load(@app)
  end
end
```

## Testing with Mix

### 1. Test Configuration

```elixir
# test/test_helper.exs
ExUnit.start()

# Set test async globally
ExUnit.configure(exclude: [integration: true], timeout: :infinity)

Ecto.Adapters.SQL.Sandbox.mode(MyApp.Repo, :manual)
```

### 2. Running Tests

```bash
# All tests
mix test

# Specific file
mix test test/my_app/accounts_test.exs

# Specific line
mix test test/my_app/accounts_test.exs:42

# Failed tests only
mix test --failed

# Stale tests only
mix test --stale

# With coverage
mix test --cover
mix coveralls.html  # If using excoveralls

# Trace mode (no async)
mix test --trace

# Seed for reproducibility
mix test --seed 12345

# Include excluded tests
mix test --include integration

# Max failures
mix test --max-failures 3
```

## Common Patterns

### 1. Database Tasks

```elixir
defp aliases do
  [
    "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
    "ecto.reset": ["ecto.drop", "ecto.setup"],
    "ecto.migrate": ["ecto.migrate", "ecto.dump"],
    "ecto.rollback": ["ecto.rollback", "ecto.dump"]
  ]
end
```

### 2. Asset Pipeline

```elixir
defp aliases do
  [
    "assets.deploy": [
      "cmd --cd assets npm run build",
      "esbuild default --minify",
      "tailwind default --minify",
      "phx.digest"
    ]
  ]
end
```

### 3. Code Quality Checks

```elixir
defp aliases do
  [
    lint: [
      "compile --warnings-as-errors",
      "format --check-formatted",
      "credo --strict"
    ],
    "lint.fix": [
      "format",
      "credo --strict --fix"
    ]
  ]
end
```

## Performance Tips

### 1. Compilation

```bash
# Parallel compilation
MIX_ENV=prod mix compile --force --jobs 4

# Compiler diagnostics
mix compile --warnings-as-errors
mix compile --all-warnings
```

### 2. Dependency Resolution

```elixir
# Lock exact versions
# mix.lock is committed to version control

# Update cautiously
mix deps.update specific_package  # Not --all
```

### 3. Build Caching

```bash
# CI caching strategy
_build/
deps/
```

## Common Pitfalls

### 1. Forgetting to Run mix deps.get

```bash
# Always after cloning or pulling dependency changes
mix deps.get
```

### 2. Not Cleaning After Major Changes

```bash
# Clean build artifacts
mix clean
mix deps.clean --all
mix compile
```

### 3. Wrong Environment

```bash
# Ensure correct environment
MIX_ENV=prod mix compile  # Production
MIX_ENV=test mix test     # Test (default for test)
mix compile               # Development (default)
```

## Troubleshooting

### 1. Dependency Conflicts

```bash
# View dependency tree
mix deps.tree

# Unlock specific dependency
mix deps.unlock phoenix

# Clean and reinstall
mix deps.clean --all
mix deps.get
```

### 2. Compilation Errors

```bash
# Force clean rebuild
mix do clean, compile --force

# Check for warnings
mix compile --warnings-as-errors
```

### 3. Release Issues

```bash
# Debug release
_build/prod/rel/my_app/bin/my_app version

# Check runtime config
_build/prod/rel/my_app/bin/my_app eval "Application.get_all_env(:my_app)"
```

## Related Resources

- [Configuration Guide](/en/learn/swe/prog-lang/elixir/how-to/configuration)
- [Deployment Guide](/en/learn/swe/prog-lang/elixir/how-to/deployment)
- [Dependencies Guide](/en/learn/swe/prog-lang/elixir/how-to/dependencies)
- [Testing Guide](/en/learn/swe/prog-lang/elixir/how-to/testing)
