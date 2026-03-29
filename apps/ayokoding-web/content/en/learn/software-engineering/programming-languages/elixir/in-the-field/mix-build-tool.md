---
title: "Mix Build Tool"
date: 2026-02-05T00:00:00+07:00
draft: false
description: "Build automation with Mix from manual compilation to production-ready builds with dependencies, custom tasks, and umbrella projects"
weight: 1000033
tags: ["elixir", "mix", "build-tools", "dependencies", "hex"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/distributed-systems"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/hex-package-management"
---

## Why Build Tools Matter

Mix is Elixir's built-in build tool that automates compilation, dependency management, testing, and deployment. Understanding Mix fundamentals is essential for production Elixir development.

**Core benefits**:

- **Automation**: Single command for compile, test, and release
- **Dependency management**: Automatic resolution via Hex package manager
- **Task system**: Extensible with custom Mix tasks
- **Project conventions**: Standard structure across all Elixir projects
- **Development workflow**: Integrated testing, documentation, and code formatting

**Problem**: Manual compilation with `elixirc` requires managing dependencies, build steps, test execution, and releases by hand. This becomes unmaintainable as projects grow.

**Solution**: Mix provides production-ready build automation with dependency resolution, task orchestration, and release management built-in.

## Manual Compilation: elixirc

Elixir provides `elixirc` compiler for manual compilation. Understanding this shows what Mix automates.

**Basic compilation**:

```bash
# Compile single file
elixirc hello.ex
# => Compiles hello.ex to Elixir.Hello.beam
# => Output: BEAM bytecode file
# => No output directory control

# Execute compiled module
iex
# => Starts Elixir interactive shell
# => Loads .beam files from current directory

iex> Hello.greet()
# => Calls function from compiled module
# => Output: "Hello, World!"
```

**Module example**:

```elixir
# File: hello.ex
defmodule Hello do
  # => Defines module Hello
  # => Module name becomes Elixir.Hello internally

  def greet do
    # => Public function definition
    # => Type: () -> String.t()
    IO.puts("Hello, World!")
    # => Prints to stdout
    # => Returns :ok
  end
end
```

```bash
elixirc hello.ex
# => Compiles to Elixir.Hello.beam
# => File created in current directory
# => No build organization

ls *.beam
# => Output: Elixir.Hello.beam
# => BEAM bytecode for Erlang VM
```

**Multiple files**:

```bash
# Compile multiple files
elixirc math.ex calculator.ex
# => Compiles both files
# => Creates Elixir.Math.beam and Elixir.Calculator.beam
# => No dependency tracking between modules

# Output directory (-o flag)
mkdir -p build
elixirc -o build/ hello.ex math.ex
# => Compiles to build/ directory
# => Better organization than current directory
```

**With dependencies** (manual management):

```elixir
# File: app.ex
defmodule App do
  def run do
    # => Attempts to use external library
    Jason.encode!(%{message: "Hello"})
    # => Requires jason library
    # => Must be manually compiled and available
  end
end
```

```bash
# Download dependency manually
git clone https://github.com/michalmuskala/jason.git deps/jason
# => Clones jason library to deps/
# => Manual version management

# Compile dependency
cd deps/jason
elixirc -o ../../build/jason lib/*.ex
# => Compiles jason to build/jason/
# => Manual compilation of dependencies
cd ../..

# Compile application with dependency
elixirc -pa build/jason -o build/ app.ex
# => -pa adds build/jason to code path
# => Finds compiled jason modules
# => Manual path management
```

## Limitations of Manual Compilation

**Why elixirc doesn't scale**:

1. **No dependency resolution**: Must manually download and compile dependencies
2. **No transitive dependencies**: Dependencies of dependencies require manual tracking
3. **No version management**: No conflict resolution or version pinning
4. **No build lifecycle**: Must manually orchestrate compile → test → release
5. **No incremental compilation**: Always recompiles all files
6. **No project structure**: No conventions for organizing code
7. **No testing framework integration**: Must manually run tests
8. **No release management**: No production release creation

**Before**: Manual `elixirc` with shell scripts and manual dependency management
**After**: Mix with automated dependency resolution and build lifecycle

## Mix: Elixir's Build Tool

Mix is built into Elixir, providing production-ready build automation without external tools.

### Creating a Mix Project

Mix enforces standard project structure with conventions.

**Initialize project**:

```bash
mix new myapp
# => Creates new Mix project
# => Standard directory structure
# => Generates mix.exs configuration

cd myapp
tree
# => Output:
# myapp/
# ├── mix.exs              # Project configuration
# ├── README.md            # Project documentation
# ├── .formatter.exs       # Code formatter config
# ├── .gitignore          # Git ignore file
# ├── lib/                # Application source code
# │   └── myapp.ex
# └── test/               # Test files
#     ├── myapp_test.exs
#     └── test_helper.exs
```

**Project types**:

```bash
# Library project (default)
mix new mylib
# => Creates library project
# => No supervision tree

# Application project (with supervision)
mix new myapp --sup
# => Creates OTP application
# => Includes Application module
# => Supervision tree for production

# Umbrella project (multi-app)
mix new myumbrella --umbrella
# => Creates umbrella project structure
# => Multiple apps in apps/ directory
```

### mix.exs Configuration

The `mix.exs` file defines project metadata, dependencies, and build configuration.

**Basic structure**:

```elixir
# File: mix.exs
defmodule Myapp.MixProject do
  # => Mix project module
  # => Defines project configuration
  use Mix.Project
  # => Imports Mix.Project behavior
  # => Provides project/0 callback

  def project do
    # => Returns project configuration
    # => Type: keyword list
    [
      app: :myapp,
      # => Application name (atom)
      # => Used for releases and dependencies

      version: "0.1.0",
      # => Semantic version string
      # => Format: major.minor.patch

      elixir: "~> 1.17",
      # => Elixir version requirement
      # => ~> 1.17 means >= 1.17.0 and < 2.0.0

      start_permanent: Mix.env() == :prod,
      # => Start application as permanent in production
      # => Crashes halt the VM (fail-fast)

      deps: deps()
      # => Dependencies function
      # => Returns list of dependencies
    ]
  end

  def application do
    # => Application configuration
    # => Defines OTP application behavior
    [
      extra_applications: [:logger]
      # => Start :logger application automatically
      # => Logger is built-in logging system
    ]
  end

  defp deps do
    # => Private function returning dependencies
    # => Type: list({atom(), String.t()} | {atom(), String.t(), keyword()})
    [
      # Dependency examples will follow
    ]
  end
end
```

### Mix Tasks and Build Lifecycle

Mix provides built-in tasks for common development operations.

**Core tasks**:

```bash
# Compile project
mix compile
# => Compiles lib/ directory to _build/dev/lib/myapp/ebin/
# => Incremental compilation (only changed files)
# => Creates .beam files

# Run tests
mix test
# => Compiles test files
# => Runs ExUnit test suite
# => Reports test results

# Run interactive shell with project loaded
iex -S mix
# => Starts IEx with project compiled and loaded
# => All modules available for interactive use
# => Recompile with recompile()

# Format code
mix format
# => Formats code according to .formatter.exs
# => Consistent style across project
# => Modifies files in-place

# Generate documentation
mix docs
# => Generates HTML documentation with ExDoc
# => Requires {:ex_doc, "~> 0.31", only: :dev} dependency
# => Output: doc/ directory
```

**Build environments**:

```bash
# Development environment (default)
mix compile
# => MIX_ENV=dev (default)
# => Includes development dependencies
# => Compiled to _build/dev/

# Test environment
MIX_ENV=test mix compile
# => Includes test dependencies
# => Compiled to _build/test/

# Production environment
MIX_ENV=prod mix compile
# => Optimized compilation
# => No dev/test dependencies
# => Compiled to _build/prod/
```

**Task listing**:

```bash
mix help
# => Lists all available Mix tasks
# => Includes built-in and custom tasks
# => Shows task descriptions

mix help compile
# => Shows detailed help for compile task
# => Describes flags and options
```

## Dependency Management with Hex

Mix integrates with Hex, Elixir's package manager, for dependency resolution.

### Adding Dependencies

Dependencies are declared in `mix.exs` and automatically downloaded.

**Common dependencies**:

```elixir
defp deps do
  [
    # JSON encoding/decoding
    {:jason, "~> 1.4"},
    # => Version: >= 1.4.0 and < 2.0.0
    # => Hex package :jason
    # => Downloaded from hex.pm

    # HTTP client
    {:httpoison, "~> 2.2"},
    # => HTTP client library
    # => Transitive dependencies handled automatically

    # Database wrapper
    {:ecto_sql, "~> 3.11"},
    # => SQL database toolkit
    # => Includes Ecto.Repo, migrations, queries

    # PostgreSQL adapter
    {:postgrex, ">= 0.0.0"},
    # => PostgreSQL driver for Ecto
    # => >= 0.0.0 allows any version

    # Phoenix web framework
    {:phoenix, "~> 1.7"},
    # => Full-featured web framework
    # => Many transitive dependencies

    # Development-only dependencies
    {:ex_doc, "~> 0.31", only: :dev, runtime: false},
    # => only: :dev means dev environment only
    # => runtime: false means not included in releases

    # Test-only dependencies
    {:mox, "~> 1.1", only: :test}
    # => Mock library for tests
    # => only: :test means test environment only
  ]
end
```

**Version requirements**:

| Syntax     | Meaning                             | Example        |
| ---------- | ----------------------------------- | -------------- |
| `~> 1.4`   | >= 1.4.0 and < 2.0.0                | 1.4.3, 1.9.0   |
| `~> 1.4.1` | >= 1.4.1 and < 1.5.0                | 1.4.2, 1.4.9   |
| `>= 1.0.0` | Any version >= 1.0.0                | 1.0.0, 2.0.0   |
| `== 1.4.0` | Exact version 1.4.0 only            | 1.4.0          |
| `or: true` | Optional dependency (user must add) | Not downloaded |

**Git dependencies**:

```elixir
defp deps do
  [
    # From git repository
    {:my_lib, git: "https://github.com/user/my_lib.git"},
    # => Clones from git repository
    # => Uses default branch

    # Specific branch
    {:my_lib, git: "https://github.com/user/my_lib.git", branch: "develop"},
    # => Uses develop branch

    # Specific tag
    {:my_lib, git: "https://github.com/user/my_lib.git", tag: "v1.0.0"},
    # => Uses git tag v1.0.0

    # Specific commit
    {:my_lib, git: "https://github.com/user/my_lib.git", ref: "abc123"}
    # => Uses specific commit SHA
  ]
end
```

**Path dependencies** (local development):

```elixir
defp deps do
  [
    # Local path dependency
    {:my_lib, path: "../my_lib"},
    # => Uses local directory
    # => Useful for development
    # => Not suitable for releases

    # In umbrella projects
    {:my_lib, in_umbrella: true}
    # => Dependency in same umbrella project
    # => Automatic path resolution
  ]
end
```

### Dependency Operations

Mix automates dependency fetching, compilation, and updates.

**Fetch dependencies**:

```bash
mix deps.get
# => Downloads dependencies from Hex
# => Clones git dependencies
# => Creates deps/ directory
# => Generates mix.lock file

tree deps/
# => Output:
# deps/
# ├── jason/          # Downloaded jason package
# ├── httpoison/      # Downloaded httpoison package
# └── hackney/        # Transitive dependency of httpoison
```

**Compile dependencies**:

```bash
mix deps.compile
# => Compiles all dependencies
# => Output: _build/dev/lib/*/ebin/
# => Only recompiles changed dependencies

# Force recompile specific dependency
mix deps.compile jason --force
# => Recompiles jason even if unchanged
```

**Update dependencies**:

```bash
# Update all dependencies
mix deps.update --all
# => Updates to latest versions matching requirements
# => Updates mix.lock with new versions

# Update specific dependency
mix deps.update jason
# => Updates only jason (and its dependencies)
```

**Clean dependencies**:

```bash
# Remove compiled dependencies
mix deps.clean --all
# => Removes _build/*/lib/*/ebin/
# => Keeps source in deps/

# Remove unused dependencies
mix deps.unlock --unused
# => Removes dependencies not in mix.exs
# => Cleans mix.lock
```

### mix.lock Version Locking

Mix generates `mix.lock` to ensure reproducible builds.

**Lock file example**:

```elixir
# File: mix.lock (auto-generated)
%{
  "jason": {:hex, :jason, "1.4.1", "sha256hash...", [:mix], [], "hexpm", "hexhash"},
  # => Locked to jason 1.4.1
  # => SHA-256 hash for integrity verification
  # => Build tool: :mix
  # => No dependencies for jason

  "httpoison": {:hex, :httpoison, "2.2.1", "sha256hash...", [:mix], [{:hackney, "~> 1.17"}], "hexpm", "hexhash"},
  # => Locked to httpoison 2.2.1
  # => Depends on hackney ~> 1.17

  "hackney": {:hex, :hackney, "1.20.1", "sha256hash...", [:rebar3], [...], "hexpm", "hexhash"}
  # => Transitive dependency of httpoison
  # => Build tool: :rebar3 (Erlang build tool)
}
```

**Lock file behavior**:

```bash
# First developer
mix deps.get
# => Downloads dependencies
# => Generates mix.lock

# Second developer (later)
git pull
# => Gets mix.lock from repository
mix deps.get
# => Downloads EXACT versions from mix.lock
# => Reproducible builds across team
```

**Lock file management**:

```bash
# Update mix.lock after changing mix.exs
mix deps.get
# => Updates lock file with new dependencies

# Commit mix.lock to version control
git add mix.lock
git commit -m "feat: add jason dependency"
# => Ensures team uses same versions
# => Required for reproducible builds
```

## Custom Mix Tasks

Mix is extensible with custom tasks for project-specific automation.

### Creating Custom Tasks

Define tasks as modules in `lib/mix/tasks/`.

**Basic custom task**:

```elixir
# File: lib/mix/tasks/hello.ex
defmodule Mix.Tasks.Hello do
  # => Custom Mix task module
  # => Naming: Mix.Tasks.{TaskName}
  use Mix.Task
  # => Imports Mix.Task behavior
  # => Provides run/1 callback

  @shortdoc "Prints hello message"
  # => Short description for mix help
  # => Shown in task listing

  @moduledoc """
  Prints a hello message to the console.

  ## Usage

      mix hello

  ## Options

      --name - Name to greet (default: World)
  """
  # => Full documentation
  # => Shown with mix help hello

  def run(args) do
    # => Entry point for task
    # => args: list of command-line arguments
    {opts, _, _} = OptionParser.parse(args, switches: [name: :string])
    # => Parses command-line flags
    # => opts: parsed options as keyword list

    name = opts[:name] || "World"
    # => Gets --name flag or defaults to "World"

    Mix.shell().info("Hello, #{name}!")
    # => Prints to shell
    # => Mix.shell() abstraction for testing
  end
end
```

**Run custom task**:

```bash
mix hello
# => Output: Hello, World!

mix hello --name Elixir
# => Output: Hello, Elixir!
```

### Production Task Example: Seeding Database

Custom tasks for production operations like database seeding.

**Database seed task**:

```elixir
# File: lib/mix/tasks/seed.ex
defmodule Mix.Tasks.Seed do
  # => Custom task for database seeding
  use Mix.Task
  # => Mix task behavior

  @shortdoc "Seeds the database with initial data"

  def run(_args) do
    # => Task entry point
    Mix.Task.run("app.start")
    # => Starts OTP application
    # => Ensures Repo is running

    alias Myapp.Repo
    alias Myapp.Accounts.User
    # => Aliases for brevity

    users = [
      %{email: "admin@example.com", role: :admin},
      # => User data structure
      %{email: "user@example.com", role: :user}
    ]
    # => Seed data

    Enum.each(users, fn user_data ->
      # => Iterates over seed data
      case Repo.get_by(User, email: user_data.email) do
        # => Checks if user exists
        nil ->
          # => User doesn't exist
          %User{}
          |> User.changeset(user_data)
          # => Creates changeset
          |> Repo.insert!()
          # => Inserts into database
          # => insert! raises on error

          Mix.shell().info("Created user: #{user_data.email}")
          # => Logs creation

        _user ->
          # => User exists
          Mix.shell().info("User already exists: #{user_data.email}")
          # => Logs skip
      end
    end)

    Mix.shell().info("Seeding complete!")
  end
end
```

**Run seed task**:

```bash
MIX_ENV=prod mix seed
# => Runs in production environment
# => Starts application
# => Seeds production database
# => Idempotent (safe to run multiple times)
```

## Mix Aliases: Task Composition

Aliases combine multiple tasks into single command.

**Define aliases in mix.exs**:

```elixir
def project do
  [
    # ... other config
    aliases: aliases()
    # => Calls aliases/0 function
  ]
end

defp aliases do
  # => Returns keyword list of alias definitions
  [
    # Setup alias
    setup: ["deps.get", "ecto.setup"],
    # => Runs: mix deps.get, then mix ecto.setup
    # => Type: list(String.t())

    # Ecto setup
    "ecto.setup": ["ecto.create", "ecto.migrate", "seed"],
    # => Creates DB, runs migrations, seeds data
    # => Calls custom seed task

    # Ecto reset
    "ecto.reset": ["ecto.drop", "ecto.setup"],
    # => Drops database and recreates
    # => Useful for development reset

    # Test setup
    test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
    # => Overrides built-in test task
    # => Ensures test database is ready
    # => --quiet suppresses output

    # Quality checks
    quality: ["format --check-formatted", "credo --strict", "dialyzer"],
    # => Checks code format, runs static analysis
    # => Good for CI pipelines

    # Deploy
    deploy: ["compile", "assets.deploy", "phx.digest", "release"]
    # => Production deployment steps
    # => Compiles, processes assets, creates release
  ]
end
```

**Run aliases**:

```bash
# Setup project
mix setup
# => Runs deps.get, ecto.create, ecto.migrate, seed
# => Single command for full setup

# Reset database
mix ecto.reset
# => Drops and recreates database
# => Useful when migrations broken

# Run quality checks
mix quality
# => Format check, Credo analysis, Dialyzer
# => Pre-commit validation
```

## Production: Releases with Mix

Mix creates production releases with `mix release` (built-in since Elixir 1.9).

**Release configuration**:

```elixir
# File: mix.exs
def project do
  [
    # ... other config
    releases: [
      myapp: [
        # => Release name
        include_executables_for: [:unix],
        # => Creates shell scripts for Unix
        # => Excludes Windows .bat files

        applications: [
          myapp: :permanent
          # => Starts myapp application as permanent
          # => Crashes halt the VM
        ],

        steps: [:assemble, :tar],
        # => Build steps
        # => :assemble creates release directory
        # => :tar creates tarball for distribution

        strip_beams: true
        # => Removes debug info from BEAM files
        # => Smaller release size
      ]
    ]
  ]
end
```

**Create release**:

```bash
# Build production release
MIX_ENV=prod mix release
# => Compiles with MIX_ENV=prod
# => Creates _build/prod/rel/myapp/
# => Self-contained release
# => Includes ERTS (Erlang Runtime System)

# Release structure
tree _build/prod/rel/myapp/
# => Output:
# _build/prod/rel/myapp/
# ├── bin/
# │   ├── myapp          # Start script
# │   └── myapp.bat      # Windows start script
# ├── erts-14.2.5/       # Erlang runtime
# ├── lib/               # Application and dependencies
# └── releases/
#     └── 0.1.0/         # Release version
```

**Run release**:

```bash
# Start release
_build/prod/rel/myapp/bin/myapp start
# => Starts application as daemon
# => Runs in background

# Start with IEx console
_build/prod/rel/myapp/bin/myapp start_iex
# => Starts with interactive shell
# => Useful for production debugging

# Remote console to running release
_build/prod/rel/myapp/bin/myapp remote
# => Connects to running release
# => Full remote debugging

# Stop release
_build/prod/rel/myapp/bin/myapp stop
# => Gracefully stops application
```

**Containerized deployment** (Docker):

```dockerfile
# File: Dockerfile
# Stage 1: Build
FROM elixir:1.17-alpine AS builder

# Install build dependencies
RUN apk add --no-cache build-base git
# => build-base: C compiler for NIFs
# => git: for git dependencies

WORKDIR /app

# Copy mix files
COPY mix.exs mix.lock ./
# => Dependency configuration
RUN mix local.hex --force && \
    mix local.rebar --force
# => Installs Hex and Rebar3

# Fetch dependencies
RUN mix deps.get --only prod
# => Downloads production dependencies only
# => Caches dependency layer

# Copy source
COPY lib lib/
COPY config config/
# => Application source and configuration

# Build release
RUN MIX_ENV=prod mix release
# => Creates production release
# => Output: _build/prod/rel/myapp/

# Stage 2: Runtime
FROM alpine:3.19 AS app

# Install runtime dependencies
RUN apk add --no-cache libstdc++ openssl ncurses-libs
# => libstdc++: C++ standard library (for ERTS)
# => openssl: SSL/TLS support
# => ncurses-libs: terminal handling

WORKDIR /app

# Copy release from builder
COPY --from=builder /app/_build/prod/rel/myapp ./
# => Copies only release artifacts
# => Small final image (~50MB)

# Run release
CMD ["bin/myapp", "start"]
# => Starts application
# => Runs in foreground for Docker
```

**Build and run Docker image**:

```bash
# Build image
docker build -t myapp:latest .
# => Builds multi-stage image
# => Final image size: ~50MB

# Run container
docker run -p 4000:4000 myapp:latest
# => Starts application
# => Exposes port 4000
```

## Umbrella Projects: Multi-App Organization

Umbrella projects organize multiple related applications in single repository.

**Create umbrella**:

```bash
mix new donations_platform --umbrella
# => Creates umbrella project
cd donations_platform

tree -L 2
# => Output:
# donations_platform/
# ├── mix.exs               # Umbrella config
# ├── apps/                 # Applications directory
# └── config/               # Shared configuration
```

**Add applications to umbrella**:

```bash
cd apps

# Core domain logic
mix new core --sup
# => OTP application with supervision
# => Business logic, domain models

# Web API
mix new web --sup
# => Phoenix web application
# => HTTP API endpoints

# Background jobs
mix new worker --sup
# => Background job processor
# => Async tasks

cd ..
tree -L 2 apps/
# => Output:
# apps/
# ├── core/
# │   ├── mix.exs
# │   └── lib/
# ├── web/
# │   ├── mix.exs
# │   └── lib/
# └── worker/
#     ├── mix.exs
#     └── lib/
```

**Inter-app dependencies**:

```elixir
# File: apps/web/mix.exs
defp deps do
  [
    # Depend on core app in umbrella
    {:core, in_umbrella: true},
    # => in_umbrella: true for umbrella dependencies
    # => Automatic path resolution

    # External dependencies
    {:phoenix, "~> 1.7"},
    {:plug_cowboy, "~> 2.7"}
  ]
end
```

**Umbrella operations**:

```bash
# From umbrella root

# Compile all apps
mix compile
# => Compiles apps in dependency order
# => core before web (web depends on core)

# Run tests for all apps
mix test
# => Runs test suites for all apps
# => Aggregated results

# Run specific app tests
mix test apps/web/test
# => Tests only web app

# Start specific app
iex -S mix run --no-start
iex> Application.ensure_all_started(:web)
# => Starts web app and dependencies
# => core started automatically
```

**Umbrella benefits**:

- **Modularity**: Clear boundaries between applications
- **Shared dependencies**: Single deps/ directory
- **Incremental compilation**: Only changed apps recompile
- **Independent deployment**: Deploy apps separately if needed
- **Team scaling**: Teams own specific apps

## Best Practices

**Always commit mix.lock**:

```bash
git add mix.lock
git commit -m "chore: update dependencies"
# => Ensures reproducible builds
# => Team uses exact versions
```

**Use aliases for common workflows**:

```elixir
defp aliases do
  [
    setup: ["deps.get", "ecto.setup"],
    ci: ["format --check-formatted", "test", "credo"]
  ]
end
```

**Pin production dependencies**:

```elixir
# Avoid >= 0.0.0 in production
{:postgrex, ">= 0.0.0"}  # ❌ Too permissive

# Use ~> for stability
{:postgrex, "~> 0.17"}   # ✅ Stable updates only
```

**Separate dev/test dependencies**:

```elixir
{:ex_doc, "~> 0.31", only: :dev, runtime: false},
{:credo, "~> 1.7", only: [:dev, :test], runtime: false}
# => only: environment restriction
# => runtime: false excludes from releases
```

**Use umbrella projects for large systems**:

- **Single service**: Regular Mix project
- **Multiple services**: Umbrella project with shared core
- **Microservices**: Separate repositories (not umbrella)

## Summary

Mix provides complete build automation:

- **Standard library**: `elixirc` for manual compilation (learning only)
- **Limitations**: No dependency resolution, no build lifecycle
- **Mix fundamentals**: `mix new`, `mix.exs`, `mix compile`, `mix test`
- **Dependencies**: Hex integration, `mix deps.get`, `mix.lock` locking
- **Custom tasks**: Extend Mix for production operations
- **Aliases**: Combine tasks for workflows
- **Releases**: `mix release` for production deployments
- **Umbrella projects**: Multi-app organization

**Progressive adoption**:

1. Start with `mix new` for project structure
2. Add dependencies to `mix.exs`
3. Create custom tasks for production operations
4. Define aliases for common workflows
5. Use `mix release` for production builds
6. Consider umbrella projects for large systems

**Production build example** (donation platform):

```bash
# Full production build
MIX_ENV=prod mix do deps.get, compile, release
# => Fetches prod dependencies
# => Compiles with optimizations
# => Creates self-contained release
```

**Next steps**: Explore Hex package management for publishing libraries, umbrella projects for complex systems, and hot code upgrades for zero-downtime deployments.
