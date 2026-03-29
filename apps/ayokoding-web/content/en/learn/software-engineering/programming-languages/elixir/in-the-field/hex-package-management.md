---
title: "Hex Package Management"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000034
description: "Package dependency management with Hex and Mix for production Elixir applications"
tags: ["elixir", "hex", "mix", "dependencies", "package-management"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/mix-build-tool"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/umbrella-projects"
---

**Need to use external libraries in Elixir?** This guide teaches Hex package management through the OTP-First progression, starting with manual dependency copying to understand versioning challenges before introducing Hex's semantic versioning and transitive dependency resolution.

## Why Hex Matters

Production applications depend on external libraries:

- **Web frameworks** - Phoenix for web applications, REST APIs
- **Database libraries** - Ecto for database access, query building
- **JSON processing** - Jason for API serialization, data parsing
- **Testing tools** - ExUnit for testing, Mox for mocking
- **Utilities** - Timex for datetime, Decimal for precise arithmetic

Elixir provides package management through:

1. **Manual copying** - Copy .ex files into project (no version control)
2. **Hex package manager** - Central repository with semantic versioning (production standard)

**Our approach**: Start with manual dependency copying to understand version conflicts, then see how Hex solves them with dependency resolution and semantic versioning.

## OTP Primitives - Manual Dependency Copying

### Copying External Code

Let's add a JSON library manually:

```elixir
# Manual JSON library (simplified)
# File: lib/manual_json.ex

defmodule ManualJSON do
  # Encode Elixir data to JSON string
  def encode(data) do
    case data do
      nil ->
        "null"                                       # => JSON null literal
                                                     # => No quotes for null

      true -> "true"                                 # => JSON boolean literal
                                                     # => String "true" (not atom)
      false -> "false"                               # => JSON boolean literal
                                                     # => String "false" (not atom)

      num when is_number(num) ->
        to_string(num)                               # => Convert number to string
                                                     # => JSON numbers are strings

      str when is_binary(str) ->
        "\"#{escape_string(str)}\""                  # => JSON string with quotes
                                                     # => Escaped special characters
                                                     # => Returns: "escaped_value"

      list when is_list(list) ->
        items = Enum.map(list, &encode/1)            # => Recursively encode elements
                                                     # => items: List of JSON strings
        "[#{Enum.join(items, ",")}]"                 # => JSON array format
                                                     # => Comma-separated values

      map when is_map(map) ->
        pairs = Enum.map(map, fn {k, v} ->
          "\"#{k}\":#{encode(v)}"                    # => Format key-value pair
                                                     # => "key":value syntax
        end)
        "{#{Enum.join(pairs, ",")}}"                 # => JSON object format
                                                     # => Curly braces with pairs
    end
  end
  # => Returns: JSON string representation
  # => Handles all basic Elixir types

  defp escape_string(str) do
    str
    |> String.replace("\\", "\\\\")                  # => Escape backslashes first
                                                     # => \ becomes \\
    |> String.replace("\"", "\\\"")                  # => Escape double quotes
                                                     # => " becomes \"
    |> String.replace("\n", "\\n")                   # => Escape newlines
                                                     # => Newline becomes \n
  end
  # => Returns: Escaped string safe for JSON
end

# Usage
ManualJSON.encode(%{name: "Alice", age: 30})         # => "{\"name\":\"Alice\",\"age\":30}"
ManualJSON.encode([1, 2, 3])                         # => "[1,2,3]"
ManualJSON.encode(nil)                               # => "null"
```

### Version Management Problem

What happens when the library updates?

```elixir
# Original version: lib/manual_json.ex (v1.0)
defmodule ManualJSON do
  def encode(data), do: # ... implementation
end

# Developer updates library manually to v2.0
# New API: encode/2 with options
defmodule ManualJSON do
  def encode(data, opts \\ []) do
    # New implementation with options support
  end
end
# => Breaking change: Different function signature
# => Old code: ManualJSON.encode(data) still works (default opts)
# => But behavior may change unexpectedly

# Multiple files use old API
# lib/api_controller.ex
ManualJSON.encode(response)                          # => Which version semantics?

# lib/log_formatter.ex
ManualJSON.encode(log_data)                          # => Which version semantics?

# No way to track which version assumptions code makes!
```

### Dependency Conflicts

Two libraries need different versions:

```elixir
# Project structure with manual dependencies
# lib/
#   manual_json.ex          (v1.0)
#   http_client.ex          (depends on manual_json v1.0)
#   websocket_handler.ex    (depends on manual_json v2.0)

# http_client.ex expects v1.0 API
defmodule HTTPClient do
  def send(data) do
    body = ManualJSON.encode(data)                   # => Expects v1.0 behavior
    # ... HTTP request
  end
end

# websocket_handler.ex expects v2.0 API
defmodule WebSocketHandler do
  def broadcast(data) do
    json = ManualJSON.encode(data, pretty: true)     # => Expects v2.0 with options
    # ... WebSocket broadcast
  end
end

# CONFLICT: Can only have one version of manual_json.ex!
# => Either http_client breaks or websocket_handler breaks
# => No way to use both v1.0 and v2.0 simultaneously
```

## Hex Package Manager

### Installing from Hex.pm

Hex provides centralized package repository:

```elixir
# mix.exs - Dependency specification
defmodule MyApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :myapp,                                   # => Application name
      version: "0.1.0",                              # => Application version
      elixir: "~> 1.14",                             # => Elixir version requirement
      deps: deps()                                   # => Dependency function
    ]
  end

  defp deps do
    [
      {:jason, "~> 1.4"},                            # => JSON library from Hex
                                                     # => ~> 1.4: Semantic version constraint
                                                     # => Allows: 1.4.x, 1.5.x, etc.
                                                     # => Blocks: 2.0.0 (breaking changes)

      {:phoenix, "~> 1.7"},                          # => Web framework
      {:ecto, "~> 3.10"},                            # => Database wrapper
      {:plug, "~> 1.14"}                             # => Web server interface
    ]
  end
  # => Hex resolves all transitive dependencies automatically
end

# Terminal: Install dependencies
$ mix deps.get
# => Resolves dependency tree
# => Downloads packages from hex.pm
# => Compiles dependencies
# => Creates mix.lock file (exact versions)
```

### Semantic Versioning

Hex uses SemVer format: MAJOR.MINOR.PATCH

```elixir
# Version constraints in mix.exs
defp deps do
  [
    # Exact version
    {:jason, "1.4.0"},                               # => Only 1.4.0 allowed
                                                     # => Too restrictive

    # Pessimistic constraint (recommended)
    {:jason, "~> 1.4.0"},                            # => Allows: 1.4.0, 1.4.1, 1.4.2
                                                     # => Blocks: 1.5.0 (minor bump)
                                                     # => Patch updates only

    {:phoenix, "~> 1.7"},                            # => Allows: 1.7.x, 1.8.x, 1.9.x
                                                     # => Blocks: 2.0.0 (major bump)
                                                     # => Minor updates allowed

    # Greater than or equal
    {:ecto, ">= 3.10.0"},                            # => Any version >= 3.10.0
                                                     # => Dangerous: Allows breaking changes

    # Range constraint
    {:plug, ">= 1.14.0 and < 2.0.0"},                # => Explicit range
  ]
end

# SemVer guarantees:
# MAJOR: Breaking changes (1.x -> 2.x)
# MINOR: New features, backward compatible (1.1 -> 1.2)
# PATCH: Bug fixes, backward compatible (1.1.0 -> 1.1.1)
```

### Dependency Resolution

Hex resolves transitive dependencies automatically:

```elixir
# mix.exs - Direct dependencies only
defp deps do
  [
    {:phoenix, "~> 1.7"},                            # => Direct: Web framework
    {:ecto, "~> 3.10"}                               # => Direct: Database library
  ]
end

# mix deps.tree - Shows full dependency tree
$ mix deps.tree
myapp
├── phoenix 1.7.10                                   # => Direct dependency
│   ├── plug 1.15.3                                  # => Transitive: Phoenix needs Plug
│   ├── plug_crypto 2.0.0                            # => Transitive: Plug needs plug_crypto
│   ├── phoenix_pubsub 2.1.3                         # => Transitive: Phoenix pub/sub
│   └── phoenix_view 2.0.3                           # => Transitive: Template rendering
└── ecto 3.10.3                                      # => Direct dependency
    ├── decimal 2.1.1                                # => Transitive: Ecto precision math
    ├── jason 1.4.1                                  # => Transitive: Ecto JSON encoding
    └── telemetry 1.2.1                              # => Transitive: Ecto metrics

# Hex automatically:
# 1. Resolves compatible versions across all dependencies
# 2. Downloads transitive dependencies (you don't specify them)
# 3. Detects conflicts and suggests solutions
# 4. Locks exact versions in mix.lock
```

### Conflict Resolution

When dependencies conflict, Hex reports the issue:

```elixir
# mix.exs
defp deps do
  [
    {:phoenix, "~> 1.7"},                            # => Needs plug ~> 1.14
    {:some_plugin, "~> 2.0"}                         # => Needs plug ~> 1.10
  ]
end

# mix deps.get
$ mix deps.get
Resolving Hex dependencies...
# => Hex finds compatible plug version that satisfies both
# => If impossible, reports conflict:

Failed to use "plug" (version 1.15.3) because
  phoenix 1.7.10 requires ~> 1.14
  some_plugin 2.0.0 requires ~> 1.10
# => Both requirements can be satisfied by plug 1.14.x or 1.15.x
# => Hex picks 1.15.3 (latest compatible)

# Conflict example (no resolution):
defp deps do
  [
    {:library_a, "~> 1.0"},                          # => Needs jason ~> 1.4
    {:library_b, "~> 2.0"}                           # => Needs jason ~> 1.2
  ]
end
# => Hex resolves to jason 1.4.1 (satisfies both)

defp deps do
  [
    {:library_a, "~> 1.0"},                          # => Needs jason ~> 1.4
    {:library_b, "~> 2.0"}                           # => Needs jason < 1.4
  ]
end
# => CONFLICT: No version satisfies both
# => Solution: Update library_b or find alternative
```

## Production Use - Publishing Packages

### Creating Publishable Package

Publishing a Zakat calculation library to hex.pm:

```elixir
# mix.exs - Package configuration
defmodule ZakatCalculator.MixProject do
  use Mix.Project

  def project do
    [
      app: :zakat_calculator,                        # => Package name on Hex
      version: "1.0.0",                              # => Initial release
      elixir: "~> 1.14",                             # => Minimum Elixir version
      description: "Islamic Zakat calculation library with multiple asset types",
      package: package(),                            # => Hex package metadata
      deps: deps()
    ]
  end

  defp package do
    [
      name: "zakat_calculator",                      # => Hex package name
      licenses: ["MIT"],                             # => Open source license
      links: %{
        "GitHub" => "https://github.com/user/zakat_calculator"
      },
      files: [
        "lib",                                       # => Include lib/ directory
        "mix.exs",                                   # => Include project file
        "README.md",                                 # => Include documentation
        "LICENSE"                                    # => Include license file
      ]
    ]
  end

  defp deps do
    [
      {:decimal, "~> 2.0"},                          # => Precise money calculations
      {:ex_doc, "~> 0.29", only: :dev}               # => Documentation generator
    ]
  end
end

# Publish to Hex.pm
$ mix hex.publish
Publishing zakat_calculator 1.0.0
  App: zakat_calculator
  Name: zakat_calculator
  Description: Islamic Zakat calculation library with multiple asset types
  Version: 1.0.0
  Build tools: mix
  Licenses: MIT
  Links:
    GitHub: https://github.com/user/zakat_calculator
  Elixir: ~> 1.14

Proceed? [Yn] y
# => Package published to hex.pm
# => Available via: {:zakat_calculator, "~> 1.0"}
```

### Private Hex Repository

For proprietary packages, use private Hex:

```elixir
# Organization-level private Hex repository
# Mix configuration: mix.exs
defmodule InternalApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :internal_app,
      version: "0.1.0",
      deps: deps()
    ]
  end

  defp deps do
    [
      # Public Hex package
      {:jason, "~> 1.4"},                            # => From hex.pm

      # Private Hex package
      {:company_auth, "~> 2.1",                      # => From organization Hex
       organization: "mycompany"}                    # => Private repository name
    ]
  end
end

# Configure private Hex access
# ~/.hex/hex.config
%{
  "hexpm:mycompany" => %{
    "api_key" => "abc123...",                        # => Organization API key
    "api_url" => "https://hex.mycompany.com/api"     # => Private Hex server URL
  }
}

# mix deps.get fetches from both public and private Hex
$ mix deps.get
* Getting jason (Hex package)                        # => From hex.pm
* Getting company_auth (Hex package)                 # => From private Hex
# => Resolves dependencies from multiple sources
```

### Dependency Locking

mix.lock ensures reproducible builds:

```elixir
# mix.lock - Generated by mix deps.get
%{
  "decimal": {:hex, :decimal, "2.1.1", "5611dca...", [:mix], [], "hexpm", "53cfe..."},
  # => Package: decimal
  # => Source: hex
  # => Version: 2.1.1 (exact)
  # => Checksum: Verifies integrity
  # => Registry: hexpm

  "jason": {:hex, :jason, "1.4.1", "af1504...", [:mix], [{:decimal, "~> 1.0 or ~> 2.0", [hex: :decimal, repo: "hexpm", optional: true]}], "hexpm", "fbb01..."},
  # => Includes transitive dependency requirements
  # => Optional dependencies listed

  "phoenix": {:hex, :phoenix, "1.7.10", "02189...", [:mix], [
    {:castore, ">= 0.0.0", [hex: :castore, repo: "hexpm", optional: false]},
    {:jason, "~> 1.0", [hex: :jason, repo: "hexpm", optional: true]},
    {:phoenix_pubsub, "~> 2.1", [hex: :phoenix_pubsub, repo: "hexpm", optional: false]},
    {:plug, "~> 1.14", [hex: :plug, repo: "hexpm", optional: false]},
    {:telemetry, "~> 0.4 or ~> 1.0", [hex: :telemetry, repo: "hexpm", optional: false]}
  ], "hexpm", "cf784..."}
  # => Lists all transitive dependencies with version constraints
}

# mix.lock guarantees:
# - Exact versions across environments (dev, test, prod)
# - Checksum verification (detects corruption)
# - Reproducible builds (same dependencies everywhere)
# - Commit to version control (team consistency)
```

## Key Takeaways

**Manual Dependencies**:

- Copy .ex files into project (no version tracking)
- Version conflicts break code unpredictably
- No transitive dependency resolution
- Fragile, error-prone

**Hex Package Manager**:

- Central repository (hex.pm) with semantic versioning
- Automatic transitive dependency resolution
- mix.lock ensures reproducible builds
- Conflict detection and resolution
- Public and private Hex repositories

**Production patterns**: Semantic version constraints (~> 1.4), dependency locking (mix.lock), private Hex for proprietary code, checksum verification for security.

**OTP-First insight**: Mix and Hex build on BEAM's code loading and versioning features to provide dependency management that works with hot code upgrades and application supervision trees.
