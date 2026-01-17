---
title: "Dependencies"
date: 2025-12-21T18:20:00+07:00
draft: false
description: "Manage Elixir dependencies using Hex package manager with version constraints, dependency resolution, and publishing packages."
weight: 1000020
tags: ["elixir", "hex", "dependencies", "packages", "versioning", "how-to"]
---

**Need to manage external libraries?** Hex is Elixir's package manager for dependency management and publishing.

## Prerequisites

- Elixir and Mix installed
- Basic understanding of Mix projects
- Completed [Quick Start Tutorial](/en/learn/software-engineering/programming-languages/elixir/tutorials/quick-start)

## Problem

Modern applications rely on external libraries for functionality like HTTP clients, JSON parsing, database access, and more. You need a reliable way to declare, fetch, compile, and update these dependencies while managing version compatibility.

**Challenges:**

- Declaring dependencies with appropriate version constraints
- Resolving version conflicts between transitive dependencies
- Keeping dependencies updated without breaking changes
- Managing development vs production dependencies
- Understanding dependency trees and compilation order

## Solution

Use **Hex.pm** package manager integrated with Mix for comprehensive dependency management.

## How It Works

### 1. Adding Dependencies

Edit your `mix.exs` file to declare dependencies:

```elixir
defmodule MyApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :my_app,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  defp deps do
    [
      # Phoenix framework
      {:phoenix, "~> 1.7.0"},

      # Database
      {:ecto_sql, "~> 3.10"},
      {:postgrex, ">= 0.0.0"},

      # JSON
      {:jason, "~> 1.4"},

      # HTTP client
      {:httpoison, "~> 2.0"},

      # Testing
      {:ex_machina, "~> 2.7", only: :test},

      # Development
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
    ]
  end
end
```

### 2. Fetching Dependencies

```bash
mix deps.get

mix deps.compile

mix do deps.get, deps.compile
```

### 3. Version Constraints

Hex uses semantic versioning (SemVer):

```elixir
{:package, "~> 1.2"}      # >= 1.2.0 and < 2.0.0
{:package, "~> 1.2.3"}    # >= 1.2.3 and < 1.3.0
{:package, "~> 1.2.3-rc"} # >= 1.2.3-rc and < 1.3.0

{:package, ">= 1.0.0"}    # Any version >= 1.0.0
{:package, ">= 1.2.0 and < 2.0.0"}

{:package, "== 1.2.3"}    # Exactly 1.2.3

{:package, ">= 0.0.0"}
```

**Best practice:** Use `~>` for flexibility with safety:

```elixir
{:phoenix, "~> 1.7.0"}  # Gets patches and minor updates, not major
```

### 4. Dependency Sources

#### From Hex.pm (Default)

```elixir
{:phoenix, "~> 1.7.0"}
```

#### From Git Repository

```elixir
{:my_lib, git: "https://github.com/user/my_lib.git", tag: "v1.0.0"}

{:my_lib, git: "https://github.com/user/my_lib.git", branch: "main"}

{:my_lib, git: "https://github.com/user/my_lib.git",
  ref: "abc123def456"}

{:my_lib, github: "user/my_lib", tag: "v1.0.0"}
```

#### From Local Path

```elixir
{:my_lib, path: "../my_lib"}
```

Useful for:

- Umbrella applications
- Local development
- Testing changes before publishing

### 5. Environment-Specific Dependencies

```elixir
defp deps do
  [
    # All environments
    {:phoenix, "~> 1.7.0"},

    # Test only
    {:ex_machina, "~> 2.7", only: :test},

    # Development only
    {:phoenix_live_reload, "~> 1.4", only: :dev},

    # Dev and test, not production
    {:credo, "~> 1.7", only: [:dev, :test], runtime: false},

    # Runtime false = compile-time only
    {:dialyxir, "~> 1.3", only: :dev, runtime: false}
  ]
end
```

### 6. Optional Dependencies

```elixir
defp deps do
  [
    {:poison, "~> 5.0", optional: true},
    {:jason, "~> 1.4", optional: true}
  ]
end
```

The application can use either JSON library without requiring both.

### 7. Dependency Management Commands

```bash
mix deps.tree

mix hex.outdated

mix deps.update phoenix

mix deps.update --all

mix deps.clean --all
mix deps.get
mix deps.compile

mix hex.info phoenix

mix deps.unlock --unused
mix deps.clean --unused
```

### 8. Lock File

`mix.lock` ensures reproducible builds:

```elixir
%{
  "phoenix": {:hex, :phoenix, "1.7.10", "..."},
  "plug": {:hex, :plug, "1.15.2", "..."},
  # ... all transitive dependencies
}
```

**Best practices:**

- Commit `mix.lock` to version control
- Update lock file: `mix deps.update package_name`
- Verify integrity: `mix deps.get --check-locked`

### 9. Private Hex Repositories

For company-internal packages:

```elixir
config :hex,
  api_url: "https://hex.mycompany.com/api",
  api_key: System.get_env("HEX_API_KEY")

defp deps do
  [
    {:internal_lib, "~> 1.0", organization: "mycompany"}
  ]
end
```

### 10. Publishing Your Package

```bash
mix hex.user register

```

```elixir
def project do
  [
    app: :my_library,
    version: "1.0.0",
    description: "A helpful library",
    package: package(),
    docs: [main: "readme", extras: ["README.md"]]
  ]
end

defp package do
  [
    name: "my_library",
    files: ~w(lib mix.exs README.md LICENSE),
    licenses: ["MIT"],
    links: %{"GitHub" => "https://github.com/user/my_library"},
    maintainers: ["Your Name"]
  ]
end
```

```bash
mix hex.build

mix hex.publish
```

## Variations

### Umbrella Application Dependencies

```elixir
defp deps do
  [
    {:app_b, in_umbrella: true},
    {:phoenix, "~> 1.7.0"}
  ]
end
```

### Override Transitive Dependencies

```elixir
defp deps do
  [
    {:parent_lib, "~> 1.0"},
    # Override parent_lib's dependency on old_lib
    {:old_lib, "~> 2.0", override: true}
  ]
end
```

### Sparse Checkout for Git Dependencies

```elixir
{:my_lib,
  git: "https://github.com/user/monorepo.git",
  sparse: "packages/my_lib",
  tag: "v1.0.0"
}
```

## Advanced Patterns

### 1. Conditional Dependencies

```elixir
defp deps do
  base_deps() ++ env_deps(Mix.env())
end

defp base_deps do
  [{:phoenix, "~> 1.7.0"}]
end

defp env_deps(:test), do: [{:ex_machina, "~> 2.7"}]
defp env_deps(:dev), do: [{:credo, "~> 1.7"}]
defp env_deps(_), do: []
```

### 2. Dependency Aliases

```bash
def project do
  [
    aliases: aliases()
  ]
end

defp aliases do
  [
    setup: ["deps.get", "ecto.setup"],
    "ecto.setup": ["ecto.create", "ecto.migrate"],
    "deps.refresh": ["deps.clean --all", "deps.get"]
  ]
end
```

### 3. Hex Organization Management

```bash
mix hex.organization create mycompany

mix hex.organization member add mycompany user@example.com

mix hex.publish --organization=mycompany
```

## Use Cases

**Public Packages:**

- Web frameworks (Phoenix, Plug)
- Database clients (Ecto, Postgrex)
- HTTP clients (HTTPoison, Finch)
- JSON libraries (Jason, Poison)

**Private Packages:**

- Shared business logic across services
- Internal authentication libraries
- Company-specific utilities
- Proprietary algorithms

**Development Tools:**

- Code analysis (Credo, Dialyxir)
- Testing (ExUnit, ExMachina)
- Documentation (ExDoc)
- Live reload (Phoenix.LiveReload)

## Troubleshooting

### Dependency Conflicts

```bash

1. Check dependency tree
   mix deps.tree

2. Try updating
   mix deps.update --all

3. Use override if necessary
   {:package, "~> 1.0", override: true}
```

### Compilation Errors

```bash
mix deps.clean --all
mix deps.get
mix compile --force
```

### Lock File Issues

```bash
rm mix.lock
mix deps.get
```

## Best Practices

1. **Pin versions in production:**

   ```elixir
   {:phoenix, "~> 1.7.10"}  # Not "~> 1.7"
   ```

2. **Commit mix.lock:**
   Ensures team uses same versions

3. **Review updates:**

   ```bash
   mix hex.outdated
   # Review changelog before updating
   mix deps.update package_name
   ```

4. **Minimize dependencies:**
   Each dependency adds compilation time and potential vulnerabilities

5. **Use runtime: false for compile-time tools:**

   ```elixir
   {:credo, "~> 1.7", only: :dev, runtime: false}
   ```

6. **Document unusual constraints:**

   ```elixir
   # Pinned due to bug in 2.x - see issue #123
   {:library, "~> 1.9.0"}
   ```

## Common Pitfalls

1. **Using exact versions:** Prevents patch updates
2. **Not committing mix.lock:** Team gets different versions
3. **Ignoring security updates:** Run `mix hex.audit` regularly
4. **Circular dependencies:** Design issue, refactor into shared lib
5. **Too many dependencies:** Each adds complexity and attack surface

## Related Resources

- [Mix Guide](/en/learn/software-engineering/programming-languages/elixir/how-to/mix)
- [Configuration Guide](/en/learn/software-engineering/programming-languages/elixir/how-to/configuration)
- [Deployment Guide](/en/learn/software-engineering/programming-languages/elixir/how-to/deployment)
- [Best Practices](/en/learn/software-engineering/programming-languages/elixir/explanation/best-practices)
- [Hex.pm Documentation](https://hex.pm/docs)
