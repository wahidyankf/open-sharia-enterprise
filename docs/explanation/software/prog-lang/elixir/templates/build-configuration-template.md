# Build Configuration Template

**Category**: DDD Template
**Complexity**: ⭐⭐⭐ (Medium-High)
**Prerequisites**: Mix, Docker, CI/CD basics

## Overview

Complete project setup for DDD applications including Mix configuration, environment management, Docker containers, and CI/CD pipelines.

## Template Code

### mix.exs

```elixir
defmodule Financial.MixProject do
  use Mix.Project

  def project do
    [
      app: :financial,
      version: "1.0.0",
      elixir: "~> 1.17",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps(),
      releases: releases()
    ]
  end

  def application do
    [
      mod: {Financial.Application, []},
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:phoenix, "~> 1.7.0"},
      {:phoenix_ecto, "~> 4.4"},
      {:ecto_sql, "~> 3.12"},
      {:postgrex, ">= 0.0.0"},
      {:phoenix_live_view, "~> 1.0"},
      {:jason, "~> 1.4"},
      {:decimal, "~> 2.1"},
      {:oban, "~> 2.15"}
    ]
  end

  defp aliases do
    [
      setup: ["deps.get", "ecto.setup"],
      "ecto.setup": ["ecto.create", "ecto.migrate"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"]
    ]
  end

  defp releases do
    [
      financial: [
        version: "1.0.0",
        applications: [financial: :permanent]
      ]
    ]
  end
end
```

### config/runtime.exs

```elixir
import Config

if config_env() == :prod do
  database_url = System.get_env("DATABASE_URL") ||
    raise "DATABASE_URL not available"

  config :financial, Financial.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
    ssl: true

  secret_key_base = System.get_env("SECRET_KEY_BASE") ||
    raise "SECRET_KEY_BASE not available"

  config :financial, FinancialWeb.Endpoint,
    http: [port: String.to_integer(System.get_env("PORT") || "4000")],
    secret_key_base: secret_key_base
end
```

### Dockerfile

```dockerfile
FROM hexpm/elixir:1.17.3-erlang-27.2-alpine-3.20.3 AS build

RUN apk add --no-cache build-base git

WORKDIR /app

COPY mix.exs mix.lock ./
RUN mix local.hex --force && \
    mix local.rebar --force && \
    mix deps.get --only prod

COPY config config
COPY lib lib
COPY priv priv

RUN MIX_ENV=prod mix compile
RUN MIX_ENV=prod mix release

FROM alpine:3.20.3 AS app

RUN apk add --no-cache libstdc++ openssl ncurses-libs

WORKDIR /app

COPY --from=build /app/_build/prod/rel/financial ./

CMD ["bin/financial", "start"]
```

### docker-compose.yml

```yaml
version: "3.8"

services:
  db:
    image: postgres:16-alpine
    environment:
      POSTGRES_USER: financial
      POSTGRES_PASSWORD: financial
      POSTGRES_DB: financial_dev
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

  app:
    build: .
    environment:
      DATABASE_URL: ecto://financial:financial@db/financial_dev
      SECRET_KEY_BASE: "change_me"
    ports:
      - "4000:4000"
    depends_on:
      - db

volumes:
  postgres_data:
```

### .github/workflows/ci.yml

```yaml
name: CI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest

    services:
      postgres:
        image: postgres:16
        env:
          POSTGRES_PASSWORD: postgres
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

    steps:
      - uses: actions/checkout@v3

      - name: Set up Elixir
        uses: erlef/setup-beam@v1
        with:
          elixir-version: "1.17"
          otp-version: "27"

      - name: Install dependencies
        run: mix deps.get

      - name: Run tests
        run: mix test
        env:
          MIX_ENV: test
```

## Best Practices

1. Environment-specific configuration
2. Docker multi-stage builds
3. Automated CI/CD
4. Release configuration
5. Security (environment variables)

## Resources

- [Back to Templates README](README.md)
- [Mix Releases](https://hexdocs.pm/mix/Mix.Tasks.Release.html)

---

**Last Updated**: 2025-01-23
