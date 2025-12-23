---
title: "Deployment"
date: 2025-12-21T18:15:00+07:00
draft: false
description: "Deploy Elixir applications using Mix releases, Docker, environment configuration, and production best practices for reliable deployments."
weight: 1000019
tags:
  [
    "elixir",
    "deployment",
    "releases",
    "docker",
    "production",
    "devops",
    "how-to",
  ]
---

**Ready to deploy Elixir apps to production?** Mix releases create self-contained packages with BEAM runtime.

## Prerequisites

- Working Elixir application
- Basic Docker knowledge (optional)
- Understanding of environment variables
- Completed [Intermediate Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate)

## Problem

Deploying Elixir applications requires bundling your code with the BEAM runtime, managing configuration across environments, handling hot upgrades, and ensuring zero-downtime deployments. Traditional deployment methods don't work well with the BEAM's capabilities.

**Challenges:**

- Creating standalone executables without Elixir installed on target
- Managing environment-specific configuration
- Handling database migrations during deployment
- Implementing zero-downtime deployments
- Monitoring and managing running applications

## Solution

Use **Mix releases** to create self-contained packages with embedded BEAM runtime, combined with Docker for containerization and proper configuration management.

## How It Works

### 1. Basic Mix Release

Configure release in `mix.exs`:

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
      releases: releases()
    ]
  end

  defp releases do
    [
      my_app: [
        include_executables_for: [:unix],
        applications: [runtime_tools: :permanent],
        steps: [:assemble, :tar]
      ]
    ]
  end
end
```

Build release:

```bash
# Set production environment
export MIX_ENV=prod

# Fetch and compile dependencies
mix deps.get --only prod
mix deps.compile

# Compile application
mix compile

# Generate release
mix release

# Output: _build/prod/rel/my_app/
```

### 2. Running Releases

```bash
# Start in foreground
_build/prod/rel/my_app/bin/my_app start

# Start as daemon
_build/prod/rel/my_app/bin/my_app daemon

# Stop application
_build/prod/rel/my_app/bin/my_app stop

# Restart application
_build/prod/rel/my_app/bin/my_app restart

# Remote console
_build/prod/rel/my_app/bin/my_app remote

# Check version
_build/prod/rel/my_app/bin/my_app version
```

### 3. Runtime Configuration

Create `config/runtime.exs`:

```elixir
import Config

if config_env() == :prod do
  database_url = System.get_env("DATABASE_URL") ||
    raise "DATABASE_URL not set"

  config :my_app, MyApp.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")

  secret_key_base = System.get_env("SECRET_KEY_BASE") ||
    raise "SECRET_KEY_BASE not set"

  port = String.to_integer(System.get_env("PORT") || "4000")

  config :my_app, MyAppWeb.Endpoint,
    http: [port: port],
    secret_key_base: secret_key_base,
    server: true
end
```

### 4. Docker Deployment

Multi-stage Dockerfile:

```dockerfile
# Build stage
FROM elixir:1.14-alpine AS build

# Install build dependencies
RUN apk add --no-cache build-base git

WORKDIR /app

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Copy dependency files
COPY mix.exs mix.lock ./
RUN mix deps.get --only prod

# Copy application code
COPY config config
COPY lib lib
COPY priv priv

# Compile dependencies and application
RUN mix deps.compile
RUN mix compile

# Build release
RUN mix release

# Runtime stage
FROM alpine:3.18 AS app

# Install runtime dependencies
RUN apk add --no-cache \
    libstdc++ \
    openssl \
    ncurses-libs \
    libgcc

# Create app user
RUN addgroup -g 1000 app && \
    adduser -u 1000 -G app -s /bin/sh -D app

WORKDIR /app

# Copy release from build stage
COPY --from=build --chown=app:app /app/_build/prod/rel/my_app ./

USER app

EXPOSE 4000

CMD ["/app/bin/my_app", "start"]
```

Build and run:

```bash
# Build image
docker build -t my_app:latest .

# Run container
docker run -d \
  -p 4000:4000 \
  -e DATABASE_URL="postgres://user:pass@db/myapp" \
  -e SECRET_KEY_BASE="secret" \
  --name my_app \
  my_app:latest

# View logs
docker logs -f my_app

# Remote console
docker exec -it my_app /app/bin/my_app remote
```

### 5. Phoenix Applications

Additional Phoenix configuration:

```elixir
# config/runtime.exs
config :my_app, MyAppWeb.Endpoint,
  url: [host: System.get_env("HOST", "example.com"), port: 443, scheme: "https"],
  http: [port: String.to_integer(System.get_env("PORT") || "4000")],
  secret_key_base: secret_key_base,
  server: true  # Start endpoint on release start

# Enable static file serving
config :my_app, MyAppWeb.Endpoint,
  cache_static_manifest: "priv/static/cache_manifest.json"
```

Build assets before release:

```bash
# Install Node dependencies
cd assets && npm install && cd ..

# Build assets
mix assets.deploy

# Build release
MIX_ENV=prod mix release
```

### 6. Database Migrations

Run migrations during deployment:

```elixir
# lib/my_app/release.ex
defmodule MyApp.Release do
  @moduledoc """
  Tasks to run in production releases
  """
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

Run migration:

```bash
# In release
_build/prod/rel/my_app/bin/my_app eval "MyApp.Release.migrate()"

# In Docker
docker exec my_app /app/bin/my_app eval "MyApp.Release.migrate()"
```

### 7. Hot Upgrades (Advanced)

Generate upgrade:

```elixir
# Update version in mix.exs
def project do
  [version: "0.2.0"]
end

# Build new release
MIX_ENV=prod mix release --upgrade

# Generate upgrade package
mix release.gen.appup --from=0.1.0 --to=0.2.0
```

Apply upgrade:

```bash
# Copy upgrade to releases directory
cp _build/prod/rel/my_app/releases/0.2.0/my_app.tar.gz /path/to/production/releases/

# Apply upgrade (no downtime)
bin/my_app upgrade 0.2.0
```

### 8. Clustering

Configure clustering:

```elixir
# config/runtime.exs
config :libcluster,
  topologies: [
    k8s: [
      strategy: Cluster.Strategy.Kubernetes,
      config: [
        mode: :dns,
        kubernetes_node_basename: "my_app",
        kubernetes_selector: "app=my_app",
        polling_interval: 10_000
      ]
    ]
  ]
```

In Kubernetes:

```yaml
apiVersion: v1
kind: Service
metadata:
  name: my-app
spec:
  clusterIP: None # Headless service
  selector:
    app: my-app
  ports:
    - port: 4000
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: my-app
spec:
  replicas: 3
  selector:
    matchLabels:
      app: my-app
  template:
    metadata:
      labels:
        app: my-app
    spec:
      containers:
        - name: my-app
          image: my-app:latest
          env:
            - name: POD_IP
              valueFrom:
                fieldRef:
                  fieldPath: status.podIP
            - name: RELEASE_NODE
              value: "my_app@$(POD_IP)"
```

### 9. Health Checks

Add health check endpoint:

```elixir
# lib/my_app_web/controllers/health_controller.ex
defmodule MyAppWeb.HealthController do
  use MyAppWeb, :controller

  def index(conn, _params) do
    # Check database
    case Ecto.Adapters.SQL.query(MyApp.Repo, "SELECT 1", []) do
      {:ok, _} ->
        json(conn, %{status: "healthy", database: "connected"})
      {:error, _} ->
        conn
        |> put_status(:service_unavailable)
        |> json(%{status: "unhealthy", database: "disconnected"})
    end
  end
end

# router.ex
scope "/", MyAppWeb do
  get "/health", HealthController, :index
end
```

Docker healthcheck:

```dockerfile
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD wget --no-verbose --tries=1 --spider http://localhost:4000/health || exit 1
```

### 10. Environment Variables

Use `.env` file for local development:

```bash
# .env
export DATABASE_URL="postgres://localhost/myapp_dev"
export SECRET_KEY_BASE="local_secret"
export PORT="4000"

# Source it
source .env
```

Production environment:

```bash
# systemd service file
Environment="DATABASE_URL=postgres://..."
Environment="SECRET_KEY_BASE=..."
Environment="PORT=4000"

# Docker
docker run -e DATABASE_URL="..." my_app

# Kubernetes ConfigMap
apiVersion: v1
kind: ConfigMap
metadata:
  name: my-app-config
data:
  PORT: "4000"
---
# Then reference in deployment
envFrom:
- configMapRef:
    name: my-app-config
```

## Variations

### Deploying to Fly.io

```bash
# Install flyctl
curl -L https://fly.io/install.sh | sh

# Launch app
fly launch

# Deploy
fly deploy

# Open app
fly open

# View logs
fly logs

# Scale
fly scale count 3
```

### Deploying to Gigalixir

```bash
# Install CLI
pip3 install gigalixir

# Create app
gigalixir create my_app

# Deploy
git push gigalixir main

# Run migrations
gigalixir ps:migrate

# Scale
gigalixir ps:scale --replicas=3
```

### Systemd Service

```ini
# /etc/systemd/system/my_app.service
[Unit]
Description=My Elixir App
After=network.target

[Service]
Type=forking
User=my_app
Group=my_app
WorkingDirectory=/opt/my_app
EnvironmentFile=/opt/my_app/.env
ExecStart=/opt/my_app/bin/my_app daemon
ExecStop=/opt/my_app/bin/my_app stop
Restart=on-failure
RestartSec=5
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target
```

Enable and start:

```bash
sudo systemctl enable my_app
sudo systemctl start my_app
sudo systemctl status my_app
sudo journalctl -u my_app -f
```

## Advanced Patterns

### 1. Blue-Green Deployment

```bash
# Deploy new version as "green"
docker run -d --name my_app_green my_app:v2

# Health check
curl http://green:4000/health

# Switch nginx upstream
nginx -s reload

# Remove old "blue"
docker stop my_app_blue
docker rm my_app_blue
```

### 2. Canary Deployment

```yaml
# Kubernetes
apiVersion: apps/v1
kind: Deployment
metadata:
  name: my-app-stable
spec:
  replicas: 9 # 90% traffic
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: my-app-canary
spec:
  replicas: 1 # 10% traffic
```

### 3. Release Commands

```elixir
# rel/config.exs
release :my_app do
  set commands: [
    migrate: "rel/commands/migrate.sh",
    seed: "rel/commands/seed.sh"
  ]
end
```

```bash
#!/bin/sh
# rel/commands/migrate.sh
bin/my_app eval "MyApp.Release.migrate()"
```

## Use Cases

**Small Applications:**

- Single server deployment
- Systemd service
- Simple migrations

**Medium Applications:**

- Docker containers
- Load balancer
- Health checks
- Automated deployments

**Large Applications:**

- Kubernetes clusters
- Hot upgrades
- Distributed tracing
- Auto-scaling

## Troubleshooting

### Release Won't Start

```bash
# Check logs
tail -f _build/prod/rel/my_app/tmp/log/erlang.log.*

# Check environment
_build/prod/rel/my_app/bin/my_app eval "System.get_env()"

# Verify config
_build/prod/rel/my_app/bin/my_app eval ":sys.get_state(MyApp.Endpoint)"
```

### Missing Compile-Time Configuration

```elixir
# Don't do this (won't work in releases)
def my_function do
  Application.get_env(:my_app, :some_value)  # Compile-time value
end

# Do this instead
def my_function do
  Application.fetch_env!(:my_app, :some_value)  # Runtime value
end
```

### Port Already in Use

```bash
# Find process using port
lsof -i :4000

# Kill it
kill -9 <PID>
```

## Best Practices

1. **Use runtime.exs for production config:**
   All secrets and environment-specific config

2. **Version your releases:**

   ```elixir
   version: "#{System.get_env("VERSION", "0.1.0")}"
   ```

3. **Include health checks:**
   Monitor database, cache, external services

4. **Automate migrations:**
   Run automatically before starting application

5. **Use structured logging:**
   JSON logs for easy parsing

6. **Monitor resource usage:**
   CPU, memory, connection pools

7. **Test releases locally:**

   ```bash
   MIX_ENV=prod mix release
   _build/prod/rel/my_app/bin/my_app start
   ```

8. **Document deployment process:**
   Runbooks for common issues

## Common Pitfalls

1. **Forgetting `server: true`:** Phoenix won't start endpoint
2. **Hardcoded localhost:** Use environment variables
3. **Missing environment variables:** App crashes on startup
4. **Not running migrations:** Database out of sync
5. **Using compile-time config:** Won't work in releases

## Related Resources

- [Configuration Guide](/en/learn/software-engineering/programming-language/elixir/how-to/configuration)
- [Monitoring Guide](/en/learn/software-engineering/programming-language/elixir/how-to/monitoring)
- [Docker Guide](https://docs.docker.com/)
- [Kubernetes Guide](https://kubernetes.io/docs/)
- [Mix Release Documentation](https://hexdocs.pm/mix/Mix.Tasks.Release.html)
