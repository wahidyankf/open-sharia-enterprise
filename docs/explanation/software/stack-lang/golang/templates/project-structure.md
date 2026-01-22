# Go Project Structure Template

Standard Go project layout following community conventions and platform standards.

## Directory Structure

```
my-go-project/
├── cmd/                          # Main applications
│   ├── server/                   # HTTP/gRPC server
│   │   └── main.go              # Server entry point
│   ├── cli/                      # CLI tool
│   │   └── main.go              # CLI entry point
│   └── worker/                   # Background worker
│       └── main.go              # Worker entry point
├── internal/                     # Private application code
│   ├── domain/                   # Domain models and business logic
│   │   ├── user.go              # User aggregate
│   │   ├── transaction.go       # Transaction aggregate
│   │   └── money.go             # Money value object
│   ├── application/              # Application services
│   │   ├── user_service.go      # User service
│   │   └── transaction_service.go
│   ├── infrastructure/           # Infrastructure adapters
│   │   ├── database/            # Database implementations
│   │   │   ├── postgres.go
│   │   │   └── migrations/
│   │   ├── http/                # HTTP handlers
│   │   │   ├── server.go
│   │   │   ├── user_handler.go
│   │   │   └── middleware/
│   │   └── grpc/                # gRPC services
│   │       ├── server.go
│   │       └── user_service.go
│   └── config/                   # Configuration
│       ├── config.go
│       └── config_test.go
├── pkg/                          # Public library code
│   ├── logger/                   # Logging utilities
│   │   └── logger.go
│   └── validator/                # Validation utilities
│       └── validator.go
├── api/                          # API definitions
│   ├── openapi/                  # OpenAPI/Swagger specs
│   │   └── api.yaml
│   └── proto/                    # Protobuf definitions
│       ├── user.proto
│       └── transaction.proto
├── scripts/                      # Build and deploy scripts
│   ├── build.sh
│   ├── test.sh
│   └── deploy.sh
├── deployments/                  # Deployment configurations
│   ├── docker/
│   │   └── Dockerfile
│   └── kubernetes/
│       ├── deployment.yaml
│       └── service.yaml
├── test/                         # Integration and E2E tests
│   ├── integration/
│   │   └── user_test.go
│   └── e2e/
│       └── api_test.go
├── docs/                         # Project documentation
│   ├── architecture.md
│   └── api.md
├── go.mod                        # Go module definition
├── go.sum                        # Dependency checksums
├── Makefile                      # Build automation
├── .golangci.yml                 # Linter configuration
├── .env.example                  # Environment variables template
└── README.md                     # Project overview
```

## Key Directories Explained

### `cmd/`

Contains main applications for this project. Each subdirectory is a separate executable.

**Convention**: Name after the application (server, cli, worker)

**Example**:

```go
// cmd/server/main.go
package main

import (
    "context"
    "log/slog"
    "os"
    "os/signal"
    "syscall"

    "example.com/my-go-project/internal/config"
    "example.com/my-go-project/internal/infrastructure/http"
)

func main() {
    // Load configuration
    cfg, err := config.Load()
    if err != nil {
        slog.Error("failed to load config", "error", err)
        os.Exit(1)
    }

    // Initialize logger
    logger := slog.New(slog.NewJSONHandler(os.Stdout, nil))

    // Create HTTP server
    srv := http.NewServer(cfg, logger)

    // Start server
    go func() {
        if err := srv.Start(); err != nil {
            logger.Error("server failed", "error", err)
            os.Exit(1)
        }
    }()

    // Wait for interrupt signal
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
    <-quit

    logger.Info("shutting down server")

    // Graceful shutdown
    if err := srv.Shutdown(context.Background()); err != nil {
        logger.Error("server shutdown failed", "error", err)
    }
}
```

### `internal/`

Private application code that cannot be imported by other projects. Go enforces this at compile time.

**Structure**:

- **domain/**: Pure domain models and business logic (no external dependencies)
- **application/**: Application services orchestrating domain logic
- **infrastructure/**: Adapters for external systems (database, HTTP, gRPC, messaging)
- **config/**: Configuration loading and validation

**Example domain model**:

```go
// internal/domain/money.go
package domain

import (
    "fmt"
)

// Money represents a monetary value with currency
type Money struct {
    amount   int64  // Amount in smallest currency unit (cents)
    currency string // ISO 4217 currency code
}

// NewMoney creates a new Money value object
func NewMoney(amount int64, currency string) (Money, error) {
    if currency == "" {
        return Money{}, fmt.Errorf("currency cannot be empty")
    }
    return Money{amount: amount, currency: currency}, nil
}

// Amount returns the amount in smallest currency unit
func (m Money) Amount() int64 {
    return m.amount
}

// Currency returns the currency code
func (m Money) Currency() string {
    return m.currency
}

// Add adds two Money values (same currency)
func (m Money) Add(other Money) (Money, error) {
    if m.currency != other.currency {
        return Money{}, fmt.Errorf("cannot add different currencies: %s and %s", m.currency, other.currency)
    }
    return Money{amount: m.amount + other.amount, currency: m.currency}, nil
}
```

### `pkg/`

Public library code that can be imported by other projects. Only put code here if it's truly reusable and stable.

**Convention**: Keep this directory small. Most code belongs in `internal/`.

**Example**:

```go
// pkg/logger/logger.go
package logger

import (
    "log/slog"
    "os"
)

// New creates a new structured logger
func New(level slog.Level) *slog.Logger {
    opts := &slog.HandlerOptions{
        Level: level,
    }
    handler := slog.NewJSONHandler(os.Stdout, opts)
    return slog.New(handler)
}
```

### `api/`

API definitions and contracts (OpenAPI, Protobuf).

**OpenAPI example**:

```yaml
# api/openapi/api.yaml
openapi: 3.0.0
info:
  title: My Go Project API
  version: 1.0.0
paths:
  /users/{id}:
    get:
      summary: Get user by ID
      parameters:
        - name: id
          in: path
          required: true
          schema:
            type: string
      responses:
        "200":
          description: User found
          content:
            application/json:
              schema:
                $ref: "#/components/schemas/User"
```

**Protobuf example**:

```protobuf
// api/proto/user.proto
syntax = "proto3";

package api.v1;

option go_package = "example.com/my-go-project/api/proto/v1";

service UserService {
  rpc GetUser(GetUserRequest) returns (GetUserResponse);
  rpc CreateUser(CreateUserRequest) returns (CreateUserResponse);
}

message GetUserRequest {
  string id = 1;
}

message GetUserResponse {
  User user = 1;
}

message User {
  string id = 1;
  string email = 2;
  string name = 3;
}
```

### `test/`

Integration and end-to-end tests. Unit tests go next to the code they test.

**Integration test example**:

```go
// test/integration/user_test.go
//go:build integration

package integration

import (
    "context"
    "testing"

    "github.com/stretchr/testify/assert"
    "github.com/stretchr/testify/require"
    "github.com/testcontainers/testcontainers-go"
    "github.com/testcontainers/testcontainers-go/modules/postgres"
)

func TestUserRepository(t *testing.T) {
    ctx := context.Background()

    // Start PostgreSQL container
    pgContainer, err := postgres.Run(ctx,
        "postgres:16-alpine",
        postgres.WithDatabase("testdb"),
        postgres.WithUsername("user"),
        postgres.WithPassword("password"),
    )
    require.NoError(t, err)
    defer func() {
        _ = testcontainers.TerminateContainer(pgContainer)
    }()

    // Get connection string
    connStr, err := pgContainer.ConnectionString(ctx)
    require.NoError(t, err)

    // Run tests...
    t.Run("create user", func(t *testing.T) {
        // Test implementation
    })
}
```

## go.mod Example

```
module example.com/my-go-project

go 1.25.6

require (
    github.com/gin-gonic/gin v1.11.0
    github.com/stretchr/testify v1.10.0
    google.golang.org/grpc v1.68.0
    google.golang.org/protobuf v1.36.5
)
```

## Makefile Example

```makefile
.PHONY: help build test lint clean run

help: ## Show this help
 @grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

build: ## Build all binaries
 @echo "Building..."
 @go build -o bin/server ./cmd/server
 @go build -o bin/cli ./cmd/cli

test: ## Run tests
 @echo "Running tests..."
 @go test -v -race ./...

test-integration: ## Run integration tests
 @echo "Running integration tests..."
 @go test -v -race -tags=integration ./test/integration/...

lint: ## Run linters
 @echo "Running linters..."
 @golangci-lint run

fmt: ## Format code
 @echo "Formatting code..."
 @go fmt ./...
 @gofmt -s -w .

clean: ## Clean build artifacts
 @echo "Cleaning..."
 @rm -rf bin/
 @go clean

run: ## Run server locally
 @echo "Starting server..."
 @go run ./cmd/server

.DEFAULT_GOAL := help
```

## Best Practices

### 1. Follow Standard Layout

Use the community-accepted standard layout. Other Go developers will immediately understand your project structure.

### 2. Keep internal/ Private

Put most code in `internal/` to prevent unwanted external dependencies. Only expose stable, reusable code in `pkg/`.

### 3. Hexagonal Architecture

Organize `internal/` by layers:

- **domain/**: Pure business logic (no external dependencies)
- **application/**: Orchestration and use cases
- **infrastructure/**: External system adapters

### 4. Separate Concerns

Keep HTTP handlers thin. Move business logic to services. Keep domain models pure.

### 5. Use Dependency Injection

Pass dependencies explicitly through constructors. Avoid global state.

```go
// Good: Explicit dependencies
type UserService struct {
    repo   UserRepository
    logger *slog.Logger
}

func NewUserService(repo UserRepository, logger *slog.Logger) *UserService {
    return &UserService{repo: repo, logger: logger}
}

// Bad: Global state
var globalDB *sql.DB

type UserService struct{}

func (s *UserService) GetUser(id string) (User, error) {
    return globalDB.QueryRow(...) // Bad: hidden dependency
}
```

### 6. Table-Driven Tests

Co-locate tests with production code:

```go
// internal/domain/money_test.go
package domain

import (
    "testing"

    "github.com/stretchr/testify/assert"
)

func TestMoney_Add(t *testing.T) {
    tests := []struct {
        name    string
        m1      Money
        m2      Money
        want    Money
        wantErr bool
    }{
        {
            name: "add same currency",
            m1:   Money{amount: 100, currency: "USD"},
            m2:   Money{amount: 50, currency: "USD"},
            want: Money{amount: 150, currency: "USD"},
        },
        {
            name:    "add different currency",
            m1:      Money{amount: 100, currency: "USD"},
            m2:      Money{amount: 50, currency: "EUR"},
            wantErr: true,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got, err := tt.m1.Add(tt.m2)
            if tt.wantErr {
                assert.Error(t, err)
                return
            }
            assert.NoError(t, err)
            assert.Equal(t, tt.want, got)
        })
    }
}
```

## Common Mistakes

### 1. Over-Nesting in pkg/

Don't create deep hierarchies in `pkg/`. Keep it flat and minimal.

```go
// Bad
pkg/utils/helpers/strings/converter/case.go

// Good
pkg/strutil/case.go
```

### 2. Mixing Concerns in cmd/

Keep `cmd/` focused on wiring dependencies and starting the application. Business logic belongs elsewhere.

```go
// Bad
func main() {
    // 100 lines of business logic
    if user.Balance < transaction.Amount {
        // ...
    }
}

// Good
func main() {
    cfg := config.Load()
    svc := service.New(cfg)
    svc.Start()
}
```

### 3. Exposing Too Much in pkg/

Only put stable, reusable code in `pkg/`. When in doubt, use `internal/`.

## Related Documentation

- [Go Best Practices](../ex-so-stla-go__best-practices.md) - Coding standards
- [Go Idioms](../ex-so-stla-go__idioms.md) - Go-specific patterns
- [Go Domain-Driven Design](../ex-so-stla-go__domain-driven-design.md) - DDD structure
- [Go Test-Driven Development](../ex-so-stla-go__test-driven-development.md) - Testing patterns

---

**Last Updated**: 2026-01-22
**Go Version**: 1.25.6
