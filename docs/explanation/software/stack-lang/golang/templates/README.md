# Go Templates

This directory contains standardized code templates and examples for Go development, designed to accelerate development while maintaining consistency and quality across the Open Sharia Enterprise platform.

## Go Philosophy: Simplicity and Composition

Go templates emphasize **composition over inheritance**. Unlike Java's class hierarchies, Go uses:

- **Structs for data** - Simple data containers without behavior inheritance
- **Interfaces for behavior** - Small, focused contracts (often 1-2 methods)
- **Embedding for composition** - Struct embedding replaces inheritance
- **Explicit over implicit** - No magic, no hidden dependencies

**Key Difference from Java**: Go has no classes, no inheritance, no generics polymorphism (before Go 1.18), and no exceptions. Templates reflect these constraints through explicit error handling, interface-based abstraction, and struct composition.

## Available Templates

### Application Structure Templates

#### [Project Structure Template](./project-structure.md)

Standard Go project layout following community conventions with hexagonal architecture.

**Key Features:**

- `cmd/` for application entry points
- `internal/` for private application code (domain, application, infrastructure)
- `pkg/` for public library code (minimal usage)
- `api/` for OpenAPI and Protobuf definitions
- TestContainers for integration tests

**Use When:** Setting up new Go projects, organizing existing codebases

**Comparison with Java**: Go enforces `internal/` privacy at compile time (vs Java's package-private). No `src/` directory - Go modules define project roots.

### Service Templates

#### [HTTP Server Example](./http-server-example.md)

Complete HTTP server implementation using Go 1.22+ enhanced routing with standard library.

**Key Features:**

- Go 1.22+ method-specific handlers (`GET /users/{id}`)
- Structured logging with `log/slog`
- Graceful shutdown patterns
- Middleware chain (logging, recovery, CORS)
- `httptest` for handler testing

**Use When:** Building REST APIs without third-party routers, leveraging Go 1.22+ routing improvements

**Comparison with Java**: No Spring Boot annotations - explicit route registration. No dependency injection framework - manual wiring in `main.go`.

#### [gRPC Service Example](./grpc-service-example.md)

Production-ready gRPC service with Protocol Buffers, interceptors, and streaming.

**Key Features:**

- Proto3 definitions with `protoc-gen-go`
- Unary and streaming RPCs (server, client, bidirectional)
- Interceptors for logging, recovery, authentication
- Error handling with `status.Error()` codes
- Graceful shutdown with `GracefulStop()`

**Use When:** Building microservices with gRPC, implementing streaming APIs, requiring strong API contracts

**Comparison with Java**: No annotation-based service definitions - explicit interface implementation. Manual interceptor chaining vs Spring's declarative AOP.

### Domain-Driven Design Templates

Go's approach to DDD differs from Java due to lack of inheritance. Templates demonstrate idiomatic Go patterns:

#### Entity Template (Planned)

DDD entity with identity, lifecycle, and business logic using Go structs.

**Key Features (Expected):**

- Identity-based equality (compare IDs, not structs)
- Factory functions for creation (no constructors)
- Unexported fields with exported getters
- State transition methods with validation
- Financial domain example (Zakat Account)

**Use When:** Creating entities with identity and mutable state

**Go-Specific Patterns:**

```go
type ZakatAccount struct {
  id        string        // Unexported field
  balance   Money         // Value object embedding
  status    AccountStatus // Enum via type alias
  createdAt time.Time
}

// Factory function replaces constructor
func NewZakatAccount(id string, initialBalance Money) (*ZakatAccount, error) {
  if id == "" {
    return nil, fmt.Errorf("id cannot be empty")
  }
  return &ZakatAccount{id: id, balance: initialBalance}, nil
}

// Exported getter for private field
func (a *ZakatAccount) ID() string {
  return a.id
}

// Business method with validation
func (a *ZakatAccount) Deposit(amount Money) error {
  if amount.Amount() <= 0 {
    return fmt.Errorf("amount must be positive")
  }
  a.balance = a.balance.Add(amount)
  return nil
}
```

**Comparison with Java**: No `private` keyword - use unexported fields (lowercase). No inheritance - use composition. No exceptions - return `error`.

#### Value Object Template (Planned)

Immutable value object with value-based equality using Go structs.

**Key Features (Expected):**

- Complete immutability (unexported fields, no setters)
- Value-based equality with custom `Equals()` method
- Self-validation in factory functions
- Side-effect free operations (return new instances)
- Financial domain example (Money)

**Use When:** Modeling concepts without identity (amounts, dates, addresses)

**Go-Specific Patterns:**

```go
type Money struct {
  amount   int64  // Unexported - immutable
  currency string // Unexported - immutable
}

// Factory with validation
func NewMoney(amount int64, currency string) (Money, error) {
  if currency == "" {
    return Money{}, fmt.Errorf("currency cannot be empty")
  }
  return Money{amount: amount, currency: currency}, nil
}

// Read-only getters
func (m Money) Amount() int64 {
  return m.amount
}

// Operations return new instances (immutability)
func (m Money) Add(other Money) (Money, error) {
  if m.currency != other.currency {
    return Money{}, fmt.Errorf("currency mismatch: %s vs %s", m.currency, other.currency)
  }
  return Money{amount: m.amount + other.amount, currency: m.currency}, nil
}

// Value-based equality
func (m Money) Equals(other Money) bool {
  return m.amount == other.amount && m.currency == other.currency
}
```

**Comparison with Java**: No `final` keyword needed - unexported fields are effectively immutable. No `equals()`/`hashCode()` overrides - provide explicit `Equals()` method.

#### Aggregate Template (Planned)

Aggregate root enforcing transactional boundaries with child entity management.

**Key Features (Expected):**

- Transactional boundary definition
- Child entity management through root only (no public access)
- Aggregate-level invariant enforcement
- Domain event registration (event slice)
- Financial domain example (Donation Campaign)

**Use When:** Defining consistency boundaries, managing complex entity graphs, enforcing invariants across multiple entities

**Go-Specific Patterns:**

```go
type DonationCampaign struct {
  id           string
  donations    []*Donation // Private slice - managed through root
  goalAmount   Money
  currentTotal Money
  events       []DomainEvent // Event registration
}

// Add child entity through root (enforces invariants)
func (c *DonationCampaign) AddDonation(amount Money, donor string) error {
  // Validate aggregate-level invariant
  newTotal := c.currentTotal.Add(amount)
  if newTotal.Amount() > c.goalAmount.Amount()*2 {
    return fmt.Errorf("donation would exceed 2x goal")
  }

  donation := NewDonation(amount, donor, c.id)
  c.donations = append(c.donations, donation)
  c.currentTotal = newTotal

  // Register domain event
  c.events = append(c.events, DonationReceivedEvent{...})

  return nil
}

// Domain events accessor (read-only copy)
func (c *DonationCampaign) DomainEvents() []DomainEvent {
  return append([]DomainEvent{}, c.events...) // Return copy
}
```

**Comparison with Java**: No `@Transactional` annotation - aggregate defines boundary explicitly. No JPA cascading - manual child management in code.

#### Domain Event Template (Planned)

Immutable domain event capturing significant business occurrences.

**Key Features (Expected):**

- Immutable struct design (unexported fields)
- Past-tense naming convention (`ZakatPaymentProcessed`)
- Event metadata (ID, timestamp, aggregate ID)
- Event handler interface pattern
- Financial domain examples (ZakatPaymentProcessed, DonationReceived)

**Use When:** Capturing domain state changes, enabling loose coupling between aggregates, building event-sourced systems

**Go-Specific Patterns:**

```go
type ZakatPaymentProcessed struct {
  eventID     string
  aggregateID string
  amount      Money
  timestamp   time.Time
  metadata    map[string]string
}

// Factory for event creation
func NewZakatPaymentProcessed(aggregateID string, amount Money) ZakatPaymentProcessed {
  return ZakatPaymentProcessed{
    eventID:     uuid.New().String(),
    aggregateID: aggregateID,
    amount:      amount,
    timestamp:   time.Now(),
    metadata:    make(map[string]string),
  }
}

// Event handler interface (small, focused)
type EventHandler interface {
  Handle(ctx context.Context, event DomainEvent) error
}

// Type assertion for specific event handling
type ZakatPaymentHandler struct {
  repo ZakatRepository
}

func (h *ZakatPaymentHandler) Handle(ctx context.Context, event DomainEvent) error {
  evt, ok := event.(ZakatPaymentProcessed)
  if !ok {
    return fmt.Errorf("unexpected event type")
  }
  // Handle event...
  return nil
}
```

**Comparison with Java**: No event inheritance hierarchy - use interfaces. No Spring `@EventListener` - manual event routing with type assertions.

### Infrastructure Templates

#### Repository Template (Planned)

DDD repository with collection-like interface and business-oriented queries.

**Key Features (Expected):**

- Interface definition in domain layer
- Implementation in infrastructure layer
- Collection-like methods (`Save()`, `FindByID()`, `Remove()`)
- Business-oriented query methods (`FindByNisabStatus()`)
- Specification pattern for complex queries (functional options)

**Use When:** Persisting aggregates, querying by business criteria, implementing data access layer

**Go-Specific Patterns:**

```go
// Domain layer interface
type ZakatAccountRepository interface {
  Save(ctx context.Context, account *ZakatAccount) error
  FindByID(ctx context.Context, id string) (*ZakatAccount, error)
  FindByNisabStatus(ctx context.Context, status NisabStatus) ([]*ZakatAccount, error)
  Remove(ctx context.Context, id string) error
}

// Infrastructure implementation
type PostgresZakatAccountRepository struct {
  db *sql.DB
}

func (r *PostgresZakatAccountRepository) Save(ctx context.Context, account *ZakatAccount) error {
  query := `INSERT INTO zakat_accounts (id, balance, status) VALUES ($1, $2, $3)
            ON CONFLICT (id) DO UPDATE SET balance = $2, status = $3`
  _, err := r.db.ExecContext(ctx, query, account.ID(), account.Balance(), account.Status())
  return err
}

// Specification pattern with functional options
type AccountFilter func(*sql.Rows) ([]*ZakatAccount, error)

func WithMinBalance(min Money) AccountFilter {
  return func(rows *sql.Rows) ([]*ZakatAccount, error) {
    // Filter logic
  }
}
```

**Comparison with Java**: No Spring Data JPA - manual SQL or ORM integration. No `@Repository` annotation - explicit dependency injection in `main.go`.

#### Service Layer Template (Planned)

Application service orchestrating domain operations and managing transactions.

**Key Features (Expected):**

- Transaction management with `database/sql` or custom interface
- Cross-aggregate orchestration
- Domain event publishing to event bus
- Request/response structs (DTOs)
- Financial domain example (Zakat Calculation Service)

**Use When:** Coordinating complex operations across multiple aggregates, managing transaction boundaries, integrating external services

**Go-Specific Patterns:**

```go
// Request/Response structs (plain structs, not classes)
type CalculateZakatRequest struct {
  AccountID string
  Year      int
}

type CalculateZakatResponse struct {
  ZakatDue Money
  Rate     float64
}

// Application service
type ZakatCalculationService struct {
  accountRepo ZakatAccountRepository
  eventBus    EventBus
  txManager   TransactionManager
}

func (s *ZakatCalculationService) CalculateZakat(ctx context.Context, req CalculateZakatRequest) (*CalculateZakatResponse, error) {
  // Start transaction
  tx, err := s.txManager.Begin(ctx)
  if err != nil {
    return nil, fmt.Errorf("failed to start transaction: %w", err)
  }
  defer tx.Rollback()

  // Fetch account
  account, err := s.accountRepo.FindByID(ctx, req.AccountID)
  if err != nil {
    return nil, fmt.Errorf("account not found: %w", err)
  }

  // Domain logic
  zakatDue, err := account.CalculateZakat(req.Year)
  if err != nil {
    return nil, err
  }

  // Save changes
  if err := s.accountRepo.Save(ctx, account); err != nil {
    return nil, err
  }

  // Publish event
  s.eventBus.Publish(ctx, NewZakatCalculatedEvent(account.ID(), zakatDue))

  // Commit transaction
  if err := tx.Commit(); err != nil {
    return nil, fmt.Errorf("failed to commit: %w", err)
  }

  return &CalculateZakatResponse{ZakatDue: zakatDue, Rate: 0.025}, nil
}
```

**Comparison with Java**: No `@Transactional` annotation - explicit transaction management. No dependency injection container - manual wiring.

### Build & Configuration Templates

#### Build Configuration Template (Planned)

Go module configuration with dependency management, testing, and CI/CD integration.

**Key Features (Expected):**

- `go.mod` and `go.sum` for dependency management
- Makefile for build automation
- Build tags for integration tests (`//go:build integration`)
- Code coverage with `go test -cover`
- Linting with `golangci-lint`

**Use When:** Setting up new Go projects, standardizing build processes, configuring CI/CD pipelines

**Go-Specific Patterns:**

```makefile
.PHONY: test test-integration lint build

test:
 @go test -v -race -cover ./...

test-integration:
 @go test -v -race -tags=integration ./test/integration/...

lint:
 @golangci-lint run

build:
 @go build -o bin/server ./cmd/server
```

```go
//go:build integration

package integration

import (
  "testing"
  "github.com/testcontainers/testcontainers-go"
)

func TestUserRepository_Integration(t *testing.T) {
  // Integration test with TestContainers
}
```

**Comparison with Java**: No Maven/Gradle XML/Groovy - declarative `go.mod`. Build tags replace Maven profiles. `go test` is built-in (no JUnit dependency).

## Usage Guidelines

### Getting Started

1. **Choose the Right Template**: Select the template matching your use case (HTTP vs gRPC, entity vs value object)
2. **Copy and Customize**: Copy the template and replace placeholder values (change package names, domain concepts)
3. **Follow Go Conventions**: Maintain idiomatic Go patterns (exported/unexported naming, error handling, interfaces)
4. **Add Financial Context**: Use Sharia-compliant financial examples where applicable

### Template Naming Convention

Template files follow the repository pattern: `{descriptive-name}.md` (no ex-so-stla-go prefix in templates/ directory)

### Common Patterns Across Templates

#### Validation

All templates emphasize early validation in factory functions:

```go
func NewMoney(amount int64, currency string) (Money, error) {
  if currency == "" {
    return Money{}, fmt.Errorf("currency cannot be empty")
  }
  if amount < 0 {
    return Money{}, fmt.Errorf("amount cannot be negative")
  }
  return Money{amount: amount, currency: currency}, nil
}
```

#### Immutability

Value objects use unexported fields and return new instances:

```go
type Money struct {
  amount   int64  // Unexported - immutable
  currency string // Unexported - immutable
}

// Operations return new instances
func (m Money) Add(other Money) (Money, error) {
  // Validation...
  return Money{amount: m.amount + other.amount, currency: m.currency}, nil
}
```

#### Factory Functions

Prefer factory functions over direct struct initialization:

```go
// Good: Validation in factory
func NewUser(email, name string) (*User, error) {
  if email == "" {
    return nil, fmt.Errorf("email is required")
  }
  return &User{email: email, name: name}, nil
}

// Bad: Direct initialization bypasses validation
user := &User{email: "", name: "Bob"} // No validation!
```

#### Explicit Error Handling

Use error wrapping with `fmt.Errorf` and `%w`:

```go
account, err := repo.FindByID(ctx, id)
if err != nil {
  return nil, fmt.Errorf("failed to find account %s: %w", id, err)
}

// Check for specific errors
if errors.Is(err, ErrAccountNotFound) {
  return nil, status.Error(codes.NotFound, "account not found")
}
```

#### Interface-Based Abstraction

Keep interfaces small and focused (often 1-3 methods):

```go
// Good: Small, focused interface
type UserRepository interface {
  Save(ctx context.Context, user *User) error
  FindByID(ctx context.Context, id string) (*User, error)
}

// Bad: Large interface (violates Interface Segregation Principle)
type UserRepository interface {
  Save(ctx context.Context, user *User) error
  FindByID(ctx context.Context, id string) (*User, error)
  FindByEmail(ctx context.Context, email string) (*User, error)
  FindAll(ctx context.Context) ([]*User, error)
  Delete(ctx context.Context, id string) error
  Update(ctx context.Context, user *User) error
  Count(ctx context.Context) (int, error)
  // 20 more methods...
}
```

## Financial Domain Integration

All templates include examples from the financial domain relevant to Open Sharia Enterprise:

- **Zakat Calculation**: 2.5% wealth tax with nisab threshold and haul period
- **Donation Processing**: Campaign management with beneficiaries
- **Account Management**: Sharia-compliant wealth tracking
- **Payment Processing**: Transaction recording and receipt generation

These examples demonstrate:

- Islamic finance principles in Go (immutable Money values)
- Regulatory compliance requirements (audit trails, event sourcing)
- Real-world business complexity (multi-aggregate transactions)
- Domain-driven design in Go (composition over inheritance)

## Quality Standards

All templates enforce these quality standards:

- **Test Coverage**: 80% line coverage (checked with `go test -cover`)
- **Code Linting**: Zero `golangci-lint` warnings
- **Error Handling**: All errors wrapped with context (`fmt.Errorf` with `%w`)
- **Documentation**: Package-level and exported symbol documentation
- **Formatting**: `gofmt` and `goimports` applied

## Related Documentation

### Core Go Documentation

- [Go Best Practices](../ex-so-stla-go__best-practices.md) - Coding standards and idiomatic patterns
- [Go Idioms](../ex-so-stla-go__idioms.md) - Go-specific patterns and conventions
- [Domain-Driven Design](../ex-so-stla-go__domain-driven-design.md) - DDD structure in Go
- [Test-Driven Development](../ex-so-stla-go__test-driven-development.md) - Testing patterns and table-driven tests
- [Interfaces and Composition](../ex-so-stla-go__interfaces-and-composition.md) - Interface design and struct embedding
- [Error Handling](../ex-so-stla-go__error-handling.md) - Error wrapping and sentinel errors

### Go Release Documentation

- [Go 1.18 Release](../ex-so-stla-go__1.18-release.md) - Generics introduction
- [Go 1.22 Release](../ex-so-stla-go__1.22-release.md) - Enhanced HTTP routing
- [Go 1.25 Release](../ex-so-stla-go__1.25-release.md) - Latest features

## Contributing

When creating new templates:

1. Follow idiomatic Go patterns (composition, interfaces, error handling)
2. Include comprehensive code examples with comments
3. Add financial domain examples where applicable
4. Document Go-specific patterns (unexported fields, factory functions)
5. Compare with Java equivalents where helpful
6. Link to related templates and documentation
7. Update this README with the new template

## Version History

- **2026-01-22**: Templates directory created
  - Project Structure Template
  - HTTP Server Example (Go 1.22+ routing)
  - gRPC Service Example
  - Planned: Entity, Value Object, Aggregate, Domain Event, Repository, Service Layer, Build Configuration templates

---

**License**: MIT
**Maintained By**: Open Sharia Enterprise Platform Team
**Go Version**: 1.25.6 (current), 1.22+ (minimum for HTTP routing)

---

**Last Updated**: 2025-01-23
**Go Version**: 1.18+
