---
title: Go Build Configuration Template
description: Template for Makefile, go.mod, Dockerfile, and CI/CD configurations with testing, linting, and deployment automation for Go projects
category: template
tags:
  - golang
  - makefile
  - docker
  - ci-cd
  - build-automation
  - dependencies
  - linting
  - testing
related:
  - ex-so-prla-go__modules-and-dependencies.md
  - ex-so-prla-go__best-practices.md
  - ex-so-prla-go__linting-and-formatting.md
principles:
  - reproducibility
  - automation-over-manual
  - explicit-over-implicit
---

# Go Build Configuration Template

This template provides standardized build configurations for Go projects using Makefile, go.mod, golangci-lint, Docker multi-stage builds, and GitHub Actions CI/CD. The example implements a Zakat calculation service demonstrating complete build automation for financial domain applications.

## 📋 Table of Contents

1. [Overview](#-overview)
2. [Makefile Patterns](#-makefile-patterns)
3. [go.mod Best Practices](#-gomod-best-practices)
4. [golangci-lint Configuration](#-golangci-lint-configuration)
5. [Dockerfile Multi-Stage Build](#-dockerfile-multi-stage-build)
6. [CI/CD Patterns](#-cicd-patterns)
7. [Complete Example Project](#-complete-example-project)
8. [Usage Guidelines](#-usage-guidelines)

## 📊 Overview

### Build Automation in Go

Go build automation serves three primary goals:

1. **Reproducibility**: Same code produces same binary across environments
2. **Consistency**: Standardized build process for all team members
3. **Automation**: Eliminate manual steps and human error

**Key components**:

- **Makefile**: Central build automation (build, test, lint, clean)
- **go.mod**: Dependency management and version pinning
- **golangci-lint**: Comprehensive code quality checks
- **Dockerfile**: Container builds with security best practices
- **GitHub Actions**: CI/CD pipeline automation

### Why This Matters

**Without build automation**:

```bash
# Manual, error-prone process
go build -o server cmd/server/main.go
go test ./...
golangci-lint run
docker build .

# Different developers use different flags
# No version information in binaries
# Inconsistent builds across environments
```

**With build automation**:

```bash
# Standardized, reproducible
make build
make test
make lint
make docker-build

# Everyone uses same configuration
# Version info automatically injected
# Consistent builds everywhere
```

## 🛠️ Makefile Patterns

### Complete Makefile Template

```makefile
# ========================================
# Project Configuration
# ========================================

# Binary names
BINARY_NAME=zakat-calculator
BINARY_LINUX=$(BINARY_NAME)-linux
BINARY_DARWIN=$(BINARY_NAME)-darwin
BINARY_WINDOWS=$(BINARY_NAME).exe

# Directories
BUILD_DIR=bin
CMD_DIR=cmd/server
MAIN_FILE=$(CMD_DIR)/main.go

# Version information (injected at build time)
VERSION=$(shell git describe --tags --always --dirty)
COMMIT=$(shell git rev-parse --short HEAD)
BUILD_TIME=$(shell date -u '+%Y-%m-%d_%H:%M:%S')
BUILD_USER=$(shell whoami)
BUILD_HOST=$(shell hostname)

# Go configuration
GOCMD=go
GOBUILD=$(GOCMD) build
GOCLEAN=$(GOCMD) clean
GOTEST=$(GOCMD) test
GOGET=$(GOCMD) get
GOMOD=$(GOCMD) mod

# Linker flags (inject version info)
LDFLAGS=-ldflags "\
 -s -w \
 -X main.Version=$(VERSION) \
 -X main.GitCommit=$(COMMIT) \
 -X main.BuildTime=$(BUILD_TIME) \
 -X main.BuildUser=$(BUILD_USER) \
 -X main.BuildHost=$(BUILD_HOST)"

# Build flags (reproducible builds)
BUILD_FLAGS=-trimpath $(LDFLAGS)

# Test flags
TEST_FLAGS=-v -race -cover -coverprofile=coverage.out

# Linter
GOLANGCI_LINT=golangci-lint
GOLANGCI_LINT_VERSION=v1.61.0

# Docker configuration
DOCKER_IMAGE=zakat-calculator
DOCKER_TAG=$(VERSION)
DOCKER_REGISTRY=ghcr.io/openshariaenterprise

# ========================================
# Targets
# ========================================

.PHONY: all
all: clean deps lint test build

# ========================================
# Build Targets
# ========================================

.PHONY: build
build: ## Build binary for current platform
 @echo "Building $(BINARY_NAME) $(VERSION)..."
 @mkdir -p $(BUILD_DIR)
 $(GOBUILD) $(BUILD_FLAGS) -o $(BUILD_DIR)/$(BINARY_NAME) $(MAIN_FILE)
 @echo "Build complete: $(BUILD_DIR)/$(BINARY_NAME)"

.PHONY: build-all
build-all: build-linux build-darwin build-windows ## Build for all platforms

.PHONY: build-linux
build-linux: ## Build for Linux
 @echo "Building for Linux..."
 @mkdir -p $(BUILD_DIR)
 GOOS=linux GOARCH=amd64 $(GOBUILD) $(BUILD_FLAGS) -o $(BUILD_DIR)/$(BINARY_LINUX) $(MAIN_FILE)
 @echo "Linux build: $(BUILD_DIR)/$(BINARY_LINUX)"

.PHONY: build-darwin
build-darwin: ## Build for macOS
 @echo "Building for macOS..."
 @mkdir -p $(BUILD_DIR)
 GOOS=darwin GOARCH=amd64 $(GOBUILD) $(BUILD_FLAGS) -o $(BUILD_DIR)/$(BINARY_DARWIN) $(MAIN_FILE)
 @echo "macOS build: $(BUILD_DIR)/$(BINARY_DARWIN)"

.PHONY: build-windows
build-windows: ## Build for Windows
 @echo "Building for Windows..."
 @mkdir -p $(BUILD_DIR)
 GOOS=windows GOARCH=amd64 $(GOBUILD) $(BUILD_FLAGS) -o $(BUILD_DIR)/$(BINARY_WINDOWS) $(MAIN_FILE)
 @echo "Windows build: $(BUILD_DIR)/$(BINARY_WINDOWS)"

# ========================================
# Development Targets
# ========================================

.PHONY: run
run: ## Run application locally
 @echo "Running $(BINARY_NAME)..."
 $(GOCMD) run $(MAIN_FILE)

.PHONY: dev
dev: ## Run with hot reload (requires air)
 @echo "Starting development server with hot reload..."
 @which air > /dev/null || (echo "Installing air..." && go install github.com/air-verse/air@latest)
 air

# ========================================
# Test Targets
# ========================================

.PHONY: test
test: ## Run tests
 @echo "Running tests..."
 $(GOTEST) $(TEST_FLAGS) ./...

.PHONY: test-unit
test-unit: ## Run unit tests only
 @echo "Running unit tests..."
 $(GOTEST) $(TEST_FLAGS) -short ./...

.PHONY: test-integration
test-integration: ## Run integration tests only
 @echo "Running integration tests..."
 $(GOTEST) $(TEST_FLAGS) -run Integration ./...

.PHONY: test-coverage
test-coverage: test ## Generate coverage report
 @echo "Generating coverage report..."
 $(GOCMD) tool cover -html=coverage.out -o coverage.html
 @echo "Coverage report: coverage.html"

.PHONY: test-coverage-ci
test-coverage-ci: test ## Generate coverage for CI
 @echo "Generating coverage for CI..."
 $(GOCMD) tool cover -func=coverage.out

.PHONY: bench
bench: ## Run benchmarks
 @echo "Running benchmarks..."
 $(GOTEST) -bench=. -benchmem ./...

# ========================================
# Code Quality Targets
# ========================================

.PHONY: lint
lint: ## Run linter
 @echo "Running golangci-lint..."
 @which $(GOLANGCI_LINT) > /dev/null || (echo "Installing golangci-lint..." && \
  curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $$(go env GOPATH)/bin $(GOLANGCI_LINT_VERSION))
 $(GOLANGCI_LINT) run

.PHONY: lint-fix
lint-fix: ## Run linter and auto-fix issues
 @echo "Running golangci-lint with auto-fix..."
 $(GOLANGCI_LINT) run --fix

.PHONY: fmt
fmt: ## Format code
 @echo "Formatting code..."
 @gofmt -s -w .
 @goimports -w .

.PHONY: vet
vet: ## Run go vet
 @echo "Running go vet..."
 $(GOCMD) vet ./...

# ========================================
# Dependency Targets
# ========================================

.PHONY: deps
deps: ## Download dependencies
 @echo "Downloading dependencies..."
 $(GOMOD) download
 $(GOMOD) verify

.PHONY: deps-update
deps-update: ## Update dependencies
 @echo "Updating dependencies..."
 $(GOGET) -u ./...
 $(GOMOD) tidy

.PHONY: deps-tidy
deps-tidy: ## Tidy dependencies
 @echo "Tidying dependencies..."
 $(GOMOD) tidy
 $(GOMOD) verify

.PHONY: deps-vendor
deps-vendor: ## Vendor dependencies
 @echo "Vendoring dependencies..."
 $(GOMOD) vendor

# ========================================
# Docker Targets
# ========================================

.PHONY: docker-build
docker-build: ## Build Docker image
 @echo "Building Docker image $(DOCKER_IMAGE):$(DOCKER_TAG)..."
 docker build \
  --build-arg VERSION=$(VERSION) \
  --build-arg GIT_COMMIT=$(COMMIT) \
  --build-arg BUILD_TIME=$(BUILD_TIME) \
  -t $(DOCKER_IMAGE):$(DOCKER_TAG) \
  -t $(DOCKER_IMAGE):latest \
  .

.PHONY: docker-run
docker-run: ## Run Docker container
 @echo "Running Docker container..."
 docker run --rm -p 8080:8080 $(DOCKER_IMAGE):latest

.PHONY: docker-push
docker-push: ## Push Docker image to registry
 @echo "Pushing to $(DOCKER_REGISTRY)/$(DOCKER_IMAGE):$(DOCKER_TAG)..."
 docker tag $(DOCKER_IMAGE):$(DOCKER_TAG) $(DOCKER_REGISTRY)/$(DOCKER_IMAGE):$(DOCKER_TAG)
 docker tag $(DOCKER_IMAGE):latest $(DOCKER_REGISTRY)/$(DOCKER_IMAGE):latest
 docker push $(DOCKER_REGISTRY)/$(DOCKER_IMAGE):$(DOCKER_TAG)
 docker push $(DOCKER_REGISTRY)/$(DOCKER_IMAGE):latest

.PHONY: docker-scan
docker-scan: ## Scan Docker image for vulnerabilities
 @echo "Scanning Docker image for vulnerabilities..."
 docker scout cves $(DOCKER_IMAGE):$(DOCKER_TAG)

# ========================================
# Security Targets
# ========================================

.PHONY: security-scan
security-scan: ## Run security vulnerability scan
 @echo "Running security scan with govulncheck..."
 @which govulncheck > /dev/null || (echo "Installing govulncheck..." && \
  go install golang.org/x/vuln/cmd/govulncheck@latest)
 govulncheck ./...

.PHONY: security-gosec
security-gosec: ## Run gosec security scanner
 @echo "Running gosec security scanner..."
 @which gosec > /dev/null || (echo "Installing gosec..." && \
  go install github.com/securego/gosec/v2/cmd/gosec@latest)
 gosec -fmt=json -out=gosec-report.json ./...

# ========================================
# Clean Targets
# ========================================

.PHONY: clean
clean: ## Clean build artifacts
 @echo "Cleaning build artifacts..."
 $(GOCLEAN)
 rm -rf $(BUILD_DIR)
 rm -f coverage.out coverage.html
 rm -f gosec-report.json

.PHONY: clean-all
clean-all: clean ## Clean all generated files
 @echo "Cleaning all generated files..."
 rm -rf vendor/
 $(GOCMD) clean -cache -testcache -modcache

# ========================================
# Install Targets
# ========================================

.PHONY: install-tools
install-tools: ## Install development tools
 @echo "Installing development tools..."
 go install github.com/golangci/golangci-lint/cmd/golangci-lint@$(GOLANGCI_LINT_VERSION)
 go install golang.org/x/vuln/cmd/govulncheck@latest
 go install github.com/securego/gosec/v2/cmd/gosec@latest
 go install github.com/air-verse/air@latest
 go install golang.org/x/tools/cmd/goimports@latest

# ========================================
# CI Targets
# ========================================

.PHONY: ci
ci: deps lint test build ## Run CI pipeline locally

.PHONY: ci-security
ci-security: security-scan security-gosec ## Run security checks

# ========================================
# Help Target
# ========================================

.PHONY: help
help: ## Show this help message
 @echo "Usage: make [target]"
 @echo ""
 @echo "Targets:"
 @grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2}'

.DEFAULT_GOAL := help
```

### Usage Examples

```bash
# Development workflow
make deps              # Download dependencies
make fmt               # Format code
make lint              # Run linter
make test              # Run tests
make build             # Build binary

# CI/CD workflow
make ci                # Run full CI pipeline
make ci-security       # Run security scans
make docker-build      # Build Docker image
make docker-push       # Push to registry

# Cross-platform builds
make build-all         # Build for all platforms
make build-linux       # Linux binary
make build-darwin      # macOS binary
make build-windows     # Windows binary

# Development aids
make run               # Run locally
make dev               # Hot reload development
make test-coverage     # Coverage report
make help              # Show all targets
```

## 📦 go.mod Best Practices

### Complete go.mod Template

```go
module github.com/openshariaenterprise/zakat-calculator

go 1.25

// ========================================
// Direct Dependencies
// ========================================
require (
 github.com/gin-gonic/gin v1.10.0
 github.com/shopspring/decimal v1.4.0
 github.com/stretchr/testify v1.9.0
 go.uber.org/zap v1.27.0
 gorm.io/driver/postgres v1.5.9
 gorm.io/gorm v1.25.12
)

// ========================================
// Indirect Dependencies (managed by go mod tidy)
// ========================================
require (
 github.com/bytedance/sonic v1.11.6 // indirect
 github.com/gabriel-vasile/mimetype v1.4.3 // indirect
 github.com/gin-contrib/sse v0.1.0 // indirect
 github.com/go-playground/locales v0.14.1 // indirect
 github.com/go-playground/universal-translator v0.18.1 // indirect
 github.com/go-playground/validator/v10 v10.20.0 // indirect
 github.com/goccy/go-json v0.10.2 // indirect
 github.com/jackc/pgpassfile v1.0.0 // indirect
 github.com/jackc/pgservicefile v0.0.0-20240606120523-5a60cdf6a761 // indirect
 github.com/jackc/pgx/v5 v5.6.0 // indirect
 github.com/jinzhu/inflection v1.0.0 // indirect
 github.com/jinzhu/now v1.1.5 // indirect
 github.com/json-iterator/go v1.1.12 // indirect
 github.com/klauspost/cpuid/v2 v2.2.7 // indirect
 github.com/leodido/go-urn v1.4.0 // indirect
 github.com/mattn/go-isatty v0.0.20 // indirect
 github.com/modern-go/concurrent v0.0.0-20180306012644-bacd9c7ef1dd // indirect
 github.com/modern-go/reflect2 v1.0.2 // indirect
 github.com/pelletier/go-toml/v2 v2.2.2 // indirect
 github.com/pmezard/go-difflib v1.0.0 // indirect
 github.com/twitchyliquid64/golang-asm v0.15.1 // indirect
 github.com/ugorji/go/codec v1.2.12 // indirect
 go.uber.org/multierr v1.11.0 // indirect
 golang.org/x/arch v0.8.0 // indirect
 golang.org/x/crypto v0.25.0 // indirect
 golang.org/x/net v0.27.0 // indirect
 golang.org/x/sys v0.22.0 // indirect
 golang.org/x/text v0.16.0 // indirect
 google.golang.org/protobuf v1.34.1 // indirect
 gopkg.in/yaml.v3 v3.0.1 // indirect
)

// ========================================
// Replace Directives
// ========================================

// Local development (comment out for production)
// replace github.com/openshariaenterprise/financial-common => ../financial-common

// Fork replacement (security fix or feature)
// replace github.com/old/broken => github.com/our/fixed v1.2.3

// Version override (specific version needed)
// replace github.com/some/dep v1.0.0 => github.com/some/dep v1.1.0

// ========================================
// Exclude Directives
// ========================================

// Exclude broken versions
// exclude github.com/some/dep v1.5.0
```

### Key Practices

**1. Module Declaration**

```go
// Use full GitHub path
module github.com/openshariaenterprise/zakat-calculator

// NOT generic names
// module zakat-calculator  // Avoid
```

**2. Go Version Requirement**

```go
// Specify minimum Go version
go 1.25

// This enables Go 1.25 features
// Fails build on Go 1.24 or earlier
```

**3. Dependency Pinning**

```go
// GOOD: Explicit versions
require (
 github.com/gin-gonic/gin v1.10.0
 github.com/shopspring/decimal v1.4.0
)

// AVOID: Pseudo-versions in production
require (
 github.com/some/lib v0.0.0-20240615123456-abcdef123456  // Avoid
)
```

**4. Replace Directives (Local Development)**

```go
// Development workflow
replace github.com/openshariaenterprise/financial-common => ../financial-common

// Test changes across modules before publishing
// Remove before committing to main branch
```

**5. Exclude Directives (Broken Versions)**

```go
// Exclude known broken version
exclude github.com/vulnerable/lib v1.5.0

// Forces go mod to skip this version
// Useful for security vulnerabilities
```

## 🔍 golangci-lint Configuration

### Complete .golangci.yml Template

```yaml
# ========================================
# golangci-lint Configuration
# https://golangci-lint.run/usage/configuration/
# ========================================

run:
  # Timeout for analysis
  timeout: 5m

  # Directories to skip
  skip-dirs:
    - vendor
    - bin
    - testdata
    - third_party

  # Files to skip
  skip-files:
    - ".*\\.pb\\.go$"
    - ".*_generated\\.go$"

  # Number of CPUs to use (0 = all available)
  concurrency: 0

  # Allow multiple parallel golangci-lint instances
  allow-parallel-runners: true

  # Go version to target
  go: "1.25"

# ========================================
# Output Configuration
# ========================================

output:
  # Format: colored-line-number|line-number|json|tab|checkstyle|code-climate
  formats:
    - format: colored-line-number
      path: stdout

  # Print lines of code with issue
  print-issued-lines: true

  # Print linter name in the end of issue text
  print-linter-name: true

  # Make issues output unique by line
  uniq-by-line: true

  # Sort results by file, line, and column
  sort-results: true

# ========================================
# Linters Configuration
# ========================================

linters:
  # Disable all linters by default
  disable-all: true

  # Enable specific linters
  enable:
    # Correctness
    - errcheck # Check for unchecked errors
    - gosimple # Simplify code
    - govet # Go vet built-in analyzer
    - ineffassign # Detect ineffectual assignments
    - staticcheck # Go static analysis
    - typecheck # Type checking
    - unused # Find unused code

    # Style
    - gofmt # Gofmt checks formatting
    - goimports # Goimports checks import formatting
    - revive # Flexible and configurable linter

    # Complexity
    - gocyclo # Cyclomatic complexity
    - gocognit # Cognitive complexity

    # Performance
    - prealloc # Find slice declarations that could pre-allocate

    # Security
    - gosec # Security problems

    # Bugs
    - bodyclose # Check HTTP response body is closed
    - gocritic # Meta-linter with many checks
    - noctx # HTTP requests should use context

    # Error handling
    - errname # Error names should have Error suffix
    - errorlint # Error wrapping issues

    # Testing
    - testpackage # Test files should be in _test package
    - tparallel # Detect inappropriate usage of t.Parallel()

    # Code quality
    - dupl # Code duplication detection
    - misspell # Spelling mistakes
    - unconvert # Unnecessary type conversions
    - unparam # Unused function parameters

linters-settings:
  # ========================================
  # errcheck: Check for unchecked errors
  # ========================================
  errcheck:
    # Report about not checking errors in type assertions
    check-type-assertions: true

    # Report about assignment of errors to blank identifier
    check-blank: true

    # List of functions to exclude from checking
    exclude-functions:
      - fmt.Print
      - fmt.Println
      - fmt.Printf

  # ========================================
  # govet: Go vet settings
  # ========================================
  govet:
    # Enable all analyzers
    enable-all: true

    # Disable specific analyzers
    disable:
      - shadow # Too many false positives

  # ========================================
  # gocyclo: Cyclomatic complexity
  # ========================================
  gocyclo:
    # Minimal cyclomatic complexity to report
    min-complexity: 15

  # ========================================
  # gocognit: Cognitive complexity
  # ========================================
  gocognit:
    # Minimal cognitive complexity to report
    min-complexity: 20

  # ========================================
  # dupl: Code duplication
  # ========================================
  dupl:
    # Tokens count to trigger duplication
    threshold: 100

  # ========================================
  # gocritic: Go critic settings
  # ========================================
  gocritic:
    # Enable multiple checks by tags
    enabled-tags:
      - diagnostic
      - performance
      - style

    disabled-checks:
      - commentedOutCode
      - whyNoLint

  # ========================================
  # revive: Flexible linter
  # ========================================
  revive:
    # Severity of issues to report (error, warning)
    severity: warning

    # Enable all rules
    enable-all-rules: false

    # Enable specific rules
    rules:
      - name: blank-imports
      - name: context-as-argument
      - name: context-keys-type
      - name: dot-imports
      - name: error-return
      - name: error-strings
      - name: error-naming
      - name: exported
      - name: increment-decrement
      - name: var-naming
      - name: package-comments
      - name: range
      - name: receiver-naming
      - name: indent-error-flow
      - name: errorf
      - name: empty-block
      - name: superfluous-else
      - name: unused-parameter
      - name: unreachable-code
      - name: redefines-builtin-id

  # ========================================
  # gosec: Security scanner
  # ========================================
  gosec:
    # Include test files
    tests: true

    # Exclude generated files
    exclude-generated: true

    # Severity level to report (low, medium, high)
    severity: low

    # Confidence level to report (low, medium, high)
    confidence: low

# ========================================
# Issues Configuration
# ========================================

issues:
  # Maximum issues count per linter
  max-issues-per-linter: 0

  # Maximum count of issues with same text
  max-same-issues: 0

  # Exclude issues by text
  exclude:
    - "G404: Use of weak random number generator" # crypto/rand used where needed

  # Exclude issues by path
  exclude-rules:
    # Exclude linters for test files
    - path: _test\.go
      linters:
        - dupl
        - gosec
        - errcheck

    # Exclude linters for main.go
    - path: cmd/
      linters:
        - gochecknoinits

  # Independently from option `exclude` we use default exclude patterns
  exclude-use-default: false

  # Show all issues from a linter
  new: false

  # Fix found issues (if supported by linter)
  fix: false
```

### Usage Examples

```bash
# Run all enabled linters
golangci-lint run

# Run with auto-fix
golangci-lint run --fix

# Run specific linters
golangci-lint run --enable=gosec,errcheck

# Generate report
golangci-lint run --out-format=json > lint-report.json

# Check only new code (since main branch)
golangci-lint run --new-from-rev=origin/main
```

## 🐳 Dockerfile Multi-Stage Build

### Complete Dockerfile Template

```dockerfile
# ========================================
# Multi-Stage Dockerfile for Go Application
# Optimized for security and size
# ========================================

# ========================================
# Stage 1: Dependencies
# ========================================
FROM golang:1.25-alpine AS deps

# Install build dependencies
RUN apk add --no-cache git ca-certificates tzdata

# Set working directory
WORKDIR /app

# Copy go.mod and go.sum
COPY go.mod go.sum ./

# Download dependencies (cached layer)
RUN go mod download
RUN go mod verify

# ========================================
# Stage 2: Builder
# ========================================
FROM golang:1.25-alpine AS builder

# Install build dependencies
RUN apk add --no-cache git ca-certificates tzdata make

# Set working directory
WORKDIR /app

# Copy dependencies from deps stage
COPY --from=deps /go/pkg /go/pkg

# Copy source code
COPY . .

# Build arguments for version info
ARG VERSION=dev
ARG GIT_COMMIT=unknown
ARG BUILD_TIME=unknown

# Build the application
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build \
  -trimpath \
  -ldflags="-s -w \
    -X main.Version=${VERSION} \
    -X main.GitCommit=${GIT_COMMIT} \
    -X main.BuildTime=${BUILD_TIME}" \
  -o /app/bin/zakat-calculator \
  ./cmd/server/main.go

# Verify binary
RUN /app/bin/zakat-calculator --version

# ========================================
# Stage 3: Final Image
# ========================================
FROM alpine:3.20

# Install runtime dependencies
RUN apk --no-cache add ca-certificates tzdata

# Create non-root user
RUN addgroup -g 1000 appuser && \
    adduser -D -u 1000 -G appuser appuser

# Set working directory
WORKDIR /app

# Copy binary from builder
COPY --from=builder /app/bin/zakat-calculator /app/zakat-calculator

# Copy configuration files (if any)
# COPY --from=builder /app/config /app/config

# Change ownership
RUN chown -R appuser:appuser /app

# Switch to non-root user
USER appuser

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD ["/app/zakat-calculator", "healthcheck"]

# Run application
ENTRYPOINT ["/app/zakat-calculator"]
CMD ["serve"]
```

### Docker Best Practices

**1. Multi-Stage Build Benefits**

```dockerfile
# BEFORE: Single-stage (large image)
FROM golang:1.25
COPY . .
RUN go build -o app
# Result: 1.2 GB image (includes Go toolchain)

# AFTER: Multi-stage (minimal image)
FROM golang:1.25 AS builder
COPY . .
RUN go build -o app

FROM alpine:3.20
COPY --from=builder /app/app .
# Result: 15 MB image (only binary + runtime deps)
```

**2. Security Best Practices**

```dockerfile
# Non-root user
RUN adduser -D -u 1000 appuser
USER appuser

# No secrets in layers
# Use build secrets: docker build --secret id=token,src=token.txt

# Minimal base image
FROM alpine:3.20  # 5 MB vs ubuntu:22.04 77 MB

# Health checks
HEALTHCHECK CMD ["/app/healthcheck"]
```

**3. Build Flags for Security**

```bash
# CGO disabled (static binary)
CGO_ENABLED=0

# Strip debug symbols (-s -w)
-ldflags="-s -w"

# Reproducible builds (-trimpath)
-trimpath
```

### docker-compose.yml for Local Development

```yaml
version: "3.8"

services:
  # ========================================
  # Application Service
  # ========================================
  app:
    build:
      context: .
      dockerfile: Dockerfile
      args:
        VERSION: dev
        GIT_COMMIT: local
        BUILD_TIME: "2024-01-22"
    ports:
      - "8080:8080"
    environment:
      - DATABASE_URL=postgres://zakat:zakat@postgres:5432/zakat?sslmode=disable
      - REDIS_URL=redis://redis:6379/0
      - LOG_LEVEL=debug
    depends_on:
      postgres:
        condition: service_healthy
      redis:
        condition: service_healthy
    networks:
      - zakat-network
    restart: unless-stopped

  # ========================================
  # PostgreSQL Database
  # ========================================
  postgres:
    image: postgres:16.1-alpine
    environment:
      POSTGRES_DB: zakat
      POSTGRES_USER: zakat
      POSTGRES_PASSWORD: zakat
    ports:
      - "5432:5432"
    volumes:
      - postgres-data:/var/lib/postgresql/data
      - ./scripts/init-db.sql:/docker-entrypoint-initdb.d/init.sql
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U zakat"]
      interval: 10s
      timeout: 5s
      retries: 5
    networks:
      - zakat-network
    restart: unless-stopped

  # ========================================
  # Redis Cache
  # ========================================
  redis:
    image: redis:7.2.4-alpine
    ports:
      - "6379:6379"
    volumes:
      - redis-data:/data
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 10s
      timeout: 3s
      retries: 5
    networks:
      - zakat-network
    restart: unless-stopped

networks:
  zakat-network:
    driver: bridge

volumes:
  postgres-data:
  redis-data:
```

## 🚀 CI/CD Patterns

### GitHub Actions Workflow

```yaml
# .github/workflows/ci.yml
name: CI/CD Pipeline

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]
  release:
    types: [published]

env:
  GO_VERSION: "1.25"
  GOLANGCI_LINT_VERSION: "v1.61.0"

jobs:
  # ========================================
  # Code Quality Checks
  # ========================================
  quality:
    name: Code Quality
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Set up Go
        uses: actions/setup-go@v5
        with:
          go-version: ${{ env.GO_VERSION }}
          cache: true

      - name: Verify dependencies
        run: |
          go mod download
          go mod verify

      - name: Run golangci-lint
        uses: golangci/golangci-lint-action@v6
        with:
          version: ${{ env.GOLANGCI_LINT_VERSION }}
          args: --timeout=5m

      - name: Run go vet
        run: go vet ./...

      - name: Check formatting
        run: |
          gofmt -s -l . | tee /tmp/gofmt.out
          test ! -s /tmp/gofmt.out

  # ========================================
  # Unit Tests
  # ========================================
  test:
    name: Unit Tests
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Set up Go
        uses: actions/setup-go@v5
        with:
          go-version: ${{ env.GO_VERSION }}
          cache: true

      - name: Run tests
        run: make test

      - name: Generate coverage
        run: make test-coverage-ci

      - name: Upload coverage to Codecov
        uses: codecov/codecov-action@v4
        with:
          file: ./coverage.out
          flags: unittests
          name: codecov-umbrella

  # ========================================
  # Integration Tests
  # ========================================
  integration:
    name: Integration Tests
    runs-on: ubuntu-latest
    services:
      postgres:
        image: postgres:16.1-alpine
        env:
          POSTGRES_DB: zakat_test
          POSTGRES_USER: test
          POSTGRES_PASSWORD: test
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
        ports:
          - 5432:5432

      redis:
        image: redis:7.2.4-alpine
        options: >-
          --health-cmd "redis-cli ping"
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
        ports:
          - 6379:6379

    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Set up Go
        uses: actions/setup-go@v5
        with:
          go-version: ${{ env.GO_VERSION }}
          cache: true

      - name: Run integration tests
        env:
          DATABASE_URL: postgres://test:test@localhost:5432/zakat_test?sslmode=disable
          REDIS_URL: redis://localhost:6379/0
        run: make test-integration

  # ========================================
  # Security Scanning
  # ========================================
  security:
    name: Security Scan
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Set up Go
        uses: actions/setup-go@v5
        with:
          go-version: ${{ env.GO_VERSION }}
          cache: true

      - name: Run govulncheck
        run: |
          go install golang.org/x/vuln/cmd/govulncheck@latest
          govulncheck ./...

      - name: Run gosec
        uses: securego/gosec@master
        with:
          args: -fmt json -out gosec-report.json ./...

      - name: Upload gosec report
        uses: actions/upload-artifact@v4
        with:
          name: gosec-report
          path: gosec-report.json

  # ========================================
  # Build
  # ========================================
  build:
    name: Build
    runs-on: ubuntu-latest
    needs: [quality, test]
    strategy:
      matrix:
        os: [linux, darwin, windows]
        arch: [amd64, arm64]
    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        with:
          fetch-depth: 0 # Full history for version info

      - name: Set up Go
        uses: actions/setup-go@v5
        with:
          go-version: ${{ env.GO_VERSION }}
          cache: true

      - name: Build binary
        env:
          GOOS: ${{ matrix.os }}
          GOARCH: ${{ matrix.arch }}
        run: |
          VERSION=$(git describe --tags --always --dirty)
          COMMIT=$(git rev-parse --short HEAD)
          BUILD_TIME=$(date -u '+%Y-%m-%d_%H:%M:%S')

          make build \
            VERSION=${VERSION} \
            COMMIT=${COMMIT} \
            BUILD_TIME=${BUILD_TIME}

      - name: Upload artifacts
        uses: actions/upload-artifact@v4
        with:
          name: zakat-calculator-${{ matrix.os }}-${{ matrix.arch }}
          path: bin/*

  # ========================================
  # Docker Build and Push
  # ========================================
  docker:
    name: Docker Build and Push
    runs-on: ubuntu-latest
    needs: [quality, test, integration, security]
    if: github.event_name == 'push' || github.event_name == 'release'
    permissions:
      contents: read
      packages: write
    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Log in to GitHub Container Registry
        uses: docker/login-action@v3
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Extract metadata
        id: meta
        uses: docker/metadata-action@v5
        with:
          images: ghcr.io/${{ github.repository }}
          tags: |
            type=ref,event=branch
            type=ref,event=pr
            type=semver,pattern={{version}}
            type=semver,pattern={{major}}.{{minor}}
            type=sha

      - name: Build and push
        uses: docker/build-push-action@v6
        with:
          context: .
          push: true
          tags: ${{ steps.meta.outputs.tags }}
          labels: ${{ steps.meta.outputs.labels }}
          build-args: |
            VERSION=${{ github.ref_name }}
            GIT_COMMIT=${{ github.sha }}
            BUILD_TIME=${{ github.event.head_commit.timestamp }}
          cache-from: type=gha
          cache-to: type=gha,mode=max

      - name: Scan Docker image
        uses: aquasecurity/trivy-action@master
        with:
          image-ref: ghcr.io/${{ github.repository }}:${{ github.sha }}
          format: sarif
          output: trivy-results.sarif

      - name: Upload Trivy results
        uses: github/codeql-action/upload-sarif@v3
        with:
          sarif_file: trivy-results.sarif
```

### GitLab CI Configuration

```yaml
# .gitlab-ci.yml
variables:
  GO_VERSION: "1.25"
  DOCKER_DRIVER: overlay2

stages:
  - quality
  - test
  - security
  - build
  - deploy

# ========================================
# Templates
# ========================================

.go-cache:
  cache:
    paths:
      - .go/pkg/mod/
    key:
      files:
        - go.sum

# ========================================
# Code Quality
# ========================================

lint:
  stage: quality
  image: golangci/golangci-lint:v1.61.0
  extends: .go-cache
  script:
    - golangci-lint run --out-format=code-climate > gl-code-quality-report.json
  artifacts:
    reports:
      codequality: gl-code-quality-report.json

format:
  stage: quality
  image: golang:1.25
  extends: .go-cache
  script:
    - gofmt -s -l . | tee /tmp/gofmt.out
    - test ! -s /tmp/gofmt.out

# ========================================
# Testing
# ========================================

test:unit:
  stage: test
  image: golang:1.25
  extends: .go-cache
  script:
    - make test
    - make test-coverage-ci
  coverage: '/total:.*\s(\d+\.\d+)%/'
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml

test:integration:
  stage: test
  image: golang:1.25
  extends: .go-cache
  services:
    - name: postgres:16.1-alpine
      alias: postgres
    - name: redis:7.2.4-alpine
      alias: redis
  variables:
    POSTGRES_DB: zakat_test
    POSTGRES_USER: test
    POSTGRES_PASSWORD: test
    DATABASE_URL: postgres://test:test@postgres:5432/zakat_test?sslmode=disable
    REDIS_URL: redis://redis:6379/0
  script:
    - make test-integration

# ========================================
# Security
# ========================================

security:scan:
  stage: security
  image: golang:1.25
  extends: .go-cache
  script:
    - go install golang.org/x/vuln/cmd/govulncheck@latest
    - govulncheck ./...

security:gosec:
  stage: security
  image: golang:1.25
  extends: .go-cache
  script:
    - go install github.com/securego/gosec/v2/cmd/gosec@latest
    - gosec -fmt json -out gosec-report.json ./...
  artifacts:
    reports:
      sast: gosec-report.json

# ========================================
# Build
# ========================================

build:binary:
  stage: build
  image: golang:1.25
  extends: .go-cache
  script:
    - make build
  artifacts:
    paths:
      - bin/

build:docker:
  stage: build
  image: docker:latest
  services:
    - docker:dind
  before_script:
    - docker login -u $CI_REGISTRY_USER -p $CI_REGISTRY_PASSWORD $CI_REGISTRY
  script:
    - docker build --build-arg VERSION=$CI_COMMIT_TAG -t $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA .
    - docker push $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA
  only:
    - main
    - tags

# ========================================
# Deploy
# ========================================

deploy:production:
  stage: deploy
  image: alpine:latest
  script:
    - echo "Deploy to production"
    # Add deployment logic
  only:
    - tags
  when: manual
```

## 💼 Complete Example Project

### Project Structure

```
zakat-calculator/
├── cmd/
│   └── server/
│       └── main.go                 # Entry point
├── internal/
│   ├── domain/
│   │   ├── zakat.go               # Business logic
│   │   └── zakat_test.go
│   ├── handler/
│   │   ├── http.go                # HTTP handlers
│   │   └── http_test.go
│   └── repository/
│       ├── postgres.go            # Database layer
│       └── postgres_test.go
├── pkg/
│   └── calculator/
│       ├── nisab.go               # Public API
│       └── nisab_test.go
├── scripts/
│   └── init-db.sql                # Database initialization
├── .github/
│   └── workflows/
│       └── ci.yml                 # GitHub Actions
├── .golangci.yml                  # Linter configuration
├── Dockerfile                     # Multi-stage build
├── docker-compose.yml             # Local development
├── Makefile                       # Build automation
├── go.mod                         # Dependencies
├── go.sum                         # Dependency checksums
├── README.md                      # Documentation
└── .gitignore                     # Git ignore rules
```

### main.go with Version Info

```go
package main

import (
 "fmt"
 "log"
 "os"

 "github.com/openshariaenterprise/zakat-calculator/internal/handler"
 "github.com/openshariaenterprise/zakat-calculator/internal/repository"
)

// Build information (injected at compile time)
var (
 Version   = "dev"
 GitCommit = "unknown"
 BuildTime = "unknown"
 BuildUser = "unknown"
 BuildHost = "unknown"
)

func main() {
 // Handle version flag
 if len(os.Args) > 1 && os.Args[1] == "--version" {
  printVersion()
  os.Exit(0)
 }

 // Initialize application
 log.Printf("Starting Zakat Calculator %s (commit: %s)", Version, GitCommit)

 // Database connection
 db, err := repository.NewPostgresDB(os.Getenv("DATABASE_URL"))
 if err != nil {
  log.Fatalf("Failed to connect to database: %v", err)
 }
 defer db.Close()

 // HTTP server
 server := handler.NewHTTPServer(db)
 if err := server.Start(":8080"); err != nil {
  log.Fatalf("Failed to start server: %v", err)
 }
}

func printVersion() {
 fmt.Printf("Zakat Calculator\n")
 fmt.Printf("  Version:    %s\n", Version)
 fmt.Printf("  Git Commit: %s\n", GitCommit)
 fmt.Printf("  Built:      %s\n", BuildTime)
 fmt.Printf("  Built By:   %s@%s\n", BuildUser, BuildHost)
 fmt.Printf("  Go Version: %s\n", runtime.Version())
}
```

### Zakat Business Logic

```go
// internal/domain/zakat.go
package domain

import (
 "errors"
 "time"

 "github.com/shopspring/decimal"
)

var (
 ErrInsufficientWealth = errors.New("wealth below nisab threshold")
 ErrInvalidNisabType   = errors.New("invalid nisab type")
)

// NisabType represents the type of wealth for nisab calculation
type NisabType string

const (
 NisabGold   NisabType = "gold"
 NisabSilver NisabType = "silver"
 NisabCash   NisabType = "cash"
)

// ZakatCalculation represents a zakat calculation
type ZakatCalculation struct {
 ID            int64           `json:"id"`
 UserID        string          `json:"user_id"`
 WealthAmount  decimal.Decimal `json:"wealth_amount"`
 NisabValue    decimal.Decimal `json:"nisab_value"`
 NisabType     NisabType       `json:"nisab_type"`
 ZakatAmount   decimal.Decimal `json:"zakat_amount"`
 ZakatRate     decimal.Decimal `json:"zakat_rate"`
 CalculatedAt  time.Time       `json:"calculated_at"`
}

// ZakatCalculator handles zakat calculations
type ZakatCalculator struct {
 nisabProvider NisabProvider
}

// NisabProvider provides current nisab values
type NisabProvider interface {
 GetNisabValue(nisabType NisabType) (decimal.Decimal, error)
}

// NewZakatCalculator creates a new zakat calculator
func NewZakatCalculator(provider NisabProvider) *ZakatCalculator {
 return &ZakatCalculator{
  nisabProvider: provider,
 }
}

// Calculate calculates zakat for the given wealth
func (c *ZakatCalculator) Calculate(userID string, wealthAmount decimal.Decimal, nisabType NisabType) (*ZakatCalculation, error) {
 // Get current nisab value
 nisabValue, err := c.nisabProvider.GetNisabValue(nisabType)
 if err != nil {
  return nil, err
 }

 // Check if wealth meets nisab threshold
 if wealthAmount.LessThan(nisabValue) {
  return nil, ErrInsufficientWealth
 }

 // Calculate zakat (2.5% of wealth)
 zakatRate := decimal.NewFromFloat(0.025)
 zakatAmount := wealthAmount.Mul(zakatRate).Round(2)

 calculation := &ZakatCalculation{
  UserID:       userID,
  WealthAmount: wealthAmount,
  NisabValue:   nisabValue,
  NisabType:    nisabType,
  ZakatAmount:  zakatAmount,
  ZakatRate:    zakatRate,
  CalculatedAt: time.Now().UTC(),
 }

 return calculation, nil
}
```

### Tests

```go
// internal/domain/zakat_test.go
package domain_test

import (
 "testing"

 "github.com/openshariaenterprise/zakat-calculator/internal/domain"
 "github.com/shopspring/decimal"
 "github.com/stretchr/testify/assert"
 "github.com/stretchr/testify/require"
)

// MockNisabProvider for testing
type MockNisabProvider struct {
 nisabValue decimal.Decimal
 err        error
}

func (m *MockNisabProvider) GetNisabValue(nisabType domain.NisabType) (decimal.Decimal, error) {
 if m.err != nil {
  return decimal.Zero, m.err
 }
 return m.nisabValue, nil
}

func TestZakatCalculator_Calculate(t *testing.T) {
 tests := []struct {
  name          string
  userID        string
  wealthAmount  decimal.Decimal
  nisabValue    decimal.Decimal
  nisabType     domain.NisabType
  expectedZakat decimal.Decimal
  expectError   error
 }{
  {
   name:          "wealth above nisab calculates correct zakat",
   userID:        "user123",
   wealthAmount:  decimal.NewFromInt(100000),
   nisabValue:    decimal.NewFromInt(10000),
   nisabType:     domain.NisabGold,
   expectedZakat: decimal.NewFromInt(2500), // 2.5% of 100000
   expectError:   nil,
  },
  {
   name:          "wealth below nisab returns error",
   userID:        "user456",
   wealthAmount:  decimal.NewFromInt(5000),
   nisabValue:    decimal.NewFromInt(10000),
   nisabType:     domain.NisabGold,
   expectedZakat: decimal.Zero,
   expectError:   domain.ErrInsufficientWealth,
  },
  {
   name:          "wealth exactly at nisab calculates zakat",
   userID:        "user789",
   wealthAmount:  decimal.NewFromInt(10000),
   nisabValue:    decimal.NewFromInt(10000),
   nisabType:     domain.NisabSilver,
   expectedZakat: decimal.NewFromInt(250), // 2.5% of 10000
   expectError:   nil,
  },
 }

 for _, tt := range tests {
  t.Run(tt.name, func(t *testing.T) {
   // Arrange
   provider := &MockNisabProvider{
    nisabValue: tt.nisabValue,
   }
   calculator := domain.NewZakatCalculator(provider)

   // Act
   result, err := calculator.Calculate(tt.userID, tt.wealthAmount, tt.nisabType)

   // Assert
   if tt.expectError != nil {
    require.Error(t, err)
    assert.Equal(t, tt.expectError, err)
    assert.Nil(t, result)
   } else {
    require.NoError(t, err)
    require.NotNil(t, result)
    assert.Equal(t, tt.userID, result.UserID)
    assert.True(t, tt.expectedZakat.Equal(result.ZakatAmount))
    assert.True(t, tt.wealthAmount.Equal(result.WealthAmount))
    assert.True(t, tt.nisabValue.Equal(result.NisabValue))
    assert.Equal(t, tt.nisabType, result.NisabType)
   }
  })
 }
}
```

## 📚 Usage Guidelines

### Development Workflow

**1. Initial Setup**

```bash
# Clone repository
git clone https://github.com/openshariaenterprise/zakat-calculator.git
cd zakat-calculator

# Install dependencies
make deps

# Install development tools
make install-tools

# Start local services
docker-compose up -d postgres redis
```

**2. Development Cycle**

```bash
# Format code
make fmt

# Run linter
make lint

# Run tests
make test

# Build binary
make build

# Run locally
make run
```

**3. Pre-Commit Checks**

```bash
# Run full CI pipeline locally
make ci

# Fix any issues
make lint-fix
make fmt

# Run tests again
make test
```

### CI/CD Workflow

**1. Pull Request**

```bash
# GitHub Actions automatically runs:
# - Code quality checks (golangci-lint, gofmt)
# - Unit tests with coverage
# - Integration tests with database
# - Security scans (govulncheck, gosec)
# - Build verification
```

**2. Merge to Main**

```bash
# Additional steps:
# - Docker image build and push
# - Security scanning of Docker image
# - Deployment to staging (optional)
```

**3. Release Tag**

```bash
# Tag release
git tag v1.0.0
git push origin v1.0.0

# GitHub Actions builds:
# - Multi-platform binaries (Linux, macOS, Windows)
# - Docker images with version tags
# - Deployment to production (manual approval)
```

### Build Reproducibility

**Reproducible Build Requirements**:

```bash
# 1. Pin Go version
go 1.25

# 2. Lock dependencies
go.mod + go.sum (committed)

# 3. Use build flags
-trimpath              # Remove local paths
-ldflags="-s -w"      # Strip debug symbols

# 4. Inject version info
-X main.Version=$(VERSION)
-X main.GitCommit=$(COMMIT)

# 5. Verify build
sha256sum bin/zakat-calculator
```

**Same commit = same binary**:

```bash
# Build on developer machine
make build
sha256sum bin/zakat-calculator
# Output: abc123...

# Build on CI server
make build
sha256sum bin/zakat-calculator
# Output: abc123... (identical)
```

### Cross-Platform Builds

```bash
# Build for all platforms
make build-all

# Output:
# bin/zakat-calculator-linux
# bin/zakat-calculator-darwin
# bin/zakat-calculator.exe
```

### Security Best Practices

**1. Dependency Security**

```bash
# Scan for vulnerabilities
make security-scan

# Update vulnerable dependencies
go get -u github.com/vulnerable/lib@latest
make deps-tidy
```

**2. Docker Security**

```bash
# Scan Docker image
make docker-scan

# Build with security flags
CGO_ENABLED=0          # Static binary
-ldflags="-s -w"      # Strip symbols
USER appuser          # Non-root user
```

**3. Secret Management**

```bash
# NEVER commit secrets
# Use environment variables
DATABASE_URL=postgres://...

# Use .env for local development
cp .env.example .env
# Edit .env with local secrets
```

## 🔗 Related Documentation

- [Modules and Dependencies](../ex-so-prla-go__modules-and-dependencies.md) - Dependency management details
- [Best Practices](../ex-so-prla-go__best-practices.md) - Go coding standards
- [Linting and Formatting](../ex-so-prla-go__linting-and-formatting.md) - Code quality tools

## 🌐 External Resources

**Build Tools**:

- [Make Documentation](https://www.gnu.org/software/make/manual/)
- [Go Build Documentation](https://pkg.go.dev/cmd/go#hdr-Compile_packages_and_dependencies)
- [Docker Best Practices](https://docs.docker.com/develop/dev-best-practices/)

**CI/CD**:

- [GitHub Actions for Go](https://docs.github.com/en/actions/automating-builds-and-tests/building-and-testing-go)
- [GitLab CI for Go](https://docs.gitlab.com/ee/ci/examples/test-and-deploy-golang-project.html)

**Security**:

- [govulncheck](https://pkg.go.dev/golang.org/x/vuln/cmd/govulncheck)
- [gosec](https://github.com/securego/gosec)
- [Docker Scout](https://docs.docker.com/scout/)

**Code Quality**:

- [golangci-lint](https://golangci-lint.run/)
- [golangci-lint Linters](https://golangci-lint.run/usage/linters/)

---

**Principles Applied**: Reproducibility, Automation Over Manual, Explicit Over Implicit

---

**Last Updated**: 2025-01-23
**Go Version**: 1.18+
