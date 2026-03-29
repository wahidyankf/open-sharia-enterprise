---
title: "Logging Observability"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Structured logging with log package, slog (Go 1.21+), limitations, and production logging frameworks (zerolog/zap) plus observability fundamentals"
weight: 1000058
tags: ["golang", "logging", "observability", "slog", "zerolog", "zap", "opentelemetry", "production"]
---

## Why Logging & Observability Matter in Go

Effective logging and observability are critical for debugging production issues, monitoring system health, and understanding application behavior under load. Understanding standard library logging before adopting frameworks prevents over-engineering simple applications and enables informed framework selection based on performance and feature requirements.

**Core benefits**:

- **Production debugging**: Diagnose issues without debugger access
- **Performance monitoring**: Identify bottlenecks and optimization opportunities
- **Audit trails**: Track security-relevant events and compliance
- **Operational insights**: Understand system behavior and user patterns

**Problem**: Many developers start with `fmt.Println` for debugging or immediately adopt heavyweight logging frameworks without understanding tradeoffs. Simple applications end up with unnecessary dependencies, while high-throughput services use inefficient logging that impacts performance.

**Solution**: Start with `log` package for fundamentals, upgrade to `slog` (Go 1.21+) for structured logging, recognize limitations (no log levels in log, no sampling), then introduce production frameworks (zerolog/zap) with clear rationale based on performance requirements. Extend to full observability with OpenTelemetry for traces, metrics, and logs.

## Standard Library First: log Package Basics

Go's `log` package provides basic logging with timestamp and customizable prefix. Suitable for simple applications with minimal logging needs.

**Basic logging pattern**:

```go
package main

import (
    "log"
    // => Standard library for basic logging
    // => Writes to stderr by default
    "os"
    // => Standard library for file operations
)

func main() {
    // Default logger writes to stderr with timestamp
    // => Format: 2024/01/15 10:30:45 message

    log.Println("Application starting")
    // => log.Println adds timestamp and newline
    // => Output: 2024/01/15 10:30:45 Application starting
    // => Writes to stderr (os.Stderr)

    log.Printf("Server listening on port %d", 8080)
    // => log.Printf formats message like fmt.Printf
    // => Output: 2024/01/15 10:30:45 Server listening on port 8080

    // Simulate error condition
    if err := startServer(); err != nil {
        log.Fatalf("Failed to start server: %v", err)
        // => log.Fatalf logs message then calls os.Exit(1)
        // => Output: 2024/01/15 10:30:45 Failed to start server: connection refused
        // => Terminates application immediately
        // => Deferred functions NOT executed
    }
}

func startServer() error {
    return fmt.Errorf("connection refused")
    // => Simulated error
}
```

**Custom logger with prefix**:

```go
package main

import (
    "log"
    "os"
)

func main() {
    // Create custom logger with prefix
    // => Prefix helps identify log source

    logger := log.New(os.Stdout, "[APP] ", log.LstdFlags)
    // => log.New creates custom logger
    // => os.Stdout writes to standard output (not stderr)
    // => "[APP] " is prefix (shown before message)
    // => log.LstdFlags is date + time format
    // => Output: [APP] 2024/01/15 10:30:45 message

    logger.Println("Custom logger initialized")
    // => Output: [APP] 2024/01/15 10:30:45 Custom logger initialized

    // Custom flags for timestamp format
    errorLogger := log.New(os.Stderr, "[ERROR] ", log.Ldate|log.Ltime|log.Lshortfile)
    // => log.Ldate adds date (2024/01/15)
    // => log.Ltime adds time (10:30:45)
    // => log.Lshortfile adds filename:line (main.go:20)
    // => Combines with | (bitwise OR)

    errorLogger.Println("Something went wrong")
    // => Output: [ERROR] 2024/01/15 10:30:45 main.go:20: Something went wrong
    // => Includes file and line number for debugging
}
```

**Logging to file**:

```go
package main

import (
    "log"
    "os"
)

func main() {
    // Create or open log file
    // => Append mode preserves existing logs

    file, err := os.OpenFile("app.log", os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
    // => os.OpenFile opens file with specific flags
    // => os.O_APPEND appends to existing file
    // => os.O_CREATE creates file if doesn't exist
    // => os.O_WRONLY opens for writing only
    // => 0644 is file permissions (rw-r--r--)

    if err != nil {
        log.Fatalf("Failed to open log file: %v", err)
    }
    defer file.Close()
    // => Close file when function exits
    // => Ensures logs flushed to disk

    logger := log.New(file, "[APP] ", log.LstdFlags)
    // => Logger writes to file instead of stdout/stderr

    logger.Println("Application started")
    // => Writes to app.log
    // => Output in file: [APP] 2024/01/15 10:30:45 Application started

    logger.Printf("Processing %d requests", 100)
    // => Writes to app.log

    // Log errors to both file and stderr
    multiWriter := io.MultiWriter(file, os.Stderr)
    // => io.MultiWriter writes to multiple destinations
    // => Writes to file AND stderr simultaneously

    errorLogger := log.New(multiWriter, "[ERROR] ", log.LstdFlags|log.Lshortfile)
    // => Errors visible in both file and terminal

    errorLogger.Println("Database connection failed")
    // => Appears in both app.log and stderr
}
```

**Conditional logging pattern**:

```go
package main

import (
    "log"
    "os"
)

var (
    debugMode = false
    // => Global debug flag
    // => Set from environment variable or flag
)

func main() {
    // Enable debug mode from environment
    if os.Getenv("DEBUG") == "true" {
        debugMode = true
        // => DEBUG=true enables verbose logging
    }

    log.Println("Application starting (always logged)")

    if debugMode {
        log.Println("Debug mode enabled")
        log.Printf("Environment: %v", os.Environ())
        // => Debug logs only when debugMode true
        // => Avoids cluttering logs in production
    }

    processRequest("user123")
}

func processRequest(userID string) {
    log.Printf("Processing request for user: %s", userID)

    if debugMode {
        log.Printf("Debug: Looking up user %s in database", userID)
        // => Detailed debug information
    }

    // ... processing logic

    log.Printf("Request completed for user: %s", userID)
}
```

**Structured logging pattern (manual)**:

```go
package main

import (
    "encoding/json"
    "log"
    "os"
    "time"
)

type LogEntry struct {
    Timestamp string                 `json:"timestamp"`
    Level     string                 `json:"level"`
    Message   string                 `json:"message"`
    Fields    map[string]interface{} `json:"fields,omitempty"`
}

func logJSON(level, message string, fields map[string]interface{}) {
    // => Manual structured logging with JSON
    // => Easier to parse than unstructured text

    entry := LogEntry{
        Timestamp: time.Now().Format(time.RFC3339),
        // => RFC3339 format: 2024-01-15T10:30:45Z
        // => ISO 8601 compliant
        Level:   level,
        Message: message,
        Fields:  fields,
    }

    bytes, err := json.Marshal(entry)
    // => json.Marshal converts struct to JSON bytes
    if err != nil {
        log.Printf("Failed to marshal log entry: %v", err)
        return
    }

    os.Stdout.Write(bytes)
    // => Write JSON to stdout
    os.Stdout.Write([]byte("\n"))
    // => Add newline (JSON lines format)
    // => Output: {"timestamp":"2024-01-15T10:30:45Z","level":"info","message":"..."}
}

func main() {
    logJSON("info", "Application starting", nil)
    // => No additional fields

    logJSON("info", "Processing request", map[string]interface{}{
        "user_id":    "user123",
        "request_id": "req-456",
        "duration":   123,
    })
    // => Additional context fields
    // => Output: {"timestamp":"...","level":"info","message":"Processing request","fields":{"user_id":"user123",...}}
}
```

**Limitations for production logging**:

- **No log levels**: Cannot distinguish debug/info/warn/error
- **No structured logging**: Text-only, difficult to parse/query
- **No sampling**: Cannot reduce log volume for high-throughput systems
- **No log rotation**: Manual file rotation required
- **No performance optimization**: Allocates for every log call
- **No context propagation**: Cannot add request-scoped fields
- **No filtering**: Cannot filter logs by level or component
- **No JSON output**: Must manually format for log aggregation systems

## Enhanced Logging: slog (Go 1.21+ Official)

Go 1.21 introduced `slog` package for structured logging with levels. Official standard library solution replacing third-party frameworks for most use cases.

**Why slog over log package**:

- **Log levels**: Debug, Info, Warn, Error built-in
- **Structured logging**: Key-value pairs instead of text
- **Performance**: Minimal allocations with lazy evaluation
- **Handlers**: JSON, text, or custom output formats
- **Context support**: Request-scoped logging with context.Context
- **Official**: Standard library, no external dependencies

**Basic slog pattern**:

```go
package main

import (
    "log/slog"
    // => Standard library (Go 1.21+)
    // => Structured logging with levels
    "os"
)

func main() {
    // Default logger writes JSON to stderr
    // => slog.Info/Debug/Warn/Error are package-level functions
    // => Use default logger (JSON format)

    slog.Info("Application starting")
    // => Output: {"time":"2024-01-15T10:30:45Z","level":"INFO","msg":"Application starting"}
    // => JSON format for log aggregation

    slog.Info("Server listening", "port", 8080, "host", "localhost")
    // => Structured fields as key-value pairs
    // => Output: {"time":"...","level":"INFO","msg":"Server listening","port":8080,"host":"localhost"}
    // => Fields are typed (8080 is number, not string)

    slog.Warn("High memory usage", "percent", 85)
    // => Warn level for warnings
    // => Output: {"time":"...","level":"WARN","msg":"High memory usage","percent":85}

    err := fmt.Errorf("connection refused")
    slog.Error("Database connection failed", "error", err, "attempts", 3)
    // => Error level for errors
    // => Output: {"time":"...","level":"ERROR","msg":"Database connection failed","error":"connection refused","attempts":3}
}
```

**Custom slog handler (text format)**:

```go
package main

import (
    "log/slog"
    "os"
)

func main() {
    // Create text handler instead of JSON
    // => Human-readable for development

    handler := slog.NewTextHandler(os.Stdout, &slog.HandlerOptions{
        // => NewTextHandler creates text output handler
        // => os.Stdout for console output
        Level: slog.LevelDebug,
        // => Set minimum log level
        // => Debug shows all logs (Debug, Info, Warn, Error)
        // => Default is Info (hides Debug)
    })

    logger := slog.New(handler)
    // => slog.New creates logger with custom handler
    // => Can create multiple loggers with different configs

    logger.Debug("Debug message", "detail", "extra info")
    // => Output: time=2024-01-15T10:30:45.123Z level=DEBUG msg="Debug message" detail="extra info"
    // => Key=value format (logfmt style)

    logger.Info("Info message", "user_id", "user123")
    // => Output: time=... level=INFO msg="Info message" user_id=user123

    logger.Warn("Warning message", "threshold", 90)
    logger.Error("Error message", "error", "something failed")
}
```

**Logger with persistent fields**:

```go
package main

import (
    "log/slog"
    "os"
)

func main() {
    // Create logger with default fields
    // => Fields included in every log entry

    baseLogger := slog.New(slog.NewJSONHandler(os.Stdout, nil))

    appLogger := baseLogger.With(
        "service", "api-server",
        "version", "1.0.0",
        "environment", "production",
    )
    // => baseLogger.With creates child logger
    // => Child includes parent fields plus new fields
    // => Every log from appLogger includes service, version, environment

    appLogger.Info("Application starting")
    // => Output: {"time":"...","level":"INFO","msg":"Application starting","service":"api-server","version":"1.0.0","environment":"production"}

    // Create request-scoped logger
    requestLogger := appLogger.With(
        "request_id", "req-123",
        "user_id", "user456",
    )
    // => Adds request-specific fields
    // => Inherits service, version, environment from parent

    requestLogger.Info("Processing request")
    // => Output includes all fields: service, version, environment, request_id, user_id

    requestLogger.Info("Request completed", "duration_ms", 150)
    // => Additional field just for this log
    // => Output includes all parent fields + duration_ms
}
```

**Context-aware logging**:

```go
package main

import (
    "context"
    "log/slog"
    "os"
)

func main() {
    logger := slog.New(slog.NewJSONHandler(os.Stdout, nil))

    // Create context with logger
    ctx := context.Background()
    ctx = context.WithValue(ctx, "logger", logger.With("request_id", "req-789"))
    // => Store request-scoped logger in context
    // => Pass context through call chain

    handleRequest(ctx)
}

func handleRequest(ctx context.Context) {
    // Extract logger from context
    logger := ctx.Value("logger").(*slog.Logger)
    // => Retrieve logger from context
    // => Type assertion to *slog.Logger

    logger.Info("Handling request")
    // => Automatically includes request_id

    processData(ctx, "user123")
}

func processData(ctx context.Context, userID string) {
    logger := ctx.Value("logger").(*slog.Logger)

    logger.Info("Processing data", "user_id", userID)
    // => Includes request_id from context + user_id parameter
}
```

**Custom log attributes**:

```go
package main

import (
    "log/slog"
    "os"
    "time"
)

func main() {
    logger := slog.New(slog.NewJSONHandler(os.Stdout, nil))

    // Group related fields
    logger.Info("Request processed",
        slog.Group("http",
            "method", "GET",
            "path", "/api/users",
            "status", 200,
        ),
        // => Group creates nested object
        // => Output: {"time":"...","msg":"Request processed","http":{"method":"GET","path":"/api/users","status":200}}
        slog.Group("timing",
            "start", time.Now(),
            "duration_ms", 150,
        ),
    )

    // Any type for structured fields
    logger.Info("User action",
        slog.String("action", "login"),
        // => Explicit type for clarity
        slog.Int("user_id", 123),
        slog.Bool("success", true),
        slog.Duration("duration", 100*time.Millisecond),
        slog.Time("timestamp", time.Now()),
    )
}
```

**Log level filtering**:

```go
package main

import (
    "log/slog"
    "os"
)

func main() {
    // Production: Info level (hide Debug)
    prodHandler := slog.NewJSONHandler(os.Stdout, &slog.HandlerOptions{
        Level: slog.LevelInfo,
        // => Minimum level Info
        // => Debug logs ignored
    })
    prodLogger := slog.New(prodHandler)

    prodLogger.Debug("Debug message")
    // => Not logged (below Info level)

    prodLogger.Info("Info message")
    // => Logged (Info >= Info)

    prodLogger.Warn("Warning message")
    // => Logged (Warn > Info)

    // Development: Debug level (show all)
    devHandler := slog.NewTextHandler(os.Stdout, &slog.HandlerOptions{
        Level: slog.LevelDebug,
        // => Show all levels
    })
    devLogger := slog.New(devHandler)

    devLogger.Debug("Debug message")
    // => Logged in development

    // Dynamic level from environment
    level := slog.LevelInfo
    if os.Getenv("DEBUG") == "true" {
        level = slog.LevelDebug
    }

    handler := slog.NewJSONHandler(os.Stdout, &slog.HandlerOptions{
        Level: level,
        // => Level from environment
    })
    logger := slog.New(handler)
}
```

**Limitations remaining after slog**:

- **No sampling**: Cannot reduce log volume (e.g., 1 in 100 debug logs)
- **No caller skip**: Cannot customize caller location for wrapped loggers
- **Basic performance**: Good but not optimal for extreme throughput (>100K logs/sec)
- **No hooks**: Cannot add custom processing (send to multiple destinations)
- **Limited formatting**: JSON and text only, no custom formats
- **No log rotation**: Must use external tools for file rotation

## Production Frameworks: zerolog and zap

For extreme performance requirements or advanced features, zerolog and zap provide optimized logging with zero allocations.

**When to use zerolog/zap over slog**:

- **Extreme throughput**: >100K logs/second
- **Zero allocations**: GC pressure concerns
- **Sampling**: Reduce log volume at high load
- **Complex hooks**: Custom log processing (metrics, alerting)
- **Backward compatibility**: Pre-Go 1.21 projects

### zerolog - Zero Allocation Logger

**zerolog pattern**:

```go
package main

import (
    "os"

    "github.com/rs/zerolog"
    "github.com/rs/zerolog/log"
    // => External dependency: github.com/rs/zerolog
    // => Zero allocation structured logging
    // => Install: go get github.com/rs/zerolog
)

func main() {
    // Configure global logger
    zerolog.TimeFieldFormat = zerolog.TimeFormatUnix
    // => Unix timestamp format for efficiency
    // => Saves space compared to ISO 8601

    log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr})
    // => ConsoleWriter for human-readable development output
    // => Use JSON writer for production

    // Basic logging
    log.Info().Msg("Application starting")
    // => Output: {"level":"info","time":1705312245,"message":"Application starting"}
    // => Chaining API: log.Level().Fields().Msg()

    log.Info().
        Str("service", "api").
        Int("port", 8080).
        Msg("Server listening")
    // => Str() adds string field
    // => Int() adds integer field
    // => Output: {"level":"info","service":"api","port":8080,"message":"Server listening"}

    // Error logging with error field
    err := fmt.Errorf("connection refused")
    log.Error().
        Err(err).
        Str("host", "localhost").
        Int("attempts", 3).
        Msg("Database connection failed")
    // => Err() adds error field
    // => Output: {"level":"error","error":"connection refused","host":"localhost","attempts":3,"message":"..."}

    // Nested fields with Dict
    log.Info().
        Dict("http", zerolog.Dict().
            Str("method", "GET").
            Str("path", "/api/users").
            Int("status", 200),
        ).
        Msg("Request processed")
    // => Dict() creates nested object
    // => Output: {"level":"info","http":{"method":"GET","path":"/api/users","status":200},"message":"..."}
}
```

**zerolog with context**:

```go
package main

import (
    "context"
    "github.com/rs/zerolog"
    "github.com/rs/zerolog/log"
    "os"
)

func main() {
    logger := zerolog.New(os.Stdout).With().
        Timestamp().
        Str("service", "api-server").
        Logger()
    // => Create logger with default fields
    // => Timestamp() adds automatic timestamp
    // => With() creates child logger with fields

    ctx := logger.WithContext(context.Background())
    // => Store logger in context
    // => Pass context through call chain

    handleRequest(ctx, "req-123")
}

func handleRequest(ctx context.Context, requestID string) {
    logger := zerolog.Ctx(ctx).With().
        Str("request_id", requestID).
        Logger()
    // => Extract logger from context
    // => Add request-specific field

    ctx = logger.WithContext(ctx)
    // => Update context with request logger

    logger.Info().Msg("Processing request")

    processUser(ctx, "user456")
}

func processUser(ctx context.Context, userID string) {
    logger := zerolog.Ctx(ctx)
    // => Get logger from context
    // => Includes service + request_id

    logger.Info().
        Str("user_id", userID).
        Msg("Processing user")
    // => Output includes all parent fields
}
```

**zerolog sampling**:

```go
package main

import (
    "github.com/rs/zerolog"
    "github.com/rs/zerolog/log"
    "os"
)

func main() {
    // Sample debug logs: 1 in 10
    sampled := log.Sample(&zerolog.BurstSampler{
        Burst:  5,
        // => Allow first 5 logs through
        Period: 1,
        // => Then 1 per second
        NextSampler: &zerolog.BasicSampler{N: 10},
        // => After burst, sample 1 in 10
    })

    for i := 0; i < 100; i++ {
        sampled.Debug().Int("iteration", i).Msg("Debug log")
        // => Only ~10 of 100 logs actually written
        // => Reduces log volume at high load
    }

    // Info+ logs always logged
    log.Info().Msg("This is always logged")
    // => No sampling for Info, Warn, Error
}
```

### zap - Uber's Fast Logger

**zap pattern**:

```go
package main

import (
    "go.uber.org/zap"
    "go.uber.org/zap/zapcore"
    // => External dependency: go.uber.org/zap
    // => Uber's fast structured logger
    // => Install: go get go.uber.org/zap
)

func main() {
    // Production logger (JSON)
    logger, _ := zap.NewProduction()
    // => NewProduction creates JSON logger
    // => Optimized for performance
    // => Output: JSON with nanosecond timestamps
    defer logger.Sync()
    // => Flush buffered logs before exit

    logger.Info("Application starting",
        zap.String("service", "api-server"),
        zap.Int("port", 8080),
    )
    // => zap.String, zap.Int are typed field constructors
    // => Zero allocations
    // => Output: {"level":"info","msg":"Application starting","service":"api-server","port":8080}

    // Development logger (console)
    devLogger, _ := zap.NewDevelopment()
    // => NewDevelopment creates human-readable logger
    // => Stack traces for warnings
    // => Caller location included

    devLogger.Info("Development mode",
        zap.String("env", "dev"),
    )
    // => Output: 2024-01-15T10:30:45.123Z INFO main.go:20 Development mode {"env":"dev"}

    // Error logging
    err := fmt.Errorf("database error")
    logger.Error("Database connection failed",
        zap.Error(err),
        zap.String("host", "localhost"),
        zap.Int("attempts", 3),
    )
    // => zap.Error for error fields
}
```

**zap custom configuration**:

```go
package main

import (
    "go.uber.org/zap"
    "go.uber.org/zap/zapcore"
)

func main() {
    config := zap.Config{
        Level:    zap.NewAtomicLevelAt(zap.InfoLevel),
        // => Minimum log level (Info)
        Encoding: "json",
        // => JSON encoding for production
        // => Also: "console" for development
        EncoderConfig: zapcore.EncoderConfig{
            TimeKey:        "timestamp",
            LevelKey:       "level",
            MessageKey:     "message",
            CallerKey:      "caller",
            StacktraceKey:  "stacktrace",
            LineEnding:     zapcore.DefaultLineEnding,
            EncodeLevel:    zapcore.LowercaseLevelEncoder,
            EncodeTime:     zapcore.ISO8601TimeEncoder,
            EncodeDuration: zapcore.StringDurationEncoder,
            EncodeCaller:   zapcore.ShortCallerEncoder,
        },
        OutputPaths:      []string{"stdout", "app.log"},
        // => Write to stdout AND file
        ErrorOutputPaths: []string{"stderr"},
    }

    logger, _ := config.Build()
    defer logger.Sync()

    logger.Info("Custom configured logger",
        zap.String("config", "custom"),
    )
}
```

**zap with sugared logger**:

```go
package main

import (
    "go.uber.org/zap"
)

func main() {
    logger, _ := zap.NewProduction()
    defer logger.Sync()

    // Sugared logger for printf-style logging
    sugar := logger.Sugar()
    // => Sugar() creates SugaredLogger
    // => More convenient API
    // => Slightly slower (reflection-based)

    sugar.Infow("Request processed",
        "method", "GET",
        "path", "/api/users",
        "status", 200,
    )
    // => Infow uses key-value pairs
    // => w suffix = with fields

    sugar.Infof("Processing user %s", "user123")
    // => Infof uses printf-style formatting
    // => f suffix = formatted

    // Regular logger for performance-critical paths
    logger.Info("Performance critical path",
        zap.String("user_id", "user123"),
        zap.Duration("latency", 10*time.Millisecond),
    )
    // => Use non-sugared logger for best performance
}
```

## Observability: Traces, Metrics, Logs

Full observability requires three pillars: traces (request flow), metrics (aggregated data), and logs (events). OpenTelemetry provides unified instrumentation.

**Three pillars of observability**:

1. **Logs**: Events with context (what happened)
2. **Metrics**: Aggregated measurements (how much/how often)
3. **Traces**: Request flow across services (where time spent)

**OpenTelemetry pattern**:

```go
package main

import (
    "context"
    "log"

    "go.opentelemetry.io/otel"
    "go.opentelemetry.io/otel/attribute"
    "go.opentelemetry.io/otel/exporters/stdout/stdouttrace"
    "go.opentelemetry.io/otel/sdk/resource"
    sdktrace "go.opentelemetry.io/otel/sdk/trace"
    semconv "go.opentelemetry.io/otel/semconv/v1.17.0"
    "go.opentelemetry.io/otel/trace"
    // => OpenTelemetry for traces, metrics, logs
    // => Install: go get go.opentelemetry.io/otel
)

func main() {
    // Initialize tracer
    exporter, _ := stdouttrace.New(stdouttrace.WithPrettyPrint())
    // => Stdout exporter for development
    // => Production: OTLP exporter to collector

    tp := sdktrace.NewTracerProvider(
        sdktrace.WithBatcher(exporter),
        // => Batch traces before export
        sdktrace.WithResource(resource.NewWithAttributes(
            semconv.SchemaURL,
            semconv.ServiceName("api-server"),
            semconv.ServiceVersion("1.0.0"),
        )),
        // => Service metadata
    )
    defer tp.Shutdown(context.Background())

    otel.SetTracerProvider(tp)
    // => Set global tracer provider

    tracer := otel.Tracer("example")
    // => Get tracer for this package

    ctx := context.Background()
    processRequest(ctx, tracer)
}

func processRequest(ctx context.Context, tracer trace.Tracer) {
    // Start span for this operation
    ctx, span := tracer.Start(ctx, "processRequest")
    // => Create span for operation
    // => Pass context to child operations
    defer span.End()
    // => End span when function returns

    span.SetAttributes(
        attribute.String("user_id", "user123"),
        attribute.String("request_id", "req-456"),
    )
    // => Add attributes to span

    // Simulate work
    fetchUser(ctx, tracer, "user123")
    // => Child span created automatically
}

func fetchUser(ctx context.Context, tracer trace.Tracer, userID string) {
    ctx, span := tracer.Start(ctx, "fetchUser")
    defer span.End()

    span.SetAttributes(attribute.String("user_id", userID))

    // Simulate database query
    // => Span captures timing
}
```

## Trade-offs Comparison

| Aspect              | log Package        | slog (Go 1.21+)          | zerolog                | zap                    | OpenTelemetry         |
| ------------------- | ------------------ | ------------------------ | ---------------------- | ---------------------- | --------------------- |
| **Complexity**      | Minimal (stdlib)   | Low (stdlib)             | Medium (external)      | Medium (external)      | High (full platform)  |
| **Log Levels**      | ❌ None            | ✅ Debug/Info/Warn/Error | ✅ 7 levels            | ✅ 6 levels            | ✅ Integrated         |
| **Structured**      | ❌ Text only       | ✅ Key-value             | ✅ Zero-alloc          | ✅ Zero-alloc          | ✅ Full context       |
| **Performance**     | Basic              | Good                     | Excellent (zero-alloc) | Excellent (zero-alloc) | Good                  |
| **Sampling**        | ❌ None            | ❌ None                  | ✅ Built-in            | ✅ Built-in            | ✅ Trace sampling     |
| **Context Support** | ❌ Manual          | ✅ Context-aware         | ✅ Context integration | ✅ Context integration | ✅ Full propagation   |
| **Traces/Metrics**  | ❌ Logs only       | ❌ Logs only             | ❌ Logs only           | ❌ Logs only           | ✅ All three pillars  |
| **Learning Curve**  | None               | Minimal                  | Low-Medium             | Low-Medium             | High                  |
| **Go Version**      | All versions       | 1.21+ required           | All versions           | All versions           | All versions          |
| **Use Cases**       | Simple apps        | Most applications        | High-throughput        | High-throughput        | Distributed systems   |
| **Examples**        | Scripts, utilities | Microservices, APIs      | Performance-critical   | Performance-critical   | Multi-service systems |

## Best Practices

**Progressive adoption strategy**:

1. **Start with log**: Simple scripts, single-file programs
2. **Upgrade to slog** (Go 1.21+): Structured logging needs, production services
3. **Consider zerolog/zap**: >100K logs/sec, zero-allocation requirements
4. **Add OpenTelemetry**: Distributed systems, need traces + metrics + logs

**When log package sufficient**:

- Simple utilities and scripts
- Single-file programs
- Internal tools with minimal logging
- Temporary debugging

**When slog appropriate** (most use cases):

- Microservices and APIs
- Production applications (Go 1.21+)
- Need structured logging with levels
- Standard library preference

**When zerolog/zap justified**:

- Extreme performance requirements (>100K logs/sec)
- GC pressure from logging
- Need sampling for high-volume logs
- Pre-Go 1.21 projects needing structured logs

**When OpenTelemetry needed**:

- Distributed systems (microservices)
- Need distributed tracing
- Unified observability (logs + traces + metrics)
- Integration with observability platforms

**Structured logging best practices**:

```go
// DO: Use structured fields
logger.Info("Request processed",
    "method", "GET",
    "path", "/api/users",
    "status", 200,
    "duration_ms", 150,
)
// => Fields easily queryable in log aggregation

// DON'T: String interpolation
logger.Info(fmt.Sprintf("Request processed: GET /api/users 200 (150ms)"))
// => Cannot query by method, path, status
```

**Log level guidelines**:

```go
// DEBUG: Detailed diagnostic information
logger.Debug("Cache miss", "key", "user:123")

// INFO: General informational messages
logger.Info("Server started", "port", 8080)

// WARN: Warning messages (recoverable issues)
logger.Warn("High memory usage", "percent", 85)

// ERROR: Error messages (application errors)
logger.Error("Database query failed", "error", err)

// FATAL/PANIC: Critical errors (application crash)
logger.Fatal("Cannot bind to port", "port", 8080)  // Exits
```

**Context propagation pattern**:

```go
func handleRequest(ctx context.Context) {
    logger := slog.Default().With(
        "request_id", requestIDFromContext(ctx),
        "user_id", userIDFromContext(ctx),
    )

    ctx = context.WithValue(ctx, "logger", logger)
    // => Store logger in context

    // Pass context through call chain
    processData(ctx)
}

func processData(ctx context.Context) {
    logger := ctx.Value("logger").(*slog.Logger)
    // => Extract logger from context
    // => Automatically includes request_id, user_id

    logger.Info("Processing data")
}
```

**Production logging configuration**:

```go
// Production: JSON logging to stdout
// => Log aggregation systems parse JSON
// => Kubernetes collects from stdout

logger := slog.New(slog.NewJSONHandler(os.Stdout, &slog.HandlerOptions{
    Level: slog.LevelInfo,  // Hide Debug in production
}))

// Development: Text logging to stderr
// => Human-readable for local development

logger := slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{
    Level: slog.LevelDebug,  // Show all logs
}))
```

**Never log sensitive data**:

```go
// DON'T log passwords, API keys, tokens
logger.Info("User login", "password", password)  // ❌ SECURITY RISK

// DO redact or omit sensitive data
logger.Info("User login", "user_id", userID)  // ✅ No sensitive data

// DO truncate or hash if logging needed
logger.Debug("API call", "api_key", apiKey[:8]+"...")  // First 8 chars only
```

**Performance optimization**:

```go
// Check log level before expensive operations
if logger.Enabled(slog.LevelDebug) {
    // Only compute debug data if debug enabled
    debugData := computeExpensiveDebugInfo()
    logger.Debug("Debug info", "data", debugData)
}

// Lazy evaluation with slog
logger.Debug("Debug info",
    slog.Any("data", func() interface{} {
        return computeExpensiveDebugInfo()
        // => Only called if Debug level enabled
    }),
)
```

**Log rotation with external tools**:

```bash
# Use logrotate on Linux
/var/log/myapp/*.log {
    daily
    rotate 7
    compress
    delaycompress
    notifempty
    create 0644 appuser appgroup
}

# Or use lumberjack library
import "gopkg.in/natefinch/lumberjack.v2"

logger := &lumberjack.Logger{
    Filename:   "/var/log/myapp/app.log",
    MaxSize:    100,  // MB
    MaxBackups: 3,
    MaxAge:     28,   // days
    Compress:   true,
}
```
