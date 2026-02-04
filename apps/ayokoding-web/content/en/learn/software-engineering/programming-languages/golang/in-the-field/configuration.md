---
title: "Configuration"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Configuration management with os.Getenv, godotenv for .env files, limitations, and production config frameworks (viper)"
weight: 10000056
tags: ["golang", "configuration", "viper", "12-factor", "environment-variables", "production"]
---

## Why Configuration Management Matters in Go

Proper configuration management is essential for 12-factor app compliance, enabling applications to run across environments (dev/staging/prod) without code changes. Understanding environment variable patterns before adopting config frameworks prevents over-engineering simple services and enables informed framework selection for complex configuration needs.

**Core benefits**:

- **Environment portability**: Same binary runs in dev/staging/prod
- **Security**: Secrets in environment, not code
- **Flexibility**: Change behavior without recompilation
- **12-factor compliance**: Configuration in environment (factor III)

**Problem**: Many developers hardcode configuration or immediately adopt heavyweight frameworks (viper) for simple cases, adding unnecessary complexity. Conversely, complex applications using only environment variables become unmaintainable as configuration grows.

**Solution**: Start with `os.Getenv` for fundamentals, add `godotenv` for .env file support, recognize limitations (no validation, no defaults), then introduce production frameworks (viper) with clear rationale based on configuration complexity.

## Standard Library First: os.Getenv Basics

Go's `os` package provides direct environment variable access. Suitable for simple applications with few configuration values.

**Basic environment variable pattern**:

```go
package main

import (
    "fmt"
    "os"
    // => Standard library for OS operations
    // => Includes environment variable access
)

func main() {
    // os.Getenv retrieves environment variable by name
    // => Returns string value or "" if not set
    // => No error returned (empty string for missing vars)

    port := os.Getenv("PORT")
    // => port is string (may be empty)
    // => Example: PORT=8080 → port is "8080"
    // => If PORT not set → port is ""

    if port == "" {
        // => Environment variable not set
        // => Provide default value
        port = "8080"
        // => Default port for development
    }

    dbHost := os.Getenv("DB_HOST")
    // => dbHost is database hostname
    // => Example: DB_HOST=localhost

    if dbHost == "" {
        // => Required configuration missing
        fmt.Fprintln(os.Stderr, "Error: DB_HOST environment variable required")
        // => Write error to stderr
        os.Exit(1)
        // => Exit with error code 1
        // => Application cannot start without database
    }

    fmt.Printf("Starting server on port %s\n", port)
    fmt.Printf("Connecting to database at %s\n", dbHost)
    // => Output configuration for debugging
}
```

**Type conversion pattern**:

```go
package main

import (
    "fmt"
    "os"
    "strconv"
    // => Standard library for string conversions
    // => Includes ParseInt, ParseBool, ParseFloat
)

func main() {
    // Environment variables are always strings
    // => Must convert to appropriate types
    // => Manual conversion with error handling

    portStr := os.Getenv("PORT")
    if portStr == "" {
        portStr = "8080"
    }

    port, err := strconv.Atoi(portStr)
    // => strconv.Atoi converts string to int
    // => Atoi is short for ASCII to integer
    // => Returns (int, error)
    // => err is non-nil if conversion fails

    if err != nil {
        // => Invalid port value (not a number)
        fmt.Fprintf(os.Stderr, "Error: PORT must be a number, got: %s\n", portStr)
        os.Exit(1)
    }

    if port < 1 || port > 65535 {
        // => Validate port range
        // => Valid TCP ports: 1-65535
        fmt.Fprintf(os.Stderr, "Error: PORT must be between 1 and 65535, got: %d\n", port)
        os.Exit(1)
    }

    debugStr := os.Getenv("DEBUG")
    debug := false
    // => Default value for bool

    if debugStr != "" {
        debug, err = strconv.ParseBool(debugStr)
        // => ParseBool accepts: 1, t, T, true, TRUE, True, 0, f, F, false, FALSE, False
        // => Returns (bool, error)
        if err != nil {
            fmt.Fprintf(os.Stderr, "Error: DEBUG must be true/false, got: %s\n", debugStr)
            os.Exit(1)
        }
    }

    maxConnsStr := os.Getenv("MAX_CONNECTIONS")
    maxConns := 100
    // => Default max connections

    if maxConnsStr != "" {
        maxConns, err = strconv.Atoi(maxConnsStr)
        if err != nil || maxConns < 1 {
            fmt.Fprintf(os.Stderr, "Error: MAX_CONNECTIONS must be positive integer, got: %s\n", maxConnsStr)
            os.Exit(1)
        }
    }

    fmt.Printf("Port: %d, Debug: %v, MaxConns: %d\n", port, debug, maxConns)
    // => Output: Port: 8080, Debug: false, MaxConns: 100
}
```

**Configuration struct pattern**:

```go
package main

import (
    "fmt"
    "os"
    "strconv"
    "time"
)

type Config struct {
    // => Configuration struct centralizes all config values
    // => Passed to application components
    ServerPort      int
    // => HTTP server port
    DatabaseURL     string
    // => Database connection string
    LogLevel        string
    // => Logging level (debug, info, warn, error)
    RequestTimeout  time.Duration
    // => HTTP request timeout
    EnableMetrics   bool
    // => Enable Prometheus metrics endpoint
}

func LoadConfig() (*Config, error) {
    // => LoadConfig parses environment variables
    // => Returns pointer to Config and error
    // => Centralized configuration loading

    cfg := &Config{
        // => Initialize with defaults
        ServerPort:     8080,
        LogLevel:       "info",
        RequestTimeout: 30 * time.Second,
        EnableMetrics:  false,
    }

    // Parse server port
    if portStr := os.Getenv("SERVER_PORT"); portStr != "" {
        // => Override default if env var set
        port, err := strconv.Atoi(portStr)
        if err != nil {
            return nil, fmt.Errorf("invalid SERVER_PORT: %w", err)
            // => Return error, don't os.Exit (testable)
            // => %w wraps original error for error chains
        }
        cfg.ServerPort = port
    }

    // Parse database URL (required)
    cfg.DatabaseURL = os.Getenv("DATABASE_URL")
    if cfg.DatabaseURL == "" {
        return nil, fmt.Errorf("DATABASE_URL is required")
        // => Required config validation
    }

    // Parse log level with validation
    if logLevel := os.Getenv("LOG_LEVEL"); logLevel != "" {
        // => Validate against allowed values
        validLevels := map[string]bool{
            "debug": true,
            "info":  true,
            "warn":  true,
            "error": true,
        }
        if !validLevels[logLevel] {
            return nil, fmt.Errorf("invalid LOG_LEVEL: %s (must be debug/info/warn/error)", logLevel)
        }
        cfg.LogLevel = logLevel
    }

    // Parse request timeout
    if timeoutStr := os.Getenv("REQUEST_TIMEOUT"); timeoutStr != "" {
        timeout, err := time.ParseDuration(timeoutStr)
        // => time.ParseDuration parses duration strings
        // => Example: "30s", "5m", "1h30m"
        // => Returns (time.Duration, error)
        if err != nil {
            return nil, fmt.Errorf("invalid REQUEST_TIMEOUT: %w", err)
        }
        cfg.RequestTimeout = timeout
    }

    // Parse enable metrics
    if metricsStr := os.Getenv("ENABLE_METRICS"); metricsStr != "" {
        metrics, err := strconv.ParseBool(metricsStr)
        if err != nil {
            return nil, fmt.Errorf("invalid ENABLE_METRICS: %w", err)
        }
        cfg.EnableMetrics = metrics
    }

    return cfg, nil
    // => Return validated config
}

func main() {
    cfg, err := LoadConfig()
    // => Load and validate configuration
    if err != nil {
        fmt.Fprintf(os.Stderr, "Configuration error: %v\n", err)
        os.Exit(1)
    }

    fmt.Printf("Config loaded: %+v\n", cfg)
    // => %+v prints struct with field names
    // => Output: Config loaded: {ServerPort:8080 DatabaseURL:postgres://... LogLevel:info ...}
}
```

**Secret management pattern**:

```go
package main

import (
    "fmt"
    "os"
    "strings"
)

type Secrets struct {
    APIKey        string
    DatabasePassword string
    JWTSigningKey string
}

func LoadSecrets() (*Secrets, error) {
    // => Load sensitive credentials from environment
    // => Never log or print secrets

    secrets := &Secrets{
        APIKey:           os.Getenv("API_KEY"),
        DatabasePassword: os.Getenv("DB_PASSWORD"),
        JWTSigningKey:    os.Getenv("JWT_SIGNING_KEY"),
    }

    // Validate required secrets
    var missing []string

    if secrets.APIKey == "" {
        missing = append(missing, "API_KEY")
    }
    if secrets.DatabasePassword == "" {
        missing = append(missing, "DB_PASSWORD")
    }
    if secrets.JWTSigningKey == "" {
        missing = append(missing, "JWT_SIGNING_KEY")
    }

    if len(missing) > 0 {
        return nil, fmt.Errorf("missing required secrets: %s", strings.Join(missing, ", "))
        // => Clear error message without exposing values
    }

    return secrets, nil
}

func main() {
    secrets, err := LoadSecrets()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error loading secrets: %v\n", err)
        os.Exit(1)
    }

    // NEVER log secrets
    fmt.Println("Secrets loaded successfully")
    // => DO NOT print: fmt.Printf("API Key: %s\n", secrets.APIKey)

    // Use secrets in application
    _ = secrets
}
```

**Limitations for production configuration**:

- **No .env file support**: Must manually export environment variables
- **No configuration file support**: Cannot use YAML/JSON/TOML config files
- **No type safety**: Manual string conversion with verbose error handling
- **No validation**: Must manually validate each value
- **No defaults**: Must check empty strings and assign defaults
- **No nested configuration**: Flat key-value pairs only
- **No multi-environment support**: Cannot switch between dev/staging/prod configs
- **No hot reload**: Must restart application to update configuration
- **No precedence chain**: Manual checking of multiple sources (env, file, defaults)

## Enhanced Pattern: godotenv for .env Files

`godotenv` loads environment variables from `.env` files, enabling development environment configuration without exporting variables manually.

**Why godotenv enhances standard library**:

- **Development convenience**: .env file instead of manual exports
- **Local secrets**: Store dev credentials in .env (gitignored)
- **Consistency**: Same variable names in .env and production
- **Backward compatible**: Falls back to real environment variables

**Basic .env loading pattern**:

```go
package main

import (
    "fmt"
    "log"
    "os"

    "github.com/joho/godotenv"
    // => External dependency: github.com/joho/godotenv
    // => Loads .env files into environment
    // => Install: go get github.com/joho/godotenv
)

func main() {
    // Load .env file (if exists)
    // => Looks for .env in current directory
    // => Does NOT override existing environment variables
    // => Production: real env vars take precedence

    err := godotenv.Load()
    // => godotenv.Load() loads .env file
    // => Returns error if file not found
    // => Non-fatal in production (file may not exist)

    if err != nil {
        log.Println("No .env file found, using environment variables")
        // => Warning, not error
        // => Production: no .env file, use real env vars
    }

    // Access variables with os.Getenv (same as before)
    // => godotenv loaded values into environment
    // => No API changes from standard library

    port := os.Getenv("PORT")
    if port == "" {
        port = "8080"
    }

    dbHost := os.Getenv("DB_HOST")
    if dbHost == "" {
        log.Fatal("DB_HOST required")
    }

    fmt.Printf("Port: %s, DB Host: %s\n", port, dbHost)
}
```

**.env file format**:

```bash
# .env file
# Development configuration
# DO NOT commit secrets to version control

PORT=3000
# => Server port for development

DB_HOST=localhost
DB_PORT=5432
DB_NAME=myapp_dev
DB_USER=postgres
DB_PASSWORD=secret123
# => Database connection details

LOG_LEVEL=debug
# => Verbose logging for development

API_KEY=dev_key_12345
# => Development API key (not production key)

ENABLE_METRICS=false
# => Disable metrics in dev
```

**Multi-environment .env files**:

```go
package main

import (
    "fmt"
    "log"
    "os"

    "github.com/joho/godotenv"
)

func main() {
    // Load environment-specific .env file
    // => .env.development, .env.staging, .env.production
    // => Determined by ENV environment variable

    env := os.Getenv("ENV")
    // => ENV determines which .env file to load
    // => Example: ENV=staging → load .env.staging

    if env == "" {
        env = "development"
        // => Default to development environment
    }

    envFile := fmt.Sprintf(".env.%s", env)
    // => envFile is .env.development, .env.staging, etc.

    err := godotenv.Load(envFile)
    // => Load specific environment file
    // => Example: godotenv.Load(".env.staging")

    if err != nil {
        log.Printf("No %s file found, using environment variables\n", envFile)
    }

    // Optionally load local overrides
    // => .env.local for machine-specific overrides
    // => Gitignored, not committed
    // => Highest priority

    _ = godotenv.Load(".env.local")
    // => Ignore error if .env.local doesn't exist
    // => Overrides values from .env.<env>

    port := os.Getenv("PORT")
    fmt.Printf("Environment: %s, Port: %s\n", env, port)
}
```

**Configuration loading with godotenv**:

```go
package main

import (
    "fmt"
    "log"
    "os"
    "strconv"

    "github.com/joho/godotenv"
)

type Config struct {
    Environment string
    Port        int
    DatabaseURL string
    LogLevel    string
}

func LoadConfig() (*Config, error) {
    // Load .env file first (development)
    // => Ignores error (production may not have .env)
    _ = godotenv.Load()

    cfg := &Config{
        Environment: getEnv("ENVIRONMENT", "development"),
        // => getEnv helper with default
        LogLevel:    getEnv("LOG_LEVEL", "info"),
    }

    // Parse port with default
    portStr := getEnv("PORT", "8080")
    port, err := strconv.Atoi(portStr)
    if err != nil {
        return nil, fmt.Errorf("invalid PORT: %w", err)
    }
    cfg.Port = port

    // Required database URL
    cfg.DatabaseURL = os.Getenv("DATABASE_URL")
    if cfg.DatabaseURL == "" {
        return nil, fmt.Errorf("DATABASE_URL is required")
    }

    return cfg, nil
}

func getEnv(key, defaultValue string) string {
    // => Helper function for environment variable with default
    // => Reduces boilerplate
    if value := os.Getenv(key); value != "" {
        return value
    }
    return defaultValue
}

func main() {
    cfg, err := LoadConfig()
    if err != nil {
        log.Fatalf("Configuration error: %v\n", err)
    }

    fmt.Printf("Loaded config: %+v\n", cfg)
}
```

**Limitations remaining after godotenv**:

- **Still manual validation**: No automatic type checking or validation
- **No configuration files**: Only supports .env format (not YAML/JSON/TOML)
- **No nested structures**: Flat key-value pairs only
- **No hot reload**: Still requires application restart
- **Limited defaults**: Must manually handle defaults for each variable
- **No precedence configuration**: Basic env var precedence only
- **No configuration schema**: No way to define expected configuration shape

## Production Framework: Viper for Complex Configuration

Viper is the most comprehensive Go configuration framework, supporting multiple formats, precedence chains, and hot reloading. Used by Kubernetes, Hugo, and other major projects.

**Why viper over os.Getenv/godotenv**:

- **Multiple sources**: Environment variables, config files, command-line flags
- **Multiple formats**: YAML, JSON, TOML, HCL, INI, Java properties
- **Precedence chain**: Flags > Env vars > Config file > Defaults
- **Hot reload**: Watch config file for changes
- **Type safety**: GetInt, GetBool, GetDuration helpers
- **Nested configuration**: Hierarchical config structures
- **Aliases**: Multiple names for same config value

**Basic viper pattern**:

```go
package main

import (
    "fmt"
    "log"

    "github.com/spf13/viper"
    // => External dependency: github.com/spf13/viper
    // => Comprehensive configuration management
    // => Install: go get github.com/spf13/viper
)

func main() {
    // Set configuration defaults
    // => Lowest priority in precedence chain
    // => Used if no other source provides value

    viper.SetDefault("server.port", 8080)
    // => Nested key: server.port
    // => Accessed with dot notation
    viper.SetDefault("server.host", "localhost")
    viper.SetDefault("database.maxConnections", 100)
    viper.SetDefault("logging.level", "info")

    // Automatic environment variable binding
    // => Reads from environment automatically
    // => Converts dots to underscores

    viper.AutomaticEnv()
    // => Enables automatic env var reading
    // => server.port → SERVER_PORT
    // => database.maxConnections → DATABASE_MAXCONNECTIONS

    viper.SetEnvPrefix("myapp")
    // => Add prefix to environment variables
    // => server.port → MYAPP_SERVER_PORT
    // => Prevents collisions with other apps

    // Read configuration file
    // => config.yaml, config.json, config.toml

    viper.SetConfigName("config")
    // => Config file name without extension
    // => Looks for config.yaml, config.json, etc.

    viper.SetConfigType("yaml")
    // => Config file type (yaml, json, toml, hcl, ini)
    // => Required if config file has no extension

    viper.AddConfigPath(".")
    // => Look for config file in current directory
    viper.AddConfigPath("/etc/myapp")
    // => Also look in /etc/myapp (Linux standard)
    viper.AddConfigPath("$HOME/.myapp")
    // => Also look in user's home directory

    if err := viper.ReadInConfig(); err != nil {
        // => Reads config file from paths
        // => Returns error if no file found
        if _, ok := err.(viper.ConfigFileNotFoundError); ok {
            // => Config file not found (non-fatal)
            log.Println("No config file found, using defaults and environment")
        } else {
            // => Config file found but failed to parse
            log.Fatalf("Error reading config file: %v", err)
        }
    }

    // Access configuration values
    // => Type-safe getters with automatic conversion

    port := viper.GetInt("server.port")
    // => GetInt returns int (0 if not found)
    // => Converts string to int automatically
    // => Precedence: flag > env > config file > default

    host := viper.GetString("server.host")
    // => GetString returns string

    maxConns := viper.GetInt("database.maxConnections")
    logLevel := viper.GetString("logging.level")

    fmt.Printf("Server: %s:%d\n", host, port)
    fmt.Printf("Database max connections: %d\n", maxConns)
    fmt.Printf("Log level: %s\n", logLevel)
}
```

**config.yaml example**:

```yaml
server:
  port: 8080
  host: "0.0.0.0"
  readTimeout: "30s"
  writeTimeout: "30s"

database:
  host: "localhost"
  port: 5432
  name: "myapp"
  maxConnections: 100
  connectionTimeout: "10s"

logging:
  level: "info"
  format: "json"

features:
  enableMetrics: true
  enableTracing: false
```

**Configuration struct unmarshal pattern**:

```go
package main

import (
    "fmt"
    "log"
    "time"

    "github.com/spf13/viper"
)

type Config struct {
    Server   ServerConfig   `mapstructure:"server"`
    // => mapstructure tag maps YAML key to field
    // => Handles nested structures
    Database DatabaseConfig `mapstructure:"database"`
    Logging  LoggingConfig  `mapstructure:"logging"`
    Features FeaturesConfig `mapstructure:"features"`
}

type ServerConfig struct {
    Port         int           `mapstructure:"port"`
    Host         string        `mapstructure:"host"`
    ReadTimeout  time.Duration `mapstructure:"readTimeout"`
    WriteTimeout time.Duration `mapstructure:"writeTimeout"`
}

type DatabaseConfig struct {
    Host              string        `mapstructure:"host"`
    Port              int           `mapstructure:"port"`
    Name              string        `mapstructure:"name"`
    MaxConnections    int           `mapstructure:"maxConnections"`
    ConnectionTimeout time.Duration `mapstructure:"connectionTimeout"`
}

type LoggingConfig struct {
    Level  string `mapstructure:"level"`
    Format string `mapstructure:"format"`
}

type FeaturesConfig struct {
    EnableMetrics bool `mapstructure:"enableMetrics"`
    EnableTracing bool `mapstructure:"enableTracing"`
}

func LoadConfig() (*Config, error) {
    viper.SetConfigName("config")
    viper.SetConfigType("yaml")
    viper.AddConfigPath(".")
    viper.AutomaticEnv()
    viper.SetEnvPrefix("myapp")

    // Set defaults
    viper.SetDefault("server.port", 8080)
    viper.SetDefault("server.host", "localhost")
    viper.SetDefault("logging.level", "info")

    if err := viper.ReadInConfig(); err != nil {
        if _, ok := err.(viper.ConfigFileNotFoundError); !ok {
            return nil, fmt.Errorf("error reading config: %w", err)
        }
    }

    var cfg Config
    if err := viper.Unmarshal(&cfg); err != nil {
        // => viper.Unmarshal decodes config into struct
        // => Handles type conversion automatically
        // => Returns error if types mismatch
        return nil, fmt.Errorf("error unmarshaling config: %w", err)
    }

    return &cfg, nil
}

func main() {
    cfg, err := LoadConfig()
    if err != nil {
        log.Fatalf("Configuration error: %v", err)
    }

    fmt.Printf("Config loaded: %+v\n", cfg)
    // => Structured configuration ready to use
}
```

**Hot reload configuration pattern**:

```go
package main

import (
    "fmt"
    "log"

    "github.com/spf13/viper"
)

func main() {
    viper.SetConfigName("config")
    viper.SetConfigType("yaml")
    viper.AddConfigPath(".")

    if err := viper.ReadInConfig(); err != nil {
        log.Fatalf("Error reading config: %v", err)
    }

    // Watch for configuration changes
    // => Automatically reloads when config file changes
    // => Useful for updating log levels without restart

    viper.WatchConfig()
    // => Starts file watcher
    // => Polls config file for changes

    viper.OnConfigChange(func(e fsnotify.Event) {
        // => Callback when config file changes
        // => e is fsnotify.Event (file event)
        fmt.Printf("Config file changed: %s\n", e.Name)

        // Reload log level dynamically
        newLogLevel := viper.GetString("logging.level")
        fmt.Printf("Updated log level to: %s\n", newLogLevel)

        // Update application state without restart
        updateLogLevel(newLogLevel)
    })

    // Application continues running
    // => Config changes applied automatically
    select {}
    // => Block forever (real app would have logic here)
}

func updateLogLevel(level string) {
    // Update logger configuration
    // ... logging framework update
}
```

**Command-line flag integration**:

```go
package main

import (
    "flag"
    "fmt"
    "log"

    "github.com/spf13/viper"
)

func main() {
    // Define command-line flags
    // => Highest priority in precedence chain

    configFile := flag.String("config", "", "path to config file")
    port := flag.Int("port", 0, "server port")
    logLevel := flag.String("log-level", "", "logging level")

    flag.Parse()
    // => Parse command-line flags

    // Bind flags to viper
    // => Flags override env vars and config file

    if *configFile != "" {
        viper.SetConfigFile(*configFile)
        // => Use specific config file
    } else {
        viper.SetConfigName("config")
        viper.AddConfigPath(".")
    }

    // Bind individual flags
    if *port != 0 {
        viper.Set("server.port", *port)
        // => Override port from flag
        // => Highest priority
    }

    if *logLevel != "" {
        viper.Set("logging.level", *logLevel)
    }

    viper.AutomaticEnv()

    if err := viper.ReadInConfig(); err != nil {
        if _, ok := err.(viper.ConfigFileNotFoundError); !ok {
            log.Fatalf("Error reading config: %v", err)
        }
    }

    // Precedence: flag > env > config file > default
    finalPort := viper.GetInt("server.port")
    finalLogLevel := viper.GetString("logging.level")

    fmt.Printf("Server port: %d (from %s)\n", finalPort, "precedence chain")
    fmt.Printf("Log level: %s\n", finalLogLevel)
}
```

## Trade-offs Comparison

| Aspect                  | os.Getenv                  | godotenv                      | viper                                            |
| ----------------------- | -------------------------- | ----------------------------- | ------------------------------------------------ |
| **Complexity**          | Minimal (stdlib)           | Low (single dependency)       | High (comprehensive framework)                   |
| **Configuration Files** | ❌ None                    | ✅ .env only                  | ✅ YAML, JSON, TOML, HCL, INI                    |
| **Type Safety**         | ❌ Manual conversion       | ❌ Manual conversion          | ✅ Type-safe getters                             |
| **Validation**          | ❌ Manual                  | ❌ Manual                     | ❌ Manual (but easier with unmarshaling)         |
| **Defaults**            | ❌ Manual                  | ❌ Manual                     | ✅ SetDefault                                    |
| **Precedence Chain**    | ❌ Manual                  | ❌ Basic (env > .env)         | ✅ Flag > Env > File > Default                   |
| **Hot Reload**          | ❌ None                    | ❌ None                       | ✅ WatchConfig                                   |
| **Nested Config**       | ❌ Flat key-value          | ❌ Flat key-value             | ✅ Hierarchical structures                       |
| **12-Factor Compliant** | ✅ Yes                     | ✅ Yes                        | ✅ Yes                                           |
| **Learning Curve**      | None (standard library)    | Minimal (drop-in enhancement) | Moderate (framework concepts)                    |
| **Use Cases**           | Simple apps, 5-10 vars     | Development convenience       | Complex apps, multiple sources, hot reload       |
| **Examples**            | Microservices, simple CLIs | Local development, small apps | Enterprise apps, services, multi-env deployments |
| **Binary Size**         | Smallest                   | Small (~50KB added)           | Larger (~1MB added)                              |

## Best Practices

**Progressive adoption strategy**:

1. **Start with os.Getenv**: 5-10 configuration values, no .env needed
2. **Add godotenv**: Development convenience, local .env files
3. **Adopt viper**: Complex configuration (>20 values), multiple sources, hot reload needed
4. **Stay simple**: Avoid viper if godotenv + os.Getenv sufficient

**When os.Getenv sufficient**:

- Simple microservices with few config values
- Container deployments (Kubernetes ConfigMaps, Docker env vars)
- 12-factor apps with environment-only configuration
- Minimal configuration surface (5-10 values)

**When godotenv appropriate**:

- Local development convenience (.env files)
- Small applications (10-20 config values)
- Don't need config file formats besides .env
- Simple precedence (env > .env > defaults)

**When viper justified**:

- Complex applications (>20 configuration values)
- Multiple configuration sources (files, env, flags)
- Need configuration file formats (YAML/JSON/TOML)
- Hot reload requirements (change log level without restart)
- Nested configuration structures
- Multiple environment configurations

**12-factor app configuration**:

```go
// Factor III: Store config in the environment
// => Configuration varies between deploys (dev, staging, prod)
// => Never commit secrets to version control

// Good: Environment-specific configuration
cfg := Config{
    DatabaseURL: os.Getenv("DATABASE_URL"),  // Different per environment
    APIKey:      os.Getenv("API_KEY"),       // Secret, never in code
    LogLevel:    os.Getenv("LOG_LEVEL"),     // Different per environment
}

// Bad: Hardcoded configuration
cfg := Config{
    DatabaseURL: "postgres://localhost/dev",  // ❌ Environment-specific
    APIKey:      "hardcoded_key_12345",       // ❌ Security risk
    LogLevel:    "debug",                      // ❌ Production would need different value
}
```

**Secret management best practices**:

```go
// DO: Load secrets from environment
apiKey := os.Getenv("API_KEY")
if apiKey == "" {
    log.Fatal("API_KEY required")
}

// DO: Use secret management services in production
// => AWS Secrets Manager, HashiCorp Vault, GCP Secret Manager
// => Load secrets at runtime, not from config files

// DON'T: Log secrets
log.Printf("API Key: %s", apiKey)  // ❌ Never log secrets

// DON'T: Print secrets in errors
return fmt.Errorf("failed with key %s", apiKey)  // ❌

// DO: Validate secrets without exposing
if len(apiKey) < 32 {
    return fmt.Errorf("API_KEY must be at least 32 characters")  // ✅
}
```

**Configuration validation pattern**:

```go
func ValidateConfig(cfg *Config) error {
    var errors []string

    if cfg.Port < 1 || cfg.Port > 65535 {
        errors = append(errors, "port must be between 1 and 65535")
    }

    if cfg.DatabaseURL == "" {
        errors = append(errors, "database URL is required")
    }

    validLogLevels := map[string]bool{
        "debug": true, "info": true, "warn": true, "error": true,
    }
    if !validLogLevels[cfg.LogLevel] {
        errors = append(errors, "log level must be debug/info/warn/error")
    }

    if cfg.RequestTimeout < time.Second {
        errors = append(errors, "request timeout must be at least 1 second")
    }

    if len(errors) > 0 {
        return fmt.Errorf("configuration validation failed:\n- %s",
            strings.Join(errors, "\n- "))
    }

    return nil
}
```

**Environment-specific configuration files**:

```bash
# Directory structure
config/
  ├── config.yaml          # Default configuration
  ├── config.dev.yaml      # Development overrides
  ├── config.staging.yaml  # Staging overrides
  └── config.prod.yaml     # Production overrides

# Load based on environment
ENV=production go run main.go
```

```go
func LoadConfig() (*Config, error) {
    viper.SetConfigName("config")
    viper.AddConfigPath("./config")

    // Load base config
    if err := viper.ReadInConfig(); err != nil {
        return nil, err
    }

    // Load environment-specific overrides
    env := os.Getenv("ENV")
    if env != "" {
        viper.SetConfigName(fmt.Sprintf("config.%s", env))
        viper.MergeInConfig()  // Merge with base config
    }

    var cfg Config
    if err := viper.Unmarshal(&cfg); err != nil {
        return nil, err
    }

    return &cfg, nil
}
```

**Testing with configuration**:

```go
func TestLoadConfig(t *testing.T) {
    // Set environment variables for test
    os.Setenv("PORT", "9090")
    os.Setenv("LOG_LEVEL", "debug")
    defer func() {
        // Clean up after test
        os.Unsetenv("PORT")
        os.Unsetenv("LOG_LEVEL")
    }()

    cfg, err := LoadConfig()
    if err != nil {
        t.Fatalf("LoadConfig failed: %v", err)
    }

    if cfg.Port != 9090 {
        t.Errorf("Expected port 9090, got %d", cfg.Port)
    }

    if cfg.LogLevel != "debug" {
        t.Errorf("Expected log level debug, got %s", cfg.LogLevel)
    }
}
```
