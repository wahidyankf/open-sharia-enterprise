---
title: "How to Manage Configuration"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 610
description: "Handle configuration with environment variables, viper, and structured config patterns"
tags: ["golang", "configuration", "viper", "environment-variables", "config"]
---

## Problem

Hardcoded configuration makes applications inflexible across environments. Changing database URLs or API keys requires recompilation. Different environments (development, staging, production) need different values without code changes. Managing configuration securely while avoiding secrets in version control is challenging.

This guide shows effective configuration management in Go.

## Environment Variables

### Reading Environment Variables

```go
import "os"

// ✅ Read environment variable
func GetDatabaseURL() string {
    return os.Getenv("DATABASE_URL")
}

// ✅ With default value
func GetPort() string {
    port := os.Getenv("PORT")
    if port == "" {
        return "8080"
    }
    return port
}

// ✅ Required environment variable
func GetRequiredEnv(key string) (string, error) {
    value := os.Getenv(key)
    if value == "" {
        return "", fmt.Errorf("required environment variable not set: %s", key)
    }
    return value, nil
}

// ✅ Parse as integer
func GetMaxConnections() (int, error) {
    value := os.Getenv("MAX_CONNECTIONS")
    if value == "" {
        return 10, nil // Default
    }

    n, err := strconv.Atoi(value)
    if err != nil {
        return 0, fmt.Errorf("invalid MAX_CONNECTIONS: %w", err)
    }

    return n, nil
}
```

### Environment Variable Patterns

```go
// ✅ Configuration struct
type Config struct {
    DatabaseURL    string
    Port           int
    Debug          bool
    MaxConnections int
    APIKey         string
}

func LoadConfig() (*Config, error) {
    config := &Config{}

    // Required fields
    var err error
    config.DatabaseURL, err = GetRequiredEnv("DATABASE_URL")
    if err != nil {
        return nil, err
    }

    config.APIKey, err = GetRequiredEnv("API_KEY")
    if err != nil {
        return nil, err
    }

    // Optional with defaults
    config.Port = getEnvAsInt("PORT", 8080)
    config.MaxConnections = getEnvAsInt("MAX_CONNECTIONS", 10)
    config.Debug = getEnvAsBool("DEBUG", false)

    return config, nil
}

func getEnvAsInt(key string, defaultVal int) int {
    valueStr := os.Getenv(key)
    if valueStr == "" {
        return defaultVal
    }

    value, err := strconv.Atoi(valueStr)
    if err != nil {
        return defaultVal
    }

    return value
}

func getEnvAsBool(key string, defaultVal bool) bool {
    valueStr := os.Getenv(key)
    if valueStr == "" {
        return defaultVal
    }

    value, err := strconv.ParseBool(valueStr)
    if err != nil {
        return defaultVal
    }

    return value
}
```

## .env Files with godotenv

### Loading .env Files

```go
import "github.com/joho/godotenv"

// ✅ Load .env file
func main() {
    // Load .env file in development
    if err := godotenv.Load(); err != nil {
        log.Println("No .env file found") // Not fatal in production
    }

    config, err := LoadConfig()
    if err != nil {
        log.Fatal(err)
    }

    runApp(config)
}

// ✅ Load specific file
func loadDevConfig() error {
    return godotenv.Load(".env.development")
}

// ✅ Load multiple files (first found wins)
func loadEnvFiles() error {
    return godotenv.Load(".env.local", ".env")
}
```

**.env file format:**

```bash
# Database configuration
DATABASE_URL=postgresql://localhost:5432/mydb
DB_MAX_CONNECTIONS=20

# API configuration
API_KEY=sk_test_abc123
API_BASE_URL=https://api.example.com

# Application settings
PORT=8080
DEBUG=true
LOG_LEVEL=info
```

**.gitignore:**

```gitignore
# Don't commit actual secrets
.env
.env.local
.env.*.local

# Commit template
!.env.example
```

**.env.example (committed to repo):**

```bash
# Copy to .env and fill in values
DATABASE_URL=
DB_MAX_CONNECTIONS=20
API_KEY=
PORT=8080
DEBUG=false
```

## Flag Package for Command-Line Args

### Basic Flags

```go
import "flag"

func main() {
    // ✅ Define flags
    port := flag.Int("port", 8080, "Server port")
    host := flag.String("host", "localhost", "Server host")
    debug := flag.Bool("debug", false, "Enable debug mode")
    config := flag.String("config", "", "Config file path")

    // Parse flags
    flag.Parse()

    fmt.Printf("Starting server on %s:%d\n", *host, *port)
    fmt.Printf("Debug mode: %v\n", *debug)

    if *config != "" {
        fmt.Printf("Using config file: %s\n", *config)
    }
}
```

**Usage:**

```bash
# Use defaults
./app

# Override values
./app -port 3000 -debug

# Long form
./app --port=3000 --debug=true --config=/etc/app/config.yaml
```

### Flag Variables

```go
func main() {
    var config struct {
        Port  int
        Host  string
        Debug bool
    }

    // ✅ Bind flags to struct fields
    flag.IntVar(&config.Port, "port", 8080, "Server port")
    flag.StringVar(&config.Host, "host", "localhost", "Server host")
    flag.BoolVar(&config.Debug, "debug", false, "Enable debug mode")

    flag.Parse()

    // Use config directly
    fmt.Printf("Server: %s:%d\n", config.Host, config.Port)
}
```

## Viper for Advanced Configuration

### Setting Up Viper

```go
import "github.com/spf13/viper"

// ✅ Configure viper
func InitConfig() error {
    // Set config file name and paths
    viper.SetConfigName("config")
    viper.SetConfigType("yaml")
    viper.AddConfigPath(".")
    viper.AddConfigPath("$HOME/.myapp")
    viper.AddConfigPath("/etc/myapp")

    // Environment variables
    viper.AutomaticEnv()
    viper.SetEnvPrefix("MYAPP") // MYAPP_DATABASE_URL

    // Set defaults
    viper.SetDefault("port", 8080)
    viper.SetDefault("database.max_connections", 10)

    // Read config file
    if err := viper.ReadInConfig(); err != nil {
        if _, ok := err.(viper.ConfigFileNotFoundError); ok {
            // Config file not found; using defaults and env vars
            log.Println("No config file found, using defaults")
        } else {
            return fmt.Errorf("reading config: %w", err)
        }
    }

    return nil
}

func main() {
    if err := InitConfig(); err != nil {
        log.Fatal(err)
    }

    // Access values
    port := viper.GetInt("port")
    dbURL := viper.GetString("database.url")
    debug := viper.GetBool("debug")

    fmt.Printf("Port: %d\n", port)
    fmt.Printf("DB URL: %s\n", dbURL)
}
```

### config.yaml Format

```yaml
# Application settings
port: 8080
debug: false
log_level: info

# Database configuration
database:
  url: postgresql://localhost:5432/mydb
  max_connections: 20
  timeout: 30s

# API configuration
api:
  base_url: https://api.example.com
  key: ${API_KEY} # From environment variable
  timeout: 10s

# Feature flags
features:
  new_ui: true
  beta_features: false
```

### Unmarshal to Struct

```go
type Config struct {
    Port     int          `mapstructure:"port"`
    Debug    bool         `mapstructure:"debug"`
    LogLevel string       `mapstructure:"log_level"`
    Database DatabaseConfig `mapstructure:"database"`
    API      APIConfig    `mapstructure:"api"`
}

type DatabaseConfig struct {
    URL            string `mapstructure:"url"`
    MaxConnections int    `mapstructure:"max_connections"`
    Timeout        string `mapstructure:"timeout"`
}

type APIConfig struct {
    BaseURL string `mapstructure:"base_url"`
    Key     string `mapstructure:"key"`
    Timeout string `mapstructure:"timeout"`
}

// ✅ Unmarshal config to struct
func LoadConfigStruct() (*Config, error) {
    if err := InitConfig(); err != nil {
        return nil, err
    }

    var config Config
    if err := viper.Unmarshal(&config); err != nil {
        return nil, fmt.Errorf("unmarshaling config: %w", err)
    }

    return &config, nil
}

func main() {
    config, err := LoadConfigStruct()
    if err != nil {
        log.Fatal(err)
    }

    fmt.Printf("Port: %d\n", config.Port)
    fmt.Printf("DB URL: %s\n", config.Database.URL)
}
```

## Configuration Priority

### Layered Configuration

```go
// ✅ Priority order (highest to lowest):
// 1. Command-line flags
// 2. Environment variables
// 3. Config file
// 4. Default values

func SetupConfig() (*Config, error) {
    // 1. Set defaults
    viper.SetDefault("port", 8080)
    viper.SetDefault("host", "localhost")

    // 2. Read config file
    viper.SetConfigName("config")
    viper.AddConfigPath(".")
    viper.ReadInConfig() // Ignore error if not found

    // 3. Bind environment variables
    viper.AutomaticEnv()

    // 4. Bind command-line flags
    flag.Int("port", 8080, "Server port")
    flag.String("host", "localhost", "Server host")
    flag.Parse()
    viper.BindPFlags(flag.CommandLine)

    var config Config
    if err := viper.Unmarshal(&config); err != nil {
        return nil, err
    }

    return &config, nil
}
```

## Validation

### Config Validation

```go
// ✅ Validate configuration
func (c *Config) Validate() error {
    if c.Port < 1024 || c.Port > 65535 {
        return fmt.Errorf("port must be between 1024 and 65535, got %d", c.Port)
    }

    if c.Database.URL == "" {
        return fmt.Errorf("database URL is required")
    }

    if c.Database.MaxConnections < 1 {
        return fmt.Errorf("max connections must be at least 1")
    }

    if c.API.Key == "" {
        return fmt.Errorf("API key is required")
    }

    return nil
}

func main() {
    config, err := LoadConfigStruct()
    if err != nil {
        log.Fatal(err)
    }

    if err := config.Validate(); err != nil {
        log.Fatalf("Invalid configuration: %v", err)
    }

    runApp(config)
}
```

## Security Best Practices

### Avoiding Secrets in Code

```go
// ❌ Hardcoded secrets
const (
    APIKey = "sk_live_secret123"  // NEVER do this
    DBPassword = "mypassword"     // NEVER do this
)

// ✅ Load from environment
func GetAPIKey() (string, error) {
    key := os.Getenv("API_KEY")
    if key == "" {
        return "", errors.New("API_KEY not set")
    }
    return key, nil
}

// ✅ Fail fast on missing secrets
func MustGetSecret(key string) string {
    value := os.Getenv(key)
    if value == "" {
        log.Fatalf("Required secret not set: %s", key)
    }
    return value
}
```

### Secret Management

```go
// ✅ Load secrets from secret manager (AWS Secrets Manager example)
import "github.com/aws/aws-sdk-go/service/secretsmanager"

func GetSecretFromAWS(secretName string) (string, error) {
    svc := secretsmanager.New(session.New())

    input := &secretsmanager.GetSecretValueInput{
        SecretId: aws.String(secretName),
    }

    result, err := svc.GetSecretValue(input)
    if err != nil {
        return "", fmt.Errorf("getting secret: %w", err)
    }

    return *result.SecretString, nil
}

// ✅ Cache secrets
type SecretCache struct {
    secrets map[string]string
    mu      sync.RWMutex
}

func (c *SecretCache) Get(key string) (string, error) {
    c.mu.RLock()
    value, ok := c.secrets[key]
    c.mu.RUnlock()

    if ok {
        return value, nil
    }

    // Fetch from secret manager
    value, err := GetSecretFromAWS(key)
    if err != nil {
        return "", err
    }

    // Cache it
    c.mu.Lock()
    c.secrets[key] = value
    c.mu.Unlock()

    return value, nil
}
```

## Hot Reloading Configuration

### Watch Config Changes

```go
// ✅ Watch for config file changes
func WatchConfig(callback func()) {
    viper.WatchConfig()
    viper.OnConfigChange(func(e fsnotify.Event) {
        log.Println("Config file changed:", e.Name)
        callback()
    })
}

func main() {
    if err := InitConfig(); err != nil {
        log.Fatal(err)
    }

    config, _ := LoadConfigStruct()

    // Reload config on file change
    WatchConfig(func() {
        newConfig, err := LoadConfigStruct()
        if err != nil {
            log.Println("Error reloading config:", err)
            return
        }

        // Update application config
        updateAppConfig(newConfig)
        log.Println("Configuration reloaded")
    })

    runApp(config)
}
```

## Summary

Configuration management in Go uses environment variables as the primary mechanism, with .env files for development convenience and flag package for command-line overrides. Environment variables work universally across deployment platforms and keep secrets out of version control.

godotenv loads .env files in development without requiring environment variable setup on each developer machine. Production deployments use actual environment variables. Never commit .env files containing secrets - use .env.example templates instead.

flag package provides command-line argument parsing with type safety and automatic help text generation. Define flags with default values, parse with flag.Parse(), access through pointers. Combine with environment variables for flexible configuration.

Viper offers advanced configuration features - multiple file formats (YAML, JSON, TOML), hierarchical configuration, environment variable binding, command-line flag binding, and configuration file watching. Use when applications need complex configuration with multiple sources.

Configuration priority follows command-line flags > environment variables > config files > defaults. This ordering lets operators override any setting at runtime without modifying files. Viper implements this priority automatically.

Validation ensures configuration correctness at startup. Check required fields exist, validate ranges for numeric values, verify URLs parse correctly. Fail fast during initialization rather than crashing later during request handling.

Never hardcode secrets in source code. Load from environment variables, secret management services (AWS Secrets Manager, HashiCorp Vault), or encrypted configuration files. Fail immediately if required secrets are missing.

Configuration reloading with Viper's watch capability enables updating settings without restarting applications. Watch configuration files for changes, reload on modification, apply new settings. Useful for feature flags and operational parameters.

Struct-based configuration with mapstructure tags provides type safety and validation. Unmarshal configuration into structs with proper types, validate with custom validation methods. This approach catches configuration errors at startup, not during runtime.

## Related Content

- [Go Best Practices and Idioms](/en/learn/swe/prog-lang/golang/explanation/best-practices)
- [How to Handle Files and Resources](/en/learn/swe/prog-lang/golang/how-to/handle-files-and-resources)
- [How to Build CLI Applications](/en/learn/swe/prog-lang/golang/how-to/build-cli-applications)
