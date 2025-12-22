---
title: "How to Manage Configuration"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 1000008
description: "Handle configuration with environment variables, viper, and structured config patterns"
tags: ["golang", "configuration", "viper", "environment-variables", "config"]
categories: ["learn"]
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

## How It Works

### Configuration Loading Pipeline

Go configuration follows a layered approach with priority resolution:

1. **Default Values**: Hardcoded fallbacks in application code
2. **Config Files**: YAML/JSON/TOML loaded from filesystem
3. **Environment Variables**: OS-level configuration (12-factor apps)
4. **Command-Line Flags**: Runtime overrides via CLI arguments
5. **Merge and Resolve**: Higher priority sources override lower ones

### Environment Variable Resolution

`os.Getenv()` reads from process environment:

- Queries OS environment table (inherited from parent process)
- Returns empty string if variable not set (no error)
- Case-sensitive on Unix-like systems
- Available at any point in program execution
- No parsing - always returns string

### Viper Configuration Cascade

Viper merges multiple configuration sources:

```
defaults (SetDefault)
    ↓
config files (ReadInConfig)
    ↓ overrides
environment variables (AutomaticEnv)
    ↓ overrides
command-line flags (BindPFlags)
    ↓ overrides
explicit Set calls (highest priority)
```

Each layer overrides values from layers above it.

### godotenv File Parsing

godotenv loads `.env` files into environment:

1. **Read File**: Parse `.env` line by line
2. **Parse Lines**: Extract `KEY=value` pairs
3. **Set Environment**: Call `os.Setenv()` for each pair
4. **Skip Existing**: Does NOT override existing env vars
5. **Comments**: Ignore lines starting with `#`

**Important**: godotenv sets actual environment variables, affecting `os.Getenv()` globally.

### Flag Package Parsing

`flag` package provides CLI argument parsing:

- **Definition Phase**: `flag.String()`, `flag.Int()` register flags
- **Parse Phase**: `flag.Parse()` reads `os.Args`
- **Access Phase**: Dereference flag pointers to get values
- **Type Safety**: Automatic type conversion and validation
- **Help Text**: `-h` or `-help` shows usage automatically

## Variations

### 1. Multiple Environment Files

Load different `.env` files per environment:

```go
func LoadEnvByEnvironment() error {
    env := os.Getenv("GO_ENV")
    if env == "" {
        env = "development"
    }

    // Load base .env
    if err := godotenv.Load(".env"); err != nil {
        log.Println("No .env file")
    }

    // Load environment-specific file (overrides base)
    envFile := fmt.Sprintf(".env.%s", env)
    if err := godotenv.Load(envFile); err != nil {
        log.Printf("No %s file\n", envFile)
    }

    return nil
}

// File structure:
// .env              (shared defaults)
// .env.development  (dev overrides)
// .env.production   (prod overrides)
// .env.test         (test overrides)
```

**Trade-offs**: Flexible per-environment config but more files to manage.

### 2. Configuration as Code

Use Go code for complex configuration logic:

```go
type Config struct {
    Environment string
    Database    DatabaseConfig
    Cache       CacheConfig
}

func NewConfig() *Config {
    env := os.Getenv("GO_ENV")
    if env == "" {
        env = "development"
    }

    config := &Config{Environment: env}

    switch env {
    case "production":
        config.Database = DatabaseConfig{
            Host:           os.Getenv("DB_HOST"),
            MaxConnections: 100,
            SSLMode:        "require",
        }
        config.Cache = CacheConfig{
            Enabled: true,
            TTL:     time.Hour,
        }
    case "development":
        config.Database = DatabaseConfig{
            Host:           "localhost",
            MaxConnections: 10,
            SSLMode:        "disable",
        }
        config.Cache = CacheConfig{
            Enabled: false,
        }
    }

    return config
}
```

**Trade-offs**: Type-safe and flexible but requires recompilation for changes.

### 3. Remote Configuration with etcd/Consul

Fetch configuration from distributed key-value stores:

```go
import "github.com/hashicorp/consul/api"

func LoadConfigFromConsul() (*Config, error) {
    client, err := api.NewClient(api.DefaultConfig())
    if err != nil {
        return nil, err
    }

    kv := client.KV()

    // Get configuration values
    pair, _, err := kv.Get("app/config/database_url", nil)
    if err != nil {
        return nil, err
    }

    config := &Config{
        DatabaseURL: string(pair.Value),
    }

    return config, nil
}
```

**Trade-offs**: Centralized config and dynamic updates but adds infrastructure dependency.

### 4. Configuration Providers Pattern

Abstract configuration source behind interface:

```go
type ConfigProvider interface {
    GetString(key string) string
    GetInt(key string) int
    GetBool(key string) bool
}

// Environment provider
type EnvProvider struct{}

func (p *EnvProvider) GetString(key string) string {
    return os.Getenv(key)
}

// Viper provider
type ViperProvider struct{}

func (p *ViperProvider) GetString(key string) string {
    return viper.GetString(key)
}

// Use in application
type App struct {
    config ConfigProvider
}

func (app *App) Start() {
    dbURL := app.config.GetString("DATABASE_URL")
    // Use dbURL...
}
```

**Trade-offs**: Decouples from specific config library but adds abstraction layer.

### 5. Typed Configuration with Struct Tags

Use struct tags for validation and transformation:

```go
import "github.com/kelseyhightower/envconfig"

type Config struct {
    Port     int    `envconfig:"PORT" default:"8080"`
    Host     string `envconfig:"HOST" default:"localhost"`
    Debug    bool   `envconfig:"DEBUG" default:"false"`
    DBMaxConn int   `envconfig:"DB_MAX_CONN" required:"true"`
}

func LoadTypedConfig() (*Config, error) {
    var config Config
    err := envconfig.Process("", &config)
    if err != nil {
        return nil, err
    }
    return &config, nil
}
```

**Trade-offs**: Declarative and type-safe but limited to environment variables.

## Common Pitfalls

### 1. Hardcoding Secrets

**Problem**: Secrets committed to version control:

```go
// Bad: Secret in code
const APIKey = "sk_live_1234567890abcdef"

func callAPI() {
    client := api.NewClient(APIKey)  // Secret exposed!
}
```

**Solution**: Always load secrets from environment:

```go
// Good: Secret from environment
func callAPI() error {
    apiKey := os.Getenv("API_KEY")
    if apiKey == "" {
        return errors.New("API_KEY not set")
    }

    client := api.NewClient(apiKey)
    return nil
}

// Even better: Fail fast at startup
func MustGetAPIKey() string {
    apiKey := os.Getenv("API_KEY")
    if apiKey == "" {
        log.Fatal("API_KEY environment variable required")
    }
    return apiKey
}
```

### 2. Not Validating Configuration

**Problem**: Invalid configuration causes runtime failures:

```go
// Bad: No validation
func main() {
    port := os.Getenv("PORT")
    // What if PORT="invalid"? Runtime panic!
    portNum, _ := strconv.Atoi(port)
    server.Start(portNum)
}
```

**Solution**: Validate early at startup:

```go
// Good: Validate and fail fast
func LoadConfig() (*Config, error) {
    portStr := os.Getenv("PORT")
    if portStr == "" {
        return nil, errors.New("PORT required")
    }

    port, err := strconv.Atoi(portStr)
    if err != nil {
        return nil, fmt.Errorf("PORT must be integer: %w", err)
    }

    if port < 1024 || port > 65535 {
        return nil, fmt.Errorf("PORT must be 1024-65535, got %d", port)
    }

    return &Config{Port: port}, nil
}

func main() {
    config, err := LoadConfig()
    if err != nil {
        log.Fatalf("Configuration error: %v", err)
    }

    server.Start(config.Port)
}
```

### 3. Ignoring Config File Errors

**Problem**: Silently ignoring missing or invalid config files:

```go
// Bad: Ignoring errors
viper.ReadInConfig()  // Might fail silently
config := viper.GetString("database.url")  // Returns empty string on error!
```

**Solution**: Handle config file errors explicitly:

```go
// Good: Explicit error handling
if err := viper.ReadInConfig(); err != nil {
    if _, ok := err.(viper.ConfigFileNotFoundError); ok {
        log.Println("Config file not found, using defaults")
    } else {
        log.Fatalf("Error reading config file: %v", err)
    }
}

dbURL := viper.GetString("database.url")
if dbURL == "" {
    log.Fatal("database.url not configured")
}
```

### 4. Not Setting Defaults

**Problem**: Application breaks when optional config missing:

```go
// Bad: No defaults
timeout := os.Getenv("TIMEOUT")
timeoutDuration, _ := time.ParseDuration(timeout)  // Zero value if empty!
// timeoutDuration is 0, requests never timeout
```

**Solution**: Always provide sensible defaults:

```go
// Good: Defaults for optional config
func GetTimeout() time.Duration {
    timeoutStr := os.Getenv("TIMEOUT")
    if timeoutStr == "" {
        return 30 * time.Second  // Default
    }

    timeout, err := time.ParseDuration(timeoutStr)
    if err != nil {
        log.Printf("Invalid TIMEOUT, using default: %v", err)
        return 30 * time.Second
    }

    return timeout
}
```

### 5. Mutating Configuration at Runtime

**Problem**: Changing configuration during execution leads to race conditions:

```go
// Bad: Mutable global config
var GlobalConfig = &Config{}

func UpdateConfig(newPort int) {
    GlobalConfig.Port = newPort  // Race condition!
}

func HandleRequest() {
    port := GlobalConfig.Port  // Might read mid-update
}
```

**Solution**: Make configuration immutable or use proper synchronization:

```go
// Good: Immutable config
func main() {
    config := LoadConfig()  // Load once

    server := NewServer(config)  // Pass to components
    server.Start()
}

// Better: If config must update, use atomic operations
type AtomicConfig struct {
    value atomic.Value  // stores *Config
}

func (ac *AtomicConfig) Load() *Config {
    return ac.value.Load().(*Config)
}

func (ac *AtomicConfig) Store(config *Config) {
    ac.value.Store(config)
}
```

### 6. Mixing Configuration Concerns

**Problem**: Business logic mixed with configuration loading:

```go
// Bad: Logic in config loading
func LoadConfig() *Config {
    port, _ := strconv.Atoi(os.Getenv("PORT"))

    // Business logic doesn't belong here!
    if port == 8080 {
        setupDevMode()
    } else {
        setupProdMode()
    }

    return &Config{Port: port}
}
```

**Solution**: Separate configuration from application logic:

```go
// Good: Pure configuration loading
func LoadConfig() (*Config, error) {
    port, err := getEnvAsInt("PORT", 8080)
    if err != nil {
        return nil, err
    }

    return &Config{Port: port}, nil
}

// Business logic elsewhere
func main() {
    config, err := LoadConfig()
    if err != nil {
        log.Fatal(err)
    }

    // Application logic based on config
    if config.Port == 8080 {
        setupDevMode()
    } else {
        setupProdMode()
    }

    runApp(config)
}
```

## Related Patterns

**Related Tutorial**: See [Beginner Tutorial - Configuration](/en/learn/swe/prog-lang/golang/tutorials/beginner#configuration) for configuration basics.

**Related How-To**: See [Handle Files and Resources](/en/learn/swe/prog-lang/golang/how-to/handle-files-and-resources) for config file reading, [Build CLI Applications](/en/learn/swe/prog-lang/golang/how-to/build-cli-applications) for flag parsing, [Handle Errors Effectively](/en/learn/swe/prog-lang/golang/how-to/handle-errors-effectively) for config validation errors.

**Related Cookbook**: See Cookbook recipes "Environment Variables", "Viper Configuration", "Configuration Validation" for ready-to-use config patterns.

**Related Explanation**: See [Best Practices](/en/learn/swe/prog-lang/golang/explanation/best-practices) for configuration principles.
