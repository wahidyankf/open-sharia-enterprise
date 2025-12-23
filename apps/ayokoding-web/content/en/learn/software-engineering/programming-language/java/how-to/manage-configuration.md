---
title: "Manage Configuration"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 1000007
description: "Handle application configuration with properties files, environment variables, and configuration best practices"
tags: ["java", "configuration", "properties", "environment-variables"]
categories: ["learn"]
---

## Problem

Hardcoded configuration values make applications inflexible and insecure. Changing database URLs, API keys, or feature flags requires recompiling code. Accidentally committing secrets to version control exposes sensitive information. Different environments (development, staging, production) need different values.

This guide shows effective configuration management patterns in Java.

## Properties Files

### Loading Properties

```java
import java.util.Properties;
import java.io.IOException;
import java.io.InputStream;

// ✅ Load properties from classpath
public class ConfigLoader {
  private final Properties properties;

  public ConfigLoader(String filename) throws IOException {
    properties = new Properties();
    try (InputStream input = getClass().getClassLoader()
           .getResourceAsStream(filename)) {

      if (input == null) {
        throw new IOException("Unable to find " + filename);
      }

      properties.load(input);
    }
  }

  public String get(String key) {
    return properties.getProperty(key);
  }

  public String get(String key, String defaultValue) {
    return properties.getProperty(key, defaultValue);
  }

  public int getInt(String key) {
    return Integer.parseInt(properties.getProperty(key));
  }

  public boolean getBoolean(String key) {
    return Boolean.parseBoolean(properties.getProperty(key));
  }
}

// Usage
ConfigLoader config = new ConfigLoader("application.properties");
String dbUrl = config.get("database.url");
int maxConnections = config.getInt("database.max.connections");
```

### application.properties Format

```properties
# Database configuration
database.url=jdbc:postgresql://localhost:5432/mydb
database.username=dbuser
database.password=dbpass
database.max.connections=20

# API configuration
api.base.url=https://api.example.com
api.timeout.seconds=30
api.retry.attempts=3

# Feature flags
feature.new.dashboard.enabled=true
feature.beta.checkout.enabled=false

# Application settings
app.name=MyApplication
app.version=1.0.0
app.environment=development
```

**Why it matters**: Properties files separate configuration from code. Change values without recompiling. Different files for different environments - `application-dev.properties`, `application-prod.properties`.

## Environment Variables

### Reading Environment Variables

```java
// ✅ Read environment variable
public class EnvConfig {

  public static String getDatabaseUrl() {
    return System.getenv("DATABASE_URL");
  }

  public static String getDatabaseUrlOrDefault() {
    String url = System.getenv("DATABASE_URL");
    return url != null ? url : "jdbc:postgresql://localhost:5432/defaultdb";
  }

  public static int getPort() {
    String port = System.getenv("PORT");
    return port != null ? Integer.parseInt(port) : 8080;
  }

  // ✅ Required environment variable
  public static String getRequiredEnv(String name) {
    String value = System.getenv(name);
    if (value == null) {
      throw new IllegalStateException(
        "Required environment variable not set: " + name
      );
    }
    return value;
  }
}

// Usage
String apiKey = EnvConfig.getRequiredEnv("API_KEY");
int port = EnvConfig.getPort();
```

### Environment Variable Naming

```bash
# ✅ Naming conventions
DATABASE_URL=jdbc:postgresql://localhost:5432/mydb
API_KEY=sk_live_abc123xyz
MAX_CONNECTIONS=20
ENABLE_DEBUG=true

# Uppercase with underscores
# Descriptive names
# No spaces
```

**Why environment variables**: Cloud platforms (Heroku, AWS, etc.) use environment variables for configuration. Secrets stay out of version control. Same codebase runs in different environments with different values.

## Configuration Priority

### Layered Configuration

```java
// ✅ Configuration priority: env vars > properties > defaults
public class AppConfig {
  private final Properties properties;

  public AppConfig() throws IOException {
    properties = new Properties();

    // Load default properties
    try (InputStream input = getClass().getClassLoader()
           .getResourceAsStream("application.properties")) {
      if (input != null) {
        properties.load(input);
      }
    }
  }

  public String get(String key, String defaultValue) {
    // Priority 1: Environment variable
    String envValue = System.getenv(toEnvName(key));
    if (envValue != null) {
      return envValue;
    }

    // Priority 2: Properties file
    String propValue = properties.getProperty(key);
    if (propValue != null) {
      return propValue;
    }

    // Priority 3: Default value
    return defaultValue;
  }

  // Convert property name to environment variable name
  // database.url -> DATABASE_URL
  private String toEnvName(String propertyName) {
    return propertyName.toUpperCase().replace('.', '_');
  }
}

// Usage
AppConfig config = new AppConfig();
String dbUrl = config.get("database.url", "jdbc:postgresql://localhost:5432/db");
// Checks: DATABASE_URL (env) > database.url (props) > default
```

**Priority order rationale**:

1. **Environment variables** - Runtime configuration, highest priority
2. **Properties files** - Application defaults, middle priority
3. **Code defaults** - Fallback values, lowest priority

## Command-Line Arguments

### Parsing with Args

```java
// ✅ Simple argument parsing
public class CommandLineParser {

  public static void main(String[] args) {
    if (args.length < 2) {
      System.err.println("Usage: java App <input-file> <output-file>");
      System.exit(1);
    }

    String inputFile = args[0];
    String outputFile = args[1];

    processFiles(inputFile, outputFile);
  }
}

// ✅ Named arguments parser
public class ArgsParser {
  private final Map<String, String> arguments;

  public ArgsParser(String[] args) {
    arguments = new HashMap<>();

    for (int i = 0; i < args.length; i++) {
      if (args[i].startsWith("--")) {
        String key = args[i].substring(2);

        if (i + 1 < args.length && !args[i + 1].startsWith("--")) {
          arguments.put(key, args[i + 1]);
          i++; // Skip next arg
        } else {
          arguments.put(key, "true"); // Flag without value
        }
      }
    }
  }

  public String get(String key) {
    return arguments.get(key);
  }

  public String getOrDefault(String key, String defaultValue) {
    return arguments.getOrDefault(key, defaultValue);
  }

  public boolean has(String key) {
    return arguments.containsKey(key);
  }
}

// Usage
// java App --input data.txt --output result.txt --verbose
ArgsParser parser = new ArgsParser(args);
String input = parser.get("input");
String output = parser.get("output");
boolean verbose = parser.has("verbose");
```

### Using Apache Commons CLI

```java
import org.apache.commons.cli.*;

// ✅ Robust command-line parsing
public class CliExample {

  public static void main(String[] args) {
    Options options = new Options();

    options.addOption("i", "input", true, "Input file path");
    options.addOption("o", "output", true, "Output file path");
    options.addOption("v", "verbose", false, "Enable verbose output");
    options.addOption("h", "help", false, "Show help");

    CommandLineParser parser = new DefaultParser();
    HelpFormatter formatter = new HelpFormatter();

    try {
      CommandLine cmd = parser.parse(options, args);

      if (cmd.hasOption("help")) {
        formatter.printHelp("myapp", options);
        return;
      }

      String input = cmd.getOptionValue("input");
      String output = cmd.getOptionValue("output");
      boolean verbose = cmd.hasOption("verbose");

      if (input == null || output == null) {
        System.err.println("Input and output files required");
        formatter.printHelp("myapp", options);
        System.exit(1);
      }

      processFiles(input, output, verbose);

    } catch (ParseException e) {
      System.err.println("Parsing failed: " + e.getMessage());
      formatter.printHelp("myapp", options);
      System.exit(1);
    }
  }
}
```

## Configuration with TypeSafe Config

### Using TypeSafe Config Library

```java
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;

// ✅ Load configuration with TypeSafe Config
public class TypeSafeConfigExample {

  public static void main(String[] args) {
    Config config = ConfigFactory.load(); // Loads application.conf

    // Get simple values
    String dbUrl = config.getString("database.url");
    int maxConnections = config.getInt("database.maxConnections");
    boolean enableCache = config.getBoolean("cache.enabled");

    // Get lists
    List<String> allowedHosts = config.getStringList("server.allowedHosts");

    // Get nested objects
    Config databaseConfig = config.getConfig("database");
    String username = databaseConfig.getString("username");

    // Get with defaults
    int timeout = config.hasPath("api.timeout")
      ? config.getInt("api.timeout")
      : 30;
  }
}
```

### application.conf Format

```hocon
# TypeSafe Config format (HOCON)
database {
  url = "jdbc:postgresql://localhost:5432/mydb"
  username = "dbuser"
  password = ${DATABASE_PASSWORD} # From environment variable
  maxConnections = 20
}

server {
  port = 8080
  host = "0.0.0.0"
  allowedHosts = ["localhost", "example.com"]
}

cache {
  enabled = true
  ttl = 3600
}

api {
  timeout = 30
  retryAttempts = 3
}
```

## Configuration Profiles

### Environment-Specific Configuration

```java
// ✅ Load profile-specific properties
public class ProfileConfig {
  private final Properties properties;

  public ProfileConfig() throws IOException {
    String environment = System.getenv("APP_ENV");
    if (environment == null) {
      environment = "development";
    }

    properties = new Properties();

    // Load base properties
    loadProperties("application.properties");

    // Load environment-specific properties (overrides base)
    loadProperties("application-" + environment + ".properties");
  }

  private void loadProperties(String filename) throws IOException {
    try (InputStream input = getClass().getClassLoader()
           .getResourceAsStream(filename)) {
      if (input != null) {
        properties.load(input);
      }
    }
  }

  public String get(String key) {
    return properties.getProperty(key);
  }
}
```

**File structure:**

```
src/main/resources/
├── application.properties           # Base configuration
├── application-development.properties # Dev overrides
├── application-staging.properties   # Staging overrides
└── application-production.properties # Production overrides
```

## Security Best Practices

### Avoiding Hardcoded Secrets

```java
// ❌ Hardcoded secrets - NEVER do this
public class DatabaseConnection {
  private static final String PASSWORD = "mySecretPassword123"; // BAD!
}

// ✅ Secrets from environment variables
public class DatabaseConnection {
  private static String getPassword() {
    String password = System.getenv("DB_PASSWORD");
    if (password == null) {
      throw new IllegalStateException("DB_PASSWORD not set");
    }
    return password;
  }
}

// ✅ Secrets from external secret management
public class SecretManager {

  public static String getSecret(String secretName) {
    // Fetch from AWS Secrets Manager, HashiCorp Vault, etc.
    return fetchFromSecretStore(secretName);
  }
}
```

### .gitignore for Configuration

```gitignore
# ✅ Ignore environment-specific configs
application-local.properties
application-*.properties
!application.properties      # Keep base config

# Ignore environment files
.env
.env.local
.env.*.local

# Ignore sensitive data
secrets.properties
private-key.pem
```

### Environment File Template

```properties
# .env.template (committed to version control)
# Copy to .env and fill in values

DATABASE_URL=jdbc:postgresql://localhost:5432/mydb
DATABASE_USERNAME=
DATABASE_PASSWORD=

API_KEY=
API_SECRET=

# .env (NOT committed - in .gitignore)
DATABASE_URL=jdbc:postgresql://prod-db:5432/proddb
DATABASE_USERNAME=prod_user
DATABASE_PASSWORD=actual_secret_password

API_KEY=sk_live_abc123xyz
API_SECRET=secret_value_here
```

## Validation and Defaults

### Configuration Validation

```java
// ✅ Validate configuration on startup
public class ConfigValidator {
  private final AppConfig config;

  public ConfigValidator(AppConfig config) {
    this.config = config;
  }

  public void validate() {
    requireNotEmpty("database.url");
    requireNotEmpty("database.username");
    requireNotEmpty("database.password");

    validateRange("database.max.connections", 1, 100);
    validateRange("server.port", 1024, 65535);

    validateUrl("api.base.url");
  }

  private void requireNotEmpty(String key) {
    String value = config.get(key);
    if (value == null || value.isEmpty()) {
      throw new IllegalStateException(
        "Required configuration missing: " + key
      );
    }
  }

  private void validateRange(String key, int min, int max) {
    int value = Integer.parseInt(config.get(key));
    if (value < min || value > max) {
      throw new IllegalStateException(
        String.format("%s must be between %d and %d", key, min, max)
      );
    }
  }

  private void validateUrl(String key) {
    String value = config.get(key);
    try {
      new URL(value);
    } catch (MalformedURLException e) {
      throw new IllegalStateException(
        "Invalid URL for " + key + ": " + value
      );
    }
  }
}

// Usage
AppConfig config = new AppConfig();
ConfigValidator validator = new ConfigValidator(config);
validator.validate(); // Fails fast on startup if config invalid
```

### Type-Safe Configuration Class

```java
// ✅ Strongly-typed configuration
public class DatabaseConfig {
  private final String url;
  private final String username;
  private final String password;
  private final int maxConnections;
  private final int timeout;

  private DatabaseConfig(Builder builder) {
    this.url = Objects.requireNonNull(builder.url, "url required");
    this.username = Objects.requireNonNull(builder.username, "username required");
    this.password = Objects.requireNonNull(builder.password, "password required");

    if (builder.maxConnections < 1) {
      throw new IllegalArgumentException("maxConnections must be positive");
    }
    this.maxConnections = builder.maxConnections;

    if (builder.timeout < 0) {
      throw new IllegalArgumentException("timeout cannot be negative");
    }
    this.timeout = builder.timeout;
  }

  public String getUrl() { return url; }
  public String getUsername() { return username; }
  public String getPassword() { return password; }
  public int getMaxConnections() { return maxConnections; }
  public int getTimeout() { return timeout; }

  public static Builder builder() {
    return new Builder();
  }

  public static class Builder {
    private String url;
    private String username;
    private String password;
    private int maxConnections = 10; // Default
    private int timeout = 30; // Default

    public Builder url(String url) {
      this.url = url;
      return this;
    }

    public Builder username(String username) {
      this.username = username;
      return this;
    }

    public Builder password(String password) {
      this.password = password;
      return this;
    }

    public Builder maxConnections(int maxConnections) {
      this.maxConnections = maxConnections;
      return this;
    }

    public Builder timeout(int timeout) {
      this.timeout = timeout;
      return this;
    }

    public DatabaseConfig build() {
      return new DatabaseConfig(this);
    }
  }
}

// Usage
DatabaseConfig config = DatabaseConfig.builder()
  .url(System.getenv("DATABASE_URL"))
  .username(System.getenv("DATABASE_USERNAME"))
  .password(System.getenv("DATABASE_PASSWORD"))
  .maxConnections(20)
  .build(); // Validates on build
```

## Summary

Configuration management separates values from code, enabling different settings for different environments without recompilation. Properties files store application defaults, environment variables provide runtime configuration, and command-line arguments allow per-execution customization.

Properties files use key-value pairs loaded through Java's Properties class. Create type-safe accessors that handle missing values, default values, and type conversion. Organize properties logically with dot notation - `database.url`, `api.timeout`, `feature.enabled`.

Environment variables override properties files and keep secrets out of version control. Cloud platforms expect environment variables for configuration. Convert property names to environment variable names consistently - `database.url` becomes `DATABASE_URL`.

Configuration priority follows env vars > properties > defaults. Environment variables have highest priority for runtime overrides, properties files provide middle priority for application defaults, and code defaults catch missing values. This layering supports local development, testing, and production with the same codebase.

Command-line arguments enable per-execution configuration. Parse them manually for simple cases or use Apache Commons CLI for robust parsing with help messages and validation. Named arguments (--input file.txt) provide clearer interfaces than positional arguments.

Never hardcode secrets in source code. Use environment variables for secrets, keep secret-containing files in .gitignore, and commit template files showing required variables. Consider secret management services like AWS Secrets Manager or HashiCorp Vault for production systems.

Validate configuration on startup to fail fast. Check required values exist, validate ranges for numeric values, verify URL formats, and ensure security-sensitive values are present. Type-safe configuration classes with builders enforce validation at compile time and provide clear APIs.

Environment-specific configuration through profile-based property files enables dev/staging/production differences. Load base properties then override with environment-specific properties. This keeps common configuration DRY while allowing environment variations.

## Related Content

- [Java Best Practices and Design Principles](/en/learn/software-engineering/programming-language/java/explanation/best-practices)
- [How to Handle Files and Resources](/en/learn/software-engineering/programming-language/java/how-to/handle-files-and-resources)
- [How to Implement Proper Exception Handling](/en/learn/software-engineering/programming-language/java/how-to/exception-handling)
