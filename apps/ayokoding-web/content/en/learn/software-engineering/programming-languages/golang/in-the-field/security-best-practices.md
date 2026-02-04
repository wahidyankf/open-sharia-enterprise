---
title: "Security Best Practices"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Security best practices for Go: input validation, SQL injection prevention, password hashing, secrets management"
weight: 10000068
tags: ["golang", "security", "validation", "sql-injection", "password-hashing", "secrets", "production"]
---

## Why Security Best Practices Matter

Security vulnerabilities in production systems lead to data breaches, financial losses, and regulatory violations. Understanding input validation, SQL injection prevention, password hashing, and secrets management protects user data, prevents unauthorized access, and ensures compliance with security standards (PCI-DSS, GDPR, HIPAA).

**Core benefits**:

- **Data protection**: Prevents unauthorized data access
- **Injection prevention**: Blocks SQL injection, XSS, command injection
- **Credential security**: Protects passwords with strong hashing
- **Compliance**: Meets regulatory security requirements

**Problem**: Standard library provides basic tools (crypto/bcrypt, database/sql) but no comprehensive validation framework or secrets management. Manual implementation is error-prone and insecure.

**Solution**: Start with standard library patterns (prepared statements, bcrypt hashing) to understand fundamentals, then use validation libraries (go-playground/validator) and secrets management tools (environment variables, Vault) for production security.

## Standard Library: Input Validation

Go standard library provides basic validation through type system and manual checks.

**Pattern from standard library**:

```go
package main

import (
    "errors"
    // => Standard library for error creation
    "fmt"
    // => Standard library for formatted output
    "net/mail"
    // => Standard library for email validation
    // => RFC 5322 compliant parsing
    "strings"
    // => String manipulation
    "unicode"
    // => Unicode character classification
)

type User struct {
    Username string
    // => Username field (must be validated)
    Email    string
    // => Email field (must be valid email format)
    Age      int
    // => Age field (must be positive)
}

func validateUsername(username string) error {
    // => Validates username requirements
    // => Returns error if validation fails

    if len(username) < 3 {
        // => Username too short

        return errors.New("username must be at least 3 characters")
    }

    if len(username) > 20 {
        // => Username too long

        return errors.New("username must be at most 20 characters")
    }

    for _, r := range username {
        // => Iterate over runes (Unicode code points)
        // => r is rune (int32)

        if !unicode.IsLetter(r) && !unicode.IsDigit(r) && r != '_' {
            // => Allow letters, digits, underscore only
            // => Reject special characters

            return fmt.Errorf("username contains invalid character: %c", r)
        }
    }

    return nil
    // => Validation passed
}

func validateEmail(email string) error {
    // => Validates email format
    // => Uses RFC 5322 parsing

    if email == "" {
        // => Empty email

        return errors.New("email cannot be empty")
    }

    _, err := mail.ParseAddress(email)
    // => ParseAddress validates RFC 5322 format
    // => Returns error if invalid format
    // => Checks: local@domain.tld structure

    if err != nil {
        return errors.New("invalid email format")
    }

    return nil
}

func validateAge(age int) error {
    // => Validates age constraints

    if age < 0 {
        // => Negative age invalid

        return errors.New("age cannot be negative")
    }

    if age < 13 {
        // => COPPA compliance (under 13 requires parental consent)

        return errors.New("user must be at least 13 years old")
    }

    if age > 120 {
        // => Unrealistic age

        return errors.New("age must be at most 120")
    }

    return nil
}

func validateUser(user User) error {
    // => Validates all user fields
    // => Returns first validation error

    if err := validateUsername(user.Username); err != nil {
        // => Username validation failed

        return fmt.Errorf("username: %w", err)
        // => Wrap with field context
    }

    if err := validateEmail(user.Email); err != nil {
        // => Email validation failed

        return fmt.Errorf("email: %w", err)
    }

    if err := validateAge(user.Age); err != nil {
        // => Age validation failed

        return fmt.Errorf("age: %w", err)
    }

    return nil
    // => All validations passed
}

func main() {
    // Test valid user
    validUser := User{
        Username: "john_doe",
        Email:    "john@example.com",
        Age:      25,
    }

    if err := validateUser(validUser); err != nil {
        // => Validation failed

        fmt.Println("Validation error:", err)
    } else {
        fmt.Println("Valid user!")
        // => Output: Valid user!
    }

    // Test invalid user
    invalidUser := User{
        Username: "ab",           // Too short
        Email:    "invalid-email",
        Age:      -5,
    }

    if err := validateUser(invalidUser); err != nil {
        fmt.Println("Validation error:", err)
        // => Output: Validation error: username: username must be at least 3 characters
        // => First error returned
    }
}
```

**Limitations for production validation**:

- Verbose (manual validation for each field)
- No validation tags (declarative validation)
- No custom validators (reusable validation logic)
- No error aggregation (returns first error only)
- No struct field validation (must write boilerplate)

## Production Framework: go-playground/validator

go-playground/validator provides declarative validation with struct tags.

**Adding validator**:

```bash
go get github.com/go-playground/validator/v10
# => Installs validation library
# => v10 latest version (supports Go 1.18+ generics)
```

**Pattern: Declarative Validation**:

```go
package main

import (
    "fmt"
    // => Standard library for formatted output

    "github.com/go-playground/validator/v10"
    // => Validation library with struct tags
    // => v10 supports custom validators, localization
)

type User struct {
    Username string `validate:"required,min=3,max=20,alphanum"`
    // => required: cannot be empty
    // => min=3: minimum 3 characters
    // => max=20: maximum 20 characters
    // => alphanum: letters and numbers only

    Email    string `validate:"required,email"`
    // => required: cannot be empty
    // => email: valid RFC 5322 email format

    Age      int    `validate:"required,min=13,max=120"`
    // => required: must be provided
    // => min=13: COPPA compliance
    // => max=120: realistic age limit

    Password string `validate:"required,min=8,containsany=!@#$%^&*"`
    // => required: cannot be empty
    // => min=8: minimum password length
    // => containsany: must contain special character
}

var validate *validator.Validate
// => Global validator instance
// => Reuse across application (thread-safe)

func init() {
    // => init() runs before main()
    // => Initializes validator

    validate = validator.New()
    // => Creates new validator instance
    // => Caches struct validation rules
}

func validateUser(user User) error {
    // => Validates user with struct tags
    // => Returns validation errors

    err := validate.Struct(user)
    // => Struct() validates all fields with tags
    // => err is nil if validation passes
    // => err contains all validation errors

    if err != nil {
        // => Validation failed

        for _, err := range err.(validator.ValidationErrors) {
            // => Iterate over validation errors
            // => err is validator.FieldError

            fmt.Printf("Field: %s, Error: %s, Tag: %s\n",
                err.Field(),     // => Field name (Username, Email, Age)
                err.Error(),     // => Error message
                err.Tag())       // => Failed tag (required, email, min)
            // => Output: Field: Username, Error: Field validation for 'Username' failed on the 'min' tag, Tag: min
        }

        return err
    }

    return nil
}

func main() {
    // Valid user
    validUser := User{
        Username: "john_doe123",
        Email:    "john@example.com",
        Age:      25,
        Password: "pass@1234",  // Contains special char
    }

    if err := validateUser(validUser); err == nil {
        fmt.Println("Valid user!")
        // => Output: Valid user!
    }

    // Invalid user (multiple errors)
    invalidUser := User{
        Username: "ab",               // Too short
        Email:    "invalid",          // Invalid email
        Age:      10,                 // Under 13
        Password: "pass",             // Too short, no special char
    }

    validateUser(invalidUser)
    // => Output: Multiple validation errors
    // => Field: Username, Error: ..., Tag: min
    // => Field: Email, Error: ..., Tag: email
    // => Field: Age, Error: ..., Tag: min
    // => Field: Password, Error: ..., Tag: min
}
```

**Pattern: Custom Validators**:

```go
package main

import (
    "fmt"
    "strings"

    "github.com/go-playground/validator/v10"
)

type User struct {
    Username string `validate:"required,no_profanity"`
    // => no_profanity is custom validator
    // => Checks for banned words
}

func validateNoProfanity(fl validator.FieldLevel) bool {
    // => Custom validation function
    // => fl.Field() is reflect.Value of field
    // => Returns true if validation passes

    value := fl.Field().String()
    // => value is field value (username)

    bannedWords := []string{"badword1", "badword2"}
    // => List of banned words
    // => Production: load from database or config

    for _, banned := range bannedWords {
        // => Check each banned word

        if strings.Contains(strings.ToLower(value), banned) {
            // => Username contains banned word
            // => Case-insensitive check

            return false
            // => Validation failed
        }
    }

    return true
    // => Validation passed
}

func main() {
    validate := validator.New()

    validate.RegisterValidation("no_profanity", validateNoProfanity)
    // => Registers custom validator
    // => "no_profanity" is tag name
    // => validateNoProfanity is validation function

    user := User{
        Username: "john_badword1",
        // => Contains banned word
    }

    err := validate.Struct(user)
    if err != nil {
        fmt.Println("Validation failed:", err)
        // => Output: Validation failed: Key: 'User.Username' Error:Field validation for 'Username' failed on the 'no_profanity' tag
    }
}
```

## SQL Injection Prevention

SQL injection occurs when user input is concatenated into SQL queries. Prepared statements prevent injection by separating SQL structure from data.

**Pattern: Prepared Statements (Standard Library)**:

```go
package main

import (
    "database/sql"
    // => Standard library for SQL databases
    "fmt"

    _ "github.com/lib/pq"
    // => PostgreSQL driver (blank import registers driver)
)

func searchUsersUnsafe(db *sql.DB, searchTerm string) error {
    // => VULNERABLE: SQL injection possible
    // => DO NOT USE IN PRODUCTION

    query := fmt.Sprintf("SELECT * FROM users WHERE username = '%s'", searchTerm)
    // => query is "SELECT * FROM users WHERE username = 'john'"
    // => DANGER: searchTerm not escaped
    // => Attacker input: "john' OR '1'='1"
    // => Result: SELECT * FROM users WHERE username = 'john' OR '1'='1'
    // => Returns ALL users (authentication bypass)

    rows, err := db.Query(query)
    // => Executes concatenated query
    // => SQL injection vulnerability
    defer rows.Close()

    return err
}

func searchUsersSafe(db *sql.DB, searchTerm string) error {
    // => SAFE: uses prepared statement
    // => Production pattern

    query := "SELECT * FROM users WHERE username = $1"
    // => $1 is placeholder for parameter
    // => SQL structure fixed (cannot be modified by input)
    // => Database separates structure from data

    rows, err := db.Query(query, searchTerm)
    // => searchTerm bound to $1 parameter
    // => Database escapes special characters
    // => Attacker input: "john' OR '1'='1"
    // => Treated as literal string (searches for that exact username)
    // => SQL injection impossible

    defer rows.Close()

    return err
}

func main() {
    db, _ := sql.Open("postgres", "postgres://user:pass@localhost/db")
    defer db.Close()

    // Vulnerable query (DO NOT USE)
    searchUsersUnsafe(db, "john' OR '1'='1")
    // => SQL injection: returns all users

    // Safe query (USE THIS)
    searchUsersSafe(db, "john' OR '1'='1")
    // => No injection: searches for literal string
}
```

**Key principle**: NEVER concatenate user input into SQL queries. ALWAYS use prepared statements ($1, $2, etc. for PostgreSQL, ? for MySQL).

## Password Hashing

Passwords must NEVER be stored in plaintext. Use bcrypt for secure password hashing.

**Pattern: bcrypt Hashing (Standard Library)**:

```go
package main

import (
    "fmt"
    // => Standard library for formatted output

    "golang.org/x/crypto/bcrypt"
    // => bcrypt hashing algorithm
    // => Part of Go extended libraries (golang.org/x)
    // => Standard for password hashing
)

func hashPassword(password string) (string, error) {
    // => Hashes password with bcrypt
    // => Returns hashed password string

    hash, err := bcrypt.GenerateFromPassword([]byte(password), bcrypt.DefaultCost)
    // => bcrypt.DefaultCost is 10 (2^10 = 1024 iterations)
    // => Higher cost = more secure but slower
    // => hash is []byte (convert to string for storage)
    // => Includes salt (random data to prevent rainbow tables)
    // => Same password produces different hashes (due to salt)

    if err != nil {
        return "", err
    }

    return string(hash), nil
    // => hash is "$2a$10$N9qo8uLOickgx2ZMRZoMye..."
    // => $2a: bcrypt identifier
    // => $10: cost factor
    // => Next 22 chars: salt
    // => Remaining: hash
}

func checkPassword(password, hash string) bool {
    // => Validates password against stored hash
    // => Returns true if password matches

    err := bcrypt.CompareHashAndPassword([]byte(hash), []byte(password))
    // => CompareHashAndPassword extracts salt from hash
    // => Hashes password with same salt
    // => Compares result with stored hash
    // => Returns nil if match, error if mismatch

    return err == nil
    // => true if password matches
    // => false if password doesn't match
}

func main() {
    password := "mySecurePass123!"

    // Hash password during registration
    hash, err := hashPassword(password)
    // => hash is "$2a$10$..."
    // => Store hash in database (not password)

    if err != nil {
        fmt.Println("Hashing error:", err)
        return
    }

    fmt.Println("Password hash:", hash)
    // => Output: Password hash: $2a$10$N9qo8uLOickgx2ZMRZoMye...

    // Validate password during login
    isValid := checkPassword("mySecurePass123!", hash)
    fmt.Println("Password valid:", isValid)
    // => Output: Password valid: true

    isValid = checkPassword("wrongPassword", hash)
    fmt.Println("Wrong password:", isValid)
    // => Output: Wrong password: false
}
```

**Why bcrypt**:

- Slow by design (prevents brute-force attacks)
- Includes salt (prevents rainbow table attacks)
- Adaptive cost (increase cost over time as hardware improves)
- Industry standard (used by major platforms)

**Pattern: Argon2 (Alternative to bcrypt)**:

```go
package main

import (
    "crypto/rand"
    // => Cryptographically secure random number generator
    "encoding/base64"
    // => Base64 encoding for hash storage
    "fmt"

    "golang.org/x/crypto/argon2"
    // => Argon2 hashing algorithm
    // => Winner of Password Hashing Competition (2015)
    // => More secure than bcrypt (memory-hard)
)

func hashPasswordArgon2(password string) (string, error) {
    // => Hashes password with Argon2id
    // => Argon2id combines Argon2i and Argon2d

    salt := make([]byte, 16)
    // => 16-byte salt (128 bits)

    _, err := rand.Read(salt)
    // => Fill salt with cryptographically secure random bytes
    // => rand.Read from crypto/rand (not math/rand)

    if err != nil {
        return "", err
    }

    hash := argon2.IDKey([]byte(password), salt, 1, 64*1024, 4, 32)
    // => argon2.IDKey parameters:
    // => password: password to hash
    // => salt: random salt (16 bytes)
    // => time: 1 iteration (adjust based on security requirements)
    // => memory: 64 MB (64*1024 KB)
    // => threads: 4 parallel threads
    // => keyLen: 32 bytes output (256 bits)
    // => hash is 32-byte []byte

    encoded := base64.StdEncoding.EncodeToString(append(salt, hash...))
    // => Combine salt and hash for storage
    // => First 16 bytes: salt
    // => Next 32 bytes: hash
    // => Encode as base64 string

    return encoded, nil
    // => encoded is base64 string (store in database)
}

func checkPasswordArgon2(password, encoded string) (bool, error) {
    // => Validates password against Argon2 hash

    decoded, err := base64.StdEncoding.DecodeString(encoded)
    // => Decode base64 to bytes
    // => decoded contains salt + hash

    if err != nil {
        return false, err
    }

    salt := decoded[:16]
    // => Extract salt (first 16 bytes)

    hash := decoded[16:]
    // => Extract hash (remaining 32 bytes)

    newHash := argon2.IDKey([]byte(password), salt, 1, 64*1024, 4, 32)
    // => Hash password with same parameters and salt
    // => newHash should match stored hash if password correct

    if string(newHash) != string(hash) {
        // => Hashes don't match

        return false, nil
    }

    return true, nil
    // => Password matches
}

func main() {
    password := "mySecurePass123!"

    hash, _ := hashPasswordArgon2(password)
    fmt.Println("Argon2 hash:", hash)

    isValid, _ := checkPasswordArgon2(password, hash)
    fmt.Println("Password valid:", isValid)
    // => Output: Password valid: true
}
```

## Secrets Management

Secrets (API keys, database passwords) must NEVER be hardcoded. Use environment variables or secret managers.

**Pattern: Environment Variables**:

```go
package main

import (
    "fmt"
    "os"
    // => Standard library for environment variables
)

type Config struct {
    DatabaseURL string
    // => Database connection string
    APIKey      string
    // => Third-party API key
    JWTSecret   string
    // => JWT signing secret
}

func loadConfig() (*Config, error) {
    // => Loads configuration from environment variables
    // => Fails if required secrets missing

    dbURL := os.Getenv("DATABASE_URL")
    // => DATABASE_URL environment variable
    // => Example: postgres://user:pass@localhost/db

    if dbURL == "" {
        // => Required secret missing

        return nil, fmt.Errorf("DATABASE_URL not set")
    }

    apiKey := os.Getenv("API_KEY")
    if apiKey == "" {
        return nil, fmt.Errorf("API_KEY not set")
    }

    jwtSecret := os.Getenv("JWT_SECRET")
    if jwtSecret == "" {
        return nil, fmt.Errorf("JWT_SECRET not set")
    }

    return &Config{
        DatabaseURL: dbURL,
        APIKey:      apiKey,
        JWTSecret:   jwtSecret,
    }, nil
}

func main() {
    config, err := loadConfig()
    // => Loads secrets from environment

    if err != nil {
        fmt.Println("Configuration error:", err)
        return
    }

    fmt.Println("Config loaded successfully")
    // => Secrets loaded, never logged
}
```

**Setting environment variables**:

```bash
# .env file (for development, add to .gitignore)
DATABASE_URL=postgres://user:pass@localhost/db
API_KEY=sk_test_123456789
JWT_SECRET=super-secret-random-string-256-bits

# Load environment variables
export $(cat .env | xargs)

# Run application
go run main.go
```

## Production Best Practices

**Use govulncheck for vulnerability scanning**:

```bash
go install golang.org/x/vuln/cmd/govulncheck@latest
# => Installs vulnerability scanner

govulncheck ./...
# => Scans for known vulnerabilities in dependencies
# => Reports CVEs affecting your code
```

**Use gosec for static analysis**:

```bash
go install github.com/securego/gosec/v2/cmd/gosec@latest
# => Installs security linter

gosec ./...
# => Scans for security issues
# => Detects: hardcoded secrets, weak crypto, SQL injection
```

**Sanitize user input before logging**:

```go
// GOOD: sanitize sensitive data
import "regexp"

func sanitizeEmail(email string) string {
    re := regexp.MustCompile(`(@.+)`)
    return re.ReplaceAllString(email, "@***")
}

log.Printf("User login attempt: %s", sanitizeEmail(email))
// => Output: User login attempt: john@***

// BAD: log sensitive data
log.Printf("User login: %s, password: %s", email, password)
// => NEVER log passwords
```

## Summary

Security best practices protect production systems from vulnerabilities. Standard library provides basic tools: manual input validation, prepared statements for SQL injection prevention, bcrypt for password hashing. Production systems use go-playground/validator for declarative validation, govulncheck for vulnerability scanning, and environment variables for secrets management. Always use prepared statements, never store plaintext passwords, scan for vulnerabilities regularly, and load secrets from environment variables.

**Key takeaways**:

- Use prepared statements ($1, $2) to prevent SQL injection
- Hash passwords with bcrypt (10+ cost factor)
- Validate input with go-playground/validator (declarative tags)
- Load secrets from environment variables (never hardcode)
- Scan for vulnerabilities with govulncheck and gosec
- Sanitize sensitive data before logging
- Use Argon2 for memory-hard hashing (more secure than bcrypt)
