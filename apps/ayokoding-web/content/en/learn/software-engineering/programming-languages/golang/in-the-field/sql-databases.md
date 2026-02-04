---
title: "SQL Databases"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Working with SQL databases using database/sql, prepared statements, connection pooling, and ORMs (GORM/sqlx)"
weight: 10000046
tags: ["golang", "sql", "database", "gorm", "sqlx", "postgresql", "mysql", "production"]
---

## Why SQL Database Integration Matters

SQL databases power most production applications. Go's `database/sql` package provides a vendor-neutral interface for SQL databases with connection pooling, prepared statements, and transaction support built-in. Understanding the standard library prevents common pitfalls like connection leaks, SQL injection, and N+1 queries.

**Core benefits**:

- **Vendor-neutral API**: Works with PostgreSQL, MySQL, SQLite, SQL Server via drivers
- **Built-in connection pooling**: Automatic connection reuse and management
- **Prepared statements**: SQL injection prevention and performance
- **Transaction support**: ACID guarantees for multi-statement operations

**Problem**: Many developers struggle with manual row scanning (verbose, error-prone), connection lifecycle management (leaks), or jump to ORMs without understanding database/sql fundamentals (masking performance issues).

**Solution**: Start with `database/sql` for direct control and understanding, recognize limitations (manual mapping, verbose queries), then introduce `sqlx` for convenience or `GORM` for complex domain models.

## Standard Library First: database/sql

Go's `database/sql` package provides database-independent SQL interface. Database drivers (PostgreSQL's `pgx`, MySQL's `go-sql-driver/mysql`) implement the interface.

**Basic connection pattern**:

```go
package main

import (
    "database/sql"
    // => Standard library for SQL databases
    // => Vendor-neutral interface
    // => Requires driver package

    _ "github.com/lib/pq"
    // => PostgreSQL driver
    // => Underscore import registers driver
    // => Imports for side effects (driver registration)
    // => Alternative drivers: pgx, go-sql-driver/mysql

    "fmt"
    "log"
)

func main() {
    dsn := "postgres://user:password@localhost:5432/dbname?sslmode=disable"
    // => Data Source Name (connection string)
    // => Format: postgres://username:password@host:port/database?options
    // => sslmode=disable for local development (production: require)

    db, err := sql.Open("postgres", dsn)
    // => sql.Open creates *sql.DB (connection pool)
    // => "postgres" is driver name (registered by lib/pq)
    // => Does NOT actually connect yet (lazy connection)
    // => db is connection pool, not single connection
    // => Safe for concurrent use

    if err != nil {
        log.Fatalf("Failed to open database: %v", err)
    }
    defer db.Close()
    // => db.Close closes connection pool
    // => Releases all connections
    // => Call when application shuts down

    if err := db.Ping(); err != nil {
        // => db.Ping verifies connection works
        // => Actually connects to database (first real connection)
        // => Returns error if database unreachable
        log.Fatalf("Failed to ping database: %v", err)
    }

    fmt.Println("Connected to database")
}
```

**Connection pool configuration**:

```go
package main

import (
    "database/sql"
    _ "github.com/lib/pq"
    "time"
    // => Standard library for duration
)

func setupDB(dsn string) (*sql.DB, error) {
    db, err := sql.Open("postgres", dsn)
    if err != nil {
        return nil, err
    }

    db.SetMaxOpenConns(25)
    // => SetMaxOpenConns limits total open connections
    // => Default: unlimited (dangerous for production)
    // => Recommended: 25-100 based on database capacity
    // => Prevents overwhelming database with connections

    db.SetMaxIdleConns(5)
    // => SetMaxIdleConns limits idle connections kept in pool
    // => Default: 2 (low for high-traffic apps)
    // => Idle connections ready for immediate use
    // => Too high: wastes database resources

    db.SetConnMaxLifetime(5 * time.Minute)
    // => SetConnMaxLifetime limits connection lifetime
    // => Closes connections after 5 minutes
    // => Prevents stale connections
    // => Recommended: 5-15 minutes

    db.SetConnMaxIdleTime(1 * time.Minute)
    // => SetConnMaxIdleTime closes idle connections after duration
    // => Frees connections not used in 1 minute
    // => Balances readiness vs resource usage

    if err := db.Ping(); err != nil {
        return nil, err
    }

    return db, nil
}
```

**Query pattern (single row)**:

```go
package main

import (
    "database/sql"
    _ "github.com/lib/pq"
    "fmt"
    "log"
)

type User struct {
    ID    int
    Name  string
    Email string
}

func getUserByID(db *sql.DB, id int) (*User, error) {
    // => Returns pointer to User and error

    query := "SELECT id, name, email FROM users WHERE id = $1"
    // => SQL query with placeholder
    // => $1 is PostgreSQL placeholder (MySQL uses ?)
    // => Placeholders prevent SQL injection
    // => Database driver escapes values

    var user User
    // => user is zero-value User

    err := db.QueryRow(query, id).Scan(&user.ID, &user.Name, &user.Email)
    // => db.QueryRow executes query expecting single row
    // => query is SQL statement
    // => id replaces $1 placeholder
    // => Returns *sql.Row (single row)
    // => .Scan extracts column values into variables
    // => &user.ID, &user.Name, &user.Email are destinations
    // => Column order must match SELECT order
    // => Returns sql.ErrNoRows if no match

    if err == sql.ErrNoRows {
        // => sql.ErrNoRows indicates query returned no rows
        // => Not an error in many cases (user not found)
        return nil, fmt.Errorf("user %d not found", id)
    } else if err != nil {
        // => Other errors (connection, syntax, type mismatch)
        return nil, fmt.Errorf("query failed: %w", err)
    }

    return &user, nil
}

func main() {
    db, err := setupDB("postgres://user:password@localhost/dbname?sslmode=disable")
    if err != nil {
        log.Fatal(err)
    }
    defer db.Close()

    user, err := getUserByID(db, 1)
    if err != nil {
        log.Printf("Error: %v", err)
        return
    }

    fmt.Printf("User: %+v\n", user)
    // => Output: User: {ID:1 Name:Alice Email:alice@example.com}
}
```

**Query pattern (multiple rows)**:

```go
package main

import (
    "database/sql"
    _ "github.com/lib/pq"
    "fmt"
)

func getUsers(db *sql.DB) ([]User, error) {
    query := "SELECT id, name, email FROM users ORDER BY id"

    rows, err := db.Query(query)
    // => db.Query executes query returning multiple rows
    // => rows is *sql.Rows (cursor over result set)
    // => Must close rows to release connection
    // => Holds connection until closed

    if err != nil {
        return nil, fmt.Errorf("query failed: %w", err)
    }
    defer rows.Close()
    // => rows.Close releases connection back to pool
    // => CRITICAL: forgetting this causes connection leaks
    // => defer ensures closure even if error occurs

    var users []User
    // => users accumulates query results

    for rows.Next() {
        // => rows.Next advances to next row
        // => Returns true if row available, false when done
        // => Must call before accessing row data

        var user User

        err := rows.Scan(&user.ID, &user.Name, &user.Email)
        // => rows.Scan extracts current row values
        // => Column order must match SELECT order
        if err != nil {
            return nil, fmt.Errorf("scan failed: %w", err)
        }

        users = append(users, user)
        // => Accumulate user into slice
    }

    if err := rows.Err(); err != nil {
        // => rows.Err checks for iteration errors
        // => Returns error if iteration incomplete (connection lost, etc.)
        // => Always check after loop
        return nil, fmt.Errorf("rows iteration failed: %w", err)
    }

    return users, nil
}
```

**Insert pattern**:

```go
package main

import (
    "database/sql"
    _ "github.com/lib/pq"
    "fmt"
)

func createUser(db *sql.DB, name, email string) (int, error) {
    // => Returns generated user ID and error

    query := `
        INSERT INTO users (name, email)
        VALUES ($1, $2)
        RETURNING id
    `
    // => INSERT statement with RETURNING clause
    // => RETURNING id returns generated ID (PostgreSQL feature)
    // => MySQL alternative: use db.Exec + result.LastInsertId()

    var id int
    err := db.QueryRow(query, name, email).Scan(&id)
    // => QueryRow executes INSERT
    // => name replaces $1, email replaces $2
    // => Scan extracts returned id
    // => Single statement (atomic)

    if err != nil {
        return 0, fmt.Errorf("insert failed: %w", err)
    }

    return id, nil
}
```

**Update pattern**:

```go
package main

import (
    "database/sql"
    _ "github.com/lib/pq"
    "fmt"
)

func updateUserEmail(db *sql.DB, id int, email string) error {
    query := "UPDATE users SET email = $1 WHERE id = $2"

    result, err := db.Exec(query, email, id)
    // => db.Exec executes statement not returning rows
    // => result is sql.Result (metadata about execution)
    // => Use for INSERT, UPDATE, DELETE without RETURNING

    if err != nil {
        return fmt.Errorf("update failed: %w", err)
    }

    rowsAffected, err := result.RowsAffected()
    // => RowsAffected returns number of rows modified
    // => 0 if no rows matched WHERE clause
    if err != nil {
        return fmt.Errorf("rows affected check failed: %w", err)
    }

    if rowsAffected == 0 {
        // => No rows affected: user not found
        return fmt.Errorf("user %d not found", id)
    }

    return nil
}
```

**Transaction pattern**:

```go
package main

import (
    "context"
    // => Standard library for context
    "database/sql"
    _ "github.com/lib/pq"
    "fmt"
)

func transferBalance(db *sql.DB, fromUserID, toUserID int, amount float64) error {
    // => Transfers balance between users (atomic operation)

    ctx := context.Background()
    // => ctx for cancellation (production: use request context)

    tx, err := db.BeginTx(ctx, nil)
    // => db.BeginTx starts transaction
    // => tx is *sql.Tx (transaction)
    // => nil means default isolation level
    // => Returns error if connection unavailable

    if err != nil {
        return fmt.Errorf("begin transaction: %w", err)
    }

    defer func() {
        // => Ensure transaction finalized

        if p := recover(); p != nil {
            // => Rollback on panic
            tx.Rollback()
            panic(p)
            // => Re-panic after rollback
        } else if err != nil {
            // => Rollback on error
            tx.Rollback()
        } else {
            // => Commit on success
            err = tx.Commit()
            // => tx.Commit finalizes transaction
            // => Changes visible to other connections
            // => Returns error if commit fails
        }
    }()

    // Deduct from sender
    _, err = tx.ExecContext(ctx, "UPDATE users SET balance = balance - $1 WHERE id = $2", amount, fromUserID)
    // => tx.ExecContext executes statement in transaction
    // => Changes not visible outside transaction until commit
    if err != nil {
        return fmt.Errorf("deduct balance: %w", err)
    }

    // Add to receiver
    _, err = tx.ExecContext(ctx, "UPDATE users SET balance = balance + $1 WHERE id = $2", amount, toUserID)
    if err != nil {
        return fmt.Errorf("add balance: %w", err)
    }

    return nil
    // => defer commits transaction if no error
}
```

**Prepared statements pattern**:

```go
package main

import (
    "database/sql"
    _ "github.com/lib/pq"
    "fmt"
)

func getUsersByIDsPrepared(db *sql.DB, ids []int) ([]User, error) {
    // => Prepared statement useful when executing same query multiple times

    stmt, err := db.Prepare("SELECT id, name, email FROM users WHERE id = $1")
    // => db.Prepare creates prepared statement
    // => stmt is *sql.Stmt (reusable query)
    // => Statement parsed once, executed multiple times
    // => Performance benefit for repeated queries

    if err != nil {
        return nil, fmt.Errorf("prepare failed: %w", err)
    }
    defer stmt.Close()
    // => stmt.Close releases statement resources
    // => Must close when done

    var users []User

    for _, id := range ids {
        var user User

        err := stmt.QueryRow(id).Scan(&user.ID, &user.Name, &user.Email)
        // => stmt.QueryRow executes prepared statement
        // => Reuses parsed statement (no re-parsing)

        if err == sql.ErrNoRows {
            continue
            // => Skip not found users
        } else if err != nil {
            return nil, fmt.Errorf("query failed: %w", err)
        }

        users = append(users, user)
    }

    return users, nil
}
```

**Limitations for production**:

- **Manual row scanning**: Verbose `Scan` calls for each query (error-prone)
- **No query builder**: String concatenation for dynamic queries (dangerous)
- **No automatic mapping**: Cannot scan into struct directly
- **No relationship loading**: Must manually JOIN or execute multiple queries
- **No caching**: Every query hits database
- **Verbose error handling**: Manual error check for every operation

## Production Enhancement: Progression Strategy

For production applications, choose based on complexity and team preferences:

1. **database/sql** → Simple CRUD (1-5 tables, direct control priority)
2. **sqlx** → Moderate complexity (5-20 tables, convenience without ORM)
3. **GORM** → Complex domains (20+ tables, relationships, migrations)

### sqlx: Extension with Struct Scanning

`sqlx` extends `database/sql` with struct scanning, named parameters, and convenience methods. Compatible with standard library (use `sqlx.DB` as drop-in replacement for `sql.DB`).

**Installing sqlx**:

```bash
go get github.com/jmoiron/sqlx
# => Downloads sqlx package
# => Compatible with database/sql drivers
```

**sqlx query pattern**:

```go
package main

import (
    "github.com/jmoiron/sqlx"
    // => sqlx package (extends database/sql)
    _ "github.com/lib/pq"
    "fmt"
)

type User struct {
    ID    int    `db:"id"`
    // => db:"id" maps to database column
    // => Similar to json tags
    Name  string `db:"name"`
    Email string `db:"email"`
}

func getUserByIDSqlx(db *sqlx.DB, id int) (*User, error) {
    var user User

    err := db.Get(&user, "SELECT id, name, email FROM users WHERE id = $1", id)
    // => db.Get executes query and scans into struct
    // => &user is destination struct
    // => Automatically maps columns to struct fields using db tags
    // => More concise than manual Scan
    // => Returns sql.ErrNoRows if not found

    if err != nil {
        return nil, fmt.Errorf("query failed: %w", err)
    }

    return &user, nil
}

func getUsersSqlx(db *sqlx.DB) ([]User, error) {
    var users []User

    err := db.Select(&users, "SELECT id, name, email FROM users ORDER BY id")
    // => db.Select executes query and scans into slice
    // => &users is destination slice
    // => Automatically scans all rows into slice
    // => More concise than manual loop + Scan

    if err != nil {
        return nil, fmt.Errorf("query failed: %w", err)
    }

    return users, nil
}

func main() {
    db, err := sqlx.Connect("postgres", "postgres://user:password@localhost/dbname?sslmode=disable")
    // => sqlx.Connect is sqlx.Open + Ping
    // => Returns *sqlx.DB (wrapper around *sql.DB)

    if err != nil {
        panic(err)
    }
    defer db.Close()

    user, _ := getUserByIDSqlx(db, 1)
    fmt.Printf("User: %+v\n", user)

    users, _ := getUsersSqlx(db)
    fmt.Printf("Users: %d\n", len(users))
}
```

**sqlx named queries**:

```go
package main

import (
    "github.com/jmoiron/sqlx"
    _ "github.com/lib/pq"
)

func createUserNamed(db *sqlx.DB, user User) error {
    query := `
        INSERT INTO users (name, email)
        VALUES (:name, :email)
        RETURNING id
    `
    // => Named parameters (:name, :email)
    // => Map to struct fields via db tags

    rows, err := db.NamedQuery(query, user)
    // => db.NamedQuery executes query with struct
    // => user.Name replaces :name
    // => user.Email replaces :email
    // => More readable than positional parameters

    if err != nil {
        return err
    }
    defer rows.Close()

    if rows.Next() {
        rows.Scan(&user.ID)
        // => Extract returned ID
    }

    return rows.Err()
}
```

### GORM: Full-Featured ORM

GORM is a full-featured ORM with associations, migrations, hooks, and query builder. Use for complex domains with relationships.

**Installing GORM**:

```bash
go get -u gorm.io/gorm
go get -u gorm.io/driver/postgres
# => GORM core + PostgreSQL driver
```

**GORM basics**:

```go
package main

import (
    "gorm.io/driver/postgres"
    // => GORM PostgreSQL driver
    "gorm.io/gorm"
    // => GORM core
    "fmt"
)

type User struct {
    ID    uint   `gorm:"primaryKey"`
    // => gorm:"primaryKey" marks primary key
    // => uint is GORM convention for auto-increment IDs

    Name  string `gorm:"size:100;not null"`
    // => size:100 sets VARCHAR(100)
    // => not null adds NOT NULL constraint

    Email string `gorm:"uniqueIndex;not null"`
    // => uniqueIndex adds unique index
}

func main() {
    dsn := "host=localhost user=user password=password dbname=dbname port=5432 sslmode=disable"

    db, err := gorm.Open(postgres.Open(dsn), &gorm.Config{})
    // => gorm.Open connects to database
    // => Returns *gorm.DB (GORM database handle)

    if err != nil {
        panic(err)
    }

    // Auto-migrate schema
    db.AutoMigrate(&User{})
    // => db.AutoMigrate creates/updates table
    // => Inspects User struct, generates SQL
    // => Creates users table with columns
    // => Non-destructive (adds columns, doesn't drop)

    // Create user
    user := User{Name: "Alice", Email: "alice@example.com"}
    result := db.Create(&user)
    // => db.Create inserts record
    // => user.ID populated with generated ID
    // => result.Error contains error if any

    if result.Error != nil {
        fmt.Printf("Create failed: %v\n", result.Error)
    } else {
        fmt.Printf("Created user ID: %d\n", user.ID)
    }

    // Find by ID
    var foundUser User
    db.First(&foundUser, user.ID)
    // => db.First finds first record matching condition
    // => foundUser populated with database values
    // => Second arg is primary key value

    fmt.Printf("Found: %+v\n", foundUser)

    // Query all users
    var users []User
    db.Find(&users)
    // => db.Find retrieves all records
    // => users slice populated

    fmt.Printf("Total users: %d\n", len(users))

    // Update
    db.Model(&user).Update("Email", "newemail@example.com")
    // => db.Model specifies record to update
    // => Update changes single field
    // => Updates database immediately

    // Delete
    db.Delete(&user)
    // => db.Delete removes record
    // => Soft delete if DeletedAt field exists
}
```

**GORM associations**:

```go
type User struct {
    ID    uint
    Name  string
    Posts []Post `gorm:"foreignKey:UserID"`
    // => One-to-many relationship
    // => Posts is slice (multiple posts)
    // => foreignKey:UserID specifies foreign key column
}

type Post struct {
    ID     uint
    Title  string
    UserID uint
    // => Foreign key referencing User.ID
    User   User `gorm:"constraint:OnDelete:CASCADE;"`
    // => Belongs-to relationship
    // => OnDelete:CASCADE deletes posts when user deleted
}

func createUserWithPosts(db *gorm.DB) {
    user := User{
        Name: "Alice",
        Posts: []Post{
            {Title: "First Post"},
            {Title: "Second Post"},
        },
    }

    db.Create(&user)
    // => Creates user and associated posts
    // => Single call creates multiple records
    // => Foreign keys set automatically
}

func getUserWithPosts(db *gorm.DB, id uint) (*User, error) {
    var user User

    err := db.Preload("Posts").First(&user, id).Error
    // => db.Preload("Posts") eager loads association
    // => Executes separate query for posts
    // => user.Posts populated
    // => Without Preload, Posts is empty

    return &user, err
}
```

## Trade-offs Comparison

| Aspect              | database/sql              | sqlx                    | GORM                               |
| ------------------- | ------------------------- | ----------------------- | ---------------------------------- |
| **Dependencies**    | Driver only               | sqlx + driver           | GORM + driver                      |
| **Performance**     | Highest (direct)          | High (minimal overhead) | Medium (ORM overhead)              |
| **Boilerplate**     | High (manual Scan)        | Low (struct scanning)   | Very Low (automatic)               |
| **Query Building**  | String concatenation      | String concatenation    | Query builder API                  |
| **Relationships**   | Manual JOINs              | Manual JOINs            | Automatic (Preload)                |
| **Migrations**      | External tool             | External tool           | Built-in (AutoMigrate)             |
| **Type Safety**     | Manual                    | Struct tags             | Struct tags + validation           |
| **Learning Curve**  | Medium                    | Low (extends sql)       | Medium-High (ORM concepts)         |
| **N+1 Query Risk**  | Manual control            | Manual control          | High (must use Preload)            |
| **Transaction API** | Explicit (BeginTx)        | Explicit (Beginx)       | Simplified (Transaction)           |
| **When to Use**     | Simple CRUD, full control | Moderate complexity     | Complex domains, rapid development |

## Best Practices

**database/sql best practices**:

1. **Configure connection pool**: Set MaxOpenConns, MaxIdleConns, ConnMaxLifetime
2. **Always defer rows.Close()**: Prevents connection leaks
3. **Check rows.Err()**: After iteration loop
4. **Use QueryRow for single row**: More efficient than Query
5. **Use Exec for non-SELECT**: INSERT/UPDATE/DELETE without RETURNING
6. **Use transactions for multi-statement**: Ensures atomicity
7. **Use prepared statements**: For repeated queries (performance)
8. **Never concatenate SQL**: Use placeholders to prevent injection

**sqlx best practices**:

1. **Use Get for single row**: More concise than QueryRow
2. **Use Select for multiple rows**: Automatically scans into slice
3. **Define db tags**: Map struct fields to column names
4. **Use NamedQuery**: For complex queries with many parameters

**GORM best practices**:

1. **Use Preload carefully**: Prevent N+1 queries
2. **Use Select("field1, field2")**: Avoid loading unnecessary columns
3. **Enable query logging**: Debug slow queries
4. **Use transactions explicitly**: For multi-model operations
5. **Avoid AutoMigrate in production**: Use versioned migrations
6. **Index foreign keys**: Improve join performance
7. **Use batch operations**: `CreateInBatches` for bulk inserts

**General SQL best practices**:

1. **Index frequently queried columns**: WHERE, JOIN, ORDER BY columns
2. **Limit result sets**: Use LIMIT for pagination
3. **Use connection pooling**: Reuse connections
4. **Monitor slow queries**: Log queries >100ms
5. **Use read replicas**: Distribute read load
6. **Implement retry logic**: Handle transient failures
7. **Validate input**: Before constructing queries
8. **Use context for timeouts**: Prevent hanging queries
