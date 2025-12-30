---
title: Database Integration
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000012
description: Practical guide to integrating databases in Rust applications
tags: ["rust", "how-to", "database", "sqlx", "diesel", "orm", "sql", "postgres", "sqlite"]
---

**Need to integrate a database in your Rust application?** This guide covers async database queries with SQLx, ORM with Diesel, migrations, connection pooling, and transactions.

## Problem: Connecting to a Database

### Scenario

You need to connect to a PostgreSQL, MySQL, or SQLite database.

### Solution: Use SQLx

```toml
[dependencies]
sqlx = { version = "0.7", features = ["runtime-tokio-rustls", "postgres"] }
tokio = { version = "1.0", features = ["full"] }
```

```rust
use sqlx::postgres::PgPool;

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let pool = PgPool::connect("postgres://user:pass@localhost/database").await?;

    println!("Connected to database");

    Ok(())
}
```

**SQLite**:

```toml
sqlx = { version = "0.7", features = ["runtime-tokio-rustls", "sqlite"] }
```

```rust
use sqlx::sqlite::SqlitePool;

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let pool = SqlitePool::connect("sqlite:database.db").await?;
    Ok(())
}
```

---

## Problem: Running Queries

### Scenario

You need to execute SQL queries and fetch results.

### Solution: Use query! Macro for Type-Safe Queries

```rust
use sqlx::PgPool;

#[derive(Debug)]
struct User {
    id: i32,
    name: String,
    email: String,
}

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let pool = PgPool::connect("postgres://user:pass@localhost/database").await?;

    // Compile-time checked query
    let users = sqlx::query_as!(
        User,
        "SELECT id, name, email FROM users"
    )
    .fetch_all(&pool)
    .await?;

    for user in users {
        println!("{:?}", user);
    }

    Ok(())
}
```

**Manual mapping with query_as**:

```rust
use sqlx::FromRow;

#[derive(Debug, FromRow)]
struct User {
    id: i32,
    name: String,
    email: String,
}

let users = sqlx::query_as::<_, User>("SELECT id, name, email FROM users")
    .fetch_all(&pool)
    .await?;
```

---

## Problem: Inserting Data

### Scenario

You need to insert records into the database.

### Solution: Use Parameterized Queries

```rust
use sqlx::PgPool;

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let pool = PgPool::connect("postgres://user:pass@localhost/database").await?;

    let name = "Alice";
    let email = "alice@example.com";

    let result = sqlx::query!(
        "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id",
        name,
        email
    )
    .fetch_one(&pool)
    .await?;

    println!("Inserted user with ID: {}", result.id);

    Ok(())
}
```

---

## Problem: Updating and Deleting

### Scenario

You need to modify or remove data.

### Solution: Use UPDATE and DELETE Queries

**Update**:

```rust
let updated = sqlx::query!(
    "UPDATE users SET email = $1 WHERE id = $2",
    "newemail@example.com",
    1
)
.execute(&pool)
.await?;

println!("Updated {} rows", updated.rows_affected());
```

**Delete**:

```rust
let deleted = sqlx::query!(
    "DELETE FROM users WHERE id = $1",
    1
)
.execute(&pool)
.await?;

println!("Deleted {} rows", deleted.rows_affected());
```

---

## Problem: Transactions

### Scenario

You need to execute multiple queries atomically.

### Solution: Use begin() Transaction

```rust
use sqlx::PgPool;

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let pool = PgPool::connect("postgres://user:pass@localhost/database").await?;

    let mut tx = pool.begin().await?;

    // All queries within transaction
    sqlx::query!("INSERT INTO users (name, email) VALUES ($1, $2)", "Alice", "alice@example.com")
        .execute(&mut *tx)
        .await?;

    sqlx::query!("INSERT INTO users (name, email) VALUES ($1, $2)", "Bob", "bob@example.com")
        .execute(&mut *tx)
        .await?;

    // Commit transaction
    tx.commit().await?;

    println!("Transaction committed");

    Ok(())
}
```

**Rollback on error**:

```rust
let mut tx = pool.begin().await?;

let result = async {
    sqlx::query!("INSERT INTO users (name, email) VALUES ($1, $2)", "Alice", "alice@example.com")
        .execute(&mut *tx)
        .await?;

    sqlx::query!("INSERT INTO invalid_table (col) VALUES ($1)", "value")
        .execute(&mut *tx)
        .await?;

    Ok::<_, sqlx::Error>(())
}.await;

match result {
    Ok(_) => tx.commit().await?,
    Err(e) => {
        tx.rollback().await?;
        println!("Transaction rolled back: {}", e);
    }
}
```

---

## Problem: Migrations

### Scenario

You need to manage database schema changes.

### Solution: Use SQLx CLI

**Install SQLx CLI**:

```bash
cargo install sqlx-cli
```

**Create migration**:

```bash
sqlx migrate add create_users_table
```

**migrations/TIMESTAMP_create_users_table.sql**:

```sql
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT NOT NULL UNIQUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
```

**Run migrations**:

```bash
sqlx migrate run --database-url postgres://user:pass@localhost/database
```

**In code**:

```rust
use sqlx::migrate::MigrateDatabase;
use sqlx::PgPool;

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let pool = PgPool::connect("postgres://user:pass@localhost/database").await?;

    sqlx::migrate!("./migrations")
        .run(&pool)
        .await?;

    println!("Migrations applied");

    Ok(())
}
```

---

## Problem: Using an ORM (Diesel)

### Scenario

You prefer an ORM over raw SQL.

### Solution: Use Diesel

```toml
[dependencies]
diesel = { version = "2.1", features = ["postgres"] }
dotenvy = "0.15"
```

**Setup**:

```bash
cargo install diesel_cli --no-default-features --features postgres
diesel setup
diesel migration generate create_users
```

**migrations/.../up.sql**:

```sql
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR NOT NULL,
    email VARCHAR NOT NULL
)
```

**Run migration**:

```bash
diesel migration run
```

**src/schema.rs** (auto-generated):

```rust
diesel::table! {
    users (id) {
        id -> Int4,
        name -> Varchar,
        email -> Varchar,
    }
}
```

**src/models.rs**:

```rust
use diesel::prelude::*;
use crate::schema::users;

#[derive(Queryable, Selectable)]
#[diesel(table_name = users)]
pub struct User {
    pub id: i32,
    pub name: String,
    pub email: String,
}

#[derive(Insertable)]
#[diesel(table_name = users)]
pub struct NewUser<'a> {
    pub name: &'a str,
    pub email: &'a str,
}
```

**src/main.rs**:

```rust
use diesel::prelude::*;
use diesel::pg::PgConnection;
use dotenvy::dotenv;
use std::env;

mod models;
mod schema;

use models::{User, NewUser};
use schema::users;

fn establish_connection() -> PgConnection {
    dotenv().ok();
    let database_url = env::var("DATABASE_URL")
        .expect("DATABASE_URL must be set");
    PgConnection::establish(&database_url)
        .expect("Error connecting to database")
}

fn main() {
    let mut conn = establish_connection();

    // Insert
    let new_user = NewUser {
        name: "Alice",
        email: "alice@example.com",
    };

    diesel::insert_into(users::table)
        .values(&new_user)
        .execute(&mut conn)
        .expect("Error inserting user");

    // Query
    let results = users::table
        .load::<User>(&mut conn)
        .expect("Error loading users");

    for user in results {
        println!("{}: {}", user.name, user.email);
    }
}
```

---

## Problem: Connection Pooling

### Scenario

You need to manage multiple database connections efficiently.

### Solution: SQLx Automatically Pools Connections

```rust
use sqlx::postgres::{PgPool, PgPoolOptions};

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let pool = PgPoolOptions::new()
        .max_connections(5)
        .connect("postgres://user:pass@localhost/database")
        .await?;

    // Pool automatically manages connections
    let user_count = sqlx::query_scalar!("SELECT COUNT(*) FROM users")
        .fetch_one(&pool)
        .await?;

    println!("User count: {:?}", user_count);

    Ok(())
}
```

---

## Problem: Handling Optional Fields

### Scenario

Database columns can be NULL.

### Solution: Use Option<T>

```rust
use sqlx::FromRow;

#[derive(Debug, FromRow)]
struct User {
    id: i32,
    name: String,
    email: String,
    phone: Option<String>,  // NULL column
}

let users = sqlx::query_as::<_, User>("SELECT id, name, email, phone FROM users")
    .fetch_all(&pool)
    .await?;

for user in users {
    match user.phone {
        Some(phone) => println!("{} has phone: {}", user.name, phone),
        None => println!("{} has no phone", user.name),
    }
}
```

---

## Problem: Dynamic Queries

### Scenario

You need to build queries dynamically based on user input.

### Solution: Use QueryBuilder

```rust
use sqlx::QueryBuilder;
use sqlx::postgres::Postgres;

async fn search_users(
    pool: &PgPool,
    name: Option<String>,
    email: Option<String>,
) -> Result<Vec<User>, sqlx::Error> {
    let mut query = QueryBuilder::<Postgres>::new("SELECT id, name, email FROM users WHERE 1=1");

    if let Some(name) = name {
        query.push(" AND name = ");
        query.push_bind(name);
    }

    if let Some(email) = email {
        query.push(" AND email = ");
        query.push_bind(email);
    }

    query.build_query_as::<User>()
        .fetch_all(pool)
        .await
}
```

---

## Problem: Batch Inserts

### Scenario

You need to insert many records efficiently.

### Solution: Use Batch Insert

```rust
use sqlx::QueryBuilder;

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let pool = PgPool::connect("postgres://user:pass@localhost/database").await?;

    let users = vec![
        ("Alice", "alice@example.com"),
        ("Bob", "bob@example.com"),
        ("Charlie", "charlie@example.com"),
    ];

    let mut query_builder = QueryBuilder::new("INSERT INTO users (name, email) ");

    query_builder.push_values(users.iter(), |mut b, user| {
        b.push_bind(user.0)
         .push_bind(user.1);
    });

    query_builder.build()
        .execute(&pool)
        .await?;

    println!("Batch inserted {} users", users.len());

    Ok(())
}
```

---

## Common Pitfalls

### Pitfall 1: SQL Injection

**Problem**: String concatenation for queries.

```rust
// Bad - SQL injection risk!
let id = "1; DROP TABLE users;";
let query = format!("SELECT * FROM users WHERE id = {}", id);
```

**Solution**: Always use parameterized queries.

```rust
// Good
let id = 1;
let user = sqlx::query_as::<_, User>("SELECT * FROM users WHERE id = $1")
    .bind(id)
    .fetch_one(&pool)
    .await?;
```

### Pitfall 2: Not Handling Errors

**Problem**: Using unwrap() on database operations.

**Solution**: Properly handle errors with ? or match.

```rust
match sqlx::query!("SELECT * FROM users").fetch_all(&pool).await {
    Ok(users) => println!("Found {} users", users.len()),
    Err(e) => eprintln!("Database error: {}", e),
}
```

### Pitfall 3: Long-Running Transactions

**Problem**: Holding transactions open for too long.

**Solution**: Keep transactions short, only around critical operations.

---

## Related Resources

- [Async/Await Patterns](/en/learn/software-engineering/programming-languages/rust/how-to/async-await-patterns) - Async fundamentals
- [REST API Development](/en/learn/software-engineering/programming-languages/rust/how-to/rest-api-development) - Building APIs with databases
- [Error Handling](/en/learn/software-engineering/programming-languages/rust/how-to/error-handling-strategies) - Database error handling
- [Resources](/en/learn/software-engineering/programming-languages/rust/reference/resources) - Database crates

---

**Integrate databases effectively in your Rust applications!**
