---
title: REST API Development
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000011
description: Practical guide to building REST APIs with Rust using Axum or Actix-web
tags:
  ["rust", "how-to", "rest-api", "web", "axum", "actix-web", "http", "backend"]
---

**Need to build a REST API in Rust?** This guide covers setting up a web server, routing, request handling, JSON serialization, middleware, and deployment.

## Problem: Creating a Basic HTTP Server

### Scenario

You need to start a web server and handle HTTP requests.

### Solution: Use Axum

```toml
[dependencies]
axum = "0.7"
tokio = { version = "1.0", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

```rust
use axum::{routing::get, Router};

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/", get(root));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    println!("Server running on http://127.0.0.1:3000");

    axum::serve(listener, app).await.unwrap();
}

async fn root() -> &'static str {
    "Hello, World!"
}
```

**Test**:

```bash
curl http://localhost:3000
```

---

## Problem: Defining Routes

### Scenario

Your API needs multiple endpoints with different HTTP methods.

### Solution: Set Up Routes

```rust
use axum::{
    routing::{get, post, put, delete},
    Router,
};

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/", get(root))
        .route("/users", get(list_users).post(create_user))
        .route("/users/:id", get(get_user).put(update_user).delete(delete_user));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap();
}

async fn root() -> &'static str {
    "API v1"
}

async fn list_users() -> &'static str {
    "List all users"
}

async fn create_user() -> &'static str {
    "Create user"
}

async fn get_user() -> &'static str {
    "Get user"
}

async fn update_user() -> &'static str {
    "Update user"
}

async fn delete_user() -> &'static str {
    "Delete user"
}
```

---

## Problem: Extracting Path Parameters

### Scenario

You need to get values from the URL path.

### Solution: Use Path Extractor

```rust
use axum::{
    extract::Path,
    routing::get,
    Router,
};

async fn get_user(Path(id): Path<u32>) -> String {
    format!("User ID: {}", id)
}

async fn get_post(Path((user_id, post_id)): Path<(u32, u32)>) -> String {
    format!("User {}, Post {}", user_id, post_id)
}

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/users/:id", get(get_user))
        .route("/users/:user_id/posts/:post_id", get(get_post));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap();
}
```

---

## Problem: JSON Request and Response

### Scenario

Your API needs to accept and return JSON.

### Solution: Use JSON Extractor and Response

```rust
use axum::{
    extract::Json,
    routing::{get, post},
    Router,
};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
    email: String,
}

#[derive(Deserialize)]
struct CreateUser {
    name: String,
    email: String,
}

async fn list_users() -> Json<Vec<User>> {
    let users = vec![
        User {
            id: 1,
            name: "Alice".to_string(),
            email: "alice@example.com".to_string(),
        },
        User {
            id: 2,
            name: "Bob".to_string(),
            email: "bob@example.com".to_string(),
        },
    ];

    Json(users)
}

async fn create_user(Json(payload): Json<CreateUser>) -> Json<User> {
    let user = User {
        id: 1,
        name: payload.name,
        email: payload.email,
    };

    Json(user)
}

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/users", get(list_users).post(create_user));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap();
}
```

**Test**:

```bash
curl http://localhost:3000/users
curl -X POST http://localhost:3000/users \
  -H "Content-Type: application/json" \
  -d '{"name":"Charlie","email":"charlie@example.com"}'
```

---

## Problem: Query Parameters

### Scenario

You need to extract query parameters from the URL.

### Solution: Use Query Extractor

```rust
use axum::{
    extract::Query,
    routing::get,
    Json, Router,
};
use serde::Deserialize;

#[derive(Deserialize)]
struct Pagination {
    page: Option<u32>,
    per_page: Option<u32>,
}

async fn list_users(Query(pagination): Query<Pagination>) -> String {
    let page = pagination.page.unwrap_or(1);
    let per_page = pagination.per_page.unwrap_or(10);

    format!("Page: {}, Per page: {}", page, per_page)
}

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/users", get(list_users));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap();
}
```

**Test**:

```bash
curl "http://localhost:3000/users?page=2&per_page=20"
```

---

## Problem: Shared Application State

### Scenario

Handlers need to access shared data (database pool, config).

### Solution: Use State Extractor

```rust
use axum::{
    extract::State,
    routing::get,
    Router,
};
use std::sync::{Arc, Mutex};

#[derive(Clone)]
struct AppState {
    counter: Arc<Mutex<u32>>,
}

async fn get_count(State(state): State<AppState>) -> String {
    let count = state.counter.lock().unwrap();
    format!("Count: {}", *count)
}

async fn increment(State(state): State<AppState>) -> String {
    let mut count = state.counter.lock().unwrap();
    *count += 1;
    format!("Count: {}", *count)
}

#[tokio::main]
async fn main() {
    let state = AppState {
        counter: Arc::new(Mutex::new(0)),
    };

    let app = Router::new()
        .route("/count", get(get_count))
        .route("/increment", get(increment))
        .with_state(state);

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap();
}
```

---

## Problem: Error Handling

### Scenario

You need to handle errors and return appropriate HTTP status codes.

### Solution: Use Result and Custom Error Type

```rust
use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
    Json, Router,
};
use serde_json::json;

enum AppError {
    NotFound,
    InternalError,
}

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        let (status, message) = match self {
            AppError::NotFound => (StatusCode::NOT_FOUND, "Resource not found"),
            AppError::InternalError => (StatusCode::INTERNAL_SERVER_ERROR, "Internal error"),
        };

        (status, Json(json!({ "error": message }))).into_response()
    }
}

async fn get_user(axum::extract::Path(id): axum::extract::Path<u32>) -> Result<Json<serde_json::Value>, AppError> {
    if id == 0 {
        return Err(AppError::NotFound);
    }

    Ok(Json(json!({
        "id": id,
        "name": "User"
    })))
}

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/users/:id", get(get_user));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap();
}
```

---

## Problem: Middleware

### Scenario

You need to add logging, authentication, or CORS.

### Solution: Use Tower Middleware

```rust
use axum::{
    middleware::{self, Next},
    response::Response,
    Router,
};
use tower_http::cors::{Any, CorsLayer};
use std::time::Instant;

async fn logging_middleware(
    req: axum::http::Request<axum::body::Body>,
    next: Next,
) -> Response {
    let start = Instant::now();
    let method = req.method().clone();
    let uri = req.uri().clone();

    let response = next.run(req).await;

    let duration = start.elapsed();
    println!("{} {} - {:?}", method, uri, duration);

    response
}

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/", axum::routing::get(|| async { "Hello!" }))
        .layer(middleware::from_fn(logging_middleware))
        .layer(
            CorsLayer::new()
                .allow_origin(Any)
                .allow_methods(Any),
        );

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap();
}
```

---

## Problem: File Uploads

### Scenario

Your API needs to accept file uploads.

### Solution: Use Multipart

```rust
use axum::{
    extract::Multipart,
    routing::post,
    Router,
};
use tokio::fs;

async fn upload(mut multipart: Multipart) -> String {
    while let Some(field) = multipart.next_field().await.unwrap() {
        let name = field.name().unwrap().to_string();
        let data = field.bytes().await.unwrap();

        println!("Received field: {} ({} bytes)", name, data.len());

        // Save file
        fs::write(format!("uploads/{}", name), &data).await.unwrap();
    }

    "File uploaded successfully".to_string()
}

#[tokio::main]
async fn main() {
    fs::create_dir_all("uploads").await.unwrap();

    let app = Router::new()
        .route("/upload", post(upload));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap();
}
```

---

## Problem: Database Integration

### Scenario

Your API needs to query a database.

### Solution: Use SQLx with Axum

```toml
[dependencies]
sqlx = { version = "0.7", features = ["runtime-tokio-rustls", "sqlite"] }
```

```rust
use axum::{
    extract::{Path, State},
    routing::get,
    Json, Router,
};
use sqlx::sqlite::SqlitePool;
use serde::Serialize;

#[derive(Serialize, sqlx::FromRow)]
struct User {
    id: i64,
    name: String,
}

#[derive(Clone)]
struct AppState {
    pool: SqlitePool,
}

async fn list_users(State(state): State<AppState>) -> Json<Vec<User>> {
    let users = sqlx::query_as::<_, User>("SELECT id, name FROM users")
        .fetch_all(&state.pool)
        .await
        .unwrap();

    Json(users)
}

async fn get_user(
    State(state): State<AppState>,
    Path(id): Path<i64>,
) -> Json<Option<User>> {
    let user = sqlx::query_as::<_, User>("SELECT id, name FROM users WHERE id = ?")
        .bind(id)
        .fetch_optional(&state.pool)
        .await
        .unwrap();

    Json(user)
}

#[tokio::main]
async fn main() {
    let pool = SqlitePool::connect("sqlite::memory:").await.unwrap();

    sqlx::query("CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)")
        .execute(&pool)
        .await
        .unwrap();

    let state = AppState { pool };

    let app = Router::new()
        .route("/users", get(list_users))
        .route("/users/:id", get(get_user))
        .with_state(state);

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap();
}
```

---

## Problem: Testing APIs

### Scenario

You need to test your API endpoints.

### Solution: Use Tower Test Utilities

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use axum::body::Body;
    use axum::http::{Request, StatusCode};
    use tower::ServiceExt;

    #[tokio::test]
    async fn test_root() {
        let app = Router::new()
            .route("/", axum::routing::get(|| async { "Hello!" }));

        let response = app
            .oneshot(Request::builder().uri("/").body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
    }
}
```

---

## Common Pitfalls

### Pitfall 1: Not Handling CORS

**Problem**: Browser requests fail due to CORS.

**Solution**: Add CORS middleware.

```rust
use tower_http::cors::{Any, CorsLayer};

let app = Router::new()
    .route("/api/users", get(list_users))
    .layer(CorsLayer::new().allow_origin(Any));
```

### Pitfall 2: Blocking Operations in Handlers

**Problem**: Calling blocking code in async handler.

**Solution**: Use spawn_blocking.

```rust
async fn handler() -> String {
    tokio::task::spawn_blocking(|| {
        // Blocking operation
        std::thread::sleep(std::time::Duration::from_secs(1));
        "Done".to_string()
    }).await.unwrap()
}
```

---

## Related Resources

- [Async/Await Patterns](/en/learn/software-engineering/programming-language/rust/how-to/async-await-patterns) - Async fundamentals
- [Database Integration](/en/learn/software-engineering/programming-language/rust/how-to/database-integration) - Database patterns
- [Error Handling](/en/learn/software-engineering/programming-language/rust/how-to/error-handling-strategies) - Error handling
- [Tutorials: Intermediate](/en/learn/software-engineering/programming-language/rust/tutorials/intermediate) - Web development basics

---

**Build production-ready REST APIs with Rust!**
