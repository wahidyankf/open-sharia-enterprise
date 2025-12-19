---
title: HTTP Server Patterns
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 630
description: Build production-ready HTTP servers with routing, middleware, and best practices
tags: ["golang", "go", "http", "web-server", "rest-api"]
---

## Basic HTTP Server

Start with the standard library `net/http` package.

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
    "net/http"
)

func helloHandler(w http.ResponseWriter, r *http.Request) {
    w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(map[string]string{
        "message": "Hello, World!",
    })
}

func main() {
    http.HandleFunc("/hello", helloHandler)

    fmt.Println("Server starting on :8080")
    log.Fatal(http.ListenAndServe(":8080", nil))
}
```

## RESTful API with Router

Use a router for clean RESTful endpoints.

```go
package main

import (
    "encoding/json"
    "log"
    "net/http"
    "strings"
)

type User struct {
    ID    string `json:"id"`
    Name  string `json:"name"`
    Email string `json:"email"`
}

var users = map[string]User{
    "1": {ID: "1", Name: "Alice", Email: "alice@example.com"},
    "2": {ID: "2", Name: "Bob", Email: "bob@example.com"},
}

func usersHandler(w http.ResponseWriter, r *http.Request) {
    w.Header().Set("Content-Type", "application/json")

    switch r.Method {
    case http.MethodGet:
        // GET /users - list all
        if r.URL.Path == "/users" {
            json.NewEncoder(w).Encode(users)
            return
        }

        // GET /users/{id} - get one
        id := strings.TrimPrefix(r.URL.Path, "/users/")
        if user, ok := users[id]; ok {
            json.NewEncoder(w).Encode(user)
        } else {
            http.Error(w, "User not found", http.StatusNotFound)
        }

    case http.MethodPost:
        // POST /users - create
        var user User
        if err := json.NewDecoder(r.Body).Decode(&user); err != nil {
            http.Error(w, err.Error(), http.StatusBadRequest)
            return
        }
        users[user.ID] = user
        w.WriteHeader(http.StatusCreated)
        json.NewEncoder(w).Encode(user)

    default:
        http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
    }
}

func main() {
    http.HandleFunc("/users", usersHandler)
    http.HandleFunc("/users/", usersHandler)

    log.Println("Server starting on :8080")
    log.Fatal(http.ListenAndServe(":8080", nil))
}
```

## Middleware Pattern

Create reusable middleware for cross-cutting concerns.

```go
package main

import (
    "log"
    "net/http"
    "time"
)

// Middleware type
type Middleware func(http.HandlerFunc) http.HandlerFunc

// Logging middleware
func loggingMiddleware(next http.HandlerFunc) http.HandlerFunc {
    return func(w http.ResponseWriter, r *http.Request) {
        start := time.Now()
        next(w, r)
        log.Printf("%s %s %v", r.Method, r.URL.Path, time.Since(start))
    }
}

// Authentication middleware
func authMiddleware(next http.HandlerFunc) http.HandlerFunc {
    return func(w http.ResponseWriter, r *http.Request) {
        token := r.Header.Get("Authorization")
        if token != "Bearer secret" {
            http.Error(w, "Unauthorized", http.StatusUnauthorized)
            return
        }
        next(w, r)
    }
}

// CORS middleware
func corsMiddleware(next http.HandlerFunc) http.HandlerFunc {
    return func(w http.ResponseWriter, r *http.Request) {
        w.Header().Set("Access-Control-Allow-Origin", "*")
        w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE")
        w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")

        if r.Method == http.MethodOptions {
            w.WriteHeader(http.StatusOK)
            return
        }

        next(w, r)
    }
}

// Chain multiple middleware
func chain(f http.HandlerFunc, middlewares ...Middleware) http.HandlerFunc {
    for _, m := range middlewares {
        f = m(f)
    }
    return f
}

func helloHandler(w http.ResponseWriter, r *http.Request) {
    w.Write([]byte("Hello, authenticated user!"))
}

func main() {
    http.HandleFunc("/hello", chain(helloHandler,
        loggingMiddleware,
        authMiddleware,
        corsMiddleware,
    ))

    log.Println("Server starting on :8080")
    log.Fatal(http.ListenAndServe(":8080", nil))
}
```

## Context for Request-Scoped Values

Use context to pass request-scoped data.

```go
package main

import (
    "context"
    "fmt"
    "log"
    "net/http"
)

type contextKey string

const userIDKey contextKey = "userID"

func authMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        // Extract user ID from token (simplified)
        userID := "user123"

        // Add to context
        ctx := context.WithValue(r.Context(), userIDKey, userID)
        next.ServeHTTP(w, r.WithContext(ctx))
    })
}

func profileHandler(w http.ResponseWriter, r *http.Request) {
    userID, ok := r.Context().Value(userIDKey).(string)
    if !ok {
        http.Error(w, "Unauthorized", http.StatusUnauthorized)
        return
    }

    fmt.Fprintf(w, "Profile for user: %s", userID)
}

func main() {
    mux := http.NewServeMux()
    mux.HandleFunc("/profile", profileHandler)

    handler := authMiddleware(mux)

    log.Println("Server starting on :8080")
    log.Fatal(http.ListenAndServe(":8080", handler))
}
```

## JSON Response Helper

Create helper functions for consistent JSON responses.

```go
package main

import (
    "encoding/json"
    "log"
    "net/http"
)

type Response struct {
    Success bool        `json:"success"`
    Data    interface{} `json:"data,omitempty"`
    Error   string      `json:"error,omitempty"`
}

func respondJSON(w http.ResponseWriter, status int, payload interface{}) {
    w.Header().Set("Content-Type", "application/json")
    w.WriteHeader(status)
    json.NewEncoder(w).Encode(payload)
}

func respondSuccess(w http.ResponseWriter, data interface{}) {
    respondJSON(w, http.StatusOK, Response{
        Success: true,
        Data:    data,
    })
}

func respondError(w http.ResponseWriter, status int, message string) {
    respondJSON(w, status, Response{
        Success: false,
        Error:   message,
    })
}

func usersHandler(w http.ResponseWriter, r *http.Request) {
    users := []map[string]string{
        {"id": "1", "name": "Alice"},
        {"id": "2", "name": "Bob"},
    }
    respondSuccess(w, users)
}

func errorHandler(w http.ResponseWriter, r *http.Request) {
    respondError(w, http.StatusBadRequest, "Invalid request")
}

func main() {
    http.HandleFunc("/users", usersHandler)
    http.HandleFunc("/error", errorHandler)

    log.Println("Server starting on :8080")
    log.Fatal(http.ListenAndServe(":8080", nil))
}
```

## Graceful Shutdown

Handle shutdown signals properly.

```go
package main

import (
    "context"
    "fmt"
    "log"
    "net/http"
    "os"
    "os/signal"
    "syscall"
    "time"
)

func main() {
    mux := http.NewServeMux()
    mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        time.Sleep(2 * time.Second) // Simulate work
        fmt.Fprintln(w, "Hello!")
    })

    server := &http.Server{
        Addr:    ":8080",
        Handler: mux,
    }

    // Start server in goroutine
    go func() {
        log.Println("Server starting on :8080")
        if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
            log.Fatalf("Server error: %v", err)
        }
    }()

    // Wait for interrupt signal
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
    <-quit

    log.Println("Shutting down server...")

    // Graceful shutdown with timeout
    ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
    defer cancel()

    if err := server.Shutdown(ctx); err != nil {
        log.Fatalf("Server forced to shutdown: %v", err)
    }

    log.Println("Server stopped")
}
```

## Request Validation

Validate incoming requests systematically.

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
    "net/http"
    "strings"
)

type CreateUserRequest struct {
    Name  string `json:"name"`
    Email string `json:"email"`
    Age   int    `json:"age"`
}

func (r *CreateUserRequest) Validate() error {
    if strings.TrimSpace(r.Name) == "" {
        return fmt.Errorf("name is required")
    }
    if !strings.Contains(r.Email, "@") {
        return fmt.Errorf("invalid email format")
    }
    if r.Age < 18 {
        return fmt.Errorf("age must be 18 or older")
    }
    return nil
}

func createUserHandler(w http.ResponseWriter, r *http.Request) {
    var req CreateUserRequest
    if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
        http.Error(w, "Invalid JSON", http.StatusBadRequest)
        return
    }

    if err := req.Validate(); err != nil {
        http.Error(w, err.Error(), http.StatusBadRequest)
        return
    }

    // Process valid request
    w.WriteHeader(http.StatusCreated)
    fmt.Fprintf(w, "User %s created", req.Name)
}

func main() {
    http.HandleFunc("/users", createUserHandler)

    log.Println("Server starting on :8080")
    log.Fatal(http.ListenAndServe(":8080", nil))
}
```

## File Upload Handler

Handle file uploads securely.

```go
package main

import (
    "fmt"
    "io"
    "log"
    "net/http"
    "os"
    "path/filepath"
)

const maxUploadSize = 10 << 20 // 10 MB

func uploadHandler(w http.ResponseWriter, r *http.Request) {
    if r.Method != http.MethodPost {
        http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
        return
    }

    // Limit request size
    r.Body = http.MaxBytesReader(w, r.Body, maxUploadSize)
    if err := r.ParseMultipartForm(maxUploadSize); err != nil {
        http.Error(w, "File too large", http.StatusBadRequest)
        return
    }

    file, header, err := r.FormFile("file")
    if err != nil {
        http.Error(w, "Error retrieving file", http.StatusBadRequest)
        return
    }
    defer file.Close()

    // Validate file type
    if !isValidFileType(header.Filename) {
        http.Error(w, "Invalid file type", http.StatusBadRequest)
        return
    }

    // Create destination file
    dst, err := os.Create(filepath.Join("uploads", header.Filename))
    if err != nil {
        http.Error(w, "Error saving file", http.StatusInternalServerError)
        return
    }
    defer dst.Close()

    // Copy file
    if _, err := io.Copy(dst, file); err != nil {
        http.Error(w, "Error saving file", http.StatusInternalServerError)
        return
    }

    fmt.Fprintf(w, "File uploaded successfully: %s", header.Filename)
}

func isValidFileType(filename string) bool {
    ext := filepath.Ext(filename)
    validExts := []string{".jpg", ".jpeg", ".png", ".pdf"}
    for _, valid := range validExts {
        if ext == valid {
            return true
        }
    }
    return false
}

func main() {
    os.Mkdir("uploads", 0755)
    http.HandleFunc("/upload", uploadHandler)

    log.Println("Server starting on :8080")
    log.Fatal(http.ListenAndServe(":8080", nil))
}
```

## Related Resources

**Learn more**:

- [Concurrency Patterns](/en/learn/swe/prog-lang/golang/how-to/concurrency-patterns) - Goroutines for servers
- [Error Handling](/en/learn/swe/prog-lang/golang/how-to/error-handling) - Server error patterns
- [Cheat Sheet](/en/learn/swe/prog-lang/golang/reference/cheat-sheet) - HTTP syntax reference
