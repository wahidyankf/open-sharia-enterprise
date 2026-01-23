# HTTP Server Example (Go 1.22+ Enhanced Routing)

Complete HTTP server implementation using Go 1.22+ enhanced routing patterns from the standard library.

## Overview

This example demonstrates building a production-ready HTTP server with:

- Go 1.22+ enhanced routing (method handlers, wildcards, path values)
- Structured logging with log/slog
- Graceful shutdown
- Middleware (logging, recovery, CORS)
- Error handling
- Testing with httptest

## Complete Server Implementation

### Main Server

```go
// internal/infrastructure/http/server.go
package http

import (
    "context"
    "errors"
    "fmt"
    "log/slog"
    "net/http"
    "time"

    "example.com/my-go-project/internal/config"
)

// Server represents the HTTP server
type Server struct {
    httpServer *http.Server
    logger     *slog.Logger
}

// NewServer creates a new HTTP server
func NewServer(cfg *config.Config, logger *slog.Logger) *Server {
    mux := http.NewServeMux()

    // Create handlers
    userHandler := NewUserHandler(logger)
    healthHandler := NewHealthHandler()

    // Register routes using Go 1.22+ enhanced routing

    // Health check (exact match with {$})
    mux.HandleFunc("GET /health{$}", healthHandler.Check)

    // User routes with method-specific handlers
    mux.HandleFunc("GET /api/v1/users", userHandler.List)
    mux.HandleFunc("POST /api/v1/users", userHandler.Create)
    mux.HandleFunc("GET /api/v1/users/{id}", userHandler.GetByID)
    mux.HandleFunc("PUT /api/v1/users/{id}", userHandler.Update)
    mux.HandleFunc("DELETE /api/v1/users/{id}", userHandler.Delete)

    // Wildcard path for user documents
    mux.HandleFunc("GET /api/v1/users/{id}/documents/{path...}", userHandler.GetDocument)

    // Wrap mux with middleware
    handler := LoggingMiddleware(logger)(
        RecoveryMiddleware(logger)(
            CORSMiddleware()(mux),
        ),
    )

    return &Server{
        httpServer: &http.Server{
            Addr:         fmt.Sprintf(":%d", cfg.HTTP.Port),
            Handler:      handler,
            ReadTimeout:  cfg.HTTP.ReadTimeout,
            WriteTimeout: cfg.HTTP.WriteTimeout,
            IdleTimeout:  cfg.HTTP.IdleTimeout,
        },
        logger: logger,
    }
}

// Start starts the HTTP server
func (s *Server) Start() error {
    s.logger.Info("starting HTTP server", "addr", s.httpServer.Addr)

    if err := s.httpServer.ListenAndServe(); err != nil && !errors.Is(err, http.ErrServerClosed) {
        return fmt.Errorf("http server failed: %w", err)
    }

    return nil
}

// Shutdown gracefully shuts down the HTTP server
func (s *Server) Shutdown(ctx context.Context) error {
    s.logger.Info("shutting down HTTP server")

    if err := s.httpServer.Shutdown(ctx); err != nil {
        return fmt.Errorf("server shutdown failed: %w", err)
    }

    s.logger.Info("HTTP server stopped")
    return nil
}
```

### User Handler

```go
// internal/infrastructure/http/user_handler.go
package http

import (
    "encoding/json"
    "fmt"
    "log/slog"
    "net/http"
    "strings"

    "example.com/my-go-project/internal/application"
    "example.com/my-go-project/internal/domain"
)

// UserHandler handles user-related HTTP requests
type UserHandler struct {
    service *application.UserService
    logger  *slog.Logger
}

// NewUserHandler creates a new UserHandler
func NewUserHandler(logger *slog.Logger) *UserHandler {
    return &UserHandler{
        logger: logger,
    }
}

// List handles GET /api/v1/users
func (h *UserHandler) List(w http.ResponseWriter, r *http.Request) {
    ctx := r.Context()

    // Parse query parameters
    limit := r.URL.Query().Get("limit")
    offset := r.URL.Query().Get("offset")

    h.logger.InfoContext(ctx, "listing users",
        "limit", limit,
        "offset", offset,
    )

    // Call service
    users, err := h.service.List(ctx, limit, offset)
    if err != nil {
        h.respondError(w, r, http.StatusInternalServerError, "failed to list users", err)
        return
    }

    h.respondJSON(w, r, http.StatusOK, users)
}

// GetByID handles GET /api/v1/users/{id}
func (h *UserHandler) GetByID(w http.ResponseWriter, r *http.Request) {
    ctx := r.Context()

    // Extract path value using Go 1.22+ PathValue
    id := r.PathValue("id")

    h.logger.InfoContext(ctx, "getting user", "id", id)

    user, err := h.service.GetByID(ctx, id)
    if err != nil {
        if errors.Is(err, domain.ErrUserNotFound) {
            h.respondError(w, r, http.StatusNotFound, "user not found", err)
            return
        }
        h.respondError(w, r, http.StatusInternalServerError, "failed to get user", err)
        return
    }

    h.respondJSON(w, r, http.StatusOK, user)
}

// Create handles POST /api/v1/users
func (h *UserHandler) Create(w http.ResponseWriter, r *http.Request) {
    ctx := r.Context()

    var req CreateUserRequest
    if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
        h.respondError(w, r, http.StatusBadRequest, "invalid request body", err)
        return
    }

    // Validate request
    if err := req.Validate(); err != nil {
        h.respondError(w, r, http.StatusBadRequest, "validation failed", err)
        return
    }

    h.logger.InfoContext(ctx, "creating user", "email", req.Email)

    user, err := h.service.Create(ctx, req.Email, req.Name)
    if err != nil {
        if errors.Is(err, domain.ErrUserExists) {
            h.respondError(w, r, http.StatusConflict, "user already exists", err)
            return
        }
        h.respondError(w, r, http.StatusInternalServerError, "failed to create user", err)
        return
    }

    h.respondJSON(w, r, http.StatusCreated, user)
}

// Update handles PUT /api/v1/users/{id}
func (h *UserHandler) Update(w http.ResponseWriter, r *http.Request) {
    ctx := r.Context()

    id := r.PathValue("id")

    var req UpdateUserRequest
    if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
        h.respondError(w, r, http.StatusBadRequest, "invalid request body", err)
        return
    }

    h.logger.InfoContext(ctx, "updating user", "id", id)

    user, err := h.service.Update(ctx, id, req.Name)
    if err != nil {
        if errors.Is(err, domain.ErrUserNotFound) {
            h.respondError(w, r, http.StatusNotFound, "user not found", err)
            return
        }
        h.respondError(w, r, http.StatusInternalServerError, "failed to update user", err)
        return
    }

    h.respondJSON(w, r, http.StatusOK, user)
}

// Delete handles DELETE /api/v1/users/{id}
func (h *UserHandler) Delete(w http.ResponseWriter, r *http.Request) {
    ctx := r.Context()

    id := r.PathValue("id")

    h.logger.InfoContext(ctx, "deleting user", "id", id)

    if err := h.service.Delete(ctx, id); err != nil {
        if errors.Is(err, domain.ErrUserNotFound) {
            h.respondError(w, r, http.StatusNotFound, "user not found", err)
            return
        }
        h.respondError(w, r, http.StatusInternalServerError, "failed to delete user", err)
        return
    }

    w.WriteHeader(http.StatusNoContent)
}

// GetDocument handles GET /api/v1/users/{id}/documents/{path...}
// Demonstrates wildcard path matching
func (h *UserHandler) GetDocument(w http.ResponseWriter, r *http.Request) {
    ctx := r.Context()

    id := r.PathValue("id")
    path := r.PathValue("path") // Captures remaining path segments

    h.logger.InfoContext(ctx, "getting document",
        "user_id", id,
        "path", path,
    )

    // Example: /api/v1/users/123/documents/invoices/2024/january.pdf
    // id = "123"
    // path = "invoices/2024/january.pdf"

    document, err := h.service.GetDocument(ctx, id, path)
    if err != nil {
        h.respondError(w, r, http.StatusNotFound, "document not found", err)
        return
    }

    // Set content type based on file extension
    contentType := getContentType(path)
    w.Header().Set("Content-Type", contentType)
    w.Write(document)
}

// Helper methods

func (h *UserHandler) respondJSON(w http.ResponseWriter, r *http.Request, status int, data interface{}) {
    w.Header().Set("Content-Type", "application/json")
    w.WriteHeader(status)

    if err := json.NewEncoder(w).Encode(data); err != nil {
        h.logger.ErrorContext(r.Context(), "failed to encode response", "error", err)
    }
}

func (h *UserHandler) respondError(w http.ResponseWriter, r *http.Request, status int, message string, err error) {
    h.logger.ErrorContext(r.Context(), message, "error", err)

    response := ErrorResponse{
        Error:   message,
        Status:  status,
        Message: err.Error(),
    }

    h.respondJSON(w, r, status, response)
}

func getContentType(path string) string {
    switch {
    case strings.HasSuffix(path, ".pdf"):
        return "application/pdf"
    case strings.HasSuffix(path, ".json"):
        return "application/json"
    case strings.HasSuffix(path, ".xml"):
        return "application/xml"
    default:
        return "application/octet-stream"
    }
}

// Request/Response types

type CreateUserRequest struct {
    Email string `json:"email"`
    Name  string `json:"name"`
}

func (r CreateUserRequest) Validate() error {
    if r.Email == "" {
        return fmt.Errorf("email is required")
    }
    if r.Name == "" {
        return fmt.Errorf("name is required")
    }
    return nil
}

type UpdateUserRequest struct {
    Name string `json:"name"`
}

type ErrorResponse struct {
    Error   string `json:"error"`
    Status  int    `json:"status"`
    Message string `json:"message"`
}
```

### Middleware

```go
// internal/infrastructure/http/middleware.go
package http

import (
    "log/slog"
    "net/http"
    "runtime/debug"
    "time"
)

// LoggingMiddleware logs HTTP requests
func LoggingMiddleware(logger *slog.Logger) func(http.Handler) http.Handler {
    return func(next http.Handler) http.Handler {
        return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            start := time.Now()

            // Wrap ResponseWriter to capture status code
            wrapped := &responseWriter{ResponseWriter: w, statusCode: http.StatusOK}

            next.ServeHTTP(wrapped, r)

            logger.InfoContext(r.Context(), "http request",
                "method", r.Method,
                "path", r.URL.Path,
                "status", wrapped.statusCode,
                "duration_ms", time.Since(start).Milliseconds(),
                "remote_addr", r.RemoteAddr,
            )
        })
    }
}

// RecoveryMiddleware recovers from panics
func RecoveryMiddleware(logger *slog.Logger) func(http.Handler) http.Handler {
    return func(next http.Handler) http.Handler {
        return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            defer func() {
                if err := recover(); err != nil {
                    logger.ErrorContext(r.Context(), "panic recovered",
                        "error", err,
                        "stack", string(debug.Stack()),
                    )

                    w.WriteHeader(http.StatusInternalServerError)
                    w.Write([]byte(`{"error":"internal server error"}`))
                }
            }()

            next.ServeHTTP(w, r)
        })
    }
}

// CORSMiddleware adds CORS headers
func CORSMiddleware() func(http.Handler) http.Handler {
    return func(next http.Handler) http.Handler {
        return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            w.Header().Set("Access-Control-Allow-Origin", "*")
            w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
            w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")

            if r.Method == http.MethodOptions {
                w.WriteHeader(http.StatusOK)
                return
            }

            next.ServeHTTP(w, r)
        })
    }
}

// responseWriter wraps http.ResponseWriter to capture status code
type responseWriter struct {
    http.ResponseWriter
    statusCode int
}

func (rw *responseWriter) WriteHeader(code int) {
    rw.statusCode = code
    rw.ResponseWriter.WriteHeader(code)
}
```

### Health Handler

```go
// internal/infrastructure/http/health_handler.go
package http

import (
    "encoding/json"
    "net/http"
    "time"
)

// HealthHandler handles health check requests
type HealthHandler struct{}

// NewHealthHandler creates a new HealthHandler
func NewHealthHandler() *HealthHandler {
    return &HealthHandler{}
}

// Check handles GET /health
func (h *HealthHandler) Check(w http.ResponseWriter, r *http.Request) {
    response := HealthResponse{
        Status:    "healthy",
        Timestamp: time.Now(),
    }

    w.Header().Set("Content-Type", "application/json")
    w.WriteHeader(http.StatusOK)
    json.NewEncoder(w).Encode(response)
}

type HealthResponse struct {
    Status    string    `json:"status"`
    Timestamp time.Time `json:"timestamp"`
}
```

### Testing

```go
// internal/infrastructure/http/user_handler_test.go
package http

import (
    "bytes"
    "encoding/json"
    "net/http"
    "net/http/httptest"
    "testing"

    "github.com/stretchr/testify/assert"
    "github.com/stretchr/testify/require"
)

func TestUserHandler_GetByID(t *testing.T) {
    tests := []struct {
        name           string
        userID         string
        setupMock      func(*MockUserService)
        expectedStatus int
        expectedBody   string
    }{
        {
            name:   "user found",
            userID: "123",
            setupMock: func(m *MockUserService) {
                m.On("GetByID", mock.Anything, "123").Return(
                    User{ID: "123", Email: "test@example.com", Name: "Test User"},
                    nil,
                )
            },
            expectedStatus: http.StatusOK,
        },
        {
            name:   "user not found",
            userID: "999",
            setupMock: func(m *MockUserService) {
                m.On("GetByID", mock.Anything, "999").Return(
                    User{},
                    domain.ErrUserNotFound,
                )
            },
            expectedStatus: http.StatusNotFound,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            // Setup
            mockService := new(MockUserService)
            tt.setupMock(mockService)

            handler := &UserHandler{
                service: mockService,
                logger:  slog.Default(),
            }

            // Create request with path value
            req := httptest.NewRequest(http.MethodGet, "/api/v1/users/"+tt.userID, nil)
            req.SetPathValue("id", tt.userID)

            w := httptest.NewRecorder()

            // Execute
            handler.GetByID(w, req)

            // Assert
            assert.Equal(t, tt.expectedStatus, w.Code)
            mockService.AssertExpectations(t)
        })
    }
}

func TestUserHandler_Create(t *testing.T) {
    reqBody := CreateUserRequest{
        Email: "new@example.com",
        Name:  "New User",
    }

    body, err := json.Marshal(reqBody)
    require.NoError(t, err)

    req := httptest.NewRequest(http.MethodPost, "/api/v1/users", bytes.NewReader(body))
    req.Header.Set("Content-Type", "application/json")

    w := httptest.NewRecorder()

    mockService := new(MockUserService)
    mockService.On("Create", mock.Anything, reqBody.Email, reqBody.Name).Return(
        User{ID: "456", Email: reqBody.Email, Name: reqBody.Name},
        nil,
    )

    handler := &UserHandler{
        service: mockService,
        logger:  slog.Default(),
    }

    // Execute
    handler.Create(w, req)

    // Assert
    assert.Equal(t, http.StatusCreated, w.Code)

    var response User
    err = json.NewDecoder(w.Body).Decode(&response)
    require.NoError(t, err)
    assert.Equal(t, "456", response.ID)
    assert.Equal(t, reqBody.Email, response.Email)

    mockService.AssertExpectations(t)
}
```

## Go 1.22+ Routing Features Demonstrated

### 1. Method-Specific Handlers

```go
// Old way (Go 1.21 and earlier)
mux.HandleFunc("/users", func(w http.ResponseWriter, r *http.Request) {
    switch r.Method {
    case http.MethodGet:
        listUsers(w, r)
    case http.MethodPost:
        createUser(w, r)
    default:
        http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
    }
})

// New way (Go 1.22+)
mux.HandleFunc("GET /users", listUsers)
mux.HandleFunc("POST /users", createUsers)
```

### 2. Path Values with {id}

```go
// Extract path parameter
mux.HandleFunc("GET /users/{id}", func(w http.ResponseWriter, r *http.Request) {
    id := r.PathValue("id") // Go 1.22+ PathValue method
    // Use id...
})
```

### 3. Wildcard Paths with {path...}

```go
// Capture remaining path segments
mux.HandleFunc("GET /files/{path...}", func(w http.ResponseWriter, r *http.Request) {
    path := r.PathValue("path")
    // path = "documents/2024/invoice.pdf" for /files/documents/2024/invoice.pdf
})
```

### 4. Exact Match with {$}

```go
// Only matches exact path
mux.HandleFunc("GET /health{$}", healthCheck) // Matches /health but not /health/
```

## Best Practices

1. **Use Go 1.22+ Routing**: Leverage enhanced routing instead of third-party routers for simple APIs
2. **Structured Logging**: Use log/slog for structured, contextual logging
3. **Graceful Shutdown**: Always implement graceful shutdown
4. **Middleware Pattern**: Chain middleware for cross-cutting concerns
5. **Error Handling**: Return appropriate HTTP status codes and error messages
6. **Testing**: Use httptest for handler testing
7. **Context Propagation**: Pass context through all layers

## Related Documentation

- [Go Web Services](../ex-so-stla-go__web-services.md) - Complete web services guide
- [Go Best Practices](../ex-so-stla-go__best-practices.md) - HTTP best practices
- [Go Release 1.22](../ex-so-stla-go__1.22-release.md) - Enhanced HTTP routing details

---

**Last Updated**: 2025-01-23
**Go Version**: 1.18+
