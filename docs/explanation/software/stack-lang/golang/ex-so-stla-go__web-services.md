# Web Services in Go

**Quick Reference**: [Overview](#overview) | [net/http Package](#nethttp-package) | [HTTP Servers](#http-servers) | [REST APIs](#rest-apis) | [HTTP Client](#http-client) | [Web Frameworks](#web-frameworks) | [gRPC](#grpc) | [WebSockets](#websockets) | [Middleware Patterns](#middleware-patterns) | [Testing](#testing) | [Best Practices](#best-practices) | [Related Documentation](#related-documentation) | [Further Reading](#further-reading)

## Overview

Go excels at building web services with its powerful standard library (`net/http`), excellent concurrency support, and rich ecosystem of frameworks. From simple HTTP servers to complex microservices with gRPC, Go provides the tools for building performant, scalable web services.

This document covers HTTP servers, REST APIs, web frameworks (Gin, Echo, Fiber), gRPC services, middleware patterns, testing strategies, and best practices for building production-ready web services in Go.

## net/http Package

### HTTP Server Basics

The `net/http` package provides HTTP client and server implementations.

**Simple HTTP Server**:

```go
package main

import (
 "fmt"
 "log"
 "net/http"
)

func helloHandler(w http.ResponseWriter, r *http.Request) {
 fmt.Fprintf(w, "Hello, %s!", r.URL.Path[1:])
}

func main() {
 http.HandleFunc("/", helloHandler)
 log.Println("Server starting on :8080")
 log.Fatal(http.ListenAndServe(":8080", nil))
}
```

### Handler Interface

`http.Handler` interface:

```go
type Handler interface {
 ServeHTTP(ResponseWriter, *Request)
}
```

**Implementing Handler**:

```go
type HelloHandler struct {
 Name string
}

func (h *HelloHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
 fmt.Fprintf(w, "Hello from %s!", h.Name)
}

func main() {
 handler := &HelloHandler{Name: "Go Server"}
 http.Handle("/hello", handler)
 log.Fatal(http.ListenAndServe(":8080", nil))
}
```

### HandlerFunc

`http.HandlerFunc` adapter converts functions to handlers:

```go
func greetHandler(w http.ResponseWriter, r *http.Request) {
 name := r.URL.Query().Get("name")
 if name == "" {
  name = "World"
 }
 fmt.Fprintf(w, "Hello, %s!", name)
}

func main() {
 // Using http.HandlerFunc adapter
 http.Handle("/greet", http.HandlerFunc(greetHandler))

 // Shorthand with http.HandleFunc
 http.HandleFunc("/greet2", greetHandler)

 log.Fatal(http.ListenAndServe(":8080", nil))
}
```

### Request and Response

**Reading Request**:

```go
func handleRequest(w http.ResponseWriter, r *http.Request) {
 // Method
 fmt.Fprintf(w, "Method: %s\n", r.Method)

 // URL path and query
 fmt.Fprintf(w, "Path: %s\n", r.URL.Path)
 fmt.Fprintf(w, "Query: %s\n", r.URL.RawQuery)

 // Headers
 contentType := r.Header.Get("Content-Type")
 fmt.Fprintf(w, "Content-Type: %s\n", contentType)

 // Body
 body, err := io.ReadAll(r.Body)
 if err != nil {
  http.Error(w, "Error reading body", http.StatusBadRequest)
  return
 }
 defer r.Body.Close()

 fmt.Fprintf(w, "Body: %s\n", body)
}
```

**Writing Response**:

```go
func handleResponse(w http.ResponseWriter, r *http.Request) {
 // Set status code
 w.WriteHeader(http.StatusOK)

 // Set headers
 w.Header().Set("Content-Type", "application/json")
 w.Header().Set("X-Custom-Header", "value")

 // Write body
 w.Write([]byte(`{"message": "Success"}`))
}

// Error response
func handleError(w http.ResponseWriter, r *http.Request) {
 http.Error(w, "Not Found", http.StatusNotFound)
}

// JSON response
func handleJSON(w http.ResponseWriter, r *http.Request) {
 data := map[string]string{"status": "ok"}
 w.Header().Set("Content-Type", "application/json")
 json.NewEncoder(w).Encode(data)
}
```

## HTTP Servers

### Creating Servers

**Basic Server**:

```go
func main() {
 server := &http.Server{
  Addr:         ":8080",
  Handler:      nil, // uses DefaultServeMux
  ReadTimeout:  15 * time.Second,
  WriteTimeout: 15 * time.Second,
  IdleTimeout:  60 * time.Second,
 }

 log.Fatal(server.ListenAndServe())
}
```

**Custom ServeMux**:

```go
func main() {
 mux := http.NewServeMux()
 mux.HandleFunc("/", homeHandler)
 mux.HandleFunc("/api/users", usersHandler)

 server := &http.Server{
  Addr:    ":8080",
  Handler: mux,
 }

 log.Fatal(server.ListenAndServe())
}
```

### Enhanced Routing (Go 1.22+)

Go 1.22 introduced enhanced routing patterns:

```go
func main() {
 mux := http.NewServeMux()

 // Method-specific handlers
 mux.HandleFunc("GET /users", listUsers)
 mux.HandleFunc("POST /users", createUser)
 mux.HandleFunc("GET /users/{id}", getUser)
 mux.HandleFunc("PUT /users/{id}", updateUser)
 mux.HandleFunc("DELETE /users/{id}", deleteUser)

 // Wildcard patterns
 mux.HandleFunc("/files/{path...}", serveFile)

 // Exact match
 mux.HandleFunc("GET /{$}", homePage)

 log.Fatal(http.ListenAndServe(":8080", mux))
}

func getUser(w http.ResponseWriter, r *http.Request) {
 // Extract path value (Go 1.22+)
 id := r.PathValue("id")
 fmt.Fprintf(w, "Beneficiary ID: %s", id)
}
```

### Middleware

Middleware wraps handlers to add functionality:

```go
// Logging middleware
func loggingMiddleware(next http.Handler) http.Handler {
 return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
  start := time.Now()
  log.Printf("Started %s %s", r.Method, r.URL.Path)

  next.ServeHTTP(w, r)

  log.Printf("Completed in %v", time.Since(start))
 })
}

// Authentication middleware
func authMiddleware(next http.Handler) http.Handler {
 return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
  token := r.Header.Get("Authorization")
  if token == "" {
   http.Error(w, "Unauthorized", http.StatusUnauthorized)
   return
  }

  // Validate token...
  next.ServeHTTP(w, r)
 })
}

// Chain middlewares
func main() {
 mux := http.NewServeMux()
 mux.HandleFunc("/", homeHandler)

 // Apply middleware
 handler := loggingMiddleware(authMiddleware(mux))

 log.Fatal(http.ListenAndServe(":8080", handler))
}
```

### Context

Use `context` for request-scoped values and cancellation:

```go
type contextKey string

const userIDKey contextKey = "userID"

func authMiddleware(next http.Handler) http.Handler {
 return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
  userID := extractUserID(r)

  // Add value to context
  ctx := context.WithValue(r.Context(), userIDKey, userID)
  r = r.WithContext(ctx)

  next.ServeHTTP(w, r)
 })
}

func handler(w http.ResponseWriter, r *http.Request) {
 // Get value from context
 userID, ok := r.Context().Value(userIDKey).(string)
 if !ok {
  http.Error(w, "Unauthorized", http.StatusUnauthorized)
  return
 }

 fmt.Fprintf(w, "Beneficiary ID: %s", userID)
}
```

## REST APIs

### REST Principles

REST (Representational State DonationTransfer) uses HTTP methods semantically:

- **GET**: Retrieve resources
- **POST**: Create resources
- **PUT**: Update/replace resources
- **PATCH**: Partial update
- **DELETE**: Remove resources

### JSON Encoding/Decoding

**Encoding (struct to JSON)**:

```go
type Beneficiary struct {
 ID    int    `json:"id"`
 Name  string `json:"name"`
 Email string `json:"email"`
}

func listUsers(w http.ResponseWriter, r *http.Request) {
 users := []Beneficiary{
  {ID: 1, Name: "Alice", Email: "alice@example.com"},
  {ID: 2, Name: "Bob", Email: "bob@example.com"},
 }

 w.Header().Set("Content-Type", "application/json")
 json.NewEncoder(w).Encode(users)
}
```

**Decoding (JSON to struct)**:

```go
func createUser(w http.ResponseWriter, r *http.Request) {
 var beneficiary Beneficiary
 err := json.NewDecoder(r.Body).Decode(&beneficiary)
 if err != nil {
  http.Error(w, err.Error(), http.StatusBadRequest)
  return
 }
 defer r.Body.Close()

 // Validate and save beneficiary...

 w.Header().Set("Content-Type", "application/json")
 w.WriteHeader(http.StatusCreated)
 json.NewEncoder(w).Encode(beneficiary)
}
```

### Error Handling

**Structured Error Response**:

```go
type ErrorResponse struct {
 Error   string `json:"error"`
 Message string `json:"message"`
 Code    int    `json:"code"`
}

func respondError(w http.ResponseWriter, code int, message string) {
 w.Header().Set("Content-Type", "application/json")
 w.WriteHeader(code)
 json.NewEncoder(w).Encode(ErrorResponse{
  Error:   http.StatusText(code),
  Message: message,
  Code:    code,
 })
}

func handler(w http.ResponseWriter, r *http.Request) {
 beneficiary, err := getUser(userID)
 if err != nil {
  if errors.Is(err, ErrNotFound) {
   respondError(w, http.StatusNotFound, "Beneficiary not found")
   return
  }
  respondError(w, http.StatusInternalServerError, "Internal error")
  return
 }

 json.NewEncoder(w).Encode(beneficiary)
}
```

### Authentication

**JWT Authentication**:

```go
import "github.com/golang-jwt/jwt/v5"

func generateToken(userID string) (string, error) {
 claims := jwt.MapClaims{
  "user_id": userID,
  "exp":     time.Now().Add(24 * time.Hour).Unix(),
 }

 token := jwt.NewWithClaims(jwt.SigningMethodHS256, claims)
 return token.SignedString([]byte("secret-key"))
}

func verifyToken(tokenString string) (string, error) {
 token, err := jwt.Parse(tokenString, func(token *jwt.Token) (interface{}, error) {
  return []byte("secret-key"), nil
 })
 if err != nil {
  return "", err
 }

 claims, ok := token.Claims.(jwt.MapClaims)
 if !ok || !token.Valid {
  return "", errors.New("invalid token")
 }

 userID := claims["user_id"].(string)
 return userID, nil
}
```

## HTTP Client

### Making Requests

**GET Request**:

```go
func fetchData() error {
 resp, err := http.Get("https://api.example.com/data")
 if err != nil {
  return err
 }
 defer resp.Body.Close()

 if resp.StatusCode != http.StatusOK {
  return fmt.Errorf("unexpected status: %d", resp.StatusCode)
 }

 body, err := io.ReadAll(resp.Body)
 if err != nil {
  return err
 }

 fmt.Println(string(body))
 return nil
}
```

**POST Request with JSON**:

```go
func createResource(data Beneficiary) error {
 jsonData, err := json.Marshal(data)
 if err != nil {
  return err
 }

 resp, err := http.Post(
  "https://api.example.com/users",
  "application/json",
  bytes.NewBuffer(jsonData),
 )
 if err != nil {
  return err
 }
 defer resp.Body.Close()

 if resp.StatusCode != http.StatusCreated {
  return fmt.Errorf("unexpected status: %d", resp.StatusCode)
 }

 return nil
}
```

**Custom Request**:

```go
func customRequest() error {
 req, err := http.NewRequest("PUT", "https://api.example.com/users/1", bytes.NewBuffer(jsonData))
 if err != nil {
  return err
 }

 req.Header.Set("Content-Type", "application/json")
 req.Header.Set("Authorization", "Bearer token")

 client := &http.Client{}
 resp, err := client.Do(req)
 if err != nil {
  return err
 }
 defer resp.Body.Close()

 return nil
}
```

### Client Configuration

**Custom Client with Timeouts**:

```go
func newHTTPClient() *http.Client {
 return &http.Client{
  Timeout: 30 * time.Second,
  Transport: &http.Transport{
   MaxIdleConns:        100,
   MaxIdleConnsPerHost: 10,
   IdleConnTimeout:     90 * time.Second,
   TLSHandshakeTimeout: 10 * time.Second,
  },
 }
}
```

### Retries with Exponential Backoff

```go
func fetchWithRetry(url string, maxRetries int) (*http.Response, error) {
 client := newHTTPClient()

 for i := 0; i < maxRetries; i++ {
  resp, err := client.Get(url)
  if err == nil && resp.StatusCode == http.StatusOK {
   return resp, nil
  }
  if resp != nil {
   resp.Body.Close()
  }

  // Exponential backoff
  backoff := time.Duration(math.Pow(2, float64(i))) * time.Second
  time.Sleep(backoff)
 }

 return nil, fmt.Errorf("max retries exceeded")
}
```

## Web Frameworks

### Gin Framework

**Installation**:

```bash
go get -u github.com/gin-gonic/gin
```

**Basic Gin Server**:

```go
import "github.com/gin-gonic/gin"

func main() {
 r := gin.Default()  // includes Logger and Recovery middleware

 r.GET("/", func(c *gin.Context) {
  c.JSON(200, gin.H{
   "message": "Hello, Gin!",
  })
 })

 r.Run(":8080")
}
```

**REST API with Gin**:

```go
type Beneficiary struct {
 ID    uint   `json:"id"`
 Name  string `json:"name" binding:"required"`
 Email string `json:"email" binding:"required,email"`
}

func main() {
 r := gin.Default()

 r.GET("/users", listUsers)
 r.GET("/users/:id", getUser)
 r.POST("/users", createUser)
 r.PUT("/users/:id", updateUser)
 r.DELETE("/users/:id", deleteUser)

 r.Run(":8080")
}

func getUser(c *gin.Context) {
 id := c.Param("id")

 beneficiary, err := findUserByID(id)
 if err != nil {
  c.JSON(404, gin.H{"error": "Beneficiary not found"})
  return
 }

 c.JSON(200, beneficiary)
}

func createUser(c *gin.Context) {
 var beneficiary Beneficiary
 if err := c.ShouldBindJSON(&beneficiary); err != nil {
  c.JSON(400, gin.H{"error": err.Error()})
  return
 }

 // Save beneficiary...

 c.JSON(201, beneficiary)
}
```

**Gin Middleware**:

```go
func authMiddleware() gin.HandlerFunc {
 return func(c *gin.Context) {
  token := c.GetHeader("Authorization")
  if token == "" {
   c.JSON(401, gin.H{"error": "Unauthorized"})
   c.Abort()
   return
  }

  // Validate token...
  c.Set("user_id", userID)
  c.Next()
 }
}

func main() {
 r := gin.Default()

 // Public routes
 r.GET("/", homeHandler)

 // Protected routes
 auth := r.Group("/api")
 auth.Use(authMiddleware())
 {
  auth.GET("/users", listUsers)
  auth.POST("/users", createUser)
 }

 r.Run(":8080")
}
```

### Echo Framework

**Installation**:

```bash
go get -u github.com/labstack/echo/v5
```

**Basic Echo Server**:

```go
import "github.com/labstack/echo/v5"

func main() {
 e := echo.New()

 e.GET("/", func(c echo.Context) error {
  return c.JSON(200, map[string]string{
   "message": "Hello, Echo!",
  })
 })

 e.Logger.Fatal(e.Start(":8080"))
}
```

**REST API with Echo**:

```go
func main() {
 e := echo.New()

 e.GET("/users", listUsers)
 e.GET("/users/:id", getUser)
 e.POST("/users", createUser)
 e.PUT("/users/:id", updateUser)
 e.DELETE("/users/:id", deleteUser)

 e.Logger.Fatal(e.Start(":8080"))
}

func getUser(c echo.Context) error {
 id := c.Param("id")

 beneficiary, err := findUserByID(id)
 if err != nil {
  return echo.NewHTTPError(404, "Beneficiary not found")
 }

 return c.JSON(200, beneficiary)
}

func createUser(c echo.Context) error {
 beneficiary := new(Beneficiary)
 if err := c.Bind(beneficiary); err != nil {
  return echo.NewHTTPError(400, "Invalid request")
 }

 // Validate
 if err := c.Validate(beneficiary); err != nil {
  return echo.NewHTTPError(400, err.Error())
 }

 // Save beneficiary...

 return c.JSON(201, beneficiary)
}
```

### Fiber Framework

**Installation**:

```bash
go get -u github.com/gofiber/fiber/v2
```

**Basic Fiber Server**:

```go
import "github.com/gofiber/fiber/v2"

func main() {
 app := fiber.New()

 app.Get("/", func(c *fiber.Ctx) error {
  return c.JSON(fiber.Map{
   "message": "Hello, Fiber!",
  })
 })

 app.Listen(":8080")
}
```

**REST API with Fiber**:

```go
func main() {
 app := fiber.New()

 app.Get("/users", listUsers)
 app.Get("/users/:id", getUser)
 app.Post("/users", createUser)
 app.Put("/users/:id", updateUser)
 app.Delete("/users/:id", deleteUser)

 app.Listen(":8080")
}

func getUser(c *fiber.Ctx) error {
 id := c.Params("id")

 beneficiary, err := findUserByID(id)
 if err != nil {
  return c.Status(404).JSON(fiber.Map{
   "error": "Beneficiary not found",
  })
 }

 return c.JSON(beneficiary)
}

func createUser(c *fiber.Ctx) error {
 beneficiary := new(Beneficiary)
 if err := c.BodyParser(beneficiary); err != nil {
  return c.Status(400).JSON(fiber.Map{
   "error": "Invalid request",
  })
 }

 // Save beneficiary...

 return c.Status(201).JSON(beneficiary)
}
```

## gRPC

### Protocol Buffers

**Define service (.proto file)**:

```protobuf
syntax = "proto3";

package beneficiary;

option go_package = "github.com/username/project/pb";

message Beneficiary {
  int32 id = 1;
  string name = 2;
  string email = 3;
}

message GetUserRequest {
  int32 id = 1;
}

message ListUsersRequest {
  int32 page = 1;
  int32 page_size = 2;
}

message ListUsersResponse {
  repeated Beneficiary users = 1;
}

service UserService {
  rpc GetUser(GetUserRequest) returns (Beneficiary);
  rpc ListUsers(ListUsersRequest) returns (ListUsersResponse);
  rpc CreateUser(Beneficiary) returns (Beneficiary);
}
```

**Generate Go code**:

```bash
protoc --go_out=. --go-grpc_out=. beneficiary.proto
```

### gRPC Server

```go
import (
 "context"
 "log"
 "net"

 "google.golang.org/grpc"
 pb "github.com/username/project/pb"
)

type userServer struct {
 pb.UnimplementedUserServiceServer
}

func (s *userServer) GetUser(ctx context.Context, req *pb.GetUserRequest) (*pb.Beneficiary, error) {
 beneficiary := &pb.Beneficiary{
  Id:    req.Id,
  Name:  "Alice",
  Email: "alice@example.com",
 }
 return beneficiary, nil
}

func (s *userServer) ListUsers(ctx context.Context, req *pb.ListUsersRequest) (*pb.ListUsersResponse, error) {
 users := []*pb.Beneficiary{
  {Id: 1, Name: "Alice", Email: "alice@example.com"},
  {Id: 2, Name: "Bob", Email: "bob@example.com"},
 }
 return &pb.ListUsersResponse{Users: users}, nil
}

func main() {
 lis, err := net.Listen("tcp", ":50051")
 if err != nil {
  log.Fatalf("failed to listen: %v", err)
 }

 s := grpc.NewServer()
 pb.RegisterUserServiceServer(s, &userServer{})

 log.Println("gRPC server listening on :50051")
 if err := s.Serve(lis); err != nil {
  log.Fatalf("failed to serve: %v", err)
 }
}
```

### gRPC Client

```go
func main() {
 conn, err := grpc.Dial("localhost:50051", grpc.WithInsecure())
 if err != nil {
  log.Fatalf("did not connect: %v", err)
 }
 defer conn.Close()

 client := pb.NewUserServiceClient(conn)

 // GetUser
 beneficiary, err := client.GetUser(context.Background(), &pb.GetUserRequest{Id: 1})
 if err != nil {
  log.Fatalf("could not get beneficiary: %v", err)
 }
 fmt.Printf("Beneficiary: %v\n", beneficiary)

 // ListUsers
 users, err := client.ListUsers(context.Background(), &pb.ListUsersRequest{
  Page:     1,
  PageSize: 10,
 })
 if err != nil {
  log.Fatalf("could not list users: %v", err)
 }
 fmt.Printf("Users: %v\n", users.Users)
}
```

### Streaming

**Server Streaming**:

```protobuf
service UserService {
  rpc StreamUsers(ListUsersRequest) returns (stream Beneficiary);
}
```

```go
func (s *userServer) StreamUsers(req *pb.ListUsersRequest, stream pb.UserService_StreamUsersServer) error {
 users := getUsers()
 for _, beneficiary := range users {
  if err := stream.Send(beneficiary); err != nil {
   return err
  }
 }
 return nil
}
```

**Client Streaming**:

```protobuf
service UserService {
  rpc CreateUsers(stream Beneficiary) returns (CreateUsersResponse);
}
```

```go
func (s *userServer) CreateUsers(stream pb.UserService_CreateUsersServer) error {
 count := 0
 for {
  beneficiary, err := stream.Recv()
  if err == io.EOF {
   return stream.SendAndClose(&pb.CreateUsersResponse{
    Count: int32(count),
   })
  }
  if err != nil {
   return err
  }

  // Save beneficiary...
  count++
 }
}
```

## WebSockets

### gorilla/websocket

**Installation**:

```bash
go get -u github.com/gorilla/websocket
```

**WebSocket Server**:

```go
import (
 "github.com/gorilla/websocket"
 "net/http"
)

var upgrader = websocket.Upgrader{
 CheckOrigin: func(r *http.Request) bool {
  return true  // Allow all origins (be careful in production)
 },
}

func wsHandler(w http.ResponseWriter, r *http.Request) {
 conn, err := upgrader.Upgrade(w, r, nil)
 if err != nil {
  log.Println(err)
  return
 }
 defer conn.Close()

 for {
  messageType, message, err := conn.ReadMessage()
  if err != nil {
   log.Println(err)
   break
  }

  log.Printf("Received: %s", message)

  // Echo message back
  err = conn.WriteMessage(messageType, message)
  if err != nil {
   log.Println(err)
   break
  }
 }
}

func main() {
 http.HandleFunc("/ws", wsHandler)
 log.Fatal(http.ListenAndServe(":8080", nil))
}
```

**WebSocket Client**:

```go
func main() {
 url := "ws://localhost:8080/ws"
 conn, _, err := websocket.DefaultDialer.Dial(url, nil)
 if err != nil {
  log.Fatal(err)
 }
 defer conn.Close()

 // Send message
 err = conn.WriteMessage(websocket.TextMessage, []byte("Hello, Server!"))
 if err != nil {
  log.Fatal(err)
 }

 // Receive message
 _, message, err := conn.ReadMessage()
 if err != nil {
  log.Fatal(err)
 }
 log.Printf("Received: %s", message)
}
```

## Middleware Patterns

### Logging Middleware

```go
func loggingMiddleware(next http.Handler) http.Handler {
 return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
  start := time.Now()

  // Create response wrapper to capture status code
  rw := &responseWriter{ResponseWriter: w, statusCode: http.StatusOK}

  next.ServeHTTP(rw, r)

  duration := time.Since(start)
  log.Printf(
   "%s %s %d %v",
   r.Method,
   r.URL.Path,
   rw.statusCode,
   duration,
  )
 })
}

type responseWriter struct {
 http.ResponseWriter
 statusCode int
}

func (rw *responseWriter) WriteHeader(code int) {
 rw.statusCode = code
 rw.ResponseWriter.WriteHeader(code)
}
```

### CORS Middleware

```go
func corsMiddleware(next http.Handler) http.Handler {
 return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
  w.Header().Set("Access-Control-Allow-Origin", "*")
  w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")

  if r.Method == "OPTIONS" {
   w.WriteHeader(http.StatusNoContent)
   return
  }

  next.ServeHTTP(w, r)
 })
}
```

### Rate Limiting Middleware

```go
import "golang.org/x/time/rate"

func rateLimitMiddleware(limiter *rate.Limiter) func(http.Handler) http.Handler {
 return func(next http.Handler) http.Handler {
  return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
   if !limiter.Allow() {
    http.Error(w, "Rate limit exceeded", http.StatusTooManyRequests)
    return
   }
   next.ServeHTTP(w, r)
  })
 }
}

func main() {
 limiter := rate.NewLimiter(10, 20)  // 10 requests per second, burst of 20

 mux := http.NewServeMux()
 mux.HandleFunc("/", homeHandler)

 handler := rateLimitMiddleware(limiter)(mux)
 log.Fatal(http.ListenAndServe(":8080", handler))
}
```

## Testing

### httptest Package

**Testing Handlers**:

```go
import (
 "net/http"
 "net/http/httptest"
 "testing"
)

func TestHelloHandler(t *testing.T) {
 req := httptest.NewRequest("GET", "/hello?name=Alice", nil)
 rr := httptest.NewRecorder()

 helloHandler(rr, req)

 if status := rr.Code; status != http.StatusOK {
  t.Errorf("handler returned wrong status code: got %v want %v",
   status, http.StatusOK)
 }

 expected := "Hello, Alice!"
 if rr.Body.String() != expected {
  t.Errorf("handler returned unexpected body: got %v want %v",
   rr.Body.String(), expected)
 }
}
```

**Testing JSON APIs**:

```go
func TestCreateUserHandler(t *testing.T) {
 beneficiary := Beneficiary{Name: "Alice", Email: "alice@example.com"}
 jsonData, _ := json.Marshal(beneficiary)

 req := httptest.NewRequest("POST", "/users", bytes.NewBuffer(jsonData))
 req.Header.Set("Content-Type", "application/json")
 rr := httptest.NewRecorder()

 createUserHandler(rr, req)

 if status := rr.Code; status != http.StatusCreated {
  t.Errorf("handler returned wrong status code: got %v want %v",
   status, http.StatusCreated)
 }

 var responseUser Beneficiary
 err := json.NewDecoder(rr.Body).Decode(&responseUser)
 if err != nil {
  t.Fatalf("could not decode response: %v", err)
 }

 if responseUser.Name != beneficiary.Name {
  t.Errorf("unexpected name: got %v want %v",
   responseUser.Name, beneficiary.Name)
 }
}
```

### Integration Tests

```go
func TestServer(t *testing.T) {
 // Create test server
 ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
  w.WriteHeader(http.StatusOK)
  w.Write([]byte("OK"))
 }))
 defer ts.Close()

 // Make request to test server
 resp, err := http.Get(ts.URL)
 if err != nil {
  t.Fatal(err)
 }
 defer resp.Body.Close()

 if resp.StatusCode != http.StatusOK {
  t.Errorf("unexpected status: got %v want %v", resp.StatusCode, http.StatusOK)
 }

 body, _ := io.ReadAll(resp.Body)
 if string(body) != "OK" {
  t.Errorf("unexpected body: got %v want %v", string(body), "OK")
 }
}
```

## Best Practices

### Performance

**Connection Pooling**:

```go
var httpClient = &http.Client{
 Transport: &http.Transport{
  MaxIdleConns:        100,
  MaxIdleConnsPerHost: 100,
  IdleConnTimeout:     90 * time.Second,
 },
 Timeout: 10 * time.Second,
}
```

**Response Streaming**:

```go
func streamHandler(w http.ResponseWriter, r *http.Request) {
 flusher, ok := w.(http.Flusher)
 if !ok {
  http.Error(w, "Streaming unsupported", http.StatusInternalServerError)
  return
 }

 w.Header().Set("Content-Type", "text/event-stream")
 w.Header().Set("Cache-Control", "no-cache")
 w.Header().Set("Connection", "keep-alive")

 for i := 0; i < 10; i++ {
  fmt.Fprintf(w, "data: Message %d\n\n", i)
  flusher.Flush()
  time.Sleep(1 * time.Second)
 }
}
```

### Security

**TLS Configuration**:

```go
func main() {
 server := &http.Server{
  Addr:      ":443",
  TLSConfig: &tls.Config{
   MinVersion:               tls.VersionTLS13,
   CurvePreferences:         []tls.CurveID{tls.CurveP521, tls.CurveP384, tls.CurveP256},
   PreferServerCipherSuites: true,
  },
 }

 log.Fatal(server.ListenAndServeTLS("cert.pem", "key.pem"))
}
```

### Graceful Shutdown

```go
func main() {
 server := &http.Server{
  Addr:    ":8080",
  Handler: mux,
 }

 // Start server in goroutine
 go func() {
  if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
   log.Fatalf("listen: %s\n", err)
  }
 }()

 // Wait for interrupt signal
 quit := make(chan os.Signal, 1)
 signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
 <-quit
 log.Println("Shutting down server...")

 // Graceful shutdown with 5-second timeout
 ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
 defer cancel()

 if err := server.Shutdown(ctx); err != nil {
  log.Fatal("Server forced to shutdown:", err)
 }

 log.Println("Server exiting")
}
```

## Related Documentation

- [Concurrency and Parallelism](./ex-so-stla-go__concurrency-and-parallelism.md) - Handling concurrent requests
- [Error Handling](./ex-so-stla-go__error-handling.md) - Error handling in web services
- [Security](./ex-so-stla-go__security.md) - Web service security
- [Testing](./ex-so-stla-go__test-driven-development.md) - Testing web services

## Further Reading

- [net/http Package](https://pkg.go.dev/net/http) - Standard library HTTP
- [Gin Documentation](https://gin-gonic.com/docs/) - Gin framework guide
- [Echo Documentation](https://echo.labstack.com/guide/) - Echo framework guide
- [gRPC Go Tutorial](https://grpc.io/docs/languages/go/) - gRPC in Go
- [WebSocket](https://github.com/gorilla/websocket) - WebSocket library

---

**Last Updated**: 2026-01-23
**Go Version**: 1.21+ (baseline), 1.22+ (recommended), 1.23 (latest)
**Maintainers**: Platform Documentation Team
