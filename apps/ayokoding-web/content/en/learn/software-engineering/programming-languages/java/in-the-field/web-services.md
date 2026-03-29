---
title: "Web Services"
date: 2026-02-03T00:00:00+07:00
draft: false
description: Comprehensive guide to building REST APIs in Java from HTTP fundamentals to Spring Boot REST
weight: 10000018
tags: ["java", "rest", "web-services", "spring-boot", "jax-rs", "http"]
---

## Why REST APIs Matter

REST (Representational State Transfer) APIs are the standard for building web services that enable communication between distributed systems. Understanding HTTP fundamentals before using frameworks ensures you make informed architectural decisions.

**Core Benefits**:

- **Interoperability**: Platform and language independent communication
- **Scalability**: Stateless design enables horizontal scaling
- **Simplicity**: Leverages HTTP protocol familiar to all developers
- **Flexibility**: JSON payloads adapt to changing requirements
- **Cacheable**: HTTP caching improves performance

**Problem**: Building HTTP servers from scratch requires handling request parsing, routing, content negotiation, error handling, and serialization - tedious and error-prone without frameworks.

**Solution**: Use REST frameworks that provide routing, serialization, validation, and error handling with minimal boilerplate.

## REST Framework Comparison

| Framework            | Pros                                            | Cons                             | Use When                           |
| -------------------- | ----------------------------------------------- | -------------------------------- | ---------------------------------- |
| **Spring Boot REST** | Full-featured, huge ecosystem, production-ready | Learning curve, heavyweight      | Production REST APIs               |
| **JAX-RS**           | Standard specification, portable                | Requires implementation          | Enterprise Java (Jakarta EE)       |
| **Servlet API**      | Built-in, low-level control                     | Verbose, manual serialization    | Understanding fundamentals         |
| **HttpServer (JDK)** | No dependencies, simple                         | Very basic, no routing framework | Learning HTTP basics, simple tools |
| **Manual sockets**   | Complete control                                | Implement HTTP protocol manually | Educational purposes only          |

**Recommendation**: Use Spring Boot REST for production applications - it's the modern standard with excellent tooling and ecosystem.

**Recommended progression**: Start with JDK HttpServer to understand HTTP fundamentals → Learn Servlet API for request/response handling → Explore JAX-RS annotations → Use Spring Boot REST for production.

## HTTP Fundamentals Quick Reference

**Foundation**: HTTP basics (request/response structure, methods, status codes) are covered in [by-example intermediate section](/en/learn/software-engineering/programming-languages/java/by-example/intermediate#http-client). This guide focuses on REST API design and framework usage.

| Method  | Purpose         | Idempotent | Common Status Codes   |
| ------- | --------------- | ---------- | --------------------- |
| GET     | Retrieve        | Yes        | 200 (OK), 404         |
| POST    | Create          | No         | 201 (Created), 400    |
| PUT     | Replace         | Yes        | 200 (OK), 404         |
| PATCH   | Update          | No         | 200 (OK), 404         |
| DELETE  | Remove          | Yes        | 204 (No Content), 404 |
| HEAD    | Get headers     | Yes        | 200 (OK)              |
| OPTIONS | Allowed methods | Yes        | 200 (OK)              |

**Status code categories**:

- **2xx**: Success (200 OK, 201 Created, 204 No Content)
- **4xx**: Client error (400 Bad Request, 401 Unauthorized, 404 Not Found)
- **5xx**: Server error (500 Internal Error, 503 Unavailable)

## HTTP Server (JDK Built-in)

Java's standard library includes com.sun.net.httpserver for basic HTTP servers. Use this to understand HTTP fundamentals.

### Basic HTTP Server

Create simple HTTP server using JDK classes.

**Pattern**:

```java
import com.sun.net.httpserver.HttpServer;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpExchange;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;

public class SimpleHttpServer {
    public static void main(String[] args) throws IOException {
        // Create server on port 8000
        HttpServer server = HttpServer.create(new InetSocketAddress(8000), 0);

        // Register handlers
        server.createContext("/hello", new HelloHandler());
        server.createContext("/api/users", new UserHandler());

        // Start server
        server.setExecutor(null); // Use default executor
        server.start();

        System.out.println("Server started on http://localhost:8000");
    }

    static class HelloHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            String response = "Hello, World!";
            exchange.sendResponseHeaders(200, response.length());

            try (OutputStream os = exchange.getResponseBody()) {
                os.write(response.getBytes(StandardCharsets.UTF_8));
            }
        }
    }

    static class UserHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            String method = exchange.getRequestMethod();

            if ("GET".equals(method)) {
                handleGet(exchange);
            } else if ("POST".equals(method)) {
                handlePost(exchange);
            } else {
                // Method not allowed
                exchange.sendResponseHeaders(405, -1);
            }
        }

        private void handleGet(HttpExchange exchange) throws IOException {
            String response = "{\"users\":[{\"id\":1,\"name\":\"Alice\"}]}";
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.sendResponseHeaders(200, response.length());

            try (OutputStream os = exchange.getResponseBody()) {
                os.write(response.getBytes(StandardCharsets.UTF_8));
            }
        }

        private void handlePost(HttpExchange exchange) throws IOException {
            // Read request body
            String body = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
            System.out.println("Received: " + body);

            // Send response
            String response = "{\"id\":2,\"name\":\"Bob\"}";
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.sendResponseHeaders(201, response.length());

            try (OutputStream os = exchange.getResponseBody()) {
                os.write(response.getBytes(StandardCharsets.UTF_8));
            }
        }
    }
}
```

**Test**:

```bash
# GET request
curl http://localhost:8000/hello
# Output: Hello, World!

# GET users
curl http://localhost:8000/api/users
# Output: {"users":[{"id":1,"name":"Alice"}]}

# POST user
curl -X POST http://localhost:8000/api/users \
  -H "Content-Type: application/json" \
  -d '{"name":"Bob"}'
# Output: {"id":2,"name":"Bob"}
```

### Why Built-in Server is Limited

**Limitations**:

1. **No routing framework**: Manual path parsing and dispatching
2. **Manual serialization**: Convert JSON manually
3. **No validation**: Validate request data manually
4. **No content negotiation**: Handle Accept headers manually
5. **Limited error handling**: Build error responses manually
6. **No dependency injection**: Wire dependencies manually
7. **Basic threading**: Simple executor, no advanced async handling

**Before**: HttpServer with manual request handling
**After**: Frameworks with routing, serialization, validation, and error handling

## REST Design Principles

REST APIs follow architectural principles for consistency and scalability.

### Resource-Oriented Design

Design APIs around resources (nouns) rather than actions (verbs).

**Good design** (resource-oriented):

```
GET    /api/payments           # List payments
GET    /api/payments/123       # Get payment by ID
POST   /api/payments           # Create payment
PUT    /api/payments/123       # Replace payment
PATCH  /api/payments/123       # Update payment
DELETE /api/payments/123       # Delete payment

GET    /api/users/456/payments # User's payments (sub-resource)
```

**Bad design** (RPC-style):

```
POST /api/getPayment            # Use GET instead
POST /api/createPayment         # Use POST /api/payments
POST /api/deletePayment         # Use DELETE /api/payments/123
GET  /api/getUserPayments       # Use GET /api/users/456/payments
```

### Idempotency

Idempotent operations produce same result when repeated.

**Idempotent methods**: GET, PUT, DELETE, HEAD, OPTIONS
**Non-idempotent methods**: POST, PATCH

**Example**:

```
PUT /api/payments/123
{"amount": 100, "status": "completed"}

# First call: Updates payment to completed
# Second call: Payment already completed, same result
# Idempotent: Safe to retry
```

```
POST /api/payments
{"amount": 100}

# First call: Creates payment with ID 123
# Second call: Creates payment with ID 124 (different!)
# Not idempotent: Retry creates duplicates
```

**Idempotency keys** (for POST):

```
POST /api/payments
Idempotency-Key: unique-key-123
{"amount": 100}

# Server tracks idempotency key
# Repeated requests with same key return same response
# Solves POST idempotency problem
```

### HATEOAS (Hypermedia)

HATEOAS (Hypermedia as the Engine of Application State) includes links to related resources in responses.

**Example**:

```json
{
  "id": "payment-123",
  "amount": 100.5,
  "status": "completed",
  "links": {
    "self": "/api/payments/payment-123",
    "customer": "/api/customers/customer-456",
    "invoice": "/api/invoices/invoice-789",
    "refund": "/api/payments/payment-123/refund"
  }
}
```

**Benefits**:

- Clients discover related resources
- API evolution without breaking clients
- Self-documenting API structure

**Note**: HATEOAS is optional in REST. Many APIs use simpler approaches.

## Servlet API (Standard - Jakarta EE)

The Servlet API provides standard request/response handling in Java web containers (Tomcat, Jetty). It's the foundation for most Java web frameworks.

### Basic Servlet

Create HTTP endpoint by extending HttpServlet.

**Pattern**:

```java
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.io.PrintWriter;

@WebServlet("/api/hello")
public class HelloServlet extends HttpServlet {

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // Set response content type
        response.setContentType("text/plain");
        response.setCharacterEncoding("UTF-8");

        // Write response
        try (PrintWriter writer = response.getWriter()) {
            writer.println("Hello, World!");
        }
    }
}
```

**See**: [JSON and API Integration](/en/learn/software-engineering/programming-languages/java/in-the-field/json-and-api-integration) for JSON serialization with Jackson.

### Request Parameters

Access query parameters and path variables.

**Query parameters**:

```java
@WebServlet("/api/search")
public class SearchServlet extends HttpServlet {

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // GET /api/search?query=java&limit=10
        String query = request.getParameter("query");       // "java"
        String limitStr = request.getParameter("limit");    // "10"

        int limit = limitStr != null ? Integer.parseInt(limitStr) : 20;

        // Process search...
        String result = performSearch(query, limit);

        response.setContentType("application/json");
        try (PrintWriter writer = response.getWriter()) {
            writer.println(result);
        }
    }
}
```

**Path variables** (manual parsing):

```java
@WebServlet("/api/users/*")
public class UserServlet extends HttpServlet {

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // GET /api/users/123
        String pathInfo = request.getPathInfo(); // "/123"
        String userId = pathInfo.substring(1);    // "123"

        // Get user by ID...
        String userJson = getUserById(userId);

        response.setContentType("application/json");
        try (PrintWriter writer = response.getWriter()) {
            writer.println(userJson);
        }
    }
}
```

### Request Body (JSON)

Read JSON request body and parse manually.

**Pattern**:

```java
import java.io.BufferedReader;
import java.util.stream.Collectors;

@WebServlet("/api/users")
public class UserServlet extends HttpServlet {

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // Read request body
        String body;
        try (BufferedReader reader = request.getReader()) {
            body = reader.lines().collect(Collectors.joining());
        }

        // Parse JSON (use Jackson - see JSON integration guide)
        // User user = objectMapper.readValue(body, User.class);

        // Create user...
        String createdUserJson = createUser(body);

        // Send response
        response.setStatus(HttpServletResponse.SC_CREATED);
        response.setContentType("application/json");
        try (PrintWriter writer = response.getWriter()) {
            writer.println(createdUserJson);
        }
    }
}
```

### Error Handling

Send error responses with appropriate status codes.

**Pattern**:

```java
@WebServlet("/api/payments")
public class PaymentServlet extends HttpServlet {

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        String paymentId = request.getParameter("id");

        try {
            if (paymentId == null || paymentId.isEmpty()) {
                // 400 Bad Request
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                response.setContentType("application/json");
                try (PrintWriter writer = response.getWriter()) {
                    writer.println("{\"error\":\"Payment ID required\"}");
                }
                return;
            }

            String payment = getPaymentById(paymentId);

            if (payment == null) {
                // 404 Not Found
                response.setStatus(HttpServletResponse.SC_NOT_FOUND);
                response.setContentType("application/json");
                try (PrintWriter writer = response.getWriter()) {
                    writer.println("{\"error\":\"Payment not found\"}");
                }
                return;
            }

            // 200 OK
            response.setContentType("application/json");
            try (PrintWriter writer = response.getWriter()) {
                writer.println(payment);
            }

        } catch (Exception e) {
            // 500 Internal Server Error
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.setContentType("application/json");
            try (PrintWriter writer = response.getWriter()) {
                writer.println("{\"error\":\"Internal server error\"}");
            }
        }
    }
}
```

### Why Servlets are Verbose

**Limitations**:

1. **Manual routing**: Path parsing and dispatching
2. **Manual serialization**: JSON conversion by hand
3. **No validation**: Validate manually
4. **Boilerplate**: Repetitive status codes, content types, error handling
5. **No dependency injection**: Manage dependencies manually

**Before**: Servlet API with manual request/response handling
**After**: JAX-RS or Spring Boot with annotations, automatic serialization, and validation

## JAX-RS (Standard - Jakarta EE)

JAX-RS is the standard specification for building REST APIs in Java. It uses annotations to reduce boilerplate.

**Popular implementations**: Jersey (reference), RESTEasy (JBoss), Apache CXF

### Basic JAX-RS Resource

Define REST endpoint using annotations.

**Pattern** (using Jersey):

**Maven dependencies**:

```xml
<dependencies>
    <!-- Jersey implementation -->
    <dependency>
        <groupId>org.glassfish.jersey.core</groupId>
        <artifactId>jersey-server</artifactId>
        <version>3.1.5</version>
    </dependency>

    <dependency>
        <groupId>org.glassfish.jersey.containers</groupId>
        <artifactId>jersey-container-servlet</artifactId>
        <version>3.1.5</version>
    </dependency>

    <!-- JSON support with Jackson -->
    <dependency>
        <groupId>org.glassfish.jersey.media</groupId>
        <artifactId>jersey-media-json-jackson</artifactId>
        <version>3.1.5</version>
    </dependency>
</dependencies>
```

**Resource class**:

```java
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;

@Path("/api/users")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class UserResource {

    @GET
    public Response listUsers() {
        List<User> users = userService.findAll();
        return Response.ok(users).build();
    }

    @GET
    @Path("/{id}")
    public Response getUser(@PathParam("id") String id) {
        User user = userService.findById(id);

        if (user == null) {
            return Response.status(Response.Status.NOT_FOUND)
                .entity(new ErrorResponse("User not found"))
                .build();
        }

        return Response.ok(user).build();
    }

    @POST
    public Response createUser(User user) {
        User created = userService.create(user);

        return Response.status(Response.Status.CREATED)
            .entity(created)
            .build();
    }

    @PUT
    @Path("/{id}")
    public Response updateUser(@PathParam("id") String id, User user) {
        User updated = userService.update(id, user);

        return Response.ok(updated).build();
    }

    @DELETE
    @Path("/{id}")
    public Response deleteUser(@PathParam("id") String id) {
        userService.delete(id);

        return Response.noContent().build();
    }
}
```

### Path and Query Parameters

Access URL parameters using annotations.

**Pattern**:

```java
@Path("/api/search")
public class SearchResource {

    // Path parameter: /api/search/products/electronics
    @GET
    @Path("/{category}")
    public Response search(
            @PathParam("category") String category,
            @QueryParam("query") String query,
            @QueryParam("limit") @DefaultValue("20") int limit,
            @QueryParam("offset") @DefaultValue("0") int offset) {

        List<Product> products = searchService.search(category, query, limit, offset);

        return Response.ok(products).build();
    }

    // Matrix parameter: /api/search/filter;minPrice=10;maxPrice=100
    @GET
    @Path("/filter")
    public Response filter(
            @MatrixParam("minPrice") BigDecimal minPrice,
            @MatrixParam("maxPrice") BigDecimal maxPrice) {

        List<Product> products = searchService.filter(minPrice, maxPrice);

        return Response.ok(products).build();
    }
}
```

### Content Negotiation

Support multiple content types using @Produces and @Consumes.

**Pattern**:

```java
@Path("/api/products")
public class ProductResource {

    // Supports both JSON and XML responses
    @GET
    @Path("/{id}")
    @Produces({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
    public Response getProduct(@PathParam("id") String id) {
        Product product = productService.findById(id);
        return Response.ok(product).build();
    }

    // Accept JSON or XML request body
    @POST
    @Consumes({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
    @Produces(MediaType.APPLICATION_JSON)
    public Response createProduct(Product product) {
        Product created = productService.create(product);
        return Response.status(Response.Status.CREATED)
            .entity(created)
            .build();
    }
}
```

**Client request**:

```bash
# Request JSON
curl -H "Accept: application/json" http://localhost:8080/api/products/123

# Request XML
curl -H "Accept: application/xml" http://localhost:8080/api/products/123
```

### Exception Mapping

Map exceptions to HTTP responses.

**Exception mapper**:

```java
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

@Provider
public class ResourceNotFoundExceptionMapper
        implements ExceptionMapper<ResourceNotFoundException> {

    @Override
    public Response toResponse(ResourceNotFoundException exception) {
        ErrorResponse error = new ErrorResponse(
            "Not Found",
            exception.getMessage()
        );

        return Response.status(Response.Status.NOT_FOUND)
            .entity(error)
            .build();
    }
}

@Provider
public class ValidationExceptionMapper
        implements ExceptionMapper<ValidationException> {

    @Override
    public Response toResponse(ValidationException exception) {
        ErrorResponse error = new ErrorResponse(
            "Validation Failed",
            exception.getMessage()
        );

        return Response.status(Response.Status.BAD_REQUEST)
            .entity(error)
            .build();
    }
}
```

**Usage**:

```java
@Path("/api/users")
public class UserResource {

    @GET
    @Path("/{id}")
    public Response getUser(@PathParam("id") String id) {
        User user = userService.findById(id);

        // Exception automatically mapped to 404
        if (user == null) {
            throw new ResourceNotFoundException("User not found: " + id);
        }

        return Response.ok(user).build();
    }
}
```

## Spring Boot REST (Production Framework)

Spring Boot REST is the modern standard for building production REST APIs with minimal configuration.

### Adding Spring Boot Web

**Maven dependencies**:

```xml
<parent>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-parent</artifactId>
    <version>3.2.1</version>
</parent>

<dependencies>
    <!-- Spring Boot Web (includes Tomcat, Jackson, validation) -->
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-web</artifactId>
    </dependency>

    <!-- Validation -->
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-validation</artifactId>
    </dependency>
</dependencies>
```

### Basic REST Controller

Create REST endpoints using @RestController.

**Pattern**:

```java
import org.springframework.web.bind.annotation.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.List;

@RestController
@RequestMapping("/api/users")
public class UserController {

    private final UserService userService;

    // Constructor injection (recommended)
    public UserController(UserService userService) {
        this.userService = userService;
    }

    @GetMapping
    public List<User> listUsers() {
        return userService.findAll();
    }

    @GetMapping("/{id}")
    public User getUser(@PathVariable String id) {
        return userService.findById(id);
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public User createUser(@RequestBody User user) {
        return userService.create(user);
    }

    @PutMapping("/{id}")
    public User updateUser(@PathVariable String id, @RequestBody User user) {
        return userService.update(id, user);
    }

    @DeleteMapping("/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void deleteUser(@PathVariable String id) {
        userService.delete(id);
    }
}
```

**Automatic features**:

- JSON serialization (Jackson automatic)
- Dependency injection (@Autowired on constructor)
- Exception handling (optional @ControllerAdvice)
- Content negotiation (automatic based on Accept header)

### Request Validation

Validate request bodies using Bean Validation annotations.

**User DTO**:

```java
import jakarta.validation.constraints.*;

public class CreateUserRequest {

    @NotBlank(message = "Username is required")
    @Size(min = 3, max = 20, message = "Username must be between 3 and 20 characters")
    private String username;

    @NotBlank(message = "Email is required")
    @Email(message = "Email must be valid")
    private String email;

    @NotNull(message = "Age is required")
    @Min(value = 18, message = "Age must be at least 18")
    @Max(value = 120, message = "Age must be at most 120")
    private Integer age;

    // Getters and setters...
}
```

**Controller**:

```java
import jakarta.validation.Valid;

@RestController
@RequestMapping("/api/users")
public class UserController {

    @PostMapping
    public ResponseEntity<User> createUser(@Valid @RequestBody CreateUserRequest request) {
        User user = userService.create(request);

        return ResponseEntity.status(HttpStatus.CREATED)
            .body(user);
    }
}
```

**Automatic validation error response**:

```bash
curl -X POST http://localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{"username":"ab","email":"invalid","age":15}'

# Response: 400 Bad Request
{
  "timestamp": "2026-02-03T14:30:00.000+00:00",
  "status": 400,
  "error": "Bad Request",
  "errors": [
    {
      "field": "username",
      "message": "Username must be between 3 and 20 characters"
    },
    {
      "field": "email",
      "message": "Email must be valid"
    },
    {
      "field": "age",
      "message": "Age must be at least 18"
    }
  ]
}
```

### Exception Handling with @ControllerAdvice

Centralize exception handling for all controllers.

**Global exception handler**:

```java
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@RestControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(ResourceNotFoundException.class)
    public ResponseEntity<ErrorResponse> handleResourceNotFound(ResourceNotFoundException ex) {
        ErrorResponse error = new ErrorResponse(
            LocalDateTime.now(),
            HttpStatus.NOT_FOUND.value(),
            "Not Found",
            ex.getMessage()
        );

        return ResponseEntity.status(HttpStatus.NOT_FOUND)
            .body(error);
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ValidationErrorResponse> handleValidationErrors(
            MethodArgumentNotValidException ex) {

        List<FieldError> errors = ex.getBindingResult()
            .getFieldErrors()
            .stream()
            .map(error -> new FieldError(
                error.getField(),
                error.getDefaultMessage()
            ))
            .collect(Collectors.toList());

        ValidationErrorResponse response = new ValidationErrorResponse(
            LocalDateTime.now(),
            HttpStatus.BAD_REQUEST.value(),
            "Validation Failed",
            errors
        );

        return ResponseEntity.badRequest()
            .body(response);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleGenericException(Exception ex) {
        ErrorResponse error = new ErrorResponse(
            LocalDateTime.now(),
            HttpStatus.INTERNAL_SERVER_ERROR.value(),
            "Internal Server Error",
            "An unexpected error occurred"
        );

        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
            .body(error);
    }
}
```

**Error response classes**:

```java
public record ErrorResponse(
    LocalDateTime timestamp,
    int status,
    String error,
    String message
) {}

public record FieldError(
    String field,
    String message
) {}

public record ValidationErrorResponse(
    LocalDateTime timestamp,
    int status,
    String error,
    List<FieldError> errors
) {}
```

### Query Parameters and Pagination

Handle query parameters and pagination.

**Pattern**:

```java
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;

@RestController
@RequestMapping("/api/users")
public class UserController {

    @GetMapping("/search")
    public Page<User> searchUsers(
            @RequestParam(required = false) String query,
            @RequestParam(required = false) String role,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(defaultValue = "username") String sortBy) {

        PageRequest pageRequest = PageRequest.of(page, size, Sort.by(sortBy));

        return userService.search(query, role, pageRequest);
    }
}
```

**Request**:

```bash
curl "http://localhost:8080/api/users/search?query=john&role=admin&page=0&size=10&sortBy=createdAt"
```

**Response**:

```json
{
  "content": [{ "id": "user-1", "username": "john_admin", "role": "admin" }],
  "pageable": {
    "pageNumber": 0,
    "pageSize": 10,
    "sort": { "sorted": true, "unsorted": false }
  },
  "totalElements": 1,
  "totalPages": 1,
  "last": true,
  "first": true,
  "numberOfElements": 1
}
```

## REST Client Patterns

Consume REST APIs from Java applications.

### HttpClient (Standard Library - Java 11+)

Use built-in HttpClient for HTTP requests.

**Pattern**:

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;

public class RestClientExample {

    private final HttpClient httpClient = HttpClient.newBuilder()
        .version(HttpClient.Version.HTTP_2)
        .connectTimeout(Duration.ofSeconds(10))
        .build();

    public String getUser(String userId) throws Exception {
        HttpRequest request = HttpRequest.newBuilder()
            .uri(URI.create("https://api.example.com/users/" + userId))
            .header("Accept", "application/json")
            .header("Authorization", "Bearer token123")
            .timeout(Duration.ofSeconds(5))
            .GET()
            .build();

        HttpResponse<String> response = httpClient.send(
            request,
            HttpResponse.BodyHandlers.ofString()
        );

        if (response.statusCode() != 200) {
            throw new RuntimeException("Request failed: " + response.statusCode());
        }

        return response.body();
    }

    public String createUser(String userJson) throws Exception {
        HttpRequest request = HttpRequest.newBuilder()
            .uri(URI.create("https://api.example.com/users"))
            .header("Content-Type", "application/json")
            .header("Authorization", "Bearer token123")
            .timeout(Duration.ofSeconds(5))
            .POST(HttpRequest.BodyPublishers.ofString(userJson))
            .build();

        HttpResponse<String> response = httpClient.send(
            request,
            HttpResponse.BodyHandlers.ofString()
        );

        if (response.statusCode() != 201) {
            throw new RuntimeException("Request failed: " + response.statusCode());
        }

        return response.body();
    }
}
```

### RestTemplate (Spring)

Use Spring's RestTemplate for simplified HTTP requests.

**Pattern**:

```java
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.*;
import org.springframework.web.client.RestTemplate;

import java.time.Duration;

public class UserClient {

    private final RestTemplate restTemplate;

    public UserClient(RestTemplateBuilder builder) {
        this.restTemplate = builder
            .rootUri("https://api.example.com")
            .setConnectTimeout(Duration.ofSeconds(10))
            .setReadTimeout(Duration.ofSeconds(5))
            .defaultHeader("Authorization", "Bearer token123")
            .build();
    }

    public User getUser(String userId) {
        return restTemplate.getForObject("/users/{id}", User.class, userId);
    }

    public User createUser(CreateUserRequest request) {
        return restTemplate.postForObject("/users", request, User.class);
    }

    public void updateUser(String userId, UpdateUserRequest request) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        HttpEntity<UpdateUserRequest> entity = new HttpEntity<>(request, headers);

        restTemplate.exchange(
            "/users/{id}",
            HttpMethod.PUT,
            entity,
            Void.class,
            userId
        );
    }

    public void deleteUser(String userId) {
        restTemplate.delete("/users/{id}", userId);
    }
}
```

### WebClient (Spring WebFlux - Reactive)

Use WebClient for reactive HTTP requests.

**Maven dependency**:

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-webflux</artifactId>
</dependency>
```

**Pattern**:

```java
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

public class UserClient {

    private final WebClient webClient;

    public UserClient(WebClient.Builder builder) {
        this.webClient = builder
            .baseUrl("https://api.example.com")
            .defaultHeader("Authorization", "Bearer token123")
            .build();
    }

    public Mono<User> getUser(String userId) {
        return webClient.get()
            .uri("/users/{id}", userId)
            .retrieve()
            .bodyToMono(User.class);
    }

    public Mono<User> createUser(CreateUserRequest request) {
        return webClient.post()
            .uri("/users")
            .bodyValue(request)
            .retrieve()
            .bodyToMono(User.class);
    }

    public Mono<Void> deleteUser(String userId) {
        return webClient.delete()
            .uri("/users/{id}", userId)
            .retrieve()
            .bodyToMono(Void.class);
    }
}
```

## Best Practices

### API Versioning

Version APIs to support backward compatibility.

**URL versioning** (recommended for REST):

```java
@RestController
@RequestMapping("/api/v1/users")
public class UserV1Controller {
    // Version 1 endpoints
}

@RestController
@RequestMapping("/api/v2/users")
public class UserV2Controller {
    // Version 2 endpoints (breaking changes)
}
```

**Header versioning**:

```java
@RestController
@RequestMapping("/api/users")
public class UserController {

    @GetMapping(headers = "API-Version=1")
    public UserV1 getUserV1(@PathVariable String id) {
        // Version 1 logic
    }

    @GetMapping(headers = "API-Version=2")
    public UserV2 getUserV2(@PathVariable String id) {
        // Version 2 logic
    }
}
```

### Rate Limiting

Protect APIs from abuse with rate limiting.

**Spring Boot with Bucket4j**:

```java
import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket;
import io.github.bucket4j.Refill;

import jakarta.servlet.*;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class RateLimitFilter implements Filter {

    private final Map<String, Bucket> buckets = new ConcurrentHashMap<>();

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {

        String clientId = getClientId(request);
        Bucket bucket = buckets.computeIfAbsent(clientId, this::createBucket);

        if (bucket.tryConsume(1)) {
            chain.doFilter(request, response);
        } else {
            HttpServletResponse httpResponse = (HttpServletResponse) response;
            httpResponse.setStatus(429); // Too Many Requests
            httpResponse.getWriter().write("{\"error\":\"Rate limit exceeded\"}");
        }
    }

    private Bucket createBucket(String clientId) {
        // 100 requests per minute
        Bandwidth limit = Bandwidth.classic(100, Refill.intervally(100, Duration.ofMinutes(1)));
        return Bucket.builder()
            .addLimit(limit)
            .build();
    }

    private String getClientId(ServletRequest request) {
        // Use IP address or API key
        return request.getRemoteAddr();
    }
}
```

### CORS Configuration

Enable Cross-Origin Resource Sharing for browser clients.

**Spring Boot**:

```java
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;

import java.util.List;

@Configuration
public class CorsConfig {

    @Bean
    public CorsFilter corsFilter() {
        CorsConfiguration config = new CorsConfiguration();

        // Allow specific origins (not * in production!)
        config.setAllowedOrigins(List.of("https://example.com"));

        // Allow credentials (cookies, authorization headers)
        config.setAllowCredentials(true);

        // Allow specific methods
        config.setAllowedMethods(List.of("GET", "POST", "PUT", "DELETE", "OPTIONS"));

        // Allow specific headers
        config.setAllowedHeaders(List.of("Authorization", "Content-Type"));

        // Expose specific headers to client
        config.setExposedHeaders(List.of("X-Total-Count"));

        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        source.registerCorsConfiguration("/api/**", config);

        return new CorsFilter(source);
    }
}
```

### Security (Authentication & Authorization)

Secure APIs with authentication and authorization.

**Spring Security (JWT)**:

```java
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;

@EnableWebSecurity
public class SecurityConfig {

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http
            .csrf().disable()
            .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS)
            .and()
            .authorizeHttpRequests(auth -> auth
                .requestMatchers("/api/public/**").permitAll()
                .requestMatchers("/api/admin/**").hasRole("ADMIN")
                .anyRequest().authenticated()
            )
            .oauth2ResourceServer().jwt();

        return http.build();
    }
}
```

**See**: [Security Practices](/en/learn/software-engineering/programming-languages/java/in-the-field/security-practices) for comprehensive security guidelines.

## Testing REST APIs

### MockMvc (Spring)

Test REST controllers without starting server.

**Pattern**:

```java
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(UserController.class)
class UserControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private UserService userService;

    @Test
    void getUser_returnsUser() throws Exception {
        User user = new User("user-1", "Alice");
        when(userService.findById("user-1")).thenReturn(user);

        mockMvc.perform(get("/api/users/user-1"))
            .andExpect(status().isOk())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(jsonPath("$.id").value("user-1"))
            .andExpect(jsonPath("$.username").value("Alice"));
    }

    @Test
    void createUser_returnsCreated() throws Exception {
        User created = new User("user-2", "Bob");
        when(userService.create(any())).thenReturn(created);

        String requestBody = "{\"username\":\"Bob\"}";

        mockMvc.perform(post("/api/users")
                .contentType(MediaType.APPLICATION_JSON)
                .content(requestBody))
            .andExpect(status().isCreated())
            .andExpect(jsonPath("$.id").value("user-2"))
            .andExpect(jsonPath("$.username").value("Bob"));
    }

    @Test
    void getUser_notFound_returns404() throws Exception {
        when(userService.findById("nonexistent")).thenReturn(null);

        mockMvc.perform(get("/api/users/nonexistent"))
            .andExpect(status().isNotFound());
    }
}
```

## Related Content

- [JSON and API Integration](/en/learn/software-engineering/programming-languages/java/in-the-field/json-and-api-integration) - JSON serialization with Jackson
- [Security Practices](/en/learn/software-engineering/programming-languages/java/in-the-field/security-practices) - API security and authentication
- [Test-Driven Development](/en/learn/software-engineering/programming-languages/java/in-the-field/test-driven-development) - Testing REST APIs
- [Cloud-Native Patterns](/en/learn/software-engineering/programming-languages/java/in-the-field/cloud-native-patterns) - Health checks and observability
- [Docker and Kubernetes](/en/learn/software-engineering/programming-languages/java/in-the-field/docker-and-kubernetes) - Deploying REST APIs
