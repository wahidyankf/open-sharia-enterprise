---
title: "Intermediate"
date: 2025-12-23T00:00:00+07:00
draft: false
weight: 10000002
description: "Master intermediate Java through 30 examples: advanced OOP, generics, functional programming, streams, file I/O, testing, and concurrency patterns"
tags:
  [
    "java",
    "tutorial",
    "by-example",
    "intermediate",
    "generics",
    "streams",
    "concurrency",
    "testing",
    "functional-programming",
  ]
---

Master intermediate Java concepts through 30 annotated code examples. Each example builds on beginner foundations, introducing advanced OOP, generics, functional programming, and concurrency patterns.

## Group 1: Advanced OOP

### Example 16: Encapsulation and JavaBeans

Encapsulation hides internal state behind controlled accessors. JavaBeans follow naming conventions for getters/setters. Records (Java 14+) provide concise immutable data classes.

**Code**:

```java
// Traditional JavaBeans pattern - encapsulation through private fields
public class Person {
    // Private fields - encapsulated state hidden from external access
    private String name;                 // => Inaccessible directly from outside class
    private int age;                     // => Must use getters/setters for access

    // Default constructor (no parameters) - required for JavaBeans
    public Person() {                    // => Creates Person with null name, 0 age
        // Fields initialized to default values (null for objects, 0 for primitives)
    }

    // Parameterized constructor for convenience
    public Person(String name, int age) {  // => Creates fully initialized Person
        this.name = name;                  // => 'this' refers to instance field
        this.age = age;                    // => Distinguishes field from parameter
    }

    // Getter methods (accessors) - provide read access to private fields
    public String getName() {              // => JavaBean naming: get + PropertyName
        return name;                       // => Returns current name value
    }                                      // => Return type matches field type (String)

    public int getAge() {                  // => JavaBean naming: get + PropertyName
        return age;                        // => Returns current age value
    }                                      // => Return type matches field type (int)

    // Setter methods (mutators) - provide controlled write access to private fields
    public void setName(String name) {     // => JavaBean naming: set + PropertyName
        this.name = name;                  // => 'this.name' = field, 'name' = parameter
    }                                      // => Void return (standard JavaBean pattern)

    public void setAge(int age) {          // => Setter with validation logic
        if (age >= 0) {                    // => Business rule: age cannot be negative
            this.age = age;                // => Update field only if validation passes
        } else {                           // => Reject invalid input
            throw new IllegalArgumentException("Age cannot be negative");
        }                                  // => Exception prevents invalid state
    }                                      // => Encapsulation enforces invariants

    // toString() for debugging and logging
    @Override
    public String toString() {             // => Overrides Object.toString()
        return "Person{name='" + name + "', age=" + age + "}";
    }                                      // => Returns human-readable representation
}

// Using JavaBeans pattern
Person person = new Person();              // => person: name=null, age=0 (default values)
person.setName("Alice");                   // => person: name="Alice", age=0
person.setAge(30);                         // => person: name="Alice", age=30

String name = person.getName();            // => name is "Alice" (read via getter)
int age = person.getAge();                 // => age is 30 (read via getter)

// Attempting invalid state triggers exception
try {
    person.setAge(-5);                     // => Validation fails (age < 0)
} catch (IllegalArgumentException e) {     // => Exception caught
    System.out.println(e.getMessage());    // => Output: "Age cannot be negative"
}                                          // => person.age remains 30 (unchanged)

// Builder pattern for complex object construction with many optional fields
public class User {
    // Final fields - immutable once constructed (cannot be reassigned)
    private final String username;         // => Required field (set in constructor)
    private final String email;            // => Required field (set in constructor)
    private final int age;                 // => Optional field (has default in Builder)
    private final String address;          // => Optional field (has default in Builder)

    // Private constructor - only Builder can create User instances
    private User(Builder builder) {        // => Accepts Builder as single parameter
        this.username = builder.username;  // => Copy username from builder
        this.email = builder.email;        // => Copy email from builder
        this.age = builder.age;            // => Copy age from builder (may be default)
        this.address = builder.address;    // => Copy address from builder (may be default)
    }                                      // => All fields final (immutable after construction)

    // Public getters - no setters (immutable object)
    public String getUsername() {          // => Returns username (cannot be changed)
        return username;
    }

    public String getEmail() {             // => Returns email (cannot be changed)
        return email;
    }

    public int getAge() {                  // => Returns age (cannot be changed)
        return age;
    }

    public String getAddress() {           // => Returns address (cannot be changed)
        return address;
    }

    // Static nested Builder class - constructs User instances fluently
    public static class Builder {          // => Nested class for building User objects
        // Mutable fields in builder (not final, will be copied to User)
        private String username;           // => Required field (set in constructor)
        private String email;              // => Required field (set in constructor)
        private int age = 0;               // => Optional with default value 0
        private String address = "";       // => Optional with default empty string

        // Builder constructor requires mandatory fields
        public Builder(String username, String email) {  // => Forces required fields
            this.username = username;      // => Set required username field
            this.email = email;            // => Set required email field
        }                                  // => Optional fields use defaults unless set

        // Fluent setter for optional age field
        public Builder age(int age) {      // => Method name matches field (no 'set' prefix)
            this.age = age;                // => Update age field in builder
            return this;                   // => Return 'this' for method chaining
        }                                  // => Enables fluent: .age(30).address("...")

        // Fluent setter for optional address field
        public Builder address(String address) {  // => Method name matches field
            this.address = address;        // => Update address field in builder
            return this;                   // => Return 'this' for method chaining
        }                                  // => Enables fluent syntax

        // Build method constructs final User instance
        public User build() {              // => Final step in builder chain
            return new User(this);         // => Pass builder to private User constructor
        }                                  // => Returns immutable User with all fields set
    }

    @Override
    public String toString() {             // => Human-readable representation
        return "User{username='" + username + "', email='" + email +
               "', age=" + age + ", address='" + address + "'}";
    }
}

// Using builder pattern - fluent API with method chaining
User user1 = new User.Builder("alice", "alice@example.com")  // => Create builder with required fields
    .age(30)                               // => Set optional age (returns builder)
    .address("123 Main St")                // => Set optional address (returns builder)
    .build();                              // => Construct final User instance
                                           // => user1: username="alice", email="alice@example.com", age=30, address="123 Main St"

// Builder with only required fields (optional fields use defaults)
User user2 = new User.Builder("bob", "bob@example.com")  // => Create builder
    .build();                              // => age=0 (default), address="" (default)
                                           // => user2: username="bob", email="bob@example.com", age=0, address=""

// Record classes (Java 14+) - concise immutable data carriers
public record Point(int x, int y) {       // => Declares record with two components (x, y)
    // Compiler automatically generates:
    // 1. Private final fields: private final int x; private final int y;
    // 2. Canonical constructor: public Point(int x, int y) { this.x = x; this.y = y; }
    // 3. Accessor methods: public int x() { return x; } public int y() { return y; }
    // 4. equals() method: compares all fields for equality
    // 5. hashCode() method: computes hash from all fields
    // 6. toString() method: returns "Point[x=10, y=20]"
}                                          // => All components final (record is immutable)

// Using record - construction and accessors
Point p1 = new Point(10, 20);              // => p1: x=10, y=20 (canonical constructor)
int x = p1.x();                            // => 10 (accessor without 'get' prefix)
int y = p1.y();                            // => 20 (accessor returns component value)

// Records provide automatic toString()
System.out.println(p1);                    // => Output: "Point[x=10, y=20]"

// Records provide automatic equals() and hashCode()
Point p2 = new Point(10, 20);              // => p2: x=10, y=20 (same values as p1)
boolean equal = p1.equals(p2);             // => true (structural equality on fields)
int hash1 = p1.hashCode();                 // => Hash based on x and y values
int hash2 = p2.hashCode();                 // => hash1 == hash2 (same values, same hash)

// Records are immutable - no setters, fields are final
// p1.x = 30;                              // ERROR: Cannot assign to final field x
// p1.setX(30);                            // ERROR: No setter methods in records

// Custom validation in record using compact constructor
public record Temperature(double celsius) {  // => Record with single component
    // Compact constructor - validates input before fields assigned
    public Temperature {                   // => No parameter list (compact syntax)
        if (celsius < -273.15) {           // => Validation: absolute zero check
            throw new IllegalArgumentException("Below absolute zero");
        }                                  // => Exception prevents invalid Temperature
    }                                      // => After validation, field assignment happens automatically
                                           // => Equivalent to: this.celsius = celsius;

    // Custom methods allowed in records (beyond auto-generated ones)
    public double fahrenheit() {           // => Computed property (not a field)
        return celsius * 9.0 / 5.0 + 32;   // => Converts Celsius to Fahrenheit
    }                                      // => celsius field accessible (private final)

    public double kelvin() {               // => Another computed property
        return celsius + 273.15;           // => Converts Celsius to Kelvin
    }
}

// Using record with validation and custom methods
Temperature temp = new Temperature(100.0); // => temp: celsius=100.0 (validation passes)
double c = temp.celsius();                 // => 100.0 (accessor)
double f = temp.fahrenheit();              // => 212.0 (custom method)
double k = temp.kelvin();                  // => 373.15 (custom method)

// Invalid temperature throws exception
try {
    Temperature invalid = new Temperature(-300.0);  // => Validation fails (< -273.15)
} catch (IllegalArgumentException e) {     // => Exception caught
    System.out.println(e.getMessage());    // => Output: "Below absolute zero"
}                                          // => invalid not created (exception thrown)

// Record with multiple components and custom constructor
public record Rectangle(double width, double height) {
    // Compact constructor with multiple validations
    public Rectangle {                     // => Validates both width and height
        if (width <= 0) {                  // => Width must be positive
            throw new IllegalArgumentException("Width must be positive");
        }
        if (height <= 0) {                 // => Height must be positive
            throw new IllegalArgumentException("Height must be positive");
        }
    }                                      // => After validation, fields assigned automatically

    // Custom method for computed property
    public double area() {                 // => Compute area from width and height
        return width * height;             // => Returns area in square units
    }

    public double perimeter() {            // => Compute perimeter
        return 2 * (width + height);       // => Returns perimeter
    }
}

Rectangle rect = new Rectangle(5.0, 3.0);  // => rect: width=5.0, height=3.0
double area = rect.area();                 // => 15.0 (5.0 * 3.0)
double perimeter = rect.perimeter();       // => 16.0 (2 * (5.0 + 3.0))
```

**Key Takeaway**: Encapsulation uses private fields with public getters/setters. Builder pattern enables fluent construction of complex objects. Records (Java 14+) automatically generate constructors, getters, `equals()`, `hashCode()`, and `toString()` for immutable data classes.

**Why It Matters**: Encapsulation protects object invariants, preventing invalid states that cause bugs. Private fields with public accessors enable validation, logging, and computed properties without exposing internal representation. The JavaBean convention (getters/setters) integrates with frameworks like Spring and Hibernate that use reflection to access properties. Records (Java 14+) provide immutable data carriers with auto-generated accessors, eliminating boilerplate while maintaining encapsulation. Proper encapsulation enables refactoring internals without breaking clients, reduces coupling, and enforces business rules through controlled access.

---

### Example 17: HTTP Filter Chain Pattern

Production middleware systems use filter chains to process requests through multiple stages (authentication, rate limiting, logging). This pattern demonstrates composition, delegation, and the Chain of Responsibility pattern in real-world HTTP request processing.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Request["HTTP Request"] --> Auth["AuthFilter<br/>Check token"]
    Auth -->|Unauthorized| Resp1["401 Response"]
    Auth -->|Authorized| Rate["RateLimitFilter<br/>Check quota"]
    Rate -->|Exceeded| Resp2["429 Response"]
    Rate -->|OK| Log["LoggingFilter<br/>Record request"]
    Log --> Handler["Request Handler"]
    Handler --> Response["200 Response"]

    style Request fill:#0173B2,color:#fff
    style Auth fill:#DE8F05,color:#fff
    style Rate fill:#029E73,color:#fff
    style Log fill:#CC78BC,color:#fff
    style Handler fill:#CA9161,color:#fff
    style Resp1 fill:#DE8F05,color:#fff
    style Resp2 fill:#DE8F05,color:#fff
    style Response fill:#029E73,color:#fff
```

**Code**:

```java
import java.util.*;                            // => Import utility classes (List, ArrayList, HashMap, etc.)
                                               // => Provides collections framework for headers and filter list

// Request model - represents HTTP request with headers and metadata
class Request {                                // => Encapsulates all request data (path, method, headers, client)
                                               // => Passed through filter chain, modified by filters
    // Private fields - encapsulated request state
    private String path;                       // => Request path (e.g., "/api/users", "/api/data")
                                               // => Identifies requested resource/endpoint
    private String method;                     // => HTTP method (GET, POST, PUT, DELETE, etc.)
                                               // => Defines operation type (read, write, update, delete)
    private Map<String, String> headers;       // => HTTP headers (Authorization, Content-Type, User-Agent, etc.)
                                               // => Key-value pairs for request metadata
                                               // => Case-sensitive keys in this implementation
    private String clientId;                   // => Client identifier (set by auth filter after authentication)
                                               // => null before authentication, set to user/app ID after
                                               // => Used by rate limiter to track per-client quotas

    // Constructor initializes request with path and method
    public Request(String path, String method) {  // => Create request for given endpoint and HTTP verb
                                               // => Called by client code before entering filter chain
        this.path = path;                      // => Store request path (immutable after construction)
                                               // => e.g., "/api/users" identifies users endpoint
        this.method = method;                  // => Store HTTP method (immutable after construction)
                                               // => e.g., "GET" for read operation
        this.headers = new HashMap<>();        // => Initialize empty headers map (mutable, grows as headers added)
                                               // => HashMap provides O(1) header lookup and insertion
    }                                          // => Request ready for header additions via setHeader()
                                               // => clientId remains null until set by authentication filter

    // Get header value by name (case-sensitive)
    public String getHeader(String name) {     // => Lookup header in map by exact name
                                               // => Used by filters to read headers (e.g., Authorization)
        return headers.get(name);              // => Returns header value or null if not found
                                               // => Null return indicates header not present
                                               // => Case-sensitive: "Authorization" != "authorization"
    }                                          // => Filters should check for null before using value

    // Set header value (used to add Authorization, Content-Type, etc.)
    public void setHeader(String name, String value) {  // => Add/update header in request
                                               // => Called by client code before entering chain
                                               // => Could be called by filters to modify request
        headers.put(name, value);              // => Store in headers map (overwrites if exists)
                                               // => put() returns previous value (ignored here)
                                               // => Allows header updates during request processing
    }                                          // => Headers map updated in-place (mutable operation)

    // Client ID getter - set by authentication filter
    public String getClientId() {              // => Returns client identifier (user/app ID)
                                               // => Used by rate limiter and logging to identify client
        return clientId;                       // => null if not authenticated yet (before auth filter runs)
                                               // => Non-null after successful authentication
                                               // => Downstream filters assume auth filter sets this
    }                                          // => Filter ordering matters: auth before rate limit

    // Client ID setter - authentication filter sets this
    public void setClientId(String id) {       // => Set authenticated client identifier after token validation
                                               // => Called by AuthenticationFilter after successful auth
        this.clientId = id;                    // => Store client ID (extracted from token or database)
                                               // => Used by downstream filters (rate limiting uses this)
                                               // => Enriches request with authenticated identity
    }                                          // => Enables per-client quota tracking and logging

    // Path getter - request endpoint
    public String getPath() {                  // => Returns request path (endpoint URL)
                                               // => Used by logging and request routing
        return path;                           // => e.g., "/api/users" identifies users endpoint
                                               // => Immutable after construction (no setter)
    }                                          // => Identifies requested resource in logs and metrics

    // Method getter - HTTP verb
    public String getMethod() {                // => Returns HTTP method (operation type)
                                               // => Used by logging and authorization checks
        return method;                         // => e.g., "GET" (read), "POST" (create), "PUT" (update), "DELETE" (remove)
                                               // => Immutable after construction (no setter)
    }                                          // => Identifies operation type in logs and metrics
}                                              // => Request carries all data through filter chain
                                               // => Filters read/modify request state as it passes through

// Response model - represents HTTP response with status code and body
class Response {                               // => Encapsulates HTTP response data (status, body)
                                               // => Returned by filters and final handler to client
    // Private fields - encapsulated response state
    private int statusCode;                    // => HTTP status code (200 OK, 401 Unauthorized, 429 Too Many Requests, etc.)
                                               // => Standard codes from HTTP specification (RFC 7231)
                                               // => 2xx success, 4xx client error, 5xx server error
    private String body;                       // => Response body content (text, JSON, HTML, etc.)
                                               // => Simplified as String (production: byte array or stream)
                                               // => Contains actual data returned to client

    // Constructor creates response with status and body
    public Response(int statusCode, String body) {  // => Initialize response with given status and content
                                               // => Private constructor pattern often used (here public for simplicity)
                                               // => Factory methods preferred for common cases
        this.statusCode = statusCode;          // => Set HTTP status code (200, 401, 429, 500, etc.)
                                               // => No validation (assumes caller provides valid code)
        this.body = body;                      // => Set response body (text content)
                                               // => Immutable after construction (no setters)
    }                                          // => Response ready to return to client
                                               // => Both fields final in production (immutable response)

    // Factory method for successful response (200 OK)
    public static Response ok(String body) {   // => Create success response with custom body
                                               // => Static factory pattern for common response type
                                               // => Clearer than new Response(200, ...) at call site
        return new Response(200, body);        // => Status 200 (OK) with given body
                                               // => 200 indicates successful request completion
    }                                          // => Used when request completes successfully (all filters passed)
                                               // => Returns data to client (user list, resource, etc.)

    // Factory method for unauthorized response (401)
    public static Response unauthorized() {    // => Create auth failure response (standard message)
                                               // => No parameters needed (standard error response)
                                               // => Factory method encapsulates status code + message
        return new Response(401, "Unauthorized");  // => Status 401 (Unauthorized) with standard message
                                               // => 401 indicates missing or invalid authentication
                                               // => Client should retry with valid credentials
    }                                          // => Used when authentication fails (invalid token)
                                               // => Chain short-circuits, response returned immediately

    // Factory method for rate limit exceeded response (429)
    public static Response tooManyRequests() { // => Create rate limit response (quota exceeded)
                                               // => Standard response for rate limiting scenarios
                                               // => 429 is HTTP standard for rate limiting (RFC 6585)
        return new Response(429, "Too Many Requests");  // => Status 429 with standard message
                                               // => 429 indicates client exceeded quota/rate limit
                                               // => Client should wait before retrying (backoff)
    }                                          // => Used when client exceeds quota (too many requests)
                                               // => Production: include Retry-After header

    // Status code getter
    public int getStatusCode() {               // => Returns HTTP status code (200, 401, 429, etc.)
                                               // => Used by logging filter to record response status
                                               // => Used by tests to verify correct response type
        return statusCode;                     // => e.g., 200 (success), 401 (auth), 429 (rate limit)
                                               // => Immutable (no setter, final in production)
    }                                          // => Status determines client behavior (retry, error, success)

    // Body getter
    public String getBody() {                  // => Returns response body content
                                               // => Used by client to read response data
                                               // => Used by tests to verify response content
        return body;                           // => Response content as string
                                               // => Could be JSON, HTML, plain text, etc.
                                               // => Immutable (no setter, final in production)
    }                                          // => Body contains actual data for client

    @Override
    public String toString() {                 // => Human-readable representation for debugging/logging
                                               // => Overrides Object.toString() for better output
                                               // => Used in print statements and debugger
        return "Response{status=" + statusCode + ", body='" + body + "'}";
                                               // => Format: "Response{status=200, body='...'}"
                                               // => Shows both status code and body content
    }                                          // => Useful for debugging and test assertions
}                                              // => Response is simple data carrier (no business logic)
                                               // => Factory methods provide convenient construction

// Filter interface - defines contract for request filters (Chain of Responsibility)
interface RequestFilter {                     // => Contract for all request processing filters
                                               // => Each filter implements this to participate in chain
                                               // => Functional interface pattern (single abstract method)
    // Filter method processes request, decides to continue chain or return early
    Response filter(Request request, FilterChain chain);
                                               // => Takes request (current state) and chain (continuation)
                                               // => Returns Response (either short-circuit or from chain.next())
                                               // => Filter can inspect/modify request before calling chain.next()
                                               // => Filter can return early (short-circuit) without calling chain.next()
                                               // => Chain of Responsibility pattern: each filter decides propagation
}                                              // => Enables composable middleware (add/remove filters easily)
                                               // => Filters testable in isolation (mock chain)

// Filter chain manages execution flow through filters (Chain of Responsibility pattern)
class FilterChain {                            // => Orchestrates filter execution in sequential order
                                               // => Maintains position state to track progress through chain
                                               // => Delegates to next filter or final handler
    // Private fields - chain state
    private List<RequestFilter> filters;       // => Ordered list of filters to execute sequentially
                                               // => Immutable list (defensive copy in constructor)
                                               // => Order matters: auth before rate limit before logging
    private int position = 0;                  // => Current position in filter list (mutable state)
                                               // => Starts at 0, incremented by next() after each filter
                                               // => Tracks how far through chain we've progressed

    // Constructor accepts list of filters to execute in order
    public FilterChain(List<RequestFilter> filters) {  // => Initialize chain with ordered filters
                                               // => Called by client code before processing requests
                                               // => Accepts List (could be Arrays.asList, ArrayList, etc.)
        this.filters = new ArrayList<>(filters);  // => Defensive copy prevents external modification
                                               // => Creates new ArrayList from passed collection
                                               // => Protects against caller modifying filter list after construction
    }                                          // => Filters execute in insertion order (preserve ordering)
                                               // => position starts at 0 (ready to execute first filter)

    // Execute next filter in chain or final handler if all filters passed
    public Response next(Request request) {    // => Advance chain to next filter or final handler
                                               // => Called by filters to continue chain (chain.next())
                                               // => Returns Response from filter or final handler
        if (position >= filters.size()) {      // => Check if all filters completed successfully
                                               // => position == filters.size() means all filters executed
                                               // => No more filters to run, ready for final handler
            // All filters passed - handle the request
            return handleRequest(request);     // => Execute final request handler (business logic)
                                               // => Only reached if all filters called chain.next()
                                               // => Returns success response (200 OK)
        }                                      // => No more filters, processing complete
                                               // => Request successfully passed all middleware
        RequestFilter filter = filters.get(position++);  // => Get current filter at position, then increment
                                               // => position++ uses post-increment (get first, increment after)
                                               // => Next call to next() will process filter at position+1
                                               // => Advances position for next filter in chain
        return filter.filter(request, this);   // => Delegate to filter's filter() method
                                               // => Pass request (current state) and this chain (for continuation)
                                               // => Filter decides: return early or call chain.next()
    }                                          // => Filter decides whether to call chain.next() or short-circuit
                                               // => Returns response from filter or downstream chain

    // Final request handler - executed after all filters pass
    private Response handleRequest(Request request) {  // => Actual business logic (core request processing)
                                               // => Private method (only called by next() after filters)
                                               // => Only executes if all filters called chain.next()
        // Simulate request processing logic (in production, routes to handlers)
        return Response.ok("Processed: " + request.getPath());
                                               // => Returns 200 OK with path (simulates success)
                                               // => Production: route to controller, fetch data, etc.
                                               // => Could return different responses based on path/method
    }                                          // => Returns success response with path in body
                                               // => Final step in request processing (all filters passed)
}                                              // => Chain manages sequential filter execution
                                               // => position state enables tracking progress
                                               // => Filters compose through delegation (not inheritance)

// Authentication filter - validates authorization tokens (first filter in chain)
class AuthenticationFilter implements RequestFilter {
                                               // => Implements RequestFilter contract (filter method)
                                               // => First filter in chain (must run before rate limiter)
                                               // => Validates client credentials before allowing access
    // In-memory token store (production: database lookup, JWT validation, OAuth)
    private Set<String> validTokens = Set.of("token123", "token456");
                                               // => Immutable set of valid tokens (Set.of() creates unmodifiable set)
                                               // => Hardcoded for demonstration (production: external store)
                                               // => Production: check database, verify JWT signature, OAuth token introspection
                                               // => Production: tokens expire, require refresh, stored securely
                                               // => Set provides O(1) containment check (efficient validation)

    @Override
    public Response filter(Request request, FilterChain chain) {
                                               // => Implements RequestFilter.filter() (required by interface)
                                               // => First filter executed (position 0 in chain)
                                               // => Receives request from client, chain for continuation
        // Extract Authorization header (standard HTTP auth header)
        String token = request.getHeader("Authorization");
                                               // => Returns token or null if missing (getHeader returns null for absent)
                                               // => Standard: "Bearer <token>" format (e.g., "Bearer token123")
                                               // => Simplified: expects plain token, not Bearer prefix
                                               // => Production: parse "Bearer" prefix, extract token
                                               // => Case-sensitive header name ("Authorization" not "authorization")

        // Validate token presence and correctness
        if (token == null || !validTokens.contains(token)) {
                                               // => Two failure cases: missing token (null) or invalid token (not in set)
                                               // => token == null: Authorization header not present (client didn't authenticate)
                                               // => !validTokens.contains(token): token present but invalid (wrong credentials)
                                               // => Short-circuit evaluation: if null, doesn't check contains (avoids NullPointerException)
            System.out.println("Auth failed: Invalid token");
                                               // => Log authentication failure (production: structured logging, metrics)
                                               // => Security: don't reveal whether token missing vs invalid (same message)
                                               // => Helps audit failed login attempts (detect attacks)
            return Response.unauthorized();    // => Return 401 Unauthorized (standard auth failure code)
                                               // => Short-circuit chain: return immediately without calling chain.next()
                                               // => Downstream filters not executed (rate limiter, logging skipped)
        }                                      // => Chain stops here for invalid auth (efficiency + security)
                                               // => Response returned directly to client without further processing

        // Authentication successful - set client ID for downstream filters
        request.setClientId("user_" + token.hashCode());
                                               // => Extract client ID from token (simplified: hash code)
                                               // => Production: decode JWT claims (sub claim), database lookup by token
                                               // => hashCode() provides unique ID per token (collision possible but unlikely here)
                                               // => Enriches request with authenticated identity for downstream use
                                               // => Rate limiter uses clientId for per-user quota tracking
        System.out.println("Auth passed: " + request.getClientId());
                                               // => Log successful authentication with client ID
                                               // => Production: structured logging (user ID, timestamp, IP, endpoint)
                                               // => Audit trail for compliance and security monitoring
        return chain.next(request);            // => Continue to next filter (rate limiter at position 1)
                                               // => Pass modified request (with clientId set)
                                               // => Delegates to chain, returns response from downstream
    }                                          // => Returns response from chain (could be from rate limiter, logging, or handler)
                                               // => Filter enriches request (sets clientId) before delegation
}                                              // => First filter: validates auth, enriches request, or short-circuits
                                               // => Critical for security: blocks unauthenticated requests early

// Rate limiting filter - prevents API abuse by limiting requests per client (second filter)
class RateLimitFilter implements RequestFilter {
                                               // => Implements RequestFilter contract (filter method)
                                               // => Second filter in chain (after authentication)
                                               // => Enforces per-client request quotas to prevent abuse
    // In-memory request counter (production: Redis, distributed cache)
    private Map<String, Integer> requestCounts = new HashMap<>();
                                               // => Maps clientId -> request count (tracks usage per client)
                                               // => HashMap provides O(1) get/put operations (efficient)
                                               // => Mutable state (updated on each request)
                                               // => Production: use Redis with TTL (time-to-live for reset)
                                               // => Production: distributed cache for multi-server deployments
                                               // => Current implementation: counts never reset (grows indefinitely)
    private int maxRequests = 100;             // => Maximum requests per client (quota limit)
                                               // => Hardcoded for demonstration (production: configurable)
                                               // => Production: different limits per client tier (free, pro, enterprise)
                                               // => Production: time-windowed limits (100 req/hour, 1000 req/day)

    @Override
    public Response filter(Request request, FilterChain chain) {
                                               // => Implements RequestFilter.filter() (required by interface)
                                               // => Second filter executed (position 1 in chain, after auth)
                                               // => Receives authenticated request (clientId already set)
        // Get client ID set by authentication filter
        String clientId = request.getClientId();
                                               // => Returns client ID set by AuthenticationFilter
                                               // => Assumes AuthenticationFilter ran first (set clientId)
                                               // => Filter ordering matters: auth before rate limit (dependency)
                                               // => null if auth filter skipped (shouldn't happen if chain ordered correctly)
                                               // => Production: validate clientId not null (defensive programming)

        // Get current request count for client (0 if not found)
        int count = requestCounts.getOrDefault(clientId, 0);
                                               // => Lookup current count in map (clientId is key)
                                               // => Returns 0 for first request from client (default value)
                                               // => Subsequent requests return incremented count from previous requests
                                               // => getOrDefault() avoids NullPointerException (returns default if absent)
                                               // => count represents requests processed so far (not including current)

        // Check if client exceeded quota
        if (count >= maxRequests) {            // => Quota exceeded check (count at or above limit)
                                               // => >= ensures limit is inclusive (100th request allowed, 101st blocked)
                                               // => Evaluated before incrementing count (current request counts toward limit)
            System.out.println("Rate limit exceeded for: " + clientId);
                                               // => Log rate limit violation with client ID
                                               // => Production: metrics (increment rate_limit_exceeded counter)
                                               // => Production: alert on sustained violations (DDoS detection)
            return Response.tooManyRequests(); // => Return 429 Too Many Requests (HTTP standard for rate limiting)
                                               // => Short-circuit chain: return immediately without calling chain.next()
                                               // => Downstream filters not executed (logging filter skipped)
        }                                      // => Chain stops here for quota exceeded (efficiency)
                                               // => Request blocked before reaching business logic (protects backend)

        // Increment request count for client
        requestCounts.put(clientId, count + 1);
                                               // => Update counter (count + 1, current request counted)
                                               // => put() overwrites previous value (replaces old count)
                                               // => Production: atomic increment in Redis (INCR command)
                                               // => Production: set TTL on counter (reset after time window)
                                               // => Thread-unsafe in current form (HashMap not thread-safe)
                                               // => Production: use ConcurrentHashMap or distributed cache
        System.out.println("Rate limit check passed: " + count + "/" + maxRequests);
                                               // => Log current quota usage (count before increment / max)
                                               // => Shows "0/100" for first request, "99/100" for 100th request
                                               // => Production: expose metrics (quota usage percentage)
                                               // => Helps monitor client usage patterns
        return chain.next(request);            // => Continue to next filter (logging filter at position 2)
                                               // => Request within quota, proceed with processing
                                               // => Delegates to chain, returns response from downstream
    }                                          // => Returns response from chain (could be from logging or handler)
                                               // => Filter tracks quota before delegation
}                                              // => Second filter: enforces quotas, tracks usage, or short-circuits
                                               // => Critical for stability: prevents abuse and overload

// Logging filter - records request metrics (observability) (third filter)
class LoggingFilter implements RequestFilter {
                                               // => Implements RequestFilter contract (filter method)
                                               // => Third filter in chain (after auth and rate limit)
                                               // => Records request metrics for observability (monitoring, debugging)
    @Override
    public Response filter(Request request, FilterChain chain) {
                                               // => Implements RequestFilter.filter() (required by interface)
                                               // => Third filter executed (position 2 in chain, last before handler)
                                               // => Receives authenticated, quota-approved request
        // Record start time for latency measurement
        long startTime = System.currentTimeMillis();
                                               // => Timestamp before processing (milliseconds since Unix epoch)
                                               // => Used to calculate request duration (end - start)
                                               // => currentTimeMillis() returns wall-clock time (not monotonic)
                                               // => Production: use System.nanoTime() for precise duration (monotonic)
                                               // => Captures time before chain.next() call (includes downstream processing)
        System.out.println("Request started: " + request.getPath());
                                               // => Log request initiation with endpoint path
                                               // => Production: structured logging (JSON with timestamp, clientId, method)
                                               // => Helps correlate request start/end in logs
                                               // => Useful for debugging slow requests

        // Continue chain to final handler (all previous filters passed)
        Response response = chain.next(request);
                                               // => Execute final handler (position 3, last in chain)
                                               // => This is the last filter before handler (all validation passed)
                                               // => Delegates to chain, waits for response from handler
                                               // => Response contains actual business logic result
                                               // => Could be 200 OK (success) from handler
                                               // => Captures response to log status code and measure duration

        // Calculate request duration
        long duration = System.currentTimeMillis() - startTime;
                                               // => Time elapsed in milliseconds (end - start)
                                               // => Includes time for all downstream processing (handler execution)
                                               // => Production: send to metrics system (Prometheus, DataDog, CloudWatch)
                                               // => Production: create histogram/percentiles (p50, p95, p99 latency)
                                               // => Helps identify slow endpoints and performance issues
        System.out.println("Request completed: " + response.getStatusCode() +
                         " in " + duration + "ms");
                                               // => Log completion with status code and latency
                                               // => Format: "Request completed: 200 in 15ms"
                                               // => Production: structured logging with more metadata
                                               // => Status code shows success/failure (200, 401, 429, 500)
                                               // => Duration helps track performance (SLA monitoring)
        return response;                       // => Return response unchanged (pass-through)
                                               // => Logging filter observes but doesn't modify response
                                               // => Response propagates back through chain to client
    }                                          // => Logging doesn't modify response (read-only filter)
                                               // => Observability filter: measures and records, doesn't alter behavior
}                                              // => Third filter: records metrics, measures latency, always continues
                                               // => Critical for observability: enables monitoring and debugging

// Building and using the filter chain - composable middleware (client code)
List<RequestFilter> filters = List.of(        // => Create ordered list of filters (immutable list)
                                               // => List.of() creates unmodifiable list (cannot add/remove after creation)
                                               // => Filter execution order: same as insertion order
    new AuthenticationFilter(),                // => 1st filter: Authenticate (validates token, sets clientId)
                                               // => Must run first (other filters depend on clientId)
    new RateLimitFilter(),                     // => 2nd filter: Check quota (uses clientId from auth)
                                               // => Depends on auth filter setting clientId
    new LoggingFilter()                        // => 3rd filter: Log metrics (observability, no side effects)
                                               // => Last filter before handler (measures total latency)
);                                             // => Order matters: auth before rate limit (dependency)
                                               // => Changing order breaks functionality (rate limiter needs clientId)
                                               // => filters list has 3 elements (indices 0, 1, 2)

FilterChain chain = new FilterChain(filters);  // => Create chain with ordered filters
                                               // => Defensive copy made in constructor (protects against external modification)
                                               // => Chain position starts at 0 (ready to execute first filter)
                                               // => Single chain instance can process multiple requests
                                               // => NOTE: RateLimitFilter has mutable state (requestCounts)
                                               // => Production: create new chain per request or use thread-safe filters

// Successful request - all filters pass (happy path)
Request req1 = new Request("/api/users", "GET");  // => Create GET request for users endpoint
                                               // => path="/api/users", method="GET", headers=empty, clientId=null
req1.setHeader("Authorization", "token123");   // => Set valid auth token (present in validTokens set)
                                               // => headers map now: {"Authorization" -> "token123"}
                                               // => Token will pass authentication check
Response resp1 = chain.next(req1);             // => Execute filter chain from position 0
                                               // => Triggers auth filter → rate limit filter → logging filter → handler
                                               // => All filters call chain.next() (no short-circuits)
// Output sequence (filters execute in order):
// Auth passed: user_XXXXXX                   // => AuthenticationFilter logs success (XXXXXX is hashCode)
                                               // => clientId set to "user_<hashCode>" in request
// Rate limit check passed: 0/100             // => RateLimitFilter logs quota usage (first request from this client)
                                               // => count=0 before increment, maxRequests=100
// Request started: /api/users                // => LoggingFilter logs start (startTime captured)
// Request completed: 200 in 1ms              // => LoggingFilter logs completion (200 OK, ~1ms latency)
                                               // => Handler returned Response.ok("Processed: /api/users")
// => resp1: Response{status=200, body='Processed: /api/users'}
                                               // => Success response returned through all filters back to client
                                               // => All filters executed, request completed successfully

// Failed authentication - chain short-circuits at first filter (auth failure path)
Request req2 = new Request("/api/data", "GET");  // => Create GET request for data endpoint
                                               // => path="/api/data", method="GET", headers=empty, clientId=null
req2.setHeader("Authorization", "invalid");    // => Set invalid token (NOT in validTokens set)
                                               // => headers map now: {"Authorization" -> "invalid"}
                                               // => Token will fail authentication check
Response resp2 = chain.next(req2);             // => Execute filter chain from position 0
                                               // => Triggers auth filter, which returns 401 immediately
                                               // => Auth filter does NOT call chain.next() (short-circuit)
// Output:
// Auth failed: Invalid token                 // => AuthenticationFilter logs failure
                                               // => validTokens.contains("invalid") is false
                                               // => Returns Response.unauthorized() without calling chain.next()
// => resp2: Response{status=401, body='Unauthorized'}
                                               // => 401 Unauthorized response returned to client
// => RateLimitFilter and LoggingFilter NOT executed (chain stopped)
                                               // => position never advanced beyond 0 (auth filter short-circuited)
                                               // => Efficient: invalid requests blocked early (no rate limit check, no logging)
                                               // => Security: unauthorized requests don't consume quota

// Rate limit exceeded - chain short-circuits at second filter (quota exhausted path)
Request req3 = new Request("/api/data", "GET");  // => Create GET request for data endpoint
                                               // => path="/api/data", method="GET", headers=empty, clientId=null
req3.setHeader("Authorization", "token123");   // => Set valid token (will pass auth)
                                               // => headers map now: {"Authorization" -> "token123"}
// Simulate 100 previous requests (exhaust quota)
RateLimitFilter rateLimiter = new RateLimitFilter();  // => Create separate rate limiter (simulates state)
                                               // => Note: This is NOT the same instance as in chain
                                               // => Demonstration issue: should manipulate chain's rate limiter state
                                               // => For demo, assume chain's rate limiter already has 100 requests for this client
for (int i = 0; i < 100; i++) {                // => Simulate exhausting quota (loop 100 times)
                                               // => Production: distributed counter (Redis) would track actual requests
    // Simulate 100 requests (production: distributed counter)
                                               // => Each request increments requestCounts for this clientId
                                               // => After 100 requests: requestCounts.get(clientId) == 100
}                                              // => Client quota now exhausted (count >= maxRequests)
Response resp3 = chain.next(req3);             // => Execute filter chain from position 0
                                               // => Triggers auth filter (passes) → rate limit filter (blocks)
                                               // => Rate limit filter does NOT call chain.next() (short-circuit)
// Output:
// Auth passed: user_XXXXXX                   // => AuthenticationFilter passes (valid token)
                                               // => clientId set, position advances to 1
// Rate limit exceeded for: user_XXXXXX       // => RateLimitFilter blocks (count=100 >= maxRequests=100)
                                               // => Returns Response.tooManyRequests() without calling chain.next()
// => resp3: Response{status=429, body='Too Many Requests'}
                                               // => 429 Too Many Requests response returned to client
// => LoggingFilter NOT executed (chain stopped at rate limiter)
                                               // => position never advanced beyond 1 (rate filter short-circuited)
                                               // => Efficient: quota-exceeded requests blocked before handler (protects backend)
```

**Key Takeaway**: Filter chains demonstrate composition and the Chain of Responsibility pattern. Each filter decides whether to continue the chain or return early. This pattern enables modular, testable middleware—add/remove filters without changing core logic. Production systems use this for authentication, rate limiting, logging, compression, and error handling. Filters compose through delegation, avoiding inheritance coupling.

**Why It Matters**: The Filter Chain pattern enables composable middleware for request processing—authentication, logging, rate limiting, CORS handling. It's fundamental to servlet containers (Tomcat), web frameworks (Spring), and HTTP libraries. Filters decouple cross-cutting concerns from business logic, making code modular and reusable. Understanding the chain-of-responsibility pattern enables implementing custom filters for API gateways, reverse proxies, and web services. Filter chains enable consistent request handling across endpoints, improving security, observability, and maintainability in production web applications.

---

### Example 18: Generics Deep Dive

Generics provide compile-time type safety for classes and methods. Bounded type parameters constrain allowable types. Wildcards enable flexible APIs.

**Code**:

```java
import java.util.*;                            // => Import collections framework (List, ArrayList, Arrays)
                                               // => Used for generic collections examples later

// Generic class with type parameter <T> (unbounded type parameter)
class Box<T> {                                 // => T is placeholder for any type (type parameter)
                                               // => T replaced with actual type at instantiation (String, Integer, etc.)
                                               // => Unbounded: no constraints on T (can be any reference type)
                                               // => Generic types provide type-safe containers without casting
    private T content;                         // => Field of type T (determined at instantiation)
                                               // => Type unknown at compile time (replaced with actual type)
                                               // => content is null initially (reference type default)

    // Setter accepts type T (whatever T is bound to at instantiation)
    public void set(T content) {               // => Parameter must match type T
                                               // => Compile-time check: only T-typed values accepted
                                               // => this.content refers to field, content refers to parameter
        this.content = content;                // => Store content of type T (assign parameter to field)
                                               // => No casting needed (type-safe assignment)
    }                                          // => Setter enables modifying box content after creation

    // Getter returns type T (no casting needed by caller)
    public T get() {                           // => Return type is T (actual type known at call site)
                                               // => Caller knows exact type (no Object cast needed)
        return content;                        // => Returns T (compile-time type safety)
                                               // => null if content never set (reference type default)
    }                                          // => Caller gets exact type, no cast needed
                                               // => Pre-generics: would return Object, require (T) cast
}                                              // => Generic class eliminates casting and enables type safety
                                               // => Compiler erases T to Object at runtime (type erasure)

// Using generic class with String type parameter
Box<String> stringBox = new Box<>();          // => T bound to String (type argument in angle brackets)
                                               // => Diamond operator <> infers type from left side (Java 7+)
                                               // => Equivalent to: new Box<String>() (explicit type)
                                               // => stringBox can only hold String values (compile-time enforced)
stringBox.set("Hello");                        // => OK: "Hello" is String (matches T=String)
                                               // => Compiler checks: String literal assignable to T (T=String)
                                               // => content field now holds "Hello" (String reference)
String str = stringBox.get();                  // => "Hello" (no cast needed, returns String)
                                               // => get() return type is String (T bound to String)
                                               // => Pre-generics: would need (String) stringBox.get() cast
                                               // => str is "Hello" (type-safe retrieval without ClassCastException risk)

// Using generic class with Integer type parameter
Box<Integer> intBox = new Box<>();            // => T bound to Integer (different type from stringBox)
                                               // => Separate type instantiation (independent from Box<String>)
                                               // => Diamond operator <> infers Integer from left side
                                               // => intBox can only hold Integer values (compile-time enforced)
intBox.set(42);                                // => OK: 42 is Integer (autoboxing from int primitive)
                                               // => Primitive int auto-converted to Integer object
                                               // => content field now holds Integer(42) (wrapper object)
// intBox.set("text");                         // ERROR: "text" is String, not Integer (compile-time error)
                                               // => Compiler prevents type mismatch (String incompatible with T=Integer)
                                               // => Prevents ClassCastException at runtime (caught at compile time)
                                               // => Generics enable early error detection (compile vs runtime)

// Generic method with type parameter <T> (method-level generics)
public static <T> void printArray(T[] array) {  // => <T> before return type declares type parameter
                                               // => T scoped to this method only (not class-level)
                                               // => Method can be generic even if class is not generic
                                               // => Type parameter <T> is separate from any class <T>
    for (T element : array) {                  // => Each element is type T (enhanced for loop)
                                               // => element type matches array component type
                                               // => T could be String, Integer, or any reference type
        System.out.println(element);           // => Print element (calls toString() on element)
                                               // => Works for any type that has toString() (all Objects)
    }                                          // => Works for arrays of any reference type (not primitives)
}                                              // => Generic method enables type-safe array printing
                                               // => No casting needed, works for all reference types

// Calling generic method (type inference from arguments)
String[] strings = {"A", "B", "C"};            // => Array of String (3 elements)
                                               // => Array literal syntax (implicitly String[])
Integer[] ints = {1, 2, 3};                    // => Array of Integer (3 elements, autoboxed from int)
                                               // => Integer[] not int[] (generics don't work with primitives)
printArray(strings);                           // => T inferred as String from argument type (String[])
                                               // => Compiler determines T=String without explicit type argument
                                               // => Type inference based on actual argument (method call)
                                               // => Output: A \n B \n C (each element on new line)
printArray(ints);                              // => T inferred as Integer from argument type (Integer[])
                                               // => Compiler determines T=Integer from Integer[] argument
                                               // => Separate invocation (different T than previous call)
                                               // => Output: 1 \n 2 \n 3 (toString() on Integer objects)

// Bounded type parameters - <T extends Type> constrains allowable types
class NumberBox<T extends Number> {            // => T must be Number or subclass (Integer, Double, Long, etc.)
                                               // => Upper bound: T constrained to Number hierarchy only
                                               // => Rejects non-numeric types at compile time (String, List, etc.)
                                               // => extends keyword used for both classes and interfaces
    private T number;                          // => Field of type T (constrained to Number subclasses)
                                               // => Stronger guarantee than unbounded T (can call Number methods)
                                               // => number is null initially (reference type default)

    // Setter accepts only Number subclasses
    public void set(T number) {                // => Parameter must extend Number (compile-time enforced)
                                               // => Can only pass Number or subclasses (Integer, Double, etc.)
        this.number = number;                  // => Store number of type T (type-safe assignment)
                                               // => No casting needed (T constrained to Number)
    }                                          // => Setter ensures only numeric types stored

    // Can call Number methods on T (because T extends Number)
    public double doubleValue() {              // => Returns double primitive (Number.doubleValue())
                                               // => Converts T to double regardless of actual type
        return number.doubleValue();           // => Number method available (T extends Number guarantee)
                                               // => Works for Integer, Double, Long, etc. (all have doubleValue())
    }                                          // => Bounded type enables calling superclass methods on T
                                               // => Unbounded T can only call Object methods
}                                              // => Bounded generics trade flexibility for type safety

// Using bounded generic class
NumberBox<Integer> intNumBox = new NumberBox<>();  // => OK: Integer extends Number (satisfied bound)
                                               // => T bound to Integer (valid subclass of Number)
                                               // => Diamond operator <> infers Integer
intNumBox.set(42);                             // => OK: 42 is Integer (extends Number, matches T)
                                               // => Autoboxing from int to Integer
                                               // => number field now holds Integer(42)
double d1 = intNumBox.doubleValue();           // => 42.0 (Integer.doubleValue() called)
                                               // => Integer.doubleValue() returns 42.0 (double)
                                               // => d1 is 42.0 (primitive double)

NumberBox<Double> doubleBox = new NumberBox<>();  // => OK: Double extends Number (satisfied bound)
                                               // => T bound to Double (different type from Integer)
                                               // => Separate instantiation (independent from NumberBox<Integer>)
doubleBox.set(3.14);                           // => OK: 3.14 is Double (extends Number, matches T)
                                               // => Double literal (not float, which would be 3.14f)
                                               // => number field now holds Double(3.14)
double d2 = doubleBox.doubleValue();           // => 3.14 (Double.doubleValue() called)
                                               // => Double.doubleValue() returns 3.14 (unchanged)
                                               // => d2 is 3.14 (primitive double)

// NumberBox<String> strBox = new NumberBox<>();  // ERROR: String doesn't extend Number
                                               // => Compile-time error: String violates bound
                                               // => String not in Number hierarchy (fails T extends Number)
                                               // => Compiler prevents type mismatch early (not runtime)

// Multiple bounds - <T extends Class & Interface1 & Interface2>
class MultiBox<T extends Number & Comparable<T>> {
                                               // => T must extend Number AND implement Comparable<T>
                                               // => Multiple constraints using & separator
                                               // => Class bound first, then interface bounds (syntax requirement)
                                               // => Ensures T supports both arithmetic and comparison
    // Method uses both Number and Comparable methods
    public T max(T a, T b) {                   // => Returns larger of two values (generic max)
                                               // => Parameters type T (must satisfy both bounds)
        return a.compareTo(b) > 0 ? a : b;     // => Uses Comparable.compareTo() (guaranteed by bound)
                                               // => compareTo() returns >0 if a > b, <0 if a < b, 0 if equal
                                               // => Ternary operator returns a if a > b, else b
    }                                          // => Multiple bounds enable using multiple APIs on T
                                               // => Could also call Number.doubleValue() (both bounds available)
}                                              // => Multiple bounds enable rich constraints on type parameters

MultiBox<Integer> multiBox = new MultiBox<>();  // => OK: Integer extends Number AND implements Comparable<Integer>
                                               // => Integer satisfies both bounds (Number + Comparable)
                                               // => T bound to Integer (type parameter instantiation)
Integer max = multiBox.max(10, 20);            // => 20 (compareTo() returns negative for 10 < 20)
                                               // => 10.compareTo(20) returns negative (10 < 20)
                                               // => Ternary returns b (20) when a.compareTo(b) <= 0
                                               // => max is 20 (Integer object)
// MultiBox<String> strMulti = new MultiBox<>();  // ERROR: String doesn't extend Number
                                               // => String implements Comparable<String> but not Number
                                               // => Violates first bound (T extends Number)
                                               // => Both bounds must be satisfied (AND logic, not OR)

// Wildcards for flexible method parameters - unknown type (?)
public static void printList(List<?> list) {   // => Accepts List of any type (?, unknown wildcard)
                                               // => Can't assume element type, read as Object
    for (Object obj : list) {                  // => Elements read as Object (safest common type)
        System.out.println(obj);               // => Print each element (Object.toString())
    }
    // list.add(42);                           // ERROR: can't add to List<?> (type unknown)
                                               // => ? could be List<String>, can't add Integer
}                                              // => Read-only wildcard (producer, not consumer)

// Calling printList with different types
List<String> strings2 = Arrays.asList("A", "B");  // => List<String>
List<Integer> ints2 = Arrays.asList(1, 2);     // => List<Integer>
printList(strings2);                           // => OK: List<String> is List<?>
                                               // => Output: A \n B
printList(ints2);                              // => OK: List<Integer> is List<?>
                                               // => Output: 1 \n 2

// Upper bounded wildcard - <? extends Type> (covariant, read-only)
public static double sumNumbers(List<? extends Number> list) {
                                               // => Accepts List of Number or any subclass
                                               // => List<Integer>, List<Double>, etc. all accepted
    double sum = 0;                            // => Accumulator for sum
    for (Number num : list) {                  // => Elements read as Number (safe upcast)
        sum += num.doubleValue();              // => Call Number.doubleValue() (guaranteed)
    }                                          // => Accumulate sum
    return sum;                                // => Returns sum of all numbers
    // list.add(Integer.valueOf(5));          // ERROR: can't add to List<? extends Number>
                                               // => Could be List<Double>, can't add Integer
}                                              // => Producer (reads values), not consumer

// Calling sumNumbers with different numeric types
List<Integer> intList = Arrays.asList(1, 2, 3);  // => List<Integer> (Integer extends Number)
List<Double> doubleList = Arrays.asList(1.5, 2.5);  // => List<Double> (Double extends Number)
double sum1 = sumNumbers(intList);             // => 6.0 (1 + 2 + 3, converted to double)
double sum2 = sumNumbers(doubleList);          // => 4.0 (1.5 + 2.5)

// Lower bounded wildcard - <? super Type> (contravariant, write-only)
public static void addIntegers(List<? super Integer> list) {
                                               // => Accepts List of Integer or any superclass
                                               // => List<Number>, List<Object> accepted
    list.add(1);                               // => OK: can add Integer (safe downcast)
    list.add(2);                               // => OK: Integer fits in List<? super Integer>
    // Integer x = list.get(0);                // ERROR: can't read as Integer (could be Object)
                                               // => List<Object>.get() returns Object, not Integer
    Object obj = list.get(0);                  // => OK: read as Object (safe upcast)
}                                              // => Consumer (writes values), not producer

// Calling addIntegers with different supertypes
List<Number> numList = new ArrayList<>();      // => List<Number> (Number super Integer)
List<Object> objList = new ArrayList<>();      // => List<Object> (Object super Integer)
addIntegers(numList);                          // => OK: Number super Integer
                                               // => numList now [1, 2] (Integer values)
addIntegers(objList);                          // => OK: Object super Integer
                                               // => objList now [1, 2] (Integer values as Object)

// PECS principle: Producer Extends, Consumer Super
// Use <? extends T> when reading (producing) values from structure
//   - Covariant: List<Derived> is subtype of List<? extends Base>
//   - Safe to read T (or upcast to base), unsafe to write
// Use <? super T> when writing (consuming) values into structure
//   - Contravariant: List<Base> is subtype of List<? super Derived>
//   - Safe to write T (or downcast from derived), unsafe to read as T

// Example demonstrating PECS
public static <T> void copy(List<? extends T> source, List<? super T> dest) {
                                               // => source produces T (read from), dest consumes T (write to)
    for (T item : source) {                    // => Read from source (? extends T)
        dest.add(item);                        // => Write to dest (? super T)
    }                                          // => Type-safe copy using PECS
}

List<Integer> source = Arrays.asList(1, 2, 3);  // => Source list of Integer
List<Number> dest = new ArrayList<>();         // => Destination list of Number
copy(source, dest);                            // => T inferred as Integer
                                               // => source: List<Integer> (Integer extends Integer ✓)
                                               // => dest: List<Number> (Number super Integer ✓)
                                               // => dest now [1, 2, 3] (Integer values as Number)

// Generic constructors (constructor-level type parameters)
class Holder {                                 // => Non-generic class
    private Object value;                      // => Stores any object (uses Object)

    // Generic constructor (type parameter scoped to constructor)
    public <T> Holder(T value) {               // => <T> declares constructor type parameter
                                               // => T inferred from argument type
        this.value = value;                    // => Store as Object (upcast from T)
    }                                          // => Enables type-safe construction
}

Holder h1 = new Holder("text");                // => T inferred as String (from "text")
Holder h2 = new Holder(123);                   // => T inferred as Integer (from 123, autoboxed)
                                               // => Constructor generic, class not generic

// Multiple type parameters (multi-parameter generics)
class Pair<K, V> {                             // => Two type parameters: K (key), V (value)
    private K key;                             // => Field of type K
    private V value;                           // => Field of type V

    // Constructor accepts K and V
    public Pair(K key, V value) {              // => Initialize with key and value
        this.key = key;                        // => Store key of type K
        this.value = value;                    // => Store value of type V
    }

    // Getter for key (returns K)
    public K getKey() {                        // => Returns type K
        return key;                            // => No cast needed
    }

    // Getter for value (returns V)
    public V getValue() {                      // => Returns type V
        return value;                          // => No cast needed
    }
}

// Using Pair with different type combinations
Pair<String, Integer> pair = new Pair<>("Age", 30);
                                               // => K=String, V=Integer (independent types)
String key = pair.getKey();                    // => "Age" (type-safe, no cast)
Integer value = pair.getValue();               // => 30 (type-safe, no cast)

Pair<Integer, String> reversePair = new Pair<>(1, "One");
                                               // => K=Integer, V=String (different order)
Integer numKey = reversePair.getKey();         // => 1
String strValue = reversePair.getValue();      // => "One"

// Type erasure - generics removed at runtime (implementation detail)
List<String> strList = new ArrayList<>();      // => Generic type at compile time
List<Integer> intList2 = new ArrayList<>();    // => Different generic type at compile time
System.out.println(strList.getClass() == intList2.getClass());
                                               // => true (both are ArrayList.class at runtime)
                                               // => Type parameters erased (replaced with Object or bounds)

// Type erasure implications
// 1. Can't create generic array: new T[10] (T erased to Object)
// 2. Can't instanceof with generics: obj instanceof List<String> (erased to List)
// 3. Can't overload with different type parameters: foo(List<String>) vs foo(List<Integer>) (same erasure)
// 4. Static context can't use type parameters: static T field (T doesn't exist at runtime)

// Type erasure example - what compiler sees
class Box<T> {                                 // => Compile time
    private T content;                         // => T is type parameter
}
// After erasure (runtime):
// class Box {
//     private Object content;                // => T erased to Object (unbounded)
// }

class NumberBox<T extends Number> {            // => Compile time with bound
    private T number;                          // => T bounded to Number
}
// After erasure (runtime):
// class NumberBox {
//     private Number number;                 // => T erased to bound (Number)
// }
```

**Key Takeaway**: Generics provide compile-time type safety. Bounded types (`<T extends Type>`) constrain parameters. Wildcards enable flexible APIs: `<?>` (any type), `<? extends T>` (T or subtypes), `<? super T>` (T or supertypes). PECS: Producer Extends, Consumer Super. Type erasure removes generics at runtime.

**Why It Matters**: Generics enable type-safe, reusable code without casting or runtime type checks. Understanding wildcards (? extends T for producers, ? super T for consumers) enables flexible APIs following the PECS principle. Bounded type parameters (T extends Comparable<T>) enable constrained generics for sorting and comparisons. Type erasure causes runtime type information loss—understanding this prevents confusion with reflection, overloading, and arrays. Proper generic design prevents ClassCastException, improves code maintainability, and enables powerful abstractions used throughout the Java ecosystem (Collections, Streams, Optional).

---

## Group 2: Collections Framework Advanced

### Example 19: Comparators and Sorting

`Comparable<T>` defines natural ordering. `Comparator<T>` enables custom ordering. Lambda syntax and method references simplify comparator creation.

**Code**:

```java
import java.util.*;                                 // => Import Collections API (List, Arrays, Comparator)
                                                    // => Arrays provides asList(), Collections provides sort()
                                                    // => Comparator interface in java.util package

// Comparable interface - natural ordering (intrinsic comparison)
class Person implements Comparable<Person> {        // => Person class defines its own natural ordering
                                                    // => Comparable<Person> means compare Person objects
                                                    // => Generic type parameter ensures type-safe comparison
                                                    // => Natural ordering embedded in class (intrinsic)
    String name;                                    // => Name field (String, lexicographic comparison available)
    int age;                                        // => Age field (int, numeric comparison)
                                                    // => Package-private fields (no access modifier)

    public Person(String name, int age) {           // => Constructor accepts name and age
        this.name = name;                           // => Assign name parameter to name field
        this.age = age;                             // => Assign age parameter to age field
    }                                               // => No validation (simplified example)

    // compareTo() defines natural ordering (by age)
    @Override                                       // => Override Comparable.compareTo(T) method
                                                    // => Compiler checks signature matches interface
    public int compareTo(Person other) {            // => Parameter is Person (matches generic type)
                                                    // => Return <0 if this < other, 0 if equal, >0 if this > other
                                                    // => Contract: consistent with equals() for proper behavior
        return Integer.compare(this.age, other.age);
                                                    // => Compare by age (numeric comparison)
                                                    // => Integer.compare(a, b) returns a - b safely (no overflow)
                                                    // => Don't use this.age - other.age (risk of overflow)
                                                    // => Returns negative if this younger, 0 if same age, positive if older
                                                    // => Natural ordering: younger Person < older Person
    }                                               // => compareTo() defines single canonical ordering
                                                    // => TreeSet, TreeMap use natural ordering by default

    @Override                                       // => Override Object.toString() for readable output
    public String toString() {                      // => Returns string representation of Person
        return name + "(" + age + ")";              // => Format: "Alice(30)" (name followed by age)
    }                                               // => Used by System.out.println() and collections
}                                                   // => Comparable enables sorting without external comparator

// Create list of Person objects (unsorted)
List<Person> people = Arrays.asList(                // => Arrays.asList() creates fixed-size list from varargs
                                                    // => List is mutable (can sort) but not resizable (no add/remove)
    new Person("Alice", 30),                        // => Alice is 30 years old (middle age)
    new Person("Bob", 25),                          // => Bob is 25 years old (youngest)
    new Person("Charlie", 35)                       // => Charlie is 35 years old (oldest)
);                                                  // => Initial order: [Alice(30), Bob(25), Charlie(35)]
                                                    // => Not sorted by age or name (insertion order)

Collections.sort(people);                           // => Uses natural ordering (compareTo() from Comparable)
                                                    // => Sorts in-place (mutates list)
                                                    // => No comparator argument (uses Person.compareTo())
                                                    // => Sorting algorithm: Timsort (stable, O(n log n))
System.out.println(people);                         // => [Bob(25), Alice(30), Charlie(35)]
                                                    // => Sorted by age (natural ordering defined in Person)
                                                    // => Bob youngest (25), Alice middle (30), Charlie oldest (35)
                                                    // => Stable sort: equal elements maintain relative order

// Comparator interface - custom ordering (external comparison)
Comparator<Person> byName = new Comparator<Person>() {
                                                    // => Anonymous inner class implementing Comparator<Person>
                                                    // => Custom ordering separate from natural ordering
                                                    // => Comparator<Person> means compare two Person objects
                                                    // => External comparison logic (not embedded in Person)
    @Override                                       // => Override Comparator.compare(T, T) method
                                                    // => Different signature from Comparable.compareTo(T)
    public int compare(Person p1, Person p2) {      // => Two parameters (compare two objects externally)
                                                    // => Return <0 if p1 < p2, 0 if equal, >0 if p1 > p2
        return p1.name.compareTo(p2.name);          // => Compare by name (lexicographic order)
                                                    // => String.compareTo() compares alphabetically
                                                    // => "Alice" < "Bob" < "Charlie" (alphabetical order)
    }                                               // => Custom comparator overrides natural ordering
};                                                  // => byName comparator sorts Person by name, not age

Collections.sort(people, byName);                   // => Sort with explicit comparator (overrides natural ordering)
                                                    // => Uses byName.compare() instead of Person.compareTo()
                                                    // => Sorts in-place (mutates list)
System.out.println(people);                         // => [Alice(30), Bob(25), Charlie(35)]
                                                    // => Sorted by name alphabetically (custom ordering)
                                                    // => Alice < Bob < Charlie (lexicographic order)

// Lambda comparators - concise syntax (functional interface)
Comparator<Person> byAgeLambda = (p1, p2) -> Integer.compare(p1.age, p2.age);
                                                    // => Lambda expression implementing Comparator.compare()
                                                    // => (p1, p2) -> ... replaces anonymous inner class
                                                    // => Type inference: compiler knows p1, p2 are Person
                                                    // => Functional interface: single abstract method (compare)
                                                    // => Equivalent to natural ordering but external
people.sort(byAgeLambda);                           // => List.sort() method (Java 8+, instance method)
                                                    // => Equivalent to Collections.sort(people, byAgeLambda)
                                                    // => More idiomatic (object-oriented style)
System.out.println(people);                         // => [Bob(25), Alice(30), Charlie(35)]
                                                    // => Sorted by age again (same as natural ordering)

// Comparator.comparing() with method references
Comparator<Person> byAge = Comparator.comparing(Person::getAge);
                                                    // => Factory method creates comparator from key extractor
                                                    // => Person::getAge is method reference (equivalent to p -> p.getAge())
                                                    // => Extracts age, then compares using natural ordering
                                                    // => Most concise syntax for simple comparisons
                                                    // => Requires getter method (getAge() here)
Comparator<Person> byNameRef = Comparator.comparing(Person::getName);
                                                    // => Method reference to getName() (extracts String)
                                                    // => Compares names using String.compareTo()
                                                    // => Lexicographic ordering (alphabetical)

// Note: Person class needs getter methods for method references
class Person {                                      // => Extended Person class with getter methods
    // ... previous fields and methods ...
    public String getName() { return name; }        // => Getter for name field (JavaBean convention)
                                                    // => Enables method reference Person::getName
    public int getAge() { return age; }             // => Getter for age field (JavaBean convention)
                                                    // => Enables method reference Person::getAge
}                                                   // => Getters enable Comparator.comparing() factory

// Chaining comparators - thenComparing() for multi-level sorting
Comparator<Person> byAgeThenName = Comparator
    .comparing(Person::getAge)                      // => Primary sort key: age (first comparison)
                                                    // => Extracts age, compares numerically
    .thenComparing(Person::getName);                // => Secondary sort key: name (tie-breaker)
                                                    // => Only used when ages are equal
                                                    // => Breaks ties alphabetically by name
                                                    // => Chain continues stable sort (maintains order for equal elements)

List<Person> people2 = Arrays.asList(               // => New list with duplicate ages (test tie-breaking)
    new Person("Alice", 30),                        // => Alice, age 30 (same as Bob)
    new Person("Bob", 30),                          // => Bob, age 30 (same as Alice, tie-breaker needed)
    new Person("Charlie", 25)                       // => Charlie, age 25 (younger, sorted first)
);                                                  // => Initial order: [Alice(30), Bob(30), Charlie(25)]

people2.sort(byAgeThenName);                        // => Sort by age first, then name for ties
                                                    // => Charlie (25) comes first (youngest)
                                                    // => Alice and Bob both 30, sorted alphabetically
System.out.println(people2);                        // => [Charlie(25), Alice(30), Bob(30)]
                                                    // => First sorted by age (25 < 30)
                                                    // => Then by name for same age (Alice < Bob alphabetically)
                                                    // => Multi-level sorting (composite comparator)

// Reversed comparator - reverse natural or custom ordering
Comparator<Person> byAgeReversed = Comparator
    .comparing(Person::getAge)                      // => Start with age comparison (ascending)
    .reversed();                                    // => Reverse the order (descending)
                                                    // => Negates comparison result (flips sign)
                                                    // => Oldest first, youngest last

people.sort(byAgeReversed);                         // => Sort by age in reverse (descending)
                                                    // => Oldest Person appears first
System.out.println(people);                         // => [Charlie(35), Alice(30), Bob(25)]
                                                    // => Descending age order (35 > 30 > 25)
                                                    // => reversed() flips comparison without rewriting logic

// Natural order comparators - for types implementing Comparable
Comparator<Integer> naturalOrder = Comparator.naturalOrder();
                                                    // => Uses Integer.compareTo() (natural ordering)
                                                    // => Generic method works for any Comparable type
                                                    // => Equivalent to (a, b) -> a.compareTo(b)
List<Integer> nums = Arrays.asList(5, 2, 8, 1, 9); // => Unsorted integers
nums.sort(naturalOrder);                            // => Sort ascending (1 < 2 < 5 < 8 < 9)
                                                    // => [1, 2, 5, 8, 9]
                                                    // => Natural order for Integer is ascending

Comparator<Integer> reverseOrder = Comparator.reverseOrder();
                                                    // => Reverses natural order (descending)
                                                    // => Equivalent to Comparator.naturalOrder().reversed()
nums.sort(reverseOrder);                            // => Sort descending (9 > 8 > 5 > 2 > 1)
                                                    // => [9, 8, 5, 2, 1]
                                                    // => Convenient factory for reverse sorting

// Null handling comparators - safe comparison with null values
Comparator<String> nullsFirst = Comparator.nullsFirst(Comparator.naturalOrder());
                                                    // => Treats null as smaller than any non-null value
                                                    // => Wraps natural order comparator with null handling
                                                    // => Prevents NullPointerException during comparison
List<String> withNulls = Arrays.asList("C", null, "A", "B", null);
                                                    // => List contains null values (5 elements)
                                                    // => Unsorted: [C, null, A, B, null]
withNulls.sort(nullsFirst);                         // => Nulls sorted to beginning, then natural order
                                                    // => Null comparisons handled safely (no NPE)
System.out.println(withNulls);                      // => [null, null, A, B, C]
                                                    // => Nulls first (beginning of list)
                                                    // => Then alphabetical order (A, B, C)

Comparator<String> nullsLast = Comparator.nullsLast(Comparator.naturalOrder());
                                                    // => Treats null as larger than any non-null value
                                                    // => Nulls sorted to end of list
withNulls.sort(nullsLast);                          // => Sort non-nulls first, then nulls
                                                    // => Null-safe comparison (no NPE)
System.out.println(withNulls);                      // => [A, B, C, null, null]
                                                    // => Alphabetical first (A, B, C)
                                                    // => Nulls last (end of list)

// Comparing with custom logic - extract different property types
Comparator<String> byLength = Comparator.comparingInt(String::length);
                                                    // => Specialized factory for int extraction (no boxing)
                                                    // => Extracts String length as int primitive
                                                    // => Compares integers directly (efficient)
                                                    // => comparingInt() avoids Integer wrapper overhead
List<String> words = Arrays.asList("Java", "is", "awesome");
                                                    // => Lengths: "Java" (4), "is" (2), "awesome" (7)
words.sort(byLength);                               // => Sort by string length (shortest first)
                                                    // => 2 < 4 < 7
System.out.println(words);                          // => [is, Java, awesome]
                                                    // => Sorted by length: "is" (2), "Java" (4), "awesome" (7)
                                                    // => Stable sort: equal lengths maintain insertion order

// Complex chaining - combine reversed and multi-level sorting
Comparator<Person> complex = Comparator
    .comparing(Person::getAge)                      // => Primary: age (ascending)
    .reversed()                                     // => Reverse age order (descending, oldest first)
    .thenComparing(Person::getName);                // => Secondary: name (ascending, alphabetical)
                                                    // => reversed() applies only to age, not name
                                                    // => Oldest first, then alphabetical for same age
                                                    // => Complex multi-criteria sorting in one expression
```

**Key Takeaway**: `Comparable<T>` defines natural ordering via `compareTo()`. `Comparator<T>` enables custom ordering via `compare()`. Use `Comparator.comparing()` with method references for concise comparators. Chain comparators with `thenComparing()`. `reversed()`, `nullsFirst()`, `nullsLast()` modify comparison behavior.

**Why It Matters**: Comparators enable custom sorting logic—sort by name, by age, by multiple criteria. They're essential for ordered collections (TreeSet, TreeMap) and sorting APIs (Collections.sort, Arrays.sort, Stream.sorted). Understanding natural ordering (Comparable) vs external ordering (Comparator) determines where to place comparison logic. Comparator.comparing and method references reduce boilerplate while improving readability. Proper comparison logic handles nulls, prevents integer overflow, and maintains transitivity. Sorting is fundamental to data processing, reporting, and UI presentation in production systems.

---

### Example 20: Queue and Deque

Queue provides FIFO (First-In-First-Out) semantics. Deque (Double-Ended Queue) supports both FIFO and LIFO operations. `ArrayDeque` is preferred over legacy `Stack`.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    subgraph Queue["Queue #40;FIFO#41;"]
        Q1["offer#40;#41;"] --> Q2["#91;1,2,3#93;"] --> Q3["poll#40;#41;"]
    end

    subgraph Deque["Deque #40;Both ends#41;"]
        D1["addFirst#40;#41;"] --> D2["#91;1,2,3#93;"] --> D3["addLast#40;#41;"]
        D2 --> D4["removeFirst#40;#41;"]
        D2 --> D5["removeLast#40;#41;"]
    end

    style Q1 fill:#0173B2,color:#fff
    style Q2 fill:#DE8F05,color:#fff
    style Q3 fill:#029E73,color:#fff
    style D2 fill:#CC78BC,color:#fff
```

**Code**:

```java
import java.util.*;                                 // => Import Collections API (Queue, Deque, LinkedList, ArrayDeque)
                                                    // => Queue, Deque are interfaces (multiple implementations)
                                                    // => PriorityQueue implements Queue (priority-based ordering)

// Queue interface - FIFO operations (First-In-First-Out)
Queue<String> queue = new LinkedList<>();           // => Queue is interface, LinkedList is implementation
                                                    // => LinkedList implements both List and Queue
                                                    // => Generic type <String> means queue holds String elements
                                                    // => FIFO: elements retrieved in insertion order
                                                    // => Empty queue initially (no elements)

// offer() - adds element to end (returns boolean, capacity-safe)
queue.offer("First");                               // => true (insertion succeeded)
                                                    // => queue: ["First"] (single element at head)
                                                    // => offer() returns false if capacity constraint violated
                                                    // => LinkedList unbounded (always succeeds)
queue.offer("Second");                              // => true, queue: ["First", "Second"]
                                                    // => "Second" added to tail (after "First")
                                                    // => Queue maintains insertion order
queue.offer("Third");                               // => true, queue: ["First", "Second", "Third"]
                                                    // => "Third" added to tail (last element)
                                                    // => Head is "First", tail is "Third"

// peek() - retrieves but doesn't remove head (non-destructive)
String head = queue.peek();                         // => "First" (head element)
                                                    // => Queue unchanged: ["First", "Second", "Third"]
                                                    // => peek() returns null if queue empty (safe)
                                                    // => element() throws NoSuchElementException if empty (unsafe)

// poll() - retrieves and removes head (destructive, safe)
String removed = queue.poll();                      // => "First" (removed from queue)
                                                    // => queue: ["Second", "Third"] (head removed)
                                                    // => poll() returns null if queue empty (safe)
                                                    // => remove() throws NoSuchElementException if empty (unsafe)
String next = queue.poll();                         // => "Second" (new head after "First" removed)
                                                    // => queue: ["Third"] (only one element remaining)
                                                    // => FIFO: elements removed in insertion order

// poll() on empty queue returns null (safe operation)
queue.clear();                                      // => Remove all elements (queue now empty)
                                                    // => queue.size() is 0
String empty = queue.poll();                        // => null (safe, no exception thrown)
                                                    // => poll() on empty queue returns null (not exception)
                                                    // => Always check for null when using poll()

// Priority Queue - elements ordered by natural order or comparator
PriorityQueue<Integer> pq = new PriorityQueue<>();  // => Min-heap by default (smallest element at head)
                                                    // => Uses natural ordering (Comparable<Integer>)
                                                    // => NOT sorted array (heap structure internally)
                                                    // => O(log n) insertion, O(log n) removal, O(1) peek
pq.offer(5);                                        // => Add 5 to heap (heap property maintained)
                                                    // => Internal heap structure (not [5])
pq.offer(2);                                        // => Add 2 (becomes new minimum)
                                                    // => Heap property: parent ≤ children (min-heap)
pq.offer(8);                                        // => Add 8 (larger than minimum, deeper in heap)
pq.offer(1);                                        // => Add 1 (becomes new minimum, bubbles to top)
                                                    // => Heap structure, not sorted array
                                                    // => Internal: could be [1, 2, 8, 5] or similar (heap order)

System.out.println(pq.poll());                      // => 1 (smallest element, removed from heap)
                                                    // => Output: 1
                                                    // => Heap rebalances after removal
System.out.println(pq.poll());                      // => 2 (next smallest)
                                                    // => Output: 2
System.out.println(pq.poll());                      // => 5 (next smallest)
                                                    // => Output: 5
System.out.println(pq.poll());                      // => 8 (largest, last element)
                                                    // => Output: 8
                                                    // => Elements removed in ascending order (min-heap)

// PriorityQueue with custom comparator (max heap)
PriorityQueue<Integer> maxHeap = new PriorityQueue<>(Comparator.reverseOrder());
                                                    // => Max-heap: largest element at head
                                                    // => Comparator.reverseOrder() reverses natural ordering
                                                    // => Constructor accepts initial capacity and/or comparator
maxHeap.offer(5);                                   // => Add 5 to max-heap
maxHeap.offer(2);                                   // => Add 2 (smaller, deeper in heap)
maxHeap.offer(8);                                   // => Add 8 (becomes new maximum, bubbles to top)
                                                    // => Heap property: parent ≥ children (max-heap)
System.out.println(maxHeap.poll());                 // => 8 (largest first)
                                                    // => Output: 8
                                                    // => Max-heap returns elements in descending order

// Deque interface - double-ended queue (operations on both ends)
Deque<String> deque = new ArrayDeque<>();           // => Deque is interface, ArrayDeque is implementation
                                                    // => ArrayDeque faster than LinkedList (cache-friendly)
                                                    // => Resizable array (dynamic capacity)
                                                    // => No null elements allowed (NullPointerException if tried)
                                                    // => Empty deque initially

// Add to front (head) - LIFO-style operation
deque.addFirst("A");                                // => ["A"] (single element, both head and tail)
                                                    // => addFirst() adds to front of deque
                                                    // => Throws exception if capacity constraint violated
deque.addFirst("B");                                // => ["B", "A"] (B is new head, A pushed right)
                                                    // => B is now at front (index 0 conceptually)
                                                    // => A moved to second position

// Add to back (tail) - FIFO-style operation
deque.addLast("C");                                 // => ["B", "A", "C"] (C added to end)
                                                    // => addLast() appends to tail
                                                    // => Equivalent to offer() for queue operations
deque.addLast("D");                                 // => ["B", "A", "C", "D"] (D is new tail)
                                                    // => D at end, B at front
                                                    // => Deque now has 4 elements

// Remove from front (head) - LIFO-style removal
String first = deque.removeFirst();                 // => "B" (head removed)
                                                    // => deque: ["A", "C", "D"] (B removed, A becomes new head)
                                                    // => removeFirst() throws NoSuchElementException if empty
                                                    // => pollFirst() is null-safe alternative (returns null if empty)

// Remove from back (tail) - stack pop-style removal
String last = deque.removeLast();                   // => "D" (tail removed)
                                                    // => deque: ["A", "C"] (D removed, C becomes new tail)
                                                    // => removeLast() throws NoSuchElementException if empty
                                                    // => pollLast() is null-safe alternative (returns null if empty)

// Peek at both ends (non-destructive queries)
String peekFirst = deque.peekFirst();               // => "A" (head element, deque unchanged)
                                                    // => Returns null if deque empty (safe)
                                                    // => getFirst() throws exception if empty (unsafe)
String peekLast = deque.peekLast();                 // => "C" (tail element, deque unchanged)
                                                    // => Returns null if deque empty (safe)
                                                    // => getLast() throws exception if empty (unsafe)

// Using Deque as Stack (LIFO - Last-In-First-Out)
Deque<Integer> stack = new ArrayDeque<>();          // => Stack-style usage (LIFO semantics)
                                                    // => ArrayDeque faster than legacy Stack class
                                                    // => Stack is synchronized (slower), extends Vector (legacy)
                                                    // => Deque provides push/pop/peek methods for stack operations
stack.push(1);                                      // => [1] (push to front, LIFO)
                                                    // => push() equivalent to addFirst()
                                                    // => 1 is top of stack
stack.push(2);                                      // => [2, 1] (2 pushed on top)
                                                    // => 2 is new top, 1 underneath
stack.push(3);                                      // => [3, 2, 1] (3 pushed on top)
                                                    // => 3 is top of stack (most recently pushed)
                                                    // => Stack grows to the left conceptually

int top = stack.pop();                              // => 3 (pop from front, LIFO)
                                                    // => stack: [2, 1] (3 removed, 2 becomes new top)
                                                    // => pop() equivalent to removeFirst()
                                                    // => pop() throws NoSuchElementException if empty
int peek = stack.peek();                            // => 2 (top element, stack unchanged)
                                                    // => peek() equivalent to peekFirst()
                                                    // => Returns null if stack empty

// ArrayDeque vs LinkedList for Deque implementation
// ArrayDeque: faster (cache-friendly), less memory overhead, no null elements
// => Resizable circular array (better locality of reference)
// => No node overhead (LinkedList has Node objects per element)
// => Prohibits null elements (throws NullPointerException)
// => Better for stack/queue operations (better performance)
// LinkedList: allows nulls, implements both List and Deque
// => Doubly-linked list (node overhead per element)
// => Allows null elements (no restriction)
// => Implements List interface (index-based access)
// => Use only if you need List operations or null elements

// Legacy Stack class (avoid - use Deque instead)
// Stack<Integer> oldStack = new Stack<>();         // Don't use (synchronized, Vector-based)
                                                    // => Stack extends Vector (legacy, synchronized)
                                                    // => Synchronization overhead (slower)
                                                    // => Vector is legacy collection (pre-Collections Framework)
// Use Deque<Integer> stack = new ArrayDeque<>();   // Better (faster, modern)
                                                    // => ArrayDeque is modern, non-synchronized, faster
                                                    // => Provides same stack operations (push/pop/peek)
                                                    // => Better performance (no synchronization overhead)
```

**Key Takeaway**: Queue provides FIFO with `offer()`, `poll()`, `peek()`. PriorityQueue orders elements automatically. Deque supports both ends: `addFirst()`, `addLast()`, `removeFirst()`, `removeLast()`. Use `ArrayDeque` as Stack instead of legacy `Stack` class. `LinkedList` implements both List and Deque.

**Why It Matters**: Queues enable FIFO ordering for task queues, message buffers, breadth-first search. Deques provide double-ended operations—used as stacks (LIFO) or queues (FIFO). BlockingQueue implementations (ArrayBlockingQueue, LinkedBlockingQueue) enable producer-consumer patterns with built-in thread coordination. Understanding when to use which queue (LinkedList vs ArrayDeque vs PriorityQueue) impacts performance. Queues are foundational to asynchronous processing, work distribution, and event handling in concurrent systems. They enable decoupling producers from consumers, improving scalability and resilience.

---

### Example 21: Streams Advanced Operations

Streams support complex transformations through chaining. `flatMap` flattens nested structures. Collectors enable powerful aggregations like grouping and partitioning.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Source["Data Source<br/>#91;#91;1,2#93;,#91;3,4#93;#93;"] --> FlatMap["flatMap<br/>Flatten to #91;1,2,3,4#93;"]
    FlatMap --> Filter["filter<br/>#40;even#41;"]
    Filter --> Collect["Collectors<br/>groupingBy/partitioningBy"]
    Collect --> Result["Result<br/>#123;true:#91;2,4#93;#125;"]

    style Source fill:#0173B2,color:#fff
    style FlatMap fill:#DE8F05,color:#fff
    style Filter fill:#029E73,color:#fff
    style Collect fill:#CC78BC,color:#fff
    style Result fill:#CA9161,color:#fff
```

**Code**:

```java
import java.util.*;                                 // => Import Collections API (List, Arrays, Map, Optional)
import java.util.stream.*;                          // => Import Streams API (Stream, Collectors, IntStream)
                                                    // => Collectors utility class for terminal operations

// flatMap() - flattens nested structures (one-to-many transformation)
List<List<Integer>> nested = Arrays.asList(         // => List of lists (nested structure, 2 levels deep)
    Arrays.asList(1, 2),                            // => First inner list: [1, 2]
    Arrays.asList(3, 4),                            // => Second inner list: [3, 4]
    Arrays.asList(5, 6)                             // => Third inner list: [5, 6]
);                                                  // => Nested structure: [[1,2], [3,4], [5,6]]
                                                    // => Cannot directly stream integers (need flatten)

List<Integer> flattened = nested.stream()          // => Stream<List<Integer>> (stream of lists)
    .flatMap(list -> list.stream())                // => Flattens [[1,2],[3,4],[5,6]] to Stream<Integer>
                                                    // => Each List<Integer> mapped to Stream<Integer>
                                                    // => flatMap merges all streams into single stream
                                                    // => Result: Stream of integers 1, 2, 3, 4, 5, 6
                                                    // => map() would give Stream<Stream<Integer>> (nested, wrong)
    .collect(Collectors.toList());                  // => [1, 2, 3, 4, 5, 6] (single flat list)
                                                    // => toList() creates new ArrayList from stream elements
                                                    // => Collects all integers into single list (flattened)

// flatMap with Strings - split and flatten
List<String> words = Arrays.asList("Hello", "World");
                                                    // => Two strings: "Hello" (5 chars), "World" (5 chars)
List<String> letters = words.stream()              // => Stream<String> with 2 elements
    .flatMap(word -> Arrays.stream(word.split("")))
                                                    // => split("") splits each word into character array
                                                    // => "Hello" -> ["H","e","l","l","o"]
                                                    // => Arrays.stream() converts String[] to Stream<String>
                                                    // => flatMap flattens all character streams into one
                                                    // => Stream: "H","e","l","l","o","W","o","r","l","d"
    .distinct()                                     // => Remove duplicates (stateful operation)
                                                    // => "l" and "o" appear multiple times, kept once
                                                    // => Stream becomes: "H","e","l","o","W","r","d"
    .collect(Collectors.toList());                  // => [H, e, l, o, W, r, d] (unique letters)
                                                    // => Order of first occurrence preserved
                                                    // => Note: "l" appears once despite two "l" in "Hello"

// Collectors.groupingBy() - group elements by classifier function
class Person {                                      // => Simple Person class for demonstration
    String name;                                    // => Name field (String)
    String city;                                    // => City field (grouping key)
    int age;                                        // => Age field (for partitioning)

    Person(String name, String city, int age) {     // => Constructor
        this.name = name;                           // => Assign name
        this.city = city;                           // => Assign city
        this.age = age;                             // => Assign age
    }                                               // => Package-private fields (simplified)
}

List<Person> people = Arrays.asList(                // => List of 4 people (2 from NYC, 2 from LA)
    new Person("Alice", "NYC", 30),                 // => Alice in NYC, age 30
    new Person("Bob", "LA", 25),                    // => Bob in LA, age 25
    new Person("Charlie", "NYC", 35),               // => Charlie in NYC, age 35
    new Person("David", "LA", 28)                   // => David in LA, age 28
);                                                  // => People distributed across two cities

Map<String, List<Person>> byCity = people.stream() // => Stream<Person> with 4 elements
    .collect(Collectors.groupingBy(p -> p.city));   // => Group by city (classifier function)
                                                    // => groupingBy() creates Map<String, List<Person>>
                                                    // => Key: city name (String from p.city)
                                                    // => Value: List<Person> with that city
                                                    // => {NYC=[Alice, Charlie], LA=[Bob, David]}
                                                    // => Two groups: NYC has 2 people, LA has 2 people

// Collectors.partitioningBy() - split into two groups (true/false, binary split)
Map<Boolean, List<Person>> byAge = people.stream() // => Stream<Person> with 4 elements
    .collect(Collectors.partitioningBy(p -> p.age >= 30));
                                                    // => Predicate: age >= 30 (boolean result)
                                                    // => partitioningBy() creates Map<Boolean, List<Person>>
                                                    // => Key: true (age >= 30) or false (age < 30)
                                                    // => Value: List<Person> matching predicate
                                                    // => {false=[Bob, David], true=[Alice, Charlie]}
                                                    // => Bob (25) and David (28) in false group
                                                    // => Alice (30) and Charlie (35) in true group
                                                    // => Always has both keys (true and false) even if one empty

// Collectors.joining() - concatenate strings with delimiter
String names = people.stream()                      // => Stream<Person> with 4 elements
    .map(p -> p.name)                               // => Extract name from each Person (map to String)
                                                    // => Stream<String> with names: "Alice", "Bob", "Charlie", "David"
    .collect(Collectors.joining(", "));             // => "Alice, Bob, Charlie, David"
                                                    // => joining(", ") concatenates with comma-space delimiter
                                                    // => Result is single String (not list)

String namesWithPrefix = people.stream()            // => Stream<Person> with 4 elements
    .map(p -> p.name)                               // => Extract names to Stream<String>
    .collect(Collectors.joining(", ", "Names: ", "."));
                                                    // => "Names: Alice, Bob, Charlie, David."
                                                    // => joining(delimiter, prefix, suffix)
                                                    // => "Names: " is prefix, "." is suffix
                                                    // => Elements joined with ", " delimiter in between

// Stream.of() - create stream from elements (varargs)
Stream<String> stream = Stream.of("A", "B", "C");  // => Stream<String> with 3 elements
                                                    // => Factory method (varargs parameter)
                                                    // => Equivalent to Arrays.asList(...).stream()
                                                    // => Stream.of() is more concise for ad-hoc streams

// Primitive streams - IntStream, LongStream, DoubleStream (avoid boxing)
IntStream ints = IntStream.range(1, 5);             // => 1, 2, 3, 4 (5 excluded, half-open range)
                                                    // => IntStream avoids Integer wrapper overhead
                                                    // => Specialized stream for int primitives
                                                    // => range(start, end) excludes end (half-open [1,5))
IntStream intsInclusive = IntStream.rangeClosed(1, 5);
                                                    // => 1, 2, 3, 4, 5 (5 included, closed range)
                                                    // => rangeClosed(start, end) includes end ([1,5])
                                                    // => Use rangeClosed for inclusive ranges

int sum = IntStream.range(1, 101).sum();            // => 5050 (sum of 1 to 100)
                                                    // => range(1, 101) generates 1 to 100 (101 excluded)
                                                    // => sum() is terminal operation (IntStream method)
                                                    // => Returns int primitive (no Optional)
                                                    // => Formula: n*(n+1)/2 = 100*101/2 = 5050
double average = IntStream.range(1, 6).average().getAsDouble();
                                                    // => 3.0 (average of 1, 2, 3, 4, 5)
                                                    // => average() returns OptionalDouble (stream might be empty)
                                                    // => getAsDouble() extracts double value (throws if empty)
                                                    // => (1+2+3+4+5)/5 = 15/5 = 3.0

// Parallel streams - leverage multiple cores (fork-join framework)
long count = IntStream.range(1, 1000000)            // => 999999 integers (1 to 999999)
    .parallel()                                     // => Enable parallel processing (fork-join pool)
                                                    // => Splits stream into chunks, processes concurrently
                                                    // => Uses common ForkJoinPool (CPU cores)
    .filter(n -> n % 2 == 0)                        // => Keep only even numbers
                                                    // => Parallel filtering across multiple threads
    .count();                                       // => 499999 (count of even numbers)
                                                    // => Terminal operation combines partial results
                                                    // => Half of 999999 integers are even (499999)

// reduce() with accumulator - more control over aggregation
List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
                                                    // => List of 5 integers

// reduce(identity, accumulator) - sequential reduction
int sumReduced = numbers.stream()                  // => Stream<Integer> with 5 elements
    .reduce(0, (a, b) -> a + b);                    // => 15 (sum of 1+2+3+4+5)
                                                    // => 0 is identity value (neutral element for +)
                                                    // => (a, b) -> a + b is accumulator (BinaryOperator)
                                                    // => Execution: ((((0+1)+2)+3)+4)+5 = 15
                                                    // => Returns int (not Optional) due to identity

// reduce(identity, accumulator, combiner) for parallel streams
int sumParallel = numbers.parallelStream()          // => Parallel stream (fork-join processing)
    .reduce(
        0,                                          // => Identity value (neutral for +)
        (a, b) -> a + b,                            // => Accumulator (sequential combine within chunk)
                                                    // => Combines elements within each parallel partition
        (a, b) -> a + b                             // => Combiner (parallel combine across chunks)
                                                    // => Combines partial results from different threads
    );                                              // => 15 (same result as sequential)
                                                    // => Combiner used only in parallel execution
                                                    // => Identity + accumulator + combiner ensure correctness

// Optional from stream operations - handling absence safely
Optional<Integer> max = numbers.stream()           // => Stream<Integer> with 5 elements
    .max(Comparator.naturalOrder());                // => Optional[5] (maximum element)
                                                    // => max() returns Optional<Integer> (stream might be empty)
                                                    // => naturalOrder() uses Integer.compareTo()
                                                    // => 5 is largest element
                                                    // => Optional.isPresent() is true

Optional<Integer> any = numbers.stream()           // => Stream<Integer> with 5 elements
    .filter(n -> n > 10)                            // => Filter: keep only n > 10
                                                    // => No elements satisfy predicate (all ≤ 5)
                                                    // => Stream becomes empty after filter
    .findAny();                                     // => Optional.empty (no element > 10)
                                                    // => findAny() returns Optional<Integer>
                                                    // => Optional.isPresent() is false (no value)

// peek() - debug intermediate steps (side effects for debugging)
List<Integer> result = numbers.stream()             // => Stream<Integer> with 5 elements
    .peek(n -> System.out.println("Original: " + n))
                                                    // => Prints: "Original: 1", "Original: 2", etc.
                                                    // => peek() performs action without transforming stream
                                                    // => Executes for each element (side effect)
    .filter(n -> n % 2 == 0)                        // => Keep only even numbers (2, 4)
                                                    // => Stream becomes 2 elements
    .peek(n -> System.out.println("Filtered: " + n))
                                                    // => Prints: "Filtered: 2", "Filtered: 4"
                                                    // => peek() after filter sees only filtered elements
    .map(n -> n * n)                                // => Square each element (2*2=4, 4*4=16)
                                                    // => Stream becomes 4, 16
    .peek(n -> System.out.println("Mapped: " + n))
                                                    // => Prints: "Mapped: 4", "Mapped: 16"
                                                    // => peek() after map sees squared values
    .collect(Collectors.toList());                  // => [4, 16] (collected result)
                                                    // => Terminal operation triggers all intermediate ops
                                                    // => peek() useful for debugging stream pipeline

// Collectors with downstream collectors - multi-level aggregation
Map<String, Long> cityCount = people.stream()      // => Stream<Person> with 4 elements
    .collect(Collectors.groupingBy(
        p -> p.city,                                // => Classifier: group by city (NYC, LA)
        Collectors.counting()                       // => Downstream collector: count per group
                                                    // => counting() returns Long (count of elements)
    ));                                             // => {NYC=2, LA=2}
                                                    // => NYC has 2 people (Alice, Charlie)
                                                    // => LA has 2 people (Bob, David)
                                                    // => Downstream collector applied to each group

Map<String, Integer> citySumAge = people.stream()  // => Stream<Person> with 4 elements
    .collect(Collectors.groupingBy(
        p -> p.city,                                // => Classifier: group by city
        Collectors.summingInt(p -> p.age)           // => Downstream: sum ages per group
                                                    // => summingInt() extracts int, sums values
    ));                                             // => {NYC=65, LA=53}
                                                    // => NYC: Alice(30) + Charlie(35) = 65
                                                    // => LA: Bob(25) + David(28) = 53
                                                    // => Downstream collector aggregates per group
```

**Key Takeaway**: `flatMap()` flattens nested structures into single stream. `Collectors.groupingBy()` groups by classifier. `Collectors.partitioningBy()` splits into true/false groups. `Collectors.joining()` concatenates strings. Primitive streams (`IntStream`, `LongStream`, `DoubleStream`) optimize numeric operations. `parallel()` enables multi-core processing. `peek()` debugs intermediate operations.

**Why It Matters**: Advanced stream operations (flatMap, reduce, collect) enable complex data transformations declaratively. flatMap handles nested structures (lists of lists), reduce aggregates (sum, product, concatenation), and collectors build results (grouping, partitioning, joining). Understanding stateful operations (sorted, distinct) and their performance implications prevents bottlenecks. Stream pipelines eliminate boilerplate loops, reduce bugs, and improve readability. They're essential for data processing, reporting, and ETL pipelines. Mastering streams enables elegant solutions to otherwise complex data manipulation problems.

---

## Group 3: Functional Programming

### Example 22: Lambda Expressions

Lambdas provide concise syntax for functional interfaces (interfaces with one abstract method). Method references simplify common lambda patterns. Closures capture effectively final variables.

**Code**:

```java
import java.util.*;                            // => Import collections (List, Arrays) for lambda examples
                                               // => Used for collection processing with lambdas later
import java.util.function.*;                   // => Import functional interfaces package
                                               // => Contains Predicate, Function, Consumer, Supplier, BiFunction

// Lambda syntax: (parameters) -> expression
// Basic lambda forms demonstrating parameter variations
Runnable r1 = () -> System.out.println("Hello");  // => No parameters: () required
                                               // => Lambda implements Runnable.run() (no params, void return)
                                               // => Functional interface: exactly one abstract method
                                               // => Concise alternative to anonymous class
Consumer<String> c1 = s -> System.out.println(s);  // => One parameter: parentheses optional
                                               // => s is parameter (type inferred as String from Consumer<String>)
                                               // => Lambda implements Consumer.accept(String) method
                                               // => Type inference eliminates need for (String s) declaration
BiFunction<Integer, Integer, Integer> add = (a, b) -> a + b;  // => Multiple parameters: parentheses required
                                               // => a, b inferred as Integer from BiFunction<Integer, Integer, Integer>
                                               // => Lambda body is expression (no return keyword needed)
                                               // => Returns a + b (Integer result, autoboxing applies)
Function<String, Integer> length = s -> s.length();  // => Single expression lambda (implicit return)
                                               // => s.length() is expression, return implied
                                               // => Function<String, Integer>: takes String, returns Integer
                                               // => Replaces: s -> { return s.length(); } (equivalent)

// Lambda with block body (requires braces and explicit return for non-void)
Consumer<String> printer = s -> {             // => Block body: multiple statements inside { }
                                               // => s is parameter (String inferred from Consumer<String>)
    String upper = s.toUpperCase();            // => Local variable inside lambda (intermediate value)
                                               // => upper is "HELLO" if s is "hello"
    System.out.println(upper);                 // => Print transformed value (uppercase)
};                                             // => No return needed (Consumer returns void)
                                               // => Block body requires { } for multiple statements
                                               // => Non-void block would need explicit return statement

// Functional interfaces (exactly one abstract method)
// Custom functional interface for demonstration
@FunctionalInterface                           // => Optional annotation (enforces single abstract method)
                                               // => Compile error if multiple abstract methods added
                                               // => Documents intent: interface designed for lambdas
interface Calculator {                         // => Custom functional interface (single abstract method)
                                               // => Can have default/static methods (not counted)
    int calculate(int a, int b);               // => Single abstract method (functional method)
                                               // => Signature: two ints, returns int
}                                              // => Functional interface enables lambda implementation

Calculator multiply = (a, b) -> a * b;         // => Lambda implements Calculator.calculate()
                                               // => (a, b) parameters match calculate(int, int)
                                               // => a * b expression returns int (matches return type)
                                               // => multiply is functional interface reference
int result = multiply.calculate(5, 3);         // => Invoke lambda through calculate() method
                                               // => 5 * 3 = 15 (lambda body executed)
                                               // => result is 15 (int primitive)

// Built-in functional interfaces (java.util.function package)
// Java provides common functional interfaces to avoid custom ones

// Predicate<T> - takes T, returns boolean (for filtering/testing)
Predicate<Integer> isEven = n -> n % 2 == 0;  // => Lambda implements Predicate.test(Integer)
                                               // => n % 2 == 0 returns boolean (true if even)
                                               // => Predicate<Integer>: takes Integer, returns boolean
System.out.println(isEven.test(4));            // => true (4 % 2 == 0, even number)
                                               // => test() method invokes lambda body
System.out.println(isEven.test(5));            // => false (5 % 2 == 1, odd number)
                                               // => Predicate commonly used in Stream.filter()

// Function<T, R> - takes T, returns R (for transformation/mapping)
Function<String, Integer> strLength = s -> s.length();  // => Lambda implements Function.apply(String)
                                               // => Takes String, returns Integer (length)
                                               // => Transformation function (String -> Integer)
System.out.println(strLength.apply("Hello"));  // => 5 ("Hello".length() = 5)
                                               // => apply() method invokes lambda body
                                               // => Function commonly used in Stream.map()

// Consumer<T> - takes T, returns void (for side effects)
Consumer<String> print = s -> System.out.println(s);  // => Lambda implements Consumer.accept(String)
                                               // => Takes String, performs action (println), no return
                                               // => Represents side effect (I/O operation)
print.accept("Message");                       // => Output: Message (prints to stdout)
                                               // => accept() method invokes lambda body
                                               // => Consumer commonly used in forEach()

// Supplier<T> - takes nothing, returns T (for lazy generation)
Supplier<Double> random = () -> Math.random(); // => Lambda implements Supplier.get()
                                               // => No parameters (empty ())
                                               // => Returns Double (random number 0.0-1.0)
System.out.println(random.get());              // => random double (e.g., 0.7234567890123456)
                                               // => get() method invokes lambda body
                                               // => Supplier enables lazy evaluation

// BiFunction<T, U, R> - takes two parameters T and U, returns R
BiFunction<Integer, Integer, Integer> sum = (a, b) -> a + b;  // => Lambda implements BiFunction.apply(Integer, Integer)
                                               // => Takes two Integers, returns Integer
                                               // => Binary operation (two inputs)
System.out.println(sum.apply(10, 20));         // => 30 (10 + 20 = 30)
                                               // => apply() method invokes lambda body
                                               // => BiFunction for operations needing two arguments

// Method references - ClassName::methodName (concise lambda alternatives)
// 1. Static method reference: ClassName::staticMethod
Function<String, Integer> parse = Integer::parseInt;  // => Equivalent to: s -> Integer.parseInt(s)
                                               // => parseInt is static method on Integer class
                                               // => Method reference syntax: Class::method
                                               // => Function<String, Integer>: String input, Integer output
int num = parse.apply("123");                  // => 123 (parses String "123" to Integer 123)
                                               // => apply() calls Integer.parseInt("123")
                                               // => num is 123 (int primitive, unboxed from Integer)

// 2. Instance method reference of specific object: object::instanceMethod
String prefix = "Hello, ";                     // => String instance (specific object)
                                               // => concat() is instance method on String
Function<String, String> greeter = prefix::concat;  // => Equivalent to: s -> prefix.concat(s)
                                               // => Method reference on specific object (prefix)
                                               // => Captures prefix variable (closure over prefix)
System.out.println(greeter.apply("World"));    // => "Hello, World" (prefix.concat("World"))
                                               // => apply() calls prefix.concat("World")
                                               // => Concatenates "Hello, " + "World"

// 3. Instance method of arbitrary object of particular type: ClassName::instanceMethod
Function<String, String> toUpper = String::toUpperCase;  // => Equivalent to: s -> s.toUpperCase()
                                               // => toUpperCase() is instance method (not static)
                                               // => Applies to any String instance (arbitrary object)
                                               // => First parameter becomes the object (s.toUpperCase())
System.out.println(toUpper.apply("java"));     // => "JAVA" ("java".toUpperCase())
                                               // => apply() calls "java".toUpperCase()
                                               // => Returns uppercase version

// 4. Constructor reference: ClassName::new
Supplier<ArrayList<String>> listSupplier = ArrayList::new;  // => Equivalent to: () -> new ArrayList<>()
                                               // => Constructor reference (no-arg constructor)
                                               // => Supplier<ArrayList<String>>: no params, returns ArrayList
ArrayList<String> list = listSupplier.get();   // => new ArrayList<>() (empty list)
                                               // => get() invokes ArrayList no-arg constructor
                                               // => list is empty ArrayList (size=0, capacity=10)

Function<Integer, int[]> arrayMaker = int[]::new;  // => Equivalent to: size -> new int[size]
                                               // => Array constructor reference (primitive array)
                                               // => Parameter is array size (Integer)
int[] array = arrayMaker.apply(5);             // => new int[5] (array of 5 ints, all 0)
                                               // => apply(5) creates int array with length 5
                                               // => array is [0, 0, 0, 0, 0] (default int values)

// Effectively final in closures (lambdas capture variables from enclosing scope)
int multiplier = 10;                           // => Local variable (effectively final after initialization)
                                               // => Effectively final: not modified after first assignment
                                               // => Lambdas can capture effectively final variables
Function<Integer, Integer> multiplyBy10 = n -> n * multiplier;  // => Lambda captures multiplier variable
                                               // => Closure: lambda references variable from outer scope
                                               // => multiplier value (10) captured at lambda creation
                                               // => n is parameter, multiplier is captured variable
System.out.println(multiplyBy10.apply(5));     // => 50 (5 * 10 = 50)
                                               // => apply(5) executes n * multiplier with multiplier=10
                                               // => Closure maintains reference to multiplier

// multiplier = 20;                            // ERROR: would make multiplier not effectively final
                                               // => Compile error: local variables referenced from lambda must be final or effectively final
                                               // => Once captured, variable can't be reassigned
                                               // => Prevents inconsistent state in lambda execution

// Lambda vs anonymous inner class (conciseness comparison)
// Anonymous inner class (verbose syntax)
Runnable r2 = new Runnable() {                 // => Anonymous class implements Runnable interface
                                               // => Requires full class definition with braces
    @Override                                  // => Override annotation (optional but recommended)
    public void run() {                        // => Implement abstract run() method
        System.out.println("Anonymous");       // => Method body (what to execute)
    }                                          // => Verbose: multiple lines for simple task
};                                             // => Semicolon ends statement (not class definition)

// Lambda (much more concise for same result)
Runnable r3 = () -> System.out.println("Lambda");  // => Same functionality, one line
                                               // => Lambda replaces anonymous class boilerplate
                                               // => () -> expression is entire implementation
                                               // => Much more readable and maintainable

// Lambdas in collections (forEach() example)
List<String> names = Arrays.asList("Alice", "Bob", "Charlie");  // => List of 3 strings (immutable list)
                                               // => Arrays.asList() creates fixed-size list
names.forEach(name -> System.out.println(name));  // => Consumer lambda prints each element
                                               // => forEach() accepts Consumer<String>
                                               // => Output: Alice \n Bob \n Charlie (one per line)
names.forEach(System.out::println);            // => Method reference (same as above)
                                               // => Equivalent to: name -> System.out.println(name)
                                               // => More concise when lambda just delegates to method

// Lambdas with streams (map() transformation example)
List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);  // => List of 5 integers
                                               // => Source data for stream processing
List<Integer> doubled = numbers.stream()       // => Create stream from list (stream source)
                                               // => Stream enables functional-style operations
    .map(n -> n * 2)                           // => Transform each element: multiply by 2
                                               // => map() accepts Function<Integer, Integer>
                                               // => Intermediate operation (returns Stream)
                                               // => Transforms: [1,2,3,4,5] -> [2,4,6,8,10]
    .collect(Collectors.toList());             // => Terminal operation: collect to List
                                               // => Triggers stream execution (lazy evaluation)
                                               // => doubled is [2, 4, 6, 8, 10] (new list)

// Composing functional interfaces (combining predicates)
Predicate<Integer> greaterThan5 = n -> n > 5;  // => Predicate: true if n > 5
                                               // => First condition for composition
Predicate<Integer> lessThan10 = n -> n < 10;  // => Predicate: true if n < 10
                                               // => Second condition for composition
Predicate<Integer> between5And10 = greaterThan5.and(lessThan10);  // => Composed predicate: both must be true
                                               // => and() combines predicates with logical AND
                                               // => Equivalent to: n -> (n > 5) && (n < 10)
                                               // => Range check: 5 < n < 10

System.out.println(between5And10.test(7));     // => true (7 > 5 AND 7 < 10, both conditions met)
                                               // => test() evaluates composed predicate
System.out.println(between5And10.test(12));    // => false (12 > 5 BUT 12 >= 10, second condition fails)
                                               // => AND requires both conditions true
```

**Key Takeaway**: Lambdas `(params) -> expression` provide concise syntax for functional interfaces. Built-in interfaces: `Predicate<T>`, `Function<T,R>`, `Consumer<T>`, `Supplier<T>`, `BiFunction<T,U,R>`. Method references (`::`) simplify lambdas. Closures capture effectively final variables. Lambdas are more concise than anonymous inner classes.

**Why It Matters**: Lambda expressions enable functional programming in Java—concise syntax for single-method implementations. They eliminate verbose anonymous classes, improving code readability. Understanding functional interfaces (Predicate, Function, Consumer, Supplier) enables effective library usage. Lambdas integrate with Streams, CompletableFuture, and event handling, making asynchronous and concurrent code more manageable. However, complex lambdas should use method references or named methods for clarity. Lambdas are foundational to modern Java development, enabling declarative programming patterns that reduce boilerplate and improve maintainability.

---

### Example 23: Optional for Null Safety

`Optional<T>` explicitly represents potential absence of a value, replacing null checks. Chaining methods avoid nested conditionals. Use for return types, not fields or parameters.

**Code**:

```java
import java.util.Optional;                     // => Import Optional class for null-safe value handling
                                               // => Optional<T> is container for value that may be absent
import java.util.List;                         // => Import List for stream example later
                                               // => Used with Optional.stream() in Java 9+
import java.util.stream.Collectors;            // => Import Collectors for collecting stream results
                                               // => Used in flatMap example

// Creating Optional instances (three factory methods)
Optional<String> present = Optional.of("value");  // => Optional["value"] (contains "value")
                                               // => of() requires non-null argument (throws NPE if null)
                                               // => Use when you're certain value is non-null
                                               // => present.isPresent() is true
// Optional<String> nullValue = Optional.of(null);  // ERROR: NullPointerException at creation
                                               // => of(null) throws NPE immediately (fail-fast)
                                               // => Use ofNullable() when value might be null

Optional<String> maybe = Optional.ofNullable("value");  // => Optional["value"] (contains "value")
                                               // => ofNullable() accepts null argument safely
                                               // => If argument non-null, returns Optional.of(value)
                                               // => If argument null, returns Optional.empty()
Optional<String> empty = Optional.ofNullable(null);  // => Optional.empty (no value)
                                               // => Null argument produces empty Optional
                                               // => empty.isPresent() is false
                                               // => Safe alternative to Optional.of() for nullable values

Optional<String> emptyDirect = Optional.empty();  // => Optional.empty (explicit empty creation)
                                               // => Factory method for empty Optional
                                               // => Equivalent to Optional.ofNullable(null)
                                               // => Use when you know there's no value

// Checking presence (before extracting value)
if (present.isPresent()) {                     // => true (present contains "value")
                                               // => isPresent() checks if value exists
    String value = present.get();              // => "value" (safe because we checked isPresent())
                                               // => get() extracts value (throws if empty)
}                                              // => Always check isPresent() before calling get()
                                               // => Better: use orElse/orElseGet instead of get()

if (empty.isEmpty()) {                         // => true (empty has no value)
                                               // => isEmpty() is opposite of isPresent() (Java 11+)
    System.out.println("Empty!");              // => Output: Empty! (prints because empty)
}                                              // => isEmpty() more readable than !isPresent()

// ifPresent() - execute Consumer if value present (functional approach)
present.ifPresent(value -> System.out.println(value));  // => Output: value (lambda executed)
                                               // => ifPresent() accepts Consumer<T>
                                               // => Lambda receives contained value as argument
                                               // => Executed only if Optional contains value
empty.ifPresent(value -> System.out.println(value));  // => nothing (lambda not executed, empty)
                                               // => Empty Optional: Consumer not called
                                               // => Avoids explicit if (isPresent()) check

// orElse() - provide default value (eager evaluation)
String value1 = present.orElse("default");     // => "value" (present has value, default not used)
                                               // => orElse() returns contained value if present
                                               // => Returns default if empty
String value2 = empty.orElse("default");       // => "default" (empty, so default returned)
                                               // => Default argument evaluated even if not needed
                                               // => Use orElseGet() for expensive defaults

// orElseGet() - lazy default (Supplier called only if empty)
String value3 = empty.orElseGet(() -> "computed default");  // => "computed default" (Supplier executed)
                                               // => Supplier lambda called only if Optional empty
                                               // => Efficient for expensive computations
String value4 = present.orElseGet(() -> "expensive computation");  // => "value" (Supplier not called)
                                               // => present has value, Supplier never executed
                                               // => Lazy evaluation: computation avoided
                                               // => Prefer orElseGet() for expensive defaults

// orElseThrow() - throw exception if empty (fail-fast)
try {
    String value5 = empty.orElseThrow();       // => throws NoSuchElementException (empty Optional)
                                               // => orElseThrow() with no argument (Java 10+)
                                               // => Throws NoSuchElementException if empty
} catch (Exception e) {                        // => Catch block executes (exception thrown)
    System.out.println("Exception: " + e.getMessage());  // => Output: No value present
}                                              // => Use when absence is exceptional condition

String value6 = empty.orElseThrow(() -> new IllegalStateException("Missing!"));
                                               // => throws IllegalStateException("Missing!")
                                               // => Custom exception via Supplier lambda
                                               // => Supplier called only if empty (lazy)
                                               // => Use for domain-specific exceptions

// map() - transform value if present (functor operation)
Optional<String> name = Optional.of("alice");  // => Optional["alice"] (lowercase name)
Optional<String> upper = name.map(String::toUpperCase);  // => Optional["ALICE"] (transformed)
                                               // => map() applies function to contained value
                                               // => Returns new Optional with transformed value
                                               // => String::toUpperCase method reference

Optional<String> emptyUpper = Optional.<String>empty()  // => Optional.empty (no value)
                                               // => Explicit type parameter <String> (type inference)
    .map(String::toUpperCase);                 // => Optional.empty (map not applied, empty in = empty out)
                                               // => map() on empty Optional returns empty
                                               // => Transformation function never called
                                               // => Safe chaining: no null checks needed

// flatMap() - flatten nested Optionals (monad operation)
class Person {                                 // => Inner class for demonstration
    Optional<String> getEmail() {              // => Returns Optional (email may be absent)
        return Optional.of("person@example.com");  // => Returns present Optional with email
    }                                          // => Method return type is Optional<String>
}                                              // => Class demonstrates nested Optional problem

Optional<Person> person = Optional.of(new Person());  // => Optional[Person] (contains Person instance)

// map returns Optional<Optional<String>> - nested! (wrong)
Optional<Optional<String>> nestedEmail = person.map(Person::getEmail);
                                               // => Optional[Optional["person@example.com"]] (nested)
                                               // => map() wraps return value in Optional
                                               // => getEmail() already returns Optional
                                               // => Result: Optional<Optional<String>> (nested containers)
                                               // => Hard to extract: need nestedEmail.get().get()

// flatMap flattens to Optional<String> (correct)
Optional<String> email = person.flatMap(Person::getEmail);  // => Optional["person@example.com"] (flattened)
                                               // => flatMap() unwraps Optional returned by function
                                               // => Avoids nesting: Optional<String> not Optional<Optional<String>>
                                               // => Use flatMap when function returns Optional
                                               // => Use map when function returns plain value

// filter() - keep value if matches predicate (conditional retention)
Optional<Integer> number = Optional.of(42);    // => Optional[42] (contains 42)
Optional<Integer> filtered = number.filter(n -> n > 50);  // => Optional.empty (42 not > 50, filtered out)
                                               // => filter() tests value with Predicate
                                               // => Returns empty if predicate false
                                               // => Returns same Optional if predicate true
Optional<Integer> kept = number.filter(n -> n > 40);  // => Optional[42] (42 > 40, kept)
                                               // => Predicate true: value retained
                                               // => Original Optional returned unchanged

// Chaining operations (pipeline of transformations)
Optional<String> result = Optional.of("  HELLO  ")  // => Optional["  HELLO  "] (whitespace padding)
    .map(String::trim)                         // => Optional["HELLO"] (whitespace removed)
                                               // => trim() removes leading/trailing whitespace
    .map(String::toLowerCase)                  // => Optional["hello"] (converted to lowercase)
                                               // => toLowerCase() transforms to lowercase
    .filter(s -> s.length() > 3)               // => Optional["hello"] (length 5 > 3, kept)
                                               // => filter() checks length condition
    .map(s -> s + "!");                        // => Optional["hello!"] (exclamation appended)
                                               // => Final transformation adds suffix
                                               // => Chaining avoids intermediate variables

String finalResult = result.orElse("default"); // => "hello!" (extract with default fallback)
                                               // => orElse() unwraps Optional to String
                                               // => If any step returned empty, default used

// Example: replacing null checks (verbose vs Optional)
// Traditional null checking (verbose, error-prone)
String getNullable() {                         // => Method that may return null
    return Math.random() > 0.5 ? "value" : null;  // => 50% chance of null return
}                                              // => No type-level indication of possible null

String traditional = getNullable();            // => May be "value" or null (uncertain)
String processed;                              // => Declare result variable (uninitialized)
if (traditional != null) {                     // => Null check (must remember to check)
    String upper = traditional.toUpperCase();  // => Safe: traditional not null
    if (upper.length() > 3) {                  // => Nested condition (increasing complexity)
        processed = upper;                     // => Assign transformed value
    } else {                                   // => Length <= 3 case
        processed = "default";                 // => Assign default
    }                                          // => Two levels of nesting
} else {                                       // => Null case (must handle)
    processed = "default";                     // => Assign default (code duplication)
}                                              // => Verbose: 12 lines for simple transformation

// Optional approach (cleaner, type-safe)
Optional<String> getOptional() {               // => Method signature indicates possible absence
    return Math.random() > 0.5 ? Optional.of("value") : Optional.empty();  // => Returns Optional
}                                              // => Type system documents nullability

String processedOptional = getOptional()       // => Start with Optional<String>
    .map(String::toUpperCase)                  // => Transform to uppercase if present
    .filter(s -> s.length() > 3)               // => Keep only if length > 3
    .orElse("default");                        // => Provide default if empty
                                               // => 4 lines (vs 12), no nested ifs
                                               // => Functional pipeline: declarative

// When NOT to use Optional (anti-patterns)
class BadPractice {                            // => Class demonstrating misuse patterns
    // DON'T use Optional for fields
    // private Optional<String> name;          // BAD: serialization issues, memory overhead
                                               // => Fields should be nullable or @NonNull
                                               // => Optional adds object wrapper overhead
                                               // => Breaks serialization frameworks

    // DON'T use Optional for method parameters
    // public void setName(Optional<String> name) {}  // BAD: forces callers to wrap
                                               // => Callers must create Optional: setName(Optional.of(value))
                                               // => Use nullable parameter or overloading instead
                                               // => Optional parameters add complexity

    // DON'T call get() without checking
    // String value = optional.get();          // BAD: may throw NoSuchElementException
                                               // => get() unsafe: throws if empty
                                               // => Use orElse/orElseGet/orElseThrow instead
}                                              // => Optional designed for return types only

// DO use Optional for return types (best practice)
class GoodPractice {                           // => Class demonstrating correct usage
    private String name;                       // => Field is nullable (not Optional)

    public Optional<String> getName() {        // => Return type Optional<String> (documents possibility of absence)
        return Optional.ofNullable(name);      // => GOOD: wraps nullable field at boundary
    }                                          // => ofNullable() safely handles null name
                                               // => Caller knows to handle absence

    public void processName(String name) {     // => GOOD: plain parameter (nullable)
                                               // => Caller passes null or value directly
        this.name = name;                      // => Store nullable value
    }                                          // => No Optional wrapper needed
}                                              // => Use Optional only for return types

// Stream of Optionals (Java 9+) - filtering empty values
List<Optional<String>> list = List.of(         // => List of Optional<String> (mixed present/empty)
    Optional.of("A"),                          // => First element: present
    Optional.empty(),                          // => Second element: empty
    Optional.of("B"),                          // => Third element: present
    Optional.empty(),                          // => Fourth element: empty
    Optional.of("C")                           // => Fifth element: present
);                                             // => Immutable list (Java 9+ List.of())

List<String> values = list.stream()            // => Create stream of Optional<String>
    .flatMap(Optional::stream)                 // => Flatten: Optional.stream() returns Stream of 0 or 1 element
                                               // => Present Optional -> Stream with 1 element
                                               // => Empty Optional -> empty Stream
                                               // => flatMap flattens all streams into one
    .collect(Collectors.toList());             // => ["A", "B", "C"] (empty Optionals filtered out)
                                               // => Result: List<String> with only present values
                                               // => Alternative to manual filtering with isPresent()
```

**Key Takeaway**: `Optional<T>` explicitly handles potential absence. Create with `of()`, `ofNullable()`, `empty()`. Check with `isPresent()`, `isEmpty()`. Extract with `get()` (after checking), `orElse()`, `orElseGet()`, `orElseThrow()`. Transform with `map()`, `flatMap()`, `filter()`. Use for return types, NOT fields or parameters. Chaining avoids nested null checks.

**Why It Matters**: Optional eliminates NullPointerException by forcing explicit absence handling. It provides a clear API (map, flatMap, orElse, orElseThrow) for null-safe transformations. Optional.ofNullable safely wraps possibly-null values from legacy APIs. However, Optional has overhead—use for method return types, not fields or parameters. Chaining Optional methods enables building null-safe pipelines without nested null checks. Optional improves code clarity by making absence explicit in type signatures, documenting intent, and preventing null-related production crashes that cost millions in losses annually.

---

### Example 24: Method References and Functional Composition

Method references provide cleaner syntax for lambdas that just call methods. Functional composition builds complex operations from simple functions.

**Code**:

```java
import java.util.*;
import java.util.function.*;

// Four types of method references

// 1. Static method reference: ClassName::staticMethod
Function<String, Integer> parseInt = Integer::parseInt;
int num = parseInt.apply("123"); // => 123

// Equivalent lambda
Function<String, Integer> parseIntLambda = s -> Integer.parseInt(s);

// 2. Instance method of specific object: object::instanceMethod
String prefix = "Hello, ";
Function<String, String> greeter = prefix::concat;
String greeting = greeter.apply("World"); // => "Hello, World"

// Equivalent lambda
Function<String, String> greeterLambda = s -> prefix.concat(s);

// 3. Instance method of arbitrary object: ClassName::instanceMethod
Function<String, Integer> length = String::length;
int len = length.apply("Java"); // => 4

// Equivalent lambda
Function<String, Integer> lengthLambda = s -> s.length();

List<String> words = Arrays.asList("apple", "banana", "cherry");
words.sort(String::compareToIgnoreCase); // => [apple, banana, cherry]

// 4. Constructor reference: ClassName::new
Supplier<List<String>> listMaker = ArrayList::new;
List<String> list = listMaker.get(); // => new ArrayList<>()

Function<Integer, int[]> arrayMaker = int[]::new;
int[] array = arrayMaker.apply(10); // => new int[10]

// Constructor with parameters
BiFunction<String, Integer, Person> personMaker = Person::new;
Person person = personMaker.apply("Alice", 30); // => new Person("Alice", 30)

// Function composition with andThen()
Function<Integer, Integer> multiplyBy2 = n -> n * 2;
Function<Integer, Integer> add3 = n -> n + 3;

Function<Integer, Integer> combined = multiplyBy2.andThen(add3);
int result1 = combined.apply(5); // => (5 * 2) + 3 = 13

// Function composition with compose()
Function<Integer, Integer> composed = add3.compose(multiplyBy2);
int result2 = composed.apply(5); // => (5 * 2) + 3 = 13 (same as andThen)

// Difference: order of execution
// f.andThen(g) = g(f(x))
// f.compose(g) = f(g(x))

Function<Integer, Integer> subtract1 = n -> n - 1;
Function<Integer, Integer> square = n -> n * n;

Function<Integer, Integer> squareThenSubtract = square.andThen(subtract1);
System.out.println(squareThenSubtract.apply(5)); // => 25 - 1 = 24

Function<Integer, Integer> subtractThenSquare = square.compose(subtract1);
System.out.println(subtractThenSquare.apply(5)); // => (5 - 1)² = 16

// Predicate composition
Predicate<Integer> greaterThan5 = n -> n > 5;
Predicate<Integer> even = n -> n % 2 == 0;

// and() - both predicates must be true
Predicate<Integer> evenAndGreaterThan5 = even.and(greaterThan5);
System.out.println(evenAndGreaterThan5.test(6)); // => true
System.out.println(evenAndGreaterThan5.test(4)); // => false

// or() - at least one predicate must be true
Predicate<Integer> evenOrGreaterThan5 = even.or(greaterThan5);
System.out.println(evenOrGreaterThan5.test(4)); // => true (even)
System.out.println(evenOrGreaterThan5.test(7)); // => true (greater than 5)
System.out.println(evenOrGreaterThan5.test(3)); // => false (neither)

// negate() - reverses predicate
Predicate<Integer> odd = even.negate();
System.out.println(odd.test(3)); // => true
System.out.println(odd.test(4)); // => false

// Consumer composition with andThen()
Consumer<String> print = s -> System.out.println(s);
Consumer<String> log = s -> System.err.println("Log: " + s);

Consumer<String> printAndLog = print.andThen(log);
printAndLog.accept("Message");
// => prints "Message" to stdout
// => prints "Log: Message" to stderr

// Practical example: data processing pipeline
List<String> names = Arrays.asList("alice", "BOB", "Charlie");

Function<String, String> trim = String::trim;
Function<String, String> lowercase = String::toLowerCase;
Function<String, String> capitalize = s ->
    s.substring(0, 1).toUpperCase() + s.substring(1);

Function<String, String> normalize = trim
    .andThen(lowercase)
    .andThen(capitalize);

List<String> normalized = names.stream()
    .map(normalize)
    .collect(Collectors.toList()); // => ["Alice", "Bob", "Charlie"]

// BiFunction doesn't have andThen/compose, but can be adapted
BiFunction<Integer, Integer, Integer> add = (a, b) -> a + b;
Function<Integer, Integer> times2 = n -> n * 2;

// Chain BiFunction result through Function
BiFunction<Integer, Integer, Integer> addThenTimes2 =
    (a, b) -> times2.apply(add.apply(a, b));

int result3 = addThenTimes2.apply(3, 4); // => (3 + 4) * 2 = 14
```

**Key Takeaway**: Method references (`::`) simplify lambdas: static (`ClassName::method`), instance (`object::method`), arbitrary (`ClassName::method`), constructor (`ClassName::new`). Function composition: `andThen()` executes f then g, `compose()` executes g then f. Predicate composition: `and()`, `or()`, `negate()`. Consumer composition: `andThen()`. Build complex operations by chaining simple functions.

**Why It Matters**: Method references provide syntactic sugar when lambdas just delegate to existing methods. They improve readability (String::toUpperCase vs s -> s.toUpperCase()) and integrate seamlessly with Streams and functional interfaces. Understanding the four types (static, instance, constructor, arbitrary object) enables appropriate usage. Method references shine in simple delegation but can obscure type information—balance conciseness with clarity. They're pervasive in modern Java codebases (Collections processing, Stream operations, event handlers), so recognizing and using them effectively is essential for idiomatic Java code.

---

## Group 4: I/O and File Handling

### Example 25: File I/O with NIO.2

NIO.2 (`java.nio.file` package) provides modern, efficient file operations. `Path` represents file system paths. `Files` offers rich static methods. `try-with-resources` ensures proper resource management.

**Code**:

```java
import java.nio.file.*;
import java.io.IOException;
import java.util.List;
import java.util.stream.Stream;

// Path interface - represents file system path
Path path = Paths.get("example.txt"); // => relative path
Path absolute = Paths.get("/Users/name/example.txt"); // => absolute path
Path multi = Paths.get("folder", "subfolder", "file.txt"); // => folder/subfolder/file.txt

// Reading files (Java 11+)
try {
    // readString() - reads entire file as String
    String content = Files.readString(path); // => file content as String
    System.out.println(content);
} catch (IOException e) {
    System.out.println("Error reading file: " + e.getMessage());
}

// readAllLines() - reads file as List<String>
try {
    List<String> lines = Files.readAllLines(path); // => each line as list element
    for (String line : lines) {
        System.out.println(line);
    }
} catch (IOException e) {
    System.out.println("Error: " + e.getMessage());
}

// Writing files
try {
    // writeString() - writes String to file (Java 11+)
    Files.writeString(path, "Hello, World!"); // => overwrites file

    // write() - writes bytes
    List<String> content = List.of("Line 1", "Line 2", "Line 3");
    Files.write(path, content); // => writes list as lines

    // Append to file
    Files.writeString(path, "\nAppended line", StandardOpenOption.APPEND);
} catch (IOException e) {
    System.out.println("Error writing: " + e.getMessage());
}

// File checks
boolean exists = Files.exists(path); // => true if file exists
boolean notExists = Files.notExists(path); // => true if doesn't exist
boolean isDirectory = Files.isDirectory(path); // => true if directory
boolean isRegularFile = Files.isRegularFile(path); // => true if regular file
boolean readable = Files.isReadable(path); // => true if readable
boolean writable = Files.isWritable(path); // => true if writable

// File properties
try {
    long size = Files.size(path); // => file size in bytes
    FileTime modifiedTime = Files.getLastModifiedTime(path); // => last modified time
} catch (IOException e) {
    System.out.println("Error: " + e.getMessage());
}

// Directory traversal with walk()
try (Stream<Path> paths = Files.walk(Paths.get("."))) {
    paths.filter(Files::isRegularFile)
         .filter(p -> p.toString().endsWith(".txt"))
         .forEach(System.out::println); // => prints all .txt files
} catch (IOException e) {
    System.out.println("Error walking: " + e.getMessage());
}

// File operations
try {
    // copy() - copies file
    Path source = Paths.get("source.txt");
    Path dest = Paths.get("dest.txt");
    Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING);

    // move() - moves or renames file
    Path newPath = Paths.get("renamed.txt");
    Files.move(dest, newPath, StandardCopyOption.REPLACE_EXISTING);

    // delete() - deletes file (throws exception if doesn't exist)
    Files.delete(newPath);

    // deleteIfExists() - deletes if exists (no exception)
    boolean deleted = Files.deleteIfExists(path); // => true if deleted

    // Creating directories
    Path dir = Paths.get("newFolder");
    Files.createDirectory(dir); // => creates single directory

    Path nested = Paths.get("parent/child/grandchild");
    Files.createDirectories(nested); // => creates all parent directories
} catch (IOException e) {
    System.out.println("Error: " + e.getMessage());
}

// try-with-resources for AutoCloseable
try (BufferedReader reader = Files.newBufferedReader(path)) {
    String line;
    while ((line = reader.readLine()) != null) {
        System.out.println(line);
    }
} catch (IOException e) {
    System.out.println("Error: " + e.getMessage());
} // reader auto-closed even if exception thrown

// Streaming lines (lazy loading for large files)
try (Stream<String> lines = Files.lines(path)) {
    lines.filter(line -> line.contains("keyword"))
         .forEach(System.out::println);
} catch (IOException e) {
    System.out.println("Error: " + e.getMessage());
}
```

**Key Takeaway**: NIO.2 provides modern file I/O. `Path` represents paths; `Paths.get()` creates them. `Files` offers static methods: `readString()`, `readAllLines()`, `writeString()`, `write()` for content. `exists()`, `isDirectory()`, `size()` for properties. `copy()`, `move()`, `delete()` for operations. `walk()` traverses directories. `try-with-resources` auto-closes resources.

**Why It Matters**: NIO.2 (java.nio.file) provides modern file I/O APIs replacing legacy java.io.File with cleaner behavior and better error handling. Files class offers convenient methods for reading/writing entire files, walking directory trees, and watching for changes. Path objects handle filesystem paths correctly across platforms (Windows vs Unix). Understanding buffered vs direct I/O impacts performance—buffering reduces system calls. File operations integrated with Streams enable processing large files line-by-line without loading entire contents into memory, essential for logs and datasets exceeding available RAM.

---

(Continue with remaining examples 26-35 to reach target length...)

### Example 26: Streams for I/O

Byte streams (`InputStream`/`OutputStream`) handle binary data. Character streams (`Reader`/`Writer`) handle text. Buffering improves performance. Bridge classes convert between byte and character streams.

**Code**:

```java
import java.io.*;

// Byte streams - InputStream/OutputStream
try (FileInputStream fis = new FileInputStream("data.bin");
     FileOutputStream fos = new FileOutputStream("output.bin")) {

    int byteRead;
    while ((byteRead = fis.read()) != -1) { // Read one byte at a time
        fos.write(byteRead); // Write byte
    }
} catch (IOException e) {
    e.printStackTrace();
}

// Character streams - Reader/Writer
try (FileReader reader = new FileReader("input.txt");
     FileWriter writer = new FileWriter("output.txt")) {

    int charRead;
    while ((charRead = reader.read()) != -1) {
        writer.write(charRead);
    }
} catch (IOException e) {
    e.printStackTrace();
}

// Buffered streams - improve performance
try (BufferedReader br = new BufferedReader(new FileReader("large.txt"));
     BufferedWriter bw = new BufferedWriter(new FileWriter("copy.txt"))) {

    String line;
    while ((line = br.readLine()) != null) { // Read line by line
        bw.write(line);
        bw.newLine(); // Platform-independent line separator
    }
} catch (IOException e) {
    e.printStackTrace();
}

// Bridge classes - InputStreamReader, OutputStreamWriter
try (InputStream is = new FileInputStream("data.txt");
     InputStreamReader isr = new InputStreamReader(is, "UTF-8");
     BufferedReader br = new BufferedReader(isr)) {

    String line = br.readLine(); // Convert bytes to characters
} catch (IOException e) {
    e.printStackTrace();
}
```

**Key Takeaway**: Byte streams (`InputStream`/`OutputStream`) for binary data. Character streams (`Reader`/`Writer`) for text. `BufferedReader`/`BufferedWriter` improve performance. `InputStreamReader`/`OutputStreamWriter` bridge byte and character streams. Always use `try-with-resources` for automatic closure.

**Why It Matters**: Lambda expressions enable functional programming patterns, reducing boilerplate in event handlers, callbacks, and collection processing. They power modern Java APIs like Streams, CompletableFuture, and reactive frameworks. Understanding lambda syntax and effectively final requirements prevents compilation errors. Method references provide concise alternatives when lambdas just delegate to methods. Functional interfaces enable strategy pattern without verbose anonymous classes. Lambdas make concurrent and asynchronous code more readable, improving maintainability in complex systems.

---

## Group 6: Concurrency Basics

### Example 32: Threads and Runnable

Threads enable concurrency. `Runnable` separates task from thread. `start()` creates new thread; `run()` executes in current thread. `join()` waits for completion.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    New["NEW<br/>Thread created"] --> Runnable["RUNNABLE<br/>start() called"]
    Runnable --> Running["RUNNING<br/>Executing"]
    Running --> Waiting["WAITING<br/>wait()/join()"]
    Running --> Blocked["BLOCKED<br/>Waiting for lock"]
    Waiting --> Runnable
    Blocked --> Runnable
    Running --> Terminated["TERMINATED<br/>Completed"]

    style New fill:#0173B2,color:#fff
    style Runnable fill:#DE8F05,color:#fff
    style Running fill:#029E73,color:#fff
    style Waiting fill:#CC78BC,color:#fff
    style Terminated fill:#CA9161,color:#fff
```

**Code**:

```java
// Runnable interface - task to execute
Runnable task = new Runnable() {
    @Override
    public void run() {
        System.out.println("Task running in: " + Thread.currentThread().getName());
    }
};

// Creating and starting thread
Thread thread = new Thread(task);
thread.start(); // Creates new thread and calls run()
// thread.run(); // WRONG: executes in current thread, doesn't create new thread

// Lambda version (Runnable is functional interface)
Thread lambdaThread = new Thread(() -> {
    System.out.println("Lambda task in: " + Thread.currentThread().getName());
});
lambdaThread.start();

// Thread.sleep() - pause execution
try {
    Thread.sleep(1000); // Sleep for 1000ms (1 second)
} catch (InterruptedException e) {
    e.printStackTrace();
}

// Thread.currentThread() - get current thread
Thread current = Thread.currentThread();
System.out.println("Name: " + current.getName());
System.out.println("ID: " + current.getId());

// join() - wait for thread to complete
Thread worker = new Thread(() -> {
    for (int i = 0; i < 5; i++) {
        System.out.println("Working: " + i);
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {}
    }
});

worker.start();
try {
    worker.join(); // Wait for worker to finish
    System.out.println("Worker completed");
} catch (InterruptedException e) {}

// Daemon threads - background threads that don't prevent JVM shutdown
Thread daemon = new Thread(() -> {
    while (true) {
        System.out.println("Daemon running");
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {}
    }
});
daemon.setDaemon(true); // Must be set before start()
daemon.start();

// Thread states (NEW, RUNNABLE, BLOCKED, WAITING, TIMED_WAITING, TERMINATED)
Thread t = new Thread(() -> {});
System.out.println(t.getState()); // => NEW
t.start();
System.out.println(t.getState()); // => RUNNABLE or TERMINATED (depends on timing)
```

**Key Takeaway**: Threads enable concurrency. `Runnable` defines tasks. `start()` creates new thread; `run()` executes in current thread. `Thread.sleep()` pauses execution. `join()` waits for completion. Daemon threads run in background. Thread states: NEW, RUNNABLE, BLOCKED, WAITING, TERMINATED.

**Why It Matters**: Parallel streams exploit multi-core CPUs, scaling throughput for CPU-bound operations on large datasets. They automatically partition data, process chunks concurrently, and merge results—no manual thread management. However, parallel streams have overhead—only beneficial for substantial workloads (thousands of elements, heavy computation). Stateful operations (sorted, distinct) and shared mutable state cause correctness issues. Understanding when parallelization helps (CPU-bound) vs hurts (I/O-bound, small datasets) prevents performance regressions. Measure before parallelizing—premature parallelization causes bugs.

---

## Group 5: Date/Time and Testing

### Example 27: Serialization and JSON

Serialization converts objects to byte streams for persistence. `Serializable` marker interface enables object serialization. `transient` keyword excludes fields. JSON is preferred for human-readable, language-agnostic data.

**Code**:

```java
import java.io.*;

// Serializable marker interface
class Person implements Serializable {
    private static final long serialVersionUID = 1L; // Version control

    private String name;
    private int age;
    private transient String password; // Excluded from serialization

    public Person(String name, int age, String password) {
        this.name = name;
        this.age = age;
        this.password = password;
    }
}

// Serialization - object to bytes
try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream("person.ser"))) {
    Person person = new Person("Alice", 30, "secret");
    oos.writeObject(person); // Serialize object
} catch (IOException e) {
    e.printStackTrace();
}

// Deserialization - bytes to object
try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream("person.ser"))) {
    Person person = (Person) ois.readObject(); // Deserialize
    System.out.println(person.name); // => "Alice"
    System.out.println(person.password); // => null (transient field)
} catch (IOException | ClassNotFoundException e) {
    e.printStackTrace();
}
```

**Key Takeaway**: `Serializable` enables object persistence. `serialVersionUID` maintains version compatibility. `transient` excludes sensitive fields. JSON (via Gson/Jackson) is preferred for human-readable interchange.

**Why It Matters**: Streams provide declarative data processing, separating what (filter, map, reduce) from how (parallelization, optimization). They eliminate boilerplate loops, reducing bugs from manual iteration logic. Lazy evaluation optimizes performance—intermediate operations don't execute until terminal operation triggers. Understanding stream lifecycle (create, intermediate, terminal) prevents accidental reuse. Parallel streams leverage multi-core CPUs transparently, scaling throughput. Streams integrate with Optional, enabling null-safe pipelines. Stream-based code is more concise, readable, and testable than imperative loops.

---

### Example 28: Modern Date and Time API

`java.time` (Java 8+) provides immutable, thread-safe date/time classes. `LocalDate`/`LocalTime`/`LocalDateTime` for human time. `Instant` for machine time. `ZonedDateTime` for time zones.

**Code**:

```java
import java.time.*;
import java.time.format.DateTimeFormatter;

// LocalDate - date without time
LocalDate today = LocalDate.now(); // => 2025-12-23
LocalDate birthday = LocalDate.of(1990, Month.JANUARY, 15); // => 1990-01-15

// LocalTime - time without date
LocalTime now = LocalTime.now(); // => 14:30:45.123
LocalTime noon = LocalTime.of(12, 0); // => 12:00

// LocalDateTime - date and time
LocalDateTime dateTime = LocalDateTime.now(); // => 2025-12-23T14:30:45
LocalDateTime specific = LocalDateTime.of(2025, 12, 25, 10, 30); // => 2025-12-25T10:30

// Instant - timestamp (machine time)
Instant instant = Instant.now(); // => 2025-12-23T07:30:45Z (UTC)

// ZonedDateTime - time with timezone
ZonedDateTime zoned = ZonedDateTime.now(ZoneId.of("America/New_York"));

// Period - duration in days/months/years
Period period = Period.between(birthday, today); // => 35 years
int years = period.getYears(); // => 35

// Duration - duration in hours/minutes/seconds
Duration duration = Duration.between(noon, now); // => PT2H30M45S
long seconds = duration.getSeconds(); // => 9045

// DateTimeFormatter - parsing and formatting
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
String formatted = dateTime.format(formatter); // => "2025-12-23 14:30"
LocalDateTime parsed = LocalDateTime.parse("2025-12-25 10:30", formatter);

// Comparing dates
boolean before = birthday.isBefore(today); // => true
boolean after = birthday.isAfter(today); // => false
```

**Key Takeaway**: `java.time` provides immutable, thread-safe classes. `LocalDate`/`LocalTime`/`LocalDateTime` for human-readable time. `Instant` for machine timestamps. `Period` for date-based duration. `Duration` for time-based duration. `DateTimeFormatter` for formatting.

**Why It Matters**: Optional eliminates NullPointerException—the billion-dollar mistake. It forces explicit handling of absence, preventing silent null propagation that causes cryptic errors later. Optional.ofNullable safely wraps possibly-null values from legacy APIs. Chaining (map, flatMap) enables null-safe transformations without nested null checks. However, Optional has overhead—don't use for fields or method parameters (use null checks). It's designed for method return types signaling possible absence. Proper Optional usage improves code clarity and prevents null-related production crashes.

---

### Example 29: Regular Expressions

`Pattern` and `Matcher` enable powerful text matching. `String` methods provide convenience for simple cases. Groups extract matched substrings.

**Code**:

```java
import java.util.regex.*;

// Pattern.compile() and Matcher
Pattern pattern = Pattern.compile("\\d+"); // Regex: one or more digits
Matcher matcher = pattern.matcher("Order 123 costs $45");

// find() - find next match
while (matcher.find()) {
    System.out.println(matcher.group()); // => "123", "45"
}

// matches() - entire string must match
boolean fullMatch = Pattern.matches("\\d{3}", "123"); // => true (exactly 3 digits)
boolean noMatch = Pattern.matches("\\d{3}", "12"); // => false

// String convenience methods
String text = "Contact: alice@example.com";
boolean hasEmail = text.matches(".*@.*\\..*"); // => true
String[] parts = text.split(": "); // => ["Contact", "alice@example.com"]
String replaced = text.replaceAll("@", " AT "); // => "Contact: alice AT example.com"

// Groups - capturing parts of matches
Pattern emailPattern = Pattern.compile("(\\w+)@(\\w+\\.\\w+)");
Matcher emailMatcher = emailPattern.matcher("alice@example.com");

if (emailMatcher.find()) {
    String full = emailMatcher.group(0); // => "alice@example.com" (entire match)
    String username = emailMatcher.group(1); // => "alice" (first group)
    String domain = emailMatcher.group(2); // => "example.com" (second group)
}

// Pattern flags
Pattern caseInsensitive = Pattern.compile("hello", Pattern.CASE_INSENSITIVE);
Matcher m = caseInsensitive.matcher("HELLO World");
System.out.println(m.find()); // => true
```

**Key Takeaway**: `Pattern` and `Matcher` enable regex matching. `find()` finds next match. `matches()` checks entire string. `group()` extracts matched text. String methods (`matches()`, `split()`, `replaceAll()`) provide convenience for simple patterns.

**Why It Matters**: Method references provide syntactic sugar when lambdas just delegate to methods, improving readability. They integrate seamlessly with Stream operations (map, filter, forEach). Understanding the four types (static, instance, constructor, arbitrary object) enables appropriate usage. Method references reduce verbosity but can obscure type information—balance conciseness with clarity. They're pervasive in modern Java codebases, so recognizing them is essential. However, complex lambdas should remain explicit for clarity. Method references shine in simple delegation scenarios.

---

### Example 30: JUnit Testing Basics

JUnit enables automated unit testing. `@Test` marks test methods. Assertions verify expected behavior. Lifecycle annotations organize setup/teardown.

**Code**:

```java
import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.*;

class Calculator {
    public int add(int a, int b) {
        return a + b;
    }

    public int divide(int a, int b) {
        if (b == 0) throw new IllegalArgumentException("Division by zero");
        return a / b;
    }
}

class CalculatorTest {
    private Calculator calc;

    @BeforeEach // Runs before each test method
    void setUp() {
        calc = new Calculator();
    }

    @Test // Marks a test method
    void testAdd() {
        int result = calc.add(2, 3);
        assertEquals(5, result); // Assert expected == actual
    }

    @Test
    void testDivide() {
        int result = calc.divide(10, 2);
        assertEquals(5, result);
    }

    @Test
    void testDivideByZero() {
        // assertThrows - verify exception thrown
        assertThrows(IllegalArgumentException.class, () -> {
            calc.divide(10, 0);
        });
    }

    @Test
    void testAssertions() {
        assertTrue(5 > 3); // Assert boolean condition
        assertFalse(5 < 3);
        assertNotNull(calc); // Assert not null
        assertNull(null); // Assert null
    }

    @AfterEach // Runs after each test method
    void tearDown() {
        calc = null;
    }

    @BeforeAll // Runs once before all tests (static)
    static void setUpClass() {
        System.out.println("Starting tests");
    }

    @AfterAll // Runs once after all tests (static)
    static void tearDownClass() {
        System.out.println("Tests complete");
    }
}
```

**Key Takeaway**: `@Test` marks test methods. Assertions (`assertEquals`, `assertTrue`, `assertNotNull`, `assertThrows`) verify behavior. `@BeforeEach`/`@AfterEach` for per-test setup. `@BeforeAll`/`@AfterAll` for class-level setup. Test isolation ensures reliability.

**Why It Matters**: Functional interfaces enable treating behavior as data—passing functions as arguments, returning functions. They power callback patterns, event handling, and strategy pattern implementations. SAM (Single Abstract Method) interfaces from pre-Java 8 code automatically work with lambdas. Understanding common functional interfaces (Predicate, Function, Consumer) enables effective library usage. Custom functional interfaces should use @FunctionalInterface annotation for compile-time validation. Functional programming patterns reduce coupling and improve testability through dependency injection of behavior.

---

### Example 31: Mockito for Testing

Mockito simulates dependencies for isolated testing. `@Mock` creates mock objects. `when().thenReturn()` stubs behavior. `verify()` checks interactions.

**Code**:

```java
import org.mockito.*;
import static org.mockito.Mockito.*;

interface UserRepository {
    User findById(Long id);
    void save(User user);
}

class UserService {
    private UserRepository repository;

    public UserService(UserRepository repository) {
        this.repository = repository;
    }

    public User getUser(Long id) {
        return repository.findById(id);
    }
}

class UserServiceTest {
    @Mock
    private UserRepository mockRepository;

    @InjectMocks
    private UserService service;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testGetUser() {
        // Arrange: stub mock behavior
        User expected = new User(1L, "Alice");
        when(mockRepository.findById(1L)).thenReturn(expected);

        // Act: call method under test
        User actual = service.getUser(1L);

        // Assert: verify result
        assertEquals(expected, actual);

        // Verify: check interaction
        verify(mockRepository).findById(1L); // Verify method was called
    }
}
```

**Key Takeaway**: Mockito creates mock dependencies. `@Mock` creates mocks. `@InjectMocks` injects mocks into tested class. `when().thenReturn()` stubs method behavior. `verify()` checks method was called with expected arguments.

**Why It Matters**: Collectors transform Streams into concrete collections, aggregations, or custom data structures. They enable concise data processing—grouping, partitioning, aggregating—without manual loops. Downstream collectors compose operations (group then count, group then average). Understanding Collectors.toMap handle duplicates prevents runtime exceptions. Custom collectors enable domain-specific aggregations. Collectors are the bridge between declarative Stream processing and concrete results, making complex data transformations simple and readable.

---

### Example 33: Thread-Safe Session Management

Production applications need thread-safe session management where multiple threads concurrently create, access, and clean up user sessions. `ConcurrentHashMap` with atomic operations enables lock-free, high-performance session handling.

**Code**:

```java
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.concurrent.*;

// Session data model
class Session {
    private final String userId;
    private final Instant createdAt;
    private Instant expiresAt;
    private final ConcurrentHashMap<String, Object> attributes;

    public Session(String userId, int expiryMinutes) {
        this.userId = userId;
        this.createdAt = Instant.now();
        this.expiresAt = Instant.now().plus(expiryMinutes, ChronoUnit.MINUTES);
        this.attributes = new ConcurrentHashMap<>();
    }

    public boolean isExpired() {
        return Instant.now().isAfter(expiresAt); // => true if expired
    }

    public void renewExpiry(int minutes) {
        this.expiresAt = Instant.now().plus(minutes, ChronoUnit.MINUTES);
    }

    public void setAttribute(String key, Object value) {
        attributes.put(key, value); // Thread-safe put
    }

    public Object getAttribute(String key) {
        return attributes.get(key);
    }

    public String getUserId() { return userId; }
    public Instant getExpiresAt() { return expiresAt; }
}

// Thread-safe session manager using ConcurrentHashMap
class UserSessionManager {
    private final ConcurrentHashMap<String, Session> sessions;
    private final int defaultExpiryMinutes;

    public UserSessionManager(int expiryMinutes) {
        this.sessions = new ConcurrentHashMap<>();
        this.defaultExpiryMinutes = expiryMinutes;
    }

    // Atomic get-or-create pattern using computeIfAbsent
    public Session getOrCreateSession(String userId) {
        return sessions.computeIfAbsent(userId, id -> {
            Session session = new Session(id, defaultExpiryMinutes);
            System.out.println("Created new session for: " + id);
            return session; // => Atomically creates if absent
        });
    }

    // Get existing session (no creation)
    public Session getSession(String userId) {
        Session session = sessions.get(userId);
        if (session != null && session.isExpired()) {
            sessions.remove(userId); // Clean up expired
            return null; // => Session expired
        }
        return session;
    }

    // Renew session expiry
    public void renewSession(String userId, int minutes) {
        sessions.computeIfPresent(userId, (id, session) -> {
            session.renewExpiry(minutes);
            System.out.println("Renewed session for: " + id);
            return session; // => Returns updated session
        });
    }

    // Invalidate (logout)
    public void invalidateSession(String userId) {
        Session removed = sessions.remove(userId);
        if (removed != null) {
            System.out.println("Invalidated session for: " + userId);
        }
    }

    // Clean up all expired sessions (periodic cleanup task)
    public int cleanupExpiredSessions() {
        int cleaned = 0;
        for (var entry : sessions.entrySet()) {
            if (entry.getValue().isExpired()) {
                sessions.remove(entry.getKey()); // Thread-safe removal
                cleaned++;
            }
        }
        System.out.println("Cleaned up " + cleaned + " expired sessions");
        return cleaned; // => Number of sessions removed
    }

    // Get active session count
    public int getActiveSessionCount() {
        return sessions.size();
    }

    // Atomic merge operation - update session attributes
    public void updateSessionAttribute(String userId, String key, Object value) {
        sessions.computeIfPresent(userId, (id, session) -> {
            session.setAttribute(key, value);
            return session; // => Updated session
        });
    }
}

// Example usage with multiple concurrent threads
UserSessionManager sessionManager = new UserSessionManager(30); // 30-minute expiry

// Thread 1: User login
new Thread(() -> {
    Session session = sessionManager.getOrCreateSession("user123");
    session.setAttribute("role", "admin");
    // => Created new session for: user123
}).start();

// Thread 2: Same user (concurrent access)
new Thread(() -> {
    Session session = sessionManager.getOrCreateSession("user123");
    // => Returns existing session (no duplicate creation)
    String role = (String) session.getAttribute("role");
    System.out.println("Role: " + role); // => "admin"
}).start();

// Thread 3: Periodic cleanup task
ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);
scheduler.scheduleAtFixedRate(
    () -> sessionManager.cleanupExpiredSessions(),
    1, 5, TimeUnit.MINUTES // => Run every 5 minutes
);

// Graceful shutdown
scheduler.shutdown();

// Demonstrating atomic operations
Session s1 = sessionManager.getOrCreateSession("alice"); // => Creates new
Session s2 = sessionManager.getOrCreateSession("alice"); // => Returns same instance
System.out.println(s1 == s2); // => true (atomic operation ensures single instance)

sessionManager.invalidateSession("alice"); // => Logout
Session s3 = sessionManager.getSession("alice"); // => null (invalidated)
```

**Key Takeaway**: `ConcurrentHashMap` provides thread-safe operations without explicit locking. `computeIfAbsent()` atomically creates sessions (get-or-create pattern). `computeIfPresent()` atomically updates existing entries. This pattern scales better than `synchronized` for read-heavy workloads because `ConcurrentHashMap` uses lock striping. Use scheduled tasks for periodic cleanup of expired sessions. Production systems use this pattern for user sessions, caches, and connection pools.

**Why It Matters**: Grouping and partitioning enable data aggregation—histogram generation, pivot tables, category-based analysis. They replace verbose imperative loops with declarative operations. Downstream collectors enable multi-level aggregation (group then average, partition then count). Partitioning is optimized for two-way splits (true/false predicates). These operations are essential for reporting, analytics, and data transformation pipelines. Understanding complex collectors enables elegant solutions to otherwise tedious data processing problems.

---

### Example 34: ExecutorService and Thread Pools

`ExecutorService` manages thread pools efficiently. `submit()` returns `Future` for async results. `Callable` returns values; `Runnable` doesn't.

**Code**:

```java
import java.util.concurrent.*;

// Fixed thread pool
ExecutorService executor = Executors.newFixedThreadPool(4); // 4 threads

// Submit Runnable (no return value)
executor.submit(() -> {
    System.out.println("Task running");
});

// Submit Callable (returns value)
Future<Integer> future = executor.submit(() -> {
    Thread.sleep(1000);
    return 42;
});

try {
    Integer result = future.get(); // Blocks until result available
    System.out.println(result); // => 42
} catch (Exception e) {
    e.printStackTrace();
}

// Shutdown executor
executor.shutdown(); // No new tasks accepted
try {
    if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
        executor.shutdownNow(); // Force shutdown
    }
} catch (InterruptedException e) {
    executor.shutdownNow();
}
```

**Key Takeaway**: `ExecutorService` manages thread pools. `newFixedThreadPool()` creates fixed-size pool. `submit()` submits tasks, returns `Future`. `Callable` returns values. Always `shutdown()` and `awaitTermination()`.

**Why It Matters**: Threads enable concurrency—handling multiple requests simultaneously, performing background tasks, improving responsiveness. However, threads are expensive (1MB stack space each), and manual thread management is error-prone (deadlocks, races, resource leaks). Extending Thread is inflexible (single inheritance)—prefer Runnable/Callable. Understanding thread lifecycle (new, runnable, blocked, terminated) prevents resource leaks. Modern Java prefers higher-level abstractions (ExecutorService, CompletableFuture) over raw threads. Threads are foundational to understanding Java's concurrency model.

---

### Example 35: CompletableFuture for Async

`CompletableFuture` enables async programming without callback hell. Chaining methods compose operations. `exceptionally()` handles errors.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Supply["supplyAsync()<br/>Start async"] --> Then["thenApply()<br/>Transform"]
    Then --> Accept["thenAccept()<br/>Consume"]
    Then --> Error["exceptionally()<br/>Handle error"]

    style Supply fill:#0173B2,color:#fff
    style Then fill:#DE8F05,color:#fff
    style Accept fill:#029E73,color:#fff
    style Error fill:#CC78BC,color:#fff
```

**Code**:

```java
import java.util.concurrent.CompletableFuture;

// supplyAsync - start async computation
CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> {
    // Runs in separate thread
    return "Hello";
});

// thenApply - transform result
future.thenApply(s -> s + " World")
      .thenApply(String::toUpperCase)
      .thenAccept(s -> System.out.println(s)); // => "HELLO WORLD"

// exceptionally - handle errors
CompletableFuture<Integer> withError = CompletableFuture.supplyAsync(() -> {
    if (true) throw new RuntimeException("Error!");
    return 42;
}).exceptionally(ex -> {
    System.out.println("Caught: " + ex.getMessage());
    return 0; // Default value
});

// thenCombine - combine two futures
CompletableFuture<Integer> f1 = CompletableFuture.supplyAsync(() -> 10);
CompletableFuture<Integer> f2 = CompletableFuture.supplyAsync(() -> 20);

CompletableFuture<Integer> combined = f1.thenCombine(f2, (a, b) -> a + b);
System.out.println(combined.join()); // => 30

// allOf - wait for all futures
CompletableFuture<Void> all = CompletableFuture.allOf(f1, f2);
all.join(); // Waits for both to complete
```

**Key Takeaway**: `CompletableFuture` enables async programming. `supplyAsync()` starts async computation. `thenApply()` transforms, `thenAccept()` consumes. `exceptionally()` handles errors. `thenCombine()` combines futures. `allOf()`/`anyOf()` wait for multiple futures.

**Why It Matters**: ExecutorService abstracts thread management, providing thread pools that reuse threads, reducing overhead. Fixed thread pools limit concurrency preventing resource exhaustion. Cached thread pools scale dynamically for I/O-bound tasks. Understanding executor shutdown (shutdown vs shutdownNow) prevents zombie threads. Executors enable decoupling task submission from execution policy—change from single-threaded to multi-threaded without modifying task code. Proper thread pool sizing (CPU-bound: cores, I/O-bound: cores \* blocking factor) optimizes throughput. Production systems use executors for all concurrent work.

---

## Group 6: Advanced Streams and Collectors

### Example 36: Custom Collectors

Custom collectors enable specialized aggregation logic beyond built-in collectors. Implement `Collector` interface for domain-specific operations. Use `Collector.of()` for simple cases.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Stream["Stream Elements"] --> Supplier["supplier()<br/>Create container"]
    Supplier --> Accumulator["accumulator()<br/>Add elements"]
    Accumulator --> Combiner["combiner()<br/>Merge containers"]
    Combiner --> Finisher["finisher()<br/>Transform result"]
    Finisher --> Result["Final Result"]

    style Stream fill:#0173B2,color:#fff
    style Supplier fill:#DE8F05,color:#fff
    style Accumulator fill:#029E73,color:#fff
    style Combiner fill:#CC78BC,color:#fff
    style Finisher fill:#CA9161,color:#fff
    style Result fill:#0173B2,color:#fff
```

**Code**:

```java
import java.util.stream.*;
import java.util.*;
import java.util.function.*;

// Built-in collectors - common patterns
List<String> names = List.of("Alice", "Bob", "Charlie", "David"); // => 4 names

// toList - collect to ArrayList
List<String> list = names.stream()
    .filter(s -> s.length() > 3) // => Filter: Alice, Charlie, David
    .collect(Collectors.toList()); // => [Alice, Charlie, David]

// toSet - collect to HashSet (removes duplicates)
Set<Integer> lengths = names.stream()
    .map(String::length) // => [5, 3, 7, 5]
    .collect(Collectors.toSet()); // => [3, 5, 7] (unique lengths)

// toMap - collect to HashMap
Map<String, Integer> nameToLength = names.stream()
    .collect(Collectors.toMap(
        name -> name, // => Key function
        String::length // => Value function
    )); // => {Alice=5, Bob=3, Charlie=7, David=5}

// joining - concatenate strings
String joined = names.stream()
    .collect(Collectors.joining(", ")); // => "Alice, Bob, Charlie, David"

String withPrefix = names.stream()
    .collect(Collectors.joining(", ", "[", "]")); // => "[Alice, Bob, Charlie, David]"

// groupingBy - group elements by classifier
Map<Integer, List<String>> byLength = names.stream()
    .collect(Collectors.groupingBy(String::length));
// => {3=[Bob], 5=[Alice, David], 7=[Charlie]}

// partitioningBy - split into two groups (true/false)
Map<Boolean, List<String>> partition = names.stream()
    .collect(Collectors.partitioningBy(s -> s.length() > 4));
// => {false=[Bob], true=[Alice, Charlie, David]}

// counting - count elements
long count = names.stream()
    .collect(Collectors.counting()); // => 4

// summingInt - sum integer values
int totalLength = names.stream()
    .collect(Collectors.summingInt(String::length)); // => 5+3+7+5 = 20

// averagingInt - average of integer values
double avgLength = names.stream()
    .collect(Collectors.averagingInt(String::length)); // => 20/4 = 5.0

// summarizingInt - statistics (count, sum, min, max, average)
IntSummaryStatistics stats = names.stream()
    .collect(Collectors.summarizingInt(String::length));
System.out.println(stats.getCount()); // => 4
System.out.println(stats.getSum()); // => 20
System.out.println(stats.getMin()); // => 3
System.out.println(stats.getMax()); // => 7
System.out.println(stats.getAverage()); // => 5.0

// Custom collector using Collector.of()
// Example: collect to immutable list
Collector<String, List<String>, List<String>> toImmutableList = Collector.of(
    ArrayList::new, // => Supplier: create mutable container
    List::add, // => Accumulator: add element to container
    (list1, list2) -> { // => Combiner: merge two containers
        list1.addAll(list2); // => Combine lists
        return list1; // => Return merged list
    },
    Collections::unmodifiableList // => Finisher: make immutable
);

List<String> immutable = names.stream()
    .collect(toImmutableList); // => Immutable list
// immutable.add("Eve"); // => Throws UnsupportedOperationException

// Custom collector: concatenate with custom separator and brackets
Collector<String, StringBuilder, String> customJoin = Collector.of(
    StringBuilder::new, // => Supplier: create StringBuilder
    (sb, s) -> { // => Accumulator: add element
        if (sb.length() > 0) sb.append(" | "); // => Add separator if not first
        sb.append(s); // => Append element
    },
    (sb1, sb2) -> { // => Combiner: merge StringBuilders
        if (sb1.length() > 0 && sb2.length() > 0) {
            sb1.append(" | "); // => Add separator between merged parts
        }
        return sb1.append(sb2); // => Combine and return
    },
    sb -> "[" + sb.toString() + "]" // => Finisher: wrap in brackets
);

String custom = names.stream()
    .collect(customJoin); // => "[Alice | Bob | Charlie | David]"

// Downstream collectors - combine collectors
Map<Integer, Long> lengthCounts = names.stream()
    .collect(Collectors.groupingBy(
        String::length, // => Group by length
        Collectors.counting() // => Count elements in each group
    )); // => {3=1, 5=2, 7=1}

Map<Integer, String> lengthToNames = names.stream()
    .collect(Collectors.groupingBy(
        String::length, // => Group by length
        Collectors.joining(", ") // => Join names in each group
    )); // => {3=Bob, 5=Alice, David, 7=Charlie}

// collectingAndThen - transform collector result
List<String> unmodifiable = names.stream()
    .collect(Collectors.collectingAndThen(
        Collectors.toList(), // => Collect to list first
        Collections::unmodifiableList // => Then make immutable
    )); // => Immutable list

// teeing - apply two collectors and combine results (Java 12+)
record MinMax(String min, String max) {} // => Record for result

MinMax minMax = names.stream()
    .collect(Collectors.teeing(
        Collectors.minBy(Comparator.naturalOrder()), // => First collector: min
        Collectors.maxBy(Comparator.naturalOrder()), // => Second collector: max
        (min, max) -> new MinMax(min.orElse(""), max.orElse("")) // => Combine results
    )); // => MinMax[min=Alice, max=David]

// filtering - filter before collecting (Java 9+)
Map<Integer, List<String>> longNamesGrouped = names.stream()
    .collect(Collectors.groupingBy(
        String::length,
        Collectors.filtering(s -> s.length() > 4, Collectors.toList()) // => Filter in collector
    )); // => {3=[], 5=[Alice, David], 7=[Charlie]}

// mapping - map before collecting
Map<Integer, List<Integer>> lengthToChars = names.stream()
    .collect(Collectors.groupingBy(
        String::length,
        Collectors.mapping(s -> s.charAt(0) - 'A', Collectors.toList()) // => Map to first char
    )); // => {3=[1], 5=[0, 3], 7=[2]}
```

**Key Takeaway**: Built-in collectors handle common aggregations: `toList()`, `toSet()`, `toMap()`, `joining()`, `groupingBy()`, `partitioningBy()`. Collectors compose with downstream collectors. Custom collectors use `Collector.of()` with supplier, accumulator, combiner, and finisher. Use `collectingAndThen()` to transform final result. `teeing()` applies two collectors simultaneously.

**Why It Matters**: CompletableFuture enables non-blocking asynchronous programming with composable operations—chaining, combining, error handling. It prevents blocking threads while waiting for I/O (network, disk, external APIs), improving scalability. Understanding completion stages (thenApply, thenCompose, thenCombine) enables building async pipelines. Exception handling (exceptionally, handle) prevents silent failures. CompletableFuture powers modern reactive frameworks (Spring WebFlux) and async HTTP clients. It's essential for building responsive, scalable applications that efficiently utilize system resources.

---

### Example 37: Method References Deep Dive

Method references provide concise syntax for lambda expressions that call existing methods. Four types: static, instance, constructor, and arbitrary object. Understand when to use each type.

**Code**:

```java
import java.util.*;
import java.util.function.*;

// 1. Static method reference - ClassName::staticMethod
List<String> numbers = List.of("1", "2", "3", "4"); // => String numbers

// Lambda version
List<Integer> parsed1 = numbers.stream()
    .map(s -> Integer.parseInt(s)) // => Lambda calling static method
    .toList(); // => [1, 2, 3, 4]

// Method reference version
List<Integer> parsed2 = numbers.stream()
    .map(Integer::parseInt) // => Static method reference (cleaner)
    .toList(); // => [1, 2, 3, 4]

// 2. Instance method reference on specific object - object::instanceMethod
String prefix = "Number: "; // => Specific String object

// Lambda version
List<String> prefixed1 = numbers.stream()
    .map(s -> prefix.concat(s)) // => Lambda calling instance method on prefix
    .toList(); // => ["Number: 1", "Number: 2", "Number: 3", "Number: 4"]

// Method reference version
List<String> prefixed2 = numbers.stream()
    .map(prefix::concat) // => Instance method reference on prefix object
    .toList(); // => ["Number: 1", "Number: 2", "Number: 3", "Number: 4"]

// 3. Instance method reference on arbitrary object - ClassName::instanceMethod
List<String> names = List.of("alice", "BOB", "Charlie"); // => Mixed case names

// Lambda version
List<String> upper1 = names.stream()
    .map(s -> s.toUpperCase()) // => Lambda calling instance method on each element
    .toList(); // => ["ALICE", "BOB", "CHARLIE"]

// Method reference version
List<String> upper2 = names.stream()
    .map(String::toUpperCase) // => Instance method reference (each element calls toUpperCase)
    .toList(); // => ["ALICE", "BOB", "CHARLIE"]

// Sorting with instance method reference
List<String> sorted = names.stream()
    .sorted(String::compareToIgnoreCase) // => Instance method reference for comparator
    .toList(); // => ["alice", "BOB", "Charlie"] (case-insensitive sort)

// 4. Constructor reference - ClassName::new
List<Integer> lengths = List.of(5, 3, 7, 4); // => Integer list

// Lambda version
List<String> strings1 = lengths.stream()
    .map(len -> new String(new char[len]).replace('\0', 'x')) // => Lambda creating objects
    .toList(); // => ["xxxxx", "xxx", "xxxxxxx", "xxxx"]

// Constructor reference for simple cases
class Person {
    private String name; // => Person field

    public Person(String name) {
        this.name = name; // => Constructor initializes name
    }

    public String getName() { return name; } // => Getter
}

// Constructor reference
List<Person> people = names.stream()
    .map(Person::new) // => Constructor reference (equivalent to s -> new Person(s))
    .toList(); // => List of Person objects

// Array constructor reference
String[] array = names.stream()
    .toArray(String[]::new); // => Array constructor reference
// => Creates String[] of appropriate size: ["alice", "BOB", "Charlie"]

// Method reference with multiple parameters
BiFunction<String, String, String> concat1 = (a, b) -> a.concat(b); // => Lambda
BiFunction<String, String, String> concat2 = String::concat; // => Method reference

String result = concat2.apply("Hello", " World"); // => "Hello World"

// Method reference in forEach
names.stream()
    .forEach(System.out::println); // => Instance method reference on System.out
// => Prints: alice, BOB, Charlie

// Method reference with Comparator
List<String> byLength = names.stream()
    .sorted(Comparator.comparing(String::length)) // => Method reference in comparing()
    .toList(); // => ["BOB", "alice", "Charlie"] (sorted by length)

List<String> byLengthReversed = names.stream()
    .sorted(Comparator.comparing(String::length).reversed()) // => Reversed comparator
    .toList(); // => ["Charlie", "alice", "BOB"]

// Chained comparators with method references
record Employee(String name, int age, double salary) {} // => Employee record

List<Employee> employees = List.of(
    new Employee("Alice", 30, 50000), // => Employee 1
    new Employee("Bob", 25, 45000), // => Employee 2
    new Employee("Charlie", 30, 55000) // => Employee 3
);

List<Employee> sorted2 = employees.stream()
    .sorted(Comparator
        .comparing(Employee::age) // => Sort by age first
        .thenComparing(Employee::salary) // => Then by salary
        .reversed()) // => Reverse entire order
    .toList(); // => [Charlie(30, 55000), Alice(30, 50000), Bob(25, 45000)]

// Method reference with Optional
Optional<String> opt = Optional.of("hello"); // => Optional containing "hello"

// Lambda version
opt.map(s -> s.toUpperCase()).ifPresent(s -> System.out.println(s)); // => "HELLO"

// Method reference version
opt.map(String::toUpperCase) // => Method reference for map
   .ifPresent(System.out::println); // => Method reference for ifPresent: "HELLO"

// Method reference limitations
// Cannot use when:
// 1. Lambda has additional logic beyond method call
List<String> withLogic = names.stream()
    .map(s -> {
        System.out.println("Processing: " + s); // => Extra logic
        return s.toUpperCase(); // => Cannot use method reference here
    })
    .toList();

// 2. Lambda modifies parameters before passing
List<String> modified = names.stream()
    .map(s -> Integer.parseInt(s.trim())) // => Can't use Integer::parseInt (needs trim first)
    .toList();

// Practical patterns with method references
class StringUtils {
    public static String reverse(String s) {
        return new StringBuilder(s).reverse().toString(); // => Reverse string
    }
}

List<String> reversed = names.stream()
    .map(StringUtils::reverse) // => Static method reference to custom utility
    .toList(); // => ["ecila", "BOB", "eilrahC"]

// Supplier with constructor reference
Supplier<List<String>> listSupplier = ArrayList::new; // => Constructor reference
List<String> newList = listSupplier.get(); // => Creates new ArrayList
newList.add("item"); // => Add to new list

// Consumer with method reference
Consumer<String> printer = System.out::println; // => Method reference to println
names.forEach(printer); // => Prints each name
```

**Key Takeaway**: Four method reference types: static (`ClassName::staticMethod`), instance on object (`object::method`), instance on arbitrary (`ClassName::method`), constructor (`ClassName::new`). Method references are shorthand for lambdas that only call one method. Use when lambda body is a single method call with matching parameters. Cannot use when lambda has additional logic or parameter transformation. Array constructors use `Type[]::new`.

**Why It Matters**: Concurrent collections provide thread-safe operations without manual synchronization, preventing races and deadlocks. ConcurrentHashMap enables lock-free reads and fine-grained locking for writes, outperforming synchronized maps. CopyOnWriteArrayList optimizes for read-heavy workloads (cache, event listeners). Understanding when to use which collection (ConcurrentHashMap vs Collections.synchronizedMap) prevents performance bottlenecks. BlockingQueue enables producer-consumer patterns with built-in coordination. These collections are foundational to concurrent systems—caches, work queues, shared state.

---

### Example 38: Date and Time API (java.time)

Modern Date-Time API (Java 8+) provides immutable, thread-safe date/time handling. Replaces legacy `Date` and `Calendar`. Understand `LocalDate`, `LocalTime`, `LocalDateTime`, `ZonedDateTime`, and `Duration`/`Period`.

**Code**:

```java
import java.time.*;
import java.time.format.*;
import java.time.temporal.*;

// LocalDate - date without time (year-month-day)
LocalDate today = LocalDate.now(); // => Current date: 2025-12-25
LocalDate specificDate = LocalDate.of(2024, 1, 15); // => 2024-01-15
LocalDate parsed = LocalDate.parse("2024-03-20"); // => 2024-03-20

System.out.println(today.getYear()); // => 2025
System.out.println(today.getMonth()); // => DECEMBER (enum)
System.out.println(today.getMonthValue()); // => 12
System.out.println(today.getDayOfMonth()); // => 25
System.out.println(today.getDayOfWeek()); // => WEDNESDAY (enum)
System.out.println(today.getDayOfYear()); // => 359

// LocalTime - time without date (hour:minute:second.nanosecond)
LocalTime now = LocalTime.now(); // => Current time: 14:30:45.123456789
LocalTime specificTime = LocalTime.of(10, 30); // => 10:30:00
LocalTime withSeconds = LocalTime.of(10, 30, 45); // => 10:30:45
LocalTime withNanos = LocalTime.of(10, 30, 45, 123456789); // => 10:30:45.123456789

System.out.println(now.getHour()); // => 14
System.out.println(now.getMinute()); // => 30
System.out.println(now.getSecond()); // => 45
System.out.println(now.getNano()); // => 123456789

// LocalDateTime - date and time without timezone
LocalDateTime datetime = LocalDateTime.now(); // => Current date-time: 2025-12-25T14:30:45.123
LocalDateTime specific = LocalDateTime.of(2024, 1, 15, 10, 30); // => 2024-01-15T10:30
LocalDateTime combined = LocalDateTime.of(today, now); // => Combine date and time

// ZonedDateTime - date-time with timezone
ZonedDateTime zoned = ZonedDateTime.now(); // => Current with default timezone
ZonedDateTime newYork = ZonedDateTime.now(ZoneId.of("America/New_York"));
// => Current time in New York: 2025-12-25T09:30:45.123-05:00[America/New_York]

ZonedDateTime tokyo = zoned.withZoneSameInstant(ZoneId.of("Asia/Tokyo"));
// => Same instant converted to Tokyo timezone

// Instant - timestamp (seconds since Unix epoch: 1970-01-01T00:00:00Z)
Instant timestamp = Instant.now(); // => Current timestamp: 2025-12-25T14:30:45.123Z
long epochSeconds = timestamp.getEpochSecond(); // => Seconds since epoch: 1735139445
long epochMillis = timestamp.toEpochMilli(); // => Milliseconds since epoch

// Arithmetic operations - immutable (returns new instances)
LocalDate tomorrow = today.plusDays(1); // => 2025-12-26 (today unchanged)
LocalDate nextWeek = today.plusWeeks(1); // => 2026-01-01
LocalDate nextMonth = today.plusMonths(1); // => 2026-01-25
LocalDate nextYear = today.plusYears(1); // => 2026-12-25

LocalDate yesterday = today.minusDays(1); // => 2025-12-24
LocalDate lastMonth = today.minusMonths(1); // => 2025-11-25

LocalTime later = now.plusHours(2).plusMinutes(30); // => 17:00:45.123 (2.5 hours later)
LocalTime earlier = now.minusHours(1); // => 13:30:45.123 (1 hour earlier)

// with methods - modify specific fields
LocalDate modified = today
    .withYear(2030) // => Change year to 2030
    .withMonth(6) // => Change month to June
    .withDayOfMonth(15); // => Change day to 15th: 2030-06-15

LocalTime modifiedTime = now
    .withHour(9) // => Change hour to 9
    .withMinute(0) // => Change minute to 0
    .withSecond(0) // => Change second to 0: 09:00:00

// TemporalAdjusters - complex date adjustments
LocalDate firstDayOfMonth = today.with(TemporalAdjusters.firstDayOfMonth());
// => 2025-12-01

LocalDate lastDayOfMonth = today.with(TemporalAdjusters.lastDayOfMonth());
// => 2025-12-31

LocalDate nextMonday = today.with(TemporalAdjusters.next(DayOfWeek.MONDAY));
// => Next Monday after today

LocalDate firstMonday = today.with(TemporalAdjusters.firstInMonth(DayOfWeek.MONDAY));
// => First Monday of current month

// Period - date-based duration (years, months, days)
Period period = Period.between(specificDate, today); // => Period from 2024-01-15 to today
System.out.println(period.getYears()); // => 1 (year difference)
System.out.println(period.getMonths()); // => 11 (month difference)
System.out.println(period.getDays()); // => 10 (day difference)

Period twoWeeks = Period.ofWeeks(2); // => 14 days
Period threeMonths = Period.ofMonths(3); // => 3 months
LocalDate future = today.plus(threeMonths); // => 2026-03-25

// Duration - time-based duration (hours, minutes, seconds, nanos)
Duration duration = Duration.between(specificTime, now); // => Duration from 10:30 to now
System.out.println(duration.toHours()); // => Hours difference: 4
System.out.println(duration.toMinutes()); // => Minutes difference: 240
System.out.println(duration.getSeconds()); // => Total seconds

Duration oneHour = Duration.ofHours(1); // => 1 hour
Duration thirtyMinutes = Duration.ofMinutes(30); // => 30 minutes
LocalTime laterTime = now.plus(oneHour); // => 15:30:45.123

// Formatting and parsing
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
String formatted = datetime.format(formatter); // => "2025-12-25 14:30:45"

LocalDateTime parsedDateTime = LocalDateTime.parse("2024-03-20 10:30:00", formatter);
// => 2024-03-20T10:30:00

// Predefined formatters
String iso = today.format(DateTimeFormatter.ISO_DATE); // => "2025-12-25"
String custom = today.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")); // => "25/12/2025"

// Comparison
boolean isBefore = specificDate.isBefore(today); // => true (2024-01-15 before today)
boolean isAfter = specificDate.isAfter(today); // => false
boolean isEqual = today.isEqual(today); // => true

// ChronoUnit - calculate time units between dates
long daysBetween = ChronoUnit.DAYS.between(specificDate, today); // => Days: 709
long monthsBetween = ChronoUnit.MONTHS.between(specificDate, today); // => Months: 23
long hoursBetween = ChronoUnit.HOURS.between(specificTime, now); // => Hours: 4

// Leap year check
boolean isLeapYear = today.isLeapYear(); // => false (2025 is not leap year)
boolean was2024Leap = specificDate.isLeapYear(); // => true (2024 is leap year)

// Length of month
int daysInMonth = today.lengthOfMonth(); // => 31 (December has 31 days)
int daysInYear = today.lengthOfYear(); // => 365 (2025 is not leap year)

// Practical example: calculate age
LocalDate birthDate = LocalDate.of(1990, 5, 15); // => Birth: 1990-05-15
Period age = Period.between(birthDate, today); // => Age period
System.out.println("Age: " + age.getYears() + " years, " +
                   age.getMonths() + " months, " +
                   age.getDays() + " days");
// => "Age: 35 years, 7 months, 10 days"

// Working with different timezones
ZoneId londonZone = ZoneId.of("Europe/London"); // => London timezone
ZoneId sydneyZone = ZoneId.of("Australia/Sydney"); // => Sydney timezone

ZonedDateTime londonTime = ZonedDateTime.now(londonZone);
ZonedDateTime sydneyTime = londonTime.withZoneSameInstant(sydneyZone);
// => Same instant, different timezone display

// Time until specific event
LocalDateTime event = LocalDateTime.of(2026, 1, 1, 0, 0); // => New Year 2026
Duration timeUntil = Duration.between(LocalDateTime.now(), event);
System.out.println("Days until event: " + timeUntil.toDays()); // => Days remaining
```

**Key Takeaway**: Modern Date-Time API is immutable and thread-safe. `LocalDate` for dates, `LocalTime` for times, `LocalDateTime` for both (no timezone). `ZonedDateTime` includes timezone. `Instant` for Unix timestamps. `Period` for date-based durations (years/months/days), `Duration` for time-based (hours/minutes/seconds). All operations return new instances. Use `DateTimeFormatter` for formatting/parsing. `TemporalAdjusters` for complex date math. `ChronoUnit` for calculating differences.

**Why It Matters**: Synchronization prevents race conditions—ensuring only one thread modifies shared state at a time. However, locks cause contention reducing throughput, and incorrect locking causes deadlocks. Understanding intrinsic locks (synchronized) vs explicit locks (ReentrantLock) enables appropriate locking strategies. Minimize critical sections to reduce contention. Prefer concurrent collections and atomic variables over manual synchronization—they're less error-prone. Synchronization is essential but tricky—modern Java provides higher-level constructs reducing the need for manual locking.

---

### Example 39: Regular Expressions

Regular expressions (regex) enable pattern matching in strings. Java's `Pattern` and `Matcher` provide powerful text processing. Understand basic patterns, groups, and replacements for validation and extraction.

**Code**:

```java
import java.util.regex.*;
import java.util.*;

// Basic pattern matching
String text = "Hello World 123"; // => Input text

// matches() - entire string must match pattern
boolean matches1 = text.matches("Hello World \\d+"); // => true (\\d+ = one or more digits)
boolean matches2 = text.matches("Hello"); // => false (doesn't match entire string)

// Pattern compilation - reuse patterns efficiently
Pattern pattern = Pattern.compile("\\d+"); // => Compile pattern once
Matcher matcher = pattern.matcher(text); // => Create matcher for text

// find() - find substring matching pattern
if (matcher.find()) {
    System.out.println("Found: " + matcher.group()); // => "123" (matched digits)
    System.out.println("Start: " + matcher.start()); // => 12 (start index)
    System.out.println("End: " + matcher.end()); // => 15 (end index)
}

// Find all matches
String multipleNumbers = "Order 123 has 45 items and costs 67 dollars"; // => Multiple numbers
Matcher numberMatcher = Pattern.compile("\\d+").matcher(multipleNumbers);

while (numberMatcher.find()) {
    System.out.println("Found number: " + numberMatcher.group());
}
// => Prints: "123", "45", "67"

// Capturing groups - extract parts of match
String email = "user@example.com"; // => Email address
Pattern emailPattern = Pattern.compile("([a-zA-Z0-9]+)@([a-zA-Z0-9.]+)");
// => Group 1: username, Group 2: domain

Matcher emailMatcher = emailPattern.matcher(email);
if (emailMatcher.matches()) {
    System.out.println("Full match: " + emailMatcher.group(0)); // => "user@example.com"
    System.out.println("Username: " + emailMatcher.group(1)); // => "user"
    System.out.println("Domain: " + emailMatcher.group(2)); // => "example.com"
}

// Named groups (Java 7+)
Pattern namedPattern = Pattern.compile("(?<user>[a-zA-Z0-9]+)@(?<domain>[a-zA-Z0-9.]+)");
Matcher namedMatcher = namedPattern.matcher(email);

if (namedMatcher.matches()) {
    System.out.println("User: " + namedMatcher.group("user")); // => "user"
    System.out.println("Domain: " + namedMatcher.group("domain")); // => "example.com"
}

// Common patterns
String phonePattern = "\\d{3}-\\d{3}-\\d{4}"; // => 123-456-7890 format
boolean validPhone = "555-123-4567".matches(phonePattern); // => true

String zipPattern = "\\d{5}(-\\d{4})?"; // => 12345 or 12345-6789 format
boolean validZip1 = "12345".matches(zipPattern); // => true
boolean validZip2 = "12345-6789".matches(zipPattern); // => true
boolean validZip3 = "123".matches(zipPattern); // => false

// URL pattern (simplified)
String urlPattern = "https?://[a-zA-Z0-9.-]+\\.[a-z]{2,}(/.*)?";
boolean validUrl = "https://example.com/path".matches(urlPattern); // => true

// replaceAll() - replace all matches
String withSpaces = "hello   world  test"; // => Multiple spaces
String normalized = withSpaces.replaceAll("\\s+", " "); // => "hello world test" (single spaces)

String text2 = "Error: code 123, Error: code 456"; // => Multiple errors
String replaced = text2.replaceAll("Error: code (\\d+)", "Bug #$1");
// => "Bug #123, Bug #456" ($1 references group 1)

// replaceFirst() - replace only first match
String onlyFirst = text2.replaceFirst("Error", "Warning");
// => "Warning: code 123, Error: code 456"

// split() - split by pattern
String csv = "apple,banana,cherry,date"; // => CSV data
String[] fruits = csv.split(","); // => ["apple", "banana", "cherry", "date"]

String irregular = "one  two   three    four"; // => Irregular spacing
String[] words = irregular.split("\\s+"); // => ["one", "two", "three", "four"]

// Pattern flags
Pattern caseInsensitive = Pattern.compile("hello", Pattern.CASE_INSENSITIVE);
boolean matches3 = caseInsensitive.matcher("HELLO").matches(); // => true

Pattern multiline = Pattern.compile("^Hello", Pattern.MULTILINE);
// => ^ matches start of each line, not just start of string

Pattern dotall = Pattern.compile(".*", Pattern.DOTALL);
// => . matches newlines too

// Greedy vs reluctant quantifiers
String html = "<div>Content</div><span>More</span>"; // => HTML tags

// Greedy (default) - matches as much as possible
Matcher greedy = Pattern.compile("<.*>").matcher(html);
if (greedy.find()) {
    System.out.println(greedy.group()); // => "<div>Content</div><span>More</span>"
    // => Matches from first < to last >
}

// Reluctant (lazy) - matches as little as possible
Matcher reluctant = Pattern.compile("<.*?>").matcher(html);
while (reluctant.find()) {
    System.out.println(reluctant.group());
}
// => Prints: "<div>", "</div>", "<span>", "</span>"
// => Each match is minimal

// Lookahead and lookbehind
String prices = "Price: $100, $200, $300"; // => Prices with $ prefix

// Positive lookahead - match followed by pattern
Matcher lookahead = Pattern.compile("\\d+(?= dollars)").matcher("10 dollars");
// => Matches "10" only if followed by " dollars"

// Positive lookbehind - match preceded by pattern
Matcher lookbehind = Pattern.compile("(?<=\\$)\\d+").matcher(prices);
while (lookbehind.find()) {
    System.out.println(lookbehind.group());
}
// => Prints: "100", "200", "300" (numbers after $)

// Validation patterns
class Validators {
    public static boolean isEmail(String email) {
        String pattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$";
        return email.matches(pattern); // => Validates email format
    }

    public static boolean isStrongPassword(String password) {
        // At least 8 chars, one uppercase, one lowercase, one digit, one special char
        String pattern = "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[@$!%*?&])[A-Za-z\\d@$!%*?&]{8,}$";
        return password.matches(pattern); // => Validates password strength
    }

    public static boolean isIPv4(String ip) {
        String pattern = "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$";
        return ip.matches(pattern); // => Validates IPv4 address
    }
}

boolean validEmail = Validators.isEmail("user@example.com"); // => true
boolean invalidEmail = Validators.isEmail("invalid@"); // => false

boolean strongPass = Validators.isStrongPassword("Pass123!"); // => true
boolean weakPass = Validators.isStrongPassword("password"); // => false

boolean validIP = Validators.isIPv4("192.168.1.1"); // => true
boolean invalidIP = Validators.isIPv4("999.999.999.999"); // => false

// Extract all matches into list
String sentence = "The price is $100 and the tax is $15"; // => Sentence with prices
Pattern pricePattern = Pattern.compile("\\$\\d+"); // => $ followed by digits
Matcher priceMatcher = pricePattern.matcher(sentence);

List<String> allPrices = new ArrayList<>();
while (priceMatcher.find()) {
    allPrices.add(priceMatcher.group()); // => Add each match
}
// => allPrices: ["$100", "$15"]

// Or using results() stream (Java 9+)
List<String> pricesStream = pricePattern.matcher(sentence)
    .results() // => Stream of MatchResult
    .map(MatchResult::group) // => Extract matched text
    .toList(); // => ["$100", "$15"]

// Quote literal strings for regex
String literal = "Price: $100 (special)"; // => String with special regex chars
String escaped = Pattern.quote(literal); // => Escapes all special chars
Pattern literalPattern = Pattern.compile(escaped); // => Matches literal string exactly
```

**Key Takeaway**: `Pattern.compile()` compiles regex patterns. `Matcher` finds matches with `find()`, `matches()`, `replaceAll()`. Capturing groups `()` extract substrings (access with `group(n)` or named `group("name")`). Common quantifiers: `*` (0+), `+` (1+), `?` (0-1), `{n,m}` (n to m). Character classes: `\\d` (digit), `\\w` (word), `\\s` (space). Use `?` for reluctant matching. Lookahead `(?=)` and lookbehind `(?<=)` match positions. `Pattern.quote()` escapes special characters for literal matching.

**Why It Matters**: Deadlocks occur when threads circularly wait for locks held by each other, halting progress. They're insidious—causing hangs in production without crashes. Prevention strategies (lock ordering, timeouts) are essential for concurrent systems. Understanding deadlock conditions (mutual exclusion, hold and wait, no preemption, circular wait) enables designing deadlock-free systems. Breaking any condition prevents deadlocks. Liveness issues (starvation, livelock) also cause subtle failures. Concurrent programming requires understanding these failure modes to build reliable systems.

---

### Example 40: NIO.2 File Operations

NIO.2 (New I/O, Java 7+) provides modern file operations with `Path`, `Files`, and `FileSystem`. Replaces legacy `File` class with better error handling and more features. Essential for production file handling.

**Code**:

```java
import java.nio.file.*;
import java.nio.file.attribute.*;
import java.io.*;
import java.util.*;
import java.util.stream.*;

// Path creation - modern alternative to File
Path path1 = Paths.get("/home/user/file.txt"); // => Absolute path
Path path2 = Paths.get("relative", "path", "file.txt"); // => relative/path/file.txt
Path path3 = Path.of("/tmp", "data.txt"); // => Java 11+ factory method

// Current working directory
Path currentDir = Paths.get("").toAbsolutePath(); // => Current directory
Path userHome = Paths.get(System.getProperty("user.home")); // => User home directory

// Path operations
Path absolute = path2.toAbsolutePath(); // => Convert to absolute path
Path normalized = Paths.get("/home/user/../data/./file.txt").normalize();
// => /home/data/file.txt (removes . and ..)

Path parent = path1.getParent(); // => /home/user
Path fileName = path1.getFileName(); // => file.txt
Path root = path1.getRoot(); // => / (Unix) or C:\ (Windows)

// Join paths
Path base = Paths.get("/home/user"); // => Base directory
Path joined = base.resolve("documents/file.txt"); // => /home/user/documents/file.txt
Path relative = base.relativize(joined); // => documents/file.txt

// File existence and type checks
Path testFile = Paths.get("test.txt"); // => Test file path

boolean exists = Files.exists(testFile); // => true/false
boolean notExists = Files.notExists(testFile); // => true/false
boolean isRegularFile = Files.isRegularFile(testFile); // => true if regular file
boolean isDirectory = Files.isDirectory(testFile); // => true if directory
boolean isSymbolicLink = Files.isSymbolicLink(testFile); // => true if symlink
boolean isReadable = Files.isReadable(testFile); // => true if readable
boolean isWritable = Files.isWritable(testFile); // => true if writable
boolean isExecutable = Files.isExecutable(testFile); // => true if executable

// Reading files
// 1. Read all bytes
byte[] bytes = Files.readAllBytes(testFile); // => All bytes as array
String content1 = new String(bytes); // => Convert to string

// 2. Read all lines
List<String> lines = Files.readAllLines(testFile); // => All lines as List<String>

// 3. Stream lines (memory efficient for large files)
try (Stream<String> lineStream = Files.lines(testFile)) {
    lineStream.filter(line -> line.contains("important")) // => Filter lines
              .forEach(System.out::println); // => Print matching lines
}

// 4. Read as String (Java 11+)
String content2 = Files.readString(testFile); // => Entire file as String

// Writing files
// 1. Write bytes
byte[] data = "Hello World".getBytes(); // => Convert string to bytes
Files.write(testFile, data); // => Write bytes (creates or overwrites)

// 2. Write lines
List<String> linesToWrite = List.of("Line 1", "Line 2", "Line 3");
Files.write(testFile, linesToWrite); // => Write lines (overwrites)

// Append to file
Files.write(testFile, "New line\n".getBytes(), StandardOpenOption.APPEND);
// => Append bytes to existing file

// 3. Write string (Java 11+)
Files.writeString(testFile, "Content"); // => Write string (overwrites)

// Creating directories
Path newDir = Paths.get("parent/child/grandchild"); // => Directory path

Files.createDirectory(Paths.get("parent")); // => Create single directory (parent must exist)
Files.createDirectories(newDir); // => Create all parent directories if needed

// Copying files
Path source = Paths.get("source.txt"); // => Source file
Path destination = Paths.get("destination.txt"); // => Destination file

Files.copy(source, destination); // => Copy (fails if destination exists)
Files.copy(source, destination, StandardCopyOption.REPLACE_EXISTING);
// => Copy and replace if exists

// Copy directory (shallow - doesn't copy contents)
Files.copy(Paths.get("sourceDir"), Paths.get("destDir"));

// Moving/renaming files
Path oldName = Paths.get("old.txt"); // => Old path
Path newName = Paths.get("new.txt"); // => New path

Files.move(oldName, newName); // => Move/rename (fails if destination exists)
Files.move(oldName, newName, StandardCopyOption.REPLACE_EXISTING);
// => Move and replace if exists

// Deleting files
Files.delete(testFile); // => Delete file (throws exception if not exists)
Files.deleteIfExists(testFile); // => Delete if exists (returns true if deleted)

// File attributes
BasicFileAttributes attrs = Files.readAttributes(testFile, BasicFileAttributes.class);

long size = attrs.size(); // => File size in bytes
FileTime created = attrs.creationTime(); // => Creation time
FileTime modified = attrs.lastModifiedTime(); // => Last modified time
FileTime accessed = attrs.lastAccessTime(); // => Last access time
boolean isDir = attrs.isDirectory(); // => true if directory
boolean isFile = attrs.isRegularFile(); // => true if regular file

// Or get individual attributes
long size2 = Files.size(testFile); // => File size
FileTime modified2 = Files.getLastModifiedTime(testFile); // => Last modified

// Set last modified time
FileTime newTime = FileTime.fromMillis(System.currentTimeMillis());
Files.setLastModifiedTime(testFile, newTime); // => Update modified time

// List directory contents
Path directory = Paths.get("."); // => Current directory

// 1. List all entries (not recursive)
try (DirectoryStream<Path> stream = Files.newDirectoryStream(directory)) {
    for (Path entry : stream) {
        System.out.println(entry); // => Print each entry
    }
}

// 2. List with glob pattern filter
try (DirectoryStream<Path> stream = Files.newDirectoryStream(directory, "*.txt")) {
    for (Path entry : stream) {
        System.out.println(entry); // => Print only .txt files
    }
}

// 3. Stream directory entries (Java 8+)
try (Stream<Path> paths = Files.list(directory)) {
    paths.filter(Files::isRegularFile) // => Only regular files
         .forEach(System.out::println); // => Print each file
}

// 4. Walk directory tree (recursive)
try (Stream<Path> paths = Files.walk(directory)) {
    paths.filter(p -> p.toString().endsWith(".java")) // => Find all .java files
         .forEach(System.out::println); // => Print each Java file
}

// Walk with max depth
try (Stream<Path> paths = Files.walk(directory, 2)) { // => Max 2 levels deep
    paths.forEach(System.out::println);
}

// Find files matching predicate
try (Stream<Path> paths = Files.find(directory, 3,
        (path, attrs) -> attrs.isRegularFile() && attrs.size() > 1024)) {
    paths.forEach(System.out::println); // => Files > 1KB, max 3 levels deep
}

// Temporary files and directories
Path tempFile = Files.createTempFile("prefix-", "-suffix.txt");
// => Creates temp file: prefix-1234567890-suffix.txt in temp directory

Path tempDir = Files.createTempDirectory("myapp-");
// => Creates temp directory: myapp-1234567890 in temp directory

// Delete temporary files on exit
tempFile.toFile().deleteOnExit(); // => Delete when JVM exits

// File watching (monitor file system changes)
WatchService watcher = FileSystems.getDefault().newWatchService();
Path dirToWatch = Paths.get("."); // => Directory to watch

dirToWatch.register(watcher,
    StandardWatchEventKinds.ENTRY_CREATE, // => Watch for new files
    StandardWatchEventKinds.ENTRY_DELETE, // => Watch for deletions
    StandardWatchEventKinds.ENTRY_MODIFY); // => Watch for modifications

// Watch loop (runs until interrupted)
// WatchKey key;
// while ((key = watcher.take()) != null) {
//     for (WatchEvent<?> event : key.pollEvents()) {
//         System.out.println("Event: " + event.kind() + " - " + event.context());
//     }
//     key.reset();
// }

// Symbolic links
Path link = Paths.get("link.txt"); // => Link path
Path target = Paths.get("target.txt"); // => Target path

// Files.createSymbolicLink(link, target); // => Create symbolic link (Unix/Linux)

// Read link target
if (Files.isSymbolicLink(link)) {
    Path linkTarget = Files.readSymbolicLink(link); // => Get target path
    System.out.println("Link points to: " + linkTarget);
}

// POSIX file permissions (Unix/Linux)
// Set<PosixFilePermission> perms = PosixFilePermissions.fromString("rw-r--r--");
// Files.setPosixFilePermissions(testFile, perms);

// Practical: copy directory recursively
void copyDirectory(Path source, Path target) throws IOException {
    Files.walk(source)
         .forEach(sourcePath -> {
             try {
                 Path targetPath = target.resolve(source.relativize(sourcePath));
                 if (Files.isDirectory(sourcePath)) {
                     Files.createDirectories(targetPath); // => Create directories
                 } else {
                     Files.copy(sourcePath, targetPath, StandardCopyOption.REPLACE_EXISTING);
                     // => Copy files
                 }
             } catch (IOException e) {
                 e.printStackTrace();
             }
         });
}

// Practical: delete directory recursively
void deleteDirectory(Path directory) throws IOException {
    Files.walk(directory)
         .sorted(Comparator.reverseOrder()) // => Delete children before parents
         .forEach(path -> {
             try {
                 Files.delete(path); // => Delete each path
             } catch (IOException e) {
                 e.printStackTrace();
             }
         });
}
```

**Key Takeaway**: NIO.2 uses `Path` (not `File`) for paths. `Files` class provides static methods for all operations. `Files.exists()`, `isDirectory()`, `isRegularFile()` check status. Reading: `readAllBytes()`, `readAllLines()`, `lines()` (stream), `readString()`. Writing: `write()`, `writeString()`. `copy()`, `move()`, `delete()` with options. `createDirectories()` creates all parents. `Files.walk()` traverses recursively. `Files.list()` lists directory. Attributes via `readAttributes()`. `WatchService` monitors changes. Always use try-with-resources for streams.

**Why It Matters**: Files is the modern API for file operations, replacing legacy File class with cleaner, more correct behavior. It handles edge cases (symbolic links, permissions) correctly. Integration with Streams enables processing large files efficiently without loading entire contents into memory—essential for logs, datasets. Understanding IOException handling prevents resource leaks. Atomic file operations (move, delete) prevent partial writes during failures. Modern file handling is critical for reliable data processing in production systems.

---

### Example 41: Annotations in Practice

Annotations add metadata to code for frameworks, tools, and runtime processing. Built-in annotations (`@Override`, `@Deprecated`, `@SuppressWarnings`) ensure code quality. Custom annotations enable framework magic.

**Code**:

```java
import java.lang.annotation.*;
import java.lang.reflect.*;

// Built-in annotations - code quality and compiler checks
class Parent {
    public void process() {
        System.out.println("Parent process"); // => Parent method
    }

    @Deprecated(since = "2.0", forRemoval = true) // => Mark for removal
    public void oldMethod() {
        System.out.println("Old method"); // => Deprecated method
    }
}

class Child extends Parent {
    @Override // => Compiler verifies this overrides parent method
    public void process() {
        System.out.println("Child process"); // => Child override
    }

    // @Override
    // public void typoMethod() {} // => ERROR: no such method in parent

    @SuppressWarnings("deprecation") // => Suppress deprecation warning
    public void useOldMethod() {
        oldMethod(); // => No warning despite calling deprecated method
    }

    @SuppressWarnings({"unchecked", "rawtypes"}) // => Multiple warnings
    public void rawTypes() {
        List list = new ArrayList(); // => Raw type (normally warns)
        list.add("item"); // => Unchecked operation (normally warns)
    }
}

// @FunctionalInterface - marks interface as functional (single abstract method)
@FunctionalInterface
interface Calculator {
    int calculate(int a, int b); // => Single abstract method

    // int anotherMethod(int x); // => ERROR: multiple abstract methods not allowed

    default void log() { // => Default methods allowed
        System.out.println("Calculating");
    }

    static void helper() { // => Static methods allowed
        System.out.println("Helper");
    }
}

// Lambda works because of single abstract method
Calculator adder = (a, b) -> a + b; // => Lambda implementation
int sum = adder.calculate(5, 3); // => 8

// Custom annotation definition
@Retention(RetentionPolicy.RUNTIME) // => Available at runtime (for reflection)
@Target(ElementType.METHOD) // => Can be applied to methods only
@interface Test {
    String description() default ""; // => Annotation parameter with default
    int timeout() default 0; // => Timeout in milliseconds
    String[] tags() default {}; // => Array parameter
}

// Multiple target types
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD}) // => Apply to classes or methods
@interface Audited {
    String value(); // => Required parameter (no default)
}

// Annotation with no parameters (marker annotation)
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
@interface Benchmark {}

// Using custom annotations
@Audited("UserService") // => Class-level annotation
class UserService {
    @Test(description = "Test user creation", timeout = 5000, tags = {"integration", "database"})
    public void testCreateUser() {
        System.out.println("Testing user creation"); // => Test method
    }

    @Test(description = "Test user deletion", tags = {"integration"})
    public void testDeleteUser() {
        System.out.println("Testing user deletion"); // => Test method
    }

    @Benchmark // => Marker annotation (no parameters)
    public void performanceTest() {
        System.out.println("Performance test"); // => Benchmark method
    }

    public void regularMethod() {
        // No annotations
    }
}

// Processing annotations with reflection
void runTests(Class<?> testClass) throws Exception {
    Object instance = testClass.getDeclaredConstructor().newInstance(); // => Create instance

    for (Method method : testClass.getDeclaredMethods()) {
        if (method.isAnnotationPresent(Test.class)) { // => Check if @Test present
            Test test = method.getAnnotation(Test.class); // => Get annotation

            System.out.println("Running test: " + method.getName());
            System.out.println("  Description: " + test.description());
            System.out.println("  Timeout: " + test.timeout() + "ms");
            System.out.println("  Tags: " + Arrays.toString(test.tags()));

            try {
                method.invoke(instance); // => Invoke test method
                System.out.println("  Result: PASSED");
            } catch (Exception e) {
                System.out.println("  Result: FAILED - " + e.getCause());
            }
        }
    }
}

// Run tests
runTests(UserService.class);
// => Outputs:
// Running test: testCreateUser
//   Description: Test user creation
//   Timeout: 5000ms
//   Tags: [integration, database]
//   Testing user creation
//   Result: PASSED
// Running test: testDeleteUser
//   Description: Test user deletion
//   Timeout: 0ms
//   Tags: [integration]
//   Testing user deletion
//   Result: PASSED

// Retention policies
@Retention(RetentionPolicy.SOURCE) // => Discarded by compiler (e.g., @Override)
@interface SourceOnly {}

@Retention(RetentionPolicy.CLASS) // => Stored in class file, not available at runtime (default)
@interface ClassOnly {}

@Retention(RetentionPolicy.RUNTIME) // => Available at runtime via reflection
@interface RuntimeAccessible {}

// Target types (where annotations can be applied)
@Target(ElementType.TYPE) // => Classes, interfaces, enums
@interface TypeAnnotation {}

@Target(ElementType.FIELD) // => Fields
@interface FieldAnnotation {}

@Target(ElementType.METHOD) // => Methods
@interface MethodAnnotation {}

@Target(ElementType.PARAMETER) // => Method parameters
@interface ParameterAnnotation {}

@Target(ElementType.CONSTRUCTOR) // => Constructors
@interface ConstructorAnnotation {}

@Target(ElementType.LOCAL_VARIABLE) // => Local variables
@interface LocalVarAnnotation {}

@Target(ElementType.ANNOTATION_TYPE) // => Other annotations (meta-annotation)
@interface MetaAnnotation {}

@Target(ElementType.PACKAGE) // => Packages (in package-info.java)
@interface PackageAnnotation {}

// Java 8+ type annotations
@Target(ElementType.TYPE_USE) // => Any type use (variables, casts, generics, etc.)
@interface NonNull {}

@Target(ElementType.TYPE_PARAMETER) // => Type parameters in generics
@interface TypeParam {}

// Type annotation examples
class TypeAnnotations {
    @NonNull String name; // => Field with type annotation

    public @NonNull String getName() { // => Return type annotation
        return name;
    }

    public void process(@NonNull String input) { // => Parameter type annotation
        @NonNull String local = input; // => Local variable type annotation
    }

    List<@NonNull String> names; // => Generic type argument annotation

    public <@TypeParam T> void generic(T value) { // => Type parameter annotation
    }
}

// Repeatable annotations (Java 8+)
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
@Repeatable(Schedules.class) // => Container annotation
@interface Schedule {
    String day();
    int hour();
}

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
@interface Schedules {
    Schedule[] value(); // => Array of repeatable annotation
}

// Using repeatable annotations
class ScheduledTasks {
    @Schedule(day = "Monday", hour = 9)
    @Schedule(day = "Wednesday", hour = 14)
    @Schedule(day = "Friday", hour = 11)
    public void backupData() {
        System.out.println("Backing up data"); // => Scheduled task
    }
}

// Accessing repeatable annotations
Method backupMethod = ScheduledTasks.class.getMethod("backupData");
Schedule[] schedules = backupMethod.getAnnotationsByType(Schedule.class);
// => Returns all @Schedule annotations

for (Schedule schedule : schedules) {
    System.out.println("Scheduled: " + schedule.day() + " at " + schedule.hour() + ":00");
}
// => Prints:
// Scheduled: Monday at 9:00
// Scheduled: Wednesday at 14:00
// Scheduled: Friday at 11:00

// Practical annotation: dependency injection
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
@interface Inject {}

class SimpleInjector {
    public static void inject(Object instance) throws Exception {
        Class<?> clazz = instance.getClass();

        for (Field field : clazz.getDeclaredFields()) {
            if (field.isAnnotationPresent(Inject.class)) {
                field.setAccessible(true); // => Allow access to private field

                // Simple DI: create instance of field type
                Object dependency = field.getType().getDeclaredConstructor().newInstance();
                field.set(instance, dependency); // => Inject dependency

                System.out.println("Injected: " + field.getName());
            }
        }
    }
}

class Database {
    public void connect() {
        System.out.println("Database connected"); // => Database method
    }
}

class Service {
    @Inject
    private Database database; // => Field to be injected

    public void useDatabase() {
        database.connect(); // => Use injected dependency
    }
}

// Use simple DI
Service service = new Service();
SimpleInjector.inject(service); // => Injects Database instance
service.useDatabase(); // => "Database connected"
```

**Key Takeaway**: Built-in annotations: `@Override` (verify override), `@Deprecated` (mark obsolete), `@SuppressWarnings` (suppress warnings), `@FunctionalInterface` (enforce single abstract method). Custom annotations use `@interface`. `@Retention` controls lifecycle (SOURCE/CLASS/RUNTIME). `@Target` specifies where annotation applies. Access via reflection with `isAnnotationPresent()`, `getAnnotation()`. `@Repeatable` allows multiple instances. Type annotations (`@Target(TYPE_USE)`) annotate any type use. Annotations enable framework magic (testing, DI, validation).

**Why It Matters**: Serialization converts objects to byte streams for storage or network transmission—essential for caching (Redis), messaging (Kafka), RPC. However, default serialization is fragile (breaks with class changes), slow, and has security risks (deserialization attacks). JSON serialization (Jackson, Gson) is more maintainable, human-readable, and cross-language compatible. Understanding serialVersionUID prevents deserialization failures. Avoid default serialization—prefer explicit serialization formats (JSON, Protocol Buffers) that version gracefully and integrate with non-Java systems.

---

### Example 42: Optional Deep Dive

`Optional` represents potentially absent values, replacing null checks. Functional methods (`map`, `flatMap`, `filter`, `orElse`) compose operations. Use to make nullability explicit in APIs.

**Code**:

```java
import java.util.*;
import java.util.stream.*;

// Creating Optional instances
Optional<String> present = Optional.of("Hello"); // => Optional with value
// Optional<String> nullError = Optional.of(null); // => ERROR: NullPointerException

Optional<String> nullable = Optional.ofNullable("World"); // => Optional with value
Optional<String> empty = Optional.ofNullable(null); // => Optional.empty (no NPE)
Optional<String> explicitEmpty = Optional.empty(); // => Explicitly empty

// Checking presence
boolean hasValue = present.isPresent(); // => true (value present)
boolean isEmpty = empty.isEmpty(); // => true (Java 11+, no value)

// Getting values
String value1 = present.get(); // => "Hello" (throws if empty - avoid!)
String value2 = present.orElse("Default"); // => "Hello" (returns value)
String value3 = empty.orElse("Default"); // => "Default" (fallback when empty)

// orElseGet - lazy evaluation (supplier called only if empty)
String value4 = present.orElseGet(() -> "Computed Default"); // => "Hello" (supplier not called)
String value5 = empty.orElseGet(() -> {
    System.out.println("Computing default"); // => Executes only if empty
    return "Computed Default";
}); // => "Computed Default"

// orElseThrow - throw exception if empty
String value6 = present.orElseThrow(); // => "Hello" (Java 10+)
String value7 = present.orElseThrow(() -> new IllegalStateException("No value!"));
// => "Hello" (custom exception)

// String value8 = empty.orElseThrow(); // => NoSuchElementException

// ifPresent - execute if value present
present.ifPresent(val -> System.out.println("Value: " + val)); // => "Value: Hello"
empty.ifPresent(val -> System.out.println("Value: " + val)); // => Nothing printed

// ifPresentOrElse - execute one action if present, another if empty (Java 9+)
present.ifPresentOrElse(
    val -> System.out.println("Present: " + val), // => Executed: "Present: Hello"
    () -> System.out.println("Empty") // => Not executed
);

empty.ifPresentOrElse(
    val -> System.out.println("Present: " + val), // => Not executed
    () -> System.out.println("Empty") // => Executed: "Empty"
);

// map - transform value if present
Optional<String> mapped = present.map(String::toUpperCase); // => Optional["HELLO"]
Optional<String> emptyMapped = empty.map(String::toUpperCase); // => Optional.empty (no-op)

Optional<Integer> length = present.map(String::length); // => Optional[5]

// Chaining maps
Optional<String> result = present
    .map(String::toUpperCase) // => Optional["HELLO"]
    .map(s -> s + "!") // => Optional["HELLO!"]
    .map(s -> s.repeat(2)); // => Optional["HELLO!HELLO!"]

// flatMap - avoid nested Optionals
class User {
    private Optional<String> email; // => User may have email

    public User(String email) {
        this.email = Optional.ofNullable(email);
    }

    public Optional<String> getEmail() {
        return email; // => Returns Optional
    }
}

Optional<User> user = Optional.of(new User("user@example.com"));

// Wrong: nested Optional (Optional<Optional<String>>)
// Optional<Optional<String>> nestedEmail = user.map(User::getEmail);

// Correct: flatMap unwraps
Optional<String> email = user.flatMap(User::getEmail); // => Optional["user@example.com"]

Optional<String> emailUpper = user
    .flatMap(User::getEmail) // => Optional["user@example.com"]
    .map(String::toUpperCase); // => Optional["USER@EXAMPLE.COM"]

// filter - keep value only if matches predicate
Optional<String> filtered = present.filter(s -> s.length() > 3);
// => Optional["Hello"] (length 5 > 3, passes filter)

Optional<String> filtered2 = present.filter(s -> s.length() > 10);
// => Optional.empty (length 5 not > 10, fails filter)

// Chaining filter
Optional<String> result2 = present
    .filter(s -> s.startsWith("H")) // => Passes
    .filter(s -> s.length() == 5) // => Passes
    .map(String::toUpperCase); // => Optional["HELLO"]

// or - return alternative Optional if empty (Java 9+)
Optional<String> first = Optional.empty(); // => Empty
Optional<String> second = Optional.of("Second"); // => Has value

Optional<String> combined = first.or(() -> second); // => Optional["Second"]
Optional<String> combined2 = present.or(() -> second); // => Optional["Hello"] (first wins)

// stream - convert Optional to Stream (Java 9+)
Stream<String> stream1 = present.stream(); // => Stream with one element
Stream<String> stream2 = empty.stream(); // => Empty stream

// Practical: filter list to only present values
List<Optional<String>> optionals = List.of(
    Optional.of("A"), // => Present
    Optional.empty(), // => Empty
    Optional.of("B"), // => Present
    Optional.empty(), // => Empty
    Optional.of("C") // => Present
);

List<String> values = optionals.stream()
    .flatMap(Optional::stream) // => Flatten Optionals to values only
    .toList(); // => ["A", "B", "C"] (empties removed)

// equals and hashCode
Optional<String> opt1 = Optional.of("Hello"); // => Optional with "Hello"
Optional<String> opt2 = Optional.of("Hello"); // => Optional with "Hello"
Optional<String> opt3 = Optional.of("World"); // => Optional with "World"

boolean equal = opt1.equals(opt2); // => true (same value)
boolean notEqual = opt1.equals(opt3); // => false (different values)
boolean emptyEquals = Optional.empty().equals(Optional.empty()); // => true

// Anti-patterns - what NOT to do

// BAD: Using get() without checking
// String bad1 = optional.get(); // => Can throw NoSuchElementException

// BAD: Checking with isPresent before get (defeats purpose)
if (present.isPresent()) {
    String bad2 = present.get(); // => Better to use orElse, ifPresent, or map
}

// GOOD: Use functional methods
String good1 = present.orElse("Default"); // => Functional approach

// BAD: Optional for collections (use empty collection instead)
// Optional<List<String>> bad3 = Optional.of(new ArrayList<>());
List<String> good2 = new ArrayList<>(); // => Just use empty list

// BAD: Optional fields in classes (serialization issues)
class BadClass {
    // private Optional<String> name; // => Don't do this (use null or @Nullable)
}

// GOOD: Optional in return types
class GoodClass {
    public Optional<String> findName(int id) {
        // Return Optional to indicate possible absence
        return id > 0 ? Optional.of("Name") : Optional.empty();
    }
}

// Practical: repository pattern with Optional
interface UserRepository {
    Optional<User> findById(int id); // => May not find user
    Optional<User> findByEmail(String email); // => May not find user
}

class UserService {
    private UserRepository repository;

    public String getUserEmail(int id) {
        return repository.findById(id) // => Optional<User>
            .flatMap(User::getEmail) // => Optional<String>
            .orElse("No email"); // => String (fallback if user not found or no email)
    }

    public void processUser(int id) {
        repository.findById(id) // => Optional<User>
            .filter(u -> u.getEmail().isPresent()) // => Only users with email
            .flatMap(User::getEmail) // => Get email
            .map(String::toUpperCase) // => Uppercase
            .ifPresent(email -> System.out.println("Processing: " + email));
            // => Only executes if user found and has email
    }
}

// Combining multiple Optionals
Optional<String> firstName = Optional.of("John"); // => First name
Optional<String> lastName = Optional.of("Doe"); // => Last name

// Using map and flatMap to combine
Optional<String> fullName = firstName.flatMap(first ->
    lastName.map(last -> first + " " + last)
); // => Optional["John Doe"]

// If either is empty, result is empty
Optional<String> fullName2 = firstName.flatMap(first ->
    Optional.<String>empty().map(last -> first + " " + last)
); // => Optional.empty

// Converting null-returning methods to Optional
String nullableResult = legacyMethod(); // => May return null
Optional<String> safe = Optional.ofNullable(nullableResult); // => Wraps in Optional

// Migrating from null checks
// OLD:
String oldValue = getValue();
if (oldValue != null) {
    System.out.println(oldValue.toUpperCase());
}

// NEW:
Optional.ofNullable(getValue())
    .map(String::toUpperCase)
    .ifPresent(System.out::println);
```

**Key Takeaway**: Create with `of()` (non-null), `ofNullable()` (nullable), `empty()`. Get values: `orElse()` (default), `orElseGet()` (lazy supplier), `orElseThrow()` (exception). Transform: `map()` (unwrapped), `flatMap()` (nested Optionals). Filter: `filter()` (predicate). Conditionals: `ifPresent()`, `ifPresentOrElse()`. Combine: `or()` (alternative). Convert: `stream()`. Avoid: `get()` without check, `isPresent()`+`get()` pattern, Optional fields. Use in return types to make nullability explicit. Chain operations functionally instead of imperative null checks.

**Why It Matters**: Network programming enables distributed systems—microservices, HTTP APIs, database connections, message queues. Understanding sockets, clients, servers is foundational to backend development. HTTP clients (HttpClient) abstract common patterns, but understanding underlying sockets prevents misuse. Blocking I/O limits scalability—NIO and async libraries (Netty) enable handling thousands of connections. Network failures are inevitable—timeout configuration, retry logic, and error handling are critical for resilience. Network programming is essential for building modern distributed applications.

---

### Example 43: Sealed Classes and Pattern Matching

Sealed classes (Java 17+) restrict inheritance to known subclasses. Combined with pattern matching in switch expressions, they enable exhaustive type checking. Essential for domain modeling with finite type hierarchies.

**Sealed Class Hierarchy:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Sealed["sealed interface Shape<br/>permits Circle, Rectangle, Triangle"]
    Sealed --> Circle["record Circle<br/>double radius"]
    Sealed --> Rectangle["record Rectangle<br/>double width, height"]
    Sealed --> Triangle["record Triangle<br/>double base, height"]

    style Sealed fill:#0173B2,color:#fff
    style Circle fill:#029E73,color:#fff
    style Rectangle fill:#029E73,color:#fff
    style Triangle fill:#029E73,color:#fff
```

**Pattern Matching Switch (Exhaustive):**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Switch["switch shape"]
    Switch --> CaseC["case Circle c<br/>area = πr²"]
    Switch --> CaseR["case Rectangle r<br/>area = w × h"]
    Switch --> CaseT["case Triangle t<br/>area = ½bh"]
    CaseC --> Result["Exhaustive<br/>No default needed"]
    CaseR --> Result
    CaseT --> Result

    style Switch fill:#0173B2,color:#fff
    style CaseC fill:#029E73,color:#fff
    style CaseR fill:#029E73,color:#fff
    style CaseT fill:#029E73,color:#fff
    style Result fill:#CC78BC,color:#fff
```

**Code**:

```java
// Sealed interface - restricts implementations
sealed interface Shape
    permits Circle, Rectangle, Triangle {} // => Only these 3 can implement

// Records implementing sealed interface
record Circle(double radius) implements Shape {}
record Rectangle(double width, double height) implements Shape {}
record Triangle(double base, double height) implements Shape {}

// Attempting to implement Shape from outside permitted types
// class Pentagon implements Shape {} // => ERROR: not permitted

// Pattern matching switch with exhaustiveness checking
double area(Shape shape) {
    return switch (shape) {
        case Circle c -> Math.PI * c.radius() * c.radius(); // => πr²
        case Rectangle r -> r.width() * r.height(); // => width × height
        case Triangle t -> 0.5 * t.base() * t.height(); // => ½ × base × height
        // No default needed - compiler knows all cases covered!
    };
}

Circle circle = new Circle(5.0); // => Circle with radius 5.0
double circleArea = area(circle); // => 78.54 (π × 5²)

Rectangle rect = new Rectangle(4.0, 6.0); // => Rectangle 4×6
double rectArea = area(rect); // => 24.0

// Sealed classes (not interfaces)
sealed abstract class Vehicle
    permits Car, Truck, Motorcycle {

    private final String licensePlate; // => Common field

    protected Vehicle(String licensePlate) {
        this.licensePlate = licensePlate; // => Constructor
    }

    public String getLicensePlate() {
        return licensePlate; // => Getter
    }
}

// Final subclass (cannot be further extended)
final class Car extends Vehicle {
    private final int seats; // => Car-specific field

    public Car(String licensePlate, int seats) {
        super(licensePlate); // => Call parent constructor
        this.seats = seats; // => Initialize seats
    }

    public int getSeats() {
        return seats; // => Getter
    }
}

// Non-sealed subclass (can be extended by anyone)
non-sealed class Truck extends Vehicle {
    private final double capacity; // => Truck-specific field

    public Truck(String licensePlate, double capacity) {
        super(licensePlate);
        this.capacity = capacity;
    }

    public double getCapacity() {
        return capacity;
    }
}

// Can extend non-sealed class
class PickupTruck extends Truck {
    public PickupTruck(String licensePlate) {
        super(licensePlate, 1000.0); // => Fixed capacity
    }
}

// Sealed subclass (further restricts)
sealed class Motorcycle extends Vehicle
    permits SportBike, Cruiser {

    protected Motorcycle(String licensePlate) {
        super(licensePlate);
    }
}

final class SportBike extends Motorcycle {
    public SportBike(String licensePlate) {
        super(licensePlate);
    }
}

final class Cruiser extends Motorcycle {
    public Cruiser(String licensePlate) {
        super(licensePlate);
    }
}

// Pattern matching with sealed classes
String describe(Vehicle vehicle) {
    return switch (vehicle) {
        case Car c -> "Car with " + c.getSeats() + " seats"; // => Car pattern
        case Truck t -> "Truck with capacity " + t.getCapacity() + " kg"; // => Truck pattern
        case Motorcycle m -> "Motorcycle: " + m.getLicensePlate(); // => Motorcycle pattern
        // No default needed - all Vehicle types covered
    };
}

Car myCar = new Car("ABC-123", 5); // => 5-seater car
String desc = describe(myCar); // => "Car with 5 seats"

// Nested pattern matching with sealed types
String detailedDescribe(Vehicle vehicle) {
    return switch (vehicle) {
        case Car c -> "Car with " + c.getSeats() + " seats";
        case Truck t -> "Truck with capacity " + t.getCapacity() + " kg";
        case SportBike sb -> "Sport bike: " + sb.getLicensePlate(); // => Specific motorcycle type
        case Cruiser cr -> "Cruiser: " + cr.getLicensePlate(); // => Specific motorcycle type
        // Compiler ensures all cases covered (including Motorcycle subtypes)
    };
}

// Guards in pattern matching (when clauses)
String categorizeVehicle(Vehicle vehicle) {
    return switch (vehicle) {
        case Car c when c.getSeats() > 5 -> "Large car"; // => Guard condition
        case Car c when c.getSeats() <= 5 -> "Small car";
        case Truck t when t.getCapacity() > 5000 -> "Heavy truck";
        case Truck t -> "Light truck"; // => Catches remaining trucks
        case Motorcycle m -> "Motorcycle";
    };
}

Car largeCar = new Car("XYZ-789", 7); // => 7-seater
String category = categorizeVehicle(largeCar); // => "Large car"

// Sealed enum alternative (traditional approach)
enum TraditionalShape {
    CIRCLE, RECTANGLE, TRIANGLE // => Fixed set of constants
}

// vs sealed types (modern approach with data)
// Sealed types carry data (radius, width, height)
// Enums just represent constants without associated data

// Practical: Result type (success or error)
sealed interface Result<T, E>
    permits Success, Failure {
}

record Success<T, E>(T value) implements Result<T, E> {}
record Failure<T, E>(E error) implements Result<T, E> {}

// Using Result type
Result<Integer, String> divide(int a, int b) {
    if (b == 0) {
        return new Failure<>("Division by zero"); // => Error case
    }
    return new Success<>(a / b); // => Success case
}

Result<Integer, String> result = divide(10, 2); // => Success(5)

String message = switch (result) {
    case Success<Integer, String> s -> "Result: " + s.value(); // => "Result: 5"
    case Failure<Integer, String> f -> "Error: " + f.error(); // => Not executed
};

Result<Integer, String> error = divide(10, 0); // => Failure("Division by zero")

String errorMessage = switch (error) {
    case Success<Integer, String> s -> "Result: " + s.value(); // => Not executed
    case Failure<Integer, String> f -> "Error: " + f.error(); // => "Error: Division by zero"
};

// Practical: JSON AST (Abstract Syntax Tree)
sealed interface Json
    permits JsonObject, JsonArray, JsonString, JsonNumber, JsonBoolean, JsonNull {}

record JsonObject(Map<String, Json> fields) implements Json {}
record JsonArray(List<Json> elements) implements Json {}
record JsonString(String value) implements Json {}
record JsonNumber(double value) implements Json {}
record JsonBoolean(boolean value) implements Json {}
record JsonNull() implements Json {}

// Pattern matching for JSON processing
String toJsonString(Json json) {
    return switch (json) {
        case JsonObject obj -> "{" + obj.fields().entrySet().stream()
            .map(e -> "\"" + e.getKey() + "\":" + toJsonString(e.getValue()))
            .collect(Collectors.joining(",")) + "}";
        case JsonArray arr -> "[" + arr.elements().stream()
            .map(this::toJsonString)
            .collect(Collectors.joining(",")) + "]";
        case JsonString str -> "\"" + str.value() + "\"";
        case JsonNumber num -> String.valueOf(num.value());
        case JsonBoolean bool -> String.valueOf(bool.value());
        case JsonNull n -> "null";
    };
}

// Create JSON structure
Json json = new JsonObject(Map.of(
    "name", new JsonString("Alice"),
    "age", new JsonNumber(30),
    "active", new JsonBoolean(true),
    "address", new JsonNull()
));

String jsonStr = toJsonString(json); // => {"name":"Alice","age":30.0,"active":true,"address":null}

// Benefits of sealed types:
// 1. Exhaustiveness checking - compiler ensures all cases handled
// 2. Domain modeling - express finite type hierarchies explicitly
// 3. Pattern matching - switch expressions with no default
// 4. Maintainability - adding new subtype causes compile errors in all switches
// 5. Documentation - permitted types are explicit in interface/class declaration

// Restrictions:
// - Sealed type and permitted subtypes must be in same package (or module)
// - Permitted subtypes must be final, sealed, or non-sealed
// - Cannot extend sealed type outside permitted list
```

**Key Takeaway**: Sealed classes/interfaces use `sealed` with `permits` clause to restrict subclasses. Permitted types must be `final` (no further extension), `sealed` (controlled extension), or `non-sealed` (open extension). Pattern matching switch with sealed types enables exhaustiveness checking (no `default` needed). Guards (`when` clauses) add conditions to patterns. Ideal for finite type hierarchies (shapes, results, ASTs). Compiler ensures all cases covered—adding new type causes compile errors in all switches. Must be in same package/module as sealed parent.

**Why It Matters**: Annotations enable declarative programming—express intent without boilerplate. They power frameworks (Spring's @Autowired, JPA's @Entity, JUnit's @Test) enabling configuration as code. Understanding retention (source, class, runtime) determines annotation availability. Reflection enables processing annotations at runtime—driving dependency injection, validation, persistence. However, annotations can obscure behavior—balance conciseness with explicitness. Custom annotations enable domain-specific configurations. Annotations are fundamental to modern Java development—nearly all frameworks rely on them.

---

### Example 44: Records Advanced Patterns

Records (Java 14+) are immutable data carriers with automatic constructor, getters, `equals`, `hashCode`, and `toString`. Compact constructors validate data. Records work with sealed types and pattern matching for powerful domain modeling.

**Code**:

```java
// Basic record - immutable data class
record Point(int x, int y) {} // => Automatic constructor, getters, equals, hashCode, toString

Point p1 = new Point(3, 4); // => Create record instance
int x = p1.x(); // => 3 (accessor method, not field)
int y = p1.y(); // => 4

System.out.println(p1); // => "Point[x=3, y=4]" (automatic toString)

Point p2 = new Point(3, 4); // => Same values as p1
boolean equal = p1.equals(p2); // => true (automatic equals based on fields)
int hash1 = p1.hashCode(); // => Consistent with equals
int hash2 = p2.hashCode(); // => Same as hash1

// Compact constructor - validation and normalization
record Range(int start, int end) {
    // Compact constructor - implicit parameters
    public Range {
        if (start > end) {
            // Swap if inverted
            int temp = start; // => Store start temporarily
            start = end; // => Assign end to start
            end = temp; // => Assign temp to end
        }
        // Fields automatically assigned after compact constructor
    }
}

Range range1 = new Range(10, 20); // => start=10, end=20 (no swap)
Range range2 = new Range(20, 10); // => start=10, end=20 (swapped automatically)

// Validation in compact constructor
record Person(String name, int age) {
    public Person {
        if (name == null || name.isBlank()) {
            throw new IllegalArgumentException("Name required"); // => Validation
        }
        if (age < 0 || age > 150) {
            throw new IllegalArgumentException("Invalid age"); // => Validation
        }
        // Normalization
        name = name.trim(); // => Remove whitespace
    }
}

Person alice = new Person("  Alice  ", 30); // => name trimmed to "Alice"
// Person invalid = new Person("", 200); // => IllegalArgumentException

// Canonical constructor - explicit parameters
record Employee(String name, int id, double salary) {
    // Explicit canonical constructor
    public Employee(String name, int id, double salary) {
        this.name = name; // => Explicit assignment
        this.id = id;
        this.salary = Math.max(0, salary); // => Ensure non-negative
    }
}

// Additional constructors
record Book(String title, String author, int year) {
    // Compact constructor
    public Book {
        if (title == null || author == null) {
            throw new IllegalArgumentException("Title and author required");
        }
    }

    // Additional constructor - must call canonical constructor
    public Book(String title, String author) {
        this(title, author, 2024); // => Delegate to canonical with default year
    }
}

Book book1 = new Book("Java Guide", "Smith", 2023); // => Canonical constructor
Book book2 = new Book("Python Guide", "Jones"); // => Additional constructor (year=2024)

// Static methods and fields
record MathUtils(int value) {
    public static final double PI = 3.14159; // => Static constant

    public static int add(int a, int b) {
        return a + b; // => Static method
    }

    public int square() {
        return value * value; // => Instance method using field
    }
}

int sum = MathUtils.add(5, 3); // => 8 (static method)
MathUtils m = new MathUtils(5);
int squared = m.square(); // => 25 (instance method)

// Overriding accessor methods
record Temperature(double celsius) {
    // Override accessor to provide computed value
    public double fahrenheit() {
        return celsius * 9.0 / 5.0 + 32; // => Convert to Fahrenheit
    }

    @Override
    public double celsius() {
        return Math.round(celsius * 10) / 10.0; // => Round to 1 decimal place
    }
}

Temperature temp = new Temperature(25.67); // => 25.67°C
double c = temp.celsius(); // => 25.7 (rounded by overridden accessor)
double f = temp.fahrenheit(); // => 78.206 (computed)

// Generic records
record Pair<T, U>(T first, U second) {} // => Generic record

Pair<String, Integer> pair = new Pair<>("Age", 30); // => Pair of String and Integer
String key = pair.first(); // => "Age"
Integer value = pair.second(); // => 30

// Record with collections (defensive copying)
record Playlist(String name, List<String> songs) {
    // Compact constructor with defensive copy
    public Playlist {
        songs = List.copyOf(songs); // => Create immutable copy (defensive)
    }

    // Accessor already returns immutable list
}

List<String> mySongs = new ArrayList<>(List.of("Song 1", "Song 2"));
Playlist playlist = new Playlist("Favorites", mySongs);

mySongs.add("Song 3"); // => Modify original list
System.out.println(playlist.songs().size()); // => 2 (playlist unaffected, has immutable copy)

// playlist.songs().add("Song 4"); // => UnsupportedOperationException (immutable)

// Nested records
record Address(String street, String city, String zip) {}

record Customer(String name, Address address, List<String> phoneNumbers) {
    public Customer {
        phoneNumbers = List.copyOf(phoneNumbers); // => Defensive copy
    }
}

Address addr = new Address("123 Main St", "Springfield", "12345");
Customer customer = new Customer("Alice", addr, List.of("555-1234", "555-5678"));

String city = customer.address().city(); // => "Springfield" (nested access)

// Pattern matching with records (Java 16+)
record Rectangle(double width, double height) {}
record Circle(double radius) {}

// Deconstruction in switch (Java 19+, preview in 19-20, standard in 21+)
double area(Object shape) {
    return switch (shape) {
        case Rectangle(double w, double h) -> w * h; // => Deconstruct Rectangle
        case Circle(double r) -> Math.PI * r * r; // => Deconstruct Circle
        case null -> 0; // => Handle null
        default -> throw new IllegalArgumentException("Unknown shape");
    };
}

Rectangle rect = new Rectangle(5, 10);
double rectArea = area(rect); // => 50.0 (5 × 10)

Circle circ = new Circle(3);
double circArea = area(circ); // => 28.27 (π × 3²)

// Nested pattern matching (Java 21+)
record Point3D(int x, int y, int z) {}
record Line(Point3D start, Point3D end) {}

String describeLine(Line line) {
    return switch (line) {
        case Line(Point3D(int x1, int y1, int z1), Point3D(int x2, int y2, int z2)) ->
            "Line from (" + x1 + "," + y1 + "," + z1 + ") to (" + x2 + "," + y2 + "," + z2 + ")";
        // => Deconstructs Line AND both Point3D records
    };
}

Line line = new Line(new Point3D(0, 0, 0), new Point3D(10, 20, 30));
String desc = describeLine(line); // => "Line from (0,0,0) to (10,20,30)"

// Records with sealed types (powerful combination)
sealed interface JsonValue permits JsonText, JsonNum, JsonBool {}
record JsonText(String value) implements JsonValue {}
record JsonNum(double value) implements JsonValue {}
record JsonBool(boolean value) implements JsonValue {}

String formatJson(JsonValue json) {
    return switch (json) {
        case JsonText(String s) -> "\"" + s + "\""; // => Deconstruct and use value
        case JsonNum(double n) -> String.valueOf(n);
        case JsonBool(boolean b) -> String.valueOf(b);
        // No default needed (sealed + exhaustive)
    };
}

JsonValue text = new JsonText("hello");
String formatted = formatJson(text); // => "\"hello\""

// Record limitations
// - Cannot extend other classes (implicit extends Record)
// - Cannot declare instance fields beyond components
// - All fields are final (immutable)
// - Cannot be abstract
// - Cannot have native methods

// What you CAN do:
// - Implement interfaces ✓
// - Have static fields and methods ✓
// - Override methods (including accessors) ✓
// - Have instance methods ✓
// - Have nested types ✓
// - Use generics ✓

// Implementing interface
interface Drawable {
    void draw();
}

record Shape(String name) implements Drawable {
    @Override
    public void draw() {
        System.out.println("Drawing " + name); // => Instance method from interface
    }
}

Shape shape = new Shape("Circle");
shape.draw(); // => "Drawing Circle"

// Immutability with mutable components (anti-pattern)
record BadRecord(List<String> items) {} // => DON'T: mutable component

List<String> list = new ArrayList<>(List.of("A", "B"));
BadRecord bad = new BadRecord(list);
list.add("C"); // => Modifies record's internal state! (bad)

// Better: defensive copy in constructor
record GoodRecord(List<String> items) {
    public GoodRecord {
        items = List.copyOf(items); // => Immutable copy
    }
}

List<String> list2 = new ArrayList<>(List.of("X", "Y"));
GoodRecord good = new GoodRecord(list2);
list2.add("Z"); // => Doesn't affect record (good)
```

**Key Takeaway**: Records are immutable data classes with automatic constructor, accessors, `equals`, `hashCode`, `toString`. Compact constructor validates/normalizes without parameter list. Additional constructors must delegate to canonical. Override accessors for computed values. Use defensive copying for mutable components. Pattern matching deconstructs records. Combine with sealed types for exhaustive checking. Cannot extend classes or add instance fields. Ideal for DTOs, value objects, and domain models. Enforce immutability by using `List.copyOf()` for collections.

**Why It Matters**: Reflection enables runtime introspection and manipulation—frameworks use it for dependency injection, ORM, serialization, testing. It breaks encapsulation intentionally—accessing private fields, invoking private methods. However, reflection is slow, fragile (breaks with refactoring), and bypasses compile-time safety. Use it sparingly—prefer interfaces and composition. Understanding reflection is essential for framework developers and debugging, but application code should avoid it. Most reflection use cases (DI, serialization) are better served by annotation processing or code generation.

---

### Example 45: Stream Performance and Parallelization

Streams provide functional operations but have performance characteristics. Understand when to use sequential vs parallel streams, performance pitfalls, and optimization techniques for production code.

**Code**:

```java
import java.util.*;
import java.util.stream.*;
import java.util.concurrent.*;

// Sequential vs Parallel streams
List<Integer> numbers = IntStream.rangeClosed(1, 1000000) // => 1 million numbers
    .boxed()
    .toList(); // => [1, 2, 3, ..., 1000000]

// Sequential stream - single thread
long start = System.nanoTime();
long sum1 = numbers.stream() // => Sequential stream
    .mapToLong(Integer::longValue) // => Convert to long
    .sum(); // => Sum all numbers: 500000500000
long end = System.nanoTime();
System.out.println("Sequential: " + (end - start) / 1_000_000 + "ms"); // => ~50ms

// Parallel stream - multiple threads
start = System.nanoTime();
long sum2 = numbers.parallelStream() // => Parallel stream (uses ForkJoinPool)
    .mapToLong(Integer::longValue)
    .sum(); // => Same result: 500000500000
end = System.nanoTime();
System.out.println("Parallel: " + (end - start) / 1_000_000 + "ms"); // => ~15ms (faster)

// Creating parallel streams
Stream<Integer> parallel1 = numbers.parallelStream(); // => From collection
Stream<Integer> parallel2 = Stream.of(1, 2, 3).parallel(); // => Make stream parallel
Stream<Integer> parallel3 = IntStream.range(0, 100).parallel().boxed(); // => Primitive stream

// Check if stream is parallel
boolean isParallel = parallel1.isParallel(); // => true

// Convert between sequential and parallel
Stream<Integer> sequential = parallel1.sequential(); // => Make sequential
Stream<Integer> parallelAgain = sequential.parallel(); // => Make parallel again

// When parallel is faster (CPU-bound operations)
List<String> words = IntStream.range(0, 10000)
    .mapToObj(i -> "word" + i)
    .toList(); // => 10,000 words

// CPU-intensive operation
start = System.nanoTime();
long count1 = words.stream()
    .filter(s -> s.length() > 3) // => Simple filter
    .map(String::toUpperCase) // => CPU-intensive transformation
    .count();
end = System.nanoTime();
System.out.println("Sequential: " + (end - start) / 1_000_000 + "ms");

start = System.nanoTime();
long count2 = words.parallelStream()
    .filter(s -> s.length() > 3)
    .map(String::toUpperCase)
    .count();
end = System.nanoTime();
System.out.println("Parallel: " + (end - start) / 1_000_000 + "ms"); // => Usually faster

// When parallel is slower (small datasets or cheap operations)
List<Integer> small = List.of(1, 2, 3, 4, 5); // => Small dataset

start = System.nanoTime();
int sum3 = small.stream().mapToInt(i -> i).sum(); // => Sequential
end = System.nanoTime();
long seqTime = end - start;

start = System.nanoTime();
int sum4 = small.parallelStream().mapToInt(i -> i).sum(); // => Parallel
end = System.nanoTime();
long parTime = end - start;

// Parallel has overhead (thread coordination) - slower for small datasets
System.out.println("Parallel overhead: " + (parTime - seqTime) / 1000 + "μs");

// Thread safety issues with parallel streams
List<Integer> unsafeList = new ArrayList<>(); // => NOT thread-safe

// WRONG: ArrayList not thread-safe for parallel writes
numbers.parallelStream()
    .forEach(unsafeList::add); // => Race condition! Results unpredictable

System.out.println("Unsafe size: " + unsafeList.size()); // => NOT 1000000 (some lost)

// CORRECT: Use thread-safe collection
List<Integer> safeList = numbers.parallelStream()
    .collect(Collectors.toCollection(CopyOnWriteArrayList::new));
// => Thread-safe collection

System.out.println("Safe size: " + safeList.size()); // => 1000000 (all present)

// BEST: Use collectors (designed for parallel)
List<Integer> collected = numbers.parallelStream()
    .filter(n -> n % 2 == 0) // => Even numbers only
    .collect(Collectors.toList()); // => Thread-safe collector

// Order preservation with parallel streams
List<Integer> ordered = IntStream.range(0, 100)
    .parallel() // => Parallel stream
    .boxed()
    .collect(Collectors.toList()); // => Maintains encounter order: [0,1,2,...,99]

List<Integer> unordered = IntStream.range(0, 100)
    .parallel()
    .unordered() // => Hint: order not important (can improve performance)
    .boxed()
    .collect(Collectors.toList()); // => May not maintain order (but faster)

// forEachOrdered vs forEach in parallel
System.out.println("forEach (unordered):");
IntStream.range(0, 10)
    .parallel()
    .forEach(System.out::print); // => 4723061598 (unpredictable order)

System.out.println("\nforEachOrdered (ordered):");
IntStream.range(0, 10)
    .parallel()
    .forEachOrdered(System.out::print); // => 0123456789 (guaranteed order, slower)

// Spliterator - controls parallel splitting
List<Integer> list = new ArrayList<>(numbers);
Spliterator<Integer> spliterator = list.spliterator();

// Characteristics affect parallel performance
int characteristics = spliterator.characteristics();
boolean sized = spliterator.hasCharacteristics(Spliterator.SIZED); // => true (known size)
boolean ordered = spliterator.hasCharacteristics(Spliterator.ORDERED); // => true (has order)

// Custom ForkJoinPool for parallel streams
ForkJoinPool customPool = new ForkJoinPool(4); // => 4 threads (default uses all cores)

try {
    long customSum = customPool.submit(() ->
        numbers.parallelStream()
            .mapToLong(Integer::longValue)
            .sum()
    ).get(); // => Execute in custom pool (4 threads)

    System.out.println("Custom pool result: " + customSum);
} catch (Exception e) {
    e.printStackTrace();
} finally {
    customPool.shutdown(); // => Clean up custom pool
}

// Performance anti-patterns

// 1. Boxing/unboxing overhead
// SLOW: Boxing Integer to int repeatedly
long slowSum = numbers.stream()
    .map(i -> i * 2) // => Boxing/unboxing on each operation
    .reduce(0, Integer::sum); // => Inefficient

// FAST: Use primitive streams
long fastSum = numbers.stream()
    .mapToInt(Integer::intValue) // => Convert to IntStream once
    .map(i -> i * 2) // => Primitive operations (no boxing)
    .sum(); // => Efficient

// 2. Stateful operations in parallel (problematic)
AtomicInteger counter = new AtomicInteger(0); // => Shared mutable state

// AVOID: Stateful lambda in parallel
numbers.parallelStream()
    .map(n -> n * counter.incrementAndGet()) // => Race condition risk
    .collect(Collectors.toList()); // => Unpredictable results

// BETTER: Stateless operations
List<Integer> doubled = numbers.parallelStream()
    .map(n -> n * 2) // => Stateless (no shared mutable state)
    .collect(Collectors.toList()); // => Predictable, thread-safe

// 3. Interference during stream operations
List<Integer> source = new ArrayList<>(List.of(1, 2, 3, 4, 5));

// WRONG: Modifying source during stream
// source.stream()
//     .forEach(n -> source.add(n * 2)); // => ConcurrentModificationException

// CORRECT: Don't modify source, create new collection
List<Integer> result = source.stream()
    .map(n -> n * 2)
    .collect(Collectors.toList()); // => [2, 4, 6, 8, 10]

// Stream reuse (not allowed)
Stream<Integer> stream = numbers.stream();
long count3 = stream.count(); // => First terminal operation: 1000000
// long count4 = stream.count(); // => ERROR: stream already operated on

// Best practices for parallel streams:

// 1. Use for large datasets (10,000+ elements)
boolean useFarkJoinPool = numbers.size() > 10_000; // => Heuristic for parallelization

// 2. Use for CPU-intensive operations (not I/O)
// GOOD for parallel: complex calculations, transformations
// BAD for parallel: database queries, file I/O (use CompletableFuture instead)

// 3. Ensure thread safety
// Use Collectors, avoid shared mutable state, use thread-safe data structures

// 4. Prefer stateless operations
// map, filter, flatMap are stateless and parallelize well
// sorted, distinct are stateful and may reduce parallel performance

// 5. Measure performance
// Always benchmark sequential vs parallel for your specific workload

// Performance comparison: collect vs reduce
// collect is designed for parallelism
List<String> collected2 = numbers.parallelStream()
    .map(String::valueOf)
    .collect(Collectors.toList()); // => Efficient parallel collection

// reduce may have overhead for complex accumulation
String concatenated = numbers.stream()
    .limit(100)
    .map(String::valueOf)
    .reduce("", (a, b) -> a + "," + b); // => Inefficient for strings (use joining instead)

String joined = numbers.stream()
    .limit(100)
    .map(String::valueOf)
    .collect(Collectors.joining(",")); // => Efficient (designed for this use case)

// Primitive streams for performance
// BAD: Stream<Integer> with boxing
double avg1 = numbers.stream()
    .mapToDouble(Integer::doubleValue) // => Boxing/unboxing
    .average()
    .orElse(0.0);

// GOOD: IntStream without boxing
double avg2 = numbers.stream()
    .mapToInt(Integer::intValue) // => Convert to primitive once
    .average()
    .orElse(0.0); // => No boxing during operations

// Summary statistics (efficient)
IntSummaryStatistics stats = numbers.stream()
    .mapToInt(Integer::intValue)
    .summaryStatistics();

System.out.println("Count: " + stats.getCount()); // => 1000000
System.out.println("Sum: " + stats.getSum()); // => 500000500000
System.out.println("Min: " + stats.getMin()); // => 1
System.out.println("Max: " + stats.getMax()); // => 1000000
System.out.println("Average: " + stats.getAverage()); // => 500000.5
```

**Key Takeaway**: Parallel streams use `parallelStream()` or `.parallel()`. Best for large datasets (10k+ elements) with CPU-intensive operations. Overhead makes parallel slower for small data or cheap operations. Use `Collectors` for thread-safe accumulation. Avoid shared mutable state in parallel lambdas. `forEachOrdered()` maintains order (slower than `forEach()`). Primitive streams (`IntStream`, `LongStream`, `DoubleStream`) avoid boxing overhead. Custom `ForkJoinPool` controls thread count. Always benchmark sequential vs parallel. Stream reuse not allowed. Prefer stateless operations (`map`, `filter`) over stateful (`sorted`, `distinct`) for parallelism.

**Why It Matters**: Generics enable type-safe, reusable code—collections, algorithms, frameworks. Understanding wildcards (? extends, ? super) enables flexible APIs (PECS: Producer Extends, Consumer Super). Bounded type parameters enable constrained generics (T extends Comparable). Type erasure causes runtime type information loss—understanding this prevents confusion with reflection and overloading. Raw types break type safety—always use generics. Proper generics usage prevents ClassCastException, improves code readability, and enables powerful abstractions like Stream processing.
