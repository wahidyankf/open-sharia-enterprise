---
title: "Advanced"
weight: 11000003
date: 2025-01-29T00:00:00+07:00
draft: false
description: "Advanced-level Spring Framework examples covering REST APIs, Security, caching, async processing, and testing (Coverage: 75-95%)"
tags: ["spring", "java", "kotlin", "advanced", "rest", "security", "testing"]
---

This tutorial provides 25 advanced Spring Framework examples (51-75) covering production-ready patterns. Focus includes REST APIs, Security, caching, async execution, and comprehensive testing strategies.

**Coverage**: 75-95% of Spring Framework features
**Prerequisites**: Complete [Beginner](/en/learn/software-engineering/platform-web/jvm-spring/by-example/beginner) and [Intermediate](/en/learn/software-engineering/platform-web/jvm-spring/by-example/intermediate) tutorials first

## REST API Development (Examples 51-55)

### Example 51: @RestController and ResponseEntity (Coverage: 76.5%)

Demonstrates RESTful API with proper HTTP responses.

**Java Implementation**:

```java
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController  // => @Controller + @ResponseBody combined
@RequestMapping("/api/donations")
public class DonationRestController {
    @GetMapping("/{id}")
    public ResponseEntity<String> getById(@PathVariable Long id) {
        // => ResponseEntity controls HTTP status and headers

        if (id <= 0) {
            return ResponseEntity.badRequest().body("Invalid ID");
            // => Returns HTTP 400 Bad Request
        }

        String donation = "Donation #" + id;
        return ResponseEntity.ok(donation);
        // => Returns HTTP 200 OK with body
    }

    @PostMapping
    public ResponseEntity<String> create(@RequestBody String donor) {
        String result = "Created for " + donor;

        return ResponseEntity
            .status(HttpStatus.CREATED)  // => HTTP 201 Created
            .header("Location", "/api/donations/123")  // => Custom header
            .body(result);
        // => Full control over response
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*

@RestController  // => @Controller + @ResponseBody combined
@RequestMapping("/api/donations")
class DonationRestController {
    @GetMapping("/{id}")
    fun getById(@PathVariable id: Long): ResponseEntity<String> {
        // => ResponseEntity controls HTTP status and headers

        return if (id <= 0) {
            ResponseEntity.badRequest().body("Invalid ID")
            // => Returns HTTP 400 Bad Request
        } else {
            val donation = "Donation #$id"
            ResponseEntity.ok(donation)
            // => Returns HTTP 200 OK with body
        }
    }

    @PostMapping
    fun create(@RequestBody donor: String): ResponseEntity<String> {
        val result = "Created for $donor"

        return ResponseEntity
            .status(HttpStatus.CREATED)  // => HTTP 201 Created
            .header("Location", "/api/donations/123")  // => Custom header
            .body(result)
        // => Full control over response
    }
}
```

**Key Takeaways**:

- ResponseEntity provides full HTTP response control
- Fluent builder API for status, headers, body
- Type-safe response bodies
- RESTful status codes (200, 201, 400, etc.)

**Related Documentation**:

- [REST Controller Documentation](https://docs.spring.io/spring-framework/reference/web/webmvc/mvc-controller/ann-rest.html)

---

### Example 52: Content Negotiation (Coverage: 78.0%)

Demonstrates producing different content types.

**Java Implementation**:

```java
import org.springframework.web.bind.annotation.*;

class Donation {
    private String donor;
    private double amount;

    public Donation(String donor, double amount) {
        this.donor = donor;
        this.amount = amount;
    }

    public String getDonor() { return donor; }
    public double getAmount() { return amount; }
}

@RestController
@RequestMapping("/api/donations")
public class ContentNegotiationController {
    @GetMapping(produces = "application/json")
    // => Produces JSON when Accept: application/json
    public Donation getAsJson() {
        return new Donation("Ali", 500.0);
        // => Serialized to: {"donor":"Ali","amount":500.0}
    }

    @GetMapping(produces = "application/xml")
    // => Produces XML when Accept: application/xml
    public Donation getAsXml() {
        return new Donation("Fatima", 300.0);
        // => Serialized to: <Donation><donor>Fatima</donor>...</Donation>
        // => Requires JAXB annotations on Donation class
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.web.bind.annotation.*

data class Donation(val donor: String, val amount: Double)

@RestController
@RequestMapping("/api/donations")
class ContentNegotiationController {
    @GetMapping(produces = ["application/json"])
    // => Produces JSON when Accept: application/json
    fun getAsJson(): Donation {
        return Donation("Ali", 500.0)
        // => Serialized to: {"donor":"Ali","amount":500.0}
    }

    @GetMapping(produces = ["application/xml"])
    // => Produces XML when Accept: application/xml
    fun getAsXml(): Donation {
        return Donation("Fatima", 300.0)
        // => Serialized to: <Donation><donor>Fatima</donor>...</Donation>
        // => Requires JAXB annotations on Donation class
    }
}
```

**Key Takeaways**:

- produces attribute specifies content types
- Content negotiation via Accept header
- Jackson handles JSON automatically
- JAXB for XML serialization

**Related Documentation**:

- [Content Negotiation Documentation](https://docs.spring.io/spring-framework/reference/web/webmvc/mvc-config/content-negotiation.html)

---

### Example 53: CORS Configuration (Coverage: 79.5%)

Demonstrates Cross-Origin Resource Sharing setup.

**Java Implementation**:

```java
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api")
@CrossOrigin(
    origins = "http://localhost:3000",  // => Allowed origin
    methods = {RequestMethod.GET, RequestMethod.POST},  // => Allowed methods
    allowedHeaders = "*",  // => All headers allowed
    maxAge = 3600  // => Preflight cache duration (seconds)
)
public class CorsController {
    @GetMapping("/data")
    public String getData() {
        return "CORS enabled data";
        // => Accessible from http://localhost:3000
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api")
@CrossOrigin(
    origins = ["http://localhost:3000"],  // => Allowed origin
    methods = [RequestMethod.GET, RequestMethod.POST],  // => Allowed methods
    allowedHeaders = ["*"],  // => All headers allowed
    maxAge = 3600  // => Preflight cache duration (seconds)
)
class CorsController {
    @GetMapping("/data")
    fun getData(): String {
        return "CORS enabled data"
        // => Accessible from http://localhost:3000
    }
}
```

**Key Takeaways**:

- @CrossOrigin enables CORS
- Specify allowed origins, methods, headers
- Method-level or class-level annotation
- Global CORS config also possible

**Related Documentation**:

- [CORS Documentation](https://docs.spring.io/spring-framework/reference/web/webmvc-cors.html)

---

### Example 54: API Versioning (Coverage: 81.0%)

Demonstrates REST API versioning strategies.

**Java Implementation**:

```java
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/donations")
// => URI versioning: version in path
public class DonationsV1Controller {
    @GetMapping
    public String listV1() {
        return "Donations API v1";
        // => Simple string response
    }
}

@RestController
@RequestMapping("/api/v2/donations")
// => Version 2 with improved response
public class DonationsV2Controller {
    @GetMapping
    public DonationResponse listV2() {
        return new DonationResponse("Enhanced v2 response");
        // => Structured response object
    }
}

class DonationResponse {
    private String message;
    public DonationResponse(String message) { this.message = message; }
    public String getMessage() { return message; }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/donations")
// => URI versioning: version in path
class DonationsV1Controller {
    @GetMapping
    fun listV1(): String {
        return "Donations API v1"
        // => Simple string response
    }
}

@RestController
@RequestMapping("/api/v2/donations")
// => Version 2 with improved response
class DonationsV2Controller {
    @GetMapping
    fun listV2(): DonationResponse {
        return DonationResponse("Enhanced v2 response")
        // => Structured response object
    }
}

data class DonationResponse(val message: String)
```

**Key Takeaways**:

- URI versioning most common (/v1/, /v2/)
- Separate controllers per version
- Maintains backward compatibility
- Can deprecate old versions gradually

**Related Documentation**:

- [API Versioning Documentation](https://docs.spring.io/spring-framework/reference/web/webmvc/mvc-controller/ann-requestmapping.html#mvc-ann-requestmapping-consumes-produces)

---

### Example 55: Global Exception Handler (Coverage: 82.5%)

Demonstrates centralized exception handling.

**Java Implementation**:

```java
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

class ErrorResponse {
    private String message;
    private int status;

    public ErrorResponse(String message, int status) {
        this.message = message;
        this.status = status;
    }

    public String getMessage() { return message; }
    public int getStatus() { return status; }
}

@RestControllerAdvice  // => Global exception handler for all controllers
public class GlobalExceptionHandler {
    @ExceptionHandler(IllegalArgumentException.class)
    // => Handles IllegalArgumentException from any controller
    public ResponseEntity<ErrorResponse> handleIllegalArgument(
        IllegalArgumentException ex
    ) {
        ErrorResponse error = new ErrorResponse(ex.getMessage(), 400);
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
        // => Returns 400 with error details
    }

    @ExceptionHandler(Exception.class)
    // => Catch-all for unhandled exceptions
    public ResponseEntity<ErrorResponse> handleGeneral(Exception ex) {
        ErrorResponse error = new ErrorResponse("Internal error", 500);
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
        // => Returns 500 for unexpected errors
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*

data class ErrorResponse(val message: String, val status: Int)

@RestControllerAdvice  // => Global exception handler for all controllers
class GlobalExceptionHandler {
    @ExceptionHandler(IllegalArgumentException::class)
    // => Handles IllegalArgumentException from any controller
    fun handleIllegalArgument(ex: IllegalArgumentException): ResponseEntity<ErrorResponse> {
        val error = ErrorResponse(ex.message ?: "Bad request", 400)
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error)
        // => Returns 400 with error details
    }

    @ExceptionHandler(Exception::class)
    // => Catch-all for unhandled exceptions
    fun handleGeneral(ex: Exception): ResponseEntity<ErrorResponse> {
        val error = ErrorResponse("Internal error", 500)
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error)
        // => Returns 500 for unexpected errors
    }
}
```

**Key Takeaways**:

- @RestControllerAdvice for global handlers
- Consistent error responses across all endpoints
- Multiple @ExceptionHandler methods
- Prioritizes specific exceptions over general

**Related Documentation**:

- [Global Exception Handler Documentation](https://docs.spring.io/spring-framework/reference/web/webmvc/mvc-controller/ann-advice.html)

---

## Spring Security (Examples 56-60)

### Example 56: Basic Security Configuration (Coverage: 84.0%)

Demonstrates securing endpoints with Spring Security.

**Java Implementation**:

```java
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.web.SecurityFilterChain;

@Configuration
public class SecurityConfig {
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
            .authorizeHttpRequests(auth -> auth
                .requestMatchers("/public/**").permitAll()
                // => Public endpoints (no auth required)

                .requestMatchers("/api/**").authenticated()
                // => API endpoints require authentication

                .anyRequest().denyAll()
                // => Deny all other requests
            )
            .httpBasic();  // => HTTP Basic authentication

        return http.build();  // => Builds security filter chain
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.security.config.annotation.web.builders.HttpSecurity
import org.springframework.security.web.SecurityFilterChain

@Configuration
class SecurityConfig {
    @Bean
    fun filterChain(http: HttpSecurity): SecurityFilterChain {
        http
            .authorizeHttpRequests { auth ->
                auth
                    .requestMatchers("/public/**").permitAll()
                    // => Public endpoints (no auth required)

                    .requestMatchers("/api/**").authenticated()
                    // => API endpoints require authentication

                    .anyRequest().denyAll()
                    // => Deny all other requests
            }
            .httpBasic { }  // => HTTP Basic authentication

        return http.build()  // => Builds security filter chain
    }
}
```

**Key Takeaways**:

- SecurityFilterChain configures security rules
- authorizeHttpRequests() defines URL patterns
- permitAll(), authenticated(), denyAll() control access
- httpBasic() enables basic authentication

**Related Documentation**:

- [Security Configuration Documentation](https://docs.spring.io/spring-security/reference/servlet/configuration/java.html)

---

### Example 57: Method Security (Coverage: 85.5%)

Demonstrates securing methods with annotations.

**Java Implementation**:

```java
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Service;

@Service
public class SecureService {
    @PreAuthorize("hasRole('ADMIN')")
    // => Only users with ADMIN role can access
    // => Throws AccessDeniedException if unauthorized
    public void adminOperation() {
        System.out.println("Admin operation executed");
    }

    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    // => Users with USER OR ADMIN role allowed
    public void userOperation() {
        System.out.println("User operation executed");
    }

    @PreAuthorize("#username == authentication.name")
    // => SpEL expression: user can only access own data
    // => #username param must match authenticated user
    public void accessOwnData(String username) {
        System.out.println("Accessing data for: " + username);
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.security.access.prepost.PreAuthorize
import org.springframework.stereotype.Service

@Service
class SecureService {
    @PreAuthorize("hasRole('ADMIN')")
    // => Only users with ADMIN role can access
    // => Throws AccessDeniedException if unauthorized
    fun adminOperation() {
        println("Admin operation executed")
    }

    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    // => Users with USER OR ADMIN role allowed
    fun userOperation() {
        println("User operation executed")
    }

    @PreAuthorize("#username == authentication.name")
    // => SpEL expression: user can only access own data
    // => #username param must match authenticated user
    fun accessOwnData(username: String) {
        println("Accessing data for: $username")
    }
}
```

**Key Takeaways**:

- @PreAuthorize before method execution
- SpEL expressions for complex rules
- hasRole(), hasAnyRole() for role checks
- Access method parameters in expressions

**Related Documentation**:

- [Method Security Documentation](https://docs.spring.io/spring-security/reference/servlet/authorization/method-security.html)

---

### Example 58: Custom UserDetailsService (Coverage: 87.0%)

Demonstrates custom user authentication.

**Java Implementation**:

```java
import org.springframework.security.core.userdetails.*;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Service;
import java.util.List;

@Service
public class CustomUserDetailsService implements UserDetailsService {
    @Override
    public UserDetails loadUserByUsername(String username)
        throws UsernameNotFoundException {
        // => Called during authentication
        // => Load user from database

        if ("admin".equals(username)) {
            return User.builder()
                .username("admin")
                .password("{noop}password")  // => {noop} = no password encoding
                .authorities(List.of(
                    new SimpleGrantedAuthority("ROLE_ADMIN")
                    // => Grants ADMIN role
                ))
                .build();
            // => Returns UserDetails for Spring Security
        }

        throw new UsernameNotFoundException("User not found: " + username);
        // => Authentication fails
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.security.core.userdetails.*
import org.springframework.security.core.authority.SimpleGrantedAuthority
import org.springframework.stereotype.Service

@Service
class CustomUserDetailsService : UserDetailsService {
    override fun loadUserByUsername(username: String): UserDetails {
        // => Called during authentication
        // => Load user from database

        return if (username == "admin") {
            User.builder()
                .username("admin")
                .password("{noop}password")  // => {noop} = no password encoding
                .authorities(listOf(
                    SimpleGrantedAuthority("ROLE_ADMIN")
                    // => Grants ADMIN role
                ))
                .build()
            // => Returns UserDetails for Spring Security
        } else {
            throw UsernameNotFoundException("User not found: $username")
            // => Authentication fails
        }
    }
}
```

**Key Takeaways**:

- UserDetailsService loads user data
- Called automatically during authentication
- Return UserDetails with username, password, authorities
- UsernameNotFoundException for unknown users

**Related Documentation**:

- [UserDetailsService Documentation](https://docs.spring.io/spring-security/reference/servlet/authentication/passwords/user-details-service.html)

---

### Example 59: JWT Token Authentication (Coverage: 88.5%)

Demonstrates JWT-based authentication (simplified).

**Java Implementation**:

```java
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import org.springframework.stereotype.Service;
import java.util.Date;

@Service
public class JwtService {
    private static final String SECRET = "mySecretKey";
    // => Secret key for signing tokens
    // => In production: use strong secret, store securely

    public String generateToken(String username) {
        return Jwts.builder()
            .setSubject(username)  // => Token subject (username)
            .setIssuedAt(new Date())  // => Issue time
            .setExpiration(new Date(System.currentTimeMillis() + 86400000))
            // => Expiration: 24 hours from now
            .signWith(SignatureAlgorithm.HS256, SECRET)
            // => Sign with HMAC SHA-256
            .compact();
        // => Returns JWT string
    }

    public String extractUsername(String token) {
        return Jwts.parser()
            .setSigningKey(SECRET)  // => Verify signature
            .parseClaimsJws(token)  // => Parse and validate
            .getBody()
            .getSubject();  // => Extract username from subject claim
    }
}
```

**Kotlin Implementation**:

```kotlin
import io.jsonwebtoken.Jwts
import io.jsonwebtoken.SignatureAlgorithm
import org.springframework.stereotype.Service
import java.util.Date

@Service
class JwtService {
    companion object {
        private const val SECRET = "mySecretKey"
        // => Secret key for signing tokens
        // => In production: use strong secret, store securely
    }

    fun generateToken(username: String): String {
        return Jwts.builder()
            .setSubject(username)  // => Token subject (username)
            .setIssuedAt(Date())  // => Issue time
            .setExpiration(Date(System.currentTimeMillis() + 86400000))
            // => Expiration: 24 hours from now
            .signWith(SignatureAlgorithm.HS256, SECRET)
            // => Sign with HMAC SHA-256
            .compact()
        // => Returns JWT string
    }

    fun extractUsername(token: String): String {
        return Jwts.parser()
            .setSigningKey(SECRET)  // => Verify signature
            .parseClaimsJws(token)  // => Parse and validate
            .body
            .subject  // => Extract username from subject claim
    }
}
```

**Key Takeaways**:

- JWT for stateless authentication
- Tokens contain claims (subject, expiration)
- Signed with secret key
- Requires JJWT library dependency

**Related Documentation**:

- [JWT Token Authentication Documentation](https://docs.spring.io/spring-security/reference/servlet/oauth2/resource-server/jwt.html)

---

### Example 60: Password Encoding (Coverage: 90.0%)

Demonstrates secure password storage.

**Java Implementation**:

```java
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

@Configuration
public class PasswordConfig {
    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
        // => BCrypt hashing algorithm
        // => Slow by design (protects against brute force)
    }
}

@Service
class PasswordService {
    private final PasswordEncoder encoder;

    public PasswordService(PasswordEncoder encoder) {
        this.encoder = encoder;
    }

    public String encodePassword(String raw) {
        String encoded = encoder.encode(raw);
        // => Hashes password with random salt
        // => Same input produces different hash each time

        System.out.println("Raw: " + raw);
        System.out.println("Encoded: " + encoded);
        return encoded;
    }

    public boolean matches(String raw, String encoded) {
        return encoder.matches(raw, encoded);
        // => Verifies password against hash
        // => Returns true if match
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder
import org.springframework.security.crypto.password.PasswordEncoder
import org.springframework.stereotype.Service

@Configuration
class PasswordConfig {
    @Bean
    fun passwordEncoder(): PasswordEncoder {
        return BCryptPasswordEncoder()
        // => BCrypt hashing algorithm
        // => Slow by design (protects against brute force)
    }
}

@Service
class PasswordService(private val encoder: PasswordEncoder) {
    fun encodePassword(raw: String): String {
        val encoded = encoder.encode(raw)
        // => Hashes password with random salt
        // => Same input produces different hash each time

        println("Raw: $raw")
        println("Encoded: $encoded")
        return encoded
    }

    fun matches(raw: String, encoded: String): Boolean {
        return encoder.matches(raw, encoded)
        // => Verifies password against hash
        // => Returns true if match
    }
}
```

**Key Takeaways**:

- Never store plain passwords
- BCrypt recommended for password hashing
- Random salt prevents rainbow table attacks
- matches() for verification

**Related Documentation**:

- [Password Encoding Documentation](https://docs.spring.io/spring-security/reference/features/authentication/password-storage.html)

---

## Advanced Patterns (Examples 61-65)

### Example 61: Spring Cache Abstraction (Coverage: 91.5%)

Demonstrates method-level caching.

**Java Implementation**:

```java
import org.springframework.cache.annotation.*;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Service;

@Configuration
@EnableCaching  // => Enables Spring caching support
public class CacheConfig {
}

@Service
@CacheConfig(cacheNames = "donations")
// => Default cache name for this class
class DonationCacheService {
    @Cacheable  // => Result cached after first call
                // => Subsequent calls return cached value
    public String getDonation(Long id) {
        System.out.println("Loading from database: " + id);
        // => Only printed on cache miss
        return "Donation #" + id;
    }

    @CachePut(key = "#id")
    // => Updates cache with new value
    // => Method always executed
    public String updateDonation(Long id, String data) {
        System.out.println("Updating: " + id);
        return data;  // => Cached
    }

    @CacheEvict(key = "#id")
    // => Removes from cache
    // => Next getDonation() will reload
    public void deleteDonation(Long id) {
        System.out.println("Deleting: " + id);
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.cache.annotation.*
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Service

@Configuration
@EnableCaching  // => Enables Spring caching support
class CacheConfig

@Service
@CacheConfig(cacheNames = ["donations"])
// => Default cache name for this class
class DonationCacheService {
    @Cacheable  // => Result cached after first call
                // => Subsequent calls return cached value
    fun getDonation(id: Long): String {
        println("Loading from database: $id")
        // => Only printed on cache miss
        return "Donation #$id"
    }

    @CachePut(key = "#id")
    // => Updates cache with new value
    // => Method always executed
    fun updateDonation(id: Long, data: String): String {
        println("Updating: $id")
        return data  // => Cached
    }

    @CacheEvict(key = "#id")
    // => Removes from cache
    // => Next getDonation() will reload
    fun deleteDonation(id: Long) {
        println("Deleting: $id")
    }
}
```

**Key Takeaways**:

- @Cacheable caches method results
- @CachePut updates cache
- @CacheEvict removes from cache
- @EnableCaching required

**Related Documentation**:

- [Spring Cache Abstraction Documentation](https://docs.spring.io/spring-framework/reference/integration/cache.html)

---

### Example 62: Async Method Execution (Coverage: 93.0%)

Demonstrates asynchronous processing with @Async.

**Java Implementation**:

```java
import org.springframework.scheduling.annotation.*;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Service;
import java.util.concurrent.CompletableFuture;

@Configuration
@EnableAsync  // => Enables @Async support
public class AsyncConfig {
}

@Service
class NotificationService {
    @Async  // => Method runs in separate thread
            // => Caller doesn't wait for completion
    public void sendEmail(String to, String message) {
        System.out.println("Sending email to: " + to);
        // => Runs asynchronously
        try {
            Thread.sleep(2000);  // => Simulates slow operation
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        System.out.println("Email sent to: " + to);
    }

    @Async
    public CompletableFuture<String> processAsync(String data) {
        // => Returns CompletableFuture for async result
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        return CompletableFuture.completedFuture("Processed: " + data);
        // => Caller can wait for result if needed
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.scheduling.annotation.*
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Service
import java.util.concurrent.CompletableFuture

@Configuration
@EnableAsync  // => Enables @Async support
class AsyncConfig

@Service
class NotificationService {
    @Async  // => Method runs in separate thread
            // => Caller doesn't wait for completion
    fun sendEmail(to: String, message: String) {
        println("Sending email to: $to")
        // => Runs asynchronously
        Thread.sleep(2000)  // => Simulates slow operation
        println("Email sent to: $to")
    }

    @Async
    fun processAsync(data: String): CompletableFuture<String> {
        // => Returns CompletableFuture for async result
        Thread.sleep(1000)

        return CompletableFuture.completedFuture("Processed: $data")
        // => Caller can wait for result if needed
    }
}
```

**Key Takeaways**:

- @Async executes method in separate thread
- void methods fire-and-forget
- CompletableFuture for async results
- @EnableAsync required

**Related Documentation**:

- [Async Method Execution Documentation](https://docs.spring.io/spring-framework/reference/integration/scheduling.html#scheduling-annotation-support-async)

---

### Example 63: Application Events (Coverage: 94.5%)

Demonstrates event-driven architecture.

**Java Implementation**:

```java
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

class DonationEvent extends ApplicationEvent {
    // => Custom event class
    private final String donor;
    private final double amount;

    public DonationEvent(Object source, String donor, double amount) {
        super(source);  // => Event source
        this.donor = donor;
        this.amount = amount;
    }

    public String getDonor() { return donor; }
    public double getAmount() { return amount; }
}

@Service
class DonationService {
    private final ApplicationEventPublisher publisher;

    public DonationService(ApplicationEventPublisher publisher) {
        this.publisher = publisher;  // => Injected event publisher
    }

    public void createDonation(String donor, double amount) {
        System.out.println("Creating donation");

        publisher.publishEvent(new DonationEvent(this, donor, amount));
        // => Publishes event to all listeners
        // => Synchronous by default
    }
}

@Component
class DonationEventListener {
    @EventListener  // => Listens for DonationEvent
    public void handleDonation(DonationEvent event) {
        // => Called when event published
        System.out.println("Event received: " + event.getDonor() +
                           " donated $" + event.getAmount());
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.ApplicationEvent
import org.springframework.context.ApplicationEventPublisher
import org.springframework.context.event.EventListener
import org.springframework.stereotype.Component
import org.springframework.stereotype.Service

class DonationEvent(
    source: Any,
    val donor: String,
    val amount: Double
) : ApplicationEvent(source)  // => Custom event class

@Service
class DonationService(
    private val publisher: ApplicationEventPublisher
    // => Injected event publisher
) {
    fun createDonation(donor: String, amount: Double) {
        println("Creating donation")

        publisher.publishEvent(DonationEvent(this, donor, amount))
        // => Publishes event to all listeners
        // => Synchronous by default
    }
}

@Component
class DonationEventListener {
    @EventListener  // => Listens for DonationEvent
    fun handleDonation(event: DonationEvent) {
        // => Called when event published
        println("Event received: ${event.donor} donated $${event.amount}")
    }
}
```

**Key Takeaways**:

- ApplicationEvent for custom events
- ApplicationEventPublisher to publish
- @EventListener to handle events
- Decouples components

**Related Documentation**:

- [Application Events Documentation](https://docs.spring.io/spring-framework/reference/core/beans/context-introduction.html#context-functionality-events)

---

### Example 64: Scheduled Tasks (Coverage: 96.0%)

Demonstrates scheduled background tasks.

**Java Implementation**:

```java
import org.springframework.scheduling.annotation.*;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

@Configuration
@EnableScheduling  // => Enables @Scheduled support
public class SchedulingConfig {
}

@Component
class ScheduledTasks {
    @Scheduled(fixedRate = 5000)
    // => Runs every 5 seconds
    // => Fixed delay between start of executions
    public void reportStatus() {
        System.out.println("Status check at: " + System.currentTimeMillis());
    }

    @Scheduled(cron = "0 0 * * * *")
    // => Cron expression: every hour at minute 0
    // => Format: second minute hour day month weekday
    public void hourlyTask() {
        System.out.println("Hourly task executed");
    }

    @Scheduled(fixedDelay = 3000, initialDelay = 10000)
    // => Waits 10 seconds before first execution
    // => Then runs every 3 seconds after previous completion
    public void delayedTask() {
        System.out.println("Delayed task running");
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.scheduling.annotation.*
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Component

@Configuration
@EnableScheduling  // => Enables @Scheduled support
class SchedulingConfig

@Component
class ScheduledTasks {
    @Scheduled(fixedRate = 5000)
    // => Runs every 5 seconds
    // => Fixed delay between start of executions
    fun reportStatus() {
        println("Status check at: ${System.currentTimeMillis()}")
    }

    @Scheduled(cron = "0 0 * * * *")
    // => Cron expression: every hour at minute 0
    // => Format: second minute hour day month weekday
    fun hourlyTask() {
        println("Hourly task executed")
    }

    @Scheduled(fixedDelay = 3000, initialDelay = 10000)
    // => Waits 10 seconds before first execution
    // => Then runs every 3 seconds after previous completion
    fun delayedTask() {
        println("Delayed task running")
    }
}
```

**Key Takeaways**:

- @Scheduled for periodic tasks
- fixedRate for fixed intervals
- cron for complex schedules
- @EnableScheduling required

**Related Documentation**:

- [Scheduled Tasks Documentation](https://docs.spring.io/spring-framework/reference/integration/scheduling.html#scheduling-annotation-support-scheduled)

---

### Example 65: Custom Conditional Bean Registration (Coverage: 97.5%)

Demonstrates advanced conditional bean creation.

**Java Implementation**:

```java
import org.springframework.boot.autoconfigure.condition.*;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
class ConditionalConfig {
    @Bean
    @ConditionalOnProperty(
        name = "feature.cache.enabled",
        havingValue = "true"
    )
    // => Bean created only if property = true
    public CacheService cacheService() {
        return new CacheService();
        // => Registered when cache enabled
    }

    @Bean
    @ConditionalOnMissingBean(CacheService.class)
    // => Created only if CacheService bean doesn't exist
    public NoCacheService noCacheService() {
        return new NoCacheService();
        // => Fallback when cache disabled
    }

    @Bean
    @ConditionalOnClass(name = "com.mysql.jdbc.Driver")
    // => Created only if MySQL driver on classpath
    public DatabaseService mysqlService() {
        return new DatabaseService("MySQL");
    }
}

class CacheService {
    public CacheService() {
        System.out.println("CacheService created");
    }
}

class NoCacheService {
    public NoCacheService() {
        System.out.println("NoCacheService created");
    }
}

class DatabaseService {
    public DatabaseService(String type) {
        System.out.println("DatabaseService: " + type);
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.boot.autoconfigure.condition.*
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

@Configuration
class ConditionalConfig {
    @Bean
    @ConditionalOnProperty(
        name = ["feature.cache.enabled"],
        havingValue = "true"
    )
    // => Bean created only if property = true
    fun cacheService(): CacheService {
        return CacheService()
        // => Registered when cache enabled
    }

    @Bean
    @ConditionalOnMissingBean(CacheService::class)
    // => Created only if CacheService bean doesn't exist
    fun noCacheService(): NoCacheService {
        return NoCacheService()
        // => Fallback when cache disabled
    }

    @Bean
    @ConditionalOnClass(name = ["com.mysql.jdbc.Driver"])
    // => Created only if MySQL driver on classpath
    fun mysqlService(): DatabaseService {
        return DatabaseService("MySQL")
    }
}

class CacheService {
    init {
        println("CacheService created")
    }
}

class NoCacheService {
    init {
        println("NoCacheService created")
    }
}

class DatabaseService(type: String) {
    init {
        println("DatabaseService: $type")
    }
}
```

**Key Takeaways**:

- @ConditionalOnProperty for property-based registration
- @ConditionalOnMissingBean for fallbacks
- @ConditionalOnClass for classpath checks
- Enables flexible auto-configuration

**Related Documentation**:

- [Conditional Bean Registration Documentation](https://docs.spring.io/spring-framework/reference/core/beans/java/bean-annotation.html#beans-java-conditional)

---

## Testing (Examples 66-70)

### Example 66: Spring TestContext Framework (Coverage: 99.0%)

Demonstrates integration testing with Spring context.

**Java Implementation**:

```java
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.stereotype.Service;
import static org.junit.jupiter.api.Assertions.*;

@Service
class CalculatorService {
    public int add(int a, int b) {
        return a + b;
    }
}

@SpringBootTest  // => Loads full Spring application context
                 // => All beans available for testing
class CalculatorServiceTest {
    @Autowired  // => Injects actual bean from context
    private CalculatorService calculator;

    @Test
    void testAddition() {
        // => Test with real Spring-managed bean
        int result = calculator.add(5, 3);

        assertEquals(8, result);
        // => Assertion: expected 8
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.stereotype.Service
import kotlin.test.assertEquals

@Service
class CalculatorService {
    fun add(a: Int, b: Int): Int = a + b
}

@SpringBootTest  // => Loads full Spring application context
                 // => All beans available for testing
class CalculatorServiceTest {
    @Autowired  // => Injects actual bean from context
    private lateinit var calculator: CalculatorService

    @Test
    fun testAddition() {
        // => Test with real Spring-managed bean
        val result = calculator.add(5, 3)

        assertEquals(8, result)
        // => Assertion: expected 8
    }
}
```

**Key Takeaways**:

- @SpringBootTest loads application context
- @Autowired injects beans into tests
- Full integration testing
- Slower than unit tests

**Related Documentation**:

- [TestContext Framework Documentation](https://docs.spring.io/spring-framework/reference/testing/testcontext-framework.html)

---

### Example 67: MockMvc for Web Layer Testing (Coverage: 100.0%)

Demonstrates testing Spring MVC controllers.

**Java Implementation**:

```java
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.bind.annotation.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@RestController
class GreetingController {
    @GetMapping("/greet")
    public String greet(@RequestParam String name) {
        return "Hello, " + name;
    }
}

@WebMvcTest(GreetingController.class)
// => Loads only web layer (controllers)
// => Faster than @SpringBootTest
class GreetingControllerTest {
    @Autowired
    private MockMvc mockMvc;  // => Mock HTTP client

    @Test
    void testGreeting() throws Exception {
        mockMvc.perform(get("/greet").param("name", "Ali"))
            // => Performs GET /greet?name=Ali
            .andExpect(status().isOk())
            // => Expects HTTP 200
            .andExpect(content().string("Hello, Ali"));
            // => Expects response body
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest
import org.springframework.test.web.servlet.MockMvc
import org.springframework.web.bind.annotation.*
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*
import org.springframework.test.web.servlet.result.MockMvcResultMatchers.*

@RestController
class GreetingController {
    @GetMapping("/greet")
    fun greet(@RequestParam name: String): String {
        return "Hello, $name"
    }
}

@WebMvcTest(GreetingController::class)
// => Loads only web layer (controllers)
// => Faster than @SpringBootTest
class GreetingControllerTest {
    @Autowired
    private lateinit var mockMvc: MockMvc  // => Mock HTTP client

    @Test
    fun testGreeting() {
        mockMvc.perform(get("/greet").param("name", "Ali"))
            // => Performs GET /greet?name=Ali
            .andExpect(status().isOk)
            // => Expects HTTP 200
            .andExpect(content().string("Hello, Ali"))
            // => Expects response body
    }
}
```

**Key Takeaways**:

- @WebMvcTest for controller tests
- MockMvc simulates HTTP requests
- Test request/response without server
- Faster than full integration tests

**Related Documentation**:

- [MockMvc Documentation](https://docs.spring.io/spring-framework/reference/testing/spring-mvc-test-framework.html)

---

### Example 68: @Transactional in Tests (Coverage: 100.0%)

Demonstrates transactional test rollback.

**Java Implementation**:

```java
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.annotation.Transactional;
import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
@Transactional  // => Each test runs in transaction
                // => Automatically rolled back after test
class DatabaseTest {
    @Autowired
    private JdbcTemplate jdbc;

    @Test
    void testInsert() {
        jdbc.update("INSERT INTO donations (donor, amount) VALUES (?, ?)",
                    "TestUser", 100.0);
        // => Inserts data

        Integer count = jdbc.queryForObject(
            "SELECT COUNT(*) FROM donations WHERE donor = ?",
            Integer.class,
            "TestUser"
        );

        assertEquals(1, count);
        // => Assertion passes

        // => Transaction rolled back after test
        // => Database unchanged
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.transaction.annotation.Transactional
import kotlin.test.assertEquals

@SpringBootTest
@Transactional  // => Each test runs in transaction
                // => Automatically rolled back after test
class DatabaseTest {
    @Autowired
    private lateinit var jdbc: JdbcTemplate

    @Test
    fun testInsert() {
        jdbc.update("INSERT INTO donations (donor, amount) VALUES (?, ?)",
                    "TestUser", 100.0)
        // => Inserts data

        val count = jdbc.queryForObject(
            "SELECT COUNT(*) FROM donations WHERE donor = ?",
            Int::class.java,
            "TestUser"
        )

        assertEquals(1, count)
        // => Assertion passes

        // => Transaction rolled back after test
        // => Database unchanged
    }
}
```

**Key Takeaways**:

- @Transactional on test class
- Automatic rollback after each test
- Database remains clean
- Enables repeatable tests

**Related Documentation**:

- [Transactional Tests Documentation](https://docs.spring.io/spring-framework/reference/testing/testcontext-framework/tx.html)

---

### Example 69: Test Profiles (Coverage: 100.0%)

Demonstrates test-specific configuration.

**Java Implementation**:

```java
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import static org.junit.jupiter.api.Assertions.*;

// application-test.properties:
// test.value=test-data

@SpringBootTest
@ActiveProfiles("test")  // => Activates "test" profile
                         // => Loads application-test.properties
class ProfileTest {
    @Value("${test.value}")
    private String testValue;  // => Reads from test profile

    @Test
    void testProfileValue() {
        assertEquals("test-data", testValue);
        // => Uses test-specific configuration
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Value
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.context.ActiveProfiles
import kotlin.test.assertEquals

// application-test.properties:
// test.value=test-data

@SpringBootTest
@ActiveProfiles("test")  // => Activates "test" profile
                         // => Loads application-test.properties
class ProfileTest {
    @Value("\${test.value}")
    private lateinit var testValue: String  // => Reads from test profile

    @Test
    fun testProfileValue() {
        assertEquals("test-data", testValue)
        // => Uses test-specific configuration
    }
}
```

**Key Takeaways**:

- @ActiveProfiles for test configuration
- application-test.properties for test data
- Separate test/production config
- H2 in-memory DB common for tests

**Related Documentation**:

- [Test Profiles Documentation](https://docs.spring.io/spring-framework/reference/testing/testcontext-framework/ctx-management/env-profiles.html)

---

### Example 70: Mocking with @MockBean (Coverage: 100.0%)

Demonstrates mocking dependencies in tests.

**Java Implementation**:

```java
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.stereotype.Service;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

@Service
class DataService {
    public String fetchData() {
        return "real data";
    }
}

@Service
class BusinessService {
    private final DataService dataService;

    public BusinessService(DataService dataService) {
        this.dataService = dataService;
    }

    public String process() {
        return "Processed: " + dataService.fetchData();
    }
}

@SpringBootTest
class MockBeanTest {
    @MockBean  // => Replaces real bean with mock
    private DataService dataService;

    @Autowired  // => Injects BusinessService with mocked dependency
    private BusinessService businessService;

    @Test
    void testWithMock() {
        when(dataService.fetchData()).thenReturn("mock data");
        // => Configures mock behavior

        String result = businessService.process();

        assertEquals("Processed: mock data", result);
        // => Uses mock instead of real service
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.boot.test.mock.mockito.MockBean
import org.springframework.stereotype.Service
import org.mockito.Mockito.*
import kotlin.test.assertEquals

@Service
class DataService {
    fun fetchData(): String = "real data"
}

@Service
class BusinessService(private val dataService: DataService) {
    fun process(): String = "Processed: ${dataService.fetchData()}"
}

@SpringBootTest
class MockBeanTest {
    @MockBean  // => Replaces real bean with mock
    private lateinit var dataService: DataService

    @Autowired  // => Injects BusinessService with mocked dependency
    private lateinit var businessService: BusinessService

    @Test
    fun testWithMock() {
        `when`(dataService.fetchData()).thenReturn("mock data")
        // => Configures mock behavior

        val result = businessService.process()

        assertEquals("Processed: mock data", result)
        // => Uses mock instead of real service
    }
}
```

**Key Takeaways**:

- @MockBean replaces bean with mock
- Mockito for behavior configuration
- Test without external dependencies
- Fast, isolated unit tests

**Related Documentation**:

- [MockBean Documentation](https://docs.spring.io/spring-boot/api/java/org/springframework/boot/test/mock/mockito/MockBean.html)

---

## Production Patterns (Examples 71-75)

### Example 71: Custom Health Indicator (Coverage: 100.0%)

Demonstrates application health monitoring.

**Java Implementation**:

```java
import org.springframework.boot.actuate.health.*;
import org.springframework.stereotype.Component;

@Component
class CustomHealthIndicator implements HealthIndicator {
    @Override
    public Health health() {
        // => Called by /actuator/health endpoint
        // => Returns application health status

        boolean healthy = checkSystemHealth();

        if (healthy) {
            return Health.up()
                .withDetail("message", "System healthy")
                .withDetail("uptime", getUptime())
                .build();
            // => Status: UP
        } else {
            return Health.down()
                .withDetail("error", "System degraded")
                .build();
            // => Status: DOWN
        }
    }

    private boolean checkSystemHealth() {
        return true;  // => Actual health check logic
    }

    private long getUptime() {
        return System.currentTimeMillis();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.boot.actuate.health.*
import org.springframework.stereotype.Component

@Component
class CustomHealthIndicator : HealthIndicator {
    override fun health(): Health {
        // => Called by /actuator/health endpoint
        // => Returns application health status

        val healthy = checkSystemHealth()

        return if (healthy) {
            Health.up()
                .withDetail("message", "System healthy")
                .withDetail("uptime", getUptime())
                .build()
            // => Status: UP
        } else {
            Health.down()
                .withDetail("error", "System degraded")
                .build()
            // => Status: DOWN
        }
    }

    private fun checkSystemHealth(): Boolean = true  // => Actual health check logic

    private fun getUptime(): Long = System.currentTimeMillis()
}
```

**Key Takeaways**:

- HealthIndicator for custom health checks
- Exposed via Spring Boot Actuator
- Health.up() or Health.down() status
- Include diagnostic details

**Related Documentation**:

- [Custom Health Indicator Documentation](https://docs.spring.io/spring-boot/reference/actuator/endpoints.html#actuator.endpoints.health)

---

### Example 72: Custom Metrics (Coverage: 100.0%)

Demonstrates application metrics tracking.

**Java Implementation**:

```java
import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.MeterRegistry;
import org.springframework.stereotype.Service;

@Service
class DonationMetricsService {
    private final Counter donationCounter;

    public DonationMetricsService(MeterRegistry registry) {
        // => MeterRegistry auto-configured by Spring Boot

        this.donationCounter = Counter.builder("donations.total")
            // => Metric name
            .tag("type", "charity")
            // => Tag for filtering/grouping
            .description("Total donations received")
            // => Metric description
            .register(registry);
        // => Registers metric
    }

    public void recordDonation(double amount) {
        donationCounter.increment();
        // => Increments counter
        // => Exposed at /actuator/metrics/donations.total

        System.out.println("Donation recorded: $" + amount);
    }
}
```

**Kotlin Implementation**:

```kotlin
import io.micrometer.core.instrument.Counter
import io.micrometer.core.instrument.MeterRegistry
import org.springframework.stereotype.Service

@Service
class DonationMetricsService(registry: MeterRegistry) {
    // => MeterRegistry auto-configured by Spring Boot

    private val donationCounter: Counter = Counter.builder("donations.total")
        // => Metric name
        .tag("type", "charity")
        // => Tag for filtering/grouping
        .description("Total donations received")
        // => Metric description
        .register(registry)
    // => Registers metric

    fun recordDonation(amount: Double) {
        donationCounter.increment()
        // => Increments counter
        // => Exposed at /actuator/metrics/donations.total

        println("Donation recorded: $$amount")
    }
}
```

**Key Takeaways**:

- Micrometer for metrics
- Counter, Gauge, Timer available
- Tags for dimensional metrics
- Exposed via Actuator

**Related Documentation**:

- [Custom Metrics Documentation](https://docs.spring.io/spring-boot/reference/actuator/metrics.html#actuator.metrics.export.custom)

---

### Example 73: Request/Response Logging Interceptor (Coverage: 100.0%)

Demonstrates HTTP request interception.

**Java Implementation**:

```java
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.config.annotation.*;
import org.springframework.context.annotation.Configuration;
import javax.servlet.http.*;

@Component
class LoggingInterceptor implements HandlerInterceptor {
    @Override
    public boolean preHandle(
        HttpServletRequest request,
        HttpServletResponse response,
        Object handler
    ) {
        // => Called BEFORE controller method
        System.out.println("Request: " + request.getMethod() + " " +
                          request.getRequestURI());
        return true;  // => Continue to controller (false = abort)
    }

    @Override
    public void afterCompletion(
        HttpServletRequest request,
        HttpServletResponse response,
        Object handler,
        Exception ex
    ) {
        // => Called AFTER response sent
        System.out.println("Response status: " + response.getStatus());
    }
}

@Configuration
class WebConfig implements WebMvcConfigurer {
    private final LoggingInterceptor loggingInterceptor;

    public WebConfig(LoggingInterceptor loggingInterceptor) {
        this.loggingInterceptor = loggingInterceptor;
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(loggingInterceptor)
            .addPathPatterns("/api/**");
        // => Apply to /api/** paths only
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.stereotype.Component
import org.springframework.web.servlet.HandlerInterceptor
import org.springframework.web.servlet.config.annotation.*
import org.springframework.context.annotation.Configuration
import javax.servlet.http.*

@Component
class LoggingInterceptor : HandlerInterceptor {
    override fun preHandle(
        request: HttpServletRequest,
        response: HttpServletResponse,
        handler: Any
    ): Boolean {
        // => Called BEFORE controller method
        println("Request: ${request.method} ${request.requestURI}")
        return true  // => Continue to controller (false = abort)
    }

    override fun afterCompletion(
        request: HttpServletRequest,
        response: HttpServletResponse,
        handler: Any,
        ex: Exception?
    ) {
        // => Called AFTER response sent
        println("Response status: ${response.status}")
    }
}

@Configuration
class WebConfig(
    private val loggingInterceptor: LoggingInterceptor
) : WebMvcConfigurer {
    override fun addInterceptors(registry: InterceptorRegistry) {
        registry.addInterceptor(loggingInterceptor)
            .addPathPatterns("/api/**")
        // => Apply to /api/** paths only
    }
}
```

**Key Takeaways**:

- HandlerInterceptor for request/response interception
- preHandle() before controller
- afterCompletion() after response
- Configure via WebMvcConfigurer

**Related Documentation**:

- [Request/Response Logging Interceptor Documentation](https://docs.spring.io/spring-framework/reference/web/webmvc/mvc-config/interceptors.html)

---

### Example 74: Connection Pooling with HikariCP (Coverage: 100.0%)

Demonstrates database connection pooling configuration.

**Java Implementation**:

```java
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import javax.sql.DataSource;

@Configuration
class DataSourceConfig {
    @Bean
    public DataSource dataSource() {
        HikariConfig config = new HikariConfig();

        config.setJdbcUrl("jdbc:h2:mem:testdb");
        // => Database URL

        config.setUsername("sa");
        config.setPassword("");

        config.setMaximumPoolSize(10);
        // => Maximum 10 connections in pool

        config.setMinimumIdle(2);
        // => Keep at least 2 idle connections

        config.setConnectionTimeout(30000);
        // => Wait max 30 seconds for connection

        config.setIdleTimeout(600000);
        // => Close idle connections after 10 minutes

        config.setMaxLifetime(1800000);
        // => Recycle connections after 30 minutes

        return new HikariDataSource(config);
        // => Returns pooled DataSource
        // => Spring uses this for all database operations
    }
}
```

**Kotlin Implementation**:

```kotlin
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import javax.sql.DataSource

@Configuration
class DataSourceConfig {
    @Bean
    fun dataSource(): DataSource {
        val config = HikariConfig().apply {
            jdbcUrl = "jdbc:h2:mem:testdb"
            // => Database URL

            username = "sa"
            password = ""

            maximumPoolSize = 10
            // => Maximum 10 connections in pool

            minimumIdle = 2
            // => Keep at least 2 idle connections

            connectionTimeout = 30000
            // => Wait max 30 seconds for connection

            idleTimeout = 600000
            // => Close idle connections after 10 minutes

            maxLifetime = 1800000
            // => Recycle connections after 30 minutes
        }

        return HikariDataSource(config)
        // => Returns pooled DataSource
        // => Spring uses this for all database operations
    }
}
```

**Key Takeaways**:

- HikariCP default connection pool in Spring Boot
- Configure pool size, timeouts, lifecycle
- Connection reuse improves performance
- Spring Boot auto-configures if properties set

**Related Documentation**:

- [HikariCP Configuration Documentation](https://github.com/brettwooldridge/HikariCP#configuration-knobs-baby)

---

### Example 75: Custom Validation Annotation (Coverage: 100.0%)

Demonstrates creating custom validation constraint.

**Java Implementation**:

```java
import javax.validation.*;
import javax.validation.constraints.*;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.PARAMETER})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = ZakatAmountValidator.class)
// => Links annotation to validator implementation
@interface ValidZakatAmount {
    String message() default "Invalid zakat amount";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}

class ZakatAmountValidator implements ConstraintValidator<ValidZakatAmount, Double> {
    @Override
    public boolean isValid(Double value, ConstraintValidatorContext context) {
        // => Validation logic
        if (value == null) {
            return true;  // => Let @NotNull handle null check
        }

        return value >= 0 && value <= 100000;
        // => Amount must be 0-100000
    }
}

class DonationForm {
    @ValidZakatAmount(message = "Zakat amount must be 0-100000")
    // => Uses custom validator
    private Double amount;

    public Double getAmount() { return amount; }
    public void setAmount(Double amount) { this.amount = amount; }
}
```

**Kotlin Implementation**:

```kotlin
import javax.validation.*
import javax.validation.constraints.*

@Target(AnnotationTarget.FIELD, AnnotationTarget.VALUE_PARAMETER)
@Retention(AnnotationRetention.RUNTIME)
@Constraint(validatedBy = [ZakatAmountValidator::class])
// => Links annotation to validator implementation
annotation class ValidZakatAmount(
    val message: String = "Invalid zakat amount",
    val groups: Array<KClass<*>> = [],
    val payload: Array<KClass<out Payload>> = []
)

class ZakatAmountValidator : ConstraintValidator<ValidZakatAmount, Double> {
    override fun isValid(value: Double?, context: ConstraintValidatorContext): Boolean {
        // => Validation logic
        if (value == null) {
            return true  // => Let @NotNull handle null check
        }

        return value in 0.0..100000.0
        // => Amount must be 0-100000
    }
}

data class DonationForm(
    @field:ValidZakatAmount(message = "Zakat amount must be 0-100000")
    // => Uses custom validator
    val amount: Double?
)
```

**Key Takeaways**:

- Create custom validation annotations
- Implement ConstraintValidator interface
- Reusable across application
- Works with Bean Validation (@Valid)

**Related Documentation**:

- [Custom Validation Annotation Documentation](https://docs.spring.io/spring-framework/reference/core/validation/validator.html#validation-custom-constraints)

---

## Summary

This advanced tutorial covered **25 Spring Framework examples** (51-75) achieving 75-95% coverage:

**REST APIs (51-55)**:

- ResponseEntity, content negotiation, CORS, versioning, global exception handling

**Security (56-60)**:

- Security configuration, method security, UserDetailsService, JWT, password encoding

**Advanced Patterns (61-65)**:

- Caching, async execution, application events, scheduling, conditional beans

**Testing (66-70)**:

- TestContext, MockMvc, transactional tests, test profiles, mocking

**Production (71-75)**:

- Health indicators, metrics, interceptors, connection pooling, custom validation

**Congratulations!** You've completed the full Spring Framework by-example series covering 75 examples across beginner, intermediate, and advanced levels, achieving 95% framework coverage.

**Next Steps**:

- Build production applications with Spring Boot
- Explore Spring Cloud for microservices
- Study Spring Data JPA for ORM
- Learn Spring Security advanced features
- Practice with real-world Islamic finance applications
