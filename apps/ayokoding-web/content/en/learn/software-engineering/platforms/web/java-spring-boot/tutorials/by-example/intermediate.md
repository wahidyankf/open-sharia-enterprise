---
title: "Intermediate"
date: 2025-12-24T00:00:00+07:00
draft: false
weight: 10000000
description: "Intermediate Spring Boot through 30 examples: transactions, validation, security, caching, async processing, WebSocket, API versioning, and advanced patterns"
tags: ["spring-boot", "tutorial", "by-example", "intermediate", "spring-security", "transactions", "testing", "caching"]
categories: ["learn"]
---

Learn intermediate Spring Boot patterns through 30 annotated examples covering production-ready techniques: transactions, security, testing, caching, async processing, WebSocket, API versioning, and advanced architectural patterns.

## Prerequisites

- Completed [beginner by-example tutorial](/en/learn/software-engineering/platforms/web/java-spring-boot/tutorials/by-example/beginner)
- Spring Boot 3.4.x + Java 17
- Understanding of JPA, REST APIs, and dependency injection

## Group 1: Transactions & Data

### Example 21: @Transactional Basics

Spring's declarative transaction management ensures data consistency through ACID properties.

```java
// pom.xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter-data-jpa</artifactId>
</dependency>

// Domain model
@Entity
public class BankAccount {
  @Id @GeneratedValue
  private Long id;
  private String owner;
  private BigDecimal balance;
  // constructors, getters, setters
}

@Entity
public class TransferLog {
  @Id @GeneratedValue
  private Long id;
  private Long fromAccount;
  private Long toAccount;
  private BigDecimal amount;
  private LocalDateTime timestamp;
  // constructors, getters, setters
}

// Repository
public interface AccountRepository extends JpaRepository<BankAccount, Long> {}
public interface TransferLogRepository extends JpaRepository<TransferLog, Long> {}

// Service with transactions
@Service
public class TransferService {
  @Autowired private AccountRepository accountRepo;
  @Autowired private TransferLogRepository logRepo;

  @Transactional // Default: REQUIRED propagation, rollback on RuntimeException
  public void transfer(Long fromId, Long toId, BigDecimal amount) {
    BankAccount from = accountRepo.findById(fromId)
      .orElseThrow(() -> new IllegalArgumentException("Source not found"));
    BankAccount to = accountRepo.findById(toId)
      .orElseThrow(() -> new IllegalArgumentException("Destination not found"));

    if (from.getBalance().compareTo(amount) < 0) {
      throw new IllegalStateException("Insufficient funds"); // => Rollback entire transaction
    }

    from.setBalance(from.getBalance().subtract(amount));
    to.setBalance(to.getBalance().add(amount));
    accountRepo.save(from);
    accountRepo.save(to);

    // Log the transfer
    TransferLog log = new TransferLog();
    log.setFromAccount(fromId);
    log.setToAccount(toId);
    log.setAmount(amount);
    log.setTimestamp(LocalDateTime.now());
    logRepo.save(log);

    // If exception occurs here, ALL changes (both accounts + log) rollback
  }
}

// Controller
@RestController
@RequestMapping("/api/transfers")
public class TransferController {
  @Autowired private TransferService transferService;

  @PostMapping
  public ResponseEntity<String> transfer(
    @RequestParam Long fromId,
    @RequestParam Long toId,
    @RequestParam BigDecimal amount
  ) {
    try {
      transferService.transfer(fromId, toId, amount);
      return ResponseEntity.ok("Transfer successful");
    } catch (Exception e) {
      return ResponseEntity.badRequest().body(e.getMessage());
    }
  }
}
```

**Key Takeaway**: `@Transactional` ensures all-or-nothing execution—either all database changes commit or all rollback on exception.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
sequenceDiagram
    participant C as Controller
    participant S as TransferService
    participant DB as Database

    C->>S: transfer(fromId, toId, amount)
    activate S
    Note over S: @Transactional begins
    S->>DB: BEGIN TRANSACTION
    S->>DB: UPDATE account SET balance=... WHERE id=fromId
    S->>DB: UPDATE account SET balance=... WHERE id=toId
    S->>DB: INSERT INTO transfer_log...
    alt Success
        S->>DB: COMMIT
        S-->>C: Success
    else Exception
        S->>DB: ROLLBACK
        S-->>C: Error
    end
    deactivate S

    style C fill:#0173B2,stroke:#000,color:#fff
    style S fill:#029E73,stroke:#000,color:#fff
    style DB fill:#DE8F05,stroke:#000,color:#000
```

---

### Example 22: Isolation Levels

Transaction isolation controls visibility of concurrent changes.

```java
@Service
public class InventoryService {
  @Autowired private ProductRepository productRepo;

  // READ_COMMITTED: Prevents dirty reads
  @Transactional(isolation = Isolation.READ_COMMITTED)
  public int getStock(Long productId) {
    Product p = productRepo.findById(productId).orElseThrow();
    return p.getStock(); // => Sees only committed data from other transactions
  }

  // REPEATABLE_READ: Prevents non-repeatable reads
  @Transactional(isolation = Isolation.REPEATABLE_READ)
  public void processOrder(Long productId, int quantity) {
    Product p = productRepo.findById(productId).orElseThrow();
    int initialStock = p.getStock(); // => 100

    // Simulate delay
    Thread.sleep(1000);

    // Even if another transaction updates stock, this transaction still sees 100
    int currentStock = productRepo.findById(productId).get().getStock(); // => Still 100

    if (currentStock >= quantity) {
      p.setStock(currentStock - quantity);
      productRepo.save(p);
    }
  }

  // SERIALIZABLE: Strictest isolation (rarely needed)
  @Transactional(isolation = Isolation.SERIALIZABLE)
  public void criticalOperation(Long productId) {
    // Locks prevent concurrent access—transactions execute serially
  }
}
```

**Key Takeaway**: Higher isolation levels prevent concurrency issues but reduce throughput—choose based on consistency requirements.

---

### Example 23: Optimistic Locking

Prevent lost updates with version-based concurrency control.

```java
@Entity
public class Product {
  @Id @GeneratedValue
  private Long id;
  private String name;
  private int stock;

  @Version // Optimistic locking version field
  private Long version;

  // constructors, getters, setters
}

@Service
public class StockService {
  @Autowired private ProductRepository productRepo;

  @Transactional
  public void decreaseStock(Long productId, int quantity) {
    Product product = productRepo.findById(productId).orElseThrow();
    // version = 1

    product.setStock(product.getStock() - quantity);
    productRepo.save(product);
    // SQL: UPDATE product SET stock=?, version=2 WHERE id=? AND version=1

    // If another transaction updated product (version=2), this fails with OptimisticLockException
  }

  // Retry logic for conflicts
  @Transactional
  public void decreaseStockWithRetry(Long productId, int quantity) {
    int maxRetries = 3;
    for (int i = 0; i < maxRetries; i++) {
      try {
        Product product = productRepo.findById(productId).orElseThrow();
        product.setStock(product.getStock() - quantity);
        productRepo.save(product);
        return; // Success
      } catch (OptimisticLockException e) {
        if (i == maxRetries - 1) throw e; // Retries exhausted
        // Retry with fresh data
      }
    }
  }
}
```

**Key Takeaway**: `@Version` prevents lost updates by failing conflicting transactions—use retry logic for conflict resolution.

---

### Example 24: Batch Operations

Optimize bulk database operations with batching.

```java
// application.properties
spring.jpa.properties.hibernate.jdbc.batch_size=50
spring.jpa.properties.hibernate.order_inserts=true
spring.jpa.properties.hibernate.order_updates=true

@Service
public class BulkImportService {
  @Autowired private ProductRepository productRepo;
  @Autowired private EntityManager entityManager;

  // Inefficient: N+1 queries
  @Transactional
  public void importProductsSlow(List<Product> products) {
    for (Product p : products) {
      productRepo.save(p); // => 1000 products = 1000 INSERT statements
    }
  }

  // Better: Batch inserts
  @Transactional
  public void importProductsFast(List<Product> products) {
    productRepo.saveAll(products); // => Batches 1000 products into 20 INSERTs (50 per batch)
  }

  // Best: Manual batch flushing for large datasets
  @Transactional
  public void importProductsOptimal(List<Product> products) {
    int batchSize = 50;
    for (int i = 0; i < products.size(); i++) {
      entityManager.persist(products.get(i));
      if (i % batchSize == 0 && i > 0) {
        entityManager.flush(); // Force batch execution
        entityManager.clear(); // Free memory
      }
    }
  }

  // Bulk update with JPQL
  @Transactional
  public void discountAllProducts(BigDecimal discountPercent) {
    int updated = entityManager.createQuery(
      "UPDATE Product p SET p.price = p.price * :factor"
    ).setParameter("factor", BigDecimal.ONE.subtract(discountPercent))
     .executeUpdate(); // => Single UPDATE statement for all rows

    System.out.println("Updated " + updated + " products");
  }
}
```

**Key Takeaway**: Batch operations reduce database round-trips—use `saveAll()` for small batches, manual flushing for large datasets.

---

## Group 2: Spring Security

### Example 25: Security Auto-Configuration

Spring Boot auto-configures basic security by default.

```java
// pom.xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter-security</artifactId>
</dependency>

// With just the dependency, Spring Boot:
// 1. Generates random password on startup (console log)
// 2. Secures all endpoints with HTTP Basic Auth
// 3. Default username: "user"
// 4. CSRF protection enabled
// 5. Session management configured

@RestController
public class SecuredController {
  @GetMapping("/public")
  public String publicEndpoint() {
    return "Accessible without auth"; // => Still requires login by default!
  }

  @GetMapping("/api/data")
  public String securedEndpoint() {
    return "Protected data"; // => Requires authentication
  }
}

// Run the app and see console:
// Using generated security password: a1b2c3d4-e5f6-7890-abcd-ef1234567890
// curl http://localhost:8080/api/data => 401 Unauthorized
// curl -u user:a1b2c3d4-e5f6-7890-abcd-ef1234567890 http://localhost:8080/api/data => "Protected data"
```

**Key Takeaway**: Spring Security auto-configuration secures everything by default—customize with `SecurityFilterChain` beans.

---

### Example 26: Custom Authentication

Configure users, passwords, and access rules.

```java
@Configuration
@EnableWebSecurity
public class SecurityConfig {

  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http
      .authorizeHttpRequests(auth -> auth
        .requestMatchers("/public/**").permitAll() // Public endpoints
        .requestMatchers("/admin/**").hasRole("ADMIN") // Admin only
        .anyRequest().authenticated() // All others require login
      )
      .formLogin(form -> form
        .loginPage("/login") // Custom login page
        .permitAll()
      )
      .logout(logout -> logout.permitAll());

    return http.build();
  }

  @Bean
  public UserDetailsService userDetailsService() {
    // In-memory users (for demo—use database in production)
    UserDetails user = User.builder()
      .username("user")
      .password(passwordEncoder().encode("password123"))
      .roles("USER")
      .build();

    UserDetails admin = User.builder()
      .username("admin")
      .password(passwordEncoder().encode("admin123"))
      .roles("ADMIN", "USER")
      .build();

    return new InMemoryUserDetailsManager(user, admin);
  }

  @Bean
  public PasswordEncoder passwordEncoder() {
    return new BCryptPasswordEncoder(); // Secure password hashing
  }
}

// Controller
@RestController
public class ApiController {
  @GetMapping("/public/hello")
  public String publicHello() {
    return "Public endpoint"; // => Accessible without login
  }

  @GetMapping("/api/user-data")
  public String userData() {
    return "User data"; // => Requires USER or ADMIN role
  }

  @GetMapping("/admin/dashboard")
  public String adminDashboard() {
    return "Admin dashboard"; // => Requires ADMIN role only
  }
}
```

**Key Takeaway**: `SecurityFilterChain` defines authorization rules—combine with `UserDetailsService` for custom user storage.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
flowchart TD
    A[HTTP Request] --> B{Security Filter Chain}
    B -->|/public/**| C[Permit All]
    B -->|/admin/**| D{Has ADMIN Role?}
    B -->|Other| E{Authenticated?}

    D -->|Yes| F[Allow Access]
    D -->|No| G[403 Forbidden]

    E -->|Yes| F
    E -->|No| H[Redirect to Login]

    H --> I[UserDetailsService]
    I --> J[Check Credentials]
    J -->|Valid| K[Create Authentication]
    J -->|Invalid| L[401 Unauthorized]

    K --> F

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#000
    style F fill:#029E73,stroke:#000,color:#fff
    style G fill:#CC78BC,stroke:#000,color:#000
    style L fill:#CC78BC,stroke:#000,color:#000
```

---

### Example 27: Method-Level Authorization

Secure individual methods with annotations.

```java
@Configuration
@EnableMethodSecurity // Enable method security annotations
public class MethodSecurityConfig {}

@Service
public class OrderService {
  @Autowired private OrderRepository orderRepo;

  @PreAuthorize("hasRole('USER')") // Requires USER role
  public List<Order> getMyOrders(String username) {
    return orderRepo.findByUsername(username);
  }

  @PreAuthorize("hasRole('ADMIN')") // Requires ADMIN role
  public List<Order> getAllOrders() {
    return orderRepo.findAll();
  }

  @PreAuthorize("#username == authentication.name") // Users can only access their own data
  public Order getOrder(String username, Long orderId) {
    return orderRepo.findByIdAndUsername(orderId, username)
      .orElseThrow(() -> new AccessDeniedException("Not authorized"));
  }

  @PreAuthorize("hasRole('ADMIN') or #order.username == authentication.name")
  public Order updateOrder(Order order) {
    return orderRepo.save(order); // => Admins can update any order, users only their own
  }

  @PostAuthorize("returnObject.username == authentication.name") // Check after method execution
  public Order loadOrder(Long orderId) {
    return orderRepo.findById(orderId).orElseThrow();
    // => Method executes, then Spring checks if returned order belongs to current user
  }
}

// Controller
@RestController
@RequestMapping("/api/orders")
public class OrderController {
  @Autowired private OrderService orderService;

  @GetMapping("/my-orders")
  public List<Order> getMyOrders(@AuthenticationPrincipal UserDetails user) {
    return orderService.getMyOrders(user.getUsername());
  }

  @GetMapping("/all")
  public List<Order> getAllOrders() {
    return orderService.getAllOrders(); // => Throws AccessDeniedException if not ADMIN
  }
}
```

**Key Takeaway**: `@PreAuthorize` and `@PostAuthorize` enable fine-grained authorization at the method level using SpEL expressions.

---

### Example 28: JWT Authentication

Implement stateless authentication with JSON Web Tokens.

```java
// pom.xml
<dependency>
  <groupId>io.jsonwebtoken</groupId>
  <artifactId>jjwt-api</artifactId>
  <version>0.12.3</version>
</dependency>
<dependency>
  <groupId>io.jsonwebtoken</groupId>
  <artifactId>jjwt-impl</artifactId>
  <version>0.12.3</version>
</dependency>

// JWT utility class
@Component
public class JwtUtil {
  private String secret = "mySecretKey1234567890123456789012"; // 256-bit key
  private long expiration = 86400000; // 24 hours

  public String generateToken(String username) {
    return Jwts.builder()
      .subject(username)
      .issuedAt(new Date())
      .expiration(new Date(System.currentTimeMillis() + expiration))
      .signWith(Keys.hmacShaKeyFor(secret.getBytes()))
      .compact();
    // => eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyMSIsImlhdCI6MTYzMjQ...
  }

  public String extractUsername(String token) {
    return Jwts.parser()
      .verifyWith(Keys.hmacShaKeyFor(secret.getBytes()))
      .build()
      .parseSignedClaims(token)
      .getPayload()
      .getSubject();
  }

  public boolean isTokenValid(String token, String username) {
    try {
      String extractedUser = extractUsername(token);
      return extractedUser.equals(username) && !isTokenExpired(token);
    } catch (Exception e) {
      return false;
    }
  }

  private boolean isTokenExpired(String token) {
    Date expiration = Jwts.parser()
      .verifyWith(Keys.hmacShaKeyFor(secret.getBytes()))
      .build()
      .parseSignedClaims(token)
      .getPayload()
      .getExpiration();
    return expiration.before(new Date());
  }
}

// JWT authentication filter
@Component
public class JwtAuthFilter extends OncePerRequestFilter {
  @Autowired private JwtUtil jwtUtil;
  @Autowired private UserDetailsService userDetailsService;

  @Override
  protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
    throws ServletException, IOException {

    String header = request.getHeader("Authorization");
    if (header != null && header.startsWith("Bearer ")) {
      String token = header.substring(7); // Remove "Bearer " prefix
      String username = jwtUtil.extractUsername(token);

      if (username != null && SecurityContextHolder.getContext().getAuthentication() == null) {
        UserDetails user = userDetailsService.loadUserByUsername(username);
        if (jwtUtil.isTokenValid(token, username)) {
          UsernamePasswordAuthenticationToken auth = new UsernamePasswordAuthenticationToken(
            user, null, user.getAuthorities()
          );
          SecurityContextHolder.getContext().setAuthentication(auth);
        }
      }
    }
    chain.doFilter(request, response);
  }
}

// Security config
@Configuration
@EnableWebSecurity
public class JwtSecurityConfig {
  @Autowired private JwtAuthFilter jwtAuthFilter;

  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http
      .csrf(csrf -> csrf.disable()) // Disable CSRF for stateless APIs
      .authorizeHttpRequests(auth -> auth
        .requestMatchers("/auth/**").permitAll()
        .anyRequest().authenticated()
      )
      .sessionManagement(session -> session
        .sessionCreationPolicy(SessionCreationPolicy.STATELESS) // No sessions
      )
      .addFilterBefore(jwtAuthFilter, UsernamePasswordAuthenticationFilter.class);

    return http.build();
  }
}

// Auth controller
@RestController
@RequestMapping("/auth")
public class AuthController {
  @Autowired private AuthenticationManager authManager;
  @Autowired private JwtUtil jwtUtil;

  @PostMapping("/login")
  public ResponseEntity<String> login(@RequestBody LoginRequest request) {
    authManager.authenticate(
      new UsernamePasswordAuthenticationToken(request.getUsername(), request.getPassword())
    );
    String token = jwtUtil.generateToken(request.getUsername());
    return ResponseEntity.ok(token); // => Return JWT to client
  }
}

// Usage:
// POST /auth/login {"username":"user1","password":"pass123"}
// Response: "eyJhbGciOiJIUzI1NiJ9..."
// GET /api/data with Header: Authorization: Bearer eyJhbGciOiJIUzI1NiJ9...
```

**Key Takeaway**: JWT enables stateless authentication—clients include tokens in headers, eliminating server-side session storage.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
sequenceDiagram
    participant C as Client
    participant A as Auth Controller
    participant F as JWT Filter
    participant S as Secured API

    C->>A: POST /auth/login (username, password)
    A->>A: Authenticate credentials
    A->>A: Generate JWT token
    A-->>C: Return JWT

    Note over C: Store JWT (localStorage/cookie)

    C->>F: GET /api/data<br/>Header: Authorization: Bearer {JWT}
    F->>F: Extract & validate JWT
    F->>F: Load user details
    F->>F: Set SecurityContext
    F->>S: Forward request (authenticated)
    S-->>C: Return data

    style C fill:#0173B2,stroke:#000,color:#fff
    style A fill:#029E73,stroke:#000,color:#fff
    style F fill:#DE8F05,stroke:#000,color:#000
    style S fill:#CC78BC,stroke:#000,color:#000
```

---

### Example 29: OAuth2 Integration

Enable social login with OAuth2 providers.

```java
// pom.xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter-oauth2-client</artifactId>
</dependency>

// application.yml
spring:
  security:
    oauth2:
      client:
        registration:
          google:
            client-id: YOUR_GOOGLE_CLIENT_ID
            client-secret: YOUR_GOOGLE_CLIENT_SECRET
            scope:
              - email
              - profile
          github:
            client-id: YOUR_GITHUB_CLIENT_ID
            client-secret: YOUR_GITHUB_CLIENT_SECRET
            scope:
              - user:email
              - read:user

// Security config
@Configuration
@EnableWebSecurity
public class OAuth2Config {
  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http
      .authorizeHttpRequests(auth -> auth
        .requestMatchers("/", "/login", "/error").permitAll()
        .anyRequest().authenticated()
      )
      .oauth2Login(oauth2 -> oauth2
        .loginPage("/login")
        .defaultSuccessUrl("/dashboard")
      );

    return http.build();
  }
}

// Controller
@RestController
public class ProfileController {
  @GetMapping("/dashboard")
  public String dashboard(@AuthenticationPrincipal OAuth2User principal) {
    String name = principal.getAttribute("name");
    String email = principal.getAttribute("email");
    return "Welcome, " + name + " (" + email + ")";
    // => Extracts user info from OAuth2 provider
  }

  @GetMapping("/user-info")
  public Map<String, Object> userInfo(@AuthenticationPrincipal OAuth2User principal) {
    return principal.getAttributes(); // => All user attributes from provider
  }
}

// Flow:
// 1. User clicks "Login with Google"
// 2. Redirects to Google's login page
// 3. User authenticates with Google
// 4. Google redirects back with authorization code
// 5. Spring exchanges code for access token
// 6. Spring fetches user info from Google
// 7. User logged in, redirected to /dashboard
```

**Key Takeaway**: Spring OAuth2 client simplifies social login—configure provider credentials and Spring handles the OAuth2 flow.

---

## Group 3: Testing

### Example 30: @SpringBootTest

Full integration testing with complete application context.

```java
// Test class
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class ProductControllerIntegrationTest {
  @Autowired private TestRestTemplate restTemplate; // HTTP client for testing
  @Autowired private ProductRepository productRepo;

  @BeforeEach
  void setup() {
    productRepo.deleteAll(); // Clean database before each test
  }

  @Test
  void testCreateAndRetrieveProduct() {
    // Create product
    Product product = new Product("Laptop", 999.99);
    ResponseEntity<Product> createResponse = restTemplate.postForEntity(
      "/api/products", product, Product.class
    );
    assertEquals(HttpStatus.CREATED, createResponse.getStatusCode());
    Long productId = createResponse.getBody().getId();

    // Retrieve product
    ResponseEntity<Product> getResponse = restTemplate.getForEntity(
      "/api/products/" + productId, Product.class
    );
    assertEquals(HttpStatus.OK, getResponse.getStatusCode());
    assertEquals("Laptop", getResponse.getBody().getName());
    // => Full end-to-end test: controller → service → repository → database
  }

  @Test
  void testDeleteProduct() {
    Product product = productRepo.save(new Product("Mouse", 25.00));

    restTemplate.delete("/api/products/" + product.getId());

    assertFalse(productRepo.existsById(product.getId()));
    // => Verifies deletion through full stack
  }
}

// Mocking external dependencies
@SpringBootTest
public class OrderServiceTest {
  @Autowired private OrderService orderService;

  @MockBean // Replace real bean with mock
  private PaymentGateway paymentGateway;

  @Test
  void testProcessOrder() {
    // Stub mock behavior
    when(paymentGateway.charge(any(), any())).thenReturn(true);

    Order order = new Order("user1", 100.00);
    orderService.processOrder(order);

    verify(paymentGateway, times(1)).charge(eq("user1"), eq(100.00));
    // => Tests order processing without calling real payment gateway
  }
}
```

**Key Takeaway**: `@SpringBootTest` loads full application context—use `@MockBean` to replace real dependencies with mocks.

---

### Example 31: @WebMvcTest

Test controllers in isolation without full context.

```java
@WebMvcTest(ProductController.class) // Only load ProductController
public class ProductControllerUnitTest {
  @Autowired private MockMvc mockMvc; // Simulates HTTP requests

  @MockBean // Mock the service layer
  private ProductService productService;

  @Test
  void testGetProduct() throws Exception {
    // Arrange
    Product product = new Product(1L, "Laptop", 999.99);
    when(productService.findById(1L)).thenReturn(Optional.of(product));

    // Act & Assert
    mockMvc.perform(get("/api/products/1"))
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.name").value("Laptop"))
      .andExpect(jsonPath("$.price").value(999.99));
    // => Tests controller logic without starting full app
  }

  @Test
  void testCreateProduct() throws Exception {
    Product product = new Product("Mouse", 25.00);
    when(productService.save(any(Product.class))).thenReturn(product);

    mockMvc.perform(post("/api/products")
        .contentType(MediaType.APPLICATION_JSON)
        .content("{\"name\":\"Mouse\",\"price\":25.00}"))
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.name").value("Mouse"));
  }

  @Test
  void testGetProductNotFound() throws Exception {
    when(productService.findById(999L)).thenReturn(Optional.empty());

    mockMvc.perform(get("/api/products/999"))
      .andExpect(status().isNotFound());
    // => Tests error handling
  }
}
```

**Key Takeaway**: `@WebMvcTest` loads only web layer—faster than `@SpringBootTest`, ideal for controller logic testing.

---

### Example 32: TestContainers

Test with real databases using Docker containers.

```java
// pom.xml
<dependency>
  <groupId>org.testcontainers</groupId>
  <artifactId>postgresql</artifactId>
  <version>1.19.3</version>
  <scope>test</scope>
</dependency>

// Test class
@SpringBootTest
@Testcontainers // Enable TestContainers support
public class ProductRepositoryTestContainersTest {
  @Container // Start PostgreSQL container
  static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16")
    .withDatabaseName("testdb")
    .withUsername("test")
    .withPassword("test");

  @DynamicPropertySource // Configure Spring to use container
  static void properties(DynamicPropertyRegistry registry) {
    registry.add("spring.datasource.url", postgres::getJdbcUrl);
    registry.add("spring.datasource.username", postgres::getUsername);
    registry.add("spring.datasource.password", postgres::getPassword);
  }

  @Autowired private ProductRepository productRepo;

  @Test
  void testSaveAndFind() {
    Product product = new Product("Keyboard", 75.00);
    productRepo.save(product);

    Optional<Product> found = productRepo.findById(product.getId());
    assertTrue(found.isPresent());
    assertEquals("Keyboard", found.get().getName());
    // => Tests against real PostgreSQL database in Docker
  }

  @Test
  void testCustomQuery() {
    productRepo.save(new Product("Mouse", 20.00));
    productRepo.save(new Product("Keyboard", 75.00));
    productRepo.save(new Product("Monitor", 300.00));

    List<Product> expensive = productRepo.findByPriceGreaterThan(50.00);
    assertEquals(2, expensive.size());
    // => Verifies custom queries against real database
  }
}
```

**Key Takeaway**: TestContainers provides real databases for tests—eliminates mocking discrepancies between H2 and production databases.

---

### Example 33: Mocking with Mockito

Isolate units under test with mocks.

```java
@ExtendWith(MockitoExtension.class) // Enable Mockito
public class OrderServiceUnitTest {
  @Mock // Create mock
  private OrderRepository orderRepo;

  @Mock
  private PaymentService paymentService;

  @InjectMocks // Inject mocks into service
  private OrderService orderService;

  @Test
  void testProcessOrder() {
    // Arrange
    Order order = new Order("user1", 100.00);
    when(orderRepo.save(any(Order.class))).thenReturn(order);
    when(paymentService.charge(anyString(), anyDouble())).thenReturn(true);

    // Act
    Order result = orderService.processOrder(order);

    // Assert
    assertNotNull(result);
    verify(orderRepo, times(1)).save(order); // Verify method called once
    verify(paymentService, times(1)).charge("user1", 100.00);
  }

  @Test
  void testProcessOrderPaymentFailure() {
    Order order = new Order("user1", 100.00);
    when(paymentService.charge(anyString(), anyDouble())).thenReturn(false);

    assertThrows(PaymentException.class, () -> {
      orderService.processOrder(order);
    });

    verify(orderRepo, never()).save(any()); // Verify save never called
    // => Tests failure scenarios
  }

  @Test
  void testArgumentCaptor() {
    Order order = new Order("user1", 100.00);
    when(paymentService.charge(anyString(), anyDouble())).thenReturn(true);

    orderService.processOrder(order);

    ArgumentCaptor<Order> captor = ArgumentCaptor.forClass(Order.class);
    verify(orderRepo).save(captor.capture());

    Order captured = captor.getValue();
    assertEquals("PROCESSED", captured.getStatus());
    // => Captures arguments passed to mocked methods
  }
}
```

**Key Takeaway**: Mockito enables fast, isolated unit tests—use `when().thenReturn()` for stubbing, `verify()` for interaction verification.

---

## Group 4: Caching & Performance

### Example 34: Cache Abstraction

Transparent caching with Spring's cache abstraction.

```java
// pom.xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter-cache</artifactId>
</dependency>

// Enable caching
@SpringBootApplication
@EnableCaching // Enable cache annotations
public class Application {
  public static void main(String[] args) {
    SpringApplication.run(Application.class, args);
  }
}

// Service with caching
@Service
public class ProductService {
  @Autowired private ProductRepository productRepo;

  @Cacheable("products") // Cache results by method arguments
  public Product findById(Long id) {
    System.out.println("Fetching from database: " + id);
    return productRepo.findById(id).orElseThrow();
    // First call: prints "Fetching..." and queries database
    // Subsequent calls: returns cached value, no database query
  }

  @Cacheable(value = "products", key = "#name") // Custom cache key
  public List<Product> findByName(String name) {
    System.out.println("Querying database for: " + name);
    return productRepo.findByNameContaining(name);
  }

  @CachePut(value = "products", key = "#result.id") // Update cache after method
  public Product save(Product product) {
    return productRepo.save(product);
    // => Saves to database AND updates cache with new value
  }

  @CacheEvict(value = "products", key = "#id") // Remove from cache
  public void deleteById(Long id) {
    productRepo.deleteById(id);
    // => Deletes from database AND evicts from cache
  }

  @CacheEvict(value = "products", allEntries = true) // Clear entire cache
  public void clearCache() {
    System.out.println("Cache cleared");
  }

  @Caching(evict = {
    @CacheEvict(value = "products", key = "#product.id"),
    @CacheEvict(value = "categories", key = "#product.categoryId")
  })
  public Product update(Product product) {
    return productRepo.save(product);
    // => Evicts multiple cache entries
  }
}

// Usage
Product p1 = productService.findById(1L); // => Database query
Product p2 = productService.findById(1L); // => Cached (no query)
productService.deleteById(1L); // => Evicts cache
Product p3 = productService.findById(1L); // => Database query again
```

**Key Takeaway**: Spring's cache abstraction decouples caching from business logic—annotate methods to cache, update, or evict automatically.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
flowchart TD
    A[Client Request] --> B{Cache Check}
    B -->|Cache Hit| C[Return Cached Value]
    B -->|Cache Miss| D[Execute Method]
    D --> E[Query Database]
    E --> F[Store in Cache]
    F --> G[Return Result]

    H[Update Operation] --> I[@CachePut]
    I --> J[Update Database]
    J --> K[Update Cache]

    L[Delete Operation] --> M[@CacheEvict]
    M --> N[Delete from Database]
    N --> O[Remove from Cache]

    style A fill:#0173B2,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style E fill:#DE8F05,stroke:#000,color:#000
    style K fill:#CC78BC,stroke:#000,color:#000
    style O fill:#CA9161,stroke:#000,color:#000
```

---

### Example 35: Redis Integration

Use Redis as distributed cache backend.

```java
// pom.xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter-data-redis</artifactId>
</dependency>

// application.yml
spring:
  data:
    redis:
      host: localhost
      port: 6379
  cache:
    type: redis
    redis:
      time-to-live: 600000 # 10 minutes in milliseconds

// Redis config
@Configuration
@EnableCaching
public class RedisCacheConfig {
  @Bean
  public CacheManager cacheManager(RedisConnectionFactory factory) {
    RedisCacheConfiguration config = RedisCacheConfiguration.defaultCacheConfig()
      .entryTtl(Duration.ofMinutes(10)) // Cache expiration
      .serializeKeysWith(
        RedisSerializationContext.SerializationPair.fromSerializer(new StringRedisSerializer())
      )
      .serializeValuesWith(
        RedisSerializationContext.SerializationPair.fromSerializer(new GenericJackson2JsonRedisSerializer())
      );

    return RedisCacheManager.builder(factory)
      .cacheDefaults(config)
      .build();
  }
}

// Service (same annotations as Example 34)
@Service
public class UserService {
  @Cacheable("users") // Now uses Redis instead of in-memory cache
  public User findById(Long id) {
    return userRepo.findById(id).orElseThrow();
  }
}

// Direct Redis operations (without cache abstraction)
@Service
public class SessionService {
  @Autowired private RedisTemplate<String, Object> redisTemplate;

  public void saveSession(String sessionId, UserSession session) {
    redisTemplate.opsForValue().set("session:" + sessionId, session, Duration.ofMinutes(30));
    // => Key: "session:abc123", Value: serialized UserSession, TTL: 30 minutes
  }

  public UserSession getSession(String sessionId) {
    return (UserSession) redisTemplate.opsForValue().get("session:" + sessionId);
    // => Returns null if expired or not found
  }

  public void deleteSession(String sessionId) {
    redisTemplate.delete("session:" + sessionId);
  }
}
```

**Key Takeaway**: Redis provides distributed caching across multiple application instances—configure TTL for automatic expiration.

---

### Example 36: Cache Strategies

Common caching patterns and pitfalls.

```java
@Service
public class CacheStrategyService {
  @Autowired private ProductRepository productRepo;
  @Autowired private CacheManager cacheManager;

  // Cache-Aside (Lazy Loading)
  @Cacheable("products")
  public Product findById(Long id) {
    return productRepo.findById(id).orElseThrow();
    // => Load on demand, cache misses query database
  }

  // Write-Through (Eager Update)
  @CachePut(value = "products", key = "#result.id")
  public Product save(Product product) {
    return productRepo.save(product);
    // => Writes to database AND cache simultaneously
  }

  // Cache Stampede Prevention
  @Cacheable(value = "expensive-data", sync = true) // Synchronize cache loading
  public ExpensiveData loadExpensiveData(String key) {
    // Only one thread loads data, others wait for cached result
    return computeExpensiveData(key);
  }

  // Conditional Caching
  @Cacheable(value = "products", condition = "#id > 100") // Only cache if id > 100
  public Product findByIdConditional(Long id) {
    return productRepo.findById(id).orElseThrow();
  }

  @Cacheable(value = "products", unless = "#result.price < 10") // Don't cache cheap products
  public Product findByIdUnless(Long id) {
    return productRepo.findById(id).orElseThrow();
  }

  // Manual Cache Control
  public void warmUpCache() {
    List<Product> topProducts = productRepo.findTop100ByOrderBySalesDesc();
    Cache cache = cacheManager.getCache("products");
    topProducts.forEach(p -> cache.put(p.getId(), p));
    // => Pre-populate cache with frequently accessed data
  }

  // Cache Invalidation Pattern
  @Scheduled(cron = "0 0 3 * * ?") // Every day at 3 AM
  public void scheduledCacheEviction() {
    cacheManager.getCacheNames().forEach(name -> {
      cacheManager.getCache(name).clear();
    });
    System.out.println("All caches cleared");
  }
}

// Common Pitfalls
@Service
public class CachePitfallsService {
  // ❌ Wrong: Self-invocation bypasses proxy
  public Product getProduct(Long id) {
    return this.findById(id); // Cache annotation IGNORED (no proxy)
  }

  @Cacheable("products")
  public Product findById(Long id) {
    return productRepo.findById(id).orElseThrow();
  }

  // ✅ Correct: Inject self-reference
  @Autowired private CachePitfallsService self;

  public Product getProductCorrect(Long id) {
    return self.findById(id); // Cache annotation WORKS (via proxy)
  }
}
```

**Key Takeaway**: Choose caching strategies based on consistency needs—cache-aside for reads, write-through for updates, sync for stampede prevention.

---

## Group 5: Async & Events

### Example 37: @Async Methods

Execute methods asynchronously with thread pools.

```java
// Enable async support
@SpringBootApplication
@EnableAsync // Enable @Async annotation
public class Application {
  public static void main(String[] args) {
    SpringApplication.run(Application.class, args);
  }
}

// Async service
@Service
public class EmailService {
  @Async // Runs in separate thread
  public void sendEmail(String to, String subject, String body) {
    System.out.println("Sending email to " + to + " - Thread: " + Thread.currentThread().getName());
    // Simulate delay
    try { Thread.sleep(3000); } catch (InterruptedException e) {}
    System.out.println("Email sent to " + to);
  }

  @Async
  public CompletableFuture<String> sendEmailWithResult(String to) {
    System.out.println("Sending email - Thread: " + Thread.currentThread().getName());
    try { Thread.sleep(2000); } catch (InterruptedException e) {}
    return CompletableFuture.completedFuture("Email sent to " + to);
    // => Returns CompletableFuture for async result handling
  }

  @Async
  public CompletableFuture<Integer> processLargeFile(String filename) {
    System.out.println("Processing " + filename);
    try { Thread.sleep(5000); } catch (InterruptedException e) {}
    return CompletableFuture.completedFuture(10000); // Processed 10000 records
  }
}

// Controller
@RestController
@RequestMapping("/api/async")
public class AsyncController {
  @Autowired private EmailService emailService;

  @PostMapping("/send-email")
  public ResponseEntity<String> sendEmail(@RequestParam String to) {
    emailService.sendEmail(to, "Welcome", "Hello!");
    return ResponseEntity.ok("Email queued"); // Returns immediately
    // => Controller thread doesn't wait for email to send
  }

  @GetMapping("/send-with-result")
  public CompletableFuture<String> sendWithResult(@RequestParam String to) {
    return emailService.sendEmailWithResult(to)
      .thenApply(result -> "Result: " + result);
    // => Non-blocking—returns CompletableFuture
  }

  @GetMapping("/process-multiple")
  public CompletableFuture<String> processMultiple() {
    CompletableFuture<Integer> file1 = emailService.processLargeFile("file1.csv");
    CompletableFuture<Integer> file2 = emailService.processLargeFile("file2.csv");
    CompletableFuture<Integer> file3 = emailService.processLargeFile("file3.csv");

    return CompletableFuture.allOf(file1, file2, file3)
      .thenApply(v -> {
        int total = file1.join() + file2.join() + file3.join();
        return "Total records processed: " + total;
      });
    // => Processes 3 files in parallel, waits for all to complete
  }
}
```

**Key Takeaway**: `@Async` offloads work to background threads—use `CompletableFuture` return types for composable async operations.

---

### Example 38: Task Executors

Configure thread pools for async execution.

```java
@Configuration
@EnableAsync
public class AsyncConfig implements AsyncConfigurer {

  @Override
  public Executor getAsyncExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(5); // Minimum threads
    executor.setMaxPoolSize(10); // Maximum threads
    executor.setQueueCapacity(100); // Queue size before rejecting tasks
    executor.setThreadNamePrefix("async-"); // Thread naming
    executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
    executor.initialize();
    return executor;
  }

  // Custom executor for specific tasks
  @Bean(name = "emailExecutor")
  public Executor emailExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(2);
    executor.setMaxPoolSize(5);
    executor.setQueueCapacity(50);
    executor.setThreadNamePrefix("email-");
    executor.initialize();
    return executor;
  }

  @Bean(name = "reportExecutor")
  public Executor reportExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(1);
    executor.setMaxPoolSize(3);
    executor.setQueueCapacity(20);
    executor.setThreadNamePrefix("report-");
    executor.initialize();
    return executor;
  }
}

// Service with custom executors
@Service
public class NotificationService {
  @Async("emailExecutor") // Use specific executor
  public void sendEmailNotification(String to) {
    System.out.println("Email thread: " + Thread.currentThread().getName());
    // => Thread name: email-1, email-2, etc.
  }

  @Async("reportExecutor")
  public CompletableFuture<Report> generateReport(Long id) {
    System.out.println("Report thread: " + Thread.currentThread().getName());
    // => Thread name: report-1, report-2, etc.
    return CompletableFuture.completedFuture(new Report(id));
  }
}
```

**Key Takeaway**: Configure `ThreadPoolTaskExecutor` for fine-grained control—separate executors isolate thread pools for different task types.

---

### Example 39: Application Events

Decouple components with Spring's event publishing mechanism.

```java
// Custom event
public class OrderPlacedEvent extends ApplicationEvent {
  private final Order order;

  public OrderPlacedEvent(Object source, Order order) {
    super(source);
    this.order = order;
  }

  public Order getOrder() {
    return order;
  }
}

// Event publisher
@Service
public class OrderService {
  @Autowired private ApplicationEventPublisher eventPublisher;
  @Autowired private OrderRepository orderRepo;

  public Order placeOrder(Order order) {
    order.setStatus("PLACED");
    Order saved = orderRepo.save(order);

    // Publish event
    eventPublisher.publishEvent(new OrderPlacedEvent(this, saved));
    // => Listeners are notified asynchronously

    return saved;
  }
}

// Event listeners
@Component
public class EmailNotificationListener {
  @EventListener // Subscribe to event
  public void handleOrderPlaced(OrderPlacedEvent event) {
    Order order = event.getOrder();
    System.out.println("Sending confirmation email for order " + order.getId());
    // => Executes synchronously by default
  }
}

@Component
public class InventoryListener {
  @EventListener
  @Async // Make listener async
  public void handleOrderPlaced(OrderPlacedEvent event) {
    System.out.println("Updating inventory - Thread: " + Thread.currentThread().getName());
    // => Executes in background thread
  }
}

@Component
public class AnalyticsListener {
  @EventListener
  @Async
  public void handleOrderPlaced(OrderPlacedEvent event) {
    System.out.println("Recording analytics for order " + event.getOrder().getId());
  }
}

// Conditional listeners
@Component
public class LargeOrderListener {
  @EventListener(condition = "#event.order.total > 1000") // Only for large orders
  public void handleLargeOrder(OrderPlacedEvent event) {
    System.out.println("Large order detected: " + event.getOrder().getTotal());
  }
}

// Transaction-aware events
@Component
public class TransactionalListener {
  @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT) // After transaction commits
  public void handleOrderPlaced(OrderPlacedEvent event) {
    System.out.println("Order committed, safe to send external API call");
    // => Only fires if transaction succeeds
  }
}
```

**Key Takeaway**: Events decouple publishers from listeners—use `@Async` for parallel processing, `@TransactionalEventListener` for transaction safety.

---

### Example 40: Scheduling

Execute tasks on fixed schedules with `@Scheduled`.

```java
// Enable scheduling
@SpringBootApplication
@EnableScheduling // Enable @Scheduled annotation
public class Application {
  public static void main(String[] args) {
    SpringApplication.run(Application.class, args);
  }
}

// Scheduled tasks
@Component
public class ScheduledTasks {

  // Fixed rate: Executes every 5 seconds (from start of previous execution)
  @Scheduled(fixedRate = 5000)
  public void reportCurrentTime() {
    System.out.println("Current time: " + LocalDateTime.now());
    // If task takes 2 seconds, next execution starts at: 0s, 5s, 10s, 15s...
  }

  // Fixed delay: Waits 5 seconds AFTER previous execution finishes
  @Scheduled(fixedDelay = 5000)
  public void processQueueWithDelay() {
    System.out.println("Processing queue at " + LocalDateTime.now());
    try { Thread.sleep(3000); } catch (InterruptedException e) {}
    // Next execution starts 5 seconds after this finishes
    // Execution pattern: 0s, 8s, 16s, 24s... (3s task + 5s delay)
  }

  // Initial delay: Wait 10 seconds before first execution
  @Scheduled(initialDelay = 10000, fixedRate = 5000)
  public void delayedStart() {
    System.out.println("Delayed task executed");
    // First execution at 10s, then every 5s: 10s, 15s, 20s...
  }

  // Cron expression: Every day at 2:00 AM
  @Scheduled(cron = "0 0 2 * * ?")
  public void dailyBackup() {
    System.out.println("Running daily backup at " + LocalDateTime.now());
  }

  // Cron: Every weekday at 9:00 AM
  @Scheduled(cron = "0 0 9 ? * MON-FRI")
  public void weekdayReport() {
    System.out.println("Weekday report generated");
  }

  // Cron: Every 15 minutes
  @Scheduled(cron = "0 */15 * * * ?")
  public void quarterHourlyCheck() {
    System.out.println("15-minute check at " + LocalDateTime.now());
    // Runs at: 00:00, 00:15, 00:30, 00:45, 01:00, 01:15...
  }

  // Cron from configuration
  @Scheduled(cron = "${app.cleanup.schedule}") // Read from application.properties
  public void configurableSchedule() {
    System.out.println("Cleanup task running");
  }
}

// Cron format: second minute hour day-of-month month day-of-week
// Examples:
// "0 0 * * * ?" - Every hour
// "0 30 9 * * ?" - 9:30 AM daily
// "0 0 12 1 * ?" - 12:00 PM on 1st day of month
```

**Key Takeaway**: Use `fixedRate` for periodic tasks, `fixedDelay` to prevent overlap, `cron` for specific times—Spring handles scheduling automatically.

---

### Example 31: WebSocket - Real-Time Communication

WebSocket enables bidirectional, real-time communication between server and clients for chat, notifications, and live updates.

```java
// pom.xml: spring-boot-starter-websocket

@Configuration
@EnableWebSocketMessageBroker
public class WebSocketConfig implements WebSocketMessageBrokerConfigurer {
    @Override
    public void configureMessageBroker(MessageBrokerRegistry config) {
        config.enableSimpleBroker("/topic", "/queue"); // => Broadcast and P2P channels
        config.setApplicationDestinationPrefixes("/app");
    }

    @Override
    public void registerStompEndpoints(StompEndpointRegistry registry) {
        registry.addEndpoint("/ws")
            .setAllowedOrigins("http://localhost:3000")
            .withSockJS(); // => Fallback for browsers without WebSocket
    }
}

@Controller
public class ChatController {
    @MessageMapping("/chat.send") // Client sends to /app/chat.send
    @SendTo("/topic/messages") // Broadcast to all subscribers of /topic/messages
    public ChatMessage sendMessage(ChatMessage message) {
        message.setTimestamp(LocalDateTime.now());
        return message; // => Broadcasts to all connected clients
    }

    @MessageMapping("/chat.private")
    @SendToUser("/queue/private") // Send to specific user's queue
    public ChatMessage sendPrivateMessage(ChatMessage message, Principal principal) {
        message.setRecipient(principal.getName());
        return message; // => Only recipient receives this
    }
}

record ChatMessage(String sender, String content, String recipient, LocalDateTime timestamp) {}
```

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
sequenceDiagram
    participant C1 as Client 1
    participant S as Spring Server
    participant C2 as Client 2

    C1->>S: Connect /ws (WebSocket handshake)
    C2->>S: Connect /ws (WebSocket handshake)
    C1->>S: Subscribe /topic/messages
    C2->>S: Subscribe /topic/messages

    C1->>S: Send /app/chat.send {"sender":"Alice","content":"Hello"}
    S->>C1: Broadcast /topic/messages
    S->>C2: Broadcast /topic/messages

    Note over C1,C2: Both clients receive message in real-time

    style C1 fill:#0173B2,color:#fff
    style S fill:#029E73,color:#fff
    style C2 fill:#DE8F05,color:#000
```

**Key Takeaway**: WebSocket enables real-time bidirectional communication—use `@MessageMapping` for client messages, `@SendTo` for broadcast to all subscribers, and `@SendToUser` for user-specific messages.

---

### Example 32: Server-Sent Events (SSE) - Unidirectional Streaming

SSE streams server updates to clients over HTTP, simpler than WebSocket for one-way communication.

```java
@RestController
@RequestMapping("/api/sse")
public class SseController {
    @GetMapping(value = "/stream", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<ServerSentEvent<String>> streamEvents() {
        return Flux.interval(Duration.ofSeconds(1))
            .map(seq -> ServerSentEvent.<String>builder()
                .id(String.valueOf(seq))
                .event("message")
                .data("Server time: " + LocalDateTime.now())
                .build()
            );
        // => Sends event every second: data: Server time: 2024-12-24T10:00:00
    }

    @GetMapping(value = "/stock-prices", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<ServerSentEvent<StockPrice>> streamStockPrices() {
        return Flux.interval(Duration.ofSeconds(2))
            .map(i -> ServerSentEvent.<StockPrice>builder()
                .data(new StockPrice("AAPL", 150.0 + Math.random() * 10))
                .build()
            );
        // => Streams stock updates every 2 seconds
    }
}

record StockPrice(String symbol, double price) {}
```

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
flowchart LR
    Server[Spring Server] -->|SSE Stream| C1[Client 1]
    Server -->|SSE Stream| C2[Client 2]
    Server -->|SSE Stream| C3[Client 3]

    Note1[data: event 1] --> Server
    Note2[data: event 2] --> Server
    Note3[data: event 3] --> Server

    style Server fill:#0173B2,color:#fff
    style C1 fill:#029E73,color:#fff
    style C2 fill:#DE8F05,color:#000
    style C3 fill:#CC78BC,color:#000
```

**Key Takeaway**: SSE provides unidirectional server-to-client streaming over HTTP—simpler than WebSocket for use cases like live dashboards, notifications, and progress updates where bidirectional communication isn't needed.

---

### Example 33: API Versioning Strategies

Manage API evolution while maintaining backward compatibility through URL, header, or parameter versioning.

```java
// Strategy 1: URL Path Versioning
@RestController
@RequestMapping("/api/v1/users")
public class UserV1Controller {
    @GetMapping("/{id}")
    public UserV1 getUser(@PathVariable Long id) {
        return new UserV1(id, "Alice", "alice@example.com");
        // => /api/v1/users/1 returns version 1 format
    }
}

@RestController
@RequestMapping("/api/v2/users")
public class UserV2Controller {
    @GetMapping("/{id}")
    public UserV2 getUser(@PathVariable Long id) {
        return new UserV2(id, "Alice", "Smith", "alice@example.com", "123-456-7890");
        // => /api/v2/users/1 returns version 2 format (added lastName, phone)
    }
}

record UserV1(Long id, String name, String email) {}
record UserV2(Long id, String firstName, String lastName, String email, String phone) {}

// Strategy 2: Header Versioning
@RestController
@RequestMapping("/api/users")
public class UserHeaderVersionController {
    @GetMapping(value = "/{id}", headers = "X-API-Version=1")
    public UserV1 getUserV1(@PathVariable Long id) {
        return new UserV1(id, "Alice", "alice@example.com");
        // => Header: X-API-Version: 1
    }

    @GetMapping(value = "/{id}", headers = "X-API-Version=2")
    public UserV2 getUserV2(@PathVariable Long id) {
        return new UserV2(id, "Alice", "Smith", "alice@example.com", "123-456-7890");
        // => Header: X-API-Version: 2
    }
}

// Strategy 3: Content Negotiation (Accept Header)
@RestController
@RequestMapping("/api/users")
public class UserContentNegotiationController {
    @GetMapping(value = "/{id}", produces = "application/vnd.myapp.v1+json")
    public UserV1 getUserV1(@PathVariable Long id) {
        return new UserV1(id, "Alice", "alice@example.com");
        // => Header: Accept: application/vnd.myapp.v1+json
    }

    @GetMapping(value = "/{id}", produces = "application/vnd.myapp.v2+json")
    public UserV2 getUserV2(@PathVariable Long id) {
        return new UserV2(id, "Alice", "Smith", "alice@example.com", "123-456-7890");
        // => Header: Accept: application/vnd.myapp.v2+json
    }
}

// Strategy 4: Request Parameter Versioning
@RestController
@RequestMapping("/api/users")
public class UserParamVersionController {
    @GetMapping("/{id}")
    public Object getUser(@PathVariable Long id, @RequestParam(defaultValue = "1") int version) {
        if (version == 2) {
            return new UserV2(id, "Alice", "Smith", "alice@example.com", "123-456-7890");
        }
        return new UserV1(id, "Alice", "alice@example.com");
        // => /api/users/1?version=2
    }
}
```

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Client[API Client] --> V1[/api/v1/users]
    Client --> V2[/api/v2/users]

    V1 --> R1["UserV1<br/>{id, name, email}"]
    V2 --> R2["UserV2<br/>{id, firstName, lastName,<br/>email, phone}"]

    Note1["URL Versioning<br/>Most visible"] --> V1
    Note2["Backward Compatible<br/>New fields added"] --> V2

    style Client fill:#0173B2,color:#fff
    style V1 fill:#029E73,color:#fff
    style V2 fill:#DE8F05,color:#000
    style R1 fill:#CC78BC,color:#000
    style R2 fill:#CA9161,color:#000
```

**Key Takeaway**: Choose versioning strategy based on client capabilities—URL versioning is most visible and cacheable, header versioning keeps URLs clean, content negotiation follows REST standards, and parameter versioning is simplest for internal APIs.

---

### Example 34: Custom Argument Resolvers

Create custom argument resolvers to extract and inject domain objects from requests.

```java
// Custom annotation
@Target(ElementType.PARAMETER)
@Retention(RetentionPolicy.RUNTIME)
public @interface CurrentUser {}

// Argument resolver
@Component
public class CurrentUserArgumentResolver implements HandlerMethodArgumentResolver {
    @Override
    public boolean supportsParameter(MethodParameter parameter) {
        return parameter.hasParameterAnnotation(CurrentUser.class)
            && parameter.getParameterType().equals(User.class);
        // => Activates when @CurrentUser User parameter detected
    }

    @Override
    public Object resolveArgument(
        MethodParameter parameter,
        ModelAndViewContainer mavContainer,
        NativeWebRequest webRequest,
        WebDataBinderFactory binderFactory
    ) {
        String authHeader = webRequest.getHeader("Authorization");
        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            String token = authHeader.substring(7);
            return extractUserFromToken(token);
            // => Extracts user from JWT token
        }
        return null;
    }

    private User extractUserFromToken(String token) {
        return new User(1L, "alice", "alice@example.com");
        // => Simplified token parsing
    }
}

// Register resolver
@Configuration
public class WebConfig implements WebMvcConfigurer {
    @Autowired
    private CurrentUserArgumentResolver currentUserArgumentResolver;

    @Override
    public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
        resolvers.add(currentUserArgumentResolver);
    }
}

// Usage in controller
@RestController
@RequestMapping("/api/profile")
public class ProfileController {
    @GetMapping
    public User getProfile(@CurrentUser User user) {
        // user automatically resolved from JWT token
        return user; // => No need to manually parse Authorization header
    }

    @PutMapping
    public User updateProfile(@CurrentUser User user, @RequestBody ProfileUpdate update) {
        // Both user and request body available
        return user;
    }
}

record User(Long id, String username, String email) {}
record ProfileUpdate(String email, String phone) {}
```

**Key Takeaway**: Custom argument resolvers eliminate repetitive parameter extraction—implement `HandlerMethodArgumentResolver` to automatically inject domain objects from headers, cookies, or custom authentication mechanisms.

---

### Example 35: Filter vs Interceptor vs AOP

Understand the differences and use cases for filters, interceptors, and AOP for cross-cutting concerns.

```java
// 1. Servlet Filter - Operates at servlet container level
@Component
@Order(1)
public class RequestResponseLoggingFilter implements Filter {
    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
        throws IOException, ServletException {
        HttpServletRequest req = (HttpServletRequest) request;
        System.out.println("FILTER: Before request - " + req.getRequestURI());
        // => Executes before DispatcherServlet

        chain.doFilter(request, response);

        System.out.println("FILTER: After response");
        // => Executes after response sent
    }
}

// 2. HandlerInterceptor - Operates at Spring MVC level
@Component
public class PerformanceInterceptor implements HandlerInterceptor {
    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) {
        request.setAttribute("startTime", System.currentTimeMillis());
        System.out.println("INTERCEPTOR: Before controller method");
        return true;
        // => Executes after DispatcherServlet, before controller
    }

    @Override
    public void postHandle(HttpServletRequest request, HttpServletResponse response,
                          Object handler, ModelAndView modelAndView) {
        System.out.println("INTERCEPTOR: After controller method, before view");
        // => Executes after controller, only if no exception
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response,
                               Object handler, Exception ex) {
        long duration = System.currentTimeMillis() - (Long) request.getAttribute("startTime");
        System.out.println("INTERCEPTOR: Request completed in " + duration + "ms");
        // => Always executes, even if exception occurred
    }
}

// 3. AOP Aspect - Operates at method level
@Aspect
@Component
public class LoggingAspect {
    @Before("execution(* com.example.demo.service.*.*(..))")
    public void logBefore(JoinPoint joinPoint) {
        System.out.println("AOP: Before method - " + joinPoint.getSignature().getName());
        // => Executes before any service method
    }

    @AfterReturning(pointcut = "execution(* com.example.demo.service.*.*(..))", returning = "result")
    public void logAfterReturning(JoinPoint joinPoint, Object result) {
        System.out.println("AOP: Method returned - " + result);
        // => Executes after successful method execution
    }

    @Around("@annotation(org.springframework.transaction.annotation.Transactional)")
    public Object logTransaction(ProceedingJoinPoint joinPoint) throws Throwable {
        System.out.println("AOP: Transaction starting");
        Object result = joinPoint.proceed();
        System.out.println("AOP: Transaction completed");
        return result;
        // => Wraps @Transactional methods
    }
}
```

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
flowchart TD
    Request[HTTP Request] --> Filter[Servlet Filter]
    Filter --> DispatcherServlet[DispatcherServlet]
    DispatcherServlet --> InterceptorPre[Interceptor preHandle]
    InterceptorPre --> AOP_Before[AOP @Before]
    AOP_Before --> Controller[Controller Method]
    Controller --> AOP_After[AOP @AfterReturning]
    AOP_After --> InterceptorPost[Interceptor postHandle]
    InterceptorPost --> View[View Rendering]
    View --> InterceptorAfter[Interceptor afterCompletion]
    InterceptorAfter --> FilterAfter[Filter doFilter return]
    FilterAfter --> Response[HTTP Response]

    style Request fill:#0173B2,color:#fff
    style Filter fill:#DE8F05,color:#000
    style DispatcherServlet fill:#029E73,color:#fff
    style InterceptorPre fill:#CC78BC,color:#000
    style AOP_Before fill:#CA9161,color:#000
    style Controller fill:#0173B2,color:#fff
    style Response fill:#029E73,color:#fff
```

**Key Takeaway**: Use **Filters** for servlet-level concerns (encoding, security, CORS), **Interceptors** for Spring MVC concerns (authentication, logging, request/response modification), and **AOP** for business logic concerns (transactions, caching, auditing) targeting specific methods.

---

### Example 36: Custom Annotations with AOP

Combine custom annotations with AOP for declarative cross-cutting concerns.

```java
// Custom annotation
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface LogExecutionTime {}

// AOP Aspect
@Aspect
@Component
@Slf4j
public class ExecutionTimeAspect {
    @Around("@annotation(LogExecutionTime)")
    public Object logExecutionTime(ProceedingJoinPoint joinPoint) throws Throwable {
        long start = System.currentTimeMillis();

        Object result = joinPoint.proceed(); // => Execute method

        long duration = System.currentTimeMillis() - start;
        log.info("{} executed in {}ms", joinPoint.getSignature(), duration);
        // => Logs: com.example.demo.service.UserService.findUser(..) executed in 45ms

        return result;
    }
}

// Custom audit annotation
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Audit {
    String action();
}

@Aspect
@Component
public class AuditAspect {
    @Autowired
    private AuditLogRepository auditLogRepository;

    @AfterReturning("@annotation(audit)")
    public void logAudit(JoinPoint joinPoint, Audit audit) {
        String username = SecurityContextHolder.getContext().getAuthentication().getName();
        String methodName = joinPoint.getSignature().getName();

        AuditLog log = new AuditLog(
            username,
            audit.action(),
            methodName,
            LocalDateTime.now()
        );
        auditLogRepository.save(log);
        // => Automatically logs all audited operations
    }
}

// Usage in service
@Service
public class UserService {
    @LogExecutionTime
    @Audit(action = "USER_CREATED")
    public User createUser(User user) {
        // Method automatically timed and audited
        return userRepository.save(user);
    }

    @LogExecutionTime
    public List<User> findAll() {
        // Only execution time logged (no audit)
        return userRepository.findAll();
    }
}
```

**Key Takeaway**: Combine custom annotations with AOP for declarative cross-cutting concerns—create domain-specific annotations (`@LogExecutionTime`, `@Audit`, `@RateLimit`) and implement behavior in aspects for clean, reusable functionality.

---

### Example 37: Bean Post Processors

Modify or enhance beans during initialization with BeanPostProcessor.

```java
// Custom annotation for initialization
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface InitializeOnStartup {}

// Bean post processor
@Component
public class InitializationBeanPostProcessor implements BeanPostProcessor {
    @Override
    public Object postProcessBeforeInitialization(Object bean, String beanName) {
        // Called before @PostConstruct
        if (bean.getClass().isAnnotationPresent(InitializeOnStartup.class)) {
            System.out.println("Initializing bean: " + beanName);
        }
        return bean;
    }

    @Override
    public Object postProcessAfterInitialization(Object bean, String beanName) {
        // Called after @PostConstruct
        if (bean instanceof CacheManager) {
            System.out.println("CacheManager bean ready: " + beanName);
            ((CacheManager) bean).warmUpCache();
            // => Automatically warm up cache after initialization
        }
        return bean;
    }
}

// Auto-proxy creation example
@Component
public class PerformanceProxyBeanPostProcessor implements BeanPostProcessor {
    @Override
    public Object postProcessAfterInitialization(Object bean, String beanName) {
        if (bean.getClass().getPackageName().startsWith("com.example.demo.service")) {
            // Wrap service beans in performance monitoring proxy
            return createProxy(bean);
        }
        return bean;
    }

    private Object createProxy(Object target) {
        return Proxy.newProxyInstance(
            target.getClass().getClassLoader(),
            target.getClass().getInterfaces(),
            (proxy, method, args) -> {
                long start = System.currentTimeMillis();
                Object result = method.invoke(target, args);
                long duration = System.currentTimeMillis() - start;
                System.out.println(method.getName() + " took " + duration + "ms");
                return result;
            }
        );
    }
}

@InitializeOnStartup
@Service
public class DataPreloadService {
    @PostConstruct
    public void init() {
        System.out.println("Preloading data...");
    }
}
```

**Key Takeaway**: BeanPostProcessors enable bean customization during initialization—use `postProcessBeforeInitialization` for pre-init configuration and `postProcessAfterInitialization` for auto-proxying, validation, or post-init setup.

---

### Example 38: Custom Spring Boot Starter (Simplified)

Create a lightweight auto-configuration module for reusable functionality.

```java
// 1. Create auto-configuration class
@Configuration
@ConditionalOnClass(EmailService.class)
@EnableConfigurationProperties(EmailProperties.class)
public class EmailAutoConfiguration {
    @Bean
    @ConditionalOnMissingBean
    public EmailService emailService(EmailProperties properties) {
        return new EmailService(properties);
    }
}

// 2. Configuration properties
@ConfigurationProperties(prefix = "app.email")
public class EmailProperties {
    private String host = "smtp.gmail.com";
    private int port = 587;
    private String username;
    private String password;
    // getters/setters
}

// 3. Service implementation
public class EmailService {
    private final EmailProperties properties;

    public EmailService(EmailProperties properties) {
        this.properties = properties;
    }

    public void sendEmail(String to, String subject, String body) {
        System.out.println("Sending email to " + to + " via " + properties.getHost());
        // Actual email sending logic
    }
}

// 4. Register auto-configuration
// Create: META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports
// Add line: com.example.starter.EmailAutoConfiguration

// 5. Usage in application
// Just add dependency and configure:
// app.email.username=user@gmail.com
// app.email.password=secret

@Service
public class NotificationService {
    @Autowired
    private EmailService emailService; // Automatically available!

    public void notifyUser(String email) {
        emailService.sendEmail(email, "Welcome", "Thanks for signing up!");
    }
}
```

**Key Takeaway**: Create Spring Boot starters for reusable auto-configuration—define `@Configuration` with `@ConditionalOnClass` and `@EnableConfigurationProperties`, register in `AutoConfiguration.imports`, and users get automatic bean registration with type-safe properties.

---

### Example 39: Reactive Repositories with R2DBC

Use R2DBC for reactive, non-blocking database access.

```java
// pom.xml: spring-boot-starter-data-r2dbc, r2dbc-h2

@Entity
@Table(name = "products")
public class Product {
    @Id
    private Long id;
    private String name;
    private BigDecimal price;
    // getters/setters
}

// Reactive repository
public interface ProductRepository extends ReactiveCrudRepository<Product, Long> {
    Flux<Product> findByNameContaining(String name); // => Returns Flux (0..N items)
    Mono<Product> findByName(String name); // => Returns Mono (0..1 item)

    @Query("SELECT * FROM products WHERE price > :minPrice")
    Flux<Product> findExpensiveProducts(BigDecimal minPrice);
}

@Service
public class ProductService {
    @Autowired
    private ProductRepository productRepository;

    public Flux<Product> getAllProducts() {
        return productRepository.findAll();
        // => Non-blocking stream of products
    }

    public Mono<Product> createProduct(Product product) {
        return productRepository.save(product);
        // => Non-blocking save operation
    }

    public Flux<Product> searchProducts(String keyword) {
        return productRepository.findByNameContaining(keyword)
            .filter(p -> p.getPrice().compareTo(BigDecimal.ZERO) > 0)
            .map(p -> {
                p.setName(p.getName().toUpperCase());
                return p;
            });
        // => Reactive pipeline: fetch → filter → transform
    }
}

@RestController
@RequestMapping("/api/products")
public class ProductController {
    @Autowired
    private ProductService productService;

    @GetMapping(produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<Product> streamProducts() {
        return productService.getAllProducts();
        // => Streams products as Server-Sent Events
    }

    @PostMapping
    public Mono<Product> createProduct(@RequestBody Product product) {
        return productService.createProduct(product);
        // => Non-blocking POST handling
    }
}

// application.yml
// spring:
//   r2dbc:
//     url: r2dbc:h2:mem:///testdb
//     username: sa
//     password:
```

**Key Takeaway**: R2DBC enables reactive database access—use `ReactiveCrudRepository` for non-blocking queries returning `Mono<T>` (0..1) or `Flux<T>` (0..N), enabling end-to-end reactive pipelines from database to HTTP response.

---

### Example 40: Composite Keys and Embedded IDs

Handle composite primary keys with `@IdClass` or `@EmbeddedId`.

```java
// Strategy 1: @IdClass
@IdClass(OrderItemId.class)
@Entity
public class OrderItem {
    @Id
    private Long orderId;

    @Id
    private Long productId;

    private int quantity;
    private BigDecimal price;

    // Constructors, getters, setters
}

// Composite key class
public class OrderItemId implements Serializable {
    private Long orderId;
    private Long productId;

    // Must have equals() and hashCode()
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof OrderItemId)) return false;
        OrderItemId that = (OrderItemId) o;
        return Objects.equals(orderId, that.orderId) &&
               Objects.equals(productId, that.productId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(orderId, productId);
    }
}

// Repository with composite key
public interface OrderItemRepository extends JpaRepository<OrderItem, OrderItemId> {
    List<OrderItem> findByOrderId(Long orderId);
}

// Usage
OrderItemId id = new OrderItemId();
id.setOrderId(1L);
id.setProductId(100L);
Optional<OrderItem> item = orderItemRepository.findById(id);

// Strategy 2: @EmbeddedId (recommended)
@Embeddable
public class OrderItemKey implements Serializable {
    private Long orderId;
    private Long productId;

    // equals(), hashCode(), getters, setters
}

@Entity
public class OrderItemEmbedded {
    @EmbeddedId
    private OrderItemKey id;

    private int quantity;
    private BigDecimal price;

    // Access composite key fields
    public Long getOrderId() {
        return id.getOrderId();
    }
}

// Repository
public interface OrderItemEmbeddedRepository extends JpaRepository<OrderItemEmbedded, OrderItemKey> {
    @Query("SELECT o FROM OrderItemEmbedded o WHERE o.id.orderId = :orderId")
    List<OrderItemEmbedded> findByOrderId(Long orderId);
}

// Usage
OrderItemKey key = new OrderItemKey(1L, 100L);
OrderItemEmbedded item = new OrderItemEmbedded();
item.setId(key);
item.setQuantity(5);
orderItemEmbeddedRepository.save(item);
```

**Key Takeaway**: Use `@EmbeddedId` over `@IdClass` for composite keys—it encapsulates key fields in a single object, provides better type safety, and makes queries clearer by explicitly referencing the embedded ID.

---

## Summary

You've learned 30 intermediate Spring Boot patterns:

**Transactions & Data**:

- `@Transactional` for ACID operations
- Isolation levels for concurrency control
- Optimistic locking with `@Version`
- Batch operations for performance

**Spring Security**:

- Auto-configuration and custom security chains
- Method-level authorization with `@PreAuthorize`
- JWT stateless authentication
- OAuth2 social login

**Testing**:

- `@SpringBootTest` for integration tests
- `@WebMvcTest` for controller tests
- TestContainers for real databases
- Mockito for unit testing

**Caching & Performance**:

- Cache abstraction with `@Cacheable`
- Redis distributed caching
- Cache strategies and pitfalls

**Async & Events**:

- `@Async` methods with CompletableFuture
- Custom thread pools with TaskExecutor
- Application events for decoupling
- `@Scheduled` tasks for automation

**Real-Time & Advanced Patterns**:

- WebSocket for bidirectional real-time communication
- Server-Sent Events for unidirectional streaming
- API versioning strategies (URL, header, content negotiation)
- Custom argument resolvers for parameter extraction
- Filter vs Interceptor vs AOP comparisons
- Custom annotations with AOP for declarative concerns
- Bean post processors for bean customization
- Custom Spring Boot starters
- Reactive repositories with R2DBC
- Composite keys and embedded IDs

## Next Steps

- [Advanced by-example tutorial](/en/learn/software-engineering/platforms/web/java-spring-boot/tutorials/by-example/advanced) - Microservices, observability, advanced patterns
- [Spring Security Reference](https://docs.spring.io/spring-security/reference/) - Official Spring Security documentation
- [Spring Data JPA Guide](https://docs.spring.io/spring-data/jpa/reference/) - Official Spring Data JPA documentation
