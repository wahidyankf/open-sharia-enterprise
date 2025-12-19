---
title: Spring Boot Best Practices
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 627
description: Production-ready Spring Boot patterns for REST APIs, dependency injection, and configuration
tags: ["java", "spring-boot", "rest-api", "microservices"]
---

## Project Structure

```
src/main/java/com/example/app/
├── config/          # Configuration classes
├── controller/      # REST controllers
├── service/         # Business logic
├── repository/      # Data access
├── model/           # Domain entities
├── dto/             # Data transfer objects
├── exception/       # Custom exceptions
└── Application.java # Main class
```

## Dependency Injection

```java
// Constructor injection (recommended)
@Service
public class UserService {
    private final UserRepository userRepository;
    private final EmailService emailService;

    public UserService(UserRepository userRepository, EmailService emailService) {
        this.userRepository = userRepository;
        this.emailService = emailService;
    }
}

// Field injection (avoid)
@Service
public class UserService {
    @Autowired  // Not recommended
    private UserRepository userRepository;
}
```

## REST Controller Best Practices

```java
@RestController
@RequestMapping("/api/users")
public class UserController {
    private final UserService userService;

    public UserController(UserService userService) {
        this.userService = userService;
    }

    @GetMapping
    public ResponseEntity<List<UserDto>> getAllUsers() {
        return ResponseEntity.ok(userService.findAll());
    }

    @GetMapping("/{id}")
    public ResponseEntity<UserDto> getUser(@PathVariable Long id) {
        return userService.findById(id)
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }

    @PostMapping
    public ResponseEntity<UserDto> createUser(@Valid @RequestBody CreateUserRequest request) {
        UserDto created = userService.create(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    @PutMapping("/{id}")
    public ResponseEntity<UserDto> updateUser(
            @PathVariable Long id,
            @Valid @RequestBody UpdateUserRequest request) {
        return userService.update(id, request)
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteUser(@PathVariable Long id) {
        userService.delete(id);
        return ResponseEntity.noContent().build();
    }
}
```

## Configuration Management

```java
// application.yml
/*
spring:
  datasource:
    url: ${DB_URL:jdbc:postgresql://localhost:5432/mydb}
    username: ${DB_USER:admin}
    password: ${DB_PASSWORD:admin}
  jpa:
    hibernate:
      ddl-auto: validate
    show-sql: false
*/

// Configuration properties
@ConfigurationProperties(prefix = "app")
@Validated
public class AppProperties {
    @NotBlank
    private String apiKey;

    @Min(1000)
    @Max(10000)
    private int maxConnections;

    // Getters and setters
}

// Enable configuration
@SpringBootApplication
@EnableConfigurationProperties(AppProperties.class)
public class Application {
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
```

## Exception Handling

```java
// Custom exceptions
public class ResourceNotFoundException extends RuntimeException {
    public ResourceNotFoundException(String message) {
        super(message);
    }
}

// Global exception handler
@RestControllerAdvice
public class GlobalExceptionHandler {
    @ExceptionHandler(ResourceNotFoundException.class)
    public ResponseEntity<ErrorResponse> handleNotFound(ResourceNotFoundException ex) {
        ErrorResponse error = new ErrorResponse("NOT_FOUND", ex.getMessage());
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ErrorResponse> handleValidation(MethodArgumentNotValidException ex) {
        List<String> errors = ex.getBindingResult().getFieldErrors().stream()
            .map(err -> err.getField() + ": " + err.getDefaultMessage())
            .toList();

        ErrorResponse error = new ErrorResponse("VALIDATION_ERROR", errors.toString());
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleGeneric(Exception ex) {
        ErrorResponse error = new ErrorResponse("INTERNAL_ERROR", "An error occurred");
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
    }
}

record ErrorResponse(String code, String message) {}
```

## Security Best Practices

### Basic Security Configuration

```java
@Configuration
@EnableWebSecurity
public class SecurityConfig {
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
            .csrf(csrf -> csrf
                .csrfTokenRepository(CookieCsrfTokenRepository.withHttpOnlyFalse()))
            .authorizeHttpRequests(auth -> auth
                .requestMatchers("/api/public/**").permitAll()
                .requestMatchers("/api/admin/**").hasRole("ADMIN")
                .anyRequest().authenticated())
            .oauth2ResourceServer(oauth2 -> oauth2
                .jwt(jwt -> jwt.jwtAuthenticationConverter(jwtConverter())));

        return http.build();
    }

    @Bean
    public JwtAuthenticationConverter jwtConverter() {
        JwtAuthenticationConverter converter = new JwtAuthenticationConverter();
        converter.setJwtGrantedAuthoritiesConverter(
            new JwtGrantedAuthoritiesConverter()
        );
        return converter;
    }
}
```

### Method Security

```java
@Configuration
@EnableMethodSecurity
public class MethodSecurityConfig {
}

@Service
public class UserService {
    @PreAuthorize("hasRole('ADMIN')")
    public void deleteUser(Long id) {
        userRepository.deleteById(id);
    }

    @PreAuthorize("@userSecurity.canAccessUser(#id)")
    public User findById(Long id) {
        return userRepository.findById(id)
            .orElseThrow(() -> new UserNotFoundException(id));
    }

    @PostAuthorize("returnObject.username == authentication.name or hasRole('ADMIN')")
    public User getUser(Long id) {
        return userRepository.findById(id).orElseThrow();
    }
}

@Component
public class UserSecurity {
    public boolean canAccessUser(Long userId) {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        // Custom authorization logic
        return true;
    }
}
```

## Testing Patterns

### Unit Testing Services

```java
@ExtendWith(MockitoExtension.class)
class UserServiceTest {
    @Mock
    private UserRepository userRepository;

    @InjectMocks
    private UserService userService;

    @Test
    void testCreateUser() {
        CreateUserRequest request = new CreateUserRequest("john", "john@example.com");
        User user = new User(1L, "john", "john@example.com");

        when(userRepository.save(any(User.class))).thenReturn(user);

        User created = userService.create(request);

        assertNotNull(created);
        assertEquals("john", created.getUsername());
        verify(userRepository).save(any(User.class));
    }
}
```

### Integration Testing

```java
@SpringBootTest
@TestPropertySource(properties = {
    "spring.datasource.url=jdbc:h2:mem:testdb",
    "spring.jpa.hibernate.ddl-auto=create-drop"
})
class UserServiceIntegrationTest {
    @Autowired
    private UserService userService;

    @Test
    void testUserLifecycle() {
        // Create
        User user = userService.create("john", "john@example.com");
        assertNotNull(user.getId());

        // Read
        Optional<User> found = userService.findById(user.getId());
        assertTrue(found.isPresent());

        // Update
        userService.updateEmail(user.getId(), "newemail@example.com");
        User updated = userService.findById(user.getId()).get();
        assertEquals("newemail@example.com", updated.getEmail());

        // Delete
        userService.delete(user.getId());
        assertFalse(userService.findById(user.getId()).isPresent());
    }
}
```

## Related Resources

**Learn more**:

- [Spring Boot Documentation](https://spring.io/projects/spring-boot)
- [Resources](/en/learn/swe/prog-lang/java/reference/resources) - Spring books

**Related guides**:

- [Exception Handling](/en/learn/swe/prog-lang/java/how-to/exception-handling)
- [Cookbook](/en/learn/swe/prog-lang/java/how-to/cookbook)
