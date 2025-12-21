---
title: "How to Use Dependency Injection with Spring"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000180
description: "Practical techniques for implementing dependency injection using Spring IoC container, bean configuration, and dependency management patterns"
---

## Problem

Managing object dependencies manually leads to tight coupling, difficult testing, and complex initialization logic. Creating dependencies directly in classes makes code hard to modify and test.

```java
// Problematic approach - tight coupling
public class UserService {
    private UserRepository repository = new UserRepositoryImpl(); // Hard to test
    private EmailService emailService = new EmailServiceImpl(); // Hard to mock

    public void createUser(User user) {
        repository.save(user);
        emailService.sendWelcomeEmail(user);
    }
}
```

This guide shows practical techniques for implementing dependency injection with Spring's IoC container.

## Solution

### 1. Constructor Injection (Recommended)

Constructor injection makes dependencies explicit and enables immutability.

**Setup** (Maven dependency):

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter</artifactId>
    <version>3.2.1</version>
</dependency>
```

**Service with constructor injection**:

```java
import org.springframework.stereotype.Service;

@Service
public class UserService {
    private final UserRepository userRepository;
    private final EmailService emailService;

    // Constructor injection - dependencies are final (immutable)
    public UserService(UserRepository userRepository,
                      EmailService emailService) {
        this.userRepository = userRepository;
        this.emailService = emailService;
    }

    public void createUser(User user) {
        userRepository.save(user);
        emailService.sendWelcomeEmail(user);
    }
}

@Repository
public class UserRepositoryImpl implements UserRepository {
    private final JdbcTemplate jdbcTemplate;

    public UserRepositoryImpl(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public void save(User user) {
        String sql = "INSERT INTO users (username, email) VALUES (?, ?)";
        jdbcTemplate.update(sql, user.getUsername(), user.getEmail());
    }
}

@Service
public class EmailServiceImpl implements EmailService {
    private final JavaMailSender mailSender;

    public EmailServiceImpl(JavaMailSender mailSender) {
        this.mailSender = mailSender;
    }

    @Override
    public void sendWelcomeEmail(User user) {
        // Send email implementation
        System.out.println("Sending welcome email to: " + user.getEmail());
    }
}
```

### 2. Bean Configuration

Configure beans using Java-based configuration or annotations.

**Java configuration approach**:

```java
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;
import com.zaxxer.hikari.HikariDataSource;
import javax.sql.DataSource;

@Configuration
public class AppConfig {

    @Bean
    public DataSource dataSource() {
        HikariDataSource dataSource = new HikariDataSource();
        dataSource.setJdbcUrl("jdbc:postgresql://localhost:5432/mydb");
        dataSource.setUsername("user");
        dataSource.setPassword("password");
        dataSource.setMaximumPoolSize(10);
        return dataSource;
    }

    @Bean
    public JdbcTemplate jdbcTemplate(DataSource dataSource) {
        return new JdbcTemplate(dataSource);
    }

    @Bean
    @Profile("dev")
    public EmailService devEmailService() {
        return new ConsoleEmailService(); // Logs to console
    }

    @Bean
    @Profile("prod")
    @Primary
    public EmailService prodEmailService(JavaMailSender mailSender) {
        return new SmtpEmailService(mailSender); // Sends real emails
    }

    @Bean
    public UserService userService(UserRepository userRepository,
                                   EmailService emailService) {
        return new UserService(userRepository, emailService);
    }
}
```

**Component scanning approach**:

```java
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication
@ComponentScan(basePackages = "com.example") // Scan for components
public class Application {
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}

// Beans discovered automatically with stereotype annotations
@Service   // Business logic
@Repository // Data access
@Controller // Web controllers
@Component  // Generic components
```

### 3. Dependency Resolution Patterns

Handle multiple implementations and conditional beans.

**Qualifier for multiple implementations**:

```java
public interface NotificationService {
    void sendNotification(String message);
}

@Service
@Qualifier("email")
public class EmailNotificationService implements NotificationService {
    @Override
    public void sendNotification(String message) {
        System.out.println("Email: " + message);
    }
}

@Service
@Qualifier("sms")
public class SmsNotificationService implements NotificationService {
    @Override
    public void sendNotification(String message) {
        System.out.println("SMS: " + message);
    }
}

@Service
public class NotificationManager {
    private final NotificationService emailService;
    private final NotificationService smsService;

    public NotificationManager(
        @Qualifier("email") NotificationService emailService,
        @Qualifier("sms") NotificationService smsService
    ) {
        this.emailService = emailService;
        this.smsService = smsService;
    }

    public void sendAll(String message) {
        emailService.sendNotification(message);
        smsService.sendNotification(message);
    }
}
```

**Conditional beans**:

```java
import org.springframework.boot.autoconfigure.condition.*;
import org.springframework.context.annotation.Conditional;

@Configuration
public class ConditionalConfig {

    @Bean
    @ConditionalOnProperty(name = "feature.cache.enabled", havingValue = "true")
    public CacheService cacheService() {
        return new RedisCacheService();
    }

    @Bean
    @ConditionalOnMissingBean(CacheService.class)
    public CacheService noCacheService() {
        return new NoOpCacheService(); // Fallback when cache disabled
    }

    @Bean
    @ConditionalOnClass(name = "com.example.AdvancedFeature")
    public AdvancedService advancedService() {
        return new AdvancedServiceImpl();
    }

    @Bean
    @ConditionalOnMissingClass("com.example.AdvancedFeature")
    public AdvancedService basicService() {
        return new BasicServiceImpl();
    }
}
```

### 4. Bean Lifecycle Management

Control bean initialization and destruction.

**Lifecycle callbacks**:

```java
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;

@Service
public class DatabaseService {
    private Connection connection;

    // Method 1: @PostConstruct annotation (recommended)
    @PostConstruct
    public void initialize() {
        System.out.println("Initializing database connection...");
        connection = createConnection();
    }

    @PreDestroy
    public void cleanup() {
        System.out.println("Closing database connection...");
        if (connection != null) {
            connection.close();
        }
    }

    // Method 2: InitializingBean interface
    @Override
    public void afterPropertiesSet() throws Exception {
        // Called after all properties set
        System.out.println("All properties configured");
    }

    // Method 3: DisposableBean interface
    @Override
    public void destroy() throws Exception {
        // Called on bean destruction
        System.out.println("Bean destroyed");
    }

    private Connection createConnection() {
        // Connection creation logic
        return new Connection();
    }
}

// Method 4: Custom init/destroy methods in @Bean
@Configuration
public class Config {
    @Bean(initMethod = "init", destroyMethod = "close")
    public CustomService customService() {
        return new CustomService();
    }
}
```

## How It Works

### Spring IoC Container Lifecycle

```mermaid
graph TD
    A[Application Start] --> B[Load Configuration]
    B --> C[Component Scan]
    C --> D[Bean Definition Registry]
    D --> E[Dependency Resolution]
    E --> F{All Dependencies Available?}
    F -->|Yes| G[Bean Instantiation]
    F -->|No| H[Throw NoSuchBeanDefinitionException]
    G --> I[Dependency Injection]
    I --> J[@PostConstruct Callbacks]
    J --> K[Bean Ready]
    K --> L[Application Running]
    L --> M[Application Shutdown]
    M --> N[@PreDestroy Callbacks]
    N --> O[Beans Destroyed]

    style A fill:#0173B2,stroke:#000000,color:#FFFFFF
    style D fill:#DE8F05,stroke:#000000,color:#FFFFFF
    style F fill:#029E73,stroke:#000000,color:#FFFFFF
    style G fill:#CC78BC,stroke:#000000,color:#FFFFFF
    style K fill:#029E73,stroke:#000000,color:#FFFFFF

%% Color palette: Blue (#0173B2), Orange (#DE8F05), Teal (#029E73), Purple (#CC78BC)
%% Blue = Start, Orange = Registry, Teal = Decision/Success, Purple = Instantiation
```

**Key concepts**:

1. **IoC Container**: Spring ApplicationContext manages bean lifecycle and dependencies
2. **Dependency Injection**: Container injects dependencies automatically (constructor, setter, or field)
3. **Bean Scopes**: Control bean lifecycle (singleton, prototype, request, session, application)
4. **Stereotype Annotations**: @Service, @Repository, @Controller, @Component mark beans for scanning

### Bean Scopes

Spring supports multiple bean scopes:

- **Singleton** (default): One instance per Spring container
- **Prototype**: New instance each time bean is requested
- **Request**: One instance per HTTP request (web apps)
- **Session**: One instance per HTTP session (web apps)
- **Application**: One instance per ServletContext (web apps)

```java
import org.springframework.context.annotation.Scope;
import org.springframework.web.context.WebApplicationContext;

@Service
@Scope("singleton") // Default - single instance
public class SingletonService { }

@Service
@Scope("prototype") // New instance per request
public class PrototypeService { }

@Service
@Scope(WebApplicationContext.SCOPE_REQUEST)
public class RequestScopedService { }
```

## Variations

### Field Injection (Not Recommended)

Field injection is concise but has drawbacks:

```java
import org.springframework.beans.factory.annotation.Autowired;

@Service
public class UserService {
    @Autowired // Field injection - avoid in new code
    private UserRepository userRepository;

    // Issues:
    // 1. Cannot make fields final (no immutability)
    // 2. Hard to test (requires reflection to inject mocks)
    // 3. Hides dependencies (not visible in constructor)
    // 4. Circular dependencies not caught at compile time
}
```

### Setter Injection

Setter injection allows optional dependencies:

```java
@Service
public class UserService {
    private UserRepository userRepository;
    private CacheService cacheService; // Optional dependency

    @Autowired
    public void setUserRepository(UserRepository userRepository) {
        this.userRepository = userRepository; // Required
    }

    @Autowired(required = false)
    public void setCacheService(CacheService cacheService) {
        this.cacheService = cacheService; // Optional
    }
}
```

### Lookup Method Injection

Inject prototype beans into singleton beans:

```java
import org.springframework.beans.factory.annotation.Lookup;

@Service
public abstract class CommandManager {

    @Lookup
    protected abstract Command createCommand(); // Returns prototype bean

    public void process() {
        Command command = createCommand(); // New instance each time
        command.execute();
    }
}

@Component
@Scope("prototype")
public class Command {
    public void execute() {
        System.out.println("Executing command");
    }
}
```

## Common Pitfalls

**Pitfall 1: Circular Dependencies**

Avoid circular dependencies between beans:

```java
// Bad: Circular dependency
@Service
public class ServiceA {
    private final ServiceB serviceB;
    public ServiceA(ServiceB serviceB) { this.serviceB = serviceB; }
}

@Service
public class ServiceB {
    private final ServiceA serviceA;
    public ServiceB(ServiceA serviceA) { this.serviceA = serviceA; }
}
// Throws BeanCurrentlyInCreationException

// Good: Refactor to remove circular dependency
@Service
public class ServiceA {
    private final SharedService sharedService;
    public ServiceA(SharedService sharedService) {
        this.sharedService = sharedService;
    }
}

@Service
public class ServiceB {
    private final SharedService sharedService;
    public ServiceB(SharedService sharedService) {
        this.sharedService = sharedService;
    }
}
```

**Pitfall 2: Missing @Component Annotations**

Beans must be annotated or configured:

```java
// Bad: No annotation - bean not registered
public class MyService {
    // Not managed by Spring
}

// Good: Annotated with stereotype
@Service
public class MyService {
    // Managed by Spring
}

// Or configure in @Configuration class
@Configuration
public class AppConfig {
    @Bean
    public MyService myService() {
        return new MyService();
    }
}
```

**Pitfall 3: Prototype Beans in Singleton**

Prototype beans injected into singletons remain same instance:

```java
// Bad: Prototype bean injected once into singleton
@Service // Singleton scope
public class SingletonService {
    private final PrototypeBean prototypeBean;

    public SingletonService(PrototypeBean prototypeBean) {
        this.prototypeBean = prototypeBean; // Same instance always
    }
}

// Good: Use Provider or Lookup method
@Service
public class SingletonService {
    private final Provider<PrototypeBean> prototypeBeanProvider;

    public SingletonService(Provider<PrototypeBean> prototypeBeanProvider) {
        this.prototypeBeanProvider = prototypeBeanProvider;
    }

    public void doSomething() {
        PrototypeBean bean = prototypeBeanProvider.get(); // New instance
    }
}
```

**Pitfall 4: Field Injection in Tests**

Field injection makes unit testing difficult:

```java
// Bad: Field injection - hard to test
@Service
public class UserService {
    @Autowired
    private UserRepository userRepository; // Cannot mock easily
}

// Good: Constructor injection - easy to test
@Service
public class UserService {
    private final UserRepository userRepository;

    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }
}

// Test with mock
@Test
public void testCreateUser() {
    UserRepository mockRepo = Mockito.mock(UserRepository.class);
    UserService service = new UserService(mockRepo); // Easy to inject mock
    // Test service with mock repository
}
```

## Related Patterns

**Related Tutorial**: See [Beginner Tutorial - Dependency Injection Basics](../tutorials/beginner.md#dependency-injection) for DI fundamentals and [Intermediate Tutorial - Spring Framework](../tutorials/intermediate.md#spring-framework) for Spring IoC container details.

**Related How-To**: See [Write Effective Tests](./write-effective-tests.md) for testing with dependency injection and [Build REST APIs with Spring](./build-rest-apis-spring.md) for DI in web controllers.

**Related Cookbook**: See Cookbook recipes "Spring Bean Configuration", "Conditional Bean Creation", and "Bean Lifecycle Management" for copy-paste ready DI patterns.

**Related Explanation**: See [Best Practices - Dependency Injection](../explanation/best-practices.md#dependency-injection) for DI design principles.

## Further Reading

- [Spring Framework Reference - IoC Container](https://docs.spring.io/spring-framework/reference/core/beans.html) - Official Spring IoC documentation
- [Dependency Injection Principles](https://martinfowler.com/articles/injection.html) - Martin Fowler on DI patterns
- [Spring Boot Auto-Configuration](https://docs.spring.io/spring-boot/reference/using/auto-configuration.html) - Spring Boot DI features
- [Effective Java (Item 5)](https://www.oreilly.com/library/view/effective-java-3rd/9780134686097/) - Prefer dependency injection to hardwiring resources
