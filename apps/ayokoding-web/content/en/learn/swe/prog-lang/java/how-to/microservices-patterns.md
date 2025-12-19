---
title: Microservices Patterns
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 630
description: Implement service discovery, circuit breakers, and API gateway patterns
tags: ["java", "microservices", "spring-cloud", "resilience"]
---

## Service Discovery (Eureka)

```java
// Eureka Server
@SpringBootApplication
@EnableEurekaServer
public class EurekaServerApplication {
    public static void main(String[] args) {
        SpringApplication.run(EurekaServerApplication.class, args);
    }
}

// Eureka Client
@SpringBootApplication
@EnableDiscoveryClient
public class UserServiceApplication {
    public static void main(String[] args) {
        SpringApplication.run(UserServiceApplication.class, args);
    }
}

// Service-to-service communication
@Service
public class OrderService {
    @Autowired
    private RestTemplate restTemplate;

    public User getUser(Long userId) {
        return restTemplate.getForObject(
            "http://user-service/api/users/" + userId,
            User.class
        );
    }
}
```

## Circuit Breaker (Resilience4j)

### Basic Circuit Breaker

```java
@Service
public class ExternalApiService {
    @CircuitBreaker(name = "externalApi", fallbackMethod = "fallback")
    public String callExternalApi() {
        return restTemplate.getForObject("https://api.example.com/data", String.class);
    }

    private String fallback(Exception e) {
        return "Fallback response";
    }
}
```

### Circuit Breaker Configuration

```yaml
# application.yml
resilience4j:
  circuitbreaker:
    instances:
      externalApi:
        registerHealthIndicator: true
        slidingWindowSize: 10
        minimumNumberOfCalls: 5
        permittedNumberOfCallsInHalfOpenState: 3
        automaticTransitionFromOpenToHalfOpenEnabled: true
        waitDurationInOpenState: 5s
        failureRateThreshold: 50
        eventConsumerBufferSize: 10
```

### Advanced Circuit Breaker with Retry

```java
@Service
public class PaymentService {
    private final RestTemplate restTemplate;

    @CircuitBreaker(name = "payment", fallbackMethod = "paymentFallback")
    @Retry(name = "payment")
    @RateLimiter(name = "payment")
    public PaymentResponse processPayment(PaymentRequest request) {
        return restTemplate.postForObject(
            "https://payment-api.example.com/process",
            request,
            PaymentResponse.class
        );
    }

    private PaymentResponse paymentFallback(PaymentRequest request, Exception e) {
        log.error("Payment processing failed, using fallback", e);
        return PaymentResponse.failed("Service temporarily unavailable");
    }
}
```

### Circuit Breaker Events

```java
@Component
public class CircuitBreakerListener {
    @Autowired
    private CircuitBreakerRegistry circuitBreakerRegistry;

    @PostConstruct
    public void init() {
        circuitBreakerRegistry.circuitBreaker("externalApi")
            .getEventPublisher()
            .onStateTransition(event ->
                log.info("Circuit breaker state changed: {}", event))
            .onError(event ->
                log.error("Circuit breaker error: {}", event))
            .onSuccess(event ->
                log.info("Circuit breaker success: {}", event));
    }
}
```

## API Gateway (Spring Cloud Gateway)

### Basic Gateway Configuration

```java
@SpringBootApplication
public class GatewayApplication {
    public static void main(String[] args) {
        SpringApplication.run(GatewayApplication.class, args);
    }

    @Bean
    public RouteLocator customRouteLocator(RouteLocatorBuilder builder) {
        return builder.routes()
            .route("user-service", r -> r.path("/api/users/**")
                .uri("lb://user-service"))
            .route("order-service", r -> r.path("/api/orders/**")
                .uri("lb://order-service"))
            .build();
    }
}
```

### Advanced Gateway with Filters

```java
@Bean
public RouteLocator advancedRoutes(RouteLocatorBuilder builder) {
    return builder.routes()
        .route("auth-service", r -> r.path("/api/auth/**")
            .filters(f -> f
                .addRequestHeader("X-Gateway", "true")
                .addResponseHeader("X-Response-Time", String.valueOf(System.currentTimeMillis()))
                .retry(3))
            .uri("lb://auth-service"))
        .route("product-service", r -> r.path("/api/products/**")
            .filters(f -> f
                .circuitBreaker(c -> c.setName("productService")
                    .setFallbackUri("forward:/fallback/products"))
                .requestRateLimiter(c -> c
                    .setRateLimiter(redisRateLimiter())
                    .setKeyResolver(userKeyResolver())))
            .uri("lb://product-service"))
        .build();
}

@Bean
public RedisRateLimiter redisRateLimiter() {
    return new RedisRateLimiter(10, 20); // replenishRate, burstCapacity
}

@Bean
public KeyResolver userKeyResolver() {
    return exchange -> Mono.just(
        exchange.getRequest().getHeaders().getFirst("X-User-Id")
    );
}
```

### Global Filters

```java
@Component
public class AuthenticationFilter implements GlobalFilter, Ordered {
    @Override
    public Mono<Void> filter(ServerWebExchange exchange, GatewayFilterChain chain) {
        String token = exchange.getRequest().getHeaders().getFirst("Authorization");

        if (token == null || !isValidToken(token)) {
            exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
            return exchange.getResponse().setComplete();
        }

        return chain.filter(exchange);
    }

    @Override
    public int getOrder() {
        return -1; // High priority
    }

    private boolean isValidToken(String token) {
        // Token validation logic
        return token.startsWith("Bearer ");
    }
}
```

## Distributed Tracing (Sleuth & Zipkin)

### Enable Tracing

```xml
<!-- pom.xml -->
<dependency>
    <groupId>org.springframework.cloud</groupId>
    <artifactId>spring-cloud-starter-sleuth</artifactId>
</dependency>
<dependency>
    <groupId>org.springframework.cloud</groupId>
    <artifactId>spring-cloud-sleuth-zipkin</artifactId>
</dependency>
```

### Configuration

```yaml
# application.yml
spring:
  sleuth:
    sampler:
      probability: 1.0 # Sample all requests (production: 0.1)
  zipkin:
    base-url: http://localhost:9411
    enabled: true
```

### Using Tracing

```java
@Service
public class OrderService {
    private final Tracer tracer;
    private final RestTemplate restTemplate;

    public Order createOrder(OrderRequest request) {
        Span span = tracer.nextSpan().name("create-order").start();
        try {
            span.tag("order.type", request.getType());
            span.tag("order.items", String.valueOf(request.getItems().size()));

            // Business logic
            Order order = new Order();
            order.setItems(request.getItems());

            // Call another service
            User user = fetchUser(request.getUserId());
            order.setUser(user);

            return orderRepository.save(order);
        } finally {
            span.end();
        }
    }

    private User fetchUser(Long userId) {
        return restTemplate.getForObject(
            "http://user-service/api/users/" + userId,
            User.class
        );
    }
}
```

## Config Server Pattern

### Config Server Setup

```java
@SpringBootApplication
@EnableConfigServer
public class ConfigServerApplication {
    public static void main(String[] args) {
        SpringApplication.run(ConfigServerApplication.class, args);
    }
}
```

```yaml
# bootstrap.yml (Config Server)
spring:
  cloud:
    config:
      server:
        git:
          uri: https://github.com/your-org/config-repo
          default-label: main
          search-paths: "{application}"
```

### Config Client Setup

```yaml
# bootstrap.yml (Microservice)
spring:
  application:
    name: user-service
  cloud:
    config:
      uri: http://localhost:8888
      fail-fast: true
      retry:
        max-attempts: 5
```

### Using Configuration

```java
@RestController
@RefreshScope  // Enable dynamic config refresh
public class UserController {
    @Value("${user.service.max-connections}")
    private int maxConnections;

    @Value("${user.service.timeout}")
    private Duration timeout;

    @GetMapping("/config")
    public Map<String, Object> getConfig() {
        return Map.of(
            "maxConnections", maxConnections,
            "timeout", timeout
        );
    }
}
```

## Related Resources

**Learn more**:

- [Resources](/en/learn/swe/prog-lang/java/reference/resources) - Spring Cloud docs
- [Spring Boot Best Practices](/en/learn/swe/prog-lang/java/how-to/spring-boot-best-practices)
