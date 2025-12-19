---
title: Reactive Programming with Project Reactor
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 631
description: Master Mono, Flux, and reactive streams for non-blocking applications
tags: ["java", "reactive", "reactor", "webflux"]
---

## Mono and Flux Basics

```java
import reactor.core.publisher.*;

// Mono - 0 or 1 element
Mono<String> mono = Mono.just("Hello");
Mono<String> empty = Mono.empty();
Mono<String> error = Mono.error(new RuntimeException("Error"));

// Flux - 0 to N elements
Flux<Integer> flux = Flux.just(1, 2, 3, 4, 5);
Flux<Integer> range = Flux.range(1, 10);
Flux<Long> interval = Flux.interval(Duration.ofSeconds(1));

// Subscribe
mono.subscribe(
    value -> System.out.println("Value: " + value),
    error -> System.err.println("Error: " + error),
    () -> System.out.println("Complete")
);
```

## Transforming Streams

```java
// Map
Flux<String> uppercase = Flux.just("a", "b", "c")
    .map(String::toUpperCase);

// FlatMap
Flux<String> flattened = Flux.just("user1", "user2")
    .flatMap(userId -> fetchUser(userId));

// Filter
Flux<Integer> evens = Flux.range(1, 10)
    .filter(n -> n % 2 == 0);

// Reduce
Mono<Integer> sum = Flux.range(1, 10)
    .reduce(0, Integer::sum);
```

## Error Handling

```java
Flux<String> handled = Flux.just("1", "2", "invalid", "4")
    .map(Integer::parseInt)
    .onErrorContinue((err, obj) -> {
        System.err.println("Error parsing: " + obj);
    })
    .map(Object::toString);

// Fallback
Mono<String> withFallback = mono
    .onErrorReturn("Default value");

// Retry
Flux<String> retried = flux
    .retry(3)
    .retryWhen(Retry.backoff(3, Duration.ofSeconds(1)));
```

## Advanced Error Handling Patterns

### Error Recovery Strategies

```java
// onErrorResume - switch to alternative publisher
Mono<User> user = fetchUser(userId)
    .onErrorResume(err -> {
        log.error("Failed to fetch user, using cache", err);
        return fetchFromCache(userId);
    });

// onErrorMap - transform error type
Flux<Data> data = fetchData()
    .onErrorMap(IOException.class,
        err -> new ServiceException("Data fetch failed", err));

// doOnError - side effect without changing stream
Flux<Event> events = eventStream
    .doOnError(err -> log.error("Event stream error", err))
    .retry(3);
```

### Timeout and Fallback

```java
Mono<Response> response = externalApiCall()
    .timeout(Duration.ofSeconds(5))
    .onErrorResume(TimeoutException.class,
        err -> Mono.just(Response.defaultResponse()))
    .doOnError(err -> log.error("API call failed", err));
```

## Advanced Flux and Mono Operations

### Combining Publishers

```java
// Zip - combine latest values
Mono<UserProfile> profile = Mono.zip(
    fetchUser(userId),
    fetchOrders(userId),
    fetchPreferences(userId),
    (user, orders, prefs) -> new UserProfile(user, orders, prefs)
);

// Merge - combine multiple streams
Flux<Event> allEvents = Flux.merge(
    userEventStream(),
    systemEventStream(),
    auditEventStream()
);

// Concat - sequential combination
Flux<Data> combined = Flux.concat(
    fetchFromCache(),
    fetchFromDatabase(),
    fetchFromApi()
);
```

### Window and Buffer

```java
// Buffer - collect elements into lists
Flux<List<Integer>> buffered = Flux.range(1, 100)
    .buffer(10); // Groups of 10

// Window - split into multiple Flux
Flux<Flux<Integer>> windowed = Flux.range(1, 100)
    .window(Duration.ofSeconds(1));

// bufferTimeout - by count or time
Flux<List<Event>> batches = eventStream
    .bufferTimeout(100, Duration.ofSeconds(5));
```

## Backpressure Strategies

### Buffer Strategies

```java
// onBackpressureBuffer - buffer with limit
Flux.range(1, 1000)
    .onBackpressureBuffer(100)
    .subscribe(new BaseSubscriber<Integer>() {
        @Override
        protected void hookOnSubscribe(Subscription subscription) {
            request(10); // Request 10 items
        }

        @Override
        protected void hookOnNext(Integer value) {
            System.out.println(value);
            request(1); // Request next item
        }
    });

// onBackpressureDrop - drop excess items
Flux<Event> events = eventProducer()
    .onBackpressureDrop(event ->
        log.warn("Dropped event: {}", event));

// onBackpressureLatest - keep only latest
Flux<Data> latestData = dataStream()
    .onBackpressureLatest();
```

### Custom Backpressure

```java
Flux<Integer> controlled = Flux.create(sink -> {
    for (int i = 1; i <= 1000; i++) {
        sink.next(i);
    }
    sink.complete();
}, FluxSink.OverflowStrategy.BUFFER);
```

## Testing Reactive Code

### StepVerifier for Unit Tests

```java
import reactor.test.StepVerifier;

@Test
void testMonoSuccess() {
    Mono<String> mono = Mono.just("Hello");

    StepVerifier.create(mono)
        .expectNext("Hello")
        .verifyComplete();
}

@Test
void testFluxSequence() {
    Flux<Integer> flux = Flux.just(1, 2, 3, 4, 5);

    StepVerifier.create(flux)
        .expectNext(1, 2, 3, 4, 5)
        .verifyComplete();
}

@Test
void testErrorHandling() {
    Mono<String> mono = Mono.error(new RuntimeException("Error"));

    StepVerifier.create(mono)
        .expectError(RuntimeException.class)
        .verify();
}

@Test
void testWithVirtualTime() {
    StepVerifier.withVirtualTime(() -> Flux.interval(Duration.ofHours(1)).take(3))
        .expectSubscription()
        .thenAwait(Duration.ofHours(3))
        .expectNext(0L, 1L, 2L)
        .verifyComplete();
}
```

### Testing WebFlux Controllers

```java
@WebFluxTest(UserController.class)
class UserControllerTest {
    @Autowired
    private WebTestClient webClient;

    @MockBean
    private UserService userService;

    @Test
    void testGetUser() {
        User user = new User("1", "john", "john@example.com");
        when(userService.findById("1")).thenReturn(Mono.just(user));

        webClient.get()
            .uri("/api/users/1")
            .exchange()
            .expectStatus().isOk()
            .expectBody(User.class)
            .isEqualTo(user);
    }

    @Test
    void testGetAllUsers() {
        Flux<User> users = Flux.just(
            new User("1", "john", "john@example.com"),
            new User("2", "jane", "jane@example.com")
        );
        when(userService.findAll()).thenReturn(users);

        webClient.get()
            .uri("/api/users")
            .exchange()
            .expectStatus().isOk()
            .expectBodyList(User.class)
            .hasSize(2);
    }
}
```

## Spring WebFlux

```java
@RestController
@RequestMapping("/api/users")
public class UserController {
    @Autowired
    private UserService userService;

    @GetMapping("/{id}")
    public Mono<User> getUser(@PathVariable String id) {
        return userService.findById(id);
    }

    @GetMapping
    public Flux<User> getAllUsers() {
        return userService.findAll();
    }

    @PostMapping
    public Mono<User> createUser(@RequestBody User user) {
        return userService.save(user);
    }
}
```

## Related Resources

**Learn more**:

- [Project Reactor Documentation](https://projectreactor.io/)
- [Resources](/en/learn/swe/prog-lang/java/reference/resources) - Reactive programming books
