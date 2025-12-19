---
title: Testing with JUnit 5 and Mockito
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 629
description: Write effective unit and integration tests with JUnit 5, Mockito, and Spring Boot Test
tags: ["java", "testing", "junit", "mockito", "spring-boot-test"]
---

## JUnit 5 Basics

```java
import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.*;

class CalculatorTest {
    private Calculator calculator;

    @BeforeEach
    void setUp() {
        calculator = new Calculator();
    }

    @Test
    @DisplayName("Should add two numbers correctly")
    void testAddition() {
        assertEquals(5, calculator.add(2, 3));
    }

    @ParameterizedTest
    @ValueSource(ints = {1, 2, 3, 4, 5})
    void testPositiveNumbers(int number) {
        assertTrue(number > 0);
    }

    @Test
    void testException() {
        assertThrows(ArithmeticException.class, () -> {
            calculator.divide(10, 0);
        });
    }
}
```

## JUnit 5 Advanced Features

### Parameterized Tests

```java
@ParameterizedTest
@CsvSource({
    "apple, 5",
    "banana, 6",
    "orange, 6"
})
void testStringLength(String input, int expected) {
    assertEquals(expected, input.length());
}

@ParameterizedTest
@MethodSource("provideTestData")
void testWithMethodSource(String input, int expected) {
    assertEquals(expected, input.length());
}

static Stream<Arguments> provideTestData() {
    return Stream.of(
        Arguments.of("hello", 5),
        Arguments.of("world", 5),
        Arguments.of("test", 4)
    );
}

@ParameterizedTest
@EnumSource(Month.class)
void testWithEnum(Month month) {
    assertNotNull(month);
    assertTrue(month.getValue() >= 1 && month.getValue() <= 12);
}
```

### Nested Tests

```java
@DisplayName("User service tests")
class UserServiceTest {
    private UserService userService;

    @BeforeEach
    void setUp() {
        userService = new UserService(new InMemoryUserRepository());
    }

    @Nested
    @DisplayName("When user exists")
    class WhenUserExists {
        private User existingUser;

        @BeforeEach
        void createUser() {
            existingUser = userService.create("john", "john@example.com");
        }

        @Test
        @DisplayName("Should find user by username")
        void shouldFindByUsername() {
            Optional<User> found = userService.findByUsername("john");
            assertTrue(found.isPresent());
            assertEquals("john", found.get().getUsername());
        }

        @Test
        @DisplayName("Should update user email")
        void shouldUpdateEmail() {
            userService.updateEmail("john", "newemail@example.com");
            Optional<User> updated = userService.findByUsername("john");
            assertEquals("newemail@example.com", updated.get().getEmail());
        }
    }

    @Nested
    @DisplayName("When user does not exist")
    class WhenUserDoesNotExist {
        @Test
        @DisplayName("Should return empty optional")
        void shouldReturnEmpty() {
            Optional<User> found = userService.findByUsername("nonexistent");
            assertFalse(found.isPresent());
        }

        @Test
        @DisplayName("Should throw exception on update")
        void shouldThrowOnUpdate() {
            assertThrows(UserNotFoundException.class, () -> {
                userService.updateEmail("nonexistent", "email@example.com");
            });
        }
    }
}
```

### Test Lifecycle

```java
class LifecycleTest {
    @BeforeAll
    static void initAll() {
        System.out.println("Before all tests");
    }

    @BeforeEach
    void init() {
        System.out.println("Before each test");
    }

    @Test
    void testOne() {
        System.out.println("Test 1");
    }

    @Test
    void testTwo() {
        System.out.println("Test 2");
    }

    @AfterEach
    void tearDown() {
        System.out.println("After each test");
    }

    @AfterAll
    static void tearDownAll() {
        System.out.println("After all tests");
    }
}
```

## Mockito

### Basic Mocking

```java
import org.mockito.*;
import static org.mockito.Mockito.*;

class UserServiceTest {
    @Mock
    private UserRepository userRepository;

    @InjectMocks
    private UserService userService;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testFindUser() {
        User user = new User("john", "john@example.com");
        when(userRepository.findByUsername("john")).thenReturn(Optional.of(user));

        Optional<User> found = userService.findByUsername("john");

        assertTrue(found.isPresent());
        assertEquals("john", found.get().getUsername());
        verify(userRepository).findByUsername("john");
    }
}
```

### Advanced Mockito Patterns

```java
// Argument matchers
when(userRepository.findById(anyLong())).thenReturn(Optional.of(user));
when(userRepository.findByEmail(contains("@example.com"))).thenReturn(List.of(user));

// Argument captor
@Test
void testArgumentCapture() {
    ArgumentCaptor<User> captor = ArgumentCaptor.forClass(User.class);

    userService.createUser("john", "john@example.com");

    verify(userRepository).save(captor.capture());
    User captured = captor.getValue();
    assertEquals("john", captured.getUsername());
    assertEquals("john@example.com", captured.getEmail());
}

// Multiple calls
when(orderRepository.findAll())
    .thenReturn(List.of(order1))
    .thenReturn(List.of(order1, order2));

// Exception throwing
when(userRepository.findById(999L))
    .thenThrow(new ResourceNotFoundException("User not found"));

// doAnswer for void methods
doAnswer(invocation -> {
    User user = invocation.getArgument(0);
    user.setId(1L);
    return null;
}).when(userRepository).save(any(User.class));
```

### Spy vs Mock

```java
@Test
void testSpy() {
    List<String> list = new ArrayList<>();
    List<String> spyList = spy(list);

    spyList.add("one");
    spyList.add("two");

    assertEquals(2, spyList.size());
    verify(spyList).add("one");
    verify(spyList).add("two");
}

@Test
void testMock() {
    List<String> mockList = mock(List.class);
    when(mockList.size()).thenReturn(100);

    assertEquals(100, mockList.size()); // Returns stubbed value
    verify(mockList).size();
}
```

## Spring Boot Test

### MockMvc Testing

```java
@SpringBootTest
@AutoConfigureMockMvc
class UserControllerTest {
    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private UserService userService;

    @Test
    void testGetUser() throws Exception {
        User user = new User(1L, "john", "john@example.com");
        when(userService.findById(1L)).thenReturn(Optional.of(user));

        mockMvc.perform(get("/api/users/1"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.username").value("john"))
            .andExpect(jsonPath("$.email").value("john@example.com"));
    }

    @Test
    void testCreateUser() throws Exception {
        User user = new User(1L, "jane", "jane@example.com");
        when(userService.create(any(CreateUserRequest.class))).thenReturn(user);

        mockMvc.perform(post("/api/users")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{\"username\":\"jane\",\"email\":\"jane@example.com\"}"))
            .andExpect(status().isCreated())
            .andExpect(jsonPath("$.username").value("jane"));
    }
}
```

## Integration Testing

### Database Integration Tests

```java
@SpringBootTest
@Transactional
class UserRepositoryIntegrationTest {
    @Autowired
    private UserRepository userRepository;

    @Test
    void testSaveAndFind() {
        User user = new User();
        user.setUsername("john");
        user.setEmail("john@example.com");

        User saved = userRepository.save(user);
        assertNotNull(saved.getId());

        Optional<User> found = userRepository.findById(saved.getId());
        assertTrue(found.isPresent());
        assertEquals("john", found.get().getUsername());
    }

    @Test
    void testFindByUsername() {
        User user = new User();
        user.setUsername("jane");
        user.setEmail("jane@example.com");
        userRepository.save(user);

        Optional<User> found = userRepository.findByUsername("jane");
        assertTrue(found.isPresent());
        assertEquals("jane@example.com", found.get().getEmail());
    }
}
```

### TestContainers

```java
@SpringBootTest
@Testcontainers
class UserServiceIntegrationTest {
    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:15")
        .withDatabaseName("testdb")
        .withUsername("test")
        .withPassword("test");

    @DynamicPropertySource
    static void configureProperties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", postgres::getJdbcUrl);
        registry.add("spring.datasource.username", postgres::getUsername);
        registry.add("spring.datasource.password", postgres::getPassword);
    }

    @Autowired
    private UserService userService;

    @Test
    void testCreateUser() {
        User user = userService.create("john", "john@example.com");
        assertNotNull(user.getId());
        assertEquals("john", user.getUsername());
    }

    @Test
    void testFindUser() {
        userService.create("jane", "jane@example.com");
        Optional<User> found = userService.findByUsername("jane");
        assertTrue(found.isPresent());
    }
}
```

### REST API Integration Tests

```java
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class UserApiIntegrationTest {
    @Autowired
    private TestRestTemplate restTemplate;

    @Test
    void testGetUser() {
        ResponseEntity<User> response = restTemplate.getForEntity(
            "/api/users/1",
            User.class
        );

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("john", response.getBody().getUsername());
    }

    @Test
    void testCreateUser() {
        CreateUserRequest request = new CreateUserRequest("jane", "jane@example.com");

        ResponseEntity<User> response = restTemplate.postForEntity(
            "/api/users",
            request,
            User.class
        );

        assertEquals(HttpStatus.CREATED, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("jane", response.getBody().getUsername());
    }
}
```

## Related Resources

**Learn more**:

- [Write Effective Tests](/en/learn/swe/prog-lang/java/how-to/write-effective-tests)
- [Resources](/en/learn/swe/prog-lang/java/reference/resources) - Testing books
