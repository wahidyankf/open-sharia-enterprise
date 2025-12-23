---
title: "Write Effective Tests"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 1000009
description: "Master JUnit 5 (Jupiter) testing with proper structure, assertions, mocking, and test organization"
tags: ["java", "junit", "testing", "tdd", "mockito"]
categories: ["learn"]
---

## Problem

Tests provide confidence that code works correctly, but poorly written tests create false confidence or become maintenance burdens. Common problems include unclear test structure, inadequate assertions, over-mocking, and testing implementation instead of behavior.

This guide shows how to write effective tests in Java using JUnit 5 (Jupiter).

## Test Structure and Organization

### Use the AAA Pattern

Structure tests with Arrange, Act, Assert for clarity.

```java
// ✅ Clear AAA structure
@Test
void shouldCalculateTotalPriceWithTax() {
  // Arrange - Set up test data
  Order order = new Order();
  order.addItem(new OrderItem("Widget", new BigDecimal("100.00"), 2));
  order.addItem(new OrderItem("Gadget", new BigDecimal("50.00"), 1));
  BigDecimal taxRate = new BigDecimal("0.10");

  // Act - Execute the code under test
  BigDecimal total = order.calculateTotalWithTax(taxRate);

  // Assert - Verify the outcome
  BigDecimal expected = new BigDecimal("275.00");
  assertEquals(0, expected.compareTo(total));
}

// ❌ Unclear structure - everything mixed
@Test
void testOrder() {
  Order order = new Order();
  BigDecimal total = order.calculateTotalWithTax(new BigDecimal("0.10"));
  order.addItem(new OrderItem("Widget", new BigDecimal("100.00"), 2));
  assertEquals(0, new BigDecimal("275.00").compareTo(total)); // Wrong order!
}
```

**Why it matters**: AAA pattern makes tests readable like documentation. Anyone can understand what's being tested, what action triggers behavior, and what outcome is expected. Tests become specifications that explain how code should behave.

### Name Tests Descriptively

Test names should describe the scenario and expected behavior.

```java
// ✅ Descriptive test names
@Test
void shouldThrowExceptionWhenWithdrawingMoreThanBalance() {
  Account account = new Account(new BigDecimal("100.00"));

  assertThrows(InsufficientFundsException.class, () -> {
    account.withdraw(new BigDecimal("150.00"));
  });
}

@Test
void shouldApplyDiscountWhenOrderExceedsMinimumAmount() {
  Order order = new Order();
  order.addItem(new OrderItem("Widget", new BigDecimal("100.00"), 1));

  BigDecimal total = order.calculateTotal();

  assertTrue(total.compareTo(new BigDecimal("90.00")) == 0);
}

// ❌ Unclear names
@Test
void test1() { /* ... */ }

@Test
void testAccount() { /* ... */ }

@Test
void withdrawTest() { /* ... */ }
```

**Naming conventions:**

- `should[ExpectedBehavior]When[Condition]`
- `given[Condition]When[Action]Then[Outcome]`
- `[method]_[scenario]_[expectedResult]`

### Organize Tests by Class

```java
// ✅ Test class organization
class AccountTest {

  @Nested
  @DisplayName("Deposit operations")
  class DepositTests {

    @Test
    void shouldIncreaseBalanceWhenDepositingPositiveAmount() {
      Account account = new Account(new BigDecimal("100.00"));

      account.deposit(new BigDecimal("50.00"));

      assertEquals(0, new BigDecimal("150.00").compareTo(account.getBalance()));
    }

    @Test
    void shouldThrowExceptionWhenDepositingNegativeAmount() {
      Account account = new Account(new BigDecimal("100.00"));

      assertThrows(IllegalArgumentException.class, () -> {
        account.deposit(new BigDecimal("-10.00"));
      });
    }
  }

  @Nested
  @DisplayName("Withdrawal operations")
  class WithdrawalTests {

    @Test
    void shouldDecreaseBalanceWhenWithdrawingValidAmount() {
      Account account = new Account(new BigDecimal("100.00"));

      account.withdraw(new BigDecimal("30.00"));

      assertEquals(0, new BigDecimal("70.00").compareTo(account.getBalance()));
    }

    @Test
    void shouldThrowExceptionWhenWithdrawingMoreThanBalance() {
      Account account = new Account(new BigDecimal("100.00"));

      assertThrows(InsufficientFundsException.class, () -> {
        account.withdraw(new BigDecimal("150.00"));
      });
    }
  }
}
```

## JUnit 5 Basics

### Test Lifecycle Annotations

```java
class LifecycleTest {

  @BeforeAll
  static void setupAll() {
    // Runs once before all tests in class
    // Use for expensive setup (database connection, etc.)
    System.out.println("Setup before all tests");
  }

  @AfterAll
  static void teardownAll() {
    // Runs once after all tests in class
    // Use for cleanup (close connections, etc.)
    System.out.println("Cleanup after all tests");
  }

  @BeforeEach
  void setupEach() {
    // Runs before each test method
    // Use for common test setup
    System.out.println("Setup before each test");
  }

  @AfterEach
  void teardownEach() {
    // Runs after each test method
    // Use for cleanup after each test
    System.out.println("Cleanup after each test");
  }

  @Test
  void firstTest() {
    System.out.println("Running first test");
  }

  @Test
  void secondTest() {
    System.out.println("Running second test");
  }
}

// Output:
// Setup before all tests
// Setup before each test
// Running first test
// Cleanup after each test
// Setup before each test
// Running second test
// Cleanup after each test
// Cleanup after all tests
```

### Assertions

```java
class AssertionTest {

  @Test
  void demonstrateBasicAssertions() {
    // Equality assertions
    assertEquals(5, 2 + 3);
    assertEquals("hello", "hel" + "lo");
    assertNotEquals(5, 2 + 2);

    // Boolean assertions
    assertTrue(5 > 3);
    assertFalse(5 < 3);

    // Null assertions
    assertNull(null);
    assertNotNull(new Object());

    // Same/Not Same (reference equality)
    String str1 = new String("test");
    String str2 = new String("test");
    assertEquals(str1, str2);        // Content equal
    assertNotSame(str1, str2);       // Different objects

    String str3 = "test";
    String str4 = "test";
    assertSame(str3, str4);          // Same object (string interning)
  }

  @Test
  void demonstrateCollectionAssertions() {
    List<String> names = List.of("Alice", "Bob", "Charlie");

    // Collection size
    assertEquals(3, names.size());

    // Contains
    assertTrue(names.contains("Bob"));
    assertFalse(names.contains("Dave"));

    // Array equality
    int[] expected = {1, 2, 3};
    int[] actual = {1, 2, 3};
    assertArrayEquals(expected, actual);
  }

  @Test
  void demonstrateExceptionAssertions() {
    // Assert that exception is thrown
    assertThrows(IllegalArgumentException.class, () -> {
      throw new IllegalArgumentException("Invalid");
    });

    // Assert specific exception with message verification
    IllegalArgumentException exception = assertThrows(
      IllegalArgumentException.class,
      () -> new Account(-100)
    );
    assertTrue(exception.getMessage().contains("balance cannot be negative"));

    // Assert no exception thrown
    assertDoesNotThrow(() -> {
      new Account(100);
    });
  }

  @Test
  void demonstrateAssertionsWithMessages() {
    // ✅ Custom failure messages
    assertEquals(5, actualValue(), "Expected value to be 5");

    // ✅ Lazy message evaluation (only computed on failure)
    assertEquals(5, actualValue(), () ->
      "Expected value to be 5, but got " + actualValue()
    );
  }

  @Test
  void demonstrateAssertAll() {
    // ✅ Assert multiple conditions (all evaluated even if some fail)
    User user = new User("Alice", 30, "alice@example.com");

    assertAll("User properties",
      () -> assertEquals("Alice", user.getName()),
      () -> assertEquals(30, user.getAge()),
      () -> assertEquals("alice@example.com", user.getEmail())
    );
    // If any assertion fails, all results are reported
  }
}
```

## Parameterized Tests

Test the same logic with different inputs.

```java
class ParameterizedTestExample {

  @ParameterizedTest
  @ValueSource(ints = {1, 3, 5, 7, 9})
  void shouldReturnTrueForOddNumbers(int number) {
    assertTrue(isOdd(number));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", "  ", "\t", "\n"})
  void shouldReturnTrueForBlankStrings(String input) {
    assertTrue(input.isBlank());
  }

  @ParameterizedTest
  @CsvSource({
    "1, 1, 2",
    "2, 3, 5",
    "5, 5, 10",
    "10, -5, 5"
  })
  void shouldAddTwoNumbers(int a, int b, int expected) {
    assertEquals(expected, a + b);
  }

  @ParameterizedTest
  @CsvSource({
    "apple, APPLE",
    "hello world, HELLO WORLD",
    "JUnit, JUNIT"
  })
  void shouldConvertToUpperCase(String input, String expected) {
    assertEquals(expected, input.toUpperCase());
  }

  @ParameterizedTest
  @MethodSource("emailProvider")
  void shouldValidateEmailFormat(String email, boolean valid) {
    assertEquals(valid, EmailValidator.isValid(email));
  }

  static Stream<Arguments> emailProvider() {
    return Stream.of(
      Arguments.of("test@example.com", true),
      Arguments.of("invalid.email", false),
      Arguments.of("", false),
      Arguments.of("test@", false),
      Arguments.of("@example.com", false)
    );
  }

  @ParameterizedTest
  @EnumSource(Month.class)
  void shouldHaveValidMonthName(Month month) {
    assertNotNull(month.name());
    assertTrue(month.name().length() > 2);
  }
}
```

**Why it matters**: Parameterized tests eliminate duplication when testing similar scenarios with different data. One test method covers many cases, making tests more maintainable and comprehensive.

## Mocking with Mockito

### Basic Mocking

```java
class UserServiceTest {

  @Test
  void shouldReturnUserWhenFound() {
    // Arrange - Create mock
    UserRepository mockRepository = mock(UserRepository.class);
    User expectedUser = new User("123", "Alice");

    // Configure mock behavior
    when(mockRepository.findById("123")).thenReturn(Optional.of(expectedUser));

    UserService service = new UserService(mockRepository);

    // Act
    User actualUser = service.getUser("123");

    // Assert
    assertEquals(expectedUser, actualUser);
    verify(mockRepository).findById("123"); // Verify method was called
  }

  @Test
  void shouldThrowExceptionWhenUserNotFound() {
    UserRepository mockRepository = mock(UserRepository.class);

    when(mockRepository.findById("999")).thenReturn(Optional.empty());

    UserService service = new UserService(mockRepository);

    assertThrows(UserNotFoundException.class, () -> {
      service.getUser("999");
    });

    verify(mockRepository).findById("999");
  }
}
```

### Mock Annotations

```java
@ExtendWith(MockitoExtension.class)
class OrderServiceTest {

  @Mock
  private OrderRepository orderRepository;

  @Mock
  private PaymentService paymentService;

  @InjectMocks
  private OrderService orderService; // Mocks injected automatically

  @Test
  void shouldProcessOrderSuccessfully() {
    Order order = new Order("order-123", new BigDecimal("100.00"));

    when(orderRepository.save(any(Order.class))).thenReturn(order);
    when(paymentService.processPayment(any(), any())).thenReturn(true);

    OrderResult result = orderService.processOrder(order);

    assertTrue(result.isSuccess());
    verify(orderRepository).save(order);
    verify(paymentService).processPayment(eq(order.getId()), any());
  }
}
```

### Argument Matchers

```java
class ArgumentMatcherTest {

  @Test
  void demonstrateArgumentMatchers() {
    UserRepository mock = mock(UserRepository.class);

    // Any matcher
    when(mock.findById(any(String.class))).thenReturn(Optional.of(new User()));

    // Specific value
    when(mock.findByEmail(eq("test@example.com"))).thenReturn(Optional.of(new User()));

    // Custom matcher
    when(mock.findById(argThat(id -> id.length() > 5))).thenReturn(Optional.of(new User()));

    // Verify with matchers
    UserService service = new UserService(mock);
    service.getUser("user-123");

    verify(mock).findById(startsWith("user-"));
    verify(mock, times(1)).findById(anyString());
    verify(mock, never()).delete(any());
  }
}
```

### Stubbing Multiple Calls

```java
@Test
void shouldHandleMultipleCalls() {
  EmailService mock = mock(EmailService.class);

  // Different returns for consecutive calls
  when(mock.send(anyString()))
    .thenReturn(true)      // First call
    .thenReturn(false)     // Second call
    .thenThrow(new RuntimeException()); // Third call

  assertTrue(mock.send("test1"));
  assertFalse(mock.send("test2"));
  assertThrows(RuntimeException.class, () -> mock.send("test3"));
}
```

### Avoiding Over-Mocking

```java
// ❌ Over-mocking - testing implementation details
@Test
void badTest() {
  List<String> mockList = mock(List.class);
  when(mockList.size()).thenReturn(5);
  when(mockList.get(0)).thenReturn("first");

  // Testing mock behavior, not real code!
  assertEquals(5, mockList.size());
}

// ✅ Mock only external dependencies
@Test
void goodTest() {
  // Use real objects when possible
  List<String> list = new ArrayList<>();
  list.add("first");
  list.add("second");

  assertEquals(2, list.size());
  assertEquals("first", list.get(0));
}

// ✅ Mock external services
@Test
void mockExternalDependencies() {
  // Mock external API client
  ApiClient mockClient = mock(ApiClient.class);
  when(mockClient.fetchData()).thenReturn(testData);

  // Real business logic
  DataProcessor processor = new DataProcessor(mockClient);
  Result result = processor.process();

  assertNotNull(result);
}
```

## When NOT to Test

```java
// ❌ Don't test trivial getters/setters
@Test
void testGettersAndSetters() {
  User user = new User();
  user.setName("Alice");
  assertEquals("Alice", user.getName()); // Waste of time
}

// ❌ Don't test framework code
@Test
void testSpringConfiguration() {
  // Testing that Spring autowiring works
  // Trust the framework!
}

// ❌ Don't test private methods directly
@Test
void testPrivateMethod() throws Exception {
  Method method = MyClass.class.getDeclaredMethod("privateHelper");
  method.setAccessible(true);
  // If it's important, test through public API
}

// ✅ Test public behavior
@Test
void testPublicBehavior() {
  MyClass instance = new MyClass();
  // Public method that uses private helper internally
  String result = instance.processData("input");
  assertEquals("expected", result);
  // Private methods tested indirectly
}

// ✅ Test business logic
@Test
void shouldCalculateDiscountCorrectly() {
  PricingService service = new PricingService();

  BigDecimal price = new BigDecimal("100.00");
  BigDecimal discount = service.calculateDiscount(price, CustomerType.PREMIUM);

  assertEquals(0, new BigDecimal("20.00").compareTo(discount));
}
```

## Test Coverage and TDD Basics

### Meaningful Coverage

```java
// ✅ Cover important paths
class PricingServiceTest {

  @Test
  void shouldApplyNoDiscountForRegularCustomers() {
    // Happy path
  }

  @Test
  void shouldApply10PercentDiscountForPremiumCustomers() {
    // Alternative path
  }

  @Test
  void shouldApply20PercentDiscountForVIPCustomers() {
    // Another alternative
  }

  @Test
  void shouldThrowExceptionForNullPrice() {
    // Error path
  }

  @Test
  void shouldThrowExceptionForNegativePrice() {
    // Edge case
  }
}
```

### TDD Workflow

```java
// ✅ Red-Green-Refactor cycle

// 1. RED - Write failing test first
@Test
void shouldCalculateTotalPrice() {
  ShoppingCart cart = new ShoppingCart();
  cart.addItem(new Item("Widget", 10.00, 2));
  cart.addItem(new Item("Gadget", 5.00, 1));

  assertEquals(25.00, cart.calculateTotal());
  // Test fails - calculateTotal() doesn't exist yet
}

// 2. GREEN - Write minimum code to pass
public class ShoppingCart {
  private List<Item> items = new ArrayList<>();

  public void addItem(Item item) {
    items.add(item);
  }

  public double calculateTotal() {
    return items.stream()
      .mapToDouble(item -> item.getPrice() * item.getQuantity())
      .sum();
  }
}
// Test passes!

// 3. REFACTOR - Improve code while keeping tests green
public double calculateTotal() {
  return items.stream()
    .map(Item::getSubtotal)
    .reduce(0.0, Double::sum);
}
// Tests still pass, code is cleaner
```

## Testing Best Practices

### Test One Thing

```java
// ❌ Testing multiple things
@Test
void testEverything() {
  User user = new User("Alice", "alice@example.com");
  user.setAge(25);

  assertTrue(user.isValid());
  assertEquals("ALICE", user.getUppercaseName());
  assertTrue(user.canVote());
  // Too many assertions
}

// ✅ One test per behavior
@Test
void shouldReturnTrueWhenUserDataIsValid() {
  User user = new User("Alice", "alice@example.com");
  assertTrue(user.isValid());
}

@Test
void shouldReturnUppercaseNameWhenRequested() {
  User user = new User("Alice", "alice@example.com");
  assertEquals("ALICE", user.getUppercaseName());
}

@Test
void shouldAllowVotingWhenAgeIs18OrAbove() {
  User user = new User("Alice", "alice@example.com");
  user.setAge(25);
  assertTrue(user.canVote());
}
```

### Fast Tests

```java
// ✅ Fast unit tests
@Test
void fastTest() {
  Calculator calculator = new Calculator();
  assertEquals(5, calculator.add(2, 3));
  // Runs in milliseconds
}

// ❌ Slow tests (save for integration tests)
@Test
void slowTest() throws InterruptedException {
  Thread.sleep(5000); // Don't do this in unit tests!
  // Use @Tag("integration") for slow tests
}
```

### Independent Tests

```java
// ❌ Tests depend on execution order
static User sharedUser;

@Test
void test1_CreateUser() {
  sharedUser = new User("Alice");
  // BAD: Next test depends on this
}

@Test
void test2_UpdateUser() {
  sharedUser.setAge(25); // Fails if test1 doesn't run first!
}

// ✅ Independent tests
@Test
void shouldCreateUser() {
  User user = new User("Alice");
  assertNotNull(user);
}

@Test
void shouldUpdateUserAge() {
  User user = new User("Alice");
  user.setAge(25);
  assertEquals(25, user.getAge());
}
```

## Summary

Effective testing in Java begins with clear test structure using the AAA pattern - Arrange, Act, Assert. Each test sets up data, executes the code under test, and verifies outcomes. Descriptive test names document expected behavior, turning tests into specifications that explain how code should work.

JUnit 5 provides lifecycle annotations for setup and teardown. Use @BeforeAll for expensive one-time setup, @BeforeEach for common setup before each test, and corresponding @After annotations for cleanup. Organize related tests with @Nested classes to group tests logically and improve readability.

Assertions verify expected outcomes. JUnit 5 provides assertEquals for equality, assertTrue/assertFalse for boolean conditions, assertThrows for exceptions, and assertAll for validating multiple conditions together. Custom failure messages clarify what went wrong when tests fail.

Parameterized tests eliminate duplication when testing the same logic with different data. @ValueSource supplies simple values, @CsvSource handles multiple parameters, @MethodSource provides complex arguments, and @EnumSource tests all enum values. One test method covers many scenarios.

Mockito handles external dependencies without requiring real databases, APIs, or services. Mock only external dependencies - use real objects for business logic. Configure mocks with when/thenReturn, verify method calls with verify(), and use argument matchers for flexible matching. Avoid over-mocking which tests mock behavior instead of real code.

Write tests for public behavior, not implementation details. Don't test trivial getters/setters, framework code, or private methods directly. Focus on business logic and important code paths. Test coverage measures how much code runs during tests, but meaningful coverage tests important behaviors, edge cases, and error paths.

TDD follows Red-Green-Refactor cycles. Write a failing test first (red), implement minimum code to pass (green), then refactor while keeping tests passing. This rhythm produces testable code with built-in test coverage.

Keep tests fast, independent, and focused. Each test should verify one behavior and run in milliseconds. Tests shouldn't depend on execution order or shared state. Fast, reliable tests encourage running them frequently, catching bugs early.

## Related Content

- [Java Best Practices and Design Principles](/en/learn/software-engineering/programming-language/java/explanation/best-practices)
- [How to Implement Proper Exception Handling](/en/learn/software-engineering/programming-language/java/how-to/exception-handling)
- [How to Refactor God Classes](/en/learn/software-engineering/programming-language/java/how-to/refactor-god-classes)
