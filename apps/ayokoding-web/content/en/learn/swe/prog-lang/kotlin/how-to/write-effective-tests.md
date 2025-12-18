---
title: "How to Test with JUnit and MockK"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 615
description: "Write effective tests using JUnit 5 and MockK mocking framework"
tags: ["kotlin", "testing", "junit", "mockk", "unit-testing"]
categories: ["learn"]
---

## Problem

Effective testing requires unit tests, mocking dependencies, and testing coroutines. JUnit provides the testing framework, but Java mocking libraries don't work well with Kotlin features. MockK is designed specifically for Kotlin.

This guide shows how to write tests with JUnit 5 and MockK.

## Basic Setup

### Project Configuration

Add testing dependencies.

```kotlin
// ✅ build.gradle.kts
dependencies {
  testImplementation(kotlin("test"))
  testImplementation("io.mockk:mockk:1.13.9")
  testImplementation("org.jetbrains.kotlinx:kotlinx-coroutines-test:1.8.0")
}

tasks.test {
  useJUnitPlatform()
}
```

### Simple Test

Write basic JUnit tests.

```kotlin
import kotlin.test.*

class CalculatorTest {
  // ✅ Basic test
  @Test
  fun `add two numbers`() {
    val calculator = Calculator()
    val result = calculator.add(2, 3)
    assertEquals(5, result)
  }

  // ✅ Test with assertions
  @Test
  fun `divide by zero throws exception`() {
    val calculator = Calculator()
    assertFailsWith<ArithmeticException> {
      calculator.divide(10, 0)
    }
  }

  // ✅ Multiple assertions
  @Test
  fun `calculator operations`() {
    val calc = Calculator()

    assertEquals(5, calc.add(2, 3))
    assertEquals(-1, calc.subtract(2, 3))
    assertEquals(6, calc.multiply(2, 3))
    assertEquals(2, calc.divide(6, 3))
  }
}
```

**Pattern**: Use backticks for readable test names, kotlin.test assertions.

## MockK Basics

### Creating Mocks

Mock dependencies with MockK.

```kotlin
import io.mockk.*

// ✅ Interface mock
interface UserRepository {
  fun findById(id: String): User?
  fun save(user: User): User
}

class UserServiceTest {
  // ✅ Create mock
  private val repository = mockk<UserRepository>()
  private val service = UserService(repository)

  @Test
  fun `get user by id`() {
    val user = User("1", "Alice", "alice@example.com")

    // ✅ Define mock behavior
    every { repository.findById("1") } returns user

    // ✅ Call service
    val result = service.getUser("1")

    // ✅ Verify result
    assertEquals(user, result)

    // ✅ Verify mock called
    verify { repository.findById("1") }
  }
}
```

**How it works**: `mockk<T>()` creates mock, `every` defines behavior, `verify` checks calls.

### Mock Behavior

Define various mock responses.

```kotlin
class UserServiceTest {
  private val repository = mockk<UserRepository>()

  @Test
  fun `different mock behaviors`() {
    // ✅ Return value
    every { repository.findById("1") } returns user1

    // ✅ Return null
    every { repository.findById("999") } returns null

    // ✅ Throw exception
    every { repository.save(any()) } throws IllegalStateException("DB error")

    // ✅ Multiple calls return different values
    every { repository.findById("2") } returnsMany listOf(user2, null)

    // ✅ Answer with lambda
    every { repository.save(any()) } answers {
      val user = firstArg<User>()
      user.copy(id = "generated-id")
    }
  }
}
```

## Argument Matching

### Matchers

Match method arguments flexibly.

```kotlin
@Test
fun `argument matchers`() {
  val repository = mockk<UserRepository>()

  // ✅ any() - matches any argument
  every { repository.findById(any()) } returns null

  // ✅ eq() - matches exact value
  every { repository.findById(eq("1")) } returns user1

  // ✅ match - custom matcher
  every {
    repository.save(match { it.email.contains("@") })
  } returns user

  // ✅ Slot for capturing arguments
  val slot = slot<String>()
  every { repository.findById(capture(slot)) } returns user

  repository.findById("123")
  assertEquals("123", slot.captured)  // Captured value
}
```

### Argument Verification

Verify method calls with argument checks.

```kotlin
@Test
fun `verify with arguments`() {
  val repository = mockk<UserRepository>(relaxed = true)
  val service = UserService(repository)

  service.createUser("Alice", "alice@example.com")

  // ✅ Verify exact arguments
  verify { repository.save(match { it.name == "Alice" }) }

  // ✅ Verify call count
  verify(exactly = 1) { repository.save(any()) }

  // ✅ Verify not called
  verify(exactly = 0) { repository.findById(any()) }

  // ✅ Verify at least/at most
  verify(atLeast = 1) { repository.save(any()) }
  verify(atMost = 2) { repository.save(any()) }
}
```

## Setup and Teardown

### Test Lifecycle

Use JUnit lifecycle methods.

```kotlin
class UserServiceTest {
  private lateinit var repository: UserRepository
  private lateinit var service: UserService

  // ✅ Run before each test
  @BeforeEach
  fun setup() {
    repository = mockk()
    service = UserService(repository)
  }

  // ✅ Run after each test
  @AfterEach
  fun teardown() {
    clearAllMocks()
  }

  @Test
  fun `test 1`() {
    // repository and service initialized
  }

  @Test
  fun `test 2`() {
    // Fresh mocks for each test
  }
}
```

### Test Fixtures

Share test data.

```kotlin
class UserServiceTest {
  // ✅ Companion object for shared data
  companion object {
    val testUser = User("1", "Alice", "alice@example.com")
    val testUsers = listOf(
      User("1", "Alice", "alice@example.com"),
      User("2", "Bob", "bob@example.com")
    )

    // ✅ @BeforeAll - runs once for class
    @BeforeAll
    @JvmStatic
    fun setupClass() {
      // One-time setup
    }
  }

  @Test
  fun `use test fixtures`() {
    assertEquals("Alice", testUser.name)
  }
}
```

## Coroutine Testing

### Testing Suspend Functions

Test coroutines with kotlinx-coroutines-test.

```kotlin
import kotlinx.coroutines.test.*
import kotlin.test.Test

class AsyncUserServiceTest {
  @Test
  fun `fetch user asynchronously`() = runTest {
    val repository = mockk<UserRepository>()

    // ✅ Mock suspend function
    coEvery { repository.fetchUserAsync("1") } returns user

    val service = UserService(repository)
    val result = service.fetchUser("1")

    assertEquals(user, result)

    // ✅ Verify suspend function called
    coVerify { repository.fetchUserAsync("1") }
  }

  @Test
  fun `handle async error`() = runTest {
    val repository = mockk<UserRepository>()

    // ✅ Mock suspend function throwing exception
    coEvery { repository.fetchUserAsync(any()) } throws NetworkException()

    val service = UserService(repository)

    assertFailsWith<NetworkException> {
      service.fetchUser("1")
    }
  }
}
```

**Pattern**: Use `coEvery` and `coVerify` for suspend functions, `runTest` for test scope.

### Testing Flow

Test Flow emissions.

```kotlin
import kotlinx.coroutines.flow.flowOf
import kotlinx.coroutines.flow.toList

@Test
fun `test flow emissions`() = runTest {
  val repository = mockk<UserRepository>()

  // ✅ Mock Flow
  val users = listOf(user1, user2, user3)
  every { repository.getAllUsersFlow() } returns flowOf(*users.toTypedArray())

  val service = UserService(repository)
  val result = service.getAllUsers().toList()

  assertEquals(users, result)
}

@Test
fun `test StateFlow updates`() = runTest {
  val viewModel = UserViewModel()

  viewModel.loadUser("1")

  // ✅ Verify StateFlow value
  assertEquals(UiState.Success(user), viewModel.uiState.value)
}
```

## Relaxed Mocks

### Simplifying Mock Setup

Use relaxed mocks for simpler tests.

```kotlin
// ❌ Normal mock - must define every method
val repository = mockk<UserRepository>()
every { repository.findById(any()) } returns null
every { repository.save(any()) } returns user
every { repository.delete(any()) } returns true

// ✅ Relaxed mock - returns defaults
val repository = mockk<UserRepository>(relaxed = true)
// All methods return default values automatically

@Test
fun `relaxed mock example`() {
  val repository = mockk<UserRepository>(relaxed = true)
  val service = UserService(repository)

  // ✅ Only mock what matters
  every { repository.findById("1") } returns user

  service.processUser("1")

  // ✅ Verify only important calls
  verify { repository.findById("1") }
  // Other methods can be called without explicit setup
}
```

**Use case**: When you don't care about most interactions.

## Common Pitfalls

### Forgetting to Mock Dependencies

```kotlin
// ❌ NullPointerException - method not mocked
@Test
fun `unmocked method`() {
  val repository = mockk<UserRepository>()
  // Forgot: every { repository.findById(any()) } returns ...

  val service = UserService(repository)
  val result = service.getUser("1")  // ❌ Crashes!
}

// ✅ Mock all used methods
@Test
fun `properly mocked`() {
  val repository = mockk<UserRepository>()
  every { repository.findById(any()) } returns null

  val service = UserService(repository)
  val result = service.getUser("1")  // ✅ Works
}
```

### Not Using coEvery for Suspend Functions

```kotlin
// ❌ Wrong - every doesn't work with suspend
@Test
fun `wrong mock for suspend function`() = runTest {
  val repository = mockk<UserRepository>()
  every { repository.fetchUserAsync("1") } returns user  // ❌ Won't work!
}

// ✅ Use coEvery
@Test
fun `correct mock for suspend function`() = runTest {
  val repository = mockk<UserRepository>()
  coEvery { repository.fetchUserAsync("1") } returns user  // ✅ Correct
}
```

### Over-Verifying

```kotlin
// ❌ Testing implementation details
@Test
fun `over verification`() {
  service.updateUser("1", update)

  verify { repository.findById("1") }
  verify { repository.save(any()) }
  verify { cache.invalidate("1") }
  verify { logger.info(any()) }
  // Too many implementation details!
}

// ✅ Verify behavior, not implementation
@Test
fun `behavior verification`() {
  service.updateUser("1", update)

  verify { repository.save(match { it.id == "1" }) }
  // Only verify the important outcome
}
```

## Variations

### Spy Objects

Spy on real objects.

```kotlin
// ✅ Spy wraps real object
val realService = UserService(realRepository)
val spy = spyk(realService)

// ✅ Partially mock
every { spy.validateUser(any()) } returns true

// Real methods called unless mocked
spy.createUser("Alice", "alice@example.com")

verify { spy.createUser(any(), any()) }
```

### Object Mocking

Mock Kotlin objects.

```kotlin
object Logger {
  fun log(message: String) {
    println(message)
  }
}

@Test
fun `mock object`() {
  mockkObject(Logger)

  every { Logger.log(any()) } just Runs

  Logger.log("test")

  verify { Logger.log("test") }

  unmockkObject(Logger)
}
```

## Related Patterns

**Learn more**:

- [Coroutines Basics](/en/learn/swe/prog-lang/kotlin/how-to/handle-coroutines-and-async) - Testing async code
- [Intermediate Tutorial - Testing](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#testing-strategies) - Testing strategies
- [Beginner Tutorial - Testing](/en/learn/swe/prog-lang/kotlin/tutorials/beginner#testing-with-junit-5) - Testing fundamentals

**Cookbook recipes**:

- [Unit Testing with JUnit](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#unit-testing-junit) - Quick reference patterns
- [Mocking with MockK](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#mocking-mockk) - Mocking patterns
- [Coroutine Testing](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#coroutine-testing) - Async testing
