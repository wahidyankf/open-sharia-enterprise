---
title: "Intermediate"
date: 2026-01-31T00:00:00+07:00
draft: false
weight: 10000002
description: "Examples 31-58: Mocking, async testing, and production TDD patterns (40-75% coverage)"
tags: ["tdd", "tutorial", "by-example", "intermediate", "mocking", "async-testing"]
---

This tutorial covers intermediate TDD techniques including test doubles, asynchronous testing, dependency injection, and production testing patterns used in real-world applications.

## Example 31: Introduction to Test Doubles - Stubs

Stubs replace dependencies with controlled implementations that return predetermined values. They enable testing code in isolation from external systems.

```typescript
// Red: Test user service without database
test("getUserName returns name from repository", () => {
  const userService = new UserService(); // => FAILS: needs repository
  expect(userService.getUserName(1)).toBe("Alice");
});
```

**Green: Create stub repository**

```typescript
interface UserRepository {
  // => Interface for dependency
  findById(id: number): { id: number; name: string } | null;
}

class StubUserRepository implements UserRepository {
  // => Stub implementation
  findById(id: number): { id: number; name: string } | null {
    // => Returns hardcoded data
    return { id: 1, name: "Alice" }; // => Predetermined response
  }
}

class UserService {
  constructor(private repository: UserRepository) {} // => Dependency injection

  getUserName(id: number): string {
    const user = this.repository.findById(id); // => Calls stub
    return user ? user.name : "Unknown"; // => Returns name or default
  }
}

test("getUserName returns name from repository", () => {
  const stubRepo = new StubUserRepository(); // => Create stub
  const userService = new UserService(stubRepo); // => Inject stub
  expect(userService.getUserName(1)).toBe("Alice"); // => Test passes
});
```

**Key Takeaway**: Stubs replace dependencies with controllable implementations. Use stubs when you need specific return values without actual dependency behavior.

**Why It Matters**: Stubs enable fast, isolated unit tests without databases or APIs. Google's testing infrastructure research shows stubbed tests run 100-1000x faster than integration tests, enabling rapid Red-Green-Refactor cycles that maintain development velocity.

## Example 32: Test Doubles - Mocks

Mocks verify that specific methods were called with expected arguments. Unlike stubs (which provide data), mocks verify behavior and interactions.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73
graph LR
    A[Test Code]
    B[Mock Object]
    C[Verification]

    A -->|Calls method| B
    B -->|Records call| C
    C -->|Asserts called correctly| A

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
```

**Red: Test email sending behavior**

```typescript
test("notifyUser sends email to correct address", () => {
  const notifier = new UserNotifier(); // => FAILS: needs email service
  notifier.notifyUser(1, "Welcome!");
  // Need to verify email was sent
});
```

**Green: Create mock email service**

```typescript
interface EmailService {
  send(to: string, message: string): void;
}

class MockEmailService implements EmailService {
  calls: Array<{ to: string; message: string }> = []; // => Tracks calls

  send(to: string, message: string): void {
    this.calls.push({ to, message }); // => Records interaction
  }

  wasCalled(): boolean {
    return this.calls.length > 0; // => Verification helper
  }

  wasCalledWith(to: string, message: string): boolean {
    return this.calls.some((call) => call.to === to && call.message === message);
  }
}

class UserNotifier {
  constructor(
    private emailService: EmailService,
    private userRepo: UserRepository,
  ) {}

  notifyUser(userId: number, message: string): void {
    const user = this.userRepo.findById(userId); // => Get user
    if (user) {
      this.emailService.send(user.email, message); // => Send email
    }
  }
}

test("notifyUser sends email to user address", () => {
  const stubRepo = {
    // => Stub for user data
    findById: (id: number) => ({ id, name: "Alice", email: "alice@example.com" }),
  };
  const mockEmail = new MockEmailService(); // => Mock for verification
  const notifier = new UserNotifier(mockEmail, stubRepo);

  notifier.notifyUser(1, "Welcome!"); // => Execute behavior

  expect(mockEmail.wasCalled()).toBe(true); // => Verify email sent
  expect(mockEmail.wasCalledWith("alice@example.com", "Welcome!")).toBe(true);
}); // => Verify correct arguments
```

**Key Takeaway**: Mocks verify behavior (method calls and arguments) while stubs provide data. Use mocks to test interactions between objects.

**Why It Matters**: Mock verification catches integration bugs early. Netflix's microservices architecture relies heavily on mock testing to verify service interactions without deploying full environments, reducing integration testing time from hours to minutes.

## Example 33: Test Doubles - Spies

Spies wrap real implementations to track calls while preserving actual behavior. They combine real functionality with call verification.

**Red: Test logging wrapper**

```typescript
test("processOrder logs order processing", () => {
  const processor = new OrderProcessor(); // => FAILS: needs to verify logging
  processor.processOrder({ id: 1, total: 100 });
  // Need to verify logger was called without replacing logger behavior
});
```

**Green: Create spy logger**

```typescript
interface Logger {
  log(message: string): void;
}

class ConsoleLogger implements Logger {
  log(message: string): void {
    console.log(message); // => Real implementation
  }
}

class SpyLogger implements Logger {
  private realLogger: Logger; // => Wraps real logger
  calls: string[] = []; // => Tracks calls

  constructor(realLogger: Logger) {
    this.realLogger = realLogger;
  }

  log(message: string): void {
    this.calls.push(message); // => Record call
    this.realLogger.log(message); // => Delegate to real logger
  }
}

class OrderProcessor {
  constructor(private logger: Logger) {}

  processOrder(order: { id: number; total: number }): void {
    this.logger.log(`Processing order ${order.id}`); // => Log action
    // ... processing logic ...
    this.logger.log(`Order ${order.id} completed: $${order.total}`);
  }
}

test("processOrder logs start and completion", () => {
  const spyLogger = new SpyLogger(new ConsoleLogger()); // => Spy wraps real logger
  const processor = new OrderProcessor(spyLogger);

  processor.processOrder({ id: 1, total: 100 });

  expect(spyLogger.calls).toContain("Processing order 1"); // => Verify first log
  expect(spyLogger.calls).toContain("Order 1 completed: $100"); // => Verify second log
  expect(spyLogger.calls.length).toBe(2); // => Verify call count
});
```

**Key Takeaway**: Spies track calls while preserving real behavior. Use spies when you need both actual functionality and call verification.

**Why It Matters**: Spies enable testing side effects without mocking. Airbnb's testing guidelines prefer spies over mocks for logging and analytics because spies catch real implementation bugs while still verifying interactions.

## Example 34: Test Doubles - Fakes

Fakes are lightweight, working implementations that replace complex dependencies. They behave realistically but use simplified logic.

**Red: Test user service with database**

```typescript
test("createUser stores user in database", () => {
  const userService = new UserService(); // => FAILS: needs database
  const user = userService.createUser("alice", "alice@example.com");
  expect(userService.findById(user.id)).toEqual(user);
});
```

**Green: Create fake in-memory database**

```typescript
interface Database {
  insert(table: string, data: any): number;
  findById(table: string, id: number): any;
}

class FakeDatabase implements Database {
  // => Fake implementation
  private data: Map<string, Map<number, any>> = new Map(); // => In-memory storage
  private nextId = 1;

  insert(table: string, data: any): number {
    if (!this.data.has(table)) {
      this.data.set(table, new Map()); // => Create table if needed
    }
    const id = this.nextId++; // => Generate ID
    const record = { ...data, id }; // => Add ID to record
    this.data.get(table)!.set(id, record); // => Store record
    return id; // => Return generated ID
  }

  findById(table: string, id: number): any {
    return this.data.get(table)?.get(id); // => Retrieve record
  }
}

class UserService {
  constructor(private db: Database) {}

  createUser(name: string, email: string): { id: number; name: string; email: string } {
    const id = this.db.insert("users", { name, email }); // => Insert user
    return { id, name, email }; // => Return created user
  }

  findById(id: number): any {
    return this.db.findById("users", id); // => Retrieve user
  }
}

test("createUser stores and retrieves user", () => {
  const fakeDb = new FakeDatabase(); // => Fake database
  const userService = new UserService(fakeDb);

  const user = userService.createUser("alice", "alice@example.com");

  expect(user.id).toBe(1); // => Verify ID generated
  expect(userService.findById(1)).toEqual({
    // => Verify stored correctly
    id: 1,
    name: "alice",
    email: "alice@example.com",
  });
});
```

**Key Takeaway**: Fakes implement realistic behavior with simplified logic. Use fakes for complex dependencies like databases or file systems when you need working behavior without full infrastructure.

**Why It Matters**: Fakes enable fast integration testing without infrastructure setup. Martin Fowler's testing patterns show that fake implementations reduce test suite execution time by 90% compared to real databases while maintaining high confidence in integration logic.

## Example 35: Dependency Injection for Testability

Dependency injection makes code testable by allowing dependencies to be swapped with test doubles. Constructor injection is the most explicit pattern.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73
graph TD
    A[Production Code]
    B[Real Dependencies]
    C[Test Code]
    D[Test Doubles]
    E[Service Under Test]

    A -->|Injects| B
    B -->|Used by| E
    C -->|Injects| D
    D -->|Used by| E

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#000
    style E fill:#CA9161,stroke:#000,color:#000
```

**Red: Hard-coded dependency (untestable)**

```typescript
class PaymentProcessor {
  process(amount: number): boolean {
    const gateway = new PayPalGateway(); // => FAIL: Hard-coded dependency
    return gateway.charge(amount); // => Cannot test without real PayPal
  }
}
```

**Green: Constructor injection**

```typescript
interface PaymentGateway {
  charge(amount: number): boolean;
}

class PayPalGateway implements PaymentGateway {
  charge(amount: number): boolean {
    // Real PayPal API call
    return true; // => Actual implementation
  }
}

class MockPaymentGateway implements PaymentGateway {
  chargeAttempts: number[] = []; // => Track calls

  charge(amount: number): boolean {
    this.chargeAttempts.push(amount); // => Record attempt
    return amount < 1000; // => Mock logic: success under $1000
  }
}

class PaymentProcessor {
  constructor(private gateway: PaymentGateway) {} // => Dependency injected

  process(amount: number): boolean {
    return this.gateway.charge(amount); // => Uses injected gateway
  }
}

test("process charges correct amount", () => {
  const mockGateway = new MockPaymentGateway(); // => Test double
  const processor = new PaymentProcessor(mockGateway); // => Inject mock

  const result = processor.process(500);

  expect(result).toBe(true); // => Verify success
  expect(mockGateway.chargeAttempts).toEqual([500]); // => Verify amount
});

test("process handles large amounts", () => {
  const mockGateway = new MockPaymentGateway();
  const processor = new PaymentProcessor(mockGateway);

  const result = processor.process(1500); // => Over limit

  expect(result).toBe(false); // => Verify failure
});
```

**Key Takeaway**: Inject dependencies through constructors instead of creating them internally. This makes code testable by allowing test doubles to replace real dependencies.

**Why It Matters**: Hard-coded dependencies make testing impossible without hitting real services. Spotify's architecture guidelines mandate dependency injection after discovering that untestable code with hard-coded dependencies accumulated 3x more bugs than injectable code.

## Example 36: Testing Promises - Basic Resolution

Promises represent asynchronous operations. TDD requires testing both resolution and rejection paths with proper async handling.

**Red: Test async function**

```typescript
test("fetchUser returns user data", async () => {
  // => FAILS: fetchUser not defined
  const user = await fetchUser(1);
  expect(user.name).toBe("Alice");
});
```

**Green: Minimal async implementation**

```typescript
async function fetchUser(id: number): Promise<{ id: number; name: string }> {
  // => Returns Promise
  return { id, name: "Alice" }; // => Resolved value
}

test("fetchUser returns user data", async () => {
  // => async test function
  const user = await fetchUser(1); // => Await promise
  expect(user.name).toBe("Alice"); // => Assert on resolved value
});
```

**Refactored: Test both success and failure**

```typescript
async function fetchUser(id: number): Promise<{ id: number; name: string }> {
  if (id <= 0) {
    // => Validation
    throw new Error("Invalid user ID"); // => Promise rejection
  }
  return { id, name: "Alice" }; // => Promise resolution
}

describe("fetchUser", () => {
  test("resolves with user data for valid ID", async () => {
    const user = await fetchUser(1);
    expect(user).toEqual({ id: 1, name: "Alice" });
  });

  test("rejects with error for invalid ID", async () => {
    // => Test rejection
    await expect(fetchUser(-1)).rejects.toThrow("Invalid user ID");
  }); // => Expects promise rejection
});
```

**Key Takeaway**: Mark test functions `async` to use `await`. Test both promise resolution (success) and rejection (failure) paths.

**Why It Matters**: Untested promise rejections cause unhandled errors in production. Node.js processes crash on unhandled promise rejections by default - Uber's incident reports show 35% of their service outages stemmed from untested async error paths.

## Example 37: Testing Async/Await Patterns

Async/await makes asynchronous code readable but requires careful error handling. TDD ensures try-catch blocks work correctly.

**Red: Test async error handling**

```typescript
test("processData handles errors gracefully", async () => {
  const result = await processData("invalid"); // => FAILS: processData not defined
  expect(result).toEqual({ success: false, error: "Invalid data" });
});
```

**Green: Async error handling implementation**

```typescript
async function processData(data: string): Promise<{ success: boolean; error?: string }> {
  try {
    // => Try block
    if (data === "invalid") {
      throw new Error("Invalid data"); // => Simulated error
    }
    return { success: true }; // => Success case
  } catch (error) {
    // => Catch block
    return { success: false, error: (error as Error).message };
  } // => Error case
}

test("processData returns success for valid data", async () => {
  const result = await processData("valid");
  expect(result).toEqual({ success: true });
});

test("processData handles errors gracefully", async () => {
  const result = await processData("invalid");
  expect(result).toEqual({ success: false, error: "Invalid data" });
});
```

**Refactored: Test complex async workflows**

```typescript
async function fetchAndProcess(id: number): Promise<{ data: string; processed: boolean }> {
  const response = await fetch(`/api/data/${id}`); // => Async operation 1
  const rawData = await response.json(); // => Async operation 2

  if (!rawData.valid) {
    throw new Error("Invalid response");
  }

  return { data: rawData.content, processed: true }; // => Combined result
}

test("fetchAndProcess chains async operations", async () => {
  // Mock global fetch
  global.fetch = jest.fn(() =>
    Promise.resolve({
      json: () => Promise.resolve({ valid: true, content: "test data" }),
    }),
  ) as jest.Mock;

  const result = await fetchAndProcess(1);

  expect(result).toEqual({ data: "test data", processed: true });
  expect(fetch).toHaveBeenCalledWith("/api/data/1"); // => Verify fetch called
});
```

**Key Takeaway**: Test async error handling explicitly with try-catch blocks. Mock async dependencies (like fetch) to test async workflows without network calls.

**Why It Matters**: Async error handling bugs cascade through promise chains. Airbnb's frontend reliability improved 50% after implementing mandatory async error path testing, catching errors before they reached users.

## Example 38: Testing Callbacks

Callbacks represent older async patterns. TDD with callbacks requires done callbacks in test frameworks to handle asynchronous assertions.

**Red: Test callback-based function**

```typescript
test("readFile calls callback with data", (done) => {
  // => done parameter for async test
  readFile("data.txt", (error, data) => {
    // => FAILS: readFile not defined
    expect(error).toBeNull();
    expect(data).toBe("file contents");
    done(); // => Signal test completion
  });
});
```

**Green: Callback implementation**

```typescript
function readFile(path: string, callback: (error: Error | null, data: string | null) => void): void {
  // => Callback signature
  if (path.includes("error")) {
    callback(new Error("File not found"), null); // => Error case
  } else {
    callback(null, "file contents"); // => Success case
  }
}

test("readFile calls callback with data", (done) => {
  readFile("data.txt", (error, data) => {
    expect(error).toBeNull(); // => No error
    expect(data).toBe("file contents"); // => Has data
    done(); // => Must call done() to pass
  });
});
```

**Refactored: Test both success and error callbacks**

```typescript
describe("readFile", () => {
  test("calls callback with data on success", (done) => {
    readFile("data.txt", (error, data) => {
      expect(error).toBeNull();
      expect(data).toBe("file contents");
      done(); // => Signal completion
    });
  });

  test("calls callback with error on failure", (done) => {
    readFile("error.txt", (error, data) => {
      expect(error).toBeInstanceOf(Error); // => Has error
      expect(error?.message).toBe("File not found");
      expect(data).toBeNull(); // => No data
      done();
    });
  });

  test("handles callback errors properly", (done) => {
    readFile("data.txt", (error, data) => {
      try {
        expect(data).toBe("file contents"); // => Assertion
        done(); // => Success path
      } catch (assertionError) {
        done(assertionError); // => Fail test with assertion error
      }
    });
  });
});
```

**Key Takeaway**: Use `done` callback parameter in callback-based tests. Call `done()` to signal test completion, or `done(error)` to fail the test.

**Why It Matters**: Callback testing requires explicit completion signaling. Forgotten `done()` calls cause tests to timeout instead of passing/failing, creating false positives that Netflix's testing team identified as 15% of their flaky tests.

## Example 39: Testing Timers and Delays

Code with timers (setTimeout, setInterval) runs slowly in tests. Use fake timers to control time and test time-based logic instantly.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73
graph LR
    A[Real Timer]
    B[Fake Timer]
    C[Instant Test]

    A -->|Takes 5 seconds| C
    B -->|Takes 0 seconds| C

    style A fill:#DE8F05,stroke:#000,color:#fff
    style B fill:#029E73,stroke:#000,color:#fff
    style C fill:#0173B2,stroke:#000,color:#fff
```

**Red: Test with real timer (slow)**

```typescript
test("debounce delays function call", (done) => {
  // => FAILS: debounce not defined
  let called = false;
  const fn = debounce(() => {
    called = true;
  }, 1000); // => 1 second delay

  fn();
  setTimeout(() => {
    expect(called).toBe(true);
    done(); // => Test takes 1+ seconds
  }, 1100);
});
```

**Green: Implementation with fake timers**

```typescript
function debounce(fn: () => void, delay: number): () => void {
  let timeoutId: NodeJS.Timeout | null = null;

  return () => {
    if (timeoutId) clearTimeout(timeoutId); // => Clear existing timer
    timeoutId = setTimeout(fn, delay); // => Set new timer
  };
}

describe("debounce", () => {
  beforeEach(() => {
    jest.useFakeTimers(); // => Enable fake timers
  });

  afterEach(() => {
    jest.useRealTimers(); // => Restore real timers
  });

  test("delays function call by specified time", () => {
    let called = false;
    const fn = debounce(() => {
      called = true;
    }, 1000);

    fn(); // => Schedule call
    expect(called).toBe(false); // => Not called immediately

    jest.advanceTimersByTime(999); // => Fast-forward 999ms
    expect(called).toBe(false); // => Still not called

    jest.advanceTimersByTime(1); // => Fast-forward 1ms (total 1000ms)
    expect(called).toBe(true); // => Now called
  }); // => Test runs instantly

  test("cancels previous timer on rapid calls", () => {
    let callCount = 0;
    const fn = debounce(() => {
      callCount++;
    }, 1000);

    fn(); // => First call
    jest.advanceTimersByTime(500);
    fn(); // => Second call (cancels first)
    jest.advanceTimersByTime(500);
    expect(callCount).toBe(0); // => Neither fired yet

    jest.advanceTimersByTime(500); // => Complete second delay
    expect(callCount).toBe(1); // => Only second call executed
  });
});
```

**Key Takeaway**: Use `jest.useFakeTimers()` to control time in tests. Fast-forward time with `jest.advanceTimersByTime()` to test timer logic instantly.

**Why It Matters**: Real timers make tests slow and flaky. Google's testing infrastructure uses fake timers universally, reducing timer-based test execution from minutes to milliseconds while eliminating timing-related flakiness.

## Example 40: Testing HTTP Requests - Mocking Fetch

HTTP requests need mocking in unit tests to avoid network dependencies. Mock fetch to test HTTP client logic without real API calls.

**Red: Test HTTP client**

```typescript
test("fetchUserData retrieves user from API", async () => {
  // => FAILS: fetchUserData not defined
  const user = await fetchUserData(1);
  expect(user.name).toBe("Alice");
});
```

**Green: Mock fetch implementation**

```typescript
async function fetchUserData(id: number): Promise<{ id: number; name: string }> {
  const response = await fetch(`/api/users/${id}`); // => HTTP call
  return response.json(); // => Parse JSON
}

describe("fetchUserData", () => {
  beforeEach(() => {
    global.fetch = jest.fn(); // => Mock fetch globally
  });

  afterEach(() => {
    jest.restoreAllMocks(); // => Clean up mocks
  });

  test("retrieves user data from API", async () => {
    (global.fetch as jest.Mock).mockResolvedValue({
      // => Mock response
      json: async () => ({ id: 1, name: "Alice" }),
    });

    const user = await fetchUserData(1);

    expect(user).toEqual({ id: 1, name: "Alice" }); // => Verify result
    expect(fetch).toHaveBeenCalledWith("/api/users/1"); // => Verify URL
  });

  test("handles API errors", async () => {
    (global.fetch as jest.Mock).mockRejectedValue(new Error("Network error"));

    await expect(fetchUserData(1)).rejects.toThrow("Network error");
  });
});
```

**Refactored: Test with dependency injection**

```typescript
interface HttpClient {
  get(url: string): Promise<any>;
}

class FetchHttpClient implements HttpClient {
  async get(url: string): Promise<any> {
    const response = await fetch(url); // => Real fetch
    return response.json();
  }
}

class MockHttpClient implements HttpClient {
  responses: Map<string, any> = new Map(); // => Predefined responses

  async get(url: string): Promise<any> {
    if (this.responses.has(url)) {
      return this.responses.get(url); // => Return mock data
    }
    throw new Error(`No mock response for ${url}`);
  }
}

class UserService {
  constructor(private http: HttpClient) {} // => Inject HTTP client

  async fetchUserData(id: number): Promise<{ id: number; name: string }> {
    return this.http.get(`/api/users/${id}`);
  }
}

test("fetchUserData uses HTTP client", async () => {
  const mockHttp = new MockHttpClient();
  mockHttp.responses.set("/api/users/1", { id: 1, name: "Alice" });

  const userService = new UserService(mockHttp);
  const user = await userService.fetchUserData(1);

  expect(user).toEqual({ id: 1, name: "Alice" });
});
```

**Key Takeaway**: Mock fetch with `jest.fn()` or inject HTTP client for better testability. Avoid real network calls in unit tests.

**Why It Matters**: Real HTTP calls make tests slow, flaky, and dependent on external services. Twitter's testing guidelines mandate HTTP mocking after measuring that mocked tests run 50x faster and have 99% fewer transient failures than tests hitting real APIs.

## Example 41: Testing with In-Memory Databases

Database tests are faster with in-memory databases than real databases. In-memory DBs provide realistic behavior without I/O overhead.

**Red: Test database operations**

```typescript
test("saveUser persists user to database", async () => {
  const userRepo = new UserRepository(); // => FAILS: needs database
  await userRepo.save({ name: "Alice", email: "alice@example.com" });
  const users = await userRepo.findAll();
  expect(users).toHaveLength(1);
});
```

**Green: In-memory database implementation**

```typescript
interface User {
  id?: number;
  name: string;
  email: string;
}

class InMemoryUserRepository {
  private users: User[] = []; // => In-memory storage
  private nextId = 1;

  async save(user: User): Promise<User> {
    const savedUser = { ...user, id: this.nextId++ }; // => Add ID
    this.users.push(savedUser); // => Store in memory
    return savedUser;
  }

  async findAll(): Promise<User[]> {
    return [...this.users]; // => Return copy
  }

  async findById(id: number): Promise<User | null> {
    return this.users.find((u) => u.id === id) || null;
  }

  async deleteAll(): Promise<void> {
    this.users = []; // => Clear storage
    this.nextId = 1;
  }
}

describe("UserRepository", () => {
  let userRepo: InMemoryUserRepository;

  beforeEach(() => {
    userRepo = new InMemoryUserRepository(); // => Fresh repository
  });

  test("saveUser persists user", async () => {
    await userRepo.save({ name: "Alice", email: "alice@example.com" });
    const users = await userRepo.findAll();

    expect(users).toHaveLength(1);
    expect(users[0].name).toBe("Alice");
  });

  test("findById retrieves correct user", async () => {
    const saved = await userRepo.save({ name: "Alice", email: "alice@example.com" });
    const found = await userRepo.findById(saved.id!);

    expect(found).toEqual(saved);
  });

  test("multiple saves maintain data", async () => {
    await userRepo.save({ name: "Alice", email: "alice@example.com" });
    await userRepo.save({ name: "Bob", email: "bob@example.com" });

    const users = await userRepo.findAll();
    expect(users).toHaveLength(2);
  });
});
```

**Key Takeaway**: Use in-memory implementations for fast database testing. Reset state in `beforeEach` to ensure test isolation.

**Why It Matters**: In-memory databases enable rapid testing without infrastructure setup. LinkedIn's data team reports 100x speedup using in-memory databases for tests versus Docker-based database instances, enabling developers to run full test suites in seconds instead of minutes.

## Example 42: Property-Based Testing Introduction

Property-based testing generates random inputs to verify properties hold for all cases. It catches edge cases traditional example-based tests miss.

**Red: Traditional example-based test**

```typescript
test("reverse reverses strings", () => {
  expect(reverse("hello")).toBe("olleh"); // => FAILS: reverse not defined
  expect(reverse("world")).toBe("dlrow");
});
```

**Green: Basic implementation**

```typescript
function reverse(str: string): string {
  return str.split("").reverse().join("");
}
```

**Refactored: Property-based test**

```typescript
import fc from "fast-check"; // => Property-based testing library

describe("reverse", () => {
  test("reversing twice returns original", () => {
    fc.assert(
      // => Property assertion
      fc.property(
        fc.string(), // => Generate random strings
        (str) => {
          const reversed = reverse(reverse(str)); // => Reverse twice
          return reversed === str; // => Property: equals original
        },
      ),
    ); // => Tests 100+ random strings
  });

  test("length is preserved", () => {
    fc.assert(
      fc.property(fc.string(), (str) => {
        const reversed = reverse(str);
        return reversed.length === str.length; // => Length property
      }),
    );
  });

  test("first char becomes last char", () => {
    fc.assert(
      fc.property(
        fc.string({ minLength: 1 }), // => Non-empty strings
        (str) => {
          const reversed = reverse(str);
          return str[0] === reversed[reversed.length - 1]; // => Position property
        },
      ),
    );
  });
});
```

**Key Takeaway**: Property-based tests verify invariants (properties that always hold) with random inputs. Use `fast-check` to generate test cases automatically.

**Why It Matters**: Property-based testing finds edge cases developers don't think of. Dropbox discovered critical file sync bugs using property-based tests that ran billions of scenarios, catching race conditions that would take years to encounter in manual testing.

## Example 43: Mutation Testing Concepts

Mutation testing verifies test quality by introducing bugs (mutations) and checking if tests catch them. It identifies weak test coverage.

**Red: Test with weak assertions**

```typescript
test("calculateDiscount applies discount", () => {
  const result = calculateDiscount(100, 0.1); // => FAILS: calculateDiscount not defined
  expect(result).toBeDefined(); // => WEAK: Only checks defined
});
```

**Green: Implementation passes weak test**

```typescript
function calculateDiscount(price: number, rate: number): number {
  return price - price * rate; // => Correct implementation
}

test("calculateDiscount applies discount", () => {
  const result = calculateDiscount(100, 0.1);
  expect(result).toBeDefined(); // => Passes but doesn't verify correctness
});
```

**Mutation: Buggy implementation still passes**

```typescript
function calculateDiscount(price: number, rate: number): number {
  return price; // => MUTATION: Removed discount calculation
} // => Weak test doesn't catch this bug!

test("calculateDiscount applies discount", () => {
  const result = calculateDiscount(100, 0.1);
  expect(result).toBeDefined(); // => Still passes! (mutation survived)
});
```

**Refactored: Strong test catches mutations**

```typescript
describe("calculateDiscount", () => {
  test("applies correct discount percentage", () => {
    const result = calculateDiscount(100, 0.1);
    expect(result).toBe(90); // => STRONG: Verifies exact value
  }); // => Would catch discount removal

  test("applies different discount rates", () => {
    expect(calculateDiscount(100, 0.2)).toBe(80); // => 20% discount
    expect(calculateDiscount(100, 0.5)).toBe(50); // => 50% discount
  }); // => Multiple assertions increase mutation detection

  test("handles zero discount", () => {
    expect(calculateDiscount(100, 0)).toBe(100); // => Edge case
  });

  test("handles full discount", () => {
    expect(calculateDiscount(100, 1)).toBe(0); // => Another edge
  });
});

function calculateDiscount(price: number, rate: number): number {
  return price - price * rate; // => Correct implementation
}
```

**Key Takeaway**: Mutation testing reveals weak tests by introducing bugs. Write specific assertions that would fail if logic changes. Tools like Stryker automate mutation testing.

**Why It Matters**: Weak tests create false confidence. Facebook's testing research shows that codebases with high mutation score (95%+ of mutations caught) have 60% fewer production bugs than codebases with low mutation scores despite similar line coverage.

## Example 44: Test Coverage Analysis

Test coverage measures which code is executed during tests. Aim for high coverage but recognize that 100% coverage doesn't guarantee correct behavior.

**Red: Uncovered code path**

```typescript
function processPayment(amount: number, method: string): string {
  // => FAILS: processPamyent not fully covered
  if (amount < 0) {
    throw new Error("Invalid amount"); // => Edge case not tested
  }

  if (method === "credit") {
    return "Processed via credit card";
  } else if (method === "debit") {
    return "Processed via debit card"; // => Not tested
  }

  return "Unknown payment method"; // => Not tested
}

test("processPayment handles credit cards", () => {
  expect(processPayment(100, "credit")).toBe("Processed via credit card");
}); // => Only covers one path
```

**Green: Full coverage achieved**

```typescript
describe("processPayment", () => {
  test("processes credit card payments", () => {
    expect(processPayment(100, "credit")).toBe("Processed via credit card");
  }); // => Covers credit path

  test("processes debit card payments", () => {
    expect(processPayment(100, "debit")).toBe("Processed via debit card");
  }); // => Covers debit path

  test("handles unknown payment methods", () => {
    expect(processPayment(100, "cash")).toBe("Unknown payment method");
  }); // => Covers default path

  test("rejects negative amounts", () => {
    expect(() => processPayment(-10, "credit")).toThrow("Invalid amount");
  }); // => Covers error path
}); // => 100% coverage achieved
```

**Coverage Report Analysis**

```typescript
// Coverage metrics:
// Line coverage: 100% (all lines executed)
// Branch coverage: 100% (all if/else paths taken)
// Function coverage: 100% (function called)
// Statement coverage: 100% (all statements run)

// But coverage doesn't catch logic errors:
test("high coverage doesn't guarantee correctness", () => {
  function add(a: number, b: number): number {
    return a * b; // => BUG: Multiply instead of add
  } // => Tests can achieve 100% coverage but be wrong

  expect(add(2, 3)).toBe(6); // => Wrong expectation (should be 5)
}); // => 100% coverage, 0% correctness
```

**Key Takeaway**: Aim for high test coverage (80%+ is good, 95%+ is excellent) but verify assertions are correct. Coverage measures execution, not correctness.

**Why It Matters**: Coverage is a necessary but insufficient quality metric. Google requires 80% minimum coverage but emphasizes assertion quality over raw numbers. Teams with high coverage AND strong assertions have 70% fewer bugs than high-coverage teams with weak tests.

## Example 45: TDD with Express.js Routes

Web applications require testing HTTP endpoints. TDD with Express uses supertest to test routes without starting a server.

**Red: Test HTTP endpoint**

```typescript
test("GET /users returns user list", async () => {
  // => FAILS: App not defined
  const response = await request(app).get("/users");
  expect(response.status).toBe(200);
  expect(response.body).toHaveLength(2);
});
```

**Green: Express app with route**

```typescript
import express from "express";
import request from "supertest";

const app = express();

const users = [
  // => In-memory data
  { id: 1, name: "Alice" },
  { id: 2, name: "Bob" },
];

app.get("/users", (req, res) => {
  // => Route handler
  res.json(users); // => Return JSON
});

describe("GET /users", () => {
  test("returns user list", async () => {
    const response = await request(app).get("/users"); // => Supertest request

    expect(response.status).toBe(200); // => Verify status code
    expect(response.body).toEqual([
      // => Verify response body
      { id: 1, name: "Alice" },
      { id: 2, name: "Bob" },
    ]);
  });
});
```

**Refactored: Test POST endpoint with validation**

```typescript
app.use(express.json()); // => Enable JSON parsing

app.post("/users", (req, res) => {
  const { name } = req.body;

  if (!name || name.trim().length === 0) {
    // => Validation
    return res.status(400).json({ error: "Name is required" });
  }

  const newUser = { id: users.length + 1, name };
  users.push(newUser);
  res.status(201).json(newUser); // => 201 Created
});

describe("POST /users", () => {
  test("creates user with valid data", async () => {
    const response = await request(app).post("/users").send({ name: "Charlie" }); // => POST with body

    expect(response.status).toBe(201);
    expect(response.body).toEqual({ id: 3, name: "Charlie" });
  });

  test("rejects request without name", async () => {
    const response = await request(app).post("/users").send({});

    expect(response.status).toBe(400);
    expect(response.body).toEqual({ error: "Name is required" });
  });

  test("rejects empty name", async () => {
    const response = await request(app).post("/users").send({ name: "   " });

    expect(response.status).toBe(400);
  });
});
```

**Key Takeaway**: Use `supertest` to test Express routes without server startup. Test all HTTP methods (GET, POST, PUT, DELETE) and response codes.

**Why It Matters**: API testing without actual servers keeps tests fast and isolated. Stripe's API testing framework using supertest runs 10,000+ endpoint tests in under 30 seconds, enabling rapid iteration without infrastructure overhead.

## Example 46: TDD with React Components

React components require testing rendering, props, and user interactions. Use React Testing Library to test components from a user's perspective.

**Red: Test component rendering**

```typescript
test("Button renders with text", () => {
  // => FAILS: Button component not defined
  render(<Button>Click me</Button>);
  expect(screen.getByText("Click me")).toBeInTheDocument();
});
```

**Green: Simple Button component**

```typescript
import React from "react";
import { render, screen, fireEvent } from "@testing-library/react";

interface ButtonProps {
  children: React.ReactNode;
  onClick?: () => void;
}

function Button({ children, onClick }: ButtonProps) {
  return <button onClick={onClick}>{children}</button>; // => Basic button
}

test("Button renders with text", () => {
  render(<Button>Click me</Button>); // => Render component
  expect(screen.getByText("Click me")).toBeInTheDocument(); // => Assert presence
});
```

**Refactored: Test interactions**

```typescript
describe("Button", () => {
  test("renders with provided text", () => {
    render(<Button>Click me</Button>);
    expect(screen.getByText("Click me")).toBeInTheDocument();
  });

  test("calls onClick handler when clicked", () => {
    const handleClick = jest.fn(); // => Mock function
    render(<Button onClick={handleClick}>Click me</Button>);

    fireEvent.click(screen.getByText("Click me")); // => Simulate click

    expect(handleClick).toHaveBeenCalledTimes(1); // => Verify call
  });

  test("calls onClick multiple times", () => {
    const handleClick = jest.fn();
    render(<Button onClick={handleClick}>Click me</Button>);

    const button = screen.getByText("Click me");
    fireEvent.click(button); // => First click
    fireEvent.click(button); // => Second click

    expect(handleClick).toHaveBeenCalledTimes(2);
  });

  test("renders without onClick handler", () => {
    render(<Button>Click me</Button>); // => No onClick prop
    const button = screen.getByText("Click me");
    fireEvent.click(button); // => Should not throw
  });
});
```

**Key Takeaway**: Use React Testing Library to test components from a user perspective. Query by text/role/label, not implementation details. Use `fireEvent` for interactions.

**Why It Matters**: Component testing prevents UI regressions. Airbnb's frontend testing strategy using React Testing Library reduced user-reported UI bugs by 80% because tests verify actual user interactions rather than implementation details.

## Example 47: Testing Event-Driven Code

Event-driven code (EventEmitter, observers) requires testing that events are emitted and handled correctly with proper ordering.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73
graph LR
    A[Event Source]
    B[Event Emitter]
    C[Event Handler 1]
    D[Event Handler 2]

    A -->|Triggers| B
    B -->|Emits| C
    B -->|Emits| D

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#029E73,stroke:#000,color:#fff
```

**Red: Test event emission**

```typescript
test("OrderProcessor emits orderPlaced event", () => {
  const processor = new OrderProcessor(); // => FAILS: OrderProcessor not defined
  const listener = jest.fn();

  processor.on("orderPlaced", listener);
  processor.placeOrder({ id: 1, total: 100 });

  expect(listener).toHaveBeenCalledWith({ id: 1, total: 100 });
});
```

**Green: EventEmitter implementation**

```typescript
import { EventEmitter } from "events";

interface Order {
  id: number;
  total: number;
}

class OrderProcessor extends EventEmitter {
  placeOrder(order: Order): void {
    // Order processing logic
    this.emit("orderPlaced", order); // => Emit event
  }
}

test("OrderProcessor emits orderPlaced event", () => {
  const processor = new OrderProcessor();
  const listener = jest.fn();

  processor.on("orderPlaced", listener); // => Register listener
  processor.placeOrder({ id: 1, total: 100 });

  expect(listener).toHaveBeenCalledWith({ id: 1, total: 100 });
});
```

**Refactored: Test multiple events and ordering**

```typescript
class OrderProcessor extends EventEmitter {
  placeOrder(order: Order): void {
    this.emit("orderValidating", order); // => Event 1
    // Validation logic
    this.emit("orderPlaced", order); // => Event 2
  }

  cancelOrder(orderId: number): void {
    this.emit("orderCancelled", orderId); // => Different event
  }
}

describe("OrderProcessor events", () => {
  let processor: OrderProcessor;

  beforeEach(() => {
    processor = new OrderProcessor();
  });

  test("emits events in correct order", () => {
    const events: string[] = [];

    processor.on("orderValidating", () => events.push("validating"));
    processor.on("orderPlaced", () => events.push("placed"));

    processor.placeOrder({ id: 1, total: 100 });

    expect(events).toEqual(["validating", "placed"]); // => Verify order
  });

  test("passes correct data to listeners", () => {
    const validatingListener = jest.fn();
    const placedListener = jest.fn();

    processor.on("orderValidating", validatingListener);
    processor.on("orderPlaced", placedListener);

    const order = { id: 1, total: 100 };
    processor.placeOrder(order);

    expect(validatingListener).toHaveBeenCalledWith(order);
    expect(placedListener).toHaveBeenCalledWith(order);
  });

  test("supports multiple listeners", () => {
    const listener1 = jest.fn();
    const listener2 = jest.fn();

    processor.on("orderPlaced", listener1);
    processor.on("orderPlaced", listener2);

    processor.placeOrder({ id: 1, total: 100 });

    expect(listener1).toHaveBeenCalled();
    expect(listener2).toHaveBeenCalled(); // => Both called
  });
});
```

**Key Takeaway**: Test event emission, event data, and event ordering. Use mock functions to verify listeners are called with correct arguments.

**Why It Matters**: Event-driven bugs are hard to debug because execution is non-linear. Slack's real-time messaging infrastructure relies heavily on tested event handling - their testing discipline caught 90% of race conditions during development that would have been catastrophic in production.

## Example 48: Testing State Machines

State machines model workflows with discrete states and transitions. TDD ensures valid transitions and prevents invalid state changes.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC
stateDiagram-v2
    [*] --> Draft
    Draft --> Published: publish()
    Published --> Archived: archive()
    Archived --> Draft: restore()
    Published --> Draft: unpublish()

    style Draft fill:#0173B2,color:#fff
    style Published fill:#029E73,color:#fff
    style Archived fill:#CC78BC,color:#000
```

**Red: Test state transitions**

```typescript
test("Document starts in Draft state", () => {
  const doc = new Document(); // => FAILS: Document not defined
  expect(doc.getState()).toBe("Draft");
});

test("Document can be published from Draft", () => {
  const doc = new Document();
  doc.publish();
  expect(doc.getState()).toBe("Published");
});
```

**Green: State machine implementation**

```typescript
type DocumentState = "Draft" | "Published" | "Archived";

class Document {
  private state: DocumentState = "Draft"; // => Initial state

  getState(): DocumentState {
    return this.state;
  }

  publish(): void {
    if (this.state === "Draft") {
      // => Valid transition
      this.state = "Published";
    } else {
      throw new Error("Cannot publish from " + this.state);
    }
  }

  archive(): void {
    if (this.state === "Published") {
      this.state = "Archived";
    } else {
      throw new Error("Cannot archive from " + this.state);
    }
  }

  restore(): void {
    if (this.state === "Archived") {
      this.state = "Draft";
    } else {
      throw new Error("Cannot restore from " + this.state);
    }
  }
}

describe("Document state machine", () => {
  test("starts in Draft state", () => {
    const doc = new Document();
    expect(doc.getState()).toBe("Draft");
  });

  test("transitions Draft -> Published", () => {
    const doc = new Document();
    doc.publish();
    expect(doc.getState()).toBe("Published");
  });

  test("transitions Published -> Archived", () => {
    const doc = new Document();
    doc.publish();
    doc.archive();
    expect(doc.getState()).toBe("Archived");
  });

  test("transitions Archived -> Draft (restore)", () => {
    const doc = new Document();
    doc.publish();
    doc.archive();
    doc.restore();
    expect(doc.getState()).toBe("Draft");
  });

  test("rejects invalid transition (Archive from Draft)", () => {
    const doc = new Document();
    expect(() => doc.archive()).toThrow("Cannot archive from Draft");
  });

  test("rejects invalid transition (Restore from Draft)", () => {
    const doc = new Document();
    expect(() => doc.restore()).toThrow("Cannot restore from Draft");
  });
});
```

**Key Takeaway**: Test all valid state transitions and reject invalid ones. State machines should throw errors for invalid transitions to prevent corruption.

**Why It Matters**: State machine bugs cause data corruption. Amazon's order processing system uses extensively tested state machines - their discipline prevents orders from entering invalid states (like shipping before payment), which would cost millions in fulfillment errors.

## Example 49: Parameterized Tests (test.each)

Parameterized tests eliminate duplication when testing multiple inputs with the same logic. Use `test.each` to run the same test with different data.

**Red: Repetitive tests**

```typescript
test("isEven returns true for 2", () => {
  expect(isEven(2)).toBe(true); // => FAILS: isEven not defined
});

test("isEven returns true for 4", () => {
  expect(isEven(4)).toBe(true);
});

test("isEven returns false for 1", () => {
  expect(isEven(1)).toBe(false);
});

test("isEven returns false for 3", () => {
  expect(isEven(3)).toBe(false);
});
```

**Green: Parameterized test implementation**

```typescript
function isEven(n: number): boolean {
  return n % 2 === 0;
}

describe("isEven", () => {
  test.each([
    // => Parameterized test table
    [2, true], // => [input, expected]
    [4, true],
    [6, true],
    [0, true],
    [1, false],
    [3, false],
    [5, false],
  ])("isEven(%i) returns %s", (input, expected) => {
    // => Test name template
    expect(isEven(input)).toBe(expected); // => Same assertion, different data
  });
});
```

**Refactored: Parameterized tests with objects**

```typescript
describe("isEven edge cases", () => {
  test.each([
    { input: -2, expected: true, description: "negative even" },
    { input: -1, expected: false, description: "negative odd" },
    { input: 0, expected: true, description: "zero" },
  ])("$description: isEven($input) = $expected", ({ input, expected }) => {
    expect(isEven(input)).toBe(expected);
  });
});

describe("FizzBuzz with parameterized tests", () => {
  test.each([
    { input: 15, expected: "FizzBuzz", reason: "divisible by 3 and 5" },
    { input: 3, expected: "Fizz", reason: "divisible by 3" },
    { input: 5, expected: "Buzz", reason: "divisible by 5" },
    { input: 1, expected: "1", reason: "not divisible" },
  ])("$reason: fizzBuzz($input) = $expected", ({ input, expected }) => {
    function fizzBuzz(n: number): string {
      if (n % 15 === 0) return "FizzBuzz";
      if (n % 3 === 0) return "Fizz";
      if (n % 5 === 0) return "Buzz";
      return String(n);
    }

    expect(fizzBuzz(input)).toBe(expected);
  });
});
```

**Key Takeaway**: Use `test.each` with arrays or objects to eliminate test duplication. Template strings in test names (`%i`, `$property`) make failure messages clear.

**Why It Matters**: Parameterized tests reduce maintenance burden while increasing test coverage. JetBrains' IDE testing uses parameterized tests extensively, covering thousands of input combinations with minimal code duplication.

## Example 50: Snapshot Testing Use Cases

Snapshot testing captures output and detects unintended changes. Use for complex outputs like rendered components or JSON responses, but verify snapshots manually.

**Red: Test component output**

```typescript
test("UserCard renders user information", () => {
  const user = { name: "Alice", email: "alice@example.com" }; // => Test data
  const { container } = render(<UserCard user={user} />); // => FAILS: UserCard not defined
  expect(container).toMatchSnapshot(); // => Snapshot assertion
});
```

**Green: Component with snapshot**

```typescript
interface User {
  name: string;
  email: string;
}

function UserCard({ user }: { user: User }) {
  return (
    <div className="user-card">
      {" "}
      {/* => Component structure */}
      <h2>{user.name}</h2>
      <p>{user.email}</p>
    </div>
  );
}

test("UserCard renders user information", () => {
  const user = { name: "Alice", email: "alice@example.com" };
  const { container } = render(<UserCard user={user} />);
  expect(container).toMatchSnapshot(); // => Creates snapshot file
});

// Generated snapshot (in __snapshots__ folder):
// exports[`UserCard renders user information 1`] = `
// <div>
//   <div class="user-card">
//     <h2>Alice</h2>
//     <p>alice@example.com</p>
//   </div>
// </div>
// `;
```

**Refactored: When to use snapshots**

```typescript
describe("Snapshot testing appropriate use cases", () => {
  test("captures complex JSON structure", () => {
    const apiResponse = {
      // => Complex output
      data: {
        users: [
          { id: 1, name: "Alice", roles: ["admin", "user"] },
          { id: 2, name: "Bob", roles: ["user"] },
        ],
        meta: { total: 2, page: 1 },
      },
    };

    expect(apiResponse).toMatchSnapshot(); // => Good use: complex structure
  });

  test("AVOID: simple values better with explicit assertions", () => {
    const sum = 2 + 2;
    expect(sum).toBe(4); // => GOOD: Explicit assertion
    // expect(sum).toMatchSnapshot(); // => BAD: Snapshot overkill
  });

  test("AVOID: snapshots for business logic", () => {
    function calculateDiscount(price: number): number {
      return price * 0.9; // => Business logic
    }

    expect(calculateDiscount(100)).toBe(90); // => GOOD: Explicit value
    // expect(calculateDiscount(100)).toMatchSnapshot(); // => BAD: Hides expected value
  });
});
```

**Key Takeaway**: Use snapshots for complex outputs (UI components, large JSON). Avoid for simple values or business logic where explicit assertions are clearer. Always review snapshot changes manually.

**Why It Matters**: Snapshot tests catch regressions in complex outputs but create false security if blindly updated. Instagram's testing team requires manual review of all snapshot changes after discovering that 40% of their UI bugs came from approved snapshots that developers didn't actually examine.

## Example 51: Testing File I/O Operations

File operations require mocking the filesystem to avoid actual file writes. Use virtual filesystems or mock fs module for fast, isolated tests.

**Red: Test file operations**

```typescript
test("saveData writes JSON to file", async () => {
  // => FAILS: saveData not defined
  await saveData("test.json", { name: "Alice" });
  const content = await readData("test.json");
  expect(content).toEqual({ name: "Alice" });
});
```

**Green: Mock filesystem implementation**

```typescript
import fs from "fs/promises";

jest.mock("fs/promises"); // => Mock fs module

async function saveData(path: string, data: any): Promise<void> {
  await fs.writeFile(path, JSON.stringify(data, null, 2));
}

async function readData(path: string): Promise<any> {
  const content = await fs.readFile(path, "utf-8");
  return JSON.parse(content);
}

describe("File operations", () => {
  beforeEach(() => {
    jest.clearAllMocks(); // => Reset mocks
  });

  test("saveData writes JSON to file", async () => {
    (fs.writeFile as jest.Mock).mockResolvedValue(undefined); // => Mock write
    (fs.readFile as jest.Mock).mockResolvedValue('{"name":"Alice"}'); // => Mock read

    await saveData("test.json", { name: "Alice" });
    const content = await readData("test.json");

    expect(content).toEqual({ name: "Alice" });
    expect(fs.writeFile).toHaveBeenCalledWith("test.json", '{\n  "name": "Alice"\n}');
  });

  test("readData handles missing files", async () => {
    (fs.readFile as jest.Mock).mockRejectedValue(new Error("ENOENT: file not found"));

    await expect(readData("missing.json")).rejects.toThrow("ENOENT");
  });
});
```

**Refactored: In-memory filesystem fake**

```typescript
class FakeFileSystem {
  private files: Map<string, string> = new Map(); // => In-memory storage

  async writeFile(path: string, content: string): Promise<void> {
    this.files.set(path, content); // => Store in memory
  }

  async readFile(path: string): Promise<string> {
    const content = this.files.get(path);
    if (!content) {
      throw new Error(`ENOENT: ${path} not found`);
    }
    return content;
  }

  reset(): void {
    this.files.clear();
  }
}

class DataStore {
  constructor(private fs: FakeFileSystem) {}

  async save(path: string, data: any): Promise<void> {
    await this.fs.writeFile(path, JSON.stringify(data));
  }

  async load(path: string): Promise<any> {
    const content = await this.fs.readFile(path);
    return JSON.parse(content);
  }
}

describe("DataStore with fake filesystem", () => {
  let fakeFs: FakeFileSystem;
  let store: DataStore;

  beforeEach(() => {
    fakeFs = new FakeFileSystem();
    store = new DataStore(fakeFs);
  });

  test("saves and loads data", async () => {
    await store.save("data.json", { name: "Alice" });
    const loaded = await store.load("data.json");

    expect(loaded).toEqual({ name: "Alice" });
  });

  test("handles non-existent files", async () => {
    await expect(store.load("missing.json")).rejects.toThrow("ENOENT");
  });
});
```

**Key Takeaway**: Mock filesystem operations with jest.mock or create fake filesystem classes. Never write real files in unit tests - they're slow and create cleanup burden.

**Why It Matters**: Real file I/O makes tests slow and fragile. GitHub's test suite mocks all filesystem operations, running 50,000+ tests in minutes that would take hours with real file writes.

## Example 52: Testing with Environment Variables

Environment variables affect behavior but pollute test scope. Save and restore process.env to ensure test isolation.

**Red: Test with environment variables**

```typescript
test("getApiUrl returns production URL in production", () => {
  // => FAILS: getApiUrl not defined
  process.env.NODE_ENV = "production";
  expect(getApiUrl()).toBe("https://api.example.com");
});
```

**Green: Environment-dependent implementation**

```typescript
function getApiUrl(): string {
  return process.env.NODE_ENV === "production" ? "https://api.example.com" : "http://localhost:3000";
}

test("getApiUrl returns production URL", () => {
  const originalEnv = process.env.NODE_ENV; // => Save original
  process.env.NODE_ENV = "production";

  expect(getApiUrl()).toBe("https://api.example.com");

  process.env.NODE_ENV = originalEnv; // => Restore original
});
```

**Refactored: Test isolation with beforeEach/afterEach**

```typescript
describe("getApiUrl", () => {
  let originalEnv: string | undefined;

  beforeEach(() => {
    originalEnv = process.env.NODE_ENV; // => Save before each test
  });

  afterEach(() => {
    process.env.NODE_ENV = originalEnv; // => Restore after each test
  });

  test("returns production URL when NODE_ENV=production", () => {
    process.env.NODE_ENV = "production";
    expect(getApiUrl()).toBe("https://api.example.com");
  });

  test("returns development URL when NODE_ENV=development", () => {
    process.env.NODE_ENV = "development";
    expect(getApiUrl()).toBe("http://localhost:3000");
  });

  test("defaults to development URL when NODE_ENV not set", () => {
    delete process.env.NODE_ENV; // => Unset variable
    expect(getApiUrl()).toBe("http://localhost:3000");
  });
});
```

**Better approach: Dependency injection**

```typescript
interface Config {
  apiUrl: string;
}

function createConfig(env: string = "development"): Config {
  return {
    apiUrl: env === "production" ? "https://api.example.com" : "http://localhost:3000",
  };
}

class ApiClient {
  constructor(private config: Config) {} // => Inject config

  getBaseUrl(): string {
    return this.config.apiUrl;
  }
}

describe("ApiClient", () => {
  test("uses production URL with production config", () => {
    const config = createConfig("production");
    const client = new ApiClient(config);

    expect(client.getBaseUrl()).toBe("https://api.example.com");
  });

  test("uses development URL with development config", () => {
    const config = createConfig("development");
    const client = new ApiClient(config);

    expect(client.getBaseUrl()).toBe("http://localhost:3000");
  });
});
```

**Key Takeaway**: Save and restore environment variables to prevent test pollution. Better yet, inject configuration objects instead of reading process.env directly.

**Why It Matters**: Environment variable pollution causes flaky tests. Heroku's testing infrastructure mandates env var isolation after discovering that 20% of test failures were caused by tests interfering with each other's environment settings.

## Example 53: CI/CD Integration for TDD

Continuous Integration runs tests automatically on every commit. Configure CI to fail builds when tests fail, maintaining quality gates.

**Red: Test CI configuration**

```yaml
# .github/workflows/test.yml
name: Test Suite

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: "18"
      - run: npm ci # => Install dependencies
      - run: npm test # => Run tests (FAILS if any test fails)
      - run: npm run test:coverage # => Generate coverage report
```

**Green: Package.json test scripts**

```json
{
  "scripts": {
    "test": "jest",
    "test:watch": "jest --watch",
    "test:coverage": "jest --coverage",
    "test:ci": "jest --ci --coverage --maxWorkers=2"
  },
  "jest": {
    "coverageThreshold": {
      "global": {
        "branches": 80,
        "functions": 80,
        "lines": 80,
        "statements": 80
      }
    }
  }
}
```

**Refactored: Pre-commit hooks**

```typescript
// .husky/pre-commit
#!/usr/bin/env sh
. "$(dirname -- "$0")/_/husky.sh"

npm run test:quick # => Run fast tests before commit

// package.json
{
  "scripts": {
    "test:quick": "jest --onlyChanged --bail", // => Only changed files
    "test:full": "jest --coverage",
    "prepare": "husky install"
  }
}
```

**Test that verifies CI-specific behavior**

```typescript
describe("CI environment detection", () => {
  test("detects CI environment", () => {
    const originalCI = process.env.CI;
    process.env.CI = "true";

    function isCI(): boolean {
      return process.env.CI === "true";
    }

    expect(isCI()).toBe(true);

    process.env.CI = originalCI; // => Restore
  });
});
```

**Key Takeaway**: Configure CI to run tests on every commit. Set coverage thresholds to enforce quality gates. Use pre-commit hooks for fast local feedback.

**Why It Matters**: Automated testing catches bugs before code review. CircleCI's research shows teams with CI-enforced testing catch 95% of bugs before production versus 60% for teams relying on manual testing.

## Example 54: Test Performance Optimization

Slow tests reduce development velocity. Optimize test performance with parallel execution, test isolation, and selective test runs.

**Red: Slow test suite**

```typescript
describe("Slow test suite", () => {
  test("test 1 takes 1 second", async () => {
    await new Promise((resolve) => setTimeout(resolve, 1000)); // => Real delay
    expect(true).toBe(true);
  });

  test("test 2 takes 1 second", async () => {
    await new Promise((resolve) => setTimeout(resolve, 1000));
    expect(true).toBe(true);
  });
  // => Total: 2+ seconds
});
```

**Green: Optimized with fake timers**

```typescript
describe("Optimized test suite", () => {
  beforeEach(() => {
    jest.useFakeTimers(); // => Enable fake timers
  });

  afterEach(() => {
    jest.useRealTimers();
  });

  test("test 1 runs instantly", () => {
    const callback = jest.fn();
    setTimeout(callback, 1000); // => Schedule callback

    jest.advanceTimersByTime(1000); // => Instant time travel

    expect(callback).toHaveBeenCalled(); // => Runs in <1ms
  });

  test("test 2 runs instantly", () => {
    const callback = jest.fn();
    setTimeout(callback, 1000);

    jest.advanceTimersByTime(1000);

    expect(callback).toHaveBeenCalled();
  });
  // => Total: <10ms (200x faster)
});
```

**Refactored: Parallel execution**

```typescript
// jest.config.js
module.exports = {
  maxWorkers: "50%", // => Use 50% of CPU cores
  testTimeout: 5000, // => 5 second timeout per test
};

// Organize tests for optimal parallelization
describe("Independent test suite 1", () => {
  // Can run in parallel with suite 2
  test("operation 1", () => {
    expect(add(1, 2)).toBe(3);
  });
});

describe("Independent test suite 2", () => {
  // Can run in parallel with suite 1
  test("operation 2", () => {
    expect(multiply(2, 3)).toBe(6);
  });
});

function add(a: number, b: number): number {
  return a + b;
}
function multiply(a: number, b: number): number {
  return a * b;
}
```

**Key Takeaway**: Use fake timers for time-based code, enable parallel test execution, and run only changed tests during development. Save full test runs for CI.

**Why It Matters**: Fast tests enable rapid Red-Green-Refactor cycles. Google's test infrastructure runs millions of tests in parallel, providing feedback in seconds rather than hours, maintaining developer flow state.

## Example 55: Flaky Test Detection and Fixes

Flaky tests pass/fail inconsistently. Identify flakiness with repeated test runs and fix root causes (timing, shared state, random data).

**Red: Flaky test example**

```typescript
test("FLAKY: random number generation", () => {
  // => FAILS intermittently
  const random = Math.random(); // => Non-deterministic
  expect(random).toBeGreaterThan(0.5); // => Fails ~50% of time
});
```

**Green: Fix with controlled randomness**

```typescript
function mockMath() {
  const originalRandom = Math.random;
  Math.random = jest.fn(() => 0.7); // => Deterministic mock

  return () => {
    Math.random = originalRandom; // => Restore function
  };
}

test("FIXED: controlled random number", () => {
  const restore = mockMath();

  const random = Math.random(); // => Always 0.7
  expect(random).toBeGreaterThan(0.5); // => Always passes

  restore();
});
```

**Refactored: Common flaky patterns and fixes**

```typescript
describe("Flaky test patterns", () => {
  // Pattern 1: Race condition
  test("FLAKY: async race condition", async () => {
    let result = 0;
    setTimeout(() => {
      result = 1;
    }, 100); // => Timing dependent

    await new Promise((resolve) => setTimeout(resolve, 50));
    expect(result).toBe(1); // => FLAKY: race condition
  });

  test("FIXED: await promises properly", async () => {
    let result = 0;
    const promise = new Promise<void>((resolve) => {
      setTimeout(() => {
        result = 1;
        resolve();
      }, 100);
    });

    await promise; // => Wait for completion
    expect(result).toBe(1); // => Reliable
  });

  // Pattern 2: Shared state
  const sharedState = { count: 0 };

  test("FLAKY: test 1 modifies shared state", () => {
    sharedState.count++; // => Side effect
    expect(sharedState.count).toBe(1); // => Depends on execution order
  });

  test("FLAKY: test 2 assumes clean state", () => {
    expect(sharedState.count).toBe(0); // => FAILS if test 1 ran first
  });

  // FIXED: Reset state in beforeEach
  describe("FIXED: isolated state", () => {
    let isolatedState: { count: number };

    beforeEach(() => {
      isolatedState = { count: 0 }; // => Fresh state per test
    });

    test("test 1 with isolated state", () => {
      isolatedState.count++;
      expect(isolatedState.count).toBe(1);
    });

    test("test 2 with isolated state", () => {
      expect(isolatedState.count).toBe(0); // => Always passes
    });
  });
});
```

**Key Takeaway**: Flaky tests stem from non-determinism (randomness, timing, shared state). Fix by controlling randomness, using fake timers, and isolating test state.

**Why It Matters**: Flaky tests destroy trust in test suites. Google's testing research shows that teams with >5% flaky tests stop running tests regularly, negating TDD benefits. Their policy mandates immediate quarantine of flaky tests.

## Example 56: Test Data Builders Pattern

Test data builders create complex test objects with readable, maintainable code. Builders provide defaults and allow customization of specific fields.

**Red: Repetitive object creation**

```typescript
test("processes order with discount", () => {
  // => Verbose object creation
  const order = {
    id: 1,
    customer: {
      id: 1,
      name: "Alice",
      email: "alice@example.com",
      tier: "premium",
    },
    items: [{ id: 1, name: "Book", price: 20, quantity: 2 }],
    total: 40,
    discount: 0.1,
  };

  expect(processOrder(order)).toBe(36); // => FAILS: processOrder not defined
});
```

**Green: Test data builder implementation**

```typescript
class OrderBuilder {
  private order = {
    // => Default values
    id: 1,
    customer: {
      id: 1,
      name: "Test User",
      email: "test@example.com",
      tier: "standard" as const,
    },
    items: [{ id: 1, name: "Item", price: 10, quantity: 1 }],
    total: 10,
    discount: 0,
  };

  withId(id: number): OrderBuilder {
    // => Fluent API
    this.order.id = id;
    return this; // => Return this for chaining
  }

  withCustomer(name: string, tier: "standard" | "premium"): OrderBuilder {
    this.order.customer = { ...this.order.customer, name, tier };
    return this;
  }

  withTotal(total: number): OrderBuilder {
    this.order.total = total;
    return this;
  }

  withDiscount(discount: number): OrderBuilder {
    this.order.discount = discount;
    return this;
  }

  build() {
    return { ...this.order }; // => Return copy
  }
}

function processOrder(order: any): number {
  return order.total * (1 - order.discount);
}

test("processes order with discount", () => {
  const order = new OrderBuilder()
    .withId(1) // => Fluent chaining
    .withCustomer("Alice", "premium")
    .withTotal(40)
    .withDiscount(0.1)
    .build();

  expect(processOrder(order)).toBe(36);
});

test("processes order without discount", () => {
  const order = new OrderBuilder().withTotal(50).build(); // => Only set needed fields

  expect(processOrder(order)).toBe(50);
});
```

**Key Takeaway**: Builders provide defaults for complex objects and allow customizing specific fields. Use fluent API (return `this`) for method chaining.

**Why It Matters**: Test data builders reduce test duplication and improve readability. LinkedIn's testing guidelines mandate builders for objects with >3 fields after measuring 40% reduction in test maintenance time.

## Example 57: Object Mother Pattern

Object mothers provide named factory methods for common test scenarios. Similar to builders but focused on predefined scenarios rather than customization.

**Red: Duplicated test scenarios**

```typescript
test("premium customer gets free shipping", () => {
  // => Scenario 1: Premium customer
  const customer = {
    id: 1,
    name: "Alice",
    tier: "premium",
    totalSpent: 5000,
  };
  expect(calculateShipping(customer)).toBe(0);
});

test("standard customer pays shipping", () => {
  // => Scenario 2: Standard customer
  const customer = {
    id: 2,
    name: "Bob",
    tier: "standard",
    totalSpent: 100,
  };
  expect(calculateShipping(customer)).toBe(10);
});
```

**Green: Object mother implementation**

```typescript
interface Customer {
  id: number;
  name: string;
  tier: "standard" | "premium";
  totalSpent: number;
}

class CustomerMother {
  // => Object mother class
  static standardCustomer(): Customer {
    // => Named scenario
    return {
      id: 1,
      name: "Standard Customer",
      tier: "standard",
      totalSpent: 100,
    };
  }

  static premiumCustomer(): Customer {
    return {
      id: 2,
      name: "Premium Customer",
      tier: "premium",
      totalSpent: 5000,
    };
  }

  static newCustomer(): Customer {
    // => Another common scenario
    return {
      id: 3,
      name: "New Customer",
      tier: "standard",
      totalSpent: 0,
    };
  }
}

function calculateShipping(customer: Customer): number {
  if (customer.tier === "premium") return 0;
  if (customer.totalSpent > 1000) return 0;
  return 10;
}

describe("calculateShipping", () => {
  test("premium customer gets free shipping", () => {
    const customer = CustomerMother.premiumCustomer(); // => Clear scenario name
    expect(calculateShipping(customer)).toBe(0);
  });

  test("standard customer pays shipping", () => {
    const customer = CustomerMother.standardCustomer();
    expect(calculateShipping(customer)).toBe(10);
  });

  test("new customer pays shipping", () => {
    const customer = CustomerMother.newCustomer();
    expect(calculateShipping(customer)).toBe(10);
  });
});
```

**Key Takeaway**: Object mothers provide named factory methods for common scenarios. Use when you have recurring test patterns rather than one-off customizations.

**Why It Matters**: Object mothers document domain scenarios through code. Thoughtworks' testing patterns show that named factory methods improve test readability by making scenarios explicit rather than buried in object literals.

## Example 58: London vs Chicago TDD Schools

Two TDD approaches differ in interaction testing (London/mockist) versus state testing (Chicago/classicist). Both are valid with different tradeoffs.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73
graph TD
    A[London School]
    B[Mock Dependencies]
    C[Test Interactions]
    D[Chicago School]
    E[Use Real Objects]
    F[Test State Changes]

    A --> B
    B --> C
    D --> E
    E --> F

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#0173B2,stroke:#000,color:#fff
    style E fill:#DE8F05,stroke:#000,color:#fff
    style F fill:#029E73,stroke:#000,color:#fff
```

**London School (Mockist) Approach**

```typescript
// London: Mock all dependencies, test interactions
test("LONDON: OrderProcessor calls PaymentGateway.charge", () => {
  const mockGateway = {
    charge: jest.fn().mockReturnValue(true), // => Mock dependency
  };

  const processor = new OrderProcessor(mockGateway);
  processor.process({ id: 1, total: 100 });

  expect(mockGateway.charge).toHaveBeenCalledWith(100); // => Verify interaction
}); // => Focus: Was charge() called correctly?

class OrderProcessor {
  constructor(private gateway: { charge: (amount: number) => boolean }) {}

  process(order: { id: number; total: number }): void {
    this.gateway.charge(order.total); // => Interaction tested
  }
}
```

**Chicago School (Classicist) Approach**

```typescript
// Chicago: Use real objects, test state
test("CHICAGO: OrderProcessor updates order status", () => {
  const realGateway = new RealPaymentGateway(); // => Real dependency
  const processor = new OrderProcessor(realGateway);

  const order = { id: 1, total: 100, status: "pending" };
  processor.process(order);

  expect(order.status).toBe("paid"); // => Verify state change
}); // => Focus: What is the final state?

class RealPaymentGateway {
  charge(amount: number): boolean {
    return amount > 0; // => Real implementation
  }
}

class OrderProcessor {
  constructor(private gateway: RealPaymentGateway) {}

  process(order: { id: number; total: number; status: string }): void {
    const success = this.gateway.charge(order.total);
    if (success) order.status = "paid"; // => State change tested
  }
}
```

**Comparison**

```typescript
describe("London vs Chicago comparison", () => {
  // London pros: Fast isolation, explicit interactions
  // London cons: Brittle tests coupled to implementation

  test("LONDON: fails when implementation changes", () => {
    const mockGateway = {
      charge: jest.fn().mockReturnValue(true),
      validate: jest.fn(), // => Added new method
    };

    const processor = new OrderProcessor(mockGateway);
    processor.process({ id: 1, total: 100 });

    expect(mockGateway.charge).toHaveBeenCalled();
    // If implementation adds validate() call, test breaks
  });

  // Chicago pros: Tests behavior, refactor-friendly
  // Chicago cons: Slower (real objects), harder to isolate

  test("CHICAGO: survives implementation changes", () => {
    const realGateway = new RealPaymentGateway();
    const processor = new OrderProcessor(realGateway);

    const order = { id: 1, total: 100, status: "pending" };
    processor.process(order);

    expect(order.status).toBe("paid");
    // Still passes even if internal calls change
  });
});
```

**Key Takeaway**: London school mocks dependencies and tests interactions (good for isolation). Chicago school uses real objects and tests state (good for refactoring). Both valid - choose based on context.

**Why It Matters**: TDD philosophy affects test design. Kent Beck (Chicago school founder) emphasizes behavior over implementation, while Steve Freeman (London school) prioritizes fast feedback and explicit contracts. Teams should choose consistently rather than mixing approaches.

---

This completes the **intermediate level (Examples 31-58)** covering test doubles, mocking strategies, asynchronous testing, database testing, property-based testing, and production TDD patterns with real frameworks like Express and React.

**Next Steps**: The advanced level (Examples 59-85) covers concurrency testing, integration testing, performance testing, mutation testing tools, test architecture patterns, and TDD in large-scale systems.
