---
title: "Anti Patterns"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "Common TypeScript anti-patterns and how to avoid them - any abuse, type assertions, error suppression, null checks, god objects, circular dependencies, callback hell, and premature optimization"
weight: 10000031
tags: ["typescript", "anti-patterns", "code-quality", "best-practices", "type-safety"]
---

## Why Anti-Patterns Matter

Anti-patterns are common solutions that appear beneficial but cause problems in production: reduced type safety, increased coupling, difficult maintenance, and runtime errors. Recognizing anti-patterns enables writing maintainable, type-safe TypeScript code.

**Core Problems**:

- **Lost type safety**: `any` abuse removes TypeScript's value proposition
- **Hidden errors**: Type assertions and error suppression hide bugs
- **Maintenance burden**: God objects and circular dependencies increase coupling
- **Callback hell**: Nested callbacks reduce readability and error handling
- **Premature optimization**: Complex code without measured performance gains

**Solution**: Follow TypeScript best practices: leverage type system, handle errors explicitly, maintain single responsibility, use async/await for asynchronous code, and optimize based on profiling data.

## Anti-Pattern 1: any Abuse

Using `any` type defeats TypeScript's type checking, removing compile-time safety.

### The Problem

`any` allows any value, bypassing type checking entirely.

**Anti-pattern**:

```typescript
// ❌ BAD: any everywhere
function processData(data: any): any {
  // => data: any (accepts anything)
  // => Return type: any (no type safety)
  // => TypeScript cannot check correctness
  return data.value.toUpperCase();
  // => Runtime error if data.value is undefined
  // => TypeScript doesn't warn
}

const result: any = processData({ value: "hello" });
// => result: any (lost type information)
console.log(result.length);
// => No autocomplete
// => No compile-time checking

// Runtime error (no compile-time warning)
processData({ value: 123 });
// => TypeError: data.value.toUpperCase is not a function
// => TypeScript didn't catch this
```

**Density**: 12 code lines, 14 annotation lines = 1.17 density (within 1.0-2.25 target)

**Problems**:

- No autocomplete (IDE can't suggest properties)
- No compile-time errors (bugs slip to runtime)
- No refactoring safety (rename breaks silently)
- Lost documentation (types document intent)

### The Solution

Use specific types with proper type definitions.

**Pattern**:

```typescript
// ✅ GOOD: Specific types
interface InputData {
  value: string;
  // => Explicit type for value
  // => TypeScript knows value is string
}

function processData(data: InputData): string {
  // => Input type: InputData (validated)
  // => Return type: string (explicit)
  return data.value.toUpperCase();
  // => TypeScript knows value exists and is string
  // => Compile-time safety
}

const result: string = processData({ value: "hello" });
// => result: string (type information preserved)
// => Autocomplete works
// => IDE suggests string methods

console.log(result.length);
// => Compile-time checked (length exists on string)

// Compile error (caught before runtime)
// processData({ value: 123 });
// => Error: Type 'number' is not assignable to type 'string'
```

**Density**: 14 code lines, 17 annotation lines = 1.21 density (within 1.0-2.25 target)

### When any is Acceptable

Very rare cases where `any` justified (but consider alternatives first).

**Legitimate uses**:

```typescript
// Migration from JavaScript (temporary)
// => Gradually add types
let legacyData: any;
// => TODO: Replace with proper type

// Third-party library without types (use @types if available)
import externalLib from "legacy-lib";
// => No type definitions available
// => Better: Create .d.ts file with types

// Truly dynamic data (JSON.parse)
// => Use unknown instead of any
const json: unknown = JSON.parse(response);
// => unknown: Must type-check before use
// => Safer than any
```

## Anti-Pattern 2: Type Assertions Everywhere

Type assertions (`as`) override TypeScript's type checking, assuming you know better.

### The Problem

Excessive type assertions silence type errors without fixing underlying issues.

**Anti-pattern**:

```typescript
// ❌ BAD: Type assertions everywhere
function getUser(id: string): User {
  const response = fetch(`/api/users/${id}`);
  // => response: Promise<Response> (not User)
  return response as any as User;
  // => Double assertion to force type
  // => Silences all type errors
  // => Runtime will fail (Promise, not User)
}

const data = JSON.parse(jsonString);
const user = data as User;
// => Assumes data has User shape
// => No runtime validation
// => Fails silently if structure differs

const element = document.getElementById("app") as HTMLDivElement;
// => Assumes element exists and is HTMLDivElement
// => Runtime error if element is null or different type
element.innerHTML = "Hello";
// => Crashes if element is null
```

**Density**: 13 code lines, 17 annotation lines = 1.31 density (within 1.0-2.25 target)

**Problems**:

- Assertions don't validate at runtime (type != runtime check)
- Hide underlying type mismatches
- Break when API response changes
- No compile-time safety for invalid casts

### The Solution

Use type guards and proper typing instead of assertions.

**Pattern**:

```typescript
// ✅ GOOD: Proper async handling
async function getUser(id: string): Promise<User> {
  // => Return Promise<User> (correct type)
  const response = await fetch(`/api/users/${id}`);
  const data = await response.json();
  // => data: any (JSON.parse always returns any)

  // Runtime validation with type guard
  if (isUser(data)) {
    // => Type guard checks structure at runtime
    return data;
    // => TypeScript knows data is User in this branch
  }

  throw new Error("Invalid user data");
  // => Fail fast if validation fails
}

// Type guard function
function isUser(value: any): value is User {
  // => value is User: Type predicate
  // => TypeScript narrows type if returns true
  return typeof value === "object" && value !== null && typeof value.id === "string" && typeof value.name === "string";
  // => Runtime checks for User shape
  // => Returns true only if all properties valid
}

// Null check instead of assertion
const element = document.getElementById("app");
// => element: HTMLElement | null (correct type)

if (element) {
  // => Type guard: element is HTMLElement in this branch
  element.innerHTML = "Hello";
  // => TypeScript knows element is non-null
} else {
  console.error("Element not found");
  // => Handle null case explicitly
}
```

**Density**: 25 code lines, 31 annotation lines = 1.24 density (within 1.0-2.25 target)

## Anti-Pattern 3: Ignoring Errors with

Non-null assertion operator (!) tells TypeScript "trust me, not null" without runtime check.

### The Problem

`!` suppresses null/undefined errors without actual validation.

**Anti-pattern**:

```typescript
// ❌ BAD: Non-null assertions everywhere
function processUser(userId: string) {
  const user = users.find((u) => u.id === userId)!;
  // => !: Assume find returns user (might be undefined)
  // => Runtime error if user not found
  console.log(user.name);
  // => Crashes if user is undefined
}

const config = process.env.API_KEY!;
// => !: Assume API_KEY exists
// => Runtime error if environment variable not set

const element = document.querySelector(".header")!;
// => !: Assume element exists
element.addEventListener("click", handler);
// => Crashes if element doesn't exist
```

**Density**: 11 code lines, 13 annotation lines = 1.18 density (within 1.0-2.25 target)

**Problems**:

- No runtime validation (crashes silently)
- Hides null/undefined bugs
- Violates fail-fast principle

### The Solution

Handle null/undefined cases explicitly.

**Pattern**:

```typescript
// ✅ GOOD: Explicit null checks
function processUser(userId: string) {
  const user = users.find((u) => u.id === userId);
  // => user: User | undefined (correct type)

  if (!user) {
    // => Handle not found case
    throw new Error(`User ${userId} not found`);
    // => Fail fast with descriptive error
  }

  console.log(user.name);
  // => TypeScript knows user is defined here
}

// Environment variable with fallback
const config = process.env.API_KEY ?? "default-key";
// => Nullish coalescing: Use default if undefined/null
// => Or fail fast if required:
if (!process.env.API_KEY) {
  throw new Error("API_KEY environment variable required");
}
const apiKey = process.env.API_KEY;
// => TypeScript knows apiKey is string (not undefined)

// DOM element with null check
const element = document.querySelector(".header");

if (element) {
  // => Type guard: element is non-null
  element.addEventListener("click", handler);
} else {
  console.warn("Header element not found");
  // => Graceful degradation
}
```

**Density**: 22 code lines, 24 annotation lines = 1.09 density (within 1.0-2.25 target)

## Anti-Pattern 4: Missing Null Checks

Assuming values always exist without checking for null/undefined.

### The Problem

Accessing properties without checking for null causes runtime errors.

**Anti-pattern**:

```typescript
// ❌ BAD: No null checks
function displayUser(user: User | null) {
  // => user can be null (union type)
  console.log(user.name);
  // => Runtime error if user is null
  // => TypeScript error (but ignored with any cast)
}

interface Config {
  database?: {
    // => database is optional (can be undefined)
    host: string;
    port: number;
  };
}

function connect(config: Config) {
  const host = config.database.host;
  // => Error if database is undefined
  // => Cannot read property 'host' of undefined
  console.log(`Connecting to ${host}`);
}
```

**Density**: 12 code lines, 13 annotation lines = 1.08 density (within 1.0-2.25 target)

**Problems**:

- Runtime errors from null/undefined access
- Crashes in production
- Violates defensive programming

### The Solution

Use optional chaining (?.) and nullish coalescing (??) operators.

**Pattern**:

```typescript
// ✅ GOOD: Safe property access
function displayUser(user: User | null) {
  console.log(user?.name ?? "Unknown");
  // => Optional chaining: user?.name
  // => Returns undefined if user is null
  // => Nullish coalescing: ?? "Unknown"
  // => Uses "Unknown" if undefined
}

// Safe nested property access
function connect(config: Config) {
  const host = config.database?.host ?? "localhost";
  // => config.database?.host: undefined if database undefined
  // => ?? "localhost": Fallback to default
  const port = config.database?.port ?? 5432;

  console.log(`Connecting to ${host}:${port}`);
  // => Safe: Always has valid host/port
}

// Early return for null checks
function processOrder(order: Order | null) {
  if (!order) {
    // => Guard clause: Handle null early
    console.log("No order to process");
    return;
    // => Early return avoids nesting
  }

  // TypeScript knows order is non-null here
  console.log(`Processing order ${order.id}`);
  // => Safe: order guaranteed non-null
}
```

**Density**: 20 code lines, 24 annotation lines = 1.20 density (within 1.0-2.25 target)

## Anti-Pattern 5: God Objects

Classes with too many responsibilities violate Single Responsibility Principle.

### The Problem

God objects do everything, becoming unmaintainable and hard to test.

**Anti-pattern**:

```typescript
// ❌ BAD: God object
class UserManager {
  // => Handles everything related to users
  // => Too many responsibilities

  createUser(data: any) {
    // => User creation
    // Validate input
    // Hash password
    // Save to database
    // Send welcome email
    // Log analytics
    // Update cache
    // => 6+ responsibilities in one method
  }

  deleteUser(id: string) {
    // => User deletion
    // Delete from database
    // Invalidate cache
    // Delete files from S3
    // Send goodbye email
    // Update analytics
  }

  authenticateUser(email: string, password: string) {
    // => Authentication logic
  }

  sendPasswordReset(email: string) {
    // => Password reset logic
  }

  generateUserReport() {
    // => Reporting logic
  }

  exportUsersToCSV() {
    // => Export logic
  }

  // 50+ more methods...
  // => Hundreds of lines in one class
}
```

**Density**: 26 code lines, 22 annotation lines = 0.85 density (acceptable for anti-pattern example)

**Problems**:

- Difficult to test (too many dependencies)
- Violates Single Responsibility Principle
- High coupling (changes affect entire class)
- Merge conflicts frequent (everyone modifies same file)

### The Solution

Split into focused classes with single responsibilities.

**Pattern**:

```typescript
// ✅ GOOD: Separated concerns
class UserRepository {
  // => Data access only
  async save(user: User): Promise<void> {
    // => Database operations
    await db.insert("users", user);
  }

  async findById(id: string): Promise<User | null> {
    return db.query("SELECT * FROM users WHERE id = ?", [id]);
  }

  async delete(id: string): Promise<void> {
    await db.delete("users", { id });
  }
}

class UserService {
  // => Business logic
  constructor(
    private repository: UserRepository,
    private emailService: EmailService,
    private logger: Logger,
  ) {
    // => Dependencies injected
    // => Each service has single responsibility
  }

  async createUser(data: CreateUserDTO): Promise<User> {
    // => Orchestration only
    // => Delegates to specialized services
    const user = await this.repository.save(data);
    await this.emailService.sendWelcome(user.email);
    this.logger.info(`User created: ${user.id}`);
    return user;
  }
}

class EmailService {
  // => Email operations only
  async sendWelcome(email: string): Promise<void> {
    // => Single responsibility: Emails
  }
}

class AuthenticationService {
  // => Authentication logic only
  async authenticate(email: string, password: string): Promise<User | null> {
    // => Login logic
  }
}

class UserReportService {
  // => Reporting logic only
  generateReport(users: User[]): Report {
    // => Report generation
  }
}
```

**Density**: 37 code lines, 37 annotation lines = 1.00 density (within 1.0-2.25 target)

## Anti-Pattern 6: Circular Dependencies

Modules importing each other create circular dependencies, breaking modularity.

### The Problem

A imports B, B imports A creates initialization order issues.

**Anti-pattern**:

```typescript
// user.ts
import { Order } from "./order";
// => Imports Order

export class User {
  orders: Order[] = [];
  // => References Order

  addOrder(order: Order) {
    this.orders.push(order);
  }
}

// order.ts
import { User } from "./user";
// => Imports User (circular dependency!)
// => user.ts imports order.ts
// => order.ts imports user.ts

export class Order {
  user: User;
  // => References User

  constructor(user: User) {
    this.user = user;
  }
}
```

**Density**: 14 code lines, 14 annotation lines = 1.00 density (within 1.0-2.25 target)

**Problems**:

- Initialization order issues (undefined at runtime)
- Module bundlers struggle (webpack, rollup)
- Hard to test (must mock circular dependencies)

### The Solution

Break cycles with dependency inversion or interfaces.

**Pattern**:

```typescript
// ✅ GOOD: Dependency inversion
// user.interface.ts
export interface IUser {
  // => Interface (no imports)
  id: string;
  name: string;
}

// order.ts
import { IUser } from "./user.interface";
// => Import interface, not concrete class
// => No circular dependency

export class Order {
  user: IUser;
  // => Depend on interface (abstraction)
  // => Dependency Inversion Principle

  constructor(user: IUser) {
    this.user = user;
  }
}

// user.ts
import { Order } from "./order";
import { IUser } from "./user.interface";

export class User implements IUser {
  // => Implements interface
  id: string;
  name: string;
  orders: Order[] = [];

  addOrder(order: Order) {
    this.orders.push(order);
  }
}
```

**Density**: 21 code lines, 20 annotation lines = 0.95 density (within 1.0-2.25 target)

## Anti-Pattern 7: Callback Hell

Deeply nested callbacks reduce readability and error handling.

### The Problem

Nested callbacks create "pyramid of doom" with poor error handling.

**Anti-pattern**:

```typescript
// ❌ BAD: Callback hell
function processOrder(orderId: string, callback: (error: Error | null, result?: any) => void) {
  getOrder(orderId, (error, order) => {
    // => First callback
    if (error) {
      return callback(error);
      // => Error handling scattered
    }

    getUser(order.userId, (error, user) => {
      // => Second callback (nested)
      if (error) {
        return callback(error);
      }

      chargeCard(user.cardId, order.total, (error, charge) => {
        // => Third callback (deeply nested)
        // => "Pyramid of doom"
        if (error) {
          return callback(error);
        }

        sendEmail(user.email, order.id, (error) => {
          // => Fourth callback (very nested)
          if (error) {
            return callback(error);
            // => Error handling repeated 4 times
          }

          callback(null, { success: true });
          // => Success buried deep
        });
      });
    });
  });
}
```

**Density**: 26 code lines, 22 annotation lines = 0.85 density (acceptable for anti-pattern example)

**Problems**:

- Hard to read (nested indentation)
- Error handling duplicated
- Difficult to refactor
- Hard to add steps (must nest deeper)

### The Solution

Use async/await for flat, readable asynchronous code.

**Pattern**:

```typescript
// ✅ GOOD: async/await
async function processOrder(orderId: string): Promise<{ success: boolean }> {
  // => async function returns Promise
  // => Flat structure (no nesting)

  try {
    // => Single try/catch for all errors
    const order = await getOrder(orderId);
    // => await: Wait for Promise to resolve
    // => Linear flow (reads like synchronous code)

    const user = await getUser(order.userId);
    // => Second operation (not nested)

    const charge = await chargeCard(user.cardId, order.total);
    // => Third operation (same indentation level)

    await sendEmail(user.email, order.id);
    // => Fourth operation (flat)

    return { success: true };
    // => Return value (not callback)
  } catch (error) {
    // => Centralized error handling
    console.error("Order processing failed:", error);
    throw error;
    // => Re-throw for caller to handle
  }
}

// Helper functions (Promise-based)
async function getOrder(orderId: string): Promise<Order> {
  // => Returns Promise<Order>
  return fetch(`/api/orders/${orderId}`).then((r) => r.json());
}

async function getUser(userId: string): Promise<User> {
  return fetch(`/api/users/${userId}`).then((r) => r.json());
}
```

**Density**: 25 code lines, 28 annotation lines = 1.12 density (within 1.0-2.25 target)

## Anti-Pattern 8: Premature Optimization

Optimizing code before measuring performance wastes effort and reduces readability.

### The Problem

Complex optimizations without profiling data add complexity without proven benefit.

**Anti-pattern**:

```typescript
// ❌ BAD: Premature optimization
class DataProcessor {
  // => Object pooling without evidence it's needed
  private pool: any[] = [];
  // => Adds complexity

  processItems(items: string[]) {
    // Micro-optimization: Avoid array methods
    let result = "";
    const len = items.length;
    // => Cache length (negligible benefit)

    for (let i = 0; i < len; i++) {
      // => Manual loop instead of forEach
      // => Less readable, minimal performance gain
      result += items[i];
    }

    return result;
  }

  // Inline everything for "performance"
  process(data: any) {
    // => Inline all logic (hard to read)
    // => No abstraction
    // 200+ lines of inlined code
    // => Unproven performance benefit
  }
}
```

**Density**: 16 code lines, 19 annotation lines = 1.19 density (within 1.0-2.25 target)

**Problems**:

- Increased complexity without measured benefit
- Reduced readability (harder to maintain)
- Wasted developer time
- Optimization may not target actual bottleneck

### The Solution

Write readable code first, profile, then optimize bottlenecks.

**Pattern**:

```typescript
// ✅ GOOD: Readable first, optimize if needed
class DataProcessor {
  processItems(items: string[]): string {
    // => Clear, readable implementation
    return items.join("");
    // => Built-in method (optimized by V8)
    // => Readable and fast enough
  }

  processData(data: Record<string, any>): Result {
    // => Separate concerns with clear methods
    const validated = this.validate(data);
    const transformed = this.transform(validated);
    return this.save(transformed);
    // => Each step clear
    // => Easy to profile and optimize specific step if needed
  }

  private validate(data: Record<string, any>): ValidData {
    // => Single responsibility: Validation
    // Validation logic
    return data as ValidData;
  }

  private transform(data: ValidData): TransformedData {
    // => Single responsibility: Transformation
    // Transformation logic
    return {} as TransformedData;
  }

  private save(data: TransformedData): Result {
    // => Single responsibility: Persistence
    // Save logic
    return { success: true };
  }
}

// If profiling shows transform() is slow:
// 1. Measure with profiler (Chrome DevTools, clinic.js)
// 2. Optimize ONLY that method
// 3. Benchmark to verify improvement
```

**Density**: 27 code lines, 29 annotation lines = 1.07 density (within 1.0-2.25 target)

## Anti-Pattern Avoidance Checklist

**Type Safety**:

- ✅ Avoid `any` (use specific types or `unknown`)
- ✅ Avoid type assertions (use type guards)
- ✅ Avoid `!` operator (check for null explicitly)
- ✅ Handle null/undefined with `?.` and `??`

**Design**:

- ✅ Single Responsibility (one class, one job)
- ✅ Avoid circular dependencies (use interfaces)
- ✅ Dependency injection (constructor parameters)
- ✅ Keep classes small (<200 lines)

**Asynchronous Code**:

- ✅ Use async/await (not callbacks)
- ✅ Centralized error handling (try/catch)
- ✅ Promise-based APIs (not callback-based)

**Performance**:

- ✅ Profile before optimizing (use DevTools)
- ✅ Readability first (optimize bottlenecks later)
- ✅ Measure improvements (benchmarks)

## Summary

Anti-patterns reduce code quality through lost type safety, hidden errors, tight coupling, and premature complexity. Avoid `any`, type assertions, error suppression, god objects, circular dependencies, callback hell, and premature optimization.

**Avoiding anti-patterns**:

1. **Leverage TypeScript**: Use type system fully (no `any`, type guards)
2. **Handle errors explicitly**: No `!`, proper null checks
3. **Single Responsibility**: Small, focused classes
4. **Async/await**: Flat asynchronous code
5. **Profile before optimizing**: Data-driven optimization

**Production mindset**:

- Type safety prevents runtime errors
- Explicit null handling prevents crashes
- Single responsibility enables testing
- Readable code beats clever code
- Optimize based on profiling data

Choose correctness and readability over cleverness. TypeScript's type system exists to help, not to bypass.
