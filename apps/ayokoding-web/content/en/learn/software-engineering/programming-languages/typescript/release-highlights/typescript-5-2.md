---
title: "TypeScript 5 2"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "TypeScript 5.2 release highlights - using declarations for explicit resource management, decorator metadata, named and anonymous tuple elements"
weight: 10000013
tags: ["typescript", "typescript-5-2", "using-declarations", "resource-management", "decorator-metadata", "tuples"]
---

## Release Overview

**TypeScript 5.2** was released on **August 24, 2023**, introducing **`using` declarations** for explicit resource management - a game-changing feature for managing resources like file handles, database connections, and locks.

**Key Metrics:**

- **Release Date:** August 24, 2023
- **Major Focus:** `using` declarations, decorator metadata, tuple improvements
- **Breaking Changes:** Minimal
- **Performance:** Improved compilation and type checking

## Using Declarations - Explicit Resource Management

**Landmark Feature:** Automatic resource cleanup with `using` declarations, implementing the ECMAScript Explicit Resource Management proposal.

### The Resource Management Problem

**Before `using` declarations:** Manual cleanup prone to errors and leaks.

```typescript
// Manual resource management - error-prone
function processFile(path: string) {
  const file = openFile(path);

  try {
    // Process file
    const data = file.read();
    processData(data);
  } finally {
    // Must remember to close
    file.close(); // ❌ Easy to forget
  }
}

// Multiple resources = nested try-finally hell
function processMultipleResources() {
  const db = connectDatabase();
  try {
    const file = openFile("data.txt");
    try {
      const lock = acquireLock();
      try {
        // Use resources
      } finally {
        lock.release();
      }
    } finally {
      file.close();
    }
  } finally {
    db.disconnect();
  }
}
```

### With Using Declarations

**Solution:** Automatic cleanup when scope exits using `Symbol.dispose`.

```typescript
// Automatic resource management
function processFile(path: string) {
  using file = openFile(path);

  // Process file
  const data = file.read();
  processData(data);

  // ✅ file.close() called automatically when function exits
  // ✅ Works even if exception thrown
  // ✅ Guaranteed cleanup
}

// Multiple resources - clean and safe
function processMultipleResources() {
  using db = connectDatabase();
  using file = openFile("data.txt");
  using lock = acquireLock();

  // Use resources
  // ✅ All disposed automatically in reverse order:
  //    1. lock.release()
  //    2. file.close()
  //    3. db.disconnect()
}
```

### Implementing Disposable Resources

**Create disposable objects using `Symbol.dispose`:**

```typescript
// Disposable file handle
class FileHandle {
  private handle: number;

  constructor(path: string) {
    this.handle = fs.openSync(path, "r");
    console.log(`Opened file: ${path}`);
  }

  read(): string {
    return fs.readFileSync(this.handle, "utf-8");
  }

  [Symbol.dispose]() {
    console.log("Closing file");
    fs.closeSync(this.handle);
  }
}

function processFile() {
  using file = new FileHandle("data.txt");
  console.log(file.read());
  // FileHandle[Symbol.dispose]() called automatically
}
```

### Real-World Application: Database Connection

**Automatic connection cleanup:**

```typescript
class DatabaseConnection {
  private conn: Connection;

  constructor(connectionString: string) {
    this.conn = createConnection(connectionString);
    console.log("Database connected");
  }

  query(sql: string): Result[] {
    return this.conn.execute(sql);
  }

  [Symbol.dispose]() {
    console.log("Closing database connection");
    this.conn.close();
  }
}

function getUserData(userId: string) {
  using db = new DatabaseConnection("postgresql://...");

  const users = db.query(`SELECT * FROM users WHERE id = '${userId}'`);
  return users[0];

  // ✅ Database connection closed automatically
  // ✅ Even if query throws error
}
```

### Real-World Application: Lock Management

**Automatic lock release preventing deadlocks:**

```typescript
class Lock {
  private lockId: string;
  private acquired: boolean = false;

  constructor(resource: string) {
    this.lockId = resource;
    this.acquire();
  }

  private acquire() {
    // Acquire lock (blocking or async)
    console.log(`Acquiring lock: ${this.lockId}`);
    this.acquired = true;
  }

  [Symbol.dispose]() {
    if (this.acquired) {
      console.log(`Releasing lock: ${this.lockId}`);
      this.acquired = false;
      // Release lock in system
    }
  }
}

function updateSharedResource() {
  using lock = new Lock("shared-resource");

  // Critical section - protected by lock
  modifySharedData();

  // ✅ Lock released automatically
  // ✅ No deadlocks from forgotten releases
}
```

### Real-World Application: API Request Timer

**Automatic timing and logging:**

```typescript
class RequestTimer {
  private startTime: number;
  private requestId: string;

  constructor(requestId: string) {
    this.requestId = requestId;
    this.startTime = Date.now();
    console.log(`[${requestId}] Request started`);
  }

  [Symbol.dispose]() {
    const duration = Date.now() - this.startTime;
    console.log(`[${this.requestId}] Request completed in ${duration}ms`);
    // Send metrics to monitoring system
    sendMetrics(this.requestId, duration);
  }
}

async function handleRequest(req: Request): Promise<Response> {
  using timer = new RequestTimer(req.id);

  // Process request
  const data = await fetchData(req.query);
  const result = processData(data);

  return { status: 200, body: result };

  // ✅ Timer automatically logs duration
  // ✅ Metrics sent regardless of success/failure
}
```

### Async Resource Management with `await using`

**Asynchronous disposal using `Symbol.asyncDispose`:**

```typescript
class AsyncDatabaseConnection {
  private conn: Connection;

  constructor(connectionString: string) {
    this.conn = createConnection(connectionString);
  }

  async query(sql: string): Promise<Result[]> {
    return await this.conn.execute(sql);
  }

  async [Symbol.asyncDispose]() {
    console.log("Closing connection gracefully");
    await this.conn.close(); // Async cleanup
  }
}

async function fetchUserData(userId: string) {
  await using db = new AsyncDatabaseConnection("postgresql://...");

  const users = await db.query(`SELECT * FROM users WHERE id = '${userId}'`);
  return users[0];

  // ✅ Async dispose called automatically
  // ✅ Waits for graceful shutdown
}
```

## Decorator Metadata

**Feature:** Decorators can now access and store metadata using `Symbol.metadata`.

### Decorator Metadata API

```typescript
type Context = {
  kind: string;
  name: string | symbol;
  metadata?: Record<string | symbol, unknown>;
};

function logMetadata(target: any, context: Context) {
  // Access shared metadata object
  context.metadata = context.metadata || {};

  // Store metadata for this decorator
  context.metadata[context.name] = {
    decorated: true,
    timestamp: Date.now(),
  };

  return target;
}

class Example {
  @logMetadata
  method() {
    // Metadata stored for later access
  }
}

// Access metadata
const metadata = Example[Symbol.metadata];
console.log(metadata); // { method: { decorated: true, timestamp: ... } }
```

### Real-World Application: Validation Metadata

**Store validation rules in metadata:**

```typescript
function validate(rules: ValidationRules) {
  return function (target: any, context: ClassFieldDecoratorContext) {
    context.metadata = context.metadata || {};
    context.metadata[`validation:${String(context.name)}`] = rules;
    return target;
  };
}

class User {
  @validate({ minLength: 3, maxLength: 20 })
  username!: string;

  @validate({ pattern: /^[^\s@]+@[^\s@]+\.[^\s@]+$/ })
  email!: string;

  @validate({ min: 18, max: 120 })
  age!: number;
}

// Extract validation rules from metadata
function getValidationRules(target: any): Map<string, ValidationRules> {
  const metadata = target[Symbol.metadata];
  const rules = new Map();

  for (const [key, value] of Object.entries(metadata)) {
    if (key.startsWith("validation:")) {
      const fieldName = key.replace("validation:", "");
      rules.set(fieldName, value);
    }
  }

  return rules;
}

const userRules = getValidationRules(User);
// Map { 'username' => { minLength: 3, ... }, 'email' => { pattern: ... }, ... }
```

## Named and Anonymous Tuple Elements

**Feature:** Tuple elements can now have optional labels without affecting type compatibility.

### Tuple Labeling

```typescript
// Named tuple elements
type Point2D = [x: number, y: number];
type Point3D = [x: number, y: number, z: number];

// Anonymous tuple elements
type RGB = [number, number, number];

// Mixed - names are purely documentation
type ColoredPoint = [x: number, y: number, color: string];

function createPoint(): [x: number, y: number] {
  return [10, 20];
}

const point: Point2D = createPoint(); // ✅ Compatible
const [x, y] = point; // ✅ Destructuring works
```

### Real-World Application: Function Return Types

**Document multiple return values:**

```typescript
// Before - unclear what values mean
function parseCoordinates(input: string): [number, number, boolean] {
  // What does the boolean represent?
  return [10, 20, true];
}

// After - self-documenting
function parseCoordinates(input: string): [x: number, y: number, valid: boolean] {
  const parts = input.split(",");
  return [parseFloat(parts[0]), parseFloat(parts[1]), parts.length === 2];
}

// Usage
const [x, y, valid] = parseCoordinates("10,20");
// ✅ Clear meaning from tuple labels
```

### Real-World Application: API Response Tuples

**Type-safe response handling:**

```typescript
type ApiResponse<T> = [data: T | null, error: Error | null, status: number];

async function fetchUser(id: string): Promise<ApiResponse<User>> {
  try {
    const response = await fetch(`/api/users/${id}`);
    const data = await response.json();
    return [data, null, response.status];
  } catch (error) {
    return [null, error as Error, 500];
  }
}

// Usage with clear semantics
const [user, error, status] = await fetchUser("123");

if (error) {
  console.error(`Request failed (${status}):`, error);
} else {
  console.log("User:", user);
}
```

## Easier Method Usage for Unions of Arrays

**Feature:** Methods on union types with arrays are now easier to use.

```typescript
// Before TypeScript 5.2 - required type narrowing
function processData(data: string[] | number[]) {
  // ❌ Error: Property 'filter' does not exist on type 'string[] | number[]'
  const filtered = data.filter((item) => item !== null);
}

// TypeScript 5.2+ - common methods work
function processData(data: string[] | number[]) {
  // ✅ OK - filter available on both string[] and number[]
  const filtered = data.filter((item) => item !== null);

  // ✅ OK - map available on both
  const mapped = data.map((item) => String(item));

  // ✅ OK - forEach available on both
  data.forEach((item) => console.log(item));
}
```

## Performance Improvements

**Build Performance:**

- Faster type checking for `using` declarations
- Improved decorator metadata processing
- Better tuple type inference

**Editor Performance:**

- Faster IntelliSense for resource management
- Improved autocomplete for tuple labels

## Breaking Changes

**Minimal breaking changes:**

1. **`lib.d.ts` updates** - Added `Symbol.dispose` and `Symbol.asyncDispose`
2. **Decorator metadata** - May affect existing decorator libraries
3. **Tuple labeling** - Stricter tuple type checking in some cases

## Migration Guide

**Step 1: Update TypeScript**

```bash
npm install -D typescript@5.2
```

**Step 2: Adopt Using Declarations**

Replace manual cleanup with `using`:

```typescript
// Before
const resource = acquire();
try {
  use(resource);
} finally {
  resource.dispose();
}

// After
using resource = acquire();
use(resource);
```

**Step 3: Implement Disposable Pattern**

Add `Symbol.dispose` to resources:

```typescript
class Resource {
  [Symbol.dispose]() {
    // Cleanup logic
  }
}
```

**Step 4: Use Tuple Labels**

Document tuple return types:

```typescript
// Add labels for clarity
function getData(): [value: string, timestamp: number] {
  return ["data", Date.now()];
}
```

## Summary

TypeScript 5.2 (August 2023) introduced explicit resource management:

- **`using` declarations** - Automatic resource cleanup with `Symbol.dispose`
- **`await using`** - Async resource cleanup with `Symbol.asyncDispose`
- **Decorator metadata** - Store metadata using `Symbol.metadata`
- **Named tuple elements** - Self-documenting tuple types
- **Easier array unions** - Common methods work on union of arrays

**Impact:** `using` declarations solve the long-standing resource management problem, preventing leaks and simplifying cleanup code.

**Next Steps:**

- Continue to [TypeScript 5.3](/en/learn/software-engineering/programming-languages/typescript/release-highlights/typescript-5-3) for import attributes and switch narrowing
- Return to [Overview](/en/learn/software-engineering/programming-languages/typescript/release-highlights/overview) for full timeline

## References

- [Official TypeScript 5.2 Release Notes](https://devblogs.microsoft.com/typescript/announcing-typescript-5-2/)
- [TypeScript 5.2 Documentation](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-5-2.html)
- [ECMAScript Explicit Resource Management Proposal](https://github.com/tc39/proposal-explicit-resource-management)
- [Using Declarations Deep Dive](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-5-2.html#using-declarations-and-explicit-resource-management)
