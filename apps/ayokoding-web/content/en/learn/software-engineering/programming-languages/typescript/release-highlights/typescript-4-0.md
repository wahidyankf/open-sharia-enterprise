---
title: "TypeScript 4 0"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "TypeScript 4.0 release highlights - variadic tuple types, labeled tuple elements, and class property inference improvements"
weight: 10000001
tags: ["typescript", "typescript-4-0", "variadic-tuples", "labeled-tuples"]
---

## Release Overview

**TypeScript 4.0** was released on **August 20, 2020**, marking a major milestone with significant type system enhancements focused on tuple types and improved inference.

**Key Metrics:**

- **Release Date:** August 20, 2020
- **Major Focus:** Tuple type flexibility and developer experience
- **Breaking Changes:** Minimal (mostly edge case fixes)
- **Performance:** 5-10% faster compilation

## Variadic Tuple Types

**Major Feature:** Enable tuples to have variable-length segments with precise type safety.

### Before TypeScript 4.0

**Problem:** Couldn't express functions with variable arguments while preserving types.

```typescript
// Could only handle fixed-length tuples
type Concat<T extends any[], U extends any[]> = [...T, ...U]; // ❌ Error in TS 3.x

// Workaround: Lose type information
function concat(...args: any[]): any[] {
  return args.flat();
}
```

### With TypeScript 4.0

**Solution:** Spread elements in tuple types preserve precise types.

```typescript
// Variadic tuple types work!
type Concat<T extends any[], U extends any[]> = [...T, ...U];

// Example usage
type Result = Concat<[1, 2], [3, 4]>; // => [1, 2, 3, 4]

// Practical function
function concat<T extends any[], U extends any[]>(arr1: readonly [...T], arr2: readonly [...U]): [...T, ...U] {
  return [...arr1, ...arr2];
}

// Type-safe concatenation
const result = concat([1, 2], ["a", "b"]); // => [1, 2, 'a', 'b']
// result type: [number, number, string, string]
```

### Real-World Application

**Type-safe function composition:**

```typescript
// Generic tuple spreading
type Tail<T extends any[]> = T extends [any, ...infer Rest] ? Rest : [];

// Partial application with preserved types
function partial<T extends any[], U extends any[], R>(
  fn: (...args: [...T, ...U]) => R,
  ...firstArgs: T
): (...args: U) => R {
  return (...lastArgs: U) => fn(...firstArgs, ...lastArgs);
}

// Example: API client with partial application
function makeRequest(method: string, url: string, headers: Record<string, string>, body?: object): Promise<Response> {
  return fetch(url, { method, headers, body: JSON.stringify(body) });
}

// Partial application
const postToAPI = partial(makeRequest, "POST", "/api/data");
// Type: (headers: Record<string, string>, body?: object) => Promise<Response>

postToAPI({ "Content-Type": "application/json" }, { data: "value" });
```

## Labeled Tuple Elements

**Feature:** Add descriptive labels to tuple elements for better readability and IDE support.

### Basic Syntax

```typescript
// Before: Position-based understanding required
type Range = [number, number];

// After: Self-documenting with labels
type Range = [start: number, end: number];

// More complex example
type HTTPResponse = [status: number, body: string, headers: Record<string, string>];

// Function using labeled tuples
function parseResponse(...args: HTTPResponse): void {
  const [status, body, headers] = args;
  console.log(`Status: ${status}`);
}
```

### Real-World Example

**API client with labeled tuples:**

```typescript
// Database query result
type QueryResult<T> = [data: T[], count: number, hasMore: boolean];

function fetchUsers(page: number, limit: number): QueryResult<User> {
  // Simulated database query
  const users: User[] = [
    /* ... */
  ];
  const total = 1000;
  const hasMore = page * limit < total;

  return [users, total, hasMore]; // Labels provide context in IDE
}

// Usage with destructuring - labels appear in IDE hints
const [users, totalCount, hasMorePages] = fetchUsers(1, 20);
// IDE shows: data: User[], count: number, hasMore: boolean
```

**Event system with labeled tuples:**

```typescript
// Event payload with clear intent
type UserEvent = [
  action: "create" | "update" | "delete",
  userId: string,
  timestamp: Date,
  metadata?: Record<string, unknown>,
];

function logUserEvent(...event: UserEvent): void {
  const [action, userId, timestamp, metadata] = event;
  console.log(`[${timestamp.toISOString()}] User ${userId}: ${action}`);
  if (metadata) {
    console.log("Metadata:", metadata);
  }
}

// IDE autocomplete shows parameter names
logUserEvent("create", "user-123", new Date(), { source: "api" });
```

## Class Property Inference from Constructors

**Improvement:** Better type inference for class properties initialized in constructors.

### Before and After

```typescript
// TypeScript 3.x - Manual annotation required
class Point {
  x: number; // ❌ Must annotate explicitly
  y: number;

  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }
}

// TypeScript 4.0 - Infers from constructor
class Point {
  x; // ✅ Inferred as number
  y;

  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }
}
```

### Practical Example

**Configuration class with inference:**

```typescript
class DatabaseConfig {
  host; // Inferred: string
  port; // Inferred: number
  database; // Inferred: string
  ssl; // Inferred: boolean
  poolSize; // Inferred: number

  constructor(host: string, port: number, database: string, ssl: boolean = true, poolSize: number = 10) {
    this.host = host;
    this.port = port;
    this.database = database;
    this.ssl = ssl;
    this.poolSize = poolSize;
  }

  getConnectionString(): string {
    const protocol = this.ssl ? "postgres" : "postgresql";
    return `${protocol}://${this.host}:${this.port}/${this.database}`;
  }
}
```

## Short-Circuiting Assignment Operators

**Feature:** Support for `||=`, `&&=`, `??=` operators from ECMAScript 2021.

### Syntax and Examples

```typescript
// Logical OR assignment (||=)
let value: string | undefined;
value ||= "default"; // Assigns if value is falsy
// Equivalent: value = value || 'default'

// Logical AND assignment (&&=)
let config: Config | null = getConfig();
config &&= processConfig(config); // Only assigns if config is truthy
// Equivalent: config = config && processConfig(config)

// Nullish coalescing assignment (??=)
let port: number | null = null;
port ??= 3000; // Assigns only if port is null/undefined
// Equivalent: port = port ?? 3000
```

### Practical Application

**Configuration defaults:**

```typescript
interface ServerConfig {
  port?: number;
  host?: string;
  timeout?: number;
  retries?: number;
}

function initializeConfig(config: ServerConfig): Required<ServerConfig> {
  // Use ??= for null/undefined checks (not 0 or empty string)
  config.port ??= 8080;
  config.host ??= "localhost";
  config.timeout ??= 30000;
  config.retries ??= 3;

  return config as Required<ServerConfig>;
}

// Example usage
const userConfig: ServerConfig = { port: 0 }; // Port 0 is valid
const fullConfig = initializeConfig(userConfig);
// fullConfig.port = 0 (preserved because ??= checks null/undefined only)
```

## `unknown` in Catch Clauses

**Improvement:** `catch` clause variables default to `unknown` instead of `any` (with `--useUnknownInCatchVariables`).

### Before and After

```typescript
// TypeScript 3.x - implicit any
try {
  riskyOperation();
} catch (error) {
  // error: any (unsafe)
  console.log(error.message); // No type checking
}

// TypeScript 4.0 - explicit unknown (safer)
try {
  riskyOperation();
} catch (error) {
  // error: unknown (type-safe)
  if (error instanceof Error) {
    console.log(error.message); // ✅ Type narrowed
  } else {
    console.log("Unknown error:", error);
  }
}
```

### Best Practice Pattern

**Type-safe error handling:**

```typescript
class APIError extends Error {
  constructor(
    message: string,
    public statusCode: number,
    public code: string,
  ) {
    super(message);
    this.name = "APIError";
  }
}

async function fetchUser(id: string): Promise<User> {
  try {
    const response = await fetch(`/api/users/${id}`);
    if (!response.ok) {
      throw new APIError("Fetch failed", response.status, "FETCH_ERROR");
    }
    return response.json();
  } catch (error) {
    // error: unknown
    if (error instanceof APIError) {
      console.error(`API Error [${error.statusCode}]: ${error.message}`);
      throw error;
    } else if (error instanceof Error) {
      console.error("Unexpected error:", error.message);
      throw new APIError("Internal error", 500, "INTERNAL_ERROR");
    } else {
      console.error("Unknown error type:", error);
      throw new APIError("Unknown error", 500, "UNKNOWN_ERROR");
    }
  }
}
```

## Custom JSX Factories

**Feature:** Configure JSX factory per file using `@jsxImportSource` pragma.

### Syntax

```typescript
/** @jsxImportSource preact */
import { h } from 'preact';

function App() {
  return <div>Hello from Preact!</div>;
}
```

**Use Case:** Enables mixing different JSX runtimes in same project (React, Preact, Vue JSX).

## Breaking Changes

**Minimal breaking changes in 4.0:**

1. **`lib.d.ts` updates** - DOM types refined (may surface latent errors)
2. **Properties overriding accessors** - More strict checking
3. **Operands for `delete` must be optional** - Type safety improvement

**Migration:** Most projects upgrade seamlessly. Review TypeScript 4.0 release notes for edge cases.

## Performance Improvements

**Build Performance:**

- 5-10% faster incremental builds
- Improved project references performance
- Better memory usage for large projects

**Editor Performance:**

- Faster IntelliSense
- Better responsiveness in large files
- Improved auto-import suggestions

## Migration Guide

**Step 1: Update TypeScript**

```bash
npm install -D typescript@4.0
```

**Step 2: Address Type Errors**

```bash
tsc --noEmit # Check for new errors
```

**Step 3: Leverage New Features**

- Refactor functions to use variadic tuples
- Add labels to existing tuple types
- Remove explicit class property types where inferred
- Enable `--useUnknownInCatchVariables` for new projects

## Summary

TypeScript 4.0 (August 2020) delivered foundational tuple type improvements:

- **Variadic tuple types** - Variable-length type-safe tuples
- **Labeled tuple elements** - Self-documenting tuple types
- **Class property inference** - Less boilerplate in classes
- **Short-circuiting operators** - Modern JavaScript syntax support
- **`unknown` in catch** - Safer error handling

**Next Steps:**

- Explore [TypeScript 4.1](/en/learn/software-engineering/programming-languages/typescript/release-highlights/typescript-4-1) for template literal types revolution
- Continue to [TypeScript 4.9](/en/learn/software-engineering/programming-languages/typescript/release-highlights/typescript-4-9) for the `satisfies` operator

## References

- [Official TypeScript 4.0 Release Notes](https://devblogs.microsoft.com/typescript/announcing-typescript-4-0/)
- [TypeScript 4.0 Documentation](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-0.html)
