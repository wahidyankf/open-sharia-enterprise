---
title: "TypeScript 5 5"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "TypeScript 5.5 release highlights - Inferred type predicates, control flow narrowing for const indexed accesses, and performance optimizations"
weight: 10000016
tags: ["typescript", "typescript-5-5", "type-predicates", "control-flow", "performance"]
---

## Release Overview

**TypeScript 5.5** was released on **June 20, 2024**, introducing **automatic type predicate inference** - eliminating the need for manual `is` type guards in many scenarios. This release also brought significant performance improvements and better control flow analysis.

**Key Metrics:**

- **Release Date:** June 20, 2024
- **Major Focus:** Inferred type predicates, control flow narrowing, performance
- **Breaking Changes:** Minimal
- **Performance:** Up to 60% faster monorepo builds, 20-30% smaller type checker memory

## Inferred Type Predicates

**Landmark Feature:** TypeScript automatically infers type predicates from function returns, eliminating manual `is` type guards.

### The Problem It Solves

**Before Inferred Type Predicates:** Manual type predicates required for every filter/validation function.

```typescript
// Before - manual type predicate required
function isString(value: unknown): value is string {
  return typeof value === "string";
}

function isNumber(value: unknown): value is number {
  return typeof value === "number";
}

function isNotNull<T>(value: T | null): value is T {
  return value !== null;
}

// Repetitive, error-prone, verbose
```

### With Inferred Type Predicates

**Solution:** TypeScript automatically infers type predicates from boolean-returning functions.

```typescript
// After - type predicates inferred automatically
function isString(value: unknown) {
  return typeof value === "string";
}
// ✅ Automatically inferred: value is string

function isNumber(value: unknown) {
  return typeof value === "number";
}
// ✅ Automatically inferred: value is number

function isNotNull<T>(value: T | null) {
  return value !== null;
}
// ✅ Automatically inferred: value is T

// Usage with automatic narrowing
const mixed: (string | number | null)[] = ["hello", 42, null, "world"];

const strings = mixed.filter(isString);
// ✅ Type automatically narrowed to: string[]

const numbers = mixed.filter(isNumber);
// ✅ Type automatically narrowed to: number[]

const nonNull = mixed.filter(isNotNull);
// ✅ Type automatically narrowed to: (string | number)[]
```

### Real-World Application: Array Filtering

**Automatic narrowing in complex filter chains:**

```typescript
interface User {
  id: number;
  name: string;
  email?: string;
  verified?: boolean;
}

const users: User[] = [
  { id: 1, name: "Alice", email: "alice@example.com", verified: true },
  { id: 2, name: "Bob" },
  { id: 3, name: "Charlie", email: "charlie@example.com" },
];

// Inferred type predicates for property checks
function hasEmail(user: User) {
  return user.email !== undefined;
}
// ✅ Automatically inferred: user is User & { email: string }

function isVerified(user: User) {
  return user.verified === true;
}
// ✅ Automatically inferred: user is User & { verified: true }

// Chain filters with automatic narrowing
const verifiedWithEmail = users
  .filter(hasEmail)
  // ✅ Type: (User & { email: string })[]
  .filter(isVerified);
// ✅ Type: (User & { email: string } & { verified: true })[]

// Type-safe access
verifiedWithEmail.forEach((user) => {
  // ✅ user.email is string (not string | undefined)
  sendEmail(user.email);
  // ✅ user.verified is true (not boolean | undefined)
  logVerification(user.verified);
});
```

### Real-World Application: Union Type Filtering

**Discriminate union types automatically:**

```typescript
type Shape =
  | { kind: "circle"; radius: number }
  | { kind: "square"; size: number }
  | { kind: "rectangle"; width: number; height: number };

// Inferred type predicates for union discrimination
function isCircle(shape: Shape) {
  return shape.kind === "circle";
}
// ✅ Automatically inferred: shape is { kind: "circle"; radius: number }

function isRectangle(shape: Shape) {
  return shape.kind === "rectangle";
}
// ✅ Automatically inferred: shape is { kind: "rectangle"; width: number; height: number }

const shapes: Shape[] = [
  { kind: "circle", radius: 10 },
  { kind: "square", size: 20 },
  { kind: "rectangle", width: 30, height: 40 },
];

// Automatic narrowing in filters
const circles = shapes.filter(isCircle);
// ✅ Type: { kind: "circle"; radius: number }[]

const rectangles = shapes.filter(isRectangle);
// ✅ Type: { kind: "rectangle"; width: number; height: number }[]

// Type-safe property access
circles.forEach((circle) => {
  // ✅ circle.radius is number (no type assertion needed)
  console.log(Math.PI * circle.radius ** 2);
});

rectangles.forEach((rect) => {
  // ✅ rect.width and rect.height are numbers
  console.log(rect.width * rect.height);
});
```

### Real-World Application: Validation Pipelines

**Build type-safe validation without manual predicates:**

```typescript
interface ApiResponse {
  success: boolean;
  data?: unknown;
  error?: string;
}

// Inferred type predicates for validation
function isSuccess(response: ApiResponse) {
  return response.success === true;
}
// ✅ Automatically inferred: response is ApiResponse & { success: true }

function hasData<T>(response: ApiResponse & { success: true }) {
  return response.data !== undefined;
}
// ✅ Automatically inferred: response is ApiResponse & { success: true; data: unknown }

function isValidUser(data: unknown): data is { id: number; name: string } {
  // Explicit predicate still needed for complex validation
  return (
    typeof data === "object" &&
    data !== null &&
    "id" in data &&
    "name" in data &&
    typeof (data as any).id === "number" &&
    typeof (data as any).name === "string"
  );
}

// Validation pipeline with automatic narrowing
async function fetchUser(id: number) {
  const response: ApiResponse = await fetch(`/api/users/${id}`).then((r) => r.json());

  if (!isSuccess(response)) {
    // ✅ response.error available (success is false)
    throw new Error(response.error || "Unknown error");
  }

  // ✅ response.success is true
  if (!hasData(response)) {
    throw new Error("No data received");
  }

  // ✅ response.data is unknown (not undefined)
  if (!isValidUser(response.data)) {
    throw new Error("Invalid user data");
  }

  // ✅ response.data is { id: number; name: string }
  return response.data;
}
```

### Real-World Application: Error Handling

**Type-safe error discrimination:**

```typescript
class ValidationError extends Error {
  constructor(
    message: string,
    public field: string,
  ) {
    super(message);
  }
}

class NetworkError extends Error {
  constructor(
    message: string,
    public statusCode: number,
  ) {
    super(message);
  }
}

// Inferred type predicates for error types
function isValidationError(error: Error) {
  return error instanceof ValidationError;
}
// ✅ Automatically inferred: error is ValidationError

function isNetworkError(error: Error) {
  return error instanceof NetworkError;
}
// ✅ Automatically inferred: error is NetworkError

try {
  await submitForm(data);
} catch (error) {
  if (error instanceof Error) {
    if (isValidationError(error)) {
      // ✅ error is ValidationError
      showFieldError(error.field, error.message);
    } else if (isNetworkError(error)) {
      // ✅ error is NetworkError
      if (error.statusCode === 429) {
        showRateLimitError();
      }
    } else {
      // ✅ error is Error (but not ValidationError or NetworkError)
      showGenericError(error.message);
    }
  }
}
```

## Control Flow Narrowing for `const` Indexed Accesses

**Feature:** TypeScript now narrows types through `const` indexed accesses on objects.

### Example

```typescript
const config = {
  development: { apiUrl: "http://localhost:3000", debug: true },
  production: { apiUrl: "https://api.example.com", debug: false },
} as const;

function getConfig(env: "development" | "production") {
  const selectedConfig = config[env];
  // ✅ Type narrowed to: { apiUrl: string; debug: boolean }
  // (before: union of both configs)

  if (env === "development") {
    // ✅ selectedConfig narrowed to: { apiUrl: "http://localhost:3000"; debug: true }
    console.log(selectedConfig.debug); // ✅ Type: true
  }
}
```

### Real-World Application: Environment Configuration

```typescript
const environments = {
  dev: {
    api: "http://localhost:3000",
    db: "localhost:5432",
    cache: "localhost:6379",
  },
  staging: {
    api: "https://staging-api.example.com",
    db: "staging-db.example.com:5432",
    cache: "staging-cache.example.com:6379",
  },
  prod: {
    api: "https://api.example.com",
    db: "prod-db.example.com:5432",
    cache: "prod-cache.example.com:6379",
  },
} as const;

function initApp(env: keyof typeof environments) {
  const config = environments[env];
  // ✅ Type narrowed based on env value

  // Type-safe configuration access
  connectToAPI(config.api); // ✅ Correct URL for environment
  connectToDB(config.db); // ✅ Correct DB connection
  connectToCache(config.cache); // ✅ Correct cache connection
}
```

## JSDoc `@import` Tag

**Feature:** Import types in JSDoc comments without runtime imports.

### Example

```typescript
// Before - runtime import needed for type
import type { User } from "./types";

/**
 * @param {User} user
 */
function greet(user) {
  return `Hello, ${user.name}`;
}
```

```typescript
// After - JSDoc import (no runtime import)
/**
 * @import { User } from "./types"
 * @param {User} user
 */
function greet(user) {
  return `Hello, ${user.name}`;
}
// ✅ No runtime import, only type information
```

### Real-World Application: JavaScript Files with Types

```javascript
/**
 * @import { Request, Response, NextFunction } from "express"
 * @import { User } from "./models/user"
 */

/**
 * Middleware to authenticate users
 * @param {Request} req
 * @param {Response} res
 * @param {NextFunction} next
 */
function authenticate(req, res, next) {
  // Type-safe parameter access without TypeScript file
  const token = req.headers.authorization;
  // ...
}

/**
 * Get user by ID
 * @param {number} id
 * @returns {Promise<User>}
 */
async function getUserById(id) {
  const user = await db.users.findById(id);
  return user;
}
```

## Regular Expression Syntax Checking

**Feature:** TypeScript validates regular expression syntax in string literals.

### Example

```typescript
// Invalid regex detected at compile time
const invalidRegex = /[a-z/;
// ❌ Error: Invalid regular expression - unclosed character class

const validRegex = /[a-z]/;
// ✅ Valid syntax

// Catches common mistakes
const unclosed = /hello(/;
// ❌ Error: Unclosed group

const invalidEscape = /\k/;
// ❌ Error: Invalid escape sequence
```

### Real-World Application: Input Validation

```typescript
// Email validation with syntax checking
const EMAIL_REGEX = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/;
// ✅ Syntax validated at compile time

// Phone validation
const PHONE_REGEX = /^\+?[1-9]\d{1,14}$/;
// ✅ Valid syntax

// Would catch syntax errors
const BROKEN_REGEX = /^\+?[1-9\d{1,14}$/;
// ❌ Error detected: unclosed character class
```

## Performance Improvements

**Monorepo Build Performance:**

- 60% faster incremental builds in large monorepos
- 20-30% reduction in type checker memory usage
- Improved project reference resolution

**Type Checking Performance:**

- Faster type predicate inference
- Optimized control flow analysis
- Better caching for const indexed accesses

**Editor Performance:**

- Faster autocomplete with inferred predicates
- Reduced lag in large projects
- Better IntelliSense responsiveness

## Breaking Changes

**Minimal breaking changes:**

1. **Inferred type predicates** - Functions may now narrow more aggressively
2. **Stricter control flow** - May reveal previously hidden type errors
3. **`lib.d.ts` updates** - ES2024 features added
4. **Regex validation** - Invalid regex syntax now caught at compile time

## Migration Guide

**Step 1: Update TypeScript**

```bash
npm install -D typescript@5.5
```

**Step 2: Remove Manual Type Predicates**

Replace manual predicates where automatic inference works:

```typescript
// Before
function isString(value: unknown): value is string {
  return typeof value === "string";
}

// After - remove explicit predicate
function isString(value: unknown) {
  return typeof value === "string";
}
// ✅ Automatically inferred
```

**Step 3: Leverage Const Indexed Access Narrowing**

Use `as const` for configuration objects:

```typescript
const config = {
  dev: {
    /* ... */
  },
  prod: {
    /* ... */
  },
} as const; // ← Add as const

function getConfig(env: keyof typeof config) {
  return config[env]; // ✅ Narrowed automatically
}
```

**Step 4: Fix Invalid Regex Syntax**

Review and fix regex validation errors:

```typescript
// Before - invalid but not caught
const regex = /[a-z/;

// After - fix syntax
const regex = /[a-z]/;
```

**Step 5: Adopt JSDoc Imports (Optional)**

Convert JavaScript files to use JSDoc imports:

```javascript
// Add JSDoc imports for type safety
/**
 * @import { User } from "./types"
 */
```

## Summary

TypeScript 5.5 (June 2024) introduced automatic type predicate inference and major performance improvements:

- **Inferred Type Predicates** - Automatic type narrowing in filter/validation functions
- **Control Flow Narrowing for Const Indexed Accesses** - Better narrowing through object lookups
- **JSDoc `@import` Tag** - Import types in JSDoc without runtime imports
- **Regular Expression Syntax Checking** - Compile-time regex validation
- **Performance Optimizations** - 60% faster builds, 20-30% less memory

**Impact:** Inferred type predicates eliminate boilerplate and make type narrowing automatic, while performance improvements make TypeScript viable for even larger codebases.

**Next Steps:**

- Continue to [TypeScript 5.6](/en/learn/software-engineering/programming-languages/typescript/release-highlights/typescript-5-6) for iterator helper methods and region-prioritized diagnostics
- Return to [Overview](/en/learn/software-engineering/programming-languages/typescript/release-highlights/overview) for full timeline

## References

- [Official TypeScript 5.5 Release Notes](https://devblogs.microsoft.com/typescript/announcing-typescript-5-5/)
- [TypeScript 5.5 Documentation](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-5-5.html)
- [Inferred Type Predicates Design](https://github.com/microsoft/TypeScript/pull/57465)
- [Control Flow Narrowing Improvements](https://github.com/microsoft/TypeScript/pull/57847)
