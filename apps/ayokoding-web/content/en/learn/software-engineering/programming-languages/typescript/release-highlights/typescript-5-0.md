---
title: "TypeScript 5 0"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "TypeScript 5.0 release highlights - Standard decorators, const type parameters, enum enhancements, speed and size optimizations"
weight: 10000011
tags: ["typescript", "typescript-5-0", "decorators", "const-type-parameters", "enums", "performance"]
---

## Release Overview

**TypeScript 5.0** was released on **March 16, 2023**, marking a major milestone in TypeScript's evolution with ECMAScript standard decorators, const type parameters, and significant performance improvements.

**Key Metrics:**

- **Release Date:** March 16, 2023
- **Major Focus:** Standard decorators, const type parameters, enum improvements
- **Breaking Changes:** Decorator changes, module resolution updates
- **Performance:** 10-15% faster build times, 20-40% smaller package size

## Decorators - ECMAScript Standard Support

**Landmark Feature:** Full support for Stage 3 ECMAScript decorators proposal, replacing experimental decorators.

### The Evolution of Decorators

**Before TypeScript 5.0:** Experimental decorators (non-standard, required `experimentalDecorators`)

**TypeScript 5.0:** Standard decorators aligned with ECMAScript proposal

```typescript
// Standard decorators (TypeScript 5.0+)
function logged(value: any, context: ClassMethodDecoratorContext) {
  // context provides metadata about decorated element
  const methodName = String(context.name);

  return function (this: any, ...args: any[]) {
    console.log(`Calling ${methodName} with:`, args);
    const result = value.call(this, ...args);
    console.log(`${methodName} returned:`, result);
    return result;
  };
}

class Calculator {
  @logged
  add(a: number, b: number): number {
    return a + b;
  }
}

const calc = new Calculator();
calc.add(2, 3);
// Output:
// Calling add with: [2, 3]
// add returned: 5
```

### Real-World Application: Validation Decorator

**Type-safe validation using standard decorators:**

```typescript
// Validation decorator with metadata
function validate(validationFn: (value: any) => boolean, message: string) {
  return function (target: any, context: ClassFieldDecoratorContext) {
    return function (this: any, initialValue: any) {
      return {
        get() {
          return initialValue;
        },
        set(newValue: any) {
          if (!validationFn(newValue)) {
            throw new Error(`${String(context.name)}: ${message}`);
          }
          initialValue = newValue;
        },
      };
    };
  };
}

class User {
  @validate((val) => val.length >= 3, "Username must be at least 3 characters")
  username: string;

  @validate((val) => val.includes("@"), "Invalid email format")
  email: string;

  constructor(username: string, email: string) {
    this.username = username;
    this.email = email;
  }
}

// ✅ Valid
const user1 = new User("john", "john@example.com");

// ❌ Throws: Username must be at least 3 characters
const user2 = new User("jo", "jo@example.com");

// ❌ Throws: Invalid email format
const user3 = new User("jane", "jane-example.com");
```

### Real-World Application: Memoization Decorator

**Cache expensive function results:**

```typescript
function memoize(target: any, context: ClassMethodDecoratorContext) {
  const cache = new Map<string, any>();

  return function (this: any, ...args: any[]) {
    const key = JSON.stringify(args);

    if (cache.has(key)) {
      console.log(`Cache hit for ${String(context.name)}(${key})`);
      return cache.get(key);
    }

    console.log(`Cache miss for ${String(context.name)}(${key})`);
    const result = target.apply(this, args);
    cache.set(key, result);
    return result;
  };
}

class Fibonacci {
  @memoize
  calculate(n: number): number {
    if (n <= 1) return n;
    return this.calculate(n - 1) + this.calculate(n - 2);
  }
}

const fib = new Fibonacci();
console.log(fib.calculate(10)); // Cache misses during recursion
console.log(fib.calculate(10)); // Cache hit - instant result
```

### Real-World Application: Dependency Injection

**Register and inject services automatically:**

```typescript
// Service registry
const serviceRegistry = new Map<any, any>();

// Register decorator
function injectable(target: any, context: ClassDecoratorContext) {
  serviceRegistry.set(target, new target());
}

// Inject decorator
function inject(serviceClass: any) {
  return function (target: any, context: ClassFieldDecoratorContext) {
    return function (this: any) {
      return serviceRegistry.get(serviceClass);
    };
  };
}

// Define services
@injectable
class LoggerService {
  log(message: string) {
    console.log(`[LOG] ${message}`);
  }
}

@injectable
class DatabaseService {
  query(sql: string) {
    console.log(`[DB] Executing: ${sql}`);
    return [{ id: 1, name: "User 1" }];
  }
}

// Use dependency injection
class UserController {
  @inject(LoggerService)
  logger!: LoggerService;

  @inject(DatabaseService)
  db!: DatabaseService;

  getUsers() {
    this.logger.log("Fetching users");
    return this.db.query("SELECT * FROM users");
  }
}

const controller = new UserController();
controller.getUsers();
// Output:
// [LOG] Fetching users
// [DB] Executing: SELECT * FROM users
```

## Const Type Parameters

**Feature:** Type parameters that prevent type widening and preserve literal types.

### The Problem with Type Widening

**Before const type parameters:**

```typescript
// Generic function without const
function makeArray<T>(value: T): T[] {
  return [value];
}

const numbers = makeArray(1); // Type: number[]
// Lost literal type 1

const strings = makeArray("hello"); // Type: string[]
// Lost literal type "hello"

// Can't create precise union types
type AllowedValues = (typeof numbers)[number]; // Type: number (too wide)
```

### With Const Type Parameters

**Solution:** Preserve exact literal types using `const` modifier.

```typescript
// Generic function WITH const
function makeArray<const T>(value: T): T[] {
  return [value];
}

const numbers = makeArray(1); // Type: 1[]
// ✅ Preserves literal type 1

const strings = makeArray("hello"); // Type: "hello"[]
// ✅ Preserves literal type "hello"

// Precise union types
type AllowedValues = (typeof numbers)[number]; // Type: 1 (precise!)
```

### Real-World Application: Type-Safe Configuration

**Create configurations with exact literal types:**

```typescript
function createConfig<const T extends Record<string, any>>(config: T): T {
  return config;
}

const apiConfig = createConfig({
  baseUrl: "https://api.example.com",
  timeout: 5000,
  retries: 3,
  endpoints: {
    users: "/api/users",
    posts: "/api/posts",
  },
});

// ✅ Type inference preserves exact values
type BaseUrl = typeof apiConfig.baseUrl;
// Type: "https://api.example.com" (exact string)

type Timeout = typeof apiConfig.timeout;
// Type: 5000 (exact number)

type Endpoints = keyof typeof apiConfig.endpoints;
// Type: "users" | "posts" (exact keys)

// Type-safe endpoint access
function callEndpoint(endpoint: Endpoints) {
  const url = apiConfig.baseUrl + apiConfig.endpoints[endpoint];
  // endpoint is precisely typed, no typos possible
}

callEndpoint("users"); // ✅ OK
callEndpoint("invalid"); // ❌ Error - not in Endpoints type
```

### Real-World Application: Route Builder

**Build type-safe route definitions:**

```typescript
function defineRoutes<const T extends Record<string, string>>(routes: T): T {
  return routes;
}

const routes = defineRoutes({
  home: "/",
  about: "/about",
  userProfile: "/user/:id",
  postDetail: "/post/:slug",
  adminDashboard: "/admin/dashboard",
});

// ✅ Exact literal types preserved
type RoutePath = (typeof routes)[keyof typeof routes];
// Type: "/" | "/about" | "/user/:id" | "/post/:slug" | "/admin/dashboard"

type RouteKey = keyof typeof routes;
// Type: "home" | "about" | "userProfile" | "postDetail" | "adminDashboard"

// Type-safe navigation
function navigate(route: RouteKey, params?: Record<string, string>) {
  let path = routes[route];
  if (params) {
    Object.entries(params).forEach(([key, value]) => {
      path = path.replace(`:${key}`, value);
    });
  }
  console.log(`Navigating to: ${path}`);
}

navigate("home"); // ✅ OK
navigate("userProfile", { id: "123" }); // ✅ OK
navigate("invalid"); // ❌ Error - not a valid route
```

## Enum Enhancements

**Feature:** Enums can now have computed members in more scenarios and better support union types.

### Enum with Computed Members

```typescript
// Allowed in TypeScript 5.0
enum Permission {
  Read = 1 << 0, // 1
  Write = 1 << 1, // 2
  Delete = 1 << 2, // 4
  // Computed from other members
  ReadWrite = Read | Write, // 3
  FullAccess = Read | Write | Delete, // 7
}

function checkPermission(userPerm: Permission, required: Permission): boolean {
  return (userPerm & required) === required;
}

const userPermissions = Permission.ReadWrite;
console.log(checkPermission(userPermissions, Permission.Read)); // true
console.log(checkPermission(userPermissions, Permission.Write)); // true
console.log(checkPermission(userPermissions, Permission.Delete)); // false
```

### Real-World Application: Feature Flags

**Bitwise enum operations for efficient feature checks:**

```typescript
enum Features {
  None = 0,
  DarkMode = 1 << 0, // 1
  Notifications = 1 << 1, // 2
  Analytics = 1 << 2, // 4
  BetaFeatures = 1 << 3, // 8
  // Combinations
  DefaultUser = DarkMode | Notifications, // 3
  Premium = DefaultUser | Analytics | BetaFeatures, // 15
}

class FeatureManager {
  private enabledFeatures: Features;

  constructor(features: Features = Features.None) {
    this.enabledFeatures = features;
  }

  enable(feature: Features) {
    this.enabledFeatures |= feature;
  }

  disable(feature: Features) {
    this.enabledFeatures &= ~feature;
  }

  isEnabled(feature: Features): boolean {
    return (this.enabledFeatures & feature) === feature;
  }

  toggle(feature: Features) {
    this.enabledFeatures ^= feature;
  }
}

// Usage
const manager = new FeatureManager(Features.DefaultUser);
console.log(manager.isEnabled(Features.DarkMode)); // true
console.log(manager.isEnabled(Features.Analytics)); // false

manager.enable(Features.Analytics);
console.log(manager.isEnabled(Features.Analytics)); // true

manager.toggle(Features.DarkMode);
console.log(manager.isEnabled(Features.DarkMode)); // false
```

## Supporting Types for Multiple Configuration Files

**Feature:** `--verbatimModuleSyntax` flag for predictable module output.

### The Module Syntax Problem

**Before TypeScript 5.0:** Module output could be unpredictable based on content.

```typescript
// Input TypeScript
import { foo } from "./module";
import type { Bar } from "./module";

// Output could vary based on tsconfig and usage
```

### With `--verbatimModuleSyntax`

**Solution:** Emit exactly what you write, preserving import/export syntax.

```typescript
// tsconfig.json
{
  "compilerOptions": {
    "verbatimModuleSyntax": true
  }
}

// Input TypeScript
import { foo } from "./module";      // ✅ Emitted as-is
import type { Bar } from "./module"; // ✅ Removed (type-only)

// Explicit type-only imports are ALWAYS removed
// Value imports are ALWAYS preserved
// No guessing or implicit behavior
```

### Real-World Application: Library Authoring

**Predictable output for library consumers:**

```typescript
// library.ts
export type Config = {
  apiKey: string;
  timeout: number;
};

export class Client {
  constructor(config: Config) {
    // Implementation
  }
}

// consumer.ts with verbatimModuleSyntax
import type { Config } from "./library"; // Not emitted (type-only)
import { Client } from "./library"; // Always emitted (value)

const config: Config = {
  apiKey: "abc123",
  timeout: 5000,
};

const client = new Client(config);

// Output JavaScript is predictable:
// import { Client } from "./library";
// (Config import completely removed)
```

## Performance Improvements

**Build Performance:**

- **10-15% faster** type checking
- **20-40% smaller** npm package size
- **Faster** `--watch` mode responsiveness
- **Reduced** memory consumption

**Editor Performance:**

- Faster IntelliSense for large projects
- Improved responsiveness with decorators
- Better performance with const type parameters

## Breaking Changes

**1. Decorator Changes**

Old experimental decorators are incompatible with standard decorators:

```typescript
// ❌ Experimental decorators (old)
// tsconfig: "experimentalDecorators": true

// ✅ Standard decorators (new)
// tsconfig: Remove "experimentalDecorators" or use both
```

**Migration:** Keep `experimentalDecorators: true` temporarily if using legacy decorators, migrate incrementally.

**2. `--verbatimModuleSyntax` Implications**

Stricter about import/export syntax:

```typescript
// ❌ Error with verbatimModuleSyntax
import { SomeType } from "./module"; // SomeType is a type, not a value

// ✅ Must be explicit
import type { SomeType } from "./module";
```

**3. Enum Behavior Changes**

Computed enum members have stricter rules about what can be computed.

**4. Module Resolution Updates**

`--module node16` and `--module nodenext` are now more strictly enforced.

## Migration Guide

**Step 1: Update TypeScript**

```bash
npm install -D typescript@5.0
```

**Step 2: Evaluate Decorators**

If using experimental decorators:

```json
// tsconfig.json - Keep experimental for now
{
  "compilerOptions": {
    "experimentalDecorators": true,
    "emitDecoratorMetadata": true
  }
}
```

**Step 3: Try Standard Decorators**

Create new decorators using standard syntax for new code:

```typescript
// New standard decorator
function logged(target: any, context: ClassMethodDecoratorContext) {
  // Standard decorator implementation
}
```

**Step 4: Consider `verbatimModuleSyntax`**

Enable for predictable module output (gradual adoption):

```json
{
  "compilerOptions": {
    "verbatimModuleSyntax": true
  }
}
```

**Step 5: Use Const Type Parameters**

Replace type widening with const parameters:

```typescript
// Before
function create<T>(value: T) { ... }

// After - preserve literals
function create<const T>(value: T) { ... }
```

## Summary

TypeScript 5.0 (March 2023) marked a major milestone with standard decorators:

- **Standard decorators** - ECMAScript Stage 3 proposal support
- **Const type parameters** - Prevent type widening, preserve literals
- **Enum enhancements** - Better computed members support
- **`--verbatimModuleSyntax`** - Predictable module output
- **Performance gains** - 10-15% faster builds, 20-40% smaller package

**Impact:** Standard decorators align TypeScript with JavaScript's future, while const type parameters solve long-standing type precision issues.

**Next Steps:**

- Continue to [TypeScript 5.1](/en/learn/software-engineering/programming-languages/typescript/release-highlights/typescript-5-1) for easier implicit returns and JSX improvements
- Return to [Overview](/en/learn/software-engineering/programming-languages/typescript/release-highlights/overview) for full timeline

## References

- [Official TypeScript 5.0 Release Notes](https://devblogs.microsoft.com/typescript/announcing-typescript-5-0/)
- [TypeScript 5.0 Documentation](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-5-0.html)
- [ECMAScript Decorators Proposal](https://github.com/tc39/proposal-decorators)
- [Const Type Parameters Deep Dive](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-5-0.html#const-type-parameters)
