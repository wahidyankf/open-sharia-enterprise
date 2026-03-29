---
title: "TypeScript 4 6"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "TypeScript 4.6 release highlights - Control flow analysis for destructured discriminated unions, improved recursion depth, indexed access inference improvements"
weight: 10000007
tags: ["typescript", "typescript-4-6", "control-flow", "destructuring", "inference"]
---

## Release Overview

**TypeScript 4.6** was released on **February 28, 2022**, introducing control flow analysis for destructured discriminated unions, improved recursion depth for recursive types, and better indexed access type inference.

**Key Metrics:**

- **Release Date:** February 28, 2022
- **Major Focus:** Advanced control flow, recursion handling, type inference
- **Breaking Changes:** Minimal
- **Performance:** Improved type checking for complex recursive types

## Control Flow Analysis for Destructured Discriminated Unions

**Landmark Feature:** TypeScript now correctly narrows types when destructuring discriminated unions.

### The Problem Before TypeScript 4.6

**Before:** Type narrowing didn't work after destructuring discriminated unions.

```typescript
// TypeScript 4.5 and earlier
type Result = { success: true; data: string } | { success: false; error: Error };

function process(result: Result) {
  const { success } = result;
  // => success is boolean

  if (success) {
    console.log(result.data);
    // => ❌ Error: Property 'data' does not exist on type 'Result'
    // => TypeScript doesn't narrow result after destructuring
  }
}
```

### With TypeScript 4.6

**Solution:** Control flow analysis tracks destructured discriminant properties.

```typescript
// TypeScript 4.6 and later
type Result = { success: true; data: string } | { success: false; error: Error };

function process(result: Result) {
  const { success } = result;
  // => success is boolean
  // => TypeScript remembers connection to result

  if (success) {
    console.log(result.data);
    // => ✅ OK - result narrowed to { success: true; data: string }
    // => data is string
  } else {
    console.error(result.error);
    // => ✅ OK - result narrowed to { success: false; error: Error }
    // => error is Error
  }
}
```

### Real-World Application: API Response Handling

**Type-safe response processing with destructuring:**

```typescript
type APIResponse<T> =
  | { status: "success"; data: T; timestamp: number }
  | { status: "error"; message: string; code: number }
  | { status: "loading" };

function handleResponse<T>(response: APIResponse<T>): T | null {
  const { status } = response;
  // => status is "success" | "error" | "loading"
  // => TypeScript tracks connection to response

  if (status === "loading") {
    console.log("Loading...");
    // => response narrowed to { status: "loading" }
    return null;
  }

  if (status === "error") {
    // => ✅ response narrowed to { status: "error"; message: string; code: number }
    console.error(`Error ${response.code}: ${response.message}`);
    // => code is number
    // => message is string
    return null;
  }

  // => ✅ response narrowed to { status: "success"; data: T; timestamp: number }
  console.log(`Received data at ${response.timestamp}`);
  // => timestamp is number

  return response.data;
  // => data is T
}

// Example usage
interface User {
  id: number;
  name: string;
}

const userResponse: APIResponse<User> = {
  status: "success",
  data: { id: 1, name: "Alice" },
  timestamp: Date.now(),
};

const user = handleResponse(userResponse);
// => user is User | null
```

### Real-World Application: Form Validation Results

**Destructured validation with preserved narrowing:**

```typescript
type ValidationResult<T> = { valid: true; value: T; warnings: string[] } | { valid: false; errors: string[] };

function processFormData<T>(result: ValidationResult<T>): void {
  const { valid } = result;
  // => valid is boolean
  // => Connection to result preserved

  if (!valid) {
    // => ✅ result narrowed to { valid: false; errors: string[] }
    result.errors.forEach((error) => {
      // => errors is string[]
      console.error(`Validation error: ${error}`);
    });
    return;
  }

  // => ✅ result narrowed to { valid: true; value: T; warnings: string[] }
  if (result.warnings.length > 0) {
    // => warnings is string[]
    console.warn(`Warnings: ${result.warnings.join(", ")}`);
  }

  saveToDatabase(result.value);
  // => value is T
}

// Example usage
interface FormData {
  email: string;
  age: number;
}

const validation: ValidationResult<FormData> = {
  valid: true,
  value: { email: "alice@example.com", age: 30 },
  warnings: ["Email domain not verified"],
};

processFormData(validation);
```

### Real-World Application: Event Handling

**Type-safe event processing with destructured discriminants:**

```typescript
type AppEvent =
  | { type: "userLogin"; userId: number; timestamp: Date }
  | { type: "userLogout"; userId: number; reason: string }
  | { type: "pageView"; path: string; referrer: string }
  | { type: "error"; message: string; stack: string };

function logEvent(event: AppEvent): void {
  const { type } = event;
  // => type is "userLogin" | "userLogout" | "pageView" | "error"
  // => Connection to event preserved

  if (type === "userLogin") {
    // => ✅ event narrowed to { type: "userLogin"; userId: number; timestamp: Date }
    console.log(`User ${event.userId} logged in at ${event.timestamp}`);
    // => userId is number
    // => timestamp is Date
  }

  if (type === "userLogout") {
    // => ✅ event narrowed to { type: "userLogout"; userId: number; reason: string }
    console.log(`User ${event.userId} logged out: ${event.reason}`);
    // => userId is number
    // => reason is string
  }

  if (type === "pageView") {
    // => ✅ event narrowed to { type: "pageView"; path: string; referrer: string }
    console.log(`Page view: ${event.path} from ${event.referrer}`);
    // => path is string
    // => referrer is string
  }

  if (type === "error") {
    // => ✅ event narrowed to { type: "error"; message: string; stack: string }
    console.error(`Error: ${event.message}\n${event.stack}`);
    // => message is string
    // => stack is string
  }
}
```

### Real-World Application: State Machine

**Type-safe state transitions with destructuring:**

```typescript
type MachineState =
  | { state: "idle"; lastAction: null }
  | { state: "loading"; progress: number; startTime: Date }
  | { state: "success"; result: string; duration: number }
  | { state: "error"; error: Error; retryCount: number };

function handleStateChange(current: MachineState, next: MachineState): void {
  const { state: currentState } = current;
  // => currentState is "idle" | "loading" | "success" | "error"

  const { state: nextState } = next;
  // => nextState is "idle" | "loading" | "success" | "error"

  // Validate transitions
  if (currentState === "idle" && nextState === "loading") {
    // => ✅ next narrowed to { state: "loading"; progress: number; startTime: Date }
    console.log(`Starting load at ${next.startTime}`);
    // => startTime is Date
  }

  if (currentState === "loading" && nextState === "success") {
    // => ✅ current narrowed to { state: "loading"; ... }
    // => ✅ next narrowed to { state: "success"; result: string; duration: number }
    const loadTime = next.duration;
    // => loadTime is number

    console.log(`Loaded in ${loadTime}ms: ${next.result}`);
    // => result is string
  }

  if (nextState === "error") {
    // => ✅ next narrowed to { state: "error"; error: Error; retryCount: number }
    console.error(`Error (retry ${next.retryCount}): ${next.error.message}`);
    // => retryCount is number
    // => error is Error
  }
}
```

## Improved Recursion Depth for Recursive Types

**Feature:** Increased recursion depth limit for recursive type aliases, reducing "Type instantiation is excessively deep" errors.

### Example with Recursive JSON Types

```typescript
// TypeScript 4.5 - hits recursion limit quickly
type JSON = string | number | boolean | null | JSON[] | { [key: string]: JSON };

// Deeply nested usage could error:
// ❌ Type instantiation is excessively deep and possibly infinite

// TypeScript 4.6 - higher recursion limit
type JSON = string | number | boolean | null | JSON[] | { [key: string]: JSON };

// ✅ Handles much deeper nesting
const deeplyNested: JSON = {
  level1: {
    level2: {
      level3: {
        level4: {
          level5: {
            value: "deep",
          },
        },
      },
    },
  },
};
```

### Real-World Application: Nested Component Props

**Type-safe deeply nested component structures:**

```typescript
type ComponentTree =
  | { type: "text"; content: string }
  | { type: "container"; children: ComponentTree[] }
  | { type: "wrapper"; child: ComponentTree };

// TypeScript 4.6 handles deep nesting
const complexUI: ComponentTree = {
  type: "container",
  children: [
    {
      type: "wrapper",
      child: {
        type: "container",
        children: [
          { type: "text", content: "Hello" },
          {
            type: "wrapper",
            child: {
              type: "container",
              children: [{ type: "text", content: "World" }],
            },
          },
        ],
      },
    },
  ],
};
// => ✅ No recursion depth errors
```

## Indexed Access Inference Improvements

**Feature:** Better type inference for indexed access types in generic contexts.

### Example with Generic Getters

```typescript
// TypeScript 4.5 - inference issues
function getProperty<T, K extends keyof T>(obj: T, key: K) {
  return obj[key];
  // => Return type: T[K]
}

// TypeScript 4.6 - improved inference
function getPropertyValue<T, K extends keyof T>(obj: T, key: K): T[K] {
  return obj[key];
  // => ✅ Better inference in complex scenarios
}

interface User {
  id: number;
  name: string;
  email: string;
}

const user: User = { id: 1, name: "Alice", email: "alice@example.com" };

const userId = getPropertyValue(user, "id");
// => ✅ userId is number (correctly inferred)

const userName = getPropertyValue(user, "name");
// => ✅ userName is string
```

### Real-World Application: Type-Safe Object Paths

**Deep property access with type inference:**

```typescript
type PathValue<T, P extends string> = P extends keyof T
  ? T[P]
  : P extends `${infer K}.${infer R}`
    ? K extends keyof T
      ? PathValue<T[K], R>
      : never
    : never;

function getNestedProperty<T, P extends string>(obj: T, path: P): PathValue<T, P> {
  const parts = path.split(".");
  let result: any = obj;

  for (const part of parts) {
    result = result[part];
  }

  return result;
}

interface Company {
  name: string;
  address: {
    street: string;
    city: string;
    country: string;
  };
}

const company: Company = {
  name: "Acme Corp",
  address: {
    street: "123 Main St",
    city: "New York",
    country: "USA",
  },
};

const city = getNestedProperty(company, "address.city");
// => ✅ city is string (correctly inferred from nested path)

const companyName = getNestedProperty(company, "name");
// => ✅ companyName is string
```

## Allowing Code in Constructors Before `super()`

**Feature:** Relaxed restrictions on code before `super()` calls in derived class constructors.

### Example

```typescript
class Base {
  constructor(public value: number) {}
}

class Derived extends Base {
  constructor(flag: boolean) {
    // TypeScript 4.6 - allowed
    const computedValue = flag ? 100 : 200;
    // => ✅ Code before super() (doesn't use 'this')

    super(computedValue);
    // => Call super with computed value
  }
}

const instance = new Derived(true);
// => instance.value is 100
```

### Real-World Application: Conditional Initialization

```typescript
class BaseLogger {
  constructor(protected level: string) {}
}

class FileLogger extends BaseLogger {
  constructor(filePath: string, debug: boolean) {
    // Compute log level before calling super
    const logLevel = debug ? "DEBUG" : "INFO";
    // => ✅ Computation before super() allowed

    // Validate file path
    if (!filePath.endsWith(".log")) {
      throw new Error("Invalid log file extension");
      // => ✅ Can throw before super()
    }

    super(logLevel);
    // => Initialize base class with computed level
  }
}
```

## `--target es2022` Support

**Feature:** New compilation target for ECMAScript 2022.

### Configuration

```json
{
  "compilerOptions": {
    "target": "es2022"
  }
}
```

**Enables:**

- Class static initialization blocks
- `#x in obj` private field checks
- Top-level `await`
- `Array.prototype.at()`

## Performance Improvements

**Build Performance:**

- 20-30% faster type checking for destructured discriminated unions
- Reduced memory usage for recursive types
- Improved incremental compilation with complex indexed access types

**Editor Performance:**

- Faster IntelliSense with destructured unions
- Better responsiveness with deeply nested types
- Reduced lag in files with complex control flow

## Breaking Changes

**Minimal breaking changes:**

1. **`lib.d.ts` updates** - ES2022 features added
2. **Stricter control flow** - May expose previously hidden errors with destructured unions
3. **Constructor behavior** - Slightly different validation for code before `super()`

## Migration Guide

**Step 1: Update TypeScript**

```bash
npm install -D typescript@4.6
# => Installs TypeScript 4.6
```

**Step 2: Leverage Destructured Union Narrowing**

Simplify code with destructured discriminants:

```typescript
// Before - avoid destructuring for narrowing
function process(result: Result) {
  if (result.success) {
    console.log(result.data);
  }
}

// After - destructure freely
function process(result: Result) {
  const { success } = result;

  if (success) {
    console.log(result.data);
    // => ✅ Narrowing works correctly
  }
}
```

**Step 3: Refactor Recursive Types**

Leverage increased recursion depth:

```typescript
// Before - manual depth limits to avoid errors
type LimitedJSON = string | number | boolean | null | LimitedJSON[] | { [key: string]: LimitedJSON };

// After - rely on improved limits
type JSON = string | number | boolean | null | JSON[] | { [key: string]: JSON };
// => ✅ Higher recursion depth supported
```

**Step 4: Consider `--target es2022`**

For modern environments, update `tsconfig.json`:

```json
{
  "compilerOptions": {
    "target": "es2022"
  }
}
```

## Upgrade Recommendations

**Immediate Actions:**

1. Update to TypeScript 4.6 for destructured union narrowing
2. Refactor discriminated union handling to use destructuring
3. Simplify recursive type definitions

**Future Considerations:**

1. Migrate to `es2022` target for modern JavaScript features
2. Leverage relaxed constructor rules for cleaner initialization
3. Use improved indexed access inference for generic utilities

## Summary

TypeScript 4.6 (February 2022) enhanced control flow and type system capabilities:

- **Destructured discriminated union narrowing** - Control flow analysis preserves narrowing after destructuring
- **Improved recursion depth** - Handle deeper recursive type aliases
- **Indexed access inference improvements** - Better type inference in generic contexts
- **Relaxed constructor rules** - Allow code before `super()` calls
- **`--target es2022`** - Support for latest ECMAScript features

**Impact:** Destructured union narrowing became a quality-of-life improvement for everyday TypeScript development, enabling more natural code patterns.

**Next Steps:**

- Continue to [TypeScript 4.7](/en/learn/software-engineering/programming-languages/typescript/release-highlights/typescript-4-7) for ECMAScript module support in Node.js
- Return to [Overview](/en/learn/software-engineering/programming-languages/typescript/release-highlights/overview) for full timeline

## References

- [Official TypeScript 4.6 Release Notes](https://devblogs.microsoft.com/typescript/announcing-typescript-4-6/)
- [TypeScript 4.6 Documentation](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-6.html)
- [Control Flow Analysis Documentation](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)
