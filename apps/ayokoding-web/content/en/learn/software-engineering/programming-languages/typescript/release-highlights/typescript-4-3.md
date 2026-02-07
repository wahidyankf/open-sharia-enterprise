---
title: "TypeScript 4 3"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "TypeScript 4.3 release highlights - separate read/write property types, override keyword, and static index signatures"
weight: 10000004
tags: ["typescript", "typescript-4-3", "property-types", "override-keyword"]
---

## Release Overview

**TypeScript 4.3** was released on **May 26, 2021**, introducing separate read/write property types for accessors, enabling more precise modeling of getters and setters.

**Key Metrics:**

- **Release Date:** May 26, 2021
- **Major Focus:** Accessor type flexibility and class improvements
- **Breaking Changes:** Minimal
- **Performance:** Improved type checking speed

## Separate Read/Write Types on Properties

**Major Feature:** Get and set accessors can now have different types.

### Syntax

```typescript
class SafeBox {
  private _value: string = "";

  // Read returns string | undefined
  get value(): string | undefined {
    return this._value || undefined;
  }

  // Write accepts string (not undefined)
  set value(newValue: string) {
    this._value = newValue;
  }
}

const box = new SafeBox();
box.value = "secret"; // ✅ Accepts string
const retrieved: string | undefined = box.value; // ✅ Returns string | undefined
box.value = undefined; // ❌ Error - setter only accepts string
```

### Real-World Application: Validation on Write

**Sanitize input while preserving read type:**

```typescript
class EmailField {
  private _email: string = "";

  // Read returns normalized email
  get email(): string {
    return this._email.toLowerCase();
  }

  // Write accepts string but validates
  set email(value: string) {
    if (!value.includes("@")) {
      throw new Error("Invalid email format");
    }
    this._email = value.trim();
  }
}

const field = new EmailField();
field.email = "  USER@EXAMPLE.COM  "; // Trims and stores
console.log(field.email); // "user@example.com" (lowercase)
```

### Real-World Application: Coercion

**Accept flexible input, return precise type:**

```typescript
class Port {
  private _port: number = 8080;

  get port(): number {
    return this._port;
  }

  // Accept string or number, coerce to number
  set port(value: string | number) {
    const parsed = typeof value === "string" ? parseInt(value, 10) : value;
    if (isNaN(parsed) || parsed < 1 || parsed > 65535) {
      throw new Error("Invalid port number");
    }
    this._port = parsed;
  }
}

const config = new Port();
config.port = "3000"; // ✅ Accepts string
config.port = 8080; // ✅ Accepts number
const portNumber: number = config.port; // ✅ Always returns number
```

## `override` Keyword

**Feature:** Explicitly mark methods that override base class methods, preventing accidental overrides.

### Syntax

```typescript
class Base {
  greet() {
    console.log("Hello from Base");
  }
}

class Derived extends Base {
  override greet() {
    // ✅ Explicit override
    console.log("Hello from Derived");
  }

  override goodbye() {
    // ❌ Error - no method 'goodbye' in base class
  }
}
```

### Compiler Flag: `--noImplicitOverride`

**Enforce explicit override keyword:**

```json
// tsconfig.json
{
  "compilerOptions": {
    "noImplicitOverride": true
  }
}
```

### Real-World Application: Framework Components

**Prevent accidental method name typos:**

```typescript
abstract class Component {
  abstract render(): string;
  mount(): void {
    console.log("Mounting component");
  }
  unmount(): void {
    console.log("Unmounting component");
  }
}

class Button extends Component {
  override render(): string {
    return "<button>Click me</button>";
  }

  override mount(): void {
    super.mount();
    console.log("Button mounted");
  }

  // Typo protection
  override unmout(): void {
    // ❌ Error - no 'unmout' method in base (catches typo)
  }
}
```

## Static Index Signatures

**Feature:** Allow index signatures on static side of classes.

### Syntax

```typescript
class APIEndpoints {
  [endpoint: string]: string;

  static [key: string]: string;
  static users = "/api/users";
  static posts = "/api/posts";
}

// Access static members dynamically
const endpoint = APIEndpoints["users"]; // "/api/users"
```

### Real-World Application: Configuration Registry

```typescript
class FeatureFlags {
  [key: string]: boolean;

  static [key: string]: boolean;
  static darkMode = true;
  static betaFeatures = false;
  static analytics = true;

  static getAll(): Record<string, boolean> {
    const flags: Record<string, boolean> = {};
    for (const key in this) {
      if (typeof this[key] === "boolean") {
        flags[key] = this[key];
      }
    }
    return flags;
  }
}

// Dynamic access
const isDarkModeEnabled = FeatureFlags["darkMode"]; // true
console.log(FeatureFlags.getAll());
// { darkMode: true, betaFeatures: false, analytics: true }
```

## Template String Type Improvements

**Improvement:** Better inference for template literal types in generic contexts.

### Example

```typescript
function makeEventName<T extends string>(prefix: T, action: string) {
  return `${prefix}:${action}` as const;
}

const event = makeEventName("user", "created");
// Type inferred: "user:created" (literal type, not just string)
```

## `await` in Non-Async Functions (Top-Level)

**Feature:** Support for top-level `await` in modules (ES2022 feature).

### Syntax

```typescript
// module.ts
const data = await fetch("/api/data").then((r) => r.json());
export { data };

// No async wrapper needed for module-level await
```

**Requirement:** Module must have `"module": "es2022"` or later in tsconfig.json.

## Performance Improvements

**Build Performance:**

- 10-15% faster type checking with getters/setters
- Improved inference for template literal types
- Better caching for project references

**Editor Performance:**

- Faster autocomplete for large class hierarchies
- Reduced memory usage for mapped types

## Breaking Changes

**Minor breaking changes:**

1. **`lib.d.ts` updates** - DOM type refinements
2. **Always-truthy promise checks** - `if (promise)` now flagged as likely bug
3. **Getters/setters with different types** - May surface latent type mismatches

## Migration Guide

**Step 1: Update TypeScript**

```bash
npm install -D typescript@4.3
```

**Step 2: Leverage `override` Keyword**

Enable `--noImplicitOverride` for safer inheritance:

```json
{
  "compilerOptions": {
    "noImplicitOverride": true
  }
}
```

**Step 3: Refactor Accessors**

Use different read/write types where appropriate:

```typescript
// Before: Same type for get/set
get value(): string | undefined {
  return this._value;
}
set value(v: string | undefined) {
  this._value = v;
}

// After: Different types
get value(): string | undefined {
  return this._value;
}
set value(v: string) {
  // No undefined accepted
  this._value = v;
}
```

## Summary

TypeScript 4.3 (May 2021) enhanced class and accessor flexibility:

- **Separate read/write types** - Different types for getters and setters
- **`override` keyword** - Explicit method override marking
- **Static index signatures** - Dynamic access to static members
- **Template literal improvements** - Better inference in generics
- **Top-level `await`** - ES2022 module feature support

**Next Steps:**

- Continue to [TypeScript 4.4](/en/learn/software-engineering/programming-languages/typescript/release-highlights/typescript-4-4) for control flow analysis improvements
- Jump to [TypeScript 4.9](/en/learn/software-engineering/programming-languages/typescript/release-highlights/typescript-4-9) for the `satisfies` operator

## References

- [Official TypeScript 4.3 Release Notes](https://devblogs.microsoft.com/typescript/announcing-typescript-4-3/)
- [TypeScript 4.3 Documentation](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-3.html)
