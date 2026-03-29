---
title: "TypeScript 5 3"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "TypeScript 5.3 release highlights - Import attributes for JSON modules, enhanced type narrowing, and isolated declarations"
weight: 10000014
tags: ["typescript", "typescript-5-3", "import-attributes", "type-narrowing", "isolated-declarations"]
---

## Release Overview

**TypeScript 5.3** was released on **November 16, 2023**, bringing significant improvements to module imports, type narrowing, and build performance with isolated declarations.

**Key Metrics:**

- **Release Date:** November 16, 2023
- **Major Focus:** Import attributes, enhanced narrowing, isolated declarations
- **Breaking Changes:** Minimal
- **Performance:** Up to 50% faster declaration emit with `--isolatedDeclarations`

## Import Attributes

**Landmark Feature:** Support for ECMAScript import attributes proposal, enabling type-safe imports with runtime validation.

### The Problem It Solves

**Before Import Attributes:** No standardized way to specify import metadata for module types like JSON, CSS, or WebAssembly.

```typescript
// Before - no type safety or validation
import config from "./config.json";
// TypeScript infers type, but no runtime validation
// Build tools need separate configuration

// Potential runtime failures:
// - Wrong file type loaded
// - Missing file not caught at build time
// - No validation of import intent
```

### With Import Attributes

**Solution:** Explicitly declare import type with attributes, enabling both compile-time and runtime validation.

```typescript
// After - explicit type assertion with validation
import config from "./config.json" with { type: "json" };
// ✅ TypeScript validates JSON structure
// ✅ Runtime validates file is JSON
// ✅ Build tools optimize JSON loading
// ✅ Bundlers tree-shake JSON imports

// Type safety preserved
type Config = typeof config;
// Type: { host: string; port: number; ... }

// Invalid attribute caught at compile time
import data from "./data.csv" with { type: "json" };
// ❌ Error: CSV file with JSON attribute mismatch
```

### Real-World Application: Configuration Management

**Type-safe configuration loading with runtime validation:**

```typescript
// config/database.json
{
  "host": "localhost",
  "port": 5432,
  "database": "production",
  "ssl": true
}

// app/database.ts
import dbConfig from "../config/database.json" with { type: "json" };

// ✅ Type inferred from JSON structure
type DBConfig = typeof dbConfig;
// Type: {
//   host: string;
//   port: number;
//   database: string;
//   ssl: boolean;
// }

function connectDatabase() {
  const connection = createConnection({
    host: dbConfig.host, // ✅ Type-safe access
    port: dbConfig.port, // ✅ Number type preserved
    database: dbConfig.database,
    ssl: dbConfig.ssl, // ✅ Boolean type preserved
  });

  // Would error if config structure changes
  // console.log(dbConfig.invalidField); // ❌ Error
}
```

### Real-World Application: Feature Flags

**Load feature flags with guaranteed JSON format:**

```typescript
// features/flags.json
{
  "darkMode": true,
  "betaFeatures": false,
  "analytics": true,
  "notifications": {
    "email": true,
    "push": false
  }
}

// app/features.ts
import flags from "./flags.json" with { type: "json" };

// ✅ Runtime guarantees JSON format
// ✅ Type safety for nested objects

type FeatureFlags = typeof flags;
// Type: {
//   darkMode: boolean;
//   betaFeatures: boolean;
//   analytics: boolean;
//   notifications: {
//     email: boolean;
//     push: boolean;
//   };
// }

function isFeatureEnabled<K extends keyof FeatureFlags>(
  feature: K
): FeatureFlags[K] {
  return flags[feature]; // ✅ Type-safe, validated at runtime
}

// Usage with autocomplete
if (isFeatureEnabled("darkMode")) {
  // ✅ Type: boolean
  enableDarkTheme();
}

if (isFeatureEnabled("notifications")) {
  // ✅ Type: { email: boolean; push: boolean }
  const emailEnabled = flags.notifications.email;
}
```

### Real-World Application: Localization Data

**Type-safe internationalization with validated JSON:**

```typescript
// locales/en.json
{
  "common": {
    "welcome": "Welcome",
    "logout": "Logout"
  },
  "errors": {
    "notFound": "Page not found",
    "unauthorized": "Access denied"
  }
}

// locales/id.json
{
  "common": {
    "welcome": "Selamat datang",
    "logout": "Keluar"
  },
  "errors": {
    "notFound": "Halaman tidak ditemukan",
    "unauthorized": "Akses ditolak"
  }
}

// i18n/translations.ts
import en from "../locales/en.json" with { type: "json" };
import id from "../locales/id.json" with { type: "json" };

// ✅ Both locales must have same structure
type TranslationKeys = typeof en;

const translations: Record<string, TranslationKeys> = {
  en,
  id, // ✅ Type-checked against en structure
};

function translate(
  locale: keyof typeof translations,
  path: string
): string {
  const keys = path.split(".");
  let result: any = translations[locale];

  for (const key of keys) {
    result = result[key];
  }

  return result as string;
}

// Type-safe translation keys
const message = translate("en", "common.welcome");
// ✅ Returns: "Welcome"

const error = translate("id", "errors.notFound");
// ✅ Returns: "Halaman tidak ditemukan"
```

### Real-World Application: API Mock Data

**Load test fixtures with guaranteed structure:**

```typescript
// tests/fixtures/users.json
[
  {
    id: 1,
    name: "Alice",
    email: "alice@example.com",
    role: "admin",
  },
  {
    id: 2,
    name: "Bob",
    email: "bob@example.com",
    role: "user",
  },
];

// tests/user.test.ts
import mockUsers from "./fixtures/users.json" with { type: "json" };

// ✅ Type inferred from JSON array structure
type MockUser = (typeof mockUsers)[number];
// Type: {
//   id: number;
//   name: string;
//   email: string;
//   role: string;
// }

describe("User API", () => {
  it("should fetch user by ID", async () => {
    // Use typed mock data in tests
    const expectedUser = mockUsers[0];
    // ✅ Type: MockUser

    const response = await fetchUser(expectedUser.id);

    expect(response).toEqual(expectedUser);
    expect(response.name).toBe("Alice"); // ✅ Type-safe
    expect(response.role).toBe("admin"); // ✅ Type-safe
  });

  it("should filter admin users", () => {
    const admins = mockUsers.filter((u) => u.role === "admin");
    // ✅ Type: MockUser[]

    expect(admins).toHaveLength(1);
    expect(admins[0].name).toBe("Alice");
  });
});
```

## `switch (true)` Narrowing

**Feature:** TypeScript now narrows types inside `switch (true)` statements based on case conditions.

### Example

```typescript
function processValue(value: string | number | boolean) {
  switch (true) {
    case typeof value === "string":
      // ✅ Narrowed to: string
      console.log(value.toUpperCase());
      break;

    case typeof value === "number":
      // ✅ Narrowed to: number
      console.log(value.toFixed(2));
      break;

    case typeof value === "boolean":
      // ✅ Narrowed to: boolean
      console.log(value ? "true" : "false");
      break;
  }
}
```

### Real-World Application: Complex Validation

```typescript
interface User {
  id: number;
  name: string;
  email?: string;
  verified?: boolean;
}

function validateUser(user: Partial<User>): string {
  switch (true) {
    case user.id === undefined:
      // ✅ TypeScript knows id might be undefined
      return "ID is required";

    case typeof user.id !== "number":
      // ✅ Narrowed: id exists but wrong type
      return "ID must be a number";

    case !user.name:
      // ✅ Narrowed: id is number, name missing
      return "Name is required";

    case user.email && !user.email.includes("@"):
      // ✅ Narrowed: email exists and is string
      return "Invalid email format";

    default:
      // ✅ Narrowed: valid user structure
      return "Valid";
  }
}
```

## Narrowing Comparisons for Booleans

**Feature:** More precise narrowing when comparing boolean variables.

### Example

```typescript
function processFlags(a: boolean, b: boolean) {
  if (a === b) {
    // ✅ Both must be same value (both true or both false)
    console.log("Flags match");
  } else {
    // ✅ Flags differ (one true, one false)
    console.log("Flags differ");
  }
}

function checkFeature(enabled: boolean | undefined) {
  if (enabled === true) {
    // ✅ Narrowed to: true (not just boolean)
    activateFeature();
  } else if (enabled === false) {
    // ✅ Narrowed to: false (not undefined)
    deactivateFeature();
  } else {
    // ✅ Narrowed to: undefined
    console.log("Feature not configured");
  }
}
```

## `instanceof` Narrowing Through Symbol Access

**Feature:** TypeScript narrows types when accessing symbol-indexed properties after `instanceof` checks.

### Example

```typescript
class CustomError extends Error {
  [Symbol.for("errorCode")]: number;

  constructor(message: string, code: number) {
    super(message);
    this[Symbol.for("errorCode")] = code;
  }
}

function handleError(error: unknown) {
  if (error instanceof CustomError) {
    // ✅ Narrowed to: CustomError
    const code = error[Symbol.for("errorCode")];
    // ✅ Type: number (preserves symbol property type)

    console.log(`Error code: ${code}`);
    console.log(`Message: ${error.message}`);
  }
}
```

## Isolated Declarations (`--isolatedDeclarations`)

**Performance Feature:** Generate declaration files (`.d.ts`) without type-checking the entire program.

### The Problem It Solves

**Before:** Declaration emit requires full type-checking, slowing down large monorepos.

```typescript
// Traditional declaration emit:
// 1. Type-check entire program
// 2. Resolve all type dependencies
// 3. Generate .d.ts files
// Time: 30-60 seconds for large projects
```

### With Isolated Declarations

**Solution:** Generate declarations file-by-file without cross-file type resolution.

```typescript
// With --isolatedDeclarations:
// 1. Process each file independently
// 2. Generate .d.ts from explicit types
// 3. Parallel processing possible
// Time: 5-15 seconds (50-75% faster)
```

### Requirements

**Explicit Return Types Required:**

```typescript
// ❌ Won't work with --isolatedDeclarations
export function calculate(x: number) {
  return x * 2; // Implicit return type requires inference
}

// ✅ Works with --isolatedDeclarations
export function calculate(x: number): number {
  return x * 2; // Explicit return type
}

// ❌ Won't work
export const config = {
  host: "localhost", // Implicit object type
  port: 8080,
};

// ✅ Works
export const config: { host: string; port: number } = {
  host: "localhost",
  port: 8080,
};
```

### Real-World Application: Monorepo Build Performance

```json
// tsconfig.json
{
  "compilerOptions": {
    "isolatedDeclarations": true,
    "declaration": true,
    "declarationMap": true
  }
}
```

**Impact:**

- 50% faster declaration emit in large monorepos
- Enables parallel .d.ts generation
- Better incremental builds
- Stricter explicit typing discipline

## Breaking Changes

**1. Import Attributes Syntax:**

```typescript
// Old assertion syntax (deprecated)
import config from "./config.json" assert { type: "json" };

// ✅ New attribute syntax (standard)
import config from "./config.json" with { type: "json" };
```

**2. Stricter `--isolatedDeclarations` Requirements:**

Requires explicit types for exported declarations (see examples above).

## Migration Guide

**Step 1: Update TypeScript**

```bash
npm install -D typescript@5.3
```

**Step 2: Adopt Import Attributes**

Replace import assertions with attributes:

```typescript
// Before
import data from "./data.json" assert { type: "json" };

// After
import data from "./data.json" with { type: "json" };
```

**Step 3: Enable Isolated Declarations (Optional)**

```json
{
  "compilerOptions": {
    "isolatedDeclarations": true
  }
}
```

Add explicit return types to exported functions:

```typescript
// Add return types
export function processData(input: Data): ProcessedData {
  // Implementation
}

// Add type annotations to exports
export const config: Config = {
  // ...
};
```

**Step 4: Leverage Enhanced Narrowing**

Use `switch (true)` for complex conditions:

```typescript
// Refactor complex if-else chains
switch (true) {
  case typeof value === "string":
    // Handle string
    break;
  case typeof value === "number":
    // Handle number
    break;
}
```

## Performance Improvements

**Declaration Emit:**

- 50% faster with `--isolatedDeclarations`
- Parallel processing in monorepos
- Better incremental builds

**Type Narrowing:**

- More precise narrowing reduces unnecessary type assertions
- Better code optimization through narrower types

**Editor Performance:**

- Faster IntelliSense with explicit types (`--isolatedDeclarations`)
- Improved autocomplete in complex switch statements

## Summary

TypeScript 5.3 (November 2023) brought standardized import attributes and major performance improvements:

- **Import Attributes** - Type-safe JSON/module imports with runtime validation
- **`switch (true)` Narrowing** - Better type narrowing in switch statements
- **Boolean Comparison Narrowing** - More precise boolean type narrowing
- **Symbol Access Narrowing** - `instanceof` narrowing preserves symbol properties
- **Isolated Declarations** - 50% faster declaration emit for large projects

**Impact:** Import attributes standardize module metadata, while isolated declarations significantly improve build performance in monorepos.

**Next Steps:**

- Continue to [TypeScript 5.4](/en/learn/software-engineering/programming-languages/typescript/release-highlights/typescript-5-4) for NoInfer utility type and preserved narrowing
- Return to [Overview](/en/learn/software-engineering/programming-languages/typescript/release-highlights/overview) for full timeline

## References

- [Official TypeScript 5.3 Release Notes](https://devblogs.microsoft.com/typescript/announcing-typescript-5-3/)
- [TypeScript 5.3 Documentation](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-5-3.html)
- [Import Attributes Proposal](https://github.com/tc39/proposal-import-attributes)
- [Isolated Declarations Design](https://github.com/microsoft/TypeScript/pull/53463)
