---
title: "TypeScript 5 4"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "TypeScript 5.4 release highlights - NoInfer utility type, preserved narrowing in closures, and faster builds with --noCheck"
weight: 10000015
tags: ["typescript", "typescript-5-4", "noinfer", "type-narrowing", "closures", "performance"]
---

## Release Overview

**TypeScript 5.4** was released on **March 6, 2024**, introducing the `NoInfer` utility type for better generic inference control and significantly improved narrowing behavior in closures.

**Key Metrics:**

- **Release Date:** March 6, 2024
- **Major Focus:** `NoInfer` utility, preserved narrowing, build performance
- **Breaking Changes:** Minimal
- **Performance:** Up to 30% faster syntax-only transpilation with `--noCheck`

## `NoInfer<T>` Utility Type

**Landmark Feature:** Prevent TypeScript from using a type position as an inference site for type parameters.

### The Problem It Solves

**Before `NoInfer`:** TypeScript infers from ALL type positions, sometimes causing unwanted inference.

```typescript
// Problem: TypeScript infers from options array
function createMenu<T extends string>(items: T[], defaultItem: T) {
  return { items, defaultItem };
}

const menu = createMenu(["home", "about"], "contact");
// ❌ Error: "contact" is not assignable to type '"home" | "about"'
// T inferred as "home" | "about" from items

// Intent: defaultItem should be ANY string, not limited to items
```

### With `NoInfer<T>`

**Solution:** Exclude specific positions from type inference.

```typescript
function createMenu<T extends string>(items: T[], defaultItem: NoInfer<T>) {
  return { items, defaultItem };
}

const menu = createMenu(["home", "about"], "contact");
// ✅ OK! T inferred only from items: "home" | "about"
// ✅ defaultItem accepts any string matching T constraint
// ✅ Type: { items: ("home" | "about")[]; defaultItem: "home" | "about" }

// TypeScript still validates defaultItem is assignable to T
const menu2 = createMenu([1, 2, 3], "default");
// ❌ Error: number[] not assignable to string[]
```

### Real-World Application: Form Validation

**Control inference for flexible validation rules:**

```typescript
interface ValidationRule<T> {
  field: keyof T;
  validator: (value: T[keyof T]) => boolean;
  message: string;
}

// Without NoInfer - too restrictive
function validate<T, K extends keyof T>(data: T, rules: ValidationRule<T>[]) {
  for (const rule of rules) {
    const value = data[rule.field];
    if (!rule.validator(value)) {
      return { valid: false, message: rule.message };
    }
  }
  return { valid: true };
}

// With NoInfer - flexible validation
function validateData<T>(data: T, rules: ValidationRule<NoInfer<T>>[]) {
  for (const rule of rules) {
    const value = data[rule.field];
    if (!rule.validator(value)) {
      return { valid: false, message: rule.message };
    }
  }
  return { valid: true };
}

interface User {
  name: string;
  email: string;
  age: number;
}

const user: User = {
  name: "Alice",
  email: "alice@example.com",
  age: 30,
};

// ✅ T inferred from data, not from rules
const result = validateData(user, [
  {
    field: "email",
    validator: (v) => typeof v === "string" && v.includes("@"),
    message: "Invalid email",
  },
  {
    field: "age",
    validator: (v) => typeof v === "number" && v >= 18,
    message: "Must be 18+",
  },
]);
// ✅ Type-safe field access, flexible rules
```

### Real-World Application: Event Handler Registration

**Prevent event names from affecting callback inference:**

```typescript
type EventMap = {
  click: MouseEvent;
  keypress: KeyboardEvent;
  focus: FocusEvent;
};

// Without NoInfer - event name affects callback type inference
function addEventListener<K extends keyof EventMap>(event: K, callback: (e: EventMap[K]) => void) {
  // Register callback
}

// With NoInfer - cleaner callback type
function on<K extends keyof EventMap>(event: K, callback: (e: NoInfer<EventMap[K]>) => void) {
  // Register callback
}

// ✅ K inferred from event, callback accepts proper event type
on("click", (e) => {
  // ✅ e is MouseEvent
  console.log(e.clientX, e.clientY);
});

on("keypress", (e) => {
  // ✅ e is KeyboardEvent
  console.log(e.key, e.code);
});

// Callback type still validated
on("click", (e: KeyboardEvent) => {});
// ❌ Error: KeyboardEvent not assignable to MouseEvent
```

### Real-World Application: Configuration Merging

**Control inference for default and override configurations:**

```typescript
interface Config {
  host: string;
  port: number;
  ssl: boolean;
  timeout: number;
}

// Infer from defaults, validate overrides without affecting inference
function createConfig<T extends Partial<Config>>(defaults: T, overrides: NoInfer<Partial<T>>): T {
  return { ...defaults, ...overrides };
}

const defaults = {
  host: "localhost",
  port: 8080,
  ssl: false,
};
// ✅ T inferred: { host: string; port: number; ssl: boolean }

const config = createConfig(defaults, {
  port: 3000, // ✅ Type-checked against defaults
  ssl: true,
});
// ✅ Type: { host: string; port: number; ssl: boolean }
// ✅ config.host is "localhost", port is 3000, ssl is true

const invalid = createConfig(defaults, {
  timeout: 5000, // ❌ Error: timeout not in defaults
});
```

### Real-World Application: Data Filtering

**Prevent filter criteria from narrowing data type:**

```typescript
interface Product {
  id: number;
  name: string;
  category: string;
  price: number;
  inStock: boolean;
}

function filterProducts<T extends Product>(products: T[], criteria: (product: NoInfer<T>) => boolean): T[] {
  return products.filter(criteria);
}

const products: Product[] = [
  { id: 1, name: "Laptop", category: "electronics", price: 1000, inStock: true },
  { id: 2, name: "Mouse", category: "electronics", price: 25, inStock: false },
  { id: 3, name: "Desk", category: "furniture", price: 300, inStock: true },
];

// ✅ T inferred from products array, not from criteria
const electronics = filterProducts(products, (p) => p.category === "electronics");
// ✅ Type: Product[] (not narrowed by criteria)

const inStock = filterProducts(products, (p) => p.inStock && p.price < 500);
// ✅ Type: Product[] with full Product interface

// Criteria still type-checked
const invalid = filterProducts(products, (p) => p.invalidField);
// ❌ Error: Property 'invalidField' does not exist
```

## Preserved Narrowing in Closures

**Feature:** Type narrowing now persists inside closures after the narrowing check.

### The Problem It Solves

**Before:** Narrowing lost inside closures, requiring redundant checks.

```typescript
function processValue(value: string | number | null) {
  if (typeof value === "string") {
    // ✅ Narrowed to string

    setTimeout(() => {
      // ❌ Lost narrowing - type is string | number | null again
      console.log(value.toUpperCase());
      // Error: value might be number or null
    }, 1000);
  }
}
```

### With Preserved Narrowing

**Solution:** Narrowing preserved inside closures.

```typescript
function processValue(value: string | number | null) {
  if (typeof value === "string") {
    // ✅ Narrowed to string

    setTimeout(() => {
      // ✅ Narrowing preserved - type is string
      console.log(value.toUpperCase());
    }, 1000);

    const callback = () => {
      // ✅ Narrowing preserved here too
      return value.toLowerCase();
    };
  }
}
```

### Real-World Application: Event Handlers

```typescript
function setupClickHandler(element: HTMLElement | null) {
  if (element !== null) {
    // ✅ Narrowed to HTMLElement

    element.addEventListener("click", () => {
      // ✅ Narrowing preserved - element is HTMLElement
      element.classList.add("clicked");
      element.style.backgroundColor = "blue";

      setTimeout(() => {
        // ✅ Still narrowed in nested closure
        element.classList.remove("clicked");
      }, 1000);
    });
  }
}
```

### Real-World Application: Async Operations

```typescript
interface User {
  id: number;
  name: string;
  email?: string;
}

async function sendWelcomeEmail(user: User) {
  if (user.email) {
    // ✅ Narrowed to string

    // Simulate async delay
    await delay(1000);

    // ✅ Narrowing preserved across await
    const emailSent = await sendEmail({
      to: user.email, // ✅ Type: string (not string | undefined)
      subject: "Welcome",
      body: `Hello ${user.name}`,
    });

    // ✅ Still narrowed in continuation
    console.log(`Email sent to ${user.email}`);
  }
}
```

### Real-World Application: Promise Chains

```typescript
interface ApiResponse<T> {
  data?: T;
  error?: string;
}

function processResponse<T>(response: ApiResponse<T>) {
  if (response.data) {
    // ✅ Narrowed: data exists

    return Promise.resolve(response.data)
      .then((data) => {
        // ✅ Narrowing preserved - data is T (not T | undefined)
        return transformData(data);
      })
      .then((transformed) => {
        // ✅ Still type-safe
        return saveToDatabase(transformed);
      })
      .catch((err) => {
        // ✅ response.data still narrowed here
        console.error(`Failed to process ${response.data}`);
      });
  }
}
```

## `Object.groupBy` and `Map.groupBy` Support

**Feature:** Built-in support for ECMAScript 2024 grouping methods.

### Object.groupBy

```typescript
interface Product {
  id: number;
  category: string;
  price: number;
}

const products: Product[] = [
  { id: 1, category: "electronics", price: 1000 },
  { id: 2, category: "electronics", price: 500 },
  { id: 3, category: "furniture", price: 300 },
];

// Group by category
const byCategory = Object.groupBy(products, (p) => p.category);
// ✅ Type: Partial<Record<string, Product[]>>
// {
//   electronics: [{ id: 1, ... }, { id: 2, ... }],
//   furniture: [{ id: 3, ... }]
// }

// Access with type safety
const electronics = byCategory.electronics;
// ✅ Type: Product[] | undefined

if (electronics) {
  // ✅ Narrowed to Product[]
  console.log(electronics.length);
}
```

### Map.groupBy

```typescript
// Group using Map for non-string keys
const byPriceRange = Map.groupBy(products, (p) => {
  if (p.price < 500) return "budget";
  if (p.price < 1000) return "mid";
  return "premium";
});
// ✅ Type: Map<string, Product[]>

// Type-safe Map access
const budget = byPriceRange.get("budget");
// ✅ Type: Product[] | undefined

// Iterate with type safety
for (const [range, items] of byPriceRange) {
  // ✅ range: string, items: Product[]
  console.log(`${range}: ${items.length} products`);
}
```

## `--noCheck` Option for Faster Builds

**Performance Feature:** Skip type-checking entirely during transpilation for faster builds.

### Use Case

**Development builds** where you want fast compilation and run type-checking separately.

```json
// tsconfig.build.json (fast transpilation)
{
  "compilerOptions": {
    "noCheck": true,
    "skipLibCheck": true
  }
}

// tsconfig.json (full type-checking)
{
  "compilerOptions": {
    "noCheck": false
  }
}
```

### Build Script Strategy

```json
{
  "scripts": {
    "build:fast": "tsc --noCheck",
    "build:check": "tsc --noEmit",
    "build": "npm run build:check && npm run build:fast"
  }
}
```

**Impact:**

- 30-50% faster transpilation
- Separate type-checking from code generation
- Better CI/CD pipeline optimization

## Breaking Changes

**Minimal breaking changes:**

1. **Closure narrowing behavior** - May reveal previously hidden type errors in closures
2. **`lib.d.ts` updates** - ES2024 features added (`Object.groupBy`, `Map.groupBy`)
3. **Stricter inference** - `NoInfer` may change inference in complex generic scenarios

## Migration Guide

**Step 1: Update TypeScript**

```bash
npm install -D typescript@5.4
```

**Step 2: Adopt `NoInfer` for Better Inference Control**

Identify functions where you want to control inference:

```typescript
// Before - inference from all positions
function merge<T>(a: T, b: T): T {
  return { ...a, ...b };
}

// After - control which parameter drives inference
function merge<T>(a: T, b: NoInfer<T>): T {
  return { ...a, ...b };
}
```

**Step 3: Review Closure Narrowing**

Check closures that previously had type errors due to lost narrowing:

```typescript
// May now work without additional checks
if (value !== null) {
  callbacks.forEach((cb) => {
    cb(value); // ✅ Now narrowed to non-null
  });
}
```

**Step 4: Use Grouping Methods**

Replace manual grouping with built-in methods:

```typescript
// Before - manual grouping
const grouped: Record<string, Product[]> = {};
for (const product of products) {
  if (!grouped[product.category]) {
    grouped[product.category] = [];
  }
  grouped[product.category].push(product);
}

// After - built-in grouping
const grouped = Object.groupBy(products, (p) => p.category);
```

**Step 5: Optimize Build Performance**

Separate type-checking from transpilation:

```json
{
  "scripts": {
    "dev": "tsc --noCheck --watch",
    "typecheck": "tsc --noEmit",
    "build": "npm run typecheck && npm run dev"
  }
}
```

## Performance Improvements

**Compilation Performance:**

- 30% faster with `--noCheck` flag
- Better incremental compilation
- Optimized type-checking in complex generics

**Editor Performance:**

- Faster IntelliSense in closures with preserved narrowing
- Improved autocomplete with `NoInfer`

**Runtime Performance:**

- `Object.groupBy` and `Map.groupBy` optimized in modern engines
- No overhead from `NoInfer` (compile-time only)

## Summary

TypeScript 5.4 (March 2024) introduced critical inference control and narrowing improvements:

- **`NoInfer<T>` Utility Type** - Precise control over generic type inference
- **Preserved Narrowing in Closures** - Type narrowing persists in callbacks and closures
- **`Object.groupBy` / `Map.groupBy`** - Native support for ECMAScript 2024 grouping
- **`--noCheck` Flag** - Faster builds by separating type-checking from transpilation

**Impact:** `NoInfer` solves long-standing generic inference problems, while preserved narrowing eliminates redundant type checks in closures.

**Next Steps:**

- Continue to [TypeScript 5.5](/en/learn/software-engineering/programming-languages/typescript/release-highlights/typescript-5-5) for inferred type predicates
- Return to [Overview](/en/learn/software-engineering/programming-languages/typescript/release-highlights/overview) for full timeline

## References

- [Official TypeScript 5.4 Release Notes](https://devblogs.microsoft.com/typescript/announcing-typescript-5-4/)
- [TypeScript 5.4 Documentation](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-5-4.html)
- [NoInfer Utility Type Design](https://github.com/microsoft/TypeScript/pull/56794)
- [Preserved Narrowing in Closures](https://github.com/microsoft/TypeScript/pull/56908)
