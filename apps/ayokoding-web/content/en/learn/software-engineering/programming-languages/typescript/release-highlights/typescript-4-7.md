---
title: "TypeScript 4 7"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "TypeScript 4.7 release highlights - ECMAScript module support in Node.js, instantiation expressions, improved function inference, variance annotations"
weight: 10000008
tags: ["typescript", "typescript-4-7", "esm", "node-js", "instantiation-expressions"]
---

## Release Overview

**TypeScript 4.7** was released on **May 24, 2022**, delivering first-class ECMAScript module (ESM) support for Node.js, instantiation expressions for generic functions, and variance annotations for better type safety.

**Key Metrics:**

- **Release Date:** May 24, 2022
- **Major Focus:** Node.js ESM support, generic function improvements, type system enhancements
- **Breaking Changes:** Minimal
- **Performance:** Improved module resolution for Node.js projects

## ECMAScript Module Support in Node.js

**Landmark Feature:** Full support for ECMAScript modules in Node.js with proper `package.json` configuration and module resolution.

### The Problem Before TypeScript 4.7

**Before:** Limited ESM support for Node.js, complex workarounds needed.

```typescript
// TypeScript 4.6 and earlier
// Difficult to configure proper ESM for Node.js
// - Required hacky tsconfig.json settings
// - Module resolution didn't match Node.js behavior
// - Import/export inconsistencies

// package.json
{
  "type": "module"  // Node.js ESM
}

// tsconfig.json - problematic configuration
{
  "compilerOptions": {
    "module": "esnext",  // Doesn't match Node.js exactly
    "moduleResolution": "node"  // CJS-oriented
  }
}
```

### With TypeScript 4.7

**Solution:** New `module` and `moduleResolution` settings designed for Node.js ESM.

```typescript
// TypeScript 4.7 and later
// package.json
{
  "type": "module"
}

// tsconfig.json - native ESM support
{
  "compilerOptions": {
    "module": "es2022",
    // => ✅ Modern ESM output

    "moduleResolution": "node16",
    // => ✅ Node.js 16+ resolution (ESM-aware)
    // => or "nodenext" for latest Node.js

    "target": "es2022"
  }
}

// Now imports work as expected
import { readFile } from "fs/promises";
// => ✅ Correctly resolves to Node.js ESM
// => .js extension required in output

import { User } from "./types.js";
// => ✅ Must include .js extension (Node.js ESM requirement)
```

### Real-World Application: Node.js ESM Project Setup

**Complete ESM project configuration:**

```json
// package.json
{
  "name": "my-esm-project",
  "type": "module",
  "engines": {
    "node": ">=16"
  },
  "scripts": {
    "build": "tsc",
    "start": "node dist/index.js"
  },
  "devDependencies": {
    "typescript": "^4.7.0"
  }
}

// tsconfig.json
{
  "compilerOptions": {
    "target": "es2022",
    "module": "es2022",
    "moduleResolution": "node16",
    "outDir": "./dist",
    "rootDir": "./src",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules"]
}
```

```typescript
// src/index.ts
import { readFile } from "fs/promises";
// => ✅ Node.js built-in ESM import

import { processUser } from "./utils.js";
// => ✅ Relative import with .js extension
// => TypeScript resolves to ./utils.ts during compilation
// => Output uses ./utils.js at runtime

const data = await readFile("./config.json", "utf-8");
// => ✅ Top-level await (ESM)

const config = JSON.parse(data);
// => config is any

processUser(config.userId);
```

### Real-World Application: Mixed CJS/ESM Package

**Support both CommonJS and ESM exports:**

```json
// package.json
{
  "name": "my-library",
  "type": "module",
  "exports": {
    ".": {
      "import": "./dist/esm/index.js",
      "require": "./dist/cjs/index.cjs"
    }
  },
  "scripts": {
    "build:esm": "tsc -p tsconfig.esm.json",
    "build:cjs": "tsc -p tsconfig.cjs.json",
    "build": "npm run build:esm && npm run build:cjs"
  }
}

// tsconfig.esm.json
{
  "extends": "./tsconfig.json",
  "compilerOptions": {
    "module": "es2022",
    "moduleResolution": "node16",
    "outDir": "./dist/esm"
  }
}

// tsconfig.cjs.json
{
  "extends": "./tsconfig.json",
  "compilerOptions": {
    "module": "commonjs",
    "moduleResolution": "node",
    "outDir": "./dist/cjs",
    "outExtension": { ".js": ".cjs" }
  }
}
```

## Instantiation Expressions

**Feature:** Create specialized versions of generic functions without calling them, using explicit type arguments.

### Syntax

```typescript
// Before TypeScript 4.7 - must create wrapper functions
function identity<T>(value: T): T {
  return value;
}

// Create specialized version - verbose
const numberIdentity = (value: number) => identity(value);
// => Wrapper function required

// TypeScript 4.7 - instantiation expression
const numberIdentity = identity<number>;
// => ✅ Direct instantiation without wrapper
// => numberIdentity is (value: number) => number

const stringIdentity = identity<string>;
// => ✅ stringIdentity is (value: string) => string
```

### Real-World Application: Type-Safe Event Emitters

**Specialized event handlers without wrappers:**

```typescript
class EventEmitter<T> {
  private listeners: Array<(data: T) => void> = [];

  on(listener: (data: T) => void): void {
    this.listeners.push(listener);
  }

  emit(data: T): void {
    this.listeners.forEach((listener) => listener(data));
  }
}

// Generic createEmitter function
function createEmitter<T>(): EventEmitter<T> {
  return new EventEmitter<T>();
}

// Before 4.7 - wrapper functions
const createUserEmitter = () => createEmitter<User>();
const createPostEmitter = () => createEmitter<Post>();

// TypeScript 4.7 - instantiation expressions
const createUserEmitter = createEmitter<User>;
// => ✅ () => EventEmitter<User>

const createPostEmitter = createEmitter<Post>;
// => ✅ () => EventEmitter<Post>

// Usage
interface User {
  id: number;
  name: string;
}

interface Post {
  id: number;
  title: string;
}

const userEmitter = createUserEmitter();
// => userEmitter is EventEmitter<User>

userEmitter.on((user) => {
  // => user is User
  console.log(`User: ${user.name}`);
});

userEmitter.emit({ id: 1, name: "Alice" });
// => Type-safe emission
```

### Real-World Application: API Client Factories

**Type-safe endpoint clients:**

```typescript
interface APIConfig<T> {
  baseURL: string;
  parse: (data: unknown) => T;
}

function createAPIClient<T>(config: APIConfig<T>) {
  return {
    async fetch(endpoint: string): Promise<T> {
      const response = await fetch(`${config.baseURL}${endpoint}`);
      const data = await response.json();
      return config.parse(data);
    },
  };
}

interface User {
  id: number;
  name: string;
  email: string;
}

interface Post {
  id: number;
  title: string;
  content: string;
}

// Create specialized client factories
const createUserClient = createAPIClient<User>;
// => ✅ (config: APIConfig<User>) => APIClient<User>

const createPostClient = createAPIClient<Post>;
// => ✅ (config: APIConfig<Post>) => APIClient<Post>

// Usage
const userClient = createUserClient({
  baseURL: "https://api.example.com",
  parse: (data: unknown) => data as User,
});

const user = await userClient.fetch("/users/1");
// => user is User
```

### Real-World Application: Array Method Specialization

**Reusable specialized array methods:**

```typescript
function map<T, U>(arr: T[], fn: (item: T) => U): U[] {
  return arr.map(fn);
}

// Create specialized mappers
const mapNumbers = map<number, string>;
// => ✅ (arr: number[], fn: (item: number) => string) => string[]

const mapStrings = map<string, number>;
// => ✅ (arr: string[], fn: (item: string) => number) => number[]

// Usage
const numbers = [1, 2, 3];
const strings = mapNumbers(numbers, (n) => n.toString());
// => strings is string[]
// => ["1", "2", "3"]

const words = ["one", "two", "three"];
const lengths = mapStrings(words, (s) => s.length);
// => lengths is number[]
// => [3, 3, 5]
```

## Improved Function Inference

**Feature:** Better type inference for functions in object literals and other contexts.

### Example

```typescript
// Before TypeScript 4.7 - manual type annotations needed
interface Config {
  transform: (value: string) => number;
}

const config: Config = {
  transform: (value) => {
    // => value is any (inference failed)
    return value.length;
  },
};

// TypeScript 4.7 - automatic inference
const config: Config = {
  transform: (value) => {
    // => ✅ value is string (inferred from Config)
    return value.length;
  },
};
```

### Real-World Application: Route Handler Configuration

**Type-safe route definitions with inferred handlers:**

```typescript
interface RouteConfig<T> {
  path: string;
  handler: (req: Request<T>) => Response;
}

interface Request<T> {
  params: T;
  query: Record<string, string>;
}

interface Response {
  status: number;
  body: unknown;
}

// TypeScript 4.7 - handler parameters inferred
const userRoute: RouteConfig<{ id: number }> = {
  path: "/users/:id",
  handler: (req) => {
    // => ✅ req is Request<{ id: number }>
    const userId = req.params.id;
    // => userId is number (inferred correctly)

    return {
      status: 200,
      body: { userId },
    };
  },
};

const searchRoute: RouteConfig<{}> = {
  path: "/search",
  handler: (req) => {
    // => ✅ req is Request<{}>
    const query = req.query.q;
    // => query is string | undefined

    return {
      status: 200,
      body: { query },
    };
  },
};
```

## `typeof` on Private Fields

**Feature:** Use `typeof` operator on private class fields to extract their types.

### Example

```typescript
class Container {
  #value = "secret";
  // => Private field with inferred type

  getValueType() {
    type ValueType = typeof this.#value;
    // => ✅ ValueType is string

    return typeof this.#value;
    // => Returns "string" at runtime
  }
}

const container = new Container();
// => container is Container

const valueType = container.getValueType();
// => valueType is "string"
```

### Real-World Application: Type-Safe Private State

**Extract types from private implementation details:**

```typescript
class StateMachine {
  #state = {
    current: "idle" as "idle" | "loading" | "success" | "error",
    data: null as string | null,
    error: null as Error | null,
  };

  // Use typeof for type extraction
  getCurrentState(): typeof this.#state {
    return { ...this.#state };
    // => Returns copy of private state
  }

  setState(newState: Partial<typeof this.#state>) {
    // => ✅ newState typed from private field
    this.#state = { ...this.#state, ...newState };
  }
}

const machine = new StateMachine();
// => machine is StateMachine

const state = machine.getCurrentState();
// => state is { current: "idle" | "loading" | "success" | "error"; data: string | null; error: Error | null }

machine.setState({ current: "loading" });
// => ✅ Type-safe update

// machine.setState({ current: "invalid" });
// => ❌ Error - "invalid" not in union
```

## `moduleSuffixes` Compiler Option

**Feature:** Customize module file suffix resolution for platform-specific imports.

### Configuration

```json
{
  "compilerOptions": {
    "moduleSuffixes": [".ios", ".native", ""]
  }
}
```

**Resolution order:**

```typescript
import { Component } from "./Button";
// => Tries in order:
// => 1. Button.ios.ts
// => 2. Button.native.ts
// => 3. Button.ts
```

### Real-World Application: React Native Platform-Specific Code

**Automatic platform-specific module resolution:**

```typescript
// Button.ios.ts (iOS-specific implementation)
export function Button(props: { title: string; onPress: () => void }) {
  return <IOSButton {...props} />;
}

// Button.native.ts (shared native implementation)
export function Button(props: { title: string; onPress: () => void }) {
  return <NativeButton {...props} />;
}

// Button.ts (web fallback)
export function Button(props: { title: string; onPress: () => void }) {
  return <button onClick={props.onPress}>{props.title}</button>;
}

// App.tsx - automatic resolution
import { Button } from "./Button";
// => ✅ Resolves to Button.ios.ts on iOS
// => ✅ Resolves to Button.native.ts on Android
// => ✅ Resolves to Button.ts on web
```

## Variance Annotations (Experimental)

**Feature:** Explicit variance annotations for generic type parameters using `in` and `out` modifiers.

### Syntax

```typescript
// Covariant (out) - type parameter only appears in output positions
type Source<out T> = {
  get(): T;
  // => ✅ T in output position
};

// Contravariant (in) - type parameter only appears in input positions
type Sink<in T> = {
  put(value: T): void;
  // => ✅ T in input position
};

// Invariant (in out) - type parameter in both positions
type Store<in out T> = {
  get(): T;
  put(value: T): void;
  // => T in both positions
};
```

### Real-World Application: Type-Safe Event Handlers

**Variance for handler type safety:**

```typescript
// Contravariant - handlers accept more specific types
type EventHandler<in E> = (event: E) => void;

interface MouseEvent {
  x: number;
  y: number;
}

interface ClickEvent extends MouseEvent {
  button: number;
}

// Contravariance allows this assignment
const handleMouse: EventHandler<MouseEvent> = (e) => {
  console.log(`Position: ${e.x}, ${e.y}`);
};

const handleClick: EventHandler<ClickEvent> = handleMouse;
// => ✅ Allowed - EventHandler is contravariant in E
// => MouseEvent handler can handle ClickEvent (more specific)
```

## Performance Improvements

**Build Performance:**

- 10-20% faster module resolution with Node.js ESM settings
- Improved type checking for complex generic functions
- Better incremental compilation with instantiation expressions

**Editor Performance:**

- Faster IntelliSense with ESM imports
- Better responsiveness with large Node.js projects
- Reduced lag with complex generic type inference

## Breaking Changes

**ESM-related changes:**

1. **File extension requirements** - ESM requires explicit `.js` extensions in imports
2. **`moduleResolution: "node16"/"nodenext"`** - Different resolution behavior than `"node"`
3. **`lib.d.ts` updates** - New ES2022 and Node.js type definitions

## Migration Guide

**Step 1: Update TypeScript**

```bash
npm install -D typescript@4.7
# => Installs TypeScript 4.7
```

**Step 2: Configure Node.js ESM (if applicable)**

```json
// package.json
{
  "type": "module"
}

// tsconfig.json
{
  "compilerOptions": {
    "module": "es2022",
    "moduleResolution": "node16",
    "target": "es2022"
  }
}
```

**Step 3: Add `.js` Extensions to Relative Imports**

```typescript
// Before
import { User } from "./types";

// After - ESM requires .js extension
import { User } from "./types.js";
// => TypeScript resolves to ./types.ts
// => Output uses ./types.js
```

**Step 4: Use Instantiation Expressions**

Replace wrapper functions:

```typescript
// Before
const numberParser = (value: string) => parse<number>(value);

// After
const numberParser = parse<number>;
// => ✅ Direct instantiation
```

**Step 5: Leverage Improved Inference**

Remove unnecessary type annotations:

```typescript
// Before
const config: Config = {
  transform: (value: string) => value.length,
};

// After - inferred automatically
const config: Config = {
  transform: (value) => value.length,
  // => value inferred as string
};
```

## Upgrade Recommendations

**Immediate Actions:**

1. Update to TypeScript 4.7 for Node.js ESM support
2. Configure `moduleResolution: "node16"` for Node.js projects
3. Use instantiation expressions to reduce boilerplate

**Future Considerations:**

1. Migrate existing Node.js projects to native ESM
2. Add explicit variance annotations for complex generic types
3. Use `moduleSuffixes` for platform-specific code organization

## Summary

TypeScript 4.7 (May 2022) delivered modern module system support and generic function improvements:

- **Node.js ESM support** - First-class ECMAScript module support with `moduleResolution: "node16"`
- **Instantiation expressions** - Create specialized generic functions without wrappers
- **Improved function inference** - Better type inference in object literals
- **`typeof` on private fields** - Extract types from private class members
- **`moduleSuffixes`** - Platform-specific module resolution
- **Variance annotations** - Explicit covariance/contravariance (experimental)

**Impact:** Node.js ESM support modernized TypeScript for server-side development, aligning with ECMAScript standards.

**Next Steps:**

- Continue to [TypeScript 4.8](/en/learn/software-engineering/programming-languages/typescript/release-highlights/typescript-4-8) for improved intersection types and inference
- Return to [Overview](/en/learn/software-engineering/programming-languages/typescript/release-highlights/overview) for full timeline

## References

- [Official TypeScript 4.7 Release Notes](https://devblogs.microsoft.com/typescript/announcing-typescript-4-7/)
- [TypeScript 4.7 Documentation](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-7.html)
- [ECMAScript Modules in Node.js](https://www.typescriptlang.org/docs/handbook/esm-node.html)
