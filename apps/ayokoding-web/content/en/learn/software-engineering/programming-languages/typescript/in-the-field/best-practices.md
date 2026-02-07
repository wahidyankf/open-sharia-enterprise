---
title: "Best Practices"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "TypeScript best practices for production code - naming conventions, code organization, type safety, and modern patterns"
weight: 10000011
tags: ["typescript", "best-practices", "code-quality", "conventions", "clean-code"]
---

## Why Best Practices Matter

Following TypeScript best practices creates consistent, maintainable codebases that teams can navigate confidently. Conventions reduce cognitive load, prevent bugs, and make code reviews more effective by establishing shared expectations.

**Core Benefits**:

- **Consistency**: Code looks uniform across team members
- **Readability**: Clear naming and organization aids comprehension
- **Maintainability**: Standardized patterns simplify modifications
- **Onboarding**: New developers learn conventions quickly
- **Quality**: Best practices prevent common pitfalls

**Problem**: Without conventions, every developer creates their own style, leading to inconsistent, hard-to-maintain codebases.

**Solution**: Adopt industry-standard TypeScript conventions enforced by tooling (ESLint, Prettier) to ensure quality and consistency.

## Naming Conventions

Consistent naming improves code readability and reduces cognitive load.

### Variable and Function Names

Use camelCase for variables and functions, with descriptive names.

**Pattern**:

```typescript
// ❌ BAD: Unclear abbreviations
let usr = { n: "Alice", a: 30 };
// => Abbreviated names save typing but hurt readability
// => Future developers waste time deciphering meaning

function calc(x: number, y: number): number {
  // => Generic names provide no context
  return x + y;
}

// ✅ GOOD: Descriptive camelCase names
const currentUser = { name: "Alice", age: 30 };
// => currentUser clearly indicates current authenticated user
// => name and age are self-documenting

function calculateTotal(subtotal: number, taxRate: number): number {
  // => calculateTotal describes action and purpose
  // => subtotal and taxRate clarify parameter meanings
  return subtotal * (1 + taxRate);
  // => Calculation self-evident from names
}
```

**Benefits**:

- Code self-documents intent
- No need for excessive comments
- IDE autocomplete works better

### Class and Interface Names

Use PascalCase for classes and interfaces.

**Pattern**:

```typescript
// ❌ BAD: Lowercase class names
class userService {
  // => Lowercase looks like variable
  // => Confusing in large codebase
}

// ✅ GOOD: PascalCase classes
class UserService {
  // => PascalCase clearly indicates class
  // => Follows TypeScript/JavaScript convention

  constructor(
    private userRepository: UserRepository,
    // => Dependency injected
    // => camelCase for instance variables
  ) {}

  async findById(id: string): Promise<User> {
    // => camelCase method names
    // => async clearly indicates async operation
    return await this.userRepository.findById(id);
  }
}
```

**Interface naming**:

```typescript
// ❌ BAD: "I" prefix (Hungarian notation)
interface IUser {
  // => "I" prefix is C# convention, not TypeScript
  // => TypeScript has structural typing, prefix unnecessary
  name: string;
}

// ✅ GOOD: Clean interface names
interface User {
  // => Clean name matches entity
  // => TypeScript infers structure automatically
  name: string;
  email: string;
}

interface UserRepository {
  // => Repository pattern interface
  // => Describes role, not implementation
  findById(id: string): Promise<User | null>;
  save(user: User): Promise<void>;
}
```

**Benefits**:

- Follows TypeScript/JavaScript conventions
- Clear distinction between types and values
- No Hungarian notation clutter

### Constants and Enums

Use UPPER_SNAKE_CASE for constants, PascalCase for enums.

**Pattern**:

```typescript
// Constants: UPPER_SNAKE_CASE
const MAX_RETRY_ATTEMPTS = 3;
// => Clearly indicates constant value
// => Convention signals immutability

const API_BASE_URL = "https://api.example.com";
// => Environment-specific constant
// => UPPER_SNAKE_CASE signals configuration

const DATABASE_CONNECTION_TIMEOUT_MS = 5000;
// => Timeout constant with unit suffix
// => _MS clearly indicates milliseconds

// Enums: PascalCase
enum UserRole {
  // => Enum name in PascalCase
  Admin = "ADMIN",
  // => Enum values can be UPPER_SNAKE_CASE
  User = "USER",
  Guest = "GUEST",
}

enum OrderStatus {
  // => Order lifecycle states
  Pending = "PENDING",
  Processing = "PROCESSING",
  Shipped = "SHIPPED",
  Delivered = "DELIVERED",
}
```

**Benefits**:

- Constants easy to identify
- Enums provide type-safe alternatives to magic strings

### File Naming

Use kebab-case for files, matching export names.

**Pattern**:

```
// ❌ BAD: Inconsistent file naming
UserService.ts           // PascalCase
user_repository.ts       // snake_case
userController.ts        // camelCase

// ✅ GOOD: kebab-case files
user-service.ts          // Exports UserService class
user-repository.ts       // Exports UserRepository class
user-controller.ts       // Exports UserController class
email-validator.ts       // Exports EmailValidator class
```

**Benefits**:

- Consistent across codebase
- Works on case-sensitive and case-insensitive filesystems
- Matches file content predictably

## Code Organization

Organize code into logical modules and layers.

### Project Structure

Use feature-based or layer-based organization.

**Feature-based structure** (recommended for large apps):

```
src/
├── users/                   # User feature module
│   ├── user.entity.ts       # User entity/model
│   ├── user.repository.ts   # Data access
│   ├── user.service.ts      # Business logic
│   ├── user.controller.ts   # HTTP handlers
│   ├── user.dto.ts          # Data transfer objects
│   └── user.validator.ts    # Validation logic
├── orders/                  # Order feature module
│   ├── order.entity.ts
│   ├── order.repository.ts
│   ├── order.service.ts
│   └── order.controller.ts
└── shared/                  # Shared utilities
    ├── database/            # Database connection
    ├── validation/          # Shared validators
    └── middleware/          # Express middleware
```

**Layer-based structure** (recommended for small apps):

```
src/
├── entities/                # Domain entities
│   ├── user.entity.ts
│   └── order.entity.ts
├── repositories/            # Data access layer
│   ├── user.repository.ts
│   └── order.repository.ts
├── services/                # Business logic layer
│   ├── user.service.ts
│   └── order.service.ts
├── controllers/             # Presentation layer
│   ├── user.controller.ts
│   └── order.controller.ts
└── utils/                   # Shared utilities
```

**Benefits**:

- Related code grouped together
- Easy to navigate and find files
- Scales with application size

### Import Organization

Group and order imports consistently.

**Pattern**:

```typescript
// 1. External dependencies (libraries)
import express from "express";
// => Express framework
import { z } from "zod";
// => Validation library

// 2. Internal modules (application code)
import { UserRepository } from "./user.repository";
// => Repository from same feature
import { EmailService } from "../shared/email.service";
// => Shared service from different feature

// 3. Types and interfaces
import type { User } from "./user.entity";
// => Type-only import (type keyword)
// => Avoids circular dependencies
import type { Request, Response } from "express";
// => Express types

// ✅ Benefits:
// - Grouped by category (external, internal, types)
// - Easy to identify dependencies
// - Consistent across files
```

**ESLint enforcement**:

```json
{
  "rules": {
    "import/order": [
      "error",
      {
        "groups": ["builtin", "external", "internal", "parent", "sibling", "index", "type"],
        "newlines-between": "always"
      }
    ]
  }
}
```

## Type Safety Best Practices

Maximize TypeScript's type system benefits.

### Avoid `any` Type

The `any` type disables type checking and should be avoided.

**Anti-pattern**:

```typescript
// ❌ BAD: any disables type checking
function processData(data: any): any {
  // => any accepts anything (no type safety)
  // => any returns anything (no intellisense)
  return data.value.toUpperCase();
  // => Runtime error if data.value is not string
  // => TypeScript cannot catch this
}

const result = processData({ value: 123 });
// => Compiles but crashes at runtime
// => result type is any (no type safety)
```

**Best practice**:

```typescript
// ✅ GOOD: Use specific types
interface DataWithValue {
  // => Define expected structure
  value: string;
  // => value must be string
}

function processData(data: DataWithValue): string {
  // => Type-safe input and output
  // => TypeScript verifies data.value is string
  return data.value.toUpperCase();
  // => Safe: TypeScript ensures value is string
}

const result = processData({ value: "hello" });
// => result type is string (type-safe)

// TypeScript catches errors at compile time:
// processData({ value: 123 }); // ❌ Error: Type 'number' is not assignable to 'string'
```

### Use `unknown` Instead of `any`

When type is truly unknown, use `unknown` (requires type checking before use).

**Pattern**:

```typescript
// ❌ BAD: any allows unsafe operations
function parseJSON(json: string): any {
  // => Returns any (no type safety)
  return JSON.parse(json);
  // => JSON.parse returns any
}

const user = parseJSON('{"name":"Alice"}');
user.name.toUpperCase();
// => Compiles but may crash if structure different
user.age.toString();
// => Compiles but crashes if age doesn't exist

// ✅ GOOD: unknown requires type checking
function parseJSON(json: string): unknown {
  // => Returns unknown (must check before use)
  return JSON.parse(json);
}

const data = parseJSON('{"name":"Alice"}');
// => data type is unknown

// Type guard required before use
if (isUser(data)) {
  // => Type guard narrows unknown to User
  console.log(data.name.toUpperCase());
  // => Safe: TypeScript knows data is User
}

function isUser(data: unknown): data is User {
  // => Type guard function
  // => data is User: Type predicate
  return (
    typeof data === "object" &&
    // => Check is object
    data !== null &&
    // => Check not null
    "name" in data &&
    // => Check has name property
    typeof (data as any).name === "string"
    // => Check name is string
  );
}
```

**Benefits**:

- Safer than `any` (requires type checking)
- Forces explicit type validation
- Catches more bugs at compile time

### Enable Strict Mode

Use `strict: true` in `tsconfig.json` for maximum type safety.

**Configuration** (`tsconfig.json`):

```json
{
  "compilerOptions": {
    "strict": true,
    "noImplicitAny": true,
    "strictNullChecks": true,
    "strictFunctionTypes": true,
    "strictPropertyInitialization": true,
    "noImplicitThis": true,
    "alwaysStrict": true
  }
}
```

**Benefits**:

- Catches null/undefined errors
- Prevents implicit `any`
- Stricter function type checking
- Requires property initialization

## Function Best Practices

Write clear, focused functions with single responsibilities.

### Function Length

Keep functions short and focused (≤20 lines recommended).

**Anti-pattern**:

```typescript
// ❌ BAD: Long function with multiple responsibilities
function processOrder(order: Order): void {
  // => Too many responsibilities in one function
  // => Hard to test, understand, and maintain

  // Validate order (responsibility 1)
  if (!order.items || order.items.length === 0) {
    throw new Error("Order must have items");
  }

  // Calculate total (responsibility 2)
  let total = 0;
  for (const item of order.items) {
    total += item.price * item.quantity;
  }

  // Apply discount (responsibility 3)
  if (order.customer.isPremium) {
    total *= 0.9;
  }

  // Calculate tax (responsibility 4)
  const tax = total * 0.1;
  total += tax;

  // Update inventory (responsibility 5)
  for (const item of order.items) {
    inventory.reduce(item.productId, item.quantity);
  }

  // Create invoice (responsibility 6)
  const invoice = new Invoice(order.id, total, tax);
  invoiceRepository.save(invoice);

  // Send email (responsibility 7)
  emailService.sendOrderConfirmation(order.customer.email);
}
```

**Best practice**:

```typescript
// ✅ GOOD: Split into focused functions
function processOrder(order: Order): void {
  // => Orchestrates order processing
  // => Single responsibility: coordination

  validateOrder(order);
  // => Validates order structure

  const total = calculateOrderTotal(order);
  // => Calculates total with discounts and tax

  updateInventory(order);
  // => Reduces inventory

  const invoice = createInvoice(order, total);
  // => Creates invoice record

  sendOrderConfirmation(order);
  // => Sends confirmation email
}

function validateOrder(order: Order): void {
  // => Single responsibility: validation
  if (!order.items || order.items.length === 0) {
    throw new Error("Order must have items");
  }
}

function calculateOrderTotal(order: Order): number {
  // => Single responsibility: calculation
  let subtotal = order.items.reduce((sum, item) => sum + item.price * item.quantity, 0);
  // => Calculate subtotal

  if (order.customer.isPremium) {
    subtotal *= 0.9;
    // => Apply 10% premium discount
  }

  const tax = subtotal * 0.1;
  // => Calculate 10% tax

  return subtotal + tax;
  // => Return total with tax
}
```

**Benefits**:

- Each function has single purpose
- Easy to test in isolation
- Easy to understand
- Easy to reuse

### Parameter Count

Limit function parameters (≤3 recommended, use objects for more).

**Anti-pattern**:

```typescript
// ❌ BAD: Too many parameters
function createUser(
  email: string,
  password: string,
  firstName: string,
  lastName: string,
  age: number,
  city: string,
  country: string,
  phoneNumber: string,
): User {
  // => 8 parameters: hard to remember order
  // => Easy to mix up arguments
  // => Difficult to add optional parameters
}

createUser("alice@example.com", "pass123", "Alice", "Smith", 30, "NYC", "USA", "555-1234");
// => Parameter order unclear
// => Easy to swap arguments
```

**Best practice**:

```typescript
// ✅ GOOD: Use parameter object
interface CreateUserParams {
  // => Parameter object interface
  // => Named properties (self-documenting)
  email: string;
  password: string;
  firstName: string;
  lastName: string;
  age?: number;
  // => Optional parameter (? suffix)
  city?: string;
  country?: string;
  phoneNumber?: string;
}

function createUser(params: CreateUserParams): User {
  // => Single parameter object
  // => Properties accessed by name
  const { email, password, firstName, lastName } = params;
  // => Destructure required properties

  // ... create user logic
}

createUser({
  // => Object with named properties
  // => Order doesn't matter
  email: "alice@example.com",
  password: "pass123",
  firstName: "Alice",
  lastName: "Smith",
  age: 30,
  // => Optional parameters included as needed
});
```

**Benefits**:

- Self-documenting parameter names
- Order doesn't matter
- Easy to add optional parameters
- Type-safe with interface

## Error Handling Best Practices

Handle errors explicitly and consistently.

### Use Custom Error Classes

Create custom error classes for domain-specific errors.

**Pattern**:

```typescript
// Base error class
class AppError extends Error {
  // => Application base error
  // => Extends built-in Error

  constructor(
    message: string,
    public readonly statusCode: number,
    // => HTTP status code
    public readonly isOperational: boolean = true,
    // => Operational error (expected) vs programmer error (bug)
  ) {
    super(message);
    // => Call Error constructor
    this.name = this.constructor.name;
    // => Set error name to class name
    Error.captureStackTrace(this, this.constructor);
    // => Capture stack trace
  }
}

class ValidationError extends AppError {
  // => Validation error (400 Bad Request)
  constructor(message: string) {
    super(message, 400);
    // => 400 status code for validation errors
  }
}

class NotFoundError extends AppError {
  // => Resource not found (404)
  constructor(resource: string, id: string) {
    super(`${resource} with id ${id} not found`, 404);
    // => Descriptive message with resource and ID
  }
}

class UnauthorizedError extends AppError {
  // => Authentication error (401)
  constructor(message: string = "Unauthorized") {
    super(message, 401);
  }
}

// Usage
function findUser(id: string): User {
  const user = database.users.find((u) => u.id === id);
  // => Query database

  if (!user) {
    throw new NotFoundError("User", id);
    // => Throw domain-specific error
    // => Caller knows exactly what happened
  }

  return user;
}
```

**Benefits**:

- Clear error types
- HTTP status codes included
- Consistent error handling

## Async/Await Best Practices

Use async/await for cleaner asynchronous code.

### Always Handle Promise Rejections

**Anti-pattern**:

```typescript
// ❌ BAD: Unhandled promise rejection
async function fetchUser(id: string): Promise<void> {
  const user = await fetch(`/api/users/${id}`);
  // => If fetch fails, promise rejection unhandled
  // => Crashes application in production
  console.log(user);
}
```

**Best practice**:

```typescript
// ✅ GOOD: Explicit error handling
async function fetchUser(id: string): Promise<void> {
  try {
    const response = await fetch(`/api/users/${id}`);
    // => Attempt fetch

    if (!response.ok) {
      // => Check HTTP status
      throw new Error(`HTTP ${response.status}`);
    }

    const user = await response.json();
    // => Parse JSON
    console.log(user);
  } catch (error) {
    // => Handle all errors
    console.error("Failed to fetch user:", error);
    // => Log error
    throw error;
    // => Re-throw for caller to handle
  }
}
```

**Benefits**:

- Explicit error handling
- Prevents unhandled rejections
- Clear error propagation

## Production Framework: ESLint + Prettier

Enforce best practices automatically with tooling.

**Installation**:

```bash
npm install --save-dev eslint prettier eslint-config-prettier @typescript-eslint/parser @typescript-eslint/eslint-plugin
```

**ESLint configuration** (`.eslintrc.json`):

```json
{
  "parser": "@typescript-eslint/parser",
  "extends": ["eslint:recommended", "plugin:@typescript-eslint/recommended", "prettier"],
  "rules": {
    "@typescript-eslint/no-explicit-any": "error",
    "@typescript-eslint/explicit-function-return-type": "warn",
    "max-lines-per-function": ["error", 50],
    "max-params": ["error", 3],
    "complexity": ["error", 10]
  }
}
```

**Prettier configuration** (`.prettierrc`):

```json
{
  "printWidth": 100,
  "tabWidth": 2,
  "semi": true,
  "singleQuote": false,
  "trailingComma": "all"
}
```

**Benefits**:

- Automatic code formatting
- Enforced code quality rules
- Consistent style across team

## Summary

TypeScript best practices ensure consistent, maintainable code. Use camelCase for variables/functions, PascalCase for classes/interfaces, kebab-case for files. Organize code by features or layers. Maximize type safety with strict mode, avoid `any`, use `unknown` when needed. Keep functions short, limit parameters, handle errors explicitly. Enforce practices with ESLint and Prettier.

**Production checklist**:

- ✅ Strict mode enabled (`tsconfig.json`)
- ✅ ESLint configured with TypeScript rules
- ✅ Prettier for automatic formatting
- ✅ No `any` types (use specific types or `unknown`)
- ✅ Custom error classes for domain errors
- ✅ Functions ≤50 lines, ≤3 parameters
- ✅ Consistent naming conventions

## Related Resources

- [Design Principles](/en/learn/software-engineering/programming-languages/typescript/in-the-field/design-principles) - SOLID, DRY, KISS, YAGNI
- [Type Safety](/en/learn/software-engineering/programming-languages/typescript/in-the-field/type-safety) - Advanced type system usage
- [Linting and Formatting](/en/learn/software-engineering/programming-languages/typescript/in-the-field/linting-and-formatting) - ESLint and Prettier setup
