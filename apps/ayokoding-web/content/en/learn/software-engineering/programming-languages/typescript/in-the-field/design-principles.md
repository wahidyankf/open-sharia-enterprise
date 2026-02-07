---
title: "Design Principles"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "SOLID, DRY, KISS, and YAGNI design principles applied to TypeScript for maintainable, testable production code"
weight: 10000010
tags: ["typescript", "design-principles", "solid", "dry", "kiss", "yagni", "clean-code"]
---

## Why Design Principles Matter

Design principles guide architectural decisions to create maintainable, testable, and flexible code. Following SOLID, DRY, KISS, and YAGNI principles prevents technical debt, reduces bug density, and makes codebases easier to modify as requirements evolve.

**Core Benefits**:

- **Maintainability**: Code organized by single responsibilities is easier to understand and modify
- **Testability**: Loosely coupled components can be tested in isolation
- **Flexibility**: Well-designed systems adapt to changing requirements
- **Team productivity**: Consistent principles reduce cognitive load
- **Reduced bugs**: Clear boundaries prevent unintended side effects

**Problem**: Without design principles, codebases become tangled, fragile, and resistant to change.

**Solution**: Apply SOLID, DRY, KISS, and YAGNI principles from the start to build systems that scale with complexity.

## Single Responsibility Principle (SRP)

Each class or module should have one reason to change - one responsibility.

### Violation: Multiple Responsibilities

TypeScript classes often accumulate responsibilities as features are added.

**Anti-pattern**:

```typescript
class UserService {
  // => UserService handles TOO MANY responsibilities
  // => Violates Single Responsibility Principle

  createUser(email: string, password: string): User {
    // => Responsibility 1: User creation logic
    const user = new User(email, password);
    // => Create user object

    this.validateEmail(email);
    // => Responsibility 2: Validation logic
    // => Should be separate validator

    const hashedPassword = this.hashPassword(password);
    // => Responsibility 3: Cryptography
    // => Should be separate hasher

    this.saveToDatabase(user);
    // => Responsibility 4: Database persistence
    // => Should be separate repository

    this.sendWelcomeEmail(user);
    // => Responsibility 5: Email sending
    // => Should be separate email service

    return user;
    // => Returns created user
  }

  private validateEmail(email: string): void {
    // => Email validation logic
    // => Mixed with user creation
  }

  private hashPassword(password: string): string {
    // => Password hashing logic
    // => Mixed with user creation
  }

  private saveToDatabase(user: User): void {
    // => Database logic
    // => Mixed with user creation
  }

  private sendWelcomeEmail(user: User): void {
    // => Email logic
    // => Mixed with user creation
  }
}
```

**Problems**:

- Changes to email validation affect user creation
- Cannot test user creation without database
- Cannot reuse validation in other contexts
- Every change risks breaking multiple features

### Applying SRP: Separate Responsibilities

Split into single-purpose classes.

**Pattern**:

```typescript
class EmailValidator {
  // => Single responsibility: Email validation
  // => Can be reused across application

  validate(email: string): void {
    // => Validates email format
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    // => Basic email regex
    // => Production: Use library like validator.js

    if (!emailRegex.test(email)) {
      // => Check format
      throw new Error("Invalid email format");
      // => Throw validation error
    }
  }
}

class PasswordHasher {
  // => Single responsibility: Password hashing
  // => Encapsulates cryptography

  async hash(password: string): Promise<string> {
    // => Hash password for storage
    // => Returns bcrypt hash
    return await bcrypt.hash(password, 10);
    // => 10 salt rounds
    // => Production-grade hashing
  }
}

class UserRepository {
  // => Single responsibility: User persistence
  // => Database operations only

  async save(user: User): Promise<void> {
    // => Save user to database
    await database.users.insert({
      // => Database insert operation
      email: user.email,
      passwordHash: user.passwordHash,
    });
  }
}

class EmailService {
  // => Single responsibility: Email sending
  // => External communication

  async sendWelcomeEmail(user: User): Promise<void> {
    // => Send welcome email to new user
    await mailTransport.send({
      // => Email sending operation
      to: user.email,
      subject: "Welcome!",
      body: "Thanks for signing up",
    });
  }
}

class UserService {
  // => Orchestrates user creation
  // => Coordinates other services
  // => Single responsibility: User creation workflow

  constructor(
    private validator: EmailValidator,
    // => Dependency: Email validation
    private hasher: PasswordHasher,
    // => Dependency: Password hashing
    private repository: UserRepository,
    // => Dependency: User persistence
    private emailService: EmailService,
    // => Dependency: Email sending
  ) {}

  async createUser(email: string, password: string): Promise<User> {
    // => Orchestrate user creation
    // => Delegates to specialized services

    this.validator.validate(email);
    // => Validate email (delegated)
    // => Single responsibility maintained

    const hashedPassword = await this.hasher.hash(password);
    // => Hash password (delegated)

    const user = new User(email, hashedPassword);
    // => Create user object
    // => Core responsibility

    await this.repository.save(user);
    // => Persist user (delegated)

    await this.emailService.sendWelcomeEmail(user);
    // => Send email (delegated)

    return user;
    // => Return created user
  }
}
```

**Benefits**:

- Each class has one reason to change
- Can test validation without database
- Can reuse hasher in password reset
- Changes isolated to single class

## Open/Closed Principle (OCP)

Classes should be open for extension but closed for modification.

### Violation: Modifying Existing Code

Adding new features by modifying existing code breaks OCP.

**Anti-pattern**:

```typescript
class PaymentProcessor {
  // => Processes payments
  // => Violates OCP: Must modify for new payment methods

  processPayment(amount: number, method: string): void {
    // => Payment processing logic
    // => method parameter as string (weak typing)

    if (method === "credit-card") {
      // => Credit card processing
      console.log(`Processing $${amount} via credit card`);
      // => Implementation hardcoded
    } else if (method === "paypal") {
      // => PayPal processing
      // => Added later, modified class
      console.log(`Processing $${amount} via PayPal`);
    } else if (method === "bitcoin") {
      // => Bitcoin processing
      // => Added later, modified class AGAIN
      console.log(`Processing $${amount} via Bitcoin`);
    }
    // => Every new payment method requires modification
    // => Violates Open/Closed Principle
  }
}
```

**Problems**:

- Every new payment method modifies existing code
- Risks breaking existing payment methods
- Cannot add payment methods without source code access

### Applying OCP: Extension via Polymorphism

Use interfaces and polymorphism to extend behavior.

**Pattern**:

```typescript
interface PaymentMethod {
  // => Payment method interface
  // => Defines contract for all payment methods
  process(amount: number): void;
  // => All payment methods must implement process()
}

class CreditCardPayment implements PaymentMethod {
  // => Credit card implementation
  // => Implements PaymentMethod interface

  process(amount: number): void {
    // => Process credit card payment
    console.log(`Processing $${amount} via credit card`);
    // => Credit card specific logic
    // => Stripe API call in production
  }
}

class PayPalPayment implements PaymentMethod {
  // => PayPal implementation
  // => NEW class, no modification to existing code

  process(amount: number): void {
    // => Process PayPal payment
    console.log(`Processing $${amount} via PayPal`);
    // => PayPal specific logic
    // => PayPal SDK call in production
  }
}

class BitcoinPayment implements PaymentMethod {
  // => Bitcoin implementation
  // => NEW class, no modification to existing code

  process(amount: number): void {
    // => Process Bitcoin payment
    console.log(`Processing $${amount} via Bitcoin`);
    // => Bitcoin specific logic
    // => Blockchain API call in production
  }
}

class PaymentProcessor {
  // => Orchestrates payment processing
  // => CLOSED for modification, OPEN for extension

  processPayment(amount: number, method: PaymentMethod): void {
    // => Accept any PaymentMethod implementation
    // => No modification needed for new payment methods
    method.process(amount);
    // => Delegate to payment method
    // => Polymorphic dispatch
  }
}

// Usage
const processor = new PaymentProcessor();
// => Create payment processor once

processor.processPayment(100, new CreditCardPayment());
// => Process via credit card
// => Pass concrete implementation

processor.processPayment(200, new PayPalPayment());
// => Process via PayPal
// => Same processor, different implementation

processor.processPayment(300, new BitcoinPayment());
// => Process via Bitcoin
// => Added without modifying PaymentProcessor
```

**Benefits**:

- Add payment methods without modifying existing code
- Each payment method independently testable
- No risk of breaking existing methods
- Can add methods via plugins (no source code access needed)

## DRY (Don't Repeat Yourself)

Avoid code duplication by extracting reusable abstractions.

### Violation: Code Duplication

Duplicated code leads to inconsistencies and maintenance burden.

**Anti-pattern**:

```typescript
class UserController {
  // => User management endpoints
  // => Contains duplicated validation logic

  async createUser(req: Request, res: Response): Promise<void> {
    // => Create user endpoint
    // => POST /users

    if (!req.body.email) {
      // => Validate email presence
      res.status(400).json({ error: "Email required" });
      return;
    }

    if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(req.body.email)) {
      // => Validate email format
      // => Regex duplicated in multiple places
      res.status(400).json({ error: "Invalid email" });
      return;
    }

    // ... create user logic
  }

  async updateUser(req: Request, res: Response): Promise<void> {
    // => Update user endpoint
    // => PUT /users/:id

    if (!req.body.email) {
      // => DUPLICATE: Same validation as createUser
      res.status(400).json({ error: "Email required" });
      return;
    }

    if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(req.body.email)) {
      // => DUPLICATE: Same regex as createUser
      // => Maintenance nightmare (update in multiple places)
      res.status(400).json({ error: "Invalid email" });
      return;
    }

    // ... update user logic
  }
}
```

**Problems**:

- Bug fix in one place doesn't fix others
- Changes require finding all duplicates
- Inconsistent error messages

### Applying DRY: Extract Reusable Functions

Extract duplicated logic into reusable functions.

**Pattern**:

```typescript
class EmailValidator {
  // => Reusable email validation
  // => Single source of truth

  private static readonly EMAIL_REGEX = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  // => Email validation regex
  // => Defined once, used everywhere

  static validate(email: string | undefined): void {
    // => Validate email presence and format
    // => Throws on validation failure

    if (!email) {
      // => Check presence
      throw new ValidationError("Email required");
      // => Consistent error handling
    }

    if (!this.EMAIL_REGEX.test(email)) {
      // => Check format
      // => Regex used once, not duplicated
      throw new ValidationError("Invalid email format");
      // => Consistent error message
    }
  }
}

class UserController {
  // => User management endpoints
  // => No duplicated validation logic

  async createUser(req: Request, res: Response): Promise<void> {
    // => Create user endpoint
    try {
      EmailValidator.validate(req.body.email);
      // => Reuse validation logic
      // => Single line, no duplication

      // ... create user logic
    } catch (error) {
      // => Handle validation errors
      res.status(400).json({ error: error.message });
    }
  }

  async updateUser(req: Request, res: Response): Promise<void> {
    // => Update user endpoint
    try {
      EmailValidator.validate(req.body.email);
      // => SAME validation logic
      // => Changes to validation automatically apply here

      // ... update user logic
    } catch (error) {
      res.status(400).json({ error: error.message });
    }
  }
}
```

**Benefits**:

- Single source of truth for validation
- Bug fixes apply everywhere automatically
- Consistent error messages
- Easier to test validation logic

## KISS (Keep It Simple, Stupid)

Prefer simple solutions over complex ones.

### Violation: Premature Optimization

Over-engineering solutions before they're needed.

**Anti-pattern**:

```typescript
// Premature abstraction factory
interface DataFetcher {
  // => Abstract data fetcher
  // => Complex for simple use case
  fetch<T>(config: FetchConfig): Promise<T>;
}

interface FetchConfig {
  // => Configuration object
  // => Over-engineered for simple fetch
  url: string;
  method: "GET" | "POST" | "PUT" | "DELETE";
  headers?: Record<string, string>;
  body?: any;
  retries?: number;
  timeout?: number;
  cache?: CacheStrategy;
}

type CacheStrategy = "no-cache" | "memory" | "disk" | "hybrid";
// => Complex caching strategies
// => Not needed yet

class DataFetcherFactory {
  // => Factory pattern
  // => Overkill for simple use case

  static create(strategy: CacheStrategy): DataFetcher {
    // => Creates fetcher based on strategy
    // => Complex when simple fetch() would work
    switch (strategy) {
      case "memory":
        return new MemoryCachedFetcher();
      case "disk":
        return new DiskCachedFetcher();
      case "hybrid":
        return new HybridCachedFetcher();
      default:
        return new SimpleFetcher();
    }
  }
}

// Usage: Complex for simple GET request
const fetcher = DataFetcherFactory.create("hybrid");
const data = await fetcher.fetch<User>({
  url: "/api/users/1",
  method: "GET",
  retries: 3,
  timeout: 5000,
  cache: "hybrid",
});
```

**Problems**:

- Complex abstractions for simple use case
- Hard to understand and maintain
- Premature optimization

### Applying KISS: Simple Solution

Start simple, add complexity only when needed.

**Pattern**:

```typescript
async function getUser(id: string): Promise<User> {
  // => Simple function for simple use case
  // => No abstractions until needed

  const response = await fetch(`/api/users/${id}`);
  // => Standard fetch API
  // => Built-in, no custom abstraction

  if (!response.ok) {
    // => Basic error handling
    throw new Error(`HTTP ${response.status}`);
  }

  return await response.json();
  // => Parse JSON response
  // => Simple, clear, works
}

// Usage: Simple and clear
const user = await getUser("123");
// => One line, obvious behavior
// => No factory, no config object, no cache strategy
```

**When to add complexity**:

- **Need caching?** Add it when performance problem identified
- **Need retries?** Add it when reliability problem identified
- **Need timeout?** Add it when timeout problem identified

**Benefits**:

- Easy to understand
- Easy to test
- Easy to modify
- Add complexity incrementally based on actual needs

## YAGNI (You Aren't Gonna Need It)

Don't implement features until they're actually needed.

### Violation: Building for Future

Adding features "just in case" they're needed later.

**Anti-pattern**:

```typescript
class User {
  // => User entity
  // => Contains many unused features

  id: string;
  email: string;
  passwordHash: string;
  // => Core fields (actually used)

  phoneNumber?: string;
  // => "We might need phone numbers later"
  // => Not in requirements, not implemented anywhere

  address?: Address;
  // => "Users might want to save addresses"
  // => Not in requirements

  preferences?: UserPreferences;
  // => "Users might want to customize settings"
  // => Not in requirements

  socialProfiles?: SocialProfile[];
  // => "We might add social login"
  // => Not in requirements

  paymentMethods?: PaymentMethod[];
  // => "We might add payments"
  // => Not in requirements
}

interface Address {
  // => Address structure
  // => Defined but never used
  street: string;
  city: string;
  state: string;
  zip: string;
  country: string;
}

interface UserPreferences {
  // => User preferences
  // => Defined but never used
  theme: "light" | "dark";
  language: string;
  notifications: boolean;
}
// => All these interfaces add complexity
// => None provide value yet
```

**Problems**:

- Code to maintain that provides no value
- Database schema includes unused columns
- Confuses developers ("Should I populate phoneNumber?")
- Might not match actual requirements when needed

### Applying YAGNI: Build What's Needed

Implement only what's required now.

**Pattern**:

```typescript
class User {
  // => User entity
  // => Contains ONLY what's needed NOW

  constructor(
    public readonly id: string,
    // => User ID (required)
    // => Readonly: Never changes after creation
    public readonly email: string,
    // => Email (required)
    // => Used for login
    public passwordHash: string,
    // => Password hash (required)
    // => Can change (password reset)
  ) {}
}

// When phone numbers ARE needed (later):
// 1. Add phoneNumber field
// 2. Add database migration
// 3. Add validation logic
// 4. Add to registration form
// => Do ALL of these together when feature needed
// => Don't do ANY of them "just in case"
```

**Benefits**:

- Less code to maintain
- Clearer what's actually used
- Faster development (no speculative features)
- Actual requirements might differ from speculation

## Liskov Substitution Principle (LSP)

Subtypes must be substitutable for their base types without altering correctness.

### Violation: Subtype Breaks Contract

**Anti-pattern**:

```typescript
class Rectangle {
  // => Base class: Rectangle
  // => Width and height independently settable

  constructor(
    protected width: number,
    // => Width dimension
    protected height: number,
    // => Height dimension
  ) {}

  setWidth(width: number): void {
    // => Set width
    this.width = width;
    // => Only affects width
  }

  setHeight(height: number): void {
    // => Set height
    this.height = height;
    // => Only affects height
  }

  getArea(): number {
    // => Calculate area
    return this.width * this.height;
    // => width * height
  }
}

class Square extends Rectangle {
  // => Square inherits from Rectangle
  // => VIOLATES LSP: Square has different constraints

  setWidth(width: number): void {
    // => Set width
    // => ALSO sets height (different behavior)
    this.width = width;
    this.height = width;
    // => Square constraint: width === height
    // => Violates Rectangle contract
  }

  setHeight(height: number): void {
    // => Set height
    // => ALSO sets width (different behavior)
    this.width = height;
    this.height = height;
    // => Square constraint: width === height
    // => Violates Rectangle contract
  }
}

function processRectangle(rect: Rectangle): void {
  // => Function expects Rectangle behavior
  rect.setWidth(5);
  // => Set width to 5
  rect.setHeight(4);
  // => Set height to 4
  console.log(rect.getArea());
  // => Expects: 20 (5 * 4)
}

const square = new Square(10, 10);
processRectangle(square);
// => Passes Square (substitution)
// => Output: 16 (4 * 4) NOT 20!
// => VIOLATES LSP: Subtype changes behavior
```

**Problems**:

- Square cannot substitute Rectangle
- Unexpected behavior when using Square as Rectangle
- Breaks polymorphism

### Applying LSP: Correct Hierarchy

Design hierarchies that preserve contracts.

**Pattern**:

```typescript
interface Shape {
  // => Shape interface
  // => Common contract for all shapes
  getArea(): number;
  // => All shapes calculate area
}

class Rectangle implements Shape {
  // => Rectangle implementation
  // => Independent from Square

  constructor(
    private width: number,
    private height: number,
  ) {}

  setWidth(width: number): void {
    // => Set width only
    this.width = width;
  }

  setHeight(height: number): void {
    // => Set height only
    this.height = height;
  }

  getArea(): number {
    return this.width * this.height;
  }
}

class Square implements Shape {
  // => Square implementation
  // => Independent from Rectangle
  // => NO inheritance relationship

  constructor(private side: number) {
    // => Single side dimension
    // => Square constraint enforced in constructor
  }

  setSide(side: number): void {
    // => Set side length
    // => Different API than Rectangle (correct!)
    this.side = side;
  }

  getArea(): number {
    return this.side * this.side;
  }
}

function processShape(shape: Shape): void {
  // => Function works with Shape interface
  // => Only uses getArea() (common contract)
  console.log(`Area: ${shape.getArea()}`);
  // => Works correctly for both Rectangle and Square
}

const rect = new Rectangle(5, 4);
processShape(rect);
// => Works: 20

const square = new Square(4);
processShape(square);
// => Works: 16
// => Correct substitution: Shape contract preserved
```

**Benefits**:

- Both Rectangle and Square correctly implement Shape
- No surprising behavior
- Polymorphism works correctly

## Production Framework: ESLint with Design Principle Rules

Use ESLint plugins to enforce design principles automatically.

**Installation**:

```bash
npm install --save-dev eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin eslint-plugin-sonarjs
# => ESLint with TypeScript support
# => SonarJS plugin detects code smells
```

**Configuration** (`.eslintrc.json`):

```json
{
  "parser": "@typescript-eslint/parser",
  "extends": ["eslint:recommended", "plugin:@typescript-eslint/recommended", "plugin:sonarjs/recommended"],
  "plugins": ["@typescript-eslint", "sonarjs"],
  "rules": {
    "max-lines-per-function": ["error", 50],
    "max-depth": ["error", 3],
    "complexity": ["error", 10],
    "sonarjs/cognitive-complexity": ["error", 15],
    "sonarjs/no-duplicate-string": "error",
    "sonarjs/no-identical-functions": "error"
  }
}
```

**Benefits**:

- Automatically enforces KISS (complexity limits)
- Detects DRY violations (duplicate code)
- Catches code smells early

## Trade-offs and When to Use

### When to Apply Design Principles

**Use when**:

- Building production applications (long lifetime)
- Working in teams (shared understanding)
- Code will be modified frequently (flexibility needed)
- Testing is priority (loosely coupled components)

**Relax when**:

- Prototyping (speed over correctness)
- One-off scripts (won't be maintained)
- Performance critical (optimization may violate principles)

**Decision matrix**:

| Principle | Apply When                               | Skip When                          |
| --------- | ---------------------------------------- | ---------------------------------- |
| **SRP**   | Production code, testable components     | Simple scripts, prototypes         |
| **OCP**   | Extensible systems, plugin architectures | Fixed requirements, one-time tools |
| **DRY**   | Shared logic, validation rules           | Configuration, test data           |
| **KISS**  | All code (default)                       | Proven complex requirement         |
| **YAGNI** | All code (default)                       | Known future requirement           |

## Summary

Design principles guide architectural decisions for maintainable code. SOLID principles (SRP, OCP, LSP, ISP, DIP) ensure loose coupling and high cohesion. DRY eliminates duplication. KISS prevents over-engineering. YAGNI defers speculative features.

**Key takeaways**:

- **SRP**: One responsibility per class
- **OCP**: Extend via polymorphism, not modification
- **DRY**: Extract reusable abstractions
- **KISS**: Simple solutions first, add complexity when needed
- **YAGNI**: Build what's needed now, not what might be needed

Use ESLint plugins to enforce principles automatically in production code.

## Related Resources

- [Functional Programming](/en/learn/software-engineering/programming-languages/typescript/in-the-field/functional-programming) - FP patterns and immutability
- [Best Practices](/en/learn/software-engineering/programming-languages/typescript/in-the-field/best-practices) - TypeScript coding standards
- [Anti-patterns](/en/learn/software-engineering/programming-languages/typescript/in-the-field/anti-patterns) - Common design mistakes
