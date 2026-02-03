---
title: "Design Principles"
date: 2026-02-03T00:00:00+07:00
draft: false
description: Master SOLID principles for maintainable, extensible object-oriented design in Java
weight: 10000003
tags: ["java", "solid", "design-principles", "oop", "software-design"]
---

## Understanding SOLID Principles

SOLID represents five fundamental object-oriented design principles that guide creating maintainable, extensible software. These principles prevent code rot, reduce coupling, and enable safe refactoring.

**Why SOLID matters:**

- **Maintainability**: Changes isolated to specific classes
- **Testability**: Dependencies explicit, easily mocked
- **Extensibility**: Add features without modifying existing code
- **Understandability**: Single responsibility makes code clearer

This guide explains each SOLID principle with practical examples and anti-patterns to avoid.

## S - Single Responsibility Principle (SRP)

**Principle**: A class should have one, and only one, reason to change.

**Problem**: Classes with multiple responsibilities are fragile - changes to one responsibility risk breaking others. Testing requires managing multiple concerns.

**Recognition signals:**

- Class name contains "And" or "Manager" or "Utility"
- Methods unrelated to each other
- Changes to unrelated features modify same class
- Difficult to name class clearly
- Class has many dependencies

| Characteristic    | Multiple Responsibilities | Single Responsibility  |
| ----------------- | ------------------------- | ---------------------- |
| Reasons to change | Multiple                  | One                    |
| Dependencies      | Many, unrelated           | Focused, cohesive      |
| Testing           | Complex setup             | Simple, isolated       |
| Reusability       | Low (too specific)        | High (focused purpose) |

**Example transformation:**

```java
// VIOLATES SRP: User class handles persistence, business logic, and formatting
public class User {
    private String name;
    private String email;

    // RESPONSIBILITY 1: Business logic
    public void validateEmail() {
        if (!email.contains("@")) {
            throw new IllegalArgumentException("Invalid email");
        }
    }

    // RESPONSIBILITY 2: Persistence
    public void save() {
        Connection conn = DriverManager.getConnection("jdbc:...");
        // SQL persistence code
    }

    // RESPONSIBILITY 3: Formatting
    public String toJson() {
        return "{\"name\":\"" + name + "\",\"email\":\"" + email + "\"}";
    }
}

// FOLLOWS SRP: Separate concerns
public record User(String name, String email) {
    // ONLY: Domain logic and invariants
    public User {
        if (email == null || !email.contains("@")) {
            throw new IllegalArgumentException("Invalid email");
        }
    }
}

public class UserRepository {
    // ONLY: Persistence
    public void save(User user) {
        // Database operations
    }
}

public class UserFormatter {
    // ONLY: Formatting
    public String toJson(User user) {
        return "{\"name\":\"" + user.name() + "\",\"email\":\"" + user.email() + "\"}";
    }
}
```

**Benefits:**

- Changes to persistence don't affect formatting
- Each class testable independently
- Clear, focused responsibilities

## O - Open/Closed Principle (OCP)

**Principle**: Classes should be open for extension, closed for modification.

**Problem**: Modifying existing code risks introducing bugs. Adding features shouldn't require changing working code.

**Recognition signals:**

- if-else or switch statements on type
- Modifying class to add new behavior
- Subclasses overriding multiple methods
- No way to add features without editing source

| Characteristic  | Modification Required     | Extension Supported      |
| --------------- | ------------------------- | ------------------------ |
| Adding features | Edit existing code        | Add new class            |
| Risk level      | High (can break existing) | Low (existing untouched) |
| Testing         | Retest everything         | Test new code only       |
| Deployment      | Risky changes             | Safe additions           |

**Example:**

```java
// VIOLATES OCP: Must modify calculate() for new shape types
public class AreaCalculator {
    public double calculate(Object shape) {
        if (shape instanceof Rectangle) {
            Rectangle r = (Rectangle) shape;
            return r.width() * r.height();
        } else if (shape instanceof Circle) {
            Circle c = (Circle) shape;
            return Math.PI * c.radius() * c.radius();
        }
        // PROBLEM: Adding Triangle requires modifying this method
        throw new IllegalArgumentException("Unknown shape");
    }
}

// FOLLOWS OCP: Extend via new shape implementations
public interface Shape {
    double area();
}

public record Rectangle(double width, double height) implements Shape {
    @Override
    public double area() {
        return width * height;
    }
}

public record Circle(double radius) implements Shape {
    @Override
    public double area() {
        return Math.PI * radius * radius;
    }
}

// ADDING NEW SHAPE: No modification to existing code
public record Triangle(double base, double height) implements Shape {
    @Override
    public double area() {
        return 0.5 * base * height;
    }
}

public class AreaCalculator {
    public double calculate(Shape shape) {
        return shape.area();  // POLYMORPHISM: No if-else needed
    }
}
```

**Benefits:**

- Add new shapes without modifying AreaCalculator
- Existing code remains untouched (no regression risk)
- Each shape testable independently

## L - Liskov Substitution Principle (LSP)

**Principle**: Subtypes must be substitutable for their base types without altering correctness.

**Problem**: Violating LSP breaks polymorphism. Code using base class fails with subclass instances.

**Recognition signals:**

- Subclass throws exceptions base class doesn't
- Subclass strengthens preconditions (requires more)
- Subclass weakens postconditions (guarantees less)
- Subclass changes behavior unexpectedly
- Code checks concrete type before calling method

| Characteristic   | LSP Violation        | LSP Compliant             |
| ---------------- | -------------------- | ------------------------- |
| Substitutability | Fails with subclass  | Works with all subclasses |
| Behavior         | Changes unexpectedly | Consistent with base      |
| Preconditions    | Strengthened         | Same or weakened          |
| Postconditions   | Weakened             | Same or strengthened      |

**Classic violation:**

```java
// VIOLATES LSP: Square is-a Rectangle breaks substitutability
public class Rectangle {
    protected int width;
    protected int height;

    public void setWidth(int width) { this.width = width; }
    public void setHeight(int height) { this.height = height; }
    public int getArea() { return width * height; }
}

public class Square extends Rectangle {
    @Override
    public void setWidth(int width) {
        this.width = width;
        this.height = width;  // FORCES height = width
    }

    @Override
    public void setHeight(int height) {
        this.width = height;  // FORCES width = height
        this.height = height;
    }
}

// PROBLEM: Code expecting Rectangle behavior breaks with Square
void testRectangle(Rectangle r) {
    r.setWidth(5);
    r.setHeight(4);
    assert r.getArea() == 20;  // FAILS if r is Square! (returns 16)
}
```

**LSP-compliant design:**

```java
// FOLLOWS LSP: Immutable shapes with correct hierarchy
public interface Shape {
    double area();
}

public record Rectangle(double width, double height) implements Shape {
    @Override
    public double area() {
        return width * height;
    }
}

public record Square(double side) implements Shape {
    @Override
    public double area() {
        return side * side;
    }
}

// NO SUBSTITUTION ISSUE: Square and Rectangle are siblings, not parent-child
void testShape(Shape shape) {
    double area = shape.area();  // WORKS for all Shape implementations
}
```

**Guidelines for LSP:**

- Prefer composition over inheritance
- Use immutable objects (no setters to violate)
- Ensure subclasses honor base class contracts
- Don't strengthen preconditions (require less or same input)
- Don't weaken postconditions (guarantee same or more output)

## I - Interface Segregation Principle (ISP)

**Principle**: Clients shouldn't depend on interfaces they don't use.

**Problem**: Fat interfaces force clients to depend on methods they don't need. Changes to unused methods force recompilation and retesting.

**Recognition signals:**

- Interface with many unrelated methods
- Implementations throw UnsupportedOperationException
- Clients use only subset of interface
- Method names like doEverything()
- Interface combines multiple concerns

| Characteristic        | Fat Interface                    | Segregated Interfaces                |
| --------------------- | -------------------------------- | ------------------------------------ |
| Methods per interface | Many, unrelated                  | Few, cohesive                        |
| Dependencies          | Clients depend on unused methods | Clients depend only on what they use |
| Impact of changes     | Affects all clients              | Affects only relevant clients        |
| Implementation burden | Implement all methods            | Implement only needed methods        |

**Example:**

```java
// VIOLATES ISP: Fat interface forces unnecessary dependencies
public interface Worker {
    void work();
    void eat();
    void sleep();
    void getPaid();
}

public class HumanWorker implements Worker {
    @Override
    public void work() { /* ... */ }
    @Override
    public void eat() { /* ... */ }
    @Override
    public void sleep() { /* ... */ }
    @Override
    public void getPaid() { /* ... */ }
}

public class RobotWorker implements Worker {
    @Override
    public void work() { /* ... */ }
    @Override
    public void eat() { throw new UnsupportedOperationException(); }  // PROBLEM
    @Override
    public void sleep() { throw new UnsupportedOperationException(); }  // PROBLEM
    @Override
    public void getPaid() { throw new UnsupportedOperationException(); }  // PROBLEM
}

// FOLLOWS ISP: Segregated interfaces
public interface Workable {
    void work();
}

public interface Eatable {
    void eat();
}

public interface Sleepable {
    void sleep();
}

public interface Payable {
    void getPaid();
}

public class HumanWorker implements Workable, Eatable, Sleepable, Payable {
    @Override
    public void work() { /* ... */ }
    @Override
    public void eat() { /* ... */ }
    @Override
    public void sleep() { /* ... */ }
    @Override
    public void getPaid() { /* ... */ }
}

public class RobotWorker implements Workable {
    @Override
    public void work() { /* ... */ }
    // NO NEED: to implement eat(), sleep(), getPaid()
}
```

**Benefits:**

- RobotWorker depends only on Workable
- Changes to Eatable don't affect RobotWorker
- Clear contracts (no UnsupportedOperationException)

## D - Dependency Inversion Principle (DIP)

**Principle**: High-level modules shouldn't depend on low-level modules. Both should depend on abstractions.

**Problem**: Direct dependencies on concrete implementations create tight coupling. Cannot swap implementations or test in isolation.

**Recognition signals:**

- Direct instantiation (new ConcreteClass())
- Static method calls for dependencies
- Difficult to test (can't mock dependencies)
- Cannot swap implementations
- Changes to implementation break clients

| Characteristic       | Concrete Dependency          | Abstraction Dependency       |
| -------------------- | ---------------------------- | ---------------------------- |
| Coupling             | Tight (knows concrete class) | Loose (knows only interface) |
| Testing              | Difficult (can't mock)       | Easy (inject mocks)          |
| Flexibility          | Fixed implementation         | Swappable implementations    |
| Dependency direction | High-level → Low-level       | Both → Abstraction           |

**Example:**

```java
// VIOLATES DIP: Depends on concrete EmailService
public class UserNotifier {
    private EmailService emailService = new EmailService();  // CONCRETE

    public void notifyUser(User user, String message) {
        emailService.sendEmail(user.email(), message);  // COUPLED
    }
}

// PROBLEM: Cannot switch to SMS, cannot test without sending real emails

// FOLLOWS DIP: Depends on abstraction
public interface NotificationService {
    void send(String recipient, String message);
}

public class EmailService implements NotificationService {
    @Override
    public void send(String recipient, String message) {
        // Email implementation
    }
}

public class SmsService implements NotificationService {
    @Override
    public void send(String recipient, String message) {
        // SMS implementation
    }
}

public class UserNotifier {
    private final NotificationService notificationService;  // ABSTRACTION

    // DEPENDENCY INJECTION
    public UserNotifier(NotificationService notificationService) {
        this.notificationService = notificationService;
    }

    public void notifyUser(User user, String message) {
        notificationService.send(user.email(), message);  // DECOUPLED
    }
}

// USAGE: Inject concrete implementation
NotificationService emailService = new EmailService();
UserNotifier notifier = new UserNotifier(emailService);

// TESTING: Inject mock
NotificationService mockService = mock(NotificationService.class);
UserNotifier testNotifier = new UserNotifier(mockService);
```

**Dependency Injection patterns:**

| Pattern               | Description                        | Example                          |
| --------------------- | ---------------------------------- | -------------------------------- |
| Constructor injection | Dependencies via constructor       | `new Service(dependency)`        |
| Setter injection      | Dependencies via setters           | `service.setDependency(dep)`     |
| Interface injection   | Dependencies via interface method  | `dependency.injectInto(service)` |
| Framework injection   | DI framework provides dependencies | Spring @Autowired                |

**Benefits:**

- Testable (inject mocks)
- Flexible (swap implementations)
- Decoupled (changes isolated)

## Applying SOLID Together

**Problem**: Real systems need all five principles working in harmony.

**Example: E-commerce order processing**

```java
// S - Single Responsibility
public record Order(String id, List<OrderItem> items, BigDecimal total) {}

// O - Open/Closed: Extensible payment strategies
public interface PaymentProcessor {
    void process(Order order, BigDecimal amount);
}

public class CreditCardProcessor implements PaymentProcessor { /* ... */ }
public class PayPalProcessor implements PaymentProcessor { /* ... */ }

// L - Liskov Substitution: All processors substitutable
public class OrderService {
    public void processPayment(Order order, PaymentProcessor processor) {
        processor.process(order, order.total());  // ANY processor works
    }
}

// I - Interface Segregation: Focused interfaces
public interface OrderRepository {
    void save(Order order);
    Optional<Order> findById(String id);
}

public interface OrderNotifier {
    void notifyCustomer(Order order, String message);
}

// D - Dependency Inversion: Depend on abstractions
public class OrderProcessor {
    private final OrderRepository repository;
    private final PaymentProcessor paymentProcessor;
    private final OrderNotifier notifier;

    public OrderProcessor(OrderRepository repository,
                         PaymentProcessor paymentProcessor,
                         OrderNotifier notifier) {
        this.repository = repository;
        this.paymentProcessor = paymentProcessor;
        this.notifier = notifier;
    }

    public void process(Order order) {
        paymentProcessor.process(order, order.total());  // DIP
        repository.save(order);  // DIP
        notifier.notifyCustomer(order, "Order processed");  // DIP, ISP
    }
}
```

## Guidelines

**When to apply SOLID:**

- ✓ All production code (design for maintainability)
- ✓ Code expected to change or extend
- ✓ Shared libraries and frameworks
- ✓ Domain models and business logic

**When to simplify:**

- ✗ Prototypes and throwaway code
- ✗ One-time scripts
- ✗ Code that will never change

**Best practices:**

1. **Start with SRP**: Single responsibility clarifies design
2. **Favor composition**: Enables OCP without inheritance complexity
3. **Test substitutability**: Ensure LSP with base class tests
4. **Design small interfaces**: Easier to follow ISP
5. **Inject dependencies**: DIP enables testing and flexibility

## Conclusion

SOLID principles create maintainable, extensible designs:

- **SRP**: One reason to change per class
- **OCP**: Extend behavior without modifying existing code
- **LSP**: Subtypes substitutable for base types
- **ISP**: Clients depend only on methods they use
- **DIP**: Depend on abstractions, not concrete implementations

Apply SOLID incrementally: start with SRP (clarity) and DIP (testability), then refine with OCP (extensibility), LSP (correctness), and ISP (focused contracts). SOLID isn't dogma - apply where it adds value, not just for purity.
