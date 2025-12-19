---
title: Java Glossary
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 802
description: Comprehensive glossary of Java and JVM terminology with examples
tags: ["java", "glossary", "terminology", "reference"]
---

## A

### Abstract Class

A class that cannot be instantiated and may contain abstract methods (methods without implementation) that must be implemented by subclasses.

```java
public abstract class Animal {
    private String name;

    public Animal(String name) {
        this.name = name;
    }

    // Abstract method (no implementation)
    public abstract void makeSound();

    // Concrete method
    public String getName() {
        return name;
    }
}

public class Dog extends Animal {
    public Dog(String name) {
        super(name);
    }

    @Override
    public void makeSound() {
        System.out.println("Woof!");
    }
}
```

### Annotation

Metadata that provides information about code to the compiler or runtime. Built-in annotations include `@Override`, `@Deprecated`, `@SuppressWarnings`.

```java
@Override  // Compiler checks method overrides parent method
public String toString() {
    return "CustomObject";
}

@Deprecated  // Marks method as outdated
public void oldMethod() { }

// Custom annotation
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Logged {
    String value() default "";
}

@Logged("User login")
public void login(String username) { }
```

### Autoboxing

Automatic conversion between primitive types and their corresponding wrapper classes.

```java
// Autoboxing: int -> Integer
Integer boxed = 42;

// Unboxing: Integer -> int
int unboxed = boxed;

// In collections
List<Integer> numbers = new ArrayList<>();
numbers.add(10);  // Autoboxing int to Integer
int value = numbers.get(0);  // Unboxing Integer to int
```

## B

### Bean

A Java object that follows JavaBean conventions: has a no-argument constructor, properties accessible via getters/setters, and implements `Serializable`.

```java
public class UserBean implements Serializable {
    private String name;
    private int age;

    // No-argument constructor
    public UserBean() {}

    // Getters and setters
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public int getAge() { return age; }
    public void setAge(int age) { this.age = age; }
}
```

### Bytecode

Platform-independent intermediate code that the JVM executes. Java source code (.java) is compiled to bytecode (.class files).

```java
// Source: HelloWorld.java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}

// Compiled to: HelloWorld.class (bytecode)
// View with: javap -c HelloWorld.class
```

## C

### Classloader

Component of the JVM that loads class files into memory. Three built-in classloaders: Bootstrap, Extension, and Application.

```java
// Get classloader of current class
ClassLoader classLoader = MyClass.class.getClassLoader();

// Load class dynamically
Class<?> clazz = classLoader.loadClass("com.example.MyClass");
Object instance = clazz.getDeclaredConstructor().newInstance();
```

### Checked Exception

Exception that must be caught or declared in the method signature. Extends `Exception` but not `RuntimeException`.

```java
// IOException is checked
public void readFile(String path) throws IOException {
    FileInputStream fis = new FileInputStream(path);  // May throw IOException
    // ...
}

// Must be caught or declared
public void caller() {
    try {
        readFile("file.txt");
    } catch (IOException e) {
        System.err.println("Error reading file: " + e.getMessage());
    }
}
```

### Collector

Interface that accumulates stream elements into a mutable result container.

```java
// Built-in collectors
List<String> list = stream.collect(Collectors.toList());
Set<String> set = stream.collect(Collectors.toSet());
String joined = stream.collect(Collectors.joining(", "));

// Custom collector
Collector<String, ?, Map<Integer, List<String>>> byLength =
    Collectors.groupingBy(String::length);

Map<Integer, List<String>> grouped = strings.stream()
    .collect(byLength);
```

### CompletableFuture

Class representing an asynchronous computation that may complete in the future.

```java
// Async execution
CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> {
    // Long-running task
    return fetchDataFromAPI();
});

// Chain operations
future.thenApply(data -> process(data))
      .thenAccept(result -> System.out.println(result))
      .exceptionally(ex -> {
          System.err.println("Error: " + ex.getMessage());
          return null;
      });

// Wait for completion
String result = future.join();  // Blocking
```

## D

### Daemon Thread

Background thread that doesn't prevent JVM from exiting. Used for tasks like garbage collection.

```java
Thread daemonThread = new Thread(() -> {
    while (true) {
        // Background task
        cleanupTemporaryFiles();
        try { Thread.sleep(60000); } catch (InterruptedException e) { }
    }
});
daemonThread.setDaemon(true);  // Mark as daemon
daemonThread.start();
// JVM will exit even if this thread is running
```

### Dependency Injection (DI)

Design pattern where objects receive their dependencies from external sources rather than creating them.

```java
// Without DI (tight coupling)
public class UserService {
    private UserRepository repository = new UserRepository();  // Tight coupling
}

// With DI (loose coupling)
public class UserService {
    private final UserRepository repository;

    // Constructor injection
    public UserService(UserRepository repository) {
        this.repository = repository;
    }
}

// Spring DI example
@Service
public class UserService {
    private final UserRepository repository;

    @Autowired  // Spring injects dependency
    public UserService(UserRepository repository) {
        this.repository = repository;
    }
}
```

## E

### Encapsulation

Object-oriented principle of bundling data and methods that operate on that data within a class, and restricting direct access to data.

```java
public class BankAccount {
    private double balance;  // Private field (encapsulated)

    public void deposit(double amount) {
        if (amount > 0) {
            balance += amount;
        }
    }

    public void withdraw(double amount) {
        if (amount > 0 && amount <= balance) {
            balance -= amount;
        }
    }

    public double getBalance() {
        return balance;
    }
}
```

### Enum

Special class type that represents a fixed set of constants.

```java
public enum DayOfWeek {
    MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY;
}

// Enum with fields and methods
public enum Planet {
    EARTH(5.972e24, 6.371e6),
    MARS(6.39e23, 3.389e6);

    private final double mass;
    private final double radius;

    Planet(double mass, double radius) {
        this.mass = mass;
        this.radius = radius;
    }

    public double surfaceGravity() {
        return 6.67e-11 * mass / (radius * radius);
    }
}
```

### ExecutorService

Framework for executing tasks asynchronously using thread pools.

```java
// Fixed thread pool
ExecutorService executor = Executors.newFixedThreadPool(10);

// Submit tasks
Future<String> future = executor.submit(() -> {
    return "Task result";
});

// Shutdown
executor.shutdown();
try {
    executor.awaitTermination(1, TimeUnit.MINUTES);
} catch (InterruptedException e) {
    executor.shutdownNow();
}
```

## F

### Functional Interface

Interface with exactly one abstract method. Can be used with lambda expressions and method references.

```java
@FunctionalInterface
public interface Processor<T> {
    T process(T input);  // Single abstract method

    // Default and static methods allowed
    default void log(String message) {
        System.out.println(message);
    }
}

// Usage with lambda
Processor<String> upperCase = s -> s.toUpperCase();
String result = upperCase.process("hello");  // "HELLO"
```

### Future

Represents the result of an asynchronous computation.

```java
ExecutorService executor = Executors.newSingleThreadExecutor();

Future<Integer> future = executor.submit(() -> {
    Thread.sleep(1000);
    return 42;
});

// Check if done
boolean isDone = future.isDone();

// Get result (blocks until complete)
Integer result = future.get();

// Get with timeout
Integer result = future.get(5, TimeUnit.SECONDS);

// Cancel
future.cancel(true);
```

## G

### Garbage Collection (GC)

Automatic memory management process that reclaims memory used by objects no longer referenced.

```java
// Object eligible for GC
Object obj = new Object();
obj = null;  // No references, eligible for GC

// Force GC (not recommended in production)
System.gc();

// Weak reference (allows GC)
WeakReference<Object> weakRef = new WeakReference<>(new Object());
Object obj = weakRef.get();  // May return null if GC occurred
```

### Generic

Type parameter that allows classes, interfaces, and methods to operate on various types while providing compile-time type safety.

```java
// Generic class
public class Box<T> {
    private T content;

    public void set(T content) {
        this.content = content;
    }

    public T get() {
        return content;
    }
}

// Usage
Box<String> stringBox = new Box<>();
stringBox.set("Hello");
String value = stringBox.get();

// Generic method
public <T> T findFirst(List<T> list) {
    return list.isEmpty() ? null : list.get(0);
}

// Bounded type parameter
public <T extends Comparable<T>> T max(T a, T b) {
    return a.compareTo(b) > 0 ? a : b;
}
```

## H

### Heap

Memory area where objects are allocated. Managed by garbage collector.

```java
// Objects created on heap
String str = new String("Hello");  // Heap
List<Integer> list = new ArrayList<>();  // Heap

// Primitives and references on stack
int x = 10;  // Stack (primitive)
String ref = str;  // Stack (reference), object on heap
```

### Higher-Order Function

Function that takes other functions as parameters or returns a function.

```java
// Takes function as parameter
public <T, R> List<R> map(List<T> list, Function<T, R> mapper) {
    List<R> result = new ArrayList<>();
    for (T item : list) {
        result.add(mapper.apply(item));
    }
    return result;
}

// Usage
List<String> names = Arrays.asList("alice", "bob");
List<Integer> lengths = map(names, String::length);

// Returns function
public Function<Integer, Integer> multiplyBy(int factor) {
    return x -> x * factor;
}

Function<Integer, Integer> double = multiplyBy(2);
int result = double.apply(5);  // 10
```

## I

### Immutable

Object whose state cannot be modified after creation. Thread-safe by nature.

```java
// Immutable class
public final class ImmutablePerson {
    private final String name;
    private final int age;

    public ImmutablePerson(String name, int age) {
        this.name = name;
        this.age = age;
    }

    public String getName() { return name; }
    public int getAge() { return age; }

    // Return new instance for modifications
    public ImmutablePerson withAge(int newAge) {
        return new ImmutablePerson(name, newAge);
    }
}

// String is immutable
String str = "Hello";
String upper = str.toUpperCase();  // Creates new String
// str unchanged
```

### Interface

Contract that classes can implement, defining methods without implementation (until Java 8, which added default methods).

```java
public interface Drawable {
    void draw();  // Abstract method

    // Default method (Java 8+)
    default void display() {
        System.out.println("Displaying...");
        draw();
    }

    // Static method (Java 8+)
    static void clear() {
        System.out.println("Clearing screen");
    }
}

public class Circle implements Drawable {
    @Override
    public void draw() {
        System.out.println("Drawing circle");
    }
}
```

### Intermediate Operation

Stream operation that returns a stream, allowing method chaining. Lazy evaluation.

```java
// Intermediate operations
Stream<String> stream = list.stream()
    .filter(s -> s.length() > 3)   // Intermediate
    .map(String::toUpperCase)      // Intermediate
    .distinct()                    // Intermediate
    .sorted();                     // Intermediate

// Not executed until terminal operation
stream.forEach(System.out::println);  // Terminal operation triggers execution
```

## J

### JIT Compiler

Just-In-Time compiler that compiles bytecode to native machine code at runtime for better performance.

```java
// Hotspot JVM uses JIT compilation
// Frequently executed code (hotspots) compiled to native code
public void hotMethod() {
    // Called many times -> JIT compiled
    for (int i = 0; i < 1000000; i++) {
        compute(i);
    }
}
```

### JVM (Java Virtual Machine)

Abstract computing machine that executes Java bytecode. Platform-specific implementation of JVM enables "write once, run anywhere".

```bash
# Different JVM implementations
# HotSpot (Oracle/OpenJDK)
# J9 (IBM/Eclipse)
# GraalVM
# Azul Zing

# JVM memory areas:
# - Heap (objects)
# - Stack (method calls, local variables)
# - Metaspace (class metadata)
# - Code Cache (JIT compiled code)
```

## L

### Lambda Expression

Anonymous function that can be passed as an argument or stored in a variable. Enabled by functional interfaces.

```java
// Lambda syntax
(parameters) -> expression
(parameters) -> { statements; }

// Examples
Runnable r = () -> System.out.println("Running");
Consumer<String> print = s -> System.out.println(s);
Function<Integer, Integer> square = x -> x * x;
BiFunction<Integer, Integer, Integer> add = (x, y) -> x + y;

// Multi-line lambda
Function<String, String> process = s -> {
    String trimmed = s.trim();
    String upper = trimmed.toUpperCase();
    return upper;
};
```

## M

### Method Reference

Shorthand notation for lambda expressions that only call a method.

```java
// Instance method reference
list.forEach(System.out::println);
// Equivalent: list.forEach(s -> System.out.println(s));

// Static method reference
list.stream().map(Integer::parseInt);
// Equivalent: list.stream().map(s -> Integer.parseInt(s));

// Constructor reference
Supplier<List<String>> supplier = ArrayList::new;
// Equivalent: Supplier<List<String>> supplier = () -> new ArrayList<>();

// Instance method of arbitrary object
list.stream().map(String::toUpperCase);
// Equivalent: list.stream().map(s -> s.toUpperCase());
```

## O

### Optional

Container object that may or may not contain a non-null value. Helps avoid NullPointerException.

```java
// Creating Optionals
Optional<String> empty = Optional.empty();
Optional<String> value = Optional.of("present");
Optional<String> nullable = Optional.ofNullable(maybeNull);

// Using Optionals
String result = optional
    .map(String::toUpperCase)
    .filter(s -> s.length() > 3)
    .orElse("default");

// Avoid
if (optional.isPresent()) {
    String val = optional.get();  // Anti-pattern
}

// Better
optional.ifPresent(val -> process(val));
```

### ORM (Object-Relational Mapping)

Technique for converting data between incompatible type systems (objects and relational databases). Examples: Hibernate, JPA.

```java
// JPA Entity
@Entity
@Table(name = "users")
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String name;

    @OneToMany(mappedBy = "user")
    private List<Order> orders;
}

// Repository
public interface UserRepository extends JpaRepository<User, Long> {
    List<User> findByName(String name);
}
```

## P

### Polymorphism

Ability of objects to take many forms. Method calls resolved at runtime based on actual object type.

```java
public interface Animal {
    void makeSound();
}

public class Dog implements Animal {
    @Override
    public void makeSound() {
        System.out.println("Woof!");
    }
}

public class Cat implements Animal {
    @Override
    public void makeSound() {
        System.out.println("Meow!");
    }
}

// Polymorphism in action
Animal animal = new Dog();
animal.makeSound();  // "Woof!" - runtime binding

animal = new Cat();
animal.makeSound();  // "Meow!" - runtime binding
```

## R

### Record

Special class type (Java 14+) that serves as a transparent carrier for immutable data.

```java
// Traditional class
public class Point {
    private final int x;
    private final int y;
    // Constructor, getters, equals, hashCode, toString...
}

// Record (auto-generates everything)
public record Point(int x, int y) {}

// Record with validation
public record Point(int x, int y) {
    public Point {  // Compact constructor
        if (x < 0 || y < 0) {
            throw new IllegalArgumentException("Coordinates must be positive");
        }
    }
}
```

### Reflection

Ability to inspect and modify class structure, methods, and fields at runtime.

```java
// Get class object
Class<?> clazz = MyClass.class;
Class<?> clazz = obj.getClass();
Class<?> clazz = Class.forName("com.example.MyClass");

// Inspect fields
Field[] fields = clazz.getDeclaredFields();
for (Field field : fields) {
    System.out.println(field.getName());
}

// Invoke method
Method method = clazz.getMethod("methodName", String.class);
Object result = method.invoke(instance, "argument");

// Create instance
Object instance = clazz.getDeclaredConstructor().newInstance();
```

## S

### Sealed Class

Class (Java 17+) that restricts which classes can extend or implement it.

```java
public sealed class Shape
    permits Circle, Rectangle, Triangle {}

public final class Circle extends Shape {}
public final class Rectangle extends Shape {}
public non-sealed class Triangle extends Shape {}
// No other class can extend Shape
```

### Serialization

Process of converting an object to a byte stream for storage or transmission.

```java
// Serializable class
public class User implements Serializable {
    private static final long serialVersionUID = 1L;
    private String name;
    private transient String password;  // Not serialized
}

// Serialize
try (ObjectOutputStream oos = new ObjectOutputStream(
        new FileOutputStream("user.ser"))) {
    oos.writeObject(user);
}

// Deserialize
try (ObjectInputStream ois = new ObjectInputStream(
        new FileInputStream("user.ser"))) {
    User user = (User) ois.readObject();
}
```

### Stack

Memory area where method calls and local variables are stored. LIFO structure.

```java
public void method1() {
    int x = 10;  // Stack
    method2();
}

public void method2() {
    int y = 20;  // Stack
}

// Stack frames:
// | method2 (y=20) |
// | method1 (x=10) |
// | main           |
```

### Stream

Sequence of elements supporting sequential and parallel operations. Part of Stream API (Java 8+).

```java
// Create stream
Stream<String> stream = list.stream();

// Operations
List<String> result = stream
    .filter(s -> s.startsWith("A"))    // Filter elements
    .map(String::toUpperCase)          // Transform elements
    .sorted()                          // Sort
    .collect(Collectors.toList());     // Collect to list

// Parallel stream
long count = list.parallelStream()
    .filter(s -> s.length() > 10)
    .count();
```

## T

### Terminal Operation

Stream operation that produces a result or side-effect and closes the stream.

```java
// Terminal operations
stream.forEach(System.out::println);           // void
List<String> list = stream.collect(Collectors.toList());  // collection
long count = stream.count();                   // long
Optional<String> first = stream.findFirst();   // Optional
boolean anyMatch = stream.anyMatch(s -> s.length() > 5);  // boolean
```

### Thread

Lightweight process that executes code concurrently with other threads.

```java
// Create thread
Thread thread = new Thread(() -> {
    System.out.println("Thread running");
});
thread.start();

// Thread states: NEW, RUNNABLE, BLOCKED, WAITING, TIMED_WAITING, TERMINATED

// Join (wait for completion)
thread.join();

// Sleep
Thread.sleep(1000);

// Virtual threads (Java 21+)
Thread virtual = Thread.startVirtualThread(() -> {
    // Lightweight thread
});
```

### Type Erasure

Process where generic type information is removed during compilation. Generics exist only at compile time.

```java
// Source code
List<String> strings = new ArrayList<String>();
List<Integer> integers = new ArrayList<Integer>();

// After type erasure (bytecode)
List strings = new ArrayList();
List integers = new ArrayList();

// Cannot do this (type erasure limitation)
// if (list instanceof List<String>) {}  // Compile error
// Can only check raw type
if (list instanceof List) {}  // OK
```

## U

### Unchecked Exception

Exception that doesn't need to be caught or declared. Extends `RuntimeException`.

```java
// Common unchecked exceptions
throw new NullPointerException();
throw new IllegalArgumentException("Invalid argument");
throw new IndexOutOfBoundsException();
throw new ArithmeticException("Division by zero");

// No need to declare
public void method() {
    throw new RuntimeException();  // No throws clause needed
}
```

## V

### Varargs

Variable-length argument list. Allows passing zero or more arguments of specified type.

```java
// Varargs method
public int sum(int... numbers) {
    int total = 0;
    for (int num : numbers) {
        total += num;
    }
    return total;
}

// Usage
sum();              // 0 arguments
sum(1);             // 1 argument
sum(1, 2, 3, 4);    // 4 arguments

// Internally treated as array
public int sum(int[] numbers) {  // Equivalent
```

### Virtual Thread

Lightweight thread (Java 21+) managed by JVM rather than OS. Enables millions of concurrent threads.

```java
// Traditional thread (heavy)
Thread platformThread = new Thread(() -> {
    // Limited by OS threads
});

// Virtual thread (lightweight)
Thread virtualThread = Thread.startVirtualThread(() -> {
    // Can have millions
});

// ExecutorService with virtual threads
ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();
executor.submit(() -> {
    // Each task gets own virtual thread
});
```

## Related Resources

**Learn more**:

- [Cheat Sheet](/en/learn/swe/prog-lang/java/reference/cheat-sheet) - Quick syntax reference
- [Resources](/en/learn/swe/prog-lang/java/reference/resources) - Books, docs, tools

**Tutorials**:

- [Beginner Tutorial](/en/learn/swe/prog-lang/java/tutorials/beginner) - Java fundamentals
- [Intermediate Tutorial](/en/learn/swe/prog-lang/java/tutorials/intermediate) - Advanced concepts
