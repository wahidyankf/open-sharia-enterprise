---
title: "Glossary"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000040
description: Java-specific terminology and definitions for learners and developers
tags:
  ["java", "glossary", "reference", "terminology", "jvm", "oop", "programming"]
---

**Comprehensive glossary** of Java-specific terms, concepts, and language features. Organized alphabetically for quick reference.

## A

### Abstract Class

**Definition**: Class declared with `abstract` keyword that cannot be instantiated and may contain abstract methods without implementation.

**Example**:

```java
abstract class Animal {
    abstract void makeSound();

    void sleep() {
        System.out.println("Sleeping...");
    }
}

class Dog extends Animal {
    @Override
    void makeSound() {
        System.out.println("Woof!");
    }
}
```

**See Also**: Interface, Abstract Method, Inheritance

### Abstract Method

**Definition**: Method declared without implementation in an abstract class, must be implemented by subclasses.

**Example**:

```java
abstract class Shape {
    abstract double area();  // No implementation
}

class Circle extends Shape {
    private double radius;

    @Override
    double area() {
        return Math.PI * radius * radius;
    }
}
```

**See Also**: Abstract Class, Override, Inheritance

### Annotation

**Definition**: Metadata mechanism to provide information about code to compiler, runtime, or tools. Declared with `@` symbol.

**Example**:

```java
@Override
public String toString() {
    return "Person";
}

@Deprecated(since = "2.0")
public void oldMethod() { }

@FunctionalInterface
interface Calculator {
    int calculate(int a, int b);
}
```

**See Also**: Metadata, @Override, @Deprecated, @FunctionalInterface

### Anonymous Class

**Definition**: Inner class without a name, defined and instantiated in single expression.

**Example**:

```java
Button button = new Button();
button.setOnClickListener(new ClickListener() {
    @Override
    public void onClick() {
        System.out.println("Clicked!");
    }
});
```

**See Also**: Inner Class, Lambda Expression, Interface

### Autoboxing

**Definition**: Automatic conversion between primitive types and their corresponding wrapper classes.

**Example**:

```java
// Autoboxing: int to Integer
Integer boxed = 42;

// Unboxing: Integer to int
int unboxed = boxed;

// In collections
List<Integer> list = new ArrayList<>();
list.add(10);  // Autoboxing
int value = list.get(0);  // Unboxing
```

**See Also**: Wrapper Class, Primitive Type, Generics

## B

### Bytecode

**Definition**: Platform-independent intermediate representation of compiled Java code, stored in `.class` files and executed by JVM.

**Example**:

```bash
javac HelloWorld.java  # Produces HelloWorld.class

javap -c HelloWorld
```

**See Also**: JVM, Compilation, Class File

### Boxing

**Definition**: Converting a primitive value to its corresponding wrapper object.

**Example**:

```java
int primitive = 10;
Integer wrapper = Integer.valueOf(primitive);  // Explicit boxing

// Autoboxing
Integer auto = 20;
```

**See Also**: Autoboxing, Unboxing, Wrapper Class

## C

### Checked Exception

**Definition**: Exception that must be explicitly caught or declared in method signature with `throws` clause.

**Example**:

```java
public void readFile(String path) throws IOException {
    FileReader reader = new FileReader(path);
    // IOException is checked exception
}

// Must handle or declare
public void processFile() {
    try {
        readFile("data.txt");
    } catch (IOException e) {
        e.printStackTrace();
    }
}
```

**See Also**: Exception, Unchecked Exception, Throws, Try-Catch

### Class

**Definition**: Blueprint for creating objects, defining state (fields) and behavior (methods).

**Example**:

```java
public class Person {
    // Fields (state)
    private String name;
    private int age;

    // Constructor
    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }

    // Methods (behavior)
    public void introduce() {
        System.out.println("I'm " + name);
    }
}
```

**See Also**: Object, Constructor, Method, Field

### Class Loader

**Definition**: Part of JVM responsible for loading class files into memory at runtime.

**Example**:

```java
// Get class loader
ClassLoader loader = MyClass.class.getClassLoader();
System.out.println(loader);  // sun.misc.Launcher$AppClassLoader

// Load class dynamically
Class<?> clazz = Class.forName("com.example.DynamicClass");
```

**See Also**: JVM, Reflection, Class File, Bytecode

### Collections Framework

**Definition**: Unified architecture for representing and manipulating collections of objects (List, Set, Map, Queue).

**Example**:

```java
// List - ordered collection
List<String> list = new ArrayList<>();
list.add("Java");

// Set - no duplicates
Set<Integer> set = new HashSet<>();
set.add(1);

// Map - key-value pairs
Map<String, Integer> map = new HashMap<>();
map.put("Alice", 25);

// Queue - FIFO operations
Queue<String> queue = new LinkedList<>();
queue.offer("First");
```

**See Also**: Generics, Interface, List, Set, Map

### Constructor

**Definition**: Special method called when creating an object instance, used for initialization.

**Example**:

```java
public class User {
    private String name;
    private int age;

    // Default constructor
    public User() {
        this.name = "Unknown";
        this.age = 0;
    }

    // Parameterized constructor
    public User(String name, int age) {
        this.name = name;
        this.age = age;
    }

    // Constructor overloading
    public User(String name) {
        this(name, 0);  // Call another constructor
    }
}
```

**See Also**: Object, Class, This, Constructor Chaining

## D

### Default Method

**Definition**: Method with implementation in an interface (since Java 8), providing default behavior.

**Example**:

```java
interface Vehicle {
    void start();  // Abstract method

    // Default method
    default void honk() {
        System.out.println("Beep beep!");
    }

    // Static method
    static int getWheelCount() {
        return 4;
    }
}

class Car implements Vehicle {
    @Override
    public void start() {
        System.out.println("Car started");
    }
    // honk() inherited, no need to implement
}
```

**See Also**: Interface, Abstract Method, Static Method, Functional Interface

### Dependency Injection

**Definition**: Design pattern where dependencies are provided to objects rather than created internally.

**Example**:

```java
// Without DI
class UserService {
    private Database db = new Database();  // Tight coupling
}

// With DI
class UserService {
    private Database db;

    // Constructor injection
    public UserService(Database db) {
        this.db = db;
    }
}
```

**See Also**: Constructor, Design Pattern, Spring Framework

## E

### Encapsulation

**Definition**: OOP principle of bundling data and methods together while hiding internal implementation details.

**Example**:

```java
public class BankAccount {
    private double balance;  // Hidden state

    // Controlled access through public methods
    public double getBalance() {
        return balance;
    }

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
}
```

**See Also**: Access Modifier, Private, Public, Getter, Setter

### Enum

**Definition**: Special class type representing a fixed set of constants.

**Example**:

```java
public enum DayOfWeek {
    MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY;
}

// Enum with fields and methods
public enum Status {
    SUCCESS(200), ERROR(500);

    private final int code;

    Status(int code) {
        this.code = code;
    }

    public int getCode() {
        return code;
    }
}
```

**See Also**: Constant, Class, Constructor

### Exception

**Definition**: Object representing an error or unexpected condition during program execution.

**Example**:

```java
try {
    int result = 10 / 0;  // ArithmeticException
} catch (ArithmeticException e) {
    System.err.println("Cannot divide by zero");
} finally {
    System.out.println("Cleanup code");
}

// Throwing exceptions
public void validateAge(int age) throws IllegalArgumentException {
    if (age < 0) {
        throw new IllegalArgumentException("Age cannot be negative");
    }
}
```

**See Also**: Try-Catch, Throws, Checked Exception, Unchecked Exception

## F

### Final

**Definition**: Modifier preventing modification. For variables (constant), methods (no override), or classes (no inheritance).

**Example**:

```java
// Final variable (constant)
final int MAX_SIZE = 100;

// Final method (cannot override)
class Parent {
    final void calculate() { }
}

// Final class (cannot extend)
final class ImmutableClass {
    private final String value;

    public ImmutableClass(String value) {
        this.value = value;
    }
}
```

**See Also**: Constant, Inheritance, Override, Immutability

### Functional Interface

**Definition**: Interface with exactly one abstract method, can be implemented with lambda expressions (since Java 8).

**Example**:

```java
@FunctionalInterface
interface Calculator {
    int calculate(int a, int b);
}

// Lambda implementation
Calculator add = (a, b) -> a + b;
Calculator multiply = (a, b) -> a * b;

System.out.println(add.calculate(5, 3));  // 8

// Built-in functional interfaces
Predicate<String> isEmpty = s -> s.isEmpty();
Function<String, Integer> length = s -> s.length();
Consumer<String> print = System.out::println;
```

**See Also**: Lambda Expression, Interface, Default Method, Method Reference

## G

### Garbage Collection

**Definition**: Automatic memory management where JVM reclaims memory occupied by unreachable objects.

**Example**:

```java
public void createObjects() {
    String temp = new String("Temporary");
    // temp becomes eligible for GC after method ends
}

// Suggesting GC (not guaranteed)
System.gc();

// Reference types affecting GC
SoftReference<byte[]> soft = new SoftReference<>(largeArray);
WeakReference<Object> weak = new WeakReference<>(object);
PhantomReference<Object> phantom = new PhantomReference<>(object, queue);
```

**See Also**: JVM, Memory Management, Heap, Object Lifecycle

### Generics

**Definition**: Type parameterization allowing classes and methods to work with any data type while providing compile-time type safety.

**Example**:

```java
// Generic class
class Box<T> {
    private T value;

    public void set(T value) {
        this.value = value;
    }

    public T get() {
        return value;
    }
}

Box<String> stringBox = new Box<>();
stringBox.set("Hello");

// Generic method
public <T> T getFirst(List<T> list) {
    return list.isEmpty() ? null : list.get(0);
}

// Bounded type parameter
public <T extends Number> double sum(T a, T b) {
    return a.doubleValue() + b.doubleValue();
}
```

**See Also**: Type Parameter, Type Erasure, Wildcard, Collections Framework

### Getter

**Definition**: Method that returns the value of a private field, following JavaBeans naming convention.

**Example**:

```java
public class Person {
    private String name;
    private int age;

    // Getter methods
    public String getName() {
        return name;
    }

    public int getAge() {
        return age;
    }

    // Boolean getter uses 'is' prefix
    private boolean active;

    public boolean isActive() {
        return active;
    }
}
```

**See Also**: Setter, Encapsulation, JavaBeans, Property

## H

### Heap

**Definition**: Memory area where JVM allocates objects and class instances at runtime.

**Example**:

```java
// Objects allocated on heap
String str = new String("Hello");  // Heap allocation
Person person = new Person();      // Heap allocation

// Memory visualization:
// Stack: local variables (references)
// Heap: actual objects
```

**See Also**: Stack, Garbage Collection, Memory Management, JVM

### Heap vs Stack

**Definition**: Heap stores objects (garbage collected), Stack stores local variables and method calls (LIFO, auto-managed).

**Example**:

```java
public void method() {
    int x = 10;              // Stack: primitive
    String str = "Hello";    // Stack: reference, Heap: object
    Person p = new Person(); // Stack: reference, Heap: object

    calculate(x);            // Method call on stack
}  // Stack frame removed, heap objects eligible for GC
```

**See Also**: Heap, Stack, Memory Management, Local Variable

## I

### Inheritance

**Definition**: OOP mechanism where a class acquires properties and methods from a parent class.

**Example**:

```java
// Parent class (superclass)
class Animal {
    protected String name;

    public void eat() {
        System.out.println("Eating...");
    }
}

// Child class (subclass)
class Dog extends Animal {
    public void bark() {
        System.out.println("Woof!");
    }
}

Dog dog = new Dog();
dog.eat();   // Inherited method
dog.bark();  // Own method
```

**See Also**: Extends, Super, Override, Polymorphism, Abstract Class

### Inner Class

**Definition**: Class defined within another class, with access to outer class members.

**Example**:

```java
class Outer {
    private int x = 10;

    // Non-static inner class
    class Inner {
        void display() {
            System.out.println("x = " + x);  // Access outer member
        }
    }

    // Static nested class
    static class StaticNested {
        void display() {
            System.out.println("Static nested");
        }
    }
}

Outer outer = new Outer();
Outer.Inner inner = outer.new Inner();
Outer.StaticNested nested = new Outer.StaticNested();
```

**See Also**: Nested Class, Anonymous Class, Static

### Interface

**Definition**: Contract defining a set of abstract methods that implementing classes must provide.

**Example**:

```java
interface Drawable {
    // Abstract method (implicitly public abstract)
    void draw();

    // Default method (Java 8+)
    default void describe() {
        System.out.println("Drawing...");
    }

    // Static method (Java 8+)
    static void help() {
        System.out.println("Help text");
    }

    // Constant (implicitly public static final)
    int MAX_SIZE = 100;
}

class Circle implements Drawable {
    @Override
    public void draw() {
        System.out.println("Drawing circle");
    }
}
```

**See Also**: Abstract Method, Default Method, Implements, Multiple Inheritance

## J

### JAR (Java Archive)

**Definition**: Package file format combining multiple class files, metadata, and resources into single compressed file.

**Example**:

```bash
jar cf myapp.jar com/example/*.class

jar cfe myapp.jar com.example.Main com/example/*.class

jar xf myapp.jar

java -jar myapp.jar
```

**See Also**: Class File, Bytecode, Packaging, Manifest

### Java Virtual Machine (JVM)

**Definition**: Runtime environment that executes Java bytecode, providing platform independence.

**Example**:

```java
// JVM architecture components:
// 1. Class Loader - loads .class files
// 2. Memory Areas - heap, stack, method area
// 3. Execution Engine - interprets/JIT compiles bytecode
// 4. Garbage Collector - manages memory

// JVM information
System.out.println(System.getProperty("java.version"));
System.out.println(Runtime.getRuntime().totalMemory());
```

**See Also**: Bytecode, Class Loader, Garbage Collection, Heap

### JavaBeans

**Definition**: Reusable software component following conventions (no-arg constructor, private fields, public getters/setters).

**Example**:

```java
public class Employee implements Serializable {
    private String name;
    private int age;

    // No-arg constructor
    public Employee() { }

    // Getters and Setters
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }
}
```

**See Also**: Getter, Setter, Serialization, Constructor

## L

### Lambda Expression

**Definition**: Concise way to represent anonymous function, implementing functional interface (since Java 8).

**Example**:

```java
// Syntax: (parameters) -> expression or { statements }

// No parameters
Runnable run = () -> System.out.println("Running");

// Single parameter (parentheses optional)
Consumer<String> print = s -> System.out.println(s);

// Multiple parameters
Comparator<Integer> compare = (a, b) -> a - b;

// Block body
BiFunction<Integer, Integer, Integer> add = (a, b) -> {
    int sum = a + b;
    return sum;
};

// Usage with collections
List<String> names = Arrays.asList("Alice", "Bob", "Charlie");
names.forEach(name -> System.out.println(name));
names.sort((a, b) -> a.compareTo(b));
```

**See Also**: Functional Interface, Method Reference, Stream API

## M

### Method

**Definition**: Named block of code in a class that performs an operation.

**Example**:

```java
public class Calculator {
    // Instance method
    public int add(int a, int b) {
        return a + b;
    }

    // Method with varargs
    public int sum(int... numbers) {
        int total = 0;
        for (int n : numbers) {
            total += n;
        }
        return total;
    }

    // Method overloading
    public double add(double a, double b) {
        return a + b;
    }
}
```

**See Also**: Method Overloading, Method Overriding, Return Type, Parameter

### Method Overloading

**Definition**: Defining multiple methods with same name but different parameters in the same class.

**Example**:

```java
public class Printer {
    // Overloaded methods
    public void print(String text) {
        System.out.println(text);
    }

    public void print(int number) {
        System.out.println(number);
    }

    public void print(String text, int times) {
        for (int i = 0; i < times; i++) {
            System.out.println(text);
        }
    }
}

Printer p = new Printer();
p.print("Hello");        // Calls print(String)
p.print(42);            // Calls print(int)
p.print("Hi", 3);       // Calls print(String, int)
```

**See Also**: Method, Polymorphism, Method Signature

### Method Overriding

**Definition**: Providing specific implementation for method inherited from parent class.

**Example**:

```java
class Animal {
    public void makeSound() {
        System.out.println("Some sound");
    }
}

class Dog extends Animal {
    @Override
    public void makeSound() {
        System.out.println("Woof!");
    }
}

class Cat extends Animal {
    @Override
    public void makeSound() {
        System.out.println("Meow!");
    }
}

Animal animal = new Dog();
animal.makeSound();  // "Woof!" - runtime polymorphism
```

**See Also**: Inheritance, @Override, Polymorphism, Super

### Method Reference

**Definition**: Shorthand for lambda expression referring to existing method (since Java 8).

**Example**:

```java
// Static method reference
Function<String, Integer> parseInt = Integer::parseInt;

// Instance method reference
String str = "Hello";
Supplier<Integer> length = str::length;

// Constructor reference
Supplier<List<String>> listFactory = ArrayList::new;

// Usage in streams
List<String> names = Arrays.asList("Alice", "Bob");
names.forEach(System.out::println);  // Method reference
names.stream().map(String::toUpperCase).collect(Collectors.toList());
```

**See Also**: Lambda Expression, Functional Interface, Stream API

### Module

**Definition**: Named collection of packages and resources with explicit dependencies (since Java 9).

**Example**:

```java
// module-info.java
module com.example.myapp {
    requires java.sql;
    requires java.logging;

    exports com.example.myapp.api;
    opens com.example.myapp.model to java.persistence;
}
```

**See Also**: Package, Modularity, Java Platform Module System (JPMS)

## N

### Nested Class

**Definition**: Class defined within another class (static nested class or inner class).

**Example**:

```java
class Outer {
    private static int staticVar = 10;
    private int instanceVar = 20;

    // Static nested class
    static class StaticNested {
        void display() {
            System.out.println(staticVar);  // Can access static members
            // System.out.println(instanceVar);  // Error: cannot access instance members
        }
    }

    // Inner class (non-static)
    class Inner {
        void display() {
            System.out.println(staticVar);    // Can access static members
            System.out.println(instanceVar);  // Can access instance members
        }
    }
}
```

**See Also**: Inner Class, Static, Encapsulation

### NullPointerException

**Definition**: Runtime exception thrown when attempting to use `null` as if it were an object.

**Example**:

```java
String str = null;
int length = str.length();  // NullPointerException

// Prevention strategies
// 1. Null check
if (str != null) {
    length = str.length();
}

// 2. Optional (Java 8+)
Optional<String> optional = Optional.ofNullable(str);
int length = optional.map(String::length).orElse(0);

// 3. Objects utility
int length = Objects.requireNonNull(str).length();
```

**See Also**: Exception, Null Safety, Optional

## O

### Object

**Definition**: Instance of a class, root of Java class hierarchy. All classes inherit from `java.lang.Object`.

**Example**:

```java
Object obj = new String("Hello");

// Methods inherited from Object class
obj.toString();      // String representation
obj.equals(other);   // Equality comparison
obj.hashCode();      // Hash code
obj.getClass();      // Runtime class info

// Override Object methods
class Person {
    private String name;

    @Override
    public String toString() {
        return "Person: " + name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Person person = (Person) o;
        return Objects.equals(name, person.name);
    }
}
```

**See Also**: Class, ToString, Equals, HashCode

### Optional

**Definition**: Container object that may or may not contain a non-null value (since Java 8).

**Example**:

```java
// Creating Optional
Optional<String> optional = Optional.of("Hello");
Optional<String> empty = Optional.empty();
Optional<String> nullable = Optional.ofNullable(getValue());

// Checking presence
if (optional.isPresent()) {
    System.out.println(optional.get());
}

// Functional style
optional.ifPresent(System.out::println);

// Default values
String value = optional.orElse("Default");
String value2 = optional.orElseGet(() -> "Computed Default");
String value3 = optional.orElseThrow(() -> new RuntimeException());

// Transformation
Optional<Integer> length = optional.map(String::length);
```

**See Also**: NullPointerException, Null Safety, Functional Interface

## P

### Package

**Definition**: Namespace organizing related classes and interfaces, preventing naming conflicts.

**Example**:

```java
// Package declaration (first line of .java file)
package com.example.myapp.model;

import java.util.List;
import java.util.ArrayList;
import com.example.myapp.util.*;

public class User {
    // Class implementation
}

// Fully qualified name
com.example.myapp.model.User user = new com.example.myapp.model.User();
```

**See Also**: Import, Namespace, Module, Class

### Polymorphism

**Definition**: OOP principle allowing objects to take multiple forms, achieved through inheritance and interfaces.

**Example**:

```java
// Compile-time polymorphism (method overloading)
class Calculator {
    int add(int a, int b) { return a + b; }
    double add(double a, double b) { return a + b; }
}

// Runtime polymorphism (method overriding)
class Animal {
    void makeSound() { System.out.println("Some sound"); }
}

class Dog extends Animal {
    @Override
    void makeSound() { System.out.println("Woof!"); }
}

Animal animal = new Dog();
animal.makeSound();  // "Woof!" - determined at runtime
```

**See Also**: Inheritance, Method Overloading, Method Overriding, Interface

### Primitive Type

**Definition**: Basic data types built into Java (not objects): byte, short, int, long, float, double, char, boolean.

**Example**:

```java
// Integer types
byte b = 127;           // 8-bit
short s = 32767;        // 16-bit
int i = 2147483647;     // 32-bit
long l = 9223372036854775807L;  // 64-bit

// Floating-point types
float f = 3.14f;        // 32-bit
double d = 3.14159;     // 64-bit

// Other types
char c = 'A';           // 16-bit Unicode
boolean flag = true;    // true or false

// Default values
int defaultInt = 0;
boolean defaultBoolean = false;
```

**See Also**: Wrapper Class, Autoboxing, Reference Type

## R

### Record

**Definition**: Immutable data carrier class with concise syntax (since Java 16).

**Example**:

```java
// Record declaration
record Person(String name, int age) { }

// Equivalent to:
// - private final fields
// - constructor
// - getters (name(), age())
// - equals(), hashCode(), toString()

Person p = new Person("Alice", 30);
System.out.println(p.name());  // Getter
System.out.println(p);         // toString()

// Custom methods allowed
record Point(int x, int y) {
    // Compact constructor
    public Point {
        if (x < 0 || y < 0) {
            throw new IllegalArgumentException();
        }
    }

    // Custom method
    public double distance() {
        return Math.sqrt(x * x + y * y);
    }
}
```

**See Also**: Immutability, Data Class, Constructor, Getter

### Reflection

**Definition**: Ability to inspect and modify classes, methods, and fields at runtime.

**Example**:

```java
// Get Class object
Class<?> clazz = String.class;
Class<?> clazz2 = "Hello".getClass();
Class<?> clazz3 = Class.forName("java.lang.String");

// Inspect class
Method[] methods = clazz.getMethods();
Field[] fields = clazz.getDeclaredFields();
Constructor<?>[] constructors = clazz.getConstructors();

// Create instance
Object obj = clazz.getDeclaredConstructor().newInstance();

// Invoke method
Method method = clazz.getMethod("substring", int.class);
Object result = method.invoke("Hello", 1);  // "ello"
```

**See Also**: Class Loader, Dynamic Proxy, Annotation Processing

## S

### Sealed Class

**Definition**: Class that restricts which other classes can extend or implement it (since Java 17).

**Example**:

```java
// Sealed class with permitted subclasses
sealed class Shape permits Circle, Rectangle, Triangle { }

final class Circle extends Shape { }
final class Rectangle extends Shape { }
non-sealed class Triangle extends Shape { }  // Allows further extension

// Pattern matching with sealed classes
double area(Shape shape) {
    return switch (shape) {
        case Circle c -> Math.PI * c.radius() * c.radius();
        case Rectangle r -> r.width() * r.height();
        case Triangle t -> 0.5 * t.base() * t.height();
    };
}
```

**See Also**: Inheritance, Final, Pattern Matching, Switch Expression

### Serialization

**Definition**: Process of converting object into byte stream for storage or transmission.

**Example**:

```java
import java.io.*;

class Person implements Serializable {
    private static final long serialVersionUID = 1L;
    private String name;
    private transient int age;  // Not serialized
}

// Serialize
Person person = new Person("Alice", 30);
try (ObjectOutputStream out = new ObjectOutputStream(
        new FileOutputStream("person.ser"))) {
    out.writeObject(person);
}

// Deserialize
try (ObjectInputStream in = new ObjectInputStream(
        new FileInputStream("person.ser"))) {
    Person loaded = (Person) in.readObject();
}
```

**See Also**: Serializable, Transient, Object Stream

### Setter

**Definition**: Method that sets the value of a private field, following JavaBeans naming convention.

**Example**:

```java
public class Person {
    private String name;
    private int age;

    // Setter methods
    public void setName(String name) {
        this.name = name;
    }

    public void setAge(int age) {
        if (age >= 0) {  // Validation
            this.age = age;
        }
    }
}

Person p = new Person();
p.setName("Alice");
p.setAge(25);
```

**See Also**: Getter, Encapsulation, JavaBeans, Property

### Stack

**Definition**: Memory area storing local variables and method call information in LIFO order.

**Example**:

```java
public void method1() {
    int x = 10;      // Pushed to stack
    method2(x);      // New stack frame
}                    // Stack frame popped

public void method2(int y) {
    int z = y + 5;   // Local variable on stack
}                    // Stack frame popped

// Stack overflow
public void recursive() {
    recursive();  // Eventually causes StackOverflowError
}
```

**See Also**: Heap, Memory Management, Local Variable, Method Call

### Static

**Definition**: Modifier making members belong to class rather than instances.

**Example**:

```java
class Counter {
    // Static variable (shared by all instances)
    static int count = 0;

    // Static method
    static void increment() {
        count++;
    }

    // Static block (initialization)
    static {
        System.out.println("Class loaded");
        count = 0;
    }

    // Static nested class
    static class Helper {
        void help() { }
    }
}

Counter.increment();  // Call without instance
System.out.println(Counter.count);
```

**See Also**: Class Member, Instance, Nested Class, Class Variable

### Stream API

**Definition**: Sequence of elements supporting functional-style operations (since Java 8).

**Example**:

```java
List<String> names = Arrays.asList("Alice", "Bob", "Charlie", "David");

// Filter and collect
List<String> filtered = names.stream()
    .filter(name -> name.length() > 3)
    .collect(Collectors.toList());

// Map and reduce
int totalLength = names.stream()
    .mapToInt(String::length)
    .sum();

// Parallel stream
long count = names.parallelStream()
    .filter(name -> name.startsWith("A"))
    .count();

// Complex operations
names.stream()
    .filter(name -> name.length() > 3)
    .map(String::toUpperCase)
    .sorted()
    .forEach(System.out::println);
```

**See Also**: Lambda Expression, Functional Interface, Collections Framework

### Super

**Definition**: Keyword referring to parent class, used to access parent constructors and methods.

**Example**:

```java
class Animal {
    String name;

    Animal(String name) {
        this.name = name;
    }

    void makeSound() {
        System.out.println("Some sound");
    }
}

class Dog extends Animal {
    String breed;

    // Call parent constructor
    Dog(String name, String breed) {
        super(name);  // Must be first statement
        this.breed = breed;
    }

    @Override
    void makeSound() {
        super.makeSound();  // Call parent method
        System.out.println("Woof!");
    }
}
```

**See Also**: Inheritance, This, Constructor, Method Overriding

### Synchronized

**Definition**: Modifier ensuring only one thread can execute a method or block at a time.

**Example**:

```java
class Counter {
    private int count = 0;

    // Synchronized method
    public synchronized void increment() {
        count++;
    }

    // Synchronized block
    public void incrementBlock() {
        synchronized(this) {
            count++;
        }
    }

    // Static synchronized
    public static synchronized void staticMethod() {
        // Locks on class object
    }
}

// Preventing race conditions
Counter counter = new Counter();
Thread t1 = new Thread(() -> counter.increment());
Thread t2 = new Thread(() -> counter.increment());
```

**See Also**: Thread, Concurrency, Lock, Thread Safety

## T

### This

**Definition**: Reference to current object instance.

**Example**:

```java
class Person {
    private String name;
    private int age;

    // 'this' distinguishes field from parameter
    Person(String name, int age) {
        this.name = name;
        this.age = age;
    }

    // 'this' calls another constructor
    Person(String name) {
        this(name, 0);
    }

    // 'this' returns current instance
    Person setName(String name) {
        this.name = name;
        return this;  // Method chaining
    }

    // 'this' as argument
    void process() {
        helper(this);
    }
}
```

**See Also**: Constructor, Instance, Method, Super

### Thread

**Definition**: Lightweight process allowing concurrent execution.

**Example**:

```java
// Extending Thread class
class MyThread extends Thread {
    @Override
    public void run() {
        System.out.println("Thread running");
    }
}

// Implementing Runnable
class MyRunnable implements Runnable {
    @Override
    public void run() {
        System.out.println("Runnable running");
    }
}

// Usage
Thread t1 = new MyThread();
t1.start();

Thread t2 = new Thread(new MyRunnable());
t2.start();

// Lambda
Thread t3 = new Thread(() -> System.out.println("Lambda thread"));
t3.start();
```

**See Also**: Runnable, Synchronized, Concurrency, Executor

### Throws

**Definition**: Keyword in method signature declaring checked exceptions that method may throw.

**Example**:

```java
public void readFile(String path) throws IOException {
    FileReader reader = new FileReader(path);
    // Method declares it may throw IOException
}

// Multiple exceptions
public void process() throws IOException, SQLException {
    readFile("data.txt");
    executeQuery("SELECT * FROM users");
}

// Caller must handle or declare
public void caller() {
    try {
        readFile("data.txt");
    } catch (IOException e) {
        e.printStackTrace();
    }
}
```

**See Also**: Exception, Checked Exception, Try-Catch, Throw

### Try-Catch

**Definition**: Exception handling construct for catching and handling errors.

**Example**:

```java
try {
    int result = 10 / 0;
} catch (ArithmeticException e) {
    System.err.println("Cannot divide by zero");
} finally {
    System.out.println("Always executes");
}

// Multiple catch blocks
try {
    riskyOperation();
} catch (IOException e) {
    // Handle I/O error
} catch (SQLException e) {
    // Handle database error
} catch (Exception e) {
    // Handle all other exceptions
}

// Multi-catch (Java 7+)
try {
    operation();
} catch (IOException | SQLException e) {
    // Handle both exception types
}
```

**See Also**: Exception, Finally, Throws, Try-with-resources

### Try-with-resources

**Definition**: Automatic resource management ensuring resources are closed after use (since Java 7).

**Example**:

```java
// Resources implementing AutoCloseable are closed automatically
try (FileReader reader = new FileReader("file.txt");
     BufferedReader br = new BufferedReader(reader)) {
    String line = br.readLine();
} catch (IOException e) {
    e.printStackTrace();
}
// reader and br automatically closed

// Multiple resources
try (Connection conn = DriverManager.getConnection(url);
     Statement stmt = conn.createStatement();
     ResultSet rs = stmt.executeQuery(query)) {
    // Use resources
}
// All closed automatically in reverse order
```

**See Also**: AutoCloseable, Exception, Resource Management, Try-Catch

### Type Erasure

**Definition**: Process where generic type information is removed during compilation, replaced with bounds or Object.

**Example**:

```java
// Source code
List<String> strings = new ArrayList<>();
strings.add("Hello");

// After type erasure (bytecode equivalent)
List strings = new ArrayList();
strings.add("Hello");

// Cannot do this due to type erasure
class Container<T> {
    // Error: Cannot create instance of type parameter
    // T instance = new T();

    // Error: Cannot check instance of type parameter
    // boolean check = obj instanceof T;
}

// Workaround: pass Class object
class Container<T> {
    private Class<T> type;

    Container(Class<T> type) {
        this.type = type;
    }

    T create() throws Exception {
        return type.getDeclaredConstructor().newInstance();
    }
}
```

**See Also**: Generics, Runtime, Reflection, Type Parameter

## U

### Unchecked Exception

**Definition**: Runtime exception that doesn't need to be declared or caught (extends RuntimeException).

**Example**:

```java
// Unchecked exceptions
NullPointerException
ArrayIndexOutOfBoundsException
ArithmeticException
IllegalArgumentException

// Don't need to declare or catch
public void divide(int a, int b) {  // No 'throws' needed
    int result = a / b;  // May throw ArithmeticException
}

// Can be caught optionally
try {
    String str = null;
    str.length();  // NullPointerException
} catch (NullPointerException e) {
    // Handle if desired
}
```

**See Also**: Exception, Checked Exception, RuntimeException

### Unboxing

**Definition**: Automatic conversion from wrapper class to primitive type.

**Example**:

```java
// Unboxing: Integer to int
Integer wrapper = 100;
int primitive = wrapper;  // Auto-unboxing

// In expressions
Integer a = 10;
Integer b = 20;
int sum = a + b;  // Both unboxed for addition

// Potential NullPointerException
Integer nullable = null;
int value = nullable;  // NullPointerException during unboxing
```

**See Also**: Autoboxing, Wrapper Class, Primitive Type

## V

### Varargs

**Definition**: Variable-length argument list allowing method to accept zero or more arguments of specified type.

**Example**:

```java
// Varargs syntax: type... parameterName
public int sum(int... numbers) {
    int total = 0;
    for (int n : numbers) {
        total += n;
    }
    return total;
}

// Call with any number of arguments
sum();              // 0 arguments
sum(1);            // 1 argument
sum(1, 2, 3);      // Multiple arguments
sum(new int[]{1, 2, 3, 4, 5});  // Array

// Varargs must be last parameter
public void print(String prefix, String... messages) {
    for (String msg : messages) {
        System.out.println(prefix + msg);
    }
}
```

**See Also**: Method, Parameter, Array

### Volatile

**Definition**: Modifier ensuring variable changes are visible to all threads, preventing caching.

**Example**:

```java
class SharedResource {
    private volatile boolean flag = false;

    public void writer() {
        flag = true;  // Write visible to all threads
    }

    public void reader() {
        if (flag) {  // Always reads latest value
            System.out.println("Flag is true");
        }
    }
}

// Without volatile, threads might cache flag value
// With volatile, changes are immediately visible
```

**See Also**: Thread, Synchronized, Concurrency, Atomicity

## W

### Wrapper Class

**Definition**: Object representation of primitive types (Integer, Double, Boolean, Character, etc.).

**Example**:

```java
// Wrapper classes
Integer intWrapper = Integer.valueOf(42);
Double doubleWrapper = Double.valueOf(3.14);
Boolean boolWrapper = Boolean.valueOf(true);
Character charWrapper = Character.valueOf('A');

// Utility methods
int parsed = Integer.parseInt("123");
String binary = Integer.toBinaryString(10);  // "1010"
boolean isDigit = Character.isDigit('5');

// Constants
int maxInt = Integer.MAX_VALUE;
int minInt = Integer.MIN_VALUE;

// Collections require wrapper classes
List<Integer> numbers = new ArrayList<>();
numbers.add(42);  // Autoboxing
```

**See Also**: Primitive Type, Autoboxing, Unboxing, Generics

## Learn More

**Comprehensive Documentation**:

- [Beginner Tutorial](/en/learn/software-engineering/programming-language/java/tutorials/beginner) - Detailed explanations of concepts
- [Quick Start](/en/learn/software-engineering/programming-language/java/tutorials/quick-start) - Overview of key features
- [How-To Guides](/en/learn/software-engineering/programming-language/java/how-to) - Practical usage examples
- [Cheat Sheet](/en/learn/software-engineering/programming-language/java/reference/cheat-sheet) - Quick syntax reference

**Official Resources**:

- [Java Language Specification](https://docs.oracle.com/javase/specs/) - Official language specification
- [Java API Documentation](https://docs.oracle.com/en/java/javase/21/docs/api/) - Complete API reference
- [Java Tutorials](https://docs.oracle.com/javase/tutorial/) - Official Oracle tutorials
