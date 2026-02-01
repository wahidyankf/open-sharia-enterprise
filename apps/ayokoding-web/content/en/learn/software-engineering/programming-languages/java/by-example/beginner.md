---
title: "Beginner"
date: 2026-01-02T04:41:07+07:00
draft: false
weight: 10000001
description: "Learn Java basics through 30 annotated examples: variables, I/O, OOP fundamentals, collections, control flow, streams, and more - perfect first examples for Java"
tags: ["java", "tutorial", "by-example", "beginner", "basics", "oop", "collections", "io", "strings"]
---

Learn Java fundamentals through 30 annotated code examples. Each example is self-contained, runnable in JShell or as standalone classes, and heavily commented to show what each line does, expected outputs, and intermediate values.

## Example 1: Hello World and JVM Compilation

Java programs run on the JVM (Java Virtual Machine). Code is compiled to bytecode (`.class` files) that the JVM executes. This example shows the simplest Java program and how the compilation pipeline works.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Source["Java Source Code<br/>(.java files)"] --> Compiler["Java Compiler<br/>(javac)"]
    Compiler --> Bytecode["Bytecode<br/>(.class files)"]
    Bytecode --> JVM["Java Virtual Machine<br/>(JVM)"]
    JVM --> Output["Program Output"]

    style Source fill:#0173B2,color:#fff
    style Compiler fill:#DE8F05,color:#fff
    style Bytecode fill:#029E73,color:#fff
    style JVM fill:#CC78BC,color:#fff
    style Output fill:#CA9161,color:#fff
```

**Code**:

```java
// File: HelloWorld.java (filename must match public class name)
public class HelloWorld {                          // => Public class, one per .java file
    public static void main(String[] args) {       // => Entry point: JVM calls this first
                                                   // => args holds command-line arguments
        System.out.println("Hello, World!");       // => Output: Hello, World!
    }
}

// Compilation: javac HelloWorld.java → HelloWorld.class (bytecode)
// => javac converts source to platform-independent bytecode
// Execution: java HelloWorld → JVM loads .class and runs main()
// => JVM interprets or JIT-compiles bytecode to native code
// => Same .class runs on Windows/Linux/macOS (write once, run anywhere)
```

**Key Takeaway**: Java code is organized into classes with filenames matching public class names. The `public static void main(String[] args)` method is the fixed entry point recognized by the JVM. Code compiles to platform-independent bytecode (`.class` files) executed by the platform-specific JVM, enabling "write once, run anywhere" portability.

**Why It Matters**: The JVM architecture revolutionized software deployment by decoupling code from operating systems. Before Java, C/C++ programs required separate compilation for Windows, Linux, and macOS—often with platform-specific code branches. Java's bytecode layer lets enterprises ship one `.jar` file that runs identically across all platforms, eliminating costly multi-platform testing. This design enabled Java to dominate enterprise servers (Spring Boot, Tomcat), Android mobile apps (Dalvik/ART VMs execute Java bytecode), and big data systems (Hadoop, Spark, Kafka)—all sharing the same codebase across heterogeneous infrastructure.

---

## Example 2: Variables and Type System

Java is statically typed with two categories: primitive types (stored on stack) and reference types (stored on heap). Types can be declared explicitly or inferred with `var`.

**Code**:

```java
// 8 PRIMITIVE TYPES
byte b = 127;                    // => b = 127 (8-bit signed integer, -128 to 127)
short s = 32000;                 // => s = 32000 (16-bit signed integer)
int i = 42;                      // => i = 42 (32-bit signed integer, default)
                                 // => Default integer type
long l = 1000000L;               // => l = 1000000 (64-bit signed integer, L suffix required)
float f = 3.14f;                 // => f = 3.14 (32-bit floating point, f suffix required)
double d = 3.14159;              // => d = 3.14159 (64-bit floating point, default)
                                 // => Default decimal type
boolean bool = true;             // => bool = true (true or false, not 1/0)
char c = 'A';                    // => c = 'A' (16-bit Unicode character)

// REFERENCE TYPES
String str = "Hello";            // => str = "Hello" (reference type, stored on heap)
                                 // => Can be null
int[] array = {1, 2, 3};         // => array = [1, 2, 3] (reference to array object)
                                 // => Array is reference type

// TYPE INFERENCE
var num = 100;                   // => num = 100 (type: int, inferred from literal)
                                 // => int (inferred)
var text = "World";              // => text = "World" (type: String, inferred from literal)
                                 // => String (inferred)

System.out.println(i);           // => Output: 42
System.out.println(str);         // => Output: Hello
```

**Key Takeaway**: Java has 8 primitive types (stored on stack, cannot be null) and reference types (stored on heap, can be null). Use `var` for type inference in local variables while maintaining static type safety—the compiler infers types at compile time, not runtime.

**Why It Matters**: Java's explicit type system catches errors at compile time rather than runtime, preventing entire categories of bugs that plague dynamically typed languages like Python/JavaScript. The primitive/reference distinction optimizes memory layout—primitives avoid heap allocation overhead for simple values (ints, booleans), while reference types enable object sharing and polymorphism essential for OOP. This design lets the JVM optimize hot paths with primitive operations while maintaining object-oriented flexibility for complex data structures. Modern Java's `var` keyword (introduced in Java 10) reduces boilerplate without sacrificing type safety—unlike JavaScript/Python's dynamic typing, Java's `var` is resolved entirely at compile time, preserving performance and IDE support.

---

## Example 3: Basic Input/Output with Scanner

Java's `Scanner` class reads formatted input from various sources (console, files, strings). It's the standard way to handle user input in console applications and parse structured text data.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Input["User Input<br/>(System.in)"] --> Scanner["Scanner Object"]
    Scanner --> Parse["Parse Methods<br/>(nextInt, nextLine, etc.)"]
    Parse --> Variable["Java Variable"]

    style Input fill:#0173B2,color:#fff
    style Scanner fill:#DE8F05,color:#fff
    style Parse fill:#029E73,color:#fff
    style Variable fill:#CC78BC,color:#fff
```

**Code**:

```java
import java.util.Scanner;        // => Scanner from java.util package

Scanner scanner = new Scanner(System.in);  // => Creates Scanner for console input
                                 // => Wraps System.in (standard input stream)

System.out.print("Enter your name: ");  // => Prompts user (no newline)
String name = scanner.nextLine();  // => Reads entire line from user
                                 // => Reads line: "Alice\n" → "Alice" (strips newline)

System.out.print("Enter your age: ");  // => Prompts for age
int age = scanner.nextInt();     // => Parses integer from input
                                 // => Parses integer (newline stays in buffer!)
scanner.nextLine();              // => Consume leftover \n from buffer
                                 // => REQUIRED after nextInt (prevents nextLine bug)

System.out.println("Hello, " + name + "! You are " + age + " years old.");  // => Outputs greeting
                                 // => Output: Hello, Alice! You are 25 years old.

scanner.close();                 // => Release resources (closes System.in)
```

**Key Takeaway**: Use `Scanner` for reading console input with type-safe parsing methods (`nextInt`, `nextDouble`, `nextLine`). Always call `scanner.nextLine()` after `nextInt()` or similar methods to consume leftover newlines that would otherwise interfere with subsequent `nextLine()` calls.

**Why It Matters**: Scanner's tokenized parsing eliminates manual string-to-type conversion boilerplate common in languages like C (scanf) or Python (input() + int()). The delimiter-based approach (default: whitespace) enables easy parsing of structured data like CSV files or space-separated integers. However, the newline consumption quirk (nextInt doesn't consume trailing \n) is Java's most common beginner gotcha—responsible for thousands of StackOverflow questions. Production code often prefers BufferedReader for performance or Files.readAllLines() for modern file I/O, but Scanner remains the standard teaching tool and is still used in competitive programming and quick scripts.

---

## Example 4: Control Flow - If/Else and Switch

Java provides `if/else` for conditional branching and `switch` for multi-way branching. Modern Java (14+) adds switch expressions for more concise pattern matching.

**Code**:

```java
int score = 85;                  // => score initialized to 85 (primitive int)

// IF/ELSE - traditional conditional branching
if (score >= 90) {               // => First condition: score >= 90 (85 >= 90)
                                 // => Evaluates to false (85 < 90), skip block
                                 // => Move to next else-if branch
    System.out.println("Grade: A");
                                 // => Not executed (condition failed)
} else if (score >= 80) {        // => Second condition: score >= 80 (85 >= 80)
                                 // => Evaluates to true, enter this block
    System.out.println("Grade: B");  // => Output: Grade: B (executed)
                                 // => Skips remaining else-if and else branches
} else if (score >= 70) {        // => Not evaluated (previous branch taken)
                                 // => else-if chains stop after first match
    System.out.println("Grade: C");
                                 // => Not executed
} else {                         // => Catch-all for score < 70
                                 // => Not evaluated (score >= 80 matched)
    System.out.println("Grade: F");
                                 // => Not executed
}

// SWITCH STATEMENT - multi-way branch
String day = "Monday";           // => day references String "Monday"
switch (day) {                   // => Switch on String value (Java 7+)
                                 // => Compares day with each case using equals()
    case "Monday":               // => Matches: day.equals("Monday") is true
                                 // => Enters this case block
        System.out.println("Start of work week");
                                 // => Output: Start of work week
        break;                   // => Exit switch immediately (prevents fall-through)
                                 // => Without break: would execute next case ("Friday")
    case "Friday":               // => Not matched (already exited via break)
        System.out.println("TGIF!");
                                 // => Not executed
        break;                   // => Exit switch if this case matched
    default:                     // => Matches any value not covered by cases
                                 // => Not reached (day matched "Monday" case)
        System.out.println("Midweek");
                                 // => Not executed
}

// SWITCH EXPRESSION - modern syntax (Java 14+)
int numLetters = switch (day) {  // => Switch as expression (returns value to numLetters)
                                 // => day is "Monday", evaluates to 6
    case "Monday", "Friday" -> 6; // => Arrow syntax (implicit break, returns 6)
                                 // => Matches "Monday" (day value), returns 6
                                 // => Multiple cases with comma (OR logic)
    case "Tuesday" -> 7;         // => Returns 7 if day is "Tuesday"
                                 // => Not matched (day is "Monday")
    case "Wednesday" -> 9;       // => Returns 9 if day is "Wednesday"
                                 // => Not matched
    default -> 0;                // => Required: compiler checks exhaustiveness
                                 // => Returns 0 for any other day value
                                 // => Not reached (day matched earlier case)
};                               // => numLetters assigned 6 (from "Monday" case)
System.out.println(numLetters);  // => Output: 6 (value returned by switch expression)
```

**Key Takeaway**: Use `if/else` for simple boolean conditions and modern switch expressions for multi-way branching based on discrete values. Switch expressions with arrow syntax (`->`) eliminate fall-through bugs and enable value-returning switches, making code safer and more concise than traditional switch statements.

**Why It Matters**: Switch expressions (Java 14+) fix Java's most criticized control flow defect: the mandatory `break` keyword that caused decades of fall-through bugs where forgetting `break` executed unintended cases. The arrow syntax (`->`) provides implicit break behavior and enables switches as expressions that return values, eliminating temporary variables. This aligns Java with modern languages (Kotlin's `when`, Rust's `match`) while maintaining backward compatibility. Pattern matching in switch (Java 17+ preview, 21 stable) extends this further to type-safe object decomposition, finally bringing algebraic data type capabilities to Java's type system after 25 years.

---

## Example 5: Loops - For, While, Do-While

Java provides three loop constructs: `for` (known iteration count), `while` (condition-based), and `do-while` (execute-then-check). Enhanced for-loop simplifies collection iteration.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Start["Start Loop"] --> Init["For: Initialize i = 0<br/>While: Check condition<br/>Do-While: Execute body"]
    Init --> Check{"Condition<br/>true?"}
    Check -->|Yes| Body["Execute loop body"]
    Body --> Update["For: i++<br/>While/Do-While: No auto-update"]
    Update --> Check
    Check -->|No| End["Exit loop"]

    style Start fill:#0173B2,color:#fff
    style Check fill:#DE8F05,color:#fff
    style Body fill:#029E73,color:#fff
    style Update fill:#CC78BC,color:#fff
    style End fill:#CA9161,color:#fff
```

**Code**:

```java
// FOR LOOP - known iteration count
for (int i = 0; i < 5; i++) {    // => Initialize: i = 0
                                 // => Condition: i < 5 (checked before each iteration)
                                 // => Update: i++ (increments after each iteration)
    System.out.print(i + " ");   // => Prints current i value
                                 // => Iteration 1: i=0, output "0 "
                                 // => Iteration 2: i=1, output "1 "
                                 // => Iteration 3: i=2, output "2 "
                                 // => Iteration 4: i=3, output "3 "
                                 // => Iteration 5: i=4, output "4 "
}                                // => After loop: i=5 (out of scope)
                                 // => Output: 0 1 2 3 4
System.out.println();            // => Newline (moves cursor to next line)

// WHILE LOOP - condition-based iteration
int count = 0;                   // => count initialized to 0
while (count < 3) {              // => Condition checked BEFORE body executes
                                 // => Loop continues while count < 3
    System.out.print(count + " ");
                                 // => Iteration 1: count=0, output "0 "
                                 // => Iteration 2: count=1, output "1 "
                                 // => Iteration 3: count=2, output "2 "
    count++;                     // => Increment count manually (no auto-increment)
                                 // => count becomes 1, then 2, then 3
}                                // => count=3, condition false, exit loop
                                 // => Output: 0 1 2
System.out.println();            // => Newline

// DO-WHILE LOOP - execute-then-check pattern
int num = 0;                     // => num initialized to 0
do {                             // => Body executes FIRST (before condition check)
    System.out.print(num + " "); // => Iteration 1: num=0, output "0 "
                                 // => Iteration 2: num=1, output "1 "
                                 // => Iteration 3: num=2, output "2 "
    num++;                       // => num becomes 1, then 2, then 3
} while (num < 3);               // => Condition checked AFTER body executes
                                 // => num=3, condition false, exit loop
                                 // => Guarantees at least ONE execution (even if condition initially false)
                                 // => Output: 0 1 2
System.out.println();            // => Newline

// ENHANCED FOR LOOP - collection iteration
int[] numbers = {10, 20, 30};    // => Array with 3 elements
for (int n : numbers) {          // => "for each" syntax: for (element : collection)
                                 // => n takes value of each element sequentially
                                 // => Iteration 1: n=10, output "10 "
                                 // => Iteration 2: n=20, output "20 "
                                 // => Iteration 3: n=30, output "30 "
    System.out.print(n + " ");   // => No manual index tracking needed
                                 // => Cannot modify array elements (n is copy)
}                                // => Output: 10 20 30
```

**Key Takeaway**: Use `for` loops for known iteration counts, `while` for condition-based looping, and enhanced for-loops (`for (element : collection)`) to iterate over arrays/collections without index management. Do-while guarantees at least one execution, unlike while which may execute zero times.

**Why It Matters**: The enhanced for-loop (Java 5, 2004) eliminated a major source of off-by-one errors and verbose index arithmetic that plagued traditional for-loops. Before enhanced for, iterating arrays required manual bounds checking (`for (int i = 0; i < array.length; i++)`)—error-prone when mixing `<` vs `<=` or accessing `array[i+1]`. The enhanced syntax `for (Type elem : collection)` also provides iterator abstraction, working identically for arrays, Lists, Sets, and any Iterable, enabling generic code. Combined with streams (Java 8+), declarative iteration replaced imperative loops for most collection processing, dramatically improving code readability and reducing bugs.

---

## Example 6: Arrays and Array Operations

Arrays are fixed-size, indexed collections storing elements of a single type. They're the foundation of Java's collection framework and critical for performance-sensitive code.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Declaration["Array Declaration<br/>int[] arr"] --> Allocation["Memory Allocation<br/>new int[5]"]
    Allocation --> Elements["Contiguous Memory<br/>[0][1][2][3][4]"]
    Elements --> Access["Index Access<br/>arr[2]"]

    style Declaration fill:#0173B2,color:#fff
    style Allocation fill:#DE8F05,color:#fff
    style Elements fill:#029E73,color:#fff
    style Access fill:#CC78BC,color:#fff
```

**Code**:

```java
// ARRAY DECLARATION AND INITIALIZATION
int[] numbers = {1, 2, 3, 4, 5}; // => Inline initialization (array literal)
                                 // => numbers holds reference to heap array
                                 // => Array: [1, 2, 3, 4, 5], length=5
int[] empty = new int[10];       // => Allocates array of 10 elements
                                 // => All elements initialized to default (0 for int)
                                 // => empty: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0], length=10

// ARRAY ACCESS
int first = numbers[0];          // => Access element at index 0 (first element)
                                 // => first is 1 (0-based indexing)
int last = numbers[numbers.length - 1];
                                 // => numbers.length is 5 (property, not method)
                                 // => Access index 4 (last element, 5-1)
                                 // => last is 5
numbers[2] = 99;                 // => Mutate element at index 2
                                 // => numbers becomes [1, 2, 99, 4, 5]
                                 // => Arrays are mutable (elements can be changed)

// ARRAY UTILITIES (import java.util.Arrays)
int[] copy = Arrays.copyOf(numbers, numbers.length);
                                 // => Creates deep copy of numbers array
                                 // => copy is new array: [1, 2, 99, 4, 5]
                                 // => Separate memory location from numbers
String str = Arrays.toString(numbers);
                                 // => Converts array to readable string format
                                 // => str is "[1, 2, 99, 4, 5]"
                                 // => Useful for debugging (arrays don't have toString())
Arrays.sort(numbers);            // => In-place sort (modifies original array)
                                 // => numbers becomes [1, 2, 4, 5, 99] (ascending order)
                                 // => Uses dual-pivot quicksort internally

// MULTIDIMENSIONAL ARRAYS (array of arrays)
int[][] matrix = {               // => 2D array (array of int[] arrays)
    {1, 2, 3},                   // => Row 0: [1, 2, 3]
    {4, 5, 6}                    // => Row 1: [4, 5, 6]
};                               // => matrix has 2 rows, 3 columns
int value = matrix[1][2];        // => Access row 1, column 2
                                 // => matrix[1] is [4, 5, 6]
                                 // => matrix[1][2] is 6
                                 // => value is 6
```

**Key Takeaway**: Arrays have fixed size determined at creation and use zero-based indexing. Access array length via `.length` property (not `.length()` method). Use `Arrays` utility class for common operations like sorting, copying, and string conversion—don't reinvent these operations.

**Why It Matters**: Arrays are Java's lowest-level data structure, offering O(1) random access and minimal memory overhead (just object header + elements), making them essential for performance-critical code in games, financial systems, and data processing. Unlike higher-level collections (ArrayList, HashMap), arrays have no abstraction overhead—direct memory layout enables JIT compiler optimizations like loop unrolling and SIMD vectorization. However, fixed size is often limiting; ArrayList (backed by resizable array) is preferred for most business logic. The `Arrays` utility class (Java 1.2, 1998) standardized common operations that previously required manual loops, eliminating bugs from manual binary search or mergesort implementations.

---

## Example 7: Classes and Objects

Classes are blueprints for objects, defining fields (state) and methods (behavior). Objects are instances of classes created with the `new` keyword.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Class["Class Person<br/>#40;Blueprint#41;"] --> Fields["Fields: name, age<br/>#40;State#41;"]
    Class --> Methods["Methods: introduce#40;#41;<br/>#40;Behavior#41;"]
    Class --> Constructor["Constructor<br/>Person#40;name, age#41;"]

    Constructor --> Object1["Object: alice<br/>name=#quot;Alice#quot;, age=30"]
    Constructor --> Object2["Object: bob<br/>name=#quot;Bob#quot;, age=25"]

    style Class fill:#0173B2,color:#fff
    style Fields fill:#DE8F05,color:#fff
    style Methods fill:#DE8F05,color:#fff
    style Constructor fill:#029E73,color:#fff
    style Object1 fill:#CC78BC,color:#fff
    style Object2 fill:#CC78BC,color:#fff
```

**Code**:

```java
// CLASS DEFINITION
public class Person {            // => Blueprint for Person objects
    // FIELDS (instance variables)
    String name;                 // => Package-private field, each object has own copy
    int age;                     // => Auto-initialized (null for String, 0 for int)

    // CONSTRUCTOR
    public Person(String name, int age) {
                                 // => Constructor name MUST match class name
        this.name = name;        // => this.name: instance field, name: parameter
        this.age = age;          // => Assigns parameter to instance field
    }

    // METHOD
    public void introduce() {    // => Instance method, operates on specific object
        System.out.println("Hi, I'm " + name + " and I'm " + age + " years old.");
                                 // => name/age implicitly this.name/this.age
    }
}

// CREATING OBJECTS
Person alice = new Person("Alice", 30);
                                 // => new allocates heap memory + calls constructor
                                 // => alice holds reference to heap object
Person bob = new Person("Bob", 25);
                                 // => Separate object with independent state

// CALLING METHODS
alice.introduce();               // => this refers to alice object
                                 // => Output: Hi, I'm Alice and I'm 30 years old.
bob.introduce();                 // => this refers to bob object
                                 // => Output: Hi, I'm Bob and I'm 25 years old.

// FIELD ACCESS
System.out.println(alice.name);  // => Output: Alice
```

**Key Takeaway**: Classes define object templates with fields (state) and methods (behavior). Constructors initialize objects via `new` keyword. Each object has independent state—modifying one object doesn't affect others. Use `this` keyword to distinguish instance fields from parameters when names collide.

**Why It Matters**: Java's "everything is an object" philosophy (except primitives) forced consistent OOP design where behavior and data are always bundled together, unlike C's struct + separate functions or JavaScript's prototype-based objects. This enabled large-scale code organization through encapsulation—private fields hide implementation details while public methods expose controlled interfaces. The constructor mechanism (inspired by C++) standardized object initialization, eliminating C's uninitialized struct memory bugs. Constructors must be explicitly called via `new`, preventing C++'s stack allocation confusion (automatic constructor calls), making object lifetime explicit and simplifying garbage collection.

---

## Example 8: Inheritance and Polymorphism

Inheritance creates class hierarchies where subclasses extend superclasses, inheriting fields and methods. Polymorphism allows treating specialized objects through general types.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Animal["Animal Superclass<br/>makeSound#40;#41;"] --> Dog["Dog Subclass<br/>extends Animal"]
    Animal --> Cat["Cat Subclass<br/>extends Animal"]
    Dog --> DogObj["Dog object:<br/>makeSound#40;#41; → Woof"]
    Cat --> CatObj["Cat object:<br/>makeSound#40;#41; → Meow"]

    style Animal fill:#0173B2,color:#fff
    style Dog fill:#DE8F05,color:#fff
    style Cat fill:#DE8F05,color:#fff
    style DogObj fill:#029E73,color:#fff
    style CatObj fill:#029E73,color:#fff
```

**Code**:

```java
// SUPERCLASS
class Animal {                   // => Base class (superclass, parent class)
                                 // => Defines common behavior for all animals
    public void makeSound() {    // => Instance method with default implementation
                                 // => Subclasses can override this method
        System.out.println("Some generic animal sound");
                                 // => Default output for base Animal class
    }
}

// SUBCLASSES
class Dog extends Animal {       // => Dog inherits from Animal (is-a relationship)
                                 // => Dog IS-A Animal (subtype relationship)
                                 // => Inherits all Animal fields and methods
    @Override                    // => Annotation verifies method signature matches superclass
                                 // => Compile error if signature doesn't match Animal.makeSound()
    public void makeSound() {    // => Overrides Animal's makeSound() method
                                 // => Replaces default implementation with Dog-specific behavior
        System.out.println("Woof!");  // => Dog's specific sound
                                 // => Output: Woof! (when Dog calls makeSound)
    }
}

class Cat extends Animal {       // => Cat also extends Animal (is-a relationship)
                                 // => Cat IS-A Animal (another subtype)
                                 // => Inherits all Animal fields and methods
    @Override                    // => Verifies override of Animal.makeSound()
                                 // => Compiler checks signature correctness
    public void makeSound() {    // => Cat's version of makeSound()
                                 // => Overrides default Animal implementation
        System.out.println("Meow!");  // => Cat's specific sound
                                 // => Output: Meow! (when Cat calls makeSound)
    }
}

// POLYMORPHISM
Animal animal1 = new Dog();      // => Dog object referenced as Animal type (upcast)
                                 // => animal1 is Animal reference pointing to Dog object
                                 // => Compile-time type: Animal, Runtime type: Dog
Animal animal2 = new Cat();      // => Cat object referenced as Animal type (upcast)
                                 // => animal2 is Animal reference pointing to Cat object
                                 // => Compile-time type: Animal, Runtime type: Cat
animal1.makeSound();             // => Calls Dog's makeSound() (dynamic dispatch)
                                 // => JVM checks runtime type (Dog), calls Dog.makeSound()
                                 // => Output: Woof! (Dog's overridden method)
animal2.makeSound();             // => Calls Cat's makeSound() (dynamic dispatch)
                                 // => JVM checks runtime type (Cat), calls Cat.makeSound()
                                 // => Output: Meow! (Cat's overridden method)

// ARRAY OF POLYMORPHIC OBJECTS
Animal[] animals = {new Dog(), new Cat(), new Dog()};
                                 // => Array of Animal references (compile-time type)
                                 // => Contains Dog and Cat objects (runtime types)
                                 // => animals[0] is Dog, animals[1] is Cat, animals[2] is Dog
for (Animal a : animals) {       // => Iterate using Animal type (polymorphic iteration)
                                 // => a references Dog, then Cat, then Dog
    a.makeSound();               // => Dynamic dispatch: calls actual object's method
                                 // => Iteration 1: Dog.makeSound() → Output: Woof!
                                 // => Iteration 2: Cat.makeSound() → Output: Meow!
                                 // => Iteration 3: Dog.makeSound() → Output: Woof!
}                                // => Total output: Woof! Meow! Woof! (3 lines)
```

**Key Takeaway**: Inheritance (`extends`) creates is-a relationships where subclasses inherit superclass members. Override methods with `@Override` annotation to customize behavior. Polymorphism lets you reference subclass objects via superclass type—method calls dynamically dispatch to the actual object's overridden method at runtime.

**Why It Matters**: Polymorphism is Java's mechanism for code reuse and extensibility without modifying existing code. Before polymorphism, adding new types (like a new Animal subclass) required modifying every function that processed animals (switch statements checking type). With polymorphism, new subclasses integrate seamlessly—existing code calling `animal.makeSound()` works with new types without changes (Open/Closed Principle). This enabled frameworks like Spring and Hibernate to operate on user-defined classes through interfaces, revolutionizing enterprise Java. However, overuse created "ClassExplosion" anti-pattern in early 2000s Java (deep inheritance hierarchies); modern Java favors composition (fields of interface types) over inheritance.

---

## Example 9: Interfaces and Abstraction

Interfaces define contracts (what methods a class must implement) without implementation. Classes can implement multiple interfaces, enabling flexible type hierarchies.

**Code**:

```java
// INTERFACE DEFINITION
public interface Drawable {      // => Contract for drawable objects
    void draw();                 // => Abstract method (no body, no implementation)
    double PI = 3.14159;         // => Constant (public static final by default)
}                                // => All methods implicitly public abstract
                                 // => All fields implicitly public static final

// CLASS IMPLEMENTING INTERFACE
class Circle implements Drawable {  // => Implements Drawable contract
                                    // => Must implement all methods
    @Override                       // => Indicates overriding interface method
    public void draw() {            // => Concrete implementation required
        System.out.println("Drawing a circle");  // => Output: Drawing a circle
                                    // => Concrete implementation
    }
}

class Square implements Drawable {  // => Another Drawable implementation
    @Override                       // => Overrides draw()
    public void draw() {            // => Square's specific behavior
        System.out.println("Drawing a square");  // => Output: Drawing a square
    }
}

// MULTIPLE INTERFACE IMPLEMENTATION
interface Resizable {               // => Second interface (resizing capability)
    void resize(int factor);        // => Abstract resize method
}

class FlexibleCircle implements Drawable, Resizable {  // => Implements TWO interfaces
                                    // => Multiple interfaces allowed (unlike inheritance)
    @Override                       // => Implements Drawable.draw()
    public void draw() {            // => FlexibleCircle's draw behavior
        System.out.println("Drawing flexible circle");  // => Output: Drawing flexible circle
    }

    @Override                       // => Implements Resizable.resize()
    public void resize(int factor) {  // => FlexibleCircle's resize behavior
        System.out.println("Resizing by " + factor);  // => Output: Resizing by 3 (example)
    }
}

// POLYMORPHISM WITH INTERFACES
Drawable shape1 = new Circle();  // => Reference via interface type
                                 // => shape1 = Circle instance (polymorphic reference)
Drawable shape2 = new Square();  // => shape2 = Square instance
                                 // => Both shape1 and shape2 are Drawable type
shape1.draw();                   // => Calls Circle's draw() (dynamic dispatch)
                                 // => Output: Drawing a circle
shape2.draw();                   // => Calls Square's draw()
                                 // => Output: Drawing a square
```

**Key Takeaway**: Interfaces define method contracts without implementation, forcing implementing classes to provide behavior. Classes can implement multiple interfaces (unlike single-class inheritance), enabling flexible type hierarchies. Use interfaces to define capabilities (Drawable, Runnable, Comparable) rather than concrete types.

**Why It Matters**: Interfaces solve Java's single-inheritance limitation—while a class can only extend one superclass, it can implement unlimited interfaces, enabling role-based composition. This design pattern (interface segregation) prevents the brittle base class problem where changing a superclass breaks all subclasses. Modern Java frameworks depend entirely on interfaces: Spring's dependency injection wires interface types, JPA repositories extend interface hierarchies, and servlet containers call interface methods (Servlet, Filter, Listener). Java 8's default methods (interface methods with bodies) later enabled interface evolution without breaking implementations, crucial for adding stream operations to Collections framework after 15 years.

---

## Example 10: ArrayList - Dynamic Arrays

ArrayList is a resizable array implementation providing fast random access and automatic growth. It's Java's most commonly used collection type for ordered, index-accessible elements.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Create["ArrayList<String> names"] --> Internal["Internal array<br/>capacity=10"]
    Internal --> Add1["add Alice"] --> Grow1["[Alice, null, null, ...]"]
    Grow1 --> Add2["add Bob"] --> Grow2["[Alice, Bob, null, ...]"]
    Grow2 --> Insert["add#40;1, Charlie#41;"] --> Shift["Shift Bob right<br/>[Alice, Charlie, Bob, ...]"]

    style Create fill:#0173B2,color:#fff
    style Internal fill:#DE8F05,color:#fff
    style Add1 fill:#029E73,color:#fff
    style Add2 fill:#029E73,color:#fff
    style Insert fill:#CC78BC,color:#fff
    style Shift fill:#CA9161,color:#fff
```

**Code**:

```java
import java.util.ArrayList;      // => ArrayList from java.util package

// CREATE ArrayList
ArrayList<String> names = new ArrayList<>();
                                 // => Creates empty ArrayList with default capacity 10
                                 // => Generic type <String> specifies element type (compile-time safety)
                                 // => Diamond operator <> infers type from left side
                                 // => Backed by resizable Object[] array internally
                                 // => names holds reference to heap ArrayList object
                                 // => names is [], size=0, capacity=10

// ADD ELEMENTS
names.add("Alice");              // => Appends "Alice" to end (index 0)
                                 // => names is ["Alice"], size=1
names.add("Bob");                // => Appends "Bob" to end (index 1)
                                 // => names is ["Alice", "Bob"], size=2
names.add(1, "Charlie");         // => Insert "Charlie" at index 1
                                 // => Shifts "Bob" right to index 2 (O(n) operation)
                                 // => names is ["Alice", "Charlie", "Bob"], size=3

// ACCESS ELEMENTS
String first = names.get(0);     // => Retrieves element at index 0 (O(1) random access)
                                 // => first is "Alice" (value at index 0)
int size = names.size();         // => Returns number of elements (method, not property)
                                 // => size is 3 (current number of elements)

// MODIFY ELEMENTS
names.set(2, "Dave");            // => Replace element at index 2 with "Dave"
                                 // => "Bob" at index 2 replaced with "Dave"
                                 // => names is ["Alice", "Charlie", "Dave"], size=3

// REMOVE ELEMENTS
names.remove("Charlie");         // => Remove first occurrence of "Charlie" (O(n) search)
                                 // => Finds "Charlie" at index 1, removes it
                                 // => Shifts "Dave" left to index 1 (fills gap)
                                 // => names is ["Alice", "Dave"], size=2
names.remove(0);                 // => Remove element at index 0 (O(n) shift)
                                 // => Removes "Alice", shifts "Dave" left to index 0
                                 // => names is ["Dave"], size=1

// ITERATE
for (String name : names) {      // => Enhanced for-loop iterates over all elements
                                 // => name takes value of each element ("Dave")
    System.out.println(name);    // => Output: Dave (prints each element)
}                                // => Loop completes after 1 iteration

// CONTAINS AND SEARCH
boolean has = names.contains("Dave");
                                 // => Linear search O(n) checks if "Dave" exists
                                 // => has is true ("Dave" is in list)
int index = names.indexOf("Dave");
                                 // => Returns first index of "Dave" (O(n) search)
                                 // => index is 0 ("Dave" is at index 0)
                                 // => Returns -1 if element not found
```

**Key Takeaway**: ArrayList provides dynamic arrays that grow automatically, avoiding fixed-size limitations of primitive arrays. Use `add()` to append, `get(index)` to access, `set(index, value)` to modify, and `remove()` to delete. ArrayList maintains insertion order and allows duplicates, making it ideal for ordered collections with unknown size.

**Why It Matters**: ArrayList replaced manual array resizing logic (copying to larger arrays) that plagued pre-Collections Framework Java (before 1.2, 1998). The automatic doubling strategy (capacity doubles when full) provides amortized O(1) append performance, eliminating the O(n) cost of shifting elements in manual implementations. Generic types (`ArrayList<String>`) added in Java 5 (2004) eliminated dangerous ClassCastException runtime errors from pre-generics `ArrayList` storing Object types. Despite ArrayList's ubiquity, it's internally just a resizable `Object[]` array—understanding this reveals why random access is O(1) but insertion/deletion at arbitrary positions is O(n) due to element shifting.

---

## Example 11: HashMap - Key-Value Mappings

HashMap stores key-value pairs with O(1) average-case lookup using hash-based indexing. It's essential for fast associative data structures like caches, indexes, and dictionaries.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Key["Key: Alice"] --> Hash["hashCode#40;#41;<br/>hash value"]
    Hash --> Bucket["Bucket index<br/>#40;hash % array.length#41;"]
    Bucket --> Entry["Entry: Alice=30<br/>stored in bucket"]
    Entry --> Lookup["get Alice"] --> Found["Return value: 30"]

    style Key fill:#0173B2,color:#fff
    style Hash fill:#DE8F05,color:#fff
    style Bucket fill:#029E73,color:#fff
    style Entry fill:#CC78BC,color:#fff
    style Found fill:#CA9161,color:#fff
```

**Code**:

```java
import java.util.HashMap;
import java.util.Map;

// CREATE HashMap
HashMap<String, Integer> ages = new HashMap<>();
                                 // => Key type: String, Value type: Integer
                                 // => Backed by hash table (array + linked lists/trees)

// PUT key-value pairs
ages.put("Alice", 30);           // => Computes hash of "Alice" key using hashCode()
                                 // => Maps hash to bucket index: hash % array.length
                                 // => Stores Entry("Alice", 30) at computed bucket
                                 // => ages is {"Alice": 30}, size is 1
ages.put("Bob", 25);             // => Computes hash of "Bob", maps to bucket index
                                 // => Different hash likely results in different bucket
                                 // => Stores Entry("Bob", 25) at its bucket
                                 // => ages is {"Alice": 30, "Bob": 25}, size is 2
ages.put("Alice", 31);           // => Computes hash of "Alice" (same as before)
                                 // => Finds existing Entry with key "Alice" in bucket
                                 // => Overwrites old value 30 with new value 31
                                 // => ages is {"Alice": 31, "Bob": 25}, size still 2

// GET values by key
int aliceAge = ages.get("Alice");// => Computes hash of "Alice", looks up bucket
                                 // => aliceAge is 31 (value found)
Integer charlieAge = ages.get("Charlie");
                                 // => Computes hash, looks up bucket, key not found
                                 // => charlieAge is null (key doesn't exist, returns null)

// GET with default value (Java 8+)
int defaultAge = ages.getOrDefault("Charlie", 0);
                                 // => Looks up "Charlie", not found
                                 // => Returns default value 0 instead of null
                                 // => defaultAge is 0 (Charlie not in map)

// CHECK existence
boolean hasAlice = ages.containsKey("Alice");
                                 // => hasAlice is true
boolean has25 = ages.containsValue(25);
                                 // => has25 is true

// REMOVE
ages.remove("Bob");              // => ages is {"Alice": 31}

// ITERATE over entries
for (Map.Entry<String, Integer> entry : ages.entrySet()) {
    System.out.println(entry.getKey() + ": " + entry.getValue());
    // => Output: Alice: 31
}
```

**Key Takeaway**: HashMap provides O(1) average key-value lookup using hash codes. Keys must implement `hashCode()` and `equals()` properly. Use `put()` to insert/update, `get()` to retrieve, and `containsKey()` to check existence. HashMap does NOT maintain insertion order—use LinkedHashMap if order matters.

**Why It Matters**: HashMap revolutionized associative data structures by providing near-constant-time lookup (vs O(log n) for TreeMap, O(n) for ArrayList search). The hash function distributes keys across buckets, enabling fast retrieval even with millions of entries—critical for caches, database indexes, and routing tables. Java 8's HashMap improvements (tree bins when buckets exceed 8 elements) prevent worst-case O(n) lookups from hash collisions, a vulnerability exploited in hash collision DoS attacks pre-Java 8. String keys are common (configuration maps, JSON parsing), and String's cached `hashCode()` (computed once, stored in field) makes string-keyed HashMaps especially fast.

---

## Example 12: HashSet - Unique Collections

HashSet stores unique elements with O(1) add/contains operations. It's backed by HashMap internally, using elements as keys with a dummy value.

**Code**:

```java
import java.util.HashSet;

// CREATE HashSet
HashSet<String> unique = new HashSet<>();
                                 // => unique is empty HashSet<String>

// ADD elements (duplicates ignored)
unique.add("apple");             // => unique is {"apple"}, returns true (added)
                                 // => Uses hashCode() to determine bucket
unique.add("banana");            // => unique is {"apple", "banana"}
unique.add("apple");             // => unique unchanged (duplicate), returns false (not added)
                                 // => equals() comparison detects duplicate

// CONTAINS check
boolean has = unique.contains("apple");
                                 // => has is true (O(1) lookup via hash)

// REMOVE
unique.remove("banana");         // => unique is {"apple"}

// SIZE
int count = unique.size();       // => count is 1

// ITERATE (unordered!)
for (String item : unique) {     // => Iterates in hash table order (not insertion order)
    System.out.println(item);    // => Output: apple
}

// SET OPERATIONS
HashSet<Integer> set1 = new HashSet<>(Arrays.asList(1, 2, 3));
                                 // => set1 is {1, 2, 3}
HashSet<Integer> set2 = new HashSet<>(Arrays.asList(3, 4, 5));
                                 // => set2 is {3, 4, 5}

set1.addAll(set2);               // => Union: set1 is {1, 2, 3, 4, 5}
// set1.retainAll(set2);         // => Intersection: keeps only elements in both
// set1.removeAll(set2);         // => Difference: removes elements in set2
```

**Key Takeaway**: HashSet guarantees uniqueness using `equals()` and `hashCode()` for element comparison. Add, remove, and contains operations are O(1) average case. HashSet does NOT maintain order—use LinkedHashSet for insertion order or TreeSet for sorted order.

**Why It Matters**: HashSet implements mathematical set semantics (unique elements, set operations) with hash table performance, eliminating the O(n) duplicate-checking overhead of "contains before add" patterns with ArrayList. This makes HashSet essential for deduplication (removing duplicates from collections), membership testing (checking if element exists), and set algebra (union, intersection, difference). Internally, HashSet is just a HashMap with elements as keys and a dummy `PRESENT` constant as value—understanding this reveals why HashSet has same performance characteristics as HashMap and why element `hashCode()` quality directly impacts performance.

---

## Example 13: Control Flow - Ternary and Operators

Beyond if/else, Java provides the ternary operator (`? :`) for inline conditional expressions and short-circuit logical operators for efficient boolean evaluation.

**Code**:

```java
// TERNARY OPERATOR - inline conditional expression
int age = 20;                    // => age is 20 (primitive int)
String status = (age >= 18) ? "adult" : "minor";
                                 // => Ternary syntax: (condition) ? valueIfTrue : valueIfFalse
                                 // => Evaluates age >= 18 → true
                                 // => Returns "adult" (true branch)
                                 // => status is "adult" (20 >= 18 is true)
                                 // => More concise than if/else for simple assignments

// Equivalent if/else (more verbose)
String status2;                  // => Declare variable first
if (age >= 18) {                 // => Condition evaluates to true
    status2 = "adult";           // => Enters true branch
} else {
    status2 = "minor";
}                                // => status2 is "adult" (same result)

// SHORT-CIRCUIT OPERATORS
boolean a = true;                // => a is true
boolean b = false;               // => b is false

boolean and = a && b;            // => and is false (both must be true)
                                 // => Short-circuit: if a is false, b never evaluated
boolean or = a || b;             // => or is true (at least one true)
                                 // => Short-circuit: if a is true, b never evaluated
                                 // => Prevents unnecessary expensive operations

// Short-circuit prevents null pointer errors
String str = null;               // => str references null (no object)
if (str != null && str.length() > 0) {
                                 // => First condition: str != null → false
                                 // => Second condition: str.length() NOT evaluated (short-circuit)
                                 // => && short-circuits: if left is false, right skipped
                                 // => Safe: no NullPointerException (str.length() never called)
                                 // => Entire condition is false, if block skipped
    System.out.println("Non-empty string");
                                 // => Not executed (condition false)
}

// COMPARISON OPERATORS
int x = 10;                      // => x = 10 (primitive int)
int y = 20;                      // => y = 20 (primitive int)
boolean equal = (x == y);        // => Compares values: 10 == 20 → false
                                 // => equal = false
boolean notEqual = (x != y);     // => Compares values: 10 != 20 → true
                                 // => notEqual = true
boolean greater = (x > y);       // => Compares values: 10 > 20 → false
                                 // => greater = false
boolean lessOrEqual = (x <= y);  // => Compares values: 10 <= 20 → true
                                 // => lessOrEqual = true

// REFERENCE vs VALUE equality
String s1 = new String("hello"); // => Creates NEW String object in heap
                                 // => s1 references object at memory location A
String s2 = new String("hello"); // => Creates ANOTHER new String object in heap
                                 // => s2 references object at memory location B
                                 // => Both have same content "hello" but different locations
boolean refEqual = (s1 == s2);   // => Compares memory addresses (references)
                                 // => s1 address A != s2 address B
                                 // => refEqual = false (different objects)
boolean valueEqual = s1.equals(s2);
                                 // => Compares string content using equals() method
                                 // => "hello".equals("hello") → true
                                 // => valueEqual = true (same content)
```

**Key Takeaway**: Use ternary operator (`condition ? true : false`) for simple inline conditionals, replacing verbose if/else. Logical operators `&&` and `||` short-circuit—right side only evaluated if necessary, preventing NullPointerExceptions. For objects, use `equals()` for value comparison, `==` for reference comparison.

**Why It Matters**: Short-circuit evaluation prevents defensive null checks from becoming nested if pyramids—`if (obj != null && obj.method())` is cleaner than nested `if (obj != null) { if (obj.method()) {...} }`. The ternary operator enables functional-style expressions where every construct returns a value (common in modern Java streams), though overuse creates unreadable one-liners. The `==` vs `equals()` distinction trips up every Java beginner—`==` compares memory addresses for objects (reference equality), while `equals()` compares content (value equality). This design enables object identity checks (`list.remove(specific object reference)`) while requiring explicit value comparison, preventing C++'s operator overloading ambiguity.

---

## Example 14: Enhanced Loops and Iterators

Java's enhanced for-loop simplifies iteration over arrays and collections. Under the hood, it uses the Iterator pattern for type-safe traversal.

**Code**:

```java
import java.util.*;

// ENHANCED FOR with ArrayList
ArrayList<String> fruits = new ArrayList<>(Arrays.asList("apple", "banana", "cherry"));
                                 // => fruits is ["apple", "banana", "cherry"]

for (String fruit : fruits) {    // => Enhanced for-loop: for (element : collection)
                                 // => Internally uses Iterator (compiler translation)
                                 // => Iteration 1: fruit="apple"
                                 // => Iteration 2: fruit="banana"
                                 // => Iteration 3: fruit="cherry"
    System.out.println(fruit);   // => Output: apple, banana, cherry (3 lines)
}

// TRADITIONAL for loop with index
for (int i = 0; i < fruits.size(); i++) {
                                 // => Loop with explicit index i
                                 // => fruits.size() is 3
    System.out.println(i + ": " + fruits.get(i));
                                 // => Output: 0: apple, 1: banana, 2: cherry
                                 // => Use when index needed (numbering, parallel arrays)
}

// ITERATOR - manual iteration control
Iterator<String> iter = fruits.iterator();
                                 // => Creates iterator for fruits collection
                                 // => iter positioned before first element
while (iter.hasNext()) {         // => Check if more elements exist
                                 // => hasNext() returns true if elements remain
    String fruit = iter.next();  // => Get next element and advance iterator
                                 // => Iteration 1: fruit="apple"
                                 // => Iteration 2: fruit="banana"
                                 // => Iteration 3: fruit="cherry"
    System.out.println(fruit);   // => Output: apple, banana, cherry
    if (fruit.equals("banana")) {// => Check if current fruit is "banana"
        iter.remove();           // => Safe removal using iterator
                                 // => Removes "banana" from fruits collection
                                 // => fruits becomes ["apple", "cherry"]
    }
}

// CAUTION: ConcurrentModificationException
ArrayList<Integer> numbers = new ArrayList<>(Arrays.asList(1, 2, 3, 4));
                                 // => numbers is [1, 2, 3, 4]
for (Integer num : numbers) {    // => Enhanced for creates internal iterator
    // numbers.remove(num);      // => ERROR: Modifies collection during iteration
                                 // => Throws ConcurrentModificationException
                                 // => Fail-fast mechanism prevents iterator corruption
}

// CORRECT removal with removeIf (Java 8+)
numbers.removeIf(num -> num % 2 == 0);
                                 // => Lambda predicate: num -> num % 2 == 0
                                 // => Removes elements where predicate returns true
                                 // => Checks: 1 % 2 == 0 → false (keep)
                                 // => Checks: 2 % 2 == 0 → true (remove)
                                 // => Checks: 3 % 2 == 0 → false (keep)
                                 // => Checks: 4 % 2 == 0 → true (remove)
                                 // => numbers becomes [1, 3] (even numbers removed)
```

**Key Takeaway**: Enhanced for-loops (`for (element : collection)`) provide clean iteration syntax for read-only traversal. To remove elements during iteration, use `Iterator.remove()` or `Collection.removeIf()`, NOT direct collection modification which throws ConcurrentModificationException. Use traditional for-loops when you need the index.

**Why It Matters**: The enhanced for-loop (Java 5, 2004) eliminated index-out-of-bounds errors and verbose iterator boilerplate that plagued Java collections before. It works with any `Iterable` type (Lists, Sets, arrays), providing uniform syntax across data structures. However, the ConcurrentModificationException (thrown when modifying collection during iteration) is Java's fail-fast mechanism to prevent iterator invalidation bugs—understanding this prevents the common mistake of `list.remove(element)` inside `for (element : list)`. Modern Java's `removeIf()` method (Java 8) finally provided a safe removal API, using iterators internally to avoid the exception.

---

## Example 15: Methods and Parameter Passing

Java methods encapsulate reusable logic with parameters and return values. Parameters are pass-by-value—primitives copy values, objects copy references (both are value copies, but object references point to same heap object).

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Call["Method Call<br/>modifyPrimitive#40;num#41;"] --> CopyPrim["Copy primitive value<br/>param x = 10"]
    CopyPrim --> ModifyPrim["Modify x = 100<br/>#40;local copy#41;"]
    ModifyPrim --> ReturnPrim["Return<br/>Original num still 10"]

    Call2["Method Call<br/>modifyArray#40;arr#41;"] --> CopyRef["Copy reference<br/>param points to same array"]
    CopyRef --> ModifyHeap["Modify arr[0] = 999<br/>#40;affects original#41;"]
    ModifyHeap --> ReturnRef["Return<br/>Original array modified"]

    style Call fill:#0173B2,color:#fff
    style Call2 fill:#0173B2,color:#fff
    style CopyPrim fill:#DE8F05,color:#fff
    style CopyRef fill:#DE8F05,color:#fff
    style ModifyPrim fill:#029E73,color:#fff
    style ModifyHeap fill:#029E73,color:#fff
    style ReturnPrim fill:#CC78BC,color:#fff
    style ReturnRef fill:#CA9161,color:#fff
```

**Code**:

```java
// METHOD DEFINITION
public static int add(int a, int b) {
                                 // => public: accessible from anywhere
                                 // => static: belongs to class (not instance)
                                 // => int: return type (returns integer)
                                 // => add: method name
                                 // => (int a, int b): parameters (2 integers)
    return a + b;                // => Computes sum: 5 + 3 = 8
                                 // => Returns result to caller
}

// METHOD CALL
int sum = add(5, 3);             // => Calls add method with arguments 5 and 3
                                 // => add(5, 3) evaluates to 8
                                 // => sum is 8

// PASS-BY-VALUE for primitives
public static void modifyPrimitive(int x) {
                                 // => Parameter x receives COPY of argument value
    x = 100;                     // => Modifies local copy only
                                 // => Original variable unchanged
}

int num = 10;                    // => num is 10
modifyPrimitive(num);            // => Passes copy of num's value (10) to method
                                 // => Method modifies copy to 100, not original
                                 // => num still 10 (primitive pass-by-value)

// PASS-BY-VALUE for references
public static void modifyArray(int[] arr) {
                                 // => Parameter arr receives COPY of reference
                                 // => Copy points to SAME heap array as original
    arr[0] = 999;                // => Modifies heap array via reference copy
                                 // => Changes visible to caller (same object)
}

public static void reassignArray(int[] arr) {
                                 // => Parameter arr receives COPY of reference
    arr = new int[]{100, 200};   // => Reassigns local reference copy to NEW array
                                 // => Original reference unchanged (still points to old array)
}

int[] numbers = {1, 2, 3};       // => numbers references array [1, 2, 3] in heap
modifyArray(numbers);            // => Passes copy of reference to modifyArray
                                 // => Method modifies heap array via reference copy
                                 // => numbers is [999, 2, 3] (heap modified)
reassignArray(numbers);          // => Passes copy of reference to reassignArray
                                 // => Method reassigns local copy to new array
                                 // => Original reference unchanged
                                 // => numbers still [999, 2, 3] (original reference intact)

// RETURN VALUES
public static String greet(String name) {
                                 // => Method returns String object
    return "Hello, " + name;     // => Concatenates "Hello, " with parameter name
                                 // => Returns new String object to caller
}

String message = greet("Alice"); // => Calls greet("Alice")
                                 // => greet returns "Hello, Alice"
                                 // => message is "Hello, Alice"

// VOID METHODS (no return value)
public static void printMessage(String msg) {
                                 // => void: method doesn't return value
                                 // => Used for side effects only
    System.out.println(msg);     // => Side effect: prints to console
                                 // => No return statement needed
}
```

**Key Takeaway**: Java is strictly pass-by-value—primitives copy values, objects copy reference values (not the objects themselves). Modifying object contents via reference affects the original, but reassigning the reference variable does not. Methods can return values via `return` keyword or be `void` for side-effects-only methods.

**Why It Matters**: Pass-by-value semantics prevent the confusing aliasing bugs of C++'s pass-by-reference (where function parameters can unexpectedly modify caller variables). However, "pass-by-value for references" trips up beginners—object references are copied (so reassigning parameter doesn't affect caller), but the reference points to the same heap object (so modifying object contents affects caller). This design makes objects naturally shared (avoiding expensive deep copies) while preventing accidental parameter reassignment side effects. Understanding this distinction is crucial for debugging—`list.add()` modifies the shared List object, but `list = newList` only affects the local variable.

---

## Example 16: Exception Handling - Try/Catch/Finally

Exceptions handle errors gracefully without crashing programs. Java distinguishes checked exceptions (must handle or declare) from unchecked exceptions (runtime errors).

**Code**:

```java
// TRY-CATCH - handle exceptions
try {                            // => Protected code block (exceptions caught if thrown)
                                 // => Normal execution until exception occurs
    int result = 10 / 0;         // => ArithmeticException: division by zero
                                 // => Exception thrown, control jumps to catch block
    System.out.println(result);  // => Never executed (exception thrown above)
} catch (ArithmeticException e) {// => Catch specific exception type
                                 // => e parameter holds exception object
                                 // => Only executes if ArithmeticException thrown
    System.out.println("Cannot divide by zero!");
                                 // => Output: Cannot divide by zero!
}                                // => Program continues normally after catch

// MULTIPLE CATCH blocks (order matters: specific to general)
try {
    String text = null;          // => text references null (no object)
    System.out.println(text.length());
                                 // => NullPointerException (cannot call method on null)
                                 // => JVM throws NullPointerException automatically
} catch (NullPointerException e) {
                                 // => First catch: most specific exception
    System.out.println("Null reference!");
                                 // => Output: Null reference!
} catch (Exception e) {          // => Second catch: general catch-all
                                 // => Only executes if NOT NullPointerException
    System.out.println("Other error: " + e.getMessage());
                                 // => Gets error message from exception object
}

// FINALLY block (always executes, even if exception or return)
Scanner scanner = null;          // => Declare outside try (accessible in finally)
try {
    scanner = new Scanner(System.in);
                                 // => Create resource that needs cleanup
    // ... use scanner ...       // => Resource usage here
} catch (Exception e) {          // => Handle any exceptions
    System.out.println("Error: " + e);
} finally {                      // => Executes whether exception thrown or not
                                 // => Even executes if catch has return statement
    if (scanner != null) {       // => Check scanner was created
        scanner.close();         // => Cleanup resources (always runs)
                                 // => Prevents resource leaks
    }
}

// TRY-WITH-RESOURCES (Java 7+, automatic resource management)
try (Scanner s = new Scanner(System.in)) {
                                 // => Resources declared in () auto-close after try
                                 // => s must implement AutoCloseable interface
                                 // => Multiple resources: try (R1 r1 = ...; R2 r2 = ...)
    String input = s.nextLine(); // => Use resource normally
                                 // => input holds line read from console
}                                // => Scanner.close() called automatically (even if exception)
                                 // => Cleaner than manual finally block

// THROWING EXCEPTIONS
public static void checkAge(int age) throws IllegalArgumentException {
                                 // => throws declares method can throw exception
                                 // => Unchecked exception (doesn't require declaration)
                                 // => Checked exceptions MUST be declared or caught
    if (age < 0) {               // => Validate input
        throw new IllegalArgumentException("Age cannot be negative");
                                 // => throw keyword creates and throws exception
                                 // => Method execution stops immediately
    }                            // => If validation passes, method continues normally
}

try {
    checkAge(-5);                // => Calls method with invalid age
                                 // => Throws IllegalArgumentException immediately
} catch (IllegalArgumentException e) {
                                 // => Catches thrown exception
    System.out.println(e.getMessage());
                                 // => getMessage() returns "Age cannot be negative"
                                 // => Output: Age cannot be negative
}
```

**Key Takeaway**: Use try-catch blocks to handle exceptions gracefully. Catch specific exception types first, general types last. Use `finally` for cleanup code that must run regardless of exceptions. Use try-with-resources for automatic resource management of AutoCloseable objects like Scanner, streams, and database connections.

**Why It Matters**: Checked exceptions (IOException, SQLException) force explicit error handling via try-catch or throws declarations, preventing silent failures in file I/O or database operations. This design choice (unique to Java among mainstream languages) sparked decades of debate—proponents praise compile-time error handling enforcement, critics cite exception-handling boilerplate and generic `throws Exception` anti-patterns. Try-with-resources (Java 7, 2011) finally solved the resource leak epidemic from forgotten `finally { stream.close(); }` blocks, automatically closing resources even when exceptions occur. Modern languages (Go, Rust) chose explicit error returns instead of exceptions, but Java's exception model remains dominant in enterprise systems where failure scenarios must be documented and handled.

---

## Example 17: String Manipulation - Common Operations

Strings are immutable character sequences with extensive manipulation methods. String operations create new String objects rather than modifying existing ones.

**Code**:

```java
String text = "Hello, World!";   // => String literal stored in string pool
                                 // => text references immutable String object

// LENGTH and ACCESS
int len = text.length();         // => Returns number of characters (method call)
                                 // => len is 13 (counts all characters including comma and space)
char first = text.charAt(0);     // => Returns character at index 0 (zero-based)
                                 // => first is 'H' (type: char)

// SUBSTRING
String hello = text.substring(0, 5);
                                 // => Extracts characters from index 0 to 4 (end index exclusive)
                                 // => hello is "Hello" (5 characters, indices 0-4)
                                 // => Creates new String object (immutability)
String world = text.substring(7);// => Extracts from index 7 to end of string
                                 // => world is "World!" (starts at index 7, includes all remaining)
                                 // => Creates new String object

// CONCATENATION
String greeting = "Hi" + " " + "there";
                                 // => Concatenates 3 string literals using + operator
                                 // => greeting is "Hi there" (new String object created)
String concat = "Hello".concat(" World");
                                 // => concat() method appends " World" to "Hello"
                                 // => concat is "Hello World" (new String object)

// CASE CONVERSION
String upper = text.toUpperCase();
                                 // => Converts all characters to uppercase
                                 // => upper is "HELLO, WORLD!" (new String object)
                                 // => Original text unchanged (immutability)
String lower = text.toLowerCase();
                                 // => Converts all characters to lowercase
                                 // => lower is "hello, world!" (new String object)

// TRIMMING
String padded = "  text  ";      // => String with leading and trailing whitespace
                                 // => padded is "  text  " (2 spaces each side)
String trimmed = padded.trim();  // => Removes leading and trailing whitespace
                                 // => trimmed is "text" (no spaces, new String object)

// SEARCH
boolean contains = text.contains("World");
                                 // => Checks if substring "World" exists in text
                                 // => contains is true ("World" found at index 7)
boolean starts = text.startsWith("Hello");
                                 // => Checks if text starts with "Hello"
                                 // => starts is true (text begins with "Hello")
int index = text.indexOf("World");
                                 // => Returns first index of substring "World"
                                 // => index is 7 (position where "World" starts)
                                 // => Returns -1 if substring not found

// REPLACEMENT
String replaced = text.replace("World", "Java");
                                 // => Replaces all occurrences of "World" with "Java"
                                 // => replaced is "Hello, Java!" (new String object)
                                 // => Original text unchanged (immutability)

// SPLITTING
String csv = "apple,banana,cherry";
                                 // => Comma-separated values string
String[] fruits = csv.split(",");// => Splits string by comma delimiter
                                 // => fruits is ["apple", "banana", "cherry"] (String array)
                                 // => Array of 3 elements (split creates new String objects)

// IMMUTABILITY
String original = "Java";        // => original references String "Java"
original.toUpperCase();          // => Creates new String "JAVA", returns it
                                 // => But return value NOT assigned to anything (discarded)
                                 // => original UNCHANGED (toUpperCase doesn't modify original)
System.out.println(original);    // => Output: Java (lowercase, unchanged)
                                 // => Demonstrates immutability: operations return new Strings

String modified = original.toUpperCase();
                                 // => toUpperCase() returns new String "JAVA"
                                 // => Assigned to modified variable (captures result)
                                 // => modified is "JAVA", original still "Java"
System.out.println(modified);    // => Output: JAVA (uppercase, new String)
                                 // => Must assign result to use modified value
```

**Key Takeaway**: Strings are immutable—all manipulation methods return new String objects rather than modifying originals. This prevents accidental modifications but requires assigning results to variables. Use `+` or `concat()` for simple concatenation, `StringBuilder` for loops or repeated modifications.

**Why It Matters**: String immutability enables the string pool (literal strings share memory, reducing heap usage), thread safety (immutable objects are inherently thread-safe), and security (strings can't be modified after security checks). However, naive string concatenation in loops (`str += "x"`) creates O(n²) complexity as each concatenation allocates a new string—for 1000 iterations, this creates 1000 temporary string objects. StringBuilder solves this with mutable character buffers, providing O(n) amortized append performance. Modern Java's string deduplication (G1 garbage collector) and compact strings (Java 9, using byte[] instead of char[] for Latin-1 strings) further optimize string memory usage, critical since strings consume 25%+ of heap in typical applications.

---

## Example 18: StringBuilder - Efficient String Construction

StringBuilder provides mutable string buffers for efficient string construction in loops or repeated modifications. Unlike String concatenation, StringBuilder modifies internal buffer instead of creating new objects.

**Code**:

```java
// STRING CONCATENATION (inefficient in loops)
String result = "";
for (int i = 0; i < 1000; i++) {
    result += i + " ";           // => Creates 1000 temporary String objects (slow!)
}

// STRINGBUILDER (efficient)
StringBuilder sb = new StringBuilder();
                                 // => Mutable character buffer (default capacity 16 chars)
                                 // => Internal char[] array grows when capacity exceeded
for (int i = 0; i < 1000; i++) {
    sb.append(i).append(" ");    // => append() modifies internal buffer in-place
                                 // => First append adds number, second adds space
                                 // => Method chaining: append() returns this for fluent API
                                 // => No temporary String objects created (O(n) vs O(n²))
}                                // => Loop completes with all 1000 numbers in buffer
String efficient = sb.toString();// => Convert final buffer to immutable String
                                 // => Creates single String from accumulated characters
                                 // => efficient is "0 1 2 ... 999 " (1000 numbers + spaces)

// COMMON StringBuilder OPERATIONS
StringBuilder builder = new StringBuilder("Hello");
builder.append(" World");        // => builder contains "Hello World"
builder.insert(5, ",");          // => builder contains "Hello, World"
builder.replace(7, 12, "Java");  // => builder contains "Hello, Java"
builder.delete(5, 6);            // => builder contains "Hello Java"
builder.reverse();               // => builder contains "avaJ olleH"

String final = builder.toString();
                                 // => final is "avaJ olleH"

// INITIAL CAPACITY (performance optimization)
StringBuilder sized = new StringBuilder(1000);
                                 // => Pre-allocate capacity to avoid resizing
// => Default capacity (16) doubles when full (expensive array copy)
// => Pre-sizing avoids resizing overhead for known large strings
```

**Key Takeaway**: Use StringBuilder for string construction in loops, repeated modifications, or when building large strings. It provides mutable buffer avoiding the O(n²) overhead of repeated String concatenation. Convert to String via `toString()` when construction is complete.

**Why It Matters**: StringBuilder's mutable design prevents the exponential object allocation of naive string concatenation—concatenating 10,000 strings with `+` creates 10,000 temporary String objects and copies characters repeatedly (O(n²) complexity). StringBuilder's internal `char[]` buffer grows exponentially (doubling when full), providing amortized O(1) append and O(n) total complexity. This performance difference is dramatic: concatenating 100,000 strings takes milliseconds with StringBuilder vs seconds with `+`. Java compilers optimize trivial cases (`"a" + "b" + "c"` → single String), but cannot optimize loop concatenation, making StringBuilder essential for string-intensive code. StringBuffer (thread-safe variant) predates StringBuilder but incurs synchronization overhead—use StringBuilder unless thread safety is required.

---

## Example 19: Generics - Type-Safe Collections

Generics enable type-safe collections and methods by parameterizing types. They provide compile-time type checking, eliminating ClassCastException errors at runtime.

**Code**:

```java
import java.util.*;

// GENERIC COLLECTIONS
ArrayList<String> strings = new ArrayList<>();
                                 // => <String> specifies type
                                 // => Diamond <> infers type (Java 7+)
strings.add("hello");            // => Type-safe at compile time
strings.add("world");
// strings.add(42);              // => Compile error: wrong type

String first = strings.get(0);   // => No cast needed
                                 // => Compiler knows type

// PRE-GENERICS (Java 1.4)
ArrayList rawList = new ArrayList();
                                 // => Raw type (no generics)
                                 // => Stores Object, no type safety
rawList.add("text");
rawList.add(123);                // => Accepts any type
String str = (String) rawList.get(0);
                                 // => Requires cast
                                 // => ClassCastException risk
// Integer fail = (Integer) rawList.get(0); // => Runtime error!

// GENERIC METHODS
public static <T> void printArray(T[] array) {
                                 // => <T> method-level type parameter
                                 // => Works with any reference type
    for (T element : array) {
        System.out.print(element + " ");
    }
    System.out.println();
}

Integer[] numbers = {1, 2, 3};
String[] words = {"hello", "world"};
printArray(numbers);             // => T inferred as Integer
                                 // => Output: 1 2 3
printArray(words);               // => T inferred as String
                                 // => Output: hello world

// BOUNDED TYPE PARAMETERS
public static <T extends Number> double sum(List<T> list) {
                                 // => T must extend Number
                                 // => Enables Number methods
    double total = 0;
    for (T num : list) {
        total += num.doubleValue();
                                 // => Can call Number methods
    }
    return total;                // => Return accumulated sum as double
}

List<Integer> ints = Arrays.asList(1, 2, 3);
                                 // => Arrays.asList() creates immutable List<Integer>
double result = sum(ints);       // => T inferred as Integer (fits Number bound)
                                 // => Calls sum<Integer>(List<Integer>)
                                 // => 1.doubleValue() + 2.doubleValue() + 3.doubleValue()
                                 // => result is 6.0

// WILDCARD TYPES
public static void printList(List<?> list) {
                                 // => ? is wildcard (unknown type)
                                 // => Unbounded wildcard: accepts List of any type
                                 // => Can accept List<String>, List<Integer>, etc.
                                 // => Cannot add elements (type unknown, type safety)
                                 // => Can only read as Object
    for (Object elem : list) {   // => Elements treated as Object (unknown type)
                                 // => elem type is Object (greatest common type)
                                 // => Cannot cast to specific type (unknown at compile time)
        System.out.print(elem + " ");
                                 // => elem.toString() called (Object method)
    }
}

List<String> names = Arrays.asList("Alice", "Bob");
List<Integer> nums = Arrays.asList(1, 2, 3);
printList(names);                // => ? resolves to String at runtime
                                 // => Output: Alice Bob
printList(nums);                 // => ? resolves to Integer at runtime
                                 // => Output: 1 2 3
                                 // => Wildcard enables method to accept any List type
```

**Key Takeaway**: Generics provide compile-time type safety for collections and methods, eliminating runtime ClassCastException errors. Use `<T>` for type parameters in generic classes/methods. Use bounded types (`<T extends Class>`) to restrict acceptable types. Use wildcards (`<?>`) for flexible method parameters accepting any generic type.

**Why It Matters**: Pre-generics Java (before 1.5, 2004) required unsafe casts and stored everything as Object, causing thousands of production ClassCastException bugs when wrong types were retrieved. Generics enabled the Collections Framework to provide type-safe APIs without code duplication—one ArrayList implementation works for all types. The compiler uses type erasure (removing generic information at runtime) for backward compatibility with pre-generics bytecode, but this creates limitations: cannot create `new T[]` arrays, cannot use primitives as type parameters (`List<int>` illegal, must use `List<Integer>`), and cannot detect type at runtime (`list instanceof List<String>` illegal). Despite these quirks, generics are essential for modern Java—streams, Optional, CompletableFuture all depend on generics for type safety.

---

## Example 20: Varargs - Variable-Length Arguments

Varargs allows methods to accept variable numbers of arguments using `...` syntax. Arguments are treated as arrays inside the method.

**Code**:

```java
// VARARGS METHOD
public static int sum(int... numbers) {
                                 // => int... allows 0 or more int arguments
                                 // => Compiler converts to int[] array internally
                                 // => numbers parameter is int[] inside method body
    int total = 0;               // => Accumulator initialized to 0
    for (int num : numbers) {    // => Iterate over varargs array
                                 // => num takes each element value
        total += num;            // => Add num to running total
    }
    return total;                // => Return accumulated sum
}

// CALLING with different argument counts
int result1 = sum();             // => Called with 0 arguments
                                 // => numbers is empty array int[0]
                                 // => Loop doesn't execute (empty array)
                                 // => result1 is 0 (no additions)
int result2 = sum(5);            // => Called with 1 argument
                                 // => numbers is int[]{5}
                                 // => Loop executes once: total = 0 + 5
                                 // => result2 is 5
int result3 = sum(1, 2, 3, 4);   // => Called with 4 arguments
                                 // => numbers is int[]{1, 2, 3, 4}
                                 // => Loop: total = 0 + 1 + 2 + 3 + 4
                                 // => result3 is 10

// VARARGS with regular parameters
public static String format(String template, Object... args) {
                                 // => Regular parameter (template) MUST come first
                                 // => Varargs parameter (args) MUST be last
                                 // => args is Object[] inside method
    return String.format(template, args);
                                 // => Passes template and args array to String.format
                                 // => String.format replaces %s, %d with args elements
}

String msg = format("Hello %s, you have %d messages", "Alice", 5);
                                 // => template = "Hello %s, you have %d messages"
                                 // => args = new Object[]{"Alice", 5}
                                 // => String.format replaces %s with "Alice", %d with 5
                                 // => msg is "Hello Alice, you have 5 messages"

// VARARGS vs ARRAY parameter
public static void printArray(int[] array) {
                                 // => Requires explicit int[] array parameter
                                 // => Caller must create array manually
    for (int n : array) {        // => Iterate array elements
        System.out.print(n + " ");
                                 // => Output: 1 2 3
    }
}

printArray(new int[]{1, 2, 3});  // => Must create array explicitly with new int[]{}
                                 // => Verbose syntax (array literal required)

// Varargs equivalent (cleaner call syntax)
public static void printVarargs(int... numbers) {
                                 // => Varargs allows comma-separated arguments
                                 // => Compiler creates array automatically
    for (int n : numbers) {      // => numbers is int[] internally
        System.out.print(n + " ");
                                 // => Output: 1 2 3
    }
}

printVarargs(1, 2, 3);           // => No explicit array creation needed
                                 // => Compiler creates int[]{1, 2, 3} automatically
                                 // => Cleaner syntax for callers

// VARARGS RULES
// => 1. Only one varargs parameter allowed per method
// => 2. Varargs parameter MUST be LAST in parameter list
// public static void invalid(int... a, String s) {}  // => ERROR: varargs not last (compilation error)
```

**Key Takeaway**: Varargs (`Type... varName`) allows methods to accept variable numbers of arguments, treating them as arrays internally. Varargs must be the last parameter in the parameter list. Use varargs for flexible APIs (printf-style formatting, builders, utility methods) but prefer explicit arrays for performance-critical code to avoid hidden array allocation.

**Why It Matters**: Varargs eliminated the need for overloaded methods with different argument counts (`print(int a)`, `print(int a, int b)`, etc.) that plagued pre-Java 5 APIs. Methods like `String.format()`, `Arrays.asList()`, and logging frameworks depend on varargs for flexible argument lists. However, each varargs call creates a new array object (heap allocation), making it unsuitable for tight loops or performance-critical paths—hotspot compiler cannot always optimize away the allocation. The `@SafeVarargs` annotation (Java 7+) suppresses generic array creation warnings for varargs methods, necessary because varargs with generics `<T>` creates unchecked array allocation (due to type erasure) that the compiler warns about.

---

## Example 21: Autoboxing and Wrapper Classes

Java provides wrapper classes (Integer, Double, Boolean, etc.) to treat primitives as objects. Autoboxing automatically converts primitives to wrappers and vice versa.

**Code**:

```java
// WRAPPER CLASSES (manual boxing/unboxing)
int primitive = 42;              // => Primitive int value 42
Integer wrapped = Integer.valueOf(primitive);
                                 // => Explicit boxing: converts int 42 to Integer object
                                 // => wrapped references Integer object holding 42
int unwrapped = wrapped.intValue();
                                 // => Explicit unboxing: extracts int from Integer object
                                 // => unwrapped is 42 (primitive int)

// AUTOBOXING (Java 5+, automatic conversion)
Integer auto = 42;               // => Automatic int → Integer (compiler inserts valueOf)
                                 // => Equivalent to: Integer auto = Integer.valueOf(42)
                                 // => auto references Integer object holding 42
int primitiveAuto = auto;        // => Automatic Integer → int (compiler inserts intValue)
                                 // => Equivalent to: int primitiveAuto = auto.intValue()
                                 // => primitiveAuto is 42 (primitive int)

// COLLECTIONS require objects (cannot use primitives)
ArrayList<Integer> numbers = new ArrayList<>();
                                 // => ArrayList<int> is ILLEGAL (generics require objects)
                                 // => Must use wrapper class Integer
numbers.add(10);                 // => Autoboxing: 10 (int) → Integer.valueOf(10)
                                 // => Adds Integer object to list
numbers.add(20);                 // => Autoboxing: 20 → Integer.valueOf(20)
                                 // => numbers is [10, 20] (Integer objects)
int first = numbers.get(0);      // => Returns Integer object (value 10)
                                 // => Auto-unboxing: Integer.intValue() → 10 (int)
                                 // => first is 10 (primitive int)

// UTILITIES (wrapper class static methods)
String numberStr = "123";        // => String representation of number
int parsed = Integer.parseInt(numberStr);
                                 // => Parses String to primitive int
                                 // => parsed is 123 (int, not Integer)
Integer parsedObj = Integer.valueOf(numberStr);
                                 // => Parses String to Integer object
                                 // => parsedObj references Integer(123)

String binary = Integer.toBinaryString(42);
                                 // => Converts int to binary string representation
                                 // => binary is "101010" (42 in base-2)
int max = Integer.MAX_VALUE;     // => Constant: maximum int value
                                 // => max is 2147483647 (2^31 - 1)
int min = Integer.MIN_VALUE;     // => Constant: minimum int value
                                 // => min is -2147483648 (-2^31)

// NULL POINTER RISK (auto-unboxing danger)
Integer nullValue = null;        // => nullValue references null (no Integer object)
// int danger = nullValue;       // => Auto-unboxing attempts nullValue.intValue()
                                 // => NullPointerException! (cannot call method on null)
if (nullValue != null) {         // => Check for null BEFORE auto-unboxing
    int safe = nullValue;        // => Safe: only executes if nullValue not null
                                 // => Auto-unboxing: nullValue.intValue()
}

// WRAPPER CACHING (Integer.valueOf caches -128 to 127)
Integer a = 127;                 // => Autoboxing: Integer.valueOf(127)
                                 // => valueOf returns CACHED Integer instance for 127
Integer b = 127;                 // => Autoboxing: Integer.valueOf(127)
                                 // => valueOf returns SAME cached instance as a
System.out.println(a == b);      // => Compares references (memory addresses)
                                 // => true (a and b reference same cached object)

Integer c = 128;                 // => Autoboxing: Integer.valueOf(128)
                                 // => valueOf creates NEW Integer instance (128 > 127, not cached)
Integer d = 128;                 // => Autoboxing: Integer.valueOf(128)
                                 // => valueOf creates ANOTHER new Integer instance
System.out.println(c == d);      // => Compares references
                                 // => false (c and d reference different objects)
System.out.println(c.equals(d)); // => Compares values using equals()
                                 // => true (both hold value 128, value equality)
```

**Key Takeaway**: Wrapper classes (Integer, Double, Boolean) enable primitives to be used where objects are required (collections, generics). Autoboxing automatically converts primitives to wrappers and vice versa. Always use `equals()` for wrapper comparison, NOT `==` (except for cached values -128 to 127). Check for null before auto-unboxing to avoid NullPointerException.

**Why It Matters**: Autoboxing (Java 5, 2004) eliminated the tedious `Integer.valueOf()` and `intValue()` boilerplate that made pre-generics collections painful (`list.add(new Integer(5))`). However, automatic conversion hides performance costs—each autoboxing allocates a heap object, making loops like `Integer sum = 0; for (...) sum += i;` allocate millions of temporary Integer objects. The -128 to 127 cache optimization (valueOf returns cached instances) prevents some allocations but creates the `==` comparison trap where `Integer a = 127; Integer b = 127; a == b` is true (cached) but `Integer c = 128; Integer d = 128; c == d` is false (different objects). This is Java's most counterintuitive behavior, fixed by always using `equals()` for object comparison.

---

## Example 22: Static Members and Initialization

Static members belong to the class rather than instances. They're shared across all objects and accessible without creating instances. Static blocks initialize static fields.

**Code**:

```java
public class Counter {           // => Public class (accessible from any package)
    // STATIC FIELD - shared across all Counter objects
    private static int totalCount = 0;
                                 // => static keyword: one copy shared by ALL instances
                                 // => Class-level field, not instance-level
                                 // => Initialized to 0 when Counter class loaded
                                 // => All Counter objects share this single totalCount variable
    // INSTANCE FIELD - each object has its own copy
    private int instanceCount = 0;
                                 // => NO static: each Counter object has separate instanceCount
                                 // => Object-level field (instance-specific state)
                                 // => Each new Counter() creates new instanceCount field

    // CONSTRUCTOR
    public Counter() {           // => Constructor called when new Counter() executed
                                 // => Runs each time object created
        totalCount++;            // => Increment shared static field (class-level)
                                 // => Same totalCount modified by all instances
                                 // => Tracks total number of Counter objects created
        instanceCount++;         // => Increment instance field (object-level)
                                 // => Each object increments its own instanceCount
                                 // => Always 1 for new objects (starts at 0, then++)
    }

    // STATIC METHOD - can be called without creating object
    public static int getTotalCount() {
                                 // => static method: belongs to class, not instance
                                 // => Called via Counter.getTotalCount() (class name, no object)
                                 // => NO this reference (no specific object context)
        return totalCount;       // => Can access static fields
                                 // => Returns shared totalCount value
        // return instanceCount; // => ERROR: cannot access instance field from static method
                                 // => Static methods have no this reference
                                 // => instanceCount requires specific object (no object here)
    }

    // INSTANCE METHOD - requires object
    public int getInstanceCount() {
                                 // => Instance method: requires Counter object
                                 // => Called via c1.getInstanceCount() (object reference)
                                 // => Has this reference (current object context)
        return instanceCount;    // => Can access both instance fields (instanceCount)
                                 // => And static fields (totalCount)
                                 // => Returns this object's instanceCount value
    }

    // STATIC INITIALIZATION BLOCK - runs once when class loaded
    static {                     // => static block: executes when class loaded by JVM
                                 // => Runs ONCE per class (not per object)
                                 // => Runs before any Counter object created
                                 // => Runs before static method calls
        System.out.println("Counter class loaded");
                                 // => Output: Counter class loaded (printed once)
        totalCount = 0;          // => Initialize static field totalCount to 0
                                 // => Can perform complex initialization here
        // Complex initialization logic here
                                 // => Loading config, initializing caches, etc.
    }
}

// USAGE
Counter c1 = new Counter();      // => First Counter object created
                                 // => Triggers class loading (static block runs)
                                 // => Output: Counter class loaded (static block executes ONCE)
                                 // => Constructor runs: totalCount becomes 1, c1.instanceCount becomes 1
Counter c2 = new Counter();      // => Second Counter object created
                                 // => Static block does NOT run again (class already loaded)
                                 // => Constructor runs: totalCount becomes 2, c2.instanceCount becomes 1
Counter c3 = new Counter();      // => Third Counter object created
                                 // => Constructor runs: totalCount becomes 3, c3.instanceCount becomes 1

System.out.println(Counter.getTotalCount());
                                 // => Call static method via class name Counter
                                 // => No object needed (static method)
                                 // => Returns totalCount (3)
                                 // => Output: 3 (total objects created)
System.out.println(c1.getInstanceCount());
                                 // => Call instance method on c1 object
                                 // => Returns c1's instanceCount field
                                 // => Output: 1 (each object has instanceCount=1)

// STATIC IMPORT
import static java.lang.Math.PI;
                                 // => Import static field PI from Math class
                                 // => Allows using PI without Math. prefix
import static java.lang.Math.sqrt;
                                 // => Import static method sqrt from Math class
                                 // => Allows calling sqrt() directly

double area = PI * sqrt(25);     // => Use imported static members directly
                                 // => PI is Math.PI (3.14159...)
                                 // => sqrt(25) is Math.sqrt(25) = 5.0
                                 // => area is π * 5 = 15.707... (calculation result)
```

**Key Takeaway**: Static members belong to the class, not instances—they're shared across all objects. Static methods can only access static fields (no `this` reference). Static blocks initialize static fields when class loads. Use static for utility methods (Math.sqrt), constants (Math.PI), and shared state (counters, caches).

**Why It Matters**: Static members provide class-level state and behavior without requiring object instantiation, essential for utility classes (Math, Collections, Arrays) and singleton patterns. However, static fields are effectively global variables, creating testing difficulties (cannot easily mock or reset) and thread-safety challenges (shared mutable state requires synchronization). The static initialization block runs exactly once when the class is first loaded, useful for expensive initialization (loading config files, initializing database pools) but creates class-loading side effects that can surprise developers. Modern Java favors dependency injection over static methods for better testability, though static utilities remain ubiquitous for pure functions without state.

---

## Example 23: Access Modifiers and Encapsulation

Access modifiers control visibility of classes, fields, and methods. Encapsulation hides implementation details, exposing only public API while keeping internals private.

**Code**:

```java
// ACCESS MODIFIERS: public, private, protected, package-private (default)

public class BankAccount {       // => public: accessible anywhere
    // PRIVATE fields
    private String accountNumber; // => Only accessible within class
                                 // => External code cannot read/write directly
    private double balance;       // => Encapsulation hides state
                                 // => Prevents direct manipulation

    // PUBLIC constructor
    public BankAccount(String accountNumber, double initialBalance) {  // => Public constructor
        this.accountNumber = accountNumber;                            // => Sets account number
        this.balance = initialBalance;                                 // => Sets initial balance
                                 // => Controlled initialization
    }

    // PUBLIC methods
    public void deposit(double amount) {  // => Public API for depositing
        if (amount > 0) {                 // => Validation: positive amounts only
            balance += amount;            // => Validation logic, updates balance
        }                                 // => Rejects negative/zero amounts
    }

    public boolean withdraw(double amount) {  // => Public API for withdrawing
        if (amount > 0 && balance >= amount) {  // => Validates positive amount and sufficient balance
            balance -= amount;                  // => Deducts from balance
            return true;                        // => Success, returns true
        }
        return false;                           // => Failure (insufficient funds or invalid amount)
    }

    public double getBalance() {   // => Read-only access to balance
        return balance;            // => Returns current balance
                                   // => No setter, controlled modification
    }

    // PRIVATE helper
    private void logTransaction(String type, double amount) {  // => Private helper method
                                 // => Internal only, not accessible outside class
        System.out.println(type + ": $" + amount + ", Balance: $" + balance);  // => Logs transaction
    }
}

// PACKAGE-PRIVATE
class PackageHelper {              // => No public: same package only
                                   // => Only visible within package
    void helperMethod() {          // => Package-private method (no modifier)
                                   // => Accessible within package only
    }
}

// PROTECTED
class Animal {                     // => Base class
    protected String species;      // => Subclasses + same package
                                   // => Accessible in subclasses and package

    protected void makeSound() {   // => Subclasses can override
        System.out.println("Generic sound");  // => Output: Generic sound
    }
}

class Dog extends Animal {         // => Subclass of Animal
    public void bark() {           // => Public method
        species = "Canine";        // => Access protected field from superclass
                                   // => species = "Canine"
        makeSound();               // => Call protected method from superclass
                                   // => Output: Generic sound
    }
}

// USAGE
BankAccount account = new BankAccount("12345", 1000);  // => Creates account, balance = 1000
account.deposit(500);              // => Public method accessible, balance = 1500
                                   // => Deposits 500
double bal = account.getBalance(); // => bal = 1500
                                   // => bal is 1500
// account.balance = 0;            // => ERROR: balance is private (cannot access directly)
// account.logTransaction();       // => ERROR: logTransaction is private
```

**Key Takeaway**: Use `private` for fields to hide implementation (encapsulation), `public` for API methods that clients should use, `protected` for inheritance-accessible members, and package-private (no modifier) for package-internal helpers. Encapsulation prevents direct field access, enforcing validation through methods.

**Why It Matters**: Encapsulation is Java's enforcement of information hiding—making fields private prevents clients from creating invalid state (negative balances, null required fields). Public getter/setter methods (JavaBeans pattern) enable validation and future implementation changes without breaking clients. This design enables evolution: changing `balance` from double to BigDecimal only affects BankAccount internals, not clients calling public methods. However, naive getters/setters that just expose fields without validation (anemic domain model anti-pattern) provide no value over public fields. Modern Java records (Java 14+) eliminate getter boilerplate for immutable data classes, though mutable state still benefits from encapsulation.

---

## Example 24: Packages and Imports

Packages organize classes into namespaces, preventing name collisions. Import statements make classes from other packages accessible without fully qualified names.

**Code**:

```java
// PACKAGE DECLARATION (must be first statement, before imports)
package com.example.myapp;       // => This file belongs to com.example.myapp package
                                 // => File must be in com/example/myapp/ directory

// IMPORT STATEMENTS (after package, before class)
import java.util.ArrayList;      // => Import specific class (ArrayList)
import java.util.HashMap;        // => Import HashMap from java.util
import java.util.List;           // => Import interface (List is interface, ArrayList implements it)

import java.time.*;              // => Import all classes from package (wildcard)
                                 // => Imports LocalDate, LocalTime, LocalDateTime, etc.

import static java.lang.Math.PI; // => Static import (import static field)
                                 // => Allows using PI without Math. prefix
import static java.lang.Math.sqrt;  // => Static import for sqrt method
                                 // => Use PI and sqrt directly (no Math. prefix)

// AUTO-IMPORTED: java.lang.* (String, System, Integer, etc.)
                                 // => java.lang automatically imported (no explicit import needed)

public class App {
    public static void main(String[] args) {
        // Use imported classes without fully qualified names
        ArrayList<String> list = new ArrayList<>();
                                 // => ArrayList from java.util (imported)
        LocalDate date = LocalDate.now();
                                 // => LocalDate from java.time.* (wildcard import)

        double area = PI * sqrt(25);
                                 // => PI and sqrt from static import

        // Fully qualified name (no import needed)
        java.util.Scanner scanner = new java.util.Scanner(System.in);
                                 // => Fully qualified: package.ClassName
                                 // => Avoids import statement (verbose but explicit)
    }
}

// PACKAGE CONVENTIONS
// com.company.project.module - reverse domain name notation
// com.example.myapp.model    - models/entities
// com.example.myapp.service  - business logic
// com.example.myapp.util     - utility classes
// => Organizing structure: domain / project / layer / class

// NAME COLLISION resolution
import java.util.Date;
import java.sql.Date;            // => ERROR: Date conflicts (both java.util.Date and java.sql.Date)

// Solution: use fully qualified name for one
import java.util.Date;
// ... then use java.sql.Date for SQL dates explicitly
java.sql.Date sqlDate = new java.sql.Date(System.currentTimeMillis());
```

**Key Takeaway**: Packages organize classes into namespaces using reverse domain notation (`com.company.project`). Import statements make classes accessible without fully qualified names. Use wildcard imports (`import java.util.*`) for multiple classes from same package. Resolve name collisions by using fully qualified names for conflicting classes.

**Why It Matters**: Packages prevent the namespace pollution that plagued C/C++ where all names share a global namespace, causing conflicts when libraries define identically named classes. Java's package system enables modular development—teams can work on separate packages without coordination. The reverse domain name convention (`com.yourcompany.project`) guarantees globally unique package names, preventing conflicts when combining third-party libraries. However, wildcard imports (`import java.util.*`) are controversial—they hide which classes are used (reducing IDE navigation) but eliminate import list maintenance. Modern IDEs auto-optimize imports, making this debate moot. Java 9's module system (Project Jigsaw) added a layer above packages for stronger encapsulation and explicit dependencies, though adoption remains limited outside large frameworks.

---

## Example 25: Enums - Type-Safe Constants

Enums define fixed sets of named constants with type safety. Unlike integer constants, enums provide compile-time safety, namespacing, and can have fields and methods.

**Code**:

```java
// BASIC ENUM
public enum Day {                // => Enum type declaration
    MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY
}                                // => Each name is an enum constant (public static final)
                                 // => Constants implicitly numbered (MONDAY=0, TUESDAY=1, etc.)
                                 // => Type-safe: cannot use int where Day expected

// USAGE
Day today = Day.MONDAY;          // => Type-safe constant assignment
                                 // => today can only be Day values, not arbitrary ints
System.out.println(today);       // => Output: MONDAY (toString() returns name)
                                 // => Automatic name() and toString() methods

// SWITCH with enums
switch (today) {                 // => Switch on enum type
                                 // => Compile-time exhaustiveness checking possible
    case MONDAY:                 // => No Day.MONDAY needed (type inferred)
        System.out.println("Start of week");
                                 // => Output: Start of week
        break;                   // => Exit switch
    case FRIDAY:
        System.out.println("TGIF!");
                                 // => Output: TGIF!
        break;
    default:
        System.out.println("Midweek");
                                 // => Handles all other days
}

// ENUM with FIELDS and METHODS
public enum Planet {             // => Enum with state and behavior
    MERCURY(3.303e+23, 2.4397e6),// => Constructor call with mass and radius
    VENUS(4.869e+24, 6.0518e6),  // => Each constant has associated data
    EARTH(5.976e+24, 6.37814e6); // => Semicolon required when adding members

    private final double mass;   // => Enum can have fields (instance variables)
                                 // => final: constants are immutable
    private final double radius; // => Each enum constant has own mass/radius

    Planet(double mass, double radius) {
                                 // => Enum constructor (implicitly private)
                                 // => Cannot instantiate enum externally: new Planet(...) ← compile error
        this.mass = mass;        // => Initialize mass field
        this.radius = radius;    // => Initialize radius field
    }                            // => Constructor called once per constant

    public double mass() {       // => Enum can have methods (accessor)
                                 // => Returns mass field
        return mass;             // => Returns this constant's mass value
    }

    public double surfaceGravity() {
                                 // => Calculated method (computation from fields)
        final double G = 6.67300E-11;
                                 // => Gravitational constant
        return G * mass / (radius * radius);
                                 // => Returns surface gravity in m/s²
    }
}

// USAGE
Planet earth = Planet.EARTH;     // => References EARTH constant
                                 // => earth.mass() returns 5.976e+24 kg
double gravity = earth.surfaceGravity();
                                 // => Calls method on enum constant
                                 // => gravity is 9.802... m/s² (Earth's surface gravity)

// ENUM METHODS
Day[] allDays = Day.values();    // => Returns array of all enum constants
                                 // => allDays is [MONDAY, TUESDAY, ..., SUNDAY]
Day parsed = Day.valueOf("MONDAY");
                                 // => parsed is Day.MONDAY (String → enum)
int ordinal = Day.MONDAY.ordinal();
                                 // => ordinal is 0 (index in declaration order)

// ITERATING enums
for (Day day : Day.values()) {
    System.out.println(day);     // => Output: MONDAY, TUESDAY, ..., SUNDAY
}
```

**Key Takeaway**: Enums define type-safe constant sets preventing invalid values. Each enum constant is a singleton instance. Enums can have fields, constructors, and methods, making them more powerful than simple integer constants. Use `values()` to iterate all constants, `valueOf()` to parse strings, and `ordinal()` for declaration order.

**Why It Matters**: Pre-enum Java (before 1.5, 2004) used integer constants (`public static final int MONDAY = 0;`) which allowed invalid values (`day = 99`), lacked type safety (`int day = MONTH.JANUARY` compiles!), and had no namespacing (`NORTH` conflicts across enums). Enums fix all these issues—only valid constants allowed, compile-time type checking, and automatic namespacing. Enum constants are singleton instances created once during class loading, enabling safe `==` comparison (`day1 == day2` works, unlike String comparison). The ability to add fields/methods makes enums mini-classes—Joshua Bloch's Effective Java popularized the "enum singleton" pattern as the safest singleton implementation (serialization-safe, reflection-proof).

---

## Example 26: File I/O - Reading and Writing Files

Java provides multiple APIs for file operations. Modern NIO.2 (java.nio.file) offers simpler, more powerful file I/O than legacy java.io classes.

**Code**:

```java
import java.nio.file.*;                       // => Modern file I/O API (Java 7+)
import java.io.IOException;                   // => Checked exception for I/O errors
import java.util.List;                        // => List collection interface

// WRITE to file
String content = "Hello, File I/O!";          // => content = "Hello, File I/O!"
Path path = Paths.get("output.txt");          // => path = output.txt (relative)
                                               // => Relative path from current directory
try {                                          // => Exception handling required
    Files.writeString(path, content);          // => Creates or overwrites output.txt
                                               // => Writes content to file
    System.out.println("File written successfully");  // => Output: File written successfully
} catch (IOException e) {                      // => Checked exception must be caught
    System.out.println("Error writing file: " + e.getMessage());  // => Error handling
}

// READ from file
try {                                          // => Try block for reading
    String fileContent = Files.readString(path);  // => Reads entire file into String
                                               // => fileContent = "Hello, File I/O!"
    System.out.println(fileContent);           // => Output: Hello, File I/O!
                                               // => Output: Hello, File I/O!
} catch (IOException e) {                      // => Catches read errors
    System.out.println("Error reading file: " + e.getMessage());  // => Error message
}

// READ lines as List
try {                                          // => Try block for line reading
    List<String> lines = Files.readAllLines(path);  // => List of lines
                                               // => lines = ["Hello, File I/O!"]
    for (String line : lines) {                // => Iterate each line
        System.out.println(line);              // => Output: Hello, File I/O!
    }
} catch (IOException e) {                      // => Catches read exceptions
    e.printStackTrace();                       // => Full stack trace to stderr
}

// APPEND to file
String moreContent = "\nAppended line";        // => moreContent = "\nAppended line"
try {                                          // => Try block for appending
    Files.writeString(path, moreContent, StandardOpenOption.APPEND);  // => Appends to file
                                               // => File now: "Hello, File I/O!\nAppended line"
} catch (IOException e) {                      // => Catches append errors
    e.printStackTrace();                       // => Prints error stack
}

// CHECK file properties
boolean exists = Files.exists(path);           // => exists = true (file created above)
                                               // => true if file exists
boolean isFile = Files.isRegularFile(path);    // => isFile = true (not directory)
                                               // => true (not directory)
long size = Files.size(path);                  // => size = 31 bytes (example)
                                               // => Size in bytes

// DELETE file
try {                                          // => Try block for deletion
    Files.delete(path);                        // => Deletes output.txt
                                               // => Throws if doesn't exist
    // Files.deleteIfExists(path);             // => No exception variant
} catch (IOException e) {                      // => Catches deletion errors
    e.printStackTrace();                       // => Prints error if deletion fails
}

// CREATING directories
Path dir = Paths.get("my/nested/directory");   // => dir = my/nested/directory
try {                                          // => Try block for directory creation
    Files.createDirectories(dir);              // => Creates all parent directories
                                               // => Creates "my", then "my/nested", then "my/nested/directory"
                                               // => No error if directories already exist
} catch (IOException e) {                      // => Catches directory creation errors
    e.printStackTrace();                       // => Prints error if creation fails
}
```

**Key Takeaway**: Use java.nio.file (NIO.2) for modern file I/O—`Files.readString()` and `Files.writeString()` for simple text files, `Files.readAllLines()` for line-by-line processing. All file operations throw checked IOException requiring try-catch. Use `Paths.get()` to create Path objects representing file locations.

**Why It Matters**: NIO.2 (Java 7, 2011) finally provided a modern file API after two decades of clunky java.io streams. The old approach required verbose FileInputStream/FileOutputStream/BufferedReader boilerplate with manual resource management (forgetting `close()` caused file descriptor leaks). NIO.2's utility methods (`Files.readString()`, added Java 11) reduce this to one-liners, while try-with-resources ensures automatic closing. Path abstraction works across filesystems (local disk, network shares, ZIP files) transparently. However, `readString()` and `readAllLines()` load entire files into memory—unsuitable for large files (gigabytes) where streaming with `Files.lines()` (returns Stream<String>) is necessary. The checked IOException enforcement is controversial but prevents silent file operation failures common in languages without checked exceptions.

---

## Example 27: Lambda Expressions and Functional Interfaces

Lambda expressions provide concise syntax for anonymous functions, enabling functional programming patterns. Functional interfaces (interfaces with one abstract method) can be implemented with lambdas.

**Code**:

```java
import java.util.*;
import java.util.function.*;

// FUNCTIONAL INTERFACE - interface with single abstract method
@FunctionalInterface
interface Calculator {
    int calculate(int a, int b);
}

// LAMBDA EXPRESSION - implements functional interface
Calculator add = (a, b) -> a + b;
                                 // => (parameters) -> expression (lambda syntax)
                                 // => Compiler infers parameter types (int a, int b) from Calculator interface
                                 // => Implements Calculator.calculate() method anonymously
                                 // => add is Calculator reference to lambda implementation
int sum = add.calculate(5, 3);   // => Calls lambda with arguments 5 and 3
                                 // => Lambda executes: 5 + 3
                                 // => sum is 8 (returned value)

// LAMBDA with BLOCK body
Calculator multiply = (a, b) -> {
    int result = a * b;
    return result;               // => Explicit return needed for blocks
};

// COMMON FUNCTIONAL INTERFACES (java.util.function package)

// Predicate<T> - takes T, returns boolean
Predicate<Integer> isEven = num -> num % 2 == 0;
                                 // => Type inferred from Predicate<Integer> declaration
                                 // => num is Integer (inferred, no explicit type needed)
                                 // => Lambda tests if number is even (remainder 0)
System.out.println(isEven.test(4));
                                 // => Calls test() method with argument 4
                                 // => Lambda evaluates: 4 % 2 == 0 → true
                                 // => Output: true

// Function<T, R> - takes T, returns R
Function<String, Integer> length = str -> str.length();
                                 // => Function<String, Integer> declares input/output types
                                 // => str is String (inferred from Function<String, ...>)
                                 // => Lambda calls String.length() method
                                 // => Returns Integer (inferred from Function<..., Integer>)
System.out.println(length.apply("hello"));
                                 // => Calls apply() method with "hello"
                                 // => Lambda executes: "hello".length() → 5
                                 // => Output: 5 (Integer value)

// Consumer<T> - takes T, returns nothing (void)
Consumer<String> printer = msg -> System.out.println(msg);
printer.accept("Hello Lambda"); // => Output: Hello Lambda

// Supplier<T> - takes nothing, returns T
Supplier<Double> random = () -> Math.random();
                                 // => No parameters → empty ()
double rand = random.get();      // => rand is random value 0.0 to 1.0

// LAMBDAS with COLLECTIONS
List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);

// forEach with lambda
numbers.forEach(n -> System.out.print(n + " "));
                                 // => Output: 1 2 3 4 5

// removeIf with lambda
numbers.removeIf(n -> n % 2 == 0);
                                 // => Removes even numbers, numbers is [1, 3, 5]

// sort with lambda comparator
List<String> words = Arrays.asList("apple", "banana", "cherry");
words.sort((a, b) -> a.length() - b.length());
                                 // => Sort by length: ["apple", "banana", "cherry"]

// METHOD REFERENCE - shorthand for lambdas calling single method
Consumer<String> methodRef = System.out::println;
                                 // => Equivalent to: str -> System.out.println(str)
words.forEach(System.out::println);
                                 // => Output: apple, banana, cherry (each on new line)
```

**Key Takeaway**: Lambda expressions `(params) -> expression` provide concise syntax for functional interfaces (interfaces with one abstract method). Common functional interfaces: Predicate (condition), Function (transformation), Consumer (side effect), Supplier (factory). Method references (`Class::method`) are syntactic sugar for lambdas delegating to existing methods.

**Why It Matters**: Lambdas (Java 8, 2014) transformed Java from verbose OOP-only language to hybrid functional/OOP language, finally catching up to C#'s lambdas (2007) and modern languages. Pre-lambda Java required anonymous inner classes for callbacks (`new Runnable() { public void run() {...} }`), making functional patterns painfully verbose. Lambdas enable streams API (`list.stream().filter(...).map(...).collect()`), parallel processing, and reactive programming. The lambda syntax leverages type inference—compiler deduces parameter types from functional interface, eliminating boilerplate. However, lambdas capture variables from surrounding scope (closures), but Java enforces "effectively final" rule—captured variables cannot be modified, preventing subtle concurrency bugs from mutable closures.

---

## Example 28: Streams API - Functional Collection Processing

Streams provide declarative, functional-style operations on collections. They enable filtering, mapping, and reducing operations without explicit loops, supporting both sequential and parallel execution.

**Code**:

```java
import java.util.*;              // => Import Collections classes (List, Arrays)
import java.util.stream.*;       // => Import Stream API classes

List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
                                 // => Creates immutable List of 10 integers
                                 // => numbers is [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// FILTER - select elements matching predicate
List<Integer> evens = numbers.stream()
                                 // => Creates Stream<Integer> from numbers list
                                 // => Stream is lazy: operations don't execute yet
    .filter(n -> n % 2 == 0)     // => Lambda predicate: keep if n % 2 == 0 (even)
                                 // => Intermediate operation (lazy, returns new stream)
                                 // => Will filter: 2, 4, 6, 8, 10 pass, odds rejected
    .collect(Collectors.toList());
                                 // => Terminal operation: triggers stream execution
                                 // => Collects filtered results into new ArrayList
                                 // => evens is [2, 4, 6, 8, 10] (5 even numbers)

// MAP - transform each element
List<Integer> squares = numbers.stream()
                                 // => Creates stream from numbers list
    .map(n -> n * n)             // => Lambda: transform each n to n²
                                 // => Intermediate operation (lazy mapping)
                                 // => Will square each: 1→1, 2→4, 3→9, 4→16, etc.
    .collect(Collectors.toList());
                                 // => Terminal operation: execute and collect
                                 // => squares is [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

// FILTER + MAP chain - composable operations
List<Integer> evenSquares = numbers.stream()
                                 // => Creates stream from numbers
    .filter(n -> n % 2 == 0)     // => Lazy: filter evens (2, 4, 6, 8, 10)
                                 // => Intermediate operation (doesn't execute yet)
    .map(n -> n * n)             // => Lazy: square filtered values
                                 // => Intermediate operation (chained with filter)
    .collect(Collectors.toList());
                                 // => Terminal operation: triggers pipeline execution
                                 // => Processes: 2→4, 4→16, 6→36, 8→64, 10→100
                                 // => evenSquares is [4, 16, 36, 64, 100]

// REDUCE - combine all elements to single value
int sum = numbers.stream()       // => Creates stream
    .reduce(0, (a, b) -> a + b); // => Binary operator: accumulate a + b
                                 // => Initial value 0 (identity)
                                 // => Step 1: 0 + 1 = 1
                                 // => Step 2: 1 + 2 = 3
                                 // => Step 3: 3 + 3 = 6, ...
                                 // => Step 10: 45 + 10 = 55
                                 // => sum is 55 (sum of 1..10)

// COUNT - count elements matching filter
long count = numbers.stream()    // => Creates stream
    .filter(n -> n > 5)          // => Keeps 6, 7, 8, 9, 10 (5 elements)
                                 // => Elements > 5
    .count();                    // => Terminal operation: counts filtered elements
                                 // => count is 5 (5 numbers greater than 5)

// MIN / MAX - find minimum or maximum
Optional<Integer> max = numbers.stream()
                                 // => Creates stream
    .max(Integer::compareTo);    // => Method reference: natural ordering comparator
                                 // => Compares all elements to find maximum
                                 // => max is Optional[10] (wrapped in Optional)
int maxValue = max.orElse(0);    // => Unwraps Optional: 10 if present, 0 if empty
                                 // => maxValue is 10 (maximum value from list)

// SORT - order elements by comparator
List<Integer> sorted = numbers.stream()
                                 // => Creates stream
    .sorted(Comparator.reverseOrder())
                                 // => Sorts in reverse natural order (descending)
                                 // => Intermediate operation (sorts stream)
    .collect(Collectors.toList());
                                 // => Terminal: collect to list
                                 // => sorted is [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

// DISTINCT - remove duplicates
List<Integer> withDupes = Arrays.asList(1, 2, 2, 3, 3, 3, 4);
                                 // => List with duplicate elements
                                 // => withDupes is [1, 2, 2, 3, 3, 3, 4]
List<Integer> unique = withDupes.stream()
                                 // => Creates stream from withDupes
    .distinct()                  // => Removes duplicates (keeps first occurrence)
                                 // => Intermediate operation (stateful)
    .collect(Collectors.toList());
                                 // => Terminal: collect unique elements
                                 // => unique is [1, 2, 3, 4] (duplicates removed)

// PARALLEL STREAMS - concurrent execution
int parallelSum = numbers.parallelStream()
                                 // => Creates parallel stream (fork-join pool)
                                 // => Splits work across available CPU cores
    .filter(n -> n % 2 == 0)     // => Parallel filter: multiple threads process chunks
                                 // => Keeps even numbers: 2, 4, 6, 8, 10
    .mapToInt(n -> n)            // => Converts to IntStream (primitive int stream)
                                 // => Avoids autoboxing overhead
    .sum();                      // => Terminal: sum all filtered values
                                 // => parallelSum is 30 (2+4+6+8+10)

// STREAM from sources - creating streams
Stream<String> stream1 = Stream.of("a", "b", "c");
                                 // => Creates stream from varargs elements
                                 // => stream1 contains 3 elements: "a", "b", "c"
IntStream range = IntStream.range(1, 10);
                                 // => Creates IntStream of integers from 1 to 9
                                 // => Exclusive end: 10 not included
                                 // => range contains: 1, 2, 3, 4, 5, 6, 7, 8, 9
```

**Key Takeaway**: Streams enable functional collection processing with declarative operations: `filter` (select), `map` (transform), `reduce` (combine), `collect` (accumulate). Streams are lazy—intermediate operations (filter, map) don't execute until terminal operation (collect, count, reduce) is called. Use `parallelStream()` for concurrent processing on multi-core systems.

**Why It Matters**: Streams (Java 8, 2014) eliminated imperative loops for collection processing, making code more declarative and readable—`list.stream().filter().map().collect()` clearly expresses intent vs nested loops with temporary variables. Lazy evaluation optimizes performance: filtering 1 million elements then taking first 10 only processes ~10 elements, not the full million. Parallel streams leverage multi-core CPUs automatically—`parallelStream()` splits work across threads using fork/join pool, achieving near-linear speedup for CPU-bound operations. However, streams have overhead—for small collections (<100 elements), traditional loops are faster. The Optional return type for min/max forces null-safety, preventing NullPointerException from empty collections.

---

## Example 29: Optional - Null-Safe Value Containers

Optional is a container for values that may be absent, providing null-safe operations. It forces explicit handling of missing values, preventing NullPointerException.

**Code**:

```java
import java.util.Optional;       // => Optional class from java.util package

// CREATE Optional - three factory methods
Optional<String> present = Optional.of("hello");
                                 // => Creates Optional containing "hello" (non-null value required)
                                 // => present is Optional["hello"] (value present)
// Optional<String> nullError = Optional.of(null);
                                 // => Throws NullPointerException if argument is null
                                 // => Use ofNullable for potentially null values

Optional<String> maybe = Optional.ofNullable(null);
                                 // => Creates Optional from potentially null value
                                 // => null argument → Optional.empty() (safe handling)
                                 // => maybe is Optional.empty() (no value)

Optional<String> empty = Optional.empty();
                                 // => Creates explicitly empty Optional (no value)
                                 // => empty is Optional.empty() (singleton instance)

// CHECK presence
boolean hasValue = present.isPresent();
                                 // => hasValue is true
boolean isEmpty = empty.isEmpty();
                                 // => isEmpty is true (Java 11+)

// GET value (unsafe - throws if empty!)
String value = present.get();    // => value is "hello"
// String danger = empty.get();  // => NoSuchElementException!

// GET with default value (safe)
String withDefault = empty.orElse("default");
                                 // => withDefault is "default"
String withSupplier = empty.orElseGet(() -> "computed default");
                                 // => Supplier called only if empty (lazy)

// CONDITIONAL execution
present.ifPresent(val -> System.out.println("Value: " + val));
                                 // => Output: Value: hello
empty.ifPresent(val -> System.out.println("Not executed"));
                                 // => No output (empty Optional)

// MAP - transform value if present
Optional<Integer> length = present.map(String::length);
                                 // => length is Optional[5] ("hello".length())
Optional<Integer> emptyLength = empty.map(String::length);
                                 // => emptyLength is Optional.empty()

// FILTER - keep value if matches predicate
Optional<String> longEnough = present.filter(s -> s.length() > 3);
                                 // => longEnough is Optional["hello"] (5 > 3)
Optional<String> tooShort = present.filter(s -> s.length() > 10);
                                 // => tooShort is Optional.empty() (5 not > 10)

// FLATMAP - transform to Optional, avoid nesting
Optional<String> outer = Optional.of("hello");
Optional<String> flattened = outer.flatMap(s ->
    Optional.of(s.toUpperCase())
);                               // => flattened is Optional["HELLO"]
                                 // => map would give Optional[Optional["HELLO"]]

// CHAINING operations
String result = Optional.ofNullable("  text  ")
    .filter(s -> !s.isEmpty())   // => Keep non-empty
    .map(String::trim)           // => "text"
    .map(String::toUpperCase)    // => "TEXT"
    .orElse("EMPTY");            // => result is "TEXT"

// REAL-WORLD usage: method return types
public Optional<String> findUserById(int id) {
                                 // => Return type Optional signals: value may be absent
                                 // => Caller must handle both present and empty cases
                                 // => Forces explicit null handling at compile time
    // ... database lookup ...   // => Database query (may or may not find user)
    if (userFound) {             // => User exists in database
                                 // => Branch taken if database query successful
        return Optional.of(userName);
                                 // => Returns Optional containing user name
                                 // => Wraps found userName in Optional container
    }
    return Optional.empty();     // => Explicit "no result" signal (user not found)
                                 // => Returns empty Optional (no value)
                                 // => Prevents returning null (forces caller to handle absence)
}

Optional<String> user = findUserById(123);
                                 // => Calls method with user ID 123
                                 // => user is Optional (either present or empty)
                                 // => May contain user name or be empty
user.ifPresent(name -> System.out.println("Found: " + name));
                                 // => Lambda executes only if user present
                                 // => If present: prints "Found: [userName]"
                                 // => If empty: no output (lambda skipped)
```

**Key Takeaway**: Optional provides null-safe container for values that may be absent. Use `ofNullable()` to create from potentially null values, `orElse()` for default values, `map()`/`filter()` for transformations, and `ifPresent()` for conditional execution. Optional forces explicit missing-value handling, preventing NullPointerException.

**Why It Matters**: Optional (Java 8, 2014) brought functional null-handling from languages like Scala/Haskell to Java, forcing developers to acknowledge missing values explicitly. Before Optional, null returns were ambiguous—does `findUser()` returning null mean "not found" or "error"? Optional makes intent explicit: `Optional<User>` clearly signals "may be absent". However, Optional is NOT a general null replacement—use it for return types signaling potential absence, NOT for fields or parameters (creates more NullPointerExceptions from unwrapping). The `get()` method is dangerous (throws if empty), prefer `orElse()` or `ifPresent()`. Despite criticism (Tony Hoare called null his "billion-dollar mistake"), Optional doesn't eliminate null—just makes it visible where values may be missing.

---

## Example 30: Date and Time API - Modern Temporal Types

Java 8's java.time package provides immutable, thread-safe date/time types replacing the notoriously buggy java.util.Date and Calendar. The new API clearly separates date, time, and datetime concepts.

**Code**:

```java
import java.time.*;              // => Date/time types (LocalDate, LocalTime, etc.)
import java.time.format.DateTimeFormatter;
                                 // => Formatting/parsing utilities

// CURRENT date/time
LocalDate today = LocalDate.now();
                                 // => Current date (no time component)
                                 // => today is 2026-01-02 (example, system date)
LocalTime now = LocalTime.now(); // => Current time (no date component)
                                 // => now is 14:30:45.123456789 (example, system time)
LocalDateTime datetime = LocalDateTime.now();
                                 // => Current date AND time combined
                                 // => datetime is 2026-01-02T14:30:45.123456789

// CREATE specific date/time
LocalDate birthday = LocalDate.of(1990, 5, 15);
                                 // => Creates date: year=1990, month=5 (May), day=15
                                 // => birthday is 1990-05-15 (immutable)
LocalTime meeting = LocalTime.of(14, 30);
                                 // => Creates time: hour=14 (2 PM), minute=30
                                 // => meeting is 14:30 (2:30 PM, immutable)

// DATE arithmetic
LocalDate tomorrow = today.plusDays(1);
                                 // => Adds 1 day to today
                                 // => Returns NEW LocalDate (immutable, today unchanged)
                                 // => tomorrow is 2026-01-03 (new instance)
LocalDate lastWeek = today.minusWeeks(1);
                                 // => Subtracts 1 week (7 days) from today
                                 // => Returns NEW LocalDate (immutable)
                                 // => lastWeek is 2025-12-26 (new instance)

// EXTRACT components
int year = today.getYear();      // => Extracts year component from today
                                 // => year is 2026 (integer value)
Month month = today.getMonth();  // => Extracts month as enum value
                                 // => month is Month.JANUARY (enum, not int)
int day = today.getDayOfMonth(); // => Extracts day of month (1-31)
                                 // => day is 2 (integer value)

// COMPARISONS
boolean isBefore = birthday.isBefore(today);
                                 // => Checks if birthday is before today
                                 // => 1990-05-15 < 2026-01-02 → true
                                 // => isBefore is true
boolean isAfter = tomorrow.isAfter(today);
                                 // => Checks if tomorrow is after today
                                 // => 2026-01-03 > 2026-01-02 → true
                                 // => isAfter is true

// PERIOD - date duration
LocalDate start = LocalDate.of(2020, 1, 1);
                                 // => Start date: 2020-01-01
LocalDate end = LocalDate.of(2026, 1, 2);
                                 // => End date: 2026-01-02
Period period = Period.between(start, end);
                                 // => Calculates period between two dates
                                 // => period is P6Y1D (6 years, 0 months, 1 day)
int years = period.getYears();   // => Extracts years component from period
                                 // => years is 6 (integer value)

// DURATION - time duration
LocalTime startTime = LocalTime.of(9, 0);
                                 // => Start time: 09:00 (9 AM)
LocalTime endTime = LocalTime.of(17, 30);
                                 // => End time: 17:30 (5:30 PM)
Duration duration = Duration.between(startTime, endTime);
                                 // => Calculates duration between two times
                                 // => duration is PT8H30M (8 hours, 30 minutes)
long hours = duration.toHours(); // => Converts duration to total hours
                                 // => hours is 8 (long value, truncates 30 minutes)

// FORMATTING
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
                                 // => Creates formatter with custom pattern
                                 // => Pattern: day/month/year (DD/MM/YYYY format)
String formatted = today.format(formatter);
                                 // => Formats today using custom pattern
                                 // => formatted is "02/01/2026" (String)

// PARSING
LocalDate parsed = LocalDate.parse("15-05-1990",
                                 // => Parses string "15-05-1990" to LocalDate
    DateTimeFormatter.ofPattern("dd-MM-yyyy"));
                                 // => Uses pattern: day-month-year
                                 // => parsed is 1990-05-15 (LocalDate)

// ZONED date/time
ZonedDateTime zoned = ZonedDateTime.now(ZoneId.of("America/New_York"));
                                 // => Current date/time in New York timezone
                                 // => zoned is 2026-01-02T09:30:45.123-05:00[America/New_York]
                                 // => Includes timezone offset (-05:00) and zone ID

// INSTANT - machine timestamp
Instant timestamp = Instant.now();
                                 // => Current UTC timestamp (machine time)
                                 // => timestamp is 2026-01-02T14:30:45.123456789Z
                                 // => Nanosecond precision, UTC timezone (Z suffix)
long epochSeconds = timestamp.getEpochSecond();
                                 // => Converts timestamp to seconds since Unix epoch (1970-01-01)
                                 // => epochSeconds is 1735826445 (example, long value)
```

**Key Takeaway**: Use java.time types for date/time operations: LocalDate (date only), LocalTime (time only), LocalDateTime (date+time), ZonedDateTime (with timezone), Instant (machine timestamp). All types are immutable (methods return new instances). Use DateTimeFormatter for parsing/formatting with custom patterns.

**Why It Matters**: java.util.Date and Calendar (Java 1.0-1.1) were fundamentally broken—mutable (not thread-safe), 0-indexed months (January = 0), confusing constructors (year offset by 1900), and no timezone clarity. The java.time API (Java 8, 2014, based on Joda-Time library) fixed all issues with immutable types, clear method names (`plusDays` vs `add(Calendar.DAY_OF_MONTH, 1)`), and explicit timezone handling (LocalDateTime vs ZonedDateTime). Immutability eliminates shared-state concurrency bugs and enables safe caching. The distinction between human time (LocalDateTime) and machine time (Instant) prevents subtle bugs from timezone conversions. Modern applications use Instant for timestamps (database storage, logs) and ZonedDateTime for user-facing dates (meeting schedulers, calendars).

---
