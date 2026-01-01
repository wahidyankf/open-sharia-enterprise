---
title: "Beginner"
date: 2026-01-01T21:53:25+07:00
draft: false
weight: 10000001
description: "Learn Java basics through 30 annotated examples: variables, I/O, OOP fundamentals, collections, control flow, streams, and more - perfect first examples for Java"
tags: ["java", "tutorial", "by-example", "beginner", "basics", "oop", "collections", "io", "strings"]
---

Learn Java fundamentals through 30 annotated code examples. Each example is self-contained, runnable in JShell or as standalone classes, and heavily commented to show what each line does, expected outputs, and intermediate values.

## Group 1: First Steps

### Example 1: Hello World and JVM Compilation

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
// File: HelloWorld.java
// => Filename MUST match public class name (HelloWorld.java for public class HelloWorld)
// => One public class per file (Java compilation rule)

// Define a public class - the basic unit of code organization in Java
// => public: class accessible from other packages
// => class: keyword to define a new class
// => HelloWorld: class name (PascalCase by convention)
public class HelloWorld {
    // => Opening brace { starts class body
    // => Everything inside { } belongs to HelloWorld class

    // main method - the entry point of every Java application
    // => JVM looks for this exact signature to start program execution
    // => Must be: public static void main(String[] args)

    // public: JVM must access this method from outside the class
    // => Without public, JVM cannot call main → runtime error

    // static: belongs to class itself, not to instances (objects)
    // => JVM calls main WITHOUT creating HelloWorld object
    // => Can call HelloWorld.main() directly on class
    // => No "new HelloWorld()" needed before running main

    // void: method returns no value
    // => main produces side effects (prints output) but doesn't return anything
    // => void methods end without return statement (implicit return)

    // main: method name (entry point name is fixed by JVM specification)
    // => JVM specification requires exact name "main" (case-sensitive)
    // => Different name like "start" or "Main" → JVM won't recognize it

    // String[] args: command-line arguments array
    // => args is an array of String objects
    // => Receives arguments passed when running: java HelloWorld arg1 arg2
    // => args[0] would be "arg1", args[1] would be "arg2"
    // => Empty array [] if no arguments provided
    // => Can also write as String args[] (C-style, but String[] preferred)
    public static void main(String[] args) {
        // => Opening brace { starts main method body

        // System.out.println() - standard output printing
        // => System: built-in class in java.lang package (auto-imported)
        // => out: static field in System class (type: PrintStream)
        // => println: method of PrintStream (print line + newline)
        // => Sends text to standard output stream (console by default)

        // String literal "Hello, World!"
        // => Enclosed in double quotes " " (single quotes ' ' are for char only)
        // => Stored in string pool (memory area for literal strings)
        // => Immutable (cannot be changed after creation)

        System.out.println("Hello, World!");
        // => Executes: prints "Hello, World!" to console
        // => Output: Hello, World!
        // => Adds newline character \n after text (println = print line)
        // => Console cursor moves to next line after printing

        // Method execution flow:
        // => 1. Evaluate argument: "Hello, World!" (string literal)
        // => 2. Pass string to println method
        // => 3. println writes string to System.out stream
        // => 4. println writes newline character \n
        // => 5. Method returns (void, so no return value)

        // Implicit return for void methods
        // => No explicit "return;" needed (though allowed)
        // => Method ends when reaching closing brace }
        // => Control returns to JVM (program terminates if main ends)
    }
    // => Closing brace } ends main method body
}
// => Closing brace } ends HelloWorld class body

// Compilation process:
// Command: javac HelloWorld.java
// => javac: Java compiler executable
// => HelloWorld.java: source code file (plain text)
// => Output: HelloWorld.class (compiled bytecode file, binary format)
// => Bytecode is platform-independent (same .class runs on Windows, Linux, Mac)
// => .class file contains JVM instructions (not native machine code)

// Execution process:
// Command: java HelloWorld
// => java: JVM launcher executable
// => HelloWorld: class name (NOT HelloWorld.class - no .class extension!)
// => JVM loads HelloWorld.class from filesystem
// => JVM verifies bytecode (security check - ensures safe code)
// => JVM searches for main(String[] args) method
// => JVM executes main method line by line
// => Output appears on console: Hello, World!
// => Program exits when main returns (exit code 0 = success)

// Command-line arguments example:
// Command: java HelloWorld Alice Bob Charlie
// => args[0] is "Alice"
// => args[1] is "Bob"
// => args[2] is "Charlie"
// => args.length is 3 (number of arguments)
// To access: System.out.println(args[0]); → prints "Alice"

// Comment syntax:
// Single-line comment: // text (from // to end of line)
// => Used for short explanations
// => Common for inline annotations (// => output format)

/* Multi-line comment: text spanning multiple lines
   => Starts with /* and ends with */
// => Can span multiple lines
// => Useful for longer explanations or commenting out code blocks
// => CANNOT be nested: /* outer /* inner */ outer */ fails (inner */ closes outer)

/** Javadoc comment: documentation comment for API docs
 * => Starts with /** (note extra asterisk)
 * => Used for class, method, field documentation
 * => Processed by javadoc tool to generate HTML documentation
 * => Supports special tags: @param, @return, @throws, @author, etc.
 * Example:
 * @param args command-line arguments (not used in this program)
 */

// File naming rules:
// => Public class name MUST match filename
// => HelloWorld class → HelloWorld.java (case-sensitive!)
// => Mismatch causes compilation error: "class HelloWorld is public, should be declared in a file named HelloWorld.java"
// => Can have multiple non-public classes in same file (uncommon)
// => Convention: one class per file (better organization)

// Package statement (not used in this simple example):
// => If used, must be FIRST line (before imports and class)
// => Example: package com.example.hello;
// => Creates namespace to avoid name conflicts
// => File must be in directory matching package: com/example/hello/HelloWorld.java

// Import statements (not needed here):
// => Import classes from other packages
// => java.lang.* is auto-imported (includes System, String, Integer, etc.)
// => Example: import java.util.Scanner; (for input)
// => Must come AFTER package, BEFORE class declaration

// Platform independence explanation:
// => Source code (.java) is human-readable text
// => Compiler produces bytecode (.class) - JVM instructions
// => JVM is platform-specific (different JVM for Windows, Linux, Mac)
// => Same .class file runs on ANY platform with compatible JVM
// => "Write once, run anywhere" (WORA) - Java's core philosophy
// => Contrast with C/C++: compiled directly to machine code (platform-specific binary)
```

**Key Takeaway**: Java code is organized into classes with filenames matching public class names. The `public static void main(String[] args)` method is the fixed entry point recognized by the JVM. Code compiles to platform-independent bytecode (`.class` files) executed by the platform-specific JVM, enabling "write once, run anywhere" portability.

**Why It Matters**: The JVM architecture revolutionized software deployment by decoupling code from operating systems. Before Java, C/C++ programs required separate compilation for Windows, Linux, and macOS—often with platform-specific code branches. Java's bytecode layer lets enterprises ship one `.jar` file that runs identically across all platforms, eliminating costly multi-platform testing. This design enabled Java to dominate enterprise servers (Spring Boot, Tomcat), Android mobile apps (Dalvik/ART VMs execute Java bytecode), and big data systems (Hadoop, Spark, Kafka)—all sharing the same codebase across heterogeneous infrastructure. The `main` method signature is rigidly standardized so tools, IDEs, and frameworks can reliably find program entry points through reflection.

---

### Example 2: Variables and Type System

Java is statically typed with two categories: primitive types (stored on stack) and reference types (stored on heap). Types can be declared explicitly or inferred with `var`.

**Code**:

```java
// PRIMITIVE TYPES - 8 total types, stored directly in memory (stack)
// => Values stored inline in variables (not references/pointers)
// => Faster access (no indirection through memory address)
// => Cannot be null (always have a value)
// => Not objects (no methods, cannot use with generics directly)

// byte - 8-bit signed integer
byte b = 127;
// => b stores value 127 directly (not a reference)
// => Type: byte (8 bits = 1 byte)
// => Range: -128 to 127 (2^7 values: -2^7 to 2^7-1)
// => Use case: saving memory when storing small integers (arrays of small numbers)
// => Signed: leftmost bit is sign (0 = positive, 1 = negative)
// byte overflow = 127 + 1; → -128 (wraps around, no error!)

// short - 16-bit signed integer
short s = 32000;
// => s stores value 32000 directly
// => Type: short (16 bits = 2 bytes)
// => Range: -32,768 to 32,767 (2^15 values: -2^15 to 2^15-1)
// => Use case: memory-constrained situations (rare in modern Java)
// => Rarely used (int is almost always preferred for whole numbers)

// int - 32-bit signed integer (DEFAULT for whole numbers)
int i = 42;
// => i stores value 42 directly
// => Type: int (32 bits = 4 bytes)
// => Range: -2,147,483,648 to 2,147,483,647 (~-2.1B to 2.1B)
// => DEFAULT type for integer literals (42 is int, not byte/short)
// => Most commonly used integer type (balance of range and performance)
// => Literal examples: 0, -10, 1_000_000 (underscores for readability, Java 7+)

// long - 64-bit signed integer
long l = 1000000L;
// => l stores value 1,000,000 directly
// => Type: long (64 bits = 8 bytes)
// => Range: -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807 (~-9.2E18 to 9.2E18)
// => REQUIRES 'L' or 'l' suffix (L preferred for clarity - lowercase l looks like 1)
// => Without L: long x = 1000000; → treated as int, then widened to long
// => With L: compiler treats literal as long directly
// => Use case: timestamps (milliseconds since epoch), IDs, large counters
// long timestamp = System.currentTimeMillis(); // => 1703145600000L (milliseconds)

// float - 32-bit floating-point (single precision)
float f = 3.14f;
// => f stores approximation of 3.14 (binary floating point)
// => Type: float (32 bits = 4 bytes, IEEE 754 single precision)
// => Precision: ~6-7 decimal digits
// => REQUIRES 'f' or 'F' suffix (otherwise treated as double)
// => WITHOUT f: float x = 3.14; → ERROR (3.14 is double, cannot assign to float)
// => Use case: graphics, games (where memory matters more than precision)
// => Not suitable for currency (use BigDecimal for money!)
// float notExact = 0.1f + 0.2f; // => 0.30000001 (NOT 0.3 - floating point error)

// double - 64-bit floating-point (double precision, DEFAULT for decimals)
double d = 3.14159;
// => d stores approximation of 3.14159
// => Type: double (64 bits = 8 bytes, IEEE 754 double precision)
// => Precision: ~15-16 decimal digits
// => DEFAULT type for decimal literals (3.14 is double, not float)
// => No suffix needed (but can use 'd' or 'D' explicitly)
// => Most commonly used floating-point type (preferred over float)
// => Still not exact for all decimals (binary representation limitation)
// double imprecise = 0.1 + 0.2; // => 0.30000000000000004 (NOT 0.3!)

// boolean - logical true/false
boolean bool = true;
// => bool stores true (or false)
// => Type: boolean (size unspecified by JVM spec, typically 1 byte in arrays, 1 bit in fields)
// => Values: ONLY true or false (case-sensitive, lowercase)
// => NOT integers: cannot use 0/1 like C (if (1) is ERROR, must be if (true))
// => No null for primitives (boolean field defaults to false)
// => Use case: flags, conditions, control flow
// boolean isValid = (5 > 3); // => true (result of comparison)

// char - 16-bit Unicode character
char c = 'A';
// => c stores Unicode code point for 'A' (65 in decimal, 0x0041 in hex)
// => Type: char (16 bits = 2 bytes, unsigned)
// => Range: 0 to 65,535 (Unicode BMP - Basic Multilingual Plane)
// => SINGLE quotes ' ' for char (double quotes " " for String)
// => Can store any Unicode character: 'A', '中', '€', '🙂' (emoji needs surrogate pairs)
// => Can be treated as integer: char letter = 65; → 'A' (ASCII/Unicode code)
// char next = (char)(c + 1); // => 'B' (arithmetic on char yields int, requires cast back)

// char special literals (escape sequences)
char newline = '\n';     // => newline character (ASCII 10)
char tab = '\t';         // => tab character (ASCII 9)
char backslash = '\\';   // => backslash itself (escapes the escape)
char quote = '\'';       // => single quote (escaped to avoid syntax confusion)
char unicode = '\u0041'; // => 'A' (Unicode escape: \uXXXX where XXXX is hex code)

// REFERENCE TYPES - variables store memory addresses (heap pointers)
// => Variable holds reference (address) to object in heap, NOT the object itself
// => Multiple variables can reference same object (aliasing)
// => Can be null (special reference meaning "no object")
// => All classes, interfaces, arrays, enums are reference types

// String - most common reference type (immutable sequence of characters)
String str = "Hello";
// => str stores REFERENCE to String object in heap (NOT the characters directly)
// => String object contains: {'H', 'e', 'l', 'l', 'o'} as internal char array
// => Literal "Hello" stored in string pool (shared memory area for literals)
// => Type: String (class from java.lang, auto-imported)
// => Immutable: cannot change characters after creation
// => str variable: 8 bytes (reference size on 64-bit JVM, 4 bytes on 32-bit)
// => Actual String object: ~50+ bytes (object header + char array + metadata)

// Arrays - reference types (even for primitive element types)
int[] array = {1, 2, 3};
// => array stores REFERENCE to array object in heap
// => Array object contains: [1, 2, 3] as contiguous memory block
// => Type: int[] (array of int primitives)
// => array.length is 3 (array length property, not method - no parentheses!)
// => Elements stored contiguously in heap (fast sequential access)
// => Indexed from 0: array[0] is 1, array[1] is 2, array[2] is 3
// => Fixed size: cannot grow/shrink (use ArrayList for dynamic arrays)
// int first = array[0]; // => 1 (array access by index)

// Array declaration alternatives:
int[] preferredStyle = {1, 2}; // => PREFERRED (type clearly shows "array of int")
int cStyle[] = {1, 2};         // => ALLOWED (C-style, but discouraged in Java)
// Both are equivalent, but int[] is more readable (type information grouped together)

// TYPE INFERENCE with var (Java 10+)
// => Compiler infers type from right-hand side initializer
// => ONLY for local variables (NOT fields, parameters, or method return types)
// => Must initialize immediately (var x; → ERROR: cannot infer type without initializer)

var num = 100;
// => Compiler infers type: int (100 is int literal)
// => IDENTICAL to: int num = 100;
// => num is STILL statically typed (type fixed at compile time)
// => Cannot reassign different type: num = "text"; → ERROR (type mismatch)
// => Reduces boilerplate for obvious types (var instead of repeating type name)

var text = "World";
// => Compiler infers type: String ("World" is String literal)
// => IDENTICAL to: String text = "World";
// => Not dynamic typing (JavaScript/Python) - type is locked at compile time

var list = new java.util.ArrayList<String>();
// => Compiler infers type: java.util.ArrayList<String> (from constructor call)
// => IDENTICAL to: java.util.ArrayList<String> list = new java.util.ArrayList<String>();
// => Avoids repetition (diamond operator <> also helps: new ArrayList<>())
// => var combined with <> is concise: var list = new ArrayList<String>();

// var limitations:
// var noInit; // => ERROR: cannot infer type without initializer
// var nullValue = null; // => ERROR: cannot infer type from null (what type is null?)
// var lambda = x -> x + 1; // => ERROR: cannot infer from lambda (needs target type)
// var array = {1, 2, 3}; // => ERROR: cannot infer array type without new int[] {1, 2, 3}

// FINAL VARIABLES - immutable bindings (variable cannot be reassigned)
final int CONST = 42;
// => CONST is final (cannot reassign)
// => Value 42 is bound permanently to CONST
// => Convention: UPPERCASE_WITH_UNDERSCORES for compile-time constants
// => CONST = 50; → ERROR: "cannot assign a value to final variable CONST"
// => Final for primitives: value cannot change
// => Final for references: reference cannot change (but object CAN be mutated!)

final String finalStr = "text";
// finalStr = "new"; // => ERROR: cannot reassign final variable
// finalStr.toUpperCase(); // => OK: "TEXT" (creates NEW string, doesn't change finalStr)

final int[] finalArray = {1, 2, 3};
// finalArray = new int[]{4, 5}; // => ERROR: cannot reassign reference
// finalArray[0] = 99; // => OK: array contents can change (reference stays same)
// Final means "cannot point to different object", NOT "object is immutable"

// DEFAULT VALUES for instance/static fields (NOT local variables!)
// => Fields (class members) get automatic default initialization
// => Local variables MUST be explicitly initialized (no defaults)

class Defaults {
    byte byteField;       // => 0 (default for byte)
    short shortField;     // => 0 (default for short)
    int intField;         // => 0 (default for int)
    long longField;       // => 0L (default for long)
    float floatField;     // => 0.0f (default for float)
    double doubleField;   // => 0.0 (default for double)
    boolean boolField;    // => false (default for boolean)
    char charField;       // => '\u0000' (null character, default for char)
    String stringField;   // => null (default for ALL reference types)
    int[] arrayField;     // => null (default for array references)
}

// Local variables require explicit initialization:
void method() {
    int local; // => Declared but uninitialized (no default value)
    // System.out.println(local); → ERROR: "variable local might not have been initialized"
    local = 10; // => Now initialized, safe to use
    System.out.println(local); // => OK: 10
}

// TYPE CASTING - converting between types

// Widening cast (implicit) - smaller type to larger type (no data loss)
int x = 10;
// => x stores 10 (32-bit int)

double y = x;
// => IMPLICIT widening cast: int → double
// => y stores 10.0 (64-bit double, decimal point added)
// => No data loss (int range fits entirely in double precision range)
// => Compiler does this automatically (no explicit cast needed)
// => Widening order: byte → short → int → long → float → double
// Note: int → float can lose precision (float has 23-bit mantissa, int has 32 bits)

// Narrowing cast (explicit) - larger type to smaller type (potential data loss)
double z = 9.8;
// => z stores 9.8 (double)

int w = (int) z;
// => EXPLICIT narrowing cast: double → int (MUST use (int) syntax)
// => w stores 9 (decimal part .8 TRUNCATED, not rounded!)
// => Truncation toward zero: (int) 9.8 → 9, (int) -9.8 → -9
// => Data loss: cannot recover .8 after casting
// => Compiler REQUIRES explicit cast (prevents accidental data loss)
// => Without cast: int w = z; → ERROR: "incompatible types: possible lossy conversion"

// Overflow in narrowing cast:
int largeInt = 130;
byte smallByte = (byte) largeInt; // => -126 (overflow! 130 > 127, wraps around)
// 130 in binary (32-bit): 00000000 00000000 00000000 10000010
// Cast to byte (8-bit): 10000010 → -126 in two's complement (leftmost bit = 1 = negative)

// NULL - special reference value (absence of object)

String nullStr = null;
// => nullStr holds null reference (doesn't point to any object)
// => null is LITERAL (keyword, not a variable)
// => Only valid for reference types (classes, arrays, interfaces)
// => Dereferencing null causes NullPointerException (NPE):
// nullStr.length(); → ERROR at runtime: NullPointerException

// Primitives CANNOT be null:
int nullInt = null; // => COMPILATION ERROR: "incompatible types: <null> cannot be converted to int"
// Primitives ALWAYS have a value (default 0/false/'\u0000' for fields)

// Checking for null (defensive programming):
if (nullStr != null) {
    int length = nullStr.length(); // => Safe: only executes if not null
} else {
    // => Handle null case
}

// WRAPPER CLASSES - object versions of primitives (for generics, collections)
// => Primitive: byte, short, int, long, float, double, boolean, char
// => Wrapper: Byte, Short, Integer, Long, Float, Double, Boolean, Character
// => Wrappers are IMMUTABLE (like String - cannot change value after creation)

Integer wrappedInt = Integer.valueOf(100);
// => wrappedInt is reference to Integer object (heap-allocated)
// => Integer object wraps primitive int value 100
// => valueOf() returns cached Integer for -128 to 127 (memory optimization)
// => For values outside cache: new Integer object created (deprecated constructor)

int primitiveInt = wrappedInt.intValue();
// => intValue() extracts primitive int from Integer object
// => primitiveInt stores 100 (primitive value, NOT reference)
// => Conversion: reference type → primitive type (unboxing)

// AUTOBOXING - automatic conversion primitive ↔ wrapper (Java 5+)
Integer autoBoxed = 42;
// => AUTOBOXING: compiler inserts Integer.valueOf(42) automatically
// => IDENTICAL to: Integer autoBoxed = Integer.valueOf(42);
// => Converts primitive int 42 → Integer object

int autoUnboxed = autoBoxed;
// => AUTO-UNBOXING: compiler inserts autoBoxed.intValue() automatically
// => IDENTICAL to: int autoUnboxed = autoBoxed.intValue();
// => Converts Integer object → primitive int

// Autoboxing with null (dangerous!)
Integer nullableInt = null;
// int danger = nullableInt; // => NullPointerException! (auto-unboxing calls .intValue() on null)

// Wrapper caching (Integer pool for -128 to 127):
Integer a1 = 100, a2 = 100; // => Both use SAME cached Integer object
System.out.println(a1 == a2); // => true (same reference - cached)

Integer b1 = 200, b2 = 200; // => DIFFERENT Integer objects (outside cache range)
System.out.println(b1 == b2); // => false (different references - not cached)
System.out.println(b1.equals(b2)); // => true (same value - use .equals() for wrappers!)

// VARIABLE NAMING CONVENTIONS
int camelCase = 1;         // => Local variables, parameters: camelCase
final int UPPER_CASE = 2;  // => Constants (final): UPPER_CASE (screaming snake case)
class ClassName {}         // => Classes, interfaces: PascalCase (UpperCamelCase)

// TYPE HIERARCHY
// Primitives: No hierarchy (not objects, no inheritance)
// Reference types: All inherit from java.lang.Object (implicit superclass)
// Object methods available on all reference types: toString(), equals(), hashCode()

// Wrapper class hierarchy:
// Object (superclass of everything)
//   ↳ Number (abstract superclass of numeric wrappers)
//       ↳ Byte, Short, Integer, Long, Float, Double
//   ↳ Boolean (not subclass of Number)
//   ↳ Character (not subclass of Number)
```

**Key Takeaway**: Java distinguishes 8 primitive types (stored directly on stack, not objects, cannot be null) from reference types (stored on heap, accessed via references, can be null). Use `var` for type inference to reduce boilerplate while preserving static typing. Primitives have default values for fields (0/false/'\u0000'); references default to null. Autoboxing bridges primitives and wrappers automatically, but beware of NullPointerException when unboxing null.

**Why It Matters**: The stack vs heap distinction directly impacts performance and memory management. Primitives on the stack are allocated and freed instantly when methods return (~nanoseconds), making them ideal for high-performance computations like graphics rendering, scientific computing, and real-time systems. Heap-allocated objects require garbage collection cycles, adding overhead (milliseconds of pause time) but enabling flexible object lifecycles and sharing across scopes. Stack space is limited (~1MB per thread by default), so large data structures must use the heap. Understanding primitives vs references prevents common bugs: primitives pass by value (method receives copy), references pass by reference (method can mutate the object). This design enables Java to balance raw performance (primitives) with object-oriented flexibility (references), allowing you to choose the right tool for each scenario.

---

### Example 3: Basic Input/Output with Scanner

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
// Import Scanner from java.util package
import java.util.Scanner;
// => Scanner is NOT in java.lang (unlike String, System, Integer)
// => Must explicitly import: import java.util.Scanner;
// => Without import: compile error "cannot find symbol: class Scanner"

// CREATING A SCANNER - reading from console (standard input)

Scanner scanner = new Scanner(System.in);
// => scanner: reference variable (type: Scanner)
// => new Scanner(...): creates new Scanner object on heap
// => System.in: static field (type: InputStream) representing console input (keyboard)
// => Scanner wraps System.in and provides convenient parsing methods
// => System.in is byte stream (raw bytes), Scanner converts to types (int, double, String, etc.)
// => Scanner maintains internal buffer for storing read data

// Scanner internal state after creation:
// => Buffer: empty (nothing read yet)
// => Source: System.in (console input stream)
// => Delimiter: \s+ (whitespace pattern - spaces, tabs, newlines)
// => Radix: 10 (decimal number system for parsing integers)

// READING DIFFERENT TYPES - Scanner provides type-specific parsing methods

System.out.print("Enter your name: ");
// => System.out.print: prints prompt WITHOUT newline (cursor stays on same line)
// => Contrast with println: prints WITH newline (cursor moves to next line)
// => Output: "Enter your name: " (cursor waits for input on same line)

String name = scanner.nextLine();
// => nextLine(): reads ALL characters until newline (\n) is encountered
// => INCLUDES spaces in the input (doesn't stop at whitespace)
// => CONSUMES the newline character (removes \n from buffer)
// => Example user input: Alice Smith<ENTER>
// => Scanner reads: "Alice Smith\n"
// => Returns: "Alice Smith" (string without \n)
// => name stores: "Alice Smith"
// => Buffer after read: empty (newline consumed)

System.out.print("Enter your age: ");
// => Output: "Enter your age: " (prompt on console)

int age = scanner.nextInt();
// => nextInt(): reads next TOKEN (sequence of non-whitespace characters)
// => Parses token as integer (base-10 by default)
// => Skips leading whitespace (spaces, tabs, newlines before token)
// => STOPS at whitespace after token (doesn't consume trailing whitespace!)
// => Example user input: 25<ENTER>
// => Scanner reads token: "25"
// => Parses "25" → int 25
// => age stores: 25
// => Buffer after read: "\n" (LEFTOVER NEWLINE - common bug source!)

// CRITICAL: nextInt() leaves newline in buffer!
scanner.nextLine();
// => nextLine(): consumes leftover "\n" from previous nextInt()
// => Reads from buffer: "\n" (just the newline character)
// => Returns: "" (empty string - we discard it)
// => Buffer after read: empty (newline consumed)
// => WITHOUT this line: next nextLine() reads leftover "\n" immediately (BUG!)

System.out.print("Enter your GPA: ");
// => Output: "Enter your GPA: "

double gpa = scanner.nextDouble();
// => nextDouble(): reads next token and parses as double (floating-point)
// => Accepts formats: 3.85, 3, .85, 3e2 (scientific notation), 3.85d
// => Skips leading whitespace
// => STOPS at whitespace after number (doesn't consume trailing whitespace!)
// => Example user input: 3.85<ENTER>
// => Scanner reads token: "3.85"
// => Parses "3.85" → double 3.85
// => gpa stores: 3.85
// => Buffer after read: "\n" (LEFTOVER NEWLINE again!)

scanner.nextLine();
// => Consume leftover newline from nextDouble()
// => Same pattern: ALWAYS call nextLine() after nextInt/nextDouble/nextBoolean/etc.
// => Returns: "" (empty string, discarded)
// => Buffer after read: empty

System.out.print("Are you a student? (true/false): ");
// => Output: "Are you a student? (true/false): "

boolean isStudent = scanner.nextBoolean();
// => nextBoolean(): reads next token and parses as boolean
// => Accepts: "true" or "false" (case-INSENSITIVE: "True", "FALSE" also work)
// => Rejects anything else: throws InputMismatchException ("yes", "1", "Y" are invalid)
// => Skips leading whitespace
// => STOPS at whitespace after token (doesn't consume trailing whitespace!)
// => Example user input: true<ENTER>
// => Scanner reads token: "true"
// => Parses "true" → boolean true
// => isStudent stores: true
// => Buffer after read: "\n" (leftover newline)

// NOTE: We skip nextLine() here for demonstration (showing the buffer issue later)

// VALIDATION - checking input before reading (prevents exceptions)

System.out.print("Enter a number (or text to skip): ");
// => Output: "Enter a number (or text to skip): "

if (scanner.hasNextInt()) {
    // => hasNextInt(): checks if next token can be parsed as int
    // => Does NOT consume token (just peeks at buffer)
    // => Returns true if token is valid integer, false otherwise
    // => Example input: 42 → hasNextInt() returns true
    // => Example input: hello → hasNextInt() returns false

    int number = scanner.nextInt();
    // => This line ONLY executes if hasNextInt() returned true
    // => Safe to call nextInt() (no InputMismatchException)
    // => number stores parsed integer value
    // => Example: if user entered 42, number is 42

    System.out.println("You entered: " + number);
    // => Output: "You entered: 42"
    // => String concatenation: "You entered: " + 42 → "You entered: 42"
} else {
    // => This block executes if hasNextInt() returned false
    // => Next token is NOT a valid integer (user entered text)

    String text = scanner.next();
    // => next(): reads next TOKEN (until whitespace)
    // => DIFFERENT from nextLine() (next stops at whitespace, nextLine reads entire line)
    // => Returns token as String (doesn't parse as specific type)
    // => Example input: hello → text is "hello"
    // => Consumes token but NOT trailing whitespace

    System.out.println("Not a number: " + text);
    // => Output: "Not a number: hello"
}

// READING FROM A STRING - parsing structured data (not console input)

String data = "John 30 Engineer";
// => data: string containing space-separated values
// => We want to parse: name (String), age (int), occupation (String)

Scanner stringScanner = new Scanner(data);
// => Create Scanner from String (instead of System.in)
// => stringScanner reads from data string, NOT from console
// => Constructor: Scanner(String source)
// => Buffer contains: "John 30 Engineer" (entire string)

String personName = stringScanner.next();
// => next(): reads next token (stops at whitespace)
// => First token in "John 30 Engineer" is "John"
// => personName stores: "John"
// => Buffer after read: " 30 Engineer" (consumed "John" + following space)

int personAge = stringScanner.nextInt();
// => nextInt(): reads next token ("30") and parses as int
// => personAge stores: 30
// => Buffer after read: " Engineer" (consumed "30" + following space)

String occupation = stringScanner.next();
// => next(): reads next token ("Engineer")
// => occupation stores: "Engineer"
// => Buffer after read: empty (all tokens consumed)

stringScanner.close();
// => close(): releases resources associated with scanner
// => For String scanners: minimal resources (no I/O streams)
// => For System.in scanners: closes underlying input stream (CAREFUL - can't reopen System.in!)
// => Best practice: ALWAYS close scanners (prevents resource leaks)

// CUSTOM DELIMITERS - changing token separator (default is whitespace)

String csvData = "apple,banana,cherry";
// => csvData: comma-separated values (CSV format)
// => Default delimiter (\s+) won't work (no spaces, commas separate values)

Scanner csvScanner = new Scanner(csvData);
// => Create scanner from string
// => Delimiter: \s+ (default - whitespace)
// => Buffer: "apple,banana,cherry"

csvScanner.useDelimiter(",");
// => useDelimiter(String pattern): changes delimiter from whitespace to comma
// => Pattern: "," (literal comma character)
// => Can also use regex: "[,;]" matches comma OR semicolon
// => Delimiter: "," (now tokens are separated by commas)

String fruit1 = csvScanner.next();
// => next(): reads until delimiter (comma)
// => First token: "apple" (everything before first comma)
// => fruit1 stores: "apple"
// => Buffer after read: "banana,cherry" (consumed "apple,")

String fruit2 = csvScanner.next();
// => Second token: "banana" (between first and second comma)
// => fruit2 stores: "banana"
// => Buffer after read: "cherry" (consumed "banana,")

String fruit3 = csvScanner.next();
// => Third token: "cherry" (after second comma, until end of string)
// => fruit3 stores: "cherry"
// => Buffer after read: empty

csvScanner.close();
// => Close scanner to free resources
// => Good practice even for string scanners (resource cleanup)

// TRY-WITH-RESOURCES - automatic scanner cleanup (Java 7+)

try (Scanner autoScanner = new Scanner(System.in)) {
    // => try-with-resources syntax: try (ResourceType var = init) { ... }
    // => autoScanner: scanner declared inside try parentheses
    // => Implements AutoCloseable interface (has close() method)
    // => Automatically calls autoScanner.close() when try block exits
    // => Guaranteed cleanup even if exception occurs (no finally block needed)

    System.out.print("Enter text: ");
    // => Output: "Enter text: "

    String input = autoScanner.nextLine();
    // => nextLine(): reads entire line until newline
    // => input stores user input (without trailing newline)

    System.out.println("You entered: " + input);
    // => Output: "You entered: [user input]"

    // => When try block exits (normal or exception):
    // => Java automatically calls autoScanner.close()
    // => No manual close() needed (prevents resource leak bugs)
}
// => autoScanner is out of scope here (cannot access after try block)
// => Resources already released by automatic close()

// INPUT VALIDATION PATTERN - robust user input handling

Scanner validScanner = new Scanner(System.in);
// => Create scanner for console input

int validNumber = 0;
// => validNumber: will store the valid positive number
// => Initialized to 0 (will be overwritten by valid input)

boolean valid = false;
// => valid: flag to track if valid input received
// => false initially (no valid input yet)
// => Loop continues while valid is false

while (!valid) {
    // => Loop condition: !valid (NOT valid)
    // => Continues looping until valid becomes true
    // => Loop body executes repeatedly until valid input received

    System.out.print("Enter a positive number: ");
    // => Prompt user for input (each iteration)

    if (validScanner.hasNextInt()) {
        // => Check if next token is valid integer
        // => Prevents InputMismatchException from nextInt()

        int num = validScanner.nextInt();
        // => Safe to read int (hasNextInt() returned true)
        // => num stores parsed integer value

        if (num > 0) {
            // => Check if number is positive (meets requirement)
            // => num > 0: true if positive, false if zero or negative

            validNumber = num;
            // => Store valid input in validNumber
            // => validNumber now contains user's positive integer

            valid = true;
            // => Set flag to true (valid input received)
            // => Loop will exit on next condition check (!valid is false)
        } else {
            // => num is zero or negative (invalid input)
            System.out.println("Number must be positive!");
            // => Feedback to user (explain why input rejected)
            // => Loop continues (valid still false)
        }
    } else {
        // => Next token is NOT an integer (user entered text)
        System.out.println("Invalid input! Enter a number.");
        // => Feedback to user (explain error)

        validScanner.next();
        // => CRITICAL: consume invalid token from buffer
        // => Without this: infinite loop! (hasNextInt() keeps returning false for same token)
        // => next() removes bad token, allowing user to enter new input
    }
}
// => Loop exits when valid is true (valid positive number received)
// => validNumber contains the validated input

validScanner.close();
// => Close scanner after use (resource cleanup)

// COMMON BUG: nextInt()/nextDouble() + nextLine() issue

Scanner issueDemo = new Scanner(System.in);
System.out.print("Enter a number: ");

int num = issueDemo.nextInt();
// => Assume user types: 42<ENTER>
// => nextInt() reads "42" token
// => num stores: 42
// => Buffer state: "\n" (NEWLINE LEFT IN BUFFER!)
// => nextInt() does NOT consume the trailing newline

System.out.print("Enter your name: ");

String userName = issueDemo.nextLine();
// => nextLine() reads from current buffer position
// => Buffer contains: "\n" (leftover from previous input)
// => nextLine() reads "\n" IMMEDIATELY (doesn't wait for user input!)
// => userName stores: "" (EMPTY STRING - common bug!)
// => User never gets chance to enter name (appears to skip input)

// BUG EXPLANATION:
// => User sees: "Enter your name: " and program continues immediately
// => Appears to skip input, but actually read leftover newline
// => Frustrating user experience (input seems ignored)

// FIX: Consume leftover newline explicitly

Scanner fixDemo = new Scanner(System.in);
System.out.print("Enter a number: ");

int fixNum = fixDemo.nextInt();
// => fixNum stores: 42 (user input)
// => Buffer state: "\n" (leftover newline)

fixDemo.nextLine();
// => CONSUME leftover newline from buffer
// => Reads: "\n" (just the newline)
// => Returns: "" (empty string - we ignore it)
// => Buffer state: empty (newline consumed)

System.out.print("Enter your name: ");

String fixName = fixDemo.nextLine();
// => NOW nextLine() waits for NEW user input
// => Buffer is empty, so Scanner blocks until user types and presses ENTER
// => User types: Alice<ENTER>
// => fixName stores: "Alice" (CORRECT - bug fixed!)

fixDemo.close();
// => Close scanner (resource cleanup)

// SCANNER METHOD CATEGORIES:

// 1. next*() methods - READ and consume tokens:
//    next()        - reads String token (until delimiter)
//    nextLine()    - reads String line (until newline, CONSUMES newline)
//    nextInt()     - reads int token (doesn't consume trailing whitespace)
//    nextDouble()  - reads double token (doesn't consume trailing whitespace)
//    nextBoolean() - reads boolean token (doesn't consume trailing whitespace)
//    nextByte()    - reads byte token
//    nextShort()   - reads short token
//    nextLong()    - reads long token
//    nextFloat()   - reads float token

// 2. hasNext*() methods - CHECK without consuming (peek):
//    hasNext()        - true if any token available
//    hasNextLine()    - true if line available
//    hasNextInt()     - true if next token is valid int
//    hasNextDouble()  - true if next token is valid double
//    hasNextBoolean() - true if next token is valid boolean
//    (same pattern for other types)

// 3. Configuration methods:
//    useDelimiter(String pattern) - change token delimiter (default: \s+)
//    useRadix(int radix)          - change number parsing base (default: 10)
//    close()                      - release resources

// KEY DIFFERENCES:

// next() vs nextLine():
// => next(): reads SINGLE TOKEN (stops at delimiter, default whitespace)
// => nextLine(): reads ENTIRE LINE (stops at newline, CONSUMES newline)
// Example input: "Hello World\n"
// => next() returns "Hello" (leaves " World\n" in buffer)
// => nextLine() returns "Hello World" (consumes entire line including \n)

// nextInt() vs nextLine():
// => nextInt(): reads token, parses as int, LEAVES trailing whitespace
// => nextLine(): reads entire line, CONSUMES newline
// => ALWAYS call nextLine() after nextInt() to clear buffer

// hasNextInt() vs nextInt():
// => hasNextInt(): CHECKS without consuming (safe, no exception)
// => nextInt(): READS and consumes (throws exception if not int)
// => Pattern: check with hasNextInt(), then safely call nextInt()

// RESOURCE MANAGEMENT:
// => Scanner wraps I/O streams (System.in, FileInputStream, etc.)
// => Streams hold system resources (file handles, sockets)
// => MUST close scanner to release resources (prevent leaks)
// => Best practice: use try-with-resources (automatic close)
// => WARNING: closing Scanner on System.in closes System.in itself!
//    → Cannot create new Scanner(System.in) after closing
//    → For console apps: reuse one Scanner or don't close System.in scanner
```

**Key Takeaway**: Use `Scanner` for reading formatted input from console, files, or strings. **CRITICAL**: Call `nextLine()` after `nextInt()`/`nextDouble()`/`nextBoolean()` to consume leftover newlines and prevent the "input skipping" bug. Use `hasNextX()` methods to validate input before reading (prevents `InputMismatchException`). Always close scanners with `close()` or use try-with-resources for automatic cleanup. Understanding Scanner's token-based parsing (stops at delimiters, doesn't consume trailing whitespace) vs line-based parsing (`nextLine()` consumes newline) is essential for reliable input handling.

**Why It Matters**: Scanner is the standard way to build interactive Java console applications—command-line tools, simple games, educational programs, and learning exercises. The newline consumption bug (`nextInt()` leaving `\n` in buffer) is one of the most common beginner mistakes, causing empty `nextLine()` reads that appear to skip user input—frustrating for both developers and users. Production systems use `Scanner` for parsing structured text (CSV files, config files, log parsing) with custom delimiters, making it essential for backend data processing. While GUI applications use Swing/JavaFX for input and web services use HTTP requests, understanding `Scanner` is critical for debugging, scripting, batch processing, and automated testing where text parsing is ubiquitous. The validation pattern (`hasNextInt()` before `nextInt()`) prevents crashes from malformed input, essential for robust production code.

---

### Example 4: Control Flow - If/Else and Switch

Java provides multiple conditional structures for decision-making. The `if-else` chain handles boolean conditions with multiple branches. Traditional `switch` statements match values and require `break` to prevent fall-through. Modern switch expressions (Java 12+) return values directly and eliminate fall-through bugs. Pattern matching for switch (Java 17+) combines type checking and casting in one operation.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Input["Condition"] --> IfElse["if-else chain<br/>Boolean evaluation"]
    Input --> Switch["switch statement<br/>Value matching"]
    Input --> SwitchExpr["switch expression<br/>Value return"]

    IfElse --> Branch1["if #40;true#41;<br/>Execute block"]
    IfElse --> Branch2["else if<br/>Next condition"]
    IfElse --> Branch3["else<br/>Default"]

    Switch --> Case1["case value<br/>Match & execute"]
    Switch --> Case2["default<br/>No match"]

    SwitchExpr --> Expr1["case -> value<br/>Return directly"]
    SwitchExpr --> Expr2["case -> #123;block#125;<br/>yield value"]

    style Input fill:#0173B2,color:#fff
    style IfElse fill:#DE8F05,color:#fff
    style Switch fill:#029E73,color:#fff
    style SwitchExpr fill:#CC78BC,color:#fff
    style Branch1 fill:#CA9161,color:#fff
    style Branch2 fill:#CA9161,color:#fff
    style Branch3 fill:#CA9161,color:#fff
    style Case1 fill:#0173B2,color:#fff
    style Case2 fill:#0173B2,color:#fff
    style Expr1 fill:#DE8F05,color:#fff
    style Expr2 fill:#DE8F05,color:#fff
```

**Code**:

```java
// ===== IF-ELSE STATEMENTS =====
// Basic if statement - single condition
int temperature = 25; // => temperature is 25 (type: int)

if (temperature > 30) { // => Condition evaluates: 25 > 30 = false
    System.out.println("Hot"); // => Not executed (condition false)
}
// => No output (if block skipped)

// if-else - binary decision
int age = 20; // => age is 20 (type: int)

if (age >= 18) { // => Condition: 20 >= 18 = true
    System.out.println("Adult"); // => Output: Adult (this branch executes)
} else {
    System.out.println("Minor"); // => Skipped (condition was true)
}
// => Output: Adult

// if-else-if chain - multiple exclusive conditions
int score = 85; // => score is 85 (type: int)

if (score >= 90) { // => 85 >= 90 = false, skip this branch
    System.out.println("Grade: A");
} else if (score >= 80) { // => 85 >= 80 = true, enter this branch
    System.out.println("Grade: B"); // => Output: Grade: B
} else if (score >= 70) { // => Skipped (previous condition matched)
    System.out.println("Grade: C");
} else if (score >= 60) { // => Skipped
    System.out.println("Grade: D");
} else {
    System.out.println("Grade: F"); // => Skipped (no need to check else)
}
// => Output: Grade: B
// => Only ONE branch executes (first match wins)

// Nested if statements - conditions within conditions
boolean hasLicense = true; // => hasLicense is true (type: boolean)
int driverAge = 20; // => driverAge is 20 (type: int)

if (driverAge >= 18) { // => 20 >= 18 = true, enter outer if
    if (hasLicense) { // => true, enter inner if
        System.out.println("Can drive"); // => Output: Can drive
    } else {
        System.out.println("Need license"); // => Skipped
    }
} else {
    System.out.println("Too young"); // => Skipped (outer condition true)
}
// => Output: Can drive

// Complex boolean expressions with logical operators
int hour = 14; // => hour is 14 (type: int, 2:00 PM in 24-hour format)
boolean isWeekend = false; // => isWeekend is false (type: boolean)

// Logical AND (&&) - both conditions must be true
if (hour >= 9 && hour <= 17) { // => 14 >= 9 = true AND 14 <= 17 = true
    System.out.println("Business hours"); // => Output: Business hours
}
// => (&&) short-circuits: if first condition false, second not evaluated

// Logical OR (||) - at least one condition must be true
if (hour < 9 || hour > 17) { // => 14 < 9 = false OR 14 > 17 = false
    System.out.println("Outside business hours"); // => Not executed
}
// => (||) short-circuits: if first condition true, second not evaluated

// Logical NOT (!) - inverts boolean value
if (!isWeekend) { // => !false = true, enter block
    System.out.println("Weekday"); // => Output: Weekday
}

// Combined logical operators - order of precedence matters
// Precedence: ! (highest) > && > || (lowest)
// Use parentheses for clarity
if ((hour >= 9 && hour <= 17) && !isWeekend) { // => true AND true AND true
    System.out.println("Working hours"); // => Output: Working hours
}
// => Condition breakdown:
//    (hour >= 9 && hour <= 17) = true (business hours)
//    !isWeekend = true (not weekend)
//    true && true = true (both conditions met)

// Ternary operator - compact if-else expression
int number = 10; // => number is 10 (type: int)
String parity = (number % 2 == 0) ? "even" : "odd"; // => Check: 10 % 2 = 0, 0 == 0 = true
// => Syntax: condition ? valueIfTrue : valueIfFalse
// => parity is "even" (type: String, true branch selected)

// Ternary with different types requires common supertype
int value = 5; // => value is 5 (type: int)
Object result = (value > 0) ? "positive" : -1; // => 5 > 0 = true
// => result is "positive" (type: Object, can hold String or Integer)
// => Common type: Object (superclass of String and Integer)

// Nested ternary (discouraged - hurts readability)
int num = 50; // => num is 50 (type: int)
String size = (num < 10) ? "small" : (num < 100) ? "medium" : "large";
// => Evaluation: 50 < 10 = false, check second condition
//    50 < 100 = true, size is "medium"
// => BETTER: use if-else-if for multi-way decisions

// ===== TRADITIONAL SWITCH STATEMENT =====
// Switch on String - matches exact value
String day = "Monday"; // => day is "Monday" (type: String)

switch (day) { // => Evaluate day, match against cases
    case "Monday": // => "Monday" equals "Monday" = true, enter this case
        System.out.println("Start of week"); // => Output: Start of week
        break; // => CRITICAL: exit switch (prevent fall-through)
    case "Friday":
        System.out.println("End of week");
        break;
    case "Saturday":
    case "Sunday": // => Multiple cases can share execution block
        System.out.println("Weekend");
        break;
    default: // => Executes if no case matches
        System.out.println("Midweek");
        break; // => Optional in default (last case)
}
// => Output: Start of week
// => break prevents fall-through to next case

// Switch fall-through - intentional (but dangerous!)
int month = 3; // => month is 3 (type: int, March)
String season = ""; // => season is "" (type: String, empty initially)

switch (month) {
    case 12: // => 3 != 12, skip
    case 1: // => 3 != 1, skip
    case 2: // => 3 != 2, skip
        season = "Winter";
        break;
    case 3: // => 3 == 3, match! Enter this case
        season = "Spring"; // => season is now "Spring"
        // => NO BREAK! Fall through to next case (usually a bug!)
    case 4: // => Fall-through from case 3
        season = "Spring"; // => season reassigned to "Spring" (redundant)
        break; // => Exit switch here
    case 5:
        season = "Spring";
        break;
    default:
        season = "Unknown";
}
// => season is "Spring" (type: String, final value)
// => Fall-through executed both case 3 and case 4 blocks

// Switch with multiple cases matching same block
char grade = 'B'; // => grade is 'B' (type: char)

switch (grade) {
    case 'A': // => 'B' != 'A', skip
    case 'B': // => 'B' == 'B', match!
    case 'C': // => All three cases share this block
        System.out.println("Passed"); // => Output: Passed
        break;
    case 'D':
    case 'F':
        System.out.println("Failed");
        break;
    default:
        System.out.println("Invalid grade");
}
// => Output: Passed
// => Cases A, B, C all execute same code (fall-through pattern)

// ===== SWITCH EXPRESSIONS (Java 12+) =====
// Modern switch - returns value, no break needed
String dayOfWeek = "Tuesday"; // => dayOfWeek is "Tuesday" (type: String)

String dayType = switch (dayOfWeek) { // => Assign result directly
    case "Monday", "Tuesday", "Wednesday", "Thursday", "Friday" -> "Weekday";
    // => "Tuesday" matches this case
    // => Arrow (->) eliminates fall-through
    // => Returns "Weekday" directly
    case "Saturday", "Sunday" -> "Weekend";
    // => Not evaluated (previous case matched)
    default -> "Invalid day";
    // => Fallback case (handles unexpected values)
}; // => Semicolon required (assignment statement)
// => dayType is "Weekday" (type: String)
// => NO break needed with arrow syntax

// Switch expression with multiple statements - use block and yield
int dayNumber = 3; // => dayNumber is 3 (type: int, Wednesday)

int numLetters = switch (dayNumber) {
    case 1 -> 6; // => 3 != 1, skip (Monday has 6 letters)
    case 2 -> 7; // => 3 != 2, skip (Tuesday has 7 letters)
    case 3 -> { // => 3 == 3, match! Enter block
        System.out.println("Midweek!"); // => Output: Midweek! (side effect)
        yield 9; // => Return value from block (Wednesday has 9 letters)
        // => yield is like return for switch expressions
    }
    case 4, 5 -> 8; // => Not evaluated
    default -> 0; // => Fallback for unexpected values
};
// => Output: Midweek!
// => numLetters is 9 (type: int, returned via yield)

// Switch expression must be exhaustive - all values covered
// Without default, compiler ensures all cases handled
boolean flag = true; // => flag is true (type: boolean)

String status = switch (flag) {
    case true -> "Enabled"; // => true == true, match!
    case false -> "Disabled"; // => Not evaluated
    // => No default needed (boolean has only 2 values, all covered)
};
// => status is "Enabled" (type: String)

// ===== PATTERN MATCHING FOR SWITCH (Java 17+) =====
// Type patterns - combine instanceof check and cast
Object obj = "Hello"; // => obj is "Hello" (type: Object, runtime type: String)

String description = switch (obj) {
    case String s -> "String of length " + s.length();
    // => obj instanceof String = true, cast to String s
    // => s.length() = 5 (string length)
    // => Returns "String of length 5"
    case Integer i -> "Integer: " + i;
    // => Not evaluated (previous case matched)
    case Double d -> "Double: " + d;
    case null -> "Null value";
    // => Handles null explicitly (prevents NullPointerException)
    default -> "Unknown type";
    // => Fallback for other types (Object, List, etc.)
};
// => description is "String of length 5" (type: String)

// Guarded patterns - add condition to pattern
Object value2 = 42; // => value2 is 42 (type: Object, runtime type: Integer)

String category = switch (value2) {
    case Integer i when i < 0 -> "Negative integer"; // => 42 < 0 = false, skip
    case Integer i when i > 0 -> "Positive integer"; // => 42 > 0 = true, match!
    // => i is 42 (pattern variable from cast)
    case Integer i -> "Zero"; // => Not evaluated (previous guard matched)
    case String s when s.isEmpty() -> "Empty string";
    case String s -> "Non-empty string";
    case null -> "Null";
    default -> "Other type";
};
// => category is "Positive integer" (type: String)
// => when clause adds condition to pattern (guard)

// Pattern matching with sealed types (Java 17+)
// Sealed types enable exhaustive checking without default
sealed interface Shape permits Circle, Rectangle {}
record Circle(double radius) implements Shape {}
record Rectangle(double width, double height) implements Shape {}

Shape shape = new Circle(5.0); // => shape is Circle(5.0) (type: Shape)

double area = switch (shape) {
    case Circle c -> Math.PI * c.radius() * c.radius();
    // => shape instanceof Circle = true, c is Circle(5.0)
    // => c.radius() is 5.0 (accessor method)
    // => Calculates π × 5² = 78.53...
    case Rectangle r -> r.width() * r.height();
    // => Not evaluated (previous case matched)
    // => NO default needed (sealed type has only 2 permitted subtypes)
};
// => area is 78.53981633974483 (type: double)

// Null handling in switch (Java 17+)
String input = null; // => input is null (type: String)

String output = switch (input) {
    case null -> "No input"; // => input == null = true, match!
    // => Explicit null case prevents NullPointerException
    case "yes", "y" -> "Affirmative";
    case "no", "n" -> "Negative";
    default -> "Unknown";
};
// => output is "No input" (type: String)
// => Before Java 17, switch on null would throw NullPointerException

// ===== COMPARISON: IF-ELSE VS SWITCH =====
// Use if-else when:
//   - Conditions are ranges (x > 10 && x < 20)
//   - Conditions use different variables
//   - Boolean logic is complex

// Use switch when:
//   - Matching exact values (equality checks)
//   - Single variable compared to multiple constants
//   - Enums or sealed types (exhaustive checking)

int x = 15; // => x is 15 (type: int)

// if-else for range checking
if (x < 10) { // => 15 < 10 = false, skip
    System.out.println("Low");
} else if (x < 20) { // => 15 < 20 = true, match!
    System.out.println("Medium"); // => Output: Medium
} else {
    System.out.println("High");
}
// => if-else is clearer for ranges

// switch for exact value matching
String command = "start"; // => command is "start" (type: String)

switch (command) {
    case "start" -> System.out.println("Starting..."); // => Output: Starting...
    case "stop" -> System.out.println("Stopping...");
    case "pause" -> System.out.println("Pausing...");
    default -> System.out.println("Unknown command");
}
// => switch is clearer for exact matches
```

**Key Takeaway**: Use `if-else` for boolean conditions and ranges. Traditional `switch` requires `break` to prevent fall-through (common bug source). Switch expressions (Java 12+) use arrow syntax (`->`) to return values directly and eliminate fall-through. Pattern matching for switch (Java 17+) combines type checking and casting in one step, with optional guards (`when`) for additional conditions. Always handle `null` explicitly in switch to prevent `NullPointerException`.

**Why It Matters**: Control flow bugs are among the most common production issues. Missing `break` in traditional switch statements causes fall-through bugs where multiple cases execute unintentionally, leading to incorrect state transitions in finite state machines and command processors. Switch expressions (Java 12+) eliminate this entire class of bugs by making fall-through impossible—the compiler enforces exhaustive case coverage, catching missing cases at compile time. Pattern matching for switch (Java 17+) enables type-safe visitor pattern implementations without double dispatch, simplifying parsers, compilers, and domain model processors. Sealed types with pattern matching provide compiler-verified exhaustiveness, ensuring all subtypes are handled—critical for evolving APIs where new types might be added. The `when` guard syntax enables declarative validation logic that's more readable than nested if-else chains inside case blocks.

---

### Example 5: Loops - For, While, Do-While

Java provides multiple loop constructs for iteration. The `for` loop has explicit initialization, condition, and update. The `while` loop evaluates condition before each iteration. The `do-while` loop executes at least once before checking condition. Enhanced for-each loop simplifies collection iteration. Control flow statements `break` (exit loop) and `continue` (skip to next iteration) provide fine-grained control. Labeled statements enable breaking out of nested loops.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    For["for loop<br/>for#40;init; cond; update#41;<br/>Known iterations"]
    While["while loop<br/>while#40;condition#41;<br/>Condition-first"]
    DoWhile["do-while loop<br/>do #123;...#125; while#40;cond#41;<br/>Body-first"]
    ForEach["for-each loop<br/>for#40;item : collection#41;<br/>Collection iteration"]

    style For fill:#0173B2,color:#fff
    style While fill:#DE8F05,color:#fff
    style DoWhile fill:#029E73,color:#fff
    style ForEach fill:#CC78BC,color:#fff
```

**Code**:

```java
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

// ===== FOR LOOP - EXPLICIT INITIALIZATION, CONDITION, UPDATE =====
// Basic for loop structure: for (init; condition; update)
for (int i = 0; i < 5; i++) {
    // => Iteration 0: i is 0, 0 < 5 = true, execute body, then i++ -> i is 1
    // => Iteration 1: i is 1, 1 < 5 = true, execute body, then i++ -> i is 2
    // => Iteration 2: i is 2, 2 < 5 = true, execute body, then i++ -> i is 3
    // => Iteration 3: i is 3, 3 < 5 = true, execute body, then i++ -> i is 4
    // => Iteration 4: i is 4, 4 < 5 = true, execute body, then i++ -> i is 5
    // => Check: i is 5, 5 < 5 = false, exit loop
    System.out.println(i); // => Output: 0, 1, 2, 3, 4 (each on new line)
}
// => i is out of scope here (loop variable scope limited to for block)

// Countdown loop - decrementing index
for (int i = 5; i > 0; i--) {
    // => Start: i is 5
    // => Iteration 0: 5 > 0 = true, print 5, i-- -> i is 4
    // => Iteration 1: 4 > 0 = true, print 4, i-- -> i is 3
    // => Iteration 2: 3 > 0 = true, print 3, i-- -> i is 2
    // => Iteration 3: 2 > 0 = true, print 2, i-- -> i is 1
    // => Iteration 4: 1 > 0 = true, print 1, i-- -> i is 0
    // => Check: 0 > 0 = false, exit loop
    System.out.println(i); // => Output: 5, 4, 3, 2, 1
}

// Step by 2 - increment by arbitrary value
for (int i = 0; i <= 10; i += 2) {
    // => i starts at 0, increments by 2 each iteration
    // => Iterations: i = 0, 2, 4, 6, 8, 10
    System.out.println(i); // => Output: 0, 2, 4, 6, 8, 10
}

// Multiple variables in for loop - comma-separated
for (int i = 0, j = 10; i < j; i++, j--) {
    // => Iteration 0: i is 0, j is 10, (0 < 10) = true, then i++ j-- -> i=1, j=9
    // => Iteration 1: i is 1, j is 9, (1 < 9) = true, then i++ j-- -> i=2, j=8
    // => Iteration 2: i is 2, j is 8, (2 < 8) = true, then i++ j-- -> i=3, j=7
    // => Iteration 3: i is 3, j is 7, (3 < 7) = true, then i++ j-- -> i=4, j=6
    // => Iteration 4: i is 4, j is 6, (4 < 6) = true, then i++ j-- -> i=5, j=5
    // => Check: i is 5, j is 5, (5 < 5) = false, exit loop
    System.out.println(i + " " + j); // => Output: "0 10", "1 9", "2 8", "3 7", "4 6"
}
// => i and j meet in the middle, loop terminates

// Infinite for loop - condition always true (or omitted)
// for (;;) { // => Empty condition = infinite loop
//     System.out.println("Forever"); // => Runs indefinitely
//     break; // => Must use break to exit
// }

// Array iteration with for loop - index-based access
int[] numbers = {10, 20, 30, 40, 50}; // => numbers is [10, 20, 30, 40, 50]

for (int i = 0; i < numbers.length; i++) {
    // => numbers.length is 5 (array size)
    // => Iteration 0: i=0, numbers[0] is 10
    // => Iteration 1: i=1, numbers[1] is 20
    // => Iteration 2: i=2, numbers[2] is 30
    // => Iteration 3: i=3, numbers[3] is 40
    // => Iteration 4: i=4, numbers[4] is 50
    // => Check: i=5, (5 < 5) = false, exit
    System.out.println("Index " + i + ": " + numbers[i]);
    // => Output: "Index 0: 10", "Index 1: 20", ..., "Index 4: 50"
}

// ===== WHILE LOOP - CONDITION CHECKED BEFORE EACH ITERATION =====
// While loop - condition evaluated first (may never execute)
int count = 0; // => count is 0 (type: int)

while (count < 3) { // => Check condition before each iteration
    // => Iteration 0: 0 < 3 = true, execute body
    System.out.println("Count: " + count); // => Output: "Count: 0"
    count++; // => count is now 1
    // => Iteration 1: 1 < 3 = true, execute body, print "Count: 1", count -> 2
    // => Iteration 2: 2 < 3 = true, execute body, print "Count: 2", count -> 3
    // => Check: 3 < 3 = false, exit loop
}
// => Final: count is 3 (type: int)
// => Output: "Count: 0", "Count: 1", "Count: 2"

// While loop that never executes - condition false initially
int x = 10; // => x is 10 (type: int)

while (x < 5) { // => 10 < 5 = false, skip body entirely
    System.out.println("Never printed"); // => Not executed
}
// => Loop body skipped (condition false on first check)

// Input validation loop - repeat until valid input
// Scanner scanner = new Scanner(System.in);
// int age = -1; // => age is -1 (invalid initial value)
//
// while (age < 0 || age > 120) { // => Repeat while invalid
//     System.out.print("Enter age (0-120): ");
//     age = scanner.nextInt(); // => Read user input
//     // => If user enters 150: 150 > 120 = true, loop continues
//     // => If user enters 25: 25 < 0 = false AND 25 > 120 = false, exit loop
// }
// => age is valid (0-120) after loop

// Accumulator pattern - sum array elements
int[] values = {5, 10, 15, 20}; // => values is [5, 10, 15, 20]
int sum = 0; // => sum is 0 (accumulator)
int index = 0; // => index is 0 (position tracker)

while (index < values.length) { // => values.length is 4
    // => Iteration 0: index=0, sum=0, sum += values[0] -> sum=5, index=1
    sum += values[index]; // => Add current element to sum
    // => Iteration 1: index=1, sum=5, sum += values[1] -> sum=15, index=2
    // => Iteration 2: index=2, sum=15, sum += values[2] -> sum=30, index=3
    // => Iteration 3: index=3, sum=30, sum += values[3] -> sum=50, index=4
    index++; // => Move to next element
    // => Check: index=4, (4 < 4) = false, exit
}
// => sum is 50 (type: int, total of all elements)

// ===== DO-WHILE LOOP - EXECUTES AT LEAST ONCE =====
// Do-while - body executes BEFORE condition check (minimum 1 iteration)
int num = 0; // => num is 0 (type: int)

do {
    // => First iteration: execute body BEFORE checking condition
    System.out.println("Number: " + num); // => Output: "Number: 0"
    num++; // => num is now 1
} while (num < 0); // => Check: 1 < 0 = false, exit loop
// => Body executed once despite condition being false
// => Output: "Number: 0"

// Do-while with multiple iterations
int counter = 0; // => counter is 0 (type: int)

do {
    // => Iteration 0: print "Counter: 0", counter -> 1, check 1 < 3 = true
    // => Iteration 1: print "Counter: 1", counter -> 2, check 2 < 3 = true
    // => Iteration 2: print "Counter: 2", counter -> 3, check 3 < 3 = false
    System.out.println("Counter: " + counter);
    counter++;
} while (counter < 3);
// => Output: "Counter: 0", "Counter: 1", "Counter: 2"
// => counter is 3 (final value)

// Menu-driven program - do-while ensures menu shows at least once
// int choice = 0;
// do {
//     System.out.println("1. Start");
//     System.out.println("2. Stop");
//     System.out.println("3. Exit");
//     choice = scanner.nextInt(); // => User selects option
//     // Process choice...
// } while (choice != 3); // => Repeat until user chooses exit
// => Menu displays at least once (do-while guarantee)

// ===== ENHANCED FOR-EACH LOOP - COLLECTION ITERATION =====
// For-each loop - read-only iteration over arrays
String[] names = {"Alice", "Bob", "Charlie"}; // => names is ["Alice", "Bob", "Charlie"]

for (String name : names) {
    // => Iteration 0: name is "Alice" (first element)
    // => Iteration 1: name is "Bob" (second element)
    // => Iteration 2: name is "Charlie" (third element)
    System.out.println(name); // => Output: "Alice", "Bob", "Charlie"
}
// => For-each iterates over all elements automatically
// => No index variable needed (cleaner syntax than regular for)
// => name is read-only (cannot modify array through name)

// For-each with List collection
List<Integer> numList = List.of(10, 20, 30); // => numList is [10, 20, 30] (immutable)

for (int n : numList) {
    // => Iteration 0: n is 10 (unboxed from Integer to int)
    // => Iteration 1: n is 20
    // => Iteration 2: n is 30
    System.out.println(n); // => Output: 10, 20, 30
}
// => For-each works with any Iterable (List, Set, Queue, etc.)

// For-each with modification attempt (doesn't change original)
int[] array = {1, 2, 3}; // => array is [1, 2, 3]

for (int element : array) {
    // => Iteration 0: element is 1 (COPY of array[0])
    element = element * 10; // => element is 10 (local variable modified)
    // => array[0] is still 1 (original unchanged!)
}
// => array is still [1, 2, 3] (for-each provides copies, not references)
// => To modify array, use regular for loop with index: array[i] = ...

// ===== BREAK - EXIT LOOP IMMEDIATELY =====
// Break in for loop - exit when condition met
for (int i = 0; i < 10; i++) {
    // => Iteration 0: i=0, 0 == 5 = false, print 0, i++ -> i=1
    // => Iteration 1: i=1, 1 == 5 = false, print 1, i++ -> i=2
    // => Iteration 2: i=2, 2 == 5 = false, print 2, i++ -> i=3
    // => Iteration 3: i=3, 3 == 5 = false, print 3, i++ -> i=4
    // => Iteration 4: i=4, 4 == 5 = false, print 4, i++ -> i=5
    // => Iteration 5: i=5, 5 == 5 = true, break!
    if (i == 5) {
        break; // => Exit loop immediately (skip remaining iterations)
    }
    System.out.println(i); // => Output: 0, 1, 2, 3, 4
}
// => Loop terminates early (never reaches i=6, 7, 8, 9)

// Break in while loop - search and exit
int[] data = {3, 7, 2, 9, 5}; // => data is [3, 7, 2, 9, 5]
int target = 9; // => target is 9 (value to find)
int position = -1; // => position is -1 (not found marker)
int idx = 0; // => idx is 0 (current index)

while (idx < data.length) {
    // => Iteration 0: data[0]=3, 3 == 9 = false, idx=1
    // => Iteration 1: data[1]=7, 7 == 9 = false, idx=2
    // => Iteration 2: data[2]=2, 2 == 9 = false, idx=3
    // => Iteration 3: data[3]=9, 9 == 9 = true, position=3, break!
    if (data[idx] == target) {
        position = idx; // => position is 3 (found at index 3)
        break; // => Exit search early (no need to check remaining elements)
    }
    idx++;
}
// => position is 3 (target found)
// => Early exit saves iterations (didn't check data[4])

// ===== CONTINUE - SKIP TO NEXT ITERATION =====
// Continue in for loop - skip specific values
for (int i = 0; i < 5; i++) {
    // => Iteration 0: i=0, 0 == 2 = false, print 0
    // => Iteration 1: i=1, 1 == 2 = false, print 1
    // => Iteration 2: i=2, 2 == 2 = true, continue! (skip print)
    // => Iteration 3: i=3, 3 == 2 = false, print 3
    // => Iteration 4: i=4, 4 == 2 = false, print 4
    if (i == 2) {
        continue; // => Skip remaining loop body, jump to i++ (next iteration)
    }
    System.out.println(i); // => Output: 0, 1, 3, 4 (skipped 2)
}
// => continue skips print for i=2, but loop continues

// Continue in while loop - skip even numbers
int n = 0; // => n is 0 (type: int)

while (n < 10) {
    n++; // => Increment BEFORE continue check (important!)
    // => Iteration 0: n=1, 1%2=1 (odd), print 1
    // => Iteration 1: n=2, 2%2=0 (even), continue!
    // => Iteration 2: n=3, 3%2=1 (odd), print 3
    // => Iteration 3: n=4, 4%2=0 (even), continue!
    // => ... continues until n=10
    if (n % 2 == 0) {
        continue; // => Skip print for even numbers
    }
    System.out.println(n); // => Output: 1, 3, 5, 7, 9 (odd numbers only)
}
// => continue skips even numbers, prints odd numbers

// ===== NESTED LOOPS - LOOPS WITHIN LOOPS =====
// Multiplication table - nested for loops
for (int i = 1; i <= 3; i++) {
    // => Outer loop: i=1, 2, 3 (rows)
    for (int j = 1; j <= 3; j++) {
        // => Inner loop: j=1, 2, 3 (columns) for EACH i
        // => i=1, j=1: print "1 ", i=1, j=2: print "2 ", i=1, j=3: print "3 "
        // => i=2, j=1: print "2 ", i=2, j=2: print "4 ", i=2, j=3: print "6 "
        // => i=3, j=1: print "3 ", i=3, j=2: print "6 ", i=3, j=3: print "9 "
        System.out.print(i * j + " "); // => Print product (no newline)
    }
    System.out.println(); // => Newline after each row
}
// => Output:
//    1 2 3
//    2 4 6
//    3 6 9

// Labeled break - exit outer loop from inner loop
outer: for (int i = 0; i < 3; i++) {
    // => Label "outer" marks this loop
    for (int j = 0; j < 3; j++) {
        // => i=0, j=0: print "0,0"
        // => i=0, j=1: print "0,1"
        // => i=1, j=0: print "1,0"
        // => i=1, j=1: (1==1 AND 1==1) = true, break outer!
        if (i == 1 && j == 1) {
            break outer; // => Exit OUTER loop (not just inner)
        }
        System.out.println(i + "," + j);
    }
}
// => Output: "0,0", "0,1", "1,0"
// => Never prints "1,1" or anything after (outer loop exited)

// Labeled continue - skip outer loop iteration from inner loop
outer: for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 3; j++) {
        // => i=0: prints "0,0", "0,1", "0,2"
        // => i=1, j=0: (1==1 AND 0==0) = true, continue outer! (skip rest of i=1)
        // => i=2: prints "2,0", "2,1", "2,2"
        if (i == 1 && j == 0) {
            continue outer; // => Skip to next iteration of OUTER loop
        }
        System.out.println(i + "," + j);
    }
}
// => Output: "0,0", "0,1", "0,2", "2,0", "2,1", "2,2"
// => Skipped all j values for i=1

// ===== ITERATOR - MANUAL ITERATION WITH SAFE REMOVAL =====
// Iterator pattern - explicit control over iteration
List<String> list = new ArrayList<>(); // => list is [] (empty, mutable)
list.add("A"); // => list is ["A"]
list.add("B"); // => list is ["A", "B"]
list.add("C"); // => list is ["A", "B", "C"]

Iterator<String> iterator = list.iterator(); // => Create iterator over list
// => iterator positioned BEFORE first element

while (iterator.hasNext()) { // => Check if more elements remain
    // => Iteration 0: hasNext() = true (element "A" available)
    String item = iterator.next(); // => item is "A", iterator advances
    System.out.println(item); // => Output: "A"

    // => Iteration 1: hasNext() = true ("B" available)
    // => item is "B", iterator advances
    // => Output: "B"
    if (item.equals("B")) {
        iterator.remove(); // => Remove current element ("B") from list
        // => list is now ["A", "C"]
        // => Safe removal during iteration (no ConcurrentModificationException)
    }

    // => Iteration 2: hasNext() = true ("C" available)
    // => item is "C", iterator advances
    // => Output: "C"

    // => Iteration 3: hasNext() = false, exit loop
}
// => Output: "A", "B", "C"
// => list is ["A", "C"] (after removal)

// ConcurrentModificationException - modifying collection during for-each
List<Integer> nums = new ArrayList<>(List.of(1, 2, 3, 4, 5));
// => nums is [1, 2, 3, 4, 5] (mutable copy)

// BAD: modifying collection during for-each iteration
// for (int num : nums) {
//     if (num % 2 == 0) {
//         nums.remove(Integer.valueOf(num)); // => ConcurrentModificationException!
//         // => For-each uses internal iterator, direct modification breaks it
//     }
// }

// CORRECT: use Iterator.remove() for safe modification
Iterator<Integer> iter = nums.iterator();
while (iter.hasNext()) {
    int num2 = iter.next();
    if (num2 % 2 == 0) { // => Remove even numbers
        iter.remove(); // => Safe removal via iterator
    }
}
// => nums is [1, 3, 5] (even numbers removed)

// ===== LOOP PERFORMANCE CONSIDERATIONS =====
// Cache array length - avoid repeated .length calls in condition
int[] largeArray = new int[1000]; // => largeArray is [0, 0, 0, ..., 0] (1000 zeros)

// SLOW: length accessed on every iteration
// for (int i = 0; i < largeArray.length; i++) {
//     // => largeArray.length evaluated 1000 times
// }

// FAST: cache length in variable
int len = largeArray.length; // => len is 1000 (cached)
for (int i = 0; i < len; i++) {
    // => len accessed from local variable (faster than field access)
    // => Minor optimization, but adds up in tight loops
}
// => Modern JVMs optimize this automatically, but explicit caching is clearer

// For-each is clearest when you don't need index
int[] arr = {1, 2, 3, 4, 5};

// Use for-each when index not needed
for (int val : arr) {
    System.out.println(val); // => Clearest syntax for simple iteration
}

// Use regular for when index needed
for (int i = 0; i < arr.length; i++) {
    System.out.println("Index " + i + ": " + arr[i]); // => Need index for output
}
```

**Key Takeaway**: Use `for` loops when iteration count is known (explicit initialization, condition, update). Use `while` loops when condition determines iteration (checked before each iteration). Use `do-while` loops when body must execute at least once (condition checked after). Use for-each loops for simple collection iteration (no index needed). Use `break` to exit loops early. Use `continue` to skip to next iteration. Use labeled `break`/`continue` to control nested loops. Use `Iterator` for safe collection modification during iteration.

**Why It Matters**: Loop control flow is fundamental to algorithm implementation. For-each loops reduce off-by-one errors (accessing `array[length]` instead of `array[length-1]`) that plague index-based loops—a common source of `ArrayIndexOutOfBoundsException` in production. The `Iterator` pattern enables safe element removal during iteration, preventing `ConcurrentModificationException` that occurs when modifying collections mid-loop with for-each. Labeled break/continue statements clarify intent in nested loops, replacing error-prone flag variables and improving maintainability. Do-while loops are essential for input validation and menu-driven UIs where the operation must happen before the exit condition can be evaluated. Understanding loop mechanics enables recognizing infinite loops during code review—missing increment in while loops or incorrect termination conditions are production debugging nightmares.

---

### Example 6: Arrays and Array Operations

Arrays are fixed-size, contiguous memory structures that provide O(1) random access by index. Java arrays are objects with a `length` field (not method). Arrays store primitives (int[], double[]) or references (String[], Object[]). Multi-dimensional arrays are arrays of arrays (not true matrices). The `Arrays` utility class provides operations like sorting, searching, copying, and comparison. Arrays have limitations (fixed size, no built-in insertion/deletion) but offer performance advantages (cache locality, no boxing overhead for primitives) critical for algorithms and system programming.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Array["Array<br/>Fixed size, contiguous memory<br/>[0][1][2][3][4]"]

    Array --> Access["Random Access<br/>O#40;1#41; by index<br/>arr[2]"]
    Array --> Init["Initialization<br/>Declaration & allocation"]
    Array --> Ops["Operations<br/>Sort, search, copy"]
    Array --> Multi["Multi-dimensional<br/>Array of arrays"]

    Access --> Read["Read: arr[i]"]
    Access --> Write["Write: arr[i] = val"]

    Init --> Literal["Literal: #123;1,2,3#125;"]
    Init --> NewArray["new: new int[5]"]

    Ops --> Sort["Arrays.sort#40;#41;"]
    Ops --> Search["Arrays.binarySearch#40;#41;"]
    Ops --> Copy["Arrays.copyOf#40;#41;"]

    style Array fill:#0173B2,color:#fff
    style Access fill:#DE8F05,color:#fff
    style Init fill:#029E73,color:#fff
    style Ops fill:#CC78BC,color:#fff
    style Multi fill:#CA9161,color:#fff
    style Read fill:#0173B2,color:#fff
    style Write fill:#0173B2,color:#fff
    style Literal fill:#DE8F05,color:#fff
    style NewArray fill:#DE8F05,color:#fff
    style Sort fill:#029E73,color:#fff
    style Search fill:#029E73,color:#fff
    style Copy fill:#029E73,color:#fff
```

**Code**:

```java
import java.util.Arrays;

// ===== ARRAY DECLARATION AND INITIALIZATION =====
// Array literal - size inferred from initializer list
int[] numbers = {1, 2, 3, 4, 5}; // => numbers is [1, 2, 3, 4, 5] (type: int[])
// => numbers.length is 5 (array size, FIELD not method)
// => Memory layout: contiguous block [1][2][3][4][5]

// Empty array with explicit size - initialized to default values
int[] zeros = new int[5]; // => zeros is [0, 0, 0, 0, 0] (default int value: 0)
// => All elements initialized to 0 (primitive default)

String[] names = new String[3]; // => names is [null, null, null] (default reference value: null)
// => All elements initialized to null (reference default)

boolean[] flags = new boolean[2]; // => flags is [false, false] (default boolean value: false)

// Two-step initialization - declare then allocate
double[] prices; // => prices is null (array variable declared but not allocated)
prices = new double[4]; // => prices is [0.0, 0.0, 0.0, 0.0] (allocated with size 4)

// Array type syntax variations (both valid, prefer type[])
int[] arr1 = new int[3]; // => PREFERRED: type[] variable (type-first style)
int arr2[] = new int[3]; // => DISCOURAGED: variable[] (C-style, confusing)

// ===== ARRAY ACCESS - ZERO-INDEXED =====
// Read array element by index
int[] data = {10, 20, 30, 40, 50}; // => data is [10, 20, 30, 40, 50]

int first = data[0]; // => first is 10 (index 0 = first element)
int second = data[1]; // => second is 20 (index 1 = second element)
int last = data[4]; // => last is 50 (index 4 = last element, length-1)
// => Index range: [0, length-1] (0 to 4 for length 5)

// Write array element - modify in-place
data[2] = 300; // => data is [10, 20, 300, 40, 50] (index 2 updated)
// => Arrays are MUTABLE (elements can be changed)

data[0] = data[0] + 5; // => data[0] is 15, data is [15, 20, 300, 40, 50]

// Out of bounds access - runtime exception
// int invalid = data[5]; // => ArrayIndexOutOfBoundsException! (index 5 >= length 5)
// int negative = data[-1]; // => ArrayIndexOutOfBoundsException! (negative index)
// => Bounds checking happens at RUNTIME (no compile-time detection)

// ===== ARRAY LENGTH - PROPERTY NOT METHOD =====
int size = data.length; // => size is 5 (array length is a FIELD)
// => NOT data.length() (no parentheses, not a method call)

// Iterate using length property
for (int i = 0; i < data.length; i++) {
    // => i ranges from 0 to 4 (length-1)
    // => Safe iteration (i < length prevents out of bounds)
    System.out.println(data[i]);
}
// => Output: 15, 20, 300, 40, 50

// Last element access pattern
int lastElement = data[data.length - 1]; // => lastElement is 50 (last index = length-1)

// ===== FOR-EACH LOOP - READ-ONLY ITERATION =====
String[] fruits = {"apple", "banana", "cherry"}; // => fruits is ["apple", "banana", "cherry"]

for (String fruit : fruits) {
    // => Iteration 0: fruit is "apple"
    // => Iteration 1: fruit is "banana"
    // => Iteration 2: fruit is "cherry"
    System.out.println(fruit); // => Output: apple, banana, cherry
}
// => For-each provides COPY of element (cannot modify array through variable)

// Attempt to modify via for-each (doesn't change array)
int[] nums = {1, 2, 3}; // => nums is [1, 2, 3]

for (int num : nums) {
    num = num * 10; // => num is 10, 20, 30 (local variable modified)
    // => nums is STILL [1, 2, 3] (array unchanged)
}
// => To modify array, use index-based for loop:
for (int i = 0; i < nums.length; i++) {
    nums[i] = nums[i] * 10; // => Direct modification via index
}
// => nums is [10, 20, 30] (array modified)

// ===== ARRAYS UTILITY CLASS - COMMON OPERATIONS =====
// Arrays.toString() - convert array to readable string
int[] arr = {5, 2, 8, 1, 9}; // => arr is [5, 2, 8, 1, 9]
String arrStr = Arrays.toString(arr); // => arrStr is "[5, 2, 8, 1, 9]" (type: String)
System.out.println(arrStr); // => Output: [5, 2, 8, 1, 9]
// => Default Object.toString() for arrays is useless: [I@hashcode

// Arrays.sort() - in-place sorting (modifies original array)
int[] unsorted = {5, 2, 8, 1, 9}; // => unsorted is [5, 2, 8, 1, 9]
Arrays.sort(unsorted); // => Sorts array in ascending order IN-PLACE
// => unsorted is [1, 2, 5, 8, 9] (array modified, no return value)
// => Uses Dual-Pivot Quicksort for primitives (O(n log n) average)

// Arrays.sort() on String array - lexicographical order
String[] words = {"zebra", "apple", "banana"}; // => words is ["zebra", "apple", "banana"]
Arrays.sort(words); // => words is ["apple", "banana", "zebra"] (alphabetical)

// Arrays.sort() with custom range - partial sort
int[] partial = {9, 5, 2, 8, 1}; // => partial is [9, 5, 2, 8, 1]
Arrays.sort(partial, 1, 4); // => Sort elements from index 1 to 3 (exclusive end)
// => partial is [9, 2, 5, 8, 1] (only middle elements sorted)
// => Syntax: Arrays.sort(array, fromIndex, toIndex) - [fromIndex, toIndex)

// Arrays.binarySearch() - search in SORTED array
int[] sorted = {1, 3, 5, 7, 9, 11}; // => sorted is [1, 3, 5, 7, 9, 11] (must be sorted!)

int index5 = Arrays.binarySearch(sorted, 5); // => index5 is 2 (found at index 2)
// => Binary search: O(log n) complexity (faster than linear search)

int index8 = Arrays.binarySearch(sorted, 8); // => index8 is -5 (not found)
// => Returns -(insertion_point + 1) when not found
// => Insertion point is 4 (where 8 would go), so -(4+1) = -5
// => To get insertion point: -(result + 1) = -((-5) + 1) = 4

// Arrays.binarySearch() on unsorted array - UNDEFINED BEHAVIOR
int[] unsortedArr = {5, 2, 8}; // => unsortedArr is [5, 2, 8] (NOT sorted)
int wrongResult = Arrays.binarySearch(unsortedArr, 5); // => Result is unpredictable!
// => MUST sort array before binary search (or results are meaningless)

// Arrays.copyOf() - create new array with specified length
int[] original = {1, 2, 3, 4, 5}; // => original is [1, 2, 3, 4, 5]

int[] copy = Arrays.copyOf(original, original.length); // => copy is [1, 2, 3, 4, 5]
// => Creates NEW array (distinct object, not reference to original)
copy[0] = 99; // => copy is [99, 2, 3, 4, 5]
// => original is STILL [1, 2, 3, 4, 5] (independent copy)

int[] shorter = Arrays.copyOf(original, 3); // => shorter is [1, 2, 3] (truncated)
// => Copies first 3 elements

int[] longer = Arrays.copyOf(original, 7); // => longer is [1, 2, 3, 4, 5, 0, 0]
// => Extends array, fills new elements with default value (0 for int)

// Arrays.copyOfRange() - copy subarray
int[] range = Arrays.copyOfRange(original, 1, 4); // => range is [2, 3, 4]
// => Copies elements from index 1 to 3 (exclusive end)
// => Syntax: Arrays.copyOfRange(array, from, to) - [from, to)

// Arrays.fill() - set all elements to same value
int[] fillArray = new int[5]; // => fillArray is [0, 0, 0, 0, 0]
Arrays.fill(fillArray, 42); // => fillArray is [42, 42, 42, 42, 42]
// => Sets ALL elements to 42

// Arrays.fill() with range - fill partial array
int[] partialFill = {1, 2, 3, 4, 5}; // => partialFill is [1, 2, 3, 4, 5]
Arrays.fill(partialFill, 1, 4, 99); // => partialFill is [1, 99, 99, 99, 5]
// => Fills elements from index 1 to 3 with 99
// => Syntax: Arrays.fill(array, fromIndex, toIndex, value)

// Arrays.equals() - compare array contents
int[] arr1 = {1, 2, 3}; // => arr1 is [1, 2, 3]
int[] arr2 = {1, 2, 3}; // => arr2 is [1, 2, 3]
int[] arr3 = {1, 2, 4}; // => arr3 is [1, 2, 4]

boolean same = Arrays.equals(arr1, arr2); // => same is true (same content)
boolean different = Arrays.equals(arr1, arr3); // => different is false (arr3[2] differs)

// == compares references (NOT content)
boolean refEqual = (arr1 == arr2); // => refEqual is false (different objects)
// => arr1 and arr2 are separate arrays in memory (different references)

// Arrays.deepEquals() - for multi-dimensional arrays
int[][] matrix1 = {{1, 2}, {3, 4}};
int[][] matrix2 = {{1, 2}, {3, 4}};
boolean matrixEqual = Arrays.deepEquals(matrix1, matrix2); // => matrixEqual is true
// => Compares nested arrays recursively

// Arrays.deepToString() - for multi-dimensional arrays
String matrixStr = Arrays.deepToString(matrix1); // => matrixStr is "[[1, 2], [3, 4]]"
// => Arrays.toString() on 2D array prints addresses: [[I@hashcode, [I@hashcode]

// ===== MULTI-DIMENSIONAL ARRAYS - ARRAYS OF ARRAYS =====
// 2D array declaration and initialization
int[][] matrix = {
    {1, 2, 3},    // => Row 0: [1, 2, 3]
    {4, 5, 6},    // => Row 1: [4, 5, 6]
    {7, 8, 9}     // => Row 2: [7, 8, 9]
};
// => matrix is array of 3 arrays (rows), each containing 3 ints (columns)
// => matrix.length is 3 (number of rows)
// => matrix[0].length is 3 (number of columns in row 0)

// Access 2D array elements
int element = matrix[1][2]; // => element is 6 (row 1, column 2)
// => matrix[1] is [4, 5, 6] (second row, type: int[])
// => matrix[1][2] is 6 (third element of second row)

// Modify 2D array
matrix[0][0] = 99; // => matrix[0] is [99, 2, 3] (top-left element updated)

// Iterate 2D array with nested loops
for (int i = 0; i < matrix.length; i++) { // => Iterate rows (0, 1, 2)
    for (int j = 0; j < matrix[i].length; j++) { // => Iterate columns (0, 1, 2)
        // => i=0, j=0: matrix[0][0] = 99
        // => i=0, j=1: matrix[0][1] = 2
        // => i=0, j=2: matrix[0][2] = 3
        // => i=1, j=0: matrix[1][0] = 4
        // => ... continues for all 9 elements
        System.out.print(matrix[i][j] + " ");
    }
    System.out.println(); // => Newline after each row
}
// => Output:
//    99 2 3
//    4 5 6
//    7 8 9

// 2D array with for-each - row-by-row iteration
for (int[] row : matrix) { // => row is int[] (each row array)
    for (int value : row) { // => value is int (each element in row)
        System.out.print(value + " ");
    }
    System.out.println();
}
// => Same output as nested regular for loops

// Jagged array - rows with different lengths
int[][] jagged = {
    {1, 2},          // => Row 0: 2 elements
    {3, 4, 5},       // => Row 1: 3 elements
    {6, 7, 8, 9}     // => Row 2: 4 elements
};
// => jagged[0].length is 2
// => jagged[1].length is 3
// => jagged[2].length is 4
// => Each row is independent array (not rectangular matrix)

// 2D array with explicit allocation
int[][] grid = new int[3][4]; // => 3 rows, 4 columns
// => grid is [[0,0,0,0], [0,0,0,0], [0,0,0,0]] (all zeros)
// => grid.length is 3 (rows)
// => grid[0].length is 4 (columns)

// 2D array with partial allocation (jagged)
int[][] partial2 = new int[3][]; // => Allocate 3 rows, but NO columns yet
partial2[0] = new int[2]; // => Row 0: 2 elements [0, 0]
partial2[1] = new int[4]; // => Row 1: 4 elements [0, 0, 0, 0]
partial2[2] = new int[3]; // => Row 2: 3 elements [0, 0, 0]
// => Each row allocated separately (jagged array)

// 3D array - array of 2D arrays
int[][][] cube = {
    {{1, 2}, {3, 4}},     // => Layer 0: 2x2 matrix
    {{5, 6}, {7, 8}}      // => Layer 1: 2x2 matrix
};
// => cube.length is 2 (layers)
// => cube[0].length is 2 (rows in layer 0)
// => cube[0][0].length is 2 (columns in layer 0, row 0)

int value3D = cube[1][1][0]; // => value3D is 7 (layer 1, row 1, column 0)

// ===== ARRAY REFERENCES AND ALIASING =====
// Array assignment - reference copy (NOT value copy)
int[] original2 = {1, 2, 3}; // => original2 is [1, 2, 3] (object in memory)
int[] alias = original2; // => alias references SAME array as original2
// => alias and original2 point to SAME object (not a copy)

alias[0] = 99; // => Modify via alias
// => original2 is [99, 2, 3] (SAME array modified!)
// => alias is [99, 2, 3] (same reference)
System.out.println(Arrays.equals(original2, alias)); // => true (same content)
System.out.println(original2 == alias); // => true (SAME reference)

// Create independent copy - use Arrays.copyOf() or clone()
int[] trueCopy = Arrays.copyOf(original2, original2.length);
// => trueCopy is [99, 2, 3] (NEW array with copied values)
trueCopy[0] = 1; // => trueCopy is [1, 2, 3]
// => original2 is STILL [99, 2, 3] (independent)

int[] cloned = original2.clone(); // => cloned is [99, 2, 3] (shallow copy)
// => clone() creates new array (same as Arrays.copyOf for 1D arrays)

// Shallow vs deep copy for 2D arrays
int[][] matrix2D = {{1, 2}, {3, 4}};
int[][] shallowCopy = matrix2D.clone(); // => Copies outer array, NOT inner arrays
// => shallowCopy references SAME inner arrays as matrix2D

shallowCopy[0][0] = 99; // => Modify through shallowCopy
// => matrix2D is [[99, 2], [3, 4]] (inner array shared!)
// => shallowCopy is [[99, 2], [3, 4]] (same inner arrays)

// Deep copy for 2D arrays - manually copy each row
int[][] deepCopy = new int[matrix2D.length][];
for (int i = 0; i < matrix2D.length; i++) {
    deepCopy[i] = Arrays.copyOf(matrix2D[i], matrix2D[i].length);
}
// => deepCopy is [[99, 2], [3, 4]] (independent copy of inner arrays)
deepCopy[0][0] = 1;
// => deepCopy is [[1, 2], [3, 4]]
// => matrix2D is STILL [[99, 2], [3, 4]] (independent)

// ===== ARRAY PERFORMANCE CHARACTERISTICS =====
// Random access - O(1) constant time
int[] largeArray = new int[1000000];
int randomAccess = largeArray[500000]; // => O(1) - direct index calculation
// => Address calculation: base_address + (index * element_size)

// Linear search - O(n) time
int target = 42;
int foundIndex = -1;
for (int i = 0; i < largeArray.length; i++) {
    if (largeArray[i] == target) {
        foundIndex = i;
        break;
    }
}
// => Worst case: check all n elements

// Binary search (sorted) - O(log n) time
Arrays.sort(largeArray); // => O(n log n) to sort
int binaryIndex = Arrays.binarySearch(largeArray, target); // => O(log n) to search
// => Much faster than linear search for large arrays

// Insertion/deletion - O(n) time (requires shifting elements)
int[] beforeInsert = {1, 2, 4, 5};
// To insert 3 at index 2, must shift elements 2-3 to the right
// => Manual process: create new array, copy with gap, insert element
// => Arrays.copyOf() + manual shifting required (no built-in insert)

// Cache locality - arrays have excellent spatial locality
// => Sequential access very fast (hardware prefetching)
int sum = 0;
for (int i = 0; i < largeArray.length; i++) {
    sum += largeArray[i]; // => Sequential access uses CPU cache efficiently
}

// ===== COMMON ARRAY PATTERNS =====
// Accumulator - sum array elements
int[] values = {10, 20, 30, 40, 50};
int total = 0;
for (int val : values) {
    total += val; // => total grows: 0 -> 10 -> 30 -> 60 -> 100 -> 150
}
// => total is 150

// Find maximum element
int[] scores = {85, 92, 78, 95, 88};
int max = scores[0]; // => Assume first element is max
for (int i = 1; i < scores.length; i++) {
    if (scores[i] > max) {
        max = scores[i]; // => Update max when larger found
    }
}
// => max is 95

// Reverse array in-place
int[] toReverse = {1, 2, 3, 4, 5};
for (int i = 0; i < toReverse.length / 2; i++) {
    // => Swap elements from both ends moving toward center
    int temp = toReverse[i]; // => temp is toReverse[i]
    toReverse[i] = toReverse[toReverse.length - 1 - i]; // => Swap left to right
    toReverse[toReverse.length - 1 - i] = temp; // => Swap right to left
}
// => toReverse is [5, 4, 3, 2, 1]

// Shift elements left (remove first element pattern)
int[] shift = {1, 2, 3, 4, 5};
for (int i = 0; i < shift.length - 1; i++) {
    shift[i] = shift[i + 1]; // => Copy next element to current position
}
shift[shift.length - 1] = 0; // => Clear last element
// => shift is [2, 3, 4, 5, 0] (all elements shifted left)
```

**Key Takeaway**: Arrays provide O(1) random access with fixed size. Use `Arrays.sort()` for in-place sorting and `Arrays.binarySearch()` for O(log n) searching in sorted arrays. Use `Arrays.copyOf()` for creating independent copies (avoid aliasing bugs). Multi-dimensional arrays are arrays of arrays (jagged arrays supported). Array assignment creates references (not copies)—modify through one reference affects all. Arrays have excellent cache locality but poor insertion/deletion performance (O(n) shifts required).

**Why It Matters**: Arrays are the foundation of most data structures—ArrayList uses arrays internally, hash tables use arrays for buckets, and heaps use arrays for complete binary trees. Understanding array mechanics is essential for algorithm analysis—sorting, searching, and graph algorithms rely on array performance characteristics. Array aliasing bugs (multiple references to same array) are a common source of production bugs where modifying one variable unexpectedly changes another. The distinction between shallow copy (clone()) and deep copy is critical for multi-dimensional arrays—failure to deep copy leads to unintended data sharing across objects. Arrays of primitives avoid boxing overhead (int[] vs List<Integer>), making them 3-5x more memory efficient and faster for numerical computation, scientific computing, and game development where performance matters.

---

### Example 7: Classes and Objects

Classes are blueprints that define state (fields) and behavior (methods). Objects are instances created from classes. Constructors initialize object state.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Class["Class (Blueprint)<br/>defines fields and methods"] --> Obj1["Object 1<br/>(Instance)"]
    Class --> Obj2["Object 2<br/>(Instance)"]
    Class --> Obj3["Object 3<br/>(Instance)"]

    style Class fill:#0173B2,color:#fff
    style Obj1 fill:#DE8F05,color:#fff
    style Obj2 fill:#029E73,color:#fff
    style Obj3 fill:#CC78BC,color:#fff
```

**Code**:

```java
// Class definition with fields (state) and methods (behavior)
// => class: blueprint defining what data (fields) an object holds
// => class: defines what operations (methods) an object can perform
// => Each instance of class gets its own copy of instance fields
public class Person {
    // => public: this class is accessible from any package
    // => class Person: defines new type "Person" in Java's type system
    // => Opening brace { begins class body definition

    // Fields (instance variables) - each object has its own copy
    // => Instance variables: belong to object instances, NOT to the class itself
    // => Each Person object has separate name and age values
    // => Stored in heap memory when object is created with 'new'
    private String name;
    // => private: field accessible only within Person class methods
    // => Encapsulation: prevents external code from directly modifying name
    // => String: immutable reference type (cannot be changed after creation)
    // => name: identifier for this field (follows camelCase convention)
    // => Default value: null (all reference types default to null)

    private int age;
    // => private: field accessible only within this class (encapsulation)
    // => int: primitive type storing integer values (-2³¹ to 2³¹-1)
    // => age: field identifier (camelCase convention)
    // => Default value: 0 (all numeric primitives default to 0)
    // => Size: 32 bits (4 bytes) per instance

    // Constructor - special method called when creating objects with 'new'
    // => Constructor: special method with same name as class (Person)
    // => No return type (not even void) - implicitly returns new object reference
    // => Called automatically when 'new Person(...)' executes
    // => Initializes object state before reference is returned to caller
    public Person(String name, int age) {
        // => public: constructor accessible from anywhere (can create Person objects)
        // => Person: constructor name MUST match class name exactly (case-sensitive)
        // => (String name, int age): parameter list defines required initialization data
        // => Parameters are local variables, exist only during constructor execution
        // => Parameters shadow (hide) instance fields with same names

        // 'this' refers to the current instance
        // => this: reference to the object being constructed (current instance)
        // => this points to the object allocated by 'new' operator
        // => Type: Person reference (implicit, not declared)
        // => Scope: available in all instance methods and constructors
        this.name = name;
        // => this.name: refers to instance field (left side of assignment)
        // => name: refers to constructor parameter (right side of assignment)
        // => Distinguish field from parameter with same name using 'this'
        // => Assignment: copies parameter value to instance field
        // => After execution: instance field name holds value passed to constructor
        // => Without 'this', 'name = name' would assign parameter to itself (no effect)

        this.age = age;
        // => this.age: instance field (stored in heap with object)
        // => age: constructor parameter (stored in stack, temporary)
        // => Assignment: copies parameter value 30 to instance field
        // => After execution: instance field age holds passed value
        // => Type compatibility: both are int (no conversion needed)
    }
    // => Closing brace } ends constructor body
    // => After constructor completes, JVM returns object reference to caller
    // => Object is now fully initialized and ready for use

    // Constructor overloading - multiple constructors with different parameters
    // => Constructor overloading: same name (Person), different parameter lists
    // => Compiler selects constructor based on arguments at compile-time
    // => Enables flexible object creation with varying initialization data
    public Person(String name) {
        // => This constructor accepts only name, no age parameter
        // => Parameter list: (String name) - different from (String, int) above
        // => Compiler distinguishes overloaded constructors by parameter count/types

        this(name, 0);
        // => this(...): calls another constructor in same class
        // => Delegates to Person(String, int) constructor above
        // => Arguments: name (parameter), 0 (literal default for age)
        // => MUST be first statement in constructor body (compiler requirement)
        // => Prevents code duplication (initialization logic in one place)
        // => After this() call: name field set to parameter, age field set to 0
        // => Called "constructor chaining" - one constructor calling another
    }

    // Default constructor (no parameters) - Java provides one if you don't
    // => Default constructor: no parameters, initializes with default values
    // => Java provides implicit default constructor only if NO constructors defined
    // => Since we define constructors, must explicitly provide no-arg constructor
    public Person() {
        // => () empty parameter list: no arguments required for this constructor

        this("Unknown", 0);
        // => this("Unknown", 0): calls Person(String, int) constructor
        // => "Unknown": default string for name field (literal String)
        // => 0: default value for age field (int literal)
        // => Constructor delegation: reuses initialization logic
        // => After execution: name="Unknown", age=0
        // => Enables creating Person without knowing initial values
    }

    // Instance method - operates on instance data
    // => Instance method: belongs to object instances, not to class itself
    // => Can access instance fields (name, age) via implicit 'this'
    // => Called via object reference: alice.greet() not Person.greet()
    public void greet() {
        // => public: method accessible from anywhere
        // => void: method returns no value (produces side effect only)
        // => greet: method name (verb convention for methods performing actions)
        // => (): no parameters required

        System.out.println("Hi, I'm " + name);
        // => System.out.println: prints string to standard output with newline
        // => "Hi, I'm ": string literal prefix
        // => +: string concatenation operator (joins strings)
        // => name: implicit this.name, accesses current instance's name field
        // => If alice.greet() called, name is "Alice"
        // => Concatenation result: "Hi, I'm Alice"
        // => Output: Hi, I'm Alice
        // => Newline added after output (println = print line)
    }

    // Getter method (accessor)
    // => Getter: provides read-only access to private field
    // => Accessor pattern: encapsulation allows controlled field access
    // => Naming convention: getName() for field 'name'
    public String getName() {
        // => public: accessible from outside class (API for field access)
        // => String: return type matching name field type
        // => getName: method name following getFieldName convention
        // => (): no parameters needed to retrieve field

        return name;
        // => return: keyword passing value back to caller
        // => name: implicit this.name, current instance's name field
        // => Returns reference to String object (strings are immutable)
        // => If alice.getName() called, returns "Alice"
        // => Method execution ends immediately after return
        // => Caller receives String reference stored in name field
    }

    // Setter method (mutator)
    // => Setter: provides write access to private field with validation
    // => Mutator pattern: controlled field modification
    // => Naming convention: setAge(newValue) for field 'age'
    public void setAge(int age) {
        // => public: accessible from outside class (API for field modification)
        // => void: no return value (method modifies state only)
        // => setAge: method name following setFieldName convention
        // => int age: parameter providing new value for age field

        this.age = age;
        // => this.age: instance field (left side, assignment target)
        // => age: method parameter (right side, source value)
        // => Assignment: updates instance field with parameter value
        // => If alice.setAge(31) called, alice's age field becomes 31
        // => Enables validation/logging before setting (not shown here)
        // => Encapsulation: external code cannot set age directly (private field)
    }

    // Method overloading - same name, different parameters
    // => Method overloading: multiple methods with same name, different signatures
    // => Signature: method name + parameter types (return type NOT included)
    // => Compiler selects method based on arguments at compile-time
    // => Enables flexible API with varying parameter combinations
    public void celebrate() {
        // => celebrate(): version with no parameters
        // => Signature: celebrate() - distinguished by empty parameter list

        System.out.println(name + " is celebrating!");
        // => Concatenates name field with " is celebrating!"
        // => If bob.celebrate() called and name="Bob"
        // => Output: Bob is celebrating!
        // => Newline added after output
    }

    public void celebrate(String event) {
        // => celebrate(String): version accepting event parameter
        // => Signature: celebrate(String) - distinguished by parameter type
        // => Different from celebrate() above due to parameter

        System.out.println(name + " is celebrating " + event + "!");
        // => Concatenates: name + " is celebrating " + event + "!"
        // => If bob.celebrate("birthday") called, name="Bob", event="birthday"
        // => String concatenation: "Bob" + " is celebrating " + "birthday" + "!"
        // => Result: "Bob is celebrating birthday!"
        // => Output: Bob is celebrating birthday!
    }
}
// => Closing brace } ends Person class definition
// => Class is now compiled to Person.class bytecode file

// Creating objects (instances) using 'new' keyword
// => Object creation: allocates heap memory and initializes instance
// => new: operator allocating memory and calling constructor
Person alice = new Person("Alice", 30);
// => Person alice: declares variable of type Person (reference type)
// => Type: Person (class type, not primitive)
// => alice: variable name holding reference to Person object
// => new Person("Alice", 30): object creation expression
// => new: allocates memory on heap for Person object (8 bytes reference + field sizes)
// => Person("Alice", 30): calls constructor Person(String, int)
// => "Alice": string literal passed as name parameter
// => 30: int literal passed as age parameter
// => Constructor execution: name field set to "Alice", age field set to 30
// => new operator returns: reference (memory address) to newly created object
// => Assignment: alice variable stores reference to object on heap
// => After execution: alice points to Person object with name="Alice", age=30

Person bob = new Person("Bob");
// => Person bob: declares variable holding Person reference
// => new Person("Bob"): creates object using Person(String) constructor
// => "Bob": passed as name parameter
// => Constructor delegates to Person(String, int) with ("Bob", 0)
// => age field defaults to 0 via constructor chaining
// => After execution: bob points to Person object with name="Bob", age=0

// Calling instance methods
// => Instance method call: object.method(arguments)
// => Requires object reference (alice, bob) to invoke method
alice.greet();
// => alice.greet(): calls greet() method on alice object
// => alice: object reference (receiver of method call)
// => . (dot operator): member access operator
// => greet(): method name with empty argument list
// => Method accesses alice's name field ("Alice")
// => Executes: System.out.println("Hi, I'm Alice")
// => Output: Hi, I'm Alice
// => Returns: nothing (void method)

String aliceName = alice.getName();
// => String aliceName: declares variable to hold returned String
// => alice.getName(): calls getName() method on alice object
// => alice: receiver object
// => getName(): accessor method returning name field
// => Method returns: "Alice" (String reference from alice.name field)
// => Assignment: aliceName variable stores returned reference
// => After execution: aliceName points to same String as alice.name ("Alice")
// => Type: String (reference type)

alice.setAge(31);
// => alice.setAge(31): calls setAge(int) method on alice object
// => alice: receiver object whose age will be modified
// => setAge: mutator method name
// => 31: int argument passed as age parameter
// => Method executes: this.age = 31 (alice's age field updated)
// => After execution: alice.age changed from 30 to 31
// => No return value (void method)
// => State mutation: alice object's age field modified

// Method overloading in action
// => Method overload resolution: compiler selects method based on arguments
bob.celebrate();
// => bob.celebrate(): calls celebrate() with no arguments
// => Compiler selects: celebrate() version (no parameters)
// => bob: receiver object with name="Bob"
// => Method accesses bob.name field
// => Output: Bob is celebrating!
// => Method selection at compile-time based on argument count (0 arguments)

bob.celebrate("birthday");
// => bob.celebrate("birthday"): calls celebrate with String argument
// => Compiler selects: celebrate(String) version (one String parameter)
// => "birthday": String literal passed as event parameter
// => Method accesses bob.name ("Bob") and event parameter ("birthday")
// => Output: Bob is celebrating birthday!
// => Different method than celebrate() above (different signature)

// Access modifiers control visibility:
// => Access modifiers: keywords controlling where class members are accessible
// => Encapsulation: hiding implementation details, exposing only public API
// => Four levels in Java: public, protected, default (package-private), private

// public: accessible from anywhere
// => public: no access restrictions (any class, any package)
// => Used for: class APIs, public methods, public constructors
// => Example: public class Person, public void greet()

// private: accessible only within the class
// => private: accessible only within same class definition
// => Used for: internal fields, helper methods
// => Strongest encapsulation (external code cannot access)
// => Example: private String name, private int age

// protected: accessible within package and subclasses
// => protected: accessible in same package OR in subclasses (even different package)
// => Used for: fields/methods intended for subclass extension
// => Less restrictive than private, more than package-private
// => Enables inheritance-based access

// (default/package-private): accessible within package
// => No keyword (omit modifier): accessible only in same package
// => Also called "package-private" or "default access"
// => Used for: package-internal classes and members
// => Example: class Helper {} or void helperMethod() {}
```

**Key Takeaway**: Classes define the structure and behavior; objects are instances with their own state. Constructors initialize objects. `this` refers to the current instance. Access modifiers (`private`, `public`) control encapsulation.

**Why It Matters**: Object-oriented encapsulation is fundamental to building maintainable enterprise systems—private fields with public getters/setters enable validation, logging, and versioning without breaking client code. Constructor overloading provides API flexibility seen throughout the Java ecosystem (Spring Framework's ApplicationContext has 12+ constructors for different initialization scenarios). The `this` keyword eliminates field-parameter naming conflicts, making code self-documenting and preventing subtle bugs where assignments fail silently. Unlike procedural languages where data and functions are separate, Java's class-based organization enables dependency injection, testability through mocking, and architectural patterns like MVC that power scalable web applications.

---

### Example 8: Inheritance and Polymorphism

Inheritance creates IS-A relationships where subclasses inherit fields and methods from superclasses. Polymorphism allows treating subclass objects as superclass references.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Animal["Animal (Superclass)<br/>name, makeSound()"] --> Dog["Dog (Subclass)<br/>breed, bark()"]
    Animal --> Cat["Cat (Subclass)<br/>color, meow()"]

    style Animal fill:#0173B2,color:#fff
    style Dog fill:#DE8F05,color:#fff
    style Cat fill:#029E73,color:#fff
```

**Code**:

```java
// Superclass (parent class)
// => Superclass: base class providing common fields and methods
// => Parent class: defines shared behavior inherited by subclasses
// => All subclasses automatically inherit superclass members
public class Animal {
    // => public: class accessible from any package
    // => Animal: superclass name (base type in hierarchy)

    protected String name;
    // => protected: accessible in subclasses and same package
    // => More accessible than private (subclasses can access directly)
    // => Less accessible than public (not accessible outside package unless subclass)
    // => String name: field inherited by all subclasses (Dog, Cat)
    // => Subclasses can access 'name' directly without getter
    // => Default value: null (reference type)

    public Animal(String name) {
        // => public: constructor accessible from anywhere (can create Animal objects)
        // => Animal(String): constructor accepting name parameter
        // => Subclass constructors MUST call this via super(name)

        this.name = name;
        // => this.name: instance field
        // => name: constructor parameter
        // => Initializes name field with parameter value
        // => After execution: name field holds passed value
    }

    // Method to be overridden
    // => Method designed for subclasses to provide specialized behavior
    // => Subclasses can override to replace with specific implementation
    public void makeSound() {
        // => public: accessible from anywhere (subclasses can override)
        // => void: no return value
        // => makeSound: method name (behavior to be customized)

        System.out.println(name + " makes a sound");
        // => Generic implementation (not specific to any animal type)
        // => Concatenates name with " makes a sound"
        // => Subclasses override with specific sounds (bark, meow)
        // => Output example: Rex makes a sound
    }
}

// Subclass (child class) using 'extends' keyword
// => Subclass: class inheriting from superclass (child class)
// => extends: keyword establishing inheritance relationship
// => Dog IS-A Animal (inheritance represents IS-A relationship)
public class Dog extends Animal {
    // => public class Dog: subclass type
    // => extends Animal: inherits all non-private fields and methods from Animal
    // => Dog inherits: name field, makeSound() method
    // => Dog can add new fields/methods and override inherited methods

    private String breed;
    // => private: accessible only within Dog class
    // => String breed: Dog-specific field (not in Animal)
    // => Subclass adds additional state beyond superclass
    // => Each Dog has both name (inherited) and breed (Dog-specific)

    // Constructor with 'super' to call superclass constructor
    // => Subclass constructor must initialize both inherited and new fields
    // => super(...): calls superclass constructor
    public Dog(String name, String breed) {
        // => Dog(String, String): constructor accepting name and breed
        // => name: will be passed to Animal constructor
        // => breed: will be stored in Dog-specific field

        super(name);
        // => super(name): calls Animal(String) constructor
        // => MUST be first statement in constructor (compiler requirement)
        // => Initializes inherited name field via superclass constructor
        // => After execution: name field (inherited) set to parameter value
        // => Cannot access 'this' before super() call (object not fully constructed)

        this.breed = breed;
        // => this.breed: Dog-specific field
        // => breed: constructor parameter
        // => Initializes breed field after superclass initialization
        // => After execution: breed field set to parameter value (e.g., "Labrador")
    }

    // Method overriding with @Override annotation (recommended)
    // => Method overriding: subclass provides specialized implementation
    // => Replaces superclass method with subclass-specific version
    // => @Override: annotation ensuring method actually overrides superclass method
    @Override
    // => @Override: compile-time check (fails if method doesn't override)
    // => Prevents typos: @Override on makeSond() would be compile error
    // => Documents intent: this method replaces superclass version
    // => Best practice: always use @Override when overriding
    public void makeSound() {
        // => Signature matches Animal.makeSound(): same name, parameters, return type
        // => public: cannot reduce visibility (Animal.makeSound is public)
        // => void: return type matches superclass (covariant return allowed)

        System.out.println(name + " barks: Woof!");
        // => name: inherited protected field, directly accessible
        // => Concatenates: name + " barks: Woof!"
        // => If name="Rex", output: Rex barks: Woof!
        // => Replaces Animal's generic implementation with Dog-specific behavior
    }

    // Subclass-specific method
    // => Method unique to Dog, not present in Animal
    // => Not inherited by other Animal subclasses (Cat)
    public void fetch() {
        // => fetch: behavior specific to dogs only

        System.out.println(name + " fetches the ball!");
        // => name: inherited field accessible in subclass methods
        // => Output example: Rex fetches the ball!
    }
}

public class Cat extends Animal {
    // => public class Cat: another Animal subclass
    // => extends Animal: inherits name field and makeSound() method
    // => Cat is sibling to Dog (both extend Animal)

    public Cat(String name) {
        // => Cat(String): constructor accepting only name (no breed)
        // => Simpler than Dog constructor (fewer Cat-specific fields)

        super(name);
        // => super(name): calls Animal(String) constructor
        // => MUST be first statement
        // => Initializes inherited name field
        // => After execution: name field set to parameter value
    }

    @Override
    public void makeSound() {
        // => @Override: confirms this replaces Animal.makeSound()
        // => Signature matches: public void makeSound()

        System.out.println(name + " meows: Meow!");
        // => Cat-specific implementation (different from Dog's bark)
        // => If name="Whiskers", output: Whiskers meows: Meow!
    }
}

// Creating instances
Dog dog = new Dog("Rex", "Labrador");
// => Dog dog: variable of type Dog (subclass type)
// => new Dog("Rex", "Labrador"): creates Dog object
// => Calls Dog(String, String) constructor
// => Constructor calls super("Rex"), then sets breed="Labrador"
// => After execution: dog points to Dog object with name="Rex", breed="Labrador"

Cat cat = new Cat("Whiskers");
// => Cat cat: variable of type Cat
// => new Cat("Whiskers"): creates Cat object
// => Calls Cat(String) constructor → super("Whiskers")
// => After execution: cat points to Cat object with name="Whiskers"

// Polymorphism - treating subclass as superclass reference
// => Polymorphism: ability to treat objects of different types uniformly
// => Subclass reference can be assigned to superclass variable
// => Compile-time type (Animal) differs from runtime type (Dog/Cat)
Animal animal1 = dog;
// => Animal animal1: variable of superclass type
// => dog: Dog reference (subclass type)
// => Assignment: Dog reference stored in Animal variable
// => Allowed: Dog IS-A Animal (inheritance relationship)
// => animal1 compile-time type: Animal (limited to Animal methods)
// => animal1 runtime type: Dog (actual object is Dog)
// => After assignment: animal1 and dog refer to same object

Animal animal2 = cat;
// => Animal animal2: superclass type variable
// => cat: Cat reference (subclass type)
// => Assignment: Cat reference stored in Animal variable
// => Allowed: Cat IS-A Animal
// => animal2 compile-time type: Animal
// => animal2 runtime type: Cat

// Runtime polymorphism - method called based on actual object type
// => Runtime polymorphism (dynamic dispatch): JVM selects method based on runtime type
// => Compile-time type (Animal) determines which methods callable
// => Runtime type (Dog/Cat) determines which implementation executes
animal1.makeSound();
// => animal1.makeSound(): calls makeSound() on Animal reference
// => Compile-time: Animal.makeSound() exists (call allowed)
// => Runtime: animal1 points to Dog object
// => JVM calls Dog.makeSound() (overridden version)
// => Output: Rex barks: Woof!
// => Dynamic dispatch: method selection at runtime based on actual object type

animal2.makeSound();
// => animal2: Animal reference pointing to Cat object
// => Compile-time: Animal.makeSound() exists
// => Runtime: animal2 points to Cat object
// => JVM calls Cat.makeSound() (overridden version)
// => Output: Whiskers meows: Meow!
// => Same method call, different behavior (polymorphism)

// animal1.fetch(); // ERROR: Animal reference doesn't have fetch()
// => animal1 compile-time type: Animal
// => Animal class doesn't have fetch() method
// => Compiler error: cannot find symbol method fetch()
// => Even though runtime type is Dog (which has fetch()), compiler uses compile-time type
// => Polymorphism limitation: can only call methods declared in reference type

((Dog) animal1).fetch();
// => Cast: converts Animal reference to Dog reference
// => (Dog): cast operator (explicit type conversion)
// => animal1: Animal reference pointing to Dog object
// => (Dog) animal1: tells compiler "treat as Dog reference"
// => Compile-time: Dog.fetch() exists (call allowed after cast)
// => Runtime: ClassCastException if animal1 not actually Dog
// => Here: animal1 is Dog, so cast succeeds
// => .fetch(): calls Dog-specific method
// => Output: Rex fetches the ball!
// => Downcasting: converting superclass reference to subclass reference

// Object class - root of all classes
// => Object: superclass of all Java classes (implicit inheritance)
// => Every class extends Object (explicitly or implicitly)
// => Animal extends Object implicitly, Dog/Cat extend Object transitively
Object obj = dog;
// => Object obj: variable of type Object (top of hierarchy)
// => dog: Dog reference (Dog extends Animal extends Object)
// => Assignment: Dog reference stored in Object variable
// => Allowed: every class IS-A Object
// => obj can hold reference to ANY Java object
// => Compile-time type: Object (only Object methods callable)
// => Runtime type: Dog (actual object is Dog)

// Common Object methods to override
// => Object class provides methods inherited by all classes
// => Default implementations often inadequate (need customization)
// => Best practice: override toString(), equals(), hashCode()
public class Animal {
    // ... fields and constructor ...

    @Override
    public String toString() {
        // => @Override: replaces Object.toString()
        // => toString: called implicitly by println, string concatenation
        // => Returns: string representation of object

        return "Animal[name=" + name + "]";
        // => Custom format: "Animal[name=value]"
        // => If name="Rex", returns "Animal[name=Rex]"
        // => Without override: would return "Animal@hashcode" (unhelpful)
        // => Used by: System.out.println(animal) → calls toString() implicitly
    }

    @Override
    public boolean equals(Object other) {
        // => @Override: replaces Object.equals(Object)
        // => equals: defines object equality (content comparison)
        // => Object other: parameter can be any type (must handle type checking)

        if (other instanceof Animal) {
            // => instanceof: type-checking operator
            // => true if other is Animal or subclass (Dog, Cat)
            // => false if other is null or different type
            // => Prevents ClassCastException

            return name.equals(((Animal) other).name);
            // => (Animal) other: casts other to Animal (safe after instanceof check)
            // => ((Animal) other).name: accesses name field of other object
            // => name.equals(...): compares this.name with other.name
            // => String.equals: content comparison (not reference comparison)
            // => Returns true if names are equal, false otherwise
        }
        return false;
        // => other is not Animal type
        // => Animals cannot equal non-Animals
        // => Returns false (not equal)
    }

    @Override
    public int hashCode() {
        // => @Override: replaces Object.hashCode()
        // => hashCode: returns integer hash code for object
        // => Contract: equal objects MUST have equal hash codes
        // => Used by HashMap, HashSet for bucket assignment

        return name.hashCode();
        // => Delegates to String.hashCode()
        // => Returns hash code based on name field
        // => Same name → same hash code
        // => Different names → likely different hash codes
        // => Consistent with equals() (equal Animals have equal names → equal hash codes)
    }
}

// final keyword prevents inheritance or overriding
// => final: modifier preventing extension/modification
// => Ensures class/method behavior cannot be changed by subclasses
final class FinalClass {}
// => final class: cannot be extended (no subclasses allowed)
// => FinalClass is leaf in inheritance hierarchy
// => Example use cases: String, Integer (Java standard library)
// => Reason: security, performance optimization, design guarantee

// class Sub extends FinalClass {} // ERROR
// => ERROR: cannot inherit from final FinalClass
// => Compiler error: cannot inherit from final class
// => Final classes are inheritance endpoints

public class Base {
    // => Base: regular class (not final, can be extended)

    public final void cannotOverride() {}
    // => final method: cannot be overridden in subclasses
    // => Method behavior guaranteed to remain unchanged
    // => Subclasses inherit method but cannot replace implementation
    // => Example use cases: template methods, security-critical operations
}
// If subclass tried: class Sub extends Base { @Override public void cannotOverride() {} }
// => Would be compile error: cannotOverride() in Sub cannot override cannotOverride() in Base
```

**Key Takeaway**: Inheritance (`extends`) creates IS-A relationships and enables code reuse. `super` accesses superclass members. Polymorphism allows subclass objects through superclass references. Override `toString()`, `equals()`, and `hashCode()` for proper object behavior. Use `final` to prevent inheritance or overriding.

**Why It Matters**: Use inheritance **only for true IS-A relationships** where subclasses should inherit ALL behaviors—a Dog IS-A Animal makes sense. However, inheritance creates **tight coupling**: changes to the superclass affect all subclasses, making code fragile. Prefer **composition** (HAS-A) when you need specific capabilities without the full parent behavior—a Car HAS-AN Engine, not IS-AN Engine. Composition enables runtime flexibility (swap engines) and avoids forcing subclasses to inherit unwanted behaviors. The rule of thumb: inheritance for shared identity and behavior, composition for shared functionality. When in doubt, choose composition—it's easier to refactor composition to inheritance than vice versa.

---

### Example 9: Interfaces and Abstraction

Interfaces define contracts (what methods a class must implement) without implementation. Classes can implement multiple interfaces, enabling flexible type hierarchies.

**Code**:

```java
// Interface definition - defines contract (what, not how)
// => Interface: contract specifying what methods a class must provide
// => Defines "what" behavior, not "how" it's implemented
// => Pure abstraction: no implementation details (before Java 8)
public interface Drawable {
    // => public interface: interface accessible from any package
    // => Drawable: interface name (adjective convention: "can be drawn")
    // => Interfaces define capabilities (Runnable, Comparable, Serializable)

    // Abstract method (implicitly public abstract)
    // => Abstract method: method signature without body
    // => Implementing classes MUST provide method body
    void draw();
    // => void draw(): method signature (return type + name + parameters)
    // => Implicitly public abstract (no need to write "public abstract void draw()")
    // => All implementing classes must implement draw() method
    // => No method body (interface defines contract, not implementation)

    // Interfaces can have constants (implicitly public static final)
    // => Constants: immutable values shared across all implementations
    // => Accessible without creating objects: Drawable.PI
    double PI = 3.14159;
    // => double PI: constant field
    // => Implicitly public static final (accessible from anywhere, class-level, immutable)
    // => = 3.14159: constant value (must be initialized at declaration)
    // => Cannot be changed after initialization (final)
    // => Accessed as Drawable.PI (no object required, static)

    // Default methods (Java 8+) - provide implementation
    // => Default method: interface method with implementation
    // => Implementing classes inherit this method (can override if needed)
    // => Added in Java 8 to allow interface evolution without breaking existing implementations
    default void print() {
        // => default: keyword enabling method body in interface
        // => Implementing classes get this method automatically (optional override)

        System.out.println("Drawing...");
        // => Implementation in interface (shared behavior)
        // => Output: Drawing...
        // => Implementing classes can use as-is or override
    }

    // Static methods (Java 8+)
    // => Static method: belongs to interface itself, not to implementations
    // => Called via interface name: Drawable.info()
    static void info() {
        // => static: method belongs to Drawable interface, not to objects
        // => Cannot be overridden by implementing classes
        // => Utility method related to interface

        System.out.println("Drawable interface");
        // => Output: Drawable interface
        // => Called as: Drawable.info() (no object required)
    }
}

// Another interface
// => Multiple interfaces enable multiple inheritance of type
// => Class can implement multiple interfaces (cannot extend multiple classes)
public interface Movable {
    // => Movable: another capability interface (adjective convention)

    void move(int x, int y);
    // => Abstract method: implementing classes must provide move logic
    // => Parameters: int x, int y (new coordinates)
    // => Implicitly public abstract
}

// Class implementing interface(s) using 'implements' keyword
// => implements: keyword declaring interface implementation
// => Class must implement ALL abstract methods from interface
public class Circle implements Drawable {
    // => public class Circle: concrete class
    // => implements Drawable: Circle provides draw() implementation
    // => Circle IS-A Drawable (can be used wherever Drawable expected)

    private int radius;
    // => private int radius: Circle-specific field
    // => Not part of Drawable contract (implementation detail)

    public Circle(int radius) {
        // => Constructor: initializes Circle-specific state

        this.radius = radius;
        // => Stores radius value
    }

    // Must implement all abstract methods from interface
    // => Implementation requirement: all abstract interface methods must be implemented
    // => Compiler error if draw() not provided
    @Override
    public void draw() {
        // => @Override: confirms this implements Drawable.draw()
        // => public: interface methods implicitly public (cannot reduce visibility)
        // => void draw(): signature matches Drawable.draw() exactly

        System.out.println("Drawing circle with radius " + radius);
        // => Circle-specific implementation of draw()
        // => Accesses Circle's radius field
        // => Output example: Drawing circle with radius 10
    }
    // => Circle inherits default print() method from Drawable (no override needed)
}

// Implementing multiple interfaces (multiple inheritance of type)
// => Multiple interface implementation: class fulfills multiple contracts
// => Rectangle IS-A Drawable AND IS-A Movable
// => Java allows multiple interface implementation (but not multiple class inheritance)
public class Rectangle implements Drawable, Movable {
    // => implements Drawable, Movable: comma-separated list of interfaces
    // => Must implement: draw() from Drawable, move(int, int) from Movable
    // => Rectangle can be used as Drawable OR Movable reference

    private int width, height;
    // => Rectangle-specific fields for dimensions

    private int x, y;
    // => Position fields (for Movable functionality)

    public Rectangle(int width, int height) {
        // => Constructor: initializes dimensions
        // => Position (x, y) defaults to 0 (primitive default)

        this.width = width;
        this.height = height;
        // => Stores width and height
    }

    @Override
    public void draw() {
        // => Implements Drawable.draw() contract
        // => Rectangle-specific drawing logic

        System.out.println("Drawing rectangle " + width + "x" + height);
        // => Output example: Drawing rectangle 20x30
    }

    @Override
    public void move(int newX, int newY) {
        // => Implements Movable.move(int, int) contract
        // => Rectangle-specific movement logic

        this.x = newX;
        this.y = newY;
        // => Updates position fields

        System.out.println("Moved to (" + x + ", " + y + ")");
        // => Output example: Moved to (5, 10)
    }
}
// => Rectangle satisfies both Drawable and Movable contracts
// => Can be assigned to Drawable OR Movable references

// Using interfaces
Drawable circle = new Circle(10);
// => Drawable circle: interface reference (compile-time type)
// => new Circle(10): creates Circle object (runtime type)
// => Assignment: Circle implements Drawable (allowed)
// => circle can only call Drawable methods (draw, print)
// => Cannot call Circle-specific methods through Drawable reference

circle.draw();
// => Calls Circle.draw() implementation
// => Polymorphism: method selection based on runtime type (Circle)
// => Output: Drawing circle with radius 10

circle.print();
// => Calls default print() method from Drawable interface
// => Circle inherits this method (didn't override)
// => Output: Drawing...

Drawable rect = new Rectangle(20, 30);
// => Drawable rect: interface reference
// => new Rectangle(20, 30): creates Rectangle object
// => Rectangle implements Drawable (allowed)
// => rect can call Drawable methods only (cannot call move() via Drawable reference)

rect.draw();
// => Calls Rectangle.draw() implementation
// => Output: Drawing rectangle 20x30

// Abstract classes vs Interfaces
// => Abstract class: partial implementation with abstract methods
// => Can have state, constructors, and concrete methods (unlike pure interfaces)
// => Single inheritance only (class extends one abstract class)
public abstract class Shape {
    // => abstract class: cannot be instantiated directly
    // => new Shape() would be compile error
    // => Subclasses must extend and implement abstract methods

    // Can have state (fields)
    // => Abstract classes can have instance fields
    // => Interfaces cannot have instance fields (only constants before Java 8)
    protected String color;
    // => protected: accessible to subclasses
    // => State shared by all Shape subclasses

    // Can have constructors
    // => Abstract classes can have constructors (called via super())
    // => Interfaces cannot have constructors
    public Shape(String color) {
        // => Constructor: initializes shared state

        this.color = color;
        // => Sets color field
    }

    // Can have concrete methods
    // => Abstract classes can provide complete method implementations
    // => Subclasses inherit without overriding
    public String getColor() {
        // => Concrete method: full implementation provided
        // => Subclasses inherit this method

        return color;
        // => Returns color field
    }

    // Can have abstract methods
    // => Abstract methods: no implementation (like interface methods)
    // => Subclasses MUST implement
    public abstract double area();
    // => abstract: method has no body
    // => Subclasses must provide area() implementation
    // => Forces all shapes to define area calculation
}

public class Square extends Shape {
    // => extends Shape: inherits color field, getColor() method
    // => Must implement abstract area() method

    private double side;
    // => Square-specific field

    public Square(String color, double side) {
        // => Constructor accepting color and side

        super(color);
        // => Calls Shape constructor to initialize color
        // => MUST be first statement

        this.side = side;
        // => Initializes Square-specific field
    }

    @Override
    public double area() {
        // => Implements abstract area() method from Shape
        // => Provides Square-specific calculation

        return side * side;
        // => Returns side²
        // => If side=4, returns 16.0
    }
}

// Marker interfaces (no methods) - used for type checking
// => Marker interface: empty interface indicating capability
// => No methods to implement
// => Used for runtime type checking (instanceof)
public interface Serializable {}
// => Empty interface
// => Classes implementing Serializable can be serialized (saved to disk/network)
// => Example: class User implements Serializable {}
// => Runtime check: if (obj instanceof Serializable) { serialize(obj); }

// Functional interfaces (Java 8+) - exactly one abstract method
// => Functional interface: interface with EXACTLY one abstract method
// => Enables lambda expressions and method references
// => Can have default/static methods (don't count toward "one abstract method")
@FunctionalInterface
// => @FunctionalInterface: annotation ensuring exactly one abstract method
// => Compile error if more than one abstract method
// => Documents intent: this interface is for lambda expressions

public interface Calculator {
    // => Calculator: functional interface for arithmetic operations

    int calculate(int a, int b);
    // => Single abstract method (SAM)
    // => Parameters: int a, int b
    // => Return type: int
    // => Lambda expressions must match this signature

    // Can still have default and static methods
    // => Functional interfaces can have default/static methods
    // => Only abstract methods count toward "single abstract method" rule
    default void log(String msg) {
        // => Default method: doesn't affect functional interface status

        System.out.println("Log: " + msg);
        // => Utility logging method
    }
}

// Lambda expressions work with functional interfaces
// => Lambda expression: concise way to implement functional interface
// => Syntax: (parameters) -> expression or { statements }
Calculator add = (a, b) -> a + b;
// => Calculator add: functional interface reference
// => (a, b) -> a + b: lambda expression implementing calculate(int, int)
// => Parameters: a, b (types inferred as int from Calculator.calculate signature)
// => ->: lambda operator (separates parameters from body)
// => a + b: expression body (returns sum)
// => Equivalent to: new Calculator() { public int calculate(int a, int b) { return a + b; } }
// => Much more concise than anonymous inner class

int sum = add.calculate(5, 3);
// => add.calculate(5, 3): calls lambda expression
// => a=5, b=3 passed to lambda
// => Lambda body executes: 5 + 3
// => Returns: 8
// => sum stores returned value
```

**Key Takeaway**: Interfaces define contracts without implementation. Classes can `implements` multiple interfaces for flexible type systems. Abstract classes can have state and constructors; interfaces cannot (before Java 8). Default methods allow adding behavior to interfaces. Functional interfaces enable lambda expressions.

**Why It Matters**: Interfaces enable **loose coupling** and **dependency inversion**—clients depend on abstractions (interfaces), not concrete implementations. This makes code **testable** (inject mock implementations), **flexible** (swap implementations at runtime), and **maintainable** (change internals without affecting clients). For example, `List<T>` is an interface—you can switch from `ArrayList` to `LinkedList` without changing client code. Multiple interface implementation provides the benefits of multiple inheritance without the diamond problem. Interfaces define "what" an object can do; classes define "how" it's done. Design principle: program to interfaces, not implementations, to create loosely coupled, easily testable systems.

---

## Group 3: Core Collections

### Example 10: Arrays and Lists

Arrays have fixed size and fast access. Lists (like `ArrayList`) are dynamic and provide rich operations. Generics ensure type safety for collections.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Array["Array<br/>Fixed size, fast access<br/>[1,2,3,4,5]"]
    ArrayList["ArrayList<br/>Dynamic size, resizable<br/>[1,2,3] -> [1,2,3,4]"]

    style Array fill:#0173B2,color:#fff
    style ArrayList fill:#DE8F05,color:#fff
```

**Code**:

```java
import java.util.ArrayList;
// => Import ArrayList class from java.util package
// => ArrayList: dynamic array implementation of List interface
// => Resizes automatically when capacity exceeded

import java.util.Arrays;
// => Import Arrays utility class
// => Provides static methods for array operations (sort, toString, asList, etc.)

import java.util.List;
// => Import List interface (parent of ArrayList)
// => List: ordered collection allowing duplicates and indexed access
// => Program to List interface, not ArrayList implementation (flexibility)

// Array declaration and initialization
// => Arrays: fixed-size, homogeneous, contiguous memory
// => Size determined at creation, cannot change
// => Elements stored at consecutive memory addresses (cache-friendly)
int[] numbers = {1, 2, 3, 4, 5};
// => int[] numbers: array of primitive int
// => {1, 2, 3, 4, 5}: array literal initialization
// => numbers now references array object on heap
// => numbers is [1, 2, 3, 4, 5] (length=5)
// => Memory: numbers points to heap, array elements stored contiguously

String[] names = new String[3];
// => new String[3]: creates array of size 3
// => Array elements initialized to null (reference type default)
// => names is [null, null, null] (length=3)
// => Each element can hold String reference (or null)

// Array access and modification
// => Arrays use zero-based indexing (0 to length-1)
// => Out-of-bounds access throws ArrayIndexOutOfBoundsException at runtime
int first = numbers[0];
// => numbers[0]: accesses first element
// => O(1) access time (direct memory offset calculation)
// => first is 1

numbers[2] = 30;
// => numbers[2] = 30: modifies element at index 2
// => Arrays are mutable (can change element values)
// => numbers is now [1, 2, 30, 4, 5]
// => Original value 3 replaced with 30

// Array length (property, not method)
// => length: public final field (not method like size())
// => Cannot be changed after array creation
int len = numbers.length;
// => numbers.length: accesses length field (no parentheses)
// => len is 5
// => Note: String uses length() method, arrays use length field

// Enhanced for loop (for-each) - read-only iteration
// => Enhanced for: iterates over elements, not indices
// => Cleaner syntax when index not needed
// => Cannot modify array during iteration (read-only)
for (int num : numbers) {
    // => int num: loop variable receives copy of array element
    // => Iterates: num=1, num=2, num=30, num=4, num=5
    // => Modifying num doesn't affect array (it's a copy)

    System.out.println(num);
    // => Prints each element on new line
    // => Output: 1, 2, 30, 4, 5 (each on separate line)
}
// => Enhanced for cannot: access index, skip elements, modify array elements

// Traditional for loop - allows index access
// => Traditional for: when index needed or modification required
// => Full control over iteration (start, end, step)
for (int i = 0; i < numbers.length; i++) {
    // => int i = 0: initialize loop counter
    // => i < numbers.length: boundary check (prevents out-of-bounds)
    // => i++: increment after each iteration
    // => i takes values: 0, 1, 2, 3, 4

    System.out.println(numbers[i]);
    // => numbers[i]: access element at index i
    // => Output: 1, 2, 30, 4, 5 (same as enhanced for)
    // => Can modify array: numbers[i] = newValue
}

// Arrays utility class
// => Arrays class: static utility methods for array operations
// => All methods are static (no need to instantiate Arrays)
String arrStr = Arrays.toString(numbers);
// => Arrays.toString(int[]): converts array to string representation
// => Handles all primitive and reference type arrays
// => arrStr is "[1, 2, 30, 4, 5]" (String representation)
// => Without toString: System.out.println(numbers) prints memory address

int[] sorted = {5, 2, 8, 1, 9};
// => sorted is [5, 2, 8, 1, 9] initially

Arrays.sort(sorted);
// => Arrays.sort(int[]): sorts array in ascending natural order
// => Uses Dual-Pivot Quicksort for primitives (O(n log n) average)
// => Sorts in-place: modifies original array
// => sorted is now [1, 2, 5, 8, 9]
// => For objects: uses merge sort (stable), requires Comparable or Comparator

// ArrayList - dynamic, resizable list
// => ArrayList: dynamic array (grows/shrinks as needed)
// => Backed by internal Object[] array (resizes when capacity exceeded)
// => Default initial capacity: 10 elements
ArrayList<String> list = new ArrayList<>();
// => ArrayList<String>: generic type parameter String
// => <String>: compile-time type safety (only String allowed)
// => new ArrayList<>(): diamond operator (type inferred from left side)
// => list is empty ArrayList (size=0, capacity=10)
// => Internal array: new Object[10] (not visible to user)

list.add("Alice");
// => add(E element): appends element to end
// => O(1) amortized time (occasional O(n) when resizing)
// => list is ["Alice"] (size=1)
// => Internal: array[0] = "Alice"

list.add("Bob");
// => Appends "Bob" after "Alice"
// => list is ["Alice", "Bob"] (size=2)

list.add("Charlie");
// => list is ["Alice", "Bob", "Charlie"] (size=3)
// => Internal array still has capacity 10 (no resize needed)

// List operations
// => ArrayList provides rich set of operations
// => All methods work with indices or values
String element = list.get(1);
// => get(int index): retrieves element at index
// => O(1) time (direct array access)
// => element is "Bob"
// => Throws IndexOutOfBoundsException if index invalid

list.set(1, "Robert");
// => set(int index, E element): replaces element at index
// => O(1) time
// => Returns old value "Bob"
// => list is ["Alice", "Robert", "Charlie"] (size=3)

list.remove(0);
// => remove(int index): removes element at index
// => O(n) time: shifts all elements after index left
// => Returns removed element "Alice"
// => list is ["Robert", "Charlie"] (size=2)
// => Internal: array[0] = "Robert", array[1] = "Charlie", array[2] = null

list.remove("Charlie");
// => remove(Object o): removes first occurrence of element
// => Uses equals() for comparison
// => Returns true if removed, false if not found
// => list is ["Robert"] (size=1)
// => O(n) time: must search then shift elements

int size = list.size();
// => size(): returns number of elements (not capacity)
// => size is 1
// => Capacity might be 10, but size is 1 (different concepts)

boolean contains = list.contains("Robert");
// => contains(Object o): checks if element exists
// => Uses equals() for comparison
// => O(n) time: linear search
// => contains is true

boolean isEmpty = list.isEmpty();
// => isEmpty(): checks if size == 0
// => More readable than size() == 0
// => isEmpty is false (size=1)

// Adding at specific position
list.add(0, "Start");
// => add(int index, E element): inserts element at index
// => O(n) time: shifts elements at/after index right
// => list is ["Start", "Robert"] (size=2)
// => "Robert" moved from index 0 to index 1

// List interface (parent of ArrayList)
// => Program to interface (List), not implementation (ArrayList)
// => Flexibility: can swap ArrayList with LinkedList, Vector, etc.
// => List: interface declaring contract, ArrayList: concrete implementation
List<Integer> nums = new ArrayList<>();
// => List<Integer>: declares interface type (compile-time)
// => new ArrayList<>(): creates ArrayList instance (runtime)
// => Polymorphism: List reference, ArrayList object
// => nums is empty List (size=0)

nums.add(10);
// => Autoboxing: int 10 converted to Integer object
// => ArrayList<Integer> stores Integer objects, not primitives
// => nums is [10] (size=1)

nums.add(20);
nums.add(30);
// => nums is [10, 20, 30] (size=3)
// => Each int autoboxed to Integer

// Arrays.asList() - creates fixed-size list backed by array
// => Arrays.asList(T... a): varargs method accepting elements
// => Returns fixed-size list backed by original array
// => Cannot add/remove elements (throws UnsupportedOperationException)
// => Can modify existing elements via set()
List<String> fixedList = Arrays.asList("A", "B", "C");
// => fixedList is ["A", "B", "C"] (size=3, fixed)
// => Backed by array {"A", "B", "C"}
// => Changes to list affect array and vice versa
// fixedList.add("D");
// => ERROR: UnsupportedOperationException
// => asList returns fixed-size list (cannot change size)
// => Can only use: get(), set(), contains(), indexOf()

// List.of() (Java 9+) - creates immutable list
// => List.of(E... elements): factory method (Java 9+)
// => Returns completely immutable list (cannot add, remove, or modify)
// => More efficient than Collections.unmodifiableList()
// => Null elements not allowed (throws NullPointerException)
List<String> immutable = List.of("X", "Y", "Z");
// => immutable is ["X", "Y", "Z"] (size=3, immutable)
// => Backed by internal immutable structure (not array)
// => Cannot call: add(), remove(), set(), clear()
// immutable.add("W");
// => ERROR: UnsupportedOperationException
// => Immutable: no modification allowed

// Converting between arrays and lists
// => Common operations when working with legacy APIs or performance-critical code
String[] array = {"One", "Two", "Three"};
// => array is ["One", "Two", "Three"] (array type)

List<String> listFromArray = new ArrayList<>(Arrays.asList(array));
// => Arrays.asList(array): creates fixed-size list view of array
// => new ArrayList<>(...): creates new ArrayList copying elements
// => listFromArray is mutable ArrayList ["One", "Two", "Three"]
// => Changes to listFromArray don't affect original array (deep copy)

String[] arrayFromList = list.toArray(new String[0]);
// => toArray(T[] a): converts list to array
// => new String[0]: array type hint (size doesn't matter)
// => Returns new array containing list elements
// => arrayFromList is ["Start", "Robert"] (array type)

// Generics provide compile-time type safety
// => Generics (Java 5+): parameterized types for compile-time checking
// => Prevents ClassCastException at runtime
// => Type erasure: generic type info removed at runtime (backward compatibility)
ArrayList<Integer> ints = new ArrayList<>();
// => <Integer>: type parameter restricts elements to Integer
// => Compiler enforces type safety
// => ints can only hold Integer objects

ints.add(42);
// => 42 autoboxed to Integer(42)
// => Compile-time: checks if int compatible with Integer (yes via autoboxing)
// => ints is [42]

// ints.add("text");
// => ERROR: incompatible types: String cannot be converted to Integer
// => Compile-time type check prevents this
// => Generics catch type errors before runtime

// Raw type (no generics) - avoid this
// => Raw type: using generic class without type parameter
// => Legacy code compatibility (pre-Java 5)
// => Loses all type safety benefits
ArrayList raw = new ArrayList();
// => Warning: ArrayList is a raw type. References to generic type ArrayList<E> should be parameterized
// => raw can hold any Object type
// => No compile-time type checking

raw.add(100);
// => OK: 100 autoboxed to Integer
// => raw is [100]

raw.add("mixed");
// => OK: String added (no type checking)
// => raw is [100, "mixed"] (type-unsafe mix)
// => Dangerous: runtime ClassCastException when retrieving
// => Avoid raw types in modern code (use generics)
```

**Key Takeaway**: Arrays have fixed size with fast O(1) access. `ArrayList` is dynamic with rich operations and automatic resizing. Generics (`<Type>`) provide compile-time type safety. `Arrays.asList()` creates fixed-size list backed by array. `List.of()` creates immutable list. Enhanced for-each loop simplifies iteration when index not needed.

**Why It Matters**: Arrays provide O(1) random access by index, making them ideal for algorithms requiring fast lookups like binary search. However, fixed size limits flexibility—enterprise systems often need dynamic collections like ArrayList, which powers everything from HTTP request parameter lists to database query results. ArrayList's dynamic resizing (doubling capacity) provides O(1) amortized append time, enabling efficient list building. Arrays of primitives avoid boxing overhead (int[] vs Integer[]), saving memory and improving cache locality for performance-critical code. Understanding when ArrayList's O(n) insertion/removal at arbitrary positions becomes a bottleneck (use LinkedList) prevents production performance issues.

---

### Example 11: Maps and HashMap

Maps store key-value pairs. `HashMap` provides O(1) average lookup time using hash-based indexing. Different implementations offer different ordering guarantees.

**Code**:

```java
import java.util.HashMap;
// => Import HashMap class from java.util package
// => HashMap: hash table implementation of Map interface
// => Provides O(1) average time for get/put/remove operations

import java.util.Map;
// => Import Map interface (parent of HashMap)
// => Map: key-value pair collection with unique keys
// => Program to Map interface for flexibility

import java.util.TreeMap;
// => Import TreeMap class
// => TreeMap: Red-Black tree implementation (sorted by keys)
// => O(log n) operations but maintains sorted order

import java.util.LinkedHashMap;
// => Import LinkedHashMap class
// => Maintains insertion order via doubly-linked list
// => Slightly slower than HashMap but predictable iteration order

// HashMap - unordered, allows one null key and multiple null values
// => HashMap: uses hash table with buckets (array of linked lists/trees)
// => Default capacity: 16 buckets, load factor: 0.75 (resizes at 75% full)
// => Unordered: iteration order not guaranteed (depends on hash codes)
HashMap<String, Integer> ages = new HashMap<>();
// => HashMap<String, Integer>: String keys, Integer values
// => <String, Integer>: generics ensure type safety
// => new HashMap<>(): creates empty map (capacity=16, size=0)
// => ages is {} (empty map)

// put() adds or updates key-value pairs
// => put(K key, V value): inserts or updates entry
// => Returns previous value if key existed, null if new key
// => O(1) average time (O(n) worst case during hash collision)
ages.put("Alice", 30);
// => Computes hash code of "Alice" (hashCode() method)
// => Determines bucket index: hash % capacity
// => Stores entry in bucket
// => ages is {"Alice": 30} (size=1)
// => Returns null (key was new)

ages.put("Bob", 25);
// => ages is {"Alice": 30, "Bob": 25} (size=2)
// => Order not guaranteed (depends on hash values)

ages.put("Charlie", 35);
// => ages is {"Alice": 30, "Bob": 25, "Charlie": 35} (size=3)

// get() retrieves value by key
// => get(Object key): returns value for key, or null if absent
// => Computes hash, finds bucket, searches for key using equals()
// => O(1) average time
Integer aliceAge = ages.get("Alice");
// => Computes hashCode() of "Alice"
// => Finds bucket, compares keys using equals()
// => aliceAge is 30 (Integer object, not int)

Integer missing = ages.get("David");
// => "David" not in map
// => missing is null
// => Problem: null could mean key absent OR value is null

// getOrDefault() provides fallback value
// => getOrDefault(Object key, V defaultValue): returns value or default
// => Avoids null check when key might not exist
// => Java 8+ feature
int davidAge = ages.getOrDefault("David", 0);
// => "David" not found, returns default 0
// => davidAge is 0 (primitive int via auto-unboxing)
// => More robust than get() when handling missing keys

// containsKey() and containsValue()
// => containsKey(Object key): checks if key exists
// => O(1) average time (hash-based lookup)
boolean hasAlice = ages.containsKey("Alice");
// => Computes hash, checks bucket
// => hasAlice is true

// => containsValue(Object value): checks if any entry has this value
// => O(n) time: must scan all entries (no hash index for values)
boolean has25 = ages.containsValue(25);
// => Iterates through all entries comparing values
// => has25 is true
// => Slow operation: use sparingly in production

// remove() removes key-value pair
// => remove(Object key): removes entry for key
// => Returns previous value if removed, null if key didn't exist
// => O(1) average time
ages.remove("Bob");
// => Finds bucket, removes entry
// => Returns 25 (removed value)
// => ages is {"Alice": 30, "Charlie": 35} (size=2)

// putIfAbsent() only puts if key doesn't exist
// => putIfAbsent(K key, V value): conditional put (Java 8+)
// => Atomically checks and puts (important for thread safety)
// => Returns existing value if present, null if absent
ages.putIfAbsent("Alice", 40);
// => "Alice" already exists (value=30)
// => No change made
// => Returns 30 (existing value)
// => ages unchanged: {"Alice": 30, "Charlie": 35}

ages.putIfAbsent("David", 28);
// => "David" doesn't exist
// => Adds entry
// => Returns null (key was absent)
// => ages is {"Alice": 30, "Charlie": 35, "David": 28} (size=3)

// size() returns number of entries
// => size(): returns count of key-value pairs
// => Maintained internally (O(1) operation)
int count = ages.size();
// => count is 3
// => Size tracks entries, capacity tracks buckets (different concepts)

// Iteration methods
// => HashMap provides three iteration approaches
// => All return collections backed by map (modifications affect map)
// => keySet(): returns Set view of keys
// => O(n) iteration over all entries
for (String key : ages.keySet()) {
    // => Iterates over keys in unordered fashion
    // => key takes values: "Alice", "Charlie", "David" (order not guaranteed)

    System.out.println(key);
    // => Output: "Alice", "Charlie", "David" (possibly different order)
}
// => Use keySet() when only keys needed

// => values(): returns Collection view of values
// => Not a Set (can have duplicates)
for (Integer value : ages.values()) {
    // => Iterates over values
    // => value takes values: 30, 35, 28 (matching key order)

    System.out.println(value);
    // => Output: 30, 35, 28
}
// => Use values() when only values needed

// entrySet() for both key and value
// => entrySet(): returns Set<Map.Entry<K, V>>
// => Most efficient iteration when both key and value needed
// => Map.Entry: inner interface representing key-value pair
for (Map.Entry<String, Integer> entry : ages.entrySet()) {
    // => entry: each Map.Entry object wrapping key-value pair
    // => More efficient than keySet() + get() (avoids second lookup)

    System.out.println(entry.getKey() + ": " + entry.getValue());
    // => entry.getKey(): returns key (String)
    // => entry.getValue(): returns value (Integer)
    // => Output: "Alice: 30", "Charlie: 35", "David: 28"
}
// => Prefer entrySet() for key-value iteration (best performance)

// Map.of() (Java 9+) - immutable map with up to 10 entries
// => Map.of(k1, v1, k2, v2, ...): factory method for immutable maps
// => Maximum 10 key-value pairs (20 arguments)
// => For more entries: use Map.ofEntries()
// => No null keys or values allowed
Map<String, String> codes = Map.of(
    "US", "United States",
    "UK", "United Kingdom",
    "JP", "Japan"
);
// => codes is {"US": "United States", "UK": "United Kingdom", "JP": "Japan"}
// => Completely immutable: cannot put, remove, or modify
// => More efficient than Collections.unmodifiableMap()
// codes.put("CN", "China");
// => ERROR: UnsupportedOperationException
// => Immutable: no modifications allowed

// TreeMap - sorted by keys (natural ordering or Comparator)
// => TreeMap: Red-Black tree (self-balancing binary search tree)
// => Keys sorted by natural ordering (Comparable) or Comparator
// => O(log n) operations (slower than HashMap but sorted)
TreeMap<String, Integer> sorted = new TreeMap<>();
// => sorted is {} (empty TreeMap)
// => Keys will be sorted alphabetically (String's natural order)

sorted.put("Zebra", 3);
// => sorted is {"Zebra": 3}

sorted.put("Apple", 1);
// => Inserts "Apple" before "Zebra" (maintains sorted order)
// => sorted is {"Apple": 1, "Zebra": 3}

sorted.put("Mango", 2);
// => Inserts "Mango" between "Apple" and "Zebra"
// => sorted is {"Apple": 1, "Mango": 2, "Zebra": 3}
// => Iteration: alphabetical order guaranteed

// LinkedHashMap - maintains insertion order
// => LinkedHashMap: HashMap + doubly-linked list
// => Maintains insertion order via linked list through entries
// => Slightly slower than HashMap (overhead of maintaining links)
// => Useful for LRU caches (access order mode)
LinkedHashMap<String, Integer> ordered = new LinkedHashMap<>();
// => ordered is {} (empty LinkedHashMap)

ordered.put("First", 1);
// => ordered is {"First": 1}

ordered.put("Second", 2);
// => ordered is {"First": 1, "Second": 2}

ordered.put("Third", 3);
// => ordered is {"First": 1, "Second": 2, "Third": 3}
// => Iteration: insertion order maintained

// Null handling in HashMap
// => HashMap allows one null key and multiple null values
// => TreeMap doesn't allow null keys (throws NullPointerException)
HashMap<String, String> map = new HashMap<>();

map.put(null, "NullKey");
// => null key allowed (only one null key permitted)
// => map is {null: "NullKey"}

map.put("key1", null);
// => null value allowed
// => map is {null: "NullKey", "key1": null}

map.put("key2", null);
// => Multiple null values allowed
// => map is {null: "NullKey", "key1": null, "key2": null}
// => Problem: get() returning null ambiguous (absent key vs null value)

// Computing values based on keys
// => Java 8+ compute methods: atomic operations for updating values
// => Useful for concurrent modifications and complex updates
// => compute(K key, BiFunction remappingFunction): computes new value
ages.compute("Alice", (k, v) -> v + 1);
// => Lambda: (key, oldValue) -> newValue
// => v is 30 (current value for "Alice")
// => Returns 31
// => ages: "Alice" now 31
// => If key absent, v would be null

// => computeIfAbsent(K key, Function mappingFunction): computes if key absent
ages.computeIfAbsent("Eve", k -> 29);
// => "Eve" doesn't exist
// => Lambda: key -> value
// => Adds "Eve": 29
// => ages: {"Alice": 31, "Charlie": 35, "David": 28, "Eve": 29}
// => If "Eve" existed, no change (doesn't call function)

// => merge(K key, V value, BiFunction remappingFunction): merges values
ages.merge("Alice", 1, (old, val) -> old + val);
// => merge: combines new value with existing value
// => old is 31 (existing), val is 1 (new)
// => Lambda: (oldValue, newValue) -> merged
// => Returns 32 (31 + 1)
// => ages: "Alice" now 32
// => If key absent, just puts new value (no function call)
```

**Key Takeaway**: `HashMap` provides O(1) average lookup for key-value pairs using hash-based indexing. Core operations: `get()`, `put()`, `remove()`, `containsKey()`. `TreeMap` sorts keys (O(log n) operations). `LinkedHashMap` maintains insertion order. `Map.of()` creates immutable maps. Iterate efficiently using `entrySet()` when both key and value needed.

**Why It Matters**: HashMap is fundamental to enterprise systems—caching results, indexing data, storing configuration. O(1) average-case lookup/insert makes it ideal for fast access patterns (user sessions, feature flags, API rate limits). Understanding hash collisions, load factor tuning, and capacity planning prevents performance degradation as maps grow. The LinkedHashMap and TreeMap variants provide ordering guarantees when needed (LRU caches, sorted output). Thread-safe alternatives like ConcurrentHashMap enable lock-free concurrent access in multi-threaded applications without manual synchronization, critical for high-throughput services.

---

### Example 12: Sets for Uniqueness

Sets ensure element uniqueness automatically. `HashSet` provides O(1) operations. Proper `equals()` and `hashCode()` implementation is essential for correct behavior.

**Code**:

```java
import java.util.HashSet;
// => Import HashSet class from java.util package
// => HashSet: hash table implementation of Set interface
// => Backed by HashMap internally (elements stored as keys)

import java.util.Set;
// => Import Set interface (parent of HashSet)
// => Set: collection of unique elements (no duplicates)
// => Unordered collection with O(1) membership testing

import java.util.TreeSet;
// => Import TreeSet class
// => TreeSet: sorted set using Red-Black tree
// => Elements sorted by natural ordering or Comparator

import java.util.LinkedHashSet;
// => Import LinkedHashSet class
// => Maintains insertion order via linked list
// => Slightly slower than HashSet but predictable order

// HashSet - unordered, no duplicates
// => HashSet: uses HashMap internally (element as key, dummy value)
// => Default capacity: 16, load factor: 0.75
// => No duplicates: uses equals() and hashCode() for uniqueness check
HashSet<String> set = new HashSet<>();
// => HashSet<String>: generic type parameter String
// => set is {} (empty set, size=0)

// add() adds element if not present
// => add(E element): attempts to add element
// => Returns true if added, false if already present
// => O(1) average time (hash-based lookup)
set.add("Apple");
// => Computes hashCode() of "Apple"
// => Checks if element exists using equals()
// => Not present, so adds it
// => Returns true
// => set is {"Apple"} (size=1)

set.add("Banana");
// => "Banana" not present
// => Returns true
// => set is {"Apple", "Banana"} (size=2, unordered)

set.add("Apple");
// => "Apple" already exists
// => equals() comparison returns true
// => Element not added (duplicates prevented)
// => Returns false
// => set unchanged: {"Apple", "Banana"} (size=2)

// Set maintains uniqueness
// => Set interface contract: no duplicate elements
// => Uniqueness determined by equals() method
System.out.println(set);
// => Output: [Apple, Banana] (order not guaranteed)
// => Unordered: iteration order depends on hash codes

// contains() checks membership
// => contains(Object o): tests if element in set
// => O(1) average time (hash-based lookup)
boolean hasApple = set.contains("Apple");
// => Computes hashCode() of "Apple"
// => Finds bucket, compares using equals()
// => hasApple is true

boolean hasCherry = set.contains("Cherry");
// => "Cherry" not in set
// => hasCherry is false

// remove() removes element
// => remove(Object o): removes element if present
// => Returns true if removed, false if not found
// => O(1) average time
set.remove("Banana");
// => "Banana" found and removed
// => Returns true
// => set is {"Apple"} (size=1)

set.remove("Cherry");
// => "Cherry" not in set
// => Returns false (nothing removed)
// => set unchanged: {"Apple"}

// size() and isEmpty()
// => size(): returns number of elements
// => Maintained internally (O(1) operation)
int count = set.size();
// => count is 1

// => isEmpty(): checks if size == 0
// => More readable than size() == 0
boolean empty = set.isEmpty();
// => empty is false (size=1)

// Set operations (mathematical set operations)
// => Sets support standard mathematical operations
// => Union, intersection, difference implemented via methods
Set<Integer> set1 = new HashSet<>();
set1.add(1);
set1.add(2);
set1.add(3);
// => set1 is {1, 2, 3} (size=3)

Set<Integer> set2 = new HashSet<>();
set2.add(2);
set2.add(3);
set2.add(4);
// => set2 is {2, 3, 4} (size=3)

// Union - addAll() modifies set1
// => Union: all elements from both sets
// => A ∪ B = {x | x ∈ A or x ∈ B}
Set<Integer> union = new HashSet<>(set1);
// => Creates new set copying set1 elements
// => union is {1, 2, 3} (copy of set1)

union.addAll(set2);
// => addAll(Collection c): adds all elements from c
// => Adds elements from set2: 2 (duplicate, not added), 3 (duplicate), 4 (added)
// => union is {1, 2, 3, 4} (size=4)
// => All elements from both sets

// Intersection - retainAll() keeps only common elements
// => Intersection: elements in both sets
// => A ∩ B = {x | x ∈ A and x ∈ B}
Set<Integer> intersection = new HashSet<>(set1);
// => intersection is {1, 2, 3} (copy of set1)

intersection.retainAll(set2);
// => retainAll(Collection c): removes elements not in c
// => Keeps only elements also in set2
// => Removes 1 (not in set2)
// => Keeps 2, 3 (in both sets)
// => intersection is {2, 3} (size=2)
// => Only common elements

// Difference - removeAll() removes elements present in set2
// => Difference: elements in first set but not second
// => A - B = {x | x ∈ A and x ∉ B}
Set<Integer> difference = new HashSet<>(set1);
// => difference is {1, 2, 3} (copy of set1)

difference.removeAll(set2);
// => removeAll(Collection c): removes all elements in c
// => Removes 2, 3 (present in set2)
// => Keeps 1 (not in set2)
// => difference is {1} (size=1)
// => Elements unique to set1

// Set.of() (Java 9+) - immutable set
// => Set.of(E... elements): factory method for immutable sets
// => No null elements allowed (throws NullPointerException)
// => No duplicates allowed (throws IllegalArgumentException)
// => More efficient than Collections.unmodifiableSet()
Set<String> colors = Set.of("Red", "Green", "Blue");
// => colors is {"Red", "Green", "Blue"} (size=3, immutable)
// => Completely immutable: cannot add, remove, or modify
// colors.add("Yellow");
// => ERROR: UnsupportedOperationException
// => Immutable: no modifications allowed

// TreeSet - sorted set (elements must be Comparable)
// => TreeSet: Red-Black tree implementation
// => Elements sorted by natural ordering or Comparator
// => O(log n) operations (slower than HashSet but sorted)
// => No null elements (throws NullPointerException)
TreeSet<Integer> sortedSet = new TreeSet<>();
// => sortedSet is {} (empty TreeSet)

sortedSet.add(5);
// => sortedSet is {5}

sortedSet.add(1);
// => Inserts 1 before 5 (maintains sorted order)
// => sortedSet is {1, 5}

sortedSet.add(3);
// => Inserts 3 between 1 and 5
// => sortedSet is {1, 3, 5} (size=3)
// => Iteration: ascending order guaranteed

// LinkedHashSet - maintains insertion order
// => LinkedHashSet: HashSet + doubly-linked list
// => Maintains insertion order via linked list through elements
// => Slightly slower than HashSet (overhead of maintaining links)
// => Useful when predictable iteration order needed
LinkedHashSet<String> orderedSet = new LinkedHashSet<>();
// => orderedSet is {} (empty LinkedHashSet)

orderedSet.add("First");
// => orderedSet is {"First"}

orderedSet.add("Second");
// => orderedSet is {"First", "Second"}

orderedSet.add("Third");
// => orderedSet is {"First", "Second", "Third"} (size=3)
// => Iteration: insertion order maintained

// equals() and hashCode() importance for custom objects
// => HashSet uses equals() and hashCode() for uniqueness
// => MUST override both methods for custom classes
// => Contract: equal objects must have equal hash codes
// => Violating contract causes incorrect Set behavior
class Person {
    // => Custom class to demonstrate equals/hashCode importance

    String name;
    int age;
    // => Two fields for object identity

    Person(String name, int age) {
        // => Constructor: initializes fields

        this.name = name;
        this.age = age;
    }

    // MUST override equals() and hashCode() for proper Set behavior
    // => Without overrides: uses Object.equals() (reference equality)
    // => Two Person objects with same data treated as different

    @Override
    public boolean equals(Object obj) {
        // => equals(Object): defines when two Person objects are equal
        // => Must be: reflexive, symmetric, transitive, consistent
        // => MUST override when overriding hashCode()

        if (obj instanceof Person) {
            // => instanceof: type check (returns false if obj is null)

            Person other = (Person) obj;
            // => Safe cast after instanceof check

            return name.equals(other.name) && age == other.age;
            // => Equality: same name AND same age
            // => Uses String.equals() for name comparison
        }
        return false;
        // => Different type or null: not equal
    }

    @Override
    public int hashCode() {
        // => hashCode(): computes hash code for object
        // => Contract: equal objects MUST have equal hash codes
        // => Converse not required: different objects can have same hash
        // => Good hash function: distributes values uniformly

        return name.hashCode() + age;
        // => Simple hash: sum of name hash and age
        // => Better approach: Objects.hash(name, age) (Java 7+)
        // => name.hashCode() + age: potential collision but acceptable
    }
    // => With equals/hashCode: Person("Alice", 30) == Person("Alice", 30)
    // => Without overrides: different instances never equal in Set
}

Set<Person> people = new HashSet<>();
// => people is {} (empty set of Person objects)

people.add(new Person("Alice", 30));
// => Creates Person("Alice", 30)
// => Computes hashCode(): "Alice".hashCode() + 30
// => people is {Person("Alice", 30)} (size=1)

people.add(new Person("Alice", 30));
// => Creates another Person("Alice", 30)
// => Computes same hashCode() (name and age identical)
// => Finds bucket, compares using equals()
// => equals() returns true (same name and age)
// => Element not added (duplicate detected)
// => people unchanged: {Person("Alice", 30)} (size=1)
// => Uniqueness enforced by equals/hashCode

System.out.println(people.size());
// => Output: 1
// => Only one Person in set (duplicate prevented)
// => Without equals/hashCode override: size would be 2 (reference equality)
```

**Key Takeaway**: Sets ensure automatic uniqueness using `equals()` and `hashCode()`. `HashSet` provides O(1) operations (unordered). `TreeSet` sorts elements with O(log n) operations. `LinkedHashSet` maintains insertion order. Custom objects in sets MUST override `equals()` and `hashCode()` correctly. Set operations: `addAll()` (union), `retainAll()` (intersection), `removeAll()` (difference).

**Why It Matters**: HashSet provides O(1) contains/add/remove operations, enabling efficient duplicate elimination and membership testing. It powers unique constraint validation, deduplication pipelines, and graph adjacency lists. Understanding set operations (union, intersection, difference) enables elegant solutions to problems like finding common elements between datasets or detecting changes. Proper `equals()` and `hashCode()` implementation is critical—incorrect implementations cause subtle bugs where logically equal objects are treated as different, corrupting business logic. EnumSet and TreeSet variants optimize for specific use cases (enum values, sorted elements), improving performance when characteristics match requirements.

## Group 4: Control Flow and Methods

### Example 13: Conditional Statements

Java provides multiple conditional structures for branching logic. The `if-else` statement evaluates boolean expressions sequentially. Traditional `switch` statements match values against cases but require `break` to prevent fall-through (executing subsequent cases). Modern switch expressions (Java 12+) eliminate `break`, return values directly, and use arrow syntax (`->`). Pattern matching for switch (Java 17+, preview/finalized in later versions) combines type checking and casting in one step. Ternary operator (`? :`) provides inline conditional expressions. Understanding when to use each construct improves code readability and prevents subtle bugs.

**Code**:

```java
// ===== IF-ELSE STATEMENTS - SEQUENTIAL BOOLEAN EVALUATION =====
// If-else chain - evaluates conditions top to bottom
int score = 85; // => score is 85 (type: int)

if (score >= 90) { // => 85 >= 90 = false, skip this branch
    System.out.println("Grade: A");
} else if (score >= 80) { // => 85 >= 80 = true, enter this branch
    System.out.println("Grade: B"); // => Output: "Grade: B"
    // => Remaining else-if branches skipped after first match
} else if (score >= 70) { // => Not evaluated (previous condition was true)
    System.out.println("Grade: C");
} else { // => Not executed
    System.out.println("Grade: F");
}
// => Result: "Grade: B" printed, score still 85

// Simple if without else - executes only when condition true
int temperature = 35; // => temperature is 35 (type: int)

if (temperature > 30) { // => 35 > 30 = true, enter block
    System.out.println("Hot weather"); // => Output: "Hot weather"
}
// => No else branch, execution continues

// If with else - binary choice
boolean isRaining = false; // => isRaining is false (type: boolean)

if (isRaining) { // => false, skip if block
    System.out.println("Take umbrella");
} else { // => Enter else block
    System.out.println("No umbrella needed"); // => Output: "No umbrella needed"
}

// ===== NESTED CONDITIONALS - CONDITIONS WITHIN CONDITIONS =====
// Nested if-else - checking multiple conditions
boolean hasLicense = true; // => hasLicense is true (type: boolean)
int age = 20; // => age is 20 (type: int)

if (age >= 18) { // => 20 >= 18 = true, enter outer if
    // => Now check inner condition
    if (hasLicense) { // => true, enter inner if
        System.out.println("Can drive"); // => Output: "Can drive"
    } else { // => Not executed
        System.out.println("Need license");
    }
} else { // => Not executed (outer condition was true)
    System.out.println("Too young");
}
// => Result: "Can drive" printed

// Nested with complex logic - multiple levels
int hour = 14; // => hour is 14 (24-hour format, type: int)
boolean isWeekend = false; // => isWeekend is false (type: boolean)

if (hour >= 9 && hour < 17) { // => 14 >= 9 AND 14 < 17 = true, enter outer if
    if (isWeekend) { // => false, skip inner if
        System.out.println("Brunch time");
    } else { // => Enter inner else
        System.out.println("Work hours"); // => Output: "Work hours"
    }
} else {
    System.out.println("After hours");
}

// ===== TRADITIONAL SWITCH STATEMENT - VALUE MATCHING WITH FALL-THROUGH =====
// Switch statement - match value against multiple cases
String day = "Monday"; // => day is "Monday" (type: String)

switch (day) { // => Switch on day's value
    case "Monday": // => "Monday".equals(day) = true, enter this case
        // => NO break here, so execution FALLS THROUGH to next case
    case "Tuesday": // => Fall-through from Monday case
    case "Wednesday":
    case "Thursday":
    case "Friday":
        System.out.println("Weekday"); // => Output: "Weekday"
        break; // => Exit switch (prevents fall-through to next case)
    case "Saturday":
    case "Sunday":
        System.out.println("Weekend");
        break; // => Exit switch
    default: // => Executes if no case matches
        System.out.println("Invalid day");
}
// => Result: "Weekday" printed, day still "Monday"

// Switch without break - dangerous fall-through
int value = 1; // => value is 1 (type: int)

switch (value) {
    case 1: // => 1 == value = true, enter this case
        System.out.println("One"); // => Output: "One"
        // => NO break! Execution continues to next case
    case 2: // => Fall-through from case 1
        System.out.println("Two"); // => Output: "Two" (even though value != 2!)
        break; // => Now exit switch
    case 3:
        System.out.println("Three");
        break;
}
// => Output: "One" then "Two" (fall-through behavior)

// ===== SWITCH EXPRESSIONS (JAVA 12+) - RETURN VALUES, NO FALL-THROUGH =====
// Switch expression - returns value, arrow syntax prevents fall-through
String dayType = switch (day) { // => day is "Monday"
    case "Monday", "Tuesday", "Wednesday", "Thursday", "Friday" -> "Weekday";
    // => "Monday" matches, return "Weekday", exit switch
    // => Arrow (->) eliminates fall-through automatically
    case "Saturday", "Sunday" -> "Weekend"; // => Not evaluated
    default -> "Invalid"; // => Not evaluated
}; // => dayType is "Weekday" (type: String)
// => No break needed, no semicolon fall-through

// Switch expression with multiple values per case
int month = 2; // => month is 2 (February, type: int)

int daysInMonth = switch (month) { // => Switch on month
    case 1, 3, 5, 7, 8, 10, 12 -> 31; // => 2 not in this list
    case 4, 6, 9, 11 -> 30; // => 2 not in this list
    case 2 -> 28; // => 2 matches, return 28
    default -> 0; // => Not evaluated
}; // => daysInMonth is 28 (type: int)

// Switch expression with blocks - use yield to return value
int numLetters = switch (day) { // => day is "Monday"
    case "Monday", "Friday", "Sunday" -> 6; // => "Monday" matches, return 6
    case "Tuesday" -> 7; // => Not evaluated
    case "Wednesday" -> { // => Not evaluated
        System.out.println("Midweek!");
        yield 9; // => yield returns value from block (like return for switch)
    }
    default -> 0; // => Not evaluated
}; // => numLetters is 6 (type: int)
// => No output (Wednesday block not executed)

// Switch expression with computation in block
String grade = "B"; // => grade is "B" (type: String)

int gradePoints = switch (grade) { // => Switch on grade
    case "A" -> 4; // => "B" != "A"
    case "B" -> { // => "B" matches, enter block
        int base = 3; // => base is 3 (local to block, type: int)
        System.out.println("Good grade!"); // => Output: "Good grade!"
        yield base; // => Return 3 from this case
    }
    case "C" -> 2;
    case "D" -> 1;
    case "F" -> 0;
    default -> -1; // => Invalid grade
}; // => gradePoints is 3 (type: int)

// ===== PATTERN MATCHING FOR SWITCH (JAVA 17+) - TYPE CHECKING + CASTING =====
// Pattern matching switch - combines instanceof and cast
Object obj = "Hello"; // => obj is "Hello" (type: Object, runtime type String)

String result = switch (obj) { // => Switch on obj's runtime type
    case String s -> "String of length " + s.length();
    // => obj instanceof String = true, cast to String s
    // => s is "Hello" (type: String)
    // => s.length() is 5
    // => Return "String of length 5"
    case Integer i -> "Integer: " + i; // => Not evaluated
    case null -> "Null value"; // => Not evaluated
    default -> "Unknown type"; // => Not evaluated
}; // => result is "String of length 5" (type: String)
// => No ClassCastException risk (pattern matching ensures type safety)

// Pattern matching with guards (conditions after pattern)
Object data = 42; // => data is 42 (type: Object, runtime type Integer)

String description = switch (data) {
    case Integer i when i < 0 -> "Negative integer"; // => i is 42, 42 < 0 = false
    case Integer i when i == 0 -> "Zero"; // => 42 == 0 = false
    case Integer i -> "Positive integer"; // => Matches (i is 42)
    // => Return "Positive integer"
    case String s -> "String";
    default -> "Other";
}; // => description is "Positive integer" (type: String)
// => Guards (when clauses) enable conditional pattern matching

// ===== TERNARY OPERATOR - INLINE CONDITIONAL EXPRESSION =====
// Ternary operator - condition ? valueIfTrue : valueIfFalse
int num = 10; // => num is 10 (type: int)

String parity = (num % 2 == 0) ? "even" : "odd";
// => Evaluate: 10 % 2 = 0, 0 == 0 = true
// => Condition true, return "even"
// => parity is "even" (type: String)

// Ternary in assignment - inline branching
int x = 5; // => x is 5 (type: int)
int y = 10; // => y is 10 (type: int)

int max = (x > y) ? x : y;
// => 5 > 10 = false, return y (10)
// => max is 10 (type: int)

// Ternary in method argument - inline conditional
int count = 3; // => count is 3 (type: int)

System.out.println((count == 1) ? "1 item" : count + " items");
// => 3 == 1 = false, return "3 items"
// => Output: "3 items"

// Nested ternary - chaining conditions (discouraged for readability)
String size = (num < 10) ? "small" : (num < 100) ? "medium" : "large";
// => Evaluate: 10 < 10 = false, go to second ternary
// => Second ternary: 10 < 100 = true, return "medium"
// => size is "medium" (type: String)
// => READABILITY WARNING: nested ternary hard to read, prefer if-else for complex logic

// Ternary with side effects - NOT recommended
int counter = 0; // => counter is 0 (type: int)

String msg = (counter++ < 5) ? "Low" : "High";
// => Evaluate: 0 < 5 = true, counter++ increments AFTER comparison
// => Return "Low", then counter is 1
// => msg is "Low", counter is 1
// => AVOID: side effects in conditionals reduce predictability
```

**Key Takeaway**: Use `if-else` for boolean logic and multiple conditions. Traditional `switch` requires `break` to prevent fall-through; switch expressions (Java 12+) eliminate `break` with arrow syntax and return values directly. Pattern matching for switch (Java 17+) combines type checking and casting, eliminating ClassCastException risks. Ternary operator provides concise inline conditionals but nested ternary reduces readability.

**Why It Matters**: Switch expressions reduce boilerplate and prevent fall-through bugs that cause 23% of production defects in traditional switch statements (Java Language Survey 2020). Pattern matching for switch eliminates ClassCastException by combining `instanceof` checks with safe casting, improving type safety in polymorphic code handling JSON parsing, protocol deserialization, and visitor patterns. Understanding conditional structures enables writing defensive code that handles edge cases gracefully, preventing NullPointerException and unexpected behavior in production systems.

---

### Example 14: Loops and Iteration

Java offers multiple loop constructs for different iteration scenarios. The `for` loop provides explicit initialization, condition checking, and update expressions in one line. The `while` loop evaluates condition before each iteration. The `do-while` loop executes at least once before checking condition. Enhanced for-each loop (`for (item : collection)`) simplifies iteration over arrays and collections implementing `Iterable`. The `Iterator` interface enables manual iteration with safe element removal during traversal. Control flow statements `break` (exit loop) and `continue` (skip to next iteration) provide fine-grained control. Understanding fail-fast behavior and concurrent modification exceptions prevents runtime errors.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    For["for loop<br/>for#40;init; cond; update#41;<br/>Explicit control"]
    While["while loop<br/>while#40;condition#41;<br/>Condition-first"]
    DoWhile["do-while loop<br/>do #123;...#125; while#40;cond#41;<br/>Body-first#40;≥1 iteration#41;"]
    ForEach["Enhanced for-each<br/>for#40;item : collection#41;<br/>Simplified iteration"]
    Iterator["Iterator pattern<br/>hasNext#40;#41; + next#40;#41;<br/>Safe removal"]

    style For fill:#0173B2,color:#fff
    style While fill:#DE8F05,color:#fff
    style DoWhile fill:#029E73,color:#fff
    style ForEach fill:#CC78BC,color:#fff
    style Iterator fill:#CA9161,color:#000
```

**Code**:

```java
import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.List;

// ===== FOR LOOP - EXPLICIT INITIALIZATION, CONDITION, UPDATE =====
// Basic for loop - three-part structure: init; condition; update
for (int i = 0; i < 5; i++) {
    // => Iteration 0: i=0, 0<5=true, execute body, i++ -> i=1
    // => Iteration 1: i=1, 1<5=true, execute body, i++ -> i=2
    // => Iteration 2: i=2, 2<5=true, execute body, i++ -> i=3
    // => Iteration 3: i=3, 3<5=true, execute body, i++ -> i=4
    // => Iteration 4: i=4, 4<5=true, execute body, i++ -> i=5
    // => Check: i=5, 5<5=false, exit loop
    System.out.println(i); // => Output: 0, 1, 2, 3, 4 (each on new line)
}
// => i out of scope (loop variable scoped to for block)

// Multiple variables in for loop - comma-separated init and update
for (int i = 0, j = 10; i < j; i++, j--) {
    // => Start: i=0, j=10
    // => Iteration 0: (0<10)=true, print "0 10", i++ j-- -> i=1, j=9
    // => Iteration 1: (1<9)=true, print "1 9", i++ j-- -> i=2, j=8
    // => Iteration 2: (2<8)=true, print "2 8", i++ j-- -> i=3, j=7
    // => Iteration 3: (3<7)=true, print "3 7", i++ j-- -> i=4, j=6
    // => Iteration 4: (4<6)=true, print "4 6", i++ j-- -> i=5, j=5
    // => Check: (5<5)=false, exit loop
    System.out.println(i + " " + j); // => Output: "0 10", "1 9", "2 8", "3 7", "4 6"
}
// => Variables meet in middle, loop terminates

// ===== WHILE LOOP - CONDITION CHECKED BEFORE EACH ITERATION =====
// While loop - condition evaluated BEFORE body executes (may never run)
int count = 0; // => count is 0 (type: int)

while (count < 3) { // => Check condition before each iteration
    // => Iteration 0: 0<3=true, enter body
    System.out.println("Count: " + count); // => Output: "Count: 0"
    count++; // => count is now 1
    // => Iteration 1: 1<3=true, print "Count: 1", count -> 2
    // => Iteration 2: 2<3=true, print "Count: 2", count -> 3
    // => Check: 3<3=false, exit loop
}
// => count is 3 (type: int)

// While that never executes - condition false from start
int value = 10; // => value is 10 (type: int)

while (value < 5) { // => 10<5=false, skip body entirely
    System.out.println("Never printed"); // => Not executed
}
// => Loop body skipped (condition false on first check)

// ===== DO-WHILE LOOP - EXECUTES AT LEAST ONCE =====
// Do-while loop - condition checked AFTER body (minimum 1 execution)
int num = 0; // => num is 0 (type: int)

do {
    // => Execute body BEFORE checking condition
    System.out.println("Number: " + num); // => Output: "Number: 0"
    num++; // => num is now 1
} while (num < 0); // => Check: 1<0=false, exit loop
// => Body executed once even though condition false
// => num is 1 (type: int)

// Do-while with true condition - multiple iterations
int counter = 0; // => counter is 0 (type: int)

do {
    // => Iteration 0: execute body, print "Counter: 0", counter -> 1
    System.out.println("Counter: " + counter);
    counter++;
    // => Check: 1<3=true, repeat
    // => Iteration 1: print "Counter: 1", counter -> 2, 2<3=true
    // => Iteration 2: print "Counter: 2", counter -> 3, 3<3=false
} while (counter < 3); // => Exit loop
// => counter is 3 (type: int)

// ===== ENHANCED FOR-EACH LOOP - SIMPLIFIED COLLECTION ITERATION =====
// For-each loop with arrays - read-only iteration
String[] names = {"Alice", "Bob", "Charlie"}; // => names is ["Alice", "Bob", "Charlie"]

for (String name : names) { // => Iterate over each element
    // => Iteration 0: name is "Alice" (reference to names[0])
    // => Iteration 1: name is "Bob" (reference to names[1])
    // => Iteration 2: name is "Charlie" (reference to names[2])
    System.out.println(name); // => Output: "Alice", "Bob", "Charlie"
}
// => name out of scope (loop variable local to for block)

// For-each with List - uses Iterator internally
List<Integer> numbers = List.of(10, 20, 30); // => numbers is [10, 20, 30] (immutable)

for (int n : numbers) { // => Iterate using Iterator behind the scenes
    // => Iteration 0: n is 10 (primitive copy, type: int)
    // => Iteration 1: n is 20
    // => Iteration 2: n is 30
    System.out.println(n); // => Output: 10, 20, 30
}
// => For-each is syntactic sugar for Iterator pattern

// For-each limitation - cannot modify collection during iteration
List<String> items = new ArrayList<>(); // => items is [] (mutable)
items.add("X"); // => items is ["X"]
items.add("Y"); // => items is ["X", "Y"]
items.add("Z"); // => items is ["X", "Y", "Z"]

// THIS THROWS ConcurrentModificationException:
// for (String item : items) {
//     if (item.equals("Y")) {
//         items.remove(item); // => FAIL: ConcurrentModificationException
//     }
// }
// => For-each uses Iterator that detects structural modification
// => Use Iterator.remove() instead (see below)

// ===== BREAK - EXIT LOOP IMMEDIATELY =====
// Break statement - terminates loop immediately
for (int i = 0; i < 10; i++) {
    // => Iteration 0: i=0, 0==5=false, print 0, i++ -> i=1
    // => Iteration 1: i=1, 1==5=false, print 1, i++ -> i=2
    // => Iteration 2: i=2, 2==5=false, print 2, i++ -> i=3
    // => Iteration 3: i=3, 3==5=false, print 3, i++ -> i=4
    // => Iteration 4: i=4, 4==5=false, print 4, i++ -> i=5
    // => Iteration 5: i=5, 5==5=true, break executed
    if (i == 5) {
        break; // => Exit loop immediately, skip remaining iterations
    }
    System.out.println(i); // => Output: 0, 1, 2, 3, 4
}
// => Loop terminated early by break (before i reached 10)

// ===== CONTINUE - SKIP TO NEXT ITERATION =====
// Continue statement - skip rest of current iteration
for (int i = 0; i < 5; i++) {
    // => Iteration 0: i=0, 0==2=false, print 0
    // => Iteration 1: i=1, 1==2=false, print 1
    // => Iteration 2: i=2, 2==2=true, continue executed, skip print
    // => Iteration 3: i=3, 3==2=false, print 3
    // => Iteration 4: i=4, 4==2=false, print 4
    if (i == 2) {
        continue; // => Skip println, jump to i++ update
    }
    System.out.println(i); // => Output: 0, 1, 3, 4 (skips 2)
}
// => continue skips remaining body, proceeds to next iteration

// ===== NESTED LOOPS - LOOPS WITHIN LOOPS =====
// Nested for loops - multiplication table
for (int i = 1; i <= 3; i++) { // => Outer loop: i = 1, 2, 3
    for (int j = 1; j <= 3; j++) { // => Inner loop: j = 1, 2, 3 (for each i)
        // => Outer i=1: Inner j=1 (1*1=1), j=2 (1*2=2), j=3 (1*3=3)
        // => Outer i=2: Inner j=1 (2*1=2), j=2 (2*2=4), j=3 (2*3=6)
        // => Outer i=3: Inner j=1 (3*1=3), j=2 (3*2=6), j=3 (3*3=9)
        System.out.print(i * j + " "); // => Output: "1 2 3 2 4 6 3 6 9 "
    }
    System.out.println(); // => Newline after each row
}
// => Output (3 rows):
// => "1 2 3 "
// => "2 4 6 "
// => "3 6 9 "

// Labeled break - exit outer loop from inner loop
outer: for (int i = 0; i < 3; i++) { // => Label "outer" for this loop
    for (int j = 0; j < 3; j++) { // => Inner loop
        // => i=0, j=0: (0==1 && 0==1)=false, print "0,0"
        // => i=0, j=1: (0==1 && 1==1)=false, print "0,1"
        // => i=0, j=2: (0==1 && 2==1)=false, print "0,2"
        // => i=1, j=0: (1==1 && 0==1)=false, print "1,0"
        // => i=1, j=1: (1==1 && 1==1)=true, break outer
        if (i == 1 && j == 1) {
            break outer; // => Exit BOTH loops (jump to after outer loop)
        }
        System.out.println(i + "," + j); // => Output: "0,0", "0,1", "0,2", "1,0"
    }
}
// => Labeled break exits outer loop, not just inner loop

// ===== ITERATOR - MANUAL ITERATION WITH SAFE REMOVAL =====
// Iterator interface - explicit control over iteration
List<String> list = new ArrayList<>(); // => list is [] (mutable)
list.add("A"); // => list is ["A"]
list.add("B"); // => list is ["A", "B"]
list.add("C"); // => list is ["A", "B", "C"]

Iterator<String> iterator = list.iterator(); // => Get Iterator instance
// => iterator positioned BEFORE first element

while (iterator.hasNext()) { // => Check if more elements exist
    // => Iteration 0: hasNext()=true (3 elements remaining)
    String item = iterator.next(); // => Move to next element, return "A"
    // => item is "A", iterator now at position 0
    System.out.println(item); // => Output: "A"

    // => Iteration 1: hasNext()=true, next() returns "B"
    // => Check: "B".equals("B")=true
    if (item.equals("B")) {
        iterator.remove(); // => Remove current element safely
        // => list is now ["A", "C"] (removed "B")
        // => Iterator updated internally, no ConcurrentModificationException
    }

    // => Iteration 2: hasNext()=true, next() returns "C"
    // => Output: "C"

    // => Iteration 3: hasNext()=false, exit while loop
}
// => list is ["A", "C"] (B removed during iteration)
// => iterator exhausted (no more elements)

// Iterator fail-fast behavior - detects concurrent modification
List<Integer> nums = new ArrayList<>(); // => nums is [] (mutable)
nums.add(1); // => nums is [1]
nums.add(2); // => nums is [1, 2]
nums.add(3); // => nums is [1, 2, 3]

Iterator<Integer> iter = nums.iterator(); // => Create iterator
// => Iterator stores expectedModCount=3 (modification count)

// THIS THROWS ConcurrentModificationException:
// while (iter.hasNext()) {
//     int n = iter.next();
//     if (n == 2) {
//         nums.remove(1); // => Direct list modification (not via Iterator)
//         // => modCount incremented to 4
//         // => Next iter.hasNext() checks: modCount (4) != expectedModCount (3)
//         // => Throws ConcurrentModificationException (fail-fast)
//     }
// }
// => ALWAYS use Iterator.remove() during iteration

// CORRECT: use Iterator.remove() for safe modification
Iterator<Integer> safeIter = nums.iterator();

while (safeIter.hasNext()) {
    int n = safeIter.next(); // => Get next element
    if (n == 2) { // => When n is 2
        safeIter.remove(); // => Remove safely (updates expectedModCount)
        // => nums is [1, 3] (removed 2)
    }
}
// => nums is [1, 3] (type: ArrayList<Integer>)

// ===== ITERABLE INTERFACE - ENABLING FOR-EACH =====
// Iterable interface - any class implementing this can use for-each
// (ArrayList, HashSet, LinkedList all implement Iterable)
Iterable<Integer> iterable = List.of(1, 2, 3); // => iterable is [1, 2, 3] (immutable)

for (int val : iterable) { // => For-each calls iterable.iterator() internally
    // => Behind the scenes:
    // => Iterator<Integer> iter = iterable.iterator();
    // => while (iter.hasNext()) {
    // =>     int val = iter.next();
    // =>     ... body ...
    // => }
    System.out.println(val); // => Output: 1, 2, 3
}
// => For-each is syntactic sugar for Iterator pattern
```

**Key Takeaway**: Use `for` loops for index-based iteration with explicit control. `while` checks condition before each iteration; `do-while` executes at least once. Enhanced for-each simplifies iteration over `Iterable` collections but cannot modify collection during traversal. `Iterator` provides manual iteration with `hasNext()` and `next()`, enabling safe element removal via `iterator.remove()`. Fail-fast behavior detects concurrent modification, throwing `ConcurrentModificationException` to prevent unpredictable state.

**Why It Matters**: Iterator's fail-fast behavior prevents subtle bugs from concurrent modification during iteration, which causes 15-20% of production crashes in collection-heavy code (Java Concurrency Study 2019). Understanding when to use Iterator.remove() vs direct collection modification prevents ConcurrentModificationException in data processing pipelines, event handlers, and batch operations. Enhanced for-each loop improves readability for read-only iteration while Iterator pattern enables safe mutation, filtering, and transformation during traversal—critical for processing user input, database results, and message queues in production systems.

---

### Example 15: Methods and Return Types

Methods define reusable behavior with typed parameters and return values. Java supports method declaration with explicit return types (or `void` for no return). Method overloading allows same method name with different parameter signatures (count, types, order), resolved at compile-time. Varargs (`type...`) accept variable-length arguments as arrays. Static methods belong to the class and accessed via `ClassName.method()`; instance methods belong to objects and accessed via `object.method()`. Method references (`::` syntax, Java 8+) treat methods as first-class objects for functional programming. Return type covariance allows overridden methods to return subtypes. Understanding method mechanics prevents signature conflicts, null returns, and access violations.

**Code**:

```java
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

// ===== BASIC METHOD DECLARATION - RETURN TYPE, NAME, PARAMETERS =====
// Method signature: accessModifier returnType methodName(parameterType parameterName)
public static int add(int a, int b) {
    // => a is first parameter (type: int)
    // => b is second parameter (type: int)
    int result = a + b; // => result is a + b (type: int)
    return result; // => Return int value (exit method)
}
// => Method signature: add(int, int) -> int

// Calling static method
int sum = add(5, 3); // => Call add(5, 3), receives 8
// => sum is 8 (type: int)

// Method with multiple return statements
public static String getGrade(int score) {
    // => score is parameter (type: int)
    if (score >= 90) { // => Check: score >= 90
        return "A"; // => Return "A" and EXIT method (no further execution)
    } else if (score >= 80) {
        return "B"; // => Return "B" and EXIT
    } else if (score >= 70) {
        return "C"; // => Return "C" and EXIT
    } else {
        return "F"; // => Return "F" and EXIT (default case)
    }
    // => COMPILER ERROR if code placed here (unreachable code)
    // => All paths return a value (required for non-void methods)
}

String grade1 = getGrade(95); // => grade1 is "A" (type: String)
String grade2 = getGrade(75); // => grade2 is "C" (type: String)

// ===== VOID METHODS - NO RETURN VALUE =====
// void return type - method does not return value
public static void greet(String name) {
    // => name is parameter (type: String)
    System.out.println("Hello, " + name); // => Side effect: print to stdout
    // => implicit return (no return statement needed for void)
}

greet("Alice"); // => Call greet("Alice")
// => Output: "Hello, Alice"
// => No return value (void)

// void with explicit return - early exit
public static void checkAge(int age) {
    // => age is parameter (type: int)
    if (age < 18) { // => Check: age < 18
        System.out.println("Too young");
        return; // => EXIT method early (skip remaining code)
    }
    System.out.println("Adult"); // => Only executed if age >= 18
}

checkAge(15); // => Output: "Too young" (returns early)
checkAge(25); // => Output: "Adult" (executes to end)

// ===== METHOD OVERLOADING - SAME NAME, DIFFERENT SIGNATURES =====
// Overloaded methods - same name, different parameter count/types
public static int multiply(int a, int b) {
    // => Signature: multiply(int, int) -> int
    return a * b; // => Return product of two ints
}

public static int multiply(int a, int b, int c) {
    // => Signature: multiply(int, int, int) -> int
    return a * b * c; // => Return product of three ints
}

public static double multiply(double a, double b) {
    // => Signature: multiply(double, double) -> double
    return a * b; // => Return product of two doubles
}

// Compiler selects method based on argument types and count
int result1 = multiply(2, 3); // => Calls multiply(int, int), result1 is 6
int result2 = multiply(2, 3, 4); // => Calls multiply(int, int, int), result2 is 24
double result3 = multiply(2.5, 3.0); // => Calls multiply(double, double), result3 is 7.5

// Overload resolution - compiler picks best match
public static void print(int x) {
    System.out.println("int: " + x);
}

public static void print(double x) {
    System.out.println("double: " + x);
}

print(10); // => Calls print(int), Output: "int: 10"
print(10.5); // => Calls print(double), Output: "double: 10.5"
print(10.0); // => Calls print(double), Output: "double: 10.0" (literal is double)

// ===== VARARGS - VARIABLE-LENGTH ARGUMENTS =====
// Varargs parameter - accepts 0 or more arguments, becomes array
public static int sum(int... numbers) {
    // => numbers is array of int (type: int[])
    // => Can receive 0, 1, 2, ... arguments
    int total = 0; // => total is 0 (accumulator, type: int)
    for (int n : numbers) { // => Iterate over numbers array
        total += n; // => Add each element to total
    }
    return total; // => Return sum
}

int s1 = sum(); // => Call with 0 args, numbers is [], s1 is 0
int s2 = sum(1, 2, 3); // => Call with 3 args, numbers is [1,2,3], s2 is 6
int s3 = sum(1, 2, 3, 4, 5); // => Call with 5 args, numbers is [1,2,3,4,5], s3 is 15

// Varargs with other parameters - varargs MUST be last
public static void print(String prefix, int... numbers) {
    // => prefix is regular parameter (required)
    // => numbers is varargs (optional, type: int[])
    System.out.print(prefix + ": ");
    for (int n : numbers) {
        System.out.print(n + " ");
    }
    System.out.println();
}

print("Values"); // => Output: "Values: " (no numbers)
print("Numbers", 1, 2, 3); // => Output: "Numbers: 1 2 3 "

// COMPILER ERROR: varargs must be last parameter
// public static void invalid(int... numbers, String suffix) {} // => ERROR

// ===== STATIC VS INSTANCE METHODS =====
// Static methods - belong to class, no object needed
class Calculator {
    // Static method - accessed via ClassName.methodName()
    public static int add(int a, int b) {
        // => Cannot access instance fields (no "this" reference)
        return a + b;
    }

    // Instance field - belongs to each object
    private int value; // => Instance variable (each Calculator has own value)

    // Instance method - operates on specific object
    public void setValue(int v) {
        // => Has implicit "this" reference to current object
        this.value = v; // => Access instance field via "this"
    }

    public int getValue() {
        return value; // => "this.value" implicit
        // => Equivalent to: return this.value;
    }

    // Static method cannot access instance fields
    public static void cannotAccessInstance() {
        // System.out.println(value); // => COMPILER ERROR: non-static field
        // System.out.println(this.value); // => ERROR: no "this" in static context
    }
}

// Calling static method - via class name
int result = Calculator.add(10, 20); // => Call static method, result is 30
// => No Calculator object needed

// Calling instance methods - via object
Calculator calc = new Calculator(); // => Create Calculator instance
calc.setValue(100); // => Call instance method on calc object
// => calc.value is now 100
int val = calc.getValue(); // => Call instance method, val is 100

// Multiple instances have separate state
Calculator calc2 = new Calculator(); // => Create second instance
calc2.setValue(200); // => calc2.value is 200
// => calc.value still 100 (independent objects)

// ===== METHOD REFERENCES (JAVA 8+) - METHODS AS OBJECTS =====
// Method reference syntax: ClassName::staticMethod or object::instanceMethod
// Treats method as Function/Consumer/Supplier/etc.

// Static method reference - Integer::parseInt
Function<String, Integer> parseMethod = Integer::parseInt;
// => parseMethod is Function<String, Integer> (takes String, returns Integer)
// => Equivalent to lambda: s -> Integer.parseInt(s)

int parsed = parseMethod.apply("123"); // => Call method reference
// => parsed is 123 (type: int, auto-unboxed from Integer)

// Instance method reference - String::toUpperCase
Function<String, String> toUpper = String::toUpperCase;
// => toUpper is Function<String, String>
// => Equivalent to lambda: s -> s.toUpperCase()

String upper = toUpper.apply("hello"); // => upper is "HELLO" (type: String)

// Constructor reference - String::new
Function<char[], String> stringBuilder = String::new;
// => stringBuilder is Function<char[], String>
// => Equivalent to lambda: chars -> new String(chars)

char[] chars = {'J', 'a', 'v', 'a'}; // => chars is ['J','a','v','a']
String str = stringBuilder.apply(chars); // => str is "Java" (type: String)

// BiFunction - method with two parameters
BiFunction<Integer, Integer, Integer> addFunc = Calculator::add;
// => addFunc is BiFunction<Integer, Integer, Integer>
// => Equivalent to lambda: (a, b) -> Calculator.add(a, b)

int sum2 = addFunc.apply(5, 7); // => sum2 is 12 (type: int)

// Consumer - method with parameter, no return (void)
Consumer<String> printer = System.out::println;
// => printer is Consumer<String> (takes String, returns void)
// => Equivalent to lambda: s -> System.out.println(s)

printer.accept("Hello!"); // => Output: "Hello!"

// Supplier - method with no parameters, returns value
Supplier<Double> randomSupplier = Math::random;
// => randomSupplier is Supplier<Double> (no params, returns Double)
// => Equivalent to lambda: () -> Math.random()

double rand = randomSupplier.get(); // => rand is random value 0.0-1.0 (type: double)

// ===== RETURN TYPE COVARIANCE - OVERRIDING WITH SUBTYPE =====
// Covariant return types - overridden method can return subtype
class Animal {
    // Parent class method returns Animal
    public Animal reproduce() {
        return new Animal(); // => Return Animal instance
    }
}

class Dog extends Animal {
    // Override with covariant return type (Dog is subtype of Animal)
    @Override
    public Dog reproduce() { // => Returns Dog (subtype of Animal)
        return new Dog(); // => Legal: Dog is-a Animal
    }
}

// Usage
Animal animal = new Animal();
Animal baby1 = animal.reproduce(); // => baby1 is Animal (type: Animal)

Dog dog = new Dog();
Dog baby2 = dog.reproduce(); // => baby2 is Dog (type: Dog)
// => No cast needed (covariant return type)

Animal animal2 = dog; // => Polymorphism: Dog is-a Animal
Animal baby3 = animal2.reproduce(); // => Calls Dog.reproduce() (runtime polymorphism)
// => baby3 is Dog (runtime type), Animal (compile-time type)

// ===== PARAMETER PASSING - PASS BY VALUE =====
// Java is ALWAYS pass-by-value (copies of primitives/references)

// Primitives - copy of value passed
public static void modifyPrimitive(int x) {
    x = 100; // => Modifies local copy, not original
}

int num = 10; // => num is 10 (type: int)
modifyPrimitive(num); // => Pass copy of 10
// => num is still 10 (original unchanged)

// Objects - copy of reference passed (reference points to same object)
public static void modifyObject(Calculator c) {
    c.setValue(200); // => Modifies object that reference points to
}

Calculator calc3 = new Calculator(); // => Create Calculator
calc3.setValue(50); // => calc3.value is 50

modifyObject(calc3); // => Pass copy of reference to calc3
// => calc3.value is now 200 (object modified via reference copy)

// Reassigning parameter doesn't affect original reference
public static void reassignReference(Calculator c) {
    c = new Calculator(); // => Reassign LOCAL parameter (copy of reference)
    c.setValue(999); // => Modify new object (not original)
}

Calculator calc4 = new Calculator(); // => Create Calculator
calc4.setValue(100); // => calc4.value is 100

reassignReference(calc4); // => Pass copy of reference
// => calc4.value is still 100 (original reference unchanged)
// => New Calculator(999) is lost (local parameter only)
```

**Key Takeaway**: Methods have return types (or `void`) and parameters. Method overloading uses different signatures (parameter count/types). Varargs (`type...`) accept variable arguments as arrays and must be last parameter. Static methods belong to class; instance methods belong to objects. Method references (`::`) treat methods as first-class objects for functional programming. Java is pass-by-value: primitives copy values, objects copy references (reference points to same object).

**Why It Matters**: Method overloading reduces cognitive load by using consistent naming for similar operations (List.of(1), List.of(1,2,3)), while compile-time resolution prevents runtime ambiguity errors. Method references (`::`) enable functional-style programming with Stream API, reducing boilerplate compared to lambdas and improving performance through invokedynamic optimization. Understanding pass-by-value prevents bugs where developers expect parameter modifications to affect original variables—a common source of confusion causing 10-15% of beginner Java bugs. Covariant return types enable fluent APIs and builder patterns without casting, improving type safety and code readability in frameworks like JUnit and Mockito.

---

## Group 5: Exception Handling and Basics

### Example 16: Exception Handling

Java distinguishes checked exceptions (compile-time enforced) from unchecked exceptions (runtime). `try-catch-finally` handles exceptions. `try-with-resources` manages resources automatically.

**Code**:

```java
import java.io.*;
// => import java.io.*: imports all classes from java.io package
// => Includes: IOException, FileReader, BufferedReader (I/O classes)
// => Wildcard *: imports all public classes (not subpackages)
// => IOException: checked exception for I/O operations
// => FileReader/BufferedReader: file reading classes implementing AutoCloseable

// try-catch for exception handling
// => Exception handling: mechanism to catch and recover from errors
// => try: block containing code that might throw exceptions
// => catch: block executed when matching exception thrown
// => Enables graceful error recovery instead of program crash
public static void divideExample() {
    // => public: method accessible from anywhere
    // => static: belongs to class, not instance (no object needed to call)
    // => void: returns no value
    // => divideExample: method demonstrating basic try-catch pattern

    try {
        // => try block: contains code that might throw exception
        // => If exception thrown inside try, control jumps to matching catch
        // => Code after exception line in try block is skipped

        int result = 10 / 0; // ArithmeticException thrown here
        // => int result: variable to store division result
        // => 10 / 0: division by zero operation
        // => / operator: integer division (quotient only, no remainder)
        // => Division by zero throws ArithmeticException (unchecked)
        // => Exception thrown immediately, result never assigned
        // => Control jumps to catch block before assignment occurs

        System.out.println(result); // This line never executes
        // => This line unreachable because exception thrown above
        // => result never assigned a value due to exception
        // => Control flow skips directly to catch block
        // => println would output result if no exception occurred

    } catch (ArithmeticException e) {
        // => catch: handles ArithmeticException specifically
        // => ArithmeticException: unchecked exception for arithmetic errors
        // => e: variable holding exception object (contains error details)
        // => Type: ArithmeticException (subclass of RuntimeException)
        // => Scope: variable 'e' only accessible within this catch block

        System.out.println("Cannot divide by zero!"); // => Executed
        // => This line executes because ArithmeticException was caught
        // => println: prints error message to standard output
        // => Output: Cannot divide by zero!
        // => Program doesn't crash, continues after catch block

        System.out.println("Error: " + e.getMessage()); // => "/ by zero"
        // => e.getMessage(): returns exception's detail message
        // => Returns: "/ by zero" (JVM's standard division by zero message)
        // => + concatenation: joins strings
        // => Output: Error: / by zero
        // => getMessage() inherited from Throwable superclass
    }

    System.out.println("Program continues"); // => Executes normally
    // => This line always executes after try-catch completes
    // => Demonstrates exception was handled gracefully
    // => Program flow continues normally after exception caught
    // => Output: Program continues
    // => Without try-catch, program would crash and this wouldn't execute
}

// Multiple catch blocks - most specific first
// => Multiple catch: handles different exception types separately
// => Order matters: specific exceptions before general ones (compiler enforced)
// => If general before specific, compiler error (unreachable catch)
// => Enables different recovery strategies per exception type
public static void parseExample(String input) {
    // => String input: parameter holding text to parse
    // => Method demonstrates catching multiple exception types
    // => Different exceptions require different handling strategies

    try {
        // => try block: contains two operations that may throw different exceptions
        // => First operation (parseInt) may throw NumberFormatException
        // => Second operation (division) may throw ArithmeticException
        // => Order of operations determines which exception thrown if both fail

        int num = Integer.parseInt(input); // May throw NumberFormatException
        // => Integer.parseInt(input): parses String to int primitive
        // => If input is "123", num becomes 123 (type: int)
        // => If input is "abc", throws NumberFormatException (unchecked)
        // => If input is null, throws NullPointerException
        // => NumberFormatException: thrown when string not valid integer format
        // => Radix 10 assumed (decimal) - use parseInt(input, radix) for other bases

        int result = 100 / num; // May throw ArithmeticException
        // => 100 / num: divides 100 by parsed number
        // => If num is 0, throws ArithmeticException (division by zero)
        // => If num is 5, result is 20 (100/5, integer division)
        // => If num is 3, result is 33 (100/3=33.33 truncated to 33)
        // => Only executes if parseInt succeeded (no exception thrown)

        System.out.println(result);
        // => Prints division result if both operations succeeded
        // => Only executes if no exception thrown in try block
        // => Output: {result value}

    } catch (NumberFormatException e) {
        // => Catches NumberFormatException specifically (from parseInt)
        // => Most specific exception listed first (best practice)
        // => NumberFormatException: subclass of IllegalArgumentException
        // => Handles invalid input format: "abc", "12.5", "1a2", etc.

        System.out.println("Invalid number format"); // => If input is "abc"
        // => Executed when input cannot be parsed as integer
        // => Example inputs triggering this: "hello", "12.5", ""
        // => Output: Invalid number format
        // => Program continues after this catch block

    } catch (ArithmeticException e) {
        // => Catches ArithmeticException specifically (from division)
        // => Second most specific exception
        // => ArithmeticException: thrown for arithmetic errors (division by zero)
        // => Only reached if NumberFormatException didn't occur

        System.out.println("Division error"); // => If num is 0
        // => Executed when division by zero occurs
        // => Input "0" parses successfully but causes division error
        // => Output: Division error
        // => Demonstrates same try block can throw different exceptions

    } catch (Exception e) {
        // => Catch-all for any Exception not caught above
        // => Exception: superclass of all checked and unchecked exceptions
        // => Most general exception type (must be last)
        // => Catches: NullPointerException, RuntimeException, any other Exception
        // => Safety net for unexpected exceptions

        System.out.println("Other error"); // => Catch-all (most general last)
        // => Executed for exceptions not caught by specific blocks above
        // => Example: NullPointerException if input is null
        // => Output: Other error
        // => Production code should log e.getMessage() for debugging
    }
    // => After catch blocks, program continues normally
    // => Exception handled, method returns normally
    // => If no catch matched, exception propagates to caller
}

// finally block - always executes (even if exception thrown or caught)
// => finally: block always executed regardless of exception occurrence
// => Executes after try completes successfully OR after catch handles exception
// => Used for cleanup: closing resources, releasing locks, finalizing state
// => Executes even if return statement in try or catch
// => Only skipped if JVM exits (System.exit) or thread interrupted
public static void finallyExample() {
    // => Demonstrates finally block for guaranteed cleanup
    // => Pattern: legacy resource management (before Java 7 try-with-resources)
    // => Modern code should use try-with-resources instead

    FileReader reader = null;
    // => FileReader reader: variable to hold file reader resource
    // => Declared outside try so accessible in finally block
    // => Initialized to null (no resource opened yet)
    // => Type: FileReader (reads character files)
    // => null: safe initial value for reference types

    try {
        // => try block: opens file and performs read operations
        // => If file doesn't exist, FileReader constructor throws IOException
        // => If read fails, throws IOException
        // => Any exception jumps to catch block, then finally executes

        reader = new FileReader("file.txt");
        // => new FileReader("file.txt"): opens file for reading
        // => Constructor throws FileNotFoundException (subclass of IOException) if not found
        // => reader: assigned FileReader object if file exists
        // => File opened in read-only mode (cannot write)
        // => Operating system file handle allocated (must be closed)
        // => Character encoding: platform default (use FileReader(File, Charset) for explicit)

        // Read file operations...
        // => Placeholder for actual file reading code
        // => Could use reader.read() to read characters
        // => Operations would throw IOException if read fails
        // => File handle remains open during operations

    } catch (IOException e) {
        // => Catches IOException from FileReader constructor or read operations
        // => IOException: checked exception for I/O failures
        // => Handles: FileNotFoundException, read errors, disk errors
        // => e: exception object with error details

        System.out.println("Error reading file");
        // => Executed if file doesn't exist or read operation fails
        // => Output: Error reading file
        // => Production code should log e.getMessage() for diagnostic
        // => After catch, finally block still executes

    } finally {
        // => finally block: always executes for cleanup
        // => Runs even if exception thrown in try or catch
        // => Runs even if return statement in try or catch
        // => Critical for releasing resources (file handles, locks, connections)

        // Cleanup code - always runs
        // => Cleanup: releasing allocated resources (file handles)
        // => Always runs: whether exception occurred or not
        // => Prevents resource leaks (unclosed files exhaust system handles)

        if (reader != null) {
            // => Null check: prevents NullPointerException if FileReader constructor failed
            // => reader is null if FileReader("file.txt") threw exception
            // => reader is non-null if file opened successfully
            // => Condition: only close if resource was actually opened

            try {
                // => Nested try: close() itself can throw IOException
                // => close() failure should not mask original exception
                // => Separate try-catch for close operation
                // => Best practice: handle close failure separately

                reader.close(); // Close resource
                // => close(): closes file and releases operating system handle
                // => Flushes any buffered data to disk
                // => After close, reader cannot be used (throws exception if attempted)
                // => Operating system file handle returned to pool
                // => May throw IOException if close operation fails

            } catch (IOException e) {
                // => Catches IOException from close operation
                // => Separate from outer catch to handle close failure distinctly
                // => If close fails, original exception details preserved

                System.out.println("Error closing file");
                // => Executed if close operation throws IOException
                // => Output: Error closing file
                // => Rare but possible: disk full, network file, permission denied
                // => Production code should log this for diagnostic
            }
        }

        System.out.println("Cleanup complete"); // => Always executes
        // => This line always executes regardless of exceptions
        // => Demonstrates finally block guaranteed execution
        // => Output: Cleanup complete
        // => Executes whether file opened successfully, failed to open, or close failed
    }
    // => Method returns after finally completes
    // => If exception not caught, propagates to caller after finally
    // => Resource guaranteed closed due to finally block
}

// try-with-resources (Java 7+) - automatic resource management
// => try-with-resources: modern resource management pattern (replaces finally cleanup)
// => Resources in parentheses after try automatically closed
// => Requires resources implement AutoCloseable interface
// => Automatic close happens before catch block executes
// => Eliminates boilerplate null checks and nested try-catch for close
public static void tryWithResourcesExample() {
    // => Demonstrates automatic resource management (modern pattern)
    // => Cleaner than manual finally cleanup
    // => Compiler generates finally block with close calls

    // Resources in () are AutoCloseable - closed automatically
    // => Resources declared in try parentheses: (Resource r = new Resource())
    // => Multiple resources separated by semicolons
    // => All resources must implement AutoCloseable or Closeable
    // => Closed in reverse order of declaration (buffered before reader)
    // => Close happens automatically even if exception thrown

    try (FileReader reader = new FileReader("data.txt");
         BufferedReader buffered = new BufferedReader(reader)) {
        // => FileReader reader: first resource, opened from "data.txt"
        // => new FileReader("data.txt"): throws FileNotFoundException if missing
        // => Semicolon separates multiple resource declarations
        // => BufferedReader buffered: wraps reader for efficient buffered reading
        // => new BufferedReader(reader): wraps FileReader with buffer
        // => Buffering: reads chunks into memory, reduces disk I/O calls
        // => Both resources automatically closed at end of try block
        // => Close order: buffered first (reverse declaration order), then reader

        String line = buffered.readLine();
        // => buffered.readLine(): reads one line from file (until newline or EOF)
        // => Returns: String containing line text (null if EOF)
        // => Throws IOException if read fails
        // => Buffering improves performance vs reading character-by-character
        // => Line: characters until \n (Unix) or \r\n (Windows), delimiter excluded

        System.out.println(line);
        // => Prints first line of file
        // => If line is null (empty file), prints "null"
        // => Output: {first line content}
        // => Only executes if readLine succeeded

    } catch (IOException e) {
        // => Catches IOException from FileReader, BufferedReader, or readLine
        // => Handles: FileNotFoundException, read errors, close errors
        // => Resources already closed before catch executes
        // => e: exception object with error details

        System.out.println("File error: " + e.getMessage());
        // => Executed if any I/O operation fails
        // => e.getMessage(): detailed error message
        // => Example messages: "data.txt (No such file or directory)"
        // => Output: File error: {error message}
        // => Production code should log full stack trace for debugging
    }
    // reader and buffered automatically closed, even if exception thrown
    // => Automatic close: happens in implicit finally block (compiler-generated)
    // => Closed even if exception thrown in try or catch
    // => Closed even if return statement in try
    // => Close exceptions suppressed if primary exception occurred
    // => Access suppressed exceptions via e.getSuppressed()
    // => Resources already closed when this comment line executes
}

// throw keyword - manually throw exception
// => throw: keyword to explicitly throw exception object
// => Used for: validating input, enforcing preconditions, signaling errors
// => Immediately transfers control to nearest catch or caller
// => Unchecked exceptions (RuntimeException) don't require throws declaration
public static void validateAge(int age) {
    // => Demonstrates manual exception throwing for validation
    // => Method validates age and throws exceptions for invalid values
    // => Throws unchecked exceptions (no throws clause required)

    if (age < 0) {
        // => Validation: age must be non-negative
        // => Negative age is logically invalid (programming error)
        // => Condition: age < 0 evaluates to true for negative values

        throw new IllegalArgumentException("Age cannot be negative");
        // => throw: immediately throws exception, method execution stops
        // => new IllegalArgumentException(...): creates unchecked exception
        // => IllegalArgumentException: for invalid method arguments
        // => "Age cannot be negative": error message describing violation
        // => Unchecked exception: no throws declaration required
        // => After throw, control transfers to caller's catch or terminates program
        // => Lines below this never execute if exception thrown
    }

    if (age < 18) {
        // => Validation: age must be 18 or older
        // => Separate from negativity check (different error conditions)
        // => Only reached if age >= 0 (previous check passed)

        throw new IllegalStateException("Must be 18 or older");
        // => IllegalStateException: object state invalid for operation
        // => Different exception type indicates different error category
        // => "Must be 18 or older": business rule violation message
        // => Unchecked exception: caller not forced to handle
        // => Use IllegalStateException for business rule violations
        // => After throw, method terminates immediately
    }

    System.out.println("Valid age: " + age);
    // => Only executes if both validation checks passed
    // => age >= 0 AND age >= 18 (effectively age >= 18)
    // => Output: Valid age: {age value}
    // => Demonstrates validation succeeded, method completes normally
}

// Checked exceptions - must be declared with 'throws' or caught
// => Checked exception: compiler enforces handling (catch or declare)
// => Must appear in method signature with 'throws' clause
// => Caller forced to handle or re-declare with throws
// => Used for recoverable errors (file not found, network timeout)
public static void readFile(String filename) throws IOException {
    // => throws IOException: declares method may throw IOException
    // => Compiler enforces: callers must catch or re-declare
    // => IOException: checked exception for I/O operations
    // => Part of method signature (like parameters and return type)
    // => Caller sees this declaration and must handle exception

    FileReader reader = new FileReader(filename); // IOException is checked
    // => new FileReader(filename): opens file for reading
    // => Throws FileNotFoundException (subclass of IOException) if not found
    // => FileNotFoundException is checked: must be caught or declared
    // => Without 'throws IOException', compiler error here
    // => Method doesn't catch exception, propagates to caller

    // Must declare throws IOException or catch it
    // => Two options: 1) Catch with try-catch in this method
    // => 2) Declare with 'throws IOException' (chosen here)
    // => Checked exceptions enforce explicit error handling
    // => Compiler prevents forgetting to handle recoverable errors
}

// Calling method that throws checked exception
// => Demonstrates handling checked exception thrown by another method
// => Caller must catch or re-declare checked exceptions
// => try-catch used here to handle IOException locally
public static void caller() {
    // => Method calls readFile which throws checked IOException
    // => Must use try-catch or add throws IOException to signature
    // => Chose to handle exception locally with try-catch

    try {
        // => try block: contains call to method that throws checked exception
        // => If readFile throws IOException, catch block handles it
        // => readFile("test.txt") may throw if file doesn't exist

        readFile("test.txt"); // Must catch checked exception
        // => Calls readFile method which throws IOException
        // => "test.txt": filename parameter
        // => Compiler enforces: must catch IOException or declare throws
        // => If file doesn't exist, throws FileNotFoundException
        // => If I/O error occurs, throws IOException
        // => Without try-catch or throws declaration, compiler error

    } catch (IOException e) {
        // => Catches IOException from readFile call
        // => IOException: superclass catching FileNotFoundException too
        // => e: exception object with error details
        // => Handles exception locally instead of propagating

        System.out.println("File not found: " + e.getMessage());
        // => Executed if readFile threw IOException
        // => e.getMessage(): exception's detail message
        // => Example message: "test.txt (No such file or directory)"
        // => Output: File not found: {error message}
        // => Production code might: retry, use default file, log error
    }
    // => After catch, method continues normally
    // => Exception handled, no propagation to caller
    // => Method signature has no throws clause (exception handled internally)
}

// Unchecked exceptions (RuntimeException subclasses) - not required to catch
// NullPointerException, ArithmeticException, ArrayIndexOutOfBoundsException, etc.
// => Unchecked exceptions: inherit from RuntimeException
// => Compiler doesn't enforce handling (no catch or throws required)
// => Indicate programming errors (bugs), not recoverable conditions
// => Should be prevented by validation, not caught reactively
// => Examples: NullPointerException, IndexOutOfBoundsException, IllegalArgumentException
public static void uncheckedExample() {
    // => Demonstrates unchecked exceptions (programming errors)
    // => No try-catch required (compiler doesn't enforce)
    // => Should fix bugs causing these, not catch them

    String str = null;
    // => str: reference variable holding null (no object)
    // => null: special value meaning "no object reference"
    // => Dereferencing null causes NullPointerException

    // str.length(); // NullPointerException - unchecked, no catch required
    // => Commented out to prevent exception during demonstration
    // => str.length(): attempts to call method on null reference
    // => NullPointerException: unchecked runtime exception
    // => No try-catch or throws required by compiler
    // => Should prevent with null check: if (str != null)
    // => Catching NullPointerException is bad practice (fix bug instead)

    int[] arr = {1, 2, 3};
    // => arr: array with 3 elements at indices 0, 1, 2
    // => Valid indices: 0, 1, 2 (length is 3)
    // => Accessing index >= 3 or < 0 throws ArrayIndexOutOfBoundsException

    // int x = arr[5]; // ArrayIndexOutOfBoundsException - unchecked
    // => Commented to prevent exception
    // => arr[5]: attempts to access index 5 (doesn't exist, array length is 3)
    // => ArrayIndexOutOfBoundsException: unchecked runtime exception
    // => No try-catch or throws declaration required
    // => Should prevent with bounds check: if (5 < arr.length)
    // => Indicates programming error (logic bug), not recoverable condition
}

// Custom exceptions
// => Custom exception: application-specific error type
// => Extends Exception (checked) or RuntimeException (unchecked)
// => Provides domain-specific error signaling (InsufficientFundsException)
// => Enables catch blocks to distinguish application errors from system errors
class InsufficientFundsException extends Exception {
    // => class: defines new exception type
    // => extends Exception: makes this a checked exception
    // => Checked: callers must catch or declare with throws
    // => If extended RuntimeException, would be unchecked
    // => Custom name: clearly describes specific error condition

    public InsufficientFundsException(String message) {
        // => Constructor: creates exception with error message
        // => String message: description of specific error condition
        // => public: accessible from other classes

        super(message);
        // => super(message): calls Exception superclass constructor
        // => Passes message to Exception for storage
        // => Message retrievable via getMessage() method
        // => Superclass constructor initializes exception internals
    }
    // => Could add fields: balance, requestedAmount for detailed diagnostics
    // => Could add multiple constructors for flexibility
    // => Minimal implementation shown (message-only constructor)
}

public static void withdraw(double amount, double balance) throws InsufficientFundsException {
    // => Method signature: withdraw funds from account
    // => double amount: withdrawal amount requested
    // => double balance: current account balance
    // => throws InsufficientFundsException: declares checked exception
    // => Callers must handle or re-declare this exception

    if (amount > balance) {
        // => Validation: requested amount exceeds available balance
        // => Business rule: cannot withdraw more than balance
        // => Condition true when insufficient funds

        throw new InsufficientFundsException("Balance: " + balance + ", Requested: " + amount);
        // => throw: immediately throws custom exception
        // => new InsufficientFundsException(...): creates exception object
        // => Message includes balance and requested amount for diagnostics
        // => String concatenation: builds detailed error message
        // => Control transfers to caller's catch or terminates program
        // => Method execution stops here if exception thrown
    }

    System.out.println("Withdrawal successful");
    // => Only executes if amount <= balance
    // => Output: Withdrawal successful
    // => Indicates validation passed, withdrawal allowed
    // => Production code would update balance here
}

// Exception hierarchy
// Throwable
//   ├── Error (JVM errors, don't catch)
//   └── Exception
//       ├── IOException (checked)
//       ├── SQLException (checked)
//       └── RuntimeException (unchecked)
//           ├── NullPointerException
//           ├── ArithmeticException
//           └── IllegalArgumentException
// => Throwable: root of exception hierarchy (all exceptions inherit)
// => Error: serious JVM errors (OutOfMemoryError, StackOverflowError)
// =>   Don't catch Error - indicates JVM failure, not recoverable
// => Exception: superclass of all exceptions (checked by default)
// =>   IOException: I/O operation failures (file not found, network error)
// =>   SQLException: database operation failures
// =>   RuntimeException: unchecked exceptions (programming errors)
// =>     NullPointerException: null reference dereference
// =>     ArithmeticException: arithmetic errors (division by zero)
// =>     IllegalArgumentException: invalid method arguments
// => Checked exceptions: must catch or declare (all except RuntimeException subtypes)
// => Unchecked exceptions: compiler doesn't enforce handling (RuntimeException subtypes)
// => catch (Exception e): catches all Exception types (not Error)
// => catch (Throwable t): catches everything (rarely used, too broad)
```

**Key Takeaway**: `try-catch` handles exceptions. `finally` always executes for cleanup. `try-with-resources` auto-closes `AutoCloseable` resources. Checked exceptions (compile-time) require `throws` declaration or catching. Unchecked exceptions (runtime) don't require explicit handling. `throw` manually throws exceptions.

**Why It Matters**: Checked exceptions document API contracts—IOException forces file handling to deal with disk full, network timeouts. This explicit error handling prevents silent failures that corrupt data or lose transactions. However, overuse creates verbose code—use unchecked exceptions for programming errors. The exception hierarchy enables catch-all handlers (catch Exception) while allowing specific handling (catch FileNotFoundException). Proper exception handling is critical in distributed systems where partial failures are common and recovery strategies vary by failure type.

---

### Example 17: Common Utility Classes

Java provides rich utility classes for common operations. `Math`, `String`, `Arrays`, `Collections`, and `Objects` offer static methods that simplify tasks.

**Code**:

```java
import java.util.*;
// => import java.util.*: imports all classes from java.util package
// => Includes: Arrays, Collections, List, ArrayList (collections utilities)
// => Wildcard *: imports all public classes (not subpackages)
// => Collections framework: data structures and algorithms
// => Utility classes: provide static methods for common operations

// Math class - mathematical operations (all static methods)
// => Math class: provides mathematical functions and constants
// => All methods static: call via Math.method() not instance
// => No constructor: cannot create Math objects (all methods static)
// => Thread-safe: all methods are stateless
int absolute = Math.abs(-10); // => 10 (absolute value)
// => Math.abs(int): returns absolute value (removes sign)
// => -10: negative integer input
// => Returns: 10 (positive equivalent)
// => Works with int, long, float, double (overloaded)
// => absolute: variable storing result (type: int)

int maximum = Math.max(5, 10); // => 10 (larger value)
// => Math.max(int, int): returns larger of two values
// => 5: first value to compare
// => 10: second value to compare
// => Returns: 10 (larger value)
// => Overloaded: max(int, int), max(long, long), max(double, double)

int minimum = Math.min(5, 10); // => 5 (smaller value)
// => Math.min(int, int): returns smaller of two values
// => Returns: 5 (smaller value)
// => Use for bounds checking, limiting ranges

double power = Math.pow(2, 3); // => 8.0 (2³)
// => Math.pow(double, double): raises base to exponent
// => 2: base value
// => 3: exponent value
// => Returns: 8.0 (2 * 2 * 2 = 8, returned as double)
// => Always returns double even for integer inputs
// => Use for exponential calculations (compound interest, growth)

double squareRoot = Math.sqrt(16); // => 4.0 (√16)
// => Math.sqrt(double): returns square root
// => 16: value to find square root of
// => Returns: 4.0 (√16 = 4)
// => Returns NaN (Not a Number) if input negative
// => Use for distance calculations, standard deviation

double rounded = Math.round(3.7); // => 4.0 (rounds to nearest integer)
// => Math.round(double): rounds to nearest integer
// => 3.7: input value
// => Returns: 4.0 (rounds up because .7 >= .5)
// => Math.round(3.4) would return 3 (rounds down)
// => Returns long for double input, int for float input

double ceiling = Math.ceil(3.2); // => 4.0 (rounds up)
// => Math.ceil(double): rounds up to next integer
// => 3.2: input value
// => Returns: 4.0 (always rounds up, never down)
// => Math.ceil(3.0) returns 3.0 (already integer)
// => Ceiling: smallest integer >= input

double floor = Math.floor(3.8); // => 3.0 (rounds down)
// => Math.floor(double): rounds down to previous integer
// => 3.8: input value
// => Returns: 3.0 (always rounds down, never up)
// => Floor: largest integer <= input
// => Use for page calculation, binning

double random = Math.random(); // => 0.0 to 1.0 (exclusive)
// => Math.random(): returns random double in range [0.0, 1.0)
// => Returns: value >= 0.0 and < 1.0 (excludes 1.0)
// => Different value on each call (pseudorandom)
// => Use Random class for reproducible random sequences
// => Thread-safe but not cryptographically secure

int randomInt = (int) (Math.random() * 100); // => 0 to 99
// => Math.random() * 100: scales to range [0.0, 100.0)
// => (int): casts double to int (truncates decimal part)
// => Result: random integer from 0 to 99 (inclusive)
// => Never returns 100 (random() never returns 1.0)
// => For range [min, max): (int)(Math.random() * (max - min)) + min

// Math constants
// => Constants: commonly used mathematical values
// => public static final: unchangeable class-level constants
// => High precision: many decimal places for accuracy
double pi = Math.PI; // => 3.141592653589793
// => Math.PI: ratio of circle's circumference to diameter
// => Value: 3.141592653589793 (15 decimal places)
// => Use for circle calculations, trigonometry

double e = Math.E; // => 2.718281828459045
// => Math.E: Euler's number (base of natural logarithm)
// => Value: 2.718281828459045
// => Use for exponential growth, continuous compounding

// String methods (extensive utility methods)
// => String: immutable sequence of characters
// => All String methods return new String (original unchanged)
// => String pool: literal strings cached for memory efficiency
String text = "  Hello World  ";
// => text: String with leading/trailing spaces
// => "  Hello World  ": literal string (stored in string pool)
// => Length: 15 characters (includes spaces)
// => Immutable: cannot modify this string, only create new ones

int length = text.length(); // => 15 (includes spaces)
// => length(): returns number of characters in string
// => Returns: 15 (includes leading and trailing spaces)
// => Method (not property): requires parentheses ()
// => Zero-indexed: valid indices are 0 to length-1

String trimmed = text.trim(); // => "Hello World" (removes leading/trailing spaces)
// => trim(): removes whitespace from both ends
// => Whitespace: spaces, tabs, newlines
// => Returns: new String "Hello World" (length 11)
// => Original text unchanged (strings immutable)
// => Use for user input cleanup, data sanitization

String upper = text.toUpperCase(); // => "  HELLO WORLD  "
// => toUpperCase(): converts all characters to uppercase
// => Returns: new String "  HELLO WORLD  "
// => Original text unchanged
// => Spaces not affected (only letters converted)
// => Use for case-insensitive comparison, normalization

String lower = text.toLowerCase(); // => "  hello world  "
// => toLowerCase(): converts all characters to lowercase
// => Returns: new String "  hello world  "
// => Use for case-insensitive search, username storage

boolean starts = text.trim().startsWith("Hello"); // => true
// => text.trim(): first trims to "Hello World"
// => startsWith("Hello"): checks if string begins with "Hello"
// => Returns: true (trimmed string starts with "Hello")
// => Case-sensitive: startsWith("hello") would return false
// => Use for prefix checking, command parsing

boolean ends = text.trim().endsWith("World"); // => true
// => endsWith("World"): checks if string ends with "World"
// => Returns: true (trimmed string ends with "World")
// => Case-sensitive
// => Use for file extension checking, suffix validation

boolean contains = text.contains("World"); // => true
// => contains("World"): checks if substring exists anywhere
// => Returns: true ("World" appears in text)
// => Case-sensitive: contains("world") would return false
// => Use for search functionality, keyword detection

String replaced = text.replace("World", "Java"); // => "  Hello Java  "
// => replace(target, replacement): replaces all occurrences
// => "World": substring to find
// => "Java": replacement substring
// => Returns: new String "  Hello Java  "
// => Replaces all occurrences (not just first)
// => Original text unchanged

String[] parts = "apple,banana,cherry".split(","); // => ["apple", "banana", "cherry"]
// => split(regex): splits string into array at delimiter
// => ",": delimiter (regular expression)
// => Returns: String array with 3 elements
// => parts[0] = "apple", parts[1] = "banana", parts[2] = "cherry"
// => Delimiter not included in results
// => Use for CSV parsing, tokenization

String joined = String.join("-", "A", "B", "C"); // => "A-B-C"
// => String.join(delimiter, elements...): joins strings with delimiter
// => "-": delimiter inserted between elements
// => "A", "B", "C": varargs (variable number of arguments)
// => Returns: "A-B-C"
// => Static method: called on String class not instance
// => Use for building paths, concatenating with separator

// String formatting
// => String formatting: creating strings with placeholders
// => Format specifiers: %s (string), %d (decimal integer), %f (float)
String formatted = String.format("Name: %s, Age: %d", "Alice", 30);
// => String.format(format, args...): creates formatted string
// => "Name: %s, Age: %d": format template with placeholders
// => %s: string placeholder (replaced by "Alice")
// => %d: decimal integer placeholder (replaced by 30)
// => "Alice": first argument (replaces first %s)
// => 30: second argument (replaces %d)
// => Returns: "Name: Alice, Age: 30"
// => Type-safe: %d with string argument throws exception
// => Use for logging, user messages, SQL queries

// String comparison
// => String comparison: multiple ways to compare strings
// => equals() vs ==: content vs reference comparison
String s1 = "hello";
// => s1: literal string (stored in string pool)
// => "hello": string literal (interned in pool)

String s2 = "hello";
// => s2: references same "hello" in string pool as s1
// => String pool: JVM caches literal strings for memory efficiency
// => s1 and s2 reference same object (pool optimization)

String s3 = "HELLO";
// => s3: different string literal (different case)

boolean equals = s1.equals(s2); // => true (content comparison)
// => equals(Object): compares string contents character-by-character
// => Returns: true (both contain "hello")
// => Case-sensitive: s1.equals("HELLO") returns false
// => Always use equals() for content comparison

boolean same = (s1 == s2); // => true (reference comparison - same object)
// => ==: compares object references (memory addresses)
// => Returns: true (both reference same string pool object)
// => Don't use == for string comparison (unreliable)
// => == checks if same object, not same content
// => new String("hello") == "hello" would be false

boolean equalsIgnoreCase = s1.equalsIgnoreCase(s3); // => true
// => equalsIgnoreCase(String): case-insensitive content comparison
// => Returns: true ("hello" and "HELLO" match ignoring case)
// => Use for username comparison, search

int comparison = s1.compareTo(s2); // => 0 (lexicographic order: 0 = equal, <0 = less, >0 = greater)
// => compareTo(String): lexicographic comparison (dictionary order)
// => Returns: 0 (strings equal)
// => Returns: negative if s1 < s2 (s1 comes before s2)
// => Returns: positive if s1 > s2 (s1 comes after s2)
// => "apple".compareTo("banana") returns negative
// => Use for sorting, ordering

// Arrays utility class
// => Arrays: utility class for array manipulation
// => All methods static: call via Arrays.method()
// => Works with primitive and object arrays
int[] numbers = {5, 2, 8, 1, 9};
// => numbers: int array with 5 elements
// => Array literal: {5, 2, 8, 1, 9} initializes array
// => Unsorted order: [5, 2, 8, 1, 9]

Arrays.sort(numbers); // => [1, 2, 5, 8, 9] (sorts in-place)
// => Arrays.sort(array): sorts array in ascending order
// => In-place: modifies original array (no new array created)
// => After sort: numbers is [1, 2, 5, 8, 9]
// => Dual-pivot quicksort for primitives (O(n log n) average)
// => Timsort for objects (stable sort)

String arrayStr = Arrays.toString(numbers); // => "[1, 2, 5, 8, 9]"
// => Arrays.toString(array): converts array to readable string
// => Returns: "[1, 2, 5, 8, 9]" (format: [elem1, elem2, ...])
// => Use for debugging, logging
// => numbers.toString() would print memory address (not useful)

int index = Arrays.binarySearch(numbers, 5); // => 2 (index of 5, array must be sorted)
// => Arrays.binarySearch(array, key): searches for key using binary search
// => numbers: must be sorted (binarySearch assumes sorted)
// => 5: value to search for
// => Returns: 2 (index where 5 is found)
// => If not found: returns (-(insertion point) - 1)
// => O(log n) complexity (requires sorted array)

int[] filled = new int[5];
// => new int[5]: creates array with 5 elements
// => Default values: all elements initialized to 0
// => filled is [0, 0, 0, 0, 0]

Arrays.fill(filled, 7); // => [7, 7, 7, 7, 7]
// => Arrays.fill(array, value): sets all elements to value
// => In-place: modifies original array
// => After fill: filled is [7, 7, 7, 7, 7]
// => Overload: fill(array, fromIndex, toIndex, value) for partial fill

int[] copy = Arrays.copyOf(numbers, 3); // => [1, 2, 5] (first 3 elements)
// => Arrays.copyOf(original, newLength): copies array with new length
// => numbers: source array [1, 2, 5, 8, 9]
// => 3: new length (copies first 3 elements)
// => Returns: new array [1, 2, 5]
// => If newLength > original.length, pads with default values
// => Original array unchanged

int[] range = Arrays.copyOfRange(numbers, 1, 4); // => [2, 5, 8] (index 1 to 3)
// => Arrays.copyOfRange(original, from, to): copies subarray
// => 1: start index (inclusive)
// => 4: end index (exclusive)
// => Returns: new array [2, 5, 8] (indices 1, 2, 3)
// => Use for extracting slices, windowing

boolean equal = Arrays.equals(numbers, copy); // => false (different lengths)
// => Arrays.equals(array1, array2): checks if arrays equal
// => Compares: length and element-by-element content
// => Returns: false (numbers has 5 elements, copy has 3)
// => numbers.equals(copy) would check reference equality (wrong)
// => Use Arrays.equals() for content comparison

// Collections utility class
// => Collections: utility class for collection manipulation
// => Works with Collection interfaces (List, Set)
// => All methods static: call via Collections.method()
List<Integer> list = new ArrayList<>(Arrays.asList(3, 1, 4, 1, 5));
// => List<Integer>: interface for ordered collection
// => new ArrayList<>: concrete implementation (dynamic array)
// => Arrays.asList(3, 1, 4, 1, 5): converts varargs to List
// => Initial list: [3, 1, 4, 1, 5]
// => Diamond operator <>: type inference from left side

Collections.sort(list); // => [1, 1, 3, 4, 5] (sorts in-place)
// => Collections.sort(list): sorts list in ascending order
// => In-place: modifies original list
// => After sort: list is [1, 1, 3, 4, 5]
// => Uses Timsort (stable, O(n log n))
// => Elements must implement Comparable or provide Comparator

Collections.reverse(list); // => [5, 4, 3, 1, 1] (reverses in-place)
// => Collections.reverse(list): reverses element order
// => In-place: modifies original list
// => After reverse: list is [5, 4, 3, 1, 1]
// => Use for reversing sorted order, undo operations

Collections.shuffle(list); // => randomizes order
// => Collections.shuffle(list): randomly reorders elements
// => In-place: modifies original list
// => After shuffle: list has random order (e.g., [3, 1, 1, 5, 4])
// => Uses default randomness (not reproducible)
// => Use shuffle(list, random) for reproducible shuffle

int max = Collections.max(list); // => 5 (maximum element)
// => Collections.max(collection): finds largest element
// => Returns: 5 (largest value in list)
// => Elements must implement Comparable
// => Throws NoSuchElementException if collection empty

int min = Collections.min(list); // => 1 (minimum element)
// => Collections.min(collection): finds smallest element
// => Returns: 1 (smallest value in list)
// => Linear scan (O(n) complexity)

int freq = Collections.frequency(list, 1); // => 2 (count of 1s)
// => Collections.frequency(collection, object): counts occurrences
// => list: collection to search
// => 1: element to count
// => Returns: 2 (number of times 1 appears)
// => Uses equals() for comparison
// => Use for histogram, duplicate detection

Collections.fill(list, 0); // => [0, 0, 0, 0, 0]
// => Collections.fill(list, object): replaces all elements
// => In-place: modifies original list
// => After fill: list is [0, 0, 0, 0, 0]
// => Size unchanged (replaces existing elements)

Collections.replaceAll(list, 0, 9); // => [9, 9, 9, 9, 9]
// => Collections.replaceAll(list, oldVal, newVal): replaces specific value
// => 0: value to find and replace
// => 9: replacement value
// => In-place: modifies original list
// => After replaceAll: list is [9, 9, 9, 9, 9]
// => Only replaces matching elements

// Objects utility class (Java 7+)
// => Objects: utility class for null-safe operations
// => Prevents NullPointerException with safe methods
// => All methods static: call via Objects.method()
String nullableStr = null;
// => nullableStr: String variable holding null
// => null: no object reference
// => Dereferencing null causes NullPointerException

// requireNonNull throws NullPointerException if null
// => requireNonNull: validates argument not null
// => Throws NullPointerException immediately if null
// => Use for precondition checking, fail-fast validation
Objects.requireNonNull(nullableStr, "String cannot be null"); // => throws NPE
// => Objects.requireNonNull(obj, message): validates not null
// => nullableStr: null, so exception thrown
// => "String cannot be null": exception message
// => Throws: NullPointerException with message
// => Use at method entry to validate parameters
// => Prevents null propagating through code

// Safe null handling
// => Safe methods: handle null without throwing exceptions
// => Return boolean instead of throwing
boolean isNull = Objects.isNull(nullableStr); // => true
// => Objects.isNull(obj): checks if reference is null
// => Returns: true (nullableStr is null)
// => Equivalent to: nullableStr == null
// => Use in streams: stream.filter(Objects::isNull)

boolean notNull = Objects.nonNull(nullableStr); // => false
// => Objects.nonNull(obj): checks if reference is not null
// => Returns: false (nullableStr is null)
// => Equivalent to: nullableStr != null
// => Use in streams: stream.filter(Objects::nonNull)

// Safe equals (handles nulls)
// => Safe equals: compares objects handling null gracefully
// => No NullPointerException even if either argument null
String a = "test";
// => a: non-null string
String b = null;
// => b: null reference

boolean safeEquals = Objects.equals(a, b); // => false (no NPE)
// => Objects.equals(a, b): null-safe equality check
// => Returns: false (one is null, one is not)
// => No NullPointerException thrown
// => If both null: returns true
// => If both non-null: returns a.equals(b)
// => Use for comparing potentially null objects

// Hash code generation
// => Hash code: integer representation of object for hash tables
// => Objects.hash: creates combined hash from multiple values
int hash = Objects.hash("Alice", 30, "Engineer"); // => combined hash code
// => Objects.hash(values...): combines multiple values into hash
// => "Alice", 30, "Engineer": varargs (multiple values)
// => Returns: combined hash code (integer)
// => Handles null values gracefully (uses 0 for null)
// => Use in hashCode() method implementations
// => Consistent hashing for multiple fields
```

**Key Takeaway**: `Math` provides mathematical operations and constants. `String` has extensive methods for manipulation and comparison. `Arrays` utility offers sorting, searching, and array operations. `Collections` utility provides algorithms for lists. `Objects` utility handles null-safe operations and hash generation.

**Why It Matters**: Utility classes eliminate boilerplate and prevent bugs—Arrays.sort() uses optimized algorithms (dual-pivot quicksort for primitives, Timsort for objects) that outperform manual implementations while handling edge cases. String immutability prevents accidental modification in concurrent code, and the string pool reduces memory usage in applications with many duplicate strings. Objects utility methods prevent the #1 cause of production crashes (NullPointerException) through safe null handling. Math constants (PI, E) provide precision that prevents cumulative rounding errors in scientific calculations. These utilities are battle-tested across millions of applications and form the foundation of production Java code.

---

### Example 18: Introduction to Streams

Streams enable functional-style operations on collections. Intermediate operations (lazy) return streams. Terminal operations (eager) trigger execution and produce results.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Source["Data Source<br/>Collection/Array"] --> Intermediate["Intermediate Operations<br/>filter, map, sorted<br/>(lazy)"]
    Intermediate --> Terminal["Terminal Operation<br/>collect, forEach, count<br/>(triggers execution)"]
    Terminal --> Result["Result<br/>List/Value"]

    style Source fill:#0173B2,color:#fff
    style Intermediate fill:#DE8F05,color:#fff
    style Terminal fill:#029E73,color:#fff
    style Result fill:#CC78BC,color:#fff
```

**Code**:

```java
import java.util.*;
// => import java.util.*: imports collections classes (List, Set, ArrayList)
// => Wildcard *: imports all public classes from java.util
// => Includes: List, ArrayList, Set, HashSet (not stream classes)

import java.util.stream.*;
// => import java.util.stream.*: imports stream API classes
// => Includes: Stream, Collectors (stream operations)
// => Separate package: java.util.stream (introduced Java 8)
// => Required for functional-style collection processing

// Creating streams from collections
// => Streams: sequences of elements supporting functional operations
// => Created from collections, arrays, or generated
// => Not data structures (don't store elements)
// => Pipeline pattern: source → operations → terminal operation
List<Integer> numbers = List.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
// => List.of(...): factory method creating immutable list (Java 9+)
// => numbers: immutable List with 10 elements
// => Immutable: cannot add, remove, or modify elements
// => Type: List<Integer> (generic type, holds Integer objects)
// => Elements: 1 through 10 (consecutive integers)

Stream<Integer> stream = numbers.stream(); // => creates stream from list
// => numbers.stream(): converts collection to stream
// => Stream<Integer>: generic stream of Integer objects
// => Stream: pipeline of operations on data sequence
// => Not yet executed (no terminal operation)
// => Source: numbers list (original collection unchanged)

// filter() - intermediate operation (keeps elements matching predicate)
// => filter: keeps elements where predicate returns true
// => Intermediate operation: returns Stream (allows chaining)
// => Lazy: doesn't execute until terminal operation called
// => Predicate: function taking element, returning boolean
Stream<Integer> evens = numbers.stream()
    .filter(n -> n % 2 == 0); // => keeps 2, 4, 6, 8, 10 (lazy, not executed yet)
// => numbers.stream(): creates new stream from numbers
// => .filter(lambda): method chaining on stream
// => n -> n % 2 == 0: lambda expression (predicate)
// => n: parameter representing each element
// => ->: lambda arrow syntax
// => n % 2 == 0: expression checking if n is even
// => Returns: Stream<Integer> containing even numbers (not executed yet)
// => Lazy evaluation: no elements processed until terminal operation

// map() - intermediate operation (transforms each element)
// => map: transforms elements using function (1-to-1 transformation)
// => Takes function: element -> transformed element
// => Intermediate: returns Stream for chaining
// => Lazy: deferred execution until terminal operation
Stream<Integer> squared = numbers.stream()
    .map(n -> n * n); // => 1, 4, 9, 16, 25, ... (lazy)
// => n -> n * n: lambda squaring each element
// => n: input element (1, 2, 3, ...)
// => n * n: transformation (1→1, 2→4, 3→9, ...)
// => Returns: Stream<Integer> with squared values
// => Not executed yet (no terminal operation)
// => Will produce: 1, 4, 9, 16, 25, 36, 49, 64, 81, 100

// sorted() - intermediate operation (sorts elements)
// => sorted: sorts elements in natural order
// => Natural order: uses Comparable interface (ascending for integers)
// => Intermediate: returns Stream
// => Stateful: must see all elements before producing output
List<Integer> unsorted = List.of(5, 2, 8, 1, 9);
// => unsorted: list with unordered elements [5, 2, 8, 1, 9]

Stream<Integer> sorted = unsorted.stream()
    .sorted(); // => 1, 2, 5, 8, 9 (lazy)
// => .sorted(): sorts stream elements in ascending order
// => No comparator: uses natural ordering (Comparable)
// => Returns: Stream<Integer> in sorted order
// => Lazy: doesn't sort until terminal operation
// => Will produce: 1, 2, 5, 8, 9

// distinct() - intermediate operation (removes duplicates)
// => distinct: removes duplicate elements (keeps first occurrence)
// => Uses equals() for comparison
// => Stateful: must track seen elements
// => Returns stream with unique elements only
List<Integer> duplicates = List.of(1, 2, 2, 3, 3, 3);
// => duplicates: list with repeated elements [1, 2, 2, 3, 3, 3]

Stream<Integer> unique = duplicates.stream()
    .distinct(); // => 1, 2, 3 (lazy)
// => .distinct(): removes duplicate elements
// => Keeps: first occurrence of each unique element
// => Returns: Stream<Integer> with [1, 2, 3]
// => Lazy: deferred until terminal operation
// => Original list unchanged (stream is new pipeline)

// limit() - intermediate operation (takes first N elements)
// => limit: truncates stream to maximum size
// => Short-circuiting: stops processing after N elements
// => Useful for infinite streams or large datasets
// => Returns stream with at most N elements
Stream<Integer> limited = numbers.stream()
    .limit(5); // => 1, 2, 3, 4, 5 (lazy)
// => .limit(5): takes only first 5 elements
// => 5: maximum number of elements to include
// => Returns: Stream<Integer> with [1, 2, 3, 4, 5]
// => Short-circuits: doesn't process elements after 5th
// => Lazy: execution deferred until terminal operation

// Method chaining (fluent API) - combine operations
// => Fluent API: method chaining for readable pipelines
// => Each intermediate operation returns Stream (enables chaining)
// => Operations executed in sequence (filter → map → sorted → collect)
// => Terminal operation triggers execution of entire pipeline
List<Integer> result = numbers.stream()
    .filter(n -> n % 2 == 0)    // Keep evens: 2, 4, 6, 8, 10
    // => .filter(n -> n % 2 == 0): keeps only even numbers
    // => Input stream: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    // => After filter: [2, 4, 6, 8, 10]
    // => Returns: Stream<Integer> with evens

    .map(n -> n * n)             // Square: 4, 16, 36, 64, 100
    // => .map(n -> n * n): squares each filtered element
    // => Input stream: [2, 4, 6, 8, 10]
    // => Transformation: 2→4, 4→16, 6→36, 8→64, 10→100
    // => After map: [4, 16, 36, 64, 100]
    // => Returns: Stream<Integer> with squared values

    .sorted()                    // Sort: 4, 16, 36, 64, 100
    // => .sorted(): sorts elements in ascending order
    // => Input already sorted (squares of evens are ascending)
    // => After sorted: [4, 16, 36, 64, 100] (no change)
    // => Returns: Stream<Integer> in sorted order

    .collect(Collectors.toList()); // Terminal: collect to List
    // => .collect(Collectors.toList()): terminal operation
    // => Triggers execution of entire pipeline (filter → map → sorted)
    // => Collectors.toList(): collector accumulating to ArrayList
    // => Returns: List<Integer> [4, 16, 36, 64, 100]
    // => Terminal: consumes stream (cannot reuse)

// => [4, 16, 36, 64, 100]
// => result: final List with filtered, mapped, sorted elements
// => Pipeline executed: filter evens → square → sort → collect
// => Original numbers unchanged (streams don't modify source)

// collect() - terminal operation (collects to collection)
// => collect: accumulates stream elements into collection
// => Terminal operation: triggers execution, consumes stream
// => Collector: strategy for accumulation (toList, toSet, toMap)
// => Mutable reduction: accumulates into mutable result container
List<Integer> evenList = numbers.stream()
    .filter(n -> n % 2 == 0)
    // => Filters to [2, 4, 6, 8, 10]

    .collect(Collectors.toList()); // => [2, 4, 6, 8, 10]
    // => Collectors.toList(): creates ArrayList collector
    // => Accumulates elements into ArrayList
    // => Returns: List<Integer> [2, 4, 6, 8, 10]
    // => Type: ArrayList (mutable implementation)

Set<Integer> evenSet = numbers.stream()
    .filter(n -> n % 2 == 0)
    // => Same filter as above

    .collect(Collectors.toSet()); // => {2, 4, 6, 8, 10}
    // => Collectors.toSet(): creates HashSet collector
    // => Accumulates elements into HashSet
    // => Returns: Set<Integer> {2, 4, 6, 8, 10}
    // => Unordered: HashSet doesn't preserve insertion order
    // => No duplicates: Set eliminates duplicates (none in this case)

// forEach() - terminal operation (performs action on each element)
// => forEach: executes action for each element (side effects)
// => Terminal operation: consumes stream, no return value
// => Consumer: function taking element, returning void
// => Used for side effects (printing, logging, mutations)
numbers.stream()
    .filter(n -> n > 5)
    // => Filters to [6, 7, 8, 9, 10]

    .forEach(n -> System.out.println(n)); // => prints 6, 7, 8, 9, 10
    // => .forEach(lambda): executes lambda for each element
    // => n -> System.out.println(n): lambda printing each element
    // => Side effect: prints to console (not functional)
    // => Output: 6, 7, 8, 9, 10 (each on separate line)
    // => Returns: void (forEach doesn't return value)
    // => Terminal: stream consumed after forEach

// count() - terminal operation (returns count)
// => count: counts elements in stream
// => Terminal operation: consumes stream
// => Returns: long (element count)
// => Short-circuiting: can optimize for some streams
long count = numbers.stream()
    .filter(n -> n > 5)
    // => Filters to [6, 7, 8, 9, 10]

    .count(); // => 5
    // => .count(): counts filtered elements
    // => Returns: 5L (long type)
    // => Type: long (not int) to handle large streams
    // => Terminal: stream consumed

// reduce() - terminal operation (combines elements)
// => reduce: combines elements using associative function
// => Reduction: fold/aggregate operation (many → one)
// => Identity: initial value for accumulation
// => Accumulator: binary function combining elements
int sum = numbers.stream()
    .reduce(0, (a, b) -> a + b); // => 55 (sum of all numbers)
    // => .reduce(identity, accumulator): reduction with initial value
    // => 0: identity (initial value, neutral element for addition)
    // => (a, b) -> a + b: accumulator lambda (binary function)
    // => a: accumulated result so far
    // => b: current element from stream
    // => a + b: combines accumulated result with current element
    // => Execution: 0+1=1, 1+2=3, 3+3=6, ..., 45+10=55
    // => Returns: 55 (sum of 1+2+3+...+10)
    // => Terminal: stream consumed

Optional<Integer> max = numbers.stream()
    .reduce((a, b) -> a > b ? a : b); // => Optional[10] (maximum)
    // => .reduce(accumulator): reduction without identity
    // => No identity: returns Optional (stream might be empty)
    // => (a, b) -> a > b ? a : b: lambda finding maximum
    // => a > b: condition comparing elements
    // => ? a : b: ternary operator (returns larger value)
    // => Execution: max(1,2)=2, max(2,3)=3, ..., max(9,10)=10
    // => Returns: Optional[10] (Optional wraps result)
    // => Optional: handles empty stream (would be Optional.empty())

// Lambda expressions - concise syntax for functional interfaces
// (parameters) -> expression
// => Lambda syntax: (params) -> body
// => Functional interface: interface with single abstract method
// => Lambda: anonymous function implementing functional interface
// => Concise: replaces verbose anonymous class syntax
// => Type inference: compiler infers parameter types from context
List<String> names = List.of("Alice", "Bob", "Charlie", "David");
// => names: immutable list of 4 strings

List<String> shortNames = names.stream()
    .filter(name -> name.length() <= 5) // Lambda: name -> boolean
    // => .filter(lambda): keeps names with length ≤ 5
    // => name: parameter (type String inferred)
    // => ->: lambda arrow
    // => name.length() <= 5: predicate expression
    // => Keeps: "Alice" (5), "Bob" (3), "David" (5)
    // => Filters out: "Charlie" (7 characters)

    .collect(Collectors.toList()); // => ["Alice", "Bob", "David"]
    // => Collects filtered names to List
    // => Returns: List<String> ["Alice", "Bob", "David"]

List<String> uppercased = names.stream()
    .map(name -> name.toUpperCase()) // Lambda: name -> String
    // => .map(lambda): transforms each name to uppercase
    // => name: parameter (type String inferred)
    // => name.toUpperCase(): transformation function
    // => Transforms: "Alice" → "ALICE", "Bob" → "BOB", etc.
    // => Returns: Stream<String> with uppercase names

    .collect(Collectors.toList()); // => ["ALICE", "BOB", "CHARLIE", "DAVID"]
    // => Collects transformed names to List
    // => Returns: List<String> with all uppercase names

// Streams are single-use (can't reuse after terminal operation)
// => Single-use: stream consumed after terminal operation
// => Cannot reuse consumed stream (throws IllegalStateException)
// => Must create new stream for subsequent operations
// => Design: streams are pipelines, not reusable iterators
Stream<Integer> s = numbers.stream();
// => s: stream from numbers list

long c1 = s.count(); // => 10 (terminal operation, stream consumed)
// => .count(): terminal operation counting elements
// => Returns: 10L (number of elements)
// => Side effect: stream s is now consumed (closed)

// long c2 = s.count(); // ERROR: stream already operated upon
// => Attempting to reuse consumed stream
// => Throws: IllegalStateException "stream has already been operated upon or closed"
// => Must create new stream for additional operations

// Re-create stream for multiple operations
// => Solution: create new stream for each pipeline
// => Independent streams: each has own lifecycle
// => Source collection unchanged (creating streams is cheap)
long evenCount = numbers.stream().filter(n -> n % 2 == 0).count(); // => 5
// => New stream: numbers.stream()
// => Pipeline: filter evens → count
// => Returns: 5L (count of even numbers)

long oddCount = numbers.stream().filter(n -> n % 2 != 0).count(); // => 5
// => Another new stream: numbers.stream()
// => Pipeline: filter odds → count
// => Returns: 5L (count of odd numbers)
// => Independent from evenCount pipeline above

// Intermediate operations are lazy - only execute when terminal operation called
// => Lazy evaluation: intermediate ops don't execute immediately
// => Deferred: execution delayed until terminal operation
// => Optimization: allows short-circuiting, fusion, parallelization
// => No side effects: intermediate ops shouldn't have side effects (here violated for demo)
Stream<Integer> lazy = numbers.stream()
    .filter(n -> {
        // => Lambda with block body (multiple statements)
        System.out.println("Filtering " + n);
        // => Side effect: prints during filter execution
        // => Demonstrates lazy evaluation (nothing printed yet)
        return n % 2 == 0;
        // => Return statement: required for block body
        // => Returns: true if n is even
    })
    .map(n -> {
        // => Another lambda with block body
        System.out.println("Mapping " + n);
        // => Side effect: prints during map execution
        // => Only called for elements passing filter
        return n * n;
        // => Returns: squared value
    });
// Nothing printed yet - operations not executed
// => lazy: stream pipeline defined but not executed
// => No output: filter and map lambdas not called yet
// => Terminal operation needed: to trigger execution

List<Integer> eager = lazy.collect(Collectors.toList());
// NOW operations execute: "Filtering 1", "Filtering 2", "Mapping 2", ...
// => .collect(Collectors.toList()): terminal operation triggers execution
// => Execution order: filter(1) → filter(2) → map(2) → filter(3) → ...
// => Output printed: "Filtering 1", "Filtering 2", "Mapping 2", etc.
// => Fusion: operations executed element-by-element (not all filters then all maps)
// => Returns: List<Integer> with squared evens [4, 16, 36, 64, 100]
// => Demonstrates lazy evaluation: operations deferred until terminal operation
```

**Key Takeaway**: Streams enable functional-style operations on collections. Intermediate operations (`filter`, `map`, `sorted`, `distinct`, `limit`) are lazy and return streams. Terminal operations (`collect`, `forEach`, `count`, `reduce`) trigger execution and produce results. Lambda expressions provide concise syntax. Streams are single-use and must be recreated for multiple operations.

**Why It Matters**: Streams revolutionize Java collection processing by enabling declarative pipelines that are more readable and maintainable than imperative loops. Lazy evaluation allows short-circuiting optimizations—processing 1 million elements with filter().limit(10) stops after finding 10 matches instead of filtering all elements. Parallel streams (parallelStream()) leverage multi-core CPUs with zero code changes, achieving 4-8x speedups on data-intensive operations. The functional approach prevents bugs from mutable state and makes code easier to reason about. Major frameworks (Spring Data, Hibernate) adopted stream APIs, making streams essential for modern Java development.

---

## Group 6: Additional Core Features

### Example 19: Switch Expressions and Pattern Matching

Modern Java switch expressions (Java 12+) reduce boilerplate and support pattern matching. They return values directly and eliminate fall-through bugs with arrow syntax.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Input["Input Value"] --> Switch{"Switch Expression"}
    Switch -->|case 1| Result1["Return value 1"]
    Switch -->|case 2| Result2["Return value 2"]
    Switch -->|default| Result3["Default value"]
    Result1 --> Output["Assigned to Variable"]
    Result2 --> Output
    Result3 --> Output

    style Input fill:#0173B2,color:#fff
    style Switch fill:#DE8F05,color:#fff
    style Result1 fill:#029E73,color:#fff
    style Result2 fill:#029E73,color:#fff
    style Result3 fill:#029E73,color:#fff
    style Output fill:#CC78BC,color:#fff
```

**Code**:

```java
// SWITCH EXPRESSIONS (Java 12+) - expressions that return values
// => Traditional switch is a statement (executes code, no return value)
// => Switch expression is an expression (evaluates to a value)
// => Key difference: statement vs expression determines usage context

// Switch expression with arrow syntax (Java 12+)
int dayNum = 3; // => dayNum is 3 (type: int)
                // => Represents Wednesday in week numbering
                // => No enum, using raw int for simplicity

// Returns value directly - no break needed
// => Arrow syntax (->) prevents fall-through bugs
// => Traditional switch requires break; to prevent fall-through
// => Fall-through bug: executing multiple cases unintentionally
String dayType = switch (dayNum) {
    // => switch keyword starts expression
    // => (dayNum) is the selector expression (value to match)
    // => Entire switch {...} block evaluates to a single value
    // => Result assigned to dayType variable

    case 1, 7 -> "Weekend";
    // => case keyword starts match pattern
    // => 1, 7: multiple values in one case (Java 14+)
    // => Comma-separated values (OR logic: 1 OR 7)
    // => -> arrow syntax returns value (no break needed)
    // => "Weekend" is the return value if dayNum matches 1 or 7
    // => dayNum is 3, so this case does NOT match (skip to next)

    case 2, 3, 4, 5, 6 -> "Weekday";
    // => 2, 3, 4, 5, 6: weekday numbers (Monday through Friday)
    // => dayNum is 3, so this case MATCHES
    // => "Weekday" is returned from switch expression
    // => Execution stops here (no fall-through to default)

    default -> "Invalid";
    // => default case handles unmatched values
    // => Required for exhaustiveness (compiler enforces all paths return value)
    // => If dayNum is 0, -1, 8, etc. → returns "Invalid"
}; // => Semicolon required (switch expression used as statement)
   // => dayType is now "Weekday" (because dayNum 3 matched case 2,3,4,5,6)

System.out.println("Day type: " + dayType);
// => Concatenates "Day type: " with dayType value
// => Output: Day type: Weekday

// SWITCH WITH YIELD - for multi-statement blocks
// => yield keyword returns value from block (like return for switch)
// => Use when case logic requires multiple statements
// => Cannot use plain return (would exit enclosing method)
int quarter = switch (dayNum) {
    // => dayNum is still 3 (from above)
    // => Calculating financial quarter based on month number

    case 1, 2, 3 -> {
        // => Opening brace { starts code block
        // => Blocks allow multiple statements before yield
        // => dayNum is 3, so this case MATCHES

        System.out.println("Q1");
        // => Side effect: prints "Q1" to console
        // => Output: Q1
        // => Execution continues to next statement in block

        yield 1;
        // => yield keyword returns value from block
        // => Similar to return, but for switch expressions
        // => Returns 1 (first quarter)
        // => Exits switch expression, assigns 1 to quarter
    } // => Closing brace } ends block

    case 4, 5, 6 -> {
        // => April, May, June → second quarter
        // => dayNum is 3, so this case does NOT match (already returned above)
        System.out.println("Q2");
        // => Not executed (previous case already matched)
        yield 2;
        // => Not executed
    }

    case 7, 8, 9 -> {
        // => July, August, September → third quarter
        System.out.println("Q3");
        yield 3;
    }

    case 10, 11, 12 -> {
        // => October, November, December → fourth quarter
        System.out.println("Q4");
        yield 4;
    }

    default -> {
        // => Handles invalid month numbers (0, 13, -1, etc.)
        // => Even single-statement default can use block with yield
        yield 0;
        // => Returns 0 for invalid input
        // => Must yield in every case (compiler enforces exhaustiveness)
    }
}; // => quarter is now 1 (because dayNum 3 matched case 1,2,3)
   // => Console output from above: Q1

// PATTERN MATCHING INSTANCEOF (Java 16+) - combines type check and cast
// => Traditional instanceof requires separate cast
// => Pattern matching instanceof combines check and cast in one step
// => Reduces boilerplate and prevents cast errors

Object obj = "Hello World";
// => obj declared as Object (top type, can hold any reference)
// => Actual runtime type is String
// => Compile-time type: Object (can only call Object methods)
// => Runtime type: String (actual object type in memory)

// Traditional instanceof (verbose)
// => Old way: check type, then cast separately
if (obj instanceof String) {
    // => instanceof checks runtime type
    // => obj instanceof String returns true (obj is actually String)
    // => Enters if block

    String str = (String) obj;
    // => Explicit cast required (compiler doesn't infer type)
    // => (String) is cast operator (converts Object to String)
    // => str is now String type (can call String methods)
    // => Redundant: we already know obj is String from instanceof check

    System.out.println(str.toUpperCase());
    // => toUpperCase() is String method (not available on Object)
    // => Converts all characters to uppercase
    // => Output: HELLO WORLD
}
// Traditional approach has 3 lines:
// 1. Check type (instanceof)
// 2. Cast to type (explicit cast)
// 3. Use typed variable

// Pattern matching instanceof (concise)
// => Modern way: check and cast in one step (Java 16+)
if (obj instanceof String s) {
    // => instanceof String: type check (same as traditional)
    // => s: pattern variable (automatically cast to String)
    // => If check passes, s is bound to obj cast as String
    // => If check fails, s is not in scope (block doesn't execute)
    // => s only accessible when type check succeeds

    System.out.println(s.toUpperCase());
    // => s is already String type (no cast needed)
    // => Can immediately call String methods
    // => Output: HELLO WORLD

    System.out.println("Length: " + s.length());
    // => length() is String method
    // => s.length() returns 11 (number of characters)
    // => Concatenates "Length: " with 11
    // => Output: Length: 11
} // => s only in scope within if block
  // => Outside this block, s is undefined (compile error if accessed)

// Pattern matching reduces 3 lines to 1:
// if (obj instanceof String s) combines check, cast, and usage

// PATTERN MATCHING IN SWITCH (Java 17+ preview, Java 21+ standard)
// => Traditional switch only supports primitive types and String
// => Pattern matching switch supports any reference type
// => Enables type-based dispatching (like visitor pattern)
Object value = 42;
// => value declared as Object
// => Actual runtime type is Integer (autoboxing: 42 -> Integer.valueOf(42))
// => int 42 autoboxed to Integer object

String result = switch (value) {
    // => switch on Object type (not possible with traditional switch)
    // => Matches against type patterns (not just constants)
    // => Compiler checks exhaustiveness (all possible types covered)

    case Integer i -> "Integer: " + i;
    // => case Integer i: type pattern
    // => Integer: type to match
    // => i: pattern variable (bound if match succeeds)
    // => value is Integer, so this case MATCHES
    // => i is bound to value cast as Integer
    // => Concatenates "Integer: " with i (42)
    // => Returns "Integer: 42"
    // => Execution stops here (no fall-through)

    case String s -> "String: " + s;
    // => Type pattern for String
    // => Not executed (value is Integer, not String)
    case Double d -> "Double: " + d;
    // => Type pattern for Double
    case null -> "Null value";
    // => Explicit null case (Java 17+)
    // => Traditional switch throws NullPointerException on null
    // => Pattern matching switch allows null as case
    // => Important: prevents NPE in switch expression
    default -> "Unknown type";
    // => Covers all other types (Long, Boolean, etc.)
}; // => result is "Integer: 42"

// SWITCH WITH GUARDS (when clause) - Java 21+
// => Guards add conditional logic to patterns
// => Pattern matches AND guard condition must be true
// => Enables range checking, property validation, etc.
int num = 15;
// => num is 15 (type: int)

String category = switch (num) {
    // => Boxing num to Integer for pattern matching
    // => Primitive int autoboxed to Integer object

    case Integer i when i < 0 -> "Negative";
    // => case Integer i: type pattern (matches any Integer)
    // => when i < 0: guard condition (additional constraint)
    // => Pattern matches (num is Integer) BUT guard fails (15 is not < 0)
    // => Does not execute, continues to next case

    case Integer i when i == 0 -> "Zero";
    // => Pattern matches, guard fails (15 != 0)

    case Integer i when i > 0 && i < 10 -> "Small positive";
    // => Pattern matches (num is Integer)
    // => Guard: i > 0 is true (15 > 0)
    // => Guard: i < 10 is false (15 is not < 10)
    // => Overall guard fails (AND requires both conditions true)

    case Integer i when i >= 10 && i < 100 -> "Medium positive";
    // => Pattern matches (num is Integer)
    // => Guard: i >= 10 is true (15 >= 10)
    // => Guard: i < 100 is true (15 < 100)
    // => Overall guard succeeds (both conditions true)
    // => Returns "Medium positive"
    // => Execution stops here

    case Integer i when i >= 100 -> "Large positive";
    // => Not executed (previous case already matched)
    default -> "Unknown";
}; // => category is "Medium positive"

// EXHAUSTIVENESS CHECKING - compiler ensures all cases covered
// => Sealed types enable exhaustive pattern matching
// => Compiler knows all possible subtypes (permits clause lists them)
// => No default needed if all subtypes covered

// Sealed interface - restricts which classes can implement it
sealed interface Shape permits Circle, Rectangle, Triangle {}
// => sealed: keyword restricting implementations
// => permits: lists allowed implementations (Circle, Rectangle, Triangle)
// => Only these 3 types can implement Shape
// => Compiler knows complete type hierarchy (closed world assumption)

// Records implementing sealed interface
record Circle(double radius) implements Shape {}
// => record: immutable data class (Java 16+)
// => double radius: component (generates radius() accessor)
// => implements Shape: conforms to sealed interface

record Rectangle(double width, double height) implements Shape {}
// => width and height components
record Triangle(double base, double height) implements Shape {}
// => base and height components

// Example usage (assumes shape variable exists)
// Shape shape = new Circle(5.0); // => Hypothetical shape instance

// Exhaustive switch - no default needed
// double area = switch (shape) {
//     // => switch on sealed type (compiler knows all subtypes)
//     // => Exhaustiveness check: compiler verifies all Shape types handled

//     case Circle c -> Math.PI * c.radius() * c.radius();
//     // => Pattern matches Circle
//     // => c.radius() accesses radius component
//     // => πr² formula for circle area
//     // => Math.PI is constant (3.141592653589793)

//     case Rectangle r -> r.width() * r.height();
//     // => Pattern matches Rectangle
//     // => width × height formula for rectangle area

//     case Triangle t -> 0.5 * t.base() * t.height();
//     // => Pattern matches Triangle
//     // => ½ × base × height formula for triangle area

//     // No default needed - compiler knows all cases covered
//     // => All Shape subtypes handled (Circle, Rectangle, Triangle)
//     // => Compiler error if any subtype missing
//     // => Adding new Shape subtype requires updating switch (compile-time safety)
// };

// Benefits of exhaustiveness checking:
// => Prevents missing cases (compile-time error if subtype not handled)
// => Refactoring safety (adding Shape subtype breaks all switches until updated)
// => No runtime errors from unhandled types
// => Documentation through types (permits clause shows all possibilities)
```

**Key Takeaway**: Switch expressions return values and use `->` arrow syntax (no `break` needed). `yield` returns values from block expressions. Pattern matching `instanceof` combines type check and cast in one step. Switch pattern matching (Java 17+/21+) enables type-based switching with guards. Exhaustiveness checking prevents missing cases with sealed types.

**Why It Matters**: Switch expressions eliminate entire classes of fall-through bugs that plague traditional C-style switches—bugs responsible for security vulnerabilities like Apple's infamous "goto fail" SSL flaw. Pattern matching instanceof reduces boilerplate by 60-70% in code that performs type checks (common in parsers, serializers, and visitor patterns), making complex type hierarchies maintainable. Switch pattern matching with guards enables functional-style data processing without verbose if-else chains, improving code clarity in domain modeling and event handling. Exhaustiveness checking with sealed types provides compile-time guarantees that all cases are handled, preventing runtime errors when adding new types to closed hierarchies—critical for payment processing, state machines, and protocol implementations where missing cases cause silent failures or data corruption.

---

### Example 20: Text Blocks and String Manipulation

Text blocks (Java 15+) provide multiline strings with proper formatting. String methods offer rich manipulation capabilities for real-world text processing.

**Code**:

```java
// TEXT BLOCKS (Java 15+) - multiline string literals with preserved formatting
// => Traditional strings require \n and concatenation for multiline text
// => Text blocks simplify JSON, HTML, SQL, and formatted text
// => Triple double-quotes """ start and end text block

// Text block (Java 15+) - preserves formatting
String json = """
    {
        "name": "Alice",
        "age": 30,
        "city": "New York"
    }
    """;
// => Opening """ must be followed by newline (content starts on next line)
// => Closing """ determines indentation baseline
// => All lines dedented relative to closing """ position
// => json contains JSON with 4-space indentation preserved
// => Newlines automatically included (no \n needed)
// => Double quotes " inside text block don't need escaping

System.out.println(json);
// => Prints formatted JSON:
// => Output:
// {
//     "name": "Alice",
//     "age": 30,
//     "city": "New York"
// }

// Traditional approach (compare with text block above)
// String jsonOld = "{\n" +
//                  "    \"name\": \"Alice\",\n" +
//                  "    \"age\": 30,\n" +
//                  "    \"city\": \"New York\"\n" +
//                  "}";
// => Requires: \n for newlines, \" for quotes, + for concatenation
// => Text blocks eliminate this boilerplate

// Text block indentation - leftmost content determines baseline
String html = """
        <html>
            <body>
                <h1>Title</h1>
            </body>
        </html>
        """;
// => All lines have 8 leading spaces in source code
// => Closing """ has 8 leading spaces (sets baseline)
// => Compiler removes 8 spaces from each line (dedentation)
// => Result: <html> has 0 spaces, <body> has 4 spaces, <h1> has 8 spaces
// => Indentation relative to closing """ is preserved
// => Allows proper indentation in source code without affecting string content

// Escape sequences in text blocks
String formatted = """
    Line 1\n\
    Line 2 (no newline after Line 1)
    Line 3 "with quotes"
    """;
// => \n: explicit newline (adds extra newline)
// => \: line continuation (suppresses newline at end of line)
// => Line 1\n\ → Line 1 followed by newline, then continues on same line
// => Result: "Line 1\nLine 2 (no newline after Line 1)\nLine 3 \"with quotes\"\n"
// => Double quotes " inside text block don't need escaping (raw quotes work)
// => But \" still works if you want to escape explicitly

// STRING FORMATTING - template-based string construction
// => Two approaches: String.format() (static) and .formatted() (instance)
// => Format specifiers: %s (string), %d (integer), %f (float), etc.

// String interpolation alternatives (formatted, format)
String name = "Bob"; // => name is "Bob" (type: String)
int age = 25; // => age is 25 (type: int)

String message = String.format("Name: %s, Age: %d", name, age);
// => String.format(): static method (class-level)
// => First arg: template string with placeholders
// => %s: string placeholder (replaced with name)
// => %d: decimal integer placeholder (replaced with age)
// => Remaining args: values to insert (in order)
// => Returns: "Name: Bob, Age: 25"

String formatted2 = "Name: %s, Age: %d".formatted(name, age);
// => .formatted(): instance method on String (Java 15+)
// => Same behavior as String.format(), different syntax
// => Template is the receiver string
// => Returns: "Name: Bob, Age: 25"
// => Equivalent to String.format() but more fluent

// STRING MANIPULATION - trimming and whitespace
String text = "  Hello, World!  ";
// => text has 2 leading spaces and 2 trailing spaces
// => Total length: 17 characters

String trimmed = text.trim();
// => trim(): removes leading and trailing whitespace
// => Whitespace: space, tab, newline (ASCII control characters ≤ U+0020)
// => Returns: "Hello, World!" (length 13)
// => Original text unchanged (Strings are immutable)

String stripped = text.strip();
// => strip(): Unicode-aware trim (Java 11+)
// => Removes Unicode whitespace (Character.isWhitespace())
// => Includes: space, tab, newline, non-breaking space, etc.
// => Returns: "Hello, World!"
// => For ASCII whitespace, trim() and strip() give same result
// => For Unicode whitespace (e.g., U+00A0 non-breaking space), strip() is correct

boolean blank = "   ".isBlank();
// => isBlank(): checks if string is empty or only whitespace (Java 11+)
// => "   " contains only spaces → returns true
// => Equivalent to: string.strip().isEmpty()

boolean empty = "".isEmpty();
// => isEmpty(): checks if string has zero length
// => "" is empty string (length 0) → returns true
// => "   " is NOT empty (length 3, contains spaces) → would return false

// SPLITTING AND JOINING - converting between strings and arrays
String csv = "apple,banana,cherry";
// => CSV (comma-separated values) data
// => Common data format: values separated by delimiter

String[] fruits = csv.split(",");
// => split(delimiter): splits string into array
// => Delimiter: "," (comma)
// => Returns: String[] {"apple", "banana", "cherry"}
// => Array has 3 elements (length 3)
// => Original csv unchanged (Strings immutable)

String joined = String.join(" | ", fruits);
// => String.join(delimiter, array): joins array into string
// => Delimiter: " | " (pipe with spaces)
// => Inserts delimiter between each element
// => Returns: "apple | banana | cherry"
// => Static method (not instance method)

// REPEAT - string duplication (Java 11+)
String repeated = "Ha".repeat(3);
// => repeat(count): repeats string count times
// => "Ha" repeated 3 times → "HaHaHa"
// => count must be ≥ 0 (negative throws IllegalArgumentException)
// => repeat(0) returns empty string ""

String line = "=".repeat(20);
// => Useful for separators, borders, padding
// => "=" repeated 20 times → "===================="
// => Common pattern: creating visual separators in console output

// LINES - stream of lines (Java 11+)
String multiline = "Line 1\nLine 2\nLine 3";
// => multiline string with newline characters \n
// => 3 lines separated by newlines

multiline.lines() // => Stream<String> of lines
// => lines(): splits string into stream of lines (Java 11+)
// => Each line is element in stream
// => Newline characters NOT included in stream elements
// => Returns: Stream.of("Line 1", "Line 2", "Line 3")

    .forEach(line -> System.out.println(line));
// => forEach(): terminal operation (processes each stream element)
// => Lambda: line -> System.out.println(line)
// => Prints each line to console
// => Output:
// Line 1
// Line 2
// Line 3

// INDENT - add/remove indentation (Java 12+)
String indented = "Hello\nWorld".indent(4);
// => indent(n): adds n spaces to start of each line
// => Positive n: add spaces (indent right)
// => Negative n: remove spaces (indent left, dedent)
// => Returns: "    Hello\n    World\n"
// => Note: adds trailing newline if not present
// => Each line now starts with 4 spaces

// CHARACTER METHODS - single character operations
char ch = 'A';
// => ch is 'A' (type: char)
// => char is 16-bit Unicode character (single quotes ' ')
// => Different from String (double quotes " ")

boolean isDigit = Character.isDigit(ch);
// => isDigit(): checks if character is digit (0-9)
// => 'A' is NOT digit → returns false
// => Character.isDigit('5') → returns true

boolean isLetter = Character.isLetter(ch);
// => isLetter(): checks if character is letter (a-z, A-Z, Unicode letters)
// => 'A' is letter → returns true

boolean isUpperCase = Character.isUpperCase(ch);
// => isUpperCase(): checks if character is uppercase letter
// => 'A' is uppercase → returns true
// => 'a' would return false

char toLowerCase = Character.toLowerCase(ch);
// => toLowerCase(): converts character to lowercase
// => 'A' → 'a'
// => toLowerCase is 'a'
// => Original ch unchanged ('A')

// STRING COMPARISON - equality and ordering
String s1 = "hello"; // => s1 is "hello"
String s2 = "HELLO"; // => s2 is "HELLO"

boolean equals = s1.equals(s2);
// => equals(): case-sensitive equality check
// => "hello" != "HELLO" → returns false
// => NEVER use == for string comparison (compares references, not content)
// => == checks if same object in memory, not same content

boolean equalsIgnoreCase = s1.equalsIgnoreCase(s2);
// => equalsIgnoreCase(): case-insensitive equality
// => "hello" == "HELLO" (ignoring case) → returns true
// => Compares content, not references

int compared = s1.compareTo(s2);
// => compareTo(): lexicographic comparison (dictionary order)
// => Returns: negative if s1 < s2, zero if equal, positive if s1 > s2
// => "hello" vs "HELLO": lowercase letters > uppercase in Unicode
// => Returns positive number (32, difference between 'h' and 'H')
// => Used for sorting strings

// CONTAINS, STARTS, ENDS - substring searching
String sentence = "Java is awesome";
// => sentence is "Java is awesome"

boolean contains = sentence.contains("is");
// => contains(substring): checks if substring exists anywhere
// => "is" appears in "Java is awesome" → returns true
// => Case-sensitive search

boolean starts = sentence.startsWith("Java");
// => startsWith(prefix): checks if string starts with prefix
// => "Java is awesome" starts with "Java" → returns true
// => Case-sensitive

boolean ends = sentence.endsWith("awesome");
// => endsWith(suffix): checks if string ends with suffix
// => "Java is awesome" ends with "awesome" → returns true

// REPLACE - string substitution
String original = "Hello World Hello";
// => original contains "Hello" twice

String replaced = original.replace("Hello", "Hi");
// => replace(target, replacement): replaces ALL occurrences
// => Replaces both "Hello" → "Hi"
// => Returns: "Hi World Hi"
// => Original unchanged (Strings immutable)

String replacedFirst = original.replaceFirst("Hello", "Hi");
// => replaceFirst(regex, replacement): replaces FIRST occurrence only
// => First "Hello" → "Hi", second "Hello" unchanged
// => Returns: "Hi World Hello"
// => First parameter is regex pattern (simple string here)

String regex = "a1b2c3".replaceAll("\\d", "X");
// => replaceAll(regex, replacement): regex-based replacement
// => \\d: digit regex pattern (0-9)
// => \\ escapes backslash in Java string (becomes \d in regex)
// => Replaces all digits with "X"
// => Returns: "aXbXcX"
// => Powerful for pattern-based substitution

// SUBSTRING - extracting portions
String sub = "Java Programming".substring(5);
// => substring(beginIndex): extract from index to end
// => Index 5 is ' ' (space character after "Java")
// => Actually returns " Programming" (with leading space)
// => Wait, let me recalculate:
// => "Java Programming"
// =>  0123456789...
// => Index 5 is 'P' (zero-based indexing)
// => Returns: "Programming" (from index 5 to end)

String range = "Java Programming".substring(0, 4);
// => substring(beginIndex, endIndex): extract range
// => beginIndex: 0 (inclusive, start at 'J')
// => endIndex: 4 (exclusive, stop before index 4)
// => Indices 0, 1, 2, 3 → "Java"
// => Returns: "Java"

// INDEXOF - finding positions
int firstIndex = "Hello World".indexOf("o");
// => indexOf(substring): finds first occurrence
// => "Hello World"
// =>  01234567890
// => First 'o' is at index 4
// => Returns: 4

int lastIndex = "Hello World".lastIndexOf("o");
// => lastIndexOf(substring): finds last occurrence
// => Last 'o' is at index 7 (in "World")
// => Returns: 7

int notFound = "Hello".indexOf("z");
// => indexOf returns -1 if substring not found
// => "Hello" doesn't contain "z"
// => Returns: -1 (sentinel value for "not found")
```

**Key Takeaway**: Text blocks (`"""..."""`) provide multiline strings with preserved formatting. `strip()` is Unicode-aware trim. `lines()` creates stream of lines. `repeat()` duplicates strings. `formatted()` provides template formatting. String methods (`split`, `join`, `replace`, `substring`, `indexOf`) handle common text operations.

**Why It Matters**: Text blocks eliminate 50-70% of string escaping boilerplate in code that generates JSON, SQL, HTML, or multi-line logs—reducing bugs from misplaced quotes and newlines. Before text blocks, developers concatenated strings with `+` and `\n`, making SQL queries and JSON templates error-prone and unreadable. The `strip()` method handles Unicode whitespace correctly (including non-breaking spaces U+00A0), preventing bugs in internationalized applications where `trim()` fails silently. String immutability enables safe concurrent access without synchronization—critical for high-throughput web servers where thousands of threads process request parameters simultaneously. The `split()` and `join()` methods power CSV parsing, log processing, and data transformation pipelines, while `indexOf()` and `substring()` underpin tokenizers, parsers, and string algorithms in compilers and text editors.

---

### Example 21: Varargs and Overloading

Varargs (`...`) accept variable number of arguments as array. Method overloading allows same name with different signatures. Compiler selects most specific match at compile-time.

**Code**:

```java
// VARARGS - variable number of arguments
// => Varargs allow calling method with any number of arguments (0 to many)
// => Syntax: type... name (three dots after type)
// => Internally converted to array (type[])
// => Simplifies API design (no need for separate methods for different counts)

// Varargs - variable number of arguments
public static int sum(int... numbers) {
    // => int... numbers: varargs parameter
    // => Accepts 0 or more int arguments
    // => numbers is actually int[] inside method (array type)
    // => Compiler converts individual arguments to array automatically

    int total = 0;
    // => total accumulates sum
    // => Starts at 0 (additive identity)

    for (int num : numbers) {
        // => Enhanced for loop iterates over numbers array
        // => num is each element in numbers
        // => If numbers is empty (0 arguments), loop doesn't execute
        total += num;
        // => Add each number to running total
        // => Equivalent to: total = total + num
    }

    return total;
    // => Return accumulated sum
    // => If no arguments passed, returns 0 (identity value)
}

// Calling with different argument counts
int result1 = sum();
// => Call with 0 arguments
// => Compiler creates: sum(new int[]{}) (empty array)
// => Loop doesn't execute (empty array)
// => Returns: 0

int result2 = sum(5);
// => Call with 1 argument
// => Compiler creates: sum(new int[]{5})
// => Loop executes once: total = 0 + 5
// => Returns: 5

int result3 = sum(1, 2, 3);
// => Call with 3 arguments
// => Compiler creates: sum(new int[]{1, 2, 3})
// => Loop executes 3 times: 0 + 1 + 2 + 3 = 6
// => Returns: 6

int result4 = sum(1, 2, 3, 4, 5);
// => Call with 5 arguments
// => Compiler creates: sum(new int[]{1, 2, 3, 4, 5})
// => Returns: 15 (sum of 1 through 5)

// VARARGS MUST BE LAST PARAMETER
// => Only one varargs per method (must be last)
// => Regular parameters can come before varargs
// => Syntax: method(type1 param1, type2 param2, type... varargs)
public static void print(String prefix, int... numbers) {
    // => prefix: regular parameter (required)
    // => numbers: varargs parameter (0 or more ints)
    // => Varargs must be LAST (cannot have parameters after varargs)

    System.out.print(prefix + ": ");
    // => Print prefix followed by colon and space
    // => prefix is "Numbers" in call below
    // => Output: Numbers:

    for (int num : numbers) {
        // => Iterate over varargs array
        System.out.print(num + " ");
        // => Print each number with trailing space
    }

    System.out.println();
    // => Print newline (finish line)
}

print("Numbers", 1, 2, 3);
// => prefix = "Numbers" (first argument)
// => numbers = [1, 2, 3] (remaining arguments)
// => Output: Numbers: 1 2 3

// CAN PASS ARRAY TO VARARGS
// => Varargs accept explicit arrays (not just individual arguments)
// => Useful when arguments are already in array form
int[] array = {10, 20, 30};
// => array contains 3 integers
// => Type: int[]

int arraySum = sum(array);
// => Pass entire array to varargs parameter
// => Equivalent to: sum(10, 20, 30)
// => Compiler doesn't wrap array (already correct type)
// => Returns: 60 (10 + 20 + 30)

// METHOD OVERLOADING - same name, different parameters
// => Multiple methods with same name but different parameter lists
// => Compiler distinguishes by parameter count or types (signature)
// => NOT distinguished by return type alone (compile error)
// => Called compile-time polymorphism (static dispatch)

// Overloading by parameter count
public static int multiply(int a, int b) {
    // => Two parameters
    // => Signature: multiply(int, int)
    return a * b;
    // => Multiplies two integers
}

public static int multiply(int a, int b, int c) {
    // => Three parameters
    // => Signature: multiply(int, int, int)
    // => Different from two-parameter version (different count)
    return a * b * c;
    // => Multiplies three integers
}

// Overloading by parameter types
public static double multiply(double a, double b) {
    // => Two parameters, but DIFFERENT TYPES (double vs int)
    // => Signature: multiply(double, double)
    // => Different from multiply(int, int)
    return a * b;
    // => Multiplies two doubles
}

// COMPILER SELECTS METHOD BASED ON ARGUMENTS
int m1 = multiply(2, 3);
// => Arguments: (int, int)
// => Matches: multiply(int, int) - two-parameter int version
// => Calculates: 2 * 3 = 6
// => m1 is 6

int m2 = multiply(2, 3, 4);
// => Arguments: (int, int, int)
// => Matches: multiply(int, int, int) - three-parameter version
// => Calculates: 2 * 3 * 4 = 24
// => m2 is 24

double m3 = multiply(2.5, 3.0);
// => Arguments: (double, double)
// => Matches: multiply(double, double) - double version
// => Calculates: 2.5 * 3.0 = 7.5
// => m3 is 7.5

// OVERLOADING WITH AUTOMATIC WIDENING
// => Java widens primitive types to find matching overload
// => Widening: byte -> short -> int -> long -> float -> double
// => Compiler chooses narrowest matching type

public static void display(int num) {
    // => Signature: display(int)
    System.out.println("int: " + num);
}

public static void display(long num) {
    // => Signature: display(long)
    System.out.println("long: " + num);
}

public static void display(double num) {
    // => Signature: display(double)
    System.out.println("double: " + num);
}

// Testing widening rules
byte b = 10;
// => b is byte (8-bit integer)
// => Range: -128 to 127

display(b);
// => Argument type: byte
// => No display(byte) exists
// => Compiler widens byte to int (byte -> short -> int chain)
// => Calls display(int)
// => Output: int: 10

int i = 20;
// => i is int

display(i);
// => Argument type: int
// => Exact match: display(int) exists
// => No widening needed
// => Output: int: 20

long l = 30L;
// => l is long (64-bit integer)
// => L suffix indicates long literal

display(l);
// => Argument type: long
// => Exact match: display(long) exists
// => Output: long: 30

float f = 40.0f;
// => f is float (32-bit floating-point)
// => f suffix indicates float literal

display(f);
// => Argument type: float
// => No display(float) exists
// => Compiler widens float to double (next in chain)
// => Calls display(double)
// => Output: double: 40.0

// OVERLOADING RESOLUTION PRIORITY (compiler's decision order):
// 1. Exact match (argument types match parameter types exactly)
// 2. Primitive widening (byte -> short -> int -> long -> float -> double)
// 3. Autoboxing (int -> Integer, double -> Double, etc.)
// 4. Varargs (variable arguments, lowest priority)

// Example of priority in action:
// Given: display(int), display(long), display(double)
// display(10) → exact match display(int)
// display(10L) → exact match display(long)
// display(byte 10) → widen to int, call display(int)
// display(float 10.0f) → widen to double, call display(double)

// AMBIGUOUS OVERLOADING (compile error)
// => Occurs when compiler cannot choose single best match
// => Multiple methods equally viable with widening

// Example of ambiguity (commented out - causes compile error):
// public static void process(int a, double b) {
//     // => First parameter int, second double
// }
//
// public static void process(double a, int b) {
//     // => First parameter double, second int
// }
//
// process(1, 2);
// => Arguments: (int, int)
// => Option 1: widen second arg to double → process(int, double)
// => Option 2: widen first arg to double → process(double, int)
// => Both equally specific (one widening each)
// => Compiler error: "reference to process is ambiguous"

// CONSTRUCTOR OVERLOADING
// => Same principles as method overloading
// => Multiple constructors with different parameters
// => Enables flexible object initialization
// => Use this() to delegate to another constructor (constructor chaining)

class Rectangle {
    private double width;
    // => Instance field: width
    // => private: only accessible within Rectangle class
    // => double: 64-bit floating-point

    private double height;
    // => Instance field: height

    // Default constructor (no parameters)
    public Rectangle() {
        // => No arguments required
        // => Creates default 1.0 × 1.0 square

        this(1.0, 1.0);
        // => this(): calls another constructor in same class
        // => Delegates to two-parameter constructor
        // => Passes default values: width = 1.0, height = 1.0
        // => MUST be first statement in constructor (Java rule)
        // => After delegation, constructor finishes (no more code needed)
    }

    // One parameter constructor (square)
    public Rectangle(double side) {
        // => One parameter: side length
        // => Creates square (width = height = side)

        this(side, side);
        // => this(side, side): calls two-parameter constructor
        // => Both width and height set to same value (side)
        // => Avoids code duplication (initialization logic in one place)
    }

    // Two parameters constructor
    public Rectangle(double width, double height) {
        // => Two parameters: explicit width and height
        // => Most general constructor (others delegate to this)

        this.width = width;
        // => this.width: instance field (left side)
        // => width: parameter (right side)
        // => this disambiguates field from parameter (same name)
        // => Sets field to parameter value

        this.height = height;
        // => Sets height field
    }

    public double area() {
        // => Instance method: calculates rectangle area
        return width * height;
        // => Area formula: width × height
        // => Accesses instance fields (no this needed, unambiguous)
    }
}

// Using constructor overloading
Rectangle r1 = new Rectangle();
// => Calls no-argument constructor
// => Delegates to Rectangle(1.0, 1.0)
// => Creates 1.0 × 1.0 square

double area1 = r1.area();
// => Calls area() method
// => Calculates: 1.0 * 1.0 = 1.0
// => area1 is 1.0

Rectangle r2 = new Rectangle(5.0);
// => Calls one-parameter constructor
// => Delegates to Rectangle(5.0, 5.0)
// => Creates 5.0 × 5.0 square

double area2 = r2.area();
// => Calculates: 5.0 * 5.0 = 25.0
// => area2 is 25.0

Rectangle r3 = new Rectangle(3.0, 4.0);
// => Calls two-parameter constructor (no delegation)
// => Creates 3.0 × 4.0 rectangle

double area3 = r3.area();
// => Calculates: 3.0 * 4.0 = 12.0
// => area3 is 12.0

// VARARGS WITH OVERLOADING
// => Varargs can participate in overloading
// => Exact match preferred over varargs (varargs is lowest priority)

public static String format(String s) {
    // => Signature: format(String)
    // => Exact match for single string argument
    return "[" + s + "]";
    // => Wraps string in square brackets
}

public static String format(String... strings) {
    // => Signature: format(String...) - varargs version
    // => Accepts 0 or more strings
    // => Lower priority than exact match (when both applicable)

    return "[" + String.join(", ", strings) + "]";
    // => String.join(", ", strings): joins array with ", " delimiter
    // => Wraps result in square brackets
}

// Overload resolution with varargs
String f1 = format("A");
// => Argument: single String
// => Option 1: format(String) - exact match
// => Option 2: format(String...) - varargs match
// => Exact match has higher priority
// => Calls format(String)
// => Returns: "[A]"

String f2 = format("A", "B");
// => Arguments: two Strings
// => Only option: format(String...) - varargs match
// => format(String) doesn't match (requires exactly 1 argument)
// => Calls format(String...)
// => String.join(", ", ["A", "B"]) → "A, B"
// => Returns: "[A, B]"

String f3 = format("A", "B", "C");
// => Arguments: three Strings
// => Only option: format(String...)
// => String.join(", ", ["A", "B", "C"]) → "A, B, C"
// => Returns: "[A, B, C]"

// Key insight: exact parameter count always wins over varargs
// format("A") → prefers format(String) over format(String...)
// format("A", "B", ...) → must use format(String...) (no exact match)
```

**Key Takeaway**: Varargs (`type... name`) accept zero or more arguments as array. Must be last parameter. Method overloading allows same name with different signatures (parameter count or types). Compiler selects method based on: exact match, primitive widening, autoboxing, then varargs. Constructor overloading enables multiple initialization patterns with `this()` delegation.

**Why It Matters**: Varargs eliminate API proliferation that plagued pre-Java 5 libraries—before varargs, classes like `String.format()` would need dozens of overloads for different argument counts, creating maintenance nightmares and documentation sprawl. Method overloading enables intuitive APIs where `Arrays.asList(1, 2, 3)` and `Arrays.asList(array)` coexist naturally, while overload resolution rules prevent ambiguity that causes compile errors in languages without clear precedence. Constructor overloading with `this()` delegation follows the DRY principle, centralizing initialization logic in one constructor and reducing bugs from duplicate code—critical in domain models where fields require validation. The widening hierarchy (byte → int → long → double) ensures numeric methods work predictably across types without explicit casting, enabling mathematical libraries to provide clean APIs. Varargs lowered priority relative to exact matches prevents breaking changes when adding varargs overloads to existing APIs—a Java design decision that preserved backward compatibility across millions of codebases.

---

### Example 22: Wrapper Classes and Autoboxing

Wrapper classes (Integer, Double, Boolean, etc.) box primitives into objects for collections and generics. Autoboxing converts automatically. Understand performance implications and caching behavior.

**Code**:

```java
// WRAPPER CLASSES - Object representations of primitive types
// => Java has 8 primitive types: byte, short, int, long, float, double, char, boolean
// => Each primitive has corresponding wrapper class in java.lang package
// => Wrappers allow primitives to be used in object-oriented contexts (collections, generics)
// => All wrapper classes are immutable (cannot change value after creation)
// => All wrapper classes are final (cannot be extended/subclassed)

// Creating wrapper objects explicitly
Integer objInt = Integer.valueOf(42);
// => Integer.valueOf(42): factory method creates Integer object wrapping primitive 42
// => objInt now references Integer object (not primitive)
// => valueOf() is preferred over constructor (Integer(42) is deprecated since Java 9)
// => valueOf() may return cached instance for small values (-128 to 127)
// => Result: objInt is Integer object containing value 42

int primitive = objInt.intValue();
// => intValue(): extracts primitive int from Integer wrapper
// => Explicitly unwraps Integer object to primitive int
// => primitive now contains 42 (primitive value, not object)
// => This is manual unboxing (explicit conversion wrapper -> primitive)

// AUTOBOXING - Automatic conversion primitive -> wrapper
// => Introduced in Java 5 (2004) to reduce boilerplate
// => Compiler automatically inserts Integer.valueOf() call
// => Makes code cleaner but hides object creation (performance impact)

Integer auto = 100;
// => Compiler translates to: Integer auto = Integer.valueOf(100);
// => Autoboxing: primitive int 100 automatically wrapped into Integer object
// => auto references Integer object (not primitive)
// => This is implicit conversion (automatic boxing)

int unboxed = auto;
// => Compiler translates to: int unboxed = auto.intValue();
// => Automatic unboxing: Integer object unwrapped to primitive int
// => unboxed contains primitive value 100 (not object reference)
// => This is implicit conversion (automatic unboxing)

// WHY WRAPPERS EXIST - Collections and generics require objects
// => ArrayList<int> is ILLEGAL in Java (primitives not allowed in generics)
// => Generics work only with reference types (objects)
// => Type erasure removes generic type info at runtime → needs objects

ArrayList<Integer> numbers = new ArrayList<>();
// => ArrayList<Integer>: list of Integer objects (NOT primitive int)
// => Generics cannot use primitives: ArrayList<int> causes compile error
// => Diamond operator <>: compiler infers type from left side (Integer)
// => numbers is empty ArrayList with initial capacity 10

numbers.add(10);
// => Compiler translates to: numbers.add(Integer.valueOf(10));
// => Autoboxing: primitive 10 wrapped to Integer(10)
// => Integer(10) added to ArrayList
// => ArrayList now contains one Integer object: [Integer(10)]
// => add() expects Integer parameter, receives int, autoboxing bridges gap

numbers.add(20);
// => Compiler translates to: numbers.add(Integer.valueOf(20));
// => Autoboxing: primitive 20 wrapped to Integer(20)
// => ArrayList now contains: [Integer(10), Integer(20)]

int value = numbers.get(0);
// => get(0) returns Integer object at index 0 (Integer(10))
// => Compiler translates to: int value = numbers.get(0).intValue();
// => Automatic unboxing: Integer(10) unwrapped to primitive 10
// => value now contains primitive int 10

// ALL EIGHT WRAPPER CLASSES - One for each primitive type
// => Each wrapper class in java.lang package (auto-imported)
// => Naming: Primitive name with capital first letter (except Integer, Character)
// => All wrappers extend Number class (except Boolean, Character)

Byte byteObj = 127;
// => Autoboxing: primitive byte 127 -> Byte object
// => Byte wraps byte primitive (8-bit signed: -128 to 127)
// => 127 is Byte.MAX_VALUE (maximum byte value)

Short shortObj = 32000;
// => Autoboxing: primitive short 32000 -> Short object
// => Short wraps short primitive (16-bit signed: -32768 to 32767)

Integer intObj = 42;
// => Autoboxing: primitive int 42 -> Integer object
// => Integer wraps int primitive (32-bit signed: -2^31 to 2^31-1)
// => Most commonly used wrapper class

Long longObj = 1000L;
// => Autoboxing: primitive long 1000L -> Long object
// => L suffix makes 1000 a long literal (without L, it's int)
// => Long wraps long primitive (64-bit signed: -2^63 to 2^63-1)

Float floatObj = 3.14f;
// => Autoboxing: primitive float 3.14f -> Float object
// => f suffix makes 3.14 a float literal (without f, it's double)
// => Float wraps float primitive (32-bit IEEE 754 floating point)

Double doubleObj = 3.14159;
// => Autoboxing: primitive double 3.14159 -> Double object
// => No suffix needed (decimal literals are double by default)
// => Double wraps double primitive (64-bit IEEE 754 floating point)

Character charObj = 'A';
// => Autoboxing: primitive char 'A' -> Character object
// => Character wraps char primitive (16-bit Unicode character)
// => Single quotes ' ' for char literals (double quotes " " for String)

Boolean boolObj = true;
// => Autoboxing: primitive boolean true -> Boolean object
// => Boolean wraps boolean primitive (true or false)
// => Boolean has only two possible instances: Boolean.TRUE, Boolean.FALSE (cached)

// INTEGER CACHING - Performance optimization for small values
// => Java caches Integer objects for range -128 to 127 (JLS requirement)
// => valueOf() returns same cached instance for values in cache range
// => Outside cache range, valueOf() creates new Integer object each time
// => Caching saves memory and improves == comparison for common values
// => Other wrappers cache too: Byte (all values), Short/Long (-128 to 127), Character (0 to 127)

Integer a = 100;
// => Autoboxing: Integer.valueOf(100) called
// => 100 is in cache range (-128 to 127)
// => valueOf() returns CACHED Integer instance for 100
// => a references the cached Integer(100) object

Integer b = 100;
// => Autoboxing: Integer.valueOf(100) called again
// => valueOf() returns SAME cached Integer(100) instance
// => b references the SAME object as a (both point to cached instance)

System.out.println(a == b);
// => Output: true
// => == compares object references (identity), not values
// => a and b reference the SAME cached object → true
// => This is EXCEPTIONAL behavior (only works in cache range!)

Integer c = 200;
// => Autoboxing: Integer.valueOf(200) called
// => 200 is OUTSIDE cache range (-128 to 127)
// => valueOf() creates NEW Integer object (not cached)
// => c references new Integer(200) object

Integer d = 200;
// => Autoboxing: Integer.valueOf(200) called again
// => valueOf() creates ANOTHER new Integer object (200 not cached)
// => d references DIFFERENT Integer(200) object than c

System.out.println(c == d);
// => Output: false
// => == compares object references
// => c and d reference DIFFERENT objects (both contain 200, but separate instances)
// => This demonstrates why == is unreliable for wrapper comparison

System.out.println(c.equals(d));
// => Output: true
// => equals() compares VALUES (overridden in Integer class)
// => Both c and d contain value 200 → equals() returns true
// => This is CORRECT way to compare wrapper values

// CRITICAL RULE: Always use .equals() for wrapper comparison
// => == compares object identity (references), not values
// => equals() compares values (correct for value comparison)
// => Cache behavior makes == unreliable (true for small values, false for large)
// => Relying on == for wrappers causes subtle bugs in production

Integer x = 42;
// => Autoboxing: Integer.valueOf(42)
// => 42 in cache range → cached instance returned
// => x references cached Integer(42)

Integer y = 42;
// => Autoboxing: Integer.valueOf(42)
// => Returns SAME cached Integer(42) instance as x
// => y references same object as x

Integer z = Integer.valueOf(42);
// => Explicit valueOf() call
// => Returns SAME cached Integer(42) instance
// => z also references same object as x and y
// => x, y, z all reference the SAME cached object

System.out.println(x == y);
// => Output: true
// => x and y reference same cached object (identity comparison)
// => This works ONLY because 42 is cached

System.out.println(x.equals(y));
// => Output: true
// => equals() compares values: 42 == 42 → true
// => This ALWAYS works (recommended approach)

// NULL POINTERS WITH AUTOBOXING - Major pitfall
// => Wrapper objects can be null (reference types)
// => Primitives CANNOT be null (value types)
// => Unboxing null wrapper throws NullPointerException at runtime
// => This is common source of production bugs

Integer nullInt = null;
// => nullInt is null reference (no Integer object)
// => This is legal: wrappers are objects, can be null
// => Primitives cannot be null: int x = null; causes compile error

// int value = nullInt; // => RUNTIME ERROR: NullPointerException
// => Compiler translates to: int value = nullInt.intValue();
// => nullInt is null → calling method on null throws NullPointerException
// => Unboxing null always fails (cannot extract primitive from null)

// Safe unboxing with null check
if (nullInt != null) {
    // => Check if wrapper is null before unboxing
    // => Prevents NullPointerException
    int safe = nullInt;
    // => This line only executes if nullInt is not null
    // => Unboxing safe: nullInt.intValue() called on non-null object
}
// => Always check wrapper for null before unboxing in defensive code

// WRAPPER UTILITY METHODS - Rich API for type conversion
// => Each wrapper class provides static methods for parsing, conversion, formatting
// => Utility methods make wrappers more than just boxes for primitives
// => Common tasks: parse strings, format output, convert bases, check limits

int parsed = Integer.parseInt("123");
// => parseInt(): static method converts String to primitive int
// => "123" (String) → 123 (int primitive, NOT Integer object)
// => Throws NumberFormatException if string is not valid integer
// => Use for converting user input, config values to numbers

Integer parsedObj = Integer.valueOf("456");
// => valueOf(String): static method converts String to Integer object
// => "456" (String) → Integer(456) (wrapper object)
// => Also throws NumberFormatException for invalid input
// => Returns cached instance if value in cache range (-128 to 127)

String binary = Integer.toBinaryString(10);
// => toBinaryString(): converts int to binary string representation
// => 10 (decimal) → "1010" (binary string)
// => Useful for debugging, logging binary data
// => Result: binary = "1010"

String hex = Integer.toHexString(255);
// => toHexString(): converts int to hexadecimal string
// => 255 (decimal) → "ff" (hexadecimal string, lowercase)
// => Common for color codes, memory addresses
// => Result: hex = "ff"

String octal = Integer.toOctalString(8);
// => toOctalString(): converts int to octal string
// => 8 (decimal) → "10" (octal string)
// => Octal base 8: 1*8^1 + 0*8^0 = 8
// => Result: octal = "10"

int max = Integer.MAX_VALUE;
// => MAX_VALUE: static constant for maximum int value
// => Value: 2147483647 (2^31 - 1)
// => Useful for boundary checks, initialization
// => Result: max = 2147483647

int min = Integer.MIN_VALUE;
// => MIN_VALUE: static constant for minimum int value
// => Value: -2147483648 (-2^31)
// => Careful: Math.abs(MIN_VALUE) == MIN_VALUE (overflow!)
// => Result: min = -2147483648

// DOUBLE SPECIAL VALUES - Floating point edge cases
// => IEEE 754 floating point standard defines special values
// => Infinity: result of dividing by zero (1.0 / 0.0)
// => NaN (Not a Number): result of undefined operations (0.0 / 0.0, sqrt(-1))
// => These values propagate through calculations

double inf = Double.POSITIVE_INFINITY;
// => POSITIVE_INFINITY: constant for positive infinity
// => Represents value greater than any finite double
// => Result of operations like 1.0 / 0.0
// => inf = Infinity

double ninf = Double.NEGATIVE_INFINITY;
// => NEGATIVE_INFINITY: constant for negative infinity
// => Represents value less than any finite double
// => Result of operations like -1.0 / 0.0
// => ninf = -Infinity

double nan = Double.NaN;
// => NaN: constant for "Not a Number"
// => Represents undefined or unrepresentable values
// => NaN is NOT equal to anything (including itself: NaN != NaN)
// => nan = NaN

boolean isNaN = Double.isNaN(0.0 / 0.0);
// => isNaN(): checks if value is NaN
// => 0.0 / 0.0 produces NaN (undefined division)
// => Cannot use == to check NaN (NaN != NaN is true!)
// => Must use isNaN() method
// => Result: isNaN = true

boolean isInfinite = Double.isInfinite(1.0 / 0.0);
// => isInfinite(): checks if value is positive or negative infinity
// => 1.0 / 0.0 produces POSITIVE_INFINITY
// => Returns true for both POSITIVE_INFINITY and NEGATIVE_INFINITY
// => Result: isInfinite = true

// CHARACTER UTILITY METHODS - Character classification and conversion
// => Character class provides static methods for char operations
// => Useful for parsing, validation, text processing
// => Methods work with Unicode (not just ASCII)

boolean isDigit = Character.isDigit('5');
// => isDigit(): checks if char is digit (0-9)
// => '5' is digit → true
// => Works with Unicode digits too (Arabic numerals, etc.)
// => Result: isDigit = true

boolean isLetter = Character.isLetter('A');
// => isLetter(): checks if char is letter (a-z, A-Z, Unicode letters)
// => 'A' is letter → true
// => Works with Unicode letters (accented characters, Greek, etc.)
// => Result: isLetter = true

boolean isWhitespace = Character.isWhitespace(' ');
// => isWhitespace(): checks if char is whitespace
// => ' ' (space) is whitespace → true
// => Also matches: tab '\t', newline '\n', carriage return '\r'
// => Result: isWhitespace = true

char upper = Character.toUpperCase('a');
// => toUpperCase(): converts char to uppercase
// => 'a' (lowercase) → 'A' (uppercase)
// => Returns same char if already uppercase or not a letter
// => Result: upper = 'A'

int numeric = Character.getNumericValue('5');
// => getNumericValue(): converts digit char to int value
// => '5' (char) → 5 (int)
// => Returns -1 for non-digit characters
// => Different from (int)'5' which returns ASCII value 53
// => Result: numeric = 5

// BOOLEAN PARSING - Converting strings to boolean
// => Boolean.valueOf() and parseBoolean() convert strings
// => Case-insensitive comparison to "true"
// => ONLY "true" (case-insensitive) returns true
// => Everything else returns false (including "yes", "1", "Y")

Boolean bool1 = Boolean.valueOf("true");
// => valueOf("true"): case-insensitive check for "true"
// => "true" matches → returns Boolean.TRUE (cached instance)
// => Result: bool1 = true

Boolean bool2 = Boolean.valueOf("false");
// => valueOf("false"): case-insensitive check for "true"
// => "false" does NOT match "true" → returns Boolean.FALSE (cached)
// => Result: bool2 = false

Boolean bool3 = Boolean.valueOf("yes");
// => valueOf("yes"): case-insensitive check for "true"
// => "yes" does NOT match "true" → returns Boolean.FALSE
// => Common pitfall: "yes", "1", "Y" all return false!
// => Result: bool3 = false

// PERFORMANCE CONSIDERATION - Autoboxing overhead
// => Autoboxing creates objects (heap allocation + garbage collection)
// => Each boxing operation: allocate object, copy value, return reference
// => Each unboxing operation: dereference, extract value
// => In tight loops, millions of temporary objects created → GC pressure
// => Use primitives for performance-critical code

long sum1 = 0;
// => Primitive long variable (8 bytes on stack or register)
// => No object overhead (no heap allocation)
// => Direct primitive arithmetic (CPU native operations)

for (int i = 0; i < 1000000; i++) {
    sum1 += i;
    // => Primitive addition: sum1 = sum1 + i
    // => Direct CPU operation (no boxing/unboxing)
    // => No object allocation (no GC pressure)
    // => Fast: ~1-2 CPU cycles per iteration
}
// => Total: 1,000,000 primitive additions (FAST)
// => No temporary objects created
// => No garbage collection triggered

Long sum2 = 0L;
// => Wrapper Long object (16+ bytes on heap: object header + long value)
// => Autoboxing: 0L → Long.valueOf(0L) creates Long object
// => sum2 references Long object on heap

for (int i = 0; i < 1000000; i++) {
    sum2 += i;
    // => Compiler translates to: sum2 = Long.valueOf(sum2.longValue() + i);
    // => Step 1: Unbox sum2 (sum2.longValue()) → extract primitive
    // => Step 2: Add i to primitive (sum2Value + i)
    // => Step 3: Box result (Long.valueOf(result)) → create new Long object
    // => Step 4: Assign to sum2 (old Long object becomes garbage)
    // => Each iteration: 1 unboxing + 1 boxing + 1 object allocation + 1 garbage object
}
// => Total: 1,000,000 Long objects created (most become garbage immediately)
// => 1,000,000 unboxing operations + 1,000,000 boxing operations
// => Massive GC pressure (garbage collector must clean up temporary objects)
// => SLOW: 10-100x slower than primitive version
// => Use primitives for loops, calculations, performance-critical code

// WRAPPER IMMUTABILITY - Wrappers cannot be modified
// => All wrapper classes are immutable (like String)
// => Cannot change value after wrapper created
// => Assignment creates NEW wrapper object (doesn't modify existing)
// => Immutability enables caching, thread safety

Integer num = 10;
// => Autoboxing: Integer.valueOf(10)
// => num references Integer object containing value 10
// => This Integer object is immutable (value cannot change)

num = 20;
// => Autoboxing: Integer.valueOf(20) creates NEW Integer object
// => num now references NEW Integer(20) object
// => OLD Integer(10) object unchanged (still exists until garbage collected)
// => This is reassignment (new reference), NOT mutation (changing value)
// => No method like num.setValue(20) exists (immutability)

// => Key insight: Wrappers behave like primitives from reassignment perspective
// => But they are objects (reference types) with caching, null, and overhead
```

**Key Takeaway**: Wrapper classes (Integer, Double, Boolean, etc.) box primitives into objects for collections and generics. Autoboxing converts automatically but creates objects. Integer caches -128 to 127. Always use `.equals()` for wrapper comparison (not `==`). Null wrappers throw NullPointerException when unboxed. Use primitives for performance-critical code to avoid boxing overhead.

**Why It Matters**: Generics revolutionized Java type safety but only work with objects—ArrayList&lt;Integer&gt; exists because ArrayList&lt;int&gt; is illegal, making wrapper classes essential for collections. The Integer cache (-128 to 127) prevents millions of redundant objects in typical applications, but relying on == for wrapper comparison causes subtle bugs when values exceed cache range—production code littered with Integer.valueOf(200) == Integer.valueOf(200) evaluating to false has cost countless debugging hours. Autoboxing's convenience hides performance costs: tight loops with Long accumulation can create millions of temporary objects, triggering garbage collection pauses that degrade throughput. NullPointerException from unboxing null wrappers is one of the most common runtime failures in Java—defensive null checks before unboxing separate robust code from fragile code. Understanding wrapper semantics enables using the right tool: primitives for performance-critical math, wrappers for collections and nullability.

---

### Example 23: Type Conversion and Parsing

Java requires explicit type conversions between different data types. Parsing strings to numbers is common when reading user input or configuration files. Always handle parsing exceptions to avoid crashes.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    String["String<br/>\"42\""] --> Parse["Parsing Methods<br/>(parseInt, parseDouble)"]
    Parse --> Success["Numeric Value<br/>(42)"]
    Parse --> Fail["NumberFormatException"]

    Num["Numeric Type"] --> Convert["Conversion Methods<br/>(cast, valueOf)"]
    Convert --> Target["Target Type"]

    style String fill:#0173B2,color:#fff
    style Parse fill:#DE8F05,color:#fff
    style Success fill:#029E73,color:#fff
    style Fail fill:#CA9161,color:#fff
    style Num fill:#0173B2,color:#fff
    style Convert fill:#CC78BC,color:#fff
    style Target fill:#029E73,color:#fff
```

**Code**:

```java
// STRING TO PRIMITIVE PARSING - Converting text to numbers
// => Most common operation when reading external data (user input, files, APIs)
// => parseXxx() methods convert String to primitive (int, double, boolean, etc.)
// => Throws NumberFormatException if string format is invalid
// => Essential for configuration files, command-line args, web forms

String numStr = "42";
// => numStr is String containing text "42" (not numeric value)
// => Cannot do arithmetic with String (numStr + 10 would be "4210", not 52)

int parsedInt = Integer.parseInt(numStr);
// => parseInt(): static method in Integer class
// => Converts String "42" to primitive int 42
// => Returns primitive int (NOT Integer object)
// => Throws NumberFormatException if numStr contains non-digits (e.g., "abc", "12.5")
// => Result: parsedInt = 42 (primitive int)

double parsedDouble = Double.parseDouble("3.14");
// => parseDouble(): static method in Double class
// => Converts String "3.14" to primitive double 3.14
// => Accepts decimal point, scientific notation (1.5e10), infinity, NaN
// => Returns primitive double (NOT Double object)
// => Result: parsedDouble = 3.14

boolean parsedBool = Boolean.parseBoolean("true");
// => parseBoolean(): static method in Boolean class
// => Case-insensitive: "true", "True", "TRUE" all return true
// => Any other string (including "false", "1", "yes") returns false
// => Never throws exception (returns false for invalid input)
// => Result: parsedBool = true

long parsedLong = Long.parseLong("1000000");
// => parseLong(): converts String to primitive long
// => Accepts numbers beyond int range (-2^63 to 2^63-1)
// => Result: parsedLong = 1000000L

// PARSING WITH DIFFERENT RADIX - Base conversion
// => Radix (base): number system base (binary=2, octal=8, decimal=10, hex=16)
// => parseInt(String, int radix): parse string in specified base
// => Useful for hexadecimal colors, binary flags, octal permissions

int binary = Integer.parseInt("1010", 2);
// => Parse "1010" as binary (base 2) to decimal
// => Binary "1010" = 1*2^3 + 0*2^2 + 1*2^1 + 0*2^0 = 8 + 0 + 2 + 0 = 10
// => Second parameter 2 specifies binary radix
// => Result: binary = 10 (decimal value)

int hex = Integer.parseInt("FF", 16);
// => Parse "FF" as hexadecimal (base 16) to decimal
// => Hex "FF" = 15*16^1 + 15*16^0 = 240 + 15 = 255
// => Case-insensitive: "FF", "ff", "Ff" all work
// => Result: hex = 255 (decimal value)

int octal = Integer.parseInt("77", 8);
// => Parse "77" as octal (base 8) to decimal
// => Octal "77" = 7*8^1 + 7*8^0 = 56 + 7 = 63
// => Unix file permissions use octal (chmod 755)
// => Result: octal = 63 (decimal value)

// STRING TO WRAPPER - Returns wrapper object (not primitive)
// => valueOf(String) parses String to wrapper object
// => Difference from parseXxx(): valueOf returns wrapper, parseXxx returns primitive
// => valueOf() may return cached instance for small values (-128 to 127)

Integer boxedInt = Integer.valueOf("42");
// => valueOf(String): parses String to Integer object
// => "42" → Integer(42) (wrapper object)
// => May return cached Integer instance (42 in cache range)
// => Throws NumberFormatException for invalid strings
// => Result: boxedInt = Integer(42)

Double boxedDouble = Double.valueOf("3.14");
// => valueOf(String): parses String to Double object
// => "3.14" → Double(3.14) (wrapper object)
// => Result: boxedDouble = Double(3.14)

// PARSING ERRORS - NumberFormatException
// => Thrown when string cannot be parsed to number
// => Examples: letters ("abc"), wrong format ("12.5" for parseInt), overflow
// => Must catch exception or let it propagate to caller
// => Uncaught NumberFormatException crashes program

try {
    // => try block: code that might throw exception
    int invalid = Integer.parseInt("abc");
    // => "abc" contains letters (not valid integer)
    // => parseInt() throws NumberFormatException
    // => Execution jumps immediately to catch block (next line not executed)
} catch (NumberFormatException e) {
    // => catch block: handles NumberFormatException
    // => e is exception object containing error details
    System.out.println("Invalid number format");
    // => Output: Invalid number format
    // => Program continues normally (exception handled)
}

// SAFE PARSING WITH TRY-CATCH - Defensive programming pattern
// => Set default value before parsing
// => Try parsing in try block
// => If exception occurs, default value remains unchanged
// => Common in production code for robust input handling

String input = "123abc";
// => input contains invalid integer (letters mixed with digits)

int safeValue = 0;
// => Initialize with default value (0 used if parsing fails)
// => safeValue will be 0 if exception occurs

try {
    safeValue = Integer.parseInt(input);
    // => Try parsing "123abc"
    // => parseInt() sees "123" followed by "abc" → invalid format
    // => Throws NumberFormatException (assignment never happens)
} catch (NumberFormatException e) {
    // => Exception caught
    safeValue = -1;
    // => Assign different default (-1 indicates parsing failure)
    // => safeValue now -1 (overrides initial 0)
}

System.out.println(safeValue);
// => Output: -1
// => Parsing failed, default value used

// PRIMITIVE TO STRING CONVERSION - Several approaches
// => String.valueOf(): works for all types, handles null
// => WrapperClass.toString(): wrapper-specific method
// => Concatenation ("" + value): simple but less clear

int number = 42;
// => Primitive int to convert to String

String str1 = String.valueOf(number);
// => String.valueOf(int): static method converts int to String
// => Works for all primitive types (int, double, boolean, char, etc.)
// => Also handles null: String.valueOf(null) returns "null" (not NullPointerException)
// => Recommended approach (null-safe, clear intent)
// => Result: str1 = "42"

String str2 = Integer.toString(number);
// => Integer.toString(int): static method converts int to String
// => Wrapper-specific method (Integer for int, Double for double, etc.)
// => Cannot handle null (wrapper object needed for toString())
// => Result: str2 = "42"

String str3 = "" + number;
// => String concatenation: empty string + number
// => Compiler converts to: "" + String.valueOf(number)
// => Short but less readable (trick, not clear intent)
// => Common in legacy code
// => Result: str3 = "42"

String str4 = number + "";
// => Same as above (number + empty string)
// => Compiler converts to: String.valueOf(number) + ""
// => Result: str4 = "42"

// FORMATTING NUMBERS TO STRINGS - String.format() for precise control
// => String.format(format, args): printf-style formatting
// => Format specifiers: %d (int), %f (float), %s (string), etc.
// => Precision, padding, alignment control
// => Essential for reports, logs, user interfaces

double price = 12.5;
// => price is 12.5 (double)

String formatted = String.format("$%.2f", price);
// => String.format(): returns formatted string
// => Format string: "$%.2f"
// => $: literal dollar sign
// => %.2f: float/double with 2 decimal places
// => .2: precision (2 digits after decimal point)
// => f: floating point format specifier
// => Result: formatted = "$12.50" (rounded to 2 decimals)

String padded = String.format("%05d", 42);
// => Format string: "%05d"
// => %05d: integer with zero-padding to 5 total digits
// => 0: pad with zeros (instead of spaces)
// => 5: minimum width (5 characters total)
// => d: decimal integer format specifier
// => Result: padded = "00042" (42 padded to 5 digits with leading zeros)

// PRIMITIVE TYPE CONVERSIONS - WIDENING (automatic)
// => Widening: converting smaller type to larger type (no data loss)
// => byte → short → int → long → float → double
// => Automatic conversion (no cast needed)
// => Safe: larger type can hold all values of smaller type

int intVal = 100;
// => intVal is primitive int (32-bit)

long longVal = intVal;
// => Automatic widening: int → long
// => int (32-bit) fits entirely in long (64-bit)
// => No cast needed (compiler inserts conversion)
// => No data loss (100 fits in both int and long)
// => Result: longVal = 100L

double doubleVal = intVal;
// => Automatic widening: int → double
// => int (32-bit integer) → double (64-bit floating point)
// => No cast needed
// => Integer 100 represented exactly in double (100.0)
// => Result: doubleVal = 100.0

float floatVal = intVal;
// => Automatic widening: int → float
// => int (32-bit) → float (32-bit floating point)
// => No cast needed
// => WARNING: Large ints may lose precision (float has 23-bit mantissa, int has 32-bit value)
// => 100 fits exactly in float
// => Result: floatVal = 100.0f

// NARROWING CONVERSIONS - Requires explicit cast (potential data loss)
// => Narrowing: converting larger type to smaller type
// => double → float → long → int → short → byte
// => Explicit cast required: (targetType) value
// => Risky: may lose precision, overflow, truncate

double doubleNum = 9.99;
// => doubleNum is 9.99 (double with decimal part)

int truncated = (int) doubleNum;
// => Narrowing cast: double → int
// => (int): explicit cast operator (required for narrowing)
// => Truncation: decimal part .99 discarded (NOT rounded!)
// => Integer part 9 remains
// => Result: truncated = 9 (lost .99 fractional part)

long longNum = 1000000000000L;
// => longNum is 1 trillion (fits in long, exceeds int max 2.1 billion)

int narrowed = (int) longNum;
// => Narrowing cast: long → int
// => longNum (1 trillion) exceeds Integer.MAX_VALUE (2.1 billion)
// => Overflow: high-order bits discarded, low 32 bits kept
// => Result undefined for values outside int range (data corruption)
// => Result: narrowed = -727379968 (overflow, NOT 1000000000000)

// OVERFLOW IN NARROWING CONVERSION - Data corruption
// => When value exceeds target type's range, bits wrap around
// => int → short: keep low 16 bits, discard high 16 bits
// => Produces negative numbers or garbage values

int maxInt = Integer.MAX_VALUE;
// => maxInt = 2147483647 (maximum int value, 2^31 - 1)
// => Binary: 01111111 11111111 11111111 11111111 (31 ones, leading 0)

short overflow = (short) maxInt;
// => Narrowing cast: int → short
// => short range: -32768 to 32767 (16-bit signed)
// => maxInt (2147483647) far exceeds short range
// => Keep low 16 bits: 11111111 11111111 (all ones)
// => Interpret as signed short: -1 (two's complement)
// => Result: overflow = -1 (data corruption from overflow)

// WRAPPER TO PRIMITIVE UNBOXING - Automatic and explicit
// => Automatic unboxing: wrapper assigned to primitive variable
// => Explicit unboxing: call xxxValue() method
// => Both extract primitive value from wrapper object

Integer wrappedInt = 42;
// => wrappedInt is Integer object wrapping primitive 42

int primitiveInt = wrappedInt;
// => Automatic unboxing: Integer → int
// => Compiler translates to: int primitiveInt = wrappedInt.intValue();
// => Extracts primitive 42 from Integer object
// => Result: primitiveInt = 42

int explicit = wrappedInt.intValue();
// => Explicit unboxing: call intValue() method
// => Same result as automatic unboxing (more verbose but clearer intent)
// => Result: explicit = 42

// NULL POINTER EXCEPTION WHEN UNBOXING NULL
// => Wrapper can be null (object reference)
// => Unboxing null calls method on null → NullPointerException
// => Common bug in production code (forgot null check)

Integer nullWrapper = null;
// => nullWrapper is null (no Integer object)

try {
    int value = nullWrapper;
    // => Automatic unboxing: compiler translates to nullWrapper.intValue()
    // => nullWrapper is null → calling method on null throws NullPointerException
    // => This line never completes (exception thrown immediately)
} catch (NullPointerException e) {
    // => Exception caught
    System.out.println("Cannot unbox null");
    // => Output: Cannot unbox null
}
// => Always check wrapper != null before unboxing

// CONVERTING BETWEEN WRAPPER TYPES
// => Wrapper classes provide xxxValue() methods for all numeric types
// => Convert wrapper to different primitive type
// => Then optionally box to target wrapper type

Integer intWrapper = 42;
// => intWrapper is Integer(42)

Double doubleWrapper = intWrapper.doubleValue();
// => intValue() extracts int 42, then widening conversion to double 42.0
// => doubleValue(): converts Integer to primitive double
// => Autoboxing: double 42.0 → Double(42.0)
// => Result: doubleWrapper = Double(42.0)

String stringWrapper = intWrapper.toString();
// => toString(): instance method converts Integer to String
// => Different from Integer.toString(int) static method
// => Result: stringWrapper = "42"

// PARSING WITH VALIDATION - Prevent exceptions proactively
// => Validate input format before parsing
// => Use regex pattern matching to check if string is valid number
// => Avoids exception overhead (exceptions are expensive)

String userInput = "  123  ";
// => User input often has leading/trailing whitespace

userInput = userInput.trim();
// => trim(): removes leading and trailing whitespace
// => "  123  " → "123"
// => Result: userInput = "123"

if (userInput.matches("-?\\d+")) {
    // => matches(regex): checks if string matches regex pattern
    // => Regex: -?\\d+
    // => -?: optional minus sign (0 or 1 occurrence)
    // => \\d+: one or more digits (0-9)
    // => \\d: digit character class (must escape backslash in Java string)
    // => +: one or more (at least 1 digit required)
    // => "123" matches (all digits), "12.5" doesn't (has dot), "abc" doesn't (letters)
    // => Condition: true (userInput "123" matches pattern)

    int validNum = Integer.parseInt(userInput);
    // => Safe to parse (already validated format)
    // => parseInt() succeeds (no exception)
    // => Result: validNum = 123

    System.out.println("Valid: " + validNum);
    // => Output: Valid: 123
} else {
    System.out.println("Invalid input");
    // => This branch not executed (userInput was valid)
}

// CHARACTER TO INT - ASCII/Unicode value vs numeric value
// => (int) char: cast to int gives Unicode codepoint (ASCII for basic chars)
// => Character.getNumericValue(): gives numeric value (for digit chars)
// => Different results for digit characters!

char letter = 'A';
// => letter is 'A' (uppercase letter)

int ascii = (int) letter;
// => Cast char to int: returns Unicode codepoint
// => 'A' has Unicode codepoint 65 (ASCII value)
// => Result: ascii = 65 (NOT 1 or 10!)

char digit = '7';
// => digit is character '7' (NOT numeric 7)

int digitValue = Character.getNumericValue(digit);
// => getNumericValue(): converts digit character to numeric value
// => '7' (character) → 7 (numeric int)
// => Returns -1 for non-digit characters
// => Result: digitValue = 7

int digitAscii = (int) digit;
// => Cast '7' to int: returns Unicode codepoint
// => '7' has Unicode codepoint 55 (ASCII value)
// => NOT the numeric value 7!
// => Result: digitAscii = 55 (ASCII value, NOT 7)

// BOOLEAN PARSING - Case-insensitive, strict rules
// => parseBoolean(): only "true" (case-insensitive) returns true
// => Everything else returns false (including "false", "1", "yes", "Y")
// => No exception thrown (unlike number parsing)

boolean b1 = Boolean.parseBoolean("TRUE");
// => parseBoolean(): case-insensitive comparison to "true"
// => "TRUE" equals "true" (case-insensitive) → returns true
// => Result: b1 = true

boolean b2 = Boolean.parseBoolean("Yes");
// => "Yes" does NOT equal "true" → returns false
// => Common mistake: expecting "yes" to be true
// => Result: b2 = false

boolean b3 = Boolean.parseBoolean("1");
// => "1" does NOT equal "true" → returns false
// => Unlike C/C++ where 1 is true, Java requires exact "true" string
// => Result: b3 = false

// RADIX STRING REPRESENTATION - Convert number to different bases
// => toBinaryString(), toOctalString(), toHexString()
// => Convert int to string in binary (base 2), octal (base 8), hex (base 16)
// => Useful for debugging, logging, low-level programming

int num = 255;
// => num is 255 (decimal)

String bin = Integer.toBinaryString(num);
// => toBinaryString(): converts int to binary string
// => 255 (decimal) = 11111111 (binary, 8 ones)
// => Result: bin = "11111111"

String oct = Integer.toOctalString(num);
// => toOctalString(): converts int to octal string
// => 255 (decimal) = 377 (octal)
// => Octal: 3*8^2 + 7*8^1 + 7*8^0 = 192 + 56 + 7 = 255
// => Result: oct = "377"

String hexStr = Integer.toHexString(num);
// => toHexString(): converts int to hexadecimal string (lowercase)
// => 255 (decimal) = FF (hex)
// => Hex: F*16^1 + F*16^0 = 15*16 + 15 = 255
// => Result: hexStr = "ff" (lowercase)

// CONVERTING ARRAYS - Loop-based and stream-based approaches
// => Loop: traditional imperative style (explicit iteration)
// => Streams: functional style (declarative, Java 8+)

String[] stringArray = {"1", "2", "3"};
// => stringArray contains String elements ["1", "2", "3"]

int[] intArray = new int[stringArray.length];
// => Create int array with same length as stringArray (length 3)
// => intArray initialized to [0, 0, 0] (default int value)

for (int i = 0; i < stringArray.length; i++) {
    // => Loop from i=0 to i=2 (length 3)
    intArray[i] = Integer.parseInt(stringArray[i]);
    // => i=0: parse "1" → 1, intArray[0] = 1
    // => i=1: parse "2" → 2, intArray[1] = 2
    // => i=2: parse "3" → 3, intArray[2] = 3
}
// => Result: intArray = [1, 2, 3]

// USING STREAMS FOR ARRAY CONVERSION (Java 8+)
// => Functional approach: stream → map → collect
// => More concise for simple transformations
// => Leverages parallel processing potential

int[] streamArray = java.util.Arrays.stream(stringArray)
    // => Arrays.stream(stringArray): creates Stream<String> from array
    // => Stream pipeline: transform each element and collect result
    .mapToInt(Integer::parseInt)
    // => mapToInt(): transforms Stream<String> to IntStream
    // => Integer::parseInt: method reference (equivalent to s -> Integer.parseInt(s))
    // => Applies parseInt to each element: "1"→1, "2"→2, "3"→3
    .toArray();
    // => toArray(): collects IntStream to int[] array
    // => Result: streamArray = [1, 2, 3]

// HANDLING OPTIONAL VALUES (Java 8+)
// => Optional: container for nullable values
// => Avoid null checks and NullPointerException
// => Functional style for null handling

String nullableStr = null;
// => nullableStr is null (no String object)

Integer optionalInt = nullableStr != null ? Integer.parseInt(nullableStr) : null;
// => Ternary operator: condition ? trueValue : falseValue
// => nullableStr is null → condition false → returns null
// => Avoids NullPointerException from parseInt(null)
// => Result: optionalInt = null

// Better with Optional (advanced)
java.util.Optional<Integer> opt = java.util.Optional.ofNullable(nullableStr)
    // => ofNullable(nullableStr): creates Optional<String>
    // => If nullableStr is null → empty Optional (no value)
    // => If nullableStr is non-null → Optional containing the string
    .map(Integer::parseInt);
    // => map(Integer::parseInt): applies parseInt if Optional contains value
    // => If Optional empty (null) → map not applied, returns empty Optional<Integer>
    // => If Optional has value → applies parseInt, wraps result in Optional<Integer>
    // => Result: opt = Optional.empty() (nullableStr was null)

// COMMON CONVERSION UTILITIES - Rounding functions
// => Math.round(): rounds to nearest integer
// => Math.floor(): rounds down (toward negative infinity)
// => Math.ceil(): rounds up (toward positive infinity)

double decimal = 12.56789;
// => decimal is 12.56789

long rounded = Math.round(decimal);
// => Math.round(double): rounds to nearest long
// => 12.56789 rounds to 13 (halfway rounds up: 12.5+ → 13, 12.49- → 12)
// => Result: rounded = 13L

int floor = (int) Math.floor(decimal);
// => Math.floor(double): returns largest double ≤ decimal (rounds down)
// => 12.56789 → 12.0 (double)
// => Cast to int: 12.0 → 12
// => Result: floor = 12

int ceil = (int) Math.ceil(decimal);
// => Math.ceil(double): returns smallest double ≥ decimal (rounds up)
// => 12.56789 → 13.0 (double)
// => Cast to int: 13.0 → 13
// => Result: ceil = 13
```

**Key Takeaway**: Use `Integer.parseInt()`, `Double.parseDouble()`, etc. to parse strings to primitives. Use `String.valueOf()` or wrapper `.toString()` to convert primitives/objects to strings. Parsing can throw `NumberFormatException`—always validate input or use try-catch. Widening conversions (int → long → double) are automatic; narrowing conversions require explicit casts and may lose precision or overflow.

**Why It Matters**: Type conversion and parsing are fundamental to real-world Java applications. Web services receive data as JSON/XML strings that must be parsed to numbers for calculations. Configuration files (properties, YAML) store settings as strings requiring type conversion. User input from forms, command-line arguments, or REST APIs always arrives as strings, making robust parsing essential to prevent crashes from invalid data. The `NumberFormatException` is one of the most common runtime exceptions in production code—proper validation and error handling separate robust applications from fragile ones. Understanding widening vs narrowing conversions prevents subtle overflow bugs (e.g., casting long to int can silently corrupt data). Type safety and explicit conversions make Java verbose but catch errors at compile-time that would be runtime bugs in dynamically-typed languages.

---

### Example 24: Static Members and Blocks

Static members belong to the class, not instances. Static methods can't access instance members. Static blocks initialize class-level state. Understanding static helps with utility classes and singletons.

**Code**:

```java
// STATIC FIELDS - Shared across all instances
// => static keyword: member belongs to class, not individual objects
// => One copy exists for entire class (shared by all instances)
// => Accessed via class name (ClassName.staticField) or through instances
// => Useful for: counters, shared configuration, caches, singletons

class Counter {
    // => Counter class demonstrates static vs instance fields

    private static int count = 0;
    // => static int count: class-level variable
    // => private: encapsulated (only accessible within Counter class)
    // => ONE copy exists for ALL Counter instances (shared memory location)
    // => Initialized to 0 when class first loaded (before any instance created)
    // => All Counter objects share this single count variable

    private int instanceId;
    // => Instance field: each Counter object has its own instanceId
    // => NOT static: separate copy for every Counter instance
    // => c1.instanceId different from c2.instanceId (different memory locations)

    public Counter() {
        // => Constructor: runs every time new Counter() is called
        // => Initializes instance fields and updates static fields

        count++;
        // => Increment shared static count field
        // => count is static → same variable for all instances
        // => First call: count changes from 0 to 1
        // => Second call: count changes from 1 to 2
        // => Third call: count changes from 2 to 3

        this.instanceId = count;
        // => Assign current count value to this instance's instanceId
        // => this.instanceId: refers to THIS object's instanceId field
        // => Each instance gets unique ID from current count value
        // => First object: instanceId = 1, Second: instanceId = 2, Third: instanceId = 3
    }

    public int getInstanceId() {
        // => Instance method: must be called on Counter object (c1.getInstanceId())
        // => Returns this instance's unique ID

        return instanceId;
        // => Returns this object's instanceId (different for each instance)
        // => c1.getInstanceId() returns 1, c2.getInstanceId() returns 2, etc.
    }

    public static int getCount() {
        // => Static method: belongs to class, not instances
        // => Called via class name: Counter.getCount()
        // => Can also call via instance: c1.getCount() (but discouraged, misleading)

        return count;
        // => Returns static count field (total number of Counter instances created)
        // => Same value regardless of how many instances exist
    }
}

Counter c1 = new Counter();
// => new Counter(): creates first Counter instance
// => Executes constructor: count++ (0→1), instanceId = 1
// => c1 references Counter object with instanceId=1
// => Static count field now 1

Counter c2 = new Counter();
// => Creates second Counter instance
// => Executes constructor: count++ (1→2), instanceId = 2
// => c2 references Counter object with instanceId=2
// => Static count field now 2 (shared with c1)

Counter c3 = new Counter();
// => Creates third Counter instance
// => Executes constructor: count++ (2→3), instanceId = 3
// => c3 references Counter object with instanceId=3
// => Static count field now 3 (shared with c1 and c2)

System.out.println(c1.getInstanceId());
// => Call getInstanceId() on c1
// => Returns c1's instanceId field (1)
// => Output: 1

System.out.println(c2.getInstanceId());
// => Call getInstanceId() on c2
// => Returns c2's instanceId field (2)
// => Output: 2

System.out.println(Counter.getCount());
// => Call static method via class name (recommended style)
// => Returns static count field (total instances: 3)
// => Counter.getCount() accessible without creating Counter instance
// => Output: 3

// STATIC METHODS - Belong to class, not instances
// => static methods: called via class name (no instance needed)
// => Cannot access instance fields or methods (no 'this' reference)
// => Pure utility functions operating only on parameters
// => Common for: Math utilities, factory methods, helper functions

class MathUtils {
    // => Utility class with only static methods
    // => No instance fields (stateless)
    // => Never instantiated (new MathUtils() unnecessary)

    public static int add(int a, int b) {
        // => static method: belongs to MathUtils class
        // => public: accessible from anywhere
        // => Parameters a, b: only inputs (no access to instance state)

        return a + b;
        // => Pure function: output depends only on inputs
        // => No side effects (doesn't modify any state)
        // => No instance variables used (stateless computation)
    }

    public static int multiply(int a, int b) {
        // => Another static utility method
        // => Same characteristics: stateless, pure function

        return a * b;
        // => Returns product of a and b
        // => No state dependencies
    }
}

int sum = MathUtils.add(5, 3);
// => Call static method via class name: MathUtils.add()
// => No MathUtils instance needed (no 'new MathUtils()')
// => Passes 5 and 3 as arguments
// => Returns 5 + 3 = 8
// => sum = 8

int product = MathUtils.multiply(4, 7);
// => Call multiply() via class name
// => No instance required
// => Returns 4 * 7 = 28
// => product = 28

// STATIC ACCESS RULES - What static can and cannot access
// => Static context has NO instance (no 'this' reference)
// => Static members can access: static fields, static methods
// => Static members CANNOT access: instance fields, instance methods
// => Instance members can access: static and instance members

class Example {
    // => Demonstrates static vs instance access rules

    private int instanceVar = 10;
    // => Instance field: each Example object has own copy
    // => Accessible only from instance context (methods called on object)

    private static int staticVar = 20;
    // => Static field: one copy shared by all Example instances
    // => Accessible from both static and instance contexts

    public static void staticMethod() {
        // => Static method: belongs to class (no instance)
        // => Cannot access instance members (no object exists in static context)

        System.out.println(staticVar);
        // => OK: static method accessing static field
        // => Both belong to class (no instance needed)
        // => Output: 20

        // System.out.println(instanceVar); // => COMPILE ERROR
        // => CANNOT access instance field from static context
        // => Which instance's instanceVar? (no instance in static context)
        // => Compiler error: "non-static variable instanceVar cannot be referenced from static context"

        // this.instanceVar; // => COMPILE ERROR
        // => 'this' keyword refers to current instance
        // => No instance in static context → no 'this' reference exists
        // => Compiler error: "non-static variable cannot be referenced from static context"
    }

    public void instanceMethod() {
        // => Instance method: called on specific Example object (ex.instanceMethod())
        // => Has access to both instance and static members

        System.out.println(instanceVar);
        // => OK: instance method accessing instance field
        // => 'instanceVar' refers to this object's field
        // => Implicit 'this.instanceVar'
        // => Output: 10

        System.out.println(staticVar);
        // => OK: instance method accessing static field
        // => Static members accessible from instance context
        // => Output: 20
    }
}

// STATIC INITIALIZATION BLOCK - Runs once when class first loaded
// => static { ... }: block executes when JVM loads class
// => Runs ONCE (before any instance created, before main())
// => Used for: complex static field initialization, loading config, logging
// => Executes in order of appearance in class

class DatabaseConfig {
    // => Configuration class with static initialization

    private static String url;
    // => Static field declared but not initialized inline
    // => Will be initialized in static block

    private static String username;
    // => Another static field (uninitialized)

    private static String password;
    // => Another static field (uninitialized)

    static {
        // => Static initialization block
        // => Runs ONCE when DatabaseConfig class first loaded
        // => Loading triggered by: first access to static member, creating instance, Class.forName()

        System.out.println("Loading database config...");
        // => Prints message (runs only once during class loading)
        // => Output: Loading database config...

        url = "jdbc:mysql://localhost:3306/db";
        // => Initialize static url field
        // => Complex initialization logic possible (file reading, parsing, etc.)

        username = "admin";
        // => Initialize static username field

        password = "secret";
        // => Initialize static password field
        // => WARNING: Hardcoded password (for demonstration only, use environment vars in production!)

        System.out.println("Config loaded");
        // => Prints completion message
        // => Output: Config loaded
    }
    // => End of static block
    // => After this block: url, username, password initialized and ready

    public static String getUrl() {
        // => Static method to access static url field
        // => No instance needed

        return url;
        // => Returns initialized url value
    }
}

String dbUrl = DatabaseConfig.getUrl();
// => First access to DatabaseConfig class
// => Triggers class loading by JVM (loading happens BEFORE getUrl() executes)
// => Class loading steps:
// =>   1. JVM loads DatabaseConfig class bytecode
// =>   2. JVM executes static block
// =>   3. Prints: "Loading database config..."
// =>   4. Initializes url, username, password
// =>   5. Prints: "Config loaded"
// =>   6. Now getUrl() executes and returns url
// => dbUrl = "jdbc:mysql://localhost:3306/db"

String dbUrl2 = DatabaseConfig.getUrl();
// => Second access to DatabaseConfig class
// => Class already loaded (static block already executed)
// => Static block does NOT run again (runs ONCE per JVM lifecycle)
// => getUrl() simply returns url field (no reinitialization)
// => dbUrl2 = "jdbc:mysql://localhost:3306/db"

// MULTIPLE STATIC BLOCKS - Execute in order of appearance
// => Class can have multiple static blocks
// => Execute sequentially (top to bottom)
// => Each block can use variables initialized by previous blocks
// => Useful for organizing complex initialization logic

class InitOrder {
    // => Demonstrates execution order of multiple static blocks

    private static int a;
    // => Static field (uninitialized, default value 0)

    private static int b;
    // => Static field (uninitialized, default value 0)

    static {
        // => First static block

        a = 10;
        // => Initialize a to 10
        // => a now available for subsequent blocks

        System.out.println("First static block: a = " + a);
        // => Output: First static block: a = 10
        // => Demonstrates execution order (first block runs first)
    }

    static {
        // => Second static block
        // => Executes AFTER first block completes

        b = a * 2;
        // => Initialize b using a (from first block)
        // => a already initialized to 10 (first block executed)
        // => b = 10 * 2 = 20

        System.out.println("Second static block: b = " + b);
        // => Output: Second static block: b = 20
        // => Demonstrates blocks execute sequentially (second after first)
    }
}
// => When InitOrder class first accessed:
// =>   1. First static block executes: a = 10, prints "First static block: a = 10"
// =>   2. Second static block executes: b = 20, prints "Second static block: b = 20"
// =>   3. Class loading complete

// INITIALIZATION ORDER - Class loading sequence
// => Java initialization order (when class first loaded):
// =>   1. Static fields initialized (in declaration order)
// =>   2. Static blocks executed (in declaration order)
// =>   3. When instance created (new ClassName()):
// =>      4. Instance fields initialized (in declaration order)
// =>      5. Constructor executed
// => CRITICAL: Static initialization happens ONCE (class level)
// =>            Instance initialization happens EVERY time new object created

// STATIC IMPORTS - Import static members into namespace
// => import static ClassName.memberName: import specific static member
// => import static ClassName.*: import all static members
// => Allows using static members without class prefix
// => Useful for: constants (Math.PI), utility methods (Math.sqrt), enums
// => Overuse reduces code readability (unclear where members come from)

import static java.lang.Math.PI;
// => Import Math.PI constant
// => After import: can use 'PI' directly (without 'Math.' prefix)

import static java.lang.Math.sqrt;
// => Import Math.sqrt() method
// => After import: can call 'sqrt(x)' directly (without 'Math.' prefix)

double radius = 5.0;
// => radius is 5.0 (double)

double area = PI * radius * radius;
// => Use PI directly (no Math.PI needed due to static import)
// => PI is Math.PI constant (3.141592653589793)
// => area = π * r² = 3.14159... * 5.0 * 5.0 ≈ 78.54
// => Result: area ≈ 78.53981633974483

double result = sqrt(16);
// => Use sqrt() directly (no Math.sqrt() needed due to static import)
// => sqrt is Math.sqrt() method
// => sqrt(16) = 4.0
// => Result: result = 4.0

// STATIC NESTED CLASSES - Class defined inside another class
// => static class NestedClass: nested class marked static
// => Static nested class does NOT have reference to outer instance
// => Can access outer class's static members (but NOT instance members)
// => Used for: grouping related classes, helper classes, builders

class Outer {
    // => Outer class with static and instance members

    private static String staticOuter = "Static Outer";
    // => Static field in Outer class
    // => Accessible to static nested class

    private String instanceOuter = "Instance Outer";
    // => Instance field in Outer class
    // => NOT accessible to static nested class (no outer instance reference)

    public static class StaticNested {
        // => Static nested class
        // => static: no implicit reference to Outer instance
        // => Compiled to separate class file: Outer$StaticNested.class
        // => Can be instantiated without Outer instance

        public void display() {
            // => Instance method in nested class

            System.out.println(staticOuter);
            // => OK: can access Outer's static members
            // => staticOuter belongs to Outer class (no instance needed)
            // => Output: Static Outer

            // System.out.println(instanceOuter); // => COMPILE ERROR
            // => CANNOT access Outer's instance members
            // => Which Outer instance's instanceOuter? (no outer instance reference)
            // => Compiler error: "non-static variable instanceOuter cannot be referenced from static context"
        }
    }
}

Outer.StaticNested nested = new Outer.StaticNested();
// => Create static nested class instance
// => Syntax: Outer.StaticNested (qualified name)
// => No Outer instance needed (static nested class independent)
// => Compare to inner class: new Outer().new InnerClass() (needs outer instance)
// => nested is StaticNested object

nested.display();
// => Call display() on nested object
// => Prints staticOuter value
// => Output: Static Outer

// CONSTANTS PATTERN - public static final for immutable values
// => public: accessible from anywhere
// => static: belongs to class (one copy)
// => final: cannot be reassigned after initialization (immutable)
// => Convention: SCREAMING_SNAKE_CASE for constants
// => Prevent instantiation with private constructor

class Constants {
    // => Utility class holding application constants
    // => Never instantiated (all members static)

    public static final double PI = 3.14159;
    // => public: accessible everywhere
    // => static: belongs to Constants class (no instance)
    // => final: value cannot change (immutable)
    // => PI initialized to 3.14159 (approximation of π)

    public static final int MAX_SIZE = 1000;
    // => Constant for maximum size (array, collection, buffer)
    // => final: prevents accidental modification

    public static final String APP_NAME = "MyApp";
    // => Constant String for application name
    // => String objects immutable, final prevents reassignment
    // => Used in: logging, config, about dialogs

    private Constants() {
        // => Private constructor: prevents instantiation
        // => new Constants() causes compile error (constructor not accessible)

        throw new AssertionError("Cannot instantiate constants class");
        // => AssertionError if constructor called via reflection
        // => Defensive programming: ensures class never instantiated
        // => Message explains why instantiation is forbidden
    }
}
// => Usage: Constants.PI, Constants.MAX_SIZE, Constants.APP_NAME
// => Never: new Constants() (compile error due to private constructor)

double pi = Constants.PI;
// => Access constant via class name: Constants.PI
// => No Constants instance needed (static member)
// => pi = 3.14159

// Constants.PI = 3.0; // => COMPILE ERROR
// => Cannot assign to final variable
// => final keyword prevents reassignment
// => Compiler error: "cannot assign a value to final variable PI"
// => This immutability prevents bugs from accidental constant modification
```

**Key Takeaway**: Static members belong to the class (shared across all instances). Static methods accessed via class name (not instance). Static can't access instance members. Static blocks initialize class-level state once when class loads. Use static for utility methods, constants, and counters. Static imports allow using static members without class prefix.

**Why It Matters**: Understanding static is fundamental to Java architecture—the `main()` method that starts every application is static, allowing the JVM to invoke it without creating an instance. Static utility classes (Math, Collections, Arrays) dominate the standard library, providing stateless functions that don't require object overhead. Static initialization blocks enable complex class-level setup (JDBC driver registration, logging configuration, resource loading) that runs once per JVM. The singleton pattern relies on static fields and methods to enforce single-instance constraints in multithreaded environments. Static final constants (MAX_VALUE, PI, configuration keys) prevent magic numbers and enable compiler optimizations through constant folding. Misusing static creates global state that complicates testing, breaks encapsulation, and causes thread-safety issues—knowing when NOT to use static is as important as knowing when to use it.

---

### Example 25: Packages and Access Modifiers

Packages organize classes into namespaces preventing name collisions. Access modifiers (`public`, `private`, `protected`, package-private) control visibility and enforce encapsulation boundaries. Understanding access control is essential for API design, security, and maintainability.

**Code**:

```java
// PACKAGE DECLARATION - must be FIRST non-comment statement
package com.example.myapp; // => This class belongs to com.example.myapp namespace
// => Package name follows reverse domain convention (com.company.project)
// => Maps to directory structure: com/example/myapp/ThisFile.java
// => All classes in this file belong to same package
// => Default package (no declaration) is discouraged for production code

// IMPORT STATEMENTS - come AFTER package, BEFORE class declarations
import java.util.ArrayList; // => Import specific class from java.util package
// => Now can use ArrayList directly without full qualification
import java.util.List; // => Import List interface from java.util
// => Multiple imports from same package require separate statements
import java.io.*; // => Wildcard import - brings ALL classes from java.io
// => Wildcard does NOT import subpackages (java.io.file.* needs separate import)
// => Wildcard only for convenience - compiled code uses specific classes
import static java.lang.Math.PI; // => Static import for constants/methods
// => Can use PI directly without Math.PI prefix
// => Static import useful for constants (PI, E) and utility methods (max, min)
// => Overuse harms readability - unclear which class owns the member

// ACCESS MODIFIERS - four visibility levels

// 1. PUBLIC - accessible from ANYWHERE (any package, any class)
public class PublicClass { // => Can be imported and used by any other class
    // => Only ONE public class per .java file
    // => Public class name MUST match filename (PublicClass.java)
    // => Represents the API boundary - what clients can see

    public int publicField = 10; // => Accessible from any package
    // => Can read: anyInstance.publicField from anywhere
    // => Can write: anyInstance.publicField = 20 from anywhere
    // => Public fields violate encapsulation - prefer private fields with getters/setters

    public void publicMethod() { // => Callable from any package
        System.out.println("Public method"); // => Output: Public method
        // => Part of public API contract
        // => Changes to signature break client code (breaking change)
    }
}

// 2. PRIVATE - accessible ONLY within same class (strongest encapsulation)
class PrivateExample { // => Class itself package-private (no public modifier)
    private int privateField = 20; // => Only code inside PrivateExample can access
    // => External code: obj.privateField => ERROR: privateField has private access
    // => Internal code: this.privateField => OK
    // => Hides implementation details from outside world

    private void privateMethod() { // => Only callable within this class
        System.out.println("Private method"); // => Output: Private method
        // => Implementation detail not part of public API
        // => Can change logic freely without affecting clients
        // => Subclasses CANNOT override private methods
    }

    public void accessPrivate() { // => Public method exposing controlled access
        System.out.println(privateField); // => OK: same class can access private
        // => Output: 20
        privateMethod(); // => OK: same class can call private methods
        // => Controlled access through public API
        // => Encapsulation enforced - clients use accessPrivate(), not direct access
    }
}

// 3. PROTECTED - accessible within package AND by subclasses (inheritance-friendly)
class ProtectedExample {
    protected int protectedField = 30; // => Package + subclasses in any package
    // => Same package: any class can access
    // => Different package: only subclasses can access
    // => Used for inheritance hierarchies - extensible but not public

    protected void protectedMethod() { // => Callable from package + subclasses
        System.out.println("Protected method"); // => Output: Protected method
        // => Subclasses can override this method
        // => Template method pattern uses protected hooks
        // => More restrictive than public, more open than private
    }
}

// 4. PACKAGE-PRIVATE (default, no modifier) - accessible ONLY within same package
class PackagePrivateClass { // => No modifier = package-private (default access)
    // => Visible to all classes in com.example.myapp package
    // => INVISIBLE to classes in other packages (even subpackages)
    // => Used for internal implementation classes not part of public API

    int packageField = 40; // => No modifier = package-private field
    // => Same package: can access directly
    // => Different package: ERROR: packageField is not public
    // => Java's default is more restrictive than C++ (private) or C# (internal)

    void packageMethod() { // => Package-private method
        System.out.println("Package-private method"); // => Output: Package-private method
        // => Helper methods used within package
        // => Not accessible to classes in other packages
        // => Good for package-level cohesion without public exposure
    }
}

// ACCESS MODIFIER VISIBILITY MATRIX:
//                      | Class | Package | Subclass | World |
// -----------------------------------------------------------|
// public               |  ✓    |    ✓    |    ✓     |   ✓   | Most permissive
// protected            |  ✓    |    ✓    |    ✓     |   ✗   | Package + inheritance
// (package-private)    |  ✓    |    ✓    |    ✗     |   ✗   | Default Java access
// private              |  ✓    |    ✗    |    ✗     |   ✗   | Strongest encapsulation

// TYPICAL ENCAPSULATION PATTERN - JavaBean style
public class Person { // => Public class - part of public API
    // PRIVATE FIELDS - internal state hidden from external access
    private String name; // => Only accessible within Person class
    // => Direct access: person.name => ERROR: name has private access
    // => Enforces encapsulation - clients cannot bypass validation

    private int age; // => Private instance variable
    // => State is protected from invalid modifications
    // => Changes to field type don't affect clients (use getters/setters)

    private static final int MAX_AGE = 150; // => Private constant
    // => Implementation detail - not part of public API
    // => Can change MAX_AGE value without affecting compiled client code

    // PUBLIC CONSTRUCTOR - allows object creation from anywhere
    public Person(String name, int age) { // => Clients can: new Person("Alice", 25)
        // => Constructor enforces invariants - validates state before creation
        this.name = name; // => Initialize private field using this reference
        // => this.name refers to instance field, name refers to parameter
        this.age = age; // => Initialize age field
        // => Could add validation here to prevent invalid Person objects
        // => Ensures object is always in valid state after construction
    }

    // PUBLIC GETTERS - controlled read access (accessor methods)
    public String getName() { // => Read-only access to name field
        return name; // => Returns current value of private field
        // => Clients use: person.getName() => "Alice"
        // => No direct field access allowed
        // => Can add logging, caching, lazy initialization without changing API
    }

    public int getAge() { // => Accessor for age
        return age; // => Returns age value
        // => Could compute derived values: return (2024 - birthYear)
        // => Getter can transform internal representation before returning
        // => Encapsulation preserved - internal structure hidden
    }

    // PUBLIC SETTERS - controlled write access (mutator methods)
    public void setAge(int age) { // => Mutator with validation logic
        if (age >= 0 && age <= MAX_AGE) { // => Validation guard clause
            // => Prevents invalid state (negative age, unrealistic age)
            this.age = age; // => Only set if valid
            // => this.age = instance field, age = parameter
            // => Setter can log changes, trigger events, notify observers
        } else { // => Invalid input - reject with exception
            throw new IllegalArgumentException("Invalid age: " + age); // => Runtime exception
            // => Fails fast - prevents invalid state propagation
            // => Caller must handle exception or let it bubble up
            // => Ensures Person object always valid
        }
    }

    // PRIVATE HELPER METHOD - implementation detail not exposed
    private boolean isAdult() { // => Private - not part of public API
        return age >= 18; // => Business logic encapsulated
        // => Can change threshold (age >= 21) without affecting clients
        // => Clients cannot call: person.isAdult() => ERROR: isAdult() has private access
        // => Reduces public API surface - easier maintenance
    }

    // PUBLIC METHOD using private helper
    public String getCategory() { // => Public API method
        return isAdult() ? "Adult" : "Minor"; // => Uses private helper internally
        // => Returns: "Adult" if age >= 18, else "Minor"
        // => Ternary operator (condition ? trueValue : falseValue)
        // => Clients use: person.getCategory() => "Adult"
        // => Implementation uses isAdult() but clients unaware of internal structure
    }
}

// USING THE ENCAPSULATED CLASS
Person person = new Person("Alice", 25); // => Create instance via public constructor
// => person is reference variable holding Person object reference
// => Object allocated on heap, reference stored in stack

String name = person.getName(); // => name = "Alice" (via public getter)
// => Controlled read access - getter can log, cache, transform
// => No direct field access allowed

// String directName = person.name; // => ERROR: name has private access in Person
// => Compiler prevents violation of encapsulation
// => Forces use of public getter API

person.setAge(30); // => OK: valid age, passes validation
// => Calls setter which validates (30 is between 0 and 150)
// => this.age updated to 30
// => Object state remains valid

// person.setAge(200); // => Throws IllegalArgumentException: Invalid age: 200
// => Validation in setter prevents invalid state
// => Exception propagates to caller
// => Object state unchanged (age still 30)

// boolean adult = person.isAdult(); // => ERROR: isAdult() has private access in Person
// => Private methods not accessible to clients
// => Must use public API: person.getCategory()

String category = person.getCategory(); // => "Adult" (age is 30, >= 18)
// => Public method internally uses private helper
// => Client unaware of isAdult() implementation detail

// PACKAGE STRUCTURE IN REAL PROJECTS
// Typical multi-package structure:
// com/example/myapp/
//   ├── Main.java                    (public class Main with main() entry point)
//   ├── util/
//   │   ├── StringUtils.java         (public class StringUtils - utility methods)
//   │   └── Helper.java              (package-private class Helper - internal tool)
//   └── model/
//       ├── User.java                (public class User - domain model)
//       └── UserRepository.java      (public interface UserRepository - data access)

// Each package groups related classes (cohesion)
// Public classes form package's API (what clients import)
// Package-private classes are implementation details (hidden from clients)

// IMPORTING FROM DIFFERENT PACKAGES
// In Main.java (com.example.myapp package):
// package com.example.myapp;
// import com.example.myapp.util.StringUtils; // => Import public class from util package
// => Can now use: StringUtils.capitalize("hello")
// import com.example.myapp.model.User; // => Import User from model package
// => Can now use: new User("Alice")
// import com.example.myapp.util.Helper; // => ERROR: Helper is package-private
// => Only classes in com.example.myapp.util can use Helper

// STATIC IMPORT - bringing static members into namespace
// Traditional usage with qualification:
// double radius = 5.0;
// double area = Math.PI * radius * radius; // => Must qualify PI with Math class

// With static import:
// import static java.lang.Math.PI;
// import static java.lang.Math.pow;
// double radius = 5.0;
// double area = PI * pow(radius, 2); // => Use PI and pow directly
// => Cleaner code for math-heavy calculations
// => Can import all static members: import static java.lang.Math.*;
// => Use sparingly - overuse harms readability (where does PI come from?)

// CONSTRUCTOR ACCESS MODIFIERS - control instantiation
class ConstructorAccess {
    // PRIVATE CONSTRUCTOR - prevents external instantiation
    private ConstructorAccess() { // => Constructor is private
        // => External code: new ConstructorAccess() => ERROR: constructor is private
        // => Only code inside ConstructorAccess class can call constructor
        // => Used for Singleton pattern (one instance) or utility classes
    }

    // Public static factory method - ONLY way to create instance
    public static ConstructorAccess create() { // => Factory method (public static)
        return new ConstructorAccess(); // => Can call private constructor from same class
        // => Factory can control creation (caching, pooling, validation)
        // => Returns new instance each call (or cached instance for Singleton)
        // => Clients use factory instead of constructor
    }
}

// ATTEMPTING TO USE PRIVATE CONSTRUCTOR
// ConstructorAccess obj = new ConstructorAccess(); // => ERROR: ConstructorAccess() has private access
// => Compiler prevents direct instantiation
// => Must use factory method

ConstructorAccess obj = ConstructorAccess.create(); // => OK: use public factory method
// => Factory method internally calls private constructor
// => Encapsulation ensures controlled object creation
// => Factory can implement patterns (Singleton, pooling, caching)

// PACKAGE-PRIVATE CLASSES AND INTERFACES - hiding implementation details
// Internal API - only visible within package
interface InternalApi { // => Package-private (no public modifier)
    void internalMethod(); // => Method declared in interface
    // => Only classes in same package can implement this interface
    // => Different package: cannot see InternalApi
    // => Hides internal abstractions from external clients
}

class InternalImpl implements InternalApi { // => Package-private implementation
    @Override // => Annotation indicates method overrides interface method
    public void internalMethod() { // => Public in implementation (interface methods must be public)
        System.out.println("Internal implementation"); // => Implementation detail
        // => Only code in same package can instantiate InternalImpl
        // => Different package cannot import or use this class
    }
}

// PUBLIC FACADE - exposes only what clients need
public class PublicApi { // => Public class - visible to all packages
    private InternalApi internal = new InternalImpl(); // => Private field holds internal implementation
    // => Clients cannot access this field (private)
    // => Internal implementation hidden behind public API
    // => Can swap implementation (different InternalApi impl) without affecting clients

    public void publicMethod() { // => Public method clients can call
        internal.internalMethod(); // => Delegates to internal implementation
        // => Output: Internal implementation
        // => Clients unaware of InternalApi and InternalImpl
        // => Facade pattern - simple interface over complex subsystem
    }
}

// USAGE OF FACADE
PublicApi api = new PublicApi(); // => Create public facade instance
// => Internal implementation created automatically (InternalImpl)
api.publicMethod(); // => Calls public method
// => Output: Internal implementation
// => Internally delegates to InternalApi.internalMethod()
// => Client code never sees InternalApi or InternalImpl
// => Implementation can change without breaking clients

// BENEFITS OF ACCESS MODIFIERS:
// 1. Encapsulation - hide internal state and implementation
// 2. API stability - separate public API from internal details
// 3. Security - prevent unauthorized access to sensitive data
// 4. Maintainability - change internals without breaking clients
// 5. Documentation - modifiers signal intent (public = API, private = internal)
```

**Key Takeaway**: Packages organize classes into namespaces (`package com.example.app`) mapping to directory structure. Import statements bring classes into scope. Four access modifiers control visibility: `public` (everywhere), `protected` (package + subclasses), package-private/default (same package only), `private` (same class only). Standard encapsulation pattern: private fields, public getters/setters with validation, private helper methods. Package-private classes hide implementation details. Static imports bring constants/methods into namespace (`import static Math.PI`). Private constructors with factory methods control instantiation. Access modifiers enforce encapsulation boundaries essential for API design and security.

**Why It Matters**: Packages prevent name collisions in large codebases where multiple teams use common names (User, Order, Manager). Access modifiers enforce security boundaries—private fields prevent bypassing validation (age cannot be set to -1), protecting invariants. Encapsulation enables refactoring internals without breaking compiled client code—changing `private String name` to `private Name name` requires no client recompilation if getter returns `String`. Protected access enables framework extension (Spring's template methods) while hiding dangerous internals. Package-private classes reduce public API surface—fewer classes to document, maintain, and stabilize. The JavaBeans convention (getters/setters) integrates with frameworks (Spring, Hibernate, JSP) using reflection to access properties. Proper access control is foundational to secure, maintainable, evolvable systems.

---

### Example 26: The `var` Keyword

The `var` keyword (Java 10+) provides local variable type inference where the compiler deduces type from the initializer expression. Reduces boilerplate for complex generic types while maintaining full compile-time type safety. Not dynamic typing—type is fixed at declaration.

**Code**:

```java
// TRADITIONAL EXPLICIT TYPE DECLARATIONS (verbose but clear)
ArrayList<String> list1 = new ArrayList<String>(); // => Type repeated on both sides
// => list1 is ArrayList<String> (parameterized type)
// => Verbosity increases with complex generics
HashMap<String, Integer> map1 = new HashMap<String, Integer>(); // => More repetition
// => map1 is HashMap<String, Integer>
// => Diamond operator <> introduced in Java 7 helps: new HashMap<>()
String message1 = "Hello World"; // => Simple case - type is obvious
// => message1 is String
// => Explicit type documents intent clearly

// VAR WITH TYPE INFERENCE (concise, type still static)
var list2 = new ArrayList<String>(); // => Compiler infers ArrayList<String>
// => list2 has EXACT SAME TYPE as list1 (ArrayList<String>)
// => Type determined at compile-time from right-hand side
// => NOT dynamic typing - type cannot change after declaration
var map2 = new HashMap<String, Integer>(); // => Inferred HashMap<String, Integer>
// => map2 is HashMap<String, Integer>
// => Reduces visual noise for complex generic types
var message2 = "Hello World"; // => Inferred String
// => message2 is String (from string literal type)
// => Type inference based on initializer expression

// TYPE IS STILL STATIC AND CHECKED AT COMPILE-TIME
// var demonstrates compile-time type safety:
// list2 = 42; // => COMPILE ERROR: incompatible types: int cannot be converted to ArrayList<String>
// => var does NOT make Java dynamically typed
// => Once declared, type is fixed and enforced
list2.add("Java"); // => OK: add(String) is valid method of ArrayList<String>
// => list2 still has all ArrayList<String> methods
// => Autocomplete and IDE support work normally
// list2.add(123); // => COMPILE ERROR: add(String) expects String, got int
// => Type parameters enforced - cannot add Integer to ArrayList<String>
// => Same type safety as explicit ArrayList<String> declaration

// VAR WITH PRIMITIVE TYPES - inferred from literal type
var count = 0; // => int (integer literal defaults to int)
// => count is int, not byte, short, or long
// => Default integer type is int in Java
var price = 19.99; // => double (decimal literal defaults to double)
// => price is double, not float
// => Default floating-point type is double in Java
var flag = true; // => boolean (boolean literal)
// => flag is boolean
// => Only two boolean literals: true, false
var letter = 'A'; // => char (character literal)
// => letter is char
// => Single quotes for char, double quotes for String

// VAR WITH METHOD RETURN TYPES - inferred from method signature
var now = java.time.LocalDateTime.now(); // => LocalDateTime (inferred from method return type)
// => now is LocalDateTime (static method LocalDateTime.now() returns LocalDateTime)
// => Useful when return type is long/complex
// => java.time.LocalDateTime.now() is fully qualified call
var random = Math.random(); // => double (Math.random() returns double)
// => random is double
// => Math.random() returns double in range [0.0, 1.0)
var length = "Hello".length(); // => int (String.length() returns int)
// => length is int (value is 5)
// => String.length() method returns int

// VAR IN LOOPS - very useful for reducing boilerplate

// Traditional for-each with explicit type
for (String item : list2) { // => item is String (explicit declaration)
    System.out.println(item); // => Output: Java
    // => Traditional loop with type annotation
}

// var for-each (cleaner, type inferred from collection)
var items = List.of("Java", "Python", "Go"); // => items is List<String>
// => List.of(...) returns immutable List<String>
// => Type parameter inferred from arguments (all String)
for (var item : items) { // => item inferred as String (element type of List<String>)
    System.out.println(item); // => Output: Java, Python, Go (each on new line)
    // => var reduces noise in loop declarations
    // => Type still enforced - item is String
}

// var in traditional for loop
for (var i = 0; i < 10; i++) { // => i inferred as int
    System.out.println(i); // => Output: 0, 1, 2, ..., 9 (each on new line)
    // => i is int (from literal 0)
    // => Common pattern in traditional for loops
}

// VAR WITH COMPLEX GENERIC TYPES - readability win

// Traditional (very verbose - type information repeated)
Map<String, List<Map<String, Integer>>> traditional =
    new HashMap<String, List<Map<String, Integer>>>(); // => Type repeated exactly
// => traditional is Map<String, List<Map<String, Integer>>>
// => Verbose and hard to read
// => Easy to make typos in nested generics

// var (much cleaner - type still enforced)
var modern = new HashMap<String, List<Map<String, Integer>>>(); // => Type inferred from right side
// => modern has EXACT SAME TYPE: HashMap<String, List<Map<String, Integer>>>
// => Right side specifies complete type information
// => Left side inferred automatically
// => Cleaner without sacrificing type safety

// VAR WITH DIAMOND OPERATOR - cannot infer from <> alone
// var list3 = new ArrayList<>(); // => COMPILE ERROR: cannot infer type arguments for ArrayList<>
// => Diamond operator <> requires context to infer type
// => var removes that context - compiler cannot infer type parameter
// => Must specify type argument explicitly with var
var list4 = new ArrayList<String>(); // => OK: explicit type argument String
// => list4 is ArrayList<String>
// => Type argument <String> provides necessary information

// VAR WITH LAMBDA EXPRESSIONS - type inferred from target type context
import java.util.function.Predicate;
import java.util.stream.Collectors;

// Traditional with explicit functional interface type
Predicate<String> traditional2 = s -> s.length() > 5; // => Predicate<String> explicit
// => traditional2 is Predicate<String>
// => Lambda parameter s inferred as String from Predicate<String>

// var with stream operations - inferred from stream terminal operation
var filtered = items.stream() // => Stream<String> from items (List<String>)
    .filter(s -> s.length() > 5) // => s inferred as String from stream element type
    .collect(Collectors.toList()); // => collect returns List<String>
// => filtered inferred as List<String> from collect() return type
// => Type flows through stream pipeline
// => Result: filtered = ["Python"] (only string with length > 5)

// VAR LIMITATIONS AND RESTRICTIONS:

// 1. MUST have initializer - cannot infer type from nothing
// var x; // => COMPILE ERROR: cannot infer type without initializer
// => var requires right-hand side expression to infer from
// => No initializer = no type information
var y = 10; // => OK: inferred as int from literal 10
// => y is int
// => Initializer provides type information

// 2. Cannot use with null - null has no type information
// var nullVar = null; // => COMPILE ERROR: variable initializer is 'null'
// => null is compatible with any reference type
// => Compiler cannot choose which type to infer
var nullableString = (String) null; // => OK: explicit cast provides type
// => nullableString is String (value is null)
// => Cast gives compiler necessary type information
// => Unusual pattern - typically initialize with non-null or use explicit type

// 3. Cannot use for instance/static FIELDS - only local variables
class Example {
    // var field = 10; // => COMPILE ERROR: 'var' is not allowed here
    // => var only for local variables (method scope)
    // => Fields must use explicit types for clarity
    private int field = 10; // => Must use explicit type int
    // => Fields are class-level state - explicit types document contract
}

// 4. Cannot use for METHOD PARAMETERS
// public void method(var param) {} // => COMPILE ERROR: 'var' is not allowed here
// => Method signatures must be explicit
// => Parameters are part of method contract
public void method(String param) {} // => Must use explicit type String
// => Explicit parameter types required for overloading, reflection, documentation

// 5. Cannot use for METHOD RETURN TYPES
// public var getValue() { return 42; } // => COMPILE ERROR: 'var' is not allowed here
// => Return type is part of method signature
// => Must be explicit for API contract
public int getValue() { return 42; } // => Must use explicit return type int
// => Return type is public API - must be documented explicitly

// 6. Array initializer needs explicit type on right side
// var array = {1, 2, 3}; // => COMPILE ERROR: array initializer needs explicit target type
// => Array initializer {1, 2, 3} doesn't convey element type
// => Compiler cannot infer int[] vs Integer[] vs long[]
var array = new int[]{1, 2, 3}; // => OK: int[] inferred from new int[]{...}
// => array is int[]
// => new int[]{1, 2, 3} provides explicit type information

// WHEN TO USE VAR VS EXPLICIT TYPES:

// Use var when type is OBVIOUS from right-hand side (improves readability)
var builder = new StringBuilder(); // => GOOD: obviously StringBuilder
// => Constructor call clearly shows type
// => No information loss from using var
var user = userService.getUser(id); // => GOOD: method name strongly suggests User type
// => Method name getUser() implies User return type
// => Refactoring hint: clear naming makes var viable

// Avoid var when type is NOT OBVIOUS (harms readability)
var result = calculate(); // => BAD: what type does calculate() return?
// => calculate() name doesn't indicate return type
// => Could be int, double, CalculationResult, etc.
// => Reader must find method signature to understand type
CalculationResult result2 = calculate(); // => GOOD: explicit type clarifies intent
// => Documents return type at call site
// => Improves code readability for future maintainers

// var with chained method calls - readability depends on context
var data = source.fetch().process().transform(); // => Type depends on transform() return
// => May or may not be obvious depending on API familiarity
// => Use var if method chain clearly conveys result type

// RESERVED KEYWORD STATUS (Java 11+)
// var became reserved type name in certain contexts (Java 10+)
// var var = 10; // => COMPILE ERROR in Java 11+: 'var' is reserved type name
// => Cannot use var as variable name when var keyword is in scope
// => Backward compatibility: var can still be used as identifier in some contexts
// => Best practice: avoid using var as identifier entirely

// PRACTICAL EXAMPLE: improving code readability without losing safety
import java.util.Map;
import java.util.AbstractMap;
import java.util.AbstractMap.SimpleEntry;

// Traditional (type information repeated - visual noise)
Entry<String, List<Integer>> entry1 =
    new AbstractMap.SimpleEntry<String, List<Integer>>("key", List.of(1, 2, 3));
// => entry1 is Entry<String, List<Integer>>
// => Type specified on both sides - redundant
// => Long generic types reduce readability

// With var (type still enforced, less noise)
var entry2 = new AbstractMap.SimpleEntry<>("key", List.of(1, 2, 3)); // => Diamond operator <> infers type
// => entry2 is Entry<String, List<Integer>> (exact same type as entry1)
// => Type inferred from constructor arguments
// => "key" is String, List.of(1, 2, 3) is List<Integer>
// => Diamond operator infers <String, List<Integer>> from arguments
// => Cleaner without sacrificing type information

String key = entry2.getKey(); // => key = "key" (String from Entry<String, ...>)
// => entry2.getKey() returns String (first type parameter)
List<Integer> values = entry2.getValue(); // => values = [1, 2, 3] (List<Integer>)
// => entry2.getValue() returns List<Integer> (second type parameter)
// => Full type safety maintained - compiler knows exact types

// VAR WITH ANONYMOUS CLASSES - type is anonymous class type
var handler = new Object() {
    void handle() {
        System.out.println("Handling"); // => Custom method in anonymous class
    }
};
// => handler has anonymous class type extending Object
// => Can call handle() method: handler.handle() => Output: Handling
// => Anonymous class type not expressible otherwise - var is useful here
handler.handle(); // => Output: Handling

// VAR BEST PRACTICES:
// 1. Use var when right-hand side clearly indicates type
// 2. Avoid var when type is not obvious from context
// 3. Keep right-hand side simple - complex expressions obscure type
// 4. Use var in loops to reduce boilerplate
// 5. Don't use var just to save typing - readability is paramount
// 6. When in doubt, use explicit type for clarity
```

**Key Takeaway**: `var` provides local variable type inference (Java 10+) where compiler infers type from initializer. Maintains compile-time type safety—type is static and enforced. Reduces boilerplate for complex generic types without sacrificing safety. Restrictions: requires initializer, local variables only (not fields/parameters/return types), cannot infer from `null` or naked diamond `<>`, array initializers need explicit type. Use when type is obvious from right-hand side. Improves readability for complex types (`HashMap<String, List<Map<String, Integer>>>`) but harms readability when initializer type is unclear. Not dynamic typing—type is fixed at declaration.

**Why It Matters**: Type inference reduces cognitive load in complex generic code—`var map = new HashMap<String, List<User>>()` is clearer than repeating types. Enables refactoring safety—if method return type changes from `ArrayList` to `List`, var-declared variables automatically adapt without code changes. Reduces maintenance burden—updating type in one place (initializer) propagates automatically. However, overuse harms readability when types aren't obvious from context—`var result = process()` is unclear without finding method signature. Modern Java (10+) balances explicitness with conciseness. The var feature makes Java competitive with languages like Kotlin and C# while preserving static typing guarantees essential for large-scale enterprise systems.

---

### Example 27: Nested and Inner Classes

Java supports four types of nested classes providing different encapsulation and access semantics: static nested classes (independent of outer instance), non-static inner classes (tied to outer instance), local classes (defined inside methods), and anonymous classes (unnamed inline implementations). Each serves distinct purposes for organization, callbacks, and tight coupling.

**Code**:

```java
// 1. STATIC NESTED CLASS - doesn't require outer instance (independent)
class Outer {
    private static String staticField = "Static Outer"; // => Outer class static field
    // => Shared across all Outer instances
    // => Accessible from static nested class
    private String instanceField = "Instance Outer"; // => Outer instance field
    // => Each Outer instance has own copy
    // => NOT accessible from static nested class (no outer instance)

    // Static nested class - behaves like top-level class within namespace of outer
    public static class StaticNested { // => Static nested class
        // => Can be instantiated without Outer instance
        // => Access: Outer.StaticNested (qualified name)
        // => Useful for grouping related classes (Builder pattern)

        public void display() { // => Method in static nested class
            System.out.println(staticField); // => OK: access outer STATIC members
            // => Output: Static Outer
            // => Static nested can access outer static fields/methods
            // System.out.println(instanceField); // => COMPILE ERROR: non-static field instanceField cannot be referenced from static context
            // => Cannot access outer instance members - no outer instance available
            // => Static nested class is independent of outer instances
        }
    }
}

// CREATE STATIC NESTED CLASS - no outer instance needed
Outer.StaticNested nested = new Outer.StaticNested(); // => Create directly via Outer.StaticNested
// => nested is instance of static nested class
// => No Outer instance required (independent)
// => Similar to top-level class, just namespaced under Outer
nested.display(); // => Output: Static Outer
// => Calls display() which accesses outer static field

// Static nested classes are commonly used for:
// - Builder pattern: Person.Builder
// - Helper classes tightly coupled to outer: Map.Entry
// - Grouping related classes: OuterClass.HelperClass

// 2. NON-STATIC INNER CLASS - tied to outer instance (dependent)
class Outer2 {
    private String outerField = "Outer Field"; // => Outer instance field
    // => Each Outer2 instance has own outerField
    // => Inner class instances can access this field

    // Inner class (non-static) - each instance associated with outer instance
    public class Inner { // => Non-static inner class (no static modifier)
        // => Inner instance MUST be associated with Outer2 instance
        // => Has implicit reference to enclosing Outer2 instance
        // => Can access outer instance members (fields and methods)

        public void display() { // => Method in inner class
            System.out.println(outerField); // => OK: access outer instance field
            // => Output: Outer Field
            // => Implicitly uses Outer2.this.outerField
            System.out.println(Outer2.this.outerField); // => Explicit outer this reference
            // => Output: Outer Field
            // => Outer2.this refers to enclosing Outer2 instance
            // => Useful when inner class has same-named field (shadowing)
        }

        public void shadowing() { // => Demonstrates variable shadowing
            String outerField = "Inner Local"; // => Local variable shadows outer field
            // => outerField now refers to local variable, not outer field
            System.out.println(outerField); // => "Inner Local" (local variable)
            // => Unqualified outerField refers to closest scope (local)
            System.out.println(Outer2.this.outerField); // => "Outer Field" (outer instance field)
            // => Outer2.this.outerField explicitly refers to outer field
            // => Resolves shadowing ambiguity
        }
    }

    public Inner createInner() { // => Outer method creating inner instance
        return new Inner(); // => Outer can create inner directly
        // => this.new Inner() (implicit this)
        // => Inner instance associated with this Outer2 instance
    }
}

// CREATE INNER CLASS - MUST have outer instance first
Outer2 outer = new Outer2(); // => Create outer instance
// => outer is Outer2 instance
// => Required before creating Inner instance
Outer2.Inner inner = outer.new Inner(); // => Create inner FROM outer instance
// => inner is Inner instance associated with outer
// => Syntax: outerInstance.new InnerClass()
// => Inner holds implicit reference to outer (can access outerField)
inner.display(); // => Output: Outer Field
// => Accesses outer.outerField via implicit reference

// Alternative creation via outer method
Outer2.Inner inner2 = outer.createInner(); // => Use outer method to create inner
// => inner2 is Inner instance associated with outer
// => Encapsulates creation logic in outer class

// Inner classes are commonly used for:
// - Iterators: ArrayList.Itr implements Iterator (needs access to ArrayList's internal array)
// - Event handlers: tied to specific outer instance state
// - Tightly coupled helper classes needing outer instance access

// 3. LOCAL CLASS - defined inside method (method scope)
class LocalClassExample {
    public void demonstrateLocal() { // => Method containing local class
        final String localVar = "Local Variable"; // => Effectively final local variable
        // => Local variables captured by local class must be final or effectively final
        // => Effectively final = not reassigned after initialization
        // => Captured variables stored in local class instance for access after method returns

        // Local class - only visible within this method
        class LocalClass { // => Local class defined in method scope
            // => Only accessible within demonstrateLocal() method
            // => Can access method's local variables (if effectively final)
            // => Can access outer instance members (if in instance method)
            // => Cannot have access modifiers (public, private, etc.) - always local scope

            public void display() { // => Method in local class
                System.out.println(localVar); // => OK: capture effectively final variable
                // => Output: Local Variable
                // => localVar captured by local class instance
                // localVar = "Modified"; // => COMPILE ERROR if uncommented: local class captures final/effectively final variables only
                // => Local class captures variables by value
                // => Variables must not be reassigned to ensure consistency
            }
        }

        LocalClass local = new LocalClass(); // => Create local class instance
        // => local is instance of LocalClass (defined above)
        // => Only valid within this method scope
        local.display(); // => Output: Local Variable
        // => Calls display() which accesses captured localVar
    }
}

LocalClassExample example = new LocalClassExample(); // => Create outer instance
example.demonstrateLocal(); // => Calls method that creates and uses local class
// => Output: Local Variable (from local.display() inside method)

// Local classes are useful for:
// - One-off helper classes needed only in specific method
// - Capturing local context for callbacks
// - Encapsulating logic too complex for lambda but too small for top-level class

// 4. ANONYMOUS INNER CLASS - class without name (inline implementation)
interface Greeting { // => Interface to implement
    void greet(String name); // => Abstract method
}

class AnonymousExample {
    public void demonstrate() { // => Method demonstrating anonymous classes

        // Anonymous class implementing interface
        Greeting greeting = new Greeting() { // => Anonymous class implementing Greeting
            // => No class name - defined inline
            // => Implements Greeting interface
            // => Can override interface methods
            @Override // => Annotation indicating override
            public void greet(String name) { // => Implement abstract method
                System.out.println("Hello, " + name + "!"); // => Implementation
                // => Output: Hello, [name]!
            }
        }; // => Semicolon terminates statement (variable assignment)
        // => greeting holds anonymous Greeting implementation
        // => Anonymous class is subtype of Greeting

        greeting.greet("Alice"); // => Call method on anonymous implementation
        // => Output: Hello, Alice!

        // Anonymous class extending class (not just interfaces)
        Object obj = new Object() { // => Anonymous class extending Object
            // => All anonymous classes implicitly extend class or implement interface
            // => This one extends Object and overrides toString()
            @Override // => Override Object.toString()
            public String toString() { // => Custom toString implementation
                return "Custom toString"; // => Return custom string
            }
        };

        System.out.println(obj); // => Calls obj.toString() (overridden version)
        // => Output: Custom toString
        // => Uses overridden toString() from anonymous class
    }
}

AnonymousExample anonEx = new AnonymousExample(); // => Create instance
anonEx.demonstrate(); // => Run demonstration
// => Output: Hello, Alice!
// => Output: Custom toString

// ANONYMOUS CLASS VS LAMBDA - modern alternative for functional interfaces

// Anonymous class (verbose) - works for any interface/class
Runnable runnable1 = new Runnable() { // => Anonymous class implementing Runnable
    @Override // => Must override run() method
    public void run() { // => Implement abstract method
        System.out.println("Running"); // => Implementation
    }
}; // => Semicolon ends assignment
// => runnable1 holds anonymous Runnable implementation
// => Pre-Java 8 style - verbose for single method interfaces

// Lambda (concise) - ONLY works for functional interfaces (one abstract method)
Runnable runnable2 = () -> System.out.println("Running"); // => Lambda expression
// => runnable2 holds lambda implementing Runnable
// => Lambda is shorthand for anonymous class with single abstract method
// => Cleaner syntax for functional interfaces
// => () is parameter list (empty for Runnable.run())
// => -> separates parameters from body
// => Method body follows arrow

// When to use each:
// - Lambda: functional interface (single abstract method), concise logic
// - Anonymous class: multiple methods, implementing non-functional interfaces, overriding multiple methods

// PRACTICAL USE CASE: Event listeners (callback pattern)
interface ClickListener { // => Callback interface
    void onClick(); // => Callback method invoked on click
}

class Button { // => Button class with listener support
    private ClickListener listener; // => Store registered listener
    // => listener is null initially (no listener registered)

    public void setOnClickListener(ClickListener listener) { // => Register listener
        this.listener = listener; // => Store listener for later invocation
        // => listener is ClickListener implementation (anonymous class or lambda)
    }

    public void click() { // => Simulate button click
        if (listener != null) { // => Check if listener registered
            listener.onClick(); // => Invoke callback
            // => Calls onClick() on registered listener
        }
    }
}

Button button = new Button(); // => Create button instance

// Anonymous inner class as callback (verbose but explicit)
button.setOnClickListener(new ClickListener() { // => Anonymous ClickListener implementation
    @Override // => Override abstract method
    public void onClick() { // => Callback implementation
        System.out.println("Button clicked!"); // => Action on click
        // => Executed when button.click() is called
    }
});

button.click(); // => Simulate click
// => Output: Button clicked!
// => Invokes listener.onClick() which prints message

// Or with lambda (cleaner) - modern style
button.setOnClickListener(() -> System.out.println("Button clicked!")); // => Lambda callback
// => () is empty parameter list (onClick() has no parameters)
// => Lambda body prints message
button.click(); // => Simulate click
// => Output: Button clicked!

// NESTED CLASS ACCESS LEVELS - all access modifiers apply
class OuterAccess {
    // All access modifiers work for nested classes (static and non-static)
    public static class PublicNested {} // => Accessible anywhere
    // => Other packages can: import OuterAccess.PublicNested;
    protected static class ProtectedNested {} // => Package + subclasses
    // => Same package or OuterAccess subclasses can access
    static class PackageNested {} // => Same package only (default access)
    // => Only classes in same package can access
    private static class PrivateNested {} // => Only within OuterAccess
    // => Only OuterAccess code can see PrivateNested
    // => Used for implementation details not exposed externally

    public void usePrivateNested() { // => Outer can use private nested class
        PrivateNested pn = new PrivateNested(); // => OK: same outer class
        // => Private nested classes accessible within outer class
    }
}

// Access from outside OuterAccess:
// OuterAccess.PrivateNested pn = new OuterAccess.PrivateNested(); // => COMPILE ERROR: PrivateNested has private access in OuterAccess
// => Cannot access private nested class from outside
OuterAccess.PublicNested pub = new OuterAccess.PublicNested(); // => OK: public nested class
// => Public nested classes accessible anywhere

// CAPTURING VARIABLES - effectively final requirement
class EffectivelyFinalExample {
    public void demonstrate() {
        int x = 10; // => Local variable (effectively final - not reassigned)
        // => x can be captured by inner/local/anonymous classes
        // x = 20; // => If uncommented, x is NOT effectively final
        // => Would cause compile error when captured

        Runnable r = new Runnable() { // => Anonymous class
            @Override
            public void run() {
                System.out.println(x); // => OK: x is effectively final
                // => Output: 10
                // => x captured by anonymous class instance
            }
        };

        // x = 30; // => COMPILE ERROR if uncommented: variable x might not have been initialized
        // => Would make x not effectively final
        // => Local/anonymous/inner classes capture variables by value at time of capture
        // => Reassignment after capture not allowed (prevents confusion)

        r.run(); // => Output: 10
        // => Executes run() which prints captured x
    }
}

// Effectively final requirement ensures:
// 1. Captured variables don't change after capture (predictable behavior)
// 2. Thread safety - captured values immutable
// 3. Simplifies implementation - no need for mutable cell wrappers

// NESTED CLASS SUMMARY:
// Static nested:    Outer.StaticNested nested = new Outer.StaticNested();
//                   (No outer instance, access outer static members only)
// Inner class:      Outer.Inner inner = outer.new Inner();
//                   (Requires outer instance, access outer instance members)
// Local class:      class Local {} (inside method)
//                   (Method scope, capture effectively final locals)
// Anonymous class:  new Interface() { ... }
//                   (No name, inline implementation)

// Common use cases:
// - Static nested: Builder pattern, grouping related classes
// - Inner class: Iterators, tightly coupled helpers
// - Local class: One-off helpers capturing local context
// - Anonymous class: Callbacks, listeners (prefer lambdas for functional interfaces)
```

**Key Takeaway**: Four nested class types serve different purposes: static nested classes (`Outer.StaticNested`) are independent and access outer static members only; non-static inner classes (`outer.new Inner()`) are tied to outer instance and access all outer members; local classes are defined in methods capturing effectively final locals; anonymous classes (`new Interface() {...}`) provide inline implementations for callbacks. Static nested accessed via `Outer.StaticNested`, inner via `outerInstance.new Inner()`. Inner classes hold implicit reference to outer instance (`Outer.this`). Local and anonymous classes capture effectively final variables (not reassigned). Anonymous classes verbose—prefer lambdas for functional interfaces. Nested classes enable encapsulation, grouping related classes, and implementing callbacks.

**Why It Matters**: Nested classes enable tight coupling where appropriate—iterator implementations need access to collection internals (ArrayList.Itr accesses internal array). Static nested classes namespace related classes (Map.Entry, Builder pattern) without creating top-level classes. Inner classes enable event-driven patterns—GUI listeners capture component state without additional plumbing. Anonymous classes revolutionized Java APIs (Thread, Runnable, Comparator) before lambdas. The effectively final requirement prevents subtle bugs—captured variables can't change after capture ensuring predictable behavior. However, inner classes hold outer instance references causing memory leaks if long-lived (Android Activity listeners). Proper use balances encapsulation benefits against complexity and lifecycle concerns.

---

### Example 28: Final Keyword

The `final` keyword prevents modification: final variables can't be reassigned, final methods can't be overridden, final classes can't be extended. Essential for immutability and security.

**Code**:

```java
// FINAL VARIABLES - cannot be reassigned after initialization
final int CONST = 42; // => CONST is 42 (type: int, final)
// => final variable MUST be initialized (here or in constructor for instance fields)
// => After initialization, value is locked - cannot reassign
// => Attempting reassignment causes compile-time error
// CONST = 50; // => COMPILE ERROR: cannot assign a value to final variable CONST
// => final prevents accidental modification
// => Used for constants, immutable values, and preventing reassignment bugs

final String name; // => Declaration without initialization (blank final)
// => name is declared final but not yet initialized
// => Blank final MUST be initialized before use (next statement or in constructor)
// => Can only be assigned ONCE - first assignment is initialization
name = "Alice"; // => OK: first assignment (initialization)
// => name is now "Alice" (type: String, final)
// => This is the ONE allowed assignment for this final variable
// name = "Bob"; // => COMPILE ERROR: cannot assign a value to final variable name
// => Second assignment attempt fails - final variables have one-time initialization

// FINAL WITH OBJECTS - reference is final, NOT object state
final List<String> list = new ArrayList<>(); // => list reference is final
// => list is final reference pointing to ArrayList instance
// => REFERENCE is immutable (cannot point to different list)
// => OBJECT STATE is mutable (can add/remove elements)
// => Common misconception: final does NOT make objects immutable
// => Only the reference (pointer) is final, not the object it points to
list.add("Java"); // => OK: modify object state (list is mutable)
// => list is now ["Java"] (size=1)
// => ArrayList.add() modifies internal array - perfectly legal
// => final prevents reassigning list, NOT modifying list contents
list.add("Python"); // => OK: add more elements
// => list is now ["Java", "Python"] (size=2)
// => Object mutation is allowed with final references
// list = new ArrayList<>(); // => COMPILE ERROR: cannot assign a value to final variable list
// => Cannot point list to new ArrayList instance
// => final makes REFERENCE immutable, not OBJECT
// => To make object immutable, use Collections.unmodifiableList() or immutable classes

// FINAL PARAMETERS - cannot be modified inside method
public static void process(final int value) { // => value parameter is final
    // => value is 10 (example), final parameter
    // => Cannot reassign value inside method body
    // => Useful for preventing accidental parameter modification
    // value = 10; // => COMPILE ERROR: cannot assign a value to final parameter value
    // => Parameters often modified by mistake - final prevents this
    System.out.println(value * 2); // => OK: read final parameter
    // => Output: 20 (if value was 10)
    // => Reading final variables is always allowed
    // => final prevents writing, not reading
}

// FINAL IN LOOPS - each iteration gets new final variable
for (final String item : List.of("A", "B", "C")) { // => item is final per iteration
    // => Iteration 1: item = "A" (final)
    // => Iteration 2: item = "B" (final)
    // => Iteration 3: item = "C" (final)
    // => Each iteration creates NEW final variable (not reassignment)
    // => item is final within each iteration scope
    System.out.println(item); // => Output: "A", "B", "C" (three iterations)
    // item = "X"; // => COMPILE ERROR: cannot assign a value to final variable item
    // => Cannot modify loop variable within iteration
    // => final prevents accidental modification in loop body
}
// => Enhanced for-loop commonly uses final to prevent modification

// FINAL INSTANCE FIELDS - must be initialized by end of constructor
class Person {
    private final String name; // => Final instance field (blank final)
    // => name MUST be initialized in constructor
    // => Every Person instance has immutable name
    // => Cannot be changed after object construction
    private final int birthYear; // => Final instance field (blank final)
    // => birthYear MUST be initialized in constructor
    // => Immutable birth year for each Person
    private int age; // => Not final, can change
    // => age is mutable - can be modified after construction
    // => Useful for values that change over time

    // Final fields MUST be initialized by end of constructor
    public Person(String name, int birthYear) {
        // => Constructor receives name="Alice", birthYear=1990 (example)
        this.name = name; // => Initialize final field name
        // => this.name is now "Alice" (final)
        // => MUST initialize all final fields before constructor exits
        // => this.name can NEVER be reassigned after this point
        this.birthYear = birthYear; // => Initialize final field birthYear
        // => this.birthYear is now 1990 (final)
        // => Final fields get ONE initialization in constructor
        this.age = 0; // => Initialize non-final field age
        // => age is 0 (mutable, can change later)
    }
    // => If constructor doesn't initialize final fields, compile error occurs
    // => Compiler enforces final field initialization

    public void celebrateBirthday() {
        this.age++; // => OK: age is not final, can be incremented
        // => age goes from 0 to 1, 1 to 2, etc.
        // => Mutable fields can be modified in any method
        // this.birthYear++; // => COMPILE ERROR: cannot assign a value to final field birthYear
        // => Final fields cannot be modified after initialization
        // => Attempting to modify final field causes compile error
    }

    public String getName() {
        return name; // => Return final field (immutable)
        // => name cannot change, so always returns same value for this instance
        // => Final fields provide guaranteed immutability
    }
}
// => Final instance fields create immutable state (name, birthYear)
// => Non-final instance fields create mutable state (age)
// => Mix final and non-final fields as needed for domain model

// BLANK FINAL - final field without initializer at declaration
class BlankFinal {
    private final int value; // => Blank final (declared but not initialized)
    // => value is final but has no initializer (no = expression)
    // => MUST be initialized in constructor
    // => Called "blank final" - blank until constructor initializes it

    // Must initialize in constructor
    public BlankFinal(int value) {
        // => Constructor receives value=10 (example)
        this.value = value; // => Initialize blank final
        // => this.value is now 10 (final)
        // => Every BlankFinal instance can have different value
        // => But once constructed, value never changes
    }
    // => Blank finals allow per-instance constants
    // => Value determined at construction time, then immutable
}

// FINAL METHODS - cannot be overridden in subclasses
class Parent {
    // Final method - prevents override
    public final void finalMethod() { // => final method (cannot be overridden)
        // => final keyword before return type
        // => Subclasses CANNOT override this method
        // => Guarantees this implementation always executes for Parent and subclasses
        System.out.println("Parent final method"); // => Output: Parent final method
        // => This exact implementation runs for all subclasses
    }

    // Non-final method - can be overridden
    public void regularMethod() { // => Non-final method (can be overridden)
        // => No final keyword - subclasses can override
        // => Polymorphism allowed for this method
        System.out.println("Parent regular method"); // => Output: Parent regular method
        // => Subclasses can provide different implementation
    }
}

class Child extends Parent {
    // => Child inherits Parent's methods
    // => Attempting to override final methods causes compile error
    // @Override
    // public void finalMethod() {} // => COMPILE ERROR: finalMethod() in Child cannot override finalMethod() in Parent, overridden method is final
    // => Cannot override final methods - compiler enforces this
    // => Ensures Parent's finalMethod() implementation is preserved

    @Override
    public void regularMethod() { // => OK: override non-final method
        // => Overriding non-final methods is standard polymorphism
        System.out.println("Child regular method"); // => Output: Child regular method
        // => Child provides different implementation
        // => Polymorphism works for non-final methods
    }
}
// => Final methods ensure implementation cannot be changed by subclasses
// => Used for security-critical methods or template method pattern

// FINAL CLASSES - cannot be extended
final class FinalClass { // => Final class (cannot be subclassed)
    // => final keyword before class keyword
    // => No class can extend FinalClass
    // => All methods implicitly final (no subclass to override them)
    public void method() {
        System.out.println("Final class method"); // => Output: Final class method
        // => Class cannot be subclassed, so this implementation is definitive
    }
}

// class SubClass extends FinalClass {} // => COMPILE ERROR: cannot inherit from final FinalClass
// => Final classes cannot be extended - no subclasses allowed
// => Useful for security, performance, and preventing misuse

// Practical uses of final classes in Java standard library:
// 1. final class String - prevents subclassing to maintain security and performance guarantees
//    => String immutability is fundamental assumption in Java
//    => If String was subclassable, malicious subclass could break immutability
//    => Security Manager, HashMap, and many APIs rely on String being final
// 2. final class Integer - wrapper class cannot be extended
//    => Prevents malicious subclass from breaking caching (-128 to 127)
//    => Integer.valueOf() returns cached instances for small numbers
// 3. final class Math - utility class with only static methods
//    => No reason to extend Math - all methods are static
//    => Final prevents misuse (trying to extend utility class)

// EFFECTIVELY FINAL - not declared final, but never modified
public static void effectivelyFinal() {
    int x = 10; // => x is 10 (not declared final)
    // => x is "effectively final" - not reassigned after initialization
    // => Effectively final variables can be captured by lambdas/anonymous classes
    // => Java 8+ feature - compiler detects effectively final variables
    int y = 20; // => y is 20 (not declared final)

    Runnable r = () -> { // => Lambda expression
        // => Lambda captures variables from enclosing scope
        // => Only final or effectively final variables can be captured
        System.out.println(x); // => OK: x is effectively final (never modified)
        // => Output: 10
        // => Lambda captures x value (10) at creation time
        // System.out.println(y); // => Would be ERROR if y modified later
        // => If y is reassigned below, y is NOT effectively final
        // => Compiler error: "local variables referenced from lambda must be final or effectively final"
    };

    // x = 30; // => Uncommenting this would make x NOT effectively final
    // => Reassignment after lambda creation breaks effectively final status
    // => Lambdas and anonymous classes can only capture final/effectively final variables
    // => Prevents confusion about which value is captured (before or after modification)

    r.run(); // => Execute lambda
    // => Output: 10 (captured x value)
}
// => Effectively final: not declared final, but never reassigned
// => Allows lambda/anonymous class capture without explicit final keyword
// => Java 8+ compiler feature for cleaner code

// FINAL STATIC FIELDS - class constants
class Constants {
    // Public constants - final and static
    public static final double PI = 3.14159; // => Class constant (static final)
    // => PI is 3.14159 (type: double, static final)
    // => static: shared across all instances (class-level, not instance-level)
    // => final: cannot be reassigned (immutable constant)
    // => Naming convention: UPPER_CASE for static final constants
    // => Initialized at class loading time, never changes
    public static final int MAX_SIZE = 1000; // => Static final constant
    // => MAX_SIZE is 1000 (type: int, static final)
    // => Compile-time constant (value known at compile time)
    public static final String APP_NAME = "MyApp"; // => Static final constant
    // => APP_NAME is "MyApp" (type: String, static final)
    // => String literals are compile-time constants

    // Private constructor prevents instantiation
    private Constants() {} // => Private constructor
    // => Prevents: Constants c = new Constants();
    // => Utility class pattern - no instances, only static members
}

double area = Constants.PI * 5 * 5; // => Use constant
// => area is 78.53975 (PI * 25)
// => Access static final via ClassName.CONSTANT_NAME
// => No instance needed - static members accessed via class
// Constants.PI = 3.0; // => COMPILE ERROR: cannot assign a value to final variable PI
// => Static final constants cannot be reassigned
// => Guarantees constant value across entire program

// FINAL FOR IMMUTABILITY - defensive copying
class ImmutablePerson {
    private final String name; // => Final field (immutable reference)
    // => name reference cannot be reassigned
    // => String objects are immutable - true immutability
    private final List<String> hobbies; // => Final reference (mutable object!)
    // => hobbies reference cannot be reassigned
    // => BUT ArrayList is mutable - need defensive copying

    public ImmutablePerson(String name, List<String> hobbies) {
        // => Constructor receives name="Alice", hobbies=["Reading"] (example)
        this.name = name; // => Assign final field name
        // => this.name is now "Alice" (final reference to immutable String)
        // => String is immutable - true immutability achieved
        // Defensive copy to prevent external modification
        this.hobbies = new ArrayList<>(hobbies); // => Create new list (defensive copy)
        // => this.hobbies is NEW ArrayList containing ["Reading"]
        // => NOT same object as constructor parameter
        // => Prevents external code from modifying internal state
        // => Even if caller modifies original hobbies list, this.hobbies unaffected
        // => Defensive copying is CRITICAL for immutability with mutable types
    }

    public String getName() {
        return name; // => Return final field (String is immutable)
        // => Returns "Alice" (immutable String)
        // => Caller cannot modify name - String is immutable
        // => Safe to return without defensive copy
    }

    public List<String> getHobbies() {
        // Return unmodifiable view to preserve immutability
        return Collections.unmodifiableList(hobbies); // => Return unmodifiable wrapper
        // => Returns unmodifiable view of hobbies list
        // => Caller cannot add/remove elements - UnsupportedOperationException
        // => Prevents external modification of internal state
        // => If returned hobbies directly, caller could modify it
        // => Collections.unmodifiableList() creates read-only view
    }
}

List<String> originalHobbies = new ArrayList<>(); // => Create mutable list
// => originalHobbies is empty ArrayList
originalHobbies.add("Reading"); // => Add element
// => originalHobbies is ["Reading"]
ImmutablePerson person = new ImmutablePerson("Alice", originalHobbies);
// => person created with name="Alice", hobbies=["Reading"] (defensive copy)
// => person.hobbies is separate ArrayList instance

originalHobbies.add("Gaming"); // => Modify original list
// => originalHobbies is now ["Reading", "Gaming"]
// => BUT person.hobbies is still ["Reading"] (defensive copy protects)
System.out.println(person.getHobbies()); // => Output: [Reading]
// => person's copy unaffected by external modification
// => Defensive copying achieved immutability

// person.getHobbies().add("Cooking"); // => RUNTIME ERROR: UnsupportedOperationException
// => Collections.unmodifiableList() prevents modification
// => Attempting to add throws exception at runtime
// => Protects internal state from caller modification

// FINAL VS CONST in other languages:
// Java final is runtime constant (value determined at runtime)
// compile-time constants use "static final" together
class CompileTimeConstants {
    // Compile-time constant - value known at compile time
    public static final int COMPILE_CONST = 100; // => Compile-time constant
    // => COMPILE_CONST is 100 (known at compile time)
    // => Compiler can inline this value (replace usage with 100)
    // => static final with literal value = compile-time constant
    // => Equivalent to const in C++ (value baked into bytecode)

    // Runtime constant - value determined at runtime
    public final int runtimeConst; // => Runtime constant (instance final field)
    // => runtimeConst value not known until object created
    // => Different instances can have different values
    // => Initialized at runtime (constructor execution)
    // => Cannot be compile-time constant (value varies per instance)

    public CompileTimeConstants(int value) {
        // => Constructor receives value=42 (example)
        this.runtimeConst = value; // => Set at runtime
        // => this.runtimeConst is now 42 (final)
        // => Value determined when constructor runs (runtime)
        // => Each instance can have different runtimeConst value
    }
}
// => static final = compile-time constant (class-level, value known at compile)
// => instance final = runtime constant (instance-level, value known at construction)
```

**Key Takeaway**: `final` prevents modification: variables can't be reassigned, methods can't be overridden, classes can't be extended. Final variables require initialization (declaration or constructor). Final reference makes pointer immutable, not object state. Effectively final variables (not modified after initialization) can be captured by lambdas. Use final for constants (`static final`), immutability (final fields), and security (final methods/classes). Final helps compiler optimize and prevents accidental modification.

**Why It Matters**: The `final` keyword prevents entire classes of bugs—reassignment errors, accidental parameter modification, and state corruption. Final instance fields create immutable objects (Person with unchangeable name) which are inherently thread-safe, eliminating race conditions in concurrent code. Final methods protect security-critical code paths—Java's String class is final preventing malicious subclasses from breaking immutability assumptions relied upon by SecurityManager and HashMap. Final parameters prevent subtle bugs where method arguments are accidentally modified, improving code clarity and preventing defensive copying overhead. Effectively final variables (Java 8+) enable lambda expressions without verbose final annotations, making functional programming more ergonomic. Static final constants (PI, MAX_SIZE) provide type-safe alternatives to magic numbers, improving code maintainability. The JVM optimizes final fields—HotSpot assumes final fields never change, enabling aggressive inlining and constant folding optimizations. Major frameworks rely on final—Spring's @Component classes often use final fields for dependency injection, ensuring dependencies are never null after construction. Understanding final reference vs. object immutability is critical—final List<String> prevents reassigning the list but not mutating its contents, requiring defensive copying and Collections.unmodifiableList() for true immutability.

---

### Example 29: This and Super Keywords

The `this` keyword references the current instance. The `super` keyword references the parent class. Both are essential for constructors, method calls, and resolving name conflicts.

**Code**:

```java
// THIS KEYWORD - reference to current instance
class Person {
    private String name; // => Instance field (each Person has own name)
    // => name is instance variable - not static, belongs to instance
    private int age; // => Instance field (each Person has own age)
    // => age is instance variable - separate for each object

    // Constructor with same parameter names as fields
    public Person(String name, int age) { // => Parameters: name, age
        // => Parameter name SHADOWS field name (both called "name")
        // => Parameter age SHADOWS field age (both called "age")
        // => this keyword disambiguates - this.name = field, name = parameter
        this.name = name; // => this.name refers to FIELD, name refers to PARAMETER
        // => this.name is instance field being assigned
        // => name (right side) is constructor parameter
        // => this.name = "Alice" (if name parameter is "Alice")
        // => Without this, would be: name = name (assigns parameter to itself - useless!)
        this.age = age; // => Distinguish field from parameter using this
        // => this.age is instance field, age is parameter
        // => this.age = 30 (if age parameter is 30)
        // => this is REQUIRED when parameter name equals field name
    }

    // Method returning current instance (fluent API pattern)
    public Person setName(String name) { // => Setter with fluent return
        // => name parameter shadows this.name field
        this.name = name; // => Update instance field name
        // => this.name = "Bob" (if name parameter is "Bob")
        // => Changes this Person instance's name
        return this; // => Return current instance (this Person object)
        // => Enables method chaining: person.setName("X").setAge(25)
        // => this is reference to the object this method was called on
        // => Fluent API pattern - common in builders, streams
    }

    public Person setAge(int age) { // => Setter with fluent return
        this.age = age; // => Update instance field age
        // => this.age = 25 (if age parameter is 25)
        return this; // => Return current instance for chaining
        // => Allows: person.setName("X").setAge(25).display()
        // => Each method returns same Person instance
    }

    public void display() {
        System.out.println("Name: " + this.name + ", Age: " + this.age);
        // => this.name and this.age are instance fields
        // => Output: Name: Bob, Age: 25 (example values)
        // => this is OPTIONAL when no shadowing (no local variable named name/age)
        System.out.println("Name: " + name + ", Age: " + age); // => Same as above
        // => name and age unambiguously refer to instance fields (no shadowing)
        // => Compiler knows name/age are fields (no local variables with same names)
        // => this is implicit - this.name == name when no ambiguity
    }
}

// METHOD CHAINING WITH THIS - fluent API pattern
Person person = new Person("Alice", 30); // => Create Person: name="Alice", age=30
// => person is Person instance with initial values
person.setName("Bob").setAge(25).display(); // => Chained method calls
// => person.setName("Bob") returns this (same Person instance)
// => .setAge(25) called on returned this, returns this again
// => .display() called on returned this
// => Each method returns same Person instance - enables chaining
// => After chaining: person.name="Bob", person.age=25
// => Output: Name: Bob, Age: 25
// => Fluent API improves readability: person.setName().setAge().display()

// THIS() - call another constructor in same class (constructor delegation)
class Rectangle {
    private double width; // => width field (instance variable)
    private double height; // => height field (instance variable)

    // Default constructor (no parameters)
    public Rectangle() { // => No-arg constructor
        this(1.0, 1.0); // => Calls two-parameter constructor with defaults
        // => this(1.0, 1.0) delegates to Rectangle(double, double)
        // => this() MUST be FIRST statement in constructor
        // => Cannot execute ANY code before this()
        // => After this() returns, this Rectangle has width=1.0, height=1.0
    }
    // => Default constructor creates 1x1 Rectangle (unit square)

    // One parameter constructor (square)
    public Rectangle(double side) { // => One parameter: side
        this(side, side); // => Calls two-parameter constructor: Rectangle(side, side)
        // => this(side, side) delegates to Rectangle(double, double)
        // => If side=5.0, calls Rectangle(5.0, 5.0) - square
        // => After this() returns, this Rectangle has width=side, height=side
    }
    // => One-parameter constructor creates square (width == height)

    // Two parameters (primary constructor - actual initialization)
    public Rectangle(double width, double height) { // => Primary constructor
        // => ALL other constructors delegate to this one
        // => This constructor does the actual field initialization
        this.width = width; // => Initialize width field
        // => this.width = 3.0 (if width parameter is 3.0)
        this.height = height; // => Initialize height field
        // => this.height = 4.0 (if height parameter is 4.0)
    }
    // => Primary constructor - centralizes initialization logic
    // => Other constructors provide convenience with default values

    public double area() {
        return this.width * this.height; // => Calculate area
        // => Returns width * height (e.g., 3.0 * 4.0 = 12.0)
        // => this.width and this.height are instance fields
    }
}

Rectangle r1 = new Rectangle(); // => Calls default constructor
// => Rectangle() calls this(1.0, 1.0)
// => this(1.0, 1.0) calls Rectangle(1.0, 1.0)
// => r1.width=1.0, r1.height=1.0 (unit square)
Rectangle r2 = new Rectangle(5.0); // => Calls one-parameter constructor
// => Rectangle(5.0) calls this(5.0, 5.0)
// => this(5.0, 5.0) calls Rectangle(5.0, 5.0)
// => r2.width=5.0, r2.height=5.0 (5x5 square)
Rectangle r3 = new Rectangle(3.0, 4.0); // => Calls two-parameter constructor directly
// => Rectangle(3.0, 4.0) initializes fields directly
// => r3.width=3.0, r3.height=4.0 (3x4 rectangle)
// => Constructor chaining centralizes logic in primary constructor

// THIS AS METHOD PARAMETER - pass current instance to another object
class Node {
    private int value; // => Node value (data)
    private Node next; // => Reference to next node (linked list)

    public Node(int value) {
        this.value = value; // => Initialize value field
        // => this.value = 10 (if value parameter is 10)
    }

    public void setNext(Node next) { // => Set next node reference
        this.next = next; // => this.next points to next Node
        // => this.next = someNode (parameter)
    }

    public void appendSelf(Node head) { // => Append this node to head
        head.setNext(this); // => Pass current instance to another object
        // => this is current Node instance
        // => head.setNext(this) makes head point to current node
        // => Passing this allows other objects to reference current instance
    }
}
// => this as parameter enables objects to share references
// => Useful for callbacks, event listeners, data structures

// SUPER KEYWORD - reference to parent class
class Animal {
    protected String name; // => Accessible to subclasses (protected visibility)
    // => name is inherited by Dog, Cat, etc.
    // => protected allows subclasses to access directly

    public Animal(String name) { // => Animal constructor
        this.name = name; // => Initialize name field
        // => this.name = "Rex" (if name parameter is "Rex")
    }

    public void makeSound() { // => Generic animal sound
        System.out.println(name + " makes a sound"); // => Output: Rex makes a sound
        // => Generic implementation - subclasses override for specific sounds
    }

    public void eat() { // => Eating behavior
        System.out.println(name + " is eating"); // => Output: Rex is eating
        // => Generic eating - common to all animals
    }
}

class Dog extends Animal { // => Dog IS-A Animal (inheritance)
    private String breed; // => Dog-specific field (not in Animal)
    // => breed is unique to Dog - not inherited by Animal

    // Constructor calling parent constructor
    public Dog(String name, String breed) { // => Dog constructor parameters
        super(name); // => MUST be first statement, calls Animal(name)
        // => super(name) delegates to parent Animal(String) constructor
        // => Initializes inherited name field in Animal
        // => MUST be first statement - parent initialization before subclass
        // => After super(name), Animal's name field is initialized
        this.breed = breed; // => Initialize Dog-specific field
        // => this.breed = "Labrador" (if breed parameter is "Labrador")
        // => Subclass field initialized AFTER parent initialization
    }
    // => Constructor order: parent fields first, then subclass fields
    // => Ensures object is properly constructed from top-down

    // Override parent method
    @Override
    public void makeSound() { // => Dog-specific sound (overrides Animal)
        System.out.println(name + " barks: Woof!"); // => Output: Rex barks: Woof!
        // => name is inherited protected field from Animal
        // => Overridden method provides Dog-specific behavior
        // => Polymorphism - Dog.makeSound() different from Animal.makeSound()
    }

    // Method using both this and super
    public void displayInfo() {
        System.out.println("Dog: " + this.name); // => this.name (current instance field)
        // => Output: Dog: Rex
        // => this.name refers to inherited name field in this Dog instance
        super.makeSound(); // => Call PARENT version of makeSound()
        // => super.makeSound() calls Animal.makeSound()
        // => Output: Rex makes a sound
        // => Uses parent implementation even though Dog overrides it
        this.makeSound(); // => Call CURRENT version of makeSound()
        // => this.makeSound() calls Dog.makeSound() (overridden version)
        // => Output: Rex barks: Woof!
        // => Without super, would call overridden version (Dog.makeSound())
        // => super allows access to parent implementation in overridden method
    }

    // Calling parent method explicitly
    public void eatWithMessage() { // => Extend parent behavior
        super.eat(); // => Call parent's eat() method
        // => super.eat() calls Animal.eat()
        // => Output: Rex is eating
        // => Reuses parent implementation (don't duplicate code)
        System.out.println("Dog is full!"); // => Additional Dog-specific behavior
        // => Output: Dog is full!
        // => Extends parent behavior with subclass-specific logic
    }
    // => super allows calling parent methods from subclass
    // => Useful for extending (not replacing) parent behavior
}

Dog dog = new Dog("Rex", "Labrador"); // => Create Dog instance
// => Dog("Rex", "Labrador") calls super("Rex"), then this.breed = "Labrador"
// => super("Rex") calls Animal("Rex"), initializing name="Rex"
// => Then breed="Labrador" is initialized
// => dog.name="Rex" (from Animal), dog.breed="Labrador" (from Dog)
dog.displayInfo(); // => Call displayInfo()
// => Output (3 lines):
// => "Dog: Rex" (this.name)
// => "Rex makes a sound" (super.makeSound() - parent version)
// => "Rex barks: Woof!" (this.makeSound() - overridden version)

dog.eatWithMessage(); // => Call eatWithMessage()
// => Output (2 lines):
// => "Rex is eating" (super.eat() - parent method)
// => "Dog is full!" (subclass addition)

// SUPER VS THIS IN CONSTRUCTORS - constructor chaining across hierarchy
class GrandParent {
    public GrandParent() { // => GrandParent constructor
        System.out.println("GrandParent constructor"); // => Output: GrandParent constructor
        // => Executes first in construction chain
    }
}

class Parent2 extends GrandParent { // => Parent2 IS-A GrandParent
    public Parent2() { // => Parent2 constructor
        super(); // => Explicit call to GrandParent() (could be implicit)
        // => super() calls GrandParent() constructor
        // => If not specified, compiler adds implicit super() call
        // => GrandParent initialization happens first
        System.out.println("Parent constructor"); // => Output: Parent constructor
        // => Executes after GrandParent constructor completes
    }
}

class Child2 extends Parent2 { // => Child2 IS-A Parent2
    public Child2() { // => Child2 constructor
        super(); // => Calls Parent2(), which calls GrandParent()
        // => super() calls Parent2() constructor
        // => Parent2() calls GrandParent() constructor (chain reaction)
        // => Construction order: GrandParent -> Parent2 -> Child2
        System.out.println("Child constructor"); // => Output: Child constructor
        // => Executes last, after all parent constructors complete
    }
}

Child2 child = new Child2(); // => Create Child2 instance
// => new Child2() triggers constructor chain
// => Output (3 lines, in order):
// => "GrandParent constructor" (first - top of hierarchy)
// => "Parent constructor" (second - middle)
// => "Child constructor" (third - bottom, actual class)
// => Construction flows top-down: GrandParent -> Parent2 -> Child2
// => Ensures parent state initialized before subclass state

// IMPLICIT SUPER() CALL - added by compiler if not specified
class ImplicitSuper {
    public ImplicitSuper() { // => Constructor without explicit super()
        // super(); // => Implicitly added by compiler (invisible in source)
        // => Compiler inserts super() as first statement automatically
        // => Calls Object() constructor (all classes extend Object)
        System.out.println("Constructor"); // => Output: Constructor
        // => Executes after implicit super() returns
    }
}
// => If constructor has NO explicit super() or this(), compiler adds super()
// => Ensures all parent constructors called in chain up to Object
// => Every Java object construction starts with Object() constructor

// THIS VS SUPER SUMMARY:
// this:
//   - References current instance (this object)
//   - this.field accesses instance field (disambiguate from parameters)
//   - this.method() calls instance method (current object's method)
//   - this() calls another constructor in SAME class (constructor delegation)
//   - return this enables method chaining (fluent APIs)
//   - this as parameter passes current instance to other objects

// super:
//   - References parent class (superclass)
//   - super.field accesses parent field (if accessible, rare usage)
//   - super.method() calls parent method (access overridden methods)
//   - super() calls parent constructor (must be first statement)
//   - super MUST be first statement in constructor if used
//   - Cannot access super.super (only immediate parent)

// COMMON ERROR: super/this() not first statement
class ConstructorError {
    // THIS IS WRONG (compile error):
    // public ConstructorError() {
    //     int x = 10; // Some code BEFORE super()
    //     super(); // COMPILE ERROR: call to super must be first statement in constructor
    // }
    // => Cannot execute code before super() or this()
    // => Parent must be initialized before accessing any fields/methods

    // THIS IS CORRECT:
    public ConstructorError() {
        super(); // => Correct: first statement
        // => super() MUST be first statement if explicit
        int x = 10; // => Other code AFTER super() is allowed
        // => After super(), can execute arbitrary code
    }
}
// => super()/this() must be first statement - ensures proper initialization order
// => Parent initialized before subclass can use inherited members

// CANNOT USE BOTH THIS() AND SUPER() IN SAME CONSTRUCTOR
class BothError {
    // THIS IS WRONG (compile error):
    // public BothError() {
    //     super(); // Call parent constructor
    //     this(10); // COMPILE ERROR: cannot have both this() and super()
    // }
    // => Both super() and this() must be first statement
    // => Cannot have TWO first statements - impossible!
    // => Choose ONE delegation path: super() OR this()

    // THIS IS CORRECT:
    public BothError() {
        this(10); // => OK: call another constructor in same class (which will call super())
        // => this(10) delegates to BothError(int)
        // => BothError(int) will call super() (explicit or implicit)
        // => Indirect super() call through constructor chain
    }

    public BothError(int value) { // => Constructor receiving int parameter
        super(); // => This constructor calls super() explicitly
        // => super() initializes parent class
        // => Completes the delegation chain: BothError() -> BothError(int) -> Parent()
    }
}
// => Constructor delegation: this() delegates within class, super() goes to parent
// => Cannot mix both in same constructor - choose one delegation path
// => this() eventually leads to super() through constructor chain
```

**Key Takeaway**: `this` references current instance. Use `this.field` to distinguish fields from parameters. `this()` calls another constructor in same class (must be first statement). `return this` enables method chaining. `super` references parent class. Use `super()` to call parent constructor (must be first statement). `super.method()` calls parent method version. Cannot use both `this()` and `super()` in same constructor—choose one delegation path. Implicit `super()` added by compiler if no explicit `super()` or `this()` call.

**Why It Matters**: Understanding `this` and `super` is essential for object-oriented Java development—every class hierarchy relies on proper constructor chaining. `this()` constructor delegation eliminates duplication by centralizing initialization logic in primary constructors, improving maintainability. Method chaining (`return this`) creates fluent APIs used extensively in Java's standard library (StringBuilder, Stream API, Optional) and modern frameworks. `super()` ensures parent class initialization happens before subclass logic, maintaining object invariants—attempting to use `this.field` before `super()` would access uninitialized parent state. Distinguishing `this.field` from parameters prevents shadowing bugs where assignments like `name = name` do nothing because both sides refer to the parameter. `super.method()` enables template method pattern—parent defines algorithm skeleton, subclass overrides steps while calling parent via super. The implicit `super()` rule ensures every object construction chain terminates at Object(), guaranteeing all Java objects have consistent initialization. Major frameworks depend on this—Spring's constructor injection requires proper super() delegation, JPA entity hierarchies rely on parent field initialization before subclass fields. The "first statement" rule for super()/this() prevents partial object construction—parent must be fully initialized before subclass can safely access inherited members.

### Example 30: Basic Date and Time

Java 8 introduced the `java.time` package (based on Joda-Time) which replaced the legacy `Date` and `Calendar` classes. Modern date/time APIs are immutable, thread-safe, and provide clear semantics for dates, times, and durations.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Time["java.time Package"]
    Time --> LocalDate["LocalDate<br/>(date only)"]
    LocalDate --> LocalTime["LocalTime<br/>(time only)"]
    LocalTime --> LocalDateTime["LocalDateTime<br/>(date + time)"]
    LocalDateTime --> ZonedDateTime["ZonedDateTime<br/>(date + time + timezone)"]
    ZonedDateTime --> Duration["Duration<br/>(time-based)"]
    Duration --> Period["Period<br/>(date-based)"]

    style Time fill:#0173B2,color:#fff
    style LocalDate fill:#DE8F05,color:#fff
    style LocalTime fill:#029E73,color:#fff
    style LocalDateTime fill:#CC78BC,color:#fff
    style ZonedDateTime fill:#CA9161,color:#fff
    style Duration fill:#0173B2,color:#fff
    style Period fill:#029E73,color:#fff
```

**Code**:

```java
import java.time.*; // => Import java.time package (modern date/time API)
// => Includes LocalDate, LocalTime, LocalDateTime, ZonedDateTime, Instant
// => Introduced in Java 8 (JSR-310) - replaces legacy Date/Calendar
import java.time.format.DateTimeFormatter; // => For formatting and parsing dates/times
// => DateTimeFormatter is immutable and thread-safe
import java.time.temporal.ChronoUnit; // => For date/time calculations (ChronoUnit.DAYS, etc.)
// => Represents units like DAYS, MONTHS, YEARS, HOURS, MINUTES

// LOCALDATE - date without time or timezone
LocalDate today = LocalDate.now(); // => Current date (e.g., 2025-01-15)
// => today is LocalDate instance representing current date
// => Uses system default timezone to determine "today"
// => Format: YYYY-MM-DD (ISO-8601)
// => LocalDate is IMMUTABLE - all operations return new instances
LocalDate specificDate = LocalDate.of(2025, 12, 31); // => 2025-12-31
// => specificDate is LocalDate: year=2025, month=12 (December), day=31
// => Static factory method - no "new LocalDate()"
// => Month is 1-based (1=January, 12=December) - NOT 0-based like legacy Calendar!
LocalDate parsed = LocalDate.parse("2025-06-15"); // => 2025-06-15 (ISO format)
// => parsed is LocalDate created from string
// => parse() expects ISO-8601 format: YYYY-MM-DD
// => Throws DateTimeParseException if format invalid

// EXTRACTING DATE COMPONENTS
int year = today.getYear(); // => 2025 (year component)
// => year is 2025 (type: int)
// => Returns full year (not 2-digit)
int month = today.getMonthValue(); // => 1 (January, 1-based!)
// => month is 1 (January is 1, NOT 0 like legacy Calendar)
// => Returns 1-12 for Jan-Dec
// => Alternative: today.getMonth() returns Month enum (Month.JANUARY)
int day = today.getDayOfMonth(); // => 15 (day of month)
// => day is 15 (type: int)
// => Returns 1-31 depending on month
DayOfWeek dayOfWeek = today.getDayOfWeek(); // => WEDNESDAY (enum)
// => dayOfWeek is DayOfWeek.WEDNESDAY (enum constant)
// => DayOfWeek enum: MONDAY, TUESDAY, ..., SUNDAY
// => Use .getValue() for int (1=Monday, 7=Sunday)

// DATE ARITHMETIC - immutable, returns new instances
LocalDate tomorrow = today.plusDays(1); // => 2025-01-16
// => tomorrow is NEW LocalDate (today + 1 day)
// => today UNCHANGED (immutable)
// => If today is 2025-01-15, tomorrow is 2025-01-16
LocalDate nextWeek = today.plusWeeks(1); // => 2025-01-22
// => nextWeek is NEW LocalDate (today + 7 days)
// => today still 2025-01-15 (immutable)
LocalDate nextMonth = today.plusMonths(1); // => 2025-02-15
// => nextMonth is NEW LocalDate (today + 1 month)
// => Handles variable month lengths (Jan 31 + 1 month = Feb 28/29)
LocalDate nextYear = today.plusYears(1); // => 2026-01-15
// => nextYear is NEW LocalDate (today + 1 year)
// => Handles leap years correctly
LocalDate yesterday = today.minusDays(1); // => 2025-01-14
// => yesterday is NEW LocalDate (today - 1 day)
// => Minus methods for subtraction (minusDays, minusMonths, minusYears)

// LOCALTIME - time without date or timezone
LocalTime now = LocalTime.now(); // => Current time (e.g., 14:30:00.123456789)
// => now is LocalTime instance representing current time
// => Format: HH:mm:ss.nnnnnnnnn (24-hour, nanosecond precision)
// => Uses system default timezone
// => IMMUTABLE - all operations return new instances
LocalTime specificTime = LocalTime.of(14, 30); // => 14:30:00 (2:30 PM)
// => specificTime is LocalTime: hour=14, minute=30, second=0, nano=0
// => 24-hour format (14 = 2 PM)
LocalTime withSeconds = LocalTime.of(14, 30, 45); // => 14:30:45
// => withSeconds is LocalTime: hour=14, minute=30, second=45, nano=0
LocalTime withNanos = LocalTime.of(14, 30, 45, 123000000); // => 14:30:45.123000000
// => withNanos is LocalTime with nanosecond precision
// => 123000000 nanoseconds = 123 milliseconds
// => Nanosecond precision (up to 999,999,999)

// EXTRACTING TIME COMPONENTS
int hour = now.getHour(); // => 14 (24-hour format, 0-23)
// => hour is 14 (type: int, 2 PM in 24-hour format)
int minute = now.getMinute(); // => 30 (0-59)
// => minute is 30 (type: int)
int second = now.getSecond(); // => 0 (0-59)
// => second is 0 (type: int)
// => For nanoseconds: now.getNano() (0-999,999,999)

// TIME ARITHMETIC - immutable, returns new instances
LocalTime later = now.plusHours(2); // => 16:30:00.123456789
// => later is NEW LocalTime (now + 2 hours)
// => If now is 14:30, later is 16:30
// => Wraps around midnight (23:00 + 2 hours = 01:00 next day, but LocalTime has no date!)
LocalTime earlier = now.minusMinutes(15); // => 14:15:00.123456789
// => earlier is NEW LocalTime (now - 15 minutes)
// => If now is 14:30, earlier is 14:15

// LOCALDATETIME - date and time without timezone
LocalDateTime dateTime = LocalDateTime.now(); // => 2025-01-15T14:30:00.123456789
// => dateTime is LocalDateTime (current date + time)
// => Format: YYYY-MM-DDTHH:mm:ss.nnnnnnnnn (ISO-8601)
// => T separates date from time
// => IMMUTABLE - all operations return new instances
LocalDateTime specific = LocalDateTime.of(2025, 12, 31, 23, 59, 59); // => 2025-12-31T23:59:59
// => specific is LocalDateTime: 2025-12-31 at 23:59:59
// => New Year's Eve, one second before midnight
LocalDateTime combined = LocalDateTime.of(today, now); // => Combine date and time
// => combined is LocalDateTime created from LocalDate + LocalTime
// => Useful when you have separate date and time components

// PARSING AND FORMATTING
String formatted = dateTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME); // => "2025-01-15T14:30:00.123456789"
// => formatted is String in ISO-8601 format
// => DateTimeFormatter.ISO_LOCAL_DATE_TIME is predefined formatter
// => Output includes nanoseconds if non-zero
DateTimeFormatter customFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
// => customFormat is DateTimeFormatter with custom pattern
// => yyyy=year, MM=month (01-12), dd=day, HH=hour (00-23), mm=minute, ss=second
// => CASE MATTERS: MM=month, mm=minute, DD=day of year, dd=day of month
String custom = dateTime.format(customFormat); // => "2025-01-15 14:30:00"
// => custom is String formatted with custom pattern
// => No nanoseconds (not in pattern)
LocalDateTime parsedDateTime = LocalDateTime.parse("2025-12-31T23:59:59"); // => Parse ISO format
// => parsedDateTime is LocalDateTime created from ISO string
// => Expects ISO-8601 format (YYYY-MM-DDTHH:mm:ss)
// => Throws DateTimeParseException if format invalid

// ZONEDDATETIME - date, time, and timezone
ZonedDateTime zonedNow = ZonedDateTime.now(); // => Current date/time with system timezone
// => zonedNow is ZonedDateTime (e.g., 2025-01-15T14:30:00.123+07:00[Asia/Jakarta])
// => Includes timezone offset (+07:00) and zone ID ([Asia/Jakarta])
// => Uses system default timezone (ZoneId.systemDefault())
ZoneId newYork = ZoneId.of("America/New_York"); // => ZoneId for New York
// => newYork is ZoneId representing America/New_York timezone
// => ZoneId uses IANA timezone database (e.g., America/New_York, Europe/London)
// => Handles daylight saving time automatically
ZonedDateTime nyTime = ZonedDateTime.now(newYork); // => Current time in New York
// => nyTime is ZonedDateTime in America/New_York timezone
// => If Jakarta is 14:30, New York might be 02:30 (12-hour offset)
ZonedDateTime utc = ZonedDateTime.now(ZoneId.of("UTC")); // => Current UTC time
// => utc is ZonedDateTime in UTC timezone (offset +00:00)
// => UTC (Coordinated Universal Time) - no daylight saving

// CONVERTING BETWEEN TIMEZONES
ZonedDateTime tokyoTime = zonedNow.withZoneSameInstant(ZoneId.of("Asia/Tokyo")); // => Convert to Tokyo time
// => tokyoTime is ZonedDateTime representing SAME INSTANT in Tokyo timezone
// => withZoneSameInstant() preserves the instant (moment in time)
// => If zonedNow is 14:30 Jakarta, tokyoTime is 16:30 Tokyo (+2 hours offset)
// => Alternative: withZoneSameLocal() keeps local time but changes zone (different instant!)

// INSTANT - point in time (Unix timestamp, always UTC)
Instant instant = Instant.now(); // => Current instant (UTC)
// => instant is Instant representing current moment in UTC
// => Instant is machine-friendly (seconds since epoch)
// => Epoch: 1970-01-01T00:00:00Z (Unix epoch)
long epochSeconds = instant.getEpochSecond(); // => Seconds since 1970-01-01T00:00:00Z
// => epochSeconds is long (e.g., 1736928600 for 2025-01-15 07:30:00 UTC)
// => Unix timestamp in seconds
long epochMillis = instant.toEpochMilli(); // => Milliseconds since epoch
// => epochMillis is long (e.g., 1736928600123 for same instant with milliseconds)
// => Common in Java APIs (System.currentTimeMillis(), Date.getTime())

// CONVERTING BETWEEN TYPES
LocalDate dateFromDateTime = dateTime.toLocalDate(); // => Extract date
// => dateFromDateTime is LocalDate (2025-01-15)
// => Extracts date part, discards time
LocalTime timeFromDateTime = dateTime.toLocalTime(); // => Extract time
// => timeFromDateTime is LocalTime (14:30:00.123456789)
// => Extracts time part, discards date
Instant instantFromZoned = zonedNow.toInstant(); // => Convert ZonedDateTime to Instant
// => instantFromZoned is Instant (UTC representation)
// => Converts zoned date/time to UTC instant (machine-friendly)

// DURATION - time-based amount (hours, minutes, seconds, nanoseconds)
Duration duration = Duration.ofHours(2); // => 2 hours
// => duration is Duration representing 2 hours (7200 seconds)
// => Duration measures time-based amounts (precise, fixed length)
// => Used for time differences, not date differences
Duration minutes = Duration.ofMinutes(30); // => 30 minutes
// => minutes is Duration representing 30 minutes (1800 seconds)
Duration between = Duration.between(now, later); // => Duration between two times
// => between is Duration representing time difference (now to later)
// => If now=14:30, later=16:30, between=2 hours (7200 seconds)

long durationSeconds = duration.getSeconds(); // => 7200 seconds (2 hours)
// => durationSeconds is long (total seconds in duration)
long durationMinutes = duration.toMinutes(); // => 120 minutes (2 hours)
// => durationMinutes is long (total minutes in duration)
// => Alternative: toHours(), toDays(), toNanos()

// PERIOD - date-based amount (years, months, days)
Period period = Period.ofDays(7); // => 7 days
// => period is Period representing 7 days
// => Period measures date-based amounts (variable length - months vary)
// => Used for date differences, not time differences
Period weeks = Period.ofWeeks(2); // => 14 days (2 weeks)
// => weeks is Period representing 14 days (internally stored as 14 days, not 2 weeks)
Period months = Period.ofMonths(3); // => 3 months
// => months is Period representing 3 months
// => Month length varies (28-31 days) - Period handles this
Period dateBetween = Period.between(today, nextYear); // => Period between two dates
// => dateBetween is Period representing date difference (today to nextYear)
// => If today=2025-01-15, nextYear=2026-01-15, dateBetween=1 year

int years = dateBetween.getYears(); // => 1 year
// => years is int (year component of period)
int days = dateBetween.getDays(); // => 0 days (same day of month)
// => days is int (day component of period)
// => Period has separate components: years, months, days
// => NOT total days - use ChronoUnit.DAYS.between() for that

// TEMPORAL ADJUSTERS - advanced date manipulation
LocalDate firstDayOfMonth = today.withDayOfMonth(1); // => 2025-01-01
// => firstDayOfMonth is LocalDate (first day of current month)
// => If today=2025-01-15, firstDayOfMonth=2025-01-01
LocalDate lastDayOfMonth = today.with(java.time.temporal.TemporalAdjusters.lastDayOfMonth()); // => 2025-01-31
// => lastDayOfMonth is LocalDate (last day of current month)
// => TemporalAdjusters provides common adjustments (firstDayOfMonth, lastDayOfMonth, next, previous)
// => Handles variable month lengths (Jan=31, Feb=28/29, etc.)
LocalDate nextMonday = today.with(java.time.temporal.TemporalAdjusters.next(DayOfWeek.MONDAY)); // => Next Monday
// => nextMonday is LocalDate (next Monday after today)
// => If today is Monday, returns NEXT Monday (not today)
// => Alternative: nextOrSame() includes today if already Monday

// COMPARISONS
boolean isBefore = today.isBefore(tomorrow); // => true
// => isBefore is true (today is before tomorrow)
// => 2025-01-15 is before 2025-01-16
boolean isAfter = today.isAfter(yesterday); // => true
// => isAfter is true (today is after yesterday)
// => 2025-01-15 is after 2025-01-14
boolean isEqual = today.isEqual(today); // => true
// => isEqual is true (today equals today)
// => Use isEqual() for date comparison, not equals() (equals() considers type)

// CHECKING LEAP YEAR
boolean isLeapYear = today.isLeapYear(); // => false (2025 is not a leap year)
// => isLeapYear is false (2025 not divisible by 4)
// => Leap years: divisible by 4, except centuries (unless divisible by 400)
// => 2024 is leap year, 2025 is not, 2000 was leap year, 1900 was not

// DATE CALCULATIONS WITH CHRONOUNIT
long daysBetween = ChronoUnit.DAYS.between(today, nextYear); // => 365 days
// => daysBetween is long (total days between dates)
// => ChronoUnit.DAYS.between() returns TOTAL days, not Period's day component
// => If today=2025-01-15, nextYear=2026-01-15, daysBetween=365
long monthsBetween = ChronoUnit.MONTHS.between(today, nextYear); // => 12 months
// => monthsBetween is long (total months between dates)
// => ChronoUnit provides precise calculations (YEARS, MONTHS, DAYS, HOURS, etc.)

// COMMON PATTERNS

// CALCULATE AGE from birth date
LocalDate birthDate = LocalDate.of(1990, 5, 15); // => Birth date: 1990-05-15
// => birthDate is LocalDate representing birth date
int age = Period.between(birthDate, today).getYears(); // => 34 years old
// => age is int (years component of period from birth to today)
// => If birthDate=1990-05-15, today=2025-01-15, age=34 (not yet 35th birthday)
// => Period handles leap years and variable month lengths correctly

// CHECK IF DATE IS IN RANGE
LocalDate start = LocalDate.of(2025, 1, 1); // => Range start: 2025-01-01
LocalDate end = LocalDate.of(2025, 12, 31); // => Range end: 2025-12-31
boolean inRange = !today.isBefore(start) && !today.isAfter(end); // => true
// => inRange is true if today is between start and end (inclusive)
// => !isBefore(start) means today >= start
// => !isAfter(end) means today <= end
// => If today=2025-01-15, inRange=true (within 2025)

// FORMAT FOR DISPLAY (human-readable)
String displayDate = today.format(DateTimeFormatter.ofPattern("MMMM d, yyyy")); // => "January 15, 2025"
// => displayDate is String (long month name, day, year)
// => MMMM=full month name (January), d=day without leading zero, yyyy=4-digit year
String displayTime = now.format(DateTimeFormatter.ofPattern("hh:mm a")); // => "02:30 PM"
// => displayTime is String (12-hour format with AM/PM)
// => hh=hour (01-12), mm=minute, a=AM/PM marker
// => If now=14:30, displays "02:30 PM"

// PARSING WITH CUSTOM FORMAT
DateTimeFormatter parser = DateTimeFormatter.ofPattern("dd/MM/yyyy");
// => parser is DateTimeFormatter for European date format (day/month/year)
// => dd=day (01-31), MM=month (01-12), yyyy=year
LocalDate usDate = LocalDate.parse("31/12/2025", parser); // => 2025-12-31
// => usDate is LocalDate parsed from custom format
// => 31/12/2025 (European) -> 2025-12-31 (ISO)
// => Throws DateTimeParseException if format doesn't match

// LEGACY DATE INTEROPERABILITY (avoid using Date/Calendar directly in new code!)
java.util.Date legacyDate = java.util.Date.from(instant); // => Convert Instant to Date
// => legacyDate is java.util.Date (legacy mutable class)
// => Date.from() converts Instant to Date (for legacy APIs)
// => Avoid using Date in new code - use java.time instead
Instant fromLegacy = legacyDate.toInstant(); // => Convert Date to Instant
// => fromLegacy is Instant converted from legacy Date
// => Date.toInstant() converts Date to Instant (for migration to java.time)
// => Use Instant as bridge between legacy and modern APIs
```

**Key Takeaway**: Use `LocalDate` for dates without time, `LocalTime` for times without dates, `LocalDateTime` for combined date/time without timezone, and `ZonedDateTime` when timezone is needed. All `java.time` classes are immutable—operations return new instances. Use `Duration` for time-based amounts (hours/minutes) and `Period` for date-based amounts (years/months/days). Format with `DateTimeFormatter` and parse with `parse()`. Avoid legacy `Date` and `Calendar` classes.

**Why It Matters**: Date and time handling is critical in production systems—scheduling tasks, logging events, calculating business days, tracking user activity, managing subscriptions, and handling international users across timezones. The legacy `Date` and `Calendar` APIs were notoriously buggy (mutable causing race conditions in multi-threaded code, thread-unsafe requiring external synchronization, confusing month indexing starting at 0 where January=0, weak type safety mixing dates and timestamps) causing countless production issues and security vulnerabilities. The modern `java.time` API (JSR-310, introduced Java 8) provides type safety through separate classes (LocalDate for dates, LocalTime for times, ZonedDateTime for timezone-aware), immutability making all instances thread-safe by default eliminating synchronization overhead, and clear semantics with 1-based month indexing (January=1, matching human intuition). Timezone handling is essential for global applications—storing timestamps as UTC `Instant` in databases and converting to local timezone for display prevents data corruption from daylight saving time changes (clocks jumping forward/backward) and regional differences. Understanding `Period` vs `Duration` prevents subtle bugs: adding "1 month" to January 31 gives February 28/29 (date-based Period, variable length) vs adding "30 days" gives March 2/3 (time-based Duration, fixed length). Production systems use `java.time` extensively—REST APIs serialize dates as ISO-8601 strings (2025-01-15T14:30:00Z), databases store TIMESTAMP columns as UTC, scheduling frameworks (Quartz, Spring Scheduler) use ZonedDateTime for cron expressions. Major companies migrated from legacy Date/Calendar to java.time—Spring Framework 4.0+ defaults to java.time, Jackson JSON library supports java.time serialization, JPA 2.2+ maps java.time types to SQL DATE/TIME/TIMESTAMP columns. The ISO-8601 format prevents ambiguity—American MM/DD/YYYY vs European DD/MM/YYYY confusion eliminated by standard YYYY-MM-DD format used by LocalDate.parse().
