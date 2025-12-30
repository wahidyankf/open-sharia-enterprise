---
title: "Cheat Sheet"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000030
description: Quick reference for Java syntax, operators, and common patterns
---

**Quick reference guide** for essential Java syntax and patterns. Copy-paste ready snippets for daily development.

## Variables and Types

### Variable Declaration

```java
// Variables
final String immutable = "Cannot change";  // Immutable (final)
String mutable = "Can change";             // Mutable

// Constants (compile-time)
public static final String CONSTANT = "Compile-time";

// Type inference (Java 10+)
var age = 25;                              // int
var price = 19.99;                         // double
var name = "Alice";                        // String

// Explicit types
int count = 10;
double rate = 4.5;
String message = "Hello";
```

### Primitive Types

```java
// Integer types
byte b = 127;                              // 8-bit: -128 to 127
short s = 32767;                           // 16-bit: -32,768 to 32,767
int i = 2147483647;                        // 32-bit: -2^31 to 2^31-1
long l = 9223372036854775807L;             // 64-bit: -2^63 to 2^63-1

// Floating-point types
float f = 3.14f;                           // 32-bit IEEE 754
double d = 3.14159265359;                  // 64-bit IEEE 754

// Other primitives
char c = 'A';                              // 16-bit Unicode character
boolean flag = true;                       // true or false

// Type conversion
int x = 100;
long y = x;                                // Implicit widening
int z = (int) 100L;                        // Explicit narrowing
```

### Reference Types and Wrapper Classes

```java
// Wrapper classes (autoboxing/unboxing)
Integer wrappedInt = 42;                   // Autoboxing
int primitiveInt = wrappedInt;             // Unboxing

// Common wrapper methods
Integer.parseInt("123");                   // String to int
Integer.toString(123);                     // int to String
Integer.valueOf(123);                      // int to Integer

// Null handling
String canBeNull = null;
String nonNull = Objects.requireNonNull(canBeNull, "Must not be null");

// Optional (Java 8+)
Optional<String> optional = Optional.ofNullable(canBeNull);
String value = optional.orElse("default");
optional.ifPresent(System.out::println);
```

## Classes and Objects

### Class Declaration

```java
// Basic class
public class Person {
    private String name;
    private int age;

    // Constructor
    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }

    // Getters and setters
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

    // Methods
    public void celebrateBirthday() {
        age++;
    }
}

// Create instance
Person person = new Person("Alice", 25);
```

### Constructors

```java
public class User {
    private String name;
    private int age;
    private String email;

    // Default constructor
    public User() {
        this("Unknown", 0, "");
    }

    // Parameterized constructor
    public User(String name, int age) {
        this(name, age, "");
    }

    // Full constructor
    public User(String name, int age, String email) {
        this.name = name;
        this.age = age;
        this.email = email;
    }

    // Copy constructor
    public User(User other) {
        this.name = other.name;
        this.age = other.age;
        this.email = other.email;
    }
}
```

### Records (Java 14+)

```java
// Compact data class
public record User(int id, String name, String email) {
    // Compact constructor (validation)
    public User {
        if (id < 0) {
            throw new IllegalArgumentException("ID must be positive");
        }
    }

    // Custom methods
    public String displayName() {
        return name.toUpperCase();
    }
}

// Usage
User user = new User(1, "Alice", "alice@example.com");
System.out.println(user.name());           // Auto-generated getter
User copy = new User(user.id(), "Bob", user.email());  // Copy with modification
```

### Static Members

```java
public class Counter {
    private static int count = 0;          // Shared across instances
    private int instanceId;

    public Counter() {
        instanceId = ++count;
    }

    public static int getCount() {
        return count;
    }

    // Static block (initialization)
    static {
        System.out.println("Counter class loaded");
    }
}
```

## Inheritance and Polymorphism

### Class Inheritance

```java
// Parent class
public class Animal {
    protected String name;

    public Animal(String name) {
        this.name = name;
    }

    public void makeSound() {
        System.out.println("Some sound");
    }
}

// Child class
public class Dog extends Animal {
    private String breed;

    public Dog(String name, String breed) {
        super(name);                       // Call parent constructor
        this.breed = breed;
    }

    @Override
    public void makeSound() {
        System.out.println("Woof!");
    }

    public void fetch() {
        System.out.println(name + " is fetching");
    }
}
```

### Abstract Classes

```java
public abstract class Shape {
    protected String color;

    public Shape(String color) {
        this.color = color;
    }

    // Abstract method (must be implemented)
    public abstract double area();

    // Concrete method
    public void displayColor() {
        System.out.println("Color: " + color);
    }
}

public class Circle extends Shape {
    private double radius;

    public Circle(String color, double radius) {
        super(color);
        this.radius = radius;
    }

    @Override
    public double area() {
        return Math.PI * radius * radius;
    }
}
```

### Sealed Classes (Java 17+)

```java
// Restrict inheritance
public sealed class Result permits Success, Error, Loading {
}

public final class Success extends Result {
    private final String data;

    public Success(String data) {
        this.data = data;
    }

    public String getData() {
        return data;
    }
}

public final class Error extends Result {
    private final String message;

    public Error(String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }
}

public final class Loading extends Result {
}
```

## Interfaces

### Basic Interface

```java
public interface Drawable {
    // Abstract method (implicitly public abstract)
    void draw();

    // Default method (Java 8+)
    default void display() {
        System.out.println("Displaying drawable");
    }

    // Static method (Java 8+)
    static Drawable create() {
        return () -> System.out.println("Drawing");
    }

    // Private method (Java 9+)
    private void helper() {
        System.out.println("Helper method");
    }
}

// Implementation
public class Circle implements Drawable {
    @Override
    public void draw() {
        System.out.println("Drawing circle");
    }
}
```

### Multiple Interface Implementation

```java
public interface Flyable {
    void fly();
}

public interface Swimmable {
    void swim();
}

public class Duck implements Flyable, Swimmable {
    @Override
    public void fly() {
        System.out.println("Duck flying");
    }

    @Override
    public void swim() {
        System.out.println("Duck swimming");
    }
}
```

### Functional Interfaces

```java
@FunctionalInterface
public interface Calculator {
    int calculate(int a, int b);           // Single abstract method

    // Default and static methods allowed
    default void printResult(int result) {
        System.out.println("Result: " + result);
    }
}

// Lambda implementation
Calculator add = (a, b) -> a + b;
Calculator multiply = (a, b) -> a * b;

int sum = add.calculate(5, 3);             // 8
```

## Generics

### Generic Classes

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

// Multiple type parameters
public class Pair<K, V> {
    private K key;
    private V value;

    public Pair(K key, V value) {
        this.key = key;
        this.value = value;
    }

    public K getKey() {
        return key;
    }

    public V getValue() {
        return value;
    }
}
```

### Generic Methods

```java
// Generic method
public class Utils {
    public static <T> void swap(T[] array, int i, int j) {
        T temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }

    public static <T extends Comparable<T>> T max(T a, T b) {
        return a.compareTo(b) > 0 ? a : b;
    }
}

// Usage
Integer[] numbers = {1, 2, 3};
Utils.swap(numbers, 0, 2);
Integer maxNum = Utils.max(10, 20);
```

### Bounded Type Parameters

```java
// Upper bound (extends)
public class NumberBox<T extends Number> {
    private T number;

    public double doubleValue() {
        return number.doubleValue();
    }
}

// Multiple bounds
public class Container<T extends Comparable<T> & Serializable> {
    private T item;
}

// Wildcards
public void processList(List<?> list) {           // Unknown type
    // Can only read as Object
}

public void processNumbers(List<? extends Number> list) {  // Upper bounded
    // Can read as Number
}

public void addNumbers(List<? super Integer> list) {      // Lower bounded
    list.add(42);                              // Can write Integer
}
```

## Collections Framework

### List Interface

```java
import java.util.*;

// ArrayList (dynamic array)
List<String> arrayList = new ArrayList<>();
arrayList.add("Alice");
arrayList.add("Bob");
arrayList.get(0);                          // "Alice"
arrayList.size();                          // 2
arrayList.remove("Bob");

// LinkedList (doubly-linked list)
List<String> linkedList = new LinkedList<>();
linkedList.addFirst("First");
linkedList.addLast("Last");

// Immutable list (Java 9+)
List<String> immutable = List.of("A", "B", "C");

// List operations
list.contains("Alice");                    // true
list.indexOf("Bob");                       // 1
list.isEmpty();                            // false
list.clear();                              // Remove all
```

### Set Interface

```java
// HashSet (unordered, no duplicates)
Set<String> hashSet = new HashSet<>();
hashSet.add("Alice");
hashSet.add("Bob");
hashSet.add("Alice");                      // Ignored (duplicate)

// TreeSet (sorted)
Set<Integer> treeSet = new TreeSet<>();
treeSet.add(3);
treeSet.add(1);
treeSet.add(2);                            // Sorted: [1, 2, 3]

// LinkedHashSet (insertion order)
Set<String> linkedSet = new LinkedHashSet<>();

// Immutable set (Java 9+)
Set<String> immutable = Set.of("A", "B", "C");
```

### Map Interface

```java
// HashMap (key-value pairs)
Map<String, Integer> map = new HashMap<>();
map.put("Alice", 25);
map.put("Bob", 30);
map.get("Alice");                          // 25
map.getOrDefault("Charlie", 0);            // 0
map.containsKey("Bob");                    // true
map.remove("Alice");

// Iterate map
for (Map.Entry<String, Integer> entry : map.entrySet()) {
    System.out.println(entry.getKey() + ": " + entry.getValue());
}

// TreeMap (sorted by keys)
Map<String, Integer> treeMap = new TreeMap<>();

// Immutable map (Java 9+)
Map<String, Integer> immutable = Map.of(
    "Alice", 25,
    "Bob", 30
);
```

### Queue and Deque

```java
// Queue (FIFO)
Queue<String> queue = new LinkedList<>();
queue.offer("First");
queue.offer("Second");
queue.poll();                              // "First" (remove and return)
queue.peek();                              // "Second" (view without removing)

// Deque (double-ended queue)
Deque<String> deque = new ArrayDeque<>();
deque.offerFirst("Front");
deque.offerLast("Back");
deque.pollFirst();                         // "Front"
deque.pollLast();                          // "Back"

// Stack operations
Deque<String> stack = new ArrayDeque<>();
stack.push("A");
stack.push("B");
stack.pop();                               // "B"
```

## Streams API (Java 8+)

### Creating Streams

```java
import java.util.stream.*;

// From collection
List<Integer> numbers = List.of(1, 2, 3, 4, 5);
Stream<Integer> stream = numbers.stream();

// From array
String[] words = {"a", "b", "c"};
Stream<String> arrayStream = Arrays.stream(words);

// Generate
Stream<Integer> infinite = Stream.iterate(0, n -> n + 1);
Stream<Double> random = Stream.generate(Math::random);

// Range
IntStream range = IntStream.range(1, 5);   // 1, 2, 3, 4
IntStream rangeClosed = IntStream.rangeClosed(1, 5);  // 1, 2, 3, 4, 5
```

### Stream Operations

```java
List<Integer> numbers = List.of(1, 2, 3, 4, 5);

// Filter
numbers.stream()
    .filter(n -> n > 2)                    // [3, 4, 5]
    .collect(Collectors.toList());

// Map
numbers.stream()
    .map(n -> n * 2)                       // [2, 4, 6, 8, 10]
    .collect(Collectors.toList());

// FlatMap
List<List<Integer>> nested = List.of(List.of(1, 2), List.of(3, 4));
nested.stream()
    .flatMap(List::stream)                 // [1, 2, 3, 4]
    .collect(Collectors.toList());

// Sorted
numbers.stream()
    .sorted()                              // Natural order
    .collect(Collectors.toList());

numbers.stream()
    .sorted(Comparator.reverseOrder())     // Reverse order
    .collect(Collectors.toList());

// Distinct
List.of(1, 2, 2, 3, 3, 3).stream()
    .distinct()                            // [1, 2, 3]
    .collect(Collectors.toList());

// Limit and skip
numbers.stream()
    .limit(3)                              // [1, 2, 3]
    .collect(Collectors.toList());

numbers.stream()
    .skip(2)                               // [3, 4, 5]
    .collect(Collectors.toList());
```

### Terminal Operations

```java
List<Integer> numbers = List.of(1, 2, 3, 4, 5);

// Collect
List<Integer> list = numbers.stream()
    .collect(Collectors.toList());

Set<Integer> set = numbers.stream()
    .collect(Collectors.toSet());

Map<Integer, String> map = numbers.stream()
    .collect(Collectors.toMap(
        n -> n,
        n -> "Value: " + n
    ));

// Reduce
int sum = numbers.stream()
    .reduce(0, (a, b) -> a + b);           // 15

int product = numbers.stream()
    .reduce(1, (a, b) -> a * b);           // 120

// Count
long count = numbers.stream()
    .filter(n -> n > 2)
    .count();                              // 3

// Find
Optional<Integer> first = numbers.stream()
    .filter(n -> n > 2)
    .findFirst();                          // Optional[3]

Optional<Integer> any = numbers.stream()
    .filter(n -> n > 2)
    .findAny();

// Match
boolean anyMatch = numbers.stream()
    .anyMatch(n -> n > 3);                 // true

boolean allMatch = numbers.stream()
    .allMatch(n -> n > 0);                 // true

boolean noneMatch = numbers.stream()
    .noneMatch(n -> n < 0);                // true

// ForEach
numbers.stream()
    .forEach(System.out::println);
```

## Lambda Expressions

### Basic Lambda Syntax

```java
// No parameters
Runnable r = () -> System.out.println("Hello");

// Single parameter (parentheses optional)
Consumer<String> print = s -> System.out.println(s);
Consumer<String> print2 = (s) -> System.out.println(s);

// Multiple parameters
BiFunction<Integer, Integer, Integer> add = (a, b) -> a + b;

// Block body
BiFunction<Integer, Integer, Integer> complex = (a, b) -> {
    int sum = a + b;
    System.out.println("Sum: " + sum);
    return sum;
};

// Method reference
Consumer<String> print = System.out::println;
Function<String, Integer> parse = Integer::parseInt;
```

### Functional Interfaces (java.util.function)

```java
// Predicate<T> - takes T, returns boolean
Predicate<String> isEmpty = String::isEmpty;
isEmpty.test("hello");                     // false

// Function<T, R> - takes T, returns R
Function<String, Integer> length = String::length;
length.apply("hello");                     // 5

// Consumer<T> - takes T, returns void
Consumer<String> print = System.out::println;
print.accept("hello");

// Supplier<T> - takes nothing, returns T
Supplier<String> supplier = () -> "Hello";
supplier.get();                            // "Hello"

// BiFunction<T, U, R> - takes T and U, returns R
BiFunction<Integer, Integer, Integer> add = (a, b) -> a + b;
add.apply(5, 3);                           // 8

// UnaryOperator<T> - takes T, returns T
UnaryOperator<Integer> square = x -> x * x;
square.apply(5);                           // 25

// BinaryOperator<T> - takes two T, returns T
BinaryOperator<Integer> multiply = (a, b) -> a * b;
multiply.apply(4, 5);                      // 20
```

## Exception Handling

### Try-Catch-Finally

```java
// Basic try-catch
try {
    int result = 10 / 0;
} catch (ArithmeticException e) {
    System.err.println("Cannot divide by zero: " + e.getMessage());
} finally {
    System.out.println("Cleanup");
}

// Multiple catch blocks
try {
    String s = null;
    s.length();
} catch (NullPointerException e) {
    System.err.println("Null pointer: " + e.getMessage());
} catch (Exception e) {
    System.err.println("General error: " + e.getMessage());
}

// Multi-catch (Java 7+)
try {
    // Code
} catch (IOException | SQLException e) {
    System.err.println("IO or SQL error: " + e.getMessage());
}
```

### Try-With-Resources (Java 7+)

```java
// Auto-close resources
try (BufferedReader br = new BufferedReader(new FileReader("file.txt"))) {
    String line = br.readLine();
    System.out.println(line);
} catch (IOException e) {
    e.printStackTrace();
}

// Multiple resources
try (
    FileInputStream fis = new FileInputStream("input.txt");
    FileOutputStream fos = new FileOutputStream("output.txt")
) {
    // Use resources
} catch (IOException e) {
    e.printStackTrace();
}
```

### Custom Exceptions

```java
// Custom checked exception
public class UserNotFoundException extends Exception {
    public UserNotFoundException(String message) {
        super(message);
    }
}

// Custom unchecked exception
public class InvalidInputException extends RuntimeException {
    public InvalidInputException(String message) {
        super(message);
    }
}

// Usage
public User findUser(int id) throws UserNotFoundException {
    if (id < 0) {
        throw new InvalidInputException("ID must be positive");
    }
    // Find user logic
    throw new UserNotFoundException("User not found: " + id);
}
```

## Threads and Concurrency

### Thread Creation

```java
// Extend Thread class
class MyThread extends Thread {
    @Override
    public void run() {
        System.out.println("Thread running: " + getName());
    }
}

MyThread thread = new MyThread();
thread.start();

// Implement Runnable interface
Runnable task = () -> System.out.println("Task running");
Thread thread2 = new Thread(task);
thread2.start();

// Thread methods
thread.join();                             // Wait for completion
thread.sleep(1000);                        // Sleep 1 second
thread.isAlive();                          // Check if running
```

### ExecutorService

```java
import java.util.concurrent.*;

// Fixed thread pool
ExecutorService executor = Executors.newFixedThreadPool(5);

// Submit tasks
Future<String> future = executor.submit(() -> {
    Thread.sleep(1000);
    return "Task completed";
});

// Get result (blocking)
String result = future.get();              // Waits for completion

// Shutdown
executor.shutdown();
executor.awaitTermination(1, TimeUnit.MINUTES);

// Single thread executor
ExecutorService single = Executors.newSingleThreadExecutor();

// Cached thread pool (creates threads as needed)
ExecutorService cached = Executors.newCachedThreadPool();

// Scheduled executor
ScheduledExecutorService scheduled = Executors.newScheduledThreadPool(2);
scheduled.schedule(() -> System.out.println("Task"), 5, TimeUnit.SECONDS);
```

### Synchronization

```java
public class Counter {
    private int count = 0;

    // Synchronized method
    public synchronized void increment() {
        count++;
    }

    // Synchronized block
    public void add(int value) {
        synchronized (this) {
            count += value;
        }
    }

    public synchronized int getCount() {
        return count;
    }
}

// Volatile variable (visibility guarantee)
private volatile boolean running = true;
```

### Concurrent Collections

```java
import java.util.concurrent.*;

// Thread-safe collections
ConcurrentHashMap<String, Integer> map = new ConcurrentHashMap<>();
CopyOnWriteArrayList<String> list = new CopyOnWriteArrayList<>();
BlockingQueue<String> queue = new LinkedBlockingQueue<>();

// Atomic variables
AtomicInteger counter = new AtomicInteger(0);
counter.incrementAndGet();                 // Thread-safe increment
counter.compareAndSet(0, 1);               // Compare and swap
```

## Annotations

### Built-in Annotations

```java
// Override annotation
class Child extends Parent {
    @Override
    public void method() {
        // Override parent method
    }
}

// Deprecated
@Deprecated
public void oldMethod() {
    // Deprecated method
}

// SuppressWarnings
@SuppressWarnings("unchecked")
public void rawTypeMethod() {
    List list = new ArrayList();
}

// FunctionalInterface
@FunctionalInterface
public interface Calculator {
    int calculate(int a, int b);
}
```

### Custom Annotations

```java
// Define annotation
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Test {
    String name() default "";
    int timeout() default 0;
}

// Use annotation
public class MyTest {
    @Test(name = "testAdd", timeout = 1000)
    public void testMethod() {
        // Test code
    }
}

// Read annotation via reflection
Method method = MyTest.class.getMethod("testMethod");
if (method.isAnnotationPresent(Test.class)) {
    Test test = method.getAnnotation(Test.class);
    System.out.println(test.name());
}
```

## Modules (Java 9+)

### Module Declaration

```java
// module-info.java
module com.example.myapp {
    // Export packages
    exports com.example.myapp.api;

    // Require other modules
    requires java.sql;
    requires transitive java.logging;

    // Open for reflection
    opens com.example.myapp.internal;

    // Provide service
    provides com.example.myapp.Service
        with com.example.myapp.ServiceImpl;

    // Use service
    uses com.example.myapp.Service;
}
```

## Pattern Matching

### instanceof Pattern (Java 14+)

```java
// Old way
if (obj instanceof String) {
    String s = (String) obj;
    System.out.println(s.length());
}

// New way (pattern variable)
if (obj instanceof String s) {
    System.out.println(s.length());
}

// With logical operators
if (obj instanceof String s && s.length() > 5) {
    System.out.println("Long string: " + s);
}
```

### Switch Expressions (Java 14+)

```java
// Expression (returns value)
String result = switch (day) {
    case MONDAY, FRIDAY, SUNDAY -> "6am";
    case TUESDAY -> "7am";
    case THURSDAY, SATURDAY -> "8am";
    case WEDNESDAY -> "9am";
    default -> throw new IllegalArgumentException("Invalid day");
};

// With yield (block)
int numLetters = switch (day) {
    case MONDAY, FRIDAY, SUNDAY -> 6;
    case TUESDAY -> {
        System.out.println("Tuesday");
        yield 7;
    }
    default -> throw new IllegalArgumentException();
};

// Pattern matching in switch (Java 17+)
static String formatterPatternSwitch(Object obj) {
    return switch (obj) {
        case Integer i -> "Integer: " + i;
        case String s -> "String: " + s;
        case null -> "null";
        default -> "Unknown";
    };
}
```

## Common String Operations

```java
// String creation
String s1 = "Hello";
String s2 = new String("Hello");
String s3 = String.valueOf(123);

// String methods
"hello".length();                          // 5
"hello".charAt(0);                         // 'h'
"hello".substring(1, 4);                   // "ell"
"hello".toUpperCase();                     // "HELLO"
"hello".toLowerCase();                     // "hello"
"  text  ".trim();                         // "text"
"hello".startsWith("he");                  // true
"hello".endsWith("lo");                    // true
"hello".contains("ll");                    // true
"hello".indexOf("l");                      // 2
"hello".replace("l", "x");                 // "hexxo"

// String comparison
"hello".equals("hello");                   // true
"hello".equalsIgnoreCase("HELLO");         // true
"hello".compareTo("world");                // negative

// String concatenation
String result = "Hello" + " " + "World";
String joined = String.join(", ", "A", "B", "C");  // "A, B, C"

// String splitting
String[] parts = "a,b,c".split(",");       // ["a", "b", "c"]

// String formatting
String formatted = String.format("Hello %s, age %d", "Alice", 25);
String text = "Hello %s".formatted("Alice");  // Java 15+

// Text blocks (Java 15+)
String json = """
    {
        "name": "Alice",
        "age": 25
    }
    """;

// StringBuilder (mutable)
StringBuilder sb = new StringBuilder();
sb.append("Hello");
sb.append(" ");
sb.append("World");
String result = sb.toString();             // "Hello World"
```

## Learn More

**Detailed Documentation**:

- [Quick Start Tutorial](/en/learn/software-engineering/programming-languages/java/tutorials/quick-start) - 12 Java touchpoints
- [Beginner Tutorial](/en/learn/software-engineering/programming-languages/java/tutorials/beginner) - Comprehensive fundamentals
- [Cookbook](/en/learn/software-engineering/programming-languages/java/how-to/cookbook) - Practical recipes
- [How-To Guides](/en/learn/software-engineering/programming-languages/java/how-to) - Problem-solving guides

**Official Resources**:

- [Java Language Specification](https://docs.oracle.com/javase/specs/) - Complete language documentation
- [Java API Documentation](https://docs.oracle.com/en/java/javase/21/docs/api/) - Standard library API
- [Java Tutorials](https://docs.oracle.com/javase/tutorial/) - Oracle official tutorials
