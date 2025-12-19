---
title: Java Cheat Sheet
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 801
description: Quick reference for Java syntax, patterns, and common operations
tags: ["java", "cheat-sheet", "reference", "quick-guide"]
---

## Basic Syntax

### Variables and Data Types

```java
// Primitive types
byte b = 127;              // 8-bit integer (-128 to 127)
short s = 32767;           // 16-bit integer
int i = 2147483647;        // 32-bit integer (default)
long l = 9223372036854775807L;  // 64-bit integer (L suffix)
float f = 3.14f;           // 32-bit floating point (f suffix)
double d = 3.14159265359;  // 64-bit floating point (default)
boolean bool = true;       // true or false
char c = 'A';              // 16-bit Unicode character

// Reference types
String str = "Hello";      // String literal
String str2 = new String("World");  // String object
Integer boxed = 42;        // Autoboxing
int unboxed = boxed;       // Unboxing

// var keyword (Java 10+)
var name = "Alice";        // Type inferred as String
var count = 42;            // Type inferred as Integer
var list = List.of(1, 2, 3);  // Type inferred as List<Integer>
```

### Control Flow

```java
// if-else
if (condition) {
    // code
} else if (otherCondition) {
    // code
} else {
    // code
}

// Ternary operator
String result = condition ? "yes" : "no";

// switch (traditional)
switch (value) {
    case 1:
        // code
        break;
    case 2:
    case 3:
        // code
        break;
    default:
        // code
}

// switch expression (Java 14+)
String result = switch (day) {
    case MONDAY, FRIDAY -> "Work day";
    case SATURDAY, SUNDAY -> "Weekend";
    default -> "Midweek";
};

// Pattern matching switch (Java 21+)
String formatted = switch (obj) {
    case Integer i -> String.format("int %d", i);
    case Long l -> String.format("long %d", l);
    case String s -> String.format("String %s", s);
    default -> obj.toString();
};
```

### Loops

```java
// for loop
for (int i = 0; i < 10; i++) {
    System.out.println(i);
}

// enhanced for loop
for (String item : collection) {
    System.out.println(item);
}

// while loop
while (condition) {
    // code
}

// do-while loop
do {
    // code
} while (condition);

// forEach with lambda (Java 8+)
list.forEach(item -> System.out.println(item));
list.forEach(System.out::println);  // Method reference
```

## Collections Framework

### Common Collections

```java
// List (ordered, allows duplicates)
List<String> arrayList = new ArrayList<>();
List<String> linkedList = new LinkedList<>();
List<String> immutable = List.of("a", "b", "c");  // Java 9+

// Set (unordered, no duplicates)
Set<String> hashSet = new HashSet<>();
Set<String> linkedHashSet = new LinkedHashSet<>();  // Maintains insertion order
Set<String> treeSet = new TreeSet<>();  // Sorted
Set<String> immutable = Set.of("a", "b", "c");  // Java 9+

// Map (key-value pairs)
Map<String, Integer> hashMap = new HashMap<>();
Map<String, Integer> linkedHashMap = new LinkedHashMap<>();  // Maintains insertion order
Map<String, Integer> treeMap = new TreeMap<>();  // Sorted by keys
Map<String, Integer> immutable = Map.of("a", 1, "b", 2);  // Java 9+

// Queue
Queue<String> queue = new LinkedList<>();
Queue<String> priorityQueue = new PriorityQueue<>();

// Deque (double-ended queue)
Deque<String> deque = new ArrayDeque<>();
```

### Collection Operations

```java
// Add/Remove
list.add("item");
list.add(0, "first");
list.remove("item");
list.remove(0);
list.clear();

// Query
int size = list.size();
boolean isEmpty = list.isEmpty();
boolean contains = list.contains("item");
int index = list.indexOf("item");

// Iteration
for (String item : list) { }
list.forEach(item -> System.out.println(item));
Iterator<String> it = list.iterator();
while (it.hasNext()) { String item = it.next(); }

// Conversion
String[] array = list.toArray(new String[0]);
List<String> fromArray = Arrays.asList(array);
List<String> fromArray2 = List.of(array);  // Immutable
```

## Stream API (Java 8+)

### Creating Streams

```java
// From collections
Stream<String> stream = list.stream();
Stream<String> parallelStream = list.parallelStream();

// From arrays
Stream<String> stream = Arrays.stream(array);
Stream<String> stream = Stream.of("a", "b", "c");

// From values
IntStream range = IntStream.range(0, 10);  // 0 to 9
IntStream rangeClosed = IntStream.rangeClosed(0, 10);  // 0 to 10
Stream<String> generated = Stream.generate(() -> "element");
Stream<Integer> iterate = Stream.iterate(0, n -> n + 1);
```

### Intermediate Operations

```java
// filter - select elements
stream.filter(s -> s.length() > 3)

// map - transform elements
stream.map(String::toUpperCase)
stream.map(s -> s.length())

// flatMap - flatten nested streams
stream.flatMap(s -> Arrays.stream(s.split("")))

// distinct - remove duplicates
stream.distinct()

// sorted - sort elements
stream.sorted()
stream.sorted(Comparator.reverseOrder())

// peek - perform action without consuming
stream.peek(System.out::println)

// limit - take first N elements
stream.limit(10)

// skip - skip first N elements
stream.skip(5)
```

### Terminal Operations

```java
// forEach - perform action on each element
stream.forEach(System.out::println)

// collect - accumulate to collection
List<String> list = stream.collect(Collectors.toList());
Set<String> set = stream.collect(Collectors.toSet());
Map<String, Integer> map = stream.collect(
    Collectors.toMap(s -> s, String::length)
);

// reduce - combine elements
Optional<String> reduced = stream.reduce((a, b) -> a + b);
String reduced = stream.reduce("", (a, b) -> a + b);
int sum = intStream.reduce(0, Integer::sum);

// count - count elements
long count = stream.count();

// anyMatch/allMatch/noneMatch
boolean any = stream.anyMatch(s -> s.startsWith("A"));
boolean all = stream.allMatch(s -> s.length() > 0);
boolean none = stream.noneMatch(String::isEmpty);

// findFirst/findAny
Optional<String> first = stream.findFirst();
Optional<String> any = stream.findAny();

// min/max
Optional<String> min = stream.min(Comparator.naturalOrder());
Optional<String> max = stream.max(Comparator.naturalOrder());
```

### Collectors

```java
// Basic collectors
Collectors.toList()
Collectors.toSet()
Collectors.toCollection(TreeSet::new)

// Joining strings
Collectors.joining()
Collectors.joining(", ")
Collectors.joining(", ", "[", "]")

// Grouping
Map<Integer, List<String>> byLength = stream.collect(
    Collectors.groupingBy(String::length)
);

// Partitioning
Map<Boolean, List<String>> partitioned = stream.collect(
    Collectors.partitioningBy(s -> s.length() > 3)
);

// Counting
Map<String, Long> counts = stream.collect(
    Collectors.groupingBy(Function.identity(), Collectors.counting())
);

// Summarizing
IntSummaryStatistics stats = stream.collect(
    Collectors.summarizingInt(String::length)
);
```

## Lambda Expressions and Functional Interfaces

### Functional Interfaces

```java
// Predicate<T> - takes T, returns boolean
Predicate<String> isEmpty = String::isEmpty;
Predicate<String> isLong = s -> s.length() > 10;

// Function<T, R> - takes T, returns R
Function<String, Integer> length = String::length;
Function<Integer, String> toString = Object::toString;

// Consumer<T> - takes T, returns void
Consumer<String> print = System.out::println;
Consumer<String> process = s -> { /* code */ };

// Supplier<T> - takes nothing, returns T
Supplier<String> supplier = () -> "default";
Supplier<LocalDateTime> now = LocalDateTime::now;

// BiFunction<T, U, R> - takes T and U, returns R
BiFunction<String, String, String> concat = String::concat;

// UnaryOperator<T> - takes T, returns T
UnaryOperator<String> upper = String::toUpperCase;

// BinaryOperator<T> - takes two T, returns T
BinaryOperator<Integer> sum = Integer::sum;
```

### Lambda Syntax

```java
// No parameters
() -> 42
() -> { return 42; }

// One parameter (parentheses optional)
x -> x * 2
(x) -> x * 2
x -> { return x * 2; }

// Multiple parameters
(x, y) -> x + y
(x, y) -> { return x + y; }

// Type declarations
(String s) -> s.length()
(int x, int y) -> x + y
```

### Method References

```java
// Static method
Integer::parseInt
Math::max

// Instance method of particular object
System.out::println
"string"::toUpperCase

// Instance method of arbitrary object of particular type
String::length
Object::toString

// Constructor
ArrayList::new
String::new
```

## Exception Handling

### Try-Catch-Finally

```java
try {
    // Code that may throw exception
    riskyOperation();
} catch (IOException e) {
    // Handle IOException
    logger.error("IO error", e);
} catch (SQLException e) {
    // Handle SQLException
    logger.error("Database error", e);
} catch (Exception e) {
    // Handle any other exception
    logger.error("Unexpected error", e);
} finally {
    // Always executed (cleanup)
    closeResources();
}

// Multi-catch (Java 7+)
try {
    riskyOperation();
} catch (IOException | SQLException e) {
    logger.error("Error", e);
}

// Try-with-resources (Java 7+)
try (FileInputStream fis = new FileInputStream("file.txt");
     BufferedReader br = new BufferedReader(new InputStreamReader(fis))) {
    String line = br.readLine();
} catch (IOException e) {
    logger.error("Error reading file", e);
}
// Resources automatically closed
```

### Throwing Exceptions

```java
// Throw checked exception
public void method() throws IOException {
    throw new IOException("Error message");
}

// Throw unchecked exception
public void method() {
    throw new IllegalArgumentException("Invalid argument");
}

// Custom exception
public class CustomException extends Exception {
    public CustomException(String message) {
        super(message);
    }

    public CustomException(String message, Throwable cause) {
        super(message, cause);
    }
}
```

## Java 17 LTS Features

### Records (Java 14+)

```java
// Traditional class
public class Point {
    private final int x;
    private final int y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int x() { return x; }
    public int y() { return y; }

    @Override
    public boolean equals(Object obj) { /* ... */ }

    @Override
    public int hashCode() { /* ... */ }

    @Override
    public String toString() { /* ... */ }
}

// Record (auto-generates constructor, accessors, equals, hashCode, toString)
public record Point(int x, int y) {}

// Record with custom methods
public record Point(int x, int y) {
    // Compact constructor (validation)
    public Point {
        if (x < 0 || y < 0) {
            throw new IllegalArgumentException("Coordinates must be positive");
        }
    }

    // Custom method
    public double distanceFromOrigin() {
        return Math.sqrt(x * x + y * y);
    }
}
```

### Sealed Classes (Java 17+)

```java
// Sealed class - restricts which classes can extend it
public sealed class Shape
    permits Circle, Rectangle, Triangle {
    // Common shape methods
}

public final class Circle extends Shape {
    private final double radius;
    // ...
}

public final class Rectangle extends Shape {
    private final double width;
    private final double height;
    // ...
}

public non-sealed class Triangle extends Shape {
    // Can be extended by any class
    // ...
}

// Pattern matching with sealed classes
public static double area(Shape shape) {
    return switch (shape) {
        case Circle c -> Math.PI * c.radius() * c.radius();
        case Rectangle r -> r.width() * r.height();
        case Triangle t -> 0.5 * t.base() * t.height();
    };
}
```

### Pattern Matching for instanceof (Java 16+)

```java
// Before Java 16
if (obj instanceof String) {
    String s = (String) obj;
    System.out.println(s.length());
}

// Java 16+
if (obj instanceof String s) {
    System.out.println(s.length());
}

// With logical operators
if (obj instanceof String s && s.length() > 5) {
    System.out.println("Long string: " + s);
}
```

### Text Blocks (Java 15+)

```java
// Traditional string
String json = "{\n" +
              "  \"name\": \"Alice\",\n" +
              "  \"age\": 30\n" +
              "}";

// Text block
String json = """
    {
      "name": "Alice",
      "age": 30
    }
    """;

// With formatted values
String json = """
    {
      "name": "%s",
      "age": %d
    }
    """.formatted(name, age);
```

## Optionals (Java 8+)

```java
// Creating Optionals
Optional<String> empty = Optional.empty();
Optional<String> of = Optional.of("value");  // Throws if null
Optional<String> nullable = Optional.ofNullable(maybeNull);

// Checking presence
if (optional.isPresent()) {
    String value = optional.get();
}

// Using value if present
optional.ifPresent(value -> System.out.println(value));

// Getting value with default
String value = optional.orElse("default");
String value = optional.orElseGet(() -> computeDefault());
String value = optional.orElseThrow();
String value = optional.orElseThrow(() -> new CustomException());

// Transforming
Optional<Integer> length = optional.map(String::length);
Optional<String> upper = optional.map(String::toUpperCase);

// Filtering
Optional<String> filtered = optional.filter(s -> s.length() > 5);

// FlatMap (avoid nested Optionals)
Optional<String> result = optional.flatMap(s -> findRelated(s));

// Or (Java 9+)
Optional<String> result = optional.or(() -> Optional.of("fallback"));

// Stream (Java 9+)
Stream<String> stream = optional.stream();
```

## Concurrency Basics

### Thread Creation

```java
// Extending Thread
class MyThread extends Thread {
    @Override
    public void run() {
        System.out.println("Thread running");
    }
}
new MyThread().start();

// Implementing Runnable
Runnable task = () -> System.out.println("Task running");
new Thread(task).start();

// ExecutorService
ExecutorService executor = Executors.newFixedThreadPool(10);
executor.submit(() -> System.out.println("Task running"));
executor.shutdown();
```

### CompletableFuture (Java 8+)

```java
// Async execution
CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> {
    return fetchData();
});

// Chaining
future.thenApply(data -> process(data))
      .thenAccept(result -> System.out.println(result))
      .exceptionally(ex -> {
          logger.error("Error", ex);
          return null;
      });

// Combining futures
CompletableFuture<String> future1 = CompletableFuture.supplyAsync(() -> "Hello");
CompletableFuture<String> future2 = CompletableFuture.supplyAsync(() -> "World");
CompletableFuture<String> combined = future1.thenCombine(future2,
    (s1, s2) -> s1 + " " + s2);

// Waiting for all
CompletableFuture.allOf(future1, future2, future3).join();

// Waiting for any
CompletableFuture.anyOf(future1, future2, future3).join();
```

## Common Patterns

### Builder Pattern

```java
public class User {
    private final String name;
    private final String email;
    private final int age;

    private User(Builder builder) {
        this.name = builder.name;
        this.email = builder.email;
        this.age = builder.age;
    }

    public static class Builder {
        private String name;
        private String email;
        private int age;

        public Builder name(String name) {
            this.name = name;
            return this;
        }

        public Builder email(String email) {
            this.email = email;
            return this;
        }

        public Builder age(int age) {
            this.age = age;
            return this;
        }

        public User build() {
            return new User(this);
        }
    }
}

// Usage
User user = new User.Builder()
    .name("Alice")
    .email("alice@example.com")
    .age(30)
    .build();
```

### Singleton Pattern

```java
// Eager initialization
public class Singleton {
    private static final Singleton INSTANCE = new Singleton();

    private Singleton() {}

    public static Singleton getInstance() {
        return INSTANCE;
    }
}

// Lazy initialization with enum (best practice)
public enum Singleton {
    INSTANCE;

    public void doSomething() {
        // ...
    }
}
```

## Maven/Gradle Commands

### Maven

```bash
# Build
mvn clean install
mvn package
mvn compile

# Run tests
mvn test
mvn verify

# Skip tests
mvn install -DskipTests

# Run specific test
mvn test -Dtest=TestClassName

# Create project
mvn archetype:generate

# Dependency tree
mvn dependency:tree

# Update dependencies
mvn versions:display-dependency-updates
```

### Gradle

```bash
# Build
./gradlew build
./gradlew assemble

# Run tests
./gradlew test
./gradlew check

# Skip tests
./gradlew build -x test

# Run specific test
./gradlew test --tests TestClassName

# Create project
gradle init

# Dependency tree
./gradlew dependencies

# Clean build
./gradlew clean build
```

## Related Resources

**Learn more**:

- [Glossary](/en/learn/swe/prog-lang/java/reference/glossary) - Java terminology
- [Resources](/en/learn/swe/prog-lang/java/reference/resources) - Books, docs, tools
- [Cookbook](/en/learn/swe/prog-lang/java/how-to/cookbook) - Practical recipes

**Tutorials**:

- [Beginner Tutorial](/en/learn/swe/prog-lang/java/tutorials/beginner) - Java fundamentals
- [Intermediate Tutorial](/en/learn/swe/prog-lang/java/tutorials/intermediate) - Advanced concepts
- [Advanced Tutorial](/en/learn/swe/prog-lang/java/tutorials/advanced) - Expert topics
