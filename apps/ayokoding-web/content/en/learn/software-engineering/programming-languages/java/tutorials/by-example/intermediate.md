---
title: "Intermediate"
date: 2025-12-23T00:00:00+07:00
draft: false
weight: 10000002
description: "Master intermediate Java through 30 examples: advanced OOP, generics, functional programming, streams, file I/O, testing, and concurrency patterns"
tags:
  [
    "java",
    "tutorial",
    "by-example",
    "intermediate",
    "generics",
    "streams",
    "concurrency",
    "testing",
    "functional-programming",
  ]
---

Master intermediate Java concepts through 30 annotated code examples. Each example builds on beginner foundations, introducing advanced OOP, generics, functional programming, and concurrency patterns.

## Group 1: Advanced OOP

### Example 16: Encapsulation and JavaBeans

Encapsulation hides internal state behind controlled accessors. JavaBeans follow naming conventions for getters/setters. Records (Java 14+) provide concise immutable data classes.

**Code**:

```java
// Traditional JavaBeans pattern
public class Person {
    // Private fields - encapsulated state
    private String name;
    private int age;

    // Getter methods (accessors)
    public String getName() {
        return name; // => returns name field
    }

    public int getAge() {
        return age; // => returns age field
    }

    // Setter methods (mutators)
    public void setName(String name) {
        this.name = name; // 'this' distinguishes field from parameter
    }

    public void setAge(int age) {
        if (age >= 0) { // Validation in setter
            this.age = age;
        } else {
            throw new IllegalArgumentException("Age cannot be negative");
        }
    }
}

// Builder pattern for complex object construction
public class User {
    private final String username; // Immutable (final)
    private final String email;
    private final int age;
    private final String address;

    // Private constructor - only builder can create instances
    private User(Builder builder) {
        this.username = builder.username;
        this.email = builder.email;
        this.age = builder.age;
        this.address = builder.address;
    }

    // Static nested Builder class
    public static class Builder {
        private String username; // Required
        private String email; // Required
        private int age = 0; // Optional with default
        private String address = ""; // Optional

        public Builder(String username, String email) {
            this.username = username;
            this.email = email;
        }

        public Builder age(int age) {
            this.age = age;
            return this; // Return builder for chaining
        }

        public Builder address(String address) {
            this.address = address;
            return this;
        }

        public User build() {
            return new User(this); // Construct User with builder
        }
    }
}

// Using builder pattern - fluent API
User user = new User.Builder("alice", "alice@example.com")
    .age(30)
    .address("123 Main St")
    .build(); // => User instance

// Record classes (Java 14+) - concise immutable data carriers
public record Point(int x, int y) {
    // Automatically generates:
    // - Constructor: Point(int x, int y)
    // - Getters: x(), y() (no get prefix)
    // - equals(), hashCode(), toString()
    // - Fields are final (immutable)
}

Point p1 = new Point(10, 20); // => Point[x=10, y=20]
int x = p1.x(); // => 10 (getter without 'get' prefix)
int y = p1.y(); // => 20

// Records are immutable - no setters
// p1.x = 30; // ERROR: x is final

// Custom validation in record
public record Temperature(double celsius) {
    // Compact constructor - validates input
    public Temperature {
        if (celsius < -273.15) {
            throw new IllegalArgumentException("Below absolute zero");
        }
    }

    // Custom methods allowed
    public double fahrenheit() {
        return celsius * 9.0 / 5.0 + 32; // => converts to Fahrenheit
    }
}

Temperature temp = new Temperature(100.0); // => Temperature[celsius=100.0]
double f = temp.fahrenheit(); // => 212.0
```

**Key Takeaway**: Encapsulation uses private fields with public getters/setters. Builder pattern enables fluent construction of complex objects. Records (Java 14+) automatically generate constructors, getters, `equals()`, `hashCode()`, and `toString()` for immutable data classes.

**Why It Matters**: Encapsulation protects object invariants, preventing invalid states that cause bugs. Private fields with public accessors enable validation, logging, and computed properties without exposing internal representation. The JavaBean convention (getters/setters) integrates with frameworks like Spring and Hibernate that use reflection to access properties. Records (Java 14+) provide immutable data carriers with auto-generated accessors, eliminating boilerplate while maintaining encapsulation. Proper encapsulation enables refactoring internals without breaking clients, reduces coupling, and enforces business rules through controlled access.

---

### Example 17: HTTP Filter Chain Pattern

Production middleware systems use filter chains to process requests through multiple stages (authentication, rate limiting, logging). This pattern demonstrates composition, delegation, and the Chain of Responsibility pattern in real-world HTTP request processing.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TB
    Request["HTTP Request"] --> Auth["AuthFilter<br/>Check token"]
    Auth -->|Unauthorized| Resp1["401 Response"]
    Auth -->|Authorized| Rate["RateLimitFilter<br/>Check quota"]
    Rate -->|Exceeded| Resp2["429 Response"]
    Rate -->|OK| Log["LoggingFilter<br/>Record request"]
    Log --> Handler["Request Handler"]
    Handler --> Response["200 Response"]

    style Request fill:#0173B2,color:#fff
    style Auth fill:#DE8F05,color:#fff
    style Rate fill:#029E73,color:#fff
    style Log fill:#CC78BC,color:#fff
    style Handler fill:#CA9161,color:#fff
    style Resp1 fill:#DE8F05,color:#fff
    style Resp2 fill:#DE8F05,color:#fff
    style Response fill:#029E73,color:#fff
```

**Code**:

```java
// Request and Response models
class Request {
    private String path;
    private String method;
    private Map<String, String> headers;
    private String clientId;

    public Request(String path, String method) {
        this.path = path;
        this.method = method;
        this.headers = new HashMap<>();
    }

    public String getHeader(String name) {
        return headers.get(name);
    }

    public void setHeader(String name, String value) {
        headers.put(name, value);
    }

    public String getClientId() { return clientId; }
    public void setClientId(String id) { this.clientId = id; }
    public String getPath() { return path; }
}

class Response {
    private int statusCode;
    private String body;

    public Response(int statusCode, String body) {
        this.statusCode = statusCode;
        this.body = body;
    }

    public static Response ok(String body) {
        return new Response(200, body); // => 200 OK
    }

    public static Response unauthorized() {
        return new Response(401, "Unauthorized"); // => 401 Unauthorized
    }

    public static Response tooManyRequests() {
        return new Response(429, "Too Many Requests"); // => 429 Rate Limited
    }

    public int getStatusCode() { return statusCode; }
    public String getBody() { return body; }
}

// Filter interface - chain of responsibility pattern
interface RequestFilter {
    Response filter(Request request, FilterChain chain);
}

// Filter chain manages execution flow
class FilterChain {
    private List<RequestFilter> filters;
    private int position = 0;

    public FilterChain(List<RequestFilter> filters) {
        this.filters = new ArrayList<>(filters);
    }

    public Response next(Request request) {
        if (position >= filters.size()) {
            // All filters passed - handle the request
            return handleRequest(request); // => Final handler
        }
        RequestFilter filter = filters.get(position++);
        return filter.filter(request, this); // => Delegate to next filter
    }

    private Response handleRequest(Request request) {
        // Actual request processing logic
        return Response.ok("Processed: " + request.getPath());
    }
}

// Authentication filter - validates tokens
class AuthenticationFilter implements RequestFilter {
    private Set<String> validTokens = Set.of("token123", "token456");

    @Override
    public Response filter(Request request, FilterChain chain) {
        String token = request.getHeader("Authorization");

        if (token == null || !validTokens.contains(token)) {
            System.out.println("Auth failed: Invalid token");
            return Response.unauthorized(); // => 401, chain stops here
        }

        // Extract client ID from token for downstream filters
        request.setClientId("user_" + token.hashCode());
        System.out.println("Auth passed: " + request.getClientId());
        return chain.next(request); // => Continue to next filter
    }
}

// Rate limiting filter - prevents abuse
class RateLimitFilter implements RequestFilter {
    private Map<String, Integer> requestCounts = new HashMap<>();
    private int maxRequests = 100;

    @Override
    public Response filter(Request request, FilterChain chain) {
        String clientId = request.getClientId();
        int count = requestCounts.getOrDefault(clientId, 0);

        if (count >= maxRequests) {
            System.out.println("Rate limit exceeded for: " + clientId);
            return Response.tooManyRequests(); // => 429, chain stops
        }

        requestCounts.put(clientId, count + 1); // => Increment counter
        System.out.println("Rate limit check passed: " + count + "/" + maxRequests);
        return chain.next(request); // => Continue to next filter
    }
}

// Logging filter - records requests
class LoggingFilter implements RequestFilter {
    @Override
    public Response filter(Request request, FilterChain chain) {
        long startTime = System.currentTimeMillis();
        System.out.println("Request started: " + request.getPath());

        Response response = chain.next(request); // => Continue chain

        long duration = System.currentTimeMillis() - startTime;
        System.out.println("Request completed: " + response.getStatusCode() +
                         " in " + duration + "ms");
        return response; // => Return response unchanged
    }
}

// Building and using the filter chain
List<RequestFilter> filters = List.of(
    new AuthenticationFilter(),
    new RateLimitFilter(),
    new LoggingFilter()
);

FilterChain chain = new FilterChain(filters);

// Successful request
Request req1 = new Request("/api/users", "GET");
req1.setHeader("Authorization", "token123");
Response resp1 = chain.next(req1);
// Output:
// Auth passed: user_XXXXXX
// Rate limit check passed: 0/100
// Request started: /api/users
// Request completed: 200 in 1ms
// => Response: 200 "Processed: /api/users"

// Failed authentication
Request req2 = new Request("/api/data", "GET");
req2.setHeader("Authorization", "invalid");
Response resp2 = chain.next(req2);
// Output:
// Auth failed: Invalid token
// => Response: 401 "Unauthorized" (chain stops at first filter)
```

**Key Takeaway**: Filter chains demonstrate composition and the Chain of Responsibility pattern. Each filter decides whether to continue the chain or return early. This pattern enables modular, testable middleware—add/remove filters without changing core logic. Production systems use this for authentication, rate limiting, logging, compression, and error handling. Filters compose through delegation, avoiding inheritance coupling.

**Why It Matters**: The Filter Chain pattern enables composable middleware for request processing—authentication, logging, rate limiting, CORS handling. It's fundamental to servlet containers (Tomcat), web frameworks (Spring), and HTTP libraries. Filters decouple cross-cutting concerns from business logic, making code modular and reusable. Understanding the chain-of-responsibility pattern enables implementing custom filters for API gateways, reverse proxies, and web services. Filter chains enable consistent request handling across endpoints, improving security, observability, and maintainability in production web applications.

---

### Example 18: Generics Deep Dive

Generics provide compile-time type safety for classes and methods. Bounded type parameters constrain allowable types. Wildcards enable flexible APIs.

**Code**:

```java
import java.util.*;

// Generic class with type parameter <T>
class Box<T> {
    private T content;

    public void set(T content) {
        this.content = content;
    }

    public T get() {
        return content; // => returns T
    }
}

Box<String> stringBox = new Box<>();
stringBox.set("Hello"); // => OK
String str = stringBox.get(); // => "Hello" (no cast needed)

Box<Integer> intBox = new Box<>();
intBox.set(42); // => OK
// intBox.set("text"); // ERROR: compile-time type check

// Generic method with type parameter
public static <T> void printArray(T[] array) {
    for (T element : array) {
        System.out.println(element);
    }
}

String[] strings = {"A", "B", "C"};
Integer[] ints = {1, 2, 3};
printArray(strings); // => T inferred as String
printArray(ints); // => T inferred as Integer

// Bounded type parameters - <T extends Type>
class NumberBox<T extends Number> {
    private T number;

    public void set(T number) {
        this.number = number;
    }

    public double doubleValue() {
        return number.doubleValue(); // Number methods available
    }
}

NumberBox<Integer> intNumBox = new NumberBox<>(); // => OK (Integer extends Number)
NumberBox<Double> doubleBox = new NumberBox<>(); // => OK (Double extends Number)
// NumberBox<String> strBox = new NumberBox<>(); // ERROR: String doesn't extend Number

// Multiple bounds - <T extends Class & Interface1 & Interface2>
class MultiBox<T extends Number & Comparable<T>> {
    public T max(T a, T b) {
        return a.compareTo(b) > 0 ? a : b; // => uses Comparable
    }
}

// Wildcards for flexible method parameters
public static void printList(List<?> list) {
    for (Object obj : list) { // ? can be any type, read as Object
        System.out.println(obj);
    }
    // list.add(42); // ERROR: can't add to List<?> (unknown type)
}

List<String> strings2 = Arrays.asList("A", "B");
List<Integer> ints2 = Arrays.asList(1, 2);
printList(strings2); // => OK
printList(ints2); // => OK

// Upper bounded wildcard - <? extends Type>
public static double sumNumbers(List<? extends Number> list) {
    double sum = 0;
    for (Number num : list) { // Can read as Number
        sum += num.doubleValue();
    }
    return sum; // => sum of all numbers
    // list.add(Integer.valueOf(5)); // ERROR: can't add (could be List<Double>)
}

List<Integer> intList = Arrays.asList(1, 2, 3);
List<Double> doubleList = Arrays.asList(1.5, 2.5);
double sum1 = sumNumbers(intList); // => 6.0
double sum2 = sumNumbers(doubleList); // => 4.0

// Lower bounded wildcard - <? super Type>
public static void addIntegers(List<? super Integer> list) {
    list.add(1); // => OK: can add Integer
    list.add(2);
    // Integer x = list.get(0); // ERROR: can't read as Integer (could be Object)
    Object obj = list.get(0); // => OK: read as Object
}

List<Number> numList = new ArrayList<>();
List<Object> objList = new ArrayList<>();
addIntegers(numList); // => OK (Number super Integer)
addIntegers(objList); // => OK (Object super Integer)

// PECS principle: Producer Extends, Consumer Super
// Use <? extends T> when reading (producing) values
// Use <? super T> when writing (consuming) values

// Generic constructors
class Holder {
    private Object value;

    public <T> Holder(T value) { // Generic constructor
        this.value = value;
    }
}

Holder h1 = new Holder("text"); // => T inferred as String
Holder h2 = new Holder(123); // => T inferred as Integer

// Multiple type parameters
class Pair<K, V> {
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

Pair<String, Integer> pair = new Pair<>("Age", 30);
String key = pair.getKey(); // => "Age"
Integer value = pair.getValue(); // => 30

// Type erasure - generics removed at runtime
List<String> strList = new ArrayList<>();
List<Integer> intList2 = new ArrayList<>();
System.out.println(strList.getClass() == intList2.getClass()); // => true
// Both are ArrayList at runtime (type parameters erased)
```

**Key Takeaway**: Generics provide compile-time type safety. Bounded types (`<T extends Type>`) constrain parameters. Wildcards enable flexible APIs: `<?>` (any type), `<? extends T>` (T or subtypes), `<? super T>` (T or supertypes). PECS: Producer Extends, Consumer Super. Type erasure removes generics at runtime.

**Why It Matters**: Generics enable type-safe, reusable code without casting or runtime type checks. Understanding wildcards (? extends T for producers, ? super T for consumers) enables flexible APIs following the PECS principle. Bounded type parameters (T extends Comparable<T>) enable constrained generics for sorting and comparisons. Type erasure causes runtime type information loss—understanding this prevents confusion with reflection, overloading, and arrays. Proper generic design prevents ClassCastException, improves code maintainability, and enables powerful abstractions used throughout the Java ecosystem (Collections, Streams, Optional).

---

## Group 2: Collections Framework Advanced

### Example 19: Comparators and Sorting

`Comparable<T>` defines natural ordering. `Comparator<T>` enables custom ordering. Lambda syntax and method references simplify comparator creation.

**Code**:

```java
import java.util.*;

// Comparable interface - natural ordering
class Person implements Comparable<Person> {
    String name;
    int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }

    // compareTo() defines natural ordering (by age)
    @Override
    public int compareTo(Person other) {
        return Integer.compare(this.age, other.age);
        // Return: <0 if this < other, 0 if equal, >0 if this > other
    }

    @Override
    public String toString() {
        return name + "(" + age + ")";
    }
}

List<Person> people = Arrays.asList(
    new Person("Alice", 30),
    new Person("Bob", 25),
    new Person("Charlie", 35)
);

Collections.sort(people); // Uses compareTo() - natural ordering
System.out.println(people); // => [Bob(25), Alice(30), Charlie(35)]

// Comparator interface - custom ordering
Comparator<Person> byName = new Comparator<Person>() {
    @Override
    public int compare(Person p1, Person p2) {
        return p1.name.compareTo(p2.name); // Compare by name
    }
};

Collections.sort(people, byName);
System.out.println(people); // => [Alice(30), Bob(25), Charlie(35)]

// Lambda comparators - concise syntax
Comparator<Person> byAgeLambda = (p1, p2) -> Integer.compare(p1.age, p2.age);
people.sort(byAgeLambda); // List.sort() method
System.out.println(people); // => [Bob(25), Alice(30), Charlie(35)]

// Comparator.comparing() with method references
Comparator<Person> byName2 = Comparator.comparing(Person::getName);
people.sort(byName2);

// Method references - ClassName::methodName
class Person {
    public String getName() { return name; }
    public int getAge() { return age; }
}

Comparator<Person> byAge = Comparator.comparing(Person::getAge);
Comparator<Person> byNameRef = Comparator.comparing(Person::getName);

// Chaining comparators - thenComparing()
Comparator<Person> byAgeThenName = Comparator
    .comparing(Person::getAge)           // Primary: age
    .thenComparing(Person::getName);     // Secondary: name

List<Person> people2 = Arrays.asList(
    new Person("Alice", 30),
    new Person("Bob", 30),    // Same age as Alice
    new Person("Charlie", 25)
);

people2.sort(byAgeThenName);
System.out.println(people2);
// => [Charlie(25), Alice(30), Bob(30)]
// First sorted by age, then by name for same age

// Reversed comparator
Comparator<Person> byAgeReversed = Comparator
    .comparing(Person::getAge)
    .reversed(); // Reverses the order

people.sort(byAgeReversed);
System.out.println(people); // => [Charlie(35), Alice(30), Bob(25)]

// Natural order comparators
Comparator<Integer> naturalOrder = Comparator.naturalOrder();
List<Integer> nums = Arrays.asList(5, 2, 8, 1, 9);
nums.sort(naturalOrder); // => [1, 2, 5, 8, 9]

Comparator<Integer> reverseOrder = Comparator.reverseOrder();
nums.sort(reverseOrder); // => [9, 8, 5, 2, 1]

// Null handling comparators
Comparator<String> nullsFirst = Comparator.nullsFirst(Comparator.naturalOrder());
List<String> withNulls = Arrays.asList("C", null, "A", "B", null);
withNulls.sort(nullsFirst);
System.out.println(withNulls); // => [null, null, A, B, C]

Comparator<String> nullsLast = Comparator.nullsLast(Comparator.naturalOrder());
withNulls.sort(nullsLast);
System.out.println(withNulls); // => [A, B, C, null, null]

// Comparing with custom logic
Comparator<String> byLength = Comparator.comparingInt(String::length);
List<String> words = Arrays.asList("Java", "is", "awesome");
words.sort(byLength);
System.out.println(words); // => [is, Java, awesome]

// Complex chaining
Comparator<Person> complex = Comparator
    .comparing(Person::getAge)
    .reversed()
    .thenComparing(Person::getName);
```

**Key Takeaway**: `Comparable<T>` defines natural ordering via `compareTo()`. `Comparator<T>` enables custom ordering via `compare()`. Use `Comparator.comparing()` with method references for concise comparators. Chain comparators with `thenComparing()`. `reversed()`, `nullsFirst()`, `nullsLast()` modify comparison behavior.

**Why It Matters**: Comparators enable custom sorting logic—sort by name, by age, by multiple criteria. They're essential for ordered collections (TreeSet, TreeMap) and sorting APIs (Collections.sort, Arrays.sort, Stream.sorted). Understanding natural ordering (Comparable) vs external ordering (Comparator) determines where to place comparison logic. Comparator.comparing and method references reduce boilerplate while improving readability. Proper comparison logic handles nulls, prevents integer overflow, and maintains transitivity. Sorting is fundamental to data processing, reporting, and UI presentation in production systems.

---

### Example 20: Queue and Deque

Queue provides FIFO (First-In-First-Out) semantics. Deque (Double-Ended Queue) supports both FIFO and LIFO operations. `ArrayDeque` is preferred over legacy `Stack`.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    subgraph Queue["Queue (FIFO)"]
        Q1["offer()"] --> Q2["[1,2,3]"] --> Q3["poll()"]
    end

    subgraph Deque["Deque (Both ends)"]
        D1["addFirst()"] --> D2["[1,2,3]"] --> D3["addLast()"]
        D2 --> D4["removeFirst()"]
        D2 --> D5["removeLast()"]
    end

    style Q1 fill:#0173B2,color:#fff
    style Q2 fill:#DE8F05,color:#fff
    style Q3 fill:#029E73,color:#fff
    style D2 fill:#CC78BC,color:#fff
```

**Code**:

```java
import java.util.*;

// Queue interface - FIFO operations
Queue<String> queue = new LinkedList<>();

// offer() - adds element to end (returns boolean)
queue.offer("First"); // => true, queue: ["First"]
queue.offer("Second"); // => true, queue: ["First", "Second"]
queue.offer("Third"); // => true, queue: ["First", "Second", "Third"]

// peek() - retrieves but doesn't remove head
String head = queue.peek(); // => "First" (queue unchanged)

// poll() - retrieves and removes head
String removed = queue.poll(); // => "First", queue: ["Second", "Third"]
String next = queue.poll(); // => "Second", queue: ["Third"]

// poll() on empty queue returns null
queue.clear();
String empty = queue.poll(); // => null (safe, no exception)

// Priority Queue - elements ordered by natural order or comparator
PriorityQueue<Integer> pq = new PriorityQueue<>();
pq.offer(5);
pq.offer(2);
pq.offer(8);
pq.offer(1); // => Heap structure, not sorted array

System.out.println(pq.poll()); // => 1 (smallest)
System.out.println(pq.poll()); // => 2
System.out.println(pq.poll()); // => 5
System.out.println(pq.poll()); // => 8

// PriorityQueue with custom comparator (max heap)
PriorityQueue<Integer> maxHeap = new PriorityQueue<>(Comparator.reverseOrder());
maxHeap.offer(5);
maxHeap.offer(2);
maxHeap.offer(8);
System.out.println(maxHeap.poll()); // => 8 (largest first)

// Deque interface - double-ended queue
Deque<String> deque = new ArrayDeque<>();

// Add to front
deque.addFirst("A"); // => ["A"]
deque.addFirst("B"); // => ["B", "A"]

// Add to back
deque.addLast("C"); // => ["B", "A", "C"]
deque.addLast("D"); // => ["B", "A", "C", "D"]

// Remove from front
String first = deque.removeFirst(); // => "B", deque: ["A", "C", "D"]

// Remove from back
String last = deque.removeLast(); // => "D", deque: ["A", "C"]

// Peek at both ends
String peekFirst = deque.peekFirst(); // => "A"
String peekLast = deque.peekLast(); // => "C"

// Using Deque as Stack (LIFO)
Deque<Integer> stack = new ArrayDeque<>();
stack.push(1); // => [1]
stack.push(2); // => [2, 1]
stack.push(3); // => [3, 2, 1]

int top = stack.pop(); // => 3, stack: [2, 1]
int peek = stack.peek(); // => 2

// ArrayDeque vs LinkedList for Deque
// ArrayDeque: faster, less memory, no null elements
// LinkedList: allows nulls, implements both List and Deque

// Legacy Stack class (avoid - use Deque instead)
// Stack<Integer> oldStack = new Stack<>(); // Don't use
// Use Deque<Integer> stack = new ArrayDeque<>(); // Better
```

**Key Takeaway**: Queue provides FIFO with `offer()`, `poll()`, `peek()`. PriorityQueue orders elements automatically. Deque supports both ends: `addFirst()`, `addLast()`, `removeFirst()`, `removeLast()`. Use `ArrayDeque` as Stack instead of legacy `Stack` class. `LinkedList` implements both List and Deque.

**Why It Matters**: Queues enable FIFO ordering for task queues, message buffers, breadth-first search. Deques provide double-ended operations—used as stacks (LIFO) or queues (FIFO). BlockingQueue implementations (ArrayBlockingQueue, LinkedBlockingQueue) enable producer-consumer patterns with built-in thread coordination. Understanding when to use which queue (LinkedList vs ArrayDeque vs PriorityQueue) impacts performance. Queues are foundational to asynchronous processing, work distribution, and event handling in concurrent systems. They enable decoupling producers from consumers, improving scalability and resilience.

---

### Example 21: Streams Advanced Operations

Streams support complex transformations through chaining. `flatMap` flattens nested structures. Collectors enable powerful aggregations like grouping and partitioning.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    Source["Data Source<br/>[[1,2],[3,4]]"] --> FlatMap["flatMap<br/>Flatten to [1,2,3,4]"]
    FlatMap --> Filter["filter<br/>(even)"]
    Filter --> Collect["Collectors<br/>groupingBy/partitioningBy"]
    Collect --> Result["Result<br/>{true:[2,4]}"]

    style Source fill:#0173B2,color:#fff
    style FlatMap fill:#DE8F05,color:#fff
    style Filter fill:#029E73,color:#fff
    style Collect fill:#CC78BC,color:#fff
    style Result fill:#CA9161,color:#fff
```

**Code**:

```java
import java.util.*;
import java.util.stream.*;

// flatMap() - flattens nested structures
List<List<Integer>> nested = Arrays.asList(
    Arrays.asList(1, 2),
    Arrays.asList(3, 4),
    Arrays.asList(5, 6)
);

List<Integer> flattened = nested.stream()
    .flatMap(list -> list.stream()) // Flattens [[1,2],[3,4],[5,6]] to [1,2,3,4,5,6]
    .collect(Collectors.toList()); // => [1, 2, 3, 4, 5, 6]

// flatMap with Strings
List<String> words = Arrays.asList("Hello", "World");
List<String> letters = words.stream()
    .flatMap(word -> Arrays.stream(word.split(""))) // Split each word into letters
    .distinct()
    .collect(Collectors.toList()); // => [H, e, l, o, W, r, d]

// Collectors.groupingBy() - group elements by classifier
class Person {
    String name;
    String city;
    int age;

    Person(String name, String city, int age) {
        this.name = name;
        this.city = city;
        this.age = age;
    }
}

List<Person> people = Arrays.asList(
    new Person("Alice", "NYC", 30),
    new Person("Bob", "LA", 25),
    new Person("Charlie", "NYC", 35),
    new Person("David", "LA", 28)
);

Map<String, List<Person>> byCity = people.stream()
    .collect(Collectors.groupingBy(p -> p.city));
// => {NYC=[Alice, Charlie], LA=[Bob, David]}

// Collectors.partitioningBy() - split into two groups (true/false)
Map<Boolean, List<Person>> byAge = people.stream()
    .collect(Collectors.partitioningBy(p -> p.age >= 30));
// => {false=[Bob, David], true=[Alice, Charlie]}

// Collectors.joining() - concatenate strings
String names = people.stream()
    .map(p -> p.name)
    .collect(Collectors.joining(", ")); // => "Alice, Bob, Charlie, David"

String namesWithPrefix = people.stream()
    .map(p -> p.name)
    .collect(Collectors.joining(", ", "Names: ", "."));
// => "Names: Alice, Bob, Charlie, David."

// Stream.of() - create stream from elements
Stream<String> stream = Stream.of("A", "B", "C");

// Primitive streams - IntStream, LongStream, DoubleStream
IntStream ints = IntStream.range(1, 5); // => 1, 2, 3, 4 (5 excluded)
IntStream intsInclusive = IntStream.rangeClosed(1, 5); // => 1, 2, 3, 4, 5

int sum = IntStream.range(1, 101).sum(); // => 5050 (sum of 1 to 100)
double average = IntStream.range(1, 6).average().getAsDouble(); // => 3.0

// Parallel streams - leverage multiple cores
long count = IntStream.range(1, 1000000)
    .parallel() // Enables parallel processing
    .filter(n -> n % 2 == 0)
    .count(); // => 499999

// reduce() with accumulator - more control
List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);

// reduce(identity, accumulator)
int sumReduced = numbers.stream()
    .reduce(0, (a, b) -> a + b); // => 15

// reduce(identity, accumulator, combiner) for parallel
int sumParallel = numbers.parallelStream()
    .reduce(
        0,                    // Identity value
        (a, b) -> a + b,      // Accumulator (sequential)
        (a, b) -> a + b       // Combiner (parallel combine)
    ); // => 15

// Optional from stream operations
Optional<Integer> max = numbers.stream()
    .max(Comparator.naturalOrder()); // => Optional[5]

Optional<Integer> any = numbers.stream()
    .filter(n -> n > 10)
    .findAny(); // => Optional.empty (no element > 10)

// peek() - debug intermediate steps
List<Integer> result = numbers.stream()
    .peek(n -> System.out.println("Original: " + n))
    .filter(n -> n % 2 == 0)
    .peek(n -> System.out.println("Filtered: " + n))
    .map(n -> n * n)
    .peek(n -> System.out.println("Mapped: " + n))
    .collect(Collectors.toList());

// Collectors with downstream collectors
Map<String, Long> cityCount = people.stream()
    .collect(Collectors.groupingBy(
        p -> p.city,
        Collectors.counting() // Downstream collector
    )); // => {NYC=2, LA=2}

Map<String, Integer> citySumAge = people.stream()
    .collect(Collectors.groupingBy(
        p -> p.city,
        Collectors.summingInt(p -> p.age)
    )); // => {NYC=65, LA=53}
```

**Key Takeaway**: `flatMap()` flattens nested structures into single stream. `Collectors.groupingBy()` groups by classifier. `Collectors.partitioningBy()` splits into true/false groups. `Collectors.joining()` concatenates strings. Primitive streams (`IntStream`, `LongStream`, `DoubleStream`) optimize numeric operations. `parallel()` enables multi-core processing. `peek()` debugs intermediate operations.

**Why It Matters**: Advanced stream operations (flatMap, reduce, collect) enable complex data transformations declaratively. flatMap handles nested structures (lists of lists), reduce aggregates (sum, product, concatenation), and collectors build results (grouping, partitioning, joining). Understanding stateful operations (sorted, distinct) and their performance implications prevents bottlenecks. Stream pipelines eliminate boilerplate loops, reduce bugs, and improve readability. They're essential for data processing, reporting, and ETL pipelines. Mastering streams enables elegant solutions to otherwise complex data manipulation problems.

---

## Group 3: Functional Programming

### Example 22: Lambda Expressions

Lambdas provide concise syntax for functional interfaces (interfaces with one abstract method). Method references simplify common lambda patterns. Closures capture effectively final variables.

**Code**:

```java
import java.util.*;
import java.util.function.*;

// Lambda syntax: (parameters) -> expression
Runnable r1 = () -> System.out.println("Hello"); // No parameters
Consumer<String> c1 = s -> System.out.println(s); // One parameter (parentheses optional)
BiFunction<Integer, Integer, Integer> add = (a, b) -> a + b; // Multiple parameters
Function<String, Integer> length = s -> s.length(); // Single expression

// Lambda with block body
Consumer<String> printer = s -> {
    String upper = s.toUpperCase();
    System.out.println(upper);
}; // => requires { } and explicit return for non-void

// Functional interfaces (one abstract method)
@FunctionalInterface
interface Calculator {
    int calculate(int a, int b);
}

Calculator multiply = (a, b) -> a * b;
int result = multiply.calculate(5, 3); // => 15

// Built-in functional interfaces (java.util.function package)

// Predicate<T> - takes T, returns boolean
Predicate<Integer> isEven = n -> n % 2 == 0;
System.out.println(isEven.test(4)); // => true
System.out.println(isEven.test(5)); // => false

// Function<T, R> - takes T, returns R
Function<String, Integer> strLength = s -> s.length();
System.out.println(strLength.apply("Hello")); // => 5

// Consumer<T> - takes T, returns void
Consumer<String> print = s -> System.out.println(s);
print.accept("Message"); // => prints "Message"

// Supplier<T> - takes nothing, returns T
Supplier<Double> random = () -> Math.random();
System.out.println(random.get()); // => random double

// BiFunction<T, U, R> - takes T and U, returns R
BiFunction<Integer, Integer, Integer> sum = (a, b) -> a + b;
System.out.println(sum.apply(10, 20)); // => 30

// Method references - ClassName::methodName
// 1. Static method reference
Function<String, Integer> parse = Integer::parseInt;
int num = parse.apply("123"); // => 123

// 2. Instance method reference of specific object
String prefix = "Hello, ";
Function<String, String> greeter = prefix::concat;
System.out.println(greeter.apply("World")); // => "Hello, World"

// 3. Instance method of arbitrary object of particular type
Function<String, String> toUpper = String::toUpperCase;
System.out.println(toUpper.apply("java")); // => "JAVA"

// 4. Constructor reference
Supplier<ArrayList<String>> listSupplier = ArrayList::new;
ArrayList<String> list = listSupplier.get(); // => new ArrayList<>()

Function<Integer, int[]> arrayMaker = int[]::new;
int[] array = arrayMaker.apply(5); // => new int[5]

// Effectively final in closures
int multiplier = 10; // Effectively final (not modified after initialization)
Function<Integer, Integer> multiplyBy10 = n -> n * multiplier;
System.out.println(multiplyBy10.apply(5)); // => 50

// multiplier = 20; // ERROR: would make it not effectively final

// Lambda vs anonymous inner class
// Anonymous inner class
Runnable r2 = new Runnable() {
    @Override
    public void run() {
        System.out.println("Anonymous");
    }
};

// Lambda (much more concise for same result)
Runnable r3 = () -> System.out.println("Lambda");

// Lambdas in collections
List<String> names = Arrays.asList("Alice", "Bob", "Charlie");
names.forEach(name -> System.out.println(name)); // Consumer lambda
names.forEach(System.out::println); // Method reference (same as above)

// Lambdas with streams
List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
List<Integer> doubled = numbers.stream()
    .map(n -> n * 2) // Lambda
    .collect(Collectors.toList()); // => [2, 4, 6, 8, 10]

// Composing functional interfaces
Predicate<Integer> greaterThan5 = n -> n > 5;
Predicate<Integer> lessThan10 = n -> n < 10;
Predicate<Integer> between5And10 = greaterThan5.and(lessThan10);

System.out.println(between5And10.test(7)); // => true
System.out.println(between5And10.test(12)); // => false
```

**Key Takeaway**: Lambdas `(params) -> expression` provide concise syntax for functional interfaces. Built-in interfaces: `Predicate<T>`, `Function<T,R>`, `Consumer<T>`, `Supplier<T>`, `BiFunction<T,U,R>`. Method references (`::`) simplify lambdas. Closures capture effectively final variables. Lambdas are more concise than anonymous inner classes.

**Why It Matters**: Lambda expressions enable functional programming in Java—concise syntax for single-method implementations. They eliminate verbose anonymous classes, improving code readability. Understanding functional interfaces (Predicate, Function, Consumer, Supplier) enables effective library usage. Lambdas integrate with Streams, CompletableFuture, and event handling, making asynchronous and concurrent code more manageable. However, complex lambdas should use method references or named methods for clarity. Lambdas are foundational to modern Java development, enabling declarative programming patterns that reduce boilerplate and improve maintainability.

---

### Example 23: Optional for Null Safety

`Optional<T>` explicitly represents potential absence of a value, replacing null checks. Chaining methods avoid nested conditionals. Use for return types, not fields or parameters.

**Code**:

```java
import java.util.Optional;
import java.util.List;

// Creating Optional instances
Optional<String> present = Optional.of("value"); // => Optional["value"]
// Optional<String> nullValue = Optional.of(null); // ERROR: NullPointerException

Optional<String> maybe = Optional.ofNullable("value"); // => Optional["value"]
Optional<String> empty = Optional.ofNullable(null); // => Optional.empty

Optional<String> emptyDirect = Optional.empty(); // => Optional.empty

// Checking presence
if (present.isPresent()) {
    String value = present.get(); // => "value" (safe because we checked)
}

if (empty.isEmpty()) {  // Java 11+
    System.out.println("Empty!"); // => prints "Empty!"
}

// ifPresent() - execute if value present
present.ifPresent(value -> System.out.println(value)); // => prints "value"
empty.ifPresent(value -> System.out.println(value)); // => nothing (empty)

// orElse() - provide default value
String value1 = present.orElse("default"); // => "value"
String value2 = empty.orElse("default"); // => "default"

// orElseGet() - lazy default (Supplier called only if empty)
String value3 = empty.orElseGet(() -> "computed default"); // => "computed default"
String value4 = present.orElseGet(() -> "expensive computation"); // => "value" (supplier not called)

// orElseThrow() - throw exception if empty
try {
    String value5 = empty.orElseThrow(); // => throws NoSuchElementException
} catch (Exception e) {
    System.out.println("Exception: " + e.getMessage());
}

String value6 = empty.orElseThrow(() -> new IllegalStateException("Missing!"));
// => throws IllegalStateException

// map() - transform value if present
Optional<String> name = Optional.of("alice");
Optional<String> upper = name.map(String::toUpperCase); // => Optional["ALICE"]

Optional<String> emptyUpper = Optional.<String>empty()
    .map(String::toUpperCase); // => Optional.empty (map not applied)

// flatMap() - flatten nested Optionals
class Person {
    Optional<String> getEmail() {
        return Optional.of("person@example.com");
    }
}

Optional<Person> person = Optional.of(new Person());

// map returns Optional<Optional<String>> - nested!
Optional<Optional<String>> nestedEmail = person.map(Person::getEmail);

// flatMap flattens to Optional<String>
Optional<String> email = person.flatMap(Person::getEmail); // => Optional["person@example.com"]

// filter() - keep value if matches predicate
Optional<Integer> number = Optional.of(42);
Optional<Integer> filtered = number.filter(n -> n > 50); // => Optional.empty
Optional<Integer> kept = number.filter(n -> n > 40); // => Optional[42]

// Chaining operations
Optional<String> result = Optional.of("  HELLO  ")
    .map(String::trim)           // => Optional["HELLO"]
    .map(String::toLowerCase)    // => Optional["hello"]
    .filter(s -> s.length() > 3) // => Optional["hello"]
    .map(s -> s + "!"); // => Optional["hello!"]

String finalResult = result.orElse("default"); // => "hello!"

// Example: replacing null checks
// Traditional null checking (verbose)
String getNullable() {
    return Math.random() > 0.5 ? "value" : null;
}

String traditional = getNullable();
String processed;
if (traditional != null) {
    String upper = traditional.toUpperCase();
    if (upper.length() > 3) {
        processed = upper;
    } else {
        processed = "default";
    }
} else {
    processed = "default";
}

// Optional approach (cleaner)
Optional<String> getOptional() {
    return Math.random() > 0.5 ? Optional.of("value") : Optional.empty();
}

String processedOptional = getOptional()
    .map(String::toUpperCase)
    .filter(s -> s.length() > 3)
    .orElse("default");

// When NOT to use Optional
class BadPractice {
    // DON'T use Optional for fields
    // private Optional<String> name; // BAD

    // DON'T use Optional for method parameters
    // public void setName(Optional<String> name) {} // BAD

    // DON'T call get() without checking
    // String value = optional.get(); // BAD: may throw NoSuchElementException
}

// DO use Optional for return types
class GoodPractice {
    private String name;

    public Optional<String> getName() {
        return Optional.ofNullable(name); // GOOD
    }

    public void processName(String name) { // GOOD: plain parameter
        this.name = name;
    }
}

// Stream of Optionals (Java 9+)
List<Optional<String>> list = List.of(
    Optional.of("A"),
    Optional.empty(),
    Optional.of("B"),
    Optional.empty(),
    Optional.of("C")
);

List<String> values = list.stream()
    .flatMap(Optional::stream) // Flatten Optional to Stream
    .collect(Collectors.toList()); // => ["A", "B", "C"]
```

**Key Takeaway**: `Optional<T>` explicitly handles potential absence. Create with `of()`, `ofNullable()`, `empty()`. Check with `isPresent()`, `isEmpty()`. Extract with `get()` (after checking), `orElse()`, `orElseGet()`, `orElseThrow()`. Transform with `map()`, `flatMap()`, `filter()`. Use for return types, NOT fields or parameters. Chaining avoids nested null checks.

**Why It Matters**: Optional eliminates NullPointerException by forcing explicit absence handling. It provides a clear API (map, flatMap, orElse, orElseThrow) for null-safe transformations. Optional.ofNullable safely wraps possibly-null values from legacy APIs. However, Optional has overhead—use for method return types, not fields or parameters. Chaining Optional methods enables building null-safe pipelines without nested null checks. Optional improves code clarity by making absence explicit in type signatures, documenting intent, and preventing null-related production crashes that cost millions in losses annually.

---

### Example 24: Method References and Functional Composition

Method references provide cleaner syntax for lambdas that just call methods. Functional composition builds complex operations from simple functions.

**Code**:

```java
import java.util.*;
import java.util.function.*;

// Four types of method references

// 1. Static method reference: ClassName::staticMethod
Function<String, Integer> parseInt = Integer::parseInt;
int num = parseInt.apply("123"); // => 123

// Equivalent lambda
Function<String, Integer> parseIntLambda = s -> Integer.parseInt(s);

// 2. Instance method of specific object: object::instanceMethod
String prefix = "Hello, ";
Function<String, String> greeter = prefix::concat;
String greeting = greeter.apply("World"); // => "Hello, World"

// Equivalent lambda
Function<String, String> greeterLambda = s -> prefix.concat(s);

// 3. Instance method of arbitrary object: ClassName::instanceMethod
Function<String, Integer> length = String::length;
int len = length.apply("Java"); // => 4

// Equivalent lambda
Function<String, Integer> lengthLambda = s -> s.length();

List<String> words = Arrays.asList("apple", "banana", "cherry");
words.sort(String::compareToIgnoreCase); // => [apple, banana, cherry]

// 4. Constructor reference: ClassName::new
Supplier<List<String>> listMaker = ArrayList::new;
List<String> list = listMaker.get(); // => new ArrayList<>()

Function<Integer, int[]> arrayMaker = int[]::new;
int[] array = arrayMaker.apply(10); // => new int[10]

// Constructor with parameters
BiFunction<String, Integer, Person> personMaker = Person::new;
Person person = personMaker.apply("Alice", 30); // => new Person("Alice", 30)

// Function composition with andThen()
Function<Integer, Integer> multiplyBy2 = n -> n * 2;
Function<Integer, Integer> add3 = n -> n + 3;

Function<Integer, Integer> combined = multiplyBy2.andThen(add3);
int result1 = combined.apply(5); // => (5 * 2) + 3 = 13

// Function composition with compose()
Function<Integer, Integer> composed = add3.compose(multiplyBy2);
int result2 = composed.apply(5); // => (5 * 2) + 3 = 13 (same as andThen)

// Difference: order of execution
// f.andThen(g) = g(f(x))
// f.compose(g) = f(g(x))

Function<Integer, Integer> subtract1 = n -> n - 1;
Function<Integer, Integer> square = n -> n * n;

Function<Integer, Integer> squareThenSubtract = square.andThen(subtract1);
System.out.println(squareThenSubtract.apply(5)); // => 25 - 1 = 24

Function<Integer, Integer> subtractThenSquare = square.compose(subtract1);
System.out.println(subtractThenSquare.apply(5)); // => (5 - 1)² = 16

// Predicate composition
Predicate<Integer> greaterThan5 = n -> n > 5;
Predicate<Integer> even = n -> n % 2 == 0;

// and() - both predicates must be true
Predicate<Integer> evenAndGreaterThan5 = even.and(greaterThan5);
System.out.println(evenAndGreaterThan5.test(6)); // => true
System.out.println(evenAndGreaterThan5.test(4)); // => false

// or() - at least one predicate must be true
Predicate<Integer> evenOrGreaterThan5 = even.or(greaterThan5);
System.out.println(evenOrGreaterThan5.test(4)); // => true (even)
System.out.println(evenOrGreaterThan5.test(7)); // => true (greater than 5)
System.out.println(evenOrGreaterThan5.test(3)); // => false (neither)

// negate() - reverses predicate
Predicate<Integer> odd = even.negate();
System.out.println(odd.test(3)); // => true
System.out.println(odd.test(4)); // => false

// Consumer composition with andThen()
Consumer<String> print = s -> System.out.println(s);
Consumer<String> log = s -> System.err.println("Log: " + s);

Consumer<String> printAndLog = print.andThen(log);
printAndLog.accept("Message");
// => prints "Message" to stdout
// => prints "Log: Message" to stderr

// Practical example: data processing pipeline
List<String> names = Arrays.asList("alice", "BOB", "Charlie");

Function<String, String> trim = String::trim;
Function<String, String> lowercase = String::toLowerCase;
Function<String, String> capitalize = s ->
    s.substring(0, 1).toUpperCase() + s.substring(1);

Function<String, String> normalize = trim
    .andThen(lowercase)
    .andThen(capitalize);

List<String> normalized = names.stream()
    .map(normalize)
    .collect(Collectors.toList()); // => ["Alice", "Bob", "Charlie"]

// BiFunction doesn't have andThen/compose, but can be adapted
BiFunction<Integer, Integer, Integer> add = (a, b) -> a + b;
Function<Integer, Integer> times2 = n -> n * 2;

// Chain BiFunction result through Function
BiFunction<Integer, Integer, Integer> addThenTimes2 =
    (a, b) -> times2.apply(add.apply(a, b));

int result3 = addThenTimes2.apply(3, 4); // => (3 + 4) * 2 = 14
```

**Key Takeaway**: Method references (`::`) simplify lambdas: static (`ClassName::method`), instance (`object::method`), arbitrary (`ClassName::method`), constructor (`ClassName::new`). Function composition: `andThen()` executes f then g, `compose()` executes g then f. Predicate composition: `and()`, `or()`, `negate()`. Consumer composition: `andThen()`. Build complex operations by chaining simple functions.

**Why It Matters**: Method references provide syntactic sugar when lambdas just delegate to existing methods. They improve readability (String::toUpperCase vs s -> s.toUpperCase()) and integrate seamlessly with Streams and functional interfaces. Understanding the four types (static, instance, constructor, arbitrary object) enables appropriate usage. Method references shine in simple delegation but can obscure type information—balance conciseness with clarity. They're pervasive in modern Java codebases (Collections processing, Stream operations, event handlers), so recognizing and using them effectively is essential for idiomatic Java code.

---

## Group 4: I/O and File Handling

### Example 25: File I/O with NIO.2

NIO.2 (`java.nio.file` package) provides modern, efficient file operations. `Path` represents file system paths. `Files` offers rich static methods. `try-with-resources` ensures proper resource management.

**Code**:

```java
import java.nio.file.*;
import java.io.IOException;
import java.util.List;
import java.util.stream.Stream;

// Path interface - represents file system path
Path path = Paths.get("example.txt"); // => relative path
Path absolute = Paths.get("/Users/name/example.txt"); // => absolute path
Path multi = Paths.get("folder", "subfolder", "file.txt"); // => folder/subfolder/file.txt

// Reading files (Java 11+)
try {
    // readString() - reads entire file as String
    String content = Files.readString(path); // => file content as String
    System.out.println(content);
} catch (IOException e) {
    System.out.println("Error reading file: " + e.getMessage());
}

// readAllLines() - reads file as List<String>
try {
    List<String> lines = Files.readAllLines(path); // => each line as list element
    for (String line : lines) {
        System.out.println(line);
    }
} catch (IOException e) {
    System.out.println("Error: " + e.getMessage());
}

// Writing files
try {
    // writeString() - writes String to file (Java 11+)
    Files.writeString(path, "Hello, World!"); // => overwrites file

    // write() - writes bytes
    List<String> content = List.of("Line 1", "Line 2", "Line 3");
    Files.write(path, content); // => writes list as lines

    // Append to file
    Files.writeString(path, "\nAppended line", StandardOpenOption.APPEND);
} catch (IOException e) {
    System.out.println("Error writing: " + e.getMessage());
}

// File checks
boolean exists = Files.exists(path); // => true if file exists
boolean notExists = Files.notExists(path); // => true if doesn't exist
boolean isDirectory = Files.isDirectory(path); // => true if directory
boolean isRegularFile = Files.isRegularFile(path); // => true if regular file
boolean readable = Files.isReadable(path); // => true if readable
boolean writable = Files.isWritable(path); // => true if writable

// File properties
try {
    long size = Files.size(path); // => file size in bytes
    FileTime modifiedTime = Files.getLastModifiedTime(path); // => last modified time
} catch (IOException e) {
    System.out.println("Error: " + e.getMessage());
}

// Directory traversal with walk()
try (Stream<Path> paths = Files.walk(Paths.get("."))) {
    paths.filter(Files::isRegularFile)
         .filter(p -> p.toString().endsWith(".txt"))
         .forEach(System.out::println); // => prints all .txt files
} catch (IOException e) {
    System.out.println("Error walking: " + e.getMessage());
}

// File operations
try {
    // copy() - copies file
    Path source = Paths.get("source.txt");
    Path dest = Paths.get("dest.txt");
    Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING);

    // move() - moves or renames file
    Path newPath = Paths.get("renamed.txt");
    Files.move(dest, newPath, StandardCopyOption.REPLACE_EXISTING);

    // delete() - deletes file (throws exception if doesn't exist)
    Files.delete(newPath);

    // deleteIfExists() - deletes if exists (no exception)
    boolean deleted = Files.deleteIfExists(path); // => true if deleted

    // Creating directories
    Path dir = Paths.get("newFolder");
    Files.createDirectory(dir); // => creates single directory

    Path nested = Paths.get("parent/child/grandchild");
    Files.createDirectories(nested); // => creates all parent directories
} catch (IOException e) {
    System.out.println("Error: " + e.getMessage());
}

// try-with-resources for AutoCloseable
try (BufferedReader reader = Files.newBufferedReader(path)) {
    String line;
    while ((line = reader.readLine()) != null) {
        System.out.println(line);
    }
} catch (IOException e) {
    System.out.println("Error: " + e.getMessage());
} // reader auto-closed even if exception thrown

// Streaming lines (lazy loading for large files)
try (Stream<String> lines = Files.lines(path)) {
    lines.filter(line -> line.contains("keyword"))
         .forEach(System.out::println);
} catch (IOException e) {
    System.out.println("Error: " + e.getMessage());
}
```

**Key Takeaway**: NIO.2 provides modern file I/O. `Path` represents paths; `Paths.get()` creates them. `Files` offers static methods: `readString()`, `readAllLines()`, `writeString()`, `write()` for content. `exists()`, `isDirectory()`, `size()` for properties. `copy()`, `move()`, `delete()` for operations. `walk()` traverses directories. `try-with-resources` auto-closes resources.

**Why It Matters**: NIO.2 (java.nio.file) provides modern file I/O APIs replacing legacy java.io.File with cleaner behavior and better error handling. Files class offers convenient methods for reading/writing entire files, walking directory trees, and watching for changes. Path objects handle filesystem paths correctly across platforms (Windows vs Unix). Understanding buffered vs direct I/O impacts performance—buffering reduces system calls. File operations integrated with Streams enable processing large files line-by-line without loading entire contents into memory, essential for logs and datasets exceeding available RAM.

---

(Continue with remaining examples 26-35 to reach target length...)

### Example 26: Streams for I/O

Byte streams (`InputStream`/`OutputStream`) handle binary data. Character streams (`Reader`/`Writer`) handle text. Buffering improves performance. Bridge classes convert between byte and character streams.

**Code**:

```java
import java.io.*;

// Byte streams - InputStream/OutputStream
try (FileInputStream fis = new FileInputStream("data.bin");
     FileOutputStream fos = new FileOutputStream("output.bin")) {

    int byteRead;
    while ((byteRead = fis.read()) != -1) { // Read one byte at a time
        fos.write(byteRead); // Write byte
    }
} catch (IOException e) {
    e.printStackTrace();
}

// Character streams - Reader/Writer
try (FileReader reader = new FileReader("input.txt");
     FileWriter writer = new FileWriter("output.txt")) {

    int charRead;
    while ((charRead = reader.read()) != -1) {
        writer.write(charRead);
    }
} catch (IOException e) {
    e.printStackTrace();
}

// Buffered streams - improve performance
try (BufferedReader br = new BufferedReader(new FileReader("large.txt"));
     BufferedWriter bw = new BufferedWriter(new FileWriter("copy.txt"))) {

    String line;
    while ((line = br.readLine()) != null) { // Read line by line
        bw.write(line);
        bw.newLine(); // Platform-independent line separator
    }
} catch (IOException e) {
    e.printStackTrace();
}

// Bridge classes - InputStreamReader, OutputStreamWriter
try (InputStream is = new FileInputStream("data.txt");
     InputStreamReader isr = new InputStreamReader(is, "UTF-8");
     BufferedReader br = new BufferedReader(isr)) {

    String line = br.readLine(); // Convert bytes to characters
} catch (IOException e) {
    e.printStackTrace();
}
```

**Key Takeaway**: Byte streams (`InputStream`/`OutputStream`) for binary data. Character streams (`Reader`/`Writer`) for text. `BufferedReader`/`BufferedWriter` improve performance. `InputStreamReader`/`OutputStreamWriter` bridge byte and character streams. Always use `try-with-resources` for automatic closure.

**Why It Matters**: Lambda expressions enable functional programming patterns, reducing boilerplate in event handlers, callbacks, and collection processing. They power modern Java APIs like Streams, CompletableFuture, and reactive frameworks. Understanding lambda syntax and effectively final requirements prevents compilation errors. Method references provide concise alternatives when lambdas just delegate to methods. Functional interfaces enable strategy pattern without verbose anonymous classes. Lambdas make concurrent and asynchronous code more readable, improving maintainability in complex systems.

---

## Group 6: Concurrency Basics

### Example 32: Threads and Runnable

Threads enable concurrency. `Runnable` separates task from thread. `start()` creates new thread; `run()` executes in current thread. `join()` waits for completion.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    New["NEW<br/>Thread created"] --> Runnable["RUNNABLE<br/>start() called"]
    Runnable --> Running["RUNNING<br/>Executing"]
    Running --> Waiting["WAITING<br/>wait()/join()"]
    Running --> Blocked["BLOCKED<br/>Waiting for lock"]
    Waiting --> Runnable
    Blocked --> Runnable
    Running --> Terminated["TERMINATED<br/>Completed"]

    style New fill:#0173B2,color:#fff
    style Runnable fill:#DE8F05,color:#fff
    style Running fill:#029E73,color:#fff
    style Waiting fill:#CC78BC,color:#fff
    style Terminated fill:#CA9161,color:#fff
```

**Code**:

```java
// Runnable interface - task to execute
Runnable task = new Runnable() {
    @Override
    public void run() {
        System.out.println("Task running in: " + Thread.currentThread().getName());
    }
};

// Creating and starting thread
Thread thread = new Thread(task);
thread.start(); // Creates new thread and calls run()
// thread.run(); // WRONG: executes in current thread, doesn't create new thread

// Lambda version (Runnable is functional interface)
Thread lambdaThread = new Thread(() -> {
    System.out.println("Lambda task in: " + Thread.currentThread().getName());
});
lambdaThread.start();

// Thread.sleep() - pause execution
try {
    Thread.sleep(1000); // Sleep for 1000ms (1 second)
} catch (InterruptedException e) {
    e.printStackTrace();
}

// Thread.currentThread() - get current thread
Thread current = Thread.currentThread();
System.out.println("Name: " + current.getName());
System.out.println("ID: " + current.getId());

// join() - wait for thread to complete
Thread worker = new Thread(() -> {
    for (int i = 0; i < 5; i++) {
        System.out.println("Working: " + i);
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {}
    }
});

worker.start();
try {
    worker.join(); // Wait for worker to finish
    System.out.println("Worker completed");
} catch (InterruptedException e) {}

// Daemon threads - background threads that don't prevent JVM shutdown
Thread daemon = new Thread(() -> {
    while (true) {
        System.out.println("Daemon running");
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {}
    }
});
daemon.setDaemon(true); // Must be set before start()
daemon.start();

// Thread states (NEW, RUNNABLE, BLOCKED, WAITING, TIMED_WAITING, TERMINATED)
Thread t = new Thread(() -> {});
System.out.println(t.getState()); // => NEW
t.start();
System.out.println(t.getState()); // => RUNNABLE or TERMINATED (depends on timing)
```

**Key Takeaway**: Threads enable concurrency. `Runnable` defines tasks. `start()` creates new thread; `run()` executes in current thread. `Thread.sleep()` pauses execution. `join()` waits for completion. Daemon threads run in background. Thread states: NEW, RUNNABLE, BLOCKED, WAITING, TERMINATED.

**Why It Matters**: Parallel streams exploit multi-core CPUs, scaling throughput for CPU-bound operations on large datasets. They automatically partition data, process chunks concurrently, and merge results—no manual thread management. However, parallel streams have overhead—only beneficial for substantial workloads (thousands of elements, heavy computation). Stateful operations (sorted, distinct) and shared mutable state cause correctness issues. Understanding when parallelization helps (CPU-bound) vs hurts (I/O-bound, small datasets) prevents performance regressions. Measure before parallelizing—premature parallelization causes bugs.

---

## Group 5: Date/Time and Testing

### Example 27: Serialization and JSON

Serialization converts objects to byte streams for persistence. `Serializable` marker interface enables object serialization. `transient` keyword excludes fields. JSON is preferred for human-readable, language-agnostic data.

**Code**:

```java
import java.io.*;

// Serializable marker interface
class Person implements Serializable {
    private static final long serialVersionUID = 1L; // Version control

    private String name;
    private int age;
    private transient String password; // Excluded from serialization

    public Person(String name, int age, String password) {
        this.name = name;
        this.age = age;
        this.password = password;
    }
}

// Serialization - object to bytes
try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream("person.ser"))) {
    Person person = new Person("Alice", 30, "secret");
    oos.writeObject(person); // Serialize object
} catch (IOException e) {
    e.printStackTrace();
}

// Deserialization - bytes to object
try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream("person.ser"))) {
    Person person = (Person) ois.readObject(); // Deserialize
    System.out.println(person.name); // => "Alice"
    System.out.println(person.password); // => null (transient field)
} catch (IOException | ClassNotFoundException e) {
    e.printStackTrace();
}
```

**Key Takeaway**: `Serializable` enables object persistence. `serialVersionUID` maintains version compatibility. `transient` excludes sensitive fields. JSON (via Gson/Jackson) is preferred for human-readable interchange.

**Why It Matters**: Streams provide declarative data processing, separating what (filter, map, reduce) from how (parallelization, optimization). They eliminate boilerplate loops, reducing bugs from manual iteration logic. Lazy evaluation optimizes performance—intermediate operations don't execute until terminal operation triggers. Understanding stream lifecycle (create, intermediate, terminal) prevents accidental reuse. Parallel streams leverage multi-core CPUs transparently, scaling throughput. Streams integrate with Optional, enabling null-safe pipelines. Stream-based code is more concise, readable, and testable than imperative loops.

---

### Example 28: Modern Date and Time API

`java.time` (Java 8+) provides immutable, thread-safe date/time classes. `LocalDate`/`LocalTime`/`LocalDateTime` for human time. `Instant` for machine time. `ZonedDateTime` for time zones.

**Code**:

```java
import java.time.*;
import java.time.format.DateTimeFormatter;

// LocalDate - date without time
LocalDate today = LocalDate.now(); // => 2025-12-23
LocalDate birthday = LocalDate.of(1990, Month.JANUARY, 15); // => 1990-01-15

// LocalTime - time without date
LocalTime now = LocalTime.now(); // => 14:30:45.123
LocalTime noon = LocalTime.of(12, 0); // => 12:00

// LocalDateTime - date and time
LocalDateTime dateTime = LocalDateTime.now(); // => 2025-12-23T14:30:45
LocalDateTime specific = LocalDateTime.of(2025, 12, 25, 10, 30); // => 2025-12-25T10:30

// Instant - timestamp (machine time)
Instant instant = Instant.now(); // => 2025-12-23T07:30:45Z (UTC)

// ZonedDateTime - time with timezone
ZonedDateTime zoned = ZonedDateTime.now(ZoneId.of("America/New_York"));

// Period - duration in days/months/years
Period period = Period.between(birthday, today); // => 35 years
int years = period.getYears(); // => 35

// Duration - duration in hours/minutes/seconds
Duration duration = Duration.between(noon, now); // => PT2H30M45S
long seconds = duration.getSeconds(); // => 9045

// DateTimeFormatter - parsing and formatting
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
String formatted = dateTime.format(formatter); // => "2025-12-23 14:30"
LocalDateTime parsed = LocalDateTime.parse("2025-12-25 10:30", formatter);

// Comparing dates
boolean before = birthday.isBefore(today); // => true
boolean after = birthday.isAfter(today); // => false
```

**Key Takeaway**: `java.time` provides immutable, thread-safe classes. `LocalDate`/`LocalTime`/`LocalDateTime` for human-readable time. `Instant` for machine timestamps. `Period` for date-based duration. `Duration` for time-based duration. `DateTimeFormatter` for formatting.

**Why It Matters**: Optional eliminates NullPointerException—the billion-dollar mistake. It forces explicit handling of absence, preventing silent null propagation that causes cryptic errors later. Optional.ofNullable safely wraps possibly-null values from legacy APIs. Chaining (map, flatMap) enables null-safe transformations without nested null checks. However, Optional has overhead—don't use for fields or method parameters (use null checks). It's designed for method return types signaling possible absence. Proper Optional usage improves code clarity and prevents null-related production crashes.

---

### Example 29: Regular Expressions

`Pattern` and `Matcher` enable powerful text matching. `String` methods provide convenience for simple cases. Groups extract matched substrings.

**Code**:

```java
import java.util.regex.*;

// Pattern.compile() and Matcher
Pattern pattern = Pattern.compile("\\d+"); // Regex: one or more digits
Matcher matcher = pattern.matcher("Order 123 costs $45");

// find() - find next match
while (matcher.find()) {
    System.out.println(matcher.group()); // => "123", "45"
}

// matches() - entire string must match
boolean fullMatch = Pattern.matches("\\d{3}", "123"); // => true (exactly 3 digits)
boolean noMatch = Pattern.matches("\\d{3}", "12"); // => false

// String convenience methods
String text = "Contact: alice@example.com";
boolean hasEmail = text.matches(".*@.*\\..*"); // => true
String[] parts = text.split(": "); // => ["Contact", "alice@example.com"]
String replaced = text.replaceAll("@", " AT "); // => "Contact: alice AT example.com"

// Groups - capturing parts of matches
Pattern emailPattern = Pattern.compile("(\\w+)@(\\w+\\.\\w+)");
Matcher emailMatcher = emailPattern.matcher("alice@example.com");

if (emailMatcher.find()) {
    String full = emailMatcher.group(0); // => "alice@example.com" (entire match)
    String username = emailMatcher.group(1); // => "alice" (first group)
    String domain = emailMatcher.group(2); // => "example.com" (second group)
}

// Pattern flags
Pattern caseInsensitive = Pattern.compile("hello", Pattern.CASE_INSENSITIVE);
Matcher m = caseInsensitive.matcher("HELLO World");
System.out.println(m.find()); // => true
```

**Key Takeaway**: `Pattern` and `Matcher` enable regex matching. `find()` finds next match. `matches()` checks entire string. `group()` extracts matched text. String methods (`matches()`, `split()`, `replaceAll()`) provide convenience for simple patterns.

**Why It Matters**: Method references provide syntactic sugar when lambdas just delegate to methods, improving readability. They integrate seamlessly with Stream operations (map, filter, forEach). Understanding the four types (static, instance, constructor, arbitrary object) enables appropriate usage. Method references reduce verbosity but can obscure type information—balance conciseness with clarity. They're pervasive in modern Java codebases, so recognizing them is essential. However, complex lambdas should remain explicit for clarity. Method references shine in simple delegation scenarios.

---

### Example 30: JUnit Testing Basics

JUnit enables automated unit testing. `@Test` marks test methods. Assertions verify expected behavior. Lifecycle annotations organize setup/teardown.

**Code**:

```java
import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.*;

class Calculator {
    public int add(int a, int b) {
        return a + b;
    }

    public int divide(int a, int b) {
        if (b == 0) throw new IllegalArgumentException("Division by zero");
        return a / b;
    }
}

class CalculatorTest {
    private Calculator calc;

    @BeforeEach // Runs before each test method
    void setUp() {
        calc = new Calculator();
    }

    @Test // Marks a test method
    void testAdd() {
        int result = calc.add(2, 3);
        assertEquals(5, result); // Assert expected == actual
    }

    @Test
    void testDivide() {
        int result = calc.divide(10, 2);
        assertEquals(5, result);
    }

    @Test
    void testDivideByZero() {
        // assertThrows - verify exception thrown
        assertThrows(IllegalArgumentException.class, () -> {
            calc.divide(10, 0);
        });
    }

    @Test
    void testAssertions() {
        assertTrue(5 > 3); // Assert boolean condition
        assertFalse(5 < 3);
        assertNotNull(calc); // Assert not null
        assertNull(null); // Assert null
    }

    @AfterEach // Runs after each test method
    void tearDown() {
        calc = null;
    }

    @BeforeAll // Runs once before all tests (static)
    static void setUpClass() {
        System.out.println("Starting tests");
    }

    @AfterAll // Runs once after all tests (static)
    static void tearDownClass() {
        System.out.println("Tests complete");
    }
}
```

**Key Takeaway**: `@Test` marks test methods. Assertions (`assertEquals`, `assertTrue`, `assertNotNull`, `assertThrows`) verify behavior. `@BeforeEach`/`@AfterEach` for per-test setup. `@BeforeAll`/`@AfterAll` for class-level setup. Test isolation ensures reliability.

**Why It Matters**: Functional interfaces enable treating behavior as data—passing functions as arguments, returning functions. They power callback patterns, event handling, and strategy pattern implementations. SAM (Single Abstract Method) interfaces from pre-Java 8 code automatically work with lambdas. Understanding common functional interfaces (Predicate, Function, Consumer) enables effective library usage. Custom functional interfaces should use @FunctionalInterface annotation for compile-time validation. Functional programming patterns reduce coupling and improve testability through dependency injection of behavior.

---

### Example 31: Mockito for Testing

Mockito simulates dependencies for isolated testing. `@Mock` creates mock objects. `when().thenReturn()` stubs behavior. `verify()` checks interactions.

**Code**:

```java
import org.mockito.*;
import static org.mockito.Mockito.*;

interface UserRepository {
    User findById(Long id);
    void save(User user);
}

class UserService {
    private UserRepository repository;

    public UserService(UserRepository repository) {
        this.repository = repository;
    }

    public User getUser(Long id) {
        return repository.findById(id);
    }
}

class UserServiceTest {
    @Mock
    private UserRepository mockRepository;

    @InjectMocks
    private UserService service;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testGetUser() {
        // Arrange: stub mock behavior
        User expected = new User(1L, "Alice");
        when(mockRepository.findById(1L)).thenReturn(expected);

        // Act: call method under test
        User actual = service.getUser(1L);

        // Assert: verify result
        assertEquals(expected, actual);

        // Verify: check interaction
        verify(mockRepository).findById(1L); // Verify method was called
    }
}
```

**Key Takeaway**: Mockito creates mock dependencies. `@Mock` creates mocks. `@InjectMocks` injects mocks into tested class. `when().thenReturn()` stubs method behavior. `verify()` checks method was called with expected arguments.

**Why It Matters**: Collectors transform Streams into concrete collections, aggregations, or custom data structures. They enable concise data processing—grouping, partitioning, aggregating—without manual loops. Downstream collectors compose operations (group then count, group then average). Understanding Collectors.toMap handle duplicates prevents runtime exceptions. Custom collectors enable domain-specific aggregations. Collectors are the bridge between declarative Stream processing and concrete results, making complex data transformations simple and readable.

---

### Example 33: Thread-Safe Session Management

Production applications need thread-safe session management where multiple threads concurrently create, access, and clean up user sessions. `ConcurrentHashMap` with atomic operations enables lock-free, high-performance session handling.

**Code**:

```java
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.concurrent.*;

// Session data model
class Session {
    private final String userId;
    private final Instant createdAt;
    private Instant expiresAt;
    private final ConcurrentHashMap<String, Object> attributes;

    public Session(String userId, int expiryMinutes) {
        this.userId = userId;
        this.createdAt = Instant.now();
        this.expiresAt = Instant.now().plus(expiryMinutes, ChronoUnit.MINUTES);
        this.attributes = new ConcurrentHashMap<>();
    }

    public boolean isExpired() {
        return Instant.now().isAfter(expiresAt); // => true if expired
    }

    public void renewExpiry(int minutes) {
        this.expiresAt = Instant.now().plus(minutes, ChronoUnit.MINUTES);
    }

    public void setAttribute(String key, Object value) {
        attributes.put(key, value); // Thread-safe put
    }

    public Object getAttribute(String key) {
        return attributes.get(key);
    }

    public String getUserId() { return userId; }
    public Instant getExpiresAt() { return expiresAt; }
}

// Thread-safe session manager using ConcurrentHashMap
class UserSessionManager {
    private final ConcurrentHashMap<String, Session> sessions;
    private final int defaultExpiryMinutes;

    public UserSessionManager(int expiryMinutes) {
        this.sessions = new ConcurrentHashMap<>();
        this.defaultExpiryMinutes = expiryMinutes;
    }

    // Atomic get-or-create pattern using computeIfAbsent
    public Session getOrCreateSession(String userId) {
        return sessions.computeIfAbsent(userId, id -> {
            Session session = new Session(id, defaultExpiryMinutes);
            System.out.println("Created new session for: " + id);
            return session; // => Atomically creates if absent
        });
    }

    // Get existing session (no creation)
    public Session getSession(String userId) {
        Session session = sessions.get(userId);
        if (session != null && session.isExpired()) {
            sessions.remove(userId); // Clean up expired
            return null; // => Session expired
        }
        return session;
    }

    // Renew session expiry
    public void renewSession(String userId, int minutes) {
        sessions.computeIfPresent(userId, (id, session) -> {
            session.renewExpiry(minutes);
            System.out.println("Renewed session for: " + id);
            return session; // => Returns updated session
        });
    }

    // Invalidate (logout)
    public void invalidateSession(String userId) {
        Session removed = sessions.remove(userId);
        if (removed != null) {
            System.out.println("Invalidated session for: " + userId);
        }
    }

    // Clean up all expired sessions (periodic cleanup task)
    public int cleanupExpiredSessions() {
        int cleaned = 0;
        for (var entry : sessions.entrySet()) {
            if (entry.getValue().isExpired()) {
                sessions.remove(entry.getKey()); // Thread-safe removal
                cleaned++;
            }
        }
        System.out.println("Cleaned up " + cleaned + " expired sessions");
        return cleaned; // => Number of sessions removed
    }

    // Get active session count
    public int getActiveSessionCount() {
        return sessions.size();
    }

    // Atomic merge operation - update session attributes
    public void updateSessionAttribute(String userId, String key, Object value) {
        sessions.computeIfPresent(userId, (id, session) -> {
            session.setAttribute(key, value);
            return session; // => Updated session
        });
    }
}

// Example usage with multiple concurrent threads
UserSessionManager sessionManager = new UserSessionManager(30); // 30-minute expiry

// Thread 1: User login
new Thread(() -> {
    Session session = sessionManager.getOrCreateSession("user123");
    session.setAttribute("role", "admin");
    // => Created new session for: user123
}).start();

// Thread 2: Same user (concurrent access)
new Thread(() -> {
    Session session = sessionManager.getOrCreateSession("user123");
    // => Returns existing session (no duplicate creation)
    String role = (String) session.getAttribute("role");
    System.out.println("Role: " + role); // => "admin"
}).start();

// Thread 3: Periodic cleanup task
ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);
scheduler.scheduleAtFixedRate(
    () -> sessionManager.cleanupExpiredSessions(),
    1, 5, TimeUnit.MINUTES // => Run every 5 minutes
);

// Graceful shutdown
scheduler.shutdown();

// Demonstrating atomic operations
Session s1 = sessionManager.getOrCreateSession("alice"); // => Creates new
Session s2 = sessionManager.getOrCreateSession("alice"); // => Returns same instance
System.out.println(s1 == s2); // => true (atomic operation ensures single instance)

sessionManager.invalidateSession("alice"); // => Logout
Session s3 = sessionManager.getSession("alice"); // => null (invalidated)
```

**Key Takeaway**: `ConcurrentHashMap` provides thread-safe operations without explicit locking. `computeIfAbsent()` atomically creates sessions (get-or-create pattern). `computeIfPresent()` atomically updates existing entries. This pattern scales better than `synchronized` for read-heavy workloads because `ConcurrentHashMap` uses lock striping. Use scheduled tasks for periodic cleanup of expired sessions. Production systems use this pattern for user sessions, caches, and connection pools.

**Why It Matters**: Grouping and partitioning enable data aggregation—histogram generation, pivot tables, category-based analysis. They replace verbose imperative loops with declarative operations. Downstream collectors enable multi-level aggregation (group then average, partition then count). Partitioning is optimized for two-way splits (true/false predicates). These operations are essential for reporting, analytics, and data transformation pipelines. Understanding complex collectors enables elegant solutions to otherwise tedious data processing problems.

---

### Example 34: ExecutorService and Thread Pools

`ExecutorService` manages thread pools efficiently. `submit()` returns `Future` for async results. `Callable` returns values; `Runnable` doesn't.

**Code**:

```java
import java.util.concurrent.*;

// Fixed thread pool
ExecutorService executor = Executors.newFixedThreadPool(4); // 4 threads

// Submit Runnable (no return value)
executor.submit(() -> {
    System.out.println("Task running");
});

// Submit Callable (returns value)
Future<Integer> future = executor.submit(() -> {
    Thread.sleep(1000);
    return 42;
});

try {
    Integer result = future.get(); // Blocks until result available
    System.out.println(result); // => 42
} catch (Exception e) {
    e.printStackTrace();
}

// Shutdown executor
executor.shutdown(); // No new tasks accepted
try {
    if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
        executor.shutdownNow(); // Force shutdown
    }
} catch (InterruptedException e) {
    executor.shutdownNow();
}
```

**Key Takeaway**: `ExecutorService` manages thread pools. `newFixedThreadPool()` creates fixed-size pool. `submit()` submits tasks, returns `Future`. `Callable` returns values. Always `shutdown()` and `awaitTermination()`.

**Why It Matters**: Threads enable concurrency—handling multiple requests simultaneously, performing background tasks, improving responsiveness. However, threads are expensive (1MB stack space each), and manual thread management is error-prone (deadlocks, races, resource leaks). Extending Thread is inflexible (single inheritance)—prefer Runnable/Callable. Understanding thread lifecycle (new, runnable, blocked, terminated) prevents resource leaks. Modern Java prefers higher-level abstractions (ExecutorService, CompletableFuture) over raw threads. Threads are foundational to understanding Java's concurrency model.

---

### Example 35: CompletableFuture for Async

`CompletableFuture` enables async programming without callback hell. Chaining methods compose operations. `exceptionally()` handles errors.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    Supply["supplyAsync()<br/>Start async"] --> Then["thenApply()<br/>Transform"]
    Then --> Accept["thenAccept()<br/>Consume"]
    Then --> Error["exceptionally()<br/>Handle error"]

    style Supply fill:#0173B2,color:#fff
    style Then fill:#DE8F05,color:#fff
    style Accept fill:#029E73,color:#fff
    style Error fill:#CC78BC,color:#fff
```

**Code**:

```java
import java.util.concurrent.CompletableFuture;

// supplyAsync - start async computation
CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> {
    // Runs in separate thread
    return "Hello";
});

// thenApply - transform result
future.thenApply(s -> s + " World")
      .thenApply(String::toUpperCase)
      .thenAccept(s -> System.out.println(s)); // => "HELLO WORLD"

// exceptionally - handle errors
CompletableFuture<Integer> withError = CompletableFuture.supplyAsync(() -> {
    if (true) throw new RuntimeException("Error!");
    return 42;
}).exceptionally(ex -> {
    System.out.println("Caught: " + ex.getMessage());
    return 0; // Default value
});

// thenCombine - combine two futures
CompletableFuture<Integer> f1 = CompletableFuture.supplyAsync(() -> 10);
CompletableFuture<Integer> f2 = CompletableFuture.supplyAsync(() -> 20);

CompletableFuture<Integer> combined = f1.thenCombine(f2, (a, b) -> a + b);
System.out.println(combined.join()); // => 30

// allOf - wait for all futures
CompletableFuture<Void> all = CompletableFuture.allOf(f1, f2);
all.join(); // Waits for both to complete
```

**Key Takeaway**: `CompletableFuture` enables async programming. `supplyAsync()` starts async computation. `thenApply()` transforms, `thenAccept()` consumes. `exceptionally()` handles errors. `thenCombine()` combines futures. `allOf()`/`anyOf()` wait for multiple futures.

**Why It Matters**: ExecutorService abstracts thread management, providing thread pools that reuse threads, reducing overhead. Fixed thread pools limit concurrency preventing resource exhaustion. Cached thread pools scale dynamically for I/O-bound tasks. Understanding executor shutdown (shutdown vs shutdownNow) prevents zombie threads. Executors enable decoupling task submission from execution policy—change from single-threaded to multi-threaded without modifying task code. Proper thread pool sizing (CPU-bound: cores, I/O-bound: cores \* blocking factor) optimizes throughput. Production systems use executors for all concurrent work.

---

## Group 6: Advanced Streams and Collectors

### Example 36: Custom Collectors

Custom collectors enable specialized aggregation logic beyond built-in collectors. Implement `Collector` interface for domain-specific operations. Use `Collector.of()` for simple cases.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Stream["Stream Elements"] --> Supplier["supplier()<br/>Create container"]
    Supplier --> Accumulator["accumulator()<br/>Add elements"]
    Accumulator --> Combiner["combiner()<br/>Merge containers"]
    Combiner --> Finisher["finisher()<br/>Transform result"]
    Finisher --> Result["Final Result"]

    style Stream fill:#0173B2,color:#fff
    style Supplier fill:#DE8F05,color:#fff
    style Accumulator fill:#029E73,color:#fff
    style Combiner fill:#CC78BC,color:#fff
    style Finisher fill:#CA9161,color:#fff
    style Result fill:#0173B2,color:#fff
```

**Code**:

```java
import java.util.stream.*;
import java.util.*;
import java.util.function.*;

// Built-in collectors - common patterns
List<String> names = List.of("Alice", "Bob", "Charlie", "David"); // => 4 names

// toList - collect to ArrayList
List<String> list = names.stream()
    .filter(s -> s.length() > 3) // => Filter: Alice, Charlie, David
    .collect(Collectors.toList()); // => [Alice, Charlie, David]

// toSet - collect to HashSet (removes duplicates)
Set<Integer> lengths = names.stream()
    .map(String::length) // => [5, 3, 7, 5]
    .collect(Collectors.toSet()); // => [3, 5, 7] (unique lengths)

// toMap - collect to HashMap
Map<String, Integer> nameToLength = names.stream()
    .collect(Collectors.toMap(
        name -> name, // => Key function
        String::length // => Value function
    )); // => {Alice=5, Bob=3, Charlie=7, David=5}

// joining - concatenate strings
String joined = names.stream()
    .collect(Collectors.joining(", ")); // => "Alice, Bob, Charlie, David"

String withPrefix = names.stream()
    .collect(Collectors.joining(", ", "[", "]")); // => "[Alice, Bob, Charlie, David]"

// groupingBy - group elements by classifier
Map<Integer, List<String>> byLength = names.stream()
    .collect(Collectors.groupingBy(String::length));
// => {3=[Bob], 5=[Alice, David], 7=[Charlie]}

// partitioningBy - split into two groups (true/false)
Map<Boolean, List<String>> partition = names.stream()
    .collect(Collectors.partitioningBy(s -> s.length() > 4));
// => {false=[Bob], true=[Alice, Charlie, David]}

// counting - count elements
long count = names.stream()
    .collect(Collectors.counting()); // => 4

// summingInt - sum integer values
int totalLength = names.stream()
    .collect(Collectors.summingInt(String::length)); // => 5+3+7+5 = 20

// averagingInt - average of integer values
double avgLength = names.stream()
    .collect(Collectors.averagingInt(String::length)); // => 20/4 = 5.0

// summarizingInt - statistics (count, sum, min, max, average)
IntSummaryStatistics stats = names.stream()
    .collect(Collectors.summarizingInt(String::length));
System.out.println(stats.getCount()); // => 4
System.out.println(stats.getSum()); // => 20
System.out.println(stats.getMin()); // => 3
System.out.println(stats.getMax()); // => 7
System.out.println(stats.getAverage()); // => 5.0

// Custom collector using Collector.of()
// Example: collect to immutable list
Collector<String, List<String>, List<String>> toImmutableList = Collector.of(
    ArrayList::new, // => Supplier: create mutable container
    List::add, // => Accumulator: add element to container
    (list1, list2) -> { // => Combiner: merge two containers
        list1.addAll(list2); // => Combine lists
        return list1; // => Return merged list
    },
    Collections::unmodifiableList // => Finisher: make immutable
);

List<String> immutable = names.stream()
    .collect(toImmutableList); // => Immutable list
// immutable.add("Eve"); // => Throws UnsupportedOperationException

// Custom collector: concatenate with custom separator and brackets
Collector<String, StringBuilder, String> customJoin = Collector.of(
    StringBuilder::new, // => Supplier: create StringBuilder
    (sb, s) -> { // => Accumulator: add element
        if (sb.length() > 0) sb.append(" | "); // => Add separator if not first
        sb.append(s); // => Append element
    },
    (sb1, sb2) -> { // => Combiner: merge StringBuilders
        if (sb1.length() > 0 && sb2.length() > 0) {
            sb1.append(" | "); // => Add separator between merged parts
        }
        return sb1.append(sb2); // => Combine and return
    },
    sb -> "[" + sb.toString() + "]" // => Finisher: wrap in brackets
);

String custom = names.stream()
    .collect(customJoin); // => "[Alice | Bob | Charlie | David]"

// Downstream collectors - combine collectors
Map<Integer, Long> lengthCounts = names.stream()
    .collect(Collectors.groupingBy(
        String::length, // => Group by length
        Collectors.counting() // => Count elements in each group
    )); // => {3=1, 5=2, 7=1}

Map<Integer, String> lengthToNames = names.stream()
    .collect(Collectors.groupingBy(
        String::length, // => Group by length
        Collectors.joining(", ") // => Join names in each group
    )); // => {3=Bob, 5=Alice, David, 7=Charlie}

// collectingAndThen - transform collector result
List<String> unmodifiable = names.stream()
    .collect(Collectors.collectingAndThen(
        Collectors.toList(), // => Collect to list first
        Collections::unmodifiableList // => Then make immutable
    )); // => Immutable list

// teeing - apply two collectors and combine results (Java 12+)
record MinMax(String min, String max) {} // => Record for result

MinMax minMax = names.stream()
    .collect(Collectors.teeing(
        Collectors.minBy(Comparator.naturalOrder()), // => First collector: min
        Collectors.maxBy(Comparator.naturalOrder()), // => Second collector: max
        (min, max) -> new MinMax(min.orElse(""), max.orElse("")) // => Combine results
    )); // => MinMax[min=Alice, max=David]

// filtering - filter before collecting (Java 9+)
Map<Integer, List<String>> longNamesGrouped = names.stream()
    .collect(Collectors.groupingBy(
        String::length,
        Collectors.filtering(s -> s.length() > 4, Collectors.toList()) // => Filter in collector
    )); // => {3=[], 5=[Alice, David], 7=[Charlie]}

// mapping - map before collecting
Map<Integer, List<Integer>> lengthToChars = names.stream()
    .collect(Collectors.groupingBy(
        String::length,
        Collectors.mapping(s -> s.charAt(0) - 'A', Collectors.toList()) // => Map to first char
    )); // => {3=[1], 5=[0, 3], 7=[2]}
```

**Key Takeaway**: Built-in collectors handle common aggregations: `toList()`, `toSet()`, `toMap()`, `joining()`, `groupingBy()`, `partitioningBy()`. Collectors compose with downstream collectors. Custom collectors use `Collector.of()` with supplier, accumulator, combiner, and finisher. Use `collectingAndThen()` to transform final result. `teeing()` applies two collectors simultaneously.

**Why It Matters**: CompletableFuture enables non-blocking asynchronous programming with composable operations—chaining, combining, error handling. It prevents blocking threads while waiting for I/O (network, disk, external APIs), improving scalability. Understanding completion stages (thenApply, thenCompose, thenCombine) enables building async pipelines. Exception handling (exceptionally, handle) prevents silent failures. CompletableFuture powers modern reactive frameworks (Spring WebFlux) and async HTTP clients. It's essential for building responsive, scalable applications that efficiently utilize system resources.

---

### Example 37: Method References Deep Dive

Method references provide concise syntax for lambda expressions that call existing methods. Four types: static, instance, constructor, and arbitrary object. Understand when to use each type.

**Code**:

```java
import java.util.*;
import java.util.function.*;

// 1. Static method reference - ClassName::staticMethod
List<String> numbers = List.of("1", "2", "3", "4"); // => String numbers

// Lambda version
List<Integer> parsed1 = numbers.stream()
    .map(s -> Integer.parseInt(s)) // => Lambda calling static method
    .toList(); // => [1, 2, 3, 4]

// Method reference version
List<Integer> parsed2 = numbers.stream()
    .map(Integer::parseInt) // => Static method reference (cleaner)
    .toList(); // => [1, 2, 3, 4]

// 2. Instance method reference on specific object - object::instanceMethod
String prefix = "Number: "; // => Specific String object

// Lambda version
List<String> prefixed1 = numbers.stream()
    .map(s -> prefix.concat(s)) // => Lambda calling instance method on prefix
    .toList(); // => ["Number: 1", "Number: 2", "Number: 3", "Number: 4"]

// Method reference version
List<String> prefixed2 = numbers.stream()
    .map(prefix::concat) // => Instance method reference on prefix object
    .toList(); // => ["Number: 1", "Number: 2", "Number: 3", "Number: 4"]

// 3. Instance method reference on arbitrary object - ClassName::instanceMethod
List<String> names = List.of("alice", "BOB", "Charlie"); // => Mixed case names

// Lambda version
List<String> upper1 = names.stream()
    .map(s -> s.toUpperCase()) // => Lambda calling instance method on each element
    .toList(); // => ["ALICE", "BOB", "CHARLIE"]

// Method reference version
List<String> upper2 = names.stream()
    .map(String::toUpperCase) // => Instance method reference (each element calls toUpperCase)
    .toList(); // => ["ALICE", "BOB", "CHARLIE"]

// Sorting with instance method reference
List<String> sorted = names.stream()
    .sorted(String::compareToIgnoreCase) // => Instance method reference for comparator
    .toList(); // => ["alice", "BOB", "Charlie"] (case-insensitive sort)

// 4. Constructor reference - ClassName::new
List<Integer> lengths = List.of(5, 3, 7, 4); // => Integer list

// Lambda version
List<String> strings1 = lengths.stream()
    .map(len -> new String(new char[len]).replace('\0', 'x')) // => Lambda creating objects
    .toList(); // => ["xxxxx", "xxx", "xxxxxxx", "xxxx"]

// Constructor reference for simple cases
class Person {
    private String name; // => Person field

    public Person(String name) {
        this.name = name; // => Constructor initializes name
    }

    public String getName() { return name; } // => Getter
}

// Constructor reference
List<Person> people = names.stream()
    .map(Person::new) // => Constructor reference (equivalent to s -> new Person(s))
    .toList(); // => List of Person objects

// Array constructor reference
String[] array = names.stream()
    .toArray(String[]::new); // => Array constructor reference
// => Creates String[] of appropriate size: ["alice", "BOB", "Charlie"]

// Method reference with multiple parameters
BiFunction<String, String, String> concat1 = (a, b) -> a.concat(b); // => Lambda
BiFunction<String, String, String> concat2 = String::concat; // => Method reference

String result = concat2.apply("Hello", " World"); // => "Hello World"

// Method reference in forEach
names.stream()
    .forEach(System.out::println); // => Instance method reference on System.out
// => Prints: alice, BOB, Charlie

// Method reference with Comparator
List<String> byLength = names.stream()
    .sorted(Comparator.comparing(String::length)) // => Method reference in comparing()
    .toList(); // => ["BOB", "alice", "Charlie"] (sorted by length)

List<String> byLengthReversed = names.stream()
    .sorted(Comparator.comparing(String::length).reversed()) // => Reversed comparator
    .toList(); // => ["Charlie", "alice", "BOB"]

// Chained comparators with method references
record Employee(String name, int age, double salary) {} // => Employee record

List<Employee> employees = List.of(
    new Employee("Alice", 30, 50000), // => Employee 1
    new Employee("Bob", 25, 45000), // => Employee 2
    new Employee("Charlie", 30, 55000) // => Employee 3
);

List<Employee> sorted2 = employees.stream()
    .sorted(Comparator
        .comparing(Employee::age) // => Sort by age first
        .thenComparing(Employee::salary) // => Then by salary
        .reversed()) // => Reverse entire order
    .toList(); // => [Charlie(30, 55000), Alice(30, 50000), Bob(25, 45000)]

// Method reference with Optional
Optional<String> opt = Optional.of("hello"); // => Optional containing "hello"

// Lambda version
opt.map(s -> s.toUpperCase()).ifPresent(s -> System.out.println(s)); // => "HELLO"

// Method reference version
opt.map(String::toUpperCase) // => Method reference for map
   .ifPresent(System.out::println); // => Method reference for ifPresent: "HELLO"

// Method reference limitations
// Cannot use when:
// 1. Lambda has additional logic beyond method call
List<String> withLogic = names.stream()
    .map(s -> {
        System.out.println("Processing: " + s); // => Extra logic
        return s.toUpperCase(); // => Cannot use method reference here
    })
    .toList();

// 2. Lambda modifies parameters before passing
List<String> modified = names.stream()
    .map(s -> Integer.parseInt(s.trim())) // => Can't use Integer::parseInt (needs trim first)
    .toList();

// Practical patterns with method references
class StringUtils {
    public static String reverse(String s) {
        return new StringBuilder(s).reverse().toString(); // => Reverse string
    }
}

List<String> reversed = names.stream()
    .map(StringUtils::reverse) // => Static method reference to custom utility
    .toList(); // => ["ecila", "BOB", "eilrahC"]

// Supplier with constructor reference
Supplier<List<String>> listSupplier = ArrayList::new; // => Constructor reference
List<String> newList = listSupplier.get(); // => Creates new ArrayList
newList.add("item"); // => Add to new list

// Consumer with method reference
Consumer<String> printer = System.out::println; // => Method reference to println
names.forEach(printer); // => Prints each name
```

**Key Takeaway**: Four method reference types: static (`ClassName::staticMethod`), instance on object (`object::method`), instance on arbitrary (`ClassName::method`), constructor (`ClassName::new`). Method references are shorthand for lambdas that only call one method. Use when lambda body is a single method call with matching parameters. Cannot use when lambda has additional logic or parameter transformation. Array constructors use `Type[]::new`.

**Why It Matters**: Concurrent collections provide thread-safe operations without manual synchronization, preventing races and deadlocks. ConcurrentHashMap enables lock-free reads and fine-grained locking for writes, outperforming synchronized maps. CopyOnWriteArrayList optimizes for read-heavy workloads (cache, event listeners). Understanding when to use which collection (ConcurrentHashMap vs Collections.synchronizedMap) prevents performance bottlenecks. BlockingQueue enables producer-consumer patterns with built-in coordination. These collections are foundational to concurrent systems—caches, work queues, shared state.

---

### Example 38: Date and Time API (java.time)

Modern Date-Time API (Java 8+) provides immutable, thread-safe date/time handling. Replaces legacy `Date` and `Calendar`. Understand `LocalDate`, `LocalTime`, `LocalDateTime`, `ZonedDateTime`, and `Duration`/`Period`.

**Code**:

```java
import java.time.*;
import java.time.format.*;
import java.time.temporal.*;

// LocalDate - date without time (year-month-day)
LocalDate today = LocalDate.now(); // => Current date: 2025-12-25
LocalDate specificDate = LocalDate.of(2024, 1, 15); // => 2024-01-15
LocalDate parsed = LocalDate.parse("2024-03-20"); // => 2024-03-20

System.out.println(today.getYear()); // => 2025
System.out.println(today.getMonth()); // => DECEMBER (enum)
System.out.println(today.getMonthValue()); // => 12
System.out.println(today.getDayOfMonth()); // => 25
System.out.println(today.getDayOfWeek()); // => WEDNESDAY (enum)
System.out.println(today.getDayOfYear()); // => 359

// LocalTime - time without date (hour:minute:second.nanosecond)
LocalTime now = LocalTime.now(); // => Current time: 14:30:45.123456789
LocalTime specificTime = LocalTime.of(10, 30); // => 10:30:00
LocalTime withSeconds = LocalTime.of(10, 30, 45); // => 10:30:45
LocalTime withNanos = LocalTime.of(10, 30, 45, 123456789); // => 10:30:45.123456789

System.out.println(now.getHour()); // => 14
System.out.println(now.getMinute()); // => 30
System.out.println(now.getSecond()); // => 45
System.out.println(now.getNano()); // => 123456789

// LocalDateTime - date and time without timezone
LocalDateTime datetime = LocalDateTime.now(); // => Current date-time: 2025-12-25T14:30:45.123
LocalDateTime specific = LocalDateTime.of(2024, 1, 15, 10, 30); // => 2024-01-15T10:30
LocalDateTime combined = LocalDateTime.of(today, now); // => Combine date and time

// ZonedDateTime - date-time with timezone
ZonedDateTime zoned = ZonedDateTime.now(); // => Current with default timezone
ZonedDateTime newYork = ZonedDateTime.now(ZoneId.of("America/New_York"));
// => Current time in New York: 2025-12-25T09:30:45.123-05:00[America/New_York]

ZonedDateTime tokyo = zoned.withZoneSameInstant(ZoneId.of("Asia/Tokyo"));
// => Same instant converted to Tokyo timezone

// Instant - timestamp (seconds since Unix epoch: 1970-01-01T00:00:00Z)
Instant timestamp = Instant.now(); // => Current timestamp: 2025-12-25T14:30:45.123Z
long epochSeconds = timestamp.getEpochSecond(); // => Seconds since epoch: 1735139445
long epochMillis = timestamp.toEpochMilli(); // => Milliseconds since epoch

// Arithmetic operations - immutable (returns new instances)
LocalDate tomorrow = today.plusDays(1); // => 2025-12-26 (today unchanged)
LocalDate nextWeek = today.plusWeeks(1); // => 2026-01-01
LocalDate nextMonth = today.plusMonths(1); // => 2026-01-25
LocalDate nextYear = today.plusYears(1); // => 2026-12-25

LocalDate yesterday = today.minusDays(1); // => 2025-12-24
LocalDate lastMonth = today.minusMonths(1); // => 2025-11-25

LocalTime later = now.plusHours(2).plusMinutes(30); // => 17:00:45.123 (2.5 hours later)
LocalTime earlier = now.minusHours(1); // => 13:30:45.123 (1 hour earlier)

// with methods - modify specific fields
LocalDate modified = today
    .withYear(2030) // => Change year to 2030
    .withMonth(6) // => Change month to June
    .withDayOfMonth(15); // => Change day to 15th: 2030-06-15

LocalTime modifiedTime = now
    .withHour(9) // => Change hour to 9
    .withMinute(0) // => Change minute to 0
    .withSecond(0) // => Change second to 0: 09:00:00

// TemporalAdjusters - complex date adjustments
LocalDate firstDayOfMonth = today.with(TemporalAdjusters.firstDayOfMonth());
// => 2025-12-01

LocalDate lastDayOfMonth = today.with(TemporalAdjusters.lastDayOfMonth());
// => 2025-12-31

LocalDate nextMonday = today.with(TemporalAdjusters.next(DayOfWeek.MONDAY));
// => Next Monday after today

LocalDate firstMonday = today.with(TemporalAdjusters.firstInMonth(DayOfWeek.MONDAY));
// => First Monday of current month

// Period - date-based duration (years, months, days)
Period period = Period.between(specificDate, today); // => Period from 2024-01-15 to today
System.out.println(period.getYears()); // => 1 (year difference)
System.out.println(period.getMonths()); // => 11 (month difference)
System.out.println(period.getDays()); // => 10 (day difference)

Period twoWeeks = Period.ofWeeks(2); // => 14 days
Period threeMonths = Period.ofMonths(3); // => 3 months
LocalDate future = today.plus(threeMonths); // => 2026-03-25

// Duration - time-based duration (hours, minutes, seconds, nanos)
Duration duration = Duration.between(specificTime, now); // => Duration from 10:30 to now
System.out.println(duration.toHours()); // => Hours difference: 4
System.out.println(duration.toMinutes()); // => Minutes difference: 240
System.out.println(duration.getSeconds()); // => Total seconds

Duration oneHour = Duration.ofHours(1); // => 1 hour
Duration thirtyMinutes = Duration.ofMinutes(30); // => 30 minutes
LocalTime laterTime = now.plus(oneHour); // => 15:30:45.123

// Formatting and parsing
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
String formatted = datetime.format(formatter); // => "2025-12-25 14:30:45"

LocalDateTime parsedDateTime = LocalDateTime.parse("2024-03-20 10:30:00", formatter);
// => 2024-03-20T10:30:00

// Predefined formatters
String iso = today.format(DateTimeFormatter.ISO_DATE); // => "2025-12-25"
String custom = today.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")); // => "25/12/2025"

// Comparison
boolean isBefore = specificDate.isBefore(today); // => true (2024-01-15 before today)
boolean isAfter = specificDate.isAfter(today); // => false
boolean isEqual = today.isEqual(today); // => true

// ChronoUnit - calculate time units between dates
long daysBetween = ChronoUnit.DAYS.between(specificDate, today); // => Days: 709
long monthsBetween = ChronoUnit.MONTHS.between(specificDate, today); // => Months: 23
long hoursBetween = ChronoUnit.HOURS.between(specificTime, now); // => Hours: 4

// Leap year check
boolean isLeapYear = today.isLeapYear(); // => false (2025 is not leap year)
boolean was2024Leap = specificDate.isLeapYear(); // => true (2024 is leap year)

// Length of month
int daysInMonth = today.lengthOfMonth(); // => 31 (December has 31 days)
int daysInYear = today.lengthOfYear(); // => 365 (2025 is not leap year)

// Practical example: calculate age
LocalDate birthDate = LocalDate.of(1990, 5, 15); // => Birth: 1990-05-15
Period age = Period.between(birthDate, today); // => Age period
System.out.println("Age: " + age.getYears() + " years, " +
                   age.getMonths() + " months, " +
                   age.getDays() + " days");
// => "Age: 35 years, 7 months, 10 days"

// Working with different timezones
ZoneId londonZone = ZoneId.of("Europe/London"); // => London timezone
ZoneId sydneyZone = ZoneId.of("Australia/Sydney"); // => Sydney timezone

ZonedDateTime londonTime = ZonedDateTime.now(londonZone);
ZonedDateTime sydneyTime = londonTime.withZoneSameInstant(sydneyZone);
// => Same instant, different timezone display

// Time until specific event
LocalDateTime event = LocalDateTime.of(2026, 1, 1, 0, 0); // => New Year 2026
Duration timeUntil = Duration.between(LocalDateTime.now(), event);
System.out.println("Days until event: " + timeUntil.toDays()); // => Days remaining
```

**Key Takeaway**: Modern Date-Time API is immutable and thread-safe. `LocalDate` for dates, `LocalTime` for times, `LocalDateTime` for both (no timezone). `ZonedDateTime` includes timezone. `Instant` for Unix timestamps. `Period` for date-based durations (years/months/days), `Duration` for time-based (hours/minutes/seconds). All operations return new instances. Use `DateTimeFormatter` for formatting/parsing. `TemporalAdjusters` for complex date math. `ChronoUnit` for calculating differences.

**Why It Matters**: Synchronization prevents race conditions—ensuring only one thread modifies shared state at a time. However, locks cause contention reducing throughput, and incorrect locking causes deadlocks. Understanding intrinsic locks (synchronized) vs explicit locks (ReentrantLock) enables appropriate locking strategies. Minimize critical sections to reduce contention. Prefer concurrent collections and atomic variables over manual synchronization—they're less error-prone. Synchronization is essential but tricky—modern Java provides higher-level constructs reducing the need for manual locking.

---

### Example 39: Regular Expressions

Regular expressions (regex) enable pattern matching in strings. Java's `Pattern` and `Matcher` provide powerful text processing. Understand basic patterns, groups, and replacements for validation and extraction.

**Code**:

```java
import java.util.regex.*;
import java.util.*;

// Basic pattern matching
String text = "Hello World 123"; // => Input text

// matches() - entire string must match pattern
boolean matches1 = text.matches("Hello World \\d+"); // => true (\\d+ = one or more digits)
boolean matches2 = text.matches("Hello"); // => false (doesn't match entire string)

// Pattern compilation - reuse patterns efficiently
Pattern pattern = Pattern.compile("\\d+"); // => Compile pattern once
Matcher matcher = pattern.matcher(text); // => Create matcher for text

// find() - find substring matching pattern
if (matcher.find()) {
    System.out.println("Found: " + matcher.group()); // => "123" (matched digits)
    System.out.println("Start: " + matcher.start()); // => 12 (start index)
    System.out.println("End: " + matcher.end()); // => 15 (end index)
}

// Find all matches
String multipleNumbers = "Order 123 has 45 items and costs 67 dollars"; // => Multiple numbers
Matcher numberMatcher = Pattern.compile("\\d+").matcher(multipleNumbers);

while (numberMatcher.find()) {
    System.out.println("Found number: " + numberMatcher.group());
}
// => Prints: "123", "45", "67"

// Capturing groups - extract parts of match
String email = "user@example.com"; // => Email address
Pattern emailPattern = Pattern.compile("([a-zA-Z0-9]+)@([a-zA-Z0-9.]+)");
// => Group 1: username, Group 2: domain

Matcher emailMatcher = emailPattern.matcher(email);
if (emailMatcher.matches()) {
    System.out.println("Full match: " + emailMatcher.group(0)); // => "user@example.com"
    System.out.println("Username: " + emailMatcher.group(1)); // => "user"
    System.out.println("Domain: " + emailMatcher.group(2)); // => "example.com"
}

// Named groups (Java 7+)
Pattern namedPattern = Pattern.compile("(?<user>[a-zA-Z0-9]+)@(?<domain>[a-zA-Z0-9.]+)");
Matcher namedMatcher = namedPattern.matcher(email);

if (namedMatcher.matches()) {
    System.out.println("User: " + namedMatcher.group("user")); // => "user"
    System.out.println("Domain: " + namedMatcher.group("domain")); // => "example.com"
}

// Common patterns
String phonePattern = "\\d{3}-\\d{3}-\\d{4}"; // => 123-456-7890 format
boolean validPhone = "555-123-4567".matches(phonePattern); // => true

String zipPattern = "\\d{5}(-\\d{4})?"; // => 12345 or 12345-6789 format
boolean validZip1 = "12345".matches(zipPattern); // => true
boolean validZip2 = "12345-6789".matches(zipPattern); // => true
boolean validZip3 = "123".matches(zipPattern); // => false

// URL pattern (simplified)
String urlPattern = "https?://[a-zA-Z0-9.-]+\\.[a-z]{2,}(/.*)?";
boolean validUrl = "https://example.com/path".matches(urlPattern); // => true

// replaceAll() - replace all matches
String withSpaces = "hello   world  test"; // => Multiple spaces
String normalized = withSpaces.replaceAll("\\s+", " "); // => "hello world test" (single spaces)

String text2 = "Error: code 123, Error: code 456"; // => Multiple errors
String replaced = text2.replaceAll("Error: code (\\d+)", "Bug #$1");
// => "Bug #123, Bug #456" ($1 references group 1)

// replaceFirst() - replace only first match
String onlyFirst = text2.replaceFirst("Error", "Warning");
// => "Warning: code 123, Error: code 456"

// split() - split by pattern
String csv = "apple,banana,cherry,date"; // => CSV data
String[] fruits = csv.split(","); // => ["apple", "banana", "cherry", "date"]

String irregular = "one  two   three    four"; // => Irregular spacing
String[] words = irregular.split("\\s+"); // => ["one", "two", "three", "four"]

// Pattern flags
Pattern caseInsensitive = Pattern.compile("hello", Pattern.CASE_INSENSITIVE);
boolean matches3 = caseInsensitive.matcher("HELLO").matches(); // => true

Pattern multiline = Pattern.compile("^Hello", Pattern.MULTILINE);
// => ^ matches start of each line, not just start of string

Pattern dotall = Pattern.compile(".*", Pattern.DOTALL);
// => . matches newlines too

// Greedy vs reluctant quantifiers
String html = "<div>Content</div><span>More</span>"; // => HTML tags

// Greedy (default) - matches as much as possible
Matcher greedy = Pattern.compile("<.*>").matcher(html);
if (greedy.find()) {
    System.out.println(greedy.group()); // => "<div>Content</div><span>More</span>"
    // => Matches from first < to last >
}

// Reluctant (lazy) - matches as little as possible
Matcher reluctant = Pattern.compile("<.*?>").matcher(html);
while (reluctant.find()) {
    System.out.println(reluctant.group());
}
// => Prints: "<div>", "</div>", "<span>", "</span>"
// => Each match is minimal

// Lookahead and lookbehind
String prices = "Price: $100, $200, $300"; // => Prices with $ prefix

// Positive lookahead - match followed by pattern
Matcher lookahead = Pattern.compile("\\d+(?= dollars)").matcher("10 dollars");
// => Matches "10" only if followed by " dollars"

// Positive lookbehind - match preceded by pattern
Matcher lookbehind = Pattern.compile("(?<=\\$)\\d+").matcher(prices);
while (lookbehind.find()) {
    System.out.println(lookbehind.group());
}
// => Prints: "100", "200", "300" (numbers after $)

// Validation patterns
class Validators {
    public static boolean isEmail(String email) {
        String pattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$";
        return email.matches(pattern); // => Validates email format
    }

    public static boolean isStrongPassword(String password) {
        // At least 8 chars, one uppercase, one lowercase, one digit, one special char
        String pattern = "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[@$!%*?&])[A-Za-z\\d@$!%*?&]{8,}$";
        return password.matches(pattern); // => Validates password strength
    }

    public static boolean isIPv4(String ip) {
        String pattern = "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$";
        return ip.matches(pattern); // => Validates IPv4 address
    }
}

boolean validEmail = Validators.isEmail("user@example.com"); // => true
boolean invalidEmail = Validators.isEmail("invalid@"); // => false

boolean strongPass = Validators.isStrongPassword("Pass123!"); // => true
boolean weakPass = Validators.isStrongPassword("password"); // => false

boolean validIP = Validators.isIPv4("192.168.1.1"); // => true
boolean invalidIP = Validators.isIPv4("999.999.999.999"); // => false

// Extract all matches into list
String sentence = "The price is $100 and the tax is $15"; // => Sentence with prices
Pattern pricePattern = Pattern.compile("\\$\\d+"); // => $ followed by digits
Matcher priceMatcher = pricePattern.matcher(sentence);

List<String> allPrices = new ArrayList<>();
while (priceMatcher.find()) {
    allPrices.add(priceMatcher.group()); // => Add each match
}
// => allPrices: ["$100", "$15"]

// Or using results() stream (Java 9+)
List<String> pricesStream = pricePattern.matcher(sentence)
    .results() // => Stream of MatchResult
    .map(MatchResult::group) // => Extract matched text
    .toList(); // => ["$100", "$15"]

// Quote literal strings for regex
String literal = "Price: $100 (special)"; // => String with special regex chars
String escaped = Pattern.quote(literal); // => Escapes all special chars
Pattern literalPattern = Pattern.compile(escaped); // => Matches literal string exactly
```

**Key Takeaway**: `Pattern.compile()` compiles regex patterns. `Matcher` finds matches with `find()`, `matches()`, `replaceAll()`. Capturing groups `()` extract substrings (access with `group(n)` or named `group("name")`). Common quantifiers: `*` (0+), `+` (1+), `?` (0-1), `{n,m}` (n to m). Character classes: `\\d` (digit), `\\w` (word), `\\s` (space). Use `?` for reluctant matching. Lookahead `(?=)` and lookbehind `(?<=)` match positions. `Pattern.quote()` escapes special characters for literal matching.

**Why It Matters**: Deadlocks occur when threads circularly wait for locks held by each other, halting progress. They're insidious—causing hangs in production without crashes. Prevention strategies (lock ordering, timeouts) are essential for concurrent systems. Understanding deadlock conditions (mutual exclusion, hold and wait, no preemption, circular wait) enables designing deadlock-free systems. Breaking any condition prevents deadlocks. Liveness issues (starvation, livelock) also cause subtle failures. Concurrent programming requires understanding these failure modes to build reliable systems.

---

### Example 40: NIO.2 File Operations

NIO.2 (New I/O, Java 7+) provides modern file operations with `Path`, `Files`, and `FileSystem`. Replaces legacy `File` class with better error handling and more features. Essential for production file handling.

**Code**:

```java
import java.nio.file.*;
import java.nio.file.attribute.*;
import java.io.*;
import java.util.*;
import java.util.stream.*;

// Path creation - modern alternative to File
Path path1 = Paths.get("/home/user/file.txt"); // => Absolute path
Path path2 = Paths.get("relative", "path", "file.txt"); // => relative/path/file.txt
Path path3 = Path.of("/tmp", "data.txt"); // => Java 11+ factory method

// Current working directory
Path currentDir = Paths.get("").toAbsolutePath(); // => Current directory
Path userHome = Paths.get(System.getProperty("user.home")); // => User home directory

// Path operations
Path absolute = path2.toAbsolutePath(); // => Convert to absolute path
Path normalized = Paths.get("/home/user/../data/./file.txt").normalize();
// => /home/data/file.txt (removes . and ..)

Path parent = path1.getParent(); // => /home/user
Path fileName = path1.getFileName(); // => file.txt
Path root = path1.getRoot(); // => / (Unix) or C:\ (Windows)

// Join paths
Path base = Paths.get("/home/user"); // => Base directory
Path joined = base.resolve("documents/file.txt"); // => /home/user/documents/file.txt
Path relative = base.relativize(joined); // => documents/file.txt

// File existence and type checks
Path testFile = Paths.get("test.txt"); // => Test file path

boolean exists = Files.exists(testFile); // => true/false
boolean notExists = Files.notExists(testFile); // => true/false
boolean isRegularFile = Files.isRegularFile(testFile); // => true if regular file
boolean isDirectory = Files.isDirectory(testFile); // => true if directory
boolean isSymbolicLink = Files.isSymbolicLink(testFile); // => true if symlink
boolean isReadable = Files.isReadable(testFile); // => true if readable
boolean isWritable = Files.isWritable(testFile); // => true if writable
boolean isExecutable = Files.isExecutable(testFile); // => true if executable

// Reading files
// 1. Read all bytes
byte[] bytes = Files.readAllBytes(testFile); // => All bytes as array
String content1 = new String(bytes); // => Convert to string

// 2. Read all lines
List<String> lines = Files.readAllLines(testFile); // => All lines as List<String>

// 3. Stream lines (memory efficient for large files)
try (Stream<String> lineStream = Files.lines(testFile)) {
    lineStream.filter(line -> line.contains("important")) // => Filter lines
              .forEach(System.out::println); // => Print matching lines
}

// 4. Read as String (Java 11+)
String content2 = Files.readString(testFile); // => Entire file as String

// Writing files
// 1. Write bytes
byte[] data = "Hello World".getBytes(); // => Convert string to bytes
Files.write(testFile, data); // => Write bytes (creates or overwrites)

// 2. Write lines
List<String> linesToWrite = List.of("Line 1", "Line 2", "Line 3");
Files.write(testFile, linesToWrite); // => Write lines (overwrites)

// Append to file
Files.write(testFile, "New line\n".getBytes(), StandardOpenOption.APPEND);
// => Append bytes to existing file

// 3. Write string (Java 11+)
Files.writeString(testFile, "Content"); // => Write string (overwrites)

// Creating directories
Path newDir = Paths.get("parent/child/grandchild"); // => Directory path

Files.createDirectory(Paths.get("parent")); // => Create single directory (parent must exist)
Files.createDirectories(newDir); // => Create all parent directories if needed

// Copying files
Path source = Paths.get("source.txt"); // => Source file
Path destination = Paths.get("destination.txt"); // => Destination file

Files.copy(source, destination); // => Copy (fails if destination exists)
Files.copy(source, destination, StandardCopyOption.REPLACE_EXISTING);
// => Copy and replace if exists

// Copy directory (shallow - doesn't copy contents)
Files.copy(Paths.get("sourceDir"), Paths.get("destDir"));

// Moving/renaming files
Path oldName = Paths.get("old.txt"); // => Old path
Path newName = Paths.get("new.txt"); // => New path

Files.move(oldName, newName); // => Move/rename (fails if destination exists)
Files.move(oldName, newName, StandardCopyOption.REPLACE_EXISTING);
// => Move and replace if exists

// Deleting files
Files.delete(testFile); // => Delete file (throws exception if not exists)
Files.deleteIfExists(testFile); // => Delete if exists (returns true if deleted)

// File attributes
BasicFileAttributes attrs = Files.readAttributes(testFile, BasicFileAttributes.class);

long size = attrs.size(); // => File size in bytes
FileTime created = attrs.creationTime(); // => Creation time
FileTime modified = attrs.lastModifiedTime(); // => Last modified time
FileTime accessed = attrs.lastAccessTime(); // => Last access time
boolean isDir = attrs.isDirectory(); // => true if directory
boolean isFile = attrs.isRegularFile(); // => true if regular file

// Or get individual attributes
long size2 = Files.size(testFile); // => File size
FileTime modified2 = Files.getLastModifiedTime(testFile); // => Last modified

// Set last modified time
FileTime newTime = FileTime.fromMillis(System.currentTimeMillis());
Files.setLastModifiedTime(testFile, newTime); // => Update modified time

// List directory contents
Path directory = Paths.get("."); // => Current directory

// 1. List all entries (not recursive)
try (DirectoryStream<Path> stream = Files.newDirectoryStream(directory)) {
    for (Path entry : stream) {
        System.out.println(entry); // => Print each entry
    }
}

// 2. List with glob pattern filter
try (DirectoryStream<Path> stream = Files.newDirectoryStream(directory, "*.txt")) {
    for (Path entry : stream) {
        System.out.println(entry); // => Print only .txt files
    }
}

// 3. Stream directory entries (Java 8+)
try (Stream<Path> paths = Files.list(directory)) {
    paths.filter(Files::isRegularFile) // => Only regular files
         .forEach(System.out::println); // => Print each file
}

// 4. Walk directory tree (recursive)
try (Stream<Path> paths = Files.walk(directory)) {
    paths.filter(p -> p.toString().endsWith(".java")) // => Find all .java files
         .forEach(System.out::println); // => Print each Java file
}

// Walk with max depth
try (Stream<Path> paths = Files.walk(directory, 2)) { // => Max 2 levels deep
    paths.forEach(System.out::println);
}

// Find files matching predicate
try (Stream<Path> paths = Files.find(directory, 3,
        (path, attrs) -> attrs.isRegularFile() && attrs.size() > 1024)) {
    paths.forEach(System.out::println); // => Files > 1KB, max 3 levels deep
}

// Temporary files and directories
Path tempFile = Files.createTempFile("prefix-", "-suffix.txt");
// => Creates temp file: prefix-1234567890-suffix.txt in temp directory

Path tempDir = Files.createTempDirectory("myapp-");
// => Creates temp directory: myapp-1234567890 in temp directory

// Delete temporary files on exit
tempFile.toFile().deleteOnExit(); // => Delete when JVM exits

// File watching (monitor file system changes)
WatchService watcher = FileSystems.getDefault().newWatchService();
Path dirToWatch = Paths.get("."); // => Directory to watch

dirToWatch.register(watcher,
    StandardWatchEventKinds.ENTRY_CREATE, // => Watch for new files
    StandardWatchEventKinds.ENTRY_DELETE, // => Watch for deletions
    StandardWatchEventKinds.ENTRY_MODIFY); // => Watch for modifications

// Watch loop (runs until interrupted)
// WatchKey key;
// while ((key = watcher.take()) != null) {
//     for (WatchEvent<?> event : key.pollEvents()) {
//         System.out.println("Event: " + event.kind() + " - " + event.context());
//     }
//     key.reset();
// }

// Symbolic links
Path link = Paths.get("link.txt"); // => Link path
Path target = Paths.get("target.txt"); // => Target path

// Files.createSymbolicLink(link, target); // => Create symbolic link (Unix/Linux)

// Read link target
if (Files.isSymbolicLink(link)) {
    Path linkTarget = Files.readSymbolicLink(link); // => Get target path
    System.out.println("Link points to: " + linkTarget);
}

// POSIX file permissions (Unix/Linux)
// Set<PosixFilePermission> perms = PosixFilePermissions.fromString("rw-r--r--");
// Files.setPosixFilePermissions(testFile, perms);

// Practical: copy directory recursively
void copyDirectory(Path source, Path target) throws IOException {
    Files.walk(source)
         .forEach(sourcePath -> {
             try {
                 Path targetPath = target.resolve(source.relativize(sourcePath));
                 if (Files.isDirectory(sourcePath)) {
                     Files.createDirectories(targetPath); // => Create directories
                 } else {
                     Files.copy(sourcePath, targetPath, StandardCopyOption.REPLACE_EXISTING);
                     // => Copy files
                 }
             } catch (IOException e) {
                 e.printStackTrace();
             }
         });
}

// Practical: delete directory recursively
void deleteDirectory(Path directory) throws IOException {
    Files.walk(directory)
         .sorted(Comparator.reverseOrder()) // => Delete children before parents
         .forEach(path -> {
             try {
                 Files.delete(path); // => Delete each path
             } catch (IOException e) {
                 e.printStackTrace();
             }
         });
}
```

**Key Takeaway**: NIO.2 uses `Path` (not `File`) for paths. `Files` class provides static methods for all operations. `Files.exists()`, `isDirectory()`, `isRegularFile()` check status. Reading: `readAllBytes()`, `readAllLines()`, `lines()` (stream), `readString()`. Writing: `write()`, `writeString()`. `copy()`, `move()`, `delete()` with options. `createDirectories()` creates all parents. `Files.walk()` traverses recursively. `Files.list()` lists directory. Attributes via `readAttributes()`. `WatchService` monitors changes. Always use try-with-resources for streams.

**Why It Matters**: Files is the modern API for file operations, replacing legacy File class with cleaner, more correct behavior. It handles edge cases (symbolic links, permissions) correctly. Integration with Streams enables processing large files efficiently without loading entire contents into memory—essential for logs, datasets. Understanding IOException handling prevents resource leaks. Atomic file operations (move, delete) prevent partial writes during failures. Modern file handling is critical for reliable data processing in production systems.

---

### Example 41: Annotations in Practice

Annotations add metadata to code for frameworks, tools, and runtime processing. Built-in annotations (`@Override`, `@Deprecated`, `@SuppressWarnings`) ensure code quality. Custom annotations enable framework magic.

**Code**:

```java
import java.lang.annotation.*;
import java.lang.reflect.*;

// Built-in annotations - code quality and compiler checks
class Parent {
    public void process() {
        System.out.println("Parent process"); // => Parent method
    }

    @Deprecated(since = "2.0", forRemoval = true) // => Mark for removal
    public void oldMethod() {
        System.out.println("Old method"); // => Deprecated method
    }
}

class Child extends Parent {
    @Override // => Compiler verifies this overrides parent method
    public void process() {
        System.out.println("Child process"); // => Child override
    }

    // @Override
    // public void typoMethod() {} // => ERROR: no such method in parent

    @SuppressWarnings("deprecation") // => Suppress deprecation warning
    public void useOldMethod() {
        oldMethod(); // => No warning despite calling deprecated method
    }

    @SuppressWarnings({"unchecked", "rawtypes"}) // => Multiple warnings
    public void rawTypes() {
        List list = new ArrayList(); // => Raw type (normally warns)
        list.add("item"); // => Unchecked operation (normally warns)
    }
}

// @FunctionalInterface - marks interface as functional (single abstract method)
@FunctionalInterface
interface Calculator {
    int calculate(int a, int b); // => Single abstract method

    // int anotherMethod(int x); // => ERROR: multiple abstract methods not allowed

    default void log() { // => Default methods allowed
        System.out.println("Calculating");
    }

    static void helper() { // => Static methods allowed
        System.out.println("Helper");
    }
}

// Lambda works because of single abstract method
Calculator adder = (a, b) -> a + b; // => Lambda implementation
int sum = adder.calculate(5, 3); // => 8

// Custom annotation definition
@Retention(RetentionPolicy.RUNTIME) // => Available at runtime (for reflection)
@Target(ElementType.METHOD) // => Can be applied to methods only
@interface Test {
    String description() default ""; // => Annotation parameter with default
    int timeout() default 0; // => Timeout in milliseconds
    String[] tags() default {}; // => Array parameter
}

// Multiple target types
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD}) // => Apply to classes or methods
@interface Audited {
    String value(); // => Required parameter (no default)
}

// Annotation with no parameters (marker annotation)
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
@interface Benchmark {}

// Using custom annotations
@Audited("UserService") // => Class-level annotation
class UserService {
    @Test(description = "Test user creation", timeout = 5000, tags = {"integration", "database"})
    public void testCreateUser() {
        System.out.println("Testing user creation"); // => Test method
    }

    @Test(description = "Test user deletion", tags = {"integration"})
    public void testDeleteUser() {
        System.out.println("Testing user deletion"); // => Test method
    }

    @Benchmark // => Marker annotation (no parameters)
    public void performanceTest() {
        System.out.println("Performance test"); // => Benchmark method
    }

    public void regularMethod() {
        // No annotations
    }
}

// Processing annotations with reflection
void runTests(Class<?> testClass) throws Exception {
    Object instance = testClass.getDeclaredConstructor().newInstance(); // => Create instance

    for (Method method : testClass.getDeclaredMethods()) {
        if (method.isAnnotationPresent(Test.class)) { // => Check if @Test present
            Test test = method.getAnnotation(Test.class); // => Get annotation

            System.out.println("Running test: " + method.getName());
            System.out.println("  Description: " + test.description());
            System.out.println("  Timeout: " + test.timeout() + "ms");
            System.out.println("  Tags: " + Arrays.toString(test.tags()));

            try {
                method.invoke(instance); // => Invoke test method
                System.out.println("  Result: PASSED");
            } catch (Exception e) {
                System.out.println("  Result: FAILED - " + e.getCause());
            }
        }
    }
}

// Run tests
runTests(UserService.class);
// => Outputs:
// Running test: testCreateUser
//   Description: Test user creation
//   Timeout: 5000ms
//   Tags: [integration, database]
//   Testing user creation
//   Result: PASSED
// Running test: testDeleteUser
//   Description: Test user deletion
//   Timeout: 0ms
//   Tags: [integration]
//   Testing user deletion
//   Result: PASSED

// Retention policies
@Retention(RetentionPolicy.SOURCE) // => Discarded by compiler (e.g., @Override)
@interface SourceOnly {}

@Retention(RetentionPolicy.CLASS) // => Stored in class file, not available at runtime (default)
@interface ClassOnly {}

@Retention(RetentionPolicy.RUNTIME) // => Available at runtime via reflection
@interface RuntimeAccessible {}

// Target types (where annotations can be applied)
@Target(ElementType.TYPE) // => Classes, interfaces, enums
@interface TypeAnnotation {}

@Target(ElementType.FIELD) // => Fields
@interface FieldAnnotation {}

@Target(ElementType.METHOD) // => Methods
@interface MethodAnnotation {}

@Target(ElementType.PARAMETER) // => Method parameters
@interface ParameterAnnotation {}

@Target(ElementType.CONSTRUCTOR) // => Constructors
@interface ConstructorAnnotation {}

@Target(ElementType.LOCAL_VARIABLE) // => Local variables
@interface LocalVarAnnotation {}

@Target(ElementType.ANNOTATION_TYPE) // => Other annotations (meta-annotation)
@interface MetaAnnotation {}

@Target(ElementType.PACKAGE) // => Packages (in package-info.java)
@interface PackageAnnotation {}

// Java 8+ type annotations
@Target(ElementType.TYPE_USE) // => Any type use (variables, casts, generics, etc.)
@interface NonNull {}

@Target(ElementType.TYPE_PARAMETER) // => Type parameters in generics
@interface TypeParam {}

// Type annotation examples
class TypeAnnotations {
    @NonNull String name; // => Field with type annotation

    public @NonNull String getName() { // => Return type annotation
        return name;
    }

    public void process(@NonNull String input) { // => Parameter type annotation
        @NonNull String local = input; // => Local variable type annotation
    }

    List<@NonNull String> names; // => Generic type argument annotation

    public <@TypeParam T> void generic(T value) { // => Type parameter annotation
    }
}

// Repeatable annotations (Java 8+)
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
@Repeatable(Schedules.class) // => Container annotation
@interface Schedule {
    String day();
    int hour();
}

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
@interface Schedules {
    Schedule[] value(); // => Array of repeatable annotation
}

// Using repeatable annotations
class ScheduledTasks {
    @Schedule(day = "Monday", hour = 9)
    @Schedule(day = "Wednesday", hour = 14)
    @Schedule(day = "Friday", hour = 11)
    public void backupData() {
        System.out.println("Backing up data"); // => Scheduled task
    }
}

// Accessing repeatable annotations
Method backupMethod = ScheduledTasks.class.getMethod("backupData");
Schedule[] schedules = backupMethod.getAnnotationsByType(Schedule.class);
// => Returns all @Schedule annotations

for (Schedule schedule : schedules) {
    System.out.println("Scheduled: " + schedule.day() + " at " + schedule.hour() + ":00");
}
// => Prints:
// Scheduled: Monday at 9:00
// Scheduled: Wednesday at 14:00
// Scheduled: Friday at 11:00

// Practical annotation: dependency injection
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
@interface Inject {}

class SimpleInjector {
    public static void inject(Object instance) throws Exception {
        Class<?> clazz = instance.getClass();

        for (Field field : clazz.getDeclaredFields()) {
            if (field.isAnnotationPresent(Inject.class)) {
                field.setAccessible(true); // => Allow access to private field

                // Simple DI: create instance of field type
                Object dependency = field.getType().getDeclaredConstructor().newInstance();
                field.set(instance, dependency); // => Inject dependency

                System.out.println("Injected: " + field.getName());
            }
        }
    }
}

class Database {
    public void connect() {
        System.out.println("Database connected"); // => Database method
    }
}

class Service {
    @Inject
    private Database database; // => Field to be injected

    public void useDatabase() {
        database.connect(); // => Use injected dependency
    }
}

// Use simple DI
Service service = new Service();
SimpleInjector.inject(service); // => Injects Database instance
service.useDatabase(); // => "Database connected"
```

**Key Takeaway**: Built-in annotations: `@Override` (verify override), `@Deprecated` (mark obsolete), `@SuppressWarnings` (suppress warnings), `@FunctionalInterface` (enforce single abstract method). Custom annotations use `@interface`. `@Retention` controls lifecycle (SOURCE/CLASS/RUNTIME). `@Target` specifies where annotation applies. Access via reflection with `isAnnotationPresent()`, `getAnnotation()`. `@Repeatable` allows multiple instances. Type annotations (`@Target(TYPE_USE)`) annotate any type use. Annotations enable framework magic (testing, DI, validation).

**Why It Matters**: Serialization converts objects to byte streams for storage or network transmission—essential for caching (Redis), messaging (Kafka), RPC. However, default serialization is fragile (breaks with class changes), slow, and has security risks (deserialization attacks). JSON serialization (Jackson, Gson) is more maintainable, human-readable, and cross-language compatible. Understanding serialVersionUID prevents deserialization failures. Avoid default serialization—prefer explicit serialization formats (JSON, Protocol Buffers) that version gracefully and integrate with non-Java systems.

---

### Example 42: Optional Deep Dive

`Optional` represents potentially absent values, replacing null checks. Functional methods (`map`, `flatMap`, `filter`, `orElse`) compose operations. Use to make nullability explicit in APIs.

**Code**:

```java
import java.util.*;
import java.util.stream.*;

// Creating Optional instances
Optional<String> present = Optional.of("Hello"); // => Optional with value
// Optional<String> nullError = Optional.of(null); // => ERROR: NullPointerException

Optional<String> nullable = Optional.ofNullable("World"); // => Optional with value
Optional<String> empty = Optional.ofNullable(null); // => Optional.empty (no NPE)
Optional<String> explicitEmpty = Optional.empty(); // => Explicitly empty

// Checking presence
boolean hasValue = present.isPresent(); // => true (value present)
boolean isEmpty = empty.isEmpty(); // => true (Java 11+, no value)

// Getting values
String value1 = present.get(); // => "Hello" (throws if empty - avoid!)
String value2 = present.orElse("Default"); // => "Hello" (returns value)
String value3 = empty.orElse("Default"); // => "Default" (fallback when empty)

// orElseGet - lazy evaluation (supplier called only if empty)
String value4 = present.orElseGet(() -> "Computed Default"); // => "Hello" (supplier not called)
String value5 = empty.orElseGet(() -> {
    System.out.println("Computing default"); // => Executes only if empty
    return "Computed Default";
}); // => "Computed Default"

// orElseThrow - throw exception if empty
String value6 = present.orElseThrow(); // => "Hello" (Java 10+)
String value7 = present.orElseThrow(() -> new IllegalStateException("No value!"));
// => "Hello" (custom exception)

// String value8 = empty.orElseThrow(); // => NoSuchElementException

// ifPresent - execute if value present
present.ifPresent(val -> System.out.println("Value: " + val)); // => "Value: Hello"
empty.ifPresent(val -> System.out.println("Value: " + val)); // => Nothing printed

// ifPresentOrElse - execute one action if present, another if empty (Java 9+)
present.ifPresentOrElse(
    val -> System.out.println("Present: " + val), // => Executed: "Present: Hello"
    () -> System.out.println("Empty") // => Not executed
);

empty.ifPresentOrElse(
    val -> System.out.println("Present: " + val), // => Not executed
    () -> System.out.println("Empty") // => Executed: "Empty"
);

// map - transform value if present
Optional<String> mapped = present.map(String::toUpperCase); // => Optional["HELLO"]
Optional<String> emptyMapped = empty.map(String::toUpperCase); // => Optional.empty (no-op)

Optional<Integer> length = present.map(String::length); // => Optional[5]

// Chaining maps
Optional<String> result = present
    .map(String::toUpperCase) // => Optional["HELLO"]
    .map(s -> s + "!") // => Optional["HELLO!"]
    .map(s -> s.repeat(2)); // => Optional["HELLO!HELLO!"]

// flatMap - avoid nested Optionals
class User {
    private Optional<String> email; // => User may have email

    public User(String email) {
        this.email = Optional.ofNullable(email);
    }

    public Optional<String> getEmail() {
        return email; // => Returns Optional
    }
}

Optional<User> user = Optional.of(new User("user@example.com"));

// Wrong: nested Optional (Optional<Optional<String>>)
// Optional<Optional<String>> nestedEmail = user.map(User::getEmail);

// Correct: flatMap unwraps
Optional<String> email = user.flatMap(User::getEmail); // => Optional["user@example.com"]

Optional<String> emailUpper = user
    .flatMap(User::getEmail) // => Optional["user@example.com"]
    .map(String::toUpperCase); // => Optional["USER@EXAMPLE.COM"]

// filter - keep value only if matches predicate
Optional<String> filtered = present.filter(s -> s.length() > 3);
// => Optional["Hello"] (length 5 > 3, passes filter)

Optional<String> filtered2 = present.filter(s -> s.length() > 10);
// => Optional.empty (length 5 not > 10, fails filter)

// Chaining filter
Optional<String> result2 = present
    .filter(s -> s.startsWith("H")) // => Passes
    .filter(s -> s.length() == 5) // => Passes
    .map(String::toUpperCase); // => Optional["HELLO"]

// or - return alternative Optional if empty (Java 9+)
Optional<String> first = Optional.empty(); // => Empty
Optional<String> second = Optional.of("Second"); // => Has value

Optional<String> combined = first.or(() -> second); // => Optional["Second"]
Optional<String> combined2 = present.or(() -> second); // => Optional["Hello"] (first wins)

// stream - convert Optional to Stream (Java 9+)
Stream<String> stream1 = present.stream(); // => Stream with one element
Stream<String> stream2 = empty.stream(); // => Empty stream

// Practical: filter list to only present values
List<Optional<String>> optionals = List.of(
    Optional.of("A"), // => Present
    Optional.empty(), // => Empty
    Optional.of("B"), // => Present
    Optional.empty(), // => Empty
    Optional.of("C") // => Present
);

List<String> values = optionals.stream()
    .flatMap(Optional::stream) // => Flatten Optionals to values only
    .toList(); // => ["A", "B", "C"] (empties removed)

// equals and hashCode
Optional<String> opt1 = Optional.of("Hello"); // => Optional with "Hello"
Optional<String> opt2 = Optional.of("Hello"); // => Optional with "Hello"
Optional<String> opt3 = Optional.of("World"); // => Optional with "World"

boolean equal = opt1.equals(opt2); // => true (same value)
boolean notEqual = opt1.equals(opt3); // => false (different values)
boolean emptyEquals = Optional.empty().equals(Optional.empty()); // => true

// Anti-patterns - what NOT to do

// BAD: Using get() without checking
// String bad1 = optional.get(); // => Can throw NoSuchElementException

// BAD: Checking with isPresent before get (defeats purpose)
if (present.isPresent()) {
    String bad2 = present.get(); // => Better to use orElse, ifPresent, or map
}

// GOOD: Use functional methods
String good1 = present.orElse("Default"); // => Functional approach

// BAD: Optional for collections (use empty collection instead)
// Optional<List<String>> bad3 = Optional.of(new ArrayList<>());
List<String> good2 = new ArrayList<>(); // => Just use empty list

// BAD: Optional fields in classes (serialization issues)
class BadClass {
    // private Optional<String> name; // => Don't do this (use null or @Nullable)
}

// GOOD: Optional in return types
class GoodClass {
    public Optional<String> findName(int id) {
        // Return Optional to indicate possible absence
        return id > 0 ? Optional.of("Name") : Optional.empty();
    }
}

// Practical: repository pattern with Optional
interface UserRepository {
    Optional<User> findById(int id); // => May not find user
    Optional<User> findByEmail(String email); // => May not find user
}

class UserService {
    private UserRepository repository;

    public String getUserEmail(int id) {
        return repository.findById(id) // => Optional<User>
            .flatMap(User::getEmail) // => Optional<String>
            .orElse("No email"); // => String (fallback if user not found or no email)
    }

    public void processUser(int id) {
        repository.findById(id) // => Optional<User>
            .filter(u -> u.getEmail().isPresent()) // => Only users with email
            .flatMap(User::getEmail) // => Get email
            .map(String::toUpperCase) // => Uppercase
            .ifPresent(email -> System.out.println("Processing: " + email));
            // => Only executes if user found and has email
    }
}

// Combining multiple Optionals
Optional<String> firstName = Optional.of("John"); // => First name
Optional<String> lastName = Optional.of("Doe"); // => Last name

// Using map and flatMap to combine
Optional<String> fullName = firstName.flatMap(first ->
    lastName.map(last -> first + " " + last)
); // => Optional["John Doe"]

// If either is empty, result is empty
Optional<String> fullName2 = firstName.flatMap(first ->
    Optional.<String>empty().map(last -> first + " " + last)
); // => Optional.empty

// Converting null-returning methods to Optional
String nullableResult = legacyMethod(); // => May return null
Optional<String> safe = Optional.ofNullable(nullableResult); // => Wraps in Optional

// Migrating from null checks
// OLD:
String oldValue = getValue();
if (oldValue != null) {
    System.out.println(oldValue.toUpperCase());
}

// NEW:
Optional.ofNullable(getValue())
    .map(String::toUpperCase)
    .ifPresent(System.out::println);
```

**Key Takeaway**: Create with `of()` (non-null), `ofNullable()` (nullable), `empty()`. Get values: `orElse()` (default), `orElseGet()` (lazy supplier), `orElseThrow()` (exception). Transform: `map()` (unwrapped), `flatMap()` (nested Optionals). Filter: `filter()` (predicate). Conditionals: `ifPresent()`, `ifPresentOrElse()`. Combine: `or()` (alternative). Convert: `stream()`. Avoid: `get()` without check, `isPresent()`+`get()` pattern, Optional fields. Use in return types to make nullability explicit. Chain operations functionally instead of imperative null checks.

**Why It Matters**: Network programming enables distributed systems—microservices, HTTP APIs, database connections, message queues. Understanding sockets, clients, servers is foundational to backend development. HTTP clients (HttpClient) abstract common patterns, but understanding underlying sockets prevents misuse. Blocking I/O limits scalability—NIO and async libraries (Netty) enable handling thousands of connections. Network failures are inevitable—timeout configuration, retry logic, and error handling are critical for resilience. Network programming is essential for building modern distributed applications.

---

### Example 43: Sealed Classes and Pattern Matching

Sealed classes (Java 17+) restrict inheritance to known subclasses. Combined with pattern matching in switch expressions, they enable exhaustive type checking. Essential for domain modeling with finite type hierarchies.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Sealed["sealed interface Shape<br/>permits Circle, Rectangle, Triangle"] --> Circle["record Circle(double radius)<br/>implements Shape"]
    Sealed --> Rectangle["record Rectangle(double width, double height)<br/>implements Shape"]
    Sealed --> Triangle["record Triangle(double base, double height)<br/>implements Shape"]

    Switch["switch(shape)"] --> CaseC["case Circle c -> ..."]
    Switch --> CaseR["case Rectangle r -> ..."]
    Switch --> CaseT["case Triangle t -> ..."]
    CaseC --> Result["Exhaustive<br/>No default needed"]
    CaseR --> Result
    CaseT --> Result

    style Sealed fill:#0173B2,color:#fff
    style Circle fill:#029E73,color:#fff
    style Rectangle fill:#029E73,color:#fff
    style Triangle fill:#029E73,color:#fff
    style Switch fill:#DE8F05,color:#fff
    style Result fill:#CC78BC,color:#fff
```

**Code**:

```java
// Sealed interface - restricts implementations
sealed interface Shape
    permits Circle, Rectangle, Triangle {} // => Only these 3 can implement

// Records implementing sealed interface
record Circle(double radius) implements Shape {}
record Rectangle(double width, double height) implements Shape {}
record Triangle(double base, double height) implements Shape {}

// Attempting to implement Shape from outside permitted types
// class Pentagon implements Shape {} // => ERROR: not permitted

// Pattern matching switch with exhaustiveness checking
double area(Shape shape) {
    return switch (shape) {
        case Circle c -> Math.PI * c.radius() * c.radius(); // => πr²
        case Rectangle r -> r.width() * r.height(); // => width × height
        case Triangle t -> 0.5 * t.base() * t.height(); // => ½ × base × height
        // No default needed - compiler knows all cases covered!
    };
}

Circle circle = new Circle(5.0); // => Circle with radius 5.0
double circleArea = area(circle); // => 78.54 (π × 5²)

Rectangle rect = new Rectangle(4.0, 6.0); // => Rectangle 4×6
double rectArea = area(rect); // => 24.0

// Sealed classes (not interfaces)
sealed abstract class Vehicle
    permits Car, Truck, Motorcycle {

    private final String licensePlate; // => Common field

    protected Vehicle(String licensePlate) {
        this.licensePlate = licensePlate; // => Constructor
    }

    public String getLicensePlate() {
        return licensePlate; // => Getter
    }
}

// Final subclass (cannot be further extended)
final class Car extends Vehicle {
    private final int seats; // => Car-specific field

    public Car(String licensePlate, int seats) {
        super(licensePlate); // => Call parent constructor
        this.seats = seats; // => Initialize seats
    }

    public int getSeats() {
        return seats; // => Getter
    }
}

// Non-sealed subclass (can be extended by anyone)
non-sealed class Truck extends Vehicle {
    private final double capacity; // => Truck-specific field

    public Truck(String licensePlate, double capacity) {
        super(licensePlate);
        this.capacity = capacity;
    }

    public double getCapacity() {
        return capacity;
    }
}

// Can extend non-sealed class
class PickupTruck extends Truck {
    public PickupTruck(String licensePlate) {
        super(licensePlate, 1000.0); // => Fixed capacity
    }
}

// Sealed subclass (further restricts)
sealed class Motorcycle extends Vehicle
    permits SportBike, Cruiser {

    protected Motorcycle(String licensePlate) {
        super(licensePlate);
    }
}

final class SportBike extends Motorcycle {
    public SportBike(String licensePlate) {
        super(licensePlate);
    }
}

final class Cruiser extends Motorcycle {
    public Cruiser(String licensePlate) {
        super(licensePlate);
    }
}

// Pattern matching with sealed classes
String describe(Vehicle vehicle) {
    return switch (vehicle) {
        case Car c -> "Car with " + c.getSeats() + " seats"; // => Car pattern
        case Truck t -> "Truck with capacity " + t.getCapacity() + " kg"; // => Truck pattern
        case Motorcycle m -> "Motorcycle: " + m.getLicensePlate(); // => Motorcycle pattern
        // No default needed - all Vehicle types covered
    };
}

Car myCar = new Car("ABC-123", 5); // => 5-seater car
String desc = describe(myCar); // => "Car with 5 seats"

// Nested pattern matching with sealed types
String detailedDescribe(Vehicle vehicle) {
    return switch (vehicle) {
        case Car c -> "Car with " + c.getSeats() + " seats";
        case Truck t -> "Truck with capacity " + t.getCapacity() + " kg";
        case SportBike sb -> "Sport bike: " + sb.getLicensePlate(); // => Specific motorcycle type
        case Cruiser cr -> "Cruiser: " + cr.getLicensePlate(); // => Specific motorcycle type
        // Compiler ensures all cases covered (including Motorcycle subtypes)
    };
}

// Guards in pattern matching (when clauses)
String categorizeVehicle(Vehicle vehicle) {
    return switch (vehicle) {
        case Car c when c.getSeats() > 5 -> "Large car"; // => Guard condition
        case Car c when c.getSeats() <= 5 -> "Small car";
        case Truck t when t.getCapacity() > 5000 -> "Heavy truck";
        case Truck t -> "Light truck"; // => Catches remaining trucks
        case Motorcycle m -> "Motorcycle";
    };
}

Car largeCar = new Car("XYZ-789", 7); // => 7-seater
String category = categorizeVehicle(largeCar); // => "Large car"

// Sealed enum alternative (traditional approach)
enum TraditionalShape {
    CIRCLE, RECTANGLE, TRIANGLE // => Fixed set of constants
}

// vs sealed types (modern approach with data)
// Sealed types carry data (radius, width, height)
// Enums just represent constants without associated data

// Practical: Result type (success or error)
sealed interface Result<T, E>
    permits Success, Failure {
}

record Success<T, E>(T value) implements Result<T, E> {}
record Failure<T, E>(E error) implements Result<T, E> {}

// Using Result type
Result<Integer, String> divide(int a, int b) {
    if (b == 0) {
        return new Failure<>("Division by zero"); // => Error case
    }
    return new Success<>(a / b); // => Success case
}

Result<Integer, String> result = divide(10, 2); // => Success(5)

String message = switch (result) {
    case Success<Integer, String> s -> "Result: " + s.value(); // => "Result: 5"
    case Failure<Integer, String> f -> "Error: " + f.error(); // => Not executed
};

Result<Integer, String> error = divide(10, 0); // => Failure("Division by zero")

String errorMessage = switch (error) {
    case Success<Integer, String> s -> "Result: " + s.value(); // => Not executed
    case Failure<Integer, String> f -> "Error: " + f.error(); // => "Error: Division by zero"
};

// Practical: JSON AST (Abstract Syntax Tree)
sealed interface Json
    permits JsonObject, JsonArray, JsonString, JsonNumber, JsonBoolean, JsonNull {}

record JsonObject(Map<String, Json> fields) implements Json {}
record JsonArray(List<Json> elements) implements Json {}
record JsonString(String value) implements Json {}
record JsonNumber(double value) implements Json {}
record JsonBoolean(boolean value) implements Json {}
record JsonNull() implements Json {}

// Pattern matching for JSON processing
String toJsonString(Json json) {
    return switch (json) {
        case JsonObject obj -> "{" + obj.fields().entrySet().stream()
            .map(e -> "\"" + e.getKey() + "\":" + toJsonString(e.getValue()))
            .collect(Collectors.joining(",")) + "}";
        case JsonArray arr -> "[" + arr.elements().stream()
            .map(this::toJsonString)
            .collect(Collectors.joining(",")) + "]";
        case JsonString str -> "\"" + str.value() + "\"";
        case JsonNumber num -> String.valueOf(num.value());
        case JsonBoolean bool -> String.valueOf(bool.value());
        case JsonNull n -> "null";
    };
}

// Create JSON structure
Json json = new JsonObject(Map.of(
    "name", new JsonString("Alice"),
    "age", new JsonNumber(30),
    "active", new JsonBoolean(true),
    "address", new JsonNull()
));

String jsonStr = toJsonString(json); // => {"name":"Alice","age":30.0,"active":true,"address":null}

// Benefits of sealed types:
// 1. Exhaustiveness checking - compiler ensures all cases handled
// 2. Domain modeling - express finite type hierarchies explicitly
// 3. Pattern matching - switch expressions with no default
// 4. Maintainability - adding new subtype causes compile errors in all switches
// 5. Documentation - permitted types are explicit in interface/class declaration

// Restrictions:
// - Sealed type and permitted subtypes must be in same package (or module)
// - Permitted subtypes must be final, sealed, or non-sealed
// - Cannot extend sealed type outside permitted list
```

**Key Takeaway**: Sealed classes/interfaces use `sealed` with `permits` clause to restrict subclasses. Permitted types must be `final` (no further extension), `sealed` (controlled extension), or `non-sealed` (open extension). Pattern matching switch with sealed types enables exhaustiveness checking (no `default` needed). Guards (`when` clauses) add conditions to patterns. Ideal for finite type hierarchies (shapes, results, ASTs). Compiler ensures all cases covered—adding new type causes compile errors in all switches. Must be in same package/module as sealed parent.

**Why It Matters**: Annotations enable declarative programming—express intent without boilerplate. They power frameworks (Spring's @Autowired, JPA's @Entity, JUnit's @Test) enabling configuration as code. Understanding retention (source, class, runtime) determines annotation availability. Reflection enables processing annotations at runtime—driving dependency injection, validation, persistence. However, annotations can obscure behavior—balance conciseness with explicitness. Custom annotations enable domain-specific configurations. Annotations are fundamental to modern Java development—nearly all frameworks rely on them.

---

### Example 44: Records Advanced Patterns

Records (Java 14+) are immutable data carriers with automatic constructor, getters, `equals`, `hashCode`, and `toString`. Compact constructors validate data. Records work with sealed types and pattern matching for powerful domain modeling.

**Code**:

```java
// Basic record - immutable data class
record Point(int x, int y) {} // => Automatic constructor, getters, equals, hashCode, toString

Point p1 = new Point(3, 4); // => Create record instance
int x = p1.x(); // => 3 (accessor method, not field)
int y = p1.y(); // => 4

System.out.println(p1); // => "Point[x=3, y=4]" (automatic toString)

Point p2 = new Point(3, 4); // => Same values as p1
boolean equal = p1.equals(p2); // => true (automatic equals based on fields)
int hash1 = p1.hashCode(); // => Consistent with equals
int hash2 = p2.hashCode(); // => Same as hash1

// Compact constructor - validation and normalization
record Range(int start, int end) {
    // Compact constructor - implicit parameters
    public Range {
        if (start > end) {
            // Swap if inverted
            int temp = start; // => Store start temporarily
            start = end; // => Assign end to start
            end = temp; // => Assign temp to end
        }
        // Fields automatically assigned after compact constructor
    }
}

Range range1 = new Range(10, 20); // => start=10, end=20 (no swap)
Range range2 = new Range(20, 10); // => start=10, end=20 (swapped automatically)

// Validation in compact constructor
record Person(String name, int age) {
    public Person {
        if (name == null || name.isBlank()) {
            throw new IllegalArgumentException("Name required"); // => Validation
        }
        if (age < 0 || age > 150) {
            throw new IllegalArgumentException("Invalid age"); // => Validation
        }
        // Normalization
        name = name.trim(); // => Remove whitespace
    }
}

Person alice = new Person("  Alice  ", 30); // => name trimmed to "Alice"
// Person invalid = new Person("", 200); // => IllegalArgumentException

// Canonical constructor - explicit parameters
record Employee(String name, int id, double salary) {
    // Explicit canonical constructor
    public Employee(String name, int id, double salary) {
        this.name = name; // => Explicit assignment
        this.id = id;
        this.salary = Math.max(0, salary); // => Ensure non-negative
    }
}

// Additional constructors
record Book(String title, String author, int year) {
    // Compact constructor
    public Book {
        if (title == null || author == null) {
            throw new IllegalArgumentException("Title and author required");
        }
    }

    // Additional constructor - must call canonical constructor
    public Book(String title, String author) {
        this(title, author, 2024); // => Delegate to canonical with default year
    }
}

Book book1 = new Book("Java Guide", "Smith", 2023); // => Canonical constructor
Book book2 = new Book("Python Guide", "Jones"); // => Additional constructor (year=2024)

// Static methods and fields
record MathUtils(int value) {
    public static final double PI = 3.14159; // => Static constant

    public static int add(int a, int b) {
        return a + b; // => Static method
    }

    public int square() {
        return value * value; // => Instance method using field
    }
}

int sum = MathUtils.add(5, 3); // => 8 (static method)
MathUtils m = new MathUtils(5);
int squared = m.square(); // => 25 (instance method)

// Overriding accessor methods
record Temperature(double celsius) {
    // Override accessor to provide computed value
    public double fahrenheit() {
        return celsius * 9.0 / 5.0 + 32; // => Convert to Fahrenheit
    }

    @Override
    public double celsius() {
        return Math.round(celsius * 10) / 10.0; // => Round to 1 decimal place
    }
}

Temperature temp = new Temperature(25.67); // => 25.67°C
double c = temp.celsius(); // => 25.7 (rounded by overridden accessor)
double f = temp.fahrenheit(); // => 78.206 (computed)

// Generic records
record Pair<T, U>(T first, U second) {} // => Generic record

Pair<String, Integer> pair = new Pair<>("Age", 30); // => Pair of String and Integer
String key = pair.first(); // => "Age"
Integer value = pair.second(); // => 30

// Record with collections (defensive copying)
record Playlist(String name, List<String> songs) {
    // Compact constructor with defensive copy
    public Playlist {
        songs = List.copyOf(songs); // => Create immutable copy (defensive)
    }

    // Accessor already returns immutable list
}

List<String> mySongs = new ArrayList<>(List.of("Song 1", "Song 2"));
Playlist playlist = new Playlist("Favorites", mySongs);

mySongs.add("Song 3"); // => Modify original list
System.out.println(playlist.songs().size()); // => 2 (playlist unaffected, has immutable copy)

// playlist.songs().add("Song 4"); // => UnsupportedOperationException (immutable)

// Nested records
record Address(String street, String city, String zip) {}

record Customer(String name, Address address, List<String> phoneNumbers) {
    public Customer {
        phoneNumbers = List.copyOf(phoneNumbers); // => Defensive copy
    }
}

Address addr = new Address("123 Main St", "Springfield", "12345");
Customer customer = new Customer("Alice", addr, List.of("555-1234", "555-5678"));

String city = customer.address().city(); // => "Springfield" (nested access)

// Pattern matching with records (Java 16+)
record Rectangle(double width, double height) {}
record Circle(double radius) {}

// Deconstruction in switch (Java 19+, preview in 19-20, standard in 21+)
double area(Object shape) {
    return switch (shape) {
        case Rectangle(double w, double h) -> w * h; // => Deconstruct Rectangle
        case Circle(double r) -> Math.PI * r * r; // => Deconstruct Circle
        case null -> 0; // => Handle null
        default -> throw new IllegalArgumentException("Unknown shape");
    };
}

Rectangle rect = new Rectangle(5, 10);
double rectArea = area(rect); // => 50.0 (5 × 10)

Circle circ = new Circle(3);
double circArea = area(circ); // => 28.27 (π × 3²)

// Nested pattern matching (Java 21+)
record Point3D(int x, int y, int z) {}
record Line(Point3D start, Point3D end) {}

String describeLine(Line line) {
    return switch (line) {
        case Line(Point3D(int x1, int y1, int z1), Point3D(int x2, int y2, int z2)) ->
            "Line from (" + x1 + "," + y1 + "," + z1 + ") to (" + x2 + "," + y2 + "," + z2 + ")";
        // => Deconstructs Line AND both Point3D records
    };
}

Line line = new Line(new Point3D(0, 0, 0), new Point3D(10, 20, 30));
String desc = describeLine(line); // => "Line from (0,0,0) to (10,20,30)"

// Records with sealed types (powerful combination)
sealed interface JsonValue permits JsonText, JsonNum, JsonBool {}
record JsonText(String value) implements JsonValue {}
record JsonNum(double value) implements JsonValue {}
record JsonBool(boolean value) implements JsonValue {}

String formatJson(JsonValue json) {
    return switch (json) {
        case JsonText(String s) -> "\"" + s + "\""; // => Deconstruct and use value
        case JsonNum(double n) -> String.valueOf(n);
        case JsonBool(boolean b) -> String.valueOf(b);
        // No default needed (sealed + exhaustive)
    };
}

JsonValue text = new JsonText("hello");
String formatted = formatJson(text); // => "\"hello\""

// Record limitations
// - Cannot extend other classes (implicit extends Record)
// - Cannot declare instance fields beyond components
// - All fields are final (immutable)
// - Cannot be abstract
// - Cannot have native methods

// What you CAN do:
// - Implement interfaces ✓
// - Have static fields and methods ✓
// - Override methods (including accessors) ✓
// - Have instance methods ✓
// - Have nested types ✓
// - Use generics ✓

// Implementing interface
interface Drawable {
    void draw();
}

record Shape(String name) implements Drawable {
    @Override
    public void draw() {
        System.out.println("Drawing " + name); // => Instance method from interface
    }
}

Shape shape = new Shape("Circle");
shape.draw(); // => "Drawing Circle"

// Immutability with mutable components (anti-pattern)
record BadRecord(List<String> items) {} // => DON'T: mutable component

List<String> list = new ArrayList<>(List.of("A", "B"));
BadRecord bad = new BadRecord(list);
list.add("C"); // => Modifies record's internal state! (bad)

// Better: defensive copy in constructor
record GoodRecord(List<String> items) {
    public GoodRecord {
        items = List.copyOf(items); // => Immutable copy
    }
}

List<String> list2 = new ArrayList<>(List.of("X", "Y"));
GoodRecord good = new GoodRecord(list2);
list2.add("Z"); // => Doesn't affect record (good)
```

**Key Takeaway**: Records are immutable data classes with automatic constructor, accessors, `equals`, `hashCode`, `toString`. Compact constructor validates/normalizes without parameter list. Additional constructors must delegate to canonical. Override accessors for computed values. Use defensive copying for mutable components. Pattern matching deconstructs records. Combine with sealed types for exhaustive checking. Cannot extend classes or add instance fields. Ideal for DTOs, value objects, and domain models. Enforce immutability by using `List.copyOf()` for collections.

**Why It Matters**: Reflection enables runtime introspection and manipulation—frameworks use it for dependency injection, ORM, serialization, testing. It breaks encapsulation intentionally—accessing private fields, invoking private methods. However, reflection is slow, fragile (breaks with refactoring), and bypasses compile-time safety. Use it sparingly—prefer interfaces and composition. Understanding reflection is essential for framework developers and debugging, but application code should avoid it. Most reflection use cases (DI, serialization) are better served by annotation processing or code generation.

---

### Example 45: Stream Performance and Parallelization

Streams provide functional operations but have performance characteristics. Understand when to use sequential vs parallel streams, performance pitfalls, and optimization techniques for production code.

**Code**:

```java
import java.util.*;
import java.util.stream.*;
import java.util.concurrent.*;

// Sequential vs Parallel streams
List<Integer> numbers = IntStream.rangeClosed(1, 1000000) // => 1 million numbers
    .boxed()
    .toList(); // => [1, 2, 3, ..., 1000000]

// Sequential stream - single thread
long start = System.nanoTime();
long sum1 = numbers.stream() // => Sequential stream
    .mapToLong(Integer::longValue) // => Convert to long
    .sum(); // => Sum all numbers: 500000500000
long end = System.nanoTime();
System.out.println("Sequential: " + (end - start) / 1_000_000 + "ms"); // => ~50ms

// Parallel stream - multiple threads
start = System.nanoTime();
long sum2 = numbers.parallelStream() // => Parallel stream (uses ForkJoinPool)
    .mapToLong(Integer::longValue)
    .sum(); // => Same result: 500000500000
end = System.nanoTime();
System.out.println("Parallel: " + (end - start) / 1_000_000 + "ms"); // => ~15ms (faster)

// Creating parallel streams
Stream<Integer> parallel1 = numbers.parallelStream(); // => From collection
Stream<Integer> parallel2 = Stream.of(1, 2, 3).parallel(); // => Make stream parallel
Stream<Integer> parallel3 = IntStream.range(0, 100).parallel().boxed(); // => Primitive stream

// Check if stream is parallel
boolean isParallel = parallel1.isParallel(); // => true

// Convert between sequential and parallel
Stream<Integer> sequential = parallel1.sequential(); // => Make sequential
Stream<Integer> parallelAgain = sequential.parallel(); // => Make parallel again

// When parallel is faster (CPU-bound operations)
List<String> words = IntStream.range(0, 10000)
    .mapToObj(i -> "word" + i)
    .toList(); // => 10,000 words

// CPU-intensive operation
start = System.nanoTime();
long count1 = words.stream()
    .filter(s -> s.length() > 3) // => Simple filter
    .map(String::toUpperCase) // => CPU-intensive transformation
    .count();
end = System.nanoTime();
System.out.println("Sequential: " + (end - start) / 1_000_000 + "ms");

start = System.nanoTime();
long count2 = words.parallelStream()
    .filter(s -> s.length() > 3)
    .map(String::toUpperCase)
    .count();
end = System.nanoTime();
System.out.println("Parallel: " + (end - start) / 1_000_000 + "ms"); // => Usually faster

// When parallel is slower (small datasets or cheap operations)
List<Integer> small = List.of(1, 2, 3, 4, 5); // => Small dataset

start = System.nanoTime();
int sum3 = small.stream().mapToInt(i -> i).sum(); // => Sequential
end = System.nanoTime();
long seqTime = end - start;

start = System.nanoTime();
int sum4 = small.parallelStream().mapToInt(i -> i).sum(); // => Parallel
end = System.nanoTime();
long parTime = end - start;

// Parallel has overhead (thread coordination) - slower for small datasets
System.out.println("Parallel overhead: " + (parTime - seqTime) / 1000 + "μs");

// Thread safety issues with parallel streams
List<Integer> unsafeList = new ArrayList<>(); // => NOT thread-safe

// WRONG: ArrayList not thread-safe for parallel writes
numbers.parallelStream()
    .forEach(unsafeList::add); // => Race condition! Results unpredictable

System.out.println("Unsafe size: " + unsafeList.size()); // => NOT 1000000 (some lost)

// CORRECT: Use thread-safe collection
List<Integer> safeList = numbers.parallelStream()
    .collect(Collectors.toCollection(CopyOnWriteArrayList::new));
// => Thread-safe collection

System.out.println("Safe size: " + safeList.size()); // => 1000000 (all present)

// BEST: Use collectors (designed for parallel)
List<Integer> collected = numbers.parallelStream()
    .filter(n -> n % 2 == 0) // => Even numbers only
    .collect(Collectors.toList()); // => Thread-safe collector

// Order preservation with parallel streams
List<Integer> ordered = IntStream.range(0, 100)
    .parallel() // => Parallel stream
    .boxed()
    .collect(Collectors.toList()); // => Maintains encounter order: [0,1,2,...,99]

List<Integer> unordered = IntStream.range(0, 100)
    .parallel()
    .unordered() // => Hint: order not important (can improve performance)
    .boxed()
    .collect(Collectors.toList()); // => May not maintain order (but faster)

// forEachOrdered vs forEach in parallel
System.out.println("forEach (unordered):");
IntStream.range(0, 10)
    .parallel()
    .forEach(System.out::print); // => 4723061598 (unpredictable order)

System.out.println("\nforEachOrdered (ordered):");
IntStream.range(0, 10)
    .parallel()
    .forEachOrdered(System.out::print); // => 0123456789 (guaranteed order, slower)

// Spliterator - controls parallel splitting
List<Integer> list = new ArrayList<>(numbers);
Spliterator<Integer> spliterator = list.spliterator();

// Characteristics affect parallel performance
int characteristics = spliterator.characteristics();
boolean sized = spliterator.hasCharacteristics(Spliterator.SIZED); // => true (known size)
boolean ordered = spliterator.hasCharacteristics(Spliterator.ORDERED); // => true (has order)

// Custom ForkJoinPool for parallel streams
ForkJoinPool customPool = new ForkJoinPool(4); // => 4 threads (default uses all cores)

try {
    long customSum = customPool.submit(() ->
        numbers.parallelStream()
            .mapToLong(Integer::longValue)
            .sum()
    ).get(); // => Execute in custom pool (4 threads)

    System.out.println("Custom pool result: " + customSum);
} catch (Exception e) {
    e.printStackTrace();
} finally {
    customPool.shutdown(); // => Clean up custom pool
}

// Performance anti-patterns

// 1. Boxing/unboxing overhead
// SLOW: Boxing Integer to int repeatedly
long slowSum = numbers.stream()
    .map(i -> i * 2) // => Boxing/unboxing on each operation
    .reduce(0, Integer::sum); // => Inefficient

// FAST: Use primitive streams
long fastSum = numbers.stream()
    .mapToInt(Integer::intValue) // => Convert to IntStream once
    .map(i -> i * 2) // => Primitive operations (no boxing)
    .sum(); // => Efficient

// 2. Stateful operations in parallel (problematic)
AtomicInteger counter = new AtomicInteger(0); // => Shared mutable state

// AVOID: Stateful lambda in parallel
numbers.parallelStream()
    .map(n -> n * counter.incrementAndGet()) // => Race condition risk
    .collect(Collectors.toList()); // => Unpredictable results

// BETTER: Stateless operations
List<Integer> doubled = numbers.parallelStream()
    .map(n -> n * 2) // => Stateless (no shared mutable state)
    .collect(Collectors.toList()); // => Predictable, thread-safe

// 3. Interference during stream operations
List<Integer> source = new ArrayList<>(List.of(1, 2, 3, 4, 5));

// WRONG: Modifying source during stream
// source.stream()
//     .forEach(n -> source.add(n * 2)); // => ConcurrentModificationException

// CORRECT: Don't modify source, create new collection
List<Integer> result = source.stream()
    .map(n -> n * 2)
    .collect(Collectors.toList()); // => [2, 4, 6, 8, 10]

// Stream reuse (not allowed)
Stream<Integer> stream = numbers.stream();
long count3 = stream.count(); // => First terminal operation: 1000000
// long count4 = stream.count(); // => ERROR: stream already operated on

// Best practices for parallel streams:

// 1. Use for large datasets (10,000+ elements)
boolean useFarkJoinPool = numbers.size() > 10_000; // => Heuristic for parallelization

// 2. Use for CPU-intensive operations (not I/O)
// GOOD for parallel: complex calculations, transformations
// BAD for parallel: database queries, file I/O (use CompletableFuture instead)

// 3. Ensure thread safety
// Use Collectors, avoid shared mutable state, use thread-safe data structures

// 4. Prefer stateless operations
// map, filter, flatMap are stateless and parallelize well
// sorted, distinct are stateful and may reduce parallel performance

// 5. Measure performance
// Always benchmark sequential vs parallel for your specific workload

// Performance comparison: collect vs reduce
// collect is designed for parallelism
List<String> collected2 = numbers.parallelStream()
    .map(String::valueOf)
    .collect(Collectors.toList()); // => Efficient parallel collection

// reduce may have overhead for complex accumulation
String concatenated = numbers.stream()
    .limit(100)
    .map(String::valueOf)
    .reduce("", (a, b) -> a + "," + b); // => Inefficient for strings (use joining instead)

String joined = numbers.stream()
    .limit(100)
    .map(String::valueOf)
    .collect(Collectors.joining(",")); // => Efficient (designed for this use case)

// Primitive streams for performance
// BAD: Stream<Integer> with boxing
double avg1 = numbers.stream()
    .mapToDouble(Integer::doubleValue) // => Boxing/unboxing
    .average()
    .orElse(0.0);

// GOOD: IntStream without boxing
double avg2 = numbers.stream()
    .mapToInt(Integer::intValue) // => Convert to primitive once
    .average()
    .orElse(0.0); // => No boxing during operations

// Summary statistics (efficient)
IntSummaryStatistics stats = numbers.stream()
    .mapToInt(Integer::intValue)
    .summaryStatistics();

System.out.println("Count: " + stats.getCount()); // => 1000000
System.out.println("Sum: " + stats.getSum()); // => 500000500000
System.out.println("Min: " + stats.getMin()); // => 1
System.out.println("Max: " + stats.getMax()); // => 1000000
System.out.println("Average: " + stats.getAverage()); // => 500000.5
```

**Key Takeaway**: Parallel streams use `parallelStream()` or `.parallel()`. Best for large datasets (10k+ elements) with CPU-intensive operations. Overhead makes parallel slower for small data or cheap operations. Use `Collectors` for thread-safe accumulation. Avoid shared mutable state in parallel lambdas. `forEachOrdered()` maintains order (slower than `forEach()`). Primitive streams (`IntStream`, `LongStream`, `DoubleStream`) avoid boxing overhead. Custom `ForkJoinPool` controls thread count. Always benchmark sequential vs parallel. Stream reuse not allowed. Prefer stateless operations (`map`, `filter`) over stateful (`sorted`, `distinct`) for parallelism.

**Why It Matters**: Generics enable type-safe, reusable code—collections, algorithms, frameworks. Understanding wildcards (? extends, ? super) enables flexible APIs (PECS: Producer Extends, Consumer Super). Bounded type parameters enable constrained generics (T extends Comparable). Type erasure causes runtime type information loss—understanding this prevents confusion with reflection and overloading. Raw types break type safety—always use generics. Proper generics usage prevents ClassCastException, improves code readability, and enables powerful abstractions like Stream processing.
