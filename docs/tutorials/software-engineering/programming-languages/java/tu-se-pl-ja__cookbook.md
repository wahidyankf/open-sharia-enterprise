---
title: Java Cookbook - Practical Recipes
description: Day-to-day recipes and solutions for common Java programming problems - ready to copy and use
category: tutorial
tags:
  - java
  - cookbook
  - recipes
  - practical
  - solutions
  - reference
created: 2025-12-04
updated: 2025-12-04
---

# Java Cookbook - Practical Recipes

**Solve common problems with ready-to-use recipes.** This cookbook contains practical, copy-paste-ready solutions for everyday Java development. Perfect for finding solutions to problems you encounter in real projects.

## How to Use This Cookbook

- **Search** for your problem (CTRL+F)
- **Copy** the code example
- **Adapt** to your specific needs
- **Reference** the links for deeper learning

Each recipe includes:

- Problem statement
- Copy-paste-ready code
- Brief explanation
- Common variations

---

## Working with Collections

### Recipe: Remove Duplicates from List

**Problem**: You have a list with duplicate values and want only unique elements.

```java
import java.util.*;

List<String> list = Arrays.asList("apple", "banana", "apple", "cherry", "banana");

// Using HashSet
Set<String> unique = new HashSet<>(list);
List<String> uniqueList = new ArrayList<>(unique);  // [banana, cherry, apple] - unordered

// Using LinkedHashSet to preserve order
Set<String> linkedUnique = new LinkedHashSet<>(list);
List<String> orderedUnique = new ArrayList<>(linkedUnique);  // [apple, banana, cherry]

// Using Streams (Java 8+)
List<String> streamUnique = list.stream()
    .distinct()
    .collect(Collectors.toList());  // [apple, banana, cherry]
```

**Use**: HashSet when order doesn't matter, LinkedHashSet to preserve order, Streams for functional style.

### Recipe: Find Max/Min in Collection

**Problem**: Get the largest or smallest element from a collection.

```java
List<Integer> numbers = Arrays.asList(5, 2, 8, 1, 9, 3);

// Using Collections
int max = Collections.max(numbers);  // 9
int min = Collections.min(numbers);  // 1

// Using Streams
int streamMax = numbers.stream().max(Integer::compareTo).orElse(0);
int streamMin = numbers.stream().min(Integer::compareTo).orElse(0);

// Custom comparator
List<Person> people = /* ... */;
Person oldestPerson = Collections.max(people, Comparator.comparingInt(p -> p.age));
```

### Recipe: Sort Collection by Custom Field

**Problem**: Sort objects by a specific property.

```java
class Person {
    String name;
    int age;
    // ... constructor, getters
}

List<Person> people = Arrays.asList(
    new Person("Alice", 30),
    new Person("Bob", 25),
    new Person("Charlie", 35)
);

// Sort by age
Collections.sort(people, Comparator.comparingInt(p -> p.age));
// Result: Bob (25), Alice (30), Charlie (35)

// Sort by age descending
Collections.sort(people, Comparator.comparingInt(Person::getAge).reversed());

// Sort by multiple fields (name then age)
Collections.sort(people, Comparator
    .comparing(Person::getName)
    .thenComparingInt(Person::getAge));

// Using Streams
List<Person> sorted = people.stream()
    .sorted(Comparator.comparingInt(Person::getAge))
    .collect(Collectors.toList());
```

### Recipe: Group by Property

**Problem**: Group elements by a common property.

```java
List<Person> people = /* ... */;

// Group by name using Streams
Map<String, List<Person>> groupedByName = people.stream()
    .collect(Collectors.groupingBy(Person::getName));

// Group by age
Map<Integer, List<Person>> groupedByAge = people.stream()
    .collect(Collectors.groupingBy(Person::getAge));

// Count occurrences
Map<String, Long> counts = people.stream()
    .collect(Collectors.groupingBy(Person::getName, Collectors.counting()));

// Get first element from each group
Map<Integer, Person> firstByAge = people.stream()
    .collect(Collectors.groupingBy(
        Person::getAge,
        Collectors.collectingAndThen(
            Collectors.toList(),
            list -> list.isEmpty() ? null : list.get(0)
        )
    ));
```

---

## String Operations

### Recipe: Split and Process String

**Problem**: Parse a string into parts and process each.

```java
String csv = "Alice,Bob,Charlie,Diana";

// Split and convert to list
List<String> names = Arrays.asList(csv.split(","));

// Split with Stream
List<String> streamNames = Arrays.stream(csv.split(","))
    .map(String::trim)
    .filter(n -> !n.isEmpty())
    .collect(Collectors.toList());
```

### Recipe: Join Strings with Delimiter

**Problem**: Combine multiple strings with a separator.

```java
List<String> names = Arrays.asList("Alice", "Bob", "Charlie");

// Using String.join()
String result = String.join(", ", names);  // "Alice, Bob, Charlie"

// Using Streams
String streamResult = names.stream()
    .collect(Collectors.joining(", "));

// With transformation
String uppercase = names.stream()
    .map(String::toUpperCase)
    .collect(Collectors.joining(","));  // "ALICE,BOB,CHARLIE"
```

### Recipe: Find and Replace

**Problem**: Replace text patterns in a string.

```java
String text = "Hello World, Hello Java";

// Simple replace
String simple = text.replace("Hello", "Hi");  // Case-sensitive

// Case-insensitive replace
String insensitive = text.replaceAll("(?i)hello", "Hi");

// Replace first occurrence
String firstOnly = text.replaceFirst("Hello", "Hi");

// Using Pattern for complex replacements
import java.util.regex.*;

Pattern pattern = Pattern.compile("\\b(Hello)\\b");
Matcher matcher = pattern.matcher(text);
String regex = matcher.replaceAll("Hi");
```

### Recipe: Check if String Matches Pattern

**Problem**: Validate string format against regex.

```java
// Email validation
String email = "user@example.com";
boolean isEmail = email.matches("^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}$");

// Phone number (US format)
String phone = "123-456-7890";
boolean isPhone = phone.matches("\\d{3}-\\d{3}-\\d{4}");

// IP address
String ip = "192.168.1.1";
boolean isIP = ip.matches("^(\\d{1,3}\\.){3}\\d{1,3}$");

// Using Pattern for reuse
Pattern emailPattern = Pattern.compile("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}$");
boolean valid = emailPattern.matcher(email).matches();
```

### Recipe: Format String with Values

**Problem**: Create formatted output with variables.

```java
String name = "Alice";
int age = 30;
double salary = 50000.50;

// Using String.format()
String formatted = String.format("Name: %s, Age: %d, Salary: $%.2f", name, age, salary);
// "Name: Alice, Age: 30, Salary: $50000.50"

// Using String concatenation (Java 17+ Text Blocks)
String text = """
    Name: %s
    Age: %d
    Salary: $%.2f
    """.formatted(name, age, salary);

// Using StringBuilder
StringBuilder sb = new StringBuilder();
sb.append("Name: ").append(name)
    .append(", Age: ").append(age)
    .append(", Salary: $").append(String.format("%.2f", salary));
String result = sb.toString();
```

---

## File and I/O Operations

### Recipe: Read File to String

**Problem**: Load entire file contents into a String.

```java
import java.nio.file.*;

// Java 11+ recommended way
String content = Files.readString(Paths.get("file.txt"));

// Older Java versions
List<String> lines = Files.readAllLines(Paths.get("file.txt"));
String content = String.join("\n", lines);

// With proper encoding
String content = Files.readString(
    Paths.get("file.txt"),
    StandardCharsets.UTF_8
);
```

### Recipe: Write String to File

**Problem**: Save string content to a file.

```java
import java.nio.file.*;

String content = "Hello, World!";

// Java 11+ recommended
Files.writeString(Paths.get("output.txt"), content);

// With specific encoding
Files.writeString(
    Paths.get("output.txt"),
    content,
    StandardCharsets.UTF_8
);

// Append to file
Files.writeString(
    Paths.get("output.txt"),
    "Additional line\n",
    StandardOpenOption.APPEND
);
```

### Recipe: List Files in Directory

**Problem**: Get all files from a directory.

```java
import java.nio.file.*;
import java.io.IOException;

Path dir = Paths.get("src");

// List files (not recursive)
Files.list(dir)
    .filter(Files::isRegularFile)
    .forEach(System.out::println);

// Recursive (walk entire tree)
Files.walk(dir)
    .filter(Files::isRegularFile)
    .forEach(System.out::println);

// With filter
Files.walk(dir)
    .filter(p -> p.toString().endsWith(".java"))
    .forEach(System.out::println);

// Get File objects
File[] files = new File("src").listFiles((dir1, name) -> name.endsWith(".java"));
```

### Recipe: Create Directory If Not Exists

**Problem**: Ensure directory exists before operations.

```java
import java.nio.file.*;

Path dir = Paths.get("output");

// Create if doesn't exist
if (!Files.exists(dir)) {
    Files.createDirectory(dir);
}

// Create with parents
Files.createDirectories(Paths.get("output/nested/dir"));

// Or in one line
Files.createDirectories(dir);  // Safe - no error if exists
```

---

## Dates and Times

### Recipe: Format Current Date/Time

**Problem**: Display date and time in readable format.

```java
import java.time.*;
import java.time.format.DateTimeFormatter;

// Current date and time
LocalDateTime now = LocalDateTime.now();
System.out.println(now);  // 2025-12-04T14:30:45.123

// Format to string
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
String formatted = now.format(formatter);  // "2025-12-04 14:30:45"

// Common formats
String date = LocalDate.now().toString();  // "2025-12-04"
String time = LocalTime.now().toString();  // "14:30:45.123"

// With locale
formatter = DateTimeFormatter.ofPattern("EEEE, MMMM d, yyyy", new java.util.Locale("en_US"));
String pretty = now.format(formatter);  // "Wednesday, December 4, 2025"
```

### Recipe: Parse String to Date

**Problem**: Convert string to date object.

```java
import java.time.*;
import java.time.format.DateTimeFormatter;

String dateStr = "2025-12-04";
String timeStr = "14:30:45";

LocalDate date = LocalDate.parse(dateStr);  // "2025-12-04"
LocalTime time = LocalTime.parse(timeStr);  // "14:30:45"

// Custom format
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
LocalDate customDate = LocalDate.parse("04/12/2025", formatter);

// DateTime
String dateTimeStr = "2025-12-04 14:30:45";
DateTimeFormatter dtFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
LocalDateTime dateTime = LocalDateTime.parse(dateTimeStr, dtFormatter);
```

### Recipe: Calculate Time Differences

**Problem**: Find duration between two dates.

```java
import java.time.*;

LocalDateTime start = LocalDateTime.of(2025, 12, 4, 10, 0, 0);
LocalDateTime end = LocalDateTime.of(2025, 12, 5, 15, 30, 0);

Duration duration = Duration.between(start, end);
System.out.println("Hours: " + duration.toHours());      // 29
System.out.println("Minutes: " + duration.toMinutes());  // 1770
System.out.println("Seconds: " + duration.getSeconds());

// Using Period for dates
LocalDate startDate = LocalDate.of(2025, 1, 1);
LocalDate endDate = LocalDate.of(2025, 12, 31);

Period period = Period.between(startDate, endDate);
System.out.println("Days: " + period.getDays());
System.out.println("Months: " + period.getMonths());
System.out.println("Years: " + period.getYears());
```

---

## Exception Handling

### Recipe: Try-Catch with Resource Cleanup

**Problem**: Ensure resources are properly closed even if error occurs.

```java
import java.io.*;
import java.nio.file.*;

// Try-with-resources (Java 7+) - automatically closes
try (BufferedReader reader = new BufferedReader(new FileReader("file.txt"))) {
    String line;
    while ((line = reader.readLine()) != null) {
        System.out.println(line);
    }
} catch (IOException e) {
    System.err.println("Error reading file: " + e.getMessage());
}

// Multiple resources
try (
    FileInputStream fis = new FileInputStream("input.txt");
    FileOutputStream fos = new FileOutputStream("output.txt")
) {
    byte[] buffer = new byte[1024];
    int length;
    while ((length = fis.read(buffer)) > 0) {
        fos.write(buffer, 0, length);
    }
} catch (IOException e) {
    e.printStackTrace();
}
```

### Recipe: Chain Exceptions

**Problem**: Preserve original exception while throwing new one.

```java
try {
    // Code that might fail
    int result = 10 / 0;
} catch (ArithmeticException e) {
    // Wrap in custom exception, preserving original
    throw new CustomException("Failed to calculate", e);
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

// Usage with cause chain
try {
    operation();
} catch (CustomException e) {
    System.out.println("Direct cause: " + e.getCause().getClass().getName());
}
```

---

## Testing

### Recipe: Unit Test Template

**Problem**: Write a basic test class.

```java
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import static org.junit.jupiter.api.Assertions.*;

public class CalculatorTest {
    private Calculator calculator;

    @BeforeEach
    public void setUp() {
        calculator = new Calculator();
    }

    @Test
    public void testAddition() {
        assertEquals(5, calculator.add(2, 3));
    }

    @Test
    public void testDivisionByZeroThrows() {
        assertThrows(IllegalArgumentException.class, () -> {
            calculator.divide(10, 0);
        });
    }

    @Test
    public void testMultiplication() {
        assertEquals(12, calculator.multiply(3, 4));
    }
}
```

### Recipe: Test with Parameterized Data

**Problem**: Run same test with multiple inputs.

```java
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import static org.junit.jupiter.api.Assertions.*;

public class MathTest {
    @ParameterizedTest
    @ValueSource(ints = {1, 3, 5, 7})
    public void testOddNumbers(int number) {
        assertTrue(isOdd(number));
    }

    @ParameterizedTest
    @CsvSource({
        "2,4,6",    // 2 + 4 = 6
        "3,5,8",    // 3 + 5 = 8
        "0,0,0"     // 0 + 0 = 0
    })
    public void testAddition(int a, int b, int expected) {
        assertEquals(expected, a + b);
    }

    private boolean isOdd(int n) {
        return n % 2 == 1;
    }
}
```

---

## Concurrency

### Recipe: Run Code in Background Thread

**Problem**: Execute long-running operation without blocking main thread.

```java
// Using Thread
Thread thread = new Thread(() -> {
    System.out.println("Running in background...");
    try {
        Thread.sleep(2000);
    } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
    }
    System.out.println("Background task complete");
});
thread.start();

// Using ExecutorService
import java.util.concurrent.*;

ExecutorService executor = Executors.newSingleThreadExecutor();
executor.submit(() -> {
    System.out.println("Running in thread pool");
});
executor.shutdown();  // Important: shut down when done
```

### Recipe: Wait for Multiple Threads

**Problem**: Start several threads and wait for all to finish.

```java
import java.util.concurrent.*;

ExecutorService executor = Executors.newFixedThreadPool(3);

// Submit multiple tasks
for (int i = 0; i < 5; i++) {
    final int taskId = i;
    executor.submit(() -> {
        System.out.println("Task " + taskId + " started");
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        System.out.println("Task " + taskId + " finished");
    });
}

// Wait for all to finish
executor.shutdown();
boolean completed = executor.awaitTermination(10, TimeUnit.SECONDS);

if (completed) {
    System.out.println("All tasks finished");
} else {
    System.out.println("Timeout - some tasks still running");
}
```

---

## Working with JSON

### Recipe: Convert Object to JSON

**Problem**: Serialize Java object to JSON string.

```java
import com.google.gson.Gson;

class Person {
    String name;
    int age;

    Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}

Person person = new Person("Alice", 30);

Gson gson = new Gson();
String json = gson.toJson(person);
System.out.println(json);  // {"name":"Alice","age":30}
```

### Recipe: Parse JSON to Object

**Problem**: Deserialize JSON string to Java object.

```java
import com.google.gson.Gson;

String json = "{\"name\":\"Bob\",\"age\":25}";

Gson gson = new Gson();
Person person = gson.fromJson(json, Person.class);
System.out.println(person.name);  // "Bob"
System.out.println(person.age);   // 25
```

---

## REST API Usage

### Recipe: Make HTTP GET Request

**Problem**: Fetch data from a web service.

```java
import java.net.http.*;
import java.net.URI;

HttpClient client = HttpClient.newHttpClient();

HttpRequest request = HttpRequest.newBuilder()
    .GET()
    .uri(URI.create("https://api.example.com/users/123"))
    .build();

HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

System.out.println("Status: " + response.statusCode());
System.out.println("Body: " + response.body());
```

### Recipe: Make HTTP POST Request

**Problem**: Send data to a web service.

```java
import java.net.http.*;

String jsonBody = "{\"name\":\"Alice\",\"email\":\"alice@example.com\"}";

HttpRequest request = HttpRequest.newBuilder()
    .POST(HttpRequest.BodyPublishers.ofString(jsonBody))
    .uri(URI.create("https://api.example.com/users"))
    .header("Content-Type", "application/json")
    .build();

HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
```

---

## Frequently Used Patterns

### Recipe: Builder Pattern

**Problem**: Create objects with many optional fields.

```java
public class RequestBuilder {
    private String url;
    private String method = "GET";
    private Map<String, String> headers = new HashMap<>();
    private String body = "";

    public RequestBuilder url(String url) {
        this.url = url;
        return this;
    }

    public RequestBuilder method(String method) {
        this.method = method;
        return this;
    }

    public RequestBuilder header(String key, String value) {
        headers.put(key, value);
        return this;
    }

    public RequestBuilder body(String body) {
        this.body = body;
        return this;
    }

    public Request build() {
        return new Request(url, method, headers, body);
    }
}

// Usage
Request request = new RequestBuilder()
    .url("https://api.example.com")
    .method("POST")
    .header("Authorization", "Bearer token123")
    .body("{}")
    .build();
```

### Recipe: Null Safety with Optional

**Problem**: Handle null values safely.

```java
import java.util.Optional;

Optional<String> value = Optional.ofNullable(someValue);

// Check if present
if (value.isPresent()) {
    System.out.println(value.get());
}

// Or use ifPresentOrElse
value.ifPresentOrElse(
    v -> System.out.println("Value: " + v),
    () -> System.out.println("No value")
);

// Or provide default
String result = value.orElse("default");
String result2 = value.orElseGet(() -> "computed default");
String result3 = value.orElseThrow(() -> new RuntimeException("Missing value"));

// Chain operations
String upper = value
    .map(String::toUpperCase)
    .orElse("DEFAULT");
```

---

## Final Tips

- **When stuck**: Search this cookbook or Stack Overflow
- **Before copying**: Understand what the code does
- **After copying**: Adapt to your specific context
- **Test**: Always test before using in production
- **Refactor**: Extract repeated recipes into utility classes

---

**Happy Cooking!** Use this cookbook as your daily reference for common Java problems.
