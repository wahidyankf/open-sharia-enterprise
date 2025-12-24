---
title: "Cookbook"
date: 2025-12-04T00:00:00+07:00
draft: false
description: Day-to-day recipes and solutions for common Java programming problems - ready to copy and use
weight: 1000001
tags:
  - java
  - cookbook
  - recipes
  - practical
  - solutions
  - reference
---

---

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

### Recipe 1: Remove Duplicates from List

**Problem**: You have a list with duplicate values and want only unique elements.

**Solution**:

```java
import java.util.*;
import java.util.stream.Collectors;

// Basic example - Using HashSet for simple deduplication
List<String> fruits = Arrays.asList("apple", "banana", "apple", "cherry", "banana");

// Convert to Set to remove duplicates (order not preserved)
Set<String> uniqueSet = new HashSet<>(fruits);
List<String> uniqueList = new ArrayList<>(uniqueSet);
// Output: [banana, cherry, apple] - order not guaranteed

// Using LinkedHashSet to preserve insertion order
Set<String> orderedSet = new LinkedHashSet<>(fruits);
List<String> orderedList = new ArrayList<>(orderedSet);
// Output: [apple, banana, cherry] - order preserved

System.out.println("Original: " + fruits);
System.out.println("Unique (unordered): " + uniqueList);
System.out.println("Unique (ordered): " + orderedList);
```

HashSet provides O(1) deduplication by using hash-based equality checks. LinkedHashSet maintains insertion order while removing duplicates. For simple cases, converting to a Set and back to a List is the most efficient approach.

```java
// Advanced example - Using Streams for functional style
import java.util.*;
import java.util.stream.Collectors;

List<String> fruits = Arrays.asList("apple", "banana", "apple", "cherry", "banana", "date");

// Stream-based deduplication (preserves encounter order)
List<String> uniqueFruits = fruits.stream()
    .distinct()  // Remove duplicates based on equals()
    .collect(Collectors.toList());
// Output: [apple, banana, cherry, date]

// Remove duplicates from custom objects
class Product {
    String name;
    double price;
    Product(String name, double price) { this.name = name; this.price = price; }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Product)) return false;
        Product product = (Product) o;
        return name.equals(product.name);  // Compare by name only
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);  // Hash based on name
    }
}

List<Product> products = Arrays.asList(
    new Product("Laptop", 999),
    new Product("Mouse", 25),
    new Product("Laptop", 1099)  // Duplicate name, different price
);

List<Product> uniqueProducts = products.stream()
    .distinct()
    .collect(Collectors.toList());
// Output: [Laptop (999), Mouse (25)] - second Laptop removed
```

**When to use**: Use HashSet for simple deduplication when order doesn't matter. Use LinkedHashSet when you need to preserve insertion order. Use Streams when working in a functional pipeline or when deduplicating custom objects with equals/hashCode.

---

### Recipe 2: Find Max/Min in Collection

**Problem**: Get the largest or smallest element from a collection.

**Solution**:

```java
import java.util.*;

// Basic example - Finding max/min in number collections
List<Integer> numbers = Arrays.asList(5, 2, 8, 1, 9, 3);

// Using Collections utility methods (simplest approach)
int max = Collections.max(numbers);  // Returns 9
int min = Collections.min(numbers);  // Returns 1

System.out.println("Numbers: " + numbers);
System.out.println("Maximum: " + max);
System.out.println("Minimum: " + min);

// Handle empty collections safely
List<Integer> empty = new ArrayList<>();
Integer safeMax = empty.isEmpty() ? null : Collections.max(empty);
Integer safeMin = empty.isEmpty() ? null : Collections.min(empty);
// Output: safeMax = null, safeMin = null (avoids NoSuchElementException)
```

Collections.max() and Collections.min() use natural ordering (Comparable) to find extremes. They throw NoSuchElementException for empty collections, so check isEmpty() first for safety. For primitive wrappers (Integer, Double, etc.), these methods are the most straightforward.

```java
// Advanced example - Custom objects and Stream-based approach
import java.util.*;
import java.util.stream.*;

class Product {
    String name;
    double price;
    Product(String name, double price) { this.name = name; this.price = price; }
    @Override
    public String toString() { return name + " ($" + price + ")"; }
}

List<Product> products = Arrays.asList(
    new Product("Laptop", 999.99),
    new Product("Mouse", 24.99),
    new Product("Monitor", 349.50),
    new Product("Keyboard", 79.99)
);

// Using Streams with custom comparator (more flexible)
Product cheapest = products.stream()
    .min(Comparator.comparingDouble(p -> p.price))  // Compare by price
    .orElse(null);  // Handle empty stream gracefully
// Output: Mouse ($24.99)

Product mostExpensive = products.stream()
    .max(Comparator.comparingDouble(p -> p.price))
    .orElse(null);
// Output: Laptop ($999.99)

// Find product with longest name
Product longestName = products.stream()
    .max(Comparator.comparingInt(p -> p.name.length()))
    .orElse(null);
// Output: Keyboard ($79.99)

System.out.println("Cheapest: " + cheapest);
System.out.println("Most expensive: " + mostExpensive);
System.out.println("Longest name: " + longestName);
```

**When to use**: Use Collections.max/min for simple collections with natural ordering. Use Stream.max/min with custom Comparator when working with objects or when you need to compare by specific properties. Streams integrate better with functional pipelines and provide safer empty-collection handling via Optional.

---

### Recipe 3: Sort Collection by Custom Field

**Problem**: Sort objects by a specific property.

**Solution**:

```java
import java.util.*;

// Basic example - Sorting with Comparator
class Person {
    String name;
    int age;

    Person(String name, int age) {
        this.name = name;
        this.age = age;
    }

    @Override
    public String toString() {
        return name + " (" + age + ")";
    }
}

List<Person> people = new ArrayList<>(Arrays.asList(
    new Person("Alice", 30),
    new Person("Bob", 25),
    new Person("Charlie", 35),
    new Person("Diana", 28)
));

// Sort by age (ascending) using comparingInt
people.sort(Comparator.comparingInt(p -> p.age));
System.out.println("Sorted by age: " + people);
// Output: [Bob (25), Diana (28), Alice (30), Charlie (35)]

// Sort by age descending
people.sort(Comparator.comparingInt((Person p) -> p.age).reversed());
System.out.println("Sorted by age (descending): " + people);
// Output: [Charlie (35), Alice (30), Diana (28), Bob (25)]

// Sort by name alphabetically
people.sort(Comparator.comparing(p -> p.name));
System.out.println("Sorted by name: " + people);
// Output: [Alice (30), Bob (25), Charlie (35), Diana (28)]
```

Comparator.comparingInt() and comparing() create comparators based on extracted properties. Use reversed() to invert sort order. The sort() method modifies the list in-place, unlike streams which create new collections.

```java
// Advanced example - Multi-field sorting and stream-based sorting
import java.util.*;
import java.util.stream.Collectors;

class Product {
    String category;
    String name;
    double price;

    Product(String category, String name, double price) {
        this.category = category;
        this.name = name;
        this.price = price;
    }

    @Override
    public String toString() {
        return category + "/" + name + " ($" + price + ")";
    }
}

List<Product> products = Arrays.asList(
    new Product("Electronics", "Laptop", 999.99),
    new Product("Electronics", "Mouse", 24.99),
    new Product("Books", "Java Guide", 49.99),
    new Product("Books", "Python Basics", 39.99)
);

// Multi-field sort: category first, then price
List<Product> sorted = products.stream()
    .sorted(Comparator
        .comparing((Product p) -> p.category)  // Primary sort by category
        .thenComparingDouble(p -> p.price))    // Secondary sort by price
    .collect(Collectors.toList());

System.out.println("Sorted by category, then price:");
sorted.forEach(System.out::println);
// Output:
// Books/Python Basics ($39.99)
// Books/Java Guide ($49.99)
// Electronics/Mouse ($24.99)
// Electronics/Laptop ($999.99)

// Complex: Sort by price descending within each category
List<Product> complexSort = products.stream()
    .sorted(Comparator
        .comparing((Product p) -> p.category)
        .thenComparingDouble((Product p) -> p.price).reversed())
    .collect(Collectors.toList());
```

**When to use**: Use Comparator.comparing() for object sorting by properties. For multi-field sorting, chain with thenComparing(). Use in-place sort() for mutable lists, or stream().sorted() when you need an immutable original or are building a processing pipeline.

---

### Recipe 4: Group by Property

**Problem**: Group elements by a common property.

**Solution**:

```java
import java.util.*;
import java.util.stream.Collectors;

// Basic example - Simple grouping with Collectors.groupingBy
class Person {
    String department;
    String name;
    int age;

    Person(String department, String name, int age) {
        this.department = department;
        this.name = name;
        this.age = age;
    }

    @Override
    public String toString() {
        return name + " (" + department + ", " + age + ")";
    }
}

List<Person> people = Arrays.asList(
    new Person("Engineering", "Alice", 30),
    new Person("Engineering", "Bob", 25),
    new Person("Sales", "Charlie", 35),
    new Person("Sales", "Diana", 28),
    new Person("HR", "Eve", 32)
);

// Group by department
Map<String, List<Person>> byDepartment = people.stream()
    .collect(Collectors.groupingBy(p -> p.department));

System.out.println("Grouped by department:");
byDepartment.forEach((dept, employees) -> {
    System.out.println(dept + ": " + employees);
});
// Output:
// Engineering: [Alice (Engineering, 30), Bob (Engineering, 25)]
// Sales: [Charlie (Sales, 35), Diana (Sales, 28)]
// HR: [Eve (HR, 32)]
```

Collectors.groupingBy() creates a Map where keys are the grouping property and values are Lists of matching elements. The classifier function (Person::department) extracts the grouping key. This is similar to SQL's GROUP BY.

```java
// Advanced example - Grouping with aggregation and transformation
import java.util.*;
import java.util.stream.Collectors;

List<Person> people = Arrays.asList(
    new Person("Engineering", "Alice", 30),
    new Person("Engineering", "Bob", 25),
    new Person("Sales", "Charlie", 35),
    new Person("Sales", "Diana", 28),
    new Person("HR", "Eve", 32)
);

// Count employees per department
Map<String, Long> countByDept = people.stream()
    .collect(Collectors.groupingBy(
        p -> p.department,
        Collectors.counting()  // Downstream collector for aggregation
    ));
// Output: {Engineering=2, Sales=2, HR=1}

// Calculate average age per department
Map<String, Double> avgAgeByDept = people.stream()
    .collect(Collectors.groupingBy(
        p -> p.department,
        Collectors.averagingInt(p -> p.age)
    ));
// Output: {Engineering=27.5, Sales=31.5, HR=32.0}

// Get names only, grouped by department
Map<String, List<String>> namesByDept = people.stream()
    .collect(Collectors.groupingBy(
        p -> p.department,
        Collectors.mapping(p -> p.name, Collectors.toList())
    ));
// Output: {Engineering=[Alice, Bob], Sales=[Charlie, Diana], HR=[Eve]}

// Get oldest person per department
Map<String, Person> oldestByDept = people.stream()
    .collect(Collectors.groupingBy(
        p -> p.department,
        Collectors.collectingAndThen(
            Collectors.maxBy(Comparator.comparingInt(p -> p.age)),
            opt -> opt.orElse(null)
        )
    ));
// Output: {Engineering=Alice (30), Sales=Charlie (35), HR=Eve (32)}

System.out.println("Count by dept: " + countByDept);
System.out.println("Avg age by dept: " + avgAgeByDept);
System.out.println("Names by dept: " + namesByDept);
System.out.println("Oldest by dept: " + oldestByDept);
```

**When to use**: Use groupingBy() when you need to organize collections by a common property. Combine with downstream collectors (counting, averaging, mapping) for aggregations. This pattern is essential for report generation, data analysis, and partitioning collections by criteria.

---

## String Operations

### Recipe 5: Split and Process String

**Problem**: Parse a string into parts and process each.

**Solution**:

```java
import java.util.*;
import java.util.stream.*;

// Basic example - Splitting strings with delimiter
String csv = "Alice,Bob,Charlie,Diana";

// Simple split - returns array
String[] namesArray = csv.split(",");
System.out.println("Array: " + Arrays.toString(namesArray));
// Output: [Alice, Bob, Charlie, Diana]

// Convert to List for easier manipulation
List<String> names = Arrays.asList(csv.split(","));
System.out.println("List: " + names);
// Output: [Alice, Bob, Charlie, Diana]

// Split with multiple delimiters using regex
String mixed = "Apple;Banana,Cherry|Date";
String[] fruits = mixed.split("[;,|]");  // Split on semicolon, comma, or pipe
System.out.println("Fruits: " + Arrays.toString(fruits));
// Output: [Apple, Banana, Cherry, Date]

// Split with limit (max number of splits)
String text = "one:two:three:four";
String[] limited = text.split(":", 2);  // Split only first occurrence
System.out.println("Limited: " + Arrays.toString(limited));
// Output: [one, two:three:four]
```

The split() method uses regex patterns to divide strings. Returns an array, which can be converted to List using Arrays.asList(). Use character classes like [;,|] to split on multiple delimiters.

```java
// Advanced example - Split with Stream processing and cleaning
import java.util.*;
import java.util.stream.Collectors;

String messyData = "  Alice  , Bob,  ,Charlie,   Diana  ,  ";

// Stream-based split with cleaning (trim whitespace, filter empty)
List<String> cleanNames = Arrays.stream(messyData.split(","))
    .map(String::trim)           // Remove leading/trailing whitespace
    .filter(s -> !s.isEmpty())   // Remove empty strings
    .collect(Collectors.toList());
System.out.println("Cleaned: " + cleanNames);
// Output: [Alice, Bob, Charlie, Diana]

// Split and transform in one pipeline
String numbers = "1,2,3,4,5";
List<Integer> intList = Arrays.stream(numbers.split(","))
    .map(String::trim)
    .map(Integer::parseInt)      // Parse to integers
    .collect(Collectors.toList());
System.out.println("Integers: " + intList);
// Output: [1, 2, 3, 4, 5]

// Split multi-line text
String multiline = "Line 1\nLine 2\nLine 3";
List<String> lines = Arrays.stream(multiline.split("\\n"))
    .filter(line -> !line.isEmpty())
    .collect(Collectors.toList());
System.out.println("Lines: " + lines);
// Output: [Line 1, Line 2, Line 3]

// Split on whitespace (any amount)
String sentence = "Java   is    awesome";
String[] words = sentence.split("\\s+");  // \\s+ = one or more whitespace
System.out.println("Words: " + Arrays.toString(words));
// Output: [Java, is, awesome]
```

**When to use**: Use split() for simple delimited data like CSV or tab-separated values. Combine with Stream for cleaning and transformation. For complex CSV with quoted fields or escaping, use dedicated CSV parsing libraries instead of split().

---

### Recipe 6: Join Strings with Delimiter

**Problem**: Combine multiple strings with a separator.

**Solution**:

```java
import java.util.*;
import java.util.stream.Collectors;

// Basic example - Joining strings with String.join()
List<String> names = Arrays.asList("Alice", "Bob", "Charlie");

// Simple join with comma-space
String result = String.join(", ", names);
System.out.println("Joined: " + result);
// Output: Alice, Bob, Charlie

// Join array elements
String[] colors = {"Red", "Green", "Blue"};
String colorString = String.join(" | ", colors);
System.out.println("Colors: " + colorString);
// Output: Red | Green | Blue

// Join with custom delimiter
List<String> paths = Arrays.asList("home", "user", "documents");
String path = String.join("/", paths);
System.out.println("Path: /" + path);
// Output: /home/user/documents

// Join with empty delimiter (concatenate)
String[] letters = {"H", "e", "l", "l", "o"};
String word = String.join("", letters);
System.out.println("Word: " + word);
// Output: Hello
```

String.join() is the simplest way to combine strings with a delimiter. Works with Collections and arrays. For empty delimiter, strings are concatenated directly. Null-safe (null elements become "null" string).

```java
// Advanced example - Stream-based joining with transformation
import java.util.*;
import java.util.stream.Collectors;

List<String> names = Arrays.asList("alice", "bob", "charlie");

// Join with transformation (capitalize first letter)
String capitalized = names.stream()
    .map(name -> name.substring(0, 1).toUpperCase() + name.substring(1))
    .collect(Collectors.joining(", "));
System.out.println("Capitalized: " + capitalized);
// Output: Alice, Bob, Charlie

// Join with prefix and suffix
String formatted = names.stream()
    .collect(Collectors.joining(", ", "[", "]"));  // prefix, suffix
System.out.println("Formatted: " + formatted);
// Output: [alice, bob, charlie]

// Filter and join
List<String> items = Arrays.asList("apple", "", "banana", null, "cherry", "");
String filtered = items.stream()
    .filter(s -> s != null && !s.isEmpty())  // Remove nulls and empty
    .collect(Collectors.joining(", "));
System.out.println("Filtered: " + filtered);
// Output: apple, banana, cherry

// Join with index numbers
String numbered = IntStream.range(0, names.size())
    .mapToObj(i -> (i + 1) + ". " + names.get(i))
    .collect(Collectors.joining("\n"));  // Join with newlines
System.out.println("Numbered:\n" + numbered);
// Output:
// 1. alice
// 2. bob
// 3. charlie
```

**When to use**: Use String.join() for simple concatenation with delimiters. Use Collectors.joining() when combining with Stream transformations or when you need prefix/suffix. For building complex strings in loops, consider StringBuilder for better performance.

---

### Recipe 7: Find and Replace

**Problem**: Replace text patterns in a string.

**Solution**:

```java
// Basic example - Simple string replacement
String text = "Hello World, Hello Java";

// Replace all occurrences (literal string, case-sensitive)
String allReplaced = text.replace("Hello", "Hi");
System.out.println("Replace all: " + allReplaced);
// Output: Hi World, Hi Java

// Replace only first occurrence
String firstReplaced = text.replaceFirst("Hello", "Hi");
System.out.println("Replace first: " + firstReplaced);
// Output: Hi World, Hello Java

// Replace characters
String message = "Java is great!";
String noSpaces = message.replace(" ", "_");
System.out.println("No spaces: " + noSpaces);
// Output: Java_is_great!

// Remove text (replace with empty string)
String removed = text.replace("Hello ", "");
System.out.println("Removed: " + removed);
// Output: World, Java
```

The replace() method does literal string replacement (no regex). It's fast and safe for user input. Use replaceFirst() to replace only the first match. To remove text, replace with empty string "".

```java
// Advanced example - Regex-based replacement and Pattern/Matcher
import java.util.regex.*;

String text = "Hello World, Hello Java, HELLO everyone";

// Case-insensitive replace using regex flag
String caseInsensitive = text.replaceAll("(?i)hello", "Hi");
System.out.println("Case-insensitive: " + caseInsensitive);
// Output: Hi World, Hi Java, Hi everyone

// Replace with regex pattern (word boundaries)
String wordBoundary = text.replaceAll("\\bHello\\b", "Hi");
System.out.println("Word boundary: " + wordBoundary);
// Output: Hi World, Hi Java, HELLO everyone

// Replace digits with asterisks
String withNumbers = "User123 has 45 points";
String masked = withNumbers.replaceAll("\\d+", "***");
System.out.println("Masked: " + masked);
// Output: User*** has *** points

// Using Pattern and Matcher for complex replacements
Pattern pattern = Pattern.compile("(\\w+)@(\\w+\\.\\w+)");
Matcher matcher = pattern.matcher("Contact: user@example.com or admin@test.org");

// Replace with capturing groups
String anonymized = matcher.replaceAll("****@$2");  // Keep domain, hide user
System.out.println("Anonymized: " + anonymized);
// Output: Contact: ****@example.com or ****@test.org

// Replace with callback function (using appendReplacement)
StringBuffer sb = new StringBuffer();
matcher.reset();  // Reset matcher to beginning
while (matcher.find()) {
    String replacement = matcher.group(1).toUpperCase() + "@" + matcher.group(2);
    matcher.appendReplacement(sb, replacement);
}
matcher.appendTail(sb);
System.out.println("Uppercase users: " + sb.toString());
// Output: Contact: USER@example.com or ADMIN@test.org
```

**When to use**: Use replace() for literal string replacement (safest, fastest). Use replaceAll() or replaceFirst() for regex patterns like emails, phone numbers, or word boundaries. Use Pattern/Matcher when you need to access matched groups or perform complex transformations during replacement.

---

### Recipe 8: Check if String Matches Pattern

**Problem**: Validate string format against regex.

**Solution**:

```java
import java.util.regex.Pattern;

// Basic example - Common validation patterns
String email = "user@example.com";
String phone = "123-456-7890";
String zipCode = "12345";

// Email validation (basic pattern)
boolean isEmail = email.matches("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$");
System.out.println("Valid email: " + isEmail);
// Output: true

// Phone number (US format: XXX-XXX-XXXX)
boolean isPhone = phone.matches("\\d{3}-\\d{3}-\\d{4}");
System.out.println("Valid phone: " + isPhone);
// Output: true

// ZIP code (5 digits)
boolean isZip = zipCode.matches("\\d{5}");
System.out.println("Valid ZIP: " + isZip);
// Output: true

// Username (alphanumeric, 3-16 characters)
String username = "user_123";
boolean isValidUser = username.matches("^[a-zA-Z0-9_]{3,16}$");
System.out.println("Valid username: " + isValidUser);
// Output: true

// URL validation (simple)
String url = "https://example.com/page";
boolean isUrl = url.matches("^https?://[\\w.-]+\\.[a-z]{2,}.*$");
System.out.println("Valid URL: " + isUrl);
// Output: true
```

The matches() method checks if the entire string matches a regex pattern. Returns boolean. The ^ and $ anchors ensure the whole string matches (not just a substring). Patterns are case-sensitive unless using (?i) flag.

```java
// Advanced example - Precompiled patterns and detailed validation
import java.util.regex.*;

// Compile patterns once for reuse (better performance)
Pattern emailPattern = Pattern.compile(
    "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
);
Pattern phonePattern = Pattern.compile("^\\(?(\\d{3})\\)?[- ]?(\\d{3})[- ]?(\\d{4})$");
Pattern passwordPattern = Pattern.compile(
    "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[@$!%*?&])[A-Za-z\\d@$!%*?&]{8,}$"
);

// Validate email
String[] emails = {"user@example.com", "invalid.email", "test@domain.co.uk"};
for (String e : emails) {
    boolean valid = emailPattern.matcher(e).matches();
    System.out.println(e + " -> " + valid);
}
// Output:
// user@example.com -> true
// invalid.email -> false
// test@domain.co.uk -> true

// Validate phone (multiple formats)
String[] phones = {"123-456-7890", "(123) 456-7890", "1234567890"};
for (String p : phones) {
    Matcher m = phonePattern.matcher(p);
    if (m.matches()) {
        System.out.println(p + " -> Valid (Area: " + m.group(1) + ")");
    } else {
        System.out.println(p + " -> Invalid");
    }
}
// Output:
// 123-456-7890 -> Valid (Area: 123)
// (123) 456-7890 -> Valid (Area: 123)
// 1234567890 -> Invalid

// Strong password validation (min 8 chars, uppercase, lowercase, digit, special)
String[] passwords = {"Password123!", "weak", "NoDigit!", "nospecial123"};
for (String pwd : passwords) {
    boolean strong = passwordPattern.matcher(pwd).matches();
    System.out.println(pwd + " -> " + (strong ? "Strong" : "Weak"));
}
// Output:
// Password123! -> Strong
// weak -> Weak
// NoDigit! -> Weak
// nospecial123 -> Weak
```

**When to use**: Use matches() for one-time validation checks. Compile Pattern objects for repeated validation (better performance). For production email/URL validation, consider using dedicated libraries (Apache Commons Validator, Google libphonenumber) instead of regex for better accuracy and international support.

---

### Recipe 9: Format String with Values

**Problem**: Create formatted output with variables.

**Solution**:

```java
// Basic example - String.format() for simple formatting
String name = "Alice";
int age = 30;
double salary = 50000.50;

// Using String.format() with format specifiers
String formatted = String.format("Name: %s, Age: %d, Salary: $%.2f", name, age, salary);
System.out.println(formatted);
// Output: Name: Alice, Age: 30, Salary: $50000.50

// Format with alignment and padding
String aligned = String.format("|%-10s|%5d|%10.2f|", name, age, salary);
System.out.println(aligned);
// Output: |Alice     |   30|  50000.50|
// %-10s = left-align string, 10 chars wide
// %5d = right-align integer, 5 chars wide
// %10.2f = right-align float, 10 chars wide, 2 decimals

// Format numbers with thousands separator
int population = 1234567;
String withCommas = String.format("Population: %,d", population);
System.out.println(withCommas);
// Output: Population: 1,234,567

// Format dates (basic)
java.util.Date now = new java.util.Date();
String dateFormatted = String.format("Current date: %tF %tT", now, now);
System.out.println(dateFormatted);
// Output: Current date: 2025-12-18 14:30:45
```

String.format() uses printf-style format specifiers: %s (string), %d (integer), %f (float), %tF (date). Add modifiers for alignment (- for left), width (number), and precision (.2 for 2 decimals).

```java
// Advanced example - StringBuilder and Text Blocks (Java 15+)
import java.util.*;

// Using StringBuilder for dynamic string building (efficient in loops)
List<String> items = Arrays.asList("Apple", "Banana", "Cherry");
StringBuilder sb = new StringBuilder();
sb.append("Shopping List:\n");
for (int i = 0; i < items.size(); i++) {
    sb.append(String.format("%d. %s\n", i + 1, items.get(i)));
}
String list = sb.toString();
System.out.println(list);
// Output:
// Shopping List:
// 1. Apple
// 2. Banana
// 3. Cherry

// Using formatted() method (Java 15+)
String name = "Bob";
int age = 25;
String message = "Hello, %s! You are %d years old.".formatted(name, age);
System.out.println(message);
// Output: Hello, Bob! You are 25 years old.

// Text blocks with formatting (Java 15+)
String report = """
    Employee Report
    ===============
    Name:     %s
    Age:      %d
    Salary:   $%,.2f
    Status:   %s
    """.formatted(name, age, salary, "Active");
System.out.println(report);
// Output:
// Employee Report
// ===============
// Name:     Bob
// Age:      25
// Salary:   $50,000.50
// Status:   Active

// Format table with multiple rows
String header = String.format("%-15s | %10s | %10s", "Name", "Age", "Salary");
String separator = "-".repeat(42);
String row1 = String.format("%-15s | %10d | $%,9.2f", "Alice Johnson", 30, 50000.50);
String row2 = String.format("%-15s | %10d | $%,9.2f", "Bob Smith", 25, 45000.00);

String table = String.join("\n", header, separator, row1, row2);
System.out.println(table);
// Output:
// Name            |        Age |     Salary
// ------------------------------------------
// Alice Johnson   |         30 | $50,000.50
// Bob Smith       |         25 | $45,000.00
```

**When to use**: Use String.format() for one-time formatted output or when format is dynamic. Use StringBuilder for building strings in loops (avoids creating many intermediate String objects). Use text blocks (Java 15+) for multi-line templates. For high-performance formatting in tight loops, consider StringBuilder.append() directly instead of String.format().

---

## File and I/O Operations

### Recipe 10: Read File to String

**Problem**: Load entire file contents into a String.

**Solution**:

```java
import java.nio.file.*;
import java.nio.charset.StandardCharsets;

// Basic example - Read entire file (Java 11+)
Path filePath = Paths.get("data.txt");

// Simplest way - reads entire file as single string
String content = Files.readString(filePath);
System.out.println("Content: " + content);
// Output: [entire file contents]

// Read with explicit encoding (recommended for non-UTF-8 files)
String utf8Content = Files.readString(filePath, StandardCharsets.UTF_8);
System.out.println("UTF-8 Content: " + utf8Content);

// Read file as list of lines
List<String> lines = Files.readAllLines(filePath);
System.out.println("Line count: " + lines.size());
System.out.println("First line: " + lines.get(0));
// Output: Line count: 10, First line: [first line content]

// Read small file and join lines manually
String joinedContent = String.join("\n", lines);
System.out.println("Joined: " + joinedContent);
```

Files.readString() (Java 11+) is the simplest method for reading entire file contents. For older Java versions, use readAllLines() and join. Both methods read the entire file into memory, so only use for files under a few MB.

```java
// Advanced example - Read large files and handle errors
import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.io.*;
import java.util.stream.Stream;

try {
    // For large files - read line by line with Stream (memory efficient)
    Path largePath = Paths.get("large-log.txt");

    try (Stream<String> lineStream = Files.lines(largePath)) {
        // Process lines as stream (doesn't load entire file)
        long errorCount = lineStream
            .filter(line -> line.contains("ERROR"))
            .count();
        System.out.println("Error lines: " + errorCount);
    }
    // Stream is auto-closed after try-with-resources

    // Read with BufferedReader for more control
    try (BufferedReader reader = Files.newBufferedReader(filePath, StandardCharsets.UTF_8)) {
        String line;
        while ((line = reader.readLine()) != null) {
            if (line.startsWith("Important:")) {
                System.out.println("Found: " + line);
                break;  // Stop early if found
            }
        }
    }

    // Read binary file as bytes
    byte[] bytes = Files.readAllBytes(filePath);
    System.out.println("File size: " + bytes.length + " bytes");

} catch (IOException e) {
    System.err.println("Error reading file: " + e.getMessage());
    // Handle: file not found, permission denied, etc.
}
```

**When to use**: Use readString() for small text files (< 10 MB). Use Files.lines() with Stream for large files to avoid loading entire content into memory. Use readAllBytes() for binary files. Always handle IOException for file operations.

---

### Recipe 11: Write String to File

**Problem**: Save string content to a file.

**Solution**:

```java
import java.nio.file.*;
import java.nio.charset.StandardCharsets;

// Basic example - Write string to file (Java 11+)
String content = "Hello, World!\nThis is a test file.";
Path outputPath = Paths.get("output.txt");

// Simplest way - overwrites existing file
Files.writeString(outputPath, content);
System.out.println("File written: " + outputPath);

// Write with explicit encoding
Files.writeString(outputPath, content, StandardCharsets.UTF_8);

// Write multiple lines from list
List<String> lines = Arrays.asList("Line 1", "Line 2", "Line 3");
Files.write(outputPath, lines);
System.out.println("Lines written: " + lines.size());
// Output: File contains:
// Line 1
// Line 2
// Line 3

// Write bytes directly
byte[] data = "Binary content".getBytes(StandardCharsets.UTF_8);
Files.write(outputPath, data);
```

Files.writeString() (Java 11+) overwrites the file if it exists. For older Java, use Files.write() with byte array. Default encoding is UTF-8. File is created if it doesn't exist; parent directory must exist.

```java
// Advanced example - Append, create directories, handle options
import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.io.*;

try {
    Path logPath = Paths.get("logs/app.log");

    // Create parent directories if they don't exist
    Files.createDirectories(logPath.getParent());

    // Append to file (doesn't overwrite)
    String timestamp = java.time.LocalDateTime.now().toString();
    String logEntry = timestamp + " - Application started\n";

    Files.writeString(
        logPath,
        logEntry,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,      // Create if doesn't exist
        StandardOpenOption.APPEND       // Append instead of overwrite
    );
    System.out.println("Log entry appended");

    // Write with BufferedWriter for more control
    try (BufferedWriter writer = Files.newBufferedWriter(
            logPath,
            StandardCharsets.UTF_8,
            StandardOpenOption.CREATE,
            StandardOpenOption.APPEND)) {

        writer.write(timestamp + " - Processing data\n");
        writer.write(timestamp + " - Process complete\n");
        writer.flush();  // Ensure data is written
    }

    // Atomic write (write to temp, then move - prevents corruption)
    Path tempPath = Files.createTempFile("data", ".tmp");
    Files.writeString(tempPath, content);
    Files.move(tempPath, outputPath, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
    System.out.println("Atomic write completed");

} catch (IOException e) {
    System.err.println("Error writing file: " + e.getMessage());
}
```

**When to use**: Use writeString() for simple file writes. Use APPEND option for log files. Use BufferedWriter for writing many small pieces. Use atomic writes (write-temp-then-move) for critical data to prevent file corruption if write fails. Always create parent directories first.

---

### Recipe 12: List Files in Directory

**Problem**: Get all files from a directory.

**Solution**:

```java
import java.nio.file.*;
import java.io.IOException;

// Basic example - List files in directory
Path dirPath = Paths.get("src");

// List direct children (not recursive)
try (Stream<Path> paths = Files.list(dirPath)) {
    paths.filter(Files::isRegularFile)  // Only files, not directories
         .forEach(System.out::println);
}
// Output: src/Main.java, src/Utils.java, etc.

// List only directories
try (Stream<Path> paths = Files.list(dirPath)) {
    paths.filter(Files::isDirectory)
         .forEach(dir -> System.out.println("Dir: " + dir.getFileName()));
}

// Get file count
try (Stream<Path> paths = Files.list(dirPath)) {
    long fileCount = paths.filter(Files::isRegularFile).count();
    System.out.println("Files: " + fileCount);
}

// Collect to list for further processing
try (Stream<Path> paths = Files.list(dirPath)) {
    List<Path> fileList = paths.filter(Files::isRegularFile)
                               .collect(Collectors.toList());
    System.out.println("Found " + fileList.size() + " files");
}
```

Files.list() returns a Stream of paths in the directory (one level deep). Must close Stream with try-with-resources. Use filters to select files vs directories. Stream is lazy - processes files on demand.

```java
// Advanced example - Recursive traversal and filtering
import java.nio.file.*;
import java.io.IOException;
import java.util.stream.Collectors;

try {
    Path projectDir = Paths.get(".");

    // Recursively walk entire directory tree
    try (Stream<Path> paths = Files.walk(projectDir)) {
        List<Path> javaFiles = paths
            .filter(Files::isRegularFile)
            .filter(p -> p.toString().endsWith(".java"))
            .collect(Collectors.toList());

        System.out.println("Java files found: " + javaFiles.size());
        javaFiles.forEach(System.out::println);
    }

    // Walk with max depth (limit recursion)
    try (Stream<Path> paths = Files.walk(projectDir, 2)) {  // Max 2 levels deep
        long dirCount = paths.filter(Files::isDirectory).count();
        System.out.println("Directories (2 levels): " + dirCount);
    }

    // Find files matching pattern with glob
    PathMatcher matcher = FileSystems.getDefault().getPathMatcher("glob:**.{java,kt}");
    try (Stream<Path> paths = Files.walk(projectDir)) {
        List<Path> codeFiles = paths
            .filter(Files::isRegularFile)
            .filter(matcher::matches)
            .collect(Collectors.toList());
        System.out.println("Code files (.java, .kt): " + codeFiles.size());
    }

    // Get file sizes and sort by size
    try (Stream<Path> paths = Files.walk(projectDir)) {
        paths.filter(Files::isRegularFile)
             .sorted((p1, p2) -> {
                 try {
                     return Long.compare(Files.size(p2), Files.size(p1));  // Descending
                 } catch (IOException e) {
                     return 0;
                 }
             })
             .limit(10)  // Top 10 largest
             .forEach(p -> {
                 try {
                     System.out.println(p + " -> " + Files.size(p) + " bytes");
                 } catch (IOException e) {}
             });
    }

} catch (IOException e) {
    System.err.println("Error listing files: " + e.getMessage());
}
```

**When to use**: Use Files.list() for single-level directory listing. Use Files.walk() for recursive traversal of entire tree. Use PathMatcher with glob patterns for flexible file filtering (e.g., \*.java, \*\*.txt). Always close Stream with try-with-resources to prevent resource leaks.

---

### Recipe 13: Create Directory Tree

**Problem**: Ensure directory structure exists before operations.

**Solution**:

```java
import java.nio.file.*;

// Basic example - Create directories safely
Path singleDir = Paths.get("output");
Path nestedDirs = Paths.get("output/reports/2025");

// Create single directory (parent must exist)
try {
    if (!Files.exists(singleDir)) {
        Files.createDirectory(singleDir);
        System.out.println("Created: " + singleDir);
    } else {
        System.out.println("Already exists: " + singleDir);
    }
} catch (IOException e) {
    System.err.println("Error creating directory: " + e.getMessage());
}

// Create nested directories (creates all parents)
try {
    Files.createDirectories(nestedDirs);  // Safe - no error if exists
    System.out.println("Created directory tree: " + nestedDirs);
} catch (IOException e) {
    System.err.println("Error: " + e.getMessage());
}

// Verify directory exists
boolean exists = Files.exists(nestedDirs) && Files.isDirectory(nestedDirs);
System.out.println("Directory ready: " + exists);
// Output: true
```

Files.createDirectory() creates a single directory (parent must exist). Files.createDirectories() creates entire path including parents (safe to call even if exists). Both throw IOException on failure (permission denied, disk full, etc.).

```java
// Advanced example - Create with permissions and temporary directories
import java.nio.file.*;
import java.nio.file.attribute.*;
import java.io.IOException;

try {
    // Create directory with specific permissions (POSIX systems)
    Path secureDir = Paths.get("secure-data");

    Set<PosixFilePermission> perms = PosixFilePermissions.fromString("rwx------");
    FileAttribute<Set<PosixFilePermission>> attr = PosixFilePermissions.asFileAttribute(perms);

    if (!Files.exists(secureDir)) {
        Files.createDirectory(secureDir, attr);  // Owner read/write/execute only
        System.out.println("Created secure directory: " + secureDir);
    }

    // Create temporary directory
    Path tempDir = Files.createTempDirectory("app-temp-");
    System.out.println("Temp directory: " + tempDir);
    // Output: /tmp/app-temp-1234567890 (system temp location)

    // Create temp directory with prefix in specific location
    Path customTempDir = Files.createTempDirectory(
        Paths.get("output"),
        "process-"
    );
    System.out.println("Custom temp dir: " + customTempDir);
    // Output: output/process-9876543210

    // Create directory tree for organized file storage
    String year = "2025";
    String month = "12";
    Path archiveStructure = Paths.get("archives", year, month);

    Files.createDirectories(archiveStructure);
    System.out.println("Archive structure ready: " + archiveStructure);
    // Creates: archives/2025/12/

    // Verify and create atomically (check-then-create pattern)
    Path dataDir = Paths.get("data");
    synchronized (dataDir.toString().intern()) {  // Thread-safe
        if (!Files.exists(dataDir)) {
            Files.createDirectories(dataDir);
            System.out.println("Created data directory");
        }
    }

} catch (IOException e) {
    System.err.println("Directory creation failed: " + e.getMessage());
} catch (UnsupportedOperationException e) {
    // POSIX permissions not supported on Windows
    System.err.println("Permissions not supported on this OS");
}
```

**When to use**: Use createDirectories() in most cases (safe, creates parents, idempotent). Use createDirectory() only when you want to fail if parent doesn't exist. Use createTempDirectory() for temporary work spaces (auto-deleted on some systems). Set permissions on creation for security-sensitive directories.

## Dates and Times

### Recipe 14: Format Current Date/Time

**Problem**: Display date and time in readable format.

**Solution**:

```java
import java.time.*;
import java.time.format.DateTimeFormatter;

// Basic example - Format current date and time
LocalDateTime now = LocalDateTime.now();
LocalDate today = LocalDate.now();
LocalTime currentTime = LocalTime.now();

// ISO format (default toString())
System.out.println("DateTime: " + now);
// Output: 2025-12-18T14:30:45.123456

System.out.println("Date: " + today);
// Output: 2025-12-18

System.out.println("Time: " + currentTime);
// Output: 14:30:45.123456

// Custom formatting with DateTimeFormatter
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
String formatted = now.format(formatter);
System.out.println("Formatted: " + formatted);
// Output: 2025-12-18 14:30:45

// Common date formats
DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("MM/dd/yyyy");
String usDate = today.format(dateFormatter);
System.out.println("US format: " + usDate);
// Output: 12/18/2025

// Time only
DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss");
String timeOnly = currentTime.format(timeFormatter);
System.out.println("Time: " + timeOnly);
// Output: 14:30:45
```

DateTimeFormatter.ofPattern() creates formatters with custom patterns. Use yyyy for 4-digit year, MM for month, dd for day, HH for 24-hour, hh for 12-hour. The java.time API (Java 8+) is immutable and thread-safe.

```java
// Advanced example - Locale-specific and predefined formatters
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.util.Locale;

LocalDateTime now = LocalDateTime.now();

// Predefined formatters (ISO standards)
String iso = now.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
System.out.println("ISO: " + iso);
// Output: 2025-12-18T14:30:45.123

String isoDate = now.format(DateTimeFormatter.ISO_DATE);
// Output: 2025-12-18

// Locale-aware formatting
DateTimeFormatter usFormatter = DateTimeFormatter.ofPattern(
    "EEEE, MMMM d, yyyy 'at' h:mm a",
    Locale.US
);
String usFormat = now.format(usFormatter);
System.out.println("US: " + usFormat);
// Output: Wednesday, December 18, 2025 at 2:30 PM

// French locale
DateTimeFormatter frFormatter = DateTimeFormatter.ofPattern(
    "EEEE d MMMM yyyy 'à' HH'h'mm",
    Locale.FRANCE
);
String frFormat = now.format(frFormatter);
System.out.println("FR: " + frFormat);
// Output: mercredi 18 décembre 2025 à 14h30

// Using FormatStyle (localized predefined styles)
DateTimeFormatter shortFormatter = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.SHORT);
String shortFormat = now.format(shortFormatter.withLocale(Locale.US));
System.out.println("Short: " + shortFormat);
// Output: 12/18/25, 2:30 PM

DateTimeFormatter longFormatter = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.LONG);
String longFormat = now.format(longFormatter.withLocale(Locale.US));
System.out.println("Long: " + longFormat);
// Output: December 18, 2025, 2:30:45 PM PST

// Custom timestamp for logs
DateTimeFormatter logFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS");
String timestamp = now.format(logFormatter);
System.out.println("[" + timestamp + "] Application started");
// Output: [2025-12-18 14:30:45.123] Application started
```

**When to use**: Use ofPattern() for custom formats. Use predefined formatters (ISO\_\*) for standard formats. Use FormatStyle for locale-aware formatting. Choose LocalDate for dates only, LocalTime for times only, LocalDateTime for both. For timestamps with timezone, use ZonedDateTime instead.

---

### Recipe 15: Parse String to Date

**Problem**: Convert string to date object.

**Solution**:

```java
import java.time.*;
import java.time.format.DateTimeFormatter;

// Basic example - Parse standard ISO formats
String dateStr = "2025-12-18";
String timeStr = "14:30:45";
String dateTimeStr = "2025-12-18T14:30:45";

// Parse ISO format (default parsers)
LocalDate date = LocalDate.parse(dateStr);
System.out.println("Date: " + date);
// Output: 2025-12-18

LocalTime time = LocalTime.parse(timeStr);
System.out.println("Time: " + time);
// Output: 14:30:45

LocalDateTime dateTime = LocalDateTime.parse(dateTimeStr);
System.out.println("DateTime: " + dateTime);
// Output: 2025-12-18T14:30:45

// Perform date arithmetic after parsing
LocalDate tomorrow = date.plusDays(1);
System.out.println("Tomorrow: " + tomorrow);
// Output: 2025-12-19

LocalTime oneHourLater = time.plusHours(1);
System.out.println("One hour later: " + oneHourLater);
// Output: 15:30:45
```

LocalDate.parse(), LocalTime.parse(), and LocalDateTime.parse() use ISO-8601 format by default. Parsing is strict - invalid dates throw DateTimeParseException. Parsed objects are immutable.

```java
// Advanced example - Custom formats and error handling
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

// Parse custom date formats
DateTimeFormatter usFormat = DateTimeFormatter.ofPattern("MM/dd/yyyy");
String usDateStr = "12/18/2025";
LocalDate usDate = LocalDate.parse(usDateStr, usFormat);
System.out.println("US date: " + usDate);
// Output: 2025-12-18

// Parse European format
DateTimeFormatter euFormat = DateTimeFormatter.ofPattern("dd/MM/yyyy");
LocalDate euDate = LocalDate.parse("18/12/2025", euFormat);
System.out.println("EU date: " + euDate);
// Output: 2025-12-18

// Parse datetime with custom format
DateTimeFormatter customFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
LocalDateTime customDateTime = LocalDateTime.parse("2025-12-18 14:30:45", customFormat);
System.out.println("Custom: " + customDateTime);
// Output: 2025-12-18T14:30:45

// Parse with time zone
DateTimeFormatter zonedFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss z");
String zonedStr = "2025-12-18 14:30:45 PST";
ZonedDateTime zonedDateTime = ZonedDateTime.parse(zonedStr, zonedFormat);
System.out.println("Zoned: " + zonedDateTime);
// Output: 2025-12-18T14:30:45-08:00[America/Los_Angeles]

// Error handling for invalid dates
try {
    LocalDate invalid = LocalDate.parse("2025-13-45");  // Invalid month and day
} catch (DateTimeParseException e) {
    System.err.println("Parse error: " + e.getMessage());
    // Output: Parse error: Text '2025-13-45' could not be parsed...
}

// Parse lenient formats (handle variations)
String[] dateStrings = {
    "2025-12-18",
    "12/18/2025",
    "18-Dec-2025",
    "Dec 18, 2025"
};

DateTimeFormatter[] formatters = {
    DateTimeFormatter.ISO_LOCAL_DATE,
    DateTimeFormatter.ofPattern("MM/dd/yyyy"),
    DateTimeFormatter.ofPattern("dd-MMM-yyyy"),
    DateTimeFormatter.ofPattern("MMM dd, yyyy")
};

for (int i = 0; i < dateStrings.length; i++) {
    try {
        LocalDate parsed = LocalDate.parse(dateStrings[i], formatters[i]);
        System.out.println(dateStrings[i] + " -> " + parsed);
    } catch (DateTimeParseException e) {
        System.err.println("Failed to parse: " + dateStrings[i]);
    }
}
```

**When to use**: Use parse() with default formatters for ISO-8601 strings. Use custom DateTimeFormatter for non-standard formats. Always handle DateTimeParseException when parsing user input. For lenient parsing (accepting multiple formats), try multiple formatters in sequence.

---

### Recipe 16: Calculate Time Differences

**Problem**: Find duration between two dates or times.

**Solution**:

```java
import java.time.*;

// Basic example - Calculate time differences
LocalDateTime start = LocalDateTime.of(2025, 12, 18, 10, 0, 0);
LocalDateTime end = LocalDateTime.of(2025, 12, 18, 15, 30, 45);

// Duration for time-based differences (hours, minutes, seconds)
Duration duration = Duration.between(start, end);

System.out.println("Hours: " + duration.toHours());
// Output: 5

System.out.println("Minutes: " + duration.toMinutes());
// Output: 330

System.out.println("Seconds: " + duration.getSeconds());
// Output: 19845

// Get duration in human-readable parts
long hours = duration.toHours();
long minutes = duration.toMinutesPart();  // Minutes part (0-59)
long seconds = duration.toSecondsPart();  // Seconds part (0-59)
System.out.println("Duration: " + hours + "h " + minutes + "m " + seconds + "s");
// Output: Duration: 5h 30m 45s

// Check if duration is negative
boolean isNegative = duration.isNegative();
System.out.println("Is negative: " + isNegative);
// Output: false
```

Duration measures time-based amounts (hours, minutes, seconds, nanoseconds). Use between() for LocalDateTime, LocalTime, or Instant. Duration is precise for time differences.

```java
// Advanced example - Period for date differences and complex calculations
import java.time.*;
import java.time.temporal.ChronoUnit;

// Period for date-based differences (years, months, days)
LocalDate startDate = LocalDate.of(2020, 1, 15);
LocalDate endDate = LocalDate.of(2025, 12, 18);

Period period = Period.between(startDate, endDate);
System.out.println("Years: " + period.getYears());
// Output: 5

System.out.println("Months: " + period.getMonths());
// Output: 11

System.out.println("Days: " + period.getDays());
// Output: 3

// Human-readable period
System.out.println("Period: " + period.getYears() + " years, " +
                   period.getMonths() + " months, " +
                   period.getDays() + " days");
// Output: Period: 5 years, 11 months, 3 days

// Total days between dates (using ChronoUnit)
long totalDays = ChronoUnit.DAYS.between(startDate, endDate);
System.out.println("Total days: " + totalDays);
// Output: 2164

// Other ChronoUnit calculations
long totalMonths = ChronoUnit.MONTHS.between(startDate, endDate);
long totalWeeks = ChronoUnit.WEEKS.between(startDate, endDate);
System.out.println("Total months: " + totalMonths);  // 71
System.out.println("Total weeks: " + totalWeeks);    // 309

// Calculate age
LocalDate birthDate = LocalDate.of(1990, 5, 20);
LocalDate today = LocalDate.now();
int age = Period.between(birthDate, today).getYears();
System.out.println("Age: " + age);
// Output: 35 (if today is 2025-12-18)

// Duration arithmetic
Duration oneHour = Duration.ofHours(1);
Duration thirtyMinutes = Duration.ofMinutes(30);
Duration total = oneHour.plus(thirtyMinutes);
System.out.println("Total: " + total.toMinutes() + " minutes");
// Output: 90 minutes

// Time until future event
LocalDateTime eventTime = LocalDateTime.of(2025, 12, 31, 23, 59, 59);
LocalDateTime now = LocalDateTime.now();
Duration timeUntil = Duration.between(now, eventTime);
long daysUntil = timeUntil.toDays();
long hoursUntil = timeUntil.toHoursPart();
System.out.println("Time until event: " + daysUntil + " days, " + hoursUntil + " hours");
```

**When to use**: Use Duration for time-based differences (hours, minutes, seconds) between LocalDateTime or LocalTime. Use Period for date-based differences (years, months, days) between LocalDate. Use ChronoUnit.between() for total units (total days, total months). For age calculation, use Period.getYears(). Duration and Period are immutable and chainable.

---

## Exception Handling

### Recipe 17: Try-Catch with Resource Cleanup

**Problem**: Ensure resources are properly closed even if error occurs.

**Solution**:

```java
import java.io.*;
import java.nio.file.*;

// Basic example - Try-with-resources (Java 7+)
Path filePath = Paths.get("data.txt");

// Single resource - automatically closed after try block
try (BufferedReader reader = Files.newBufferedReader(filePath)) {
    String line;
    while ((line = reader.readLine()) != null) {
        System.out.println("Line: " + line);
    }
    // Reader is automatically closed here, even if exception occurs
} catch (IOException e) {
    System.err.println("Error reading file: " + e.getMessage());
    // Output: Error reading file: data.txt (No such file or directory)
}

// Multiple resources - closed in reverse order
try (
    FileInputStream input = new FileInputStream("source.txt");
    FileOutputStream output = new FileOutputStream("destination.txt")
) {
    byte[] buffer = new byte[1024];
    int bytesRead;
    while ((bytesRead = input.read(buffer)) != -1) {
        output.write(buffer, 0, bytesRead);
    }
    System.out.println("File copied successfully");
    // Output closes first, then input (reverse order)
} catch (IOException e) {
    System.err.println("Copy failed: " + e.getMessage());
}
```

Try-with-resources automatically calls close() on all resources when exiting the try block (normal or exception). Resources must implement AutoCloseable. Multiple resources are closed in reverse order of declaration.

```java
// Advanced example - Custom resources and complex cleanup
import java.io.*;

// Custom resource with AutoCloseable
class DatabaseConnection implements AutoCloseable {
    private String connectionId;

    public DatabaseConnection(String id) throws IOException {
        this.connectionId = id;
        System.out.println("Opening connection: " + id);
        if (id.equals("fail")) {
            throw new IOException("Connection failed");
        }
    }

    public void executeQuery(String sql) {
        System.out.println("[" + connectionId + "] Executing: " + sql);
    }

    @Override
    public void close() {
        System.out.println("Closing connection: " + connectionId);
    }
}

// Usage with custom resource
try (DatabaseConnection conn = new DatabaseConnection("db-001")) {
    conn.executeQuery("SELECT * FROM users");
    // close() called automatically
} catch (IOException e) {
    System.err.println("Database error: " + e.getMessage());
}
// Output:
// Opening connection: db-001
// [db-001] Executing: SELECT * FROM users
// Closing connection: db-001

// Multiple custom resources with error handling
try (
    DatabaseConnection primary = new DatabaseConnection("primary");
    DatabaseConnection backup = new DatabaseConnection("backup")
) {
    primary.executeQuery("INSERT INTO logs VALUES ('event')");
    backup.executeQuery("INSERT INTO logs VALUES ('event')");
} catch (IOException e) {
    System.err.println("Operation failed: " + e.getMessage());
    // Both connections closed even if exception occurs
}

// Suppressed exceptions example
try (DatabaseConnection conn = new DatabaseConnection("test")) {
    throw new RuntimeException("Main exception during processing");
} catch (Exception e) {
    System.out.println("Main exception: " + e.getMessage());
    Throwable[] suppressed = e.getSuppressed();
    if (suppressed.length > 0) {
        System.out.println("Suppressed: " + suppressed[0].getMessage());
    }
}
```

**When to use**: Always use try-with-resources for AutoCloseable resources (files, database connections, network sockets). It prevents resource leaks and reduces boilerplate. Use multiple resources in one try for coordinated cleanup. Avoid manual finally blocks for resource cleanup.

---

### Recipe 18: Chain Exceptions

**Problem**: Preserve original exception while throwing new one.

**Solution**:

```java
// Basic example - Exception wrapping and chaining
public class DataProcessor {

    public void processFile(String filename) throws ProcessingException {
        try {
            String data = readFile(filename);
            int result = Integer.parseInt(data);
            System.out.println("Processed: " + result);
        } catch (IOException e) {
            // Wrap IOException in domain exception, preserve original
            throw new ProcessingException("Failed to process: " + filename, e);
        } catch (NumberFormatException e) {
            // Wrap parsing error
            throw new ProcessingException("Invalid format in: " + filename, e);
        }
    }

    private String readFile(String filename) throws IOException {
        throw new IOException("File not found: " + filename);
    }
}

// Custom exception with chaining
class ProcessingException extends Exception {
    public ProcessingException(String message, Throwable cause) {
        super(message, cause);  // Preserve original exception
    }
}

// Usage - access exception chain
try {
    new DataProcessor().processFile("data.txt");
} catch (ProcessingException e) {
    System.err.println("Error: " + e.getMessage());
    // Output: Error: Failed to process: data.txt

    System.err.println("Root cause: " + e.getCause().getMessage());
    // Output: Root cause: File not found: data.txt
}
```

Exception chaining preserves original error context while wrapping in domain-specific exceptions. Use constructor with Throwable cause parameter. Access original via getCause(). Maintains full stack trace for debugging.

```java
// Advanced example - Multi-level chaining and inspection
import java.sql.*;

// Three-level exception hierarchy
class ServiceException extends Exception {
    public ServiceException(String msg, Throwable cause) { super(msg, cause); }
}

class DatabaseException extends Exception {
    public DatabaseException(String msg, Throwable cause) { super(msg, cause); }
}

class UserService {
    public void createUser(String name) throws ServiceException {
        try {
            saveToDatabase(name);
        } catch (DatabaseException e) {
            throw new ServiceException("User creation failed for: " + name, e);
        }
    }

    private void saveToDatabase(String name) throws DatabaseException {
        try {
            executeSQL("INSERT INTO users VALUES ('" + name + "')");
        } catch (SQLException e) {
            throw new DatabaseException("Database save failed", e);
        }
    }

    private void executeSQL(String sql) throws SQLException {
        throw new SQLException("Connection timeout", "08001", 1234);
    }
}

// Inspect full exception chain
try {
    new UserService().createUser("Alice");
} catch (ServiceException e) {
    System.out.println("=== Exception Chain ===");

    Throwable current = e;
    int level = 0;
    while (current != null) {
        System.out.println("Level " + level + ": " +
                          current.getClass().getSimpleName() +
                          " - " + current.getMessage());

        // Special handling for SQLException
        if (current instanceof SQLException) {
            SQLException sql = (SQLException) current;
            System.out.println("  SQL State: " + sql.getSQLState());
            System.out.println("  Error Code: " + sql.getErrorCode());
        }

        current = current.getCause();
        level++;
    }
    // Output:
    // === Exception Chain ===
    // Level 0: ServiceException - User creation failed for: Alice
    // Level 1: DatabaseException - Database save failed
    // Level 2: SQLException - Connection timeout
    //   SQL State: 08001
    //   Error Code: 1234

    // Print full stack trace
    e.printStackTrace();
}

// Add custom context to exception chain
class ContextualException extends Exception {
    private Map<String, Object> context = new HashMap<>();

    public ContextualException(String message, Throwable cause) {
        super(message, cause);
    }

    public void addContext(String key, Object value) {
        context.put(key, value);
    }

    public Map<String, Object> getContext() {
        return context;
    }
}
```

**When to use**: Wrap low-level exceptions (IOException, SQLException) in domain-specific exceptions for better error messages. Always pass original exception as cause to preserve debugging information. Use getCause() to traverse exception chain. For multi-tier systems, create exception hierarchies with chaining at each tier. Add context information to exceptions when wrapping.

---

---

## Testing

### Recipe 19: Unit Test Template

**Problem**: Write a basic test class.

**Solution**:

```java
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import static org.junit.jupiter.api.Assertions.*;

// Basic example - JUnit 5 test structure
public class CalculatorTest {
    private Calculator calculator;

    @BeforeEach
    public void setUp() {
        // Runs before each test method
        calculator = new Calculator();
        System.out.println("Test setup complete");
    }

    @Test
    public void testAddition() {
        int result = calculator.add(2, 3);
        assertEquals(5, result);  // Expected, actual
        // Test passes if result equals 5
    }

    @Test
    public void testSubtraction() {
        assertEquals(2, calculator.subtract(5, 3));
        assertEquals(-1, calculator.subtract(3, 4));
    }

    @Test
    public void testDivisionByZeroThrows() {
        // Test that exception is thrown
        assertThrows(ArithmeticException.class, () -> {
            calculator.divide(10, 0);
        });
        // Test passes if ArithmeticException is thrown
    }

    @AfterEach
    public void tearDown() {
        // Runs after each test method
        calculator = null;
        System.out.println("Test cleanup complete");
    }
}
```

JUnit 5 uses @Test for test methods, @BeforeEach for setup, @AfterEach for cleanup. Use assertEquals(expected, actual) for value checks, assertThrows for exception testing. Each test method runs independently with fresh setup.

```java
// Advanced example - Multiple assertion types and test organization
import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.*;
import java.time.Duration;
import java.util.*;

@DisplayName("String Utility Tests")
public class StringUtilsTest {

    @BeforeAll
    static void setupClass() {
        // Runs once before all tests
        System.out.println("Starting string utility tests");
    }

    @Test
    @DisplayName("Should concatenate strings correctly")
    public void testConcatenation() {
        String result = StringUtils.concat("Hello", "World");
        assertEquals("HelloWorld", result);
        assertNotNull(result);
        assertTrue(result.length() > 0);
    }

    @Test
    public void testMultipleAssertions() {
        String input = "Java";

        // All assertions executed even if one fails
        assertAll("String properties",
            () -> assertEquals(4, input.length()),
            () -> assertTrue(input.startsWith("J")),
            () -> assertFalse(input.isEmpty()),
            () -> assertEquals("JAVA", input.toUpperCase())
        );
    }

    @Test
    public void testArrayEquality() {
        int[] expected = {1, 2, 3};
        int[] actual = {1, 2, 3};
        assertArrayEquals(expected, actual);
    }

    @Test
    public void testPerformance() {
        // Test completes within time limit
        assertTimeout(Duration.ofMillis(100), () -> {
            Thread.sleep(50);  // Simulated work
        });
    }

    @Test
    @Disabled("Feature not implemented yet")
    public void testFutureFeature() {
        // Test is skipped
    }

    @Nested
    @DisplayName("Null handling tests")
    class NullTests {
        @Test
        void shouldHandleNullInput() {
            assertThrows(NullPointerException.class, () -> {
                StringUtils.concat(null, "test");
            });
        }

        @Test
        void shouldReturnEmptyForNulls() {
            String result = StringUtils.safeConcat(null, null);
            assertNotNull(result);
            assertTrue(result.isEmpty());
        }
    }

    @AfterAll
    static void tearDownClass() {
        // Runs once after all tests
        System.out.println("All tests complete");
    }
}
```

**When to use**: Use @BeforeEach for test setup, @AfterEach for cleanup. Use @BeforeAll/@AfterAll for expensive one-time setup. Group related assertions with assertAll(). Use @Nested for organizing related tests. Use @DisplayName for readable test names. Use assertTimeout() for performance tests.

---

### Recipe 20: Test with Parameterized Data

**Problem**: Run same test with multiple inputs.

**Solution**:

```java
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.*;
import static org.junit.jupiter.api.Assertions.*;

// Basic example - ValueSource for simple inputs
public class MathTest {

    @ParameterizedTest
    @ValueSource(ints = {1, 3, 5, 7, 9})
    public void testOddNumbers(int number) {
        assertTrue(isOdd(number), number + " should be odd");
        // Test runs 5 times with different values
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "  ", "\t", "\n"})
    public void testBlankStrings(String input) {
        assertTrue(input.isBlank());
    }

    @ParameterizedTest
    @CsvSource({
        "2, 4, 6",     // 2 + 4 = 6
        "3, 5, 8",     // 3 + 5 = 8
        "0, 0, 0",     // 0 + 0 = 0
        "-1, 1, 0"     // -1 + 1 = 0
    })
    public void testAddition(int a, int b, int expected) {
        assertEquals(expected, a + b);
        // Test runs 4 times with CSV data
    }

    private boolean isOdd(int n) {
        return n % 2 != 0;
    }
}
```

Parameterized tests run the same test method multiple times with different inputs. Use @ValueSource for single values, @CsvSource for multiple parameters. Test method parameters match the provided values.

```java
// Advanced example - Multiple source types and custom providers
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.*;
import java.util.stream.Stream;

public class AdvancedParameterizedTest {

    // CSV file source
    @ParameterizedTest
    @CsvFileSource(resources = "/test-data.csv", numLinesToSkip = 1)
    public void testFromCsvFile(String input, int expected) {
        assertEquals(expected, input.length());
    }

    // Enum source
    enum Status { ACTIVE, INACTIVE, PENDING }

    @ParameterizedTest
    @EnumSource(Status.class)
    public void testAllStatuses(Status status) {
        assertNotNull(status.name());
    }

    // Method source - custom data
    @ParameterizedTest
    @MethodSource("provideStringsForTest")
    public void testStrings(String input, int expectedLength) {
        assertEquals(expectedLength, input.length());
    }

    static Stream<Arguments> provideStringsForTest() {
        return Stream.of(
            Arguments.of("Hello", 5),
            Arguments.of("Java", 4),
            Arguments.of("", 0),
            Arguments.of("Testing", 7)
        );
    }

    // Custom argument converter
    @ParameterizedTest
    @CsvSource({"ACTIVE,true", "INACTIVE,false", "PENDING,false"})
    public void testStatusConversion(Status status, boolean expected) {
        assertEquals(expected, status == Status.ACTIVE);
    }

    // Null and empty sources
    @ParameterizedTest
    @NullSource
    @EmptySource
    @ValueSource(strings = {" ", "\t"})
    public void testNullAndEmpty(String input) {
        assertTrue(input == null || input.isBlank());
    }

    // Arguments aggregation
    @ParameterizedTest
    @CsvSource({
        "Alice, 30, alice@example.com",
        "Bob, 25, bob@example.com"
    })
    public void testUserData(
        @AggregateWith(UserAggregator.class) User user
    ) {
        assertNotNull(user.name);
        assertTrue(user.age > 0);
        assertTrue(user.email.contains("@"));
    }

    // Custom aggregator
    static class UserAggregator implements ArgumentsAggregator {
        @Override
        public User aggregateArguments(
            ArgumentsAccessor accessor,
            ParameterContext context
        ) {
            return new User(
                accessor.getString(0),
                accessor.getInteger(1),
                accessor.getString(2)
            );
        }
    }

    static class User {
        String name;
        int age;
        String email;

        User(String name, int age, String email) {
            this.name = name;
            this.age = age;
            this.email = email;
        }
    }

    // Display name customization
    @ParameterizedTest(name = "#{index} - Test with {0}")
    @ValueSource(ints = {1, 2, 3, 4, 5})
    public void testWithCustomName(int number) {
        assertTrue(number > 0);
    }
    // Output: #1 - Test with 1, #2 - Test with 2, etc.
}
```

**When to use**: Use @ValueSource for testing with multiple simple values. Use @CsvSource for multiple parameters per test. Use @MethodSource for complex or programmatic test data. Use @NullSource/@EmptySource for edge case testing. Parameterized tests reduce code duplication and improve test coverage. Use custom display names for readable test output.

---

## Concurrency

### Recipe 21: Run Code in Background Thread

**Problem**: Execute long-running operation without blocking main thread.

**Solution**:

```java
import java.util.concurrent.*;

// Basic example - Simple background thread execution
Thread thread = new Thread(() -> {
    System.out.println("Background task started");
    try {
        Thread.sleep(2000);  // Simulate work
    } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        System.err.println("Task interrupted");
    }
    System.out.println("Background task complete");
});
thread.start();

System.out.println("Main thread continues...");
// Output:
// Main thread continues...
// Background task started
// Background task complete (after 2 seconds)

// Using Runnable interface
Runnable task = () -> {
    for (int i = 0; i < 3; i++) {
        System.out.println("Working... " + i);
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
};
new Thread(task).start();
```

Background threads execute concurrently with the main thread. Use Thread with Runnable or lambda for simple tasks. Always handle InterruptedException properly by calling interrupt().

```java
// Advanced example - ExecutorService for managed threading
import java.util.concurrent.*;

// Single thread executor
ExecutorService executor = Executors.newSingleThreadExecutor();

Future<String> future = executor.submit(() -> {
    Thread.sleep(1000);
    return "Task result";
});

System.out.println("Task submitted, doing other work...");

try {
    String result = future.get(2, TimeUnit.SECONDS);
    System.out.println("Result: " + result);
} catch (TimeoutException e) {
    System.err.println("Task timed out");
    future.cancel(true);
} catch (ExecutionException | InterruptedException e) {
    System.err.println("Task failed: " + e.getCause());
}

executor.shutdown();

// Cached thread pool (creates threads as needed)
ExecutorService cached = Executors.newCachedThreadPool();
for (int i = 0; i < 5; i++) {
    final int taskId = i;
    cached.submit(() -> {
        System.out.println("Task " + taskId + " on " + Thread.currentThread().getName());
    });
}
cached.shutdown();

// Scheduled executor (delayed/periodic tasks)
ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);

scheduler.schedule(() -> {
    System.out.println("Delayed task executed");
}, 2, TimeUnit.SECONDS);

ScheduledFuture<?> periodic = scheduler.scheduleAtFixedRate(() -> {
    System.out.println("Periodic task at " + System.currentTimeMillis());
}, 0, 1, TimeUnit.SECONDS);

Thread.sleep(3000);
periodic.cancel(false);
scheduler.shutdown();
```

**When to use**: Use ExecutorService for better resource management. Use newSingleThreadExecutor() for sequential tasks. Use newCachedThreadPool() for many short-lived tasks. Use ScheduledExecutorService for delayed or periodic tasks. Always call shutdown().

---

### Recipe 22: Wait for Multiple Threads

**Problem**: Start several threads and wait for all to finish.

**Solution**:

```java
import java.util.concurrent.*;

// Basic example - Wait for all tasks
ExecutorService executor = Executors.newFixedThreadPool(3);

for (int i = 0; i < 5; i++) {
    final int taskId = i;
    executor.submit(() -> {
        System.out.println("Task " + taskId + " started on " + Thread.currentThread().getName());
        try {
            Thread.sleep(1000 + (taskId * 200));
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        System.out.println("Task " + taskId + " finished");
    });
}

executor.shutdown();
boolean completed = executor.awaitTermination(10, TimeUnit.SECONDS);

if (completed) {
    System.out.println("All tasks completed");
} else {
    System.out.println("Timeout");
    executor.shutdownNow();
}
```

ExecutorService.shutdown() prevents new tasks. awaitTermination() blocks until all complete or timeout. Returns true if finished, false if timeout. Use shutdownNow() to force interrupt.

```java
// Advanced example - Collect results
import java.util.*;
import java.util.concurrent.*;

ExecutorService executor = Executors.newFixedThreadPool(4);

// Using Future list
List<Future<Integer>> futures = new ArrayList<>();
for (int i = 0; i < 5; i++) {
    final int taskId = i;
    Future<Integer> future = executor.submit(() -> {
        Thread.sleep(500);
        return taskId * taskId;
    });
    futures.add(future);
}

List<Integer> results = new ArrayList<>();
for (Future<Integer> future : futures) {
    try {
        results.add(future.get());
    } catch (ExecutionException | InterruptedException e) {
        System.err.println("Task failed: " + e.getCause());
    }
}
System.out.println("Results: " + results);
// Output: [0, 1, 4, 9, 16]

// Using invokeAll
List<Callable<String>> tasks = Arrays.asList(
    () -> { Thread.sleep(500); return "Task 1"; },
    () -> { Thread.sleep(300); return "Task 2"; }
);

List<Future<String>> allFutures = executor.invokeAll(tasks);
for (Future<String> f : allFutures) {
    try {
        System.out.println("Result: " + f.get());
    } catch (ExecutionException | InterruptedException e) {
        e.printStackTrace();
    }
}

// Using CountDownLatch
CountDownLatch latch = new CountDownLatch(3);

for (int i = 0; i < 3; i++) {
    final int id = i;
    executor.submit(() -> {
        try {
            System.out.println("Task " + id + " working");
            Thread.sleep(1000);
        } finally {
            latch.countDown();
        }
    });
}

latch.await();
System.out.println("All tasks completed (latch)");

executor.shutdown();
executor.awaitTermination(5, TimeUnit.SECONDS);
```

**When to use**: Use awaitTermination() for simple waiting. Use Future.get() to collect results. Use invokeAll() for fixed Callable lists. Use CountDownLatch for custom coordination. Always shutdown executor.

---

---

## Advanced Concurrency Patterns

### Recipe 23: CompletableFuture for Async Operations

**Problem**: Need to chain asynchronous operations without blocking.

**Solution**:

```java
import java.util.concurrent.*;

// Example 1: Basic CompletableFuture with thenApply/thenAccept
public class AsyncExample {
    public static void main(String[] args) {
        // Start async operation
        CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> {
            // Simulate API call or I/O operation
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            return "User data loaded";
        });

        // Chain transformations without blocking
        future
            .thenApply(data -> data.toUpperCase())  // Transform result
            .thenApply(upper -> "Processed: " + upper)
            .thenAccept(System.out::println)  // Consume result
            .join();  // Wait for completion

        // Output: Processed: USER DATA LOADED
    }
}
```

Basic async operations allow you to execute code in the background and chain transformations declaratively.

```java
import java.util.concurrent.*;

// Example 2: Combining multiple futures with thenCombine/allOf
public class CombinedAsyncExample {
    public static void main(String[] args) {
        // Start multiple async operations
        CompletableFuture<String> userFuture = CompletableFuture.supplyAsync(() -> {
            sleep(1000);
            return "Alice";
        });

        CompletableFuture<Integer> ageFuture = CompletableFuture.supplyAsync(() -> {
            sleep(800);
            return 30;
        });

        // Combine results from both futures
        CompletableFuture<String> combined = userFuture.thenCombine(ageFuture,
            (name, age) -> String.format("%s is %d years old", name, age));

        System.out.println(combined.join());  // Alice is 30 years old

        // Wait for ALL futures (parallel execution)
        CompletableFuture<Void> all = CompletableFuture.allOf(userFuture, ageFuture);
        all.join();
        System.out.println("All operations completed");
    }

    private static void sleep(int ms) {
        try { Thread.sleep(ms); } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
```

**When to use**: Async I/O operations like HTTP requests, database queries, or file operations where you want to avoid blocking threads and maximize throughput.

### Recipe 24: Parallel Streams

**Problem**: Process large collections using all available CPU cores.

**Solution**:

```java
import java.util.*;
import java.util.stream.*;

// Example 1: parallelStream() for filtering and mapping
public class ParallelStreamExample {
    public static void main(String[] args) {
        List<Integer> numbers = IntStream.rangeClosed(1, 1000)
            .boxed()
            .collect(Collectors.toList());

        // Sequential processing (uses one core)
        long start = System.currentTimeMillis();
        List<Integer> sequential = numbers.stream()
            .filter(n -> isPrime(n))
            .collect(Collectors.toList());
        System.out.println("Sequential: " + (System.currentTimeMillis() - start) + "ms");

        // Parallel processing (uses all cores)
        start = System.currentTimeMillis();
        List<Integer> parallel = numbers.parallelStream()
            .filter(n -> isPrime(n))
            .collect(Collectors.toList());
        System.out.println("Parallel: " + (System.currentTimeMillis() - start) + "ms");

        System.out.println("Found " + parallel.size() + " primes");
    }

    private static boolean isPrime(int n) {
        if (n <= 1) return false;
        for (int i = 2; i <= Math.sqrt(n); i++) {
            if (n % i == 0) return false;
        }
        return true;
    }
}
```

Parallel streams automatically distribute work across CPU cores, ideal for CPU-intensive operations on large datasets.

```java
import java.util.concurrent.*;
import java.util.stream.*;

// Example 2: parallel() with custom ForkJoinPool
public class CustomPoolExample {
    public static void main(String[] args) throws Exception {
        List<Integer> numbers = IntStream.rangeClosed(1, 100)
            .boxed()
            .collect(Collectors.toList());

        // Default parallel stream uses common ForkJoinPool
        // To control thread count, use custom pool
        ForkJoinPool customPool = new ForkJoinPool(4);  // 4 threads

        List<Integer> result = customPool.submit(() ->
            numbers.parallelStream()
                .map(n -> {
                    System.out.println("Processing " + n +
                        " on " + Thread.currentThread().getName());
                    return n * n;  // Square each number
                })
                .collect(Collectors.toList())
        ).get();

        customPool.shutdown();
        System.out.println("Processed " + result.size() + " items");
    }
}
```

**When to use**: CPU-intensive operations on large datasets (filtering, mapping, reducing) where the overhead of parallelization is justified. Avoid for small collections or I/O-bound tasks.

### Recipe 25: Producer-Consumer with BlockingQueue

**Problem**: Thread-safe work queue for multiple producers and consumers.

**Solution**:

```java
import java.util.concurrent.*;

// Example 1: ArrayBlockingQueue with multiple threads
public class ProducerConsumerExample {
    public static void main(String[] args) {
        BlockingQueue<String> queue = new ArrayBlockingQueue<>(10);

        // Producer thread
        Thread producer = new Thread(() -> {
            try {
                for (int i = 0; i < 20; i++) {
                    String item = "Item-" + i;
                    queue.put(item);  // Blocks if queue is full
                    System.out.println("Produced: " + item);
                    Thread.sleep(100);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        // Consumer thread
        Thread consumer = new Thread(() -> {
            try {
                for (int i = 0; i < 20; i++) {
                    String item = queue.take();  // Blocks if queue is empty
                    System.out.println("Consumed: " + item);
                    Thread.sleep(200);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        producer.start();
        consumer.start();
    }
}
```

BlockingQueue handles thread synchronization automatically - producers wait when full, consumers wait when empty.

```java
import java.util.concurrent.*;

// Example 2: LinkedBlockingQueue with shutdown mechanism
public class GracefulShutdownExample {
    private static final String POISON_PILL = "SHUTDOWN";

    public static void main(String[] args) throws InterruptedException {
        BlockingQueue<String> queue = new LinkedBlockingQueue<>();
        ExecutorService executor = Executors.newFixedThreadPool(3);

        // Start 2 producers
        for (int p = 0; p < 2; p++) {
            final int producerId = p;
            executor.submit(() -> {
                try {
                    for (int i = 0; i < 5; i++) {
                        queue.put("P" + producerId + "-Item" + i);
                        Thread.sleep(100);
                    }
                    queue.put(POISON_PILL);  // Signal shutdown
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            });
        }

        // Consumer processes until poison pill
        int poisonCount = 0;
        while (poisonCount < 2) {
            String item = queue.take();
            if (item.equals(POISON_PILL)) {
                poisonCount++;
                System.out.println("Received shutdown signal");
            } else {
                System.out.println("Processing: " + item);
            }
        }

        executor.shutdown();
        System.out.println("Graceful shutdown complete");
    }
}
```

**When to use**: Decoupling producers from consumers in multi-threaded applications, task queues, event processing systems, or any scenario where work generation and processing happen at different rates.

### Recipe 26: Thread-Safe Collections

**Problem**: Share collections between threads safely without manual synchronization.

**Solution**:

```java
import java.util.concurrent.*;
import java.util.*;

// Example 1: ConcurrentHashMap basic operations
public class ConcurrentMapExample {
    public static void main(String[] args) throws InterruptedException {
        ConcurrentHashMap<String, Integer> map = new ConcurrentHashMap<>();

        // Multiple threads updating the map
        ExecutorService executor = Executors.newFixedThreadPool(3);

        for (int i = 0; i < 3; i++) {
            final int threadId = i;
            executor.submit(() -> {
                for (int j = 0; j < 100; j++) {
                    String key = "key-" + (j % 10);
                    // Atomic operations - no race conditions
                    map.merge(key, 1, Integer::sum);  // Increment count
                }
                System.out.println("Thread " + threadId + " completed");
            });
        }

        executor.shutdown();
        executor.awaitTermination(10, TimeUnit.SECONDS);

        // All 300 increments are accounted for
        map.forEach((key, value) ->
            System.out.println(key + ": " + value));
        // Output: key-0: 30, key-1: 30, ... (each key has exactly 30)
    }
}
```

ConcurrentHashMap provides thread-safe operations without locking the entire map, allowing high-performance concurrent access.

```java
import java.util.concurrent.*;
import java.util.*;

// Example 2: CopyOnWriteArrayList for read-heavy workloads
public class CopyOnWriteExample {
    public static void main(String[] args) throws InterruptedException {
        // Perfect for read-heavy scenarios (many readers, few writers)
        CopyOnWriteArrayList<String> list = new CopyOnWriteArrayList<>();

        // Writer thread (infrequent)
        Thread writer = new Thread(() -> {
            for (int i = 0; i < 5; i++) {
                list.add("Item-" + i);
                System.out.println("Added: Item-" + i);
                try { Thread.sleep(500); } catch (InterruptedException e) {}
            }
        });

        // Multiple reader threads (frequent)
        Thread reader1 = new Thread(() -> {
            for (int i = 0; i < 10; i++) {
                // Safe iteration - no ConcurrentModificationException
                for (String item : list) {
                    System.out.println("Reader1: " + item);
                }
                try { Thread.sleep(200); } catch (InterruptedException e) {}
            }
        });

        Thread reader2 = new Thread(() -> {
            for (int i = 0; i < 10; i++) {
                System.out.println("Reader2 count: " + list.size());
                try { Thread.sleep(200); } catch (InterruptedException e) {}
            }
        });

        writer.start();
        reader1.start();
        reader2.start();

        writer.join();
        reader1.join();
        reader2.join();
    }
}
```

**When to use**: ConcurrentHashMap for general-purpose concurrent map needs (balanced read/write). CopyOnWriteArrayList when you have many readers and very few writers (iteration-heavy workloads). Avoid CopyOnWriteArrayList for write-heavy scenarios as it creates a copy on every modification.

---

## Functional Programming

### Recipe 27: Stream API Advanced Operations

**Problem**: Complex data transformations with streams.

**Solution**:

```java
import java.util.*;
import java.util.stream.*;

// Example 1: flatMap, reduce, collect with custom collectors
public class AdvancedStreamExample {
    public static void main(String[] args) {
        List<List<Integer>> nested = Arrays.asList(
            Arrays.asList(1, 2, 3),
            Arrays.asList(4, 5),
            Arrays.asList(6, 7, 8, 9)
        );

        // flatMap: Flatten nested collections
        List<Integer> flattened = nested.stream()
            .flatMap(List::stream)
            .collect(Collectors.toList());
        System.out.println("Flattened: " + flattened);
        // Output: [1, 2, 3, 4, 5, 6, 7, 8, 9]

        // reduce: Combine elements into single result
        int sum = flattened.stream()
            .reduce(0, Integer::sum);
        System.out.println("Sum: " + sum);  // 45

        // Custom collector: Join with prefix/suffix
        String formatted = flattened.stream()
            .map(String::valueOf)
            .collect(Collectors.joining(", ", "[", "]"));
        System.out.println(formatted);  // [1, 2, 3, 4, 5, 6, 7, 8, 9]

        // Custom collector: Statistics
        IntSummaryStatistics stats = flattened.stream()
            .collect(Collectors.summarizingInt(Integer::intValue));
        System.out.println("Average: " + stats.getAverage());
        System.out.println("Max: " + stats.getMax());
    }
}
```

Advanced stream operations allow complex transformations with readable, declarative code.

```java
import java.util.*;
import java.util.stream.*;

// Example 2: Partitioning and grouping with downstream collectors
public class GroupingExample {
    static class Person {
        String name;
        int age;
        String city;

        Person(String name, int age, String city) {
            this.name = name;
            this.age = age;
            this.city = city;
        }
    }

    public static void main(String[] args) {
        List<Person> people = Arrays.asList(
            new Person("Alice", 30, "NYC"),
            new Person("Bob", 25, "LA"),
            new Person("Charlie", 35, "NYC"),
            new Person("Diana", 28, "LA")
        );

        // Partition by condition (true/false groups)
        Map<Boolean, List<Person>> partitioned = people.stream()
            .collect(Collectors.partitioningBy(p -> p.age >= 30));
        System.out.println("Age >= 30: " + partitioned.get(true).size());
        System.out.println("Age < 30: " + partitioned.get(false).size());

        // Group by city with downstream collector (average age per city)
        Map<String, Double> avgAgeByCity = people.stream()
            .collect(Collectors.groupingBy(
                p -> p.city,
                Collectors.averagingInt(p -> p.age)
            ));
        avgAgeByCity.forEach((city, avgAge) ->
            System.out.println(city + " avg age: " + avgAge));
        // NYC avg age: 32.5, LA avg age: 26.5

        // Multi-level grouping
        Map<String, Map<Boolean, List<Person>>> multiLevel = people.stream()
            .collect(Collectors.groupingBy(
                p -> p.city,
                Collectors.partitioningBy(p -> p.age >= 30)
            ));
    }
}
```

**When to use**: Complex collection transformations, data aggregation, reporting, ETL pipelines, or any scenario where you need to group, partition, or summarize data from collections.

### Recipe 28: Functional Interfaces and Lambdas

**Problem**: Pass behavior as parameters without creating verbose anonymous classes.

**Solution**:

```java
import java.util.*;
import java.util.function.*;

// Example 1: Common functional interfaces (Predicate, Function, Consumer)
public class FunctionalInterfaceExample {
    public static void main(String[] args) {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

        // Predicate<T>: Takes T, returns boolean
        Predicate<Integer> isEven = n -> n % 2 == 0;
        List<Integer> evens = filter(numbers, isEven);
        System.out.println("Evens: " + evens);  // [2, 4, 6, 8, 10]

        // Function<T, R>: Takes T, returns R
        Function<Integer, String> toSquare = n -> "Square of " + n + " = " + (n * n);
        List<String> squares = map(numbers, toSquare);
        squares.forEach(System.out::println);
        // Square of 1 = 1, Square of 2 = 4, ...

        // Consumer<T>: Takes T, returns void
        Consumer<Integer> printer = n -> System.out.println("Number: " + n);
        forEach(numbers, printer);

        // BiFunction<T, U, R>: Takes T and U, returns R
        BiFunction<Integer, Integer, Integer> multiply = (a, b) -> a * b;
        System.out.println(multiply.apply(5, 3));  // 15
    }

    static <T> List<T> filter(List<T> list, Predicate<T> predicate) {
        List<T> result = new ArrayList<>();
        for (T item : list) {
            if (predicate.test(item)) result.add(item);
        }
        return result;
    }

    static <T, R> List<R> map(List<T> list, Function<T, R> mapper) {
        List<R> result = new ArrayList<>();
        for (T item : list) {
            result.add(mapper.apply(item));
        }
        return result;
    }

    static <T> void forEach(List<T> list, Consumer<T> action) {
        for (T item : list) {
            action.accept(item);
        }
    }
}
```

Functional interfaces enable passing behavior as parameters, making code more flexible and reusable.

```java
import java.util.function.*;

// Example 2: Custom functional interfaces with method references
@FunctionalInterface
interface Validator<T> {
    boolean validate(T value);

    // Default method for composition
    default Validator<T> and(Validator<T> other) {
        return value -> this.validate(value) && other.validate(value);
    }
}

public class CustomFunctionalExample {
    public static void main(String[] args) {
        // Custom functional interface
        Validator<String> notEmpty = s -> s != null && !s.isEmpty();
        Validator<String> minLength = s -> s.length() >= 3;
        Validator<String> maxLength = s -> s.length() <= 20;

        // Compose validators
        Validator<String> username = notEmpty.and(minLength).and(maxLength);

        System.out.println(username.validate("alice"));     // true
        System.out.println(username.validate("ab"));        // false (too short)
        System.out.println(username.validate(""));          // false (empty)

        // Method reference examples
        List<String> names = Arrays.asList("alice", "bob", "charlie");

        // Static method reference
        names.forEach(CustomFunctionalExample::printUpperCase);

        // Instance method reference
        StringProcessor processor = new StringProcessor();
        names.forEach(processor::process);

        // Constructor reference
        Supplier<List<String>> listFactory = ArrayList::new;
        List<String> newList = listFactory.get();
    }

    static void printUpperCase(String s) {
        System.out.println(s.toUpperCase());
    }

    static class StringProcessor {
        void process(String s) {
            System.out.println("Processing: " + s);
        }
    }
}
```

**When to use**: Callbacks, event handlers, strategy pattern implementations, validation logic, or any scenario where you need to pass behavior as parameters. Especially useful in frameworks that accept functional interfaces (Spring, JavaFX, etc.).

### Recipe 29: Method References

**Problem**: Simplify lambda expressions for method calls.

**Solution**:

```java
import java.util.*;
import java.util.function.*;

// Example 1: Static, instance, and constructor references
public class MethodReferenceExample {
    public static void main(String[] args) {
        List<String> names = Arrays.asList("alice", "bob", "charlie");

        // Lambda vs Method Reference comparison

        // 1. Static method reference
        // Lambda:  s -> Integer.parseInt(s)
        // Reference: Integer::parseInt
        List<String> numbers = Arrays.asList("1", "2", "3");
        numbers.stream()
            .map(Integer::parseInt)
            .forEach(System.out::println);

        // 2. Instance method reference on arbitrary object
        // Lambda:  s -> s.toUpperCase()
        // Reference: String::toUpperCase
        names.stream()
            .map(String::toUpperCase)
            .forEach(System.out::println);  // ALICE, BOB, CHARLIE

        // 3. Instance method reference on specific object
        // Lambda:  s -> System.out.println(s)
        // Reference: System.out::println
        names.forEach(System.out::println);

        // 4. Constructor reference
        // Lambda:  () -> new ArrayList<>()
        // Reference: ArrayList::new
        Supplier<List<String>> listSupplier = ArrayList::new;
        List<String> newList = listSupplier.get();

        // Lambda:  s -> new Person(s)
        // Reference: Person::new
        Function<String, Person> personFactory = Person::new;
        Person person = personFactory.apply("Alice");
    }

    static class Person {
        String name;
        Person(String name) { this.name = name; }
    }
}
```

Method references provide cleaner, more readable syntax than equivalent lambdas when you're just calling a method.

```java
import java.util.*;
import java.util.stream.*;

// Example 2: Chaining method references in streams
public class ChainedReferenceExample {
    static class Order {
        String id;
        double amount;
        Order(String id, double amount) {
            this.id = id;
            this.amount = amount;
        }
        double getAmount() { return amount; }
        String getId() { return id; }
    }

    public static void main(String[] args) {
        List<Order> orders = Arrays.asList(
            new Order("A1", 100.0),
            new Order("B2", 250.0),
            new Order("C3", 75.0),
            new Order("D4", 300.0)
        );

        // Chain method references for complex pipeline
        double totalHighValue = orders.stream()
            .filter(ChainedReferenceExample::isHighValue)  // Static method ref
            .map(Order::getAmount)                         // Instance method ref
            .reduce(0.0, Double::sum);                     // Static method ref

        System.out.println("Total high-value orders: $" + totalHighValue);
        // Output: Total high-value orders: $550.0

        // Sorting with method reference
        orders.stream()
            .sorted(Comparator.comparing(Order::getAmount))
            .map(Order::getId)
            .forEach(System.out::println);
        // Output: C3, A1, B2, D4

        // Multi-field sorting
        List<String> words = Arrays.asList("apple", "pie", "a", "banana", "zoo");
        words.stream()
            .sorted(Comparator.comparing(String::length)
                              .thenComparing(String::toLowerCase))
            .forEach(System.out::println);
        // Output: a, pie, zoo, apple, banana
    }

    static boolean isHighValue(Order order) {
        return order.getAmount() > 100;
    }
}
```

**When to use**: Replace simple lambdas with cleaner syntax. Use when the lambda body is just a single method call. Particularly effective in Stream pipelines for improved readability.

---

## Modern Java Features

### Recipe 30: Record Classes (Java 14+)

**Problem**: Create immutable data carriers without verbose boilerplate code.

**Solution**:

```java
// Example 1: Basic record with components and methods
public class RecordExample {
    // Traditional class (verbose)
    static class PersonOld {
        private final String name;
        private final int age;

        public PersonOld(String name, int age) {
            this.name = name;
            this.age = age;
        }

        public String getName() { return name; }
        public int getAge() { return age; }

        @Override
        public boolean equals(Object o) { /* boilerplate */ return false; }
        @Override
        public int hashCode() { /* boilerplate */ return 0; }
        @Override
        public String toString() { return "Person[name=" + name + ", age=" + age + "]"; }
    }

    // Record class (concise) - generates constructor, getters, equals, hashCode, toString
    record Person(String name, int age) {
        // Custom method
        public boolean isAdult() {
            return age >= 18;
        }

        // Compact constructor for validation
        public Person {
            if (age < 0) {
                throw new IllegalArgumentException("Age cannot be negative");
            }
        }
    }

    public static void main(String[] args) {
        Person person = new Person("Alice", 30);

        System.out.println(person.name());  // Auto-generated accessor
        System.out.println(person.age());
        System.out.println(person.isAdult());  // true
        System.out.println(person);  // Person[name=Alice, age=30]

        // Records are immutable - all fields are final
        // person.name = "Bob";  // Compilation error
    }
}
```

Records eliminate boilerplate for data transfer objects, automatically generating constructor, accessors, equals, hashCode, and toString.

```java
import java.util.*;

// Example 2: Records with validation and custom constructors
public class AdvancedRecordExample {
    // Record with validation
    record Point(int x, int y) {
        // Compact constructor - validates before assigning
        public Point {
            if (x < 0 || y < 0) {
                throw new IllegalArgumentException("Coordinates must be non-negative");
            }
        }

        // Additional constructor
        public Point() {
            this(0, 0);  // Delegates to canonical constructor
        }

        // Custom methods
        public double distanceFromOrigin() {
            return Math.sqrt(x * x + y * y);
        }
    }

    // Nested records for complex data structures
    record Address(String street, String city, String zipCode) {}
    record Employee(String name, int id, Address address) {
        public String fullAddress() {
            return address.street() + ", " + address.city() + " " + address.zipCode();
        }
    }

    public static void main(String[] args) {
        Point p1 = new Point(3, 4);
        System.out.println("Distance: " + p1.distanceFromOrigin());  // 5.0

        Point p2 = new Point();  // Uses default constructor
        System.out.println(p2);  // Point[x=0, y=0]

        // Nested records
        Address address = new Address("123 Main St", "NYC", "10001");
        Employee emp = new Employee("Alice", 101, address);
        System.out.println(emp.fullAddress());
        // Output: 123 Main St, NYC 10001

        // Records work great with collections
        List<Point> points = Arrays.asList(
            new Point(1, 2),
            new Point(3, 4),
            new Point(5, 6)
        );
        points.forEach(System.out::println);
    }
}
```

**When to use**: DTOs (Data Transfer Objects), value objects, API responses, configuration objects, or any immutable data structure. Records are perfect for modeling domain data without boilerplate. Avoid when you need mutable fields or inheritance.

### Recipe 31: Pattern Matching with Switch (Java 21+)

**Problem**: Type-safe conditional logic without verbose instanceof chains.

**Solution**:

```java
// Example 1: Pattern matching for types
public class PatternMatchingExample {
    sealed interface Shape permits Circle, Rectangle, Triangle {}
    record Circle(double radius) implements Shape {}
    record Rectangle(double width, double height) implements Shape {}
    record Triangle(double base, double height) implements Shape {}

    public static void main(String[] args) {
        Shape shape = new Circle(5.0);

        // Old way: instanceof chain
        double areaOld;
        if (shape instanceof Circle) {
            Circle c = (Circle) shape;
            areaOld = Math.PI * c.radius() * c.radius();
        } else if (shape instanceof Rectangle) {
            Rectangle r = (Rectangle) shape;
            areaOld = r.width() * r.height();
        } else if (shape instanceof Triangle) {
            Triangle t = (Triangle) shape;
            areaOld = 0.5 * t.base() * t.height();
        } else {
            areaOld = 0;
        }

        // New way: Pattern matching switch
        double area = switch (shape) {
            case Circle c -> Math.PI * c.radius() * c.radius();
            case Rectangle r -> r.width() * r.height();
            case Triangle t -> 0.5 * t.base() * t.height();
        };

        System.out.println("Area: " + area);  // Area: 78.53981633974483
    }
}
```

Pattern matching switch eliminates verbose type checking and casting, making code more concise and safer.

```java
// Example 2: Guarded patterns and null handling
public class GuardedPatternExample {
    record Point(int x, int y) {}

    public static void main(String[] args) {
        Object obj = new Point(5, 10);

        // Guarded patterns: Pattern with condition
        String result = switch (obj) {
            case null -> "Null value";
            case String s when s.isEmpty() -> "Empty string";
            case String s when s.length() < 5 -> "Short string: " + s;
            case String s -> "String: " + s;
            case Point p when p.x() == 0 && p.y() == 0 -> "Origin point";
            case Point p when p.x() > 0 && p.y() > 0 -> "First quadrant";
            case Point p -> "Point at (" + p.x() + ", " + p.y() + ")";
            case Integer i when i > 100 -> "Large number";
            case Integer i -> "Number: " + i;
            default -> "Unknown type";
        };

        System.out.println(result);  // Point at (5, 10)

        // Practical example: Processing different message types
        processMessage(new Point(3, 4));
        processMessage("Hello");
        processMessage(null);
        processMessage(42);
    }

    static void processMessage(Object message) {
        String response = switch (message) {
            case null -> "No message";
            case String s when s.startsWith("ERROR:") ->
                "Error detected: " + s.substring(6);
            case String s -> "Text message: " + s;
            case Point p when p.x() == p.y() ->
                "Diagonal point: " + p;
            case Point p ->
                "Point location: x=" + p.x() + ", y=" + p.y();
            case Integer i when i < 0 ->
                "Negative number: " + i;
            case Integer i ->
                "Positive number: " + i;
            default -> "Unknown message type";
        };
        System.out.println(response);
    }
}
```

**When to use**: Processing different types polymorphically without inheritance, handling sealed class hierarchies, parsing different message formats, or implementing type-safe state machines. Guards (`when` clauses) add conditional logic without nested if statements.

---

## Working with JSON

### Recipe 32: Convert Object to JSON

**Problem**: Serialize Java object to JSON string.

**Solution**:

```java
import com.google.gson.Gson;

// Basic example - Simple object serialization
class Person {
    String name;
    int age;

    Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}

public class JsonSerializationBasic {
    public static void main(String[] args) {
        Person person = new Person("Alice", 30);

        // Create Gson instance and serialize
        Gson gson = new Gson();
        String json = gson.toJson(person);

        System.out.println("JSON: " + json);
        // Output: JSON: {"name":"Alice","age":30}

        // Serialize list of objects
        java.util.List<Person> people = java.util.Arrays.asList(
            new Person("Alice", 30),
            new Person("Bob", 25)
        );
        String jsonArray = gson.toJson(people);
        System.out.println("Array: " + jsonArray);
        // Output: Array: [{"name":"Alice","age":30},{"name":"Bob","age":25}]
    }
}
```

Gson automatically converts Java objects to JSON by inspecting field names and values. It handles primitive types, strings, collections, and nested objects automatically.

```java
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.annotations.*;

// Advanced example - Custom serialization with annotations and formatting
class Employee {
    @SerializedName("employee_name")  // Custom JSON field name
    private String name;

    private int age;

    @Expose(serialize = true, deserialize = false)
    private String department;

    @SerializedName("hire_date")
    private java.time.LocalDate hireDate;

    private transient String password;  // Excluded from JSON

    Employee(String name, int age, String dept, java.time.LocalDate hireDate, String password) {
        this.name = name;
        this.age = age;
        this.department = dept;
        this.hireDate = hireDate;
        this.password = password;
    }
}

public class JsonSerializationAdvanced {
    public static void main(String[] args) {
        Employee emp = new Employee("Alice", 30, "Engineering",
            java.time.LocalDate.of(2020, 1, 15), "secret123");

        // Pretty-printed JSON with custom date format
        Gson gson = new GsonBuilder()
            .setPrettyPrinting()  // Format with indentation
            .excludeFieldsWithoutExposeAnnotation()  // Only serialize @Expose fields
            .setDateFormat("yyyy-MM-dd")
            .create();

        String json = gson.toJson(emp);
        System.out.println(json);
        // Output:
        // {
        //   "employee_name": "Alice",
        //   "department": "Engineering"
        // }
        // Note: password is excluded (transient), hireDate and age excluded (@Expose)

        // Without @Expose filtering
        Gson gsonAll = new GsonBuilder()
            .setPrettyPrinting()
            .setDateFormat("yyyy-MM-dd")
            .create();

        String jsonAll = gsonAll.toJson(emp);
        System.out.println(jsonAll);
        // Output:
        // {
        //   "employee_name": "Alice",
        //   "age": 30,
        //   "department": "Engineering",
        //   "hire_date": "2020-01-15"
        // }
        // Note: password still excluded (transient)
    }
}
```

**When to use**: REST API responses, saving configuration to files, logging structured data, or any scenario where you need to convert Java objects to JSON format. Use GsonBuilder for custom formatting (pretty printing, date formats, field exclusion). Use @SerializedName for custom JSON field names. Use transient for sensitive fields that should never be serialized.

### Recipe 33: Parse JSON to Object

**Problem**: Deserialize JSON string to Java object.

**Solution**:

```java
import com.google.gson.Gson;
import java.util.*;

// Basic example - Simple JSON deserialization
class Person {
    String name;
    int age;

    Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}

public class JsonDeserializationBasic {
    public static void main(String[] args) {
        // Parse single object
        String json = "{\"name\":\"Bob\",\"age\":25}";

        Gson gson = new Gson();
        Person person = gson.fromJson(json, Person.class);

        System.out.println("Name: " + person.name);  // Name: Bob
        System.out.println("Age: " + person.age);    // Age: 25

        // Parse array of objects
        String jsonArray = "[{\"name\":\"Alice\",\"age\":30},{\"name\":\"Bob\",\"age\":25}]";

        Person[] peopleArray = gson.fromJson(jsonArray, Person[].class);
        System.out.println("Count: " + peopleArray.length);  // Count: 2

        // Parse to generic List
        java.util.List<Person> peopleList = gson.fromJson(jsonArray,
            new com.google.gson.reflect.TypeToken<List<Person>>(){}.getType());

        peopleList.forEach(p -> System.out.println(p.name));
        // Output: Alice, Bob
    }
}
```

Gson automatically maps JSON fields to Java object fields by matching field names. It handles type conversions (strings to numbers, etc.) and nested objects automatically.

```java
import com.google.gson.*;
import com.google.gson.annotations.*;
import java.time.LocalDate;

// Advanced example - Complex JSON with nested objects and error handling
class Address {
    String street;
    String city;
    String zipCode;
}

class Employee {
    @SerializedName("employee_name")
    private String name;

    private int age;

    private Address address;  // Nested object

    @SerializedName("hire_date")
    private LocalDate hireDate;

    void display() {
        System.out.println("Employee: " + name + ", Age: " + age);
        if (address != null) {
            System.out.println("Address: " + address.street + ", " + address.city);
        }
    }
}

public class JsonDeserializationAdvanced {
    public static void main(String[] args) {
        // Nested JSON with custom field names
        String json = """
            {
                "employee_name": "Alice",
                "age": 30,
                "address": {
                    "street": "123 Main St",
                    "city": "NYC",
                    "zipCode": "10001"
                },
                "hire_date": "2020-01-15"
            }
            """;

        // Custom Gson with date format
        Gson gson = new GsonBuilder()
            .setDateFormat("yyyy-MM-dd")
            .create();

        Employee emp = gson.fromJson(json, Employee.class);
        emp.display();
        // Output:
        // Employee: Alice, Age: 30
        // Address: 123 Main St, NYC

        // Error handling for malformed JSON
        String badJson = "{invalid json}";
        try {
            Employee badEmp = gson.fromJson(badJson, Employee.class);
        } catch (JsonSyntaxException e) {
            System.err.println("JSON parse error: " + e.getMessage());
            // Output: JSON parse error: Expected name at line 1 column 2...
        }

        // Handle missing or extra fields gracefully
        String partialJson = "{\"employee_name\":\"Bob\"}";  // Missing age, address
        Employee partial = gson.fromJson(partialJson, Employee.class);
        System.out.println("Partial - Name: " + partial.name + ", Age: " + partial.age);
        // Output: Partial - Name: Bob, Age: 0
        // Note: Missing fields get default values (0 for int, null for objects)
    }
}
```

**When to use**: REST API responses, loading configuration files, processing webhook payloads, or any scenario where you need to convert JSON to Java objects. Use TypeToken for generic collections. Handle JsonSyntaxException for malformed JSON. Gson tolerates missing fields (assigns default values) and extra fields (ignores them).

---

## REST API Usage

### Recipe 34: Make HTTP GET Request

**Problem**: Fetch data from a web service.

**Solution**:

```java
import java.net.http.*;
import java.net.URI;
import java.io.IOException;

// Basic example - Simple GET request
public class HttpGetBasic {
    public static void main(String[] args) throws IOException, InterruptedException {
        // Create HTTP client (reusable)
        HttpClient client = HttpClient.newHttpClient();

        // Build GET request
        HttpRequest request = HttpRequest.newBuilder()
            .GET()
            .uri(URI.create("https://api.github.com/users/octocat"))
            .build();

        // Send request and get response
        HttpResponse<String> response = client.send(request,
            HttpResponse.BodyHandlers.ofString());

        System.out.println("Status: " + response.statusCode());  // Status: 200
        System.out.println("Body: " + response.body());
        // Output: Body: {"login":"octocat","id":583231,...}

        // Access response headers
        response.headers().map().forEach((key, values) -> {
            System.out.println(key + ": " + String.join(", ", values));
        });
    }
}
```

The HttpClient API (Java 11+) provides a modern, fluent interface for HTTP requests. HttpClient instances are thread-safe and reusable. The send() method blocks until the response is received.

```java
import java.net.http.*;
import java.net.URI;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;

// Advanced example - Headers, timeouts, and async requests
public class HttpGetAdvanced {
    public static void main(String[] args) throws Exception {
        // Configure client with timeout
        HttpClient client = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(10))
            .followRedirects(HttpClient.Redirect.NORMAL)  // Follow redirects automatically
            .build();

        // GET with custom headers
        HttpRequest request = HttpRequest.newBuilder()
            .GET()
            .uri(URI.create("https://api.github.com/user"))
            .header("Accept", "application/json")
            .header("Authorization", "Bearer ghp_yourtoken")
            .timeout(Duration.ofSeconds(30))  // Request-specific timeout
            .build();

        // Synchronous request
        HttpResponse<String> response = client.send(request,
            HttpResponse.BodyHandlers.ofString());

        if (response.statusCode() == 200) {
            System.out.println("Success: " + response.body());
        } else {
            System.err.println("Error " + response.statusCode() + ": " + response.body());
        }

        // Asynchronous request (non-blocking)
        CompletableFuture<HttpResponse<String>> futureResponse =
            client.sendAsync(request, HttpResponse.BodyHandlers.ofString());

        futureResponse.thenAccept(resp -> {
            System.out.println("Async status: " + resp.statusCode());
            System.out.println("Async body: " + resp.body());
        }).join();  // Wait for completion

        // Multiple parallel requests
        String[] urls = {
            "https://api.github.com/users/octocat",
            "https://api.github.com/users/torvalds",
            "https://api.github.com/users/gvanrossum"
        };

        java.util.List<CompletableFuture<HttpResponse<String>>> futures =
            java.util.Arrays.stream(urls)
                .map(url -> HttpRequest.newBuilder().uri(URI.create(url)).build())
                .map(req -> client.sendAsync(req, HttpResponse.BodyHandlers.ofString()))
                .toList();

        // Wait for all to complete
        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();

        futures.forEach(future -> {
            try {
                HttpResponse<String> resp = future.get();
                System.out.println("Parallel response: " + resp.statusCode());
            } catch (Exception e) {
                System.err.println("Request failed: " + e.getMessage());
            }
        });
    }
}
```

**When to use**: Calling REST APIs, fetching web resources, integrating with external services, or any HTTP GET operation. Use sendAsync() for non-blocking requests and parallel operations. Set timeouts to prevent hanging indefinitely. Add headers for authentication (Authorization), content negotiation (Accept), or API keys.

### Recipe 35: Make HTTP POST Request

**Problem**: Send data to a web service.

**Solution**:

```java
import java.net.http.*;
import java.net.URI;
import java.io.IOException;

// Basic example - Simple POST request with JSON body
public class HttpPostBasic {
    public static void main(String[] args) throws IOException, InterruptedException {
        HttpClient client = HttpClient.newHttpClient();

        // JSON payload
        String jsonBody = "{\"name\":\"Alice\",\"email\":\"alice@example.com\"}";

        // Build POST request
        HttpRequest request = HttpRequest.newBuilder()
            .POST(HttpRequest.BodyPublishers.ofString(jsonBody))
            .uri(URI.create("https://httpbin.org/post"))
            .header("Content-Type", "application/json")
            .build();

        // Send and receive response
        HttpResponse<String> response = client.send(request,
            HttpResponse.BodyHandlers.ofString());

        System.out.println("Status: " + response.statusCode());  // Status: 200
        System.out.println("Response: " + response.body());
        // Output: Response: {"json":{"name":"Alice","email":"alice@example.com"},...}

        // Check if successful
        if (response.statusCode() >= 200 && response.statusCode() < 300) {
            System.out.println("POST successful!");
        } else {
            System.err.println("POST failed with status: " + response.statusCode());
        }
    }
}
```

POST requests send data in the request body. Use BodyPublishers.ofString() for JSON/XML payloads. Always set Content-Type header to match body format. Check status code for success (2xx range).

```java
import java.net.http.*;
import java.net.URI;
import java.util.*;

// Advanced example - Form data, multipart, and error handling
public class HttpPostAdvanced {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();

        // Example 1: POST form data (application/x-www-form-urlencoded)
        Map<String, String> formData = Map.of(
            "username", "alice",
            "password", "secret123"
        );

        String formBody = formData.entrySet().stream()
            .map(e -> e.getKey() + "=" + java.net.URLEncoder.encode(e.getValue(),
                java.nio.charset.StandardCharsets.UTF_8))
            .reduce((a, b) -> a + "&" + b)
            .orElse("");

        HttpRequest formRequest = HttpRequest.newBuilder()
            .POST(HttpRequest.BodyPublishers.ofString(formBody))
            .uri(URI.create("https://httpbin.org/post"))
            .header("Content-Type", "application/x-www-form-urlencoded")
            .build();

        HttpResponse<String> formResponse = client.send(formRequest,
            HttpResponse.BodyHandlers.ofString());
        System.out.println("Form POST: " + formResponse.statusCode());

        // Example 2: POST with authentication and custom headers
        String jsonPayload = """
            {
                "title": "New Post",
                "content": "This is the post content",
                "author_id": 123
            }
            """;

        HttpRequest authRequest = HttpRequest.newBuilder()
            .POST(HttpRequest.BodyPublishers.ofString(jsonPayload))
            .uri(URI.create("https://api.example.com/posts"))
            .header("Content-Type", "application/json")
            .header("Authorization", "Bearer your_api_token")
            .header("X-Request-ID", UUID.randomUUID().toString())
            .timeout(java.time.Duration.ofSeconds(30))
            .build();

        try {
            HttpResponse<String> authResponse = client.send(authRequest,
                HttpResponse.BodyHandlers.ofString());

            // Handle different status codes
            switch (authResponse.statusCode()) {
                case 200, 201 -> System.out.println("Created: " + authResponse.body());
                case 400 -> System.err.println("Bad request: " + authResponse.body());
                case 401 -> System.err.println("Unauthorized - check your token");
                case 500 -> System.err.println("Server error");
                default -> System.err.println("Status: " + authResponse.statusCode());
            }
        } catch (IOException e) {
            System.err.println("Network error: " + e.getMessage());
        } catch (InterruptedException e) {
            System.err.println("Request interrupted");
            Thread.currentThread().interrupt();
        }

        // Example 3: Async POST
        client.sendAsync(authRequest, HttpResponse.BodyHandlers.ofString())
            .thenApply(HttpResponse::body)
            .thenAccept(body -> System.out.println("Async POST response: " + body))
            .exceptionally(e -> {
                System.err.println("Async POST failed: " + e.getMessage());
                return null;
            })
            .join();
    }
}
```

**When to use**: Creating resources via REST APIs, submitting forms, uploading data, authentication requests, or any HTTP POST operation. Use form-urlencoded for simple form submissions. Use JSON for API requests. Always handle different status codes (201 Created, 400 Bad Request, 401 Unauthorized, 500 Server Error). Use sendAsync() for non-blocking POST operations.

---

## Frequently Used Patterns

### Recipe 36: Builder Pattern

**Problem**: Create objects with many optional fields.

**Solution**:

```java
import java.util.*;

// Basic example - Simple builder for object construction
class HttpRequest {
    private final String url;
    private final String method;
    private final Map<String, String> headers;
    private final String body;

    // Private constructor - only accessible via builder
    private HttpRequest(Builder builder) {
        this.url = builder.url;
        this.method = builder.method;
        this.headers = new HashMap<>(builder.headers);
        this.body = builder.body;
    }

    // Builder class
    public static class Builder {
        private String url;
        private String method = "GET";  // Default value
        private Map<String, String> headers = new HashMap<>();
        private String body = "";

        public Builder url(String url) {
            this.url = url;
            return this;  // Return this for chaining
        }

        public Builder method(String method) {
            this.method = method;
            return this;
        }

        public Builder header(String key, String value) {
            headers.put(key, value);
            return this;
        }

        public Builder body(String body) {
            this.body = body;
            return this;
        }

        public HttpRequest build() {
            if (url == null || url.isEmpty()) {
                throw new IllegalStateException("URL is required");
            }
            return new HttpRequest(this);
        }
    }

    @Override
    public String toString() {
        return method + " " + url + " (headers: " + headers.size() + ")";
    }
}

public class BuilderPatternBasic {
    public static void main(String[] args) {
        // Build object with fluent API
        HttpRequest request = new HttpRequest.Builder()
            .url("https://api.example.com/users")
            .method("POST")
            .header("Authorization", "Bearer token123")
            .header("Content-Type", "application/json")
            .body("{\"name\":\"Alice\"}")
            .build();

        System.out.println(request);
        // Output: POST https://api.example.com/users (headers: 2)

        // Build with minimal configuration (uses defaults)
        HttpRequest simpleRequest = new HttpRequest.Builder()
            .url("https://example.com")
            .build();

        System.out.println(simpleRequest);
        // Output: GET https://example.com (headers: 0)
    }
}
```

Builder pattern avoids telescoping constructors (many constructor overloads) and provides a fluent, readable API for object construction. Each builder method returns `this` to enable method chaining.

```java
// Advanced example - Builder with validation and step-by-step construction
class User {
    private final String username;  // Required
    private final String email;     // Required
    private final String firstName; // Optional
    private final String lastName;  // Optional
    private final int age;          // Optional
    private final Set<String> roles; // Optional

    private User(Builder builder) {
        this.username = builder.username;
        this.email = builder.email;
        this.firstName = builder.firstName;
        this.lastName = builder.lastName;
        this.age = builder.age;
        this.roles = new HashSet<>(builder.roles);
    }

    public static class Builder {
        // Required parameters
        private final String username;
        private final String email;

        // Optional parameters with defaults
        private String firstName = "";
        private String lastName = "";
        private int age = 0;
        private Set<String> roles = new HashSet<>();

        // Constructor for required fields
        public Builder(String username, String email) {
            if (username == null || username.isEmpty()) {
                throw new IllegalArgumentException("Username is required");
            }
            if (email == null || !email.contains("@")) {
                throw new IllegalArgumentException("Valid email is required");
            }
            this.username = username;
            this.email = email;
        }

        public Builder firstName(String firstName) {
            this.firstName = firstName;
            return this;
        }

        public Builder lastName(String lastName) {
            this.lastName = lastName;
            return this;
        }

        public Builder age(int age) {
            if (age < 0 || age > 150) {
                throw new IllegalArgumentException("Age must be 0-150");
            }
            this.age = age;
            return this;
        }

        public Builder addRole(String role) {
            this.roles.add(role);
            return this;
        }

        public Builder roles(Set<String> roles) {
            this.roles = new HashSet<>(roles);
            return this;
        }

        public User build() {
            // Additional validation before building
            if (age > 0 && firstName.isEmpty()) {
                throw new IllegalStateException("First name required if age is set");
            }
            return new User(this);
        }
    }

    @Override
    public String toString() {
        return "User{" +
            "username='" + username + '\'' +
            ", email='" + email + '\'' +
            ", name='" + firstName + " " + lastName + '\'' +
            ", age=" + age +
            ", roles=" + roles +
            '}';
    }
}

public class BuilderPatternAdvanced {
    public static void main(String[] args) {
        // Build complex object step-by-step
        User admin = new User.Builder("admin", "admin@example.com")
            .firstName("Alice")
            .lastName("Anderson")
            .age(30)
            .addRole("ADMIN")
            .addRole("USER")
            .build();

        System.out.println(admin);
        // Output: User{username='admin', email='admin@example.com',
        //         name='Alice Anderson', age=30, roles=[ADMIN, USER]}

        // Build with minimal info (optional fields omitted)
        User basicUser = new User.Builder("bob", "bob@example.com")
            .addRole("USER")
            .build();

        System.out.println(basicUser);
        // Output: User{username='bob', email='bob@example.com',
        //         name=' ', age=0, roles=[USER]}
    }
}
```

**When to use**: Objects with many optional parameters, immutable objects, complex object construction with validation, or when you want a more readable alternative to telescoping constructors. Builder pattern is especially useful for configuration objects, DTOs with many fields, and API client builders. Combine with validation in build() method to ensure object integrity.

### Recipe 37: Null Safety with Optional

**Problem**: Handle null values safely.

**Solution**:

```java
import java.util.Optional;

// Basic example - Wrapping and unwrapping optional values
public class OptionalBasic {
    public static void main(String[] args) {
        // Create Optional from nullable value
        String name = "Alice";
        Optional<String> optName = Optional.ofNullable(name);

        // Check if value is present
        if (optName.isPresent()) {
            System.out.println("Name: " + optName.get());  // Name: Alice
        }

        // Handle both present and absent cases
        optName.ifPresentOrElse(
            n -> System.out.println("Hello, " + n),  // Hello, Alice
            () -> System.out.println("No name provided")
        );

        // Provide default value if absent
        String nullName = null;
        String greeting = Optional.ofNullable(nullName)
            .orElse("Guest");
        System.out.println("Hello, " + greeting);  // Hello, Guest

        // Compute default lazily (only if absent)
        String computed = Optional.ofNullable(nullName)
            .orElseGet(() -> "User_" + System.currentTimeMillis());
        System.out.println(computed);  // User_1703123456789

        // Throw exception if absent
        try {
            String required = Optional.ofNullable(nullName)
                .orElseThrow(() -> new IllegalArgumentException("Name is required"));
        } catch (IllegalArgumentException e) {
            System.err.println("Error: " + e.getMessage());  // Error: Name is required
        }
    }
}
```

Optional is a container that may or may not hold a non-null value. Use Optional.ofNullable() for values that might be null. Never call get() without checking isPresent() first. Prefer orElse/orElseGet/orElseThrow over isPresent + get pattern.

```java
import java.util.*;
import java.util.stream.*;

// Advanced example - Chaining operations and functional style
public class OptionalAdvanced {
    static class User {
        String name;
        Optional<String> email;  // Optional field

        User(String name, String email) {
            this.name = name;
            this.email = Optional.ofNullable(email);
        }

        Optional<String> getEmail() {
            return email;
        }
    }

    public static void main(String[] args) {
        // Transform optional values with map
        Optional<String> name = Optional.of("Alice");
        String upper = name
            .map(String::toUpperCase)
            .orElse("UNKNOWN");
        System.out.println(upper);  // ALICE

        // Chain multiple transformations
        Optional<Integer> length = name
            .map(String::toUpperCase)
            .map(String::length);
        System.out.println("Length: " + length.orElse(0));  // Length: 5

        // Filter based on condition
        Optional<String> longName = name
            .filter(n -> n.length() > 3);
        System.out.println("Long name: " + longName.orElse("Short"));  // Long name: Alice

        Optional<String> shortName = name
            .filter(n -> n.length() > 10);
        System.out.println("Very long name: " + shortName.orElse("Not found"));
        // Very long name: Not found

        // Nested optionals - use flatMap instead of map
        User user1 = new User("Alice", "alice@example.com");
        User user2 = new User("Bob", null);

        // Wrong: map returns Optional<Optional<String>>
        // Optional<Optional<String>> nestedEmail = Optional.of(user1).map(User::getEmail);

        // Correct: flatMap flattens nested Optional
        String email1 = Optional.of(user1)
            .flatMap(User::getEmail)
            .orElse("no-email@example.com");
        System.out.println("Email1: " + email1);  // Email1: alice@example.com

        String email2 = Optional.of(user2)
            .flatMap(User::getEmail)
            .orElse("no-email@example.com");
        System.out.println("Email2: " + email2);  // Email2: no-email@example.com

        // Use in streams
        List<User> users = Arrays.asList(
            new User("Alice", "alice@example.com"),
            new User("Bob", null),
            new User("Charlie", "charlie@example.com")
        );

        List<String> emails = users.stream()
            .map(User::getEmail)
            .flatMap(Optional::stream)  // Java 9+: Convert Optional to Stream
            .collect(Collectors.toList());
        System.out.println("Emails: " + emails);
        // Emails: [alice@example.com, charlie@example.com]

        // Or using filter + map
        List<String> emails2 = users.stream()
            .filter(u -> u.getEmail().isPresent())
            .map(u -> u.getEmail().get())
            .collect(Collectors.toList());
        System.out.println("Emails2: " + emails2);
        // Emails2: [alice@example.com, charlie@example.com]
    }
}
```

**When to use**: Method return values that might not have a result (search, lookup, parse operations), optional configuration parameters, or avoiding NullPointerException. Do NOT use Optional for fields (use null instead), method parameters (use overloading), or collections (use empty collections). Optional is designed for return values and functional pipelines. Use map() for transformations, filter() for conditional checks, flatMap() for nested optionals, and orElse/orElseGet for defaults.

### Recipe 38: Factory Pattern

**Problem**: Create objects without exposing instantiation logic.

**Solution**:

```java
// Basic example - Simple factory for creating objects
interface Shape {
    void draw();
}

class Circle implements Shape {
    @Override
    public void draw() {
        System.out.println("Drawing a circle");
    }
}

class Rectangle implements Shape {
    @Override
    public void draw() {
        System.out.println("Drawing a rectangle");
    }
}

class Triangle implements Shape {
    @Override
    public void draw() {
        System.out.println("Drawing a triangle");
    }
}

// Factory class
class ShapeFactory {
    public static Shape createShape(String type) {
        return switch (type.toLowerCase()) {
            case "circle" -> new Circle();
            case "rectangle" -> new Rectangle();
            case "triangle" -> new Triangle();
            default -> throw new IllegalArgumentException("Unknown shape type: " + type);
        };
    }
}

public class FactoryPatternBasic {
    public static void main(String[] args) {
        // Create objects without knowing implementation details
        Shape circle = ShapeFactory.createShape("circle");
        Shape rectangle = ShapeFactory.createShape("rectangle");

        circle.draw();     // Drawing a circle
        rectangle.draw();  // Drawing a rectangle

        // Client code doesn't depend on concrete classes
        // Only depends on Shape interface and ShapeFactory
    }
}
```

Factory pattern encapsulates object creation logic in a separate class. Clients request objects by type/name without knowing concrete classes. This promotes loose coupling and makes it easy to add new types without changing client code.

```java
import java.util.*;

// Advanced example - Factory with registration and configuration
interface PaymentProcessor {
    void processPayment(double amount);
}

class CreditCardProcessor implements PaymentProcessor {
    private String cardNumber;

    CreditCardProcessor(String cardNumber) {
        this.cardNumber = cardNumber;
    }

    @Override
    public void processPayment(double amount) {
        System.out.println("Processing $" + amount + " via credit card: " +
            cardNumber.substring(cardNumber.length() - 4));
    }
}

class PayPalProcessor implements PaymentProcessor {
    private String email;

    PayPalProcessor(String email) {
        this.email = email;
    }

    @Override
    public void processPayment(double amount) {
        System.out.println("Processing $" + amount + " via PayPal: " + email);
    }
}

class BankTransferProcessor implements PaymentProcessor {
    private String accountNumber;

    BankTransferProcessor(String accountNumber) {
        this.accountNumber = accountNumber;
    }

    @Override
    public void processPayment(double amount) {
        System.out.println("Processing $" + amount + " via bank transfer to: " + accountNumber);
    }
}

// Factory with configuration and registration
class PaymentProcessorFactory {
    private static Map<String, ProcessorCreator> registry = new HashMap<>();

    @FunctionalInterface
    interface ProcessorCreator {
        PaymentProcessor create(String config);
    }

    // Register processor types
    static {
        registry.put("creditcard", CreditCardProcessor::new);
        registry.put("paypal", PayPalProcessor::new);
        registry.put("banktransfer", BankTransferProcessor::new);
    }

    public static PaymentProcessor createProcessor(String type, String config) {
        ProcessorCreator creator = registry.get(type.toLowerCase());
        if (creator == null) {
            throw new IllegalArgumentException("Unknown payment type: " + type);
        }
        return creator.create(config);
    }

    // Allow runtime registration of new types
    public static void registerProcessor(String type, ProcessorCreator creator) {
        registry.put(type.toLowerCase(), creator);
    }
}

public class FactoryPatternAdvanced {
    public static void main(String[] args) {
        // Create different payment processors
        PaymentProcessor cc = PaymentProcessorFactory.createProcessor(
            "creditcard", "4111-1111-1111-1111");
        PaymentProcessor paypal = PaymentProcessorFactory.createProcessor(
            "paypal", "user@example.com");
        PaymentProcessor bank = PaymentProcessorFactory.createProcessor(
            "banktransfer", "123456789");

        cc.processPayment(100.00);
        // Output: Processing $100.0 via credit card: 1111

        paypal.processPayment(50.00);
        // Output: Processing $50.0 via PayPal: user@example.com

        bank.processPayment(200.00);
        // Output: Processing $200.0 via bank transfer to: 123456789

        // Runtime registration of new processor type
        PaymentProcessorFactory.registerProcessor("crypto",
            config -> amount -> System.out.println("Processing $" + amount + " via crypto wallet: " + config));

        PaymentProcessor crypto = PaymentProcessorFactory.createProcessor(
            "crypto", "0x1234567890");
        crypto.processPayment(75.00);
        // Output: Processing $75.0 via crypto wallet: 0x1234567890
    }
}
```

**When to use**: When you have multiple related classes and want to centralize object creation, when creation logic is complex or may change, or when you want to decouple client code from concrete implementations. Factory pattern is especially useful for plugin systems, payment processors, database connections, or any scenario where you need to create objects based on runtime conditions.

**See Also**:

- [Recipe 39: Singleton Pattern](#recipe-39-singleton-pattern) - Creational pattern
- [Recipe 41: Load Configuration](#recipe-41-load-configuration) - Configuration factory

### Recipe 39: Singleton Pattern

**Problem**: Ensure only one instance of a class exists throughout the application.

**Solution**:

```java
// Basic example - Thread-safe singleton with enum (recommended)
public enum DatabaseConnection {
    INSTANCE;

    private java.sql.Connection connection;

    DatabaseConnection() {
        // Initialize connection (happens once)
        System.out.println("Creating database connection...");
        // this.connection = DriverManager.getConnection(url, user, pass);
    }

    public void executeQuery(String sql) {
        System.out.println("Executing: " + sql);
    }

    public static DatabaseConnection getInstance() {
        return INSTANCE;
    }
}

public class SingletonPatternBasic {
    public static void main(String[] args) {
        // Get singleton instance
        DatabaseConnection db1 = DatabaseConnection.getInstance();
        DatabaseConnection db2 = DatabaseConnection.getInstance();

        // Both variables point to same instance
        System.out.println("Same instance? " + (db1 == db2));  // Same instance? true

        db1.executeQuery("SELECT * FROM users");
        db2.executeQuery("SELECT * FROM orders");
        // Output: Creating database connection...
        //         Executing: SELECT * FROM users
        //         Executing: SELECT * FROM orders
    }
}
```

Enum singleton is the recommended approach in Java - it's thread-safe, prevents reflection attacks, handles serialization correctly, and is the most concise. The enum constructor is called only once when the enum is first accessed.

```java
// Advanced example - Lazy initialization with double-checked locking
public class ConfigurationManager {
    // Volatile ensures visibility across threads
    private static volatile ConfigurationManager instance;
    private java.util.Properties config;

    // Private constructor prevents direct instantiation
    private ConfigurationManager() {
        System.out.println("Loading configuration...");
        config = new java.util.Properties();
        config.setProperty("app.name", "MyApp");
        config.setProperty("app.version", "1.0.0");
        // Load from file in real implementation
    }

    // Thread-safe lazy initialization
    public static ConfigurationManager getInstance() {
        if (instance == null) {  // First check (no locking)
            synchronized (ConfigurationManager.class) {
                if (instance == null) {  // Second check (with locking)
                    instance = new ConfigurationManager();
                }
            }
        }
        return instance;
    }

    public String getProperty(String key) {
        return config.getProperty(key);
    }

    public void setProperty(String key, String value) {
        config.setProperty(key, value);
    }
}

// Alternative: Eager initialization (thread-safe, simple)
class Logger {
    // Instance created at class loading time
    private static final Logger INSTANCE = new Logger();

    private Logger() {
        System.out.println("Logger initialized");
    }

    public static Logger getInstance() {
        return INSTANCE;
    }

    public void log(String message) {
        System.out.println("[LOG] " + message);
    }
}

// Alternative: Bill Pugh Singleton (lazy + thread-safe without synchronization)
class CacheManager {
    private CacheManager() {
        System.out.println("Cache initialized");
    }

    // Inner static class - loaded only when getInstance() is called
    private static class Holder {
        private static final CacheManager INSTANCE = new CacheManager();
    }

    public static CacheManager getInstance() {
        return Holder.INSTANCE;
    }

    public void cache(String key, Object value) {
        System.out.println("Caching: " + key);
    }
}

public class SingletonPatternAdvanced {
    public static void main(String[] args) {
        // Double-checked locking singleton
        ConfigurationManager config1 = ConfigurationManager.getInstance();
        ConfigurationManager config2 = ConfigurationManager.getInstance();
        System.out.println("Config same? " + (config1 == config2));  // true
        System.out.println("App name: " + config1.getProperty("app.name"));
        // Output: Loading configuration...
        //         Config same? true
        //         App name: MyApp

        // Eager initialization singleton
        Logger logger1 = Logger.getInstance();
        Logger logger2 = Logger.getInstance();
        logger1.log("Test message");
        // Output: Logger initialized
        //         [LOG] Test message

        // Bill Pugh singleton
        CacheManager cache1 = CacheManager.getInstance();
        CacheManager cache2 = CacheManager.getInstance();
        System.out.println("Cache same? " + (cache1 == cache2));  // true
        // Output: Cache initialized
        //         Cache same? true
    }
}
```

**When to use**: Configuration managers, logging systems, database connection pools, caches, or any resource that should have only one instance globally. Prefer enum singleton for simplicity and safety. Use Bill Pugh (Holder pattern) for lazy initialization without synchronization overhead. Avoid singletons when dependency injection is available (Spring, Guice) as they make testing harder.

---

### Recipe 40: Copy File

**Problem**: Copy a file from one location to another.

**Solution**:

```java
import java.nio.file.*;
import java.io.IOException;

// Basic example - Simple file copy with options
public class FileCopyBasic {
    public static void main(String[] args) throws IOException {
        Path source = Paths.get("input/data.txt");
        Path target = Paths.get("output/data.txt");

        // Ensure target directory exists
        Files.createDirectories(target.getParent());

        // Simple copy (fails if target exists)
        Files.copy(source, target);
        System.out.println("File copied to: " + target);

        // Copy with replace existing
        Path target2 = Paths.get("output/data-backup.txt");
        Files.copy(source, target2, StandardCopyOption.REPLACE_EXISTING);
        System.out.println("File copied (replaced): " + target2);

        // Copy and preserve attributes (timestamps, permissions)
        Path target3 = Paths.get("output/data-preserved.txt");
        Files.copy(source, target3,
            StandardCopyOption.REPLACE_EXISTING,
            StandardCopyOption.COPY_ATTRIBUTES);
        System.out.println("File copied with attributes: " + target3);

        // Verify copy
        long sourceSize = Files.size(source);
        long targetSize = Files.size(target3);
        System.out.println("Size match: " + (sourceSize == targetSize));
        // Output: Size match: true
    }
}
```

Files.copy() is the recommended way to copy files in modern Java. It's efficient, atomic, and handles edge cases. Use StandardCopyOption.REPLACE_EXISTING to overwrite existing files. Use COPY_ATTRIBUTES to preserve metadata.

```java
import java.nio.file.*;
import java.io.*;
import java.util.stream.*;

// Advanced example - Batch copy with progress and error handling
public class FileCopyAdvanced {
    public static void main(String[] args) {
        try {
            // Copy single file with options
            copyWithOptions(
                Paths.get("source.txt"),
                Paths.get("backup/source.txt")
            );

            // Copy entire directory recursively
            copyDirectory(
                Paths.get("input"),
                Paths.get("backup/input")
            );

            // Copy multiple files with filter
            Path sourceDir = Paths.get("documents");
            Path targetDir = Paths.get("backup/documents");

            Files.createDirectories(targetDir);

            // Copy only PDF files
            try (Stream<Path> paths = Files.walk(sourceDir)) {
                paths.filter(Files::isRegularFile)
                     .filter(p -> p.toString().endsWith(".pdf"))
                     .forEach(source -> {
                         try {
                             Path relative = sourceDir.relativize(source);
                             Path target = targetDir.resolve(relative);
                             Files.createDirectories(target.getParent());
                             Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
                             System.out.println("Copied: " + source.getFileName());
                         } catch (IOException e) {
                             System.err.println("Failed to copy " + source + ": " + e.getMessage());
                         }
                     });
            }

        } catch (IOException e) {
            System.err.println("Copy operation failed: " + e.getMessage());
        }
    }

    // Copy file with all options
    static void copyWithOptions(Path source, Path target) throws IOException {
        // Ensure target directory exists
        Files.createDirectories(target.getParent());

        // Copy with multiple options
        CopyOption[] options = {
            StandardCopyOption.REPLACE_EXISTING,
            StandardCopyOption.COPY_ATTRIBUTES,
            LinkOption.NOFOLLOW_LINKS  // Don't follow symbolic links
        };

        Files.copy(source, target, options);

        // Verify copy integrity
        long sourceSize = Files.size(source);
        long targetSize = Files.size(target);

        if (sourceSize == targetSize) {
            System.out.println("✓ Copied successfully: " + source + " -> " + target);
        } else {
            throw new IOException("Copy size mismatch");
        }
    }

    // Recursively copy directory
    static void copyDirectory(Path source, Path target) throws IOException {
        Files.walk(source).forEach(sourcePath -> {
            try {
                Path targetPath = target.resolve(source.relativize(sourcePath));

                if (Files.isDirectory(sourcePath)) {
                    if (!Files.exists(targetPath)) {
                        Files.createDirectory(targetPath);
                    }
                } else {
                    Files.copy(sourcePath, targetPath,
                        StandardCopyOption.REPLACE_EXISTING,
                        StandardCopyOption.COPY_ATTRIBUTES);
                }
            } catch (IOException e) {
                System.err.println("Failed to copy: " + sourcePath + " - " + e.getMessage());
            }
        });

        System.out.println("Directory copied: " + source + " -> " + target);
    }
}
```

**When to use**: File backups, archiving, deployment scripts, or any scenario where you need to duplicate files. Use Files.copy() for single files. For directories, use Files.walk() to traverse and copy recursively. Always use REPLACE_EXISTING for update scenarios. Use COPY_ATTRIBUTES to preserve timestamps and permissions. Handle IOException for permission issues or disk space errors.

---

## Configuration and Logging

### Recipe 41: Load Configuration

**Problem**: Load application settings from configuration files.

**Solution**:

```java
import java.util.Properties;
import java.io.*;
import java.nio.file.*;

// Basic example - Load properties from file
public class LoadConfigBasic {
    public static void main(String[] args) {
        try {
            // Load from .properties file
            Properties config = new Properties();
            try (InputStream input = new FileInputStream("config.properties")) {
                config.load(input);
            }

            // Access properties
            String appName = config.getProperty("app.name");
            String version = config.getProperty("app.version");
            int port = Integer.parseInt(config.getProperty("server.port", "8080"));

            System.out.println("App: " + appName);
            System.out.println("Version: " + version);
            System.out.println("Port: " + port);
            // Output: App: MyApp
            //         Version: 1.0.0
            //         Port: 8080

            // Get with default value
            String dbHost = config.getProperty("db.host", "localhost");
            System.out.println("DB Host: " + dbHost);

            // Check if property exists
            if (config.containsKey("feature.enabled")) {
                boolean enabled = Boolean.parseBoolean(
                    config.getProperty("feature.enabled"));
                System.out.println("Feature enabled: " + enabled);
            }

        } catch (IOException e) {
            System.err.println("Failed to load config: " + e.getMessage());
        }
    }
}
```

Properties files use key=value format. Use getProperty(key, defaultValue) for safe access with fallbacks. Always use try-with-resources for file streams. Parse numeric/boolean values manually with appropriate methods.

```java
import java.util.*;
import java.io.*;
import java.nio.file.*;

// Advanced example - Multi-source configuration with environment override
public class LoadConfigAdvanced {
    private Properties config;

    public LoadConfigAdvanced() throws IOException {
        config = new Properties();

        // Load in priority order (later sources override earlier)
        // 1. Default configuration (embedded in JAR)
        loadFromClasspath("default-config.properties");

        // 2. Environment-specific config
        String env = System.getProperty("app.env", "dev");
        loadFromClasspath("config-" + env + ".properties");

        // 3. External configuration file (if exists)
        Path externalConfig = Paths.get("config.properties");
        if (Files.exists(externalConfig)) {
            loadFromFile(externalConfig);
        }

        // 4. System properties (highest priority)
        config.putAll(System.getProperties());

        // 5. Environment variables override
        loadFromEnvironment();
    }

    private void loadFromClasspath(String resource) {
        try (InputStream input = getClass().getClassLoader().getResourceAsStream(resource)) {
            if (input != null) {
                config.load(input);
                System.out.println("Loaded: " + resource);
            }
        } catch (IOException e) {
            System.err.println("Failed to load " + resource + ": " + e.getMessage());
        }
    }

    private void loadFromFile(Path path) throws IOException {
        try (InputStream input = Files.newInputStream(path)) {
            config.load(input);
            System.out.println("Loaded: " + path);
        }
    }

    private void loadFromEnvironment() {
        // Map environment variables to config keys
        // Example: APP_NAME -> app.name
        Map<String, String> env = System.getenv();
        env.forEach((key, value) -> {
            String configKey = key.toLowerCase().replace('_', '.');
            config.setProperty(configKey, value);
        });
        System.out.println("Loaded environment variables");
    }

    public String getString(String key) {
        return config.getProperty(key);
    }

    public String getString(String key, String defaultValue) {
        return config.getProperty(key, defaultValue);
    }

    public int getInt(String key, int defaultValue) {
        String value = config.getProperty(key);
        return value != null ? Integer.parseInt(value) : defaultValue;
    }

    public boolean getBoolean(String key, boolean defaultValue) {
        String value = config.getProperty(key);
        return value != null ? Boolean.parseBoolean(value) : defaultValue;
    }

    public List<String> getList(String key, String delimiter) {
        String value = config.getProperty(key);
        if (value == null || value.isEmpty()) {
            return Collections.emptyList();
        }
        return Arrays.asList(value.split(delimiter));
    }

    public void printAll() {
        System.out.println("=== Configuration ===");
        config.stringPropertyNames().stream()
              .sorted()
              .forEach(key -> System.out.println(key + " = " + config.getProperty(key)));
    }

    public static void main(String[] args) throws IOException {
        LoadConfigAdvanced config = new LoadConfigAdvanced();

        // Access configuration
        System.out.println("\n=== Using Configuration ===");
        String appName = config.getString("app.name", "Unknown");
        int port = config.getInt("server.port", 8080);
        boolean debug = config.getBoolean("app.debug", false);
        List<String> allowedHosts = config.getList("allowed.hosts", ",");

        System.out.println("App Name: " + appName);
        System.out.println("Port: " + port);
        System.out.println("Debug: " + debug);
        System.out.println("Allowed Hosts: " + allowedHosts);

        // Print all configuration
        config.printAll();
    }
}
```

**When to use**: Application initialization, environment-specific settings, feature flags, or any scenario where you need external configuration. Load from multiple sources with priority order (defaults < environment-specific < external file < system properties < environment variables). Use type-safe accessor methods (getInt, getBoolean) instead of raw getProperty. Store sensitive values (passwords, API keys) in environment variables, not config files.

**See Also**:

- [Recipe 42: Structured Logging (SLF4J)](#recipe-42-structured-logging-slf4j) - Logging setup
- [Recipe 38: Factory Pattern](#recipe-38-factory-pattern) - Factory for configs

### Recipe 42: Structured Logging (SLF4J)

**Problem**: Add professional logging to your application.

**Solution**:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

// Basic example - Structured logging with SLF4J
public class StructuredLoggingBasic {
    private static final Logger logger = LoggerFactory.getLogger(StructuredLoggingBasic.class);

    public static void main(String[] args) {
        // Different log levels
        logger.trace("Trace level - most detailed");
        logger.debug("Debug level - diagnostic info");
        logger.info("Info level - general information");
        logger.warn("Warn level - warning message");
        logger.error("Error level - error occurred");

        // Parameterized logging (efficient - no string concatenation if not logged)
        String user = "alice";
        int attempts = 3;
        logger.info("User {} logged in after {} attempts", user, attempts);
        // Output: [INFO] User alice logged in after 3 attempts

        // Log with exception
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Calculation failed", e);
            // Logs full stack trace
        }

        // Conditional logging
        if (logger.isDebugEnabled()) {
            String expensiveDebugInfo = computeExpensiveInfo();
            logger.debug("Debug info: {}", expensiveDebugInfo);
        }
    }

    static String computeExpensiveInfo() {
        return "expensive operation result";
    }
}
```

SLF4J is the industry standard logging facade in Java. It provides a simple API that works with various logging implementations (Logback, Log4j2). Use parameterized logging {} placeholders instead of string concatenation for performance. Always log exceptions with logger.error(message, exception).

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;  // Mapped Diagnostic Context

// Advanced example - Contextual logging with MDC
public class StructuredLoggingAdvanced {
    private static final Logger logger = LoggerFactory.getLogger(StructuredLoggingAdvanced.class);

    public static void main(String[] args) {
        // Add context to all log messages in this thread
        MDC.put("userId", "alice");
        MDC.put("requestId", "req-12345");
        MDC.put("sessionId", "sess-67890");

        processOrder("ORD-001", 99.99);
        processOrder("ORD-002", 149.99);

        // Clear context
        MDC.clear();

        // Demonstrate structured logging patterns
        demonstrateLoggingPatterns();
    }

    static void processOrder(String orderId, double amount) {
        MDC.put("orderId", orderId);

        logger.info("Processing order for ${}", amount);

        try {
            // Simulate order processing
            if (amount > 100) {
                logger.warn("High value order detected: ${}", amount);
            }

            // Simulate success
            logger.info("Order processed successfully");

        } catch (Exception e) {
            logger.error("Order processing failed for {}", orderId, e);
        } finally {
            MDC.remove("orderId");
        }
    }

    static void demonstrateLoggingPatterns() {
        logger.info("=== Logging Best Practices ===");

        // 1. Log method entry/exit (trace level)
        logger.trace("Entering demonstrateLoggingPatterns()");

        // 2. Log important state changes
        logger.info("Application state changed from STARTING to READY");

        // 3. Log performance metrics
        long startTime = System.currentTimeMillis();
        // ... operation ...
        long duration = System.currentTimeMillis() - startTime;
        logger.info("Operation completed in {}ms", duration);

        // 4. Log with multiple parameters
        logger.info("Transaction: id={}, user={}, amount={}, status={}",
                   "TXN-123", "alice", 99.99, "COMPLETED");

        // 5. Log arrays and collections
        String[] items = {"item1", "item2", "item3"};
        logger.debug("Processing items: {}", (Object) items);  // Cast to prevent varargs interpretation

        // 6. Structured exception logging
        try {
            riskyOperation();
        } catch (Exception e) {
            logger.error("Operation failed - context: user=alice, operation=riskyOperation", e);
        }

        // 7. Don't log sensitive data
        String password = "secret123";
        logger.info("User authenticated: username=alice");  // ✓ Good
        // logger.info("User: alice, password: {}", password);  // ✗ Bad - never log passwords

        logger.trace("Exiting demonstrateLoggingPatterns()");
    }

    static void riskyOperation() throws Exception {
        throw new Exception("Simulated failure");
    }
}

// Example logback.xml configuration:
/*
<?xml version="1.0" encoding="UTF-8"?>
<configuration>
    <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
        <encoder>
            <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg [userId=%X{userId}, requestId=%X{requestId}] %n</pattern>
        </encoder>
    </appender>

    <root level="INFO">
        <appender-ref ref="CONSOLE" />
    </root>
</configuration>
*/
```

**When to use**: All production applications, debugging, monitoring, and auditing. Use MDC (Mapped Diagnostic Context) to add contextual information (userId, requestId) that appears in all logs within a thread. Choose appropriate log levels: TRACE for detailed flow, DEBUG for diagnostics, INFO for significant events, WARN for recoverable issues, ERROR for failures. Never log sensitive data (passwords, credit cards, PII). Use parameterized logging for performance and security (prevents log injection).

**See Also**:

- [Recipe 41: Load Configuration](#recipe-41-load-configuration) - Config management
- [Recipe 17: Try-Catch with Resource Cleanup](#recipe-17-try-catch-with-resource-cleanup) - Resource cleanup

---

## Common Issues and Solutions

### Issue 1: NullPointerException

**Problem**: Your code crashes with `NullPointerException` at a specific line.

**Causes and Solutions**:

1. **Null object access**:

   ```java
   // ❌ Dangerous
   String name = user.getName();
   System.out.println(name.toUpperCase());  // Crashes if name is null

   // ✅ Safe
   String name = user.getName();
   if (name != null) {
       System.out.println(name.toUpperCase());
   }

   // ✅ Better: Use Optional
   user.getName()
       .map(String::toUpperCase)
       .ifPresent(System.out::println);
   ```

2. **Uninitialized collections**:

   ```java
   // ❌ Crashes
   List<String> items;
   items.add("test");  // NullPointerException!

   // ✅ Initialize first
   List<String> items = new ArrayList<>();
   items.add("test");
   ```

3. **Method returning null**:
   ```java
   // Check method documentation
   String result = someMethod();
   if (result != null) {
       // Use result
   }
   ```

### Issue 2: ClassNotFoundException

**Problem**: `ClassNotFoundException: com.example.MyClass`

**Causes and Solutions**:

1. **Typo in class name**:
   - Double-check fully qualified name
   - Include package name: `com.example.MyClass` not just `MyClass`

2. **Missing dependency**:

   ```bash
   # If using Maven, ensure pom.xml has the dependency
   # If using Gradle, ensure build.gradle has the dependency
   ```

3. **Running from wrong directory**:

   ```bash
   # ❌ Wrong
   cd src
   java MyClass

   # ✅ Correct (run from project root)
   java -cp bin com.example.MyClass
   ```

### Issue 3: ArrayIndexOutOfBoundsException

**Problem**: `ArrayIndexOutOfBoundsException: Index 10 out of bounds for length 5`

**Solutions**:

```java
// ❌ Dangerous
int[] numbers = {1, 2, 3, 4, 5};
System.out.println(numbers[10]);  // Crashes!

// ✅ Safe: Check length
int[] numbers = {1, 2, 3, 4, 5};
if (index >= 0 && index < numbers.length) {
    System.out.println(numbers[index]);
}

// ✅ Better: Use ArrayList
List<Integer> numbers = new ArrayList<>(Arrays.asList(1, 2, 3, 4, 5));
if (index >= 0 && index < numbers.size()) {
    System.out.println(numbers.get(index));
}
```

### Issue 4: ConcurrentModificationException

**Problem**: `ConcurrentModificationException` when modifying collection during iteration

**Solutions**:

```java
List<String> items = new ArrayList<>(Arrays.asList("a", "b", "c"));

// ❌ Crashes
for (String item : items) {
    items.remove(item);  // Don't modify while iterating!
}

// ✅ Use Iterator
Iterator<String> it = items.iterator();
while (it.hasNext()) {
    String item = it.next();
    it.remove();  // Safe way to remove
}

// ✅ Or create a copy first
List<String> toRemove = new ArrayList<>();
for (String item : items) {
    if (condition) toRemove.add(item);
}
items.removeAll(toRemove);

// ✅ Or use streams
items = items.stream()
    .filter(item -> !condition)
    .collect(Collectors.toList());
```

### Issue 5: String Index Out Of Bounds

**Problem**: `StringIndexOutOfBoundsException` when accessing string characters

**Solutions**:

```java
String text = "Hello";

// ❌ Crashes
char c = text.charAt(10);  // String only has 5 chars

// ✅ Check length
if (index >= 0 && index < text.length()) {
    char c = text.charAt(index);
}

// ✅ Use substring safely
if (endIndex <= text.length()) {
    String sub = text.substring(0, endIndex);
}
```

### Issue 6: NumberFormatException

**Problem**: `NumberFormatException: For input string: "abc"`

**Solutions**:

```java
// ❌ Crashes
int num = Integer.parseInt("abc");

// ✅ Validate first
String input = "abc";
try {
    int num = Integer.parseInt(input);
    System.out.println(num);
} catch (NumberFormatException e) {
    System.out.println("Invalid number: " + input);
}

// ✅ Check if parseable
if (input.matches("-?\\d+")) {
    int num = Integer.parseInt(input);
}
```

### Issue 7: OutOfMemoryError

**Problem**: `OutOfMemoryError: Java heap space`

**Causes and Solutions**:

1. **Memory leak** (holding references):

   ```java
   // ❌ Creates millions of objects
   List<byte[]> largeData = new ArrayList<>();
   for (int i = 0; i < 1000000; i++) {
       largeData.add(new byte[10000]);  // Never cleaned up!
   }

   // ✅ Process in chunks
   for (int i = 0; i < 1000000; i++) {
       byte[] data = new byte[10000];
       processData(data);
       // data is eligible for garbage collection
   }
   ```

2. **Increase heap size**:

   ```bash
   # Increase heap to 1GB
   java -Xmx1G MyApp
   ```

3. **Check for resource leaks**:

   ```java
   // ❌ Resource not closed
   FileInputStream fis = new FileInputStream("file.txt");

   // ✅ Always close resources
   try (FileInputStream fis = new FileInputStream("file.txt")) {
       // Use fis
   }  // Automatically closed
   ```

### Issue 8: StackOverflowError

**Problem**: `StackOverflowError` - stack overflow

**Causes**:

1. **Infinite recursion**:

   ```java
   // ❌ Infinite recursion
   public int factorial(int n) {
       return n * factorial(n);  // Never stops!
   }

   // ✅ Base case required
   public int factorial(int n) {
       if (n <= 1) return 1;  // Base case
       return n * factorial(n - 1);
   }
   ```

### Issue 9: FileNotFoundException

**Problem**: `FileNotFoundException: file.txt`

**Solutions**:

```java
// ❌ Crashes if file doesn't exist
FileInputStream fis = new FileInputStream("file.txt");

// ✅ Check if file exists
File file = new File("file.txt");
if (file.exists()) {
    FileInputStream fis = new FileInputStream(file);
}

// ✅ Better: Use try-catch
try {
    FileInputStream fis = new FileInputStream("file.txt");
} catch (FileNotFoundException e) {
    System.out.println("File not found: " + e.getMessage());
}

// ✅ Check classpath for resources
InputStream is = getClass().getResourceAsStream("/file.txt");
```

### Issue 10: Type Casting Exception

**Problem**: `ClassCastException: String cannot be cast to Integer`

**Solutions**:

```java
// ❌ Crashes if object isn't really an Integer
Object obj = "123";
Integer num = (Integer) obj;  // ClassCastException!

// ✅ Use instanceof
Object obj = "123";
if (obj instanceof Integer) {
    Integer num = (Integer) obj;
}

// ✅ Safer with collections (use generics)
List<String> items = new ArrayList<>();
// Compiler prevents adding non-Strings

// ❌ Avoid raw types
List list = new ArrayList();  // Raw type - unsafe!

// ✅ Use generics
List<String> list = new ArrayList<>();
```

---

## Performance Tips

1. **String concatenation in loops**:

   ```java
   // ❌ Slow (creates new String each iteration)
   String result = "";
   for (int i = 0; i < 1000; i++) {
       result += i;
   }

   // ✅ Fast (uses StringBuilder)
   StringBuilder sb = new StringBuilder();
   for (int i = 0; i < 1000; i++) {
       sb.append(i);
   }
   String result = sb.toString();
   ```

2. **Avoid repeated method calls**:

   ```java
   // ❌ Slow
   for (int i = 0; i < list.size(); i++) {  // size() called every iteration
       // ...
   }

   // ✅ Fast
   int size = list.size();
   for (int i = 0; i < size; i++) {
       // ...
   }
   ```

3. **Use appropriate collections**:
   - **ArrayList**: Fast random access, slow insertion
   - **LinkedList**: Fast insertion, slow random access
   - **HashSet**: Fast lookup and insertion
   - **HashMap**: Fast key-value lookup

---

## Final Tips

- **When stuck**: Search this cookbook or Stack Overflow
- **Before copying**: Understand what the code does
- **After copying**: Adapt to your specific context
- **Test**: Always test before using in production
- **Refactor**: Extract repeated recipes into utility classes
- **Troubleshoot**: Check the "Common Issues" section first
- **Read stack traces**: They tell you exactly what went wrong and where

---

## Practice Exercises

Test your understanding of the cookbook recipes with these hands-on challenges.

### Exercise 1: User Management System

Build a user management system that combines multiple recipes.

**Requirements:**

- Load configuration from `users-config.properties` (Recipe 41)
- Create a User class with Builder pattern (Recipe 36)
- Store users in a thread-safe collection (Recipe 26)
- Add structured logging for all operations (Recipe 42)
- Serialize users to JSON for persistence (Recipe 32)

**Bonus:**

- Use Factory pattern to create different user types (Recipe 38)
- Implement singleton ConfigManager (Recipe 39)
- Add validation with Optional for optional fields (Recipe 37)

### Exercise 2: File Processing Pipeline

Create a file processing application.

**Requirements:**

- Read all text files from `input/` directory (Recipe 10)
- Process each file with Stream API operations (Recipe 27)
- Copy processed files to `output/` directory (Recipe 40)
- Handle exceptions properly with try-catch (Recipe 17)
- Log processing progress (Recipe 42)

**Bonus:**

- Create directories with proper structure (Recipe 13)
- Parse JSON configuration files (Recipe 33)
- Use CompletableFuture for parallel processing (Recipe 23)

### Exercise 3: REST API Client

Build a simple REST API client application.

**Requirements:**

- Make HTTP GET request to fetch users (Recipe 34)
- Parse JSON response to User objects (Recipe 33)
- Make HTTP POST request to create new user (Recipe 35)
- Convert User object to JSON (Recipe 32)
- Add error handling for network failures (Recipe 17)

**Bonus:**

- Use Builder pattern for request configuration (Recipe 36)
- Add retry logic with exponential backoff
- Implement caching with Singleton pattern (Recipe 39)

### Exercise 4: Data Transformation Tool

Create a tool that transforms and filters data.

**Requirements:**

- Read CSV data from file (Recipe 10)
- Parse and filter data with Streams (Recipe 1, Recipe 27)
- Group data by categories (Recipe 27 - groupingBy)
- Format output with String operations (Recipes 5-9)
- Write results to new file (Recipe 11)

**Bonus:**

- Use Functional Interfaces for custom filters (Recipe 28)
- Add date/time formatting for timestamps (Recipe 14)
- Implement sorting with Comparator (Recipe 2)

### Exercise 5: Configuration-Driven Application

Build an application driven by external configuration.

**Requirements:**

- Load configuration from multiple sources (Recipe 41)
- Use configuration to control application behavior
- Implement different strategies based on config (Factory pattern - Recipe 38)
- Log configuration changes and decisions (Recipe 42)
- Support feature flags (boolean config values)

**Bonus:**

- Reload configuration without restart
- Validate configuration on startup
- Use environment variables for sensitive data

---

**Happy Cooking!** Use this cookbook as your daily reference for common Java problems and solutions.
