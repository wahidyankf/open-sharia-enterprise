---
title: Stream API Advanced Patterns
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 626
description: Master advanced Stream API patterns for data processing, custom collectors, and parallel streams
tags: ["java", "streams", "functional-programming", "collectors"]
---

## Custom Collectors

### Basic Custom Collector

```java
import java.util.*;
import java.util.function.*;
import java.util.stream.*;

// Collector that joins strings with custom delimiter
public class CustomJoiningCollector implements Collector<String, StringBuilder, String> {
    private final String delimiter;
    private final String prefix;
    private final String suffix;

    public CustomJoiningCollector(String delimiter, String prefix, String suffix) {
        this.delimiter = delimiter;
        this.prefix = prefix;
        this.suffix = suffix;
    }

    @Override
    public Supplier<StringBuilder> supplier() {
        return () -> new StringBuilder(prefix);
    }

    @Override
    public BiConsumer<StringBuilder, String> accumulator() {
        return (sb, s) -> {
            if (sb.length() > prefix.length()) {
                sb.append(delimiter);
            }
            sb.append(s);
        };
    }

    @Override
    public BinaryOperator<StringBuilder> combiner() {
        return (sb1, sb2) -> {
            if (sb1.length() > prefix.length()) {
                sb1.append(delimiter);
            }
            sb1.append(sb2.substring(prefix.length()));
            return sb1;
        };
    }

    @Override
    public Function<StringBuilder, String> finisher() {
        return sb -> sb.append(suffix).toString();
    }

    @Override
    public Set<Characteristics> characteristics() {
        return Collections.emptySet();
    }
}

// Usage
String result = list.stream()
    .collect(new CustomJoiningCollector(", ", "[", "]"));
```

### Immutable Collection Collector

```java
public class ImmutableListCollector<T> implements Collector<T, List<T>, List<T>> {
    @Override
    public Supplier<List<T>> supplier() {
        return ArrayList::new;
    }

    @Override
    public BiConsumer<List<T>, T> accumulator() {
        return List::add;
    }

    @Override
    public BinaryOperator<List<T>> combiner() {
        return (list1, list2) -> {
            list1.addAll(list2);
            return list1;
        };
    }

    @Override
    public Function<List<T>, List<T>> finisher() {
        return Collections::unmodifiableList;
    }

    @Override
    public Set<Characteristics> characteristics() {
        return Set.of(Characteristics.UNORDERED);
    }
}

// Usage
List<String> immutableList = stream.collect(new ImmutableListCollector<>());
```

## Advanced Grouping and Partitioning

### Multi-Level Grouping

```java
// Group by multiple criteria
Map<String, Map<Integer, List<Person>>> grouped = people.stream()
    .collect(Collectors.groupingBy(
        Person::getCity,
        Collectors.groupingBy(Person::getAge)
    ));

// Access nested groups
List<Person> peopleInNYAge30 = grouped.get("New York").get(30);

// Group and count
Map<String, Long> cityCounts = people.stream()
    .collect(Collectors.groupingBy(
        Person::getCity,
        Collectors.counting()
    ));

// Group and sum
Map<String, Integer> totalAgeByCity = people.stream()
    .collect(Collectors.groupingBy(
        Person::getCity,
        Collectors.summingInt(Person::getAge)
    ));

// Group and collect to set
Map<String, Set<String>> namesByCity = people.stream()
    .collect(Collectors.groupingBy(
        Person::getCity,
        Collectors.mapping(Person::getName, Collectors.toSet())
    ));
```

### Custom Partitioning

```java
// Partition by predicate
Map<Boolean, List<Integer>> partitioned = numbers.stream()
    .collect(Collectors.partitioningBy(n -> n % 2 == 0));

List<Integer> evens = partitioned.get(true);
List<Integer> odds = partitioned.get(false);

// Partition with downstream collector
Map<Boolean, Long> counts = numbers.stream()
    .collect(Collectors.partitioningBy(
        n -> n % 2 == 0,
        Collectors.counting()
    ));

// Partition and transform
Map<Boolean, Set<Integer>> sets = numbers.stream()
    .collect(Collectors.partitioningBy(
        n -> n > 50,
        Collectors.toSet()
    ));
```

## FlatMap Patterns

### Flattening Nested Collections

```java
// Flatten list of lists
List<List<String>> nested = Arrays.asList(
    Arrays.asList("a", "b"),
    Arrays.asList("c", "d"),
    Arrays.asList("e", "f")
);

List<String> flattened = nested.stream()
    .flatMap(List::stream)
    .collect(Collectors.toList());
// ["a", "b", "c", "d", "e", "f"]

// Flatten optional results
List<Optional<String>> optionals = Arrays.asList(
    Optional.of("one"),
    Optional.empty(),
    Optional.of("three")
);

List<String> present = optionals.stream()
    .flatMap(Optional::stream)  // Java 9+
    .collect(Collectors.toList());
// ["one", "three"]

// Flatten object hierarchies
class Department {
    private List<Employee> employees;
    public List<Employee> getEmployees() { return employees; }
}

List<Employee> allEmployees = departments.stream()
    .flatMap(dept -> dept.getEmployees().stream())
    .collect(Collectors.toList());
```

### Processing Files and Lines

```java
// Read and process all lines from multiple files
List<String> allLines = filePaths.stream()
    .flatMap(path -> {
        try {
            return Files.lines(path);
        } catch (IOException e) {
            return Stream.empty();
        }
    })
    .collect(Collectors.toList());

// Split and flatten
List<String> words = sentences.stream()
    .flatMap(sentence -> Arrays.stream(sentence.split("\\s+")))
    .collect(Collectors.toList());
```

## Parallel Streams

### When to Use Parallel Streams

```java
// ✅ Good: Large datasets with independent operations
List<Integer> largeList = IntStream.range(0, 1_000_000)
    .boxed()
    .collect(Collectors.toList());

long sum = largeList.parallelStream()
    .mapToLong(Integer::longValue)
    .sum();

// ❌ Bad: Small datasets (overhead > benefit)
List<Integer> smallList = Arrays.asList(1, 2, 3, 4, 5);
int sum = smallList.parallelStream()  // Unnecessary parallelism
    .mapToInt(Integer::intValue)
    .sum();

// ❌ Bad: Operations with side effects
List<Integer> results = new ArrayList<>();  // Not thread-safe
numbers.parallelStream()
    .forEach(n -> results.add(n * 2));  // Race condition!

// ✅ Good: Use collect instead
List<Integer> results = numbers.parallelStream()
    .map(n -> n * 2)
    .collect(Collectors.toList());
```

### Controlling Parallelism

```java
// Default: Uses common ForkJoinPool
List<String> parallel = list.parallelStream()
    .map(String::toUpperCase)
    .collect(Collectors.toList());

// Custom ForkJoinPool (Java 8+)
ForkJoinPool customPool = new ForkJoinPool(4);
List<String> result = customPool.submit(() ->
    list.parallelStream()
        .map(String::toUpperCase)
        .collect(Collectors.toList())
).join();

// Sequential fallback
List<String> result = list.stream()
    .parallel()  // Enable parallelism
    .map(this::expensiveOperation)
    .sequential()  // Back to sequential
    .collect(Collectors.toList());
```

## Performance Optimization

### Lazy Evaluation

```java
// Intermediate operations are lazy
Stream<String> stream = list.stream()
    .filter(s -> {
        System.out.println("Filtering: " + s);
        return s.length() > 3;
    })
    .map(s -> {
        System.out.println("Mapping: " + s);
        return s.toUpperCase();
    });
// Nothing printed yet!

// Terminal operation triggers evaluation
List<String> result = stream.collect(Collectors.toList());
// Now filtering and mapping executed

// Short-circuiting terminal operations
boolean anyMatch = list.stream()
    .peek(s -> System.out.println("Checking: " + s))
    .anyMatch(s -> s.startsWith("A"));
// Stops as soon as match found
```

### Avoiding Unnecessary Operations

```java
// ❌ Bad: Multiple traversals
long count = list.stream().filter(s -> s.length() > 3).count();
List<String> filtered = list.stream().filter(s -> s.length() > 3).collect(Collectors.toList());

// ✅ Good: Single traversal
List<String> filtered = list.stream()
    .filter(s -> s.length() > 3)
    .collect(Collectors.toList());
long count = filtered.size();

// ❌ Bad: Creating unnecessary objects
String result = list.stream()
    .map(String::toUpperCase)
    .map(String::trim)
    .collect(Collectors.joining(", "));

// ✅ Good: Combine operations
String result = list.stream()
    .map(s -> s.toUpperCase().trim())
    .collect(Collectors.joining(", "));
```

## Complex Stream Patterns

### Stream of Streams

```java
// Process matrix (2D array)
int[][] matrix = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};

List<Integer> flattened = Arrays.stream(matrix)
    .flatMapToInt(Arrays::stream)
    .boxed()
    .collect(Collectors.toList());

// Find max in each row
List<Integer> maxPerRow = Arrays.stream(matrix)
    .map(row -> Arrays.stream(row).max().orElse(0))
    .collect(Collectors.toList());
```

### Conditional Processing

```java
// Process only if condition met
Stream<String> conditionalStream = condition
    ? list.stream().filter(s -> s.startsWith("A"))
    : list.stream();

List<String> result = conditionalStream.collect(Collectors.toList());

// Or using Optional
Optional.of(list)
    .filter(l -> !l.isEmpty())
    .map(List::stream)
    .orElse(Stream.empty())
    .forEach(System.out::println);
```

### Windowing and Batching

```java
// Batch elements into groups of N
public static <T> Stream<List<T>> batched(Stream<T> stream, int batchSize) {
    Iterator<T> iterator = stream.iterator();
    return Stream.generate(() -> {
        List<T> batch = new ArrayList<>(batchSize);
        for (int i = 0; i < batchSize && iterator.hasNext(); i++) {
            batch.add(iterator.next());
        }
        return batch;
    })
    .takeWhile(batch -> !batch.isEmpty());
}

// Usage
List<List<Integer>> batches = batched(
    IntStream.range(0, 100).boxed(),
    10
).collect(Collectors.toList());
// [[0-9], [10-19], ..., [90-99]]

// Sliding window
public static <T> Stream<List<T>> windowed(List<T> list, int windowSize) {
    return IntStream.rangeClosed(0, list.size() - windowSize)
        .mapToObj(start -> list.subList(start, start + windowSize));
}

List<List<Integer>> windows = windowed(
    Arrays.asList(1, 2, 3, 4, 5),
    3
).collect(Collectors.toList());
// [[1,2,3], [2,3,4], [3,4,5]]
```

## Error Handling in Streams

### Checked Exceptions

```java
// ❌ Won't compile - checked exception in lambda
stream.map(file -> Files.readString(file));  // IOException

// ✅ Wrap in unchecked exception
stream.map(file -> {
    try {
        return Files.readString(file);
    } catch (IOException e) {
        throw new UncheckedIOException(e);
    }
});

// ✅ Use Optional for error handling
stream.map(file -> {
    try {
        return Optional.of(Files.readString(file));
    } catch (IOException e) {
        return Optional.empty();
    }
})
.flatMap(Optional::stream)
.collect(Collectors.toList());

// ✅ Extract to method
private String readFileSafe(Path file) {
    try {
        return Files.readString(file);
    } catch (IOException e) {
        throw new UncheckedIOException(e);
    }
}

stream.map(this::readFileSafe).collect(Collectors.toList());
```

### Filtering Errors

```java
// Process only successful results
List<Result<String>> results = items.stream()
    .map(this::processItem)
    .filter(Result::isSuccess)
    .collect(Collectors.toList());

// Partition success and failures
Map<Boolean, List<Result<String>>> partitioned = items.stream()
    .map(this::processItem)
    .collect(Collectors.partitioningBy(Result::isSuccess));

List<Result<String>> successes = partitioned.get(true);
List<Result<String>> failures = partitioned.get(false);
```

## Primitive Streams

### IntStream, LongStream, DoubleStream

```java
// Create primitive streams
IntStream ints = IntStream.range(0, 10);  // 0 to 9
IntStream intsInclusive = IntStream.rangeClosed(0, 10);  // 0 to 10
LongStream longs = LongStream.range(0L, 1000000L);
DoubleStream doubles = DoubleStream.of(1.5, 2.7, 3.9);

// Statistics
IntSummaryStatistics stats = IntStream.range(0, 100).summaryStatistics();
System.out.println("Count: " + stats.getCount());
System.out.println("Sum: " + stats.getSum());
System.out.println("Min: " + stats.getMin());
System.out.println("Max: " + stats.getMax());
System.out.println("Average: " + stats.getAverage());

// Convert between primitive and object streams
Stream<Integer> boxed = IntStream.range(0, 10).boxed();
IntStream unboxed = Stream.of(1, 2, 3).mapToInt(Integer::intValue);

// Specialized operations
int sum = IntStream.range(0, 100).sum();
OptionalDouble avg = IntStream.range(0, 100).average();
OptionalInt max = IntStream.range(0, 100).max();
```

## Related Resources

**Learn more**:

- [Cheat Sheet](/en/learn/swe/prog-lang/java/reference/cheat-sheet) - Stream API reference
- [Glossary](/en/learn/swe/prog-lang/java/reference/glossary) - Stream terminology

**Related guides**:

- [Use Collections Effectively](/en/learn/swe/prog-lang/java/how-to/use-collections-effectively) - Collection patterns
- [Cookbook](/en/learn/swe/prog-lang/java/how-to/cookbook) - Practical examples
