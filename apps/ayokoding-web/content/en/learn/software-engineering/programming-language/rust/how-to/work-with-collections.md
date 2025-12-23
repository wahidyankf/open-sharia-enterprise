---
title: Work with Collections
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000006
description: Practical guide to using Vec, HashMap, HashSet, and other Rust collections
tags:
  [
    "rust",
    "how-to",
    "collections",
    "vec",
    "hashmap",
    "hashset",
    "btree",
    "iterators",
  ]
---

**Need to work with collections in Rust?** This guide covers vectors, hash maps, sets, and common collection operations with practical examples.

## Problem: Creating and Using Vectors

### Scenario

You need a growable array.

### Solution: Use Vec<T>

**Create vectors**:

```rust
// Empty vector
let mut v1: Vec<i32> = Vec::new();

// With initial capacity
let mut v2 = Vec::with_capacity(10);

// Using vec! macro
let v3 = vec![1, 2, 3, 4, 5];

// From iterator
let v4: Vec<i32> = (0..5).collect();
```

**Add elements**:

```rust
let mut v = Vec::new();
v.push(1);
v.push(2);
v.push(3);
```

**Access elements**:

```rust
let v = vec![1, 2, 3, 4, 5];

// Indexing (panics if out of bounds)
let third = v[2];

// Using get (returns Option)
match v.get(2) {
    Some(third) => println!("Third: {}", third),
    None => println!("No third element"),
}
```

**Iterate**:

```rust
let v = vec![1, 2, 3];

// Immutable iteration
for item in &v {
    println!("{}", item);
}

// Mutable iteration
for item in &mut v {
    *item += 1;
}

// Consuming iteration
for item in v {
    println!("{}", item);
}
// v is no longer valid
```

---

## Problem: Working with Hash Maps

### Scenario

You need key-value storage.

### Solution: Use HashMap<K, V>

```rust
use std::collections::HashMap;

// Create hash map
let mut scores = HashMap::new();

// Insert values
scores.insert(String::from("Blue"), 10);
scores.insert(String::from("Yellow"), 50);

// Access values
let team = String::from("Blue");
let score = scores.get(&team).copied().unwrap_or(0);

// Iterate
for (key, value) in &scores {
    println!("{}: {}", key, value);
}
```

**Update values**:

```rust
let mut scores = HashMap::new();

// Overwrite value
scores.insert("Blue".to_string(), 10);
scores.insert("Blue".to_string(), 25);  // Now 25

// Only insert if key doesn't exist
scores.entry("Yellow".to_string()).or_insert(50);
scores.entry("Yellow".to_string()).or_insert(100);  // Still 50

// Update based on old value
let text = "hello world wonderful world";
let mut map = HashMap::new();

for word in text.split_whitespace() {
    let count = map.entry(word).or_insert(0);
    *count += 1;
}
// map: {"hello": 1, "world": 2, "wonderful": 1}
```

**Create from iterator**:

```rust
let teams = vec!["Blue", "Yellow"];
let initial_scores = vec![10, 50];

let scores: HashMap<_, _> = teams
    .iter()
    .zip(initial_scores.iter())
    .collect();
```

---

## Problem: Using Sets for Unique Values

### Scenario

You need to store unique values.

### Solution: Use HashSet<T>

```rust
use std::collections::HashSet;

let mut set = HashSet::new();

// Insert values (returns false if already exists)
set.insert(1);
set.insert(2);
set.insert(2);  // Duplicate, returns false

// Check existence
if set.contains(&1) {
    println!("Set contains 1");
}

// Remove
set.remove(&1);

// Iterate
for item in &set {
    println!("{}", item);
}
```

**Set operations**:

```rust
let a: HashSet<_> = vec![1, 2, 3, 4].into_iter().collect();
let b: HashSet<_> = vec![3, 4, 5, 6].into_iter().collect();

// Union
let union: HashSet<_> = a.union(&b).collect();
// {1, 2, 3, 4, 5, 6}

// Intersection
let intersection: HashSet<_> = a.intersection(&b).collect();
// {3, 4}

// Difference
let difference: HashSet<_> = a.difference(&b).collect();
// {1, 2}

// Symmetric difference
let sym_diff: HashSet<_> = a.symmetric_difference(&b).collect();
// {1, 2, 5, 6}
```

---

## Problem: Maintaining Sorted Data

### Scenario

You need collections that maintain sort order.

### Solution: Use BTreeMap and BTreeSet

**BTreeMap** (sorted key-value pairs):

```rust
use std::collections::BTreeMap;

let mut map = BTreeMap::new();
map.insert(3, "c");
map.insert(1, "a");
map.insert(2, "b");

// Iterates in key order
for (key, value) in &map {
    println!("{}: {}", key, value);
}
// Output: 1: a, 2: b, 3: c
```

**BTreeSet** (sorted unique values):

```rust
use std::collections::BTreeSet;

let mut set = BTreeSet::new();
set.insert(3);
set.insert(1);
set.insert(2);

// Iterates in sorted order
for item in &set {
    println!("{}", item);
}
// Output: 1, 2, 3
```

**Range queries**:

```rust
use std::collections::BTreeMap;

let mut map = BTreeMap::new();
for i in 1..=10 {
    map.insert(i, i * 10);
}

// Get range
for (key, value) in map.range(3..=7) {
    println!("{}: {}", key, value);
}
// Output: 3: 30, 4: 40, 5: 50, 6: 60, 7: 70
```

---

## Problem: Removing Duplicates from Vec

### Scenario

You have a vector with duplicates to remove.

### Solution 1: Use HashSet

```rust
use std::collections::HashSet;

let numbers = vec![1, 2, 2, 3, 4, 4, 5];
let unique: Vec<_> = numbers.into_iter().collect::<HashSet<_>>().into_iter().collect();
// Order not preserved
```

### Solution 2: Preserve Order

```rust
use std::collections::HashSet;

let numbers = vec![1, 2, 2, 3, 4, 4, 5];
let mut seen = HashSet::new();
let unique: Vec<_> = numbers.into_iter()
    .filter(|x| seen.insert(*x))
    .collect();
// Order preserved: [1, 2, 3, 4, 5]
```

### Solution 3: Sort and Dedup

```rust
let mut numbers = vec![1, 2, 2, 3, 4, 4, 5];
numbers.sort();
numbers.dedup();
// numbers: [1, 2, 3, 4, 5]
```

---

## Problem: Transforming Collections

### Scenario

You need to transform elements in a collection.

### Solution: Use Iterator Methods

**map - Transform each element**:

```rust
let numbers = vec![1, 2, 3, 4, 5];
let squared: Vec<_> = numbers.iter()
    .map(|x| x * x)
    .collect();
// [1, 4, 9, 16, 25]
```

**filter - Keep only matching elements**:

```rust
let numbers = vec![1, 2, 3, 4, 5];
let evens: Vec<_> = numbers.iter()
    .filter(|x| *x % 2 == 0)
    .collect();
// [2, 4]
```

**filter_map - Filter and transform**:

```rust
let strings = vec!["1", "two", "3", "four", "5"];
let numbers: Vec<i32> = strings.iter()
    .filter_map(|s| s.parse().ok())
    .collect();
// [1, 3, 5]
```

**fold - Reduce to single value**:

```rust
let numbers = vec![1, 2, 3, 4, 5];
let sum = numbers.iter().fold(0, |acc, x| acc + x);
// 15

let product = numbers.iter().fold(1, |acc, x| acc * x);
// 120
```

---

## Problem: Flattening Nested Collections

### Scenario

You have nested collections to flatten.

### Solution: Use flat_map or flatten

```rust
let nested = vec![vec![1, 2], vec![3, 4], vec![5, 6]];

// Using flatten
let flat: Vec<_> = nested.iter()
    .flatten()
    .collect();
// [1, 2, 3, 4, 5, 6]

// Using flat_map
let flat: Vec<_> = nested.iter()
    .flat_map(|v| v.iter())
    .collect();
// [1, 2, 3, 4, 5, 6]
```

**With transformation**:

```rust
let numbers = vec![1, 2, 3];
let repeated: Vec<_> = numbers.iter()
    .flat_map(|x| vec![*x, *x])
    .collect();
// [1, 1, 2, 2, 3, 3]
```

---

## Problem: Grouping Elements

### Scenario

You want to group elements by some criterion.

### Solution: Use HashMap with entry API

```rust
use std::collections::HashMap;

let words = vec!["apple", "banana", "apricot", "blueberry", "cherry"];

let mut grouped: HashMap<char, Vec<&str>> = HashMap::new();

for word in words {
    let first_char = word.chars().next().unwrap();
    grouped.entry(first_char)
        .or_insert_with(Vec::new)
        .push(word);
}

// grouped: {'a': ["apple", "apricot"], 'b': ["banana", "blueberry"], 'c': ["cherry"]}
```

---

## Problem: Finding Elements

### Scenario

You need to find specific elements in a collection.

### Solution: Use Iterator Methods

**find - First matching element**:

```rust
let numbers = vec![1, 2, 3, 4, 5];
let first_even = numbers.iter().find(|x| *x % 2 == 0);
// Some(2)
```

**position - Index of first match**:

```rust
let numbers = vec![1, 2, 3, 4, 5];
let pos = numbers.iter().position(|x| *x == 3);
// Some(2)
```

**any - Check if any element matches**:

```rust
let numbers = vec![1, 2, 3, 4, 5];
let has_even = numbers.iter().any(|x| x % 2 == 0);
// true
```

**all - Check if all elements match**:

```rust
let numbers = vec![2, 4, 6, 8];
let all_even = numbers.iter().all(|x| x % 2 == 0);
// true
```

---

## Problem: Partitioning Collections

### Scenario

You want to split a collection into two groups.

### Solution: Use partition

```rust
let numbers = vec![1, 2, 3, 4, 5, 6];
let (evens, odds): (Vec<_>, Vec<_>) = numbers.into_iter()
    .partition(|x| x % 2 == 0);
// evens: [2, 4, 6], odds: [1, 3, 5]
```

---

## Problem: Chunking Collections

### Scenario

You need to process data in chunks.

### Solution: Use chunks or windows

**chunks - Non-overlapping chunks**:

```rust
let numbers = vec![1, 2, 3, 4, 5, 6, 7];

for chunk in numbers.chunks(3) {
    println!("{:?}", chunk);
}
// [1, 2, 3]
// [4, 5, 6]
// [7]
```

**windows - Sliding windows**:

```rust
let numbers = vec![1, 2, 3, 4, 5];

for window in numbers.windows(3) {
    println!("{:?}", window);
}
// [1, 2, 3]
// [2, 3, 4]
// [3, 4, 5]
```

---

## Problem: Combining Collections

### Scenario

You need to combine multiple collections.

### Solution: Use chain

```rust
let v1 = vec![1, 2, 3];
let v2 = vec![4, 5, 6];

let combined: Vec<_> = v1.iter()
    .chain(v2.iter())
    .collect();
// [1, 2, 3, 4, 5, 6]
```

**Zip - Pair elements**:

```rust
let names = vec!["Alice", "Bob", "Carol"];
let scores = vec![90, 85, 95];

let pairs: Vec<_> = names.iter()
    .zip(scores.iter())
    .collect();
// [("Alice", 90), ("Bob", 85), ("Carol", 95)]
```

---

## Problem: Efficient String Collection

### Scenario

You need to efficiently collect strings.

### Solution: Use join

```rust
let words = vec!["Hello", "world", "from", "Rust"];

let sentence = words.join(" ");
// "Hello world from Rust"

let csv = words.join(",");
// "Hello,world,from,Rust"
```

**Building strings from iterator**:

```rust
let numbers = vec![1, 2, 3, 4, 5];
let text: String = numbers.iter()
    .map(|n| n.to_string())
    .collect::<Vec<_>>()
    .join(", ");
// "1, 2, 3, 4, 5"
```

---

## Common Pitfalls

### Pitfall 1: Unnecessary Clones

**Problem**: Cloning when borrowing would work.

```rust
// Bad
let v = vec![1, 2, 3];
let sum: i32 = v.clone().iter().sum();

// Good
let sum: i32 = v.iter().sum();
```

### Pitfall 2: Not Using Iterators

**Problem**: Manual loops instead of iterator methods.

```rust
// Bad
let mut result = Vec::new();
for item in &vec {
    if item % 2 == 0 {
        result.push(item * 2);
    }
}

// Good
let result: Vec<_> = vec.iter()
    .filter(|x| *x % 2 == 0)
    .map(|x| x * 2)
    .collect();
```

### Pitfall 3: HashMap with Non-Hash Types

**Problem**: Using types that don't implement Hash.

```Solution**: Use BTreeMap for non-Hash types or implement Hash.

---

## Related Resources

- [Tutorials: Beginner](/en/learn/software-engineering/programming-language/rust/tutorials/beginner) - Collections fundamentals
- [Cookbook](/en/learn/software-engineering/programming-language/rust/how-to/cookbook) - Collection recipes
- [Best Practices](/en/learn/software-engineering/programming-language/rust/explanation/best-practices) - Idiomatic collection usage
- [Cheat Sheet](/en/learn/software-engineering/programming-language/rust/reference/cheat-sheet) - Collection syntax reference

---

**Master Rust collections for effective data manipulation!**
```
