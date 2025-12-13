---
title: "Collections: Maps"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/collections_maps).

</aside>

## Overview

From [https://clojure.org/reference/data_structures#Maps](https://clojure.org/reference/data_structures#Maps)

> A Map is a collection that maps keys to values. Two different map types are provided - hashed and sorted. Hash maps require keys that correctly support hashCode and equals. Sorted maps require keys that implement Comparable, or an instance of Comparator. Hash maps provide faster access (log32N hops) vs (logN hops), but sorted maps are, well, sorted. [count](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count) is O(1). [conj](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/conj) expects another (possibly single entry) map as the item, and returns a new map which is the old map plus the entries from the new, which may overwrite the old entries. [conj](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/conj) also accepts a MapEntry or a vector of two items (key and value). [seq](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/seq) returns a sequence of map entries, which are key/value pairs. Sorted map also supports [rseq](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/rseq), which returns the entries in reverse order. Maps implement IFn, for invoke() of one argument (a key) with an optional second argument (a default value), i.e. maps are functions of their keys. nil keys and values are ok.

## Maps Creation

### Code

```clojure
(ns collections-maps.core)

;; ---
;; Maps creation
;; ---

{:a 1, :b 2, :c 3, :d 4, :e 5}
;; => {:a 1, :b 2, :c 3, :d 4, :e 5}
{:a 1, :b "b", :c 3, :d "d"}
;; => {:a 1, :b "b", :c 3, :d "d"}
{:a 1, "b" "c", 2 "b", :d "d"}
;; => {:a 1, "b" "c", 2 "b", :d "d"}

(class {:a 1 :b 2 :c 3 :d 4 :e 5})
;; => clojure.lang.PersistentArrayMap

(hash-map :a 1 :b 2 :c 3 :d 4 :e 5)
;; => {:e 5, :c 3, :b 2, :d 4, :a 1}

(into {} [[:a 1] [:b 2] [:c 3] [:d 4] [:e 5]])
;; => {:a 1, :b 2, :c 3, :d 4, :e 5}

(= {:a 1, :b 2, :c 3, :d 4, :e 5} (hash-map :a 1 :b 2 :c 3 :d 4 :e 5))
;; => true

(assoc {:a 1 :b 2 :c 3 :d 4} :e 5)
;; => {:a 1, :b 2, :c 3, :d 4, :e 5}
(assoc {:a 1 :b 2 :c 3 :d 4} :a 5)
;; => {:a 5, :b 2, :c 3, :d 4}
(assoc {} :a 1 :b 2 :c 3 :d 4 :e 5)
;; => {:a 1, :b 2, :c 3, :d 4, :e 5}

(dissoc {:a 1 :b 2 :c 3 :d 4 :e 5} :a)
;; => {:b 2, :c 3, :d 4, :e 5}
(dissoc {:a 1 :b 2 :c 3 :d 4 :e 5} :f)
;; => {:a 1, :b 2, :c 3, :d 4, :e 5}

(conj {:a 1, :b 2, :c 3, :d 4} [:e 5])
;; => {:a 1, :b 2, :c 3, :d 4, :e 5}
```

### Explanation

Sure, here is a line-by-line explanation of the Clojure code provided:

1. `(ns collections-maps.core)` - This line defines a new namespace called `collections-maps.core`. Namespaces in Clojure are used to group related functions and data together.
2. `{:a 1, :b 2, :c 3, :d 4, :e 5}` - This is a literal syntax for creating a Clojure map, a collection of key-value pairs. Here, keywords `:a`, `:b`, `:c`, `:d`, and `:e` are the keys and `1`, `2`, `3`, `4`, `5` are the corresponding values.
3. `{:a 1, :b "b", :c 3, :d "d"}` - Another example of a map. This map contains keys of mixed types - keywords and numbers, and values of mixed types - numbers and strings.
4. `(class {:a 1 :b 2 :c 3 :d 4 :e 5})` - The `class` function returns the class (type) of the given value. In this case, it's `clojure.lang.PersistentArrayMap`, which is the type of Clojure's default map implementation.
5. `(hash-map :a 1 :b 2 :c 3 :d 4 :e 5)` - This is another way of creating a map using the `hash-map` function. The order of the key-value pairs may not be preserved.
6. `(into {} [[:a 1] [:b 2] [:c 3] [:d 4] [:e 5]])` - The `into` function is used to insert values from a collection (second argument) into a map (first argument). Here, it constructs a map from a vector of vectors, each inner vector being a key-value pair.
7. `(= {:a 1, :b 2, :c 3, :d 4, :e 5} (hash-map :a 1 :b 2 :c 3 :d 4 :e 5))` - This line compares two maps for equality. In Clojure, two maps are equal if they contain the same keys and associated values, regardless of the order.
8. `(assoc {:a 1 :b 2 :c 3 :d 4} :e 5)` - The `assoc` function adds a new key-value pair to a map or updates an existing key's value. Here, it adds `:e 5` to the map.
9. `(dissoc {:a 1 :b 2 :c 3 :d 4 :e 5} :a)` - The `dissoc` function removes a key-value pair from a map. Here, it removes the key `:a` and its associated value.
10. `(conj {:a 1, :b 2, :c 3, :d 4} [:e 5])` - The `conj` function "conjoins" a value to a collection. In the context of maps, it expects the value to be a map entry (a two-element vector) which it adds to the map. In this case, it adds the key-value pair `:e 5` to the map.

## Accessing Elements

### Code

```clojure
(ns collections-maps.core)

;; ---
;; Accessing elements
;; ---

(:a {:a 1, :b 2, :c 3, :d 4, :e 5})
;; => 1
({:a 1, :b 2, :c 3, :d 4, :e 5} :a)
;; => 1
(get {:a 1, :b 2, :c 3, :d 4, :e 5} :a)
;; => 1
(get {} :a)
;; => nil
(get {:a 1, :b 2, :c 3, :d 4, :e 5} :f "Default Value")
;; => "Default Value"

(get-in {:person {:name "John" :age 30}} [:person])
;; => {:name "John", :age 30}
(get-in {:person {:name "John" :age 30}} [:person :name])
;; => "John"
(get-in {:person {:name "John" :age 30}} [:person :address])
;; => nil
(get-in {:person {:name "John" :age 30}} [:person :address] "Default Value")
;; => "Default Value"

(keys {:a 1, :b 2, :c 3, :d 4, :e 5})
;; => (:a :b :c :d :e)
(vals {:a 1, :b 2, :c 3, :d 4, :e 5})
```

### Explanation

This code includes some examples of how to work with maps in Clojure, specifically how to access and manipulate them. Here's an explanation for each piece of code:

1. `(:a {:a 1, :b 2, :c 3, :d 4, :e 5})`
   - This is using a keyword as a function to get the corresponding value from a map. In this case, it returns `1`, the value associated with the `:a` keyword.
2. `({:a 1, :b 2, :c 3, :d 4, :e 5} :a)`
   - This is equivalent to the first operation but using a map as a function to look up a key. It also returns `1`.
3. `(get {:a 1, :b 2, :c 3, :d 4, :e 5} :a)`
   - This uses the `get` function to retrieve the value associated with the `:a` keyword from the map. It also returns `1`.
4. `(get {} :a)`
   - Here, `get` is used on an empty map with `:a` as a key, so it returns `nil` as there's no such key in the map.
5. `(get {:a 1, :b 2, :c 3, :d 4, :e 5} :f "Default Value")`
   - This uses `get` with a default value. If the key isn't found in the map, it returns the default value, in this case, `"Default Value"`.
6. `(get-in {:person {:name "John" :age 30}} [:person])`
   - `get-in` is used to retrieve a nested map from the outer map. Here, it returns the map `{:name "John", :age 30}`.
7. `(get-in {:person {:name "John" :age 30}} [:person :name])`
   - This retrieves a nested value by following the specified key path. It returns `"John"`.
8. `(get-in {:person {:name "John" :age 30}} [:person :address])`
   - This attempts to retrieve a nested value that doesn't exist, resulting in `nil`.
9. `(get-in {:person {:name "John" :age 30}} [:person :address] "Default Value")`
   - This is similar to the previous example but provides a default value when the key path doesn't exist. It returns `"Default Value"`.
10. `(keys {:a 1, :b 2, :c 3, :d 4, :e 5})`
    - This retrieves all keys from the map and returns them as a sequence: `(:a :b :c :d :e)`.
11. `(vals {:a 1, :b 2, :c 3, :d 4, :e 5})`
    - This retrieves all values from the map and returns them as a sequence: `(1 2 3 4 5)`.

In general, this code demonstrates various ways to retrieve keys and values from maps in Clojure, both at the top level and nested within other maps, and also how to specify default values if a key isn't found.

## Manipulating Maps

### Code

```clojure
(ns collections-maps.core)

;; ---
;; Manipulating maps
;; ---

(def map-a {:a 1, :b 2, :c 3, :d 4, :e 5})
(def map-b {:f 6, :g 7, :h 8, :i 9, :j 10})

(merge map-a map-b)
;; => {:e 5, :g 7, :c 3, :j 10, :h 8, :b 2, :d 4, :f 6, :i 9, :a 1}

(merge map-a)
;; => {:a 1, :b 2, :c 3, :d 4, :e 5}

(merge map-a {})
;; => {:a 1, :b 2, :c 3, :d 4, :e 5}
```

### Explanation

This Clojure code demonstrates how to manipulate maps (essentially dictionaries or key-value pairs), specifically how to merge two maps together. First, let's break it down line by line:

1. **`(ns collections-maps.core)`**: This line creates a new namespace named "collections-maps.core". This is the equivalent of a package in languages like Java. It helps to organize your code and avoid naming conflicts.
2. **`(def map-a {:a 1, :b 2, :c 3, :d 4, :e 5})`**: This line defines a map named **`map-a`** which associates keywords (**`:a`**, **`:b`**, **`:c`**, **`:d`**, **`:e`**) with the integers 1 through 5.
3. **`(def map-b {:f 6, :g 7, :h 8, :i 9, :j 10})`**: Similarly, this line defines a map named **`map-b`** with different keywords and integers.
4. **`(merge map-a map-b)`**: The **`merge`** function takes any number of maps and returns a map that combines all of them. If there are duplicate keys, the value from the last map with that key is used. Here, it combines **`map-a`** and **`map-b`** into a single map. The resulting map will contain all key-value pairs from both **`map-a`** and **`map-b`**.
5. **`(merge map-a)`**: When only one map is passed to the **`merge`** function, it returns that map. So, in this case, it just returns **`map-a`**.
6. **`(merge map-a {})`**: Here **`merge`** is called with **`map-a`** and an empty map. Since there are no key-value pairs in the empty map, the result is just **`map-a`**.

## Predicates

### Code

```clojure
(ns collections-maps.core)

;; ---
;; Predicates
;; ---

(empty? {})
;; => true
(empty? {:a 1, :b 2})
;; => false

(contains? {:a 1, :b 2, :c 3, :d 4, :e 5} :a)
;; => true
(contains? {:a 1, :b 2, :c 3, :d 4, :e 5} :f)
;; => false

(some #(= (second %) 3) {:a 1, :b 2, :c 3, :d 4, :e 5}) ;; alternative for checking if a map contains a value
;; => true
(some #(= (second %) 6) {:a 1, :b 2, :c 3, :d 4, :e 5})
;; => nil
```

### Explanation

This code demonstrates several core functions in Clojure for working with maps: key-value data structures. Let's break down each part of this code:

1. `(ns collections-maps.core)`:
   This line is defining a namespace. In Clojure, namespaces are used to group related code together. The `ns` macro is used to declare the namespace that the code that follows will be a part of. This line declares that the following code is part of the `collections-maps.core` namespace.
2. `(empty? {})`:
   This is calling the `empty?` function with an empty map as an argument. `empty?` is a predicate function that checks if a collection is empty or not. In this case, since the map is indeed empty, the function returns `true`.
3. `(empty? {:a 1, :b 2})`:
   Similar to the above, this is calling the `empty?` function but this time with a non-empty map as an argument. The map contains two key-value pairs: `:a` is mapped to `1` and `:b` is mapped to `2`. Since this map is not empty, the function returns `false`.
4. `(contains? {:a 1, :b 2, :c 3, :d 4, :e 5} :a)`:
   This line is calling the `contains?` function with a map and a key as arguments. `contains?` checks if the given map contains the specified key. In this case, the map does contain the key `:a`, so the function returns `true`.
5. `(contains? {:a 1, :b 2, :c 3, :d 4, :e 5} :f)`:
   Similar to the previous, this line is calling the `contains?` function but with the key `:f`. The map does not contain this key, so the function returns `false`.
6. `(some #(= (second %) 3) {:a 1, :b 2, :c 3, :d 4, :e 5})`:
   This line is using the `some` function in conjunction with an anonymous function `#(= (second %) 3)` to check if any value in the map equals `3`. The `some` function applies the given function to each element in the collection and returns the first non-nil result, or `nil` if there is none. The anonymous function checks if the second element of the input (which, when iterating over a map, is the value of the key-value pair) is equal to `3`. The map contains `3` as one of its values, so the function returns `true`.
7. `(some #(= (second %) 6) {:a 1, :b 2, :c 3, :d 4, :e 5})`:
   Similar to the above, this line uses the `some` function to check if any value in the map equals `6`. The map does not contain `6` as one of its values, so the function returns `nil`.

## Utility Functions

### Code

```clojure
(ns collections-maps.core)

;; ---
;; Utility functions
;; ---

(count {:a 1, :b 2, :c 3, :d 4, :e 5})
;; => 5
(count {})
;; => 0
```

### Explanation

This Clojure code resides within the namespace `collections-maps.core`. In Clojure, a namespace (defined with `ns`) contains a collection of related functions, macros, and data. The code executes the function `count` twice, once for each given map.

1. `(count {:a 1, :b 2, :c 3, :d 4, :e 5})` counts the number of key-value pairs in the map. In Clojure, a map is an associative data structure of key-value pairs. Here, the map contains five key-value pairs: `:a` maps to `1`, `:b` maps to `2`, `:c` maps to `3`, `:d` maps to `4`, and `:e` maps to `5`. Therefore, the count function returns `5`.
2. `(count {})` counts the number of key-value pairs in an empty map. As there are no key-value pairs in the empty map, the count function returns `0`.

In summary, this Clojure code demonstrates how to use the `count` function to determine the number of key-value pairs in a map. The results are returned as integers.

## Conversion to Other Collections

### Code

```clojure
(ns collections-maps.core)

;; ---
;; Conversion to other collections
;; ---

(apply list {:a 1, :b 2, :c 3, :d 4, :e 5})
;; => ([:a 1] [:b 2] [:c 3] [:d 4] [:e 5])
(apply vector {:a 1, :b 2, :c 3, :d 4, :e 5})
;; => [[:a 1] [:b 2] [:c 3] [:d 4] [:e 5]]

(apply hash-map [:a 1 :b 2 :c 3 :d 4 :e 5])
;; => {:e 5, :c 3, :b 2, :d 4, :a 1}

(into {:a 1} [[:b 2] [:c 3] [:d 4] [:e 5]])
;; => {:a 1, :b 2, :c 3, :d 4, :e 5}

(seq {:a 1, :b 2, :c 3, :d 4, :e 5})
;; => ([:a 1] [:b 2] [:c 3] [:d 4] [:e 5])

(apply str {:a 1, :b 2, :c 3, :d 4, :e 5})
;; => "[:a 1][:b 2][:c 3][:d 4][:e 5]"
(str {:a 1, :b 2, :c 3, :d 4, :e 5})
;; => "{:a 1, :b 2, :c 3, :d 4, :e 5}"
```

### Explanation

Let's go through this code snippet by snippet.

1. `(apply list {:a 1, :b 2, :c 3, :d 4, :e 5})`
   This code applies the `list` function to a map. In Clojure, maps are collections of key-value pairs. The `apply` function spreads the key-value pairs of the map into a sequence of arguments for the function (`list` in this case). The result is a list of key-value pairs representing each as a list.
2. `(apply vector {:a 1, :b 2, :c 3, :d 4, :e 5})`
   Similar to the first example, it uses `vector` instead of `list`. The result is a vector of key-value pairs, where each pair is a vector itself.
3. `(apply hash-map [:a 1 :b 2 :c 3 :d 4 :e 5])`
   This creates a hash map from a vector. The `apply` function spreads the vector elements into a sequence of arguments for the `hash-map` function, which then combines them into key-value pairs.
4. `(into {:a 1} [[:b 2] [:c 3] [:d 4] [:e 5]])`
   The `into` function takes two collections and adds the elements of the second one to the first one. In this case, it's adding the key-value pairs represented as vectors in the vector to the map.
5. `(seq {:a 1, :b 2, :c 3, :d 4, :e 5})`
   This is using the `seq` function to create a sequence from a map. Like `apply list` and `apply vector`, this generates a sequence of key-value pairs, each represented as a vector, but it doesn't wrap them in an extra list or vector.
6. `(apply str {:a 1, :b 2, :c 3, :d 4, :e 5})`
   This applies the `str` function to a map. The `str` function converts its arguments to strings and concatenates them. In this case, the key-value pairs are each converted to a string and concatenated without separating them.
7. `(str {:a 1, :b 2, :c 3, :d 4, :e 5})`
   This converts the entire map to a string. Unlike `apply str`, it doesn't convert each pair separately and then concatenates them; it converts the whole map to a single string, preserving the map's syntax.
