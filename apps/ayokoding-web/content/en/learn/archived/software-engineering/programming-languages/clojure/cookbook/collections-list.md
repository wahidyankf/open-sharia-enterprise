---
title: "Collections: List"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/collections_list).

</aside>

## Overview

In Clojure, a list is a fundamental data structure used to store an ordered collection of elements. Lists are essential in functional programming and are extensively used in Clojure. Lists are immutable, meaning that once they are created, their contents cannot be modified. This is because modifying a list would require changing the reference to the next element, and since this is not allowed, the list is immutable. Clojure lists are implemented as singly linked lists, where each element references the next element. This makes lists efficient for adding elements to the beginning of the list but inefficient for adding elements to the end of the list. Despite this, lists are still widely used in Clojure because of their simplicity and ease of use.

From [https://clojure.org/reference/data_structures#Lists](https://clojure.org/reference/data_structures#Lists):

> Lists are collections. They implement the ISeq interface directly. (Note that the empty list implements ISeq as well, however the `seq` function will always return `nil` for an empty sequence.) [count](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count) is O(1). [conj](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/conj) puts the item at the front of the list.

## List Creation

### Code

```clojure
(ns collections-list.core)

;; ---
;; List creation
;; ---

'(1 2 3 4 5)
;; => (1 2 3 4 5)

(list 1 2 3 4 5)
;; => (1 2 3 4 5)

(class '(1 2 3 4 5))
;; => clojure.lang.PersistentList

(= '(1 2 3 4 5) (list 1 2 3 4 5))
;; => true

'(1 "b" 3 "d")
;; => (1 "b" 3 "d")

(cons 1 '(2 3 4 5))
;; => (1 2 3 4 5)

(conj '(2 3 4 5) 1)
;; => (1 2 3 4 5)
```

### Explanation

This Clojure code is about creating and manipulating lists. Here's a breakdown of each line:

1. `(ns collections-list.core)`: This line declares the namespace for the following code. Namespaces in Clojure are a way to prevent naming conflicts. In this case, the namespace is `collections-list.core`.
2. `'(1 2 3 4 5)`: The quote (’) before the list is a shorthand way to create a list without evaluating its elements. This means `'(1 2 3 4 5)` will be treated as a list of elements rather than a function call.
3. `(list 1 2 3 4 5)`: This creates a list with the elements 1, 2, 3, 4, and 5. The `list` function in Clojure creates a new list with the given arguments.
4. `(class '(1 2 3 4 5))`: This retrieves the class (type) of the list `'(1 2 3 4 5)`, which is `clojure.lang.PersistentList`. A PersistentList in Clojure is a list data structure with efficient add and remove operations that don't mutate the original list.
5. `(= '(1 2 3 4 5) (list 1 2 3 4 5))`: The `=` function in Clojure is used for equality testing. This line tests if the quoted list `'(1 2 3 4 5)` is equal to the list created by `(list 1 2 3 4 5)`. It returns true, which means these two lists are the same.
6. `’(1 "b" 3 "d")`: This creates a list of various types of elements. In Clojure, lists can contain different types of data.
7. `(cons 1 '(2 3 4 5))`: The `cons` function in Clojure adds an element to the beginning of a list or sequence. This line creates a new list with 1 as the first element and the list elements `(2 3 4 5)` following.
8. `(conj '(2 3 4 5) 1)`: The `conj` function in Clojure adds an element to a collection. For lists, it adds the new element to the front of the list, which is why the result is `(1 2 3 4 5)`. If you used `conj` on a different type of collection, such as a vector, it would add the new element(s) at the end.

The overall theme of this code is demonstrating different ways to create and manipulate lists in Clojure.

## Accessing Elements

### Code

```clojure
(ns collections-list.core)

;; ---
;; Accessing elements
;; ---

(first '(1 2 3 4 5))
;; => 1
(first '())
;; => nil

(second '(1 2 3 4 5))
;; => 2
(second '())
;; => nil

(rest '(1 2 3 4 5))
;; => (2 3 4 5)
(rest '(1))
;; => ()
(rest '())
;; => ()

(nth '(1 2 3 4 5) 0)
;; => 1
(nth '() 0)
;; => Execution error (IndexOutOfBoundsException) at collections-list.core/eval8980 (REPL:48).
;;    null

(last '(1 2 3 4 5))
;; => 5
(last '(1))
;; => 1
(last '())
;; => nil
```

### Explanation

This Clojure code demonstrates various ways of accessing elements from a list (in this case, a quoted list akin to an immutable array). Here's a breakdown of each function used:

1. `first`: This function returns the first element of the collection passed to it. In this example, `(first '(1 2 3 4 5))` would return `1`, which is the first element of the list. If the collection is empty as in `(first '())`, it will return `nil`.
2. `second`: Similar to the `first` function, `second` returns the second element of the collection. If the collection has no second element or is empty, it will return `nil`.
3. `rest`: This function returns a collection of all the elements after the first one. `(rest '(1 2 3 4 5))` will give you `(2 3 4 5)`. If the list only contains one element, like `(rest '(1))`, it will return an empty list `()`. If the list is empty, it will still return an empty one `()`.
4. `nth`: This function is used to access an element at a specific position in the collection. `(nth '(1 2 3 4 5) 0)` would return `1`, the element at position `0` (Clojure uses zero-based indexing). However, if the index is out of bounds (as with `(nth '() 0)`), it will throw an `IndexOutOfBoundsException`.
5. `last`: This function returns the last element of the collection. `(last '(1 2 3 4 5))` would return `5`. If the list contains only one element, like `(last '(1))`, it will return that element. If the list is empty, it will return `nil`.

Remember that accessing a list's first or last element in Clojure is not as efficient as in array-like data structures. That's because lists in Clojure are implemented as singly linked lists, meaning you have to traverse the entire list to access the last element. For large data sets, vectors would be more efficient if you need to access elements often at the end of the collection.

## Manipulating Lists

### Code

```clojure
(ns collections-list.core)

;; ---
;; Manipulating lists
;; ---

(def list-a '(1 2 3 4 5))
(def list-b '(6 7 8 9 10))

(concat list-a list-b)
;; => (1 2 3 4 5 6 7 8 9 10)
(concat list-a)
;; => (1 2 3 4 5)
(concat list-a ())
;; => (1 2 3 4 5)

(flatten '((1 2 3) (4 5 6 (7 8 9)) (10 11 12) 13))
;; => (1 2 3 4 5 6 7 8 9 10 11 12 13)

(map (fn [item]  (+ item 1)) list-a)
;; => (2 3 4 5 6)
(map inc list-a)
;; => (2 3 4 5 6)
list-a
;; => (1 2 3 4 5)

(filter (fn [item] ((fn [item] (even? item)) item)) list-a)
;; => (2 4)
(filter (fn [item] (even? item)) list-a)
;; => (2 4)

(reduce (fn [acc item] (+ acc item)) 0 list-a)
;; => 15
(reduce + 0 list-a)
;; => 15
```

## Explanation

The given Clojure code includes different functions for manipulating lists. Here is the explanation for each section:

1. `(ns collections-list.core)`: This is the namespace declaration, a container for different functions, macros, and other data types in Clojure.
2. `(def list-a '(1 2 3 4 5))` and `(def list-b '(6 7 8 9 10))`:
   These lines define two lists, `list-a`, and `list-b`, with five elements each.
3. `concat` function: The `concat` function is used to concatenate or join together two or more lists.
   - `(concat list-a list-b)` will combine the elements of `list-a` and `list-b`, resulting in a new list with all elements from both lists.
   - `(concat list-a)` and `(concat list-a ())` will return `list-a` elements as `concat` expects at least two lists. Since only `list-a` or `list-a` and an empty list are provided, there are not enough lists to perform concatenation, so it returns the input list (`list-a`) as is.
4. `flatten` function: The `flatten` function will take a list of lists and return a list containing all the elements of the input lists but flattened into a single list.
5. `map` function: The `map` function applies a given function to each element of a list.
   - `(map (fn [item] (+ item 1)) list-a)` adds 1 to each element in `list-a`.
   - `(map inc list-a)` does the same but uses the built-in `inc` function (which increments its argument by 1) instead of a custom function.
6. `filter` function: The `filter` function applies a predicate function (a function that returns true or false) to each list element. It returns a new list that includes only the elements for which the predicate function returns true.
   - `(filter (fn [item] ((fn [item] (even? item)) item)) list-a)` and `(filter (fn [item] (even? item)) list-a)` both return a new list that includes only the even numbers from `list-a`.
7. `reduce` function: The `reduce` function applies a binary function (a function that takes two arguments) to a start value and the elements of a list from left to right to reduce the list to a single output value.
   - `(reduce (fn [acc item] (+ acc item)) 0 list-a)` and `(reduce + 0 list-a)` compute the sum of list-a elements. The binary function is `+`, the start value is `0`, and `list-a` is the list of numbers to sum.

In the end, you call the variable `list-a` to print it again; it will be `(1 2 3 4 5)` as defined because none of these operations modified the original list. They instead returned new lists since Clojure data structures are immutable.

## Predicates

### Code

```clojure
(ns collections-list.core)

;; ---
;; Predicates
;; ---
(empty? '())
;; => true
(empty? '(1 2))
;; => false

(.contains '(1 2 3 4 5) 3)
;; => true
(.contains '(1 2 3 4 5) 6)
;; => false

(every? (fn [item] (> item 3)) '(3 4 5 6))
;; => false
(every? (fn [item] (> item 1)) '(3 4 5 6))
;; => true
(every? #(even? %) '(2 4 6 8))
;; => true
(every? #(odd? %) '(2 4 6 8))
;; => false

(some (fn [item] (> item 3)) '(3 4 5 6))
;; => true
(some (fn [item] (> item 10)) '(3 4 5 6))
;; => nil
```

### Explanation

Here is an explanation of each of the code segments:

1. `ns collections-list.core`: This code declares a namespace named `collections-list.core`. Namespaces in Clojure are similar to packages in Java; they allow you to organize your code and prevent naming conflicts.
2. `empty? '()`: The `empty?` function checks if a given collection is empty. In this case, it's checking an empty list. The result is `true`, meaning the list is indeed empty.
3. `empty? '(1 2)`: This checks if the list containing `1` and `2` is empty. The result is `false` because the list contains elements.
4. `(.contains '(1 2 3 4 5) 3)`: The `.contains` method checks if a certain value is in a collection. In this case, it checks if `3` is in the list of `(1 2 3 4 5)`. The result is `true` as `3` is in the list.
5. `(.contains '(1 2 3 4 5) 6)`: Similar to the previous point, this checks if `6` is in the list of `(1 2 3 4 5)`. The result is `false` because `6` is not in the list.
6. `(every? (fn [item] (> item 3)) '(3 4 5 6))`: The `every?` function checks if every element in a collection satisfies a predicate. In this case, it checks if every element in the list `(3 4 5 6)` is greater than `3`. The result is `false` because `3` is not greater than `3`.
7. `(every? (fn [item] (> item 1)) '(3 4 5 6))`: This checks if every element in the list `(3 4 5 6)` is greater than `1`. The result is `true` because all of the elements are greater than `1`.
8. `(every? #(even? %) '(2 4 6 8))`: This uses a shorthand notation for a function `#(even? %)` to check if every element in the list `(2 4 6 8)` is even. The result is `true` because all numbers in the list are even.
9. `(every? #(odd? %) '(2 4 6 8))`: Similar to the previous point, this checks if every element in the list `(2 4 6 8)` is odd. The result is `false` because none of the numbers in the list are odd.
10. `(some (fn [item] (> item 3)) '(3 4 5 6))`: The `some` function checks if some (at least one) element in a collection satisfies a predicate. In this case, it checks if any element in the list `(3 4 5 6)` is greater than `3`. The result is `true` because `4`, `5`, and `6` are greater than `3`.
11. `(some (fn [item] (> item 10)) '(3 4 5 6))`: Similar to the previous point, this checks if any element in the list `(3 4 5 6)` is greater than `10`. The result is `nil` (equivalent to `false`), indicating that no elements in the list are greater than `10`.

## Utility Functions

### Code

```clojure
(ns collections-list.core)

;; ---
;; Utily functions
;; ---

(count '(1 2 3 4 5))
;; => 5
(count '())
;; => 0

(reverse '(1 2 3 4 5))
;; => (5 4 3 2 1)
(reverse '())
;; => ()

(sort '(5 3 1 2 4))
;; => (1 2 3 4 5)
(sort '())
;; => ()

(distinct '(1 2 3 4 5 1 2 3 4 5))
;; => (1 2 3 4 5)
(distinct '(1 2 3 4 5))
;; => (1 2 3 4 5)
(distinct '())
;; => ()

(partition 2 '(1 2 3 4 5 6))
;; => ((1 2) (3 4) (5 6))
(partition 2 '(1 2 3 4 5))
;; => ((1 2) (3 4))
(partition 2 '())
;; => ()

(take 2 '(1 2 3 4 5))
;; => (1 2)
(take 3 '(1 2 3 4 5))
;; => (1 2 3)
(take 6 '(1 2 3 4 5))
;; => (1 2 3 4 5)
(take 6 '())
;; => ()
```

### Explanation

This code demonstrates some common functions used on collections in Clojure, a functional programming language. Each function is used on different types of lists. Here's what each function does:

1. `count`: This function returns the number of elements in a collection. In this case, it's being used on a list. So, `(count '(1 2 3 4 5))` returns `5` because there are five elements in the list.
2. `reverse`: This function returns a sequence of the items in the collection in the reverse order. `(reverse '(1 2 3 4 5))` returns `(5 4 3 2 1)`.
3. `sort`: This function returns a sorted sequence of the items in the collection. `(sort '(5 3 1 2 4))` returns `(1 2 3 4 5)`, the original list is sorted in ascending order.
4. `distinct`: This function returns a sequence of the items in the collection with duplicates removed. `(distinct '(1 2 3 4 5 1 2 3 4 5))` returns `(1 2 3 4 5)`.
5. `partition`: This function partitions the collection into chunks of a specified size. `(partition 2 '(1 2 3 4 5 6))` returns `((1 2) (3 4) (5 6))`, which is the original list partitioned into pairs. If there aren't enough elements in the list to form a complete chunk, those elements are not included in the output (as seen in the case of `(partition 2 '(1 2 3 4 5))`.
6. `take`: This function returns the first n items of the collection. `(take 2 '(1 2 3 4 5))` returns `(1 2)`, which are the first two elements of the list. If the collection has fewer than n items, it returns them. This is illustrated by `(take 6 '(1 2 3 4 5))`, which returns the entire list because the list contains fewer than 6 items.

The empty lists (`'()`) examples show how these functions behave when applied to an empty collection. The result is an empty collection for all these functions except for `count`, which returns `0`.

## Conversion to Other Collections

### Code

```clojure
(ns collections-list.core)

;; ---
;; Conversion to other collections
;; ---

(vec '(1 2 3 4 5))
;; => [1 2 3 4 5]

(set '(1 2 3 4 5))
;; => #{1 4 3 2 5}

;; convert clojure list to hash-map
(into {} '({:a 2} {:b 4} {:c 6}))
;; => {:a 2, :b 4, :c 6}
(into {} '([:a 2] [:b 4] [:c 6]))
;; => {:a 2, :b 4, :c 6}
(into {} '((:a 2) (:b 4) (:c 6)))
;; => Execution error (ClassCastException) at collections-list.core/eval14028 (REPL:180).
;;    class clojure.lang.Keyword cannot be cast to class java.util.Map$Entry (clojure.lang.Keyword is in unnamed module of loader 'app'; java.util.Map$Entry is in module java.base of loader 'bootstrap')

(seq '(1 2 3 4 5))
;; => (1 2 3 4 5)

(str '(1 2 3 4 5))
;; => "(1 2 3 4 5)"
```

### Explanation

This Clojure code demonstrates how to convert between various collections. Here's what each line of code does:

1. `(ns collections-list.core)` - This line is defining a new namespace, `collections-list.core`, which is a container for code and data.
2. `(vec '(1 2 3 4 5))` - The `vec` function converts a list to a vector. A vector in Clojure is an indexed, sequential data structure. So here, it's converting the list `(1 2 3 4 5)` to the vector `[1 2 3 4 5]`.
3. `(set '(1 2 3 4 5))` - The `set` function converts a list to a set. A set in Clojure is a collection of unique elements. The order of elements in a set is not guaranteed. So here, it's converting the list `(1 2 3 4 5)` to the set `#{1 4 3 2 5}`. Notice that the order of the elements in the output set may vary.
4. `(into {} '({:a 2} {:b 4} {:c 6}))` - The `into` function is used to add items to a collection. Here, it's being used to convert a list of maps into a single map. The result is `{:a 2, :b 4, :c 6}`.
5. `(into {} '([:a 2] [:b 4] [:c 6]))` - Here, it's being used to convert a list of vectors into a single map. The result is `{:a 2, :b 4, :c 6}`.
6. `(into {} '((:a 2) (:b 4) (:c 6)))` - In this case, the `into` function tries to convert a list of lists into a map. However, because this is another list instead of a map or vector, this leads to an execution error.
7. `(seq '(1 2 3 4 5))` - The `seq` function in Clojure returns a sequence of the collection. Here, it returns a sequence from the list `(1 2 3 4 5)`. The input is already a sequence (a list), but the output is the same.
8. `(str '(1 2 3 4 5))` - The `str` function converts a data type into a string. Here, it's converting the list `(1 2 3 4 5)` into the string `"(1 2 3 4 5)"`.
