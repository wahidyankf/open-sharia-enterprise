---
title: "Collections: Vector"
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/collections_vector).

</aside>

## Overview

From [https://clojure.org/reference/data_structures#Vectors](https://clojure.org/reference/data_structures#Vectors):

> A Vector is a collection of values indexed by contiguous integers. Vectors support access to items by index in log32N hops. [count](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count) is O(1). [conj](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/conj) puts the item at the end of the vector. Vectors also support [rseq](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/rseq), which returns the items in reverse order. Vectors implement IFn, for invoke() of one argument, which they presume is an index and look up in themselves as if by nth, i.e. vectors are functions of their indices. Vectors are compared first by length, then each element is compared in order.

## Vector Creation

### Code

```clojure
(ns collections-vectors.core)

;; ---
;; Vector creation
;; ---

[1 2 3 4 5]
;; => [1 2 3 4 5]

(vector 1 2 3 4 5)
;; => [1 2 3 4 5]

(vec '(1 2 3 4 5))
;; => [1 2 3 4 5]

(class [1 2 3 4 5])
;; => clojure.lang.PersistentVector

(= [1 2 3 4 5] (vector 1 2 3 4 5))
;; => true

[1 "b" 3 "d"]
;; => [1 "b" 3 "d"]

(cons 1 [2 3 4 5])
;; => (1 2 3 4 5)

(conj [1 2 3 4] 5)
;; => [1 2 3 4 5]
```

### Explanation

Here's a breakdown of the Clojure code provided:

1. `(ns collections-vectors.core)` - This is a namespace declaration. It's how Clojure organizes its code. Here, it is creating or referring to a namespace called "collections-vectors.core".
2. `[1 2 3 4 5]` - This is a vector, a basic data structure in Clojure. It contains the elements 1, 2, 3, 4, and 5.
3. `(vector 1 2 3 4 5)` - This creates a vector using the `vector` function. It will create the same vector as above, [1 2 3 4 5].
4. `(vec '(1 2 3 4 5))` - This converts a list (represented by ' before the parentheses) to a vector using the `vec` function.
5. `(class [1 2 3 4 5])` - This retrieves the class of the vector [1 2 3 4 5], which is `clojure.lang.PersistentVector`. Clojure uses this underlying Java class to implement its vector data structure.
6. `(= [1 2 3 4 5] (vector 1 2 3 4 5))` - This checks if two vectors are equal. In this case, it's checking if the vector [1 2 3 4 5] is equal to the result of `(vector 1 2 3 4 5)`, which is also [1 2 3 4 5]. The result is true.
7. `[1 "b" 3 "d"]` - This is a vector containing mixed types. It's valid in Clojure to have a collection of mixed types.
8. `(cons 1 [2 3 4 5])` - The `cons` function adds an element to the front of a list or a sequence, not a vector. Therefore, it converts the vector to a sequence and then adds the element, resulting in a sequence (1 2 3 4 5), not a vector.
9. `(conj [1 2 3 4] 5)` - The `conj` function adds an element to a collection. In the case of a vector, it adds the element to the end, creating the vector [1 2 3 4 5].

## Accessing Elements

### Code

```clojure
(ns collections-vectors.core)

;; ---
;; Accessing elements
;; ---

(first [1 2 3 4 5])
;; => 1
(first [])
;; => nil

(second [1 2 3 4 5])
;; => 2
(second [])
;; => nil

(subvec [1 2 3 4 5] 1 3)
;; => [2 3]
(subvec [1 2 3 4 5] 1)
;; => [2 3 4 5]
(subvec [1] 1)
;; => []
(try
  (subvec [1] 2)
  (catch Exception _e "IndexOutOfBoundsException"))
;; => "IndexOutOfBoundsException"

(nth [1 2 3 4 5] 0)
;; => 1
(try
  (nth [] 0)
  (catch Exception _e "IndexOutOfBoundsException"))
;; => "IndexOutOfBoundsException"

(last [1 2 3 4 5])
;; => 5
(last [1])
;; => 1
(last [])
;; => nil
```

### Explanation

This Clojure code demonstrates how to access elements within vectors. The different functions perform the following:

1. `(first [1 2 3 4 5])` - This line returns the first element of the vector, in this case, `1`.
2. `(first [])` - This line attempts to return the first element of an empty vector, which is `nil` as there are no elements to return.
3. `(second [1 2 3 4 5])` - This line returns the second element of the vector, in this case, `2`.
4. `(second [])` - This line attempts to return the second element of an empty vector, which is `nil` as there are no elements to return.
5. `(subvec [1 2 3 4 5] 1 3)` - This line returns a sub-vector that starts from index `1` (0-indexed) and ends before index `3`. So, it returns `[2 3]`.
6. `(subvec [1 2 3 4 5] 1)` - This line returns a sub-vector that starts from index `1` and extends till the end of the vector, thus returning `[2 3 4 5]`.
7. `(subvec [1] 1)` - This line attempts to return a sub-vector starting from index `1` of a vector with only one element. There's no element at index `1`, so it returns an empty vector `[]`.
8. `(try (subvec [1] 2) (catch Exception _e "IndexOutOfBoundsException"))` - This line attempts to return a sub-vector starting from index `2` of a vector with only one element. As there's no element at index `2`, it throws an `IndexOutOfBoundsException`, which is caught and returns the string "IndexOutOfBoundsException".
9. `(nth [1 2 3 4 5] 0)` - This line returns the element at index `0` (0-indexed) in the vector, in this case `1`.
10. `(try (nth [] 0) (catch Exception _e "IndexOutOfBoundsException"))` - This line attempts to return the element at index `0` in an empty vector. As there's no element at index `0`, it throws an `IndexOutOfBoundsException`, which is caught and returns the string "IndexOutOfBoundsException".
11. `(last [1 2 3 4 5])` - This line returns the last element of the vector, in this case `5`.
12. `(last [1])` - This line returns the vector's last (and only) element, in this case `1`.
13. `(last [])` - This line attempts to return the last element of an empty vector, which is `nil` as there are no elements to return.

## Manipulating Vectors

### Code

```clojure
(ns collections-vectors.core)

;; ---
;; Manipulating vectors
;; ---

(def vector-a [1 2 3 4 5])
(def vector-b [6 7 8 9 10])

(concat vector-a vector-b)
;; => (1 2 3 4 5 6 7 8 9 10)
(concat vector-a)
;; => (1 2 3 4 5)
(concat vector-a [])
;; => (1 2 3 4 5)

(flatten [[1 2 3] [4 5 6 [7 8 9]] [10 11 12] 13])
;; => (1 2 3 4 5 6 7 8 9 10 11 12 13)

(map (fn [item]  (+ item 1)) vector-a)
;; => (2 3 4 5 6)
(map inc vector-a)
;; => (2 3 4 5 6)
vector-a
;; => [1 2 3 4 5]

(filter (fn [item] ((fn [item] (even? item)) item)) vector-a)
;; => (2 4)
(filter (fn [item] (even? item)) vector-a)
;; => (2 4)

(reduce (fn [acc item] (+ acc item)) 0 vector-a)
;; => 15
(reduce + 0 vector-a)
;; => 15
```

## Explanation

Here's the line-by-line explanation of the provided Clojure code:

1. `(ns collections-vectors.core)` - This is a namespace declaration. It defines a new namespace named `collections-vectors.core`.
2. `(def vector-a [1 2 3 4 5])` - This line defines a vector named `vector-a` that contains the numbers 1 through 5.
3. `(def vector-b [6 7 8 9 10])` - Similarly, this defines another vector, `vector-b`, with numbers 6 through 10.
4. `(concat vector-a vector-b)` - The `concat` function concatenates lists or vectors. This expression concatenates `vector-a` and `vector-b`, yielding a sequence `(1 2 3 4 5 6 7 8 9 10)`.
5. `(concat vector-a)` - If `concat` is given a single sequence, it will return that sequence. Here, it returns `(1 2 3 4 5)`.
6. `(concat vector-a [])` - Even if the second sequence is empty, `concat` will still return the first sequence. Again, it returns `(1 2 3 4 5)`.
7. `(flatten [[1 2 3] [4 5 6 [7 8 9]] [10 11 12] 13])` - The `flatten` function takes a sequence and "flattens" it, removing any nested sequences and leaving only their elements. The output of this operation is `(1 2 3 4 5 6 7 8 9 10 11 12 13)`.
8. `(map (fn [item] (+ item 1)) vector-a)` - `map` applies a function to every element of a sequence. This line applies the anonymous function `(fn [item] (+ item 1))`, which increments its argument by 1, to each element of `vector-a`, resulting in `(2 3 4 5 6)`.
9. `(map inc vector-a)` - This is another way of accomplishing the same thing as the previous line. `inc` is a built-in function that increments its argument, so the result is again `(2 3 4 5 6)`.
10. `vector-a` - This is just the vector `vector-a`, which the previous operations have not changed, so it's still `[1 2 3 4 5]`.
11. `(filter (fn [item] ((fn [item] (even? item)) item)) vector-a)` - `filter` applies a predicate function to each sequence element and returns a new sequence containing only the elements for which the predicate returned true. This line applies the anonymous function `(fn [item] (even? item))`, which tests if its argument is even, to each element of `vector-a`. The elements for which this is true are 2 and 4, so the result is `(2 4)`.
12. `(filter (fn [item] (even? item)) vector-a)` - This line is the same as the previous one, written more concisely. The result is again `(2 4)`.
13. `(reduce (fn [acc item] (+ acc item)) 0 vector-a)` - `reduce` applies a function to the elements of a sequence in a cumulative way. This line applies the anonymous function `(fn [acc item] (+ acc item))`, which adds its arguments to the elements of `vector-a`, starting with an initial accumulator value of 0. The result is the sum of the elements, which is 15.
14. `(reduce + 0 vector-a)` - This line accomplishes the same thing as the previous line, using the built-in function `+` instead of an anonymous function. The result is again 15.

## Predicates

### Code

```clojure
(ns collections-vectors.core)

;; ---
;; Predicates
;; ---

(empty? [])
;; => true
(empty? [1 2])
;; => false

(contains? [1 2 3 4 5] 3)
;; => true (contains? operates on index for vectors)
(contains? [1 2 3 4 5] 6)
;; => false (contains? operates on index for vectors)

(some #(= % 3) [1 2 3 4 5]) ;; alternative for checking if a vector contains a value
;; => true
(some #(= % 6) [1 2 3 4 5])
;; => nil

(every? (fn [item] (> item 3)) [3 4 5 6])
;; => false
(every? (fn [item] (> item 1)) [3 4 5 6])
;; => true
(every? #(even? %) [2 4 6 8])
;; => true
(every? #(odd? %) [2 4 6 8])
;; => false

(some (fn [item] (> item 3)) [3 4 5 6])
;; => true
(some (fn [item] (> item 10)) [3 4 5 6])
;; => nil
```

### Explanation

This Clojure code mainly demonstrates the use of various functions that operate on collections, specifically vectors. Here is the explanation for each of the expressions:

1. `(ns collections-vectors.core)` - This line is declaring the namespace for this section of code. The namespace in this case is `collections-vectors.core`.
2. `(empty? [])` - This function call checks if a vector is empty. The vector passed in here is empty, so it returns `true`.
3. `(empty? [1 2])` - This function call also checks if a vector is empty. The vector passed in here contains elements (1 and 2), so it returns `false`.
4. `(contains? [1 2 3 4 5] 3)` - This function checks if the index 3 is present in the vector. In this case, the vector does have an index 3, so it returns `true`.
5. `(contains? [1 2 3 4 5] 6)` - This function call is similar to the previous one but checks for index 6 in the vector. Since the vector only has indices up to 4, it returns `false`.
6. `(some #(= % 3) [1 2 3 4 5])` - This line is checking if any element in the vector is equal to 3. It returns `true` because 3 is indeed an element in the vector.
7. `(some #(= % 6) [1 2 3 4 5])` - This line is similar to the previous one, but checks if any element in the vector is equal to 6. It returns `nil` because 6 is not in the vector.
8. `(every? (fn [item] (> item 3)) [3 4 5 6])` - This function call checks if every item in the vector is greater than 3. Since 3 is not greater than 3, it returns `false`.
9. `(every? (fn [item] (> item 1)) [3 4 5 6])` - This line checks if every item in the vector is greater than 1. Since all elements are indeed greater than 1, it returns `true`.
10. `(every? #(even? %) [2 4 6 8])` - This line checks if every item in the vector is even. All the elements are even, so it returns `true`.
11. `(every? #(odd? %) [2 4 6 8])` - This line checks if every item in the vector is odd. Since none of the elements are odd, it returns `false`.
12. `(some (fn [item] (> item 3)) [3 4 5 6])` - This line checks if any item in the vector is greater than 3. Since 4, 5, and 6 are greater than 3, it returns `true`.
13. `(some (fn [item] (> item 10)) [3 4 5 6])` - This line checks if any item in the vector is greater than 10. Since none of the elements are greater than 10, it returns `nil`.

## Utility Functions

### Code

```clojure
(ns collections-vectors.core)

;; ---
;; Utily functions
;; ---

(count [1 2 3 4 5])
;; => 5
(count [])
;; => 0

(reverse [1 2 3 4 5])
;; => (5 4 3 2 1)
(reverse [])
;; => ()

(sort [5 3 1 2 4])
;; => (1 2 3 4 5)
(sort [])
;; => ()

(distinct [1 2 3 4 5 1 2 3 4 5])
;; => (1 2 3 4 5)
(distinct [1 2 3 4 5])
;; => (1 2 3 4 5)
(distinct [])
;; => ()

(partition 2 [1 2 3 4 5 6])
;; => ((1 2) (3 4) (5 6))
(partition 2 [1 2 3 4 5])
;; => ((1 2) (3 4))
(partition 2 [])
;; => ()

(take 2 [1 2 3 4 5])
;; => (1 2)
(take 3 [1 2 3 4 5])
;; => (1 2 3)
(take 6 [1 2 3 4 5])
;; => (1 2 3 4 5)
(take 6 [])
;; => ()
```

### Explanation

This Clojure code demonstrates the use of a few collection manipulation functions, primarily working with vectors.

1. `count`: This function returns the number of items in a collection. For instance, `(count [1 2 3 4 5])` will return 5, because there are 5 elements in the vector. If you apply `count` on an empty vector, as in `(count [])`, you get 0.
2. `reverse`: This function returns a sequence of the items in a collection in the reverse order. So, `(reverse [1 2 3 4 5])` will return a sequence `(5 4 3 2 1)`. For an empty vector `(reverse [])`, it returns an empty list `()`.
3. `sort`: This function returns a sorted sequence of the items in a collection. The example `(sort [5 3 1 2 4])` sorts the numbers in ascending order to yield `(1 2 3 4 5)`. Sorting an empty vector with `(sort [])` simply returns an empty list.
4. `distinct`: This function returns a sequence of the unique items in a collection, in the order they first appear. So, `(distinct [1 2 3 4 5 1 2 3 4 5])` will return `(1 2 3 4 5)`, eliminating the duplicates. The function works the same way even if the vector has no duplicates, as in `(distinct [1 2 3 4 5])`. When called with an empty vector, `(distinct [])`, it returns an empty list.
5. `partition`: This function splits a collection into chunks of a specified size. `(partition 2 [1 2 3 4 5 6])` breaks the original vector into pairs, yielding `((1 2) (3 4) (5 6))`. If the collection's size isn't a multiple of the partition size, the remaining items are simply dropped, as shown in `(partition 2 [1 2 3 4 5])`. Partitioning an empty vector returns an empty list.
6. `take`: This function returns the first N items of a collection. For instance, `(take 2 [1 2 3 4 5])` will return the first two elements `(1 2)`, `(take 3 [1 2 3 4 5])` will return the first three `(1 2 3)`. If you ask for more items than there are in the collection, as in `(take 6 [1 2 3 4 5])`, `take` will simply return all items. If you use `take` with an empty vector, it will return an empty list.

Remember that these functions return new sequences and do not modify the original collection. Clojure favors immutability and persistent data structures, fundamental concepts in functional programming.

## Conversion to Other Collections

### Code

```clojure
(ns collections-vectors.core)

;; ---
;; Conversion to other collections
;; ---

(apply list [1 2 3 4 5])
;; => (1 2 3 4 5)
(set [1 2 3 4 5])
;; => #{1 2 3 4 5}
(seq [1 2 3 4 5])
;; => (1 2 3 4 5)

;; convert clojure vector to hash-map
(into {} [{:a 2} {:b 4} {:c 6}])
;; => {:a 2, :b 4, :c 6}
(into {} [[:a 2] [:b 4] [:c 6]])
;; => {:a 2, :b 4, :c 6}

(apply str [1 2 3 4 5])
;; => "12345"
(str [1 2 3 4 5])
;; => "[1 2 3 4 5]"
```

### Explanation

This Clojure code demonstrates how to convert a vector to other collection types such as lists, sets, sequences, hash-maps, and strings.

Here is a detailed breakdown of each section:

1. `(apply list [1 2 3 4 5])`: The `apply` function is used to call the `list` function, with the arguments being the elements of the vector. This results in the conversion of the vector to a list.
2. `(set [1 2 3 4 5])`: The `set` function converts the vector to a set, removing any duplicates in the process. In this case, the vector does not contain any duplicate values.
3. `(seq [1 2 3 4 5])`: The `seq` function converts the vector to a sequence.
4. `(into {} [{:a 2} {:b 4} {:c 6}])`: The `into` function is used to merge the contents of a vector of hash-maps into a single hash-map. Each map in the vector is essentially being concatenated into one map.
5. `(into {} [[:a 2] [:b 4] [:c 6]])`: The `into` function can also be used with a vector of vectors. Each subvector is treated as a key-value pair and the function results in a hash-map.
6. `(apply str [1 2 3 4 5])`: Similar to the first example, `apply` is used to call the `str` function with the vector elements as arguments. This effectively concatenates the elements of the vector into a string.
7. `(str [1 2 3 4 5])`: Unlike the previous example, the `str` function is called directly on the vector. This returns a string representation of the vector, including the square brackets.

## Further Readings

- [https://clojure.org/reference/data_structures#Vectors](https://clojure.org/reference/data_structures#Vectors)
