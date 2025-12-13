---
title: "Collections: Set"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/collections_set).

</aside>

## Overview

Clojure sets are collections of unique elements, meaning each can only appear once. Sets are immutable, meaning they cannot be modified once a set is created.

Under the hood, Clojure sets are implemented as Java hash sets, which are implemented as hash tables. This means that set operations such as adding and removing elements, checking for membership, and computing intersections and unions are all very efficient.

Because Clojure sets are immutable, they are safe to use in concurrent programs without locks or other synchronization mechanisms. This can make them a good choice for working with data collections in multi-threaded applications.

## Set Creation

### Code

```clojure
(ns collections-set.core)

;; ---
;; Set creation
;; ---

#{1 2 3 4 5}
;; => #{1 4 3 2 5}
#{1 "b" 3 "d"}
;; => #{"d" 1 3 "b"}

(set [1 2 3 4 5 5 4 3 2 1])
;; => #{1 4 3 2 5}
(set '(1 2 3 4 5 5 4 3 2 1))
;; => #{1 4 3 2 5}

(class #{1 2 3 4 5})
;; => clojure.lang.PersistentHashSet

(= #{1 2 3 4 5} (set [1 2 3 4 5]))
;; => true

(conj #{1 2 3 4} 5)
;; => #{1 4 3 2 5}
(conj #{1 2 3 4} 4)
;; => #{1 4 3 2}
(disj #{1 2 3 4} 5)
;; => #{1 4 3 2}
(disj #{1 2 3 4} 4)
;; => #{1 3 2}

(cons 5 #{1 2 3 4})
;; => (5 1 4 3 2)
```

### Explanation

Here is a step-by-step explanation of the Clojure code you've given:

1. `(ns collections-set.core)`: This statement defines a new namespace named `collections-set.core`.
2. `#{1 2 3 4 5}`: This code creates a set of integers. The `#{}` notation is shorthand for creating a set in Clojure. The resulting order might differ from the original order due to the hash-based nature of sets in Clojure.
3. `#{1 "b" 3 "d"}`: Similar to the previous point, this code creates a set of mixed types, both integers and strings.
4. `(set [1 2 3 4 5 5 4 3 2 1])`: This line converts an array (vector) of numbers into a set, effectively eliminating duplicates.
5. `(set '(1 2 3 4 5 5 4 3 2 1))`: This does the same as the previous line but converts a list into a set.
6. `(class #{1 2 3 4 5})`: This expression returns the class of the given set, `clojure.lang.PersistentHashSet`.
7. `(= #{1 2 3 4 5} (set [1 2 3 4 5]))`: This line checks if the set `#{1 2 3 4 5}` is equal to the set created from the vector `[1 2 3 4 5]`. It will return `true` because both are the same set of unique elements.
8. `(conj #{1 2 3 4} 5)`: This adds the element `5` to the set `#{1 2 3 4}` using the `conj` function, which stands for "conjoin".
9. `(conj #{1 2 3 4} 4)`: Since sets can't have duplicate values, when trying to add an element already present in the set, the set remains unchanged.
10. `(disj #{1 2 3 4} 5)`: This expression attempts to remove the element `5` from the set `#{1 2 3 4}`. Since `5` is not in the set, the original set is returned.
11. `(disj #{1 2 3 4} 4)`: This expression removes the element `4` from the set `#{1 2 3 4}`.
12. `(cons 5 #{1 2 3 4})`: This adds the number `5` to the front of the sequence representation of the set `#{1 2 3 4}`, producing a list. Note that the `cons` function treats a set as a sequence, and its output is a sequence, not a set.

## Accessing Elements

### Code

```clojure
(ns collections-set.core)

;; ---
;; Accessing elements
;; ---

(first #{1 2 3 4 5})
;; => 1 (order is not guaranteed in sets)
(first #{})
;; => nil
(second #{1 2 3 4 5})
;; => 4 (order is not guaranteed in sets)

(last #{1 2 3 4 5})
;; => 5 (order is not guaranteed in sets)
(last #{1})
;; => 1
(last #{})
;; => nil
```

### Explanation

Sure, here's an explanation of the provided Clojure code:

1. `(ns collections-set.core)`: This is a namespace declaration. `ns` is a macro that is used to declare a namespace in Clojure. The argument `collections-set.core` is the name of the namespace. Namespaces in Clojure are used to prevent naming collisions, and can also be used to organize code logically.
2. `(first #{1 2 3 4 5})`: This is calling the `first` function on a set of integers. The `first` function in Clojure returns the first item of the collection. However, because a set in Clojure does not preserve the order of its elements, the returned element might not be what you'd expect, given the initial declaration.
3. `(first #{})`: This is calling the `first` function on an empty set. Since there are no elements in the set, `nil` is returned.
4. `(second #{1 2 3 4 5})`: This is calling the `second` function on a set of integers. The `second` function in Clojure returns the second item in the collection. As with the `first` function, the output might not match the expectation from the initial declaration due to the set's lack of order preservation.
5. `(last #{1 2 3 4 5})`: This is calling the `last` function on a set of integers. The `last` function in Clojure returns the last item in the collection. Once again, the output might not match the expectation from the initial declaration due to the set's lack of order preservation.
6. `(last #{1})`: This is calling the `last` function on a set with a single integer. Since there's only one element in the set, that element is considered the "last" one, and thus, `1` is returned.
7. `(last #{})`: This is calling the `last` function on an empty set. Since there are no elements in the set, `nil` is returned.

It's important to understand that sets in Clojure are unordered collections of distinct elements. When calling functions like `first`, `second`, or `last` on them, the returned element is not guaranteed to be in the order of initial declaration.

## Manipulating Sets

### Code

```clojure
(ns collections-set.core
  (:require [clojure.set :as set]))

;; ---
;; Manipulating sets
;; ---

(merge-with #{3 4 5} #{1 4 3 2 5})
;; => #{1 4 3 2 5}

(set/union #{1 4 3 2 5} #{7 6 9 10 8})
;; => #{7 1 4 6 3 2 9 5 10 8}
(set/intersection #{1 4 3 2 5} #{3 4 5 6 7})
;; => #{4 3 5}
(set/difference #{1 4 3 2 5} #{3 4 5})
;; => #{1 2}
```

### Explanation

This Clojure code is doing a few different operations on sets. Here's a step-by-step explanation of what each line of code is doing:

1. `(ns collections-set.core (:require [clojure.set :as set]))`:
   This line of code is defining a namespace called `collections-set.core`. Inside this namespace, it's also requiring the `clojure.set` library and assigning it an alias `set`. This library provides several functions to manipulate sets in Clojure.
2. `(merge-with #{3 4 5} #{1 4 3 2 5})`:
   This line appears to be a mistake. `merge-with` is a function designed for merging maps, not sets. So it won't do anything in this context. If this was intended to merge the sets, it should instead be `(set/union #{3 4 5} #{1 4 3 2 5})`.
3. `(set/union #{1 4 3 2 5} #{7 6 9 10 8})`:
   This function call uses `set/union` to create a new set, the union of the two input sets. The union of two sets is a set of all elements in either set. So the result is `#{7 1 4 6 3 2 9 5 10 8}`.
4. `(set/intersection #{1 4 3 2 5} #{3 4 5 6 7})`:
   This function call uses `set/intersection` to create a new set containing only the elements common to both input sets. So the result is `#{4 3 5}` because those are the elements present in both sets.
5. `(set/difference #{1 4 3 2 5} #{3 4 5})`:
   This function call uses `set/difference` to create a new set that contains the elements of the first set that are not in the second set. So the result is `#{1 2}` because those elements are present in the first set but not in the second.

## Predicates

### Code

```clojure
(ns collections-set.core
  (:require [clojure.set :as set]))

;; ---
;; Predicates
;; ---

(empty? #{})
;; => true
(empty? #{1 2})
;; => false

(contains? #{1 2 3 4 5} 3)
;; => true
(contains? #{1 2 3 4 5} 6)
;; => false

(set/superset? #{1 4 3 2 5} #{3 4 5})
;; => true
(set/subset? #{3 4 5} #{1 4 3 2 5})
;; => true
```

### Explanation

Here is an explanation of the Clojure code:

1. `(ns collections-set.core (:require [clojure.set :as set]))`: This line declares the namespace of the current file and also imports the `clojure.set` namespace as `set`.
2. `(empty? #{})`: This line checks if the set is empty. In this case, the set is indeed empty, so it returns `true`.
3. `(empty? #{1 2})`: This line checks if the set `{1 2}` is empty. The set contains two elements, `1` and `2`, so it's not empty and the expression returns `false`.
4. `(contains? #{1 2 3 4 5} 3)`: This line checks if the number `3` is contained in the set `{1 2 3 4 5}`. Because `3` is an element of the set, the expression returns `true`.
5. `(contains? #{1 2 3 4 5} 6)`: This line checks if the number `6` is contained in the set `{1 2 3 4 5}`. The number `6` is not in the set, so the function returns `false`.
6. `(set/superset? #{1 4 3 2 5} #{3 4 5})`: This line checks if the set `{1 4 3 2 5}` is a superset of the set `{3 4 5}`. A superset is a set that includes all elements of another set. Because all elements of `{3 4 5}` are included in `{1 4 3 2 5}`, the function returns `true`.
7. `(set/subset? #{3 4 5} #{1 4 3 2 5})`: This line checks if the set `{3 4 5}` is a subset of the set `{1 4 3 2 5}`. A subset is a set all of whose elements are included in another set. Because all elements of `{3 4 5}` are indeed in `{1 4 3 2 5}`, the function returns `true`.

In summary, this code is a series of set operations demonstrating functionality in the Clojure `set` library. These functions are all about checking the properties of sets, such as whether a set is empty, whether a set contains a specific element, and the relationship between two sets (subset and superset).

## Utility Functions

### Code

```clojure
(ns collections-set.core)

(count #{1 2 3 4 5})
;; => 5
(count #{})
;; => 0

(reverse #{1 2 3 4 5})
;; => (5 4 3 2 1) (order is not guaranteed in sets)
(reverse #{})
;; => ()

(sort #{5 3 1 2 4})
;; => (1 2 3 4 5)
(sort #{})
;; => ()
```

### Explanation

Let's break it down piece by piece:

1. `(ns collections-set.core)`: This line declares the namespace for this code. In Clojure, namespaces prevent name clashes between different parts of a program. Here, the namespace is named `collections-set.core`.
2. `(count #{1 2 3 4 5})`: The `count` function returns the number of items in a collection. `#{1 2 3 4 5}` creates a set with five elements. Sets in Clojure are unordered collections of unique elements. So, this code returns `5`, indicating 5 elements in the set.
3. `(count #{})`: This code is very similar to the previous, but instead, it counts the number of elements in an empty set, hence it returns `0`.
4. `(reverse #{1 2 3 4 5})`: This code attempts to reverse the set `#{1 2 3 4 5}`. However, because sets in Clojure are unordered, the result is not predictable and may not be in the reverse order of the original set's insertion order. This is why the comment says the order is not guaranteed in sets.
5. `(reverse #{})`: This line of code tries to reverse an empty set, which is also an unordered collection. Given that the set is empty, the reverse of it is also an empty set (or list), hence it returns an empty list `()`.
6. `(sort #{5 3 1 2 4})`: The `sort` function sorts the elements in a collection in ascending order. The code sorts the set `#{5 3 1 2 4}`, returning a list with the elements in ascending order `(1 2 3 4 5)`.
7. `(sort #{})`: This code sorts an empty set. Given that no elements are in the set to be sorted, the result is an empty list, so it returns `()`.

## Conversion to Other Collections

### Code

```clojure
(ns collections-set.core)

(apply list #{1 2 3 4 5})
;; => (1 2 3 4 5)
(apply vector #{1 2 3 4 5})
;; => [1 4 3 2 5]
(seq #{1 2 3 4 5})
;; => (1 2 3 4 5)
(apply str #{1 2 3 4 5})
;; => "14325"
(str #{1 2 3 4 5})
;; => "#{1 4 3 2 5}"
```

### Explanation

Here's an explanation of the Clojure code:

1. `(ns collections-set.core)` - This line sets the namespace to `collections-set.core`.
2. `(apply list #{1 2 3 4 5})` - This line applies the `list` function to the set `#{1 2 3 4 5}`. `apply` calls the provided function, passing the elements of the provided collection as arguments to the function. So here it transforms the set into a list. The output is `(1 2 3 4 5)`. The order of elements in a set in Clojure is not guaranteed as sets are an unordered collection.
3. `(apply vector #{1 2 3 4 5})` - Similarly, this applies the `vector` function to the same set. It creates a vector out of the set with the same elements. The output is `[1 4 3 2 5]`. Again, the order of the elements is not guaranteed.
4. `(seq #{1 2 3 4 5})` - This line returns a sequence from the set. It transforms the set into a sequence, which in this case, is represented as a list. The output is `(1 2 3 4 5)`.
5. `(apply str #{1 2 3 4 5})` - This line applies the `str` function to the set, transforming each number into a string and concatenating them together. The output is the string `"14325"`. Notice again the order is not guaranteed.
6. `(str #{1 2 3 4 5})` - Unlike the previous usage of `str`, here it's used to create a string representation of the entire set, including the curly braces `{}`. So the output is `"#{1 4 3 2 5}"`.

Overall, this code demonstrates the transformation of a set into various types in Clojure: a list, a vector, a sequence, and a string, using different functions. It also shows the difference between applying `str` to the elements of a set and getting a string representation of the set itself.
