---
title: "Collections: Manipulation"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/collections_manipulation).

</aside>

## Manipulating Clojure's Collections

### Code

```clojure
(ns collections-manipulation.core)

;; Map

(map (fn [x] (+ x 1)) '(1 2 3 4 5))
;; => (2 3 4 5 6)
(map #(+ % 1) [1 2 3 4 5])
;; => (2 3 4 5 6)
(map inc #{1 2 3 4 5})
;; => (2 5 4 3 6)
(map (fn [x] (val x)) {:a 1 :b 2 :c 3 :d 4 :e 5})
;; => (1 2 3 4 5)

;; Filter

(filter (fn [x] (even? x)) '(1 2 3 4 5))
;; => (2 4)
(filter #(even? %) [1 2 3 4 5])
;; => (2 4)
(filter even? #{1 2 3 4 5})
;; => (4 2)
(filter (fn [x] (even? (val x))) {:a 1 :b 2 :c 3})
;; => ([:b 2])

;; Reduce

(reduce (fn [acc x] (str acc x)) "" ["a" "b" "c"])
;; => "abc"
(reduce #(str %1 %2) "" ["a" "b" "c"])
;; => "abc"
(reduce str "" #{"a" "b" "c" "d"})
;; => "dabc"
(reduce (fn [x y] (str x (val y))) "" {:a "a" :b "b" :c "c"})
;; => "abc"

;; Side effects

(run! println [1 2 3 4 5])
;; => nil
(run! println '(1 2 3 4 5))
;; => nil
(run! println #{1 2 3 4 5})
;; => nil
(run! #(println (val %)) {:a 1 :b 2 :c 3 :d 4 :e 5})
;; => nil

(doseq [x [1 2 3 4 5]]
  (println x))
;; => nil
(doseq [x '(1 2 3 4 5)]
  (println x))
;; => nil
(doseq [x #{1 2 3 4 5}]
  (println x))
;; => nil
(doseq [x {:a 1 :b 2 :c 3 :d 4 :e 5}]
  (println (val x)))
;; => nil

;; Immutability

(def a-vector [1 2 3 4 5])
(def a-list '(1 2 3 4 5))
(def a-set #{1 2 3 4 5})
(def a-map {:a 1 :b 2 :c 3 :d 4 :e 5})

(map inc a-vector)
;; => (2 3 4 5 6)
a-vector
;; => [1 2 3 4 5]

(filter even? a-list)
;; => (2 4)
a-list
;; => (1 2 3 4 5)

a-set
;; => #{1 4 3 2 5}
(reduce str "" a-set)
;; => "14325"
a-set
;; => #{1 4 3 2 5}

(run! #(println (val %)) a-map)
;; => nil
a-map
;; => {:a 1, :b 2, :c 3, :d 4, :e 5}
(doseq [x a-map]
  (println (val x)))
;; => nil
a-map
;; => {:a 1, :b 2, :c 3, :d 4, :e 5}
```

### Explanation

Here's a breakdown of the Clojure code:

1. Namespace: `(ns collections-manipulation.core)` is declaring the namespace for the code that follows.
2. Map: Map applies a function to every item in a collection and returns a new collection with the results.
   - `(map (fn (+ x 1)) '(1 2 3 4 5))`: This is mapping an anonymous function that adds 1 to each element of the list.
   - `(map #(+ % 1) [1 2 3 4 5])`: This does the same thing, but uses a shorter syntax for the anonymous function.
   - `(map inc #{1 2 3 4 5})`: The `inc` function increments its argument by 1, so this is equivalent to the previous examples.
   - `(map (fn (val x)) {:a 1 :b 2 :c 3 :d 4 :e 5})`: This maps the `val` function (which retrieves the value from a key-value pair) over a map, effectively returning the values.
3. Filter: Filter applies a predicate function to each item in a collection and returns a new collection with only the items where the predicate returns true.
   - `(filter (fn (even? x)) '(1 2 3 4 5))`: Filters the list to only include even numbers.
   - `(filter #(even? %) [1 2 3 4 5])`: Does the same thing using a shorter syntax.
   - `(filter even? #{1 2 3 4 5})`: Equivalent to the previous examples.
   - `(filter (fn (even? (val x))) {:a 1 :b 2 :c 3})`: Filters the map to only include key-value pairs where the value is even.
4. Reduce: Reduce applies a binary function to a start value and each item in a collection in sequence, and returns a single value that aggregates the result.
   - `(reduce (fn [acc x] (str acc x)) "" ["a" "b" "c"])`: Concatenates the strings in the vector.
   - `(reduce #(str %1 %2) "" ["a" "b" "c"])`: Does the same thing using a shorter syntax.
   - `(reduce str "" #{"a" "b" "c" "d"})`: Concatenates the strings in the set.
   - `(reduce (fn [x y] (str x (val y))) "" {:a "a" :b "b" :c "c"})`: Concatenates the values in the map.
5. Side Effects: `run!` and `doseq` are used for side effects. They do not return meaningful values.
   - `(run! println [1 2 3 4 5])`: Prints each element in the vector.
   - `(run! #(println (val %)) {:a 1 :b 2 :c 3 :d 4 :e 5})`: Prints each value in the map.
   - `(doseq [x [1 2 3 4 5]] (println x))`: Does the same thing as `run!` in this context.
6. Immutability: Clojure's data structures are immutable. All "changes" to data are creating new data.
   - `(map inc a-vector)`: Returns a new vector with each element incremented, but `a-vector` remains unchanged.
   - `(filter even? a-list)`: Returns a new list with only the even numbers, but `a-list` remains unchanged.
   - `(reduce str "" a-set)`: Returns a string that concatenates all the elements in the set, but `a-set` remains unchanged.
   - `(run! #(println (val %)) a-map)`: Prints each value in the map, but `a-map` remains unchanged.
   - `(doseq [x a-map] (println (val x)))`: Does the same thing as `run!` in this context, and `a-map` remains unchanged.
