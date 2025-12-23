---
title: "Recursion"
date: 2025-12-21T16:55:00+07:00
draft: false
description: "Master recursive programming in Elixir with tail-recursive patterns, accumulator techniques, and practical list processing examples for efficient functional code."
weight: 1000003
tags:
  ["elixir", "recursion", "tail-recursion", "functional-programming", "how-to"]
---

**Need to process lists or trees efficiently in Elixir?** This guide teaches you recursive programming patterns with tail-recursion optimization, accumulator techniques, and practical examples for common data processing tasks.

## Prerequisites

- Basic Elixir syntax (pattern matching, functions)
- Understanding of lists and tuples
- Completed [Beginner Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner) or equivalent

## Problem

You need to process collections (lists, trees) or solve problems that naturally decompose into smaller subproblems. Elixir doesn't have traditional loops (`for`, `while`), so recursion is the primary iteration mechanism.

**Challenges:**

- Stack overflow from non-tail-recursive functions
- Inefficient accumulation patterns
- Choosing between recursion and higher-order functions (Enum)
- Understanding when tail-call optimization applies

## Solution

Use **tail-recursive patterns** with accumulators for efficient iteration. Elixir optimizes tail calls into constant-space loops.

### Key Patterns

1. **Basic Recursion** - Simple recursive decomposition
2. **Tail Recursion** - Accumulator-based patterns (constant stack space)
3. **Multiple Recursion** - Tree traversal and divide-and-conquer
4. **Mutual Recursion** - Interdependent recursive functions

## How It Works

### 1. Basic Recursion

**Pattern**: Decompose problem into base case + recursive case.

```elixir
defmodule BasicRecursion do
  # Sum list elements (non-tail-recursive)
  def sum([]), do: 0
  def sum([head | tail]), do: head + sum(tail)

  # Length of list (non-tail-recursive)
  def length([]), do: 0
  def length([_head | tail]), do: 1 + length(tail)

  # Reverse list (inefficient - O(n²))
  def reverse([]), do: []
  def reverse([head | tail]), do: reverse(tail) ++ [head]
end

# Usage
BasicRecursion.sum([1, 2, 3, 4])      # 10
BasicRecursion.length([1, 2, 3])       # 3
BasicRecursion.reverse([1, 2, 3])      # [3, 2, 1]
```

**Limitation**: Non-tail-recursive functions build stack frames. For large inputs (10,000+ elements), risk stack overflow.

### 2. Tail Recursion with Accumulators

**Pattern**: Pass accumulator parameter, return it in base case.

```elixir
defmodule TailRecursion do
  # Sum with accumulator (tail-recursive)
  def sum(list), do: sum(list, 0)
  defp sum([], acc), do: acc
  defp sum([head | tail], acc), do: sum(tail, acc + head)

  # Length with accumulator (tail-recursive)
  def length(list), do: length(list, 0)
  defp length([], acc), do: acc
  defp length([_head | tail], acc), do: length(tail, acc + 1)

  # Reverse with accumulator (tail-recursive, O(n))
  def reverse(list), do: reverse(list, [])
  defp reverse([], acc), do: acc
  defp reverse([head | tail], acc), do: reverse(tail, [head | acc])

  # Map with accumulator (tail-recursive)
  def map(list, func), do: map(list, func, [])
  defp map([], _func, acc), do: reverse(acc)
  defp map([head | tail], func, acc) do
    map(tail, func, [func.(head) | acc])
  end

  # Filter with accumulator (tail-recursive)
  def filter(list, predicate), do: filter(list, predicate, [])
  defp filter([], _predicate, acc), do: reverse(acc)
  defp filter([head | tail], predicate, acc) do
    if predicate.(head) do
      filter(tail, predicate, [head | acc])
    else
      filter(tail, predicate, acc)
    end
  end
end

# Usage
TailRecursion.sum([1, 2, 3, 4])                   # 10
TailRecursion.reverse([1, 2, 3])                   # [3, 2, 1]
TailRecursion.map([1, 2, 3], fn x -> x * 2 end)   # [2, 4, 6]
TailRecursion.filter([1, 2, 3, 4], fn x -> rem(x, 2) == 0 end)  # [2, 4]
```

**Why tail-recursive?**

- Final operation is recursive call (no pending work after recursion)
- Elixir optimizes tail calls to reuse stack frame (constant space)
- Can process millions of elements without stack overflow

### 3. Reduce Pattern (Fold)

**Pattern**: Generalize accumulator pattern with reducer function.

```elixir
defmodule ReducePattern do
  # Left fold (tail-recursive)
  def reduce([], acc, _func), do: acc
  def reduce([head | tail], acc, func) do
    reduce(tail, func.(head, acc), func)
  end

  # Right fold (non-tail-recursive, preserves order)
  def reduce_right([], acc, _func), do: acc
  def reduce_right([head | tail], acc, func) do
    func.(head, reduce_right(tail, acc, func))
  end

  # Examples built on reduce
  def sum(list), do: reduce(list, 0, fn x, acc -> x + acc end)
  def product(list), do: reduce(list, 1, fn x, acc -> x * acc end)

  def join(list, separator) do
    reduce(list, "", fn x, acc ->
      if acc == "", do: "#{x}", else: "#{acc}#{separator}#{x}"
    end)
  end
end

# Usage
ReducePattern.reduce([1, 2, 3], 0, fn x, acc -> x + acc end)  # 6
ReducePattern.sum([1, 2, 3, 4])                                # 10
ReducePattern.product([2, 3, 4])                               # 24
ReducePattern.join(["a", "b", "c"], ", ")                      # "a, b, c"
```

### 4. Multiple Recursion (Tree Processing)

**Pattern**: Recursive calls on multiple subproblems.

```elixir
defmodule TreeRecursion do
  # Binary tree node: {:node, value, left, right} or :leaf

  # Calculate tree size
  def size(:leaf), do: 0
  def size({:node, _value, left, right}) do
    1 + size(left) + size(right)
  end

  # Calculate tree height
  def height(:leaf), do: 0
  def height({:node, _value, left, right}) do
    1 + max(height(left), height(right))
  end

  # Sum all values in tree
  def sum(:leaf), do: 0
  def sum({:node, value, left, right}) do
    value + sum(left) + sum(right)
  end

  # Find if value exists in tree
  def contains?(:leaf, _value), do: false
  def contains?({:node, value, _left, _right}, value), do: true
  def contains?({:node, _node_value, left, right}, search_value) do
    contains?(left, search_value) or contains?(right, search_value)
  end

  # Map function over tree
  def map(:leaf, _func), do: :leaf
  def map({:node, value, left, right}, func) do
    {:node, func.(value), map(left, func), map(right, func)}
  end
end

# Usage
tree = {:node, 5,
         {:node, 3,
           {:node, 1, :leaf, :leaf},
           {:node, 4, :leaf, :leaf}},
         {:node, 8,
           {:node, 6, :leaf, :leaf},
           :leaf}}

TreeRecursion.size(tree)          # 6
TreeRecursion.height(tree)        # 3
TreeRecursion.sum(tree)           # 27
TreeRecursion.contains?(tree, 4)  # true
TreeRecursion.contains?(tree, 9)  # false

doubled = TreeRecursion.map(tree, fn x -> x * 2 end)
TreeRecursion.sum(doubled)        # 54
```

**Note**: Multiple recursion is NOT tail-recursive (pending work after each recursive call). Acceptable for tree structures with reasonable depth (<10,000 levels).

### 5. Divide and Conquer

**Pattern**: Split problem, recurse on parts, combine results.

```elixir
defmodule DivideConquer do
  # Merge sort
  def merge_sort([]), do: []
  def merge_sort([x]), do: [x]
  def merge_sort(list) do
    {left, right} = split(list)
    merge(merge_sort(left), merge_sort(right))
  end

  defp split(list) do
    mid = div(length(list), 2)
    Enum.split(list, mid)
  end

  defp merge([], right), do: right
  defp merge(left, []), do: left
  defp merge([l | left_tail] = left, [r | right_tail] = right) do
    if l <= r do
      [l | merge(left_tail, right)]
    else
      [r | merge(left, right_tail)]
    end
  end

  # Quick sort
  def quick_sort([]), do: []
  def quick_sort([pivot | tail]) do
    {smaller, larger} = partition(tail, pivot)
    quick_sort(smaller) ++ [pivot] ++ quick_sort(larger)
  end

  defp partition(list, pivot) do
    Enum.split_with(list, fn x -> x < pivot end)
  end

  # Binary search (on sorted list)
  def binary_search(list, target) do
    binary_search(list, target, 0, length(list) - 1)
  end

  defp binary_search(_list, _target, low, high) when low > high, do: nil
  defp binary_search(list, target, low, high) do
    mid = div(low + high, 2)
    mid_value = Enum.at(list, mid)

    cond do
      mid_value == target -> mid
      mid_value < target -> binary_search(list, target, mid + 1, high)
      mid_value > target -> binary_search(list, target, low, mid - 1)
    end
  end
end

# Usage
DivideConquer.merge_sort([5, 2, 8, 1, 9])       # [1, 2, 5, 8, 9]
DivideConquer.quick_sort([5, 2, 8, 1, 9])       # [1, 2, 5, 8, 9]

sorted = [1, 3, 5, 7, 9, 11, 13]
DivideConquer.binary_search(sorted, 7)          # 3 (index)
DivideConquer.binary_search(sorted, 4)          # nil
```

### 6. Mutual Recursion

**Pattern**: Functions call each other recursively.

```elixir
defmodule MutualRecursion do
  # Even/odd check via mutual recursion
  def even?(0), do: true
  def even?(n) when n > 0, do: odd?(n - 1)

  def odd?(0), do: false
  def odd?(n) when n > 0, do: even?(n - 1)

  # Parse nested structure (simplified)
  def parse_expr(tokens) do
    {expr, rest} = parse_term(tokens)
    parse_expr_rest(expr, rest)
  end

  defp parse_term([num | rest]) when is_number(num), do: {num, rest}
  defp parse_term(["(" | rest]) do
    {expr, [")" | rest2]} = parse_expr(rest)
    {expr, rest2}
  end

  defp parse_expr_rest(left, ["+" | rest]) do
    {right, rest2} = parse_term(rest)
    parse_expr_rest(left + right, rest2)
  end
  defp parse_expr_rest(result, rest), do: {result, rest}
end

# Usage
MutualRecursion.even?(4)   # true
MutualRecursion.odd?(5)    # true
MutualRecursion.even?(7)   # false

MutualRecursion.parse_expr([1, "+", 2, "+", 3])  # {6, []}
MutualRecursion.parse_expr(["(", 1, "+", 2, ")", "+", 3])  # {6, []}
```

### 7. Recursion with Multiple Accumulators

**Pattern**: Track multiple pieces of state during recursion.

```elixir
defmodule MultipleAccumulators do
  # Count evens and odds
  def count_parity(list), do: count_parity(list, 0, 0)

  defp count_parity([], evens, odds), do: {evens, odds}
  defp count_parity([head | tail], evens, odds) do
    if rem(head, 2) == 0 do
      count_parity(tail, evens + 1, odds)
    else
      count_parity(tail, evens, odds + 1)
    end
  end

  # Find min and max in single pass
  def min_max([first | rest]), do: min_max(rest, first, first)

  defp min_max([], min, max), do: {min, max}
  defp min_max([head | tail], min, max) do
    min_max(tail, min(head, min), max(head, max))
  end

  # Partition list into smaller, equal, larger
  def partition_by_pivot(list, pivot), do: partition_by_pivot(list, pivot, [], [], [])

  defp partition_by_pivot([], _pivot, smaller, equal, larger) do
    {Enum.reverse(smaller), Enum.reverse(equal), Enum.reverse(larger)}
  end
  defp partition_by_pivot([head | tail], pivot, smaller, equal, larger) do
    cond do
      head < pivot -> partition_by_pivot(tail, pivot, [head | smaller], equal, larger)
      head == pivot -> partition_by_pivot(tail, pivot, smaller, [head | equal], larger)
      head > pivot -> partition_by_pivot(tail, pivot, smaller, equal, [head | larger])
    end
  end
end

# Usage
MultipleAccumulators.count_parity([1, 2, 3, 4, 5, 6])  # {3, 3}
MultipleAccumulators.min_max([5, 2, 8, 1, 9])          # {1, 9}
MultipleAccumulators.partition_by_pivot([5, 2, 8, 5, 1, 9, 5], 5)
# {[2, 1], [5, 5, 5], [8, 9]}
```

## Variations

### When to Use Recursion vs Enum

**Use Recursion When:**

- Implementing custom iteration logic not in Enum
- Processing non-list structures (trees, graphs)
- Need precise control over iteration
- Building library functions

**Use Enum When:**

- Standard operations (map, filter, reduce)
- Readability over performance optimization
- One-off data processing

```elixir
# Recursion (custom logic)
def take_while([], _predicate), do: []
def take_while([head | tail], predicate) do
  if predicate.(head) do
    [head | take_while(tail, predicate)]
  else
    []
  end
end

# Enum (standard operations)
list = [1, 2, 3, 4, 5]
Enum.take_while(list, fn x -> x < 4 end)  # [1, 2, 3]
```

### Tail-Recursive vs Non-Tail-Recursive

**Tail-Recursive (Preferred for Large Inputs):**

```elixir
def factorial(n), do: factorial(n, 1)
defp factorial(0, acc), do: acc
defp factorial(n, acc), do: factorial(n - 1, n * acc)
```

**Non-Tail-Recursive (Simpler, OK for Small Inputs):**

```elixir
def factorial(0), do: 1
def factorial(n), do: n * factorial(n - 1)
```

### Recursion with Guards

```elixir
defmodule GuardedRecursion do
  def fibonacci(n) when n < 0, do: {:error, "negative input"}
  def fibonacci(0), do: 0
  def fibonacci(1), do: 1
  def fibonacci(n) when n > 1, do: fibonacci(n - 1) + fibonacci(n - 2)

  # Optimized with accumulator
  def fib_fast(n), do: fib_fast(n, 0, 1)
  defp fib_fast(0, a, _b), do: a
  defp fib_fast(n, a, b) when n > 0, do: fib_fast(n - 1, b, a + b)
end

GuardedRecursion.fibonacci(10)     # 55
GuardedRecursion.fib_fast(10)      # 55 (much faster for large n)
```

## Pitfalls

### Stack Overflow from Non-Tail Recursion

```elixir
# BAD: Non-tail-recursive for large lists
def sum([]), do: 0
def sum([head | tail]), do: head + sum(tail)

sum(Enum.to_list(1..100_000))  # Stack overflow!

# GOOD: Tail-recursive
def sum(list), do: sum(list, 0)
defp sum([], acc), do: acc
defp sum([head | tail], acc), do: sum(tail, acc + head)

sum(Enum.to_list(1..100_000))  # 5000050000 (no stack overflow)
```

### Inefficient List Concatenation

```elixir
# BAD: O(n²) - appending to end repeatedly
def reverse([]), do: []
def reverse([head | tail]), do: reverse(tail) ++ [head]

# GOOD: O(n) - prepend with accumulator
def reverse(list), do: reverse(list, [])
defp reverse([], acc), do: acc
defp reverse([head | tail], acc), do: reverse(tail, [head | acc])
```

### Forgetting to Reverse Accumulator

```elixir
# BAD: Order reversed
def map(list, func), do: map(list, func, [])
defp map([], _func, acc), do: acc  # Oops! Reversed order
defp map([head | tail], func, acc) do
  map(tail, func, [func.(head) | acc])
end

map([1, 2, 3], fn x -> x * 2 end)  # [6, 4, 2] (WRONG!)

# GOOD: Reverse accumulator in base case
def map(list, func), do: map(list, func, [])
defp map([], _func, acc), do: Enum.reverse(acc)
defp map([head | tail], func, acc) do
  map(tail, func, [func.(head) | acc])
end

map([1, 2, 3], fn x -> x * 2 end)  # [2, 4, 6] (CORRECT!)
```

### Inefficient Fibonacci (Exponential Time)

```elixir
# BAD: O(2^n) - recalculates same values
def fib(0), do: 0
def fib(1), do: 1
def fib(n), do: fib(n - 1) + fib(n - 2)

fib(35)  # Takes seconds!

# GOOD: O(n) - linear with accumulator
def fib(n), do: fib(n, 0, 1)
defp fib(0, a, _b), do: a
defp fib(n, a, b), do: fib(n - 1, b, a + b)

fib(35)  # Instant!
```

## Use Cases

**Data Processing:**

- List transformations (map, filter, reduce)
- Aggregations (sum, count, min/max)
- Partitioning and grouping

**Tree/Graph Algorithms:**

- Tree traversal (DFS, BFS)
- Path finding
- Structure validation

**Parsing:**

- Recursive descent parsers
- Expression evaluation
- Nested structure processing

**Mathematical Computations:**

- Factorial, Fibonacci
- Greatest common divisor (GCD)
- Combinatorics

## Related Resources

- [Pattern Matching Guide](/en/learn/software-engineering/programming-language/elixir/how-to/pattern-matching) - Essential for recursive patterns
- [Beginner Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner) - Recursion fundamentals
- [Cookbook](/en/learn/software-engineering/programming-language/elixir/how-to/cookbook) - Quick reference recipes
- [Best Practices](/en/learn/software-engineering/programming-language/elixir/explanation/best-practices) - Recursion guidelines

## Next Steps

1. Practice converting iterative algorithms to recursive forms
2. Implement common algorithms (sorting, searching) recursively
3. Study Enum module source code for real-world patterns
4. Learn when to use recursion vs higher-order functions
5. Explore GenServer for stateful recursive processes
