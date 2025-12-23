---
title: Elixir Cheat Sheet
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000001
description: Essential Elixir syntax and patterns reference - basic types, pattern matching, functions, modules, OTP, Mix commands, and more
tags: ["elixir", "reference", "cheat-sheet", "syntax", "quick-reference"]
---

**Need quick Elixir syntax lookup?** This cheat sheet covers essential Elixir patterns, syntax, and commands for functional programming with OTP.

## Variables and Basic Types

```elixir
# Variables (immutable binding)
x = 5                          # Bind x to 5
x = x + 1                      # Rebinding (not mutation)

# Atoms
:ok                            # Named constant
:error
:hello_world                   # Lowercase atoms
:"hello world"                 # Atoms with spaces

# Numbers
42                             # Integer
3.14                           # Float
1_000_000                      # Underscore for readability
0x1F                           # Hexadecimal
0o777                          # Octal
0b1010                         # Binary

# Booleans (actually atoms)
true                           # Same as :true
false                          # Same as :false
nil                            # Same as :nil

# Strings
"hello"                        # Binary string (UTF-8)
"hello #{world}"               # String interpolation
~s(hello)                      # Sigil for strings
~S(no #{interpolation})        # Sigil without interpolation

# Charlists
'hello'                        # List of code points
```

## Data Structures

### Lists

```elixir
# Lists (linked lists)
[1, 2, 3]                      # List literal
[1, 2, 3] ++ [4, 5]            # Concatenation: [1, 2, 3, 4, 5]
[1, 2, 3] -- [2]               # Subtraction: [1, 3]

# Head and tail
[head | tail] = [1, 2, 3]      # head = 1, tail = [2, 3]
[1 | [2, 3]]                   # Prepend: [1, 2, 3]

# List operations
hd([1, 2, 3])                  # Head: 1
tl([1, 2, 3])                  # Tail: [2, 3]
length([1, 2, 3])              # Length: 3
```

### Tuples

```elixir
# Tuples (contiguous memory)
{:ok, 42}                      # 2-element tuple
{:error, "not found"}          # Common pattern
{a, b, c} = {1, 2, 3}          # Pattern matching

# Tuple operations
elem({:ok, 42}, 1)             # Get element: 42
put_elem({:ok, 42}, 1, 100)    # Update: {:ok, 100}
tuple_size({:ok, 42})          # Size: 2
```

### Maps

```elixir
# Maps (key-value store)
%{"name" => "Alice", "age" => 30}
%{name: "Alice", age: 30}      # Atom keys shorthand

# Access
map[:name]                     # "Alice" (atom key)
map["name"]                    # "Alice" (string key)
map.name                       # "Alice" (atom key only)

# Update
%{map | name: "Bob"}           # Update existing key
Map.put(map, :city, "NYC")     # Add new key
```

### Keyword Lists

```elixir
# Keyword lists (list of tuples with atom keys)
[name: "Alice", age: 30]       # Special syntax
[{:name, "Alice"}, {:age, 30}] # Actual representation

# Access
opts[:name]                    # "Alice"
Keyword.get(opts, :name)       # "Alice"
Keyword.fetch!(opts, :name)    # "Alice" or raises

# Common in function options
def start_link(opts \\ []) do
  name = Keyword.get(opts, :name, "default")
end
```

### Structs

```elixir
# Define struct
defmodule User do
  defstruct name: "", age: 0
end

# Create
%User{name: "Alice", age: 30}
%User{}                        # Default values

# Update
%{user | age: 31}

# Pattern match
%User{name: name} = user
```

## Pattern Matching

```elixir
# Basic matching
{a, b} = {1, 2}                # a = 1, b = 2
[head | tail] = [1, 2, 3]      # head = 1, tail = [2, 3]

# Pin operator (match existing value)
x = 1
^x = 1                         # OK (x is 1)
^x = 2                         # Error (x != 2)

# Ignore values
{:ok, _} = {:ok, 42}           # Ignore value
{_, value} = {:ok, 42}         # value = 42

# Match in function heads
def greet(%{name: name}) do
  "Hello, #{name}"
end

# Match with guards
def check(x) when x > 0, do: :positive
def check(0), do: :zero
def check(x) when x < 0, do: :negative
```

## Operators

### Comparison

```elixir
1 == 1                         # Equality (true)
1 != 2                         # Inequality (true)
1 === 1                        # Strict equality (true)
1 === 1.0                      # false (different types)

1 < 2                          # Less than
1 <= 2                         # Less than or equal
1 > 0                          # Greater than
1 >= 1                         # Greater than or equal
```

### Boolean

```elixir
# Strict (require booleans)
true and false                 # false
true or false                  # true
not true                       # false

# Relaxed (any truthy/falsy)
nil || 42                      # 42 (first truthy)
false || 42                    # 42
42 || false                    # 42 (short-circuit)

nil && 42                      # nil (first falsy)
true && 42                     # 42
42 && false                    # false

!nil                           # true
!42                            # false (everything except nil/false is truthy)
```

### Arithmetic

```elixir
1 + 2                          # 3
5 - 3                          # 2
3 * 4                          # 12
10 / 2                         # 5.0 (always float)
div(10, 2)                     # 5 (integer division)
rem(10, 3)                     # 1 (remainder)
```

### String

```elixir
"hello" <> " world"            # Concatenation
"abc" =~ ~r/b/                 # Regex match (true)
```

### Others

```elixir
[1, 2] ++ [3]                  # List concatenation: [1, 2, 3]
[1, 2, 3] -- [2]               # List subtraction: [1, 3]
```

## Functions

### Anonymous Functions

```elixir
# Definition
add = fn a, b -> a + b end
add.(1, 2)                     # 3 (note the dot)

# Shorthand
add = &(&1 + &2)
add.(1, 2)                     # 3

# Multi-clause
fizzbuzz = fn
  0, 0, _ -> "FizzBuzz"
  0, _, _ -> "Fizz"
  _, 0, _ -> "Buzz"
  _, _, x -> x
end
```

### Named Functions

```elixir
defmodule Math do
  # Public function
  def add(a, b) do
    a + b
  end

  # Private function
  defp validate(x) when x > 0, do: :ok

  # Default arguments
  def greet(name, greeting \\ "Hello") do
    "#{greeting}, #{name}!"
  end

  # Multiple clauses
  def fib(0), do: 0
  def fib(1), do: 1
  def fib(n), do: fib(n - 1) + fib(n - 2)
end
```

### Guards

```elixir
def check(x) when is_integer(x), do: :int
def check(x) when is_float(x), do: :float
def check(x) when is_binary(x), do: :string

# Common guards
when is_atom(x)
when is_list(x)
when is_map(x)
when is_number(x)
when x > 0 and x < 100
when rem(x, 2) == 0
```

## Pipe Operator

```elixir
# Without pipe
String.upcase(String.trim("  hello  "))

# With pipe
"  hello  "
|> String.trim()
|> String.upcase()
# "HELLO"

# Pipe into function calls
1..10
|> Enum.map(&(&1 * 2))
|> Enum.filter(&(&1 > 10))
|> Enum.sum()
```

## Control Flow

### case

```elixir
case {x, y} do
  {1, _} -> "x is 1"
  {_, 2} -> "y is 2"
  {a, b} when a > b -> "x > y"
  _ -> "default"
end
```

### cond

```elixir
cond do
  x < 0 -> "negative"
  x == 0 -> "zero"
  x > 0 -> "positive"
end
```

### if/unless

```elixir
if condition do
  "true branch"
else
  "false branch"
end

unless condition do
  "false branch"
else
  "true branch"
end
```

### with

```elixir
with {:ok, user} <- fetch_user(id),
     {:ok, posts} <- fetch_posts(user),
     {:ok, comments} <- fetch_comments(posts) do
  {:ok, {user, posts, comments}}
else
  {:error, reason} -> {:error, reason}
end
```

## Modules

```elixir
defmodule MyModule do
  # Module attribute
  @greeting "Hello"

  # Public function
  def say_hello(name) do
    "#{@greeting}, #{name}!"
  end

  # Private function
  defp internal do
    :private
  end
end

# Use module
MyModule.say_hello("World")
```

### Import/Alias/Require

```elixir
# Import (bring functions into scope)
import List
first([1, 2, 3])               # Instead of List.first/1

# Alias (shorten module name)
alias MyApp.UserController, as: UC
UC.index(conn, params)

# Require (for macros)
require Logger
Logger.debug("message")
```

## Comprehensions

```elixir
# List comprehension
for x <- 1..5, do: x * 2
# [2, 4, 6, 8, 10]

# With filter
for x <- 1..10, rem(x, 2) == 0, do: x
# [2, 4, 6, 8, 10]

# Multiple generators
for x <- 1..3, y <- 1..2, do: {x, y}
# [{1, 1}, {1, 2}, {2, 1}, {2, 2}, {3, 1}, {3, 2}]

# Into (collect into different structure)
for {k, v} <- %{a: 1, b: 2}, into: %{}, do: {k, v * 2}
# %{a: 2, b: 4}
```

## Enumerables

```elixir
# Enum module (eager)
Enum.map([1, 2, 3], &(&1 * 2))          # [2, 4, 6]
Enum.filter([1, 2, 3, 4], &(&1 > 2))    # [3, 4]
Enum.reduce([1, 2, 3], 0, &(&1 + &2))   # 6
Enum.take([1, 2, 3, 4, 5], 3)           # [1, 2, 3]
Enum.sort([3, 1, 2])                    # [1, 2, 3]
Enum.uniq([1, 2, 1, 3, 2])              # [1, 2, 3]

# Stream module (lazy)
Stream.map([1, 2, 3], &(&1 * 2))        # Stream (not evaluated)
|> Enum.to_list()                       # [2, 4, 6]

# Infinite streams
Stream.cycle([1, 2, 3])                 # [1, 2, 3, 1, 2, 3, ...]
|> Stream.take(5)
|> Enum.to_list()                       # [1, 2, 3, 1, 2]
```

## Common String Functions

```elixir
String.length("hello")                  # 5
String.upcase("hello")                  # "HELLO"
String.downcase("HELLO")                # "hello"
String.trim("  hello  ")                # "hello"
String.split("a,b,c", ",")              # ["a", "b", "c"]
String.replace("hello", "l", "L")       # "heLLo"
String.contains?("hello", "ell")        # true
String.starts_with?("hello", "he")      # true
String.slice("hello", 0, 2)             # "he"
```

## Processes

```elixir
# Spawn process
pid = spawn(fn -> IO.puts("Hello") end)

# Send message
send(pid, {:hello, "world"})

# Receive message
receive do
  {:hello, msg} -> IO.puts(msg)
  {:error, _} -> IO.puts("Error")
after
  5000 -> IO.puts("Timeout")
end

# Link processes
spawn_link(fn -> :work end)

# Monitor processes
ref = Process.monitor(pid)

# Current process
self()                                  # PID of current process
Process.alive?(pid)                     # Check if process alive
```

## OTP Basics

### GenServer

```elixir
defmodule Counter do
  use GenServer

  # Client API
  def start_link(initial \\ 0) do
    GenServer.start_link(__MODULE__, initial, name: __MODULE__)
  end

  def increment do
    GenServer.call(__MODULE__, :increment)
  end

  # Server callbacks
  def init(initial), do: {:ok, initial}

  def handle_call(:increment, _from, state) do
    {:reply, state + 1, state + 1}
  end
end
```

### Supervisor

```elixir
defmodule MyApp.Supervisor do
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

  def init(:ok) do
    children = [
      {Counter, 0},
      {MyWorker, []}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

## Mix Commands

```bash
# Project management
mix new my_app                 # Create new project
mix new my_app --sup           # With supervision tree
mix compile                    # Compile project
mix clean                      # Clean build artifacts

# Dependencies
mix deps.get                   # Fetch dependencies
mix deps.update --all          # Update all dependencies
mix deps.clean --all           # Remove dependencies

# Testing
mix test                       # Run all tests
mix test test/some_test.exs    # Run specific test
mix test --trace               # Run with detailed trace

# Documentation
mix docs                       # Generate documentation
mix hex.docs open              # Open docs in browser

# Interactive
iex -S mix                     # Start IEx with project

# Releases
mix release                    # Build release
mix release --overwrite        # Rebuild release

# Formatting
mix format                     # Format code
mix format --check-formatted   # Check if formatted

# Code analysis
mix compile --warnings-as-errors
mix dialyzer                   # Type checking (requires dialyxir)
```

## IEx Commands

```elixir
# Help
h                              # Help
h(Enum)                        # Module help
h(Enum.map)                    # Function help

# Inspection
i(value)                       # Inspect value
v()                            # Last result
v(2)                           # Result 2 lines ago

# Compilation
c "path/to/file.ex"            # Compile file
r MyModule                     # Recompile module

# Process info
self()                         # Current process PID
Process.list()                 # All processes
Process.info(pid)              # Process details

# Other
clear                          # Clear screen
respawn                        # Restart IEx session
```

## Sigils

```elixir
# Strings
~s(hello)                      # String
~S(no #{interpolation})        # String without interpolation

# Charlists
~c(hello)                      # Charlist
~C(no #{interpolation})        # Charlist without interpolation

# Regex
~r/\d+/                        # Regex
~r/\d+/i                       # Case-insensitive regex

# Words
~w(foo bar baz)                # ["foo", "bar", "baz"]
~w(foo bar baz)a               # [:foo, :bar, :baz] (atoms)
~w(foo bar baz)c               # ['foo', 'bar', 'baz'] (charlists)

# Date/Time
~D[2024-12-21]                 # Date
~T[12:30:45]                   # Time
~N[2024-12-21 12:30:45]        # NaiveDateTime
```

## Error Handling

```elixir
# Try/rescue
try do
  raise "error"
rescue
  e in RuntimeError -> IO.puts("Caught: #{e.message}")
end

# Try/catch
try do
  throw(:error)
catch
  :error -> :caught
end

# After (cleanup)
try do
  risky_operation()
after
  cleanup()
end

# Common result patterns
{:ok, result}
{:error, reason}

# Case for error handling
case File.read("file.txt") do
  {:ok, content} -> process(content)
  {:error, :enoent} -> "File not found"
  {:error, reason} -> "Error: #{reason}"
end
```

## Common Patterns

### Optional Chaining

```elixir
# get_in for nested access
data = %{user: %{name: "Alice"}}
get_in(data, [:user, :name])           # "Alice"
get_in(data, [:user, :age])            # nil

# Access behaviour
user = %{name: "Alice", age: 30}
user[:name]                            # "Alice"
user[:missing]                         # nil
```

### Pipeline Error Handling

```elixir
with {:ok, user} <- fetch_user(id),
     {:ok, profile} <- fetch_profile(user) do
  {:ok, {user, profile}}
end
```

### Resource Management

```elixir
# File handle cleanup
File.open("file.txt", [:read], fn file ->
  IO.read(file, :line)
end)

# Database transaction
Repo.transaction(fn ->
  # Operations
end)
```

Need deep dives? Check out [Tutorials](/en/learn/software-engineering/programming-language/elixir/tutorials/) for comprehensive learning paths.
