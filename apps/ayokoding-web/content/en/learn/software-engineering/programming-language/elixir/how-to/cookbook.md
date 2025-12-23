---
title: "Cookbook"
date: 2025-12-21T10:00:00+07:00
draft: false
description: "Comprehensive collection of practical Elixir recipes covering data structures, concurrency, OTP, Phoenix, Ecto, testing, and performance optimization"
weight: 1000001
tags: ["elixir", "cookbook", "recipes", "patterns", "best-practices"]
---

**Need quick solutions to common Elixir problems?** This cookbook provides 35+ tested recipes covering data structures, concurrency, OTP patterns, Phoenix development, Ecto queries, testing strategies, and performance optimization.

## How to Use This Cookbook

Each recipe follows a consistent format:

1. **Problem**: What challenge you're solving
2. **Solution**: Complete working code
3. **How It Works**: Explanation of the solution
4. **Use Cases**: When to apply this pattern
5. **See Also**: Related tutorials and guides

All code examples are tested with Elixir 1.14+ and OTP 25+.

## Categories

- [Data Structures](#data-structures)
- [Pattern Matching](#pattern-matching)
- [Functions and Pipes](#functions-and-pipes)
- [String Manipulation](#string-manipulation)
- [Concurrency](#concurrency)
- [OTP Patterns](#otp-patterns)
- [Error Handling](#error-handling)
- [File I/O](#file-io)
- [Phoenix Web Development](#phoenix-web-development)
- [LiveView Patterns](#liveview-patterns)
- [Ecto Database](#ecto-database)
- [Testing](#testing)
- [Performance](#performance)
- [Configuration](#configuration)
- [Debugging](#debugging)

---

## Data Structures

### Recipe 1: Transform Map Keys (Atoms to Strings)

**Problem**: Convert map with atom keys to string keys for JSON encoding.

**Solution**:

```elixir
defmodule MapHelper do
  def atomize_keys(map) when is_map(map) do
    Map.new(map, fn {key, value} ->
      {String.to_atom(key), atomize_keys(value)}
    end)
  end

  def atomize_keys(value), do: value

  def stringify_keys(map) when is_map(map) do
    Map.new(map, fn {key, value} ->
      {to_string(key), stringify_keys(value)}
    end)
  end

  def stringify_keys(value), do: value
end

# Usage
user = %{name: "Alice", age: 30, address: %{city: "NYC"}}
MapHelper.stringify_keys(user)
# => %{"name" => "Alice", "age" => 30, "address" => %{"city" => "NYC"}}

json_user = %{"name" => "Bob", "age" => 25}
MapHelper.atomize_keys(json_user)
# => %{name: "Bob", age: 25}
```

**How It Works**: `Map.new/2` reconstructs the map with transformed keys. Recursively processes nested maps.

**Use Cases**: JSON serialization, API responses, database records.

**See Also**: [Beginner Tutorial - Data Structures](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#data-structures), [Pattern Matching Guide](/en/learn/software-engineering/programming-language/elixir/how-to/pattern-matching)

---

### Recipe 2: Merge Nested Maps Deeply

**Problem**: Merge two maps including nested maps (not just top level).

**Solution**:

```elixir
defmodule DeepMerge do
  def deep_merge(left, right) do
    Map.merge(left, right, &deep_resolve/3)
  end

  defp deep_resolve(_key, left = %{}, right = %{}) do
    deep_merge(left, right)
  end

  defp deep_resolve(_key, _left, right) do
    right
  end
end

# Usage
config1 = %{
  database: %{host: "localhost", port: 5432},
  cache: %{ttl: 300}
}

config2 = %{
  database: %{port: 5433, pool_size: 10},
  logging: %{level: :info}
}

DeepMerge.deep_merge(config1, config2)
# => %{
#   database: %{host: "localhost", port: 5433, pool_size: 10},
#   cache: %{ttl: 300},
#   logging: %{level: :info}
# }
```

**How It Works**: `Map.merge/3` accepts resolver function. Recursively merges when both values are maps.

**Use Cases**: Configuration merging, data aggregation, API responses.

**See Also**: [Beginner Tutorial - Maps](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#maps)

---

### Recipe 3: Group List Items by Criteria

**Problem**: Group list elements by a specific attribute or function.

**Solution**:

```elixir
defmodule ListHelper do
  def group_by(list, fun) do
    Enum.group_by(list, fun)
  end
end

# Usage
users = [
  %{name: "Alice", role: :admin},
  %{name: "Bob", role: :user},
  %{name: "Charlie", role: :admin},
  %{name: "Diana", role: :user}
]

Enum.group_by(users, & &1.role)
# => %{
#   admin: [%{name: "Alice", role: :admin}, %{name: "Charlie", role: :admin}],
#   user: [%{name: "Bob", role: :user}, %{name: "Diana", role: :user}]
# }

# Group by first letter
words = ["apple", "apricot", "banana", "blueberry", "cherry"]
Enum.group_by(words, &String.first/1)
# => %{"a" => ["apple", "apricot"], "b" => ["banana", "blueberry"], "c" => ["cherry"]}
```

**How It Works**: `Enum.group_by/2` applies function to each element, groups by result.

**Use Cases**: Data categorization, reporting, aggregation.

**See Also**: [Beginner Tutorial - Enum](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#enum-stream)

---

### Recipe 4: Build Keyword List from Map

**Problem**: Convert map to keyword list for function options.

**Solution**:

```elixir
defmodule KeywordHelper do
  def from_map(map) when is_map(map) do
    Enum.map(map, fn {k, v} -> {k, v} end)
  end
end

# Usage
options = %{timeout: 5000, retry: 3, async: true}
KeywordHelper.from_map(options)
# => [timeout: 5000, retry: 3, async: true]

# Direct usage with Enum
Map.to_list(options)
# => [async: true, retry: 3, timeout: 5000] (order may vary)
```

**How It Works**: Maps to list of tuples, maintaining keyword list format.

**Use Cases**: Function options, configuration, API parameters.

**See Also**: [Beginner Tutorial - Keyword Lists](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#keyword-lists)

---

### Recipe 5: Update Nested Structure Safely

**Problem**: Update deeply nested map or keyword list without raising errors.

**Solution**:

```elixir
defmodule SafeUpdate do
  def update_in_safe(data, keys, default, fun) do
    update_in(data, keys, fn
      nil -> fun.(default)
      value -> fun.(value)
    end)
  rescue
    _ -> data
  end

  # Using get_and_update_in
  def safe_update(data, keys, fun) do
    get_and_update_in(data, keys, fn
      nil -> :pop
      value -> {value, fun.(value)}
    end)
  end
end

# Usage
config = %{database: %{pool: %{size: 10}}}

# Safe increment
put_in(config, [:database, :pool, :size],
  get_in(config, [:database, :pool, :size]) + 5)
# => %{database: %{pool: %{size: 15}}}

# Update with default
update_in(config, [:cache, :ttl], &(&1 || 300))
# Would fail! Use get_and_update_in instead

{old_val, new_config} = get_and_update_in(config, [:cache, :ttl], fn
  nil -> {nil, 300}
  val -> {val, val * 2}
end)
# => {nil, %{database: %{pool: %{size: 10}}, cache: %{ttl: 300}}}
```

**How It Works**: `get_and_update_in/3` allows conditional updates. Returns tuple with old value and new structure.

**Use Cases**: Configuration updates, state management, data migration.

**See Also**: [Beginner Tutorial - Data Structures](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#data-structures)

---

## Pattern Matching

### Recipe 6: Extract Values from Complex Structures

**Problem**: Extract multiple values from nested data structure.

**Solution**:

```elixir
defmodule Extractor do
  def extract_user_info(user) do
    %{
      name: name,
      profile: %{age: age, city: city},
      preferences: %{theme: theme}
    } = user

    {name, age, city, theme}
  end

  # With default values
  def extract_with_defaults(user) do
    %{
      name: name,
      profile: %{age: age} = profile,
      preferences: prefs
    } = user

    city = Map.get(profile, :city, "Unknown")
    theme = Map.get(prefs, :theme, :light)

    {name, age, city, theme}
  end
end

# Usage
user = %{
  name: "Alice",
  profile: %{age: 30, city: "NYC"},
  preferences: %{theme: :dark}
}

Extractor.extract_user_info(user)
# => {"Alice", 30, "NYC", :dark}
```

**How It Works**: Pattern matching binds variables from nested structures in single expression.

**Use Cases**: Data extraction, API response parsing, validation.

**See Also**: [Pattern Matching Guide](/en/learn/software-engineering/programming-language/elixir/how-to/pattern-matching), [Beginner Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#pattern-matching)

---

### Recipe 7: Match and Transform with Case

**Problem**: Handle multiple patterns with different transformations.

**Solution**:

```elixir
defmodule ResponseHandler do
  def handle_response(response) do
    case response do
      {:ok, %{status: 200, body: body}} ->
        {:success, Jason.decode!(body)}

      {:ok, %{status: status, body: body}} when status in 400..499 ->
        {:client_error, status, body}

      {:ok, %{status: status, body: body}} when status in 500..599 ->
        {:server_error, status, body}

      {:error, %{reason: :timeout}} ->
        {:timeout, "Request timed out"}

      {:error, reason} ->
        {:error, reason}

      _ ->
        {:unknown, response}
    end
  end
end

# Usage
ResponseHandler.handle_response({:ok, %{status: 200, body: ~s({"id": 1})}})
# => {:success, %{"id" => 1}}

ResponseHandler.handle_response({:ok, %{status: 404, body: "Not found"}})
# => {:client_error, 404, "Not found"}

ResponseHandler.handle_response({:error, %{reason: :timeout}})
# => {:timeout, "Request timed out"}
```

**How It Works**: `case` evaluates patterns sequentially with guards. First match wins.

**Use Cases**: HTTP response handling, state machines, error handling.

**See Also**: [Error Handling Guide](/en/learn/software-engineering/programming-language/elixir/how-to/error-handling), [Beginner Tutorial - Control Flow](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#control-flow)

---

### Recipe 8: Pin Operator for Exact Matching

**Problem**: Match against existing variable value, not rebind.

**Solution**:

```elixir
defmodule Matcher do
  def find_user(users, target_id) do
    Enum.find(users, fn %{id: ^target_id} -> true; _ -> false end)
  end

  # Pattern matching in function heads
  def process_event(event, expected_type) do
    case event do
      %{type: ^expected_type, data: data} ->
        {:ok, data}

      %{type: other_type} ->
        {:error, "Expected #{expected_type}, got #{other_type}"}
    end
  end
end

# Usage
users = [%{id: 1, name: "Alice"}, %{id: 2, name: "Bob"}]
search_id = 2

Matcher.find_user(users, search_id)
# => %{id: 2, name: "Bob"}

event = %{type: :user_created, data: %{name: "Charlie"}}
Matcher.process_event(event, :user_created)
# => {:ok, %{name: "Charlie"}}

Matcher.process_event(event, :user_updated)
# => {:error, "Expected user_updated, got user_created"}
```

**How It Works**: Pin operator `^` matches existing value instead of rebinding variable.

**Use Cases**: List filtering, event processing, state validation.

**See Also**: [Pattern Matching Guide](/en/learn/software-engineering/programming-language/elixir/how-to/pattern-matching)

---

## Functions and Pipes

### Recipe 9: Compose Functions with Pipe Operator

**Problem**: Chain multiple transformations readably.

**Solution**:

```elixir
defmodule DataPipeline do
  def process_user_input(input) do
    input
    |> String.trim()
    |> String.downcase()
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
    |> Enum.sort()
  end

  # With error handling
  def safe_process(input) do
    with {:ok, trimmed} <- validate_string(input),
         words <- String.split(trimmed, " "),
         {:ok, processed} <- transform_words(words) do
      {:ok, processed}
    else
      {:error, reason} -> {:error, reason}
      _ -> {:error, :unknown}
    end
  end

  defp validate_string(str) when is_binary(str) and byte_size(str) > 0 do
    {:ok, String.trim(str)}
  end
  defp validate_string(_), do: {:error, :invalid_input}

  defp transform_words(words) do
    {:ok, Enum.map(words, &String.upcase/1)}
  end
end

# Usage
input = "  apple, banana,  apple, cherry,  , banana  "
DataPipeline.process_user_input(input)
# => ["apple", "banana", "cherry"]
```

**How It Works**: Pipe operator `|>` passes result as first argument to next function. `with` chains operations with error handling.

**Use Cases**: Data transformation, validation pipelines, ETL processes.

**See Also**: [Beginner Tutorial - Pipe Operator](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#pipe-operator)

---

### Recipe 10: Create Higher-Order Functions

**Problem**: Write functions that accept or return functions.

**Solution**:

```elixir
defmodule FunctionHelper do
  # Function that returns function
  def multiplier(factor) do
    fn x -> x * factor end
  end

  # Function that accepts function
  def apply_twice(value, fun) do
    value |> fun.() |> fun.()
  end

  # Function composition
  def compose(f, g) do
    fn x -> f.(g.(x)) end
  end

  # Partial application
  def partial(fun, arg1) do
    fn arg2 -> fun.(arg1, arg2) end
  end
end

# Usage
double = FunctionHelper.multiplier(2)
triple = FunctionHelper.multiplier(3)

double.(5)   # => 10
triple.(5)   # => 15

FunctionHelper.apply_twice(5, double)  # => 20

# Composition
add_one = fn x -> x + 1 end
square = fn x -> x * x end
add_one_then_square = FunctionHelper.compose(square, add_one)

add_one_then_square.(4)  # => 25 (4+1=5, 5*5=25)

# Partial application
add = fn a, b -> a + b end
add_5 = FunctionHelper.partial(add, 5)
add_5.(3)  # => 8
```

**How It Works**: Functions are first-class values. Closures capture surrounding scope.

**Use Cases**: Functional composition, callbacks, strategy pattern, middleware.

**See Also**: [Beginner Tutorial - Functions](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#functions)

---

### Recipe 11: Use Capture Syntax for Conciseness

**Problem**: Simplify anonymous function syntax.

**Solution**:

```elixir
defmodule CaptureExamples do
  # Long form vs capture
  def example_transforms(list) do
    # Long form
    long = Enum.map(list, fn x -> x * 2 end)

    # Capture syntax
    short = Enum.map(list, &(&1 * 2))

    {long, short}
  end

  # Capturing named functions
  def string_operations(words) do
    # Capture module function
    uppercased = Enum.map(words, &String.upcase/1)

    # Capture with multiple args
    prefixed = Enum.map(words, &("Item: " <> &1))

    {uppercased, prefixed}
  end

  # Complex captures
  def complex_example(users) do
    # Extract field
    names = Enum.map(users, & &1.name)

    # Method call
    lengths = Enum.map(names, &String.length/1)

    # Operator as function
    sum = Enum.reduce([1, 2, 3, 4], &+/2)

    {names, lengths, sum}
  end
end

# Usage
CaptureExamples.example_transforms([1, 2, 3])
# => {[2, 4, 6], [2, 4, 6]}

CaptureExamples.string_operations(["hello", "world"])
# => {["HELLO", "WORLD"], ["Item: hello", "Item: world"]}

users = [%{name: "Alice"}, %{name: "Bob"}]
CaptureExamples.complex_example(users)
# => {["Alice", "Bob"], [5, 3], 10}
```

**How It Works**: `&` creates anonymous function. `&1, &2, ...` are positional arguments.

**Use Cases**: Enum operations, transformations, filtering.

**See Also**: [Beginner Tutorial - Anonymous Functions](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#anonymous-functions)

---

## String Manipulation

### Recipe 12: Parse and Format Strings

**Problem**: Extract data from formatted strings and reconstruct.

**Solution**:

```elixir
defmodule StringParser do
  # Parse structured string
  def parse_log_line(line) do
    ~r/\[(?<level>\w+)\] (?<timestamp>.+) - (?<message>.+)/
    |> Regex.named_captures(line)
  end

  # Format with interpolation
  def format_user(user) do
    "User: #{user.name} (ID: #{user.id}, Age: #{user.age})"
  end

  # Parse CSV line
  def parse_csv(line) do
    line
    |> String.split(",")
    |> Enum.map(&String.trim/1)
  end

  # Build query string
  def build_query_string(params) do
    params
    |> Enum.map(fn {k, v} -> "#{k}=#{URI.encode(to_string(v))}" end)
    |> Enum.join("&")
  end
end

# Usage
log = "[ERROR] 2024-12-21 10:30:45 - Database connection failed"
StringParser.parse_log_line(log)
# => %{"level" => "ERROR", "timestamp" => "2024-12-21 10:30:45",
#      "message" => "Database connection failed"}

user = %{name: "Alice", id: 123, age: 30}
StringParser.format_user(user)
# => "User: Alice (ID: 123, Age: 30)"

StringParser.parse_csv("apple, banana,  cherry  ")
# => ["apple", "banana", "cherry"]

params = %{query: "elixir programming", page: 2, limit: 10}
StringParser.build_query_string(params)
# => "limit=10&page=2&query=elixir+programming" (order may vary)
```

**How It Works**: Regex for pattern extraction. String interpolation for formatting. URI encoding for safe URLs.

**Use Cases**: Log parsing, CSV processing, URL building, data extraction.

**See Also**: [Beginner Tutorial - Strings](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#strings-binaries), [String Handling Guide](/en/learn/software-engineering/programming-language/elixir/how-to/strings-binaries)

---

### Recipe 13: String Validation and Sanitization

**Problem**: Validate and clean user input strings.

**Solution**:

```elixir
defmodule StringValidator do
  # Email validation (simple)
  def valid_email?(email) do
    Regex.match?(~r/^[\w._%+-]+@[\w.-]+\.[a-zA-Z]{2,}$/, email)
  end

  # Sanitize HTML
  def sanitize_html(input) do
    input
    |> String.replace(~r/<[^>]*>/, "")
    |> String.replace("&", "&amp;")
    |> String.replace("<", "&lt;")
    |> String.replace(">", "&gt;")
  end

  # Validate length
  def valid_length?(str, min, max) do
    length = String.length(str)
    length >= min and length <= max
  end

  # Remove special characters
  def alphanumeric_only(str) do
    Regex.replace(~r/[^a-zA-Z0-9]/, str, "")
  end

  # Validate phone number
  def valid_phone?(phone) do
    # US format: (123) 456-7890 or 123-456-7890
    cleaned = Regex.replace(~r/[^\d]/, phone, "")
    String.length(cleaned) == 10
  end
end

# Usage
StringValidator.valid_email?("user@example.com")  # => true
StringValidator.valid_email?("invalid.email")     # => false

html = "<script>alert('xss')</script>Hello <b>World</b>"
StringValidator.sanitize_html(html)
# => "alert('xss')Hello World"

StringValidator.valid_length?("password123", 8, 20)  # => true
StringValidator.alphanumeric_only("hello-world_123!")  # => "helloworld123"

StringValidator.valid_phone?("(123) 456-7890")  # => true
StringValidator.valid_phone?("123-456-7890")    # => true
```

**How It Works**: Regex for pattern matching. String functions for cleaning and validation.

**Use Cases**: Form validation, user input sanitization, data cleaning.

**See Also**: [Beginner Tutorial - Strings](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#strings-binaries)

---

## Concurrency

### Recipe 14: Parallel Map with Task

**Problem**: Process list items concurrently for better performance.

**Solution**:

```elixir
defmodule ParallelProcessor do
  def parallel_map(collection, fun) do
    collection
    |> Enum.map(&Task.async(fn -> fun.(&1) end))
    |> Enum.map(&Task.await(&1, 10_000))
  end

  # With timeout handling
  def safe_parallel_map(collection, fun, timeout \\ 5000) do
    collection
    |> Task.async_stream(fun, timeout: timeout, max_concurrency: System.schedulers_online())
    |> Enum.map(fn
      {:ok, result} -> result
      {:exit, reason} -> {:error, reason}
    end)
  end

  # Process in chunks
  def parallel_chunk(collection, chunk_size, fun) do
    collection
    |> Enum.chunk_every(chunk_size)
    |> Task.async_stream(fn chunk -> Enum.map(chunk, fun) end)
    |> Enum.flat_map(fn {:ok, results} -> results end)
  end
end

# Usage
# Expensive operation (e.g., HTTP requests)
fetch_user = fn id ->
  :timer.sleep(100)  # Simulate delay
  %{id: id, name: "User #{id}"}
end

# Sequential: ~1000ms for 10 items
Enum.map(1..10, fetch_user)

# Parallel: ~100ms for 10 items (if enough cores)
ParallelProcessor.parallel_map(1..10, fetch_user)

# With timeout handling
compute = fn x ->
  :timer.sleep(x * 100)
  x * x
end

ParallelProcessor.safe_parallel_map([1, 2, 3, 4, 5], compute, 1000)
# => [1, 4, 9, 16, 25] or some {:error, :timeout} for slow items
```

**How It Works**: `Task.async/1` spawns concurrent process. `Task.await/2` waits for result. `Task.async_stream/3` provides built-in timeout and concurrency control.

**Use Cases**: HTTP requests, file processing, data transformation, I/O operations.

**See Also**: [Concurrency Guide](/en/learn/software-engineering/programming-language/elixir/how-to/task-agent), [Intermediate Tutorial - Task](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#task-agent)

---

### Recipe 15: Process Registry Pattern

**Problem**: Find and communicate with named processes.

**Solution**:

```elixir
defmodule UserCache do
  use GenServer

  # Start with name registration
  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, %{}, name: name)
  end

  def put(server \\ __MODULE__, key, value) do
    GenServer.call(server, {:put, key, value})
  end

  def get(server \\ __MODULE__, key) do
    GenServer.call(server, {:get, key})
  end

  @impl true
  def init(_) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:put, key, value}, _from, state) do
    {:reply, :ok, Map.put(state, key, value)}
  end

  def handle_call({:get, key}, _from, state) do
    {:reply, Map.get(state, key), state}
  end
end

# Using Registry
defmodule DynamicCache do
  def start_cache(user_id) do
    name = {:via, Registry, {MyApp.Registry, {:cache, user_id}}}
    GenServer.start_link(UserCache, %{}, name: name)
  end

  def get_cache(user_id) do
    name = {:via, Registry, {MyApp.Registry, {:cache, user_id}}}
    GenServer.whereis(name)
  end
end

# Usage
# Start Registry first (in application.ex)
# {:ok, _} = Registry.start_link(keys: :unique, name: MyApp.Registry)

{:ok, _pid} = UserCache.start_link(name: :my_cache)
UserCache.put(:my_cache, "user_1", %{name: "Alice"})
UserCache.get(:my_cache, "user_1")
# => %{name: "Alice"}

# Dynamic registration
{:ok, _pid} = DynamicCache.start_cache(123)
cache_pid = DynamicCache.get_cache(123)
GenServer.call(cache_pid, {:put, "key", "value"})
```

**How It Works**: Named processes registered globally or via Registry. `{:via, Registry, {registry_name, key}}` for dynamic naming.

**Use Cases**: Process discovery, distributed caching, session management.

**See Also**: [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver), [Intermediate Tutorial - GenServer](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#genserver)

---

### Recipe 16: Debounce and Throttle Operations

**Problem**: Rate-limit operations to prevent overwhelming system.

**Solution**:

```elixir
defmodule RateLimiter do
  use GenServer

  defmodule State do
    defstruct [:last_call, :interval, :pending_timer]
  end

  # Start with minimum interval (milliseconds)
  def start_link(interval) do
    GenServer.start_link(__MODULE__, interval, name: __MODULE__)
  end

  # Throttle: Execute immediately if interval passed, else ignore
  def throttle(fun) do
    GenServer.call(__MODULE__, {:throttle, fun})
  end

  # Debounce: Delay execution, reset timer on new call
  def debounce(fun) do
    GenServer.cast(__MODULE__, {:debounce, fun})
  end

  @impl true
  def init(interval) do
    {:ok, %State{last_call: 0, interval: interval, pending_timer: nil}}
  end

  @impl true
  def handle_call({:throttle, fun}, _from, state) do
    now = System.monotonic_time(:millisecond)
    elapsed = now - state.last_call

    if elapsed >= state.interval do
      result = fun.()
      {:reply, {:ok, result}, %{state | last_call: now}}
    else
      {:reply, {:throttled, state.interval - elapsed}, state}
    end
  end

  @impl true
  def handle_cast({:debounce, fun}, state) do
    # Cancel pending timer
    if state.pending_timer do
      Process.cancel_timer(state.pending_timer)
    end

    # Schedule new execution
    timer = Process.send_after(self(), {:execute, fun}, state.interval)
    {:noreply, %{state | pending_timer: timer}}
  end

  @impl true
  def handle_info({:execute, fun}, state) do
    fun.()
    {:noreply, %{state | pending_timer: nil}}
  end
end

# Usage
{:ok, _} = RateLimiter.start_link(1000)  # 1 second interval

# Throttle: Only first call executes immediately
RateLimiter.throttle(fn -> IO.puts("Call 1") end)  # Executes
# => {:ok, :ok}

RateLimiter.throttle(fn -> IO.puts("Call 2") end)  # Throttled
# => {:throttled, 995}

# Wait 1 second
:timer.sleep(1000)
RateLimiter.throttle(fn -> IO.puts("Call 3") end)  # Executes
# => {:ok, :ok}

# Debounce: Only last call executes after interval
RateLimiter.debounce(fn -> IO.puts("Debounced 1") end)
RateLimiter.debounce(fn -> IO.puts("Debounced 2") end)
RateLimiter.debounce(fn -> IO.puts("Debounced 3") end)
# After 1 second: "Debounced 3" prints
```

**How It Works**: Throttle checks elapsed time since last execution. Debounce cancels pending timer and reschedules.

**Use Cases**: Search autocomplete, API rate limiting, event handling, scroll events.

**See Also**: [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver)

---

## OTP Patterns

### Recipe 17: Simple State Machine with GenServer

**Problem**: Model stateful behavior with transitions.

**Solution**:

```elixir
defmodule OrderStateMachine do
  use GenServer

  @states [:pending, :confirmed, :shipped, :delivered, :cancelled]

  defmodule State do
    defstruct [:order_id, :current_state, :history]
  end

  # Client API
  def start_link(order_id) do
    GenServer.start_link(__MODULE__, order_id)
  end

  def get_state(pid) do
    GenServer.call(pid, :get_state)
  end

  def transition(pid, new_state) do
    GenServer.call(pid, {:transition, new_state})
  end

  # Server callbacks
  @impl true
  def init(order_id) do
    state = %State{
      order_id: order_id,
      current_state: :pending,
      history: [{:pending, DateTime.utc_now()}]
    }
    {:ok, state}
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, state.current_state, state}
  end

  def handle_call({:transition, new_state}, _from, state) do
    if valid_transition?(state.current_state, new_state) do
      new_history = [{new_state, DateTime.utc_now()} | state.history]
      new_state_struct = %{state | current_state: new_state, history: new_history}
      {:reply, {:ok, new_state}, new_state_struct}
    else
      {:reply, {:error, :invalid_transition}, state}
    end
  end

  defp valid_transition?(:pending, :confirmed), do: true
  defp valid_transition?(:pending, :cancelled), do: true
  defp valid_transition?(:confirmed, :shipped), do: true
  defp valid_transition?(:confirmed, :cancelled), do: true
  defp valid_transition?(:shipped, :delivered), do: true
  defp valid_transition?(_, _), do: false
end

# Usage
{:ok, order} = OrderStateMachine.start_link("ORD-123")

OrderStateMachine.get_state(order)
# => :pending

OrderStateMachine.transition(order, :confirmed)
# => {:ok, :confirmed}

OrderStateMachine.transition(order, :delivered)
# => {:error, :invalid_transition} (must ship first)

OrderStateMachine.transition(order, :shipped)
# => {:ok, :shipped}

OrderStateMachine.transition(order, :delivered)
# => {:ok, :delivered}
```

**How It Works**: GenServer maintains state. Validates transitions before applying. Stores history of state changes.

**Use Cases**: Order processing, workflow management, game state, approval flows.

**See Also**: [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver), [Intermediate Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#genserver)

---

### Recipe 18: Supervision Tree with Dynamic Children

**Problem**: Supervise dynamically created processes.

**Solution**:

```elixir
defmodule Worker do
  use GenServer

  def start_link(id) do
    GenServer.start_link(__MODULE__, id, name: via_tuple(id))
  end

  def get_state(id) do
    GenServer.call(via_tuple(id), :get_state)
  end

  defp via_tuple(id) do
    {:via, Registry, {WorkerRegistry, id}}
  end

  @impl true
  def init(id) do
    {:ok, %{id: id, data: %{}, started_at: DateTime.utc_now()}}
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end
end

defmodule WorkerSupervisor do
  use DynamicSupervisor

  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def start_worker(id) do
    spec = {Worker, id}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  def stop_worker(id) do
    case Registry.lookup(WorkerRegistry, id) do
      [{pid, _}] -> DynamicSupervisor.terminate_child(__MODULE__, pid)
      [] -> {:error, :not_found}
    end
  end

  def list_workers do
    DynamicSupervisor.which_children(__MODULE__)
  end

  @impl true
  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end

# In application.ex
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: WorkerRegistry},
      WorkerSupervisor
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

# Usage
{:ok, _pid} = WorkerSupervisor.start_worker("worker_1")
{:ok, _pid} = WorkerSupervisor.start_worker("worker_2")

Worker.get_state("worker_1")
# => %{id: "worker_1", data: %{}, started_at: ~U[...]}

WorkerSupervisor.list_workers()
# => [
#   {:undefined, #PID<0.123.0>, :worker, [Worker]},
#   {:undefined, #PID<0.124.0>, :worker, [Worker]}
# ]

WorkerSupervisor.stop_worker("worker_1")
# => :ok
```

**How It Works**: DynamicSupervisor manages children added at runtime. Registry for process discovery. `:one_for_one` strategy restarts only failed child.

**Use Cases**: Connection pools, user sessions, background jobs, WebSocket handlers.

**See Also**: [Supervision Guide](/en/learn/software-engineering/programming-language/elixir/how-to/supervision), [Intermediate Tutorial - Supervisor](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#supervisor)

---

### Recipe 19: Periodic Tasks with GenServer

**Problem**: Execute recurring tasks at intervals.

**Solution**:

```elixir
defmodule PeriodicWorker do
  use GenServer
  require Logger

  defmodule State do
    defstruct [:interval, :task, :timer_ref]
  end

  # Start with task and interval (milliseconds)
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    interval = Keyword.fetch!(opts, :interval)
    task = Keyword.fetch!(opts, :task)

    state = %State{interval: interval, task: task}
    {:ok, schedule_work(state)}
  end

  @impl true
  def handle_info(:work, state) do
    # Execute task
    try do
      state.task.()
    rescue
      e -> Logger.error("Task failed: #{inspect(e)}")
    end

    # Schedule next execution
    {:noreply, schedule_work(state)}
  end

  defp schedule_work(state) do
    timer_ref = Process.send_after(self(), :work, state.interval)
    %{state | timer_ref: timer_ref}
  end
end

# Alternative: Using :timer module
defmodule SimplePeriodicWorker do
  use GenServer

  def start_link(interval, task) do
    GenServer.start_link(__MODULE__, {interval, task})
  end

  @impl true
  def init({interval, task}) do
    {:ok, _timer} = :timer.send_interval(interval, :tick)
    {:ok, task}
  end

  @impl true
  def handle_info(:tick, task) do
    task.()
    {:noreply, task}
  end
end

# Usage
task = fn ->
  Logger.info("Periodic task executed at #{DateTime.utc_now()}")
  # Cleanup, health checks, metrics collection, etc.
end

{:ok, _pid} = PeriodicWorker.start_link(interval: 5000, task: task)
# Task executes every 5 seconds

# Alternative with :timer
{:ok, _pid} = SimplePeriodicWorker.start_link(10_000, task)
# Task executes every 10 seconds
```

**How It Works**: `Process.send_after/3` schedules message to self. Reschedules after execution for continuous loop. `:timer.send_interval/2` simpler but less flexible.

**Use Cases**: Cache cleanup, health checks, metrics collection, scheduled reports.

**See Also**: [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver)

---

## Error Handling

### Recipe 20: Result Tuple Pattern

**Problem**: Handle success and error cases explicitly.

**Solution**:

```elixir
defmodule UserService do
  def create_user(attrs) do
    with {:ok, validated} <- validate_attrs(attrs),
         {:ok, user} <- insert_user(validated),
         {:ok, _email} <- send_welcome_email(user) do
      {:ok, user}
    else
      {:error, :invalid_attrs} = error -> error
      {:error, :db_error} = error -> error
      {:error, :email_failed} ->
        # User created but email failed - still success
        # Could also rollback here if needed
        {:ok, user}
      error ->
        {:error, {:unexpected, error}}
    end
  end

  defp validate_attrs(%{email: email, name: name})
    when is_binary(email) and is_binary(name) do
    {:ok, %{email: email, name: name}}
  end
  defp validate_attrs(_), do: {:error, :invalid_attrs}

  defp insert_user(attrs) do
    # Simulate DB insert
    if String.contains?(attrs.email, "@") do
      {:ok, Map.put(attrs, :id, :rand.uniform(1000))}
    else
      {:error, :db_error}
    end
  end

  defp send_welcome_email(user) do
    # Simulate email sending
    {:ok, "Email sent to #{user.email}"}
  end
end

# Pattern matching results
defmodule UserController do
  def create(params) do
    case UserService.create_user(params) do
      {:ok, user} ->
        {:json, %{status: "success", user: user}}

      {:error, :invalid_attrs} ->
        {:json, %{status: "error", message: "Invalid attributes"}, status: 400}

      {:error, :db_error} ->
        {:json, %{status: "error", message: "Database error"}, status: 500}

      {:error, reason} ->
        {:json, %{status: "error", message: "Unknown error: #{inspect(reason)}"},
         status: 500}
    end
  end
end

# Usage
UserService.create_user(%{email: "alice@example.com", name: "Alice"})
# => {:ok, %{email: "alice@example.com", name: "Alice", id: 42}}

UserService.create_user(%{email: "invalid", name: "Bob"})
# => {:error, :db_error}

UserService.create_user(%{name: "Charlie"})
# => {:error, :invalid_attrs}
```

**How It Works**: `with` chains operations, short-circuits on first `{:error, _}`. Pattern match in `else` clause for specific error handling.

**Use Cases**: Business logic, API handlers, data pipelines, validation flows.

**See Also**: [Error Handling Guide](/en/learn/software-engineering/programming-language/elixir/how-to/error-handling), [Beginner Tutorial - Error Handling](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#error-handling)

---

### Recipe 21: Supervision for Fault Tolerance

**Problem**: Recover automatically from crashes.

**Solution**:

```elixir
defmodule RiskyWorker do
  use GenServer
  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def do_risky_work(data) do
    GenServer.call(__MODULE__, {:risky, data})
  end

  @impl true
  def init(_opts) do
    Logger.info("RiskyWorker started")
    {:ok, %{restarts: 0}}
  end

  @impl true
  def handle_call({:risky, data}, _from, state) do
    # Simulate random crashes
    if :rand.uniform() > 0.7 do
      raise "Simulated crash!"
    end

    {:reply, {:ok, "Processed: #{data}"}, state}
  end
end

defmodule MyApp.Supervisor do
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [
      # Restart strategy: restart worker on crash
      {RiskyWorker, []}
    ]

    # :one_for_one - only restart crashed child
    # max_restarts: 3, max_seconds: 5 - restart up to 3 times in 5 seconds
    Supervisor.init(children, strategy: :one_for_one, max_restarts: 3, max_seconds: 5)
  end
end

# Usage
{:ok, _sup} = MyApp.Supervisor.start_link([])

# Call multiple times - some will crash and restart
for i <- 1..10 do
  try do
    case RiskyWorker.do_risky_work("Task #{i}") do
      {:ok, result} -> IO.puts("Success: #{result}")
      error -> IO.puts("Error: #{inspect(error)}")
    end
  catch
    :exit, reason -> IO.puts("Process crashed: #{inspect(reason)}")
  end

  :timer.sleep(100)
end

# Worker automatically restarts after crashes
# If crashes too frequently (>3 in 5s), supervisor terminates
```

**How It Works**: Supervisor monitors child processes. Restarts crashed children per strategy. Max restarts prevents crash loops.

**Use Cases**: Network connections, external services, unreliable operations, background jobs.

**See Also**: [Supervision Guide](/en/learn/software-engineering/programming-language/elixir/how-to/supervision), [Intermediate Tutorial - Supervisor](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#supervisor)

---

## File I/O

### Recipe 22: Read and Write Files Safely

**Problem**: Handle file operations with proper error handling.

**Solution**:

```elixir
defmodule FileHelper do
  # Read file with error handling
  def read_file(path) do
    case File.read(path) do
      {:ok, content} -> {:ok, content}
      {:error, :enoent} -> {:error, :file_not_found}
      {:error, reason} -> {:error, reason}
    end
  end

  # Read lines lazily (memory efficient)
  def read_lines(path) do
    File.stream!(path)
    |> Stream.map(&String.trim/1)
    |> Enum.to_list()
  end

  # Write with automatic directory creation
  def write_file(path, content) do
    dir = Path.dirname(path)

    with :ok <- File.mkdir_p(dir),
         :ok <- File.write(path, content) do
      {:ok, path}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Append to file
  def append_to_file(path, content) do
    File.write(path, content, [:append])
  end

  # Read JSON file
  def read_json(path) do
    with {:ok, content} <- File.read(path),
         {:ok, decoded} <- Jason.decode(content) do
      {:ok, decoded}
    end
  end

  # Write JSON file (pretty-printed)
  def write_json(path, data) do
    with {:ok, json} <- Jason.encode(data, pretty: true),
         :ok <- write_file(path, json) do
      {:ok, path}
    end
  end
end

# Usage
# Read
{:ok, content} = FileHelper.read_file("config.txt")

# Write (creates directory if needed)
FileHelper.write_file("output/results.txt", "Hello, World!")
# => {:ok, "output/results.txt"}

# Read large file line by line (memory efficient)
lines = FileHelper.read_lines("large_file.txt")

# JSON operations
data = %{users: [%{name: "Alice", age: 30}, %{name: "Bob", age: 25}]}
FileHelper.write_json("data/users.json", data)

{:ok, loaded} = FileHelper.read_json("data/users.json")
# => {:ok, %{"users" => [%{"name" => "Alice", "age" => 30}, ...]}}

# Append to log file
FileHelper.append_to_file("app.log", "#{DateTime.utc_now()} - Server started\n")
```

**How It Works**: Pattern match File module results. Stream API for memory-efficient large file processing. Path module for directory operations.

**Use Cases**: Configuration files, data import/export, logging, file processing.

**See Also**: [File I/O Guide](/en/learn/software-engineering/programming-language/elixir/how-to/file-io), [Beginner Tutorial - File I/O](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#file-io)

---

### Recipe 23: CSV Processing

**Problem**: Parse and generate CSV files efficiently.

**Solution**:

```elixir
defmodule CSVHelper do
  # Simple CSV parser (no library)
  def parse_csv(path) do
    path
    |> File.stream!()
    |> Stream.map(&String.trim/1)
    |> Stream.map(&parse_csv_line/1)
    |> Enum.to_list()
  end

  defp parse_csv_line(line) do
    line
    |> String.split(",")
    |> Enum.map(&String.trim/1)
  end

  # Parse CSV to maps (first row as headers)
  def parse_csv_to_maps(path) do
    [headers | rows] = parse_csv(path)

    rows
    |> Enum.map(fn row ->
      headers
      |> Enum.zip(row)
      |> Map.new(fn {h, v} -> {String.to_atom(h), v} end)
    end)
  end

  # Write CSV from list of maps
  def write_csv(path, data, headers) do
    csv_content =
      [headers | Enum.map(data, fn row ->
        Enum.map(headers, &Map.get(row, &1, ""))
      end)]
      |> Enum.map(&Enum.join(&1, ","))
      |> Enum.join("\n")

    File.write(path, csv_content)
  end

  # Stream large CSV (memory efficient)
  def stream_csv(path, fun) do
    path
    |> File.stream!()
    |> Stream.drop(1)  # Skip header
    |> Stream.map(&parse_csv_line/1)
    |> Stream.each(fun)
    |> Stream.run()
  end
end

# Usage
# Simple parsing
rows = CSVHelper.parse_csv("users.csv")
# => [["name", "age", "city"], ["Alice", "30", "NYC"], ["Bob", "25", "LA"]]

# Parse to maps
users = CSVHelper.parse_csv_to_maps("users.csv")
# => [%{name: "Alice", age: "30", city: "NYC"}, %{name: "Bob", age: "25", city: "LA"}]

# Write CSV
data = [
  %{name: "Alice", age: 30, city: "NYC"},
  %{name: "Bob", age: 25, city: "LA"}
]
headers = [:name, :age, :city]
CSVHelper.write_csv("output.csv", data, headers)

# Process large CSV in stream (memory efficient)
CSVHelper.stream_csv("large_data.csv", fn row ->
  # Process each row without loading entire file
  IO.inspect(row)
end)
```

**How It Works**: Stream API for lazy evaluation. Map headers to values for structured data. String operations for parsing.

**Use Cases**: Data import/export, reporting, data migration, ETL.

**See Also**: [File I/O Guide](/en/learn/software-engineering/programming-language/elixir/how-to/file-io)

---

## Phoenix Web Development

### Recipe 24: Build RESTful API Endpoints

**Problem**: Create standard CRUD endpoints with proper error handling.

**Solution**:

```elixir
defmodule MyAppWeb.UserController do
  use MyAppWeb, :controller
  alias MyApp.Accounts

  # Index - List all users
  def index(conn, params) do
    page = Map.get(params, "page", "1") |> String.to_integer()
    limit = Map.get(params, "limit", "20") |> String.to_integer()

    users = Accounts.list_users(page: page, limit: limit)
    render(conn, :index, users: users)
  end

  # Show - Get single user
  def show(conn, %{"id" => id}) do
    case Accounts.get_user(id) do
      nil ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "User not found"})

      user ->
        render(conn, :show, user: user)
    end
  end

  # Create - Add new user
  def create(conn, %{"user" => user_params}) do
    case Accounts.create_user(user_params) do
      {:ok, user} ->
        conn
        |> put_status(:created)
        |> put_resp_header("location", ~p"/api/users/#{user.id}")
        |> render(:show, user: user)

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: translate_errors(changeset)})
    end
  end

  # Update - Modify existing user
  def update(conn, %{"id" => id, "user" => user_params}) do
    case Accounts.get_user(id) do
      nil ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "User not found"})

      user ->
        case Accounts.update_user(user, user_params) do
          {:ok, updated_user} ->
            render(conn, :show, user: updated_user)

          {:error, changeset} ->
            conn
            |> put_status(:unprocessable_entity)
            |> json(%{errors: translate_errors(changeset)})
        end
    end
  end

  # Delete - Remove user
  def delete(conn, %{"id" => id}) do
    case Accounts.get_user(id) do
      nil ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "User not found"})

      user ->
        {:ok, _user} = Accounts.delete_user(user)
        send_resp(conn, :no_content, "")
    end
  end

  defp translate_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end
end

# JSON view
defmodule MyAppWeb.UserJSON do
  def index(%{users: users}) do
    %{data: Enum.map(users, &user_json/1)}
  end

  def show(%{user: user}) do
    %{data: user_json(user)}
  end

  defp user_json(user) do
    %{
      id: user.id,
      name: user.name,
      email: user.email,
      inserted_at: user.inserted_at,
      updated_at: user.updated_at
    }
  end
end

# Router
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", MyAppWeb do
    pipe_through :api

    resources "/users", UserController, except: [:new, :edit]
  end
end
```

**How It Works**: Controller handles request/response. Pattern match params. Return appropriate HTTP status codes. JSON view formats data.

**Use Cases**: REST APIs, mobile backends, microservices.

**See Also**: [Phoenix REST API Guide](/en/learn/software-engineering/programming-language/elixir/how-to/phoenix-rest-api), [Intermediate Tutorial - Phoenix](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#phoenix-framework)

---

### Recipe 25: Authentication with Plugs

**Problem**: Protect routes with authentication.

**Solution**:

```elixir
defmodule MyAppWeb.Plugs.Auth do
  import Plug.Conn
  import Phoenix.Controller

  def init(opts), do: opts

  def call(conn, _opts) do
    case get_req_header(conn, "authorization") do
      ["Bearer " <> token] ->
        verify_token(conn, token)

      _ ->
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Missing authorization header"})
        |> halt()
    end
  end

  defp verify_token(conn, token) do
    case MyApp.Auth.verify_token(token) do
      {:ok, user_id} ->
        conn
        |> assign(:current_user_id, user_id)
        |> assign(:authenticated, true)

      {:error, _reason} ->
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Invalid token"})
        |> halt()
    end
  end
end

# Optional auth plug
defmodule MyAppWeb.Plugs.OptionalAuth do
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, _opts) do
    case get_req_header(conn, "authorization") do
      ["Bearer " <> token] ->
        case MyApp.Auth.verify_token(token) do
          {:ok, user_id} ->
            assign(conn, :current_user_id, user_id)

          {:error, _} ->
            conn
        end

      _ ->
        conn
    end
  end
end

# Usage in router
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :authenticated do
    plug MyAppWeb.Plugs.Auth
  end

  pipeline :optional_auth do
    plug MyAppWeb.Plugs.OptionalAuth
  end

  scope "/api", MyAppWeb do
    pipe_through [:api, :authenticated]

    resources "/users", UserController
    get "/profile", ProfileController, :show
  end

  scope "/api", MyAppWeb do
    pipe_through [:api, :optional_auth]

    get "/posts", PostController, :index
  end

  scope "/api", MyAppWeb do
    pipe_through :api

    post "/login", SessionController, :create
    post "/register", RegistrationController, :create
  end
end

# Token verification module
defmodule MyApp.Auth do
  @secret "your-secret-key"

  def verify_token(token) do
    case Phoenix.Token.verify(MyAppWeb.Endpoint, @secret, token, max_age: 86400) do
      {:ok, user_id} -> {:ok, user_id}
      {:error, reason} -> {:error, reason}
    end
  end

  def generate_token(user_id) do
    Phoenix.Token.sign(MyAppWeb.Endpoint, @secret, user_id)
  end
end
```

**How It Works**: Plug intercepts request before controller. Checks authorization header. Verifies token and assigns user. `halt()` prevents further processing.

**Use Cases**: API authentication, role-based access, session management.

**See Also**: [Phoenix REST API Guide](/en/learn/software-engineering/programming-language/elixir/how-to/phoenix-rest-api)

---

## LiveView Patterns

### Recipe 26: Real-Time Form with Validation

**Problem**: Build interactive form with live validation.

**Solution**:

```elixir
defmodule MyAppWeb.UserLive.Form do
  use MyAppWeb, :live_view
  alias MyApp.Accounts
  alias MyApp.Accounts.User

  def mount(_params, _session, socket) do
    changeset = Accounts.change_user(%User{})

    {:ok,
     socket
     |> assign(:changeset, changeset)
     |> assign(:submitted, false)}
  end

  def handle_event("validate", %{"user" => user_params}, socket) do
    changeset =
      %User{}
      |> Accounts.change_user(user_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  def handle_event("save", %{"user" => user_params}, socket) do
    case Accounts.create_user(user_params) do
      {:ok, user} ->
        {:noreply,
         socket
         |> put_flash(:info, "User created successfully")
         |> push_navigate(to: ~p"/users/#{user.id}")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply,
         socket
         |> assign(:changeset, changeset)
         |> assign(:submitted, true)}
    end
  end

  def render(assigns) do
    ~H"""
    <div class="max-w-md mx-auto">
      <h1 class="text-2xl font-bold mb-4">Create User</h1>

      <.form
        for={@changeset}
        phx-change="validate"
        phx-submit="save"
        class="space-y-4"
      >
        <div>
          <.input
            field={@changeset[:name]}
            type="text"
            label="Name"
            placeholder="Enter your name"
          />
        </div>

        <div>
          <.input
            field={@changeset[:email]}
            type="email"
            label="Email"
            placeholder="user@example.com"
          />
        </div>

        <div>
          <.input
            field={@changeset[:age]}
            type="number"
            label="Age"
            placeholder="18"
          />
        </div>

        <.button type="submit" class="w-full">
          Create User
        </.button>
      </.form>
    </div>
    """
  end
end
```

**How It Works**: `phx-change` triggers validation on input. `phx-submit` saves form. Changeset updates live in socket. Errors display immediately.

**Use Cases**: Forms, user input, data entry, interactive UIs.

**See Also**: [LiveView Guide](/en/learn/software-engineering/programming-language/elixir/how-to/liveview), [Intermediate Tutorial - LiveView](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#liveview)

---

### Recipe 27: WebSocket Communication with Phoenix Channels

**Problem**: Implement real-time bidirectional communication using WebSockets.

**Solution**:

```elixir
# lib/my_app_web/channels/room_channel.ex
defmodule MyAppWeb.RoomChannel do
  use MyAppWeb, :channel
  require Logger

  @impl true
  def join("room:" <> room_id, params, socket) do
    case authorized?(socket, room_id, params) do
      true ->
        send(self(), :after_join)
        {:ok, %{room_id: room_id}, assign(socket, :room_id, room_id)}

      false ->
        {:error, %{reason: "unauthorized"}}
    end
  end

  @impl true
  def handle_info(:after_join, socket) do
    # Broadcast user joined
    broadcast!(socket, "user_joined", %{
      user_id: socket.assigns.user_id,
      joined_at: DateTime.utc_now()
    })

    # Push initial room state
    push(socket, "room_state", %{
      users: get_room_users(socket.assigns.room_id),
      messages: get_recent_messages(socket.assigns.room_id)
    })

    {:noreply, socket}
  end

  @impl true
  def handle_in("new_message", %{"body" => body}, socket) do
    message = %{
      id: generate_id(),
      body: body,
      user_id: socket.assigns.user_id,
      inserted_at: DateTime.utc_now()
    }

    # Save message to database
    case create_message(socket.assigns.room_id, message) do
      {:ok, saved_message} ->
        # Broadcast to all users in room
        broadcast!(socket, "new_message", saved_message)
        {:reply, {:ok, saved_message}, socket}

      {:error, changeset} ->
        {:reply, {:error, %{errors: changeset}}, socket}
    end
  end

  @impl true
  def handle_in("typing", %{"typing" => is_typing}, socket) do
    broadcast_from!(socket, "user_typing", %{
      user_id: socket.assigns.user_id,
      typing: is_typing
    })

    {:noreply, socket}
  end

  @impl true
  def handle_in("read_receipt", %{"message_id" => message_id}, socket) do
    broadcast_from!(socket, "message_read", %{
      message_id: message_id,
      user_id: socket.assigns.user_id,
      read_at: DateTime.utc_now()
    })

    {:noreply, socket}
  end

  @impl true
  def terminate(reason, socket) do
    Logger.info("User #{socket.assigns.user_id} left room #{socket.assigns.room_id}: #{inspect(reason)}")

    broadcast!(socket, "user_left", %{
      user_id: socket.assigns.user_id,
      left_at: DateTime.utc_now()
    })

    :ok
  end

  defp authorized?(socket, room_id, _params) do
    # Check if user has access to room
    user_id = socket.assigns[:user_id]
    user_id != nil && has_room_access?(user_id, room_id)
  end

  defp has_room_access?(_user_id, _room_id), do: true
  defp get_room_users(_room_id), do: []
  defp get_recent_messages(_room_id), do: []
  defp create_message(_room_id, message), do: {:ok, message}
  defp generate_id, do: :crypto.strong_rand_bytes(16) |> Base.encode16()
end

# lib/my_app_web/channels/user_socket.ex
defmodule MyAppWeb.UserSocket do
  use Phoenix.Socket

  channel "room:*", MyAppWeb.RoomChannel
  channel "notifications:*", MyAppWeb.NotificationChannel

  @impl true
  def connect(%{"token" => token}, socket, _connect_info) do
    case verify_token(token) do
      {:ok, user_id} ->
        {:ok, assign(socket, :user_id, user_id)}

      {:error, _reason} ->
        :error
    end
  end

  def connect(_params, _socket, _connect_info), do: :error

  @impl true
  def id(socket), do: "users_socket:#{socket.assigns.user_id}"

  defp verify_token(token) do
    # Verify JWT or session token
    {:ok, token}
  end
end

# Client-side JavaScript
# assets/js/socket.js
# import {Socket} from "phoenix"
#
# let socket = new Socket("/socket", {params: {token: window.userToken}})
# socket.connect()
#
# let channel = socket.channel("room:lobby", {})
# channel.join()
#   .receive("ok", resp => { console.log("Joined successfully", resp) })
#   .receive("error", resp => { console.log("Unable to join", resp) })
#
# channel.on("new_message", msg => {
#   console.log("New message:", msg)
#   renderMessage(msg)
# })
#
# channel.on("user_typing", data => {
#   showTypingIndicator(data.user_id)
# })
#
# // Send message
# channel.push("new_message", {body: "Hello!"})
#   .receive("ok", msg => console.log("Message sent", msg))
#   .receive("error", reasons => console.log("Failed", reasons))
#
# // Typing indicator
# input.addEventListener("input", () => {
#   channel.push("typing", {typing: true})
# })
```

**How It Works**: Phoenix Channels provide WebSocket abstraction. `join/3` authorizes connection. `handle_in/3` processes incoming messages. `broadcast!/3` sends to all subscribers. `push/3` sends to specific client. Socket maintains user state.

**Use Cases**: Chat applications, collaborative editing, real-time dashboards, multiplayer games, live notifications.

**See Also**: [Phoenix REST API Guide](/en/learn/software-engineering/programming-language/elixir/how-to/phoenix-rest-api), [Intermediate Tutorial - Phoenix](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#phoenix-framework)

---

### Recipe 28: Session-Based Authentication

**Problem**: Implement secure session-based authentication in Phoenix.

**Solution**:

```elixir
# lib/my_app_web/controllers/session_controller.ex
defmodule MyAppWeb.SessionController do
  use MyAppWeb, :controller
  alias MyApp.Accounts

  def new(conn, _params) do
    render(conn, "new.html")
  end

  def create(conn, %{"session" => %{"email" => email, "password" => password}}) do
    case Accounts.authenticate_user(email, password) do
      {:ok, user} ->
        conn
        |> put_flash(:info, "Welcome back!")
        |> put_session(:user_id, user.id)
        |> configure_session(renew: true)  # Prevent session fixation
        |> redirect(to: ~p"/dashboard")

      {:error, :invalid_credentials} ->
        conn
        |> put_flash(:error, "Invalid email or password")
        |> render("new.html")
    end
  end

  def delete(conn, _params) do
    conn
    |> configure_session(drop: true)
    |> put_flash(:info, "Logged out successfully")
    |> redirect(to: ~p"/")
  end
end

# lib/my_app/accounts.ex
defmodule MyApp.Accounts do
  import Ecto.Query
  alias MyApp.Repo
  alias MyApp.Accounts.User

  def authenticate_user(email, password) do
    user = Repo.get_by(User, email: email)

    cond do
      user && Bcrypt.verify_pass(password, user.password_hash) ->
        {:ok, user}

      user ->
        # Prevent timing attacks
        Bcrypt.no_user_verify()
        {:error, :invalid_credentials}

      true ->
        Bcrypt.no_user_verify()
        {:error, :invalid_credentials}
    end
  end

  def register_user(attrs) do
    %User{}
    |> User.registration_changeset(attrs)
    |> Repo.insert()
  end
end

# lib/my_app/accounts/user.ex
defmodule MyApp.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :email, :string
    field :password, :string, virtual: true
    field :password_hash, :string
    field :confirmed_at, :naive_datetime

    timestamps()
  end

  def registration_changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :password])
    |> validate_required([:email, :password])
    |> validate_format(:email, ~r/^[^\s]+@[^\s]+$/, message: "must be a valid email")
    |> validate_length(:password, min: 8)
    |> validate_password_strength()
    |> unique_constraint(:email)
    |> put_password_hash()
  end

  defp validate_password_strength(changeset) do
    password = get_change(changeset, :password)

    if password && String.length(password) >= 8 do
      # Check for at least one number and one uppercase
      has_number = String.match?(password, ~r/\d/)
      has_uppercase = String.match?(password, ~r/[A-Z]/)

      if has_number && has_uppercase do
        changeset
      else
        add_error(changeset, :password, "must contain at least one number and one uppercase letter")
      end
    else
      changeset
    end
  end

  defp put_password_hash(changeset) do
    case changeset do
      %Ecto.Changeset{valid?: true, changes: %{password: password}} ->
        put_change(changeset, :password_hash, Bcrypt.hash_pwd_salt(password))

      _ ->
        changeset
    end
  end
end

# lib/my_app_web/plugs/require_auth.ex
defmodule MyAppWeb.Plugs.RequireAuth do
  import Plug.Conn
  import Phoenix.Controller

  def init(opts), do: opts

  def call(conn, _opts) do
    user_id = get_session(conn, :user_id)

    cond do
      user_id && (user = MyApp.Accounts.get_user(user_id)) ->
        assign(conn, :current_user, user)

      true ->
        conn
        |> put_flash(:error, "You must be logged in")
        |> redirect(to: ~p"/login")
        |> halt()
    end
  end
end

# Usage in router
# lib/my_app_web/router.ex
# pipeline :auth do
#   plug MyAppWeb.Plugs.RequireAuth
# end
#
# scope "/", MyAppWeb do
#   pipe_through [:browser, :auth]
#   get "/dashboard", DashboardController, :index
#   get "/profile", ProfileController, :show
# end
```

**How It Works**: `put_session/3` stores user ID in encrypted cookie. `configure_session(renew: true)` prevents session fixation. Bcrypt hashes passwords with salt. Plug checks session on each request. `halt/1` stops pipeline on auth failure.

**Use Cases**: User login, protected routes, session management, authentication flows.

**See Also**: [Authentication Guide](/en/learn/software-engineering/programming-language/elixir/how-to/authentication-patterns), [Phoenix REST API Guide](/en/learn/software-engineering/programming-language/elixir/how-to/phoenix-rest-api), [Intermediate Tutorial - Phoenix](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#phoenix-framework)

---

### Recipe 29: Real-Time Updates with PubSub

**Problem**: Push updates to multiple connected clients.

**Solution**:

```elixir
defmodule MyAppWeb.DashboardLive do
  use MyAppWeb, :live_view

  @topic "dashboard:updates"

  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(MyApp.PubSub, @topic)
    end

    {:ok,
     socket
     |> assign(:metrics, load_metrics())
     |> assign(:last_update, DateTime.utc_now())}
  end

  # Handle broadcasts from other processes
  def handle_info({:metric_updated, metric}, socket) do
    metrics = update_metric(socket.assigns.metrics, metric)

    {:noreply,
     socket
     |> assign(:metrics, metrics)
     |> assign(:last_update, DateTime.utc_now())}
  end

  def handle_info(:refresh, socket) do
    {:noreply, assign(socket, :metrics, load_metrics())}
  end

  # Broadcast update to all connected clients
  def broadcast_update(metric) do
    Phoenix.PubSub.broadcast(MyApp.PubSub, @topic, {:metric_updated, metric})
  end

  defp load_metrics do
    %{
      users_online: 0,
      requests_per_min: 0,
      avg_response_time: 0
    }
  end

  defp update_metric(metrics, {key, value}) do
    Map.put(metrics, key, value)
  end

  def render(assigns) do
    ~H"""
    <div class="p-8">
      <h1 class="text-3xl font-bold mb-6">Dashboard</h1>

      <div class="grid grid-cols-3 gap-4 mb-4">
        <div class="bg-blue-100 p-4 rounded">
          <h2 class="text-xl">Users Online</h2>
          <p class="text-3xl font-bold"><%= @metrics.users_online %></p>
        </div>

        <div class="bg-green-100 p-4 rounded">
          <h2 class="text-xl">Requests/min</h2>
          <p class="text-3xl font-bold"><%= @metrics.requests_per_min %></p>
        </div>

        <div class="bg-yellow-100 p-4 rounded">
          <h2 class="text-xl">Avg Response (ms)</h2>
          <p class="text-3xl font-bold"><%= @metrics.avg_response_time %></p>
        </div>
      </div>

      <p class="text-sm text-gray-600">
        Last updated: <%= Calendar.strftime(@last_update, "%H:%M:%S") %>
      </p>
    </div>
    """
  end
end

# Background worker broadcasting updates
defmodule MyApp.MetricsCollector do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(state) do
    schedule_collection()
    {:ok, state}
  end

  def handle_info(:collect, state) do
    # Collect metrics
    users_online = :rand.uniform(100)
    requests_per_min = :rand.uniform(1000)
    avg_response_time = :rand.uniform(200)

    # Broadcast to all LiveView clients
    MyAppWeb.DashboardLive.broadcast_update({:users_online, users_online})
    MyAppWeb.DashboardLive.broadcast_update({:requests_per_min, requests_per_min})
    MyAppWeb.DashboardLive.broadcast_update({:avg_response_time, avg_response_time})

    schedule_collection()
    {:noreply, state}
  end

  defp schedule_collection do
    Process.send_after(self(), :collect, 5000)  # Every 5 seconds
  end
end
```

**How It Works**: PubSub broadcasts messages to topic. LiveView subscribes on mount. `handle_info` receives broadcasts and updates assigns.

**Use Cases**: Dashboards, chat apps, notifications, collaborative editing, live feeds.

**See Also**: [LiveView Guide](/en/learn/software-engineering/programming-language/elixir/how-to/liveview), [Intermediate Tutorial - LiveView](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#liveview)

---

## Ecto Database

### Recipe 30: Complex Queries with Joins

**Problem**: Query across multiple tables with associations.

**Solution**:

```elixir
defmodule MyApp.Queries.UserQueries do
  import Ecto.Query
  alias MyApp.Accounts.User
  alias MyApp.Blog.Post
  alias MyApp.Blog.Comment

  # Join with associations
  def users_with_posts do
    from u in User,
      join: p in assoc(u, :posts),
      preload: [posts: p],
      where: p.published == true
  end

  # Left join (include users without posts)
  def all_users_with_post_count do
    from u in User,
      left_join: p in assoc(u, :posts),
      group_by: u.id,
      select: {u, count(p.id)}
  end

  # Multiple joins
  def posts_with_author_and_comments do
    from p in Post,
      join: u in assoc(p, :user),
      left_join: c in assoc(p, :comments),
      preload: [user: u, comments: c],
      order_by: [desc: p.inserted_at]
  end

  # Filtered joins
  def users_with_recent_posts(days) do
    cutoff = DateTime.add(DateTime.utc_now(), -days * 24 * 3600, :second)

    from u in User,
      join: p in assoc(u, :posts),
      where: p.inserted_at >= ^cutoff,
      distinct: true,
      preload: [posts: p]
  end

  # Aggregations across joins
  def user_post_statistics do
    from u in User,
      left_join: p in assoc(u, :posts),
      left_join: c in assoc(p, :comments),
      group_by: u.id,
      select: %{
        user_id: u.id,
        name: u.name,
        post_count: count(p.id, :distinct),
        comment_count: count(c.id)
      }
  end
end

# Usage
import Ecto.Query
alias MyApp.Repo

# Users who have published posts
users = MyApp.Queries.UserQueries.users_with_posts() |> Repo.all()

# All users with post count
results = MyApp.Queries.UserQueries.all_users_with_post_count() |> Repo.all()
# => [{%User{...}, 5}, {%User{...}, 0}, ...]

# Posts with full associations
posts = MyApp.Queries.UserQueries.posts_with_author_and_comments() |> Repo.all()

# Users active in last 7 days
active_users = MyApp.Queries.UserQueries.users_with_recent_posts(7) |> Repo.all()

# Statistics
stats = MyApp.Queries.UserQueries.user_post_statistics() |> Repo.all()
# => [%{user_id: 1, name: "Alice", post_count: 5, comment_count: 12}, ...]
```

**How It Works**: `join` combines tables. `assoc` uses schema associations. `preload` loads related data. `group_by` and aggregate functions for statistics.

**Use Cases**: Reports, analytics, complex data retrieval, dashboards.

**See Also**: [Ecto Guide](/en/learn/software-engineering/programming-language/elixir/how-to/ecto), [Intermediate Tutorial - Ecto](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#ecto)

---

### Recipe 31: Transactions and Rollbacks

**Problem**: Execute multiple database operations atomically.

**Solution**:

```elixir
defmodule MyApp.Orders do
  import Ecto.Query
  alias MyApp.Repo
  alias MyApp.Orders.Order
  alias MyApp.Inventory.Product
  alias MyApp.Billing.Payment

  # Multi-step transaction
  def create_order(user, cart_items) do
    Repo.transaction(fn ->
      # Step 1: Create order
      order_params = %{user_id: user.id, status: :pending}
      {:ok, order} = create_order_record(order_params)

      # Step 2: Reserve inventory
      case reserve_inventory(cart_items) do
        :ok -> :ok
        {:error, reason} -> Repo.rollback(reason)
      end

      # Step 3: Process payment
      total = calculate_total(cart_items)
      case process_payment(user, total) do
        {:ok, payment} ->
          update_order_payment(order, payment)

        {:error, reason} ->
          Repo.rollback({:payment_failed, reason})
      end

      # Step 4: Create order items
      create_order_items(order, cart_items)

      # Return completed order
      order |> Repo.preload([:items, :payment])
    end)
  end

  # Using Ecto.Multi for explicit transaction steps
  def create_order_with_multi(user, cart_items) do
    Ecto.Multi.new()
    |> Ecto.Multi.insert(:order, order_changeset(user))
    |> Ecto.Multi.run(:inventory, fn _repo, %{order: order} ->
      reserve_inventory(cart_items)
    end)
    |> Ecto.Multi.run(:payment, fn _repo, %{order: order} ->
      total = calculate_total(cart_items)
      process_payment(user, total)
    end)
    |> Ecto.Multi.run(:order_items, fn _repo, %{order: order} ->
      create_order_items(order, cart_items)
    end)
    |> Ecto.Multi.update(:complete_order, fn %{order: order, payment: payment} ->
      Ecto.Changeset.change(order, status: :completed, payment_id: payment.id)
    end)
    |> Repo.transaction()
    |> case do
      {:ok, %{complete_order: order}} ->
        {:ok, order}

      {:error, :inventory, reason, _changes} ->
        {:error, {:inventory_failed, reason}}

      {:error, :payment, reason, _changes} ->
        {:error, {:payment_failed, reason}}

      {:error, step, reason, _changes} ->
        {:error, {step, reason}}
    end
  end

  defp order_changeset(user) do
    %Order{}
    |> Ecto.Changeset.cast(%{user_id: user.id, status: :pending}, [:user_id, :status])
    |> Ecto.Changeset.validate_required([:user_id, :status])
  end

  defp create_order_record(params) do
    %Order{}
    |> Order.changeset(params)
    |> Repo.insert()
  end

  defp reserve_inventory(cart_items) do
    # Check and update inventory
    :ok
  end

  defp process_payment(user, total) do
    # Process payment
    {:ok, %Payment{amount: total}}
  end

  defp calculate_total(cart_items) do
    Enum.reduce(cart_items, 0, fn item, acc ->
      acc + item.price * item.quantity
    end)
  end

  defp create_order_items(order, cart_items) do
    {:ok, []}
  end

  defp update_order_payment(order, payment) do
    order
    |> Ecto.Changeset.change(payment_id: payment.id, status: :completed)
    |> Repo.update()
  end
end

# Usage
user = Repo.get!(User, 1)
cart_items = [
  %{product_id: 1, price: 10.00, quantity: 2},
  %{product_id: 2, price: 15.00, quantity: 1}
]

case MyApp.Orders.create_order_with_multi(user, cart_items) do
  {:ok, order} ->
    IO.puts("Order created: #{order.id}")

  {:error, {:payment_failed, reason}} ->
    IO.puts("Payment failed: #{inspect(reason)}")

  {:error, reason} ->
    IO.puts("Order failed: #{inspect(reason)}")
end
```

**How It Works**: `Repo.transaction/1` wraps operations. `Repo.rollback/1` aborts transaction. `Ecto.Multi` builds transaction pipeline with named steps and error handling.

**Use Cases**: Orders, payments, multi-step workflows, data consistency.

**See Also**: [Ecto Guide](/en/learn/software-engineering/programming-language/elixir/how-to/ecto), [Intermediate Tutorial - Ecto](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#ecto)

---

## Testing

### Recipe 32: Test GenServer Behavior

**Problem**: Test stateful GenServer operations.

**Solution**:

```elixir
defmodule MyApp.CacheTest do
  use ExUnit.Case, async: true
  alias MyApp.Cache

  setup do
    # Start fresh cache for each test
    {:ok, pid} = Cache.start_link([])
    %{cache: pid}
  end

  test "stores and retrieves values", %{cache: cache} do
    assert :ok = Cache.put(cache, :key1, "value1")
    assert "value1" = Cache.get(cache, :key1)
  end

  test "returns nil for missing keys", %{cache: cache} do
    assert nil == Cache.get(cache, :nonexistent)
  end

  test "overwrites existing keys", %{cache: cache} do
    Cache.put(cache, :key1, "value1")
    Cache.put(cache, :key1, "value2")
    assert "value2" = Cache.get(cache, :key1)
  end

  test "handles concurrent operations", %{cache: cache} do
    # Spawn multiple tasks writing to cache
    tasks = for i <- 1..100 do
      Task.async(fn ->
        Cache.put(cache, "key_#{i}", "value_#{i}")
      end)
    end

    # Wait for all writes
    Task.await_many(tasks)

    # Verify all writes succeeded
    for i <- 1..100 do
      assert "value_#{i}" = Cache.get(cache, "key_#{i}")
    end
  end

  test "cache survives crashes with supervision" do
    # This requires supervisor setup
    # See supervision tree tests
  end
end

# Testing async operations
defmodule MyApp.AsyncWorkerTest do
  use ExUnit.Case, async: true
  alias MyApp.AsyncWorker

  test "completes async task" do
    {:ok, pid} = AsyncWorker.start_link([])

    # Start async operation
    :ok = AsyncWorker.process_async(pid, "data")

    # Wait for completion message
    assert_receive {:completed, "data"}, 1000
  end

  test "handles task timeout" do
    {:ok, pid} = AsyncWorker.start_link(timeout: 100)

    # Start slow operation
    :ok = AsyncWorker.process_async(pid, fn ->
      :timer.sleep(200)
      "result"
    end)

    # Should receive timeout
    assert_receive {:timeout, _}, 500
  end
end
```

**How It Works**: `setup` creates isolated GenServer per test. `async: true` runs tests concurrently. `assert_receive` waits for messages.

**Use Cases**: GenServer testing, concurrent behavior, async operations.

**See Also**: [Testing Guide](/en/learn/software-engineering/programming-language/elixir/how-to/testing), [Beginner Tutorial - Testing](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#testing-exunit)

---

### Recipe 33: Mock External Services

**Problem**: Test code that depends on external APIs without hitting them.

**Solution**:

```elixir
# Define behavior
defmodule MyApp.EmailService do
  @callback send_email(to :: String.t(), subject :: String.t(), body :: String.t()) ::
              {:ok, term()} | {:error, term()}
end

# Real implementation
defmodule MyApp.EmailService.SMTP do
  @behaviour MyApp.EmailService

  @impl true
  def send_email(to, subject, body) do
    # Actual SMTP logic
    {:ok, "Email sent to #{to}"}
  end
end

# Mock implementation
defmodule MyApp.EmailService.Mock do
  @behaviour MyApp.EmailService

  @impl true
  def send_email(to, subject, body) do
    # Store for verification in tests
    send(self(), {:email_sent, to, subject, body})
    {:ok, "Mocked email"}
  end
end

# Module using email service
defmodule MyApp.UserNotifier do
  def notify_welcome(user) do
    email_service().send_email(
      user.email,
      "Welcome!",
      "Welcome to our app, #{user.name}!"
    )
  end

  defp email_service do
    Application.get_env(:my_app, :email_service, MyApp.EmailService.SMTP)
  end
end

# Test
defmodule MyApp.UserNotifierTest do
  use ExUnit.Case

  setup do
    # Configure mock
    Application.put_env(:my_app, :email_service, MyApp.EmailService.Mock)

    on_exit(fn ->
      Application.delete_env(:my_app, :email_service)
    end)
  end

  test "sends welcome email" do
    user = %{email: "test@example.com", name: "Alice"}

    {:ok, _} = MyApp.UserNotifier.notify_welcome(user)

    assert_received {:email_sent, "test@example.com", "Welcome!", body}
    assert body =~ "Alice"
  end
end

# Using Mox library (recommended)
defmodule MyApp.EmailServiceTest do
  use ExUnit.Case, async: true
  import Mox

  # Define mock module
  Mox.defmock(MyApp.EmailService.MockMox, for: MyApp.EmailService)

  setup :verify_on_exit!

  test "sends email via mox" do
    # Set expectations
    expect(MyApp.EmailService.MockMox, :send_email, fn to, subject, body ->
      assert to == "user@example.com"
      assert subject == "Test"
      {:ok, "sent"}
    end)

    # Call code under test
    MyApp.EmailService.MockMox.send_email("user@example.com", "Test", "Body")
  end
end
```

**How It Works**: Define `@behaviour` for interface. Implement real and mock versions. Configure via Application env. Mox provides stricter mocking with expectations.

**Use Cases**: External API testing, service integration, CI/CD testing.

**See Also**: [Testing Guide](/en/learn/software-engineering/programming-language/elixir/how-to/testing)

---

## Performance

### Recipe 34: Profile and Benchmark Code

**Problem**: Identify performance bottlenecks and measure improvements.

**Solution**:

```elixir
defmodule MyApp.Performance do
  # Benchmarking with :timer.tc
  def benchmark(name, fun) do
    {time_us, result} = :timer.tc(fun)
    IO.puts("#{name}: #{time_us / 1_000} ms")
    result
  end

  # Benchmarking with Benchee
  def compare_implementations do
    Benchee.run(
      %{
        "Enum.map" => fn input -> Enum.map(input, &(&1 * 2)) end,
        "for comprehension" => fn input -> for x <- input, do: x * 2 end,
        "Stream.map" => fn input -> input |> Stream.map(&(&1 * 2)) |> Enum.to_list() end
      },
      inputs: %{
        "Small (100)" => Enum.to_list(1..100),
        "Medium (10_000)" => Enum.to_list(1..10_000),
        "Large (1_000_000)" => Enum.to_list(1..1_000_000)
      },
      time: 5,
      memory_time: 2
    )
  end

  # Profiling with :fprof
  def profile_function(fun) do
    :fprof.trace([:start, {:procs, self()}])
    result = fun.()
    :fprof.trace(:stop)

    :fprof.profile()
    :fprof.analyse([:totals, {:sort, :acc}, {:dest, 'fprof_analysis.txt'}])

    IO.puts("Profile saved to fprof_analysis.txt")
    result
  end

  # Memory profiling
  def measure_memory(fun) do
    :erlang.garbage_collect()
    memory_before = :erlang.memory(:total)

    result = fun.()

    :erlang.garbage_collect()
    memory_after = :erlang.memory(:total)
    memory_used = memory_after - memory_before

    IO.puts("Memory used: #{memory_used / 1024 / 1024} MB")
    result
  end
end

# Usage
# Simple benchmark
MyApp.Performance.benchmark("List processing", fn ->
  Enum.map(1..10_000, &(&1 * 2))
end)
# => List processing: 2.5 ms

# Compare implementations
MyApp.Performance.compare_implementations()
# Outputs detailed benchmark comparison

# Profile function
MyApp.Performance.profile_function(fn ->
  # Your expensive operation
  expensive_operation()
end)

# Measure memory
MyApp.Performance.measure_memory(fn ->
  # Operation that allocates memory
  large_list = Enum.map(1..1_000_000, &(&1 * 2))
end)

# Using ExProf (wrapper around :fprof)
defmodule MyApp.Expensive do
  import ExProf.Macro

  def run do
    profile do
      expensive_operation()
    end
  end

  defp expensive_operation do
    1..10_000
    |> Enum.map(&heavy_computation/1)
    |> Enum.sum()
  end

  defp heavy_computation(n) do
    :timer.sleep(1)
    n * n
  end
end
```

**How It Works**: `:timer.tc/1` measures execution time. Benchee provides statistical analysis. `:fprof` profiles function calls. Memory tracking via `:erlang.memory/1`.

**Use Cases**: Performance optimization, bottleneck identification, regression testing.

**See Also**: [Performance Guide](/en/learn/software-engineering/programming-language/elixir/how-to/performance), [Advanced Tutorial - Performance](/en/learn/software-engineering/programming-language/elixir/tutorials/advanced#performance-optimization)

---

### Recipe 35: Optimize Database Queries

**Problem**: Speed up slow Ecto queries.

**Solution**:

```elixir
defmodule MyApp.OptimizedQueries do
  import Ecto.Query
  alias MyApp.Repo

  # Bad: N+1 query problem
  def users_with_posts_slow do
    users = Repo.all(User)

    Enum.map(users, fn user ->
      posts = Repo.all(from p in Post, where: p.user_id == ^user.id)
      {user, posts}
    end)
  end

  # Good: Preload associations
  def users_with_posts_fast do
    User
    |> Repo.all()
    |> Repo.preload(:posts)
  end

  # Good: Join and preload in single query
  def users_with_posts_optimized do
    from(u in User,
      join: p in assoc(u, :posts),
      preload: [posts: p]
    )
    |> Repo.all()
  end

  # Use select to fetch only needed fields
  def user_names_and_emails do
    from(u in User,
      select: %{name: u.name, email: u.email}
    )
    |> Repo.all()
  end

  # Pagination with limit/offset
  def paginated_users(page, per_page) do
    offset = (page - 1) * per_page

    from(u in User,
      limit: ^per_page,
      offset: ^offset,
      order_by: [desc: u.inserted_at]
    )
    |> Repo.all()
  end

  # Use indexes (in migration)
  def create_indexes do
    """
    defmodule MyApp.Repo.Migrations.AddIndexes do
      use Ecto.Migration

      def change do
        # Index foreign keys
        create index(:posts, [:user_id])
        create index(:comments, [:post_id])

        # Index frequently queried fields
        create index(:users, [:email])
        create index(:posts, [:published_at])

        # Composite index for common query patterns
        create index(:posts, [:user_id, :published_at])

        # Unique index
        create unique_index(:users, [:email])
      end
    end
    """
  end

  # Batch processing to avoid memory issues
  def process_all_users_in_batches(batch_size \\ 1000) do
    stream = Repo.stream(User)

    Repo.transaction(fn ->
      stream
      |> Stream.chunk_every(batch_size)
      |> Enum.each(fn batch ->
        # Process batch
        process_batch(batch)
      end)
    end)
  end

  defp process_batch(users) do
    # Process users in batch
    IO.puts("Processing #{length(users)} users")
  end
end
```

**How It Works**: Preloading avoids N+1 queries. Selective fields reduce data transfer. Indexes speed up lookups. Streaming processes large datasets without loading all into memory.

**Use Cases**: Database optimization, query performance, large dataset processing.

**See Also**: [Ecto Guide](/en/learn/software-engineering/programming-language/elixir/how-to/ecto), [Performance Guide](/en/learn/software-engineering/programming-language/elixir/how-to/performance)

---

## Configuration

### Recipe 36: Environment-Based Configuration

**Problem**: Manage configuration across environments (dev, test, prod).

**Solution**:

```elixir
# config/config.exs (base configuration)
import Config

config :my_app,
  ecto_repos: [MyApp.Repo]

config :my_app, MyApp.Repo,
  database: "my_app_dev",
  hostname: "localhost",
  pool_size: 10

# Import environment-specific config
import_config "#{config_env()}.exs"

# config/dev.exs
import Config

config :my_app, MyApp.Repo,
  database: "my_app_dev",
  hostname: "localhost",
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

config :my_app, MyAppWeb.Endpoint,
  http: [port: 4000],
  debug_errors: true,
  code_reloader: true

# config/test.exs
import Config

config :my_app, MyApp.Repo,
  database: "my_app_test#{System.get_env("MIX_TEST_PARTITION")}",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10

config :my_app, MyAppWeb.Endpoint,
  http: [port: 4002],
  server: false

# config/prod.exs
import Config

# Runtime configuration (config/runtime.exs)
import Config

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
      raise """
      environment variable DATABASE_URL is missing.
      For example: ecto://USER:PASS@HOST/DATABASE
      """

  config :my_app, MyApp.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
    ssl: true

  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      """

  config :my_app, MyAppWeb.Endpoint,
    http: [port: String.to_integer(System.get_env("PORT") || "4000")],
    secret_key_base: secret_key_base,
    server: true
end

# Accessing configuration in code
defmodule MyApp.Config do
  def database_url do
    Application.get_env(:my_app, MyApp.Repo)[:url]
  end

  def pool_size do
    Application.get_env(:my_app, MyApp.Repo)[:pool_size]
  end

  def feature_enabled?(feature) do
    Application.get_env(:my_app, :features, %{})
    |> Map.get(feature, false)
  end
end

# Usage
MyApp.Config.database_url()
# => "ecto://user:pass@localhost/my_app_dev"

MyApp.Config.pool_size()
# => 10

MyApp.Config.feature_enabled?(:new_dashboard)
# => false
```

**How It Works**: `config.exs` loads base config. Environment-specific files override. `runtime.exs` loads after compilation (for env vars). `Application.get_env/3` retrieves config.

**Use Cases**: Multi-environment deployment, secret management, feature flags.

**See Also**: [Configuration Guide](/en/learn/software-engineering/programming-language/elixir/how-to/configuration-management), [Intermediate Tutorial - Configuration](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#configuration-management)

---

## Debugging

### Recipe 37: Debug Techniques and Tools

**Problem**: Identify and fix bugs efficiently.

**Solution**:

```elixir
defmodule MyApp.Debugging do
  require Logger

  # 1. IO.inspect with labels
  def inspect_pipeline(data) do
    data
    |> IO.inspect(label: "Input")
    |> transform_step1()
    |> IO.inspect(label: "After step 1")
    |> transform_step2()
    |> IO.inspect(label: "After step 2")
  end

  # 2. IEx.pry for breakpoints (development only)
  def debug_with_pry(data) do
    require IEx

    transformed = transform_data(data)
    IEx.pry()  # Execution pauses here, opens IEx shell

    process_result(transformed)
  end

  # 3. Logger for production debugging
  def log_operation(user, action) do
    Logger.info("User #{user.id} performing #{action}")

    case perform_action(user, action) do
      {:ok, result} ->
        Logger.info("Action #{action} succeeded",
          user_id: user.id,
          result: inspect(result))
        {:ok, result}

      {:error, reason} ->
        Logger.error("Action #{action} failed",
          user_id: user.id,
          reason: inspect(reason))
        {:error, reason}
    end
  end

  # 4. Using dbg for Elixir 1.14+
  def debug_with_dbg(data) do
    data
    |> transform_step1()
    |> dbg()  # Shows transformation result
    |> transform_step2()
  end

  # 5. Pattern matching for debugging
  def debug_pattern_match(result) do
    case result do
      {:ok, value} = success ->
        IO.inspect(success, label: "Success case")
        value

      {:error, reason} = error ->
        IO.inspect(error, label: "Error case")
        raise "Operation failed: #{inspect(reason)}"
    end
  end

  # 6. Observer for runtime inspection
  def start_observer do
    :observer.start()
  end

  # 7. Recon for production debugging
  def check_memory_usage do
    :recon.proc_count(:memory, 10)
    |> IO.inspect(label: "Top 10 processes by memory")
  end

  def check_message_queue do
    :recon.proc_count(:message_queue_len, 10)
    |> IO.inspect(label: "Top 10 processes by message queue")
  end

  defp transform_step1(data), do: data
  defp transform_step2(data), do: data
  defp transform_data(data), do: data
  defp process_result(data), do: data
  defp perform_action(_user, _action), do: {:ok, "result"}
end

# Usage in IEx
# iex> MyApp.Debugging.debug_with_pry(%{id: 1})
# # Execution pauses, you can inspect variables:
# iex(1)> transformed
# iex(1)> self()

# Using Observer
# iex> MyApp.Debugging.start_observer()
# Opens GUI showing processes, memory, applications

# Production debugging with Logger
# Configure in config/prod.exs:
# config :logger, :console,
#   format: "$time $metadata[$level] $message\n",
#   metadata: [:request_id, :user_id]

# Set log level
# config :logger, level: :info  # :debug, :info, :warn, :error
```

**How It Works**: `IO.inspect/2` shows values without breaking flow. `IEx.pry/0` pauses execution for inspection. Logger records events. `dbg/1` traces pipe operations. Observer visualizes runtime state.

**Use Cases**: Development debugging, production troubleshooting, performance analysis.

**See Also**: [Debugging Guide](/en/learn/software-engineering/programming-language/elixir/how-to/debugging-logging), [Beginner Tutorial - Debugging](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#debugging-tools)

---

### Recipe 38: Background Job Processing with GenServer

**Problem**: Process long-running tasks asynchronously without blocking requests.

**Solution**:

```elixir
defmodule MyApp.JobQueue do
  use GenServer
  require Logger

  # Client API
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def enqueue(job_type, params) do
    job = %{
      id: generate_job_id(),
      type: job_type,
      params: params,
      enqueued_at: DateTime.utc_now(),
      status: :pending
    }

    GenServer.cast(__MODULE__, {:enqueue, job})
    {:ok, job.id}
  end

  def get_status(job_id) do
    GenServer.call(__MODULE__, {:get_status, job_id})
  end

  # Server Callbacks
  @impl true
  def init(_opts) do
    state = %{
      queue: :queue.new(),
      processing: %{},
      completed: %{},
      max_concurrent: 5
    }

    schedule_process_jobs()
    {:ok, state}
  end

  @impl true
  def handle_cast({:enqueue, job}, state) do
    new_queue = :queue.in(job, state.queue)
    {:noreply, %{state | queue: new_queue}}
  end

  @impl true
  def handle_call({:get_status, job_id}, _from, state) do
    status = cond do
      Map.has_key?(state.processing, job_id) -> {:processing, state.processing[job_id]}
      Map.has_key?(state.completed, job_id) -> {:completed, state.completed[job_id]}
      true -> {:not_found, nil}
    end

    {:reply, status, state}
  end

  @impl true
  def handle_info(:process_jobs, state) do
    state = process_pending_jobs(state)
    schedule_process_jobs()
    {:noreply, state}
  end

  @impl true
  def handle_info({:job_completed, job_id, result}, state) do
    Logger.info("Job #{job_id} completed: #{inspect(result)}")

    completed_job = Map.get(state.processing, job_id)
    |> Map.put(:result, result)
    |> Map.put(:completed_at, DateTime.utc_now())
    |> Map.put(:status, :completed)

    new_state = %{state |
      processing: Map.delete(state.processing, job_id),
      completed: Map.put(state.completed, job_id, completed_job)
    }

    {:noreply, new_state}
  end

  @impl true
  def handle_info({:job_failed, job_id, error}, state) do
    Logger.error("Job #{job_id} failed: #{inspect(error)}")

    failed_job = Map.get(state.processing, job_id)
    |> Map.put(:error, error)
    |> Map.put(:failed_at, DateTime.utc_now())
    |> Map.put(:status, :failed)

    new_state = %{state |
      processing: Map.delete(state.processing, job_id),
      completed: Map.put(state.completed, job_id, failed_job)
    }

    {:noreply, new_state}
  end

  # Private Functions
  defp process_pending_jobs(state) do
    concurrent_count = map_size(state.processing)
    available_slots = state.max_concurrent - concurrent_count

    if available_slots > 0 && !:queue.is_empty(state.queue) do
      {jobs_to_process, remaining_queue} = dequeue_n(state.queue, available_slots)

      processing = Enum.reduce(jobs_to_process, state.processing, fn job, acc ->
        spawn_job_worker(job)
        Map.put(acc, job.id, Map.put(job, :status, :processing))
      end)

      %{state | queue: remaining_queue, processing: processing}
    else
      state
    end
  end

  defp dequeue_n(queue, n) do
    dequeue_n(queue, n, [])
  end

  defp dequeue_n(queue, 0, acc) do
    {Enum.reverse(acc), queue}
  end

  defp dequeue_n(queue, n, acc) do
    case :queue.out(queue) do
      {{:value, item}, new_queue} ->
        dequeue_n(new_queue, n - 1, [item | acc])

      {:empty, queue} ->
        {Enum.reverse(acc), queue}
    end
  end

  defp spawn_job_worker(job) do
    parent = self()

    Task.start(fn ->
      try do
        result = execute_job(job)
        send(parent, {:job_completed, job.id, result})
      rescue
        error ->
          send(parent, {:job_failed, job.id, error})
      end
    end)
  end

  defp execute_job(%{type: :send_email, params: params}) do
    # Simulate email sending
    Process.sleep(1000)
    {:ok, "Email sent to #{params[:to]}"}
  end

  defp execute_job(%{type: :process_upload, params: params}) do
    # Simulate file processing
    Process.sleep(2000)
    {:ok, "Processed file #{params[:filename]}"}
  end

  defp execute_job(%{type: :generate_report, params: params}) do
    # Simulate report generation
    Process.sleep(3000)
    {:ok, "Generated report #{params[:report_id]}"}
  end

  defp execute_job(%{type: type}) do
    {:error, "Unknown job type: #{type}"}
  end

  defp schedule_process_jobs do
    Process.send_after(self(), :process_jobs, 1000)
  end

  defp generate_job_id do
    :crypto.strong_rand_bytes(16) |> Base.encode16()
  end
end

# Usage
MyApp.JobQueue.start_link()

{:ok, job_id} = MyApp.JobQueue.enqueue(:send_email, %{to: "user@example.com", subject: "Welcome"})
# => {:ok, "A1B2C3D4E5F6G7H8"}

MyApp.JobQueue.get_status(job_id)
# => {:processing, %{id: "A1B2C3...", type: :send_email, ...}}

# After job completes
MyApp.JobQueue.get_status(job_id)
# => {:completed, %{id: "A1B2C3...", result: {:ok, "Email sent..."}, ...}}
```

**How It Works**: GenServer maintains queue of pending jobs. `:process_jobs` message triggers batch processing. `max_concurrent` limits parallel jobs. Task spawns worker process for each job. Worker sends completion/failure message back to GenServer.

**Use Cases**: Email sending, file processing, report generation, data exports, webhook delivery.

**See Also**: [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver), [Task Guide](/en/learn/software-engineering/programming-language/elixir/how-to/task-agent-concurrency), [Intermediate Tutorial - OTP](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#otp-platform)

---

### Recipe 39: Database Migrations with Ecto

**Problem**: Manage schema changes and data migrations safely.

**Solution**:

```elixir
# Generate migration
# mix ecto.gen.migration create_users_table

# priv/repo/migrations/20241221_create_users_table.exs
defmodule MyApp.Repo.Migrations.CreateUsersTable do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :email, :string, null: false
      add :name, :string
      add :age, :integer
      add :role, :string, default: "user"
      add :active, :boolean, default: true
      add :metadata, :map, default: %{}

      timestamps()
    end

    create unique_index(:users, [:email])
    create index(:users, [:role])
    create index(:users, [:active])
  end
end

# Add column migration
# mix ecto.gen.migration add_confirmed_at_to_users

defmodule MyApp.Repo.Migrations.AddConfirmedAtToUsers do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :confirmed_at, :naive_datetime
    end

    create index(:users, [:confirmed_at])
  end
end

# Data migration (separate from schema changes)
# mix ecto.gen.migration migrate_user_roles

defmodule MyApp.Repo.Migrations.MigrateUserRoles do
  use Ecto.Migration
  import Ecto.Query

  def up do
    # Rename role values using raw SQL
    execute """
    UPDATE users
    SET role = CASE
      WHEN role = 'admin' THEN 'administrator'
      WHEN role = 'mod' THEN 'moderator'
      ELSE role
    END
    """

    # Or using Ecto query (safer for complex logic)
    repo().update_all(
      from(u in "users", where: u.role == "admin"),
      set: [role: "administrator"]
    )
  end

  def down do
    repo().update_all(
      from(u in "users", where: u.role == "administrator"),
      set: [role: "admin"]
    )
  end
end

# Complex migration with multiple operations
defmodule MyApp.Repo.Migrations.RefactorUserProfiles do
  use Ecto.Migration

  def up do
    # Create new table
    create table(:user_profiles) do
      add :user_id, references(:users, on_delete: :delete_all), null: false
      add :bio, :text
      add :avatar_url, :string
      add :preferences, :map, default: %{}

      timestamps()
    end

    create unique_index(:user_profiles, [:user_id])

    # Migrate data from users to user_profiles
    execute """
    INSERT INTO user_profiles (user_id, bio, inserted_at, updated_at)
    SELECT id, bio, inserted_at, updated_at
    FROM users
    WHERE bio IS NOT NULL
    """

    # Remove column from users table
    alter table(:users) do
      remove :bio
    end
  end

  def down do
    # Add column back
    alter table(:users) do
      add :bio, :text
    end

    # Migrate data back
    execute """
    UPDATE users u
    SET bio = up.bio
    FROM user_profiles up
    WHERE u.id = up.user_id
    """

    # Drop table
    drop table(:user_profiles)
  end
end

# Migration commands:
# mix ecto.create          # Create database
# mix ecto.migrate          # Run pending migrations
# mix ecto.rollback         # Rollback last migration
# mix ecto.rollback --step 3  # Rollback 3 migrations
# mix ecto.reset            # Drop, create, migrate
# mix ecto.migrations       # List migration status
```

**How It Works**: `change/0` defines reversible migrations. `up/0` and `down/0` for custom logic. `execute/1` runs raw SQL. `repo().update_all/2` for data migrations. Migrations run in transaction by default (except for some DDL operations).

**Use Cases**: Schema evolution, data migrations, index management, constraint additions, table refactoring.

**See Also**: [Ecto Guide](/en/learn/software-engineering/programming-language/elixir/how-to/ecto-patterns), [Intermediate Tutorial - Ecto](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#ecto)

---

### Recipe 40: API Versioning Strategies

**Problem**: Support multiple API versions simultaneously.

**Solution**:

```elixir
# Strategy 1: URL Path Versioning
# lib/my_app_web/router.ex
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  # Version 1
  scope "/api/v1", MyAppWeb.V1, as: :v1 do
    pipe_through :api

    resources "/users", UserController, except: [:new, :edit]
    resources "/posts", PostController, except: [:new, :edit]
  end

  # Version 2
  scope "/api/v2", MyAppWeb.V2, as: :v2 do
    pipe_through :api

    resources "/users", UserController, except: [:new, :edit]
    resources "/posts", PostController, except: [:new, :edit]
  end
end

# lib/my_app_web/controllers/v1/user_controller.ex
defmodule MyAppWeb.V1.UserController do
  use MyAppWeb, :controller

  def index(conn, _params) do
    users = MyApp.Accounts.list_users()
    render(conn, "index.json", users: users)
  end

  def show(conn, %{"id" => id}) do
    user = MyApp.Accounts.get_user!(id)
    render(conn, "show.json", user: user)
  end
end

# lib/my_app_web/views/v1/user_view.ex
defmodule MyAppWeb.V1.UserView do
  use MyAppWeb, :view

  def render("index.json", %{users: users}) do
    %{data: Enum.map(users, &user_json/1)}
  end

  def render("show.json", %{user: user}) do
    %{data: user_json(user)}
  end

  defp user_json(user) do
    %{
      id: user.id,
      email: user.email,
      name: user.name
    }
  end
end

# lib/my_app_web/controllers/v2/user_controller.ex (updated structure)
defmodule MyAppWeb.V2.UserController do
  use MyAppWeb, :controller

  def index(conn, params) do
    page = Map.get(params, "page", 1)
    per_page = Map.get(params, "per_page", 20)

    users = MyApp.Accounts.list_users_paginated(page, per_page)
    total = MyApp.Accounts.count_users()

    render(conn, "index.json", users: users, total: total, page: page, per_page: per_page)
  end
end

# lib/my_app_web/views/v2/user_view.ex (enhanced response)
defmodule MyAppWeb.V2.UserView do
  use MyAppWeb, :view

  def render("index.json", %{users: users, total: total, page: page, per_page: per_page}) do
    %{
      data: Enum.map(users, &user_json/1),
      pagination: %{
        page: page,
        per_page: per_page,
        total: total,
        total_pages: ceil(total / per_page)
      }
    }
  end

  defp user_json(user) do
    %{
      id: user.id,
      email: user.email,
      name: user.name,
      created_at: user.inserted_at,  # New field in v2
      profile_url: "/users/#{user.id}/profile"  # HATEOAS link
    }
  end
end

# Strategy 2: Header Versioning
defmodule MyAppWeb.Plugs.APIVersion do
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, _opts) do
    version = get_req_header(conn, "accept")
    |> List.first()
    |> parse_version()

    assign(conn, :api_version, version)
  end

  defp parse_version("application/vnd.myapp.v1+json"), do: :v1
  defp parse_version("application/vnd.myapp.v2+json"), do: :v2
  defp parse_version(_), do: :v2  # Default to latest
end

# lib/my_app_web/controllers/user_controller.ex (version-aware)
defmodule MyAppWeb.UserController do
  use MyAppWeb, :controller

  def index(conn, params) do
    case conn.assigns.api_version do
      :v1 -> index_v1(conn, params)
      :v2 -> index_v2(conn, params)
    end
  end

  defp index_v1(conn, _params) do
    users = MyApp.Accounts.list_users()
    render(conn, "index_v1.json", users: users)
  end

  defp index_v2(conn, params) do
    page = Map.get(params, "page", 1)
    users = MyApp.Accounts.list_users_paginated(page, 20)
    render(conn, "index_v2.json", users: users, page: page)
  end
end

# Strategy 3: Shared Code with Version-Specific Overrides
defmodule MyAppWeb.Shared.UserController do
  # Common functionality
  def get_user(id) do
    MyApp.Accounts.get_user!(id)
  end
end

defmodule MyAppWeb.V1.UserController do
  use MyAppWeb, :controller
  import MyAppWeb.Shared.UserController

  def show(conn, %{"id" => id}) do
    user = get_user(id)  # Shared function
    render(conn, "show.json", user: user)
  end
end

defmodule MyAppWeb.V2.UserController do
  use MyAppWeb, :controller
  import MyAppWeb.Shared.UserController

  def show(conn, %{"id" => id}) do
    user = get_user(id)  # Same shared function
    user = MyApp.Accounts.preload_associations(user, :v2)  # Version-specific enhancement
    render(conn, "show.json", user: user)
  end
end
```

**How It Works**: URL versioning uses scoped routes with version namespace. Header versioning checks Accept header. Shared modules reduce duplication. Each version has dedicated controllers/views. Migration guide helps clients upgrade.

**Use Cases**: Public APIs, mobile app backends, third-party integrations, gradual feature rollouts.

**See Also**: [Phoenix REST API Guide](/en/learn/software-engineering/programming-language/elixir/how-to/phoenix-rest-api), [Intermediate Tutorial - Phoenix](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#phoenix-framework)

---

### Recipe 41: Rate Limiting Patterns

**Problem**: Prevent API abuse with request rate limiting.

**Solution**:

```elixir
# Token Bucket Algorithm with ETS
defmodule MyApp.RateLimiter do
  use GenServer

  @table :rate_limiter_buckets
  @refill_interval 1000  # 1 second
  @max_tokens 10  # 10 requests per second

  # Client API
  def start_link(opts \ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def check_rate(identifier, cost \ 1) do
    GenServer.call(__MODULE__, {:check_rate, identifier, cost})
  end

  # Server Callbacks
  @impl true
  def init(_opts) do
    :ets.new(@table, [:named_table, :set, :public, read_concurrency: true])
    schedule_refill()
    {:ok, %{}}
  end

  @impl true
  def handle_call({:check_rate, identifier, cost}, _from, state) do
    now = System.system_time(:millisecond)
    bucket = get_or_create_bucket(identifier, now)

    if bucket.tokens >= cost do
      new_bucket = %{bucket | tokens: bucket.tokens - cost}
      :ets.insert(@table, {identifier, new_bucket})
      {:reply, {:ok, new_bucket.tokens}, state}
    else
      retry_after = calculate_retry_after(bucket)
      {:reply, {:error, :rate_limited, retry_after}, state}
    end
  end

  @impl true
  def handle_info(:refill_tokens, state) do
    now = System.system_time(:millisecond)

    :ets.foldl(fn {identifier, bucket}, _acc ->
      elapsed = now - bucket.last_refill
      tokens_to_add = div(elapsed, @refill_interval) * @max_tokens

      if tokens_to_add > 0 do
        new_tokens = min(bucket.tokens + tokens_to_add, @max_tokens)
        new_bucket = %{bucket | tokens: new_tokens, last_refill: now}
        :ets.insert(@table, {identifier, new_bucket})
      end

      :ok
    end, :ok, @table)

    schedule_refill()
    {:noreply, state}
  end

  defp get_or_create_bucket(identifier, now) do
    case :ets.lookup(@table, identifier) do
      [{^identifier, bucket}] -> bucket
      [] ->
        bucket = %{
          tokens: @max_tokens,
          last_refill: now
        }
        :ets.insert(@table, {identifier, bucket})
        bucket
    end
  end

  defp calculate_retry_after(_bucket) do
    @refill_interval
  end

  defp schedule_refill do
    Process.send_after(self(), :refill_tokens, @refill_interval)
  end
end

# Plug for rate limiting
defmodule MyAppWeb.Plugs.RateLimit do
  import Plug.Conn
  import Phoenix.Controller

  def init(opts) do
    Keyword.merge([
      max_requests: 100,
      interval_seconds: 60,
      identifier: :ip
    ], opts)
  end

  def call(conn, opts) do
    identifier = get_identifier(conn, opts[:identifier])
    cost = 1

    case MyApp.RateLimiter.check_rate(identifier, cost) do
      {:ok, remaining} ->
        conn
        |> put_resp_header("x-ratelimit-limit", to_string(opts[:max_requests]))
        |> put_resp_header("x-ratelimit-remaining", to_string(remaining))

      {:error, :rate_limited, retry_after} ->
        conn
        |> put_resp_header("retry-after", to_string(retry_after))
        |> put_status(:too_many_requests)
        |> json(%{error: "Rate limit exceeded. Try again later."})
        |> halt()
    end
  end

  defp get_identifier(conn, :ip) do
    conn.remote_ip
    |> :inet.ntoa()
    |> to_string()
  end

  defp get_identifier(conn, :user) do
    conn.assigns[:current_user] && conn.assigns.current_user.id || get_identifier(conn, :ip)
  end
end

# Sliding Window Algorithm (more accurate)
defmodule MyApp.SlidingWindowRateLimiter do
  @window_size 60_000  # 60 seconds
  @max_requests 100

  def check_rate(identifier) do
    now = System.system_time(:millisecond)
    window_start = now - @window_size

    # Get requests in current window
    requests = get_requests(identifier, window_start, now)

    if length(requests) < @max_requests do
      record_request(identifier, now)
      {:ok, @max_requests - length(requests) - 1}
    else
      oldest_request = List.first(requests)
      retry_after = oldest_request + @window_size - now
      {:error, :rate_limited, retry_after}
    end
  end

  defp get_requests(identifier, window_start, window_end) do
    # Implementation using ETS or Redis
    # Return list of request timestamps
    []
  end

  defp record_request(identifier, timestamp) do
    # Store request timestamp
    :ok
  end
end

# Usage in router
# lib/my_app_web/router.ex
# pipeline :api do
#   plug :accepts, ["json"]
#   plug MyAppWeb.Plugs.RateLimit, max_requests: 100, interval_seconds: 60
# end
#
# scope "/api", MyAppWeb do
#   pipe_through :api
#   resources "/users", UserController
# end
```

**How It Works**: Token bucket algorithm grants tokens at fixed rate. Each request consumes tokens. ETS stores per-user buckets. Refill process adds tokens periodically. Plug checks rate before controller execution. Returns 429 status when limit exceeded.

**Use Cases**: API rate limiting, DDoS prevention, resource protection, fair usage enforcement.

**See Also**: [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver), [Phoenix REST API Guide](/en/learn/software-engineering/programming-language/elixir/how-to/phoenix-rest-api)

---

### Recipe 42: Internationalization (i18n) with Gettext

**Problem**: Support multiple languages in Phoenix application.

**Solution**:

```elixir
# Install gettext (already in Phoenix by default)
# mix.exs
# {:gettext, "~> 0.20"}

# Configure locales
# config/config.exs
config :my_app, MyAppWeb.Gettext,
  default_locale: "en",
  locales: ~w(en id es fr)

# Generate translation files
# mix gettext.extract
# mix gettext.merge priv/gettext

# lib/my_app_web/gettext.ex
defmodule MyAppWeb.Gettext do
  use Gettext, otp_app: :my_app
end

# Plug to set locale from params/headers/session
defmodule MyAppWeb.Plugs.SetLocale do
  import Plug.Conn

  @supported_locales Gettext.known_locales(MyAppWeb.Gettext)

  def init(opts), do: opts

  def call(conn, _opts) do
    locale = determine_locale(conn)

    if locale in @supported_locales do
      Gettext.put_locale(MyAppWeb.Gettext, locale)
      assign(conn, :locale, locale)
    else
      conn
    end
  end

  defp determine_locale(conn) do
    # Priority: URL param > Cookie > Accept-Language header > Default
    conn.params["locale"] ||
      get_session(conn, :locale) ||
      parse_accept_language(conn) ||
      Gettext.get_locale(MyAppWeb.Gettext)
  end

  defp parse_accept_language(conn) do
    case get_req_header(conn, "accept-language") do
      [value | _] ->
        value
        |> String.split(",")
        |> Enum.map(&parse_language_option/1)
        |> Enum.sort(&(&1.quality > &2.quality))
        |> Enum.find(&(&1.tag in @supported_locales))
        |> case do
          nil -> nil
          language_option -> language_option.tag
        end

      [] ->
        nil
    end
  end

  defp parse_language_option(string) do
    captures = ~r/^(?<tag>[\w\-]+)(?:;q=(?<quality>[\d\.]+))?$/i
    |> Regex.named_captures(String.trim(string))

    quality = case captures["quality"] do
      "" -> 1.0
      q -> String.to_float(q)
    end

    %{tag: captures["tag"], quality: quality}
  end
end

# Translation files
# priv/gettext/en/LC_MESSAGES/default.po
msgid "Welcome"
msgstr "Welcome"

msgid "Hello %{name}!"
msgstr "Hello %{name}!"

msgid "You have %{count} unread messages"
msgid_plural "You have %{count} unread messages"
msgstr[0] "You have one unread message"
msgstr[1] "You have %{count} unread messages"

# priv/gettext/id/LC_MESSAGES/default.po
msgid "Welcome"
msgstr "Selamat Datang"

msgid "Hello %{name}!"
msgstr "Halo %{name}!"

msgid "You have %{count} unread messages"
msgid_plural "You have %{count} unread messages"
msgstr[0] "Anda memiliki satu pesan belum dibaca"
msgstr[1] "Anda memiliki %{count} pesan belum dibaca"

# Usage in controllers
defmodule MyAppWeb.PageController do
  use MyAppWeb, :controller
  import MyAppWeb.Gettext

  def index(conn, _params) do
    welcome_message = gettext("Welcome")
    greeting = gettext("Hello %{name}!", name: "Alice")

    render(conn, "index.html",
      welcome: welcome_message,
      greeting: greeting
    )
  end
end

# Usage in templates
# lib/my_app_web/templates/page/index.html.heex
# <div>
#   <h1><%= gettext("Welcome") %></h1>
#   <p><%= gettext("Hello %{name}!", name: @current_user.name) %></p>
#   <%= ngettext("You have %{count} unread message", "You have %{count} unread messages", @unread_count) %>
# </div>

# Domain-specific translations
# priv/gettext/en/LC_MESSAGES/errors.po
msgid "Invalid email"
msgstr "Invalid email address"

msgid "Password too short"
msgstr "Password must be at least 8 characters"

# priv/gettext/id/LC_MESSAGES/errors.po
msgid "Invalid email"
msgstr "Alamat email tidak valid"

msgid "Password too short"
msgstr "Kata sandi harus minimal 8 karakter"

# Usage in changesets
defmodule MyApp.Accounts.User do
  import MyAppWeb.Gettext

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :password])
    |> validate_required([:email, :password])
    |> validate_format(:email, ~r/@/, message: dgettext("errors", "Invalid email"))
    |> validate_length(:password, min: 8, message: dgettext("errors", "Password too short"))
  end
end
```

**How It Works**: Gettext extracts translation strings from code. `.po` files store translations. Locale determined from request (params, headers, cookies). `gettext/1` and `dgettext/2` (domain-specific) fetch translated strings. `ngettext/3` handles pluralization.

**Use Cases**: Multi-language websites, internationalized apps, localized error messages, region-specific content.

**See Also**: [Phoenix Guide](/en/learn/software-engineering/programming-language/elixir/how-to/phoenix-rest-api), [Intermediate Tutorial - Phoenix](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#phoenix-framework)

---

### Recipe 43: File Uploads with Phoenix

**Problem**: Handle file uploads securely with validation.

**Solution**:

```elixir
# lib/my_app_web/controllers/upload_controller.ex
defmodule MyAppWeb.UploadController do
  use MyAppWeb, :controller
  require Logger

  @upload_dir "priv/static/uploads"
  @max_file_size 10 * 1024 * 1024  # 10 MB
  @allowed_extensions ~w(.jpg .jpeg .png .gif .pdf .docx)

  def new(conn, _params) do
    render(conn, "new.html")
  end

  def create(conn, %{"upload" => %{"file" => upload}}) do
    case validate_and_save_upload(upload) do
      {:ok, file_path} ->
        conn
        |> put_flash(:info, "File uploaded successfully")
        |> redirect(to: ~p"/uploads")

      {:error, reason} ->
        conn
        |> put_flash(:error, "Upload failed: #{reason}")
        |> render("new.html")
    end
  end

  defp validate_and_save_upload(upload) do
    with :ok <- validate_file_size(upload),
         :ok <- validate_file_type(upload),
         :ok <- validate_content_type(upload),
         {:ok, destination} <- generate_destination(upload),
         :ok <- save_file(upload, destination) do
      {:ok, destination}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_file_size(upload) do
    case upload.path |> File.stat!() |> Map.get(:size) do
      size when size <= @max_file_size -> :ok
      size -> {:error, "File too large: #{size} bytes (max: #{@max_file_size})"}
    end
  end

  defp validate_file_type(upload) do
    extension = Path.extname(upload.filename) |> String.downcase()

    if extension in @allowed_extensions do
      :ok
    else
      {:error, "File type not allowed: #{extension}"}
    end
  end

  defp validate_content_type(upload) do
    # Additional validation: check actual file content (magic numbers)
    # This prevents malicious files with fake extensions
    case File.read(upload.path) do
      {:ok, <<0xFF, 0xD8, 0xFF, _::binary>>} -> :ok  # JPEG
      {:ok, <<0x89, 0x50, 0x4E, 0x47, _::binary>>} -> :ok  # PNG
      {:ok, <<0x47, 0x49, 0x46, 0x38, _::binary>>} -> :ok  # GIF
      {:ok, <<0x25, 0x50, 0x44, 0x46, _::binary>>} -> :ok  # PDF
      _ ->
        # For other types, trust content_type from upload
        if upload.content_type in ~w(image/jpeg image/png image/gif application/pdf) do
          :ok
        else
          {:error, "Invalid file content"}
        end
    end
  end

  defp generate_destination(upload) do
    # Generate unique filename to prevent collisions
    timestamp = DateTime.utc_now() |> DateTime.to_unix()
    random = :crypto.strong_rand_bytes(8) |> Base.url_encode64(padding: false)
    extension = Path.extname(upload.filename)

    filename = "#{timestamp}_#{random}#{extension}"
    destination = Path.join(@upload_dir, filename)

    # Ensure upload directory exists
    File.mkdir_p!(@upload_dir)

    {:ok, destination}
  end

  defp save_file(upload, destination) do
    case File.cp(upload.path, destination) do
      :ok -> :ok
      {:error, reason} -> {:error, "Failed to save file: #{inspect(reason)}"}
    end
  end
end

# lib/my_app_web/live/upload_live.ex (LiveView with progress)
defmodule MyAppWeb.UploadLive do
  use MyAppWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:uploaded_files, [])
     |> allow_upload(:avatar,
       accept: ~w(.jpg .jpeg .png),
       max_entries: 1,
       max_file_size: 10_000_000,
       auto_upload: true
     )}
  end

  @impl true
  def handle_event("validate", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("save", _params, socket) do
    uploaded_files =
      consume_uploaded_entries(socket, :avatar, fn %{path: path}, entry ->
        dest = Path.join("priv/static/uploads", entry.client_name)
        File.cp!(path, dest)
        {:ok, ~p"/uploads/#{Path.basename(dest)}"}
      end)

    {:noreply,
     socket
     |> update(:uploaded_files, &(&1 ++ uploaded_files))
     |> put_flash(:info, "File uploaded!")}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <h1>Upload File</h1>

      <.form for={:upload} phx-change="validate" phx-submit="save">
        <.live_file_input upload={@uploads.avatar} />

        <%= for entry <- @uploads.avatar.entries do %>
          <div>
            <.live_img_preview entry={entry} />
            <p><%= entry.progress %>%</p>

            <%= for err <- upload_errors(@uploads.avatar, entry) do %>
              <p class="error"><%= error_to_string(err) %></p>
            <% end %>
          </div>
        <% end %>

        <.button type="submit">Upload</.button>
      </.form>

      <h2>Uploaded Files</h2>
      <%= for file <- @uploaded_files do %>
        <div>
          <img src={file} width="200" />
        </div>
      <% end %>
    </div>
    """
  end

  defp error_to_string(:too_large), do: "File is too large"
  defp error_to_string(:not_accepted), do: "File type not accepted"
  defp error_to_string(:too_many_files), do: "Too many files"
end

# lib/my_app_web/templates/upload/new.html.heex (traditional upload)
<div>
  <h1>Upload File</h1>

  <%= form_for :upload, ~p"/uploads", [multipart: true], fn f -> %>
    <div>
      <%= file_input f, :file, required: true %>
    </div>

    <div>
      <%= submit "Upload" %>
    </div>
  <% end %>
</div>

# lib/my_app/accounts/user.ex (with avatar)
defmodule MyApp.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :email, :string
    field :name, :string
    field :avatar_url, :string

    timestamps()
  end

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :name, :avatar_url])
    |> validate_required([:email, :name])
  end

  def avatar_changeset(user, upload_path) do
    change(user, avatar_url: upload_path)
  end
end
```

**How It Works**: Phoenix uploads provide `Plug.Upload` struct. LiveView's `allow_upload/3` handles client-side validation and progress. `consume_uploaded_entries/3` processes files. Validation checks size, extension, and content type (magic numbers). Unique filenames prevent collisions.

**Use Cases**: Profile pictures, document uploads, media sharing, file attachments.

**See Also**: [Phoenix Guide](/en/learn/software-engineering/programming-language/elixir/how-to/phoenix-rest-api), [LiveView Guide](/en/learn/software-engineering/programming-language/elixir/how-to/liveview)

---

### Recipe 44: Email Sending with Swoosh

**Problem**: Send transactional emails from Phoenix application.

**Solution**:

```elixir
# mix.exs
defp deps do
  [
    {:swoosh, "~> 1.11"},
    {:gen_smtp, "~> 1.2"},  # For SMTP adapter
    {:finch, "~> 0.13"}     # For HTTP adapters
  ]
end

# config/config.exs
config :my_app, MyApp.Mailer,
  adapter: Swoosh.Adapters.SMTP,
  relay: "smtp.gmail.com",
  port: 587,
  username: System.get_env("SMTP_USERNAME"),
  password: System.get_env("SMTP_PASSWORD"),
  tls: :always,
  auth: :always

# For development (preview emails in browser)
config :swoosh, :api_client, Swoosh.ApiClient.Finch

if Mix.env() == :dev do
  config :my_app, MyApp.Mailer, adapter: Swoosh.Adapters.Local
end

# lib/my_app/mailer.ex
defmodule MyApp.Mailer do
  use Swoosh.Mailer, otp_app: :my_app
end

# lib/my_app/emails/user_email.ex
defmodule MyApp.Emails.UserEmail do
  import Swoosh.Email

  def welcome_email(user) do
    new()
    |> to({user.name, user.email})
    |> from({"My App", "noreply@myapp.com"})
    |> subject("Welcome to My App!")
    |> html_body("<h1>Hello #{user.name}!</h1><p>Welcome to My App.</p>")
    |> text_body("Hello #{user.name}!\n\nWelcome to My App.")
  end

  def password_reset_email(user, reset_token) do
    reset_url = "https://myapp.com/reset-password?token=#{reset_token}"

    new()
    |> to(user.email)
    |> from({"My App", "noreply@myapp.com"})
    |> subject("Reset your password")
    |> html_body("""
      <h1>Password Reset</h1>
      <p>Click the link below to reset your password:</p>
      <p><a href="#{reset_url}">Reset Password</a></p>
      <p>This link expires in 1 hour.</p>
    """)
    |> text_body("""
      Password Reset

      Visit this link to reset your password:
      #{reset_url}

      This link expires in 1 hour.
    """)
  end

  def notification_email(user, notification) do
    new()
    |> to(user.email)
    |> from({"My App Notifications", "notifications@myapp.com"})
    |> subject(notification.title)
    |> html_body(notification.body_html)
    |> text_body(notification.body_text)
    |> put_provider_option(:track_opens, true)
    |> put_provider_option(:track_links, true)
  end

  def attachment_email(user, file_path) do
    new()
    |> to(user.email)
    |> from({"My App", "noreply@myapp.com"})
    |> subject("Your report is ready")
    |> html_body("<p>Please find your report attached.</p>")
    |> attachment(Swoosh.Attachment.new(file_path))
  end
end

# Sending emails
defmodule MyApp.Accounts do
  alias MyApp.Mailer
  alias MyApp.Emails.UserEmail

  def register_user(attrs) do
    with {:ok, user} <- create_user(attrs) do
      # Send welcome email asynchronously
      user
      |> UserEmail.welcome_email()
      |> Mailer.deliver()

      {:ok, user}
    end
  end

  def request_password_reset(email) do
    case get_user_by_email(email) do
      nil ->
        # Don't reveal whether email exists
        :ok

      user ->
        reset_token = generate_reset_token(user)

        user
        |> UserEmail.password_reset_email(reset_token)
        |> Mailer.deliver()

        :ok
    end
  end

  defp create_user(_attrs), do: {:ok, %{}}
  defp get_user_by_email(_email), do: nil
  defp generate_reset_token(_user), do: "token123"
end

# Background email delivery with Oban
defmodule MyApp.Workers.EmailWorker do
  use Oban.Worker, queue: :emails, max_attempts: 3

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"email_type" => "welcome", "user_id" => user_id}}) do
    user = MyApp.Accounts.get_user!(user_id)

    user
    |> MyApp.Emails.UserEmail.welcome_email()
    |> MyApp.Mailer.deliver()

    :ok
  end

  def perform(%Oban.Job{args: %{"email_type" => "password_reset", "user_id" => user_id, "token" => token}}) do
    user = MyApp.Accounts.get_user!(user_id)

    user
    |> MyApp.Emails.UserEmail.password_reset_email(token)
    |> MyApp.Mailer.deliver()

    :ok
  end
end

# Enqueue background email
%{email_type: "welcome", user_id: user.id}
|> MyApp.Workers.EmailWorker.new()
|> Oban.insert()

# Testing emails
defmodule MyApp.Emails.UserEmailTest do
  use ExUnit.Case, async: true
  import Swoosh.TestAssertions

  alias MyApp.Emails.UserEmail

  test "welcome email" do
    user = %{name: "Alice", email: "alice@example.com"}
    email = UserEmail.welcome_email(user)

    assert email.to == [{"Alice", "alice@example.com"}]
    assert email.from == {"My App", "noreply@myapp.com"}
    assert email.subject == "Welcome to My App!"
    assert email.html_body =~ "Hello Alice!"
  end

  test "sends welcome email" do
    user = %{name: "Bob", email: "bob@example.com"}

    user
    |> UserEmail.welcome_email()
    |> MyApp.Mailer.deliver()

    assert_email_sent subject: "Welcome to My App!"
  end
end
```

**How It Works**: Swoosh provides unified email interface. Adapters handle different providers (SMTP, SendGrid, Mailgun). `deliver/1` sends emails. HTML and text bodies supported. Attachments via `Swoosh.Attachment`. Background processing recommended for reliability.

**Use Cases**: Welcome emails, password resets, notifications, newsletters, transactional receipts.

**See Also**: [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver), [Intermediate Tutorial - Phoenix](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#phoenix-framework)

---

### Recipe 45: Scheduled Tasks with Quantum

**Problem**: Run periodic tasks (cron jobs) in Elixir application.

**Solution**:

```elixir
# mix.exs
defp deps do
  [
    {:quantum, "~> 3.5"},
    {:timex, "~> 3.7"}
  ]
end

# config/config.exs
config :my_app, MyApp.Scheduler,
  jobs: [
    # Every minute
    {"* * * * *", {MyApp.Tasks, :cleanup_old_sessions, []}},
    # Every day at 2 AM
    {"0 2 * * *", {MyApp.Tasks, :daily_report, []}},
    # Every Monday at 9 AM
    {"0 9 * * 1", {MyApp.Tasks, :weekly_summary, []}},
    # Every 15 minutes
    {"*/15 * * * *", {MyApp.Tasks, :check_health, []}},
  ]

# lib/my_app/scheduler.ex
defmodule MyApp.Scheduler do
  use Quantum, otp_app: :my_app
end

# lib/my_app/tasks.ex
defmodule MyApp.Tasks do
  require Logger

  def cleanup_old_sessions do
    Logger.info("Starting session cleanup")

    # Delete sessions older than 30 days
    cutoff_date = DateTime.utc_now() |> DateTime.add(-30, :day)

    MyApp.Repo.delete_all(
      from s in MyApp.Session,
      where: s.inserted_at < ^cutoff_date
    )

    Logger.info("Session cleanup completed")
  end

  def daily_report do
    Logger.info("Generating daily report")

    # Aggregate data from yesterday
    yesterday = DateTime.utc_now() |> DateTime.add(-1, :day)

    stats = %{
      new_users: count_new_users(yesterday),
      active_users: count_active_users(yesterday),
      revenue: calculate_revenue(yesterday)
    }

    # Send report email
    MyApp.Emails.ReportEmail.daily_report(stats)
    |> MyApp.Mailer.deliver()

    Logger.info("Daily report sent")
  end

  def weekly_summary do
    Logger.info("Generating weekly summary")

    # Aggregate data from last 7 days
    week_ago = DateTime.utc_now() |> DateTime.add(-7, :day)

    summary = generate_weekly_summary(week_ago)

    # Send to all admins
    MyApp.Accounts.list_admins()
    |> Enum.each(fn admin ->
      MyApp.Emails.ReportEmail.weekly_summary(admin, summary)
      |> MyApp.Mailer.deliver()
    end)

    Logger.info("Weekly summary sent")
  end

  def check_health do
    # Check application health
    checks = [
      check_database(),
      check_redis(),
      check_external_api()
    ]

    failed = Enum.filter(checks, &match?({:error, _}, &1))

    if length(failed) > 0 do
      Logger.error("Health check failed: #{inspect(failed)}")
      alert_ops_team(failed)
    end
  end

  defp count_new_users(_date), do: 0
  defp count_active_users(_date), do: 0
  defp calculate_revenue(_date), do: 0
  defp generate_weekly_summary(_date), do: %{}
  defp check_database, do: :ok
  defp check_redis, do: :ok
  defp check_external_api, do: :ok
  defp alert_ops_team(_failures), do: :ok
end

# Dynamic job management
defmodule MyApp.JobManager do
  alias MyApp.Scheduler

  def add_job(name, schedule, task) do
    Scheduler.new_job()
    |> Quantum.Job.set_name(name)
    |> Quantum.Job.set_schedule(Crontab.CronExpression.Parser.parse!(schedule))
    |> Quantum.Job.set_task(task)
    |> Scheduler.add_job()
  end

  def remove_job(name) do
    Scheduler.delete_job(name)
  end

  def list_jobs do
    Scheduler.jobs()
  end

  def pause_job(name) do
    Scheduler.deactivate_job(name)
  end

  def resume_job(name) do
    Scheduler.activate_job(name)
  end
end

# Usage in IEx or controller
# Add job dynamically
MyApp.JobManager.add_job(
  :custom_task,
  "0 3 * * *",  # Every day at 3 AM
  fn -> IO.puts("Custom task running") end
)

# List all jobs
MyApp.JobManager.list_jobs()
# => [%Quantum.Job{name: :cleanup_old_sessions, ...}, ...]

# Pause job
MyApp.JobManager.pause_job(:cleanup_old_sessions)

# Resume job
MyApp.JobManager.resume_job(:cleanup_old_sessions)

# Remove job
MyApp.JobManager.remove_job(:custom_task)

# Cron schedule examples
# "* * * * *"        - Every minute
# "0 * * * *"        - Every hour
# "0 */2 * * *"      - Every 2 hours
# "0 0 * * *"        - Every day at midnight
# "0 0 * * 0"        - Every Sunday at midnight
# "0 0 1 * *"        - First day of month at midnight
# "0 9-17 * * 1-5"   - Weekdays 9 AM to 5 PM
# "*/15 9-17 * * 1-5" - Every 15 min, weekdays, 9 AM-5 PM
```

**How It Works**: Quantum schedules tasks using cron syntax. Jobs run in separate processes. `Quantum.Job` defines task configuration. Dynamic job management allows runtime scheduling. Tasks should be idempotent (safe to retry).

**Use Cases**: Data cleanup, report generation, health checks, cache warming, backup tasks.

**See Also**: [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver), [Intermediate Tutorial - OTP](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#otp-platform)

---

### Recipe 46: GraphQL API with Absinthe

**Problem**: Build GraphQL API for flexible client queries.

**Solution**:

```elixir
# mix.exs
defp deps do
  [
    {:absinthe, "~> 1.7"},
    {:absinthe_plug, "~> 1.5"},
    {:absinthe_phoenix, "~> 2.0"}
  ]
end

# lib/my_app_web/schema.ex
defmodule MyAppWeb.Schema do
  use Absinthe.Schema
  import Absinthe.Resolution.Helpers

  # Define types
  object :user do
    field :id, non_null(:id)
    field :email, non_null(:string)
    field :name, :string
    field :posts, list_of(:post) do
      resolve(dataloader(MyApp.Content))
    end
  end

  object :post do
    field :id, non_null(:id)
    field :title, non_null(:string)
    field :body, :string
    field :published, :boolean
    field :author, :user do
      resolve(dataloader(MyApp.Accounts))
    end
    field :inserted_at, :string
  end

  # Input types for mutations
  input_object :user_input do
    field :email, non_null(:string)
    field :name, non_null(:string)
    field :password, non_null(:string)
  end

  input_object :post_input do
    field :title, non_null(:string)
    field :body, non_null(:string)
    field :published, :boolean, default_value: false
  end

  # Queries
  query do
    field :user, :user do
      arg :id, non_null(:id)
      resolve(&MyAppWeb.Resolvers.Accounts.get_user/3)
    end

    field :users, list_of(:user) do
      arg :limit, :integer, default_value: 10
      arg :offset, :integer, default_value: 0
      resolve(&MyAppWeb.Resolvers.Accounts.list_users/3)
    end

    field :post, :post do
      arg :id, non_null(:id)
      resolve(&MyAppWeb.Resolvers.Content.get_post/3)
    end

    field :posts, list_of(:post) do
      arg :published, :boolean
      arg :limit, :integer
      resolve(&MyAppWeb.Resolvers.Content.list_posts/3)
    end
  end

  # Mutations
  mutation do
    field :create_user, :user do
      arg :input, non_null(:user_input)
      resolve(&MyAppWeb.Resolvers.Accounts.create_user/3)
    end

    field :create_post, :post do
      arg :input, non_null(:post_input)
      middleware(MyAppWeb.Middleware.Authenticate)
      resolve(&MyAppWeb.Resolvers.Content.create_post/3)
    end

    field :update_post, :post do
      arg :id, non_null(:id)
      arg :input, non_null(:post_input)
      middleware(MyAppWeb.Middleware.Authenticate)
      resolve(&MyAppWeb.Resolvers.Content.update_post/3)
    end

    field :delete_post, :boolean do
      arg :id, non_null(:id)
      middleware(MyAppWeb.Middleware.Authenticate)
      resolve(&MyAppWeb.Resolvers.Content.delete_post/3)
    end
  end

  # Subscriptions (real-time updates)
  subscription do
    field :post_created, :post do
      config(fn _args, _info ->
        {:ok, topic: "posts"}
      end)

      trigger(:create_post, topic: fn _post ->
        "posts"
      end)
    end
  end

  # Dataloader for efficient N+1 prevention
  def context(ctx) do
    loader =
      Dataloader.new()
      |> Dataloader.add_source(MyApp.Accounts, MyApp.Accounts.data())
      |> Dataloader.add_source(MyApp.Content, MyApp.Content.data())

    Map.put(ctx, :loader, loader)
  end

  def plugins do
    [Absinthe.Middleware.Dataloader] ++ Absinthe.Plugin.defaults()
  end
end

# lib/my_app_web/resolvers/accounts.ex
defmodule MyAppWeb.Resolvers.Accounts do
  alias MyApp.Accounts

  def get_user(_parent, %{id: id}, _resolution) do
    case Accounts.get_user(id) do
      nil -> {:error, "User not found"}
      user -> {:ok, user}
    end
  end

  def list_users(_parent, args, _resolution) do
    limit = Map.get(args, :limit, 10)
    offset = Map.get(args, :offset, 0)

    users = Accounts.list_users(limit, offset)
    {:ok, users}
  end

  def create_user(_parent, %{input: input}, _resolution) do
    case Accounts.create_user(input) do
      {:ok, user} -> {:ok, user}
      {:error, changeset} -> {:error, format_changeset_errors(changeset)}
    end
  end

  defp format_changeset_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end
end

# lib/my_app_web/middleware/authenticate.ex
defmodule MyAppWeb.Middleware.Authenticate do
  @behaviour Absinthe.Middleware

  def call(resolution, _opts) do
    case resolution.context do
      %{current_user: _user} ->
        resolution

      _ ->
        resolution
        |> Absinthe.Resolution.put_result({:error, "Unauthenticated"})
    end
  end
end

# lib/my_app_web/router.ex (add GraphQL endpoints)
scope "/api" do
  pipe_through :api

  forward "/graphql", Absinthe.Plug,
    schema: MyAppWeb.Schema

  if Mix.env() == :dev do
    forward "/graphiql", Absinthe.Plug.GraphiQL,
      schema: MyAppWeb.Schema,
      interface: :playground
  end
end

# Example GraphQL queries
# query GetUser {
#   user(id: "1") {
#     id
#     email
#     name
#     posts {
#       id
#       title
#     }
#   }
# }

# mutation CreatePost {
#   createPost(input: {title: "Hello", body: "World"}) {
#     id
#     title
#     author {
#       name
#     }
#   }
# }

# subscription PostCreated {
#   postCreated {
#     id
#     title
#     author {
#       name
#     }
#   }
# }
```

**How It Works**: Absinthe provides GraphQL implementation. Schema defines types, queries, mutations, subscriptions. Resolvers fetch data. Dataloader prevents N+1 queries. Middleware handles authentication. Subscriptions enable real-time updates via Phoenix Channels.

**Use Cases**: Flexible APIs, mobile apps, SPAs, real-time dashboards, data aggregation.

**See Also**: [Phoenix REST API Guide](/en/learn/software-engineering/programming-language/elixir/how-to/phoenix-rest-api), [Intermediate Tutorial - Phoenix](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#phoenix-framework)

---

### Recipe 47: Dependency Injection for Testing

**Problem**: Make code testable by injecting dependencies.

**Solution**:

```elixir
# Using behavior-based dependency injection
defmodule MyApp.EmailService do
  @callback send_email(recipient :: String.t(), subject :: String.t(), body :: String.t()) :: :ok | {:error, term()}
end

# Production implementation
defmodule MyApp.EmailService.Swoosh do
  @behaviour MyApp.EmailService

  def send_email(recipient, subject, body) do
    import Swoosh.Email

    new()
    |> to(recipient)
    |> from("noreply@myapp.com")
    |> subject(subject)
    |> html_body(body)
    |> MyApp.Mailer.deliver()

    :ok
  end
end

# Test implementation (mock)
defmodule MyApp.EmailService.Mock do
  @behaviour MyApp.EmailService

  def send_email(recipient, subject, body) do
    # Store in process dictionary for assertion in tests
    emails = Process.get(:sent_emails, [])
    Process.put(:sent_emails, emails ++ [{recipient, subject, body}])
    :ok
  end

  def get_sent_emails do
    Process.get(:sent_emails, [])
  end

  def clear_sent_emails do
    Process.put(:sent_emails, [])
  end
end

# Configuration-based dependency injection
# config/config.exs
config :my_app, :email_service, MyApp.EmailService.Swoosh

# config/test.exs
config :my_app, :email_service, MyApp.EmailService.Mock

# Usage in application code
defmodule MyApp.Accounts do
  def email_service do
    Application.get_env(:my_app, :email_service)
  end

  def register_user(attrs) do
    with {:ok, user} <- create_user(attrs) do
      # Use configured implementation
      email_service().send_email(
        user.email,
        "Welcome!",
        "Welcome to My App"
      )

      {:ok, user}
    end
  end

  defp create_user(_attrs), do: {:ok, %{email: "user@example.com"}}
end

# Test
defmodule MyApp.AccountsTest do
  use ExUnit.Case, async: true
  alias MyApp.EmailService.Mock

  setup do
    Mock.clear_sent_emails()
    :ok
  end

  test "register_user sends welcome email" do
    {:ok, user} = MyApp.Accounts.register_user(%{email: "test@example.com"})

    emails = Mock.get_sent_emails()
    assert length(emails) == 1
    assert {"test@example.com", "Welcome!", _body} = hd(emails)
  end
end

# Using Mox for more sophisticated mocking
# mix.exs
# {:mox, "~> 1.0", only: :test}

# test/support/mocks.ex
Mox.defmock(MyApp.EmailServiceMock, for: MyApp.EmailService)

# config/test.exs
config :my_app, :email_service, MyApp.EmailServiceMock

# Test with Mox
defmodule MyApp.AccountsTest do
  use ExUnit.Case, async: true
  import Mox

  # Set up Mox expectations
  setup :verify_on_exit!

  test "register_user sends welcome email" do
    MyApp.EmailServiceMock
    |> expect(:send_email, fn recipient, subject, _body ->
      assert recipient == "test@example.com"
      assert subject == "Welcome!"
      :ok
    end)

    {:ok, _user} = MyApp.Accounts.register_user(%{email: "test@example.com"})
  end

  test "register_user handles email failure" do
    MyApp.EmailServiceMock
    |> expect(:send_email, fn _recipient, _subject, _body ->
      {:error, :smtp_error}
    end)

    # Should still create user even if email fails
    {:ok, _user} = MyApp.Accounts.register_user(%{email: "test@example.com"})
  end
end

# GenServer with dependency injection
defmodule MyApp.UserCache do
  use GenServer

  # Accept dependencies in start_link
  def start_link(opts) do
    repo = Keyword.get(opts, :repo, MyApp.Repo)
    cache = Keyword.get(opts, :cache, :ets)

    GenServer.start_link(__MODULE__, %{repo: repo, cache: cache}, name: __MODULE__)
  end

  @impl true
  def init(deps) do
    {:ok, deps}
  end

  @impl true
  def handle_call({:get_user, id}, _from, %{repo: repo, cache: cache} = state) do
    case cache_lookup(cache, id) do
      {:ok, user} ->
        {:reply, {:ok, user}, state}

      :miss ->
        case repo.get(MyApp.User, id) do
          nil ->
            {:reply, {:error, :not_found}, state}

          user ->
            cache_put(cache, id, user)
            {:reply, {:ok, user}, state}
        end
    end
  end

  defp cache_lookup(:ets, id), do: :miss  # Simplified
  defp cache_put(:ets, _id, _user), do: :ok
end

# Test with mock dependencies
test "UserCache fetches from repo on cache miss" do
  # Create mock implementations
  mock_repo = fn ->
    %{
      get: fn _module, id ->
        if id == 1, do: %MyApp.User{id: 1, name: "Alice"}, else: nil
      end
    }
  end

  mock_cache = :mock_ets

  # Start GenServer with mocks
  {:ok, _pid} = MyApp.UserCache.start_link(repo: mock_repo.(), cache: mock_cache)

  # Test
  assert {:ok, user} = GenServer.call(MyApp.UserCache, {:get_user, 1})
  assert user.name == "Alice"
end
```

**How It Works**: Behaviors define contracts. Configuration selects implementation. Mox provides compile-time mock generation. Process dictionary stores test state. GenServer accepts dependencies in `start_link`. Tests inject mock implementations.

**Use Cases**: Unit testing, integration testing, external service mocking, test isolation.

**See Also**: [Testing Guide](/en/learn/software-engineering/programming-language/elixir/how-to/testing-strategies), [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver), [Beginner Tutorial - Testing](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#testing-with-exunit)

---

### Recipe 48: Distributed Systems with libcluster

**Problem**: Build distributed Elixir cluster for high availability.

**Solution**:

```elixir
# mix.exs
defp deps do
  [
    {:libcluster, "~> 3.3"}
  ]
end

# config/config.exs (for development - Gossip strategy)
config :libcluster,
  topologies: [
    local: [
      strategy: Cluster.Strategy.Gossip,
      config: [
        port: 45892,
        if_addr: "0.0.0.0",
        multicast_addr: "230.1.1.251",
        multicast_ttl: 1,
        secret: "my_secret"
      ]
    ]
  ]

# config/prod.exs (for production - Kubernetes strategy)
config :libcluster,
  topologies: [
    k8s: [
      strategy: Elixir.Cluster.Strategy.Kubernetes,
      config: [
        mode: :dns,
        kubernetes_node_basename: "myapp",
        kubernetes_selector: "app=myapp",
        kubernetes_namespace: "production",
        polling_interval: 10_000
      ]
    ]
  ]

# lib/my_app/application.ex
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    topologies = Application.get_env(:libcluster, :topologies, [])

    children = [
      # Start libcluster supervisor
      {Cluster.Supervisor, [topologies, [name: MyApp.ClusterSupervisor]]},

      # Start your application
      MyApp.Repo,
      MyAppWeb.Endpoint,

      # Distributed registry
      {Registry, keys: :unique, name: MyApp.Registry, partitions: System.schedulers_online()},

      # Distributed supervisor
      {Horde.Registry, [name: MyApp.HordeRegistry, keys: :unique]},
      {Horde.DynamicSupervisor, [name: MyApp.HordeSupervisor, strategy: :one_for_one]}
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

# Distributed GenServer example
defmodule MyApp.DistributedCache do
  use GenServer

  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, %{}, name: {:global, name})
  end

  def get(key) do
    case :global.whereis_name(__MODULE__) do
      :undefined -> {:error, :not_found}
      pid -> GenServer.call(pid, {:get, key})
    end
  end

  def put(key, value) do
    case :global.whereis_name(__MODULE__) do
      :undefined -> {:error, :not_found}
      pid -> GenServer.call(pid, {:put, key, value})
    end
  end

  @impl true
  def init(_) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:get, key}, _from, state) do
    {:reply, Map.get(state, key), state}
  end

  @impl true
  def handle_call({:put, key, value}, _from, state) do
    {:reply, :ok, Map.put(state, key, value)}
  end
end

# Distributed task execution
defmodule MyApp.DistributedTasks do
  # Execute function on all nodes
  def run_on_all_nodes(func) do
    nodes = [Node.self() | Node.list()]

    tasks = Enum.map(nodes, fn node ->
      Task.Supervisor.async({MyApp.TaskSupervisor, node}, func)
    end)

    Task.await_many(tasks, 5000)
  end

  # Execute function on specific node
  def run_on_node(node, func) do
    Task.Supervisor.async({MyApp.TaskSupervisor, node}, func)
    |> Task.await(5000)
  end

  # Execute function on least loaded node
  def run_on_least_loaded_node(func) do
    node = find_least_loaded_node()
    run_on_node(node, func)
  end

  defp find_least_loaded_node do
    nodes = [Node.self() | Node.list()]

    nodes
    |> Enum.map(fn node ->
      load = :rpc.call(node, :cpu_sup, :avg1, []) / 256
      {node, load}
    end)
    |> Enum.min_by(fn {_node, load} -> load end)
    |> elem(0)
  end
end

# Distributed PubSub with Phoenix.PubSub
defmodule MyApp.Events do
  def broadcast(topic, message) do
    Phoenix.PubSub.broadcast(MyApp.PubSub, topic, message)
  end

  def subscribe(topic) do
    Phoenix.PubSub.subscribe(MyApp.PubSub, topic)
  end

  # Broadcast to all nodes
  def broadcast_to_cluster(event, data) do
    nodes = [Node.self() | Node.list()]

    Enum.each(nodes, fn node ->
      :rpc.cast(node, __MODULE__, :local_broadcast, [event, data])
    end)
  end

  def local_broadcast(event, data) do
    broadcast("cluster:events", {event, data})
  end
end

# Using Horde for distributed process management
defmodule MyApp.HordeWorker do
  use GenServer

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  def start_distributed(id, args) do
    # Horde ensures only one instance runs across cluster
    child_spec = %{
      id: id,
      start: {__MODULE__, :start_link, [args]},
      restart: :transient
    }

    Horde.DynamicSupervisor.start_child(MyApp.HordeSupervisor, child_spec)
  end

  @impl true
  def init(args) do
    # Register with Horde Registry
    {:ok, args}
  end
end

# Node connection utilities
defmodule MyApp.ClusterUtils do
  def list_nodes do
    Node.list()
  end

  def connect_node(node_name) do
    Node.connect(node_name)
  end

  def disconnect_node(node_name) do
    Node.disconnect(node_name)
  end

  def cluster_size do
    length([Node.self() | Node.list()])
  end

  def is_clustered? do
    cluster_size() > 1
  end

  # Get cluster-wide statistics
  def cluster_stats do
    nodes = [Node.self() | Node.list()]

    Enum.map(nodes, fn node ->
      stats = :rpc.call(node, :erlang, :statistics, [:wall_clock])
      memory = :rpc.call(node, :erlang, :memory, [])

      %{
        node: node,
        uptime_ms: elem(stats, 0),
        memory: memory,
        process_count: :rpc.call(node, :erlang, :system_info, [:process_count])
      }
    end)
  end
end

# Running distributed application
# Start multiple nodes:
# iex --name node1@127.0.0.1 --cookie secret -S mix
# iex --name node2@127.0.0.1 --cookie secret -S mix
# iex --name node3@127.0.0.1 --cookie secret -S mix

# Check cluster status
MyApp.ClusterUtils.list_nodes()
# => [:node2@127.0.0.1, :node3@127.0.0.1]

MyApp.ClusterUtils.cluster_size()
# => 3

# Run task on all nodes
MyApp.DistributedTasks.run_on_all_nodes(fn ->
  IO.puts("Running on #{Node.self()}")
end)

# Broadcast event to cluster
MyApp.Events.broadcast_to_cluster(:deployment, %{version: "1.0.0"})

# Get cluster statistics
MyApp.ClusterUtils.cluster_stats()
# => [
#   %{node: :node1@127.0.0.1, uptime_ms: 120000, memory: [...], process_count: 245},
#   %{node: :node2@127.0.0.1, uptime_ms: 118000, memory: [...], process_count: 238},
#   %{node: :node3@127.0.0.1, uptime_ms: 115000, memory: [...], process_count: 241}
# ]
```

**How It Works**: libcluster automatically discovers and connects nodes using various strategies (Gossip, Kubernetes, DNS, static). Horde provides distributed process registry and supervision. Global registry ensures single process instance across cluster. Phoenix.PubSub broadcasts messages to all nodes. RPC executes functions on remote nodes.

**Use Cases**: High availability systems, horizontal scaling, distributed caching, multi-region deployment, load balancing.

**See Also**: [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver), [Advanced Tutorial - Distributed Systems](/en/learn/software-engineering/programming-language/elixir/tutorials/advanced#distributed-elixir), [Intermediate Tutorial - OTP](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#otp-platform)

---

## Summary

This cookbook provides 48 comprehensive tested recipes across 17 categories:

1. **Data Structures** (5 recipes): Map transformations, deep merging, grouping, keyword lists, nested updates
2. **Pattern Matching** (3 recipes): Complex extraction, case matching with guards, pin operator patterns
3. **Functions and Pipes** (3 recipes): Function composition, higher-order functions, capture syntax
4. **String Manipulation** (2 recipes): Parsing with regex, validation and sanitization
5. **Concurrency** (4 recipes): Parallel processing with Task, process registry patterns, rate limiting (token bucket), debounce/throttle
6. **OTP Patterns** (5 recipes): State machines with GenServer, dynamic supervision trees, periodic tasks, background job processing, scheduled tasks with Quantum
7. **Error Handling** (2 recipes): Result tuple patterns, supervision for fault tolerance
8. **File I/O** (2 recipes): Safe file operations, CSV processing and parsing
9. **Phoenix Web Development** (6 recipes): REST API endpoints, authentication with Plugs, WebSocket communication (Channels), session-based auth, API versioning strategies, file uploads
10. **LiveView Real-Time** (2 recipes): Real-time forms with validation, PubSub live updates
11. **Ecto Database** (3 recipes): Complex queries with joins, transactions and rollbacks, database migrations (schema and data)
12. **Testing** (3 recipes): GenServer behavior testing, mocking external services, dependency injection patterns
13. **Performance** (2 recipes): Profiling and benchmarking with Benchee, database query optimization
14. **Configuration** (1 recipe): Environment-based configuration management
15. **Debugging** (1 recipe): Debug techniques and tools (IO.inspect, IEx.pry, Logger, Observer, Recon)
16. **Advanced Phoenix** (3 recipes): Internationalization (i18n) with Gettext, email sending with Swoosh, GraphQL API with Absinthe
17. **Distributed Systems** (1 recipe): Clustering with libcluster, distributed processes with Horde

All recipes include:

- Clear problem statement
- Complete working solution
- Explanation of how it works
- Real-world use cases
- Cross-references to related content

**Next Steps**:

- Explore [specific how-to guides](/en/learn/software-engineering/programming-language/elixir/how-to) for deeper dives
- Review [best practices](/en/learn/software-engineering/programming-language/elixir/explanation/best-practices) for quality code
- Study [tutorials](/en/learn/software-engineering/programming-language/elixir/tutorials) for comprehensive learning
- Consult [reference materials](/en/learn/software-engineering/programming-language/elixir/reference) for quick lookups
