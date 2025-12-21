---
title: "Metaprogramming with Macros"
date: 2025-12-21T17:50:00+07:00
draft: false
description: "Write macros in Elixir for code generation, DSLs, and compile-time transformations using AST manipulation and quote/unquote."
weight: 1000014
tags: ["elixir", "macros", "metaprogramming", "ast", "dsl", "how-to"]
---

# Metaprogramming with Macros

**Need to generate code at compile time?** Elixir macros enable powerful metaprogramming with AST manipulation, allowing you to extend the language and create domain-specific languages.

## Prerequisites

- Strong understanding of Elixir syntax
- Familiarity with AST (Abstract Syntax Trees)
- Knowledge of quote and unquote
- Completed [Advanced Tutorial](/learn/swe/prog-lang/elixir/tutorials/advanced)

## Problem

You want to generate repetitive code, create DSLs, or perform compile-time transformations. Writing boilerplate manually is error-prone and hard to maintain.

**Challenges:**

- Understanding AST structure and manipulation
- Knowing when to use macros vs. functions
- Managing hygiene and variable scope
- Debugging macro-generated code
- Avoiding macro over-engineering

## Solution

Use **macros** to write code that writes code, transforming AST at compile time.

## How It Works

### 1. Basic Macro with quote/unquote

```elixir
defmodule MyMacros do
  defmacro say_hello(name) do
    quote do
      IO.puts("Hello, #{unquote(name)}!")
    end
  end
end

# Usage
require MyMacros
MyMacros.say_hello("World")  # Prints: Hello, World!
```

**How it works:**

1. `quote` captures the code as AST
2. `unquote` injects the value into the AST
3. The AST is injected into the caller's context at compile time

### 2. Understanding AST

```elixir
# View AST structure
quote do: 1 + 2
# => {:+, [context: Elixir, import: Kernel], [1, 2]}

quote do: foo(a, b)
# => {:foo, [], [{:a, [], Elixir}, {:b, [], Elixir}]}

# AST is just tuples and lists
{function_name, metadata, arguments}
```

### 3. Macro with Pattern Matching

```elixir
defmodule Assertions do
  defmacro assert({:==, _, [left, right]}) do
    quote do
      left_val = unquote(left)
      right_val = unquote(right)

      unless left_val == right_val do
        raise "Assertion failed: #{inspect(left_val)} != #{inspect(right_val)}"
      end
    end
  end
end

require Assertions
Assertions.assert(1 + 1 == 2)  # Passes
Assertions.assert(1 + 1 == 3)  # Raises error
```

### 4. Creating a DSL

```elixir
defmodule Router do
  defmacro __using__(_opts) do
    quote do
      import Router
      @routes []
      @before_compile Router
    end
  end

  defmacro get(path, handler) do
    quote do
      @routes [{:get, unquote(path), unquote(handler)} | @routes]
    end
  end

  defmacro post(path, handler) do
    quote do
      @routes [{:post, unquote(path), unquote(handler)} | @routes]
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def routes, do: Enum.reverse(@routes)

      def match(method, path) do
        Enum.find_value(routes(), fn
          {^method, ^path, handler} -> {:ok, handler}
          _ -> nil
        end) || {:error, :not_found}
      end
    end
  end
end

# Usage
defmodule MyRouter do
  use Router

  get "/users", &list_users/0
  get "/users/:id", &show_user/1
  post "/users", &create_user/1
end

MyRouter.routes()
# => [get: "/users", ..., post: "/users", ...]
```

## Advanced Patterns

### 1. Module Attributes in Macros

```elixir
defmodule Schema do
  defmacro __using__(_opts) do
    quote do
      import Schema
      Module.register_attribute(__MODULE__, :fields, accumulate: true)
      @before_compile Schema
    end
  end

  defmacro field(name, type) do
    quote do
      @fields {unquote(name), unquote(type)}
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def fields, do: Enum.reverse(@fields)

      def new(attrs \\ %{}) do
        struct(__MODULE__, attrs)
      end
    end
  end
end

defmodule User do
  use Schema

  defstruct [:name, :email, :age]

  field :name, :string
  field :email, :string
  field :age, :integer
end

User.fields()
# => [name: :string, email: :string, age: :integer]
```

### 2. Hygiene and Variable Scoping

```elixir
# Hygienic macro - variables don't leak
defmodule Hygienic do
  defmacro safe do
    quote do
      x = 42
      x * 2
    end
  end
end

x = 1
require Hygienic
Hygienic.safe()  # 84
x  # Still 1 - not affected by macro

# Unhygienic macro - explicit variable injection
defmodule Unhygienic do
  defmacro unsafe do
    quote do
      var!(x) = 42  # var! bypasses hygiene
    end
  end
end

x = 1
require Unhygienic
Unhygienic.unsafe()
x  # Now 42 - affected by macro
```

### 3. Context-Aware Macros

```elixir
defmodule Contextual do
  defmacro log(message) do
    quote do
      IO.puts("[#{__MODULE__}:#{__ENV__.line}] #{unquote(message)}")
    end
  end
end

defmodule MyApp do
  require Contextual

  def work do
    Contextual.log("Starting work")  # [MyApp:5] Starting work
  end
end
```

### 4. Generating Functions Dynamically

```elixir
defmodule RESTClient do
  @methods [:get, :post, :put, :delete, :patch]

  for method <- @methods do
    def unquote(method)(url, opts \\ []) do
      HTTPoison.request(unquote(method), url, "", [], opts)
    end
  end
end

RESTClient.get("https://api.example.com/users")
RESTClient.post("https://api.example.com/users", body: "...")
```

## Real-World Examples

### 1. Test Assertions DSL

```elixir
defmodule TestDSL do
  defmacro test(description, do: block) do
    test_name = String.to_atom("test_#{description}")

    quote do
      def unquote(test_name)() do
        unquote(block)
      end
    end
  end

  defmacro assert_equal(left, right) do
    quote do
      left_val = unquote(left)
      right_val = unquote(right)

      if left_val != right_val do
        raise """
        Assertion failed:
          Expected: #{inspect(right_val)}
          Got:      #{inspect(left_val)}
        """
      end
    end
  end
end

defmodule MathTest do
  import TestDSL

  test "addition" do
    assert_equal 1 + 1, 2
    assert_equal 2 + 2, 4
  end

  test "multiplication" do
    assert_equal 2 * 3, 6
    assert_equal 5 * 5, 25
  end
end
```

### 2. Validation DSL

```elixir
defmodule Validator do
  defmacro __using__(_opts) do
    quote do
      import Validator
      @validations []
      @before_compile Validator
    end
  end

  defmacro validates(field, rules) do
    quote do
      @validations [{unquote(field), unquote(rules)} | @validations]
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def validate(data) do
        Enum.reduce_while(@validations, {:ok, data}, fn {field, rules}, {:ok, data} ->
          value = Map.get(data, field)

          case validate_field(value, rules) do
            :ok -> {:cont, {:ok, data}}
            {:error, reason} -> {:halt, {:error, {field, reason}}}
          end
        end)
      end

      defp validate_field(value, rules) do
        Enum.reduce_while(rules, :ok, fn
          {:required, true}, :ok ->
            if value in [nil, ""], do: {:halt, {:error, "is required"}}, else: {:cont, :ok}

          {:min_length, min}, :ok when is_binary(value) ->
            if String.length(value) < min,
              do: {:halt, {:error, "must be at least #{min} characters"}},
              else: {:cont, :ok}

          {:format, regex}, :ok when is_binary(value) ->
            if Regex.match?(regex, value),
              do: {:cont, :ok},
              else: {:halt, {:error, "invalid format"}}

          _, :ok ->
            {:cont, :ok}
        end)
      end
    end
  end
end

defmodule UserValidator do
  use Validator

  validates :name, required: true, min_length: 2
  validates :email, required: true, format: ~r/@/
  validates :age, required: true
end

UserValidator.validate(%{name: "Al", email: "al@example.com", age: 30})
# => {:ok, %{...}}

UserValidator.validate(%{name: "", email: "invalid", age: 30})
# => {:error, {:name, "is required"}}
```

### 3. Pipeline Debugging Macro

```elixir
defmodule Pipeline do
  defmacro debug(pipeline) do
    quote do
      result = unquote(pipeline)
      IO.puts("Pipeline result: #{inspect(result)}")
      result
    end
  end

  defmacro trace(pipeline) do
    # Extract steps from pipeline
    steps = extract_pipeline_steps(pipeline)

    debug_steps =
      Enum.map(steps, fn step ->
        quote do
          result = unquote(step)
          IO.puts("  #{unquote(Macro.to_string(step))} => #{inspect(result)}")
          result
        end
      end)

    quote do
      IO.puts("Pipeline trace:")
      unquote_splicing(debug_steps)
    end
  end

  defp extract_pipeline_steps({:|>, _, [left, right]}) do
    extract_pipeline_steps(left) ++ [right]
  end

  defp extract_pipeline_steps(expr), do: [expr]
end

# Usage
require Pipeline

Pipeline.trace(
  [1, 2, 3]
  |> Enum.map(&(&1 * 2))
  |> Enum.filter(&(&1 > 3))
  |> Enum.sum()
)
# Pipeline trace:
#   [1, 2, 3] => [1, 2, 3]
#   Enum.map(&(&1 * 2)) => [2, 4, 6]
#   Enum.filter(&(&1 > 3)) => [4, 6]
#   Enum.sum() => 10
```

### 4. Configuration DSL

```elixir
defmodule Config do
  defmacro __using__(_opts) do
    quote do
      import Config
      @config %{}
      @before_compile Config
    end
  end

  defmacro set(key, value) do
    quote do
      @config Map.put(@config, unquote(key), unquote(value))
    end
  end

  defmacro env(key, env_var, default \\ nil) do
    quote do
      value = System.get_env(unquote(env_var)) || unquote(default)
      @config Map.put(@config, unquote(key), value)
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def config, do: @config
      def get(key, default \\ nil), do: Map.get(@config, key, default)
    end
  end
end

defmodule AppConfig do
  use Config

  set :app_name, "MyApp"
  set :version, "1.0.0"
  env :database_url, "DATABASE_URL", "postgres://localhost/myapp"
  env :secret_key, "SECRET_KEY"
end

AppConfig.get(:app_name)  # "MyApp"
AppConfig.get(:database_url)  # From env or default
```

## Common Pitfalls

### 1. Using Macros When Functions Suffice

**Problem:**

```elixir
# Unnecessary macro
defmacro add(a, b) do
  quote do
    unquote(a) + unquote(b)
  end
end
```

**Solution:**

```elixir
# Use a function instead
def add(a, b), do: a + b
```

**Rule:** Use macros only when you need compile-time code generation. Functions are simpler and easier to debug.

### 2. Variable Hygiene Issues

```elixir
# Problem - variable name collision
defmodule Broken do
  defmacro set_x do
    quote do
      x = 42  # Might conflict with caller's x
    end
  end
end

# Solution - use unique variable names or var!
defmodule Fixed do
  defmacro set_x do
    quote do
      unquote(Macro.var(:x, __MODULE__)) = 42
    end
  end
end
```

### 3. Not Understanding AST

```elixir
# Problem - trying to manipulate runtime values
defmacro broken(list) do
  quote do
    Enum.map(unquote(list), &(&1 * 2))  # list isn't available at compile time
  end
end

# Solution - pass the AST, not the value
defmacro fixed(list) do
  quote do
    unquote(list) |> Enum.map(&(&1 * 2))
  end
end
```

## Debugging Macros

### 1. Inspecting Macro Expansion

```elixir
# Use Macro.expand/2 to see expanded code
ast = quote do: MyMacro.my_macro(42)
Macro.expand(ast, __ENV__)
```

### 2. Using dbg in Macros

```elixir
defmacro debug_macro(expr) do
  IO.puts("Macro input AST: #{inspect(expr)}")

  result = quote do
    unquote(expr)
  end

  IO.puts("Macro output AST: #{inspect(result)}")
  result
end
```

### 3. Testing Macros

```elixir
defmodule MacroTest do
  use ExUnit.Case
  require MyMacros

  test "macro expands correctly" do
    ast = quote do: MyMacros.my_macro(42)
    expanded = Macro.expand(ast, __ENV__)

    # Assert on the expanded AST structure
    assert match?({:def, _, _}, expanded)
  end

  test "macro generates correct code" do
    # Test the runtime behavior
    assert MyMacros.my_macro(42) == expected_result
  end
end
```

## Performance Considerations

### Compile-Time vs. Runtime

```elixir
# Macro - work done at compile time
defmacro compile_time_work(n) do
  result = expensive_computation(n)  # Runs once at compile time
  quote do: unquote(result)
end

# Function - work done at runtime
def runtime_work(n) do
  expensive_computation(n)  # Runs every call
end
```

### When to Use Macros

**Good Use Cases:**

- DSLs (routing, testing, configuration)
- Code generation (repeated patterns)
- Compile-time optimization
- Extending language syntax

**Avoid For:**

- Simple data transformation (use functions)
- Runtime logic (use functions)
- When debugging simplicity matters
- When team unfamiliar with metaprogramming

## Macro Best Practices

1. **Prefer functions over macros** - Only use macros when necessary
2. **Document macro behavior** - Explain what code they generate
3. **Keep macros simple** - Complex macros are hard to maintain
4. **Test macro expansion** - Verify AST generation
5. **Respect hygiene** - Avoid variable leakage
6. **Provide escape hatches** - Allow users to opt-out of macro magic
7. **Use @before_compile wisely** - For final module modifications

## Related Resources

- [Advanced Tutorial](/learn/swe/prog-lang/elixir/tutorials/advanced)
- [Best Practices](/learn/swe/prog-lang/elixir/explanation/best-practices)
- [Protocols Guide](/learn/swe/prog-lang/elixir/how-to/protocols)
- [GenServer Guide](/learn/swe/prog-lang/elixir/how-to/genserver)
