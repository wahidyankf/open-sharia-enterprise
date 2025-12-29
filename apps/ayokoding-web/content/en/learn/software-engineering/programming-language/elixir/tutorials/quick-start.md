---
title: "Quick Start"
date: 2025-12-21T16:00:00+07:00
draft: false
description: "Master core Elixir concepts fast: pattern matching, immutability, functions, and modules. Build working programs in your first session."
weight: 100002
tags: ["elixir", "tutorial", "quick-start", "beginner", "fundamentals"]
---

**Want to write real Elixir code in your first learning session?** This quick start tutorial teaches you the 10 core concepts you need to start building programs immediately - no lengthy theory, just practical working code.

## Prerequisites

**Required:**

- Elixir installed and working (complete [Initial Setup](/en/learn/software-engineering/programming-language/elixir/tutorials/initial-setup) first)
- Text editor ready
- Terminal access

**Recommended:**

- Completed Initial Setup tutorial
- Basic programming experience (any language) helps but not required
- IEx shell open for trying examples

## What You'll Learn

By the end of this tutorial, you will:

- Use pattern matching to destructure data
- Work with immutable data structures
- Write and compose functions
- Organize code with modules
- Handle errors gracefully
- Use the pipe operator for readable code
- Understand basic recursion
- Work with comprehensions
- Create Mix projects
- Read and understand Elixir code in the wild

## Learning Path

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TB
    Start[Start: Elixir Installed] --> Pattern[Pattern Matching]
    Pattern --> Immutable[Immutability]
    Immutable --> Functions[Functions]
    Functions --> Modules[Modules]
    Modules --> Pipe[Pipe Operator]
    Pipe --> Lists[Lists & Recursion]
    Lists --> Comprehensions[Comprehensions]
    Comprehensions --> Mix[Mix Projects]
    Mix --> Ready[Ready to Code!]

    style Start fill:#0173B2
    style Ready fill:#029E73
    style Pattern fill:#DE8F05
    style Functions fill:#DE8F05
    style Modules fill:#DE8F05
```

## Coverage

This tutorial covers **5-30%** of Elixir knowledge - the essential concepts to start building programs. It's designed for rapid learning: you'll write working code immediately and understand enough to read Elixir documentation and examples.

**What's Covered:**

- Pattern matching (Elixir's superpower)
- Immutable data and why it matters
- Functions (anonymous and named)
- Modules and basic organization
- Pipe operator (`|>`)
- Lists and basic recursion
- Comprehensions for data transformation
- Mix project structure

**What's NOT Covered:**

- Advanced data structures (covered in Beginner tutorial)
- OTP and processes (covered in Intermediate tutorial)
- Phoenix web framework (covered in Intermediate tutorial)
- Metaprogramming (covered in Advanced tutorial)

**Learning Strategy:**

Each section has:

1. **Concept** - What it is and why it matters
2. **Example** - Working code you can try
3. **Try It** - Hands-on exercise
4. **Common Pitfall** - What to avoid

Open IEx (`iex` in terminal) and try every example as you read!

## 1. Pattern Matching - Elixir's Superpower

### Concept

In most languages, `=` means "assign." In Elixir, `=` means "match."

Pattern matching lets you:

- Extract values from data structures
- Verify data shapes
- Write cleaner, more expressive code

### Example

```elixir
x = 5

5 = x

10 = x

{status, result} = {:ok, "Success!"}
status
result

{:ok, _} = {:ok, "Don't care about this"}

{:ok, value} = {:ok, 42}

{:error, value} = {:ok, 42}

[first | rest] = [1, 2, 3, 4, 5]
first
rest

[a, b | tail] = [1, 2, 3, 4]
a
b
tail
```

### Try It

Open IEx and try:

```elixir
user = {"Alice", 28, "alice@example.com"}

{name, age, email} = user

IO.puts("Name: #{name}, Age: #{age}, Email: #{email}")
```

### Common Pitfall

```elixir
{a, b} = {1, 2, 3}

{a, b, _} = {1, 2, 3}
```

## 2. Immutability - Data Never Changes

### Concept

In Elixir, **data is immutable** - once created, it never changes. Instead of modifying data, you create new data.

Why immutability matters:

- No accidental side effects
- Easier to reason about code
- Enables safe concurrency (covered in Intermediate tutorial)
- Prevents entire classes of bugs

### Example

```elixir
numbers = [1, 2, 3]

new_numbers = [0 | numbers]

numbers

person = %{name: "Alice", age: 28}

older_person = %{person | age: 29}

person

doubled = Enum.map([1, 2, 3], fn x -> x * 2 end)
```

### Try It

```elixir
cart = ["apple", "banana"]

new_cart = ["orange" | cart]

IO.inspect(cart, label: "Original")
IO.inspect(new_cart, label: "New")

```

### Common Pitfall

```elixir
list = [1, 2, 3]
List.delete(list, 2)  # Returns new list
list

list = [1, 2, 3]
list = List.delete(list, 2)
list
```

## 3. Functions - Anonymous and Named

### Concept

Functions are first-class citizens in Elixir. You can:

- Pass functions as arguments
- Return functions from functions
- Store functions in variables

**Anonymous functions** - defined inline with `fn ... end`
**Named functions** - defined in modules with `def`

### Example - Anonymous Functions

```elixir
add = fn a, b -> a + b end

add.(5, 3)

add = &(&1 + &2)
add.(5, 3)

Enum.map([1, 2, 3], fn x -> x * 2 end)

handle_result = fn
  {:ok, value} -> "Success: #{value}"
  {:error, reason} -> "Failed: #{reason}"
end

handle_result.({:ok, "Data loaded"})

handle_result.({:error, "Network timeout"})
```

### Example - Named Functions

```elixir
defmodule Math do
  # Public function (accessible outside module)
  def add(a, b) do
    a + b
  end

  # Private function (only within module)
  defp multiply(a, b) do
    a * b
  end

  # Function with multiple clauses (pattern matching)
  def describe(n) when n < 0, do: "negative"
  def describe(0), do: "zero"
  def describe(n) when n > 0, do: "positive"

  # Default arguments
  def greet(name \\ "World") do
    "Hello, #{name}!"
  end
end

Math.add(5, 3)

Math.describe(-5)

Math.greet()

Math.greet("Alice")
```

### Try It

```elixir
defmodule Temperature do
  def c_to_f(celsius) do
    celsius * 9 / 5 + 32
  end

  def f_to_c(fahrenheit) do
    (fahrenheit - 32) * 5 / 9
  end
end

Temperature.c_to_f(0)    # => 32.0
Temperature.c_to_f(100)  # => 212.0
Temperature.f_to_c(32)   # => 0.0
```

### Common Pitfall

```elixir
add = fn a, b -> a + b end
add(5, 3)  # ** (CompileError) - missing dot!

add.(5, 3)  # => 8

Math.add(5, 3)  # => 8 - no dot needed
```

## 4. Modules - Organizing Code

### Concept

Modules group related functions together. They're your primary organization tool in Elixir.

**Key points:**

- Module names are atoms (start with capital letter)
- Can define multiple modules in one file
- Modules can be nested
- Use `alias` to shorten module names

### Example

```elixir
defmodule Calculator do
  def add(a, b), do: a + b
  def subtract(a, b), do: a - b
  def multiply(a, b), do: a * b
  def divide(_a, 0), do: {:error, "Cannot divide by zero"}
  def divide(a, b), do: {:ok, a / b}
end

defmodule Shop do
  defmodule Cart do
    def new, do: []
    def add_item(cart, item), do: [item | cart]
    def total(cart), do: Enum.sum(cart)
  end

  defmodule Inventory do
    def in_stock?(_item), do: true  # Simplified
  end
end

cart = Shop.Cart.new()
cart = Shop.Cart.add_item(cart, 10.50)
cart = Shop.Cart.add_item(cart, 25.00)
Shop.Cart.total(cart)

alias Shop.Cart

cart = Cart.new()
cart = Cart.add_item(cart, 10.50)
```

### Try It

```elixir
defmodule User do
  defstruct name: "", age: 0, email: ""

  def new(name, age, email) do
    %User{name: name, age: age, email: email}
  end

  def adult?(user) do
    user.age >= 18
  end

  def display(user) do
    "#{user.name} (#{user.age}) - #{user.email}"
  end
end

alice = User.new("Alice", 28, "alice@example.com")
User.display(alice)

User.adult?(alice)
```

### Common Pitfall

```elixir
def some_function do
  defmodule BadIdea do  # Won't work!
    def hello, do: "hi"
  end
end

defmodule GoodIdea do
  def hello, do: "hi"
end
```

## 5. Pipe Operator - Readable Data Transformations

### Concept

The pipe operator (`|>`) takes the result of one expression and passes it as the **first argument** to the next function. This makes data transformations read like a pipeline.

**Why it matters:**

- Code reads top to bottom, left to right
- Eliminates nested function calls
- Shows data flow clearly

### Example

```elixir
String.upcase(String.trim("  hello world  "))

"  hello world  "
|> String.trim()
|> String.upcase()

[1, 2, 3, 4, 5, 6]
|> Enum.filter(fn x -> rem(x, 2) == 0 end)
|> Enum.map(fn x -> x * x end)
|> Enum.sum()

defmodule InputProcessor do
  def process(input) do
    input
    |> String.trim()
    |> String.downcase()
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end
end

InputProcessor.process("  Apple, Banana,  , Orange  ")
```

### Try It

```elixir
[1, 2, 3, 4, 5]
|> Enum.map(fn x -> x * 2 end)     # Double each
|> Enum.filter(fn x -> x > 5 end)  # Keep > 5
|> Enum.sum()                      # Sum them

"elixir is awesome"
|> String.split()
|> Enum.map(&String.capitalize/1)
|> Enum.join(" ")
```

### Common Pitfall

```elixir
[1, 2, 3] |> Enum.map([4, 5, 6], fn x -> x * 2 end)  # Wrong!

[1, 2, 3] |> Enum.map(fn x -> x * 2 end)  # Correct!
```

## 6. Lists and Basic Recursion

### Concept

Lists are Elixir's fundamental data structure. They're implemented as linked lists:

- Fast to prepend (`[head | tail]`)
- Slow to append or access by index
- Perfect for recursive processing

**Recursion** is the natural way to process lists in Elixir (replace loops from other languages).

### Example - Lists

```elixir
numbers = [1, 2, 3, 4, 5]

[0 | numbers]

numbers ++ [6]

[1, 2] ++ [3, 4]

[1, 2, 3, 4] -- [2, 4]

[head | tail] = [1, 2, 3, 4, 5]
head  # => 1
tail  # => [2, 3, 4, 5]

2 in [1, 2, 3]

length([1, 2, 3])
```

### Example - Recursion

```elixir
defmodule ListUtils do
  # Sum all numbers in a list
  def sum([]), do: 0  # Base case: empty list
  def sum([head | tail]) do
    head + sum(tail)  # Recursive case
  end

  # Count elements
  def count([]), do: 0
  def count([_head | tail]) do
    1 + count(tail)
  end

  # Double all elements
  def double([]), do: []
  def double([head | tail]) do
    [head * 2 | double(tail)]
  end

  # Find maximum
  def max([x]), do: x  # Base: single element
  def max([head | tail]) do
    tail_max = max(tail)
    if head > tail_max, do: head, else: tail_max
  end
end

ListUtils.sum([1, 2, 3, 4, 5])

ListUtils.count([:a, :b, :c])

ListUtils.double([1, 2, 3])

ListUtils.max([3, 7, 2, 9, 4])
```

### Try It

```elixir
defmodule MyList do
  def reverse([]), do: []
  def reverse([head | tail]) do
    reverse(tail) ++ [head]
  end
end

MyList.reverse([1, 2, 3, 4, 5])

defmodule MyFilter do
  def evens([]), do: []
  def evens([head | tail]) when rem(head, 2) == 0 do
    [head | evens(tail)]
  end
  def evens([_head | tail]) do
    evens(tail)
  end
end

MyFilter.evens([1, 2, 3, 4, 5, 6])
```

### Common Pitfall

```elixir
def slow_reverse([]), do: []
def slow_reverse([head | tail]) do
  slow_reverse(tail) ++ [head]  # O(n²) - rebuilds list each time
end

def fast_reverse(list), do: reverse_acc(list, [])

defp reverse_acc([], acc), do: acc
defp reverse_acc([head | tail], acc) do
  reverse_acc(tail, [head | acc])  # O(n) - prepends
end
```

## 7. Comprehensions - Elegant Data Transformation

### Concept

Comprehensions provide a concise way to transform and filter collections. They're syntactic sugar over recursion but much more readable.

**Use comprehensions when:**

- Transforming collections
- Filtering data
- Generating combinations

### Example

```elixir
for n <- [1, 2, 3, 4, 5], do: n * n

for n <- [1, 2, 3, 4, 5], rem(n, 2) == 0, do: n * n

for x <- [1, 2], y <- [:a, :b], do: {x, y}

users = [
  {:user, "Alice", 28},
  {:user, "Bob", 35},
  {:admin, "Carol", 42}
]

for {:user, name, age} <- users, do: {name, age}

for x <- [1, 2, 3], into: %{}, do: {x, x * x}

for n <- 1..15 do
  cond do
    rem(n, 15) == 0 -> "FizzBuzz"
    rem(n, 3) == 0 -> "Fizz"
    rem(n, 5) == 0 -> "Buzz"
    true -> n
  end
end
```

### Try It

```elixir
for x <- 1..5, y <- 1..5, do: {x, y, x * y}

users = [
  %{name: "Alice", email: "alice@example.com", active: true},
  %{name: "Bob", email: "bob@example.com", active: false},
  %{name: "Carol", email: "carol@example.com", active: true}
]

for %{email: email, active: true} <- users, do: email

for x <- 0..2, y <- 0..2, do: {x, y}
```

### Common Pitfall

```elixir
for n <- [1, 2, 3], do: IO.puts(n)  # Bad practice!

Enum.each([1, 2, 3], &IO.puts/1)  # Correct!

```

## 8. Working with Maps

### Concept

Maps are Elixir's key-value data structures. Use them when you need:

- Named fields (like objects in other languages)
- Fast key lookup
- Dynamic keys

### Example

```elixir
person = %{name: "Alice", age: 28, city: "Portland"}

person[:name]

person.name  # Only works with atom keys

Map.get(person, :age)

Map.get(person, :country, "Unknown")  # With default

person = %{person | age: 29}

person = Map.put(person, :email, "alice@example.com")

%{name: name, age: age} = person
name  # => "Alice"
age   # => 29

company = %{
  name: "TechCorp",
  employees: %{
    engineering: 50,
    sales: 30,
    support: 20
  }
}

company[:employees][:engineering]

get_in(company, [:employees, :engineering])

company = put_in(company, [:employees, :engineering], 55)
```

### Try It

```elixir
products = %{
  "apple" => %{price: 1.50, stock: 100},
  "banana" => %{price: 0.80, stock: 150},
  "orange" => %{price: 1.20, stock: 80}
}

products["apple"].price

products = put_in(products, ["banana", :stock], 140)

for {name, %{price: price}} <- products, price > 1.00, do: name
```

### Common Pitfall

```elixir
person = %{name: "Alice"}
person = %{person | age: 28}  # ** (KeyError) - age doesn't exist!

person = Map.put(person, :age, 28)  # Works!

person = %{person | name: "Alicia"}  # Works - name exists
```

## 9. Error Handling

### Concept

Elixir uses tagged tuples for error handling:

- `{:ok, result}` for success
- `{:error, reason}` for failure

This is more explicit than exceptions and encourages handling errors.

### Example

```elixir
defmodule FileReader do
  def read(filename) do
    case File.read(filename) do
      {:ok, content} -> {:ok, String.upcase(content)}
      {:error, reason} -> {:error, "Failed to read: #{reason}"}
    end
  end
end

case FileReader.read("existing.txt") do
  {:ok, content} -> IO.puts("Content: #{content}")
  {:error, reason} -> IO.puts("Error: #{reason}")
end

defmodule UserProcessor do
  def process(user_id) do
    with {:ok, user} <- fetch_user(user_id),
         {:ok, profile} <- fetch_profile(user),
         {:ok, posts} <- fetch_posts(user) do
      {:ok, %{user: user, profile: profile, posts: posts}}
    else
      {:error, reason} -> {:error, "Processing failed: #{reason}"}
    end
  end

  defp fetch_user(id), do: {:ok, %{id: id, name: "Alice"}}
  defp fetch_profile(_user), do: {:ok, %{bio: "Developer"}}
  defp fetch_posts(_user), do: {:ok, ["Post 1", "Post 2"]}
end

UserProcessor.process(123)

defmodule Calculator do
  def divide!(_a, 0), do: raise("Division by zero!")
  def divide!(a, b), do: a / b
end

defmodule SafeCalculator do
  def divide(_a, 0), do: {:error, :division_by_zero}
  def divide(a, b), do: {:ok, a / b}
end
```

### Try It

```elixir
defmodule Validator do
  def validate_age(age) when age >= 0 and age <= 120 do
    {:ok, age}
  end
  def validate_age(_age) do
    {:error, "Age must be between 0 and 120"}
  end

  def validate_email(email) do
    if String.contains?(email, "@") do
      {:ok, email}
    else
      {:error, "Invalid email format"}
    end
  end
end

case Validator.validate_age(25) do
  {:ok, age} -> IO.puts("Valid age: #{age}")
  {:error, msg} -> IO.puts("Error: #{msg}")
end

with {:ok, age} <- Validator.validate_age(25),
     {:ok, email} <- Validator.validate_email("alice@example.com") do
  {:ok, %{age: age, email: email}}
end
```

### Common Pitfall

```elixir
def find_user(id) do
  raise "User not found"  # Bad!
end

def find_user(id) do
  {:error, :not_found}  # Good!
end

```

## 10. Mix Projects

### Concept

Mix is Elixir's build tool. It manages:

- Project structure
- Dependencies
- Compilation
- Testing
- Tasks

### Example

```bash
mix new my_app
cd my_app


mix test

mix compile

iex -S mix

mix format
```

**mix.exs - Project Configuration:**

```elixir
defmodule MyApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :my_app,
      version: "0.1.0",
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
    ]
  end
end
```

**Adding Dependencies:**

```elixir
defp deps do
  [
    {:jason, "~> 1.4"}  # JSON library
  ]
end
```

```bash
mix deps.get

Jason.encode!(%{name: "Alice", age: 28})
```

### Try It

```bash
mix new calculator
cd calculator

defmodule Calculator do
  def add(a, b), do: a + b
  def subtract(a, b), do: a - b
  def multiply(a, b), do: a * b
  def divide(_a, 0), do: {:error, :division_by_zero}
  def divide(a, b), do: {:ok, a / b}
end

defmodule CalculatorTest do
  use ExUnit.Case

  test "adds two numbers" do
    assert Calculator.add(2, 3) == 5
  end

  test "handles division by zero" do
    assert Calculator.divide(10, 0) == {:error, :division_by_zero}
  end
end

mix test

iex -S mix
Calculator.add(5, 3)  # => 8
```

### Common Pitfall

```bash

mix deps.get


```

## Hands-On Exercises

### Level 1: Pattern Matching and Functions

Build a simple greeter module:

```elixir
defmodule Greeter do
  # Match on different tuple patterns
  def greet({:morning, name}), do: "Good morning, #{name}!"
  def greet({:evening, name}), do: "Good evening, #{name}!"
  def greet(name), do: "Hello, #{name}!"
end

Greeter.greet({:morning, "Alice"})  # => "Good morning, Alice!"
Greeter.greet("Bob")                # => "Hello, Bob!"
```

### Level 2: Lists and Recursion

Write a function to find all even numbers in a list:

```elixir
defmodule Filter do
  def evens([]), do: []
  def evens([head | tail]) when rem(head, 2) == 0 do
    [head | evens(tail)]
  end
  def evens([_head | tail]) do
    evens(tail)
  end
end

Filter.evens([1, 2, 3, 4, 5, 6])  # => [2, 4, 6]
```

### Level 3: Maps and Comprehensions

Build a shopping cart:

```elixir
defmodule ShoppingCart do
  def new, do: %{}

  def add_item(cart, item, price) do
    Map.put(cart, item, price)
  end

  def total(cart) do
    for {_item, price} <- cart, reduce: 0 do
      sum -> sum + price
    end
  end

  def items(cart) do
    Map.keys(cart)
  end
end

cart = ShoppingCart.new()
cart = ShoppingCart.add_item(cart, "Apple", 1.50)
cart = ShoppingCart.add_item(cart, "Banana", 0.80)
ShoppingCart.total(cart)  # => 2.30
```

### Level 4: Complete Mini-Project

Create a contact manager:

```elixir
defmodule ContactManager do
  defstruct contacts: %{}

  def new, do: %ContactManager{}

  def add_contact(manager, name, email, phone) do
    contact = %{email: email, phone: phone}
    %{manager | contacts: Map.put(manager.contacts, name, contact)}
  end

  def get_contact(manager, name) do
    case Map.get(manager.contacts, name) do
      nil -> {:error, "Contact not found"}
      contact -> {:ok, contact}
    end
  end

  def list_contacts(manager) do
    for {name, contact} <- manager.contacts do
      "#{name}: #{contact.email}, #{contact.phone}"
    end
  end

  def remove_contact(manager, name) do
    %{manager | contacts: Map.delete(manager.contacts, name)}
  end
end

manager = ContactManager.new()
manager = ContactManager.add_contact(manager, "Alice", "alice@example.com", "555-1234")
manager = ContactManager.add_contact(manager, "Bob", "bob@example.com", "555-5678")

ContactManager.list_contacts(manager)

{:ok, contact} = ContactManager.get_contact(manager, "Alice")
```

## Related Content

**Previous Tutorial:**

- [Initial Setup](/en/learn/software-engineering/programming-language/elixir/tutorials/initial-setup) - Install and configure Elixir

**Next Steps:**

- [Beginner Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner) - Deep dive into language features
- [Intermediate Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate) - OTP and concurrency
- [Advanced Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/advanced) - Expert topics

**How-To Guides:**

- [Elixir Cookbook](/en/learn/software-engineering/programming-language/elixir/how-to/cookbook) - Practical recipes
- [How to Use Pattern Matching Effectively](/en/learn/software-engineering/programming-language/elixir/how-to/use-pattern-matching-effectively) - Pattern matching guide
- [How to Work with Lists](/en/learn/software-engineering/programming-language/elixir/how-to/work-with-lists) - List operations
- [How to Use Pipe Operator](/en/learn/software-engineering/programming-language/elixir/how-to/use-pipe-operator) - Pipeline patterns
- [How to Handle Errors](/en/learn/software-engineering/programming-language/elixir/how-to/handle-errors) - Error handling

**Explanations:**

- [Best Practices](/en/learn/software-engineering/programming-language/elixir/explanation/best-practices) - Elixir standards
- [Anti-Patterns](/en/learn/software-engineering/programming-language/elixir/explanation/anti-patterns) - Mistakes to avoid

**Reference:**

- [Elixir Cheat Sheet](/en/learn/software-engineering/programming-language/elixir/reference/cheat-sheet) - Syntax reference

---

## Next Steps

Congratulations! You now understand Elixir's core concepts and can write working programs.

**Continue your learning:**

1. **[Beginner Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner)** - Master all language features (0-60% coverage)
   - Advanced pattern matching (guards, pin operator)
   - Complete data structure tour (tuples, maps, keywords, structs)
   - Protocols and behaviors
   - Testing with ExUnit
   - Error handling patterns

2. **[Cookbook](/en/learn/software-engineering/programming-language/elixir/how-to/cookbook)** - Quick recipes for common problems
   - Copy-paste solutions
   - Real-world patterns

3. **Practice** - Build small projects:
   - Todo list application
   - CSV file parser
   - Text-based game
   - Simple REST API client

**Resources:**

- [Official Elixir Guides](https://elixir-lang.org/getting-started/introduction.html)
- [Elixir School](https://elixirschool.com/)
- [Exercism Elixir Track](https://exercism.org/tracks/elixir) - Practice exercises with mentoring

**Key Takeaways:**

- Pattern matching replaces many if/else conditions
- Data is immutable - transformations create new data
- Functions are first-class - pass them around freely
- Pipe operator makes code readable
- Recursion replaces loops
- Tagged tuples (`{:ok, result}`) handle errors explicitly
- Mix manages projects and dependencies

You're ready to build real Elixir applications. Keep practicing and explore the Beginner tutorial when you're ready to go deeper!
