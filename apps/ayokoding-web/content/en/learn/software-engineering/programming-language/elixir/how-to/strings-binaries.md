---
title: "Strings Binaries"
date: 2025-12-21T18:00:00+07:00
draft: false
description: "Master string manipulation and binary handling in Elixir with pattern matching, String module functions, and binary protocols."
weight: 1000016
tags: ["elixir", "strings", "binaries", "text-processing", "unicode", "how-to"]
---

**Need efficient text processing in Elixir?** This guide teaches you to work with UTF-8 strings and binaries using pattern matching, the String module, and binary syntax for high-performance text manipulation in production applications.

## Prerequisites

- Basic Elixir syntax
- Understanding of pattern matching
- Completed [Quick Start Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/quick-start)
- Familiarity with Unicode concepts (helpful)

## Problem

Text processing appears simple but hides complexity: character encoding (UTF-8 vs ASCII), grapheme clusters vs bytes, performance when processing large text, and cross-platform newline handling. Elixir strings are UTF-8 encoded binaries, requiring understanding of both string and binary operations for efficient text manipulation.

**Challenges:**

- Distinguishing between characters, graphemes, and bytes
- Efficient string concatenation and manipulation
- Binary pattern matching for parsing protocols
- Unicode normalization and case conversion
- Handling non-UTF-8 data (legacy encodings, binary protocols)
- Performance optimization for large text processing

## Solution Overview

Elixir strings are **UTF-8 binaries** with powerful pattern matching and a comprehensive String module. Use **String module** for high-level Unicode operations and **binary pattern matching** for performance-critical parsing.

**Key Concepts:**

- **String**: UTF-8 encoded binary (`"hello"`)
- **Charlists**: List of Unicode codepoints (`~c"hello"`)
- **Binaries**: Raw byte sequences (`<<1, 2, 3>>`)
- **Graphemes**: User-perceived characters (may be multiple codepoints)
- **Codepoints**: Unicode character codes

## Detailed Implementation

### 1. String Basics

#### String Creation and Concatenation

```elixir
greeting = "Hello, World!"

full_name = "John" <> " " <> "Doe"

name = "Alice"
message = "Hello, #{name}!"  # "Hello, Alice!"

text = """
This is a
multi-line
string.
"""

sql = ~s"""
SELECT * FROM users
WHERE age > 18
ORDER BY name
"""
```

#### String Module Operations

```elixir
String.length("café")  # 4 (not 5!)
byte_size("café")      # 5 (é is 2 bytes in UTF-8)

String.upcase("hello")     # "HELLO"
String.downcase("WORLD")   # "world"
String.capitalize("alice") # "Alice"

String.trim("  hello  ")        # "hello"
String.trim_leading("  hello") # "hello"
String.trim_trailing("hello  ")# "hello"

String.split("a,b,c", ",")           # ["a", "b", "c"]
String.split("hello world", " ")      # ["hello", "world"]
String.split("a|b|c", "|", parts: 2)  # ["a", "b|c"]

String.replace("hello", "l", "L")          # "heLLo"
String.replace("hello", "l", "L", global: false) # "heLlo" (first only)

String.contains?("hello world", "world")  # true
String.starts_with?("hello", "he")        # true
String.ends_with?("hello", "lo")          # true

Enum.join(["a", "b", "c"], ",")  # "a,b,c"
```

### 2. Binary Pattern Matching

Binary pattern matching enables efficient parsing without string allocations.

#### Basic Binary Patterns

```elixir
<<"GET ", path::binary>> = "GET /users"
path  # "/users"

def parse_request(request) do
  case request do
    <<"GET ", rest::binary>> ->
      {:get, parse_path(rest)}

    <<"POST ", rest::binary>> ->
      {:post, parse_path(rest)}

    <<"PUT ", rest::binary>> ->
      {:put, parse_path(rest)}

    _ ->
      {:error, :unknown_method}
  end
end

<<version::8, flags::8, rest::binary>> = <<1, 0, 100, 200>>
version  # 1
flags    # 0
rest     # <<100, 200>>

<<r::8, g::8, b::8>> = <<255, 128, 0>>  # RGB color
r  # 255 (red)
g  # 128 (green)
b  # 0   (blue)
```

#### Parsing Text Protocols

```elixir
defmodule CSV do
  def parse_line(line) do
    line
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.trim/1)
  end

  # More efficient with pattern matching
  def parse_line_fast(line) do
    parse_fields(line, [], [])
  end

  defp parse_fields("," <> rest, current, acc) do
    field = current |> Enum.reverse() |> IO.iodata_to_binary() |> String.trim()
    parse_fields(rest, [], [field | acc])
  end

  defp parse_fields(<<char, rest::binary>>, current, acc) do
    parse_fields(rest, [char | current], acc)
  end

  defp parse_fields("", current, acc) do
    field = current |> Enum.reverse() |> IO.iodata_to_binary() |> String.trim()
    Enum.reverse([field | acc])
  end
end

def parse_query_string(query) do
  query
  |> String.split("&")
  |> Enum.map(fn pair ->
    case String.split(pair, "=", parts: 2) do
      [key, value] -> {key, value}
      [key] -> {key, ""}
    end
  end)
  |> Map.new()
end

parse_query_string("name=Alice&age=30")
```

### 3. Unicode and Graphemes

Understanding Unicode is crucial for correct string handling.

#### Graphemes vs Codepoints vs Bytes

```elixir
text = "café"

String.length(text)           # 4
String.graphemes(text)        # ["c", "a", "f", "é"]

String.codepoints(text)       # ["c", "a", "f", "é"]

byte_size(text)               # 5 (é is 2 bytes)
:binary.bin_to_list(text)     # [99, 97, 102, 195, 169]
```

#### Unicode Normalization

```elixir
nfc = "é"        # Single codepoint (NFC normalized)
nfd = "e\u0301"  # e + combining acute accent (NFD normalized)

String.length(nfc)  # 1
String.length(nfd)  # 2 (different representation)

String.normalize(nfc, :nfc) == String.normalize(nfd, :nfc)  # true
```

#### Working with Emojis

```elixir
text = "Hello 👋🏽"  # Wave with skin tone (multi-codepoint grapheme)

String.length(text)      # 7 (6 chars + 1 emoji grapheme)
String.codepoints(text)  # [..., "👋", "🏽"] (wave + modifier)
String.graphemes(text)   # [..., "👋🏽"] (perceived as single unit)

String.split_at(text, 6)  # {"Hello ", "👋🏽"}
```

### 4. Advanced String Operations

#### String Slicing

```elixir
text = "Hello, World!"

String.slice(text, 0, 5)    # "Hello"
String.slice(text, 7, 5)    # "World"
String.slice(text, 0..4)    # "Hello"
String.slice(text, 7..-1)   # "World!"

String.slice(text, -6, 6)   # "World!"

String.at(text, 0)   # "H"
String.at(text, -1)  # "!"
```

#### Pattern-Based Operations

```elixir
text = "The year is 2024"
Regex.replace(~r/\d+/, text, "XXXX")  # "The year is XXXX"

Regex.scan(~r/\d+/, "Port: 8080, Timeout: 30")

regex = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
Regex.named_captures(regex, "Date: 2024-12-21")

Regex.match?(~r/@/, "alice@example.com")  # true
```

#### String Validation

```elixir
defmodule Validator do
  def email?(string) do
    String.match?(string, ~r/^[^\s@]+@[^\s@]+\.[^\s@]+$/)
  end

  def url?(string) do
    String.match?(string, ~r/^https?:\/\/.+/)
  end

  def alphanumeric?(string) do
    String.match?(string, ~r/^[a-zA-Z0-9]+$/)
  end

  def empty?(string) do
    String.trim(string) == ""
  end
end
```

### 5. Performance Optimization

#### IO Lists for Concatenation

```elixir
html =
  "<html>" <>
  "<head>" <>
  "<title>Page</title>" <>
  "</head>" <>
  "<body>Content</body>" <>
  "</html>"

html =
  ["<html>",
   "<head>",
   "<title>Page</title>",
   "</head>",
   "<body>Content</body>",
   "</html>"]
  |> IO.iodata_to_binary()

title = "Page"
content = "Content"

html = """
<html>
<head><title>#{title}</title></head>
<body>#{content}</body>
</html>
"""
```

#### Efficient String Building

```elixir
def join_bad(items) do
  Enum.reduce(items, "", fn item, acc ->
    acc <> item <> ", "
  end)
end

def join_good(items) do
  items
  |> Enum.intersperse(", ")
  |> IO.iodata_to_binary()
end

Enum.join(items, ", ")
```

## How It Works

### UTF-8 Encoding

Elixir strings use UTF-8 encoding where characters take 1-4 bytes:

- ASCII (0-127): 1 byte
- Extended Latin (128-2047): 2 bytes
- Most common (2048-65535): 3 bytes
- Rare/historic (65536+): 4 bytes

Example: "café"

- c: 99 (1 byte)
- a: 97 (1 byte)
- f: 102 (1 byte)
- é: 195, 169 (2 bytes)

Total: 5 bytes for 4 characters

### Binary Pattern Matching Efficiency

Pattern matching on binaries is highly optimized:

```elixir
<<"GET ", path::binary>> = request

```

### String Immutability

Strings are immutable - operations return new strings:

```elixir
original = "hello"
upper = String.upcase(original)

original  # Still "hello"
upper     # "HELLO"
```

## Variations

### 1. Custom String Sigils

```elixir
defmodule MySigils do
  def sigil_u(string, _opts) do
    String.upcase(string)
  end

  def sigil_t(string, _opts) do
    String.trim(string)
  end
end

import MySigils

~u/hello/  # "HELLO"
~t/  hi  / # "hi"
```

### 2. String Protocols

```elixir
defprotocol Stringify do
  def to_string(data)
end

defimpl Stringify, for: List do
  def to_string(list), do: Enum.join(list, ",")
end

defimpl Stringify, for: Map do
  def to_string(map), do: inspect(map)
end

Stringify.to_string([1, 2, 3])        # "1,2,3"
Stringify.to_string(%{a: 1})          # "%{a: 1}"
```

### 3. Custom Parsers

```elixir
defmodule JSONParser do
  def parse("{" <> rest) do
    parse_object(rest, %{})
  end

  def parse("[" <> rest) do
    parse_array(rest, [])
  end

  defp parse_object("}" <> _rest, acc), do: acc

  defp parse_object(rest, acc) do
    # Simplified JSON object parser
    # Production code should use Jason library
  end
end
```

## Pitfalls and Best Practices

### Common Mistakes

**1. Confusing Strings and Charlists**

**Bad:**

```elixir
list = 'hello'  # Charlist, not string!
String.upcase(list)  # ERROR
```

**Good:**

```elixir
string = "hello"  # String
String.upcase(string)  # "HELLO"

list = ~c"hello"
String.Chars.to_string(list)  # "hello"
```

**2. Inefficient Concatenation**

**Bad:**

```elixir
result = Enum.reduce(1..1000, "", fn i, acc ->
  acc <> Integer.to_string(i) <> ","
end)
```

**Good:**

```elixir
result =
  1..1000
  |> Enum.map(&Integer.to_string/1)
  |> Enum.join(",")
```

**3. Ignoring Unicode**

**Bad:**

```elixir
def initials(name) do
  String.slice(name, 0, 1)  # Fails for "🔥Fire" → "�"
end
```

**Good:**

```elixir
def initials(name) do
  name
  |> String.graphemes()
  |> List.first()
end
```

**4. Incorrect Byte Operations**

**Bad:**

```elixir
<<head::binary-size(3), _::binary>> = "café"
head  # <<99, 97, 102>> = "caf" (corrupted é!)
```

**Good:**

```elixir
String.slice("café", 0, 3)  # "caf"
```

## Related Resources

- [Beginner Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner) - String fundamentals
- [Pattern Matching Guide](/en/learn/software-engineering/programming-language/elixir/how-to/pattern-matching) - Binary patterns
- [File I/O Guide](/en/learn/software-engineering/programming-language/elixir/how-to/file-io) - Text file processing
- [Cookbook](/en/learn/software-engineering/programming-language/elixir/how-to/cookbook) - String recipes
