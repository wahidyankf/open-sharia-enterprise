---
title: "Files: EDN - Overview"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

EDN (Extensible Data Notation) is a versatile data format utilized within the Clojure programming language. It offers a textual representation of data that can be read by both humans and machines, making it very easy to analyze and interpret. The simplicity and efficiency of EDN make it a popular alternative to other data interchange formats, such as JSON or XML.

Developers appreciate the flexibility of EDN, as it allows for creating custom data types for specific applications. This makes working with complex data structures easier, as developers can easily convert them to EDN format. Additionally, EDN has been praised for its compactness and ease of use, particularly in comparison to the more verbose XML format.

## Basic Syntax Elements

The syntax of EDN is straightforward and encompasses several fundamental elements, which can be further expanded upon:

1. Scalars: EDN supports various scalar types, including:
   - **Strings**: Enclosed in double quotes (`"Hello, world!"`). Strings often represent text-based data, such as user input or log messages.
   - **Keywords**: Begin with a colon (`:keyword`) and represent unique identifiers. Keywords are often used to label or identify elements within data structures.
   - **Symbols**: Similar to keywords but lack the leading colon (`symbol`). Symbols are often used to represent variables or functions in code.
   - **Numbers**: Integers (`42`), decimals (`3.14`), fractions (`1/2`), and scientific notation (`1.0e10`). Numbers often represent quantitative data, such as measurements or statistics.
   - **Booleans**: Represented as `true` or `false`. Booleans are often used to represent binary or logical states in code.
   - **nil**: Denotes the absence of a value (`nil`). `nil` is often used in place of an actual value or as a placeholder.
2. Collections: EDN supports several collection types, each with their own unique properties and use cases:
   - **Lists**: Ordered sequences of values enclosed in parentheses (`(1 2 3)`). Lists are often used to represent ordered data, such as a sequence of steps in a process.
   - **Vectors**: Ordered sequences of values enclosed in square brackets (`[1 2 3]`). Vectors are similar to lists but are often used when constant-time random access is required.
   - **Sets**: Unordered collections of unique values enclosed in curly braces (`#{1 2 3}`). Sets are often used to represent a collection of distinct elements.
   - **Maps**: Key-value pairs enclosed in curly braces (`{:key1 "value1" :key2 "value2"}`). Maps are often used to represent structured data, such as configuration files or database entries.
3. Comments: EDN supports line comments (beginning with `;`) and block comments (`#_`). Comments are often used to provide context or explanations within code.
4. Metadata: EDN allows attaching metadata to values using the `^` symbol. Metadata provides additional information about the value and is commonly used for annotations or documentation purposes. Metadata can be used to add context or information about the data, such as its type or source.

## Examples of EDN Data

Let's explore some examples to illustrate valid EDN data:

- Scalar values:
  - Integer: `42`
  - Decimal: `3.14`
  - String: `"Hello, world!"`
  - Keyword: `:keyword`
  - Boolean: `true`
  - Nil: `nil`
- Collections:
  - Lists: `(1 2 3)`
  - Vectors: `[1 2 3]`
  - Sets: `#{1 2 3}`
  - Maps: `{:name "John" :age 30}`
- Metadata:
  - `^{:author "Alice"} [:book "Title"]`

## Practical Usage of EDN

EDN, or Extensible Data Notation, is a versatile and flexible data format with many practical applications. It is primarily used for representing data, rather than behavior or functions, which makes it ideal for use in a wide range of scenarios. Some of the key uses of EDN include:

- **Configuration files**: EDN is commonly used for configuring Clojure applications due to its simplicity and readability. With EDN, developers can easily create easily, read and modify configuration files without worrying about complex data structures or syntax.
- **Data serialization**: EDN allows the serialization of complex data structures into a textual format, which makes it possible to store or transfer data between systems. This is useful in situations where it is necessary to share data between different applications or programming languages.
- **Communication between systems**: With its human-readable format, EDN makes it easy to exchange data between different systems or programming languages. This is particularly useful when different systems need to communicate with each other but use different data formats or protocols.

Clojure provides built-in functions for reading and writing EDN data, which simplifies its manipulation within the language. This means developers can efficiently work with EDN data structures without worrying about parsing or serialization. Additionally, because EDN is a lightweight and flexible format, it can be used in various contexts, making it a powerful tool for developers and data scientists.

## Example of edn file

### Generic edn file

```clojure
{
 :nil nil               ;; null
 :boolean true          ;; boolean
 :string "Hello, world!" ;; string
 :character \c           ;; character
 :symbol my-symbol      ;; symbol
 :keyword :my-keyword   ;; keyword
 :integer 42            ;; integer
 :float 3.14159         ;; floating-point number
 :list (1 2 3)          ;; list
 :vector [4 5 6]        ;; vector
 :map {:a 1 :b 2}       ;; map
 :set #{7 8 9}          ;; set
 :tagged-date #inst "2023-01-01T00:00:00.000-00:00" ;; tagged literal for date
 :tagged-uuid #uuid "f81d4fae-7dec-11d0-a765-00a0c91e6bf6" ;; tagged literal for uuid
}
```

Let's go over each data type:

- `nil` is the null value in EDN, represented as `nil`.
- `boolean` is a truth value, which can be `true` or `false`.
- `string` is a sequence of characters surrounded by double quotes.
- `character` is a single character, represented as a backslash followed by the character.
- `symbol` is a named identifier, represented as a sequence of non-whitespace characters.
- `keyword` is a named identifier that evaluates to itself, represented as a colon followed by a sequence of non-whitespace characters.
- `integer` is a whole number.
- `float` is a floating-point number.
- `list` is a sequence of values enclosed in parentheses.
- `vector` is a sequence of values enclosed in square brackets.
- `map` is a collection of key-value pairs enclosed in curly braces.
- `set` is a collection of unique values enclosed in curly braces, prefixed by a `#`.
- `tagged-date` is a tagged literal that represents a date and time instant. The `#inst` tag is followed by a string in a specific format representing the date and time.
- `tagged-uuid` is a tagged literal representing a universally unique identifier (UUID). The `#uuid` tag is followed by a string that represents the UUID.

Remember that EDN also supports comments, which start with a semicolon`;` and continue to the end of the line.

### Deps.edn

The following example is a **`deps.edn`** file, which is used for Clojure project configuration. It includes dependencies, source paths, test paths, and a few aliases for commonly used tasks like running tests and starting a REPL. This **`deps.edn`** configures a project that uses Compojure for routing, Ring for HTTP handling, and Clojure Test for testing:

```clojure
{:paths ["src"]

 :deps
 {org.clojure/clojure {:mvn/version "1.10.3"}
  org.clojure/tools.cli {:mvn/version "1.0.206"}
  ring/ring-core {:mvn/version "1.9.2"}
  compojure {:mvn/version "1.6.2"}}

 :aliases
 {:test
  {:extra-paths ["test"]
   :extra-deps {org.clojure/test.check {:mvn/version "1.1.0"}}}

  :run
  {:main-opts ["-m" "my-app.core"]}

  :repl
  {:main-opts ["-m" "nrepl.cmdline" "--middleware" "[cider.nrepl/cider-middleware]"]}}

 :metadata
 {:project "My App"
  :version #inst "2023-06-16T00:00:00.000-00:00"
  :authors ["Alice" "Bob"]
  :contributors #{:Charlie :Danielle}
  :license {:name "Eclipse Public License"
            :version 2.0}}}
```

This EDN file is the configuration for a Clojure project and is usually named `deps.edn`. The file is structured as a map with four top-level keys: `:paths`, `:deps`, `:aliases`, and `:metadata`.

Let's explain each item:

1. **Paths**:

   `:paths ["src"]`

   This specifies where to find the source files for the project. In this case, source files are in the "src" directory.

2. **Dependencies**:

   `:deps` key defines the project's dependencies. Each key-value pair in the map is a dependency. The key is the dependency's name, and the value is another map specifying the dependency's version. Here's the breakdown:
   - `org.clojure/clojure {:mvn/version "1.10.3"}`: The project depends on Clojure version 1.10.3.
   - `org.clojure/tools.cli {:mvn/version "1.0.206"}`: The project uses the tools.cli library version 1.0.206.
   - `ring/ring-core {:mvn/version "1.9.2"}`: The project uses the Ring library (for handling HTTP requests) version 1.9.2.
   - `compojure {:mvn/version "1.6.2"}`: The project uses the Compojure library (for routing in web applications) version 1.6.2.

3. **Aliases**:

   The `:aliases` key defines a map of shortcuts for common tasks. Each key-value pair in the map is an alias. The key is the alias's name, and the value is another map that specifies the tasks associated with the alias:
   - `:test {:extra-paths ["test"] :extra-deps {org.clojure/test.check {:mvn/version "1.1.0"}}}`: The `:test` alias adds the "test" directory to the classpath and includes the test.check library, which is necessary for running tests.
   - `:run {:main-opts ["-m" "my-app.core"]}`: The `:run` alias specifies that the main function of the `my-app.core` namespace should be run.
   - `:repl {:main-opts ["-m" "nrepl.cmdline" "--middleware" "[cider.nrepl/cider-middleware]"]}`: The `:repl` alias starts a REPL with nREPL and CIDER middleware.

4. **Metadata**:

   This section is not standard but shows how you can extend the `deps.edn` file with more fields and data types as needed. It includes a map of some metadata about the project:
   - `:project "My App"`: The project's name is "My App".
   - `:version #inst "2023-06-16T00:00:00.000-00:00"`: The project's version is a timestamp. The `#inst` tag specifies that the following string represents a timestamp.
   - `:authors ["Alice" "Bob"]`: The authors of the project are Alice and Bob.
   - `:contributors #{:Charlie :Danielle}`: The contributors to the project are Charlie and Danielle.
   - `:license {:name "Eclipse Public License" :version 2.0}`: The project is licensed under the Eclipse Public License version 2.0.

This `deps.edn` file provides a straightforward, readable configuration for a Clojure project, including its dependencies, paths, aliases for everyday tasks, and additional metadata.

## Closing

Clojure's EDN syntax provides a concise and expressive way to represent data, making it a versatile tool for data interchange and configuration in Clojure applications. Its straightforward syntax, support for various data types, and metadata capabilities make it an efficient choice for representing structured data. By leveraging built-in functions, Clojure seamlessly handles reading and writing EDN data. With its focus on data representation, EDN simplifies the exchange of information between different systems and programming languages.

## Further Readings

- EDN format on GitHub: [https://github.com/edn-format/edn](https://github.com/edn-format/edn)
