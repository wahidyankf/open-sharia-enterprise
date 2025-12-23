---
title: "Glossary"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000002
description: Comprehensive Elixir terminology reference - core language, OTP, Phoenix, BEAM VM concepts with definitions and examples
tags: ["elixir", "reference", "glossary", "terminology", "definitions"]
---

**Need to understand Elixir terminology?** This glossary provides clear definitions with examples for all major Elixir, OTP, Phoenix, and BEAM VM concepts.

## Core Language Concepts

### Agent

**Definition**: Simple abstraction around state management using GenServer internally.

**Example**:

```elixir
{:ok, pid} = Agent.start_link(fn -> 0 end)
Agent.get(pid, fn state -> state end)  # 0
Agent.update(pid, fn state -> state + 1 end)
```

**Use case**: Simple shared state without complex logic.

**See also**: [Intermediate Tutorial - Task and Agent](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#section-4-task-and-agent-for-concurrent-tasks)

---

### Anonymous Function

**Definition**: Function without a name, defined inline using `fn` or `&` syntax.

**Example**:

```elixir
add = fn a, b -> a + b end
add.(1, 2)  # 3

double = &(&1 * 2)
double.(5)  # 10
```

**Note**: Requires dot (`.`) when calling.

**See also**: [Quick Start - Functions and Modules](/en/learn/software-engineering/programming-language/elixir/tutorials/quick-start#section-3-functions-and-modules)

---

### Atom

**Definition**: Named constant where the name is the value. Stored once in memory.

**Example**:

```elixir
:ok
:error
:my_atom
:"atom with spaces"
```

**Common uses**: Tags, status codes, module names, booleans (`true`, `false`, `nil` are atoms).

---

### Behaviour

**Definition**: Interface definition requiring implementations to provide specific callbacks.

**Example**:

```elixir
defmodule Parser do
  @callback parse(String.t()) :: {:ok, term()} | {:error, String.t()}
end

defmodule JSONParser do
  @behaviour Parser
  def parse(json), do: Jason.decode(json)
end
```

**Built-in behaviours**: GenServer, Supervisor, Application.

**See also**: [Beginner Tutorial - Protocols and Behaviours](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#section-11-protocols-and-behaviours)

---

### Charlist

**Definition**: List of Unicode code points, represented with single quotes.

**Example**:

```elixir
'hello'              # [104, 101, 108, 108, 111]
'hello' == [104, 101, 108, 108, 111]  # true
```

**Difference from string**: Strings use double quotes and are binaries.

**When to use**: Erlang interoperability (many Erlang libraries expect charlists).

---

### Comprehension

**Definition**: Syntax for generating lists, maps, or binaries from enumerables with optional filters.

**Example**:

```elixir
for x <- 1..5, do: x * 2
# [2, 4, 6, 8, 10]

for x <- 1..10, rem(x, 2) == 0, do: x
# [2, 4, 6, 8, 10]

for {k, v} <- %{a: 1, b: 2}, into: %{}, do: {k, v * 2}
# %{a: 2, b: 4}
```

**See also**: [Beginner Tutorial - Comprehensions](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#section-14-comprehensions-advanced)

---

### Guard

**Definition**: Boolean expression in function clauses that must be true for clause to match.

**Example**:

```elixir
def check(x) when is_integer(x) and x > 0, do: :positive
def check(x) when x == 0, do: :zero
def check(x) when x < 0, do: :negative
```

**Allowed in guards**: Type checks (`is_*`), comparisons, arithmetic, specific functions.

**Not allowed**: User-defined functions, most standard library functions.

**See also**: [Beginner Tutorial - Pattern Matching](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#section-1-pattern-matching-advanced)

---

### Immutability

**Definition**: Values cannot be modified after creation. Operations return new values.

**Example**:

```elixir
list = [1, 2, 3]
new_list = [0 | list]  # Creates new list
# list is still [1, 2, 3]
# new_list is [0, 1, 2, 3]
```

**Benefits**: Thread safety, predictability, easier reasoning.

---

### Keyword List

**Definition**: List of two-element tuples where first element is an atom. Special syntax support.

**Example**:

```elixir
[name: "Alice", age: 30]
# Actually [{:name, "Alice"}, {:age, 30}]

# Access
opts[:name]  # "Alice"

# Duplicates allowed (unlike maps)
[color: :red, color: :blue]  # Valid
```

**Common use**: Function options.

**See also**: [Beginner Tutorial - Data Structures](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#section-2-data-structures-complete)

---

### Macro

**Definition**: Code that generates code at compile time using AST manipulation.

**Example**:

```elixir
defmodule Unless do
  defmacro unless(condition, clauses) do
    quote do
      if !unquote(condition), do: unquote(clauses[:do])
    end
  end
end
```

**Use cases**: DSLs, code generation, compile-time optimization.

**See also**: [Advanced Tutorial - Metaprogramming and Macros](/en/learn/software-engineering/programming-language/elixir/tutorials/advanced#section-4-metaprogramming-and-macros)

---

### Map

**Definition**: Key-value data structure. Keys can be any type.

**Example**:

```elixir
%{"name" => "Alice", "age" => 30}  # String keys
%{name: "Alice", age: 30}          # Atom keys
%{1 => "one", 2 => "two"}          # Integer keys

# Access
map[:name]    # Atom key
map["name"]   # String key
map.name      # Atom key only
```

**See also**: [Beginner Tutorial - Data Structures](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#section-2-data-structures-complete)

---

### Module

**Definition**: Container for functions and macros. Unit of code organization.

**Example**:

```elixir
defmodule Math do
  def add(a, b), do: a + b
  defp internal, do: :private
end

Math.add(1, 2)  # 3
```

**Module attributes**: `@moduledoc`, `@doc`, custom attributes.

**See also**: [Quick Start - Functions and Modules](/en/learn/software-engineering/programming-language/elixir/tutorials/quick-start#section-3-functions-and-modules)

---

### Pattern Matching

**Definition**: Mechanism to match data structures against patterns and bind variables.

**Example**:

```elixir
{:ok, value} = {:ok, 42}  # value = 42
[head | tail] = [1, 2, 3]  # head = 1, tail = [2, 3]

case File.read("file.txt") do
  {:ok, content} -> process(content)
  {:error, reason} -> handle_error(reason)
end
```

**Pin operator**: `^x` matches existing value of `x`.

**See also**: [Quick Start - Pattern Matching](/en/learn/software-engineering/programming-language/elixir/tutorials/quick-start#section-1-pattern-matching)

---

### Pipe Operator

**Definition**: `|>` operator that passes result of left expression as first argument to right function.

**Example**:

```elixir
# Without pipe
String.upcase(String.trim("  hello  "))

# With pipe
"  hello  "
|> String.trim()
|> String.upcase()
# "HELLO"
```

**Convention**: Enables readable data transformation pipelines.

**See also**: [Quick Start - Pipe Operator](/en/learn/software-engineering/programming-language/elixir/tutorials/quick-start#section-4-pipe-operator)

---

### Process

**Definition**: Lightweight, isolated unit of concurrency managed by BEAM VM.

**Example**:

```elixir
pid = spawn(fn -> IO.puts("Hello from process") end)
send(pid, :message)
receive do
  :message -> IO.puts("Received")
end
```

**Characteristics**: Isolated memory, communicate via messages, lightweight (thousands/millions possible).

**See also**: [Beginner Tutorial - Processes and OTP Basics](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#section-9-processes-and-otp-basics)

---

### Protocol

**Definition**: Mechanism for polymorphism. Define behavior implemented for different types.

**Example**:

```elixir
defprotocol Stringify do
  def to_string(data)
end

defimpl Stringify, for: Integer do
  def to_string(int), do: Integer.to_string(int)
end

defimpl Stringify, for: List do
  def to_string(list), do: Enum.join(list, ", ")
end
```

**Built-in protocols**: Enumerable, Collectable, Inspect, String.Chars.

**See also**: [Beginner Tutorial - Protocols and Behaviours](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#section-11-protocols-and-behaviours)

---

### Recursion

**Definition**: Function calling itself. Primary iteration mechanism in functional programming.

**Example**:

```elixir
def sum([]), do: 0
def sum([head | tail]), do: head + sum(tail)

# Tail recursive (optimized)
def sum(list, acc \\ 0)
def sum([], acc), do: acc
def sum([head | tail], acc), do: sum(tail, head + acc)
```

**Tail recursion**: Last operation is recursive call. Optimized by compiler (no stack growth).

**See also**: [Quick Start - Lists and Recursion](/en/learn/software-engineering/programming-language/elixir/tutorials/quick-start#section-5-lists-and-recursion)

---

### Sigil

**Definition**: Textual construct with `~` prefix for alternative syntax.

**Example**:

```elixir
~s(hello)              # String
~r/\d+/                # Regex
~w(foo bar baz)        # Word list: ["foo", "bar", "baz"]
~w(foo bar baz)a       # Atom list: [:foo, :bar, :baz]
~D[2024-12-21]         # Date
```

**Uppercase sigils**: No interpolation or escape sequences.

---

### Stream

**Definition**: Lazy enumerable. Computations deferred until consumed.

**Example**:

```elixir
# Lazy (not evaluated yet)
stream = Stream.map([1, 2, 3], &(&1 * 2))

# Evaluated when consumed
Enum.to_list(stream)  # [2, 4, 6]

# Infinite streams
Stream.cycle([1, 2, 3])
|> Stream.take(5)
|> Enum.to_list()  # [1, 2, 3, 1, 2]
```

**Use case**: Large/infinite sequences, pipeline optimization.

**See also**: [Beginner Tutorial - Enum and Stream](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#section-5-enum-and-stream-for-collections)

---

### Struct

**Definition**: Extension of maps with compile-time guarantees and default values.

**Example**:

```elixir
defmodule User do
  defstruct name: "", age: 0, email: nil
end

user = %User{name: "Alice", age: 30}
user.name  # "Alice"

# Pattern match
%User{name: name} = user
```

**Constraints**: Keys are atoms, known at compile time.

**See also**: [Beginner Tutorial - Data Structures](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#section-2-data-structures-complete)

---

### Tuple

**Definition**: Fixed-size collection stored contiguously in memory.

**Example**:

```elixir
{:ok, 42}
{:error, "not found"}
{1, 2, 3}

# Pattern matching
{:ok, value} = {:ok, 42}

# Access
elem({:ok, 42}, 1)  # 42
```

**Common pattern**: Tagged tuples (`:ok`/`:error`) for function results.

---

## OTP Concepts

### Application

**Definition**: OTP component grouping related modules, including supervision tree and configuration.

**Example**:

```elixir
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      MyApp.Supervisor
    ]
    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

**Structure**: Defined in `mix.exs`, started at runtime.

**See also**: [Intermediate Tutorial - Application Structure](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#section-3-application-structure)

---

### ETS (Erlang Term Storage)

**Definition**: In-memory key-value store providing fast access with atomic operations.

**Example**:

```elixir
table = :ets.new(:my_table, [:set, :public])
:ets.insert(table, {:key, "value"})
:ets.lookup(table, :key)  # [{:key, "value"}]
```

**Table types**: `:set`, `:ordered_set`, `:bag`, `:duplicate_bag`.

**Use case**: Fast caching, shared state across processes.

---

### GenServer

**Definition**: Generic server behavior abstracting client-server pattern with state management.

**Example**:

```elixir
defmodule Counter do
  use GenServer

  def start_link(initial), do: GenServer.start_link(__MODULE__, initial)
  def increment(pid), do: GenServer.call(pid, :increment)

  def init(initial), do: {:ok, initial}
  def handle_call(:increment, _from, state), do: {:reply, state + 1, state + 1}
end
```

**Callbacks**: `init/1`, `handle_call/3`, `handle_cast/2`, `handle_info/2`, `terminate/2`.

**See also**: [Intermediate Tutorial - GenServer](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#section-1-genserver-for-state-management)

---

### OTP (Open Telecom Platform)

**Definition**: Framework for building fault-tolerant, distributed systems. Includes behaviors, supervision, applications.

**Components**:

- Behaviors: GenServer, Supervisor, Application
- Supervision trees: Fault tolerance
- Applications: Code organization
- Releases: Deployment packages

**Philosophy**: "Let it crash" - supervisors restart failed processes.

---

### Process Registry

**Definition**: Mechanism to register processes with names for easy lookup.

**Example**:

```elixir
# Local registration
GenServer.start_link(Counter, 0, name: Counter)
GenServer.call(Counter, :increment)

# Custom registry
{:ok, _} = Registry.start_link(keys: :unique, name: MyRegistry)
Registry.register(MyRegistry, :my_key, nil)
```

**Types**: Local (`:name`), global (`{:global, name}`), via (`{:via, Registry, ...}`).

---

### Supervisor

**Definition**: Process that monitors child processes and restarts them on failure.

**Example**:

```elixir
defmodule MyApp.Supervisor do
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

  def init(:ok) do
    children = [
      {Counter, 0},
      {Worker, []}
    ]
    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

**Strategies**: `:one_for_one`, `:one_for_all`, `:rest_for_one`.

**See also**: [Intermediate Tutorial - Supervisor](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#section-2-supervisor-for-fault-tolerance)

---

### Supervision Tree

**Definition**: Hierarchical structure of supervisors and workers defining fault tolerance strategy.

**Example**:

```
Application
  └─ Supervisor (one_for_one)
      ├─ Worker1
      ├─ Worker2
      └─ Supervisor (rest_for_one)
          ├─ Worker3
          └─ Worker4
```

**Principle**: Separate error handling from business logic via supervision.

**See also**: [Intermediate Tutorial - Supervisor](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#section-2-supervisor-for-fault-tolerance)

---

### Task

**Definition**: Abstraction for spawning processes for concurrent work without complex state.

**Example**:

```elixir
# Fire and forget
Task.start(fn -> IO.puts("Background work") end)

# Async/await
task = Task.async(fn -> expensive_computation() end)
result = Task.await(task)
```

**Supervised tasks**: `Task.Supervisor` for fault tolerance.

**See also**: [Intermediate Tutorial - Task and Agent](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#section-4-task-and-agent-for-concurrent-tasks)

---

## Phoenix Framework

### Channel

**Definition**: Real-time communication abstraction over WebSockets for bidirectional messaging.

**Example**:

```elixir
defmodule MyAppWeb.RoomChannel do
  use Phoenix.Channel

  def join("room:lobby", _payload, socket) do
    {:ok, socket}
  end

  def handle_in("new_msg", %{"body" => body}, socket) do
    broadcast(socket, "new_msg", %{body: body})
    {:noreply, socket}
  end
end
```

**Use case**: Chat, notifications, real-time updates.

---

### Context

**Definition**: Module grouping related functionality, providing API boundary.

**Example**:

```elixir
defmodule MyApp.Accounts do
  def list_users, do: Repo.all(User)
  def get_user!(id), do: Repo.get!(User, id)
  def create_user(attrs), do: %User{} |> User.changeset(attrs) |> Repo.insert()
end
```

**Convention**: `lib/my_app/accounts/` with `accounts.ex` as API module.

**See also**: [Intermediate Tutorial - Phoenix Framework](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#section-5-phoenix-framework-introduction)

---

### Controller

**Definition**: Handles HTTP requests, invokes business logic, renders responses.

**Example**:

```elixir
defmodule MyAppWeb.UserController do
  use MyAppWeb, :controller

  def index(conn, _params) do
    users = Accounts.list_users()
    render(conn, :index, users: users)
  end
end
```

**Pattern**: Thin controllers, business logic in contexts.

---

### Ecto

**Definition**: Database wrapper providing schema definition, queries, changesets, migrations.

**Example**:

```elixir
# Schema
defmodule User do
  use Ecto.Schema
  schema "users" do
    field :name, :string
    field :age, :integer
  end
end

# Query
import Ecto.Query
query = from u in User, where: u.age > 18, select: u
Repo.all(query)
```

**See also**: [Intermediate Tutorial - Ecto for Databases](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#section-7-ecto-for-databases)

---

### Endpoint

**Definition**: Entry point for HTTP requests. Defines plugs, routing, configuration.

**Example**:

```elixir
defmodule MyAppWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :my_app

  plug Plug.Static, at: "/", from: :my_app
  plug Plug.RequestId
  plug MyAppWeb.Router
end
```

**Responsibilities**: Request pipeline, WebSocket handling, static files.

---

### LiveView

**Definition**: Server-rendered real-time interfaces without JavaScript frameworks.

**Example**:

```elixir
defmodule MyAppWeb.CounterLive do
  use Phoenix.LiveView

  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  def handle_event("increment", _value, socket) do
    {:noreply, update(socket, :count, &(&1 + 1))}
  end

  def render(assigns) do
    ~H"""
    <div>Count: <%= @count %></div>
    <button phx-click="increment">+</button>
    """
  end
end
```

**See also**: [Intermediate Tutorial - LiveView](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate#section-6-liveview-for-real-time-interfaces)

---

### Plug

**Definition**: Composable module for building web applications. Specification for request/response transformation.

**Example**:

```elixir
defmodule MyPlug do
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, _opts) do
    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(200, "Hello, World!")
  end
end
```

**Types**: Function plugs, module plugs.

**Use case**: Authentication, logging, request transformation.

---

### PubSub

**Definition**: Publish-subscribe messaging system for distributed real-time communication.

**Example**:

```elixir
# Subscribe
Phoenix.PubSub.subscribe(MyApp.PubSub, "topic:updates")

# Publish
Phoenix.PubSub.broadcast(MyApp.PubSub, "topic:updates", {:new_data, data})

# Receive
def handle_info({:new_data, data}, socket) do
  {:noreply, assign(socket, data: data)}
end
```

**Use case**: LiveView updates, distributed events.

---

### Router

**Definition**: Maps HTTP requests to controller actions and LiveView modules.

**Example**:

```elixir
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  scope "/", MyAppWeb do
    pipe_through :browser

    get "/", PageController, :index
    live "/counter", CounterLive
    resources "/users", UserController
  end
end
```

**Features**: Scopes, pipelines, resources, live routes.

---

## BEAM VM Concepts

### BEAM (Bogdan/Björn's Erlang Abstract Machine)

**Definition**: Virtual machine executing Erlang and Elixir code with focus on concurrency and fault tolerance.

**Characteristics**:

- Preemptive scheduling
- Lightweight processes
- Hot code swapping
- Distribution support
- Garbage collection per process

**See also**: [Advanced Tutorial - BEAM VM Internals](/en/learn/software-engineering/programming-language/elixir/tutorials/advanced#section-2-beam-vm-internals)

---

### Distribution

**Definition**: Running Elixir nodes on multiple machines with transparent message passing.

**Example**:

```elixir
# Start node
iex --sname node1

# Connect nodes
Node.connect(:"node2@hostname")

# Send messages
pid = Node.spawn(:"node2@hostname", fn -> IO.puts("Remote") end)
```

**See also**: [Advanced Tutorial - Distributed Elixir](/en/learn/software-engineering/programming-language/elixir/tutorials/advanced#section-3-distributed-elixir)

---

### Garbage Collection

**Definition**: Per-process memory management. Each process has independent heap and GC.

**Benefit**: GC pauses isolated to individual processes, not entire system.

**See also**: [Advanced Tutorial - BEAM VM Internals](/en/learn/software-engineering/programming-language/elixir/tutorials/advanced#section-2-beam-vm-internals)

---

### Hot Code Swapping

**Definition**: Replacing module code in running system without downtime.

**Example**:

```elixir
# Load new version
:code.load_file(MyModule)

# Old code continues until processes finish
# New processes use new code
```

**Use case**: Zero-downtime deployments.

---

### Node

**Definition**: Running instance of BEAM VM, identified by name.

**Example**:

```elixir
Node.self()                    # Current node name
Node.list()                    # Connected nodes
Node.connect(:"other@host")    # Connect to node
```

---

### Preemptive Scheduling

**Definition**: Scheduler interrupts processes after fixed reductions, ensuring fair CPU distribution.

**Benefit**: Single process cannot monopolize scheduler. Predictable latency.

**See also**: [Advanced Tutorial - BEAM VM Internals](/en/learn/software-engineering/programming-language/elixir/tutorials/advanced#section-2-beam-vm-internals)

---

### Process Heap

**Definition**: Private memory region for each process. No shared memory between processes.

**Benefit**: Isolated GC, no lock contention, safe concurrency.

---

## Build and Tooling

### Dialyzer

**Definition**: Static analysis tool detecting type errors and inconsistencies.

**Example**:

```bash
mix dialyzer
```

**Setup**: Requires PLT (Persistent Lookup Table) build.

**See also**: [Advanced Tutorial - Type System](/en/learn/software-engineering/programming-language/elixir/tutorials/advanced#section-1-type-system-elixir-119)

---

### ExDoc

**Definition**: Documentation generator producing HTML docs from module/function documentation.

**Example**:

```elixir
@moduledoc """
User management functions.
"""

@doc """
Creates a new user.
"""
def create_user(attrs), do: ...
```

**Command**: `mix docs`

---

### ExUnit

**Definition**: Built-in testing framework.

**Example**:

```elixir
defmodule MathTest do
  use ExUnit.Case

  test "addition" do
    assert Math.add(1, 2) == 3
  end
end
```

**See also**: [Beginner Tutorial - Testing with ExUnit](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#section-8-testing-with-exunit)

---

### Hex

**Definition**: Package manager for Elixir and Erlang ecosystems.

**Commands**:

```bash
mix hex.search phoenix
mix hex.info phoenix
```

**Website**: [hex.pm](https://hex.pm)

---

### IEx (Interactive Elixir)

**Definition**: Interactive shell for Elixir. REPL for experimentation.

**Example**:

```bash
iex
iex> 1 + 2
3
iex> h Enum.map
# Documentation displayed
```

**See also**: [Initial Setup - IEx Basics](/en/learn/software-engineering/programming-language/elixir/tutorials/initial-setup#section-4-interactive-elixir-iex)

---

### Mix

**Definition**: Build tool for creating, compiling, testing, managing dependencies.

**Commands**:

```bash
mix new my_app          # Create project
mix compile             # Compile
mix test                # Run tests
mix deps.get            # Fetch dependencies
```

**See also**: [Quick Start - Mix Project Basics](/en/learn/software-engineering/programming-language/elixir/tutorials/quick-start#section-10-mix-project-basics)

---

### Mix Release

**Definition**: Self-contained production package with BEAM, application code, dependencies.

**Example**:

```bash
mix release
_build/prod/rel/my_app/bin/my_app start
```

**See also**: [Advanced Tutorial - Production Deployment](/en/learn/software-engineering/programming-language/elixir/tutorials/advanced#section-7-production-deployment)

---

Ready to dive deeper? Explore [Tutorials](/en/learn/software-engineering/programming-language/elixir/tutorials/) for comprehensive learning or browse [How-To Guides](/en/learn/software-engineering/programming-language/elixir/how-to/) for practical solutions.
