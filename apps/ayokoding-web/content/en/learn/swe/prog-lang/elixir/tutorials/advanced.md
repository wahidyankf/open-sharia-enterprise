---
title: Advanced Elixir
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000005
description: Master-level Elixir with BEAM VM internals, distributed systems, metaprogramming, macros, performance optimization, and deployment strategies
tags:
  [
    "elixir",
    "tutorial",
    "advanced",
    "beam-vm",
    "distributed",
    "metaprogramming",
    "macros",
    "performance",
    "deployment",
  ]
---

**Want to master Elixir for distributed, high-performance systems?** This tutorial covers BEAM VM internals, distributed Elixir, metaprogramming with macros, performance optimization, and production deployment.

## Coverage

This tutorial covers **85-95%** of Elixir knowledge - master-level topics for distributed systems and advanced optimization.

## Prerequisites

- [Intermediate Tutorial](/en/learn/swe/prog-lang/elixir/tutorials/intermediate) complete
- Strong understanding of OTP (GenServer, Supervisor, Application)
- Experience building Phoenix applications
- Proficiency with Ecto and testing
- Production deployment experience helpful

## Learning Outcomes

By the end of this tutorial, you will:

- Understand BEAM VM architecture (scheduler, process model, garbage collection)
- Build distributed Elixir systems with nodes and clustering
- Master metaprogramming with macros and compile-time code generation
- Optimize performance with profiling and benchmarking tools
- Handle umbrella projects for monorepo development
- Deploy Elixir releases to production
- Implement advanced OTP patterns (Registry, DynamicSupervisor, PartitionSupervisor)
- Debug production issues with observability tools

---

## Learning Path

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A[Type System 1.19+ ⭐] --> B[BEAM VM Internals ⭐]
    B --> C[Distributed Elixir ⭐]
    C --> D[Metaprogramming & Macros ⭐]
    D --> E[Performance Optimization]
    E --> F[Profiling & Benchmarking]
    F --> G[Umbrella Projects]
    G --> H[Production Deployment]
    H --> I[Observability]

    style A fill:#DE8F05,stroke:#000000,stroke-width:3px,color:#000000
    style B fill:#DE8F05,stroke:#000000,stroke-width:3px,color:#000000
    style C fill:#DE8F05,stroke:#000000,stroke-width:3px,color:#000000
    style D fill:#DE8F05,stroke:#000000,stroke-width:3px,color:#000000
```

**Color Palette**: Orange (#DE8F05 - critical sections for advanced Elixir)

**⭐ Most important sections**: Type System (1.19+), BEAM VM, Distributed Elixir, and Metaprogramming - unique strengths of Elixir!

---

## Section 1: Type System (Elixir 1.19+)

Elixir 1.19 introduced set-theoretic types for enhanced compile-time checking.

### Understanding Set-Theoretic Types

Set-theoretic types treat types as sets of values:

```elixir
# Traditional dynamic typing
def add(a, b) do
  a + b
end

# With type annotations (Elixir 1.19+)
def add(a :: integer(), b :: integer()) :: integer() do
  a + b
end
```

**Type System Benefits**:

- **Compile-time warnings**: Catch type errors before runtime
- **Better documentation**: Types serve as inline documentation
- **IDE support**: Improved autocomplete and refactoring
- **Gradual typing**: Add types incrementally (not required)

### Basic Type Annotations

```elixir
defmodule Calculator do
  # Function with type specs
  @spec add(integer(), integer()) :: integer()
  def add(a, b), do: a + b

  @spec divide(integer(), integer()) :: {:ok, float()} | {:error, String.t()}
  def divide(_a, 0), do: {:error, "Division by zero"}
  def divide(a, b), do: {:ok, a / b}

  # Multiple return types (union)
  @spec process(map()) :: {:ok, String.t()} | {:error, atom()}
  def process(%{status: :success} = data) do
    {:ok, data.message}
  end
  def process(_), do: {:error, :invalid_data}
end
```

### Type Checking with Dialyzer

```elixir
# mix.exs
defp deps do
  [
    {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false}
  ]
end

# Run type checking
mix dialyzer
```

**Example type error**:

```elixir
defmodule Broken do
  @spec add(integer(), integer()) :: integer()
  def add(a, b) do
    # Dialyzer error: return type is float, not integer
    a / b
  end
end
```

### Enhanced Type Checking (Elixir 1.19+)

**Improved inference for pattern matching**:

```elixir
defmodule Enhanced do
  # Compiler infers tighter types from pattern matching
  def process({:ok, value}) when is_integer(value) do
    # Compiler knows value is integer here
    value * 2
  end

  def process({:error, reason}) when is_binary(reason) do
    # Compiler knows reason is string here
    String.upcase(reason)
  end

  def process(_) do
    :unknown
  end
end
```

**Union types with guards**:

```elixir
defmodule TypeGuards do
  @type result :: {:ok, integer()} | {:error, String.t()}

  @spec double(result()) :: result()
  def double({:ok, n}) when is_integer(n), do: {:ok, n * 2}
  def double({:error, msg}) when is_binary(msg), do: {:error, msg}
end
```

### Set-Theoretic Type Operations

**Intersection types**:

```elixir
# Type that is both a map and has specific keys
@type user :: %{name: String.t(), age: integer()} & map()

# Function accepts any map with at least these keys
@spec greet(user()) :: String.t()
def greet(%{name: name}), do: "Hello, #{name}!"
```

**Negation types**:

```elixir
# Type that excludes nil
@type non_nil_string :: String.t() and not nil

@spec upcase(non_nil_string()) :: non_nil_string()
def upcase(str) when is_binary(str), do: String.upcase(str)
```

### Compiler Diagnostics (Elixir 1.19+)

Elixir 1.19 provides better error messages:

```elixir
defmodule DiagnosticsExample do
  def broken do
    # Better error: suggests you meant String.upcase/1
    String.uppercase("hello")
  end

  def wrong_type do
    x = 5
    # Better error: shows x is integer, can't be used as string
    String.length(x)
  end
end
```

**Enhanced warnings**:

```elixir
# Unused variable warning
def unused(a, b) do
  a + a  # Warning: variable b is unused
end

# Unreachable code warning
def unreachable do
  if true do
    :always_true
  else
    :never_reached  # Warning: unreachable code
  end
end
```

### Advanced Type Specs

**Generic types**:

```elixir
defmodule Container do
  @type t(a) :: %__MODULE__{value: a}

  defstruct [:value]

  @spec new(a) :: t(a) when a: any()
  def new(value), do: %__MODULE__{value: value}

  @spec map(t(a), (a -> b)) :: t(b) when a: any(), b: any()
  def map(%__MODULE__{value: v}, fun) do
    %__MODULE__{value: fun.(v)}
  end
end

# Usage
Container.new(5) |> Container.map(&(&1 * 2))  # %Container{value: 10}
```

**Opaque types**:

```elixir
defmodule SecureToken do
  @opaque t :: String.t()

  @spec generate() :: t()
  def generate do
    :crypto.strong_rand_bytes(32) |> Base.encode64()
  end

  @spec verify(t(), t()) :: boolean()
  def verify(token1, token2) do
    token1 == token2
  end
end

# Outside module can't inspect token internals
token = SecureToken.generate()
# This would be a Dialyzer error:
# String.length(token)  # t() is opaque, not known to be String.t()
```

**Behaviours with typespecs**:

```elixir
defmodule Storage do
  @callback put(key :: String.t(), value :: any()) :: :ok | {:error, term()}
  @callback get(key :: String.t()) :: {:ok, any()} | {:error, :not_found}
  @callback delete(key :: String.t()) :: :ok
end

defmodule MemoryStorage do
  @behaviour Storage

  @impl Storage
  @spec put(String.t(), any()) :: :ok
  def put(key, value) do
    # Implementation
    :ok
  end

  @impl Storage
  @spec get(String.t()) :: {:ok, any()} | {:error, :not_found}
  def get(key) do
    # Implementation
    {:error, :not_found}
  end

  @impl Storage
  @spec delete(String.t()) :: :ok
  def delete(_key), do: :ok
end
```

### Performance Impact of Type Checking

**Compilation performance** (Elixir 1.19 improvement):

```elixir
# Measure compilation time
time_before = :os.system_time(:millisecond)
Code.compile_file("lib/my_large_module.ex")
time_after = :os.system_time(:millisecond)

IO.puts("Compiled in #{time_after - time_before}ms")
```

**Elixir 1.19 compilation improvements**:

- 4x faster compilation on average
- Incremental compilation optimizations
- Parallel module compilation
- Reduced memory usage during compilation

**Type checking does NOT affect runtime**:

```elixir
# Types erased at runtime
@spec slow_function(integer()) :: integer()
def slow_function(n) do
  # No runtime overhead from type specs
  :timer.sleep(1000)
  n * 2
end
```

### Gradual Typing Strategy

**Start with critical functions**:

```elixir
defmodule GradualTyping do
  # Public API: Add types
  @spec create_user(map()) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  def create_user(attrs) do
    # Internal helper: No types yet
    validate_attrs(attrs)
  end

  # Private: Types optional
  defp validate_attrs(attrs) do
    # Implementation
  end
end
```

**Incremental adoption**:

1. Add types to public API first
2. Add types to modules with complex logic
3. Add types to frequently changed modules
4. Let Dialyzer guide you to remaining issues

**Pragmatic type specs**:

```elixir
# Over-specified (too rigid)
@spec process(String.t(), integer(), boolean(), atom()) :: :ok
def process(str, num, flag, type) do
  # Hard to maintain
end

# Under-specified (too loose)
@spec process(any(), any(), any(), any()) :: any()
def process(str, num, flag, type) do
  # Loses type safety
end

# Balanced (useful types)
@spec process(String.t(), pos_integer(), opts :: keyword()) :: :ok | {:error, term()}
def process(str, num, opts) do
  # Clear intent, practical constraints
end
```

### Best Practices

**Do**:

- ✅ Type public APIs and exported functions
- ✅ Use specific types (`:ok | {:error, String.t()}` vs `any()`)
- ✅ Run Dialyzer in CI/CD pipeline
- ✅ Document complex types with `@typedoc`
- ✅ Use opaque types for internal data structures

**Don't**:

- ❌ Over-specify every private function (diminishing returns)
- ❌ Use `any()` everywhere (defeats purpose)
- ❌ Ignore Dialyzer warnings (fix or suppress explicitly)
- ❌ Fight the type system (Elixir is dynamically typed at core)

---

## Section 2: BEAM VM Internals

Understanding the BEAM VM unlocks Elixir's concurrency and fault tolerance.

### Process Model

Every Elixir process runs on the BEAM VM:

```elixir
# Spawn a process
pid = spawn(fn ->
  receive do
    {:hello, sender} -> send(sender, :world)
  end
end)

# Send message
send(pid, {:hello, self()})

# Receive response
receive do
  :world -> IO.puts("Received world!")
end
```

**Process Characteristics**:

- Lightweight: 2-3 KB per process (can spawn millions)
- Isolated: No shared memory, communicate via messages
- Garbage collected independently
- Preemptively scheduled by BEAM scheduler

### Scheduler Model

BEAM uses **M:N scheduling** - multiple processes on multiple schedulers:

```elixir
# Number of schedulers (usually = CPU cores)
IO.inspect(System.schedulers_online())  # 8 on 8-core machine

# Process info
pid = spawn(fn -> :timer.sleep(5000) end)
Process.info(pid, :current_stacktrace)
Process.info(pid, :reductions)  # Work done by process
```

**How Scheduling Works**:

1. Each scheduler has a run queue of processes
2. Scheduler runs process for ~2000 reductions (instructions)
3. Process yields (I/O, receive, explicit yield) or preempted
4. Scheduler picks next process from queue
5. Work stealing balances load across schedulers

**Visualizing Schedulers**:

```
CPU Cores:        1       2       3       4
Schedulers:    Sched1  Sched2  Sched3  Sched4
Run Queues:    [P1]    [P4]    [P7]    [P10]
               [P2]    [P5]    [P8]    [P11]
               [P3]    [P6]    [P9]    [P12]
```

### Process Heap and Garbage Collection

Each process has its own heap:

```elixir
# Check process memory
pid = spawn(fn ->
  # Allocate large list
  list = Enum.to_list(1..1_000_000)
  :timer.sleep(10_000)
end)

Process.info(pid, :memory)          # Bytes used
Process.info(pid, :heap_size)       # Heap size in words
Process.info(pid, :total_heap_size) # Total heap (including old heap)
```

**Generational GC**:

- **Young heap**: New allocations
- **Old heap**: Data that survived GC
- GC runs independently per process (no stop-the-world)
- Process blocks only itself during GC

```elixir
# Force GC on process
:erlang.garbage_collect(pid)
```

### Message Passing Performance

Messages are copied between processes:

```elixir
defmodule MessageBench do
  def send_small_message(receiver, n) do
    Enum.each(1..n, fn i ->
      send(receiver, i)
    end)
  end

  def send_large_message(receiver, n) do
    large_data = Enum.to_list(1..10_000)
    Enum.each(1..n, fn _i ->
      send(receiver, large_data)
    end)
  end
end

# Small messages: Fast (copying integers is cheap)
# Large messages: Slower (copying 10K item list)
```

**Optimization**: Use ETS for shared data instead of copying:

```elixir
# Create ETS table (shared memory)
table = :ets.new(:shared_data, [:set, :public, :named_table])

# Store large data once
:ets.insert(table, {:data, Enum.to_list(1..10_000)})

# Processes read without copying
[{:data, data}] = :ets.lookup(table, :data)
```

### Process Links and Monitors

**Links**: Bidirectional, crashes propagate:

```elixir
# Link processes
parent = self()
child = spawn_link(fn ->
  :timer.sleep(1000)
  raise "Child crashed!"
end)

# Child crash kills parent unless trapped
Process.flag(:trap_exit, true)

receive do
  {:EXIT, ^child, reason} ->
    IO.puts("Child exited: #{inspect(reason)}")
end
```

**Monitors**: Unidirectional, notification only:

```elixir
child = spawn(fn ->
  :timer.sleep(1000)
  exit(:normal)
end)

ref = Process.monitor(child)

receive do
  {:DOWN, ^ref, :process, ^child, reason} ->
    IO.puts("Child went down: #{inspect(reason)}")
end
```

---

## Section 3: Distributed Elixir

Connect multiple BEAM nodes for distributed systems.

### Starting Nodes

Start named nodes:

```bash
# Terminal 1
iex --name node1@127.0.0.1 --cookie secret

# Terminal 2
iex --name node2@127.0.0.1 --cookie secret
```

**Cookie**: Shared secret for authentication (must match to connect).

### Connecting Nodes

```elixir
# On node1
Node.connect(:"node2@127.0.0.1")

# List connected nodes
Node.list()  # [:"node2@127.0.0.1"]

# List all nodes including self
Node.list([:this, :visible])  # [:"node1@127.0.0.1", :"node2@127.0.0.1"]
```

### Remote Process Spawning

Spawn processes on remote nodes:

```elixir
# On node1
pid = Node.spawn(:"node2@127.0.0.1", fn ->
  IO.puts("Running on #{inspect(Node.self())}")
  :timer.sleep(5000)
end)

# pid runs on node2
```

### Distributed Message Passing

Send messages across nodes:

```elixir
# On node2, register a process
defmodule RemoteServer do
  def start do
    pid = spawn(fn -> loop() end)
    Process.register(pid, :remote_server)
    pid
  end

  defp loop do
    receive do
      {:request, sender, data} ->
        send(sender, {:response, data * 2})
        loop()
    end
  end
end

RemoteServer.start()

# On node1, send message to node2
send({:remote_server, :"node2@127.0.0.1"}, {:request, self(), 42})

receive do
  {:response, result} -> IO.puts("Got: #{result}")  # 84
end
```

### Distributed GenServer

```elixir
defmodule DistributedCounter do
  use GenServer

  # Start on specific node
  def start_link(node, initial_value) do
    Node.spawn_link(node, fn ->
      GenServer.start_link(__MODULE__, initial_value, name: __MODULE__)
    end)
  end

  # Call across nodes
  def increment(node) do
    GenServer.call({__MODULE__, node}, :increment)
  end

  def get_value(node) do
    GenServer.call({__MODULE__, node}, :get_value)
  end

  @impl true
  def init(initial_value) do
    {:ok, initial_value}
  end

  @impl true
  def handle_call(:increment, _from, state) do
    new_state = state + 1
    {:reply, new_state, new_state}
  end

  @impl true
  def handle_call(:get_value, _from, state) do
    {:reply, state, state}
  end
end

# On node1
DistributedCounter.start_link(:"node2@127.0.0.1", 0)
DistributedCounter.increment(:"node2@127.0.0.1")  # 1
DistributedCounter.get_value(:"node2@127.0.0.1")  # 1
```

### Global Registry

Register processes globally across cluster:

```elixir
# On node1
defmodule GlobalService do
  def start_link do
    pid = spawn(fn -> loop() end)
    :global.register_name(:my_service, pid)
    pid
  end

  defp loop do
    receive do
      {:ping, sender} ->
        send(sender, :pong)
        loop()
    end
  end
end

GlobalService.start_link()

# On node2
pid = :global.whereis_name(:my_service)
send(pid, {:ping, self()})
receive do
  :pong -> IO.puts("Global service responded!")
end
```

### Distributed PubSub (Phoenix.PubSub)

```elixir
# Add to application supervision tree
children = [
  {Phoenix.PubSub, name: MyApp.PubSub}
]

# Subscribe on node1
Phoenix.PubSub.subscribe(MyApp.PubSub, "events")

# Publish on node2
Phoenix.PubSub.broadcast(MyApp.PubSub, "events", {:event, "data"})

# Receive on node1
receive do
  {:event, data} -> IO.puts("Received: #{data}")
end
```

### Network Partitions and Split Brain

Handle network partitions:

```elixir
defmodule ClusterMonitor do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    :net_kernel.monitor_nodes(true)
    {:ok, %{nodes: Node.list()}}
  end

  @impl true
  def handle_info({:nodeup, node}, state) do
    IO.puts("Node connected: #{inspect(node)}")
    {:noreply, %{state | nodes: [node | state.nodes]}}
  end

  @impl true
  def handle_info({:nodedown, node}, state) do
    IO.puts("Node disconnected: #{inspect(node)}")
    new_nodes = List.delete(state.nodes, node)
    {:noreply, %{state | nodes: new_nodes}}
  end
end
```

**Best Practices**:

- Use `:global` for small clusters (< 50 nodes)
- Use `pg` (process groups) for larger clusters
- Implement consensus algorithms for critical distributed state
- Handle network partitions explicitly

---

## Section 4: Metaprogramming and Macros

Macros enable compile-time code generation.

### Understanding the AST

Elixir code is represented as Abstract Syntax Tree:

```elixir
# Convert code to AST
quote do
  1 + 2
end
# {:+, [context: Elixir, import: Kernel], [1, 2]}

# AST structure: {function, metadata, arguments}

quote do
  if true, do: "yes", else: "no"
end
# {:if, [context: Elixir, import: Kernel],
#  [true, [do: "yes", else: "no"]]}
```

### Basic Macros

```elixir
defmodule MyMacros do
  defmacro say_hello(name) do
    quote do
      IO.puts("Hello, #{unquote(name)}!")
    end
  end
end

defmodule Test do
  require MyMacros

  def greet do
    MyMacros.say_hello("World")  # Macro expanded at compile time
  end
end

Test.greet()  # "Hello, World!"
```

### Quote and Unquote

- `quote`: Convert code to AST
- `unquote`: Inject values into AST

```elixir
defmodule MathMacros do
  defmacro multiply(a, b) do
    quote do
      unquote(a) * unquote(b)
    end
  end

  # With bind_quoted (prevents multiple evaluation)
  defmacro safe_multiply(a, b) do
    quote bind_quoted: [a: a, b: b] do
      a * b
    end
  end
end

require MathMacros
MathMacros.multiply(2, 3)  # 6

# Problem with multiple evaluation
defmodule Unsafe do
  defmacro double(x) do
    quote do
      unquote(x) + unquote(x)
    end
  end
end

require Unsafe
Unsafe.double(IO.puts("hi"))  # Prints "hi" twice! ❌

# Solution with bind_quoted
defmodule Safe do
  defmacro double(x) do
    quote bind_quoted: [x: x] do
      x + x
    end
  end
end

require Safe
Safe.double(IO.puts("hi"))  # Prints "hi" once ✅
```

### Pattern Matching in Macros

```elixir
defmodule ControlFlow do
  defmacro unless(condition, do: block) do
    quote do
      if !unquote(condition), do: unquote(block)
    end
  end

  defmacro when_ok(expr, do: block) do
    quote do
      case unquote(expr) do
        {:ok, result} -> unquote(block)
        error -> error
      end
    end
  end
end

require ControlFlow

ControlFlow.unless 1 == 2 do
  IO.puts("Math works!")
end

ControlFlow.when_ok {:ok, 42} do
  IO.puts("Success!")
end
```

### Building DSLs with Macros

```elixir
defmodule Router do
  defmacro __using__(_opts) do
    quote do
      import Router
      Module.register_attribute(__MODULE__, :routes, accumulate: true)

      @before_compile Router
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def routes do
        @routes |> Enum.reverse()
      end
    end
  end

  defmacro get(path, handler) do
    quote do
      @routes {:get, unquote(path), unquote(handler)}
    end
  end

  defmacro post(path, handler) do
    quote do
      @routes {:post, unquote(path), unquote(handler)}
    end
  end
end

defmodule MyRouter do
  use Router

  get "/", :index
  get "/about", :about
  post "/users", :create_user
end

MyRouter.routes()
# [
#   {:get, "/", :index},
#   {:get, "/about", :about},
#   {:post, "/users", :create_user}
# ]
```

### Compile-Time Configuration

```elixir
defmodule Config do
  @env Mix.env()
  @api_url if @env == :prod, do: "https://api.example.com", else: "http://localhost:4000"

  def api_url, do: @api_url
end

Config.api_url()  # Determined at compile time
```

### Macro Hygiene

Macros don't leak variables:

```elixir
defmodule Hygienic do
  defmacro set_x do
    quote do
      x = 10
    end
  end
end

require Hygienic
Hygienic.set_x()
x  # ❌ Undefined variable (hygiene prevents leakage)

# Intentionally break hygiene with var!
defmodule NonHygienic do
  defmacro set_x do
    quote do
      var!(x) = 10
    end
  end
end

require NonHygienic
NonHygienic.set_x()
x  # 10 ✅ (var! breaks hygiene)
```

**Best Practices**:

- Use macros sparingly (functions are easier to understand)
- Prefer functions over macros unless compile-time generation needed
- Use `bind_quoted` to prevent multiple evaluation
- Document macro behavior clearly
- Test macro expansion with `Macro.expand/2`

---

## Section 5: Performance Optimization

Optimize Elixir applications for production workloads.

### Profiling with `:fprof`

Profile function calls:

```elixir
defmodule Fibonacci do
  def fib(0), do: 0
  def fib(1), do: 1
  def fib(n), do: fib(n - 1) + fib(n - 2)
end

# Profile fib/1
:fprof.trace([:start])
Fibonacci.fib(20)
:fprof.trace([:stop])

:fprof.profile()
:fprof.analyse(callers: true, sort: :acc, totals: true)
```

Output shows time spent in each function.

### Profiling with `:eprof`

Time-based profiling:

```elixir
:eprof.start()
:eprof.start_profiling([self()])

# Run code to profile
result = expensive_function()

:eprof.stop_profiling()
:eprof.analyze(total: true)
```

### Benchmarking with Benchee

```elixir
# mix.exs
defp deps do
  [
    {:benchee, "~> 1.0", only: :dev}
  ]
end

# Benchmark different implementations
Benchee.run(%{
  "Enum.map" => fn input -> Enum.map(input, &(&1 * 2)) end,
  "for comprehension" => fn input -> for x <- input, do: x * 2 end,
  "Stream.map" => fn input -> input |> Stream.map(&(&1 * 2)) |> Enum.to_list() end
}, inputs: %{
  "Small" => Enum.to_list(1..100),
  "Medium" => Enum.to_list(1..10_000),
  "Large" => Enum.to_list(1..1_000_000)
})
```

### Memory Profiling

Track memory usage:

```elixir
# Check process memory
Process.info(self(), :memory)

# Profile memory allocations
:recon_alloc.memory(:allocated)
:recon_alloc.memory(:used)
```

### Optimization Techniques

**1. Tail Call Optimization**:

```elixir
# Not tail recursive (builds stack)
def sum_slow([]), do: 0
def sum_slow([h | t]), do: h + sum_slow(t)

# Tail recursive (constant stack)
def sum_fast(list), do: sum_fast(list, 0)
defp sum_fast([], acc), do: acc
defp sum_fast([h | t], acc), do: sum_fast(t, acc + h)
```

**2. Lazy Evaluation with Streams**:

```elixir
# Eager (allocates intermediate lists)
result = 1..1_000_000
  |> Enum.map(&(&1 * 2))
  |> Enum.filter(&rem(&1, 3) == 0)
  |> Enum.take(10)

# Lazy (single pass, minimal allocation)
result = 1..1_000_000
  |> Stream.map(&(&1 * 2))
  |> Stream.filter(&rem(&1, 3) == 0)
  |> Enum.take(10)
```

**3. ETS for Shared State**:

```elixir
# Slow: GenServer serializes access
defmodule CacheSlow do
  use GenServer

  def get(key) do
    GenServer.call(__MODULE__, {:get, key})
  end
end

# Fast: ETS allows concurrent reads
defmodule CacheFast do
  def get(key) do
    case :ets.lookup(:cache, key) do
      [{^key, value}] -> value
      [] -> nil
    end
  end
end
```

**4. Avoid String Concatenation in Loops**:

```elixir
# Slow (O(n²) - creates new string each iteration)
def build_slow(n) do
  Enum.reduce(1..n, "", fn i, acc ->
    acc <> Integer.to_string(i)
  end)
end

# Fast (O(n) - iolist, converted once at end)
def build_fast(n) do
  1..n
  |> Enum.map(&Integer.to_string/1)
  |> IO.iodata_to_binary()
end
```

**5. Pattern Match in Function Head**:

```elixir
# Slower (pattern match inside body)
def process(data) do
  case data do
    {:ok, value} -> value * 2
    {:error, _} -> 0
  end
end

# Faster (pattern match in function head)
def process({:ok, value}), do: value * 2
def process({:error, _}), do: 0
```

### Compiler Optimizations (Elixir 1.19+)

Elixir 1.19 introduced 4x faster compilation:

```elixir
# Check compilation time
time = :timer.tc(fn ->
  Code.compile_file("lib/my_app.ex")
end)

IO.puts("Compiled in #{elem(time, 0) / 1000}ms")
```

**Optimizations in 1.19**:

- Parallel compilation of modules
- Incremental compilation improvements
- Faster type checking with set-theoretic types
- Reduced memory usage during compilation

---

## Section 6: Umbrella Projects

Manage monorepos with umbrella projects.

### Creating Umbrella Project

```bash
mix new my_app --umbrella
cd my_app

# Create child apps
cd apps
mix new core
mix new web --sup
mix new workers --sup
```

Structure:

```
my_app/
├── apps/
│   ├── core/        # Business logic
│   ├── web/         # Phoenix app
│   └── workers/     # Background jobs
├── config/
└── mix.exs
```

### Umbrella Configuration

```elixir
# Root mix.exs
defmodule MyApp.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      version: "0.1.0",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  defp deps do
    []
  end
end

# apps/web/mix.exs
defmodule Web.MixProject do
  use Mix.Project

  def project do
    [
      app: :web,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  defp deps do
    [
      {:core, in_umbrella: true},  # Depend on sibling app
      {:phoenix, "~> 1.7"}
    ]
  end
end
```

### Working with Umbrella Apps

```bash
# Run command in all apps
mix compile
mix test

# Run command in specific app
mix cmd --app core mix test

# Start all apps
iex -S mix
```

### Shared Dependencies

Configure shared dependencies in root:

```elixir
# Root mix.exs
defp deps do
  [
    {:jason, "~> 1.4"}  # Shared by all apps
  ]
end

# apps/core/mix.exs
defp deps do
  [
    {:ecto, "~> 3.11"}  # Core-specific
  ]
end
```

---

## Section 7: Production Deployment

Deploy Elixir applications to production.

### Mix Releases

Build production release:

```bash
# Configure release
# mix.exs
def project do
  [
    # ...
    releases: [
      my_app: [
        include_executables_for: [:unix],
        applications: [runtime_tools: :permanent]
      ]
    ]
  ]
end

# Build release
MIX_ENV=prod mix release

# Run release
_build/prod/rel/my_app/bin/my_app start
_build/prod/rel/my_app/bin/my_app daemon  # Background
_build/prod/rel/my_app/bin/my_app stop
```

### Release Configuration

```elixir
# config/runtime.exs
import Config

if config_env() == :prod do
  database_url = System.fetch_env!("DATABASE_URL")
  secret_key_base = System.fetch_env!("SECRET_KEY_BASE")

  config :my_app, MyApp.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")

  config :my_app, MyAppWeb.Endpoint,
    http: [port: String.to_integer(System.get_env("PORT") || "4000")],
    secret_key_base: secret_key_base,
    server: true
end
```

### Docker Deployment

```dockerfile
# Dockerfile
FROM hexpm/elixir:1.19.4-erlang-28.2.5-alpine-3.21.3 AS build

# Install build dependencies
RUN apk add --no-cache build-base git

WORKDIR /app

# Install hex + rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Copy mix files
COPY mix.exs mix.lock ./
RUN mix deps.get --only prod

# Copy source
COPY . .

# Compile
RUN MIX_ENV=prod mix compile

# Build release
RUN MIX_ENV=prod mix release

# Production image
FROM alpine:3.21.3

RUN apk add --no-cache libstdc++ openssl ncurses-libs

WORKDIR /app

COPY --from=build /app/_build/prod/rel/my_app ./

CMD ["bin/my_app", "start"]
```

### Environment Variables

```bash
# .env.prod
export DATABASE_URL="ecto://user:pass@localhost/db"
export SECRET_KEY_BASE="long-secret-key"
export PORT="4000"
export POOL_SIZE="10"

# Load and run
source .env.prod
_build/prod/rel/my_app/bin/my_app start
```

### Health Checks and Graceful Shutdown

```elixir
# lib/my_app_web/controllers/health_controller.ex
defmodule MyAppWeb.HealthController do
  use MyAppWeb, :controller

  def check(conn, _params) do
    json(conn, %{status: "ok", timestamp: DateTime.utc_now()})
  end
end

# Graceful shutdown
# config/config.exs
config :my_app, MyAppWeb.Endpoint,
  shutdown_timeout: 30_000  # 30 seconds for graceful shutdown
```

---

## Section 8: Observability

Monitor production Elixir applications.

### Logger

```elixir
require Logger

Logger.debug("Detailed debug info")
Logger.info("User logged in: #{user_id}")
Logger.warning("Rate limit approaching")
Logger.error("Database connection failed")

# Structured logging
Logger.info("User action",
  user_id: user_id,
  action: "login",
  ip: ip_address
)
```

### Telemetry

```elixir
# Emit custom events
:telemetry.execute(
  [:my_app, :payment, :processed],
  %{amount: 100},
  %{user_id: 123}
)

# Attach handler
:telemetry.attach(
  "payment-logger",
  [:my_app, :payment, :processed],
  fn _event, measurements, metadata, _config ->
    Logger.info("Payment processed",
      amount: measurements.amount,
      user_id: metadata.user_id
    )
  end,
  nil
)
```

### Observer

```elixir
# Start observer (GUI)
:observer.start()
```

**Observer Features**:

- System overview (memory, CPU, processes)
- Process list and details
- Application tree
- ETS table viewer
- Memory allocation

### Recon for Production

```elixir
# Top memory-consuming processes
:recon.proc_count(:memory, 10)

# Top message queue sizes
:recon.proc_count(:message_queue_len, 10)

# Process info
:recon.info(pid)

# Trace function calls (live debugging)
:recon_trace.calls({Module, :function, :_}, 10)
```

---

## Related Content

**Previous Tutorials:**

- [Intermediate Tutorial](/en/learn/swe/prog-lang/elixir/tutorials/intermediate) - OTP and Phoenix
- [Beginner Tutorial](/en/learn/swe/prog-lang/elixir/tutorials/beginner) - Fundamentals

**How-To Guides:**

- [Elixir Cookbook](/en/learn/swe/prog-lang/elixir/how-to/cookbook) - Expert recipes
- [How to Build Distributed Systems](/en/learn/swe/prog-lang/elixir/how-to/build-distributed-systems) - Distribution patterns
- [How to Write Macros](/en/learn/swe/prog-lang/elixir/how-to/write-macros) - Metaprogramming guide
- [How to Optimize Performance](/en/learn/swe/prog-lang/elixir/how-to/optimize-performance) - Performance tuning
- [How to Deploy Elixir Apps](/en/learn/swe/prog-lang/elixir/how-to/deploy-elixir-apps) - Deployment strategies
- [How to Monitor Production](/en/learn/swe/prog-lang/elixir/how-to/monitor-production) - Production monitoring

**Explanations:**

- [Best Practices](/en/learn/swe/prog-lang/elixir/explanation/best-practices) - Expert standards
- [Anti-Patterns](/en/learn/swe/prog-lang/elixir/explanation/anti-patterns) - Advanced pitfalls

**Reference:**

- [Elixir Cheat Sheet](/en/learn/swe/prog-lang/elixir/reference/cheat-sheet) - Complete reference
- [Elixir Glossary](/en/learn/swe/prog-lang/elixir/reference/glossary) - Advanced terms

---

## Next Steps

**Master These Concepts**:

1. **BEAM VM**: Understand process model and scheduler
2. **Distributed Elixir**: Build multi-node systems
3. **Metaprogramming**: Write macros for DSLs
4. **Performance**: Profile and optimize critical paths

**Continue Learning**:

- [How-To Guides](/en/learn/swe/prog-lang/elixir/how-to) - Practical distributed patterns
- [Cookbook](/en/learn/swe/prog-lang/elixir/how-to/cookbook) - Advanced recipes
- [Best Practices](/en/learn/swe/prog-lang/elixir/explanation/best-practices) - Production patterns

**Advanced Projects**:

1. **Distributed Cache**: Multi-node cache with consistency
2. **Job Queue**: Distributed task processing with GenStage
3. **Monitoring System**: Custom telemetry and metrics
4. **DSL**: Build domain-specific language with macros

**Resources**:

- [Erlang Efficiency Guide](https://www.erlang.org/doc/efficiency_guide/users_guide.html)
- [Elixir in Action](https://www.manning.com/books/elixir-in-action-third-edition)
- [Designing Elixir Systems with OTP](https://pragprog.com/titles/jgotp/designing-elixir-systems-with-otp/)
- [Anti-Patterns](/en/learn/swe/prog-lang/elixir/explanation/anti-patterns)

You now have master-level Elixir knowledge for distributed, high-performance systems!
