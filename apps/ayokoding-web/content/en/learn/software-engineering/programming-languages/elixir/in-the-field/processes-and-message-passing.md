---
title: "Processes and Message Passing"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000003
description: "From spawn/send/receive primitives to Task and GenServer for production concurrency"
tags: ["elixir", "processes", "concurrency", "task", "genserver", "otp"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/anti-patterns"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/genserver-patterns"
---

**How do you build concurrent systems in Elixir?** This guide teaches the progression from BEAM's raw process primitives through Task module to GenServer, showing when each abstraction provides production value.

## Why It Matters

Process-based concurrency is Elixir's fundamental model. Unlike threads in other languages, BEAM processes are:

- **Lightweight** - Millions of processes on modest hardware
- **Isolated** - Separate memory, no shared state corruption
- **Fast communication** - Message passing optimized at VM level
- **Fault-tolerant** - Process crashes don't affect others

Real-world scenarios requiring concurrent processes:

- **Financial calculations** - Parallel invoice processing with isolation
- **Background jobs** - Email sending, report generation, data processing
- **API aggregation** - Concurrent external API calls with timeout
- **Real-time features** - Chat messages, notifications, live updates
- **Data pipelines** - ETL workflows with parallel stages

Production question: Should you use raw spawn/send/receive, Task module, or GenServer? The answer depends on your supervision and error handling requirements.

## BEAM Process Primitives

The BEAM VM provides three fundamental primitives for process-based concurrency.

### spawn/1 - Create Process

```elixir
# Raw process creation
pid = spawn(fn ->
  result = calculate_invoice_total(items)   # => Executes calculation
                                             # => Result computed in isolation
  IO.puts("Total: #{result}")                # => Output: Total: 1500
                                             # => Process exits after completion
end)
# => Returns PID (Process Identifier)
# => Type: pid()
# => Process runs independently of caller
```

Process is created, runs function, exits. No return value to caller.

### send/2 - Send Message

```elixir
# Sending message to process
send(pid, {:calculate, items})               # => Sends message to pid's mailbox
                                             # => Returns message (always succeeds)
                                             # => Type: term()
                                             # => No delivery guarantee
```

Messages are asynchronous. `send/2` returns immediately, doesn't wait for processing.

### receive/1 - Receive Message

```elixir
# Receiving message with pattern matching
receive do
  {:calculate, items} ->                     # => Pattern matches incoming message
    total = calculate_total(items)           # => Processes calculation
    {:ok, total}                             # => Returns result
                                             # => Type: {:ok, number()}

  {:error, reason} ->                        # => Matches error messages
    {:error, reason}                         # => Propagates error
                                             # => Type: {:error, term()}
after
  5000 ->                                    # => Timeout after 5 seconds
    {:error, :timeout}                       # => Returns timeout error
                                             # => Type: {:error, :timeout}
end
# => Blocks until message received or timeout
# => Pattern matching determines which clause executes
```

`receive` blocks until matching message arrives or timeout expires.

### Complete Example - Invoice Processing

```elixir
# Financial calculation with process isolation
defmodule InvoiceProcessor do
  def process_invoice(items) do
    parent = self()                          # => Current process PID
                                             # => Type: pid()

    pid = spawn(fn ->
      total = Enum.reduce(items, 0, fn item, acc ->
        acc + item.price * item.quantity     # => Calculate line total
                                             # => Accumulate sum
      end)                                   # => total: Sum of all items

      tax = total * 0.1                      # => 10% tax calculation
                                             # => Type: float()

      final = total + tax                    # => Final invoice amount
                                             # => Type: float()

      send(parent, {:result, final})         # => Send result to parent
                                             # => Returns {:result, final}
    end)                                     # => pid: Worker process PID

    receive do
      {:result, amount} ->                   # => Matches result message
        {:ok, amount}                        # => Returns successful result
                                             # => Type: {:ok, float()}
    after
      5000 ->                                # => 5 second timeout
        {:error, :timeout}                   # => Returns timeout error
    end
  end
end

# Usage
items = [
  %{price: 100, quantity: 2},                # => $200 line item
  %{price: 50, quantity: 1}                  # => $50 line item
]                                            # => items: List of invoice items

{:ok, total} = InvoiceProcessor.process_invoice(items)
# => total: 275.0 (250 + 10% tax)
# => Type: {:ok, float()}
```

This works but has production limitations.

## Limitations of Raw Primitives

Using spawn/send/receive directly creates several production problems.

### Problem 1: No Supervision

```elixir
# Process crashes - no recovery
pid = spawn(fn ->
  raise "Database connection failed"         # => Process crashes
                                             # => Error: RuntimeError
                                             # => Process terminates
                                             # => No automatic restart
end)
# => pid exists but process dead
# => No supervision to restart
# => Caller never receives result
```

Crashed processes don't restart automatically. No supervision means manual crash handling.

### Problem 2: Process Leaks

```elixir
# Spawning processes without tracking
Enum.each(1..1000, fn i ->
  spawn(fn ->
    Process.sleep(:infinity)                 # => Process sleeps forever
                                             # => Never exits
                                             # => Holds resources
  end)                                       # => Creates process leak
end)                                         # => 1000 zombie processes
# => Memory consumed by sleeping processes
# => No cleanup mechanism
# => System resource exhaustion
```

Processes that never exit leak memory. No built-in cleanup.

### Problem 3: No Return Value Mechanism

```elixir
# Raw spawn doesn't return results naturally
pid = spawn(fn ->
  result = expensive_calculation()           # => Computation completes
                                             # => result: Calculated value
  # How to get result to caller?
end)
# => Must manually implement message passing
# => Caller must know message format
# => No type safety
```

Must manually implement request-response pattern for every concurrent operation.

### Problem 4: No Timeout Handling

```elixir
# Receive without timeout blocks forever
receive do
  {:result, value} -> {:ok, value}           # => Waits indefinitely
                                             # => No automatic timeout
                                             # => Caller blocked forever
end
# => If sender crashes, receiver blocked forever
# => No built-in timeout mechanism
```

Every receive needs manual timeout handling. Easy to forget.

## Task Module - Structured Concurrency

Elixir's `Task` module provides structured abstractions over raw processes.

### Task.async/1 - Start Concurrent Task

```elixir
# Async task returns Task struct
task = Task.async(fn ->
  calculate_invoice_total(items)             # => Runs in separate process
                                             # => Calculation isolated
end)                                         # => task: Task struct
# => Type: %Task{pid: pid(), ref: reference()}
# => Task tracked and managed
```

Returns Task struct with PID and reference for tracking.

### Task.await/2 - Get Result

```elixir
# Await task result with automatic timeout
result = Task.await(task, 5000)              # => Blocks until result or timeout
                                             # => Default timeout: 5 seconds
                                             # => Returns function result
                                             # => Type: term()
# => Automatic cleanup on timeout
# => Exits calling process if task fails
```

`Task.await/2` handles timeout automatically. Default 5 seconds.

### Complete Example - Parallel Financial Calculations

```elixir
# Process multiple invoices in parallel
defmodule InvoiceService do
  def process_batch(invoices) do
    tasks = Enum.map(invoices, fn invoice ->
      Task.async(fn ->
        items_total = Enum.reduce(invoice.items, 0, fn item, acc ->
          acc + item.price * item.quantity   # => Line item calculation
        end)                                 # => items_total: Subtotal

        tax = items_total * invoice.tax_rate # => Tax calculation
                                             # => Type: float()

        total = items_total + tax            # => Final invoice total

        %{
          invoice_id: invoice.id,            # => Invoice identifier
          subtotal: items_total,             # => Pre-tax amount
          tax: tax,                          # => Tax amount
          total: total                       # => Final amount
        }
      end)                                   # => Returns Task struct
    end)                                     # => tasks: List of Task structs
    # => Type: [%Task{}]
    # => All calculations run concurrently

    results = Task.await_many(tasks, 10_000) # => Wait for all tasks
                                             # => Timeout: 10 seconds
                                             # => Returns list of results
    # => Type: [map()]

    total_revenue = Enum.reduce(results, 0, fn result, acc ->
      acc + result.total                     # => Sum all invoice totals
    end)                                     # => total_revenue: Total revenue

    %{
      processed: length(results),            # => Count of processed invoices
      total_revenue: total_revenue,          # => Sum of all invoices
      invoices: results                      # => Individual invoice results
    }
  end
end

# Usage
invoices = [
  %{id: 1, items: [%{price: 100, quantity: 2}], tax_rate: 0.1},
  %{id: 2, items: [%{price: 50, quantity: 5}], tax_rate: 0.1},
  %{id: 3, items: [%{price: 200, quantity: 1}], tax_rate: 0.1}
]                                            # => invoices: List of invoice data

result = InvoiceService.process_batch(invoices)
# => result: %{
#      processed: 3,
#      total_revenue: 715.0,                 # => (220 + 275 + 220)
#      invoices: [...]
#    }
# => All calculations ran concurrently
# => Type: map()
```

Task module provides automatic timeout, proper cleanup, and clean result handling.

### Task Benefits Over Raw Primitives

**1. Automatic Result Handling**: No manual message passing
**2. Built-in Timeout**: Default 5 seconds, configurable
**3. Proper Cleanup**: Resources released on timeout
**4. Error Propagation**: Task failures propagate to caller
**5. Process Tracking**: Task struct tracks PID and reference

Task is sufficient for fire-and-forget or async-await patterns.

## When Task Is Insufficient

Task works well for simple concurrent operations but has limitations for long-running or stateful processes.

### Limitation 1: No Persistent State

```elixir
# Task can't maintain state between operations
Task.async(fn ->
  counter = 0                                # => Local state
  counter + 1                                # => Returns 1
end) |> Task.await()                         # => Result: 1
# => Type: integer()

# Next task has no memory of previous task
Task.async(fn ->
  counter = 0                                # => State reset
  counter + 1                                # => Returns 1 again
end) |> Task.await()                         # => Result: 1 (not 2)
# => No state persistence between tasks
```

Each Task execution starts fresh. No way to maintain state.

### Limitation 2: No Supervision Strategy

```elixir
# Task.Supervisor provides basic supervision
children = [
  {Task.Supervisor, name: MyApp.TaskSupervisor}
]
# => Type: [supervisor_spec()]

Supervisor.start_link(children, strategy: :one_for_one)
# => Starts supervisor for tasks
# => Type: {:ok, pid()}

# But still no state management
Task.Supervisor.async_nolink(MyApp.TaskSupervisor, fn ->
  process_invoice(invoice)                   # => Supervised execution
                                             # => Crashes don't kill supervisor
end)
# => Still no state between invocations
```

Task.Supervisor adds supervision but doesn't solve state management.

### Limitation 3: No Request-Response Patterns

```elixir
# Task is one-shot: start, wait, result
task = Task.async(fn -> calculate() end)
result = Task.await(task)                    # => Get result once
# => Task exits after result
# => Can't send more requests to same process
```

Task is single-use. For multiple requests, need GenServer.

## GenServer - Full State Management

When you need supervision AND persistent state, use GenServer.

### GenServer Basics

```elixir
# Invoice calculator with persistent state
defmodule InvoiceCalculator do
  use GenServer                              # => Imports GenServer behavior
                                             # => Provides callbacks: init, handle_call, etc.

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, %{}, opts)
                                             # => Starts supervised process
                                             # => Initial state: empty map
                                             # => Returns {:ok, pid}
  end

  def calculate(pid, invoice) do
    GenServer.call(pid, {:calculate, invoice}, 10_000)
                                             # => Synchronous call
                                             # => Timeout: 10 seconds
                                             # => Returns result from handle_call
  end

  def get_stats(pid) do
    GenServer.call(pid, :get_stats)          # => Request statistics
                                             # => Returns current state stats
  end

  # Server Callbacks

  def init(state) do
    {:ok, state}                             # => Initial state: empty map
                                             # => Type: {:ok, map()}
  end

  def handle_call({:calculate, invoice}, _from, state) do
    subtotal = Enum.reduce(invoice.items, 0, fn item, acc ->
      acc + item.price * item.quantity       # => Calculate line totals
    end)                                     # => subtotal: Items sum

    tax = subtotal * invoice.tax_rate        # => Tax calculation
    total = subtotal + tax                   # => Final total

    result = %{
      invoice_id: invoice.id,
      subtotal: subtotal,
      tax: tax,
      total: total
    }

    # Update state with statistics
    new_state = state
    |> Map.update(:processed_count, 1, &(&1 + 1))
                                             # => Increment processed count
    |> Map.update(:total_revenue, total, &(&1 + total))
                                             # => Add to total revenue

    {:reply, result, new_state}              # => Reply with result
                                             # => Update state
                                             # => Type: {:reply, map(), map()}
  end

  def handle_call(:get_stats, _from, state) do
    stats = %{
      processed: Map.get(state, :processed_count, 0),
      revenue: Map.get(state, :total_revenue, 0)
    }
    {:reply, stats, state}                   # => Return stats, keep state
                                             # => Type: {:reply, map(), map()}
  end
end

# Usage with supervision
{:ok, pid} = InvoiceCalculator.start_link(name: MyInvoiceCalculator)
# => pid: GenServer process PID
# => Process registered with name
# => Type: {:ok, pid()}

# Process invoices
invoice1 = %{
  id: 1,
  items: [%{price: 100, quantity: 2}],
  tax_rate: 0.1
}
result1 = InvoiceCalculator.calculate(pid, invoice1)
# => result1: %{invoice_id: 1, subtotal: 200, tax: 20, total: 220}
# => State updated: processed_count: 1, total_revenue: 220

invoice2 = %{
  id: 2,
  items: [%{price: 50, quantity: 5}],
  tax_rate: 0.1
}
result2 = InvoiceCalculator.calculate(pid, invoice2)
# => result2: %{invoice_id: 2, subtotal: 250, tax: 25, total: 275}
# => State updated: processed_count: 2, total_revenue: 495

stats = InvoiceCalculator.get_stats(pid)
# => stats: %{processed: 2, revenue: 495}
# => State persisted across calls
```

GenServer maintains state across multiple requests. Process lives until explicitly stopped or supervised restart.

### GenServer Benefits Over Task

**1. Persistent State**: State maintained between calls
**2. Multiple Operations**: Single process handles many requests
**3. Supervision**: Integrates with supervision trees
**4. Named Processes**: Register with name for easy access
**5. Lifecycle Callbacks**: init, handle_call, handle_cast, terminate
**6. Complex Patterns**: Request-response, cast-and-forget, timeouts

Use GenServer when you need long-lived processes with state.

## Production Decision Matrix

| Requirement                     | Raw Spawn  | Task               | GenServer       |
| ------------------------------- | ---------- | ------------------ | --------------- |
| **Simple concurrent execution** | ✅ Minimal | ✅ Recommended     | ❌ Overkill     |
| **Automatic result handling**   | ❌ Manual  | ✅ Built-in        | ✅ Built-in     |
| **Timeout management**          | ❌ Manual  | ✅ Automatic       | ✅ Configurable |
| **Error propagation**           | ❌ Manual  | ✅ Automatic       | ✅ Supervised   |
| **Persistent state**            | ❌ No      | ❌ No              | ✅ Yes          |
| **Multiple requests**           | ❌ Hard    | ❌ One-shot        | ✅ Yes          |
| **Supervision integration**     | ❌ Manual  | ⚠️ Task.Supervisor | ✅ Full         |
| **Named processes**             | ⚠️ Manual  | ❌ No              | ✅ Built-in     |
| **Learning curve**              | Low        | Low                | Medium          |
| **Boilerplate**                 | Minimal    | Minimal            | Moderate        |

### Decision Guide

**Use Raw Spawn When**:

- Learning BEAM fundamentals
- Prototyping concepts
- Absolute minimal overhead required

**Use Task When**:

- Fire-and-forget operations (Task.start)
- Async-await patterns (Task.async + Task.await)
- No state needed between operations
- Simple parallel processing

**Use GenServer When**:

- Need persistent state
- Multiple requests to same process
- Complex lifecycle management
- Production systems requiring supervision

## Best Practices

### 1. Default to Task for Stateless Concurrency

```elixir
# Good: Task for parallel API calls
tasks = Enum.map(apis, fn api ->
  Task.async(fn -> fetch_data(api) end)      # => Concurrent API calls
end)
results = Task.await_many(tasks)             # => Collect all results

# Avoid: GenServer for stateless operations
```

Task is simpler for stateless concurrent operations.

### 2. Use GenServer for State Management

```elixir
# Good: GenServer for stateful cache
defmodule Cache do
  use GenServer

  def get(key), do: GenServer.call(__MODULE__, {:get, key})
  def put(key, value), do: GenServer.cast(__MODULE__, {:put, key, value})

  def handle_call({:get, key}, _from, state) do
    {:reply, Map.get(state, key), state}     # => State persists
  end

  def handle_cast({:put, key, value}, state) do
    {:noreply, Map.put(state, key, value)}   # => Update state
  end
end
```

GenServer natural fit for caches, counters, state machines.

### 3. Always Set Timeouts

```elixir
# Good: Explicit timeout
result = Task.await(task, 10_000)            # => 10 second timeout

# Good: Explicit GenServer timeout
GenServer.call(pid, :operation, 5_000)       # => 5 second timeout

# Avoid: Infinite timeout (default in some cases)
```

Always specify timeouts to prevent indefinite blocking.

### 4. Use Task.Supervisor for Fire-and-Forget

```elixir
# Good: Supervised fire-and-forget
Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
  send_email(user)                           # => Supervised execution
                                             # => Don't need result
end)

# Avoid: Unsupervised spawn for production
spawn(fn -> send_email(user) end)            # => No supervision
```

Task.Supervisor prevents process leaks for fire-and-forget operations.

### 5. Prefer Named GenServers

```elixir
# Good: Named GenServer
GenServer.start_link(__MODULE__, state, name: __MODULE__)
InvoiceCalculator.calculate(MyInvoiceCalculator, invoice)

# Avoid: Passing PIDs manually
{:ok, pid} = GenServer.start_link(__MODULE__, state)
InvoiceCalculator.calculate(pid, invoice)    # => PID management burden
```

Named processes eliminate PID management and enable easy access.

## Common Pitfalls

### Pitfall 1: Using spawn Without Supervision

```elixir
# Wrong: Untracked process
spawn(fn -> process_invoice(invoice) end)    # => No supervision
                                             # => Process crash unhandled
                                             # => Resource leak

# Right: Task.Supervisor
Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
  process_invoice(invoice)                   # => Supervised
end)
```

### Pitfall 2: Forgetting Timeout on await

```elixir
# Wrong: No timeout
Task.await(task)                             # => Default 5 seconds
                                             # => May be too short

# Right: Explicit timeout
Task.await(task, 30_000)                     # => 30 seconds for slow operation
```

### Pitfall 3: GenServer for One-Shot Operations

```elixir
# Wrong: GenServer for single calculation
defmodule Calculator do
  use GenServer
  # ... callbacks for single calculation
end

# Right: Task for one-shot
Task.async(fn -> calculate() end) |> Task.await()
```

GenServer adds complexity when Task sufficient.

### Pitfall 4: Not Linking Processes

```elixir
# Wrong: Unlinked spawn
spawn(fn -> work() end)                      # => Crash isolated but untracked

# Right: Linked Task
Task.async(fn -> work() end)                 # => Linked to caller
                                             # => Crash propagates
```

Linking ensures errors propagate to supervisors.

## Further Reading

**Next guides in OTP and Concurrency category**:

- [GenServer Patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/genserver-patterns) - GenServer design patterns
- [Supervisor Trees](/en/learn/software-engineering/programming-languages/elixir/in-the-field/supervisor-trees) - Supervision strategies
- [Concurrency Patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/concurrency-patterns) - Advanced concurrent patterns

**Related production topics**:

- [Error Handling Resilience](/en/learn/software-engineering/programming-languages/elixir/in-the-field/error-handling-resilience) - Let it crash philosophy
- [Performance Optimization](/en/learn/software-engineering/programming-languages/elixir/in-the-field/performance-optimization) - Process optimization strategies

## Summary

Process-based concurrency in Elixir follows clear progression:

1. **BEAM Primitives** (spawn/send/receive) - Foundation understanding
2. **Limitations** - No supervision, manual cleanup, boilerplate
3. **Task Module** - Structured async-await for stateless operations
4. **GenServer** - Full state management with supervision

**Use Task** for stateless concurrent operations with automatic result handling.

**Use GenServer** when you need persistent state, multiple requests, or complex lifecycle management.

Both integrate with supervision trees for production reliability.
