---
title: "Concurrency Patterns"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000009
description: "From spawn primitives to Task.async_stream with bounded concurrency for production-grade parallel processing"
tags: ["elixir", "concurrency", "task", "patterns", "production", "parallelism"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/process-registry-patterns"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/pattern-matching-production"
---

**How do you manage concurrent operations at scale?** This guide teaches concurrency patterns from raw BEAM primitives through Task module abstractions, showing when each pattern provides production value for parallel processing workloads.

## Why Concurrency Patterns Matter

Elixir's concurrency model enables massive parallelism, but wrong patterns create production problems:

- **Unbounded concurrency** - Spawning unlimited processes exhausts system resources
- **No backpressure** - Fast producers overwhelm slow consumers
- **Manual coordination** - Complex synchronization code for simple parallel operations
- **Resource exhaustion** - Database connection pools depleted by concurrent requests
- **Process leaks** - Fire-and-forget processes never cleaned up
- **Error isolation** - One failure cascades to unrelated operations

**Production concurrency requires patterns** that bound resource usage, handle backpressure, and provide automatic cleanup.

## Financial Domain Examples

Examples use batch Zakat calculation scenarios:

- **Parallel invoice processing** - Calculate Zakat on multiple donations
- **Bounded concurrency** - Limit concurrent calculations to protect resources
- **Backpressure handling** - Process donations at sustainable rate
- **Error isolation** - One calculation failure doesn't affect others

These demonstrate production patterns with real financial operations.

## Standard Library - Raw spawn

### Basic Concurrent Execution

BEAM provides `spawn/1` for creating concurrent processes.

```elixir
# Raw process spawning for parallel calculation
pid = spawn(fn ->
  zakat = calculate_zakat(donation)          # => Calculates 2.5% of donation
                                             # => Runs in separate process
  IO.puts("Zakat: #{zakat}")                 # => Output: Zakat: 25.0
                                             # => Process exits after output
end)
# => Returns PID (Process Identifier)
# => Type: pid()
# => Caller doesn't wait for completion
```

Process created, executes function, exits. No return value to caller.

### Parallel Processing with spawn

```elixir
# Process multiple donations concurrently
defmodule ZakatProcessor do
  def process_batch(donations) do
    parent = self()                          # => Caller process PID
                                             # => Type: pid()

    Enum.each(donations, fn donation ->
      spawn(fn ->
        zakat = donation.amount * 0.025      # => 2.5% Zakat calculation
                                             # => Type: float()

        result = %{
          id: donation.id,                   # => Donation identifier
          amount: donation.amount,           # => Original donation
          zakat: zakat                       # => Calculated Zakat
        }

        send(parent, {:result, result})      # => Send result to parent
                                             # => Type: tuple()
      end)                                   # => Returns PID
                                             # => Process runs independently
    end)                                     # => All processes spawned

    collect_results(length(donations), [])   # => Wait for all results
                                             # => Type: [map()]
  end

  defp collect_results(0, acc), do: acc      # => Base case: all collected
                                             # => Returns accumulated results

  defp collect_results(count, acc) do
    receive do
      {:result, result} ->                   # => Pattern match result message
        collect_results(count - 1, [result | acc])
                                             # => Recursive collection
                                             # => Decrements remaining count
    after
      5000 ->                                # => 5 second timeout
        {:error, :timeout}                   # => Returns timeout error
                                             # => Type: {:error, :timeout}
    end
  end
end

# Usage
donations = [
  %{id: 1, amount: 1000},                    # => $1000 donation
  %{id: 2, amount: 2000},                    # => $2000 donation
  %{id: 3, amount: 1500}                     # => $1500 donation
]

results = ZakatProcessor.process_batch(donations)
# => results: [
#      %{id: 1, amount: 1000, zakat: 25.0},
#      %{id: 2, amount: 2000, zakat: 50.0},
#      %{id: 3, amount: 1500, zakat: 37.5}
#    ]
# => All calculations ran concurrently
# => Type: [map()]
```

Raw spawn enables concurrent execution but requires manual coordination.

## Limitations of spawn

### Problem 1: No Supervision

```elixir
# Spawned process crashes - no recovery
pid = spawn(fn ->
  raise "Database connection failed"         # => Process crashes
                                             # => Error: RuntimeError
                                             # => Process terminates
                                             # => No automatic restart
end)
# => PID exists but process dead
# => No supervision to restart
# => Caller never receives result
```

Crashed processes don't restart. No supervision means manual crash handling.

### Problem 2: Unbounded Concurrency

```elixir
# Spawning unlimited processes
donations = Enum.to_list(1..10_000)          # => 10,000 donations
                                             # => Type: [integer()]

Enum.each(donations, fn donation ->
  spawn(fn ->
    calculate_zakat(donation)                # => 10,000 processes spawned
                                             # => System resource exhaustion
  end)
end)
# => All processes spawned immediately
# => No resource limits
# => Potential memory exhaustion
```

No built-in concurrency limiting. Easy to exhaust system resources.

### Problem 3: Manual Backpressure

```elixir
# Fast producer overwhelms slow consumer
producer = spawn(fn ->
  Enum.each(1..100_000, fn i ->
    send(consumer, {:item, i})               # => Sends 100k messages
                                             # => Consumer mailbox grows unbounded
  end)
end)

consumer = spawn(fn ->
  receive do
    {:item, i} ->
      :timer.sleep(100)                      # => Slow processing (100ms)
                                             # => Mailbox fills faster than consumption
  end
end)
# => Producer floods consumer
# => No automatic backpressure
# => Memory grows with mailbox size
```

No built-in backpressure mechanism. Must implement manually.

### Problem 4: Process Cleanup

```elixir
# No automatic cleanup of spawned processes
Enum.each(donations, fn donation ->
  spawn(fn ->
    # If this crashes or hangs, process leaked
    calculate_zakat(donation)                # => No supervision
                                             # => Process may leak on error
  end)
end)
# => Crashed processes not cleaned up
# => Zombie processes consume memory
```

No automatic cleanup means potential process leaks.

## Task Module - Structured Concurrency

Elixir's Task module provides structured abstractions over raw processes.

### Task.async for Parallel Operations

```elixir
# Task provides automatic cleanup and result handling
defmodule ZakatService do
  def process_batch(donations) do
    tasks = Enum.map(donations, fn donation ->
      Task.async(fn ->
        zakat = donation.amount * 0.025      # => 2.5% calculation
                                             # => Type: float()

        %{
          id: donation.id,
          amount: donation.amount,
          zakat: zakat
        }
      end)                                   # => Returns Task struct
                                             # => Task tracked and managed
    end)                                     # => tasks: List of Task structs
    # => Type: [%Task{}]
    # => All calculations run concurrently

    results = Task.await_many(tasks, 10_000) # => Wait for all tasks
                                             # => Timeout: 10 seconds
                                             # => Automatic cleanup on timeout
    # => Type: [map()]

    total_zakat = Enum.reduce(results, 0, fn result, acc ->
      acc + result.zakat                     # => Sum all Zakat amounts
                                             # => Type: float()
    end)

    %{
      processed: length(results),            # => Count of processed donations
      total_zakat: total_zakat,              # => Sum of all Zakat
      details: results                       # => Individual results
    }
  end
end

# Usage
donations = [
  %{id: 1, amount: 1000},
  %{id: 2, amount: 2000},
  %{id: 3, amount: 1500}
]

result = ZakatService.process_batch(donations)
# => result: %{
#      processed: 3,
#      total_zakat: 112.5,                   # => (25 + 50 + 37.5)
#      details: [...]
#    }
# => Automatic timeout handling
# => Automatic cleanup
# => Type: map()
```

Task provides automatic result handling, timeout, and cleanup.

### Task Benefits Over spawn

**1. Automatic Result Collection**: No manual message passing
**2. Built-in Timeout**: Configurable with automatic cleanup
**3. Error Propagation**: Task failures propagate to caller
**4. Process Tracking**: Task struct tracks PID and reference
**5. Resource Cleanup**: Tasks cleaned up on completion or timeout

Task sufficient for simple async-await patterns.

## Task.async_stream - Bounded Concurrency

### Problem with Unbounded Task.async

```elixir
# Processing 10,000 donations with Task.async
donations = load_donations(10_000)           # => 10,000 donation records
                                             # => Type: [map()]

tasks = Enum.map(donations, fn donation ->
  Task.async(fn ->
    calculate_zakat(donation)                # => 10,000 concurrent tasks
                                             # => System resource exhaustion
  end)
end)
# => All 10,000 tasks spawned immediately
# => No resource limits
# => Potential memory/CPU exhaustion
```

Task.async spawns unlimited concurrent processes. Need bounded concurrency.

### Task.async_stream Solution

```elixir
# Bounded concurrency with Task.async_stream
defmodule ZakatBatchService do
  def process_large_batch(donations) do
    donations
    |> Task.async_stream(
      fn donation ->
        zakat = donation.amount * 0.025      # => Calculate Zakat
                                             # => Type: float()

        %{
          id: donation.id,
          amount: donation.amount,
          zakat: zakat
        }
      end,
      max_concurrency: 50,                   # => Maximum 50 concurrent processes
                                             # => Automatic backpressure
      timeout: 5000,                         # => 5 second timeout per task
                                             # => Prevents hanging tasks
      ordered: false                         # => Results in completion order
                                             # => Better performance
    )
    |> Enum.reduce(%{processed: 0, total_zakat: 0, errors: 0}, fn
      {:ok, result}, acc ->
        %{
          processed: acc.processed + 1,      # => Increment processed count
          total_zakat: acc.total_zakat + result.zakat,
                                             # => Accumulate Zakat
          errors: acc.errors                 # => Maintain error count
        }

      {:exit, _reason}, acc ->
        %{acc | errors: acc.errors + 1}      # => Increment error count
                                             # => Continue processing others
    end)
  end
end

# Usage with large dataset
donations = load_donations(10_000)           # => 10,000 donations
result = ZakatBatchService.process_large_batch(donations)
# => result: %{
#      processed: 9998,
#      total_zakat: 250_000.0,
#      errors: 2
#    }
# => Maximum 50 concurrent at any time
# => Automatic backpressure
# => Type: map()
```

Task.async_stream provides bounded concurrency with automatic backpressure.

### Task.async_stream Options

```elixir
# All Task.async_stream configuration options
Task.async_stream(
  items,
  fn item -> process(item) end,
  max_concurrency: 50,                       # => Max concurrent processes
                                             # => Default: System.schedulers_online() * 2
                                             # => Controls resource usage

  timeout: 5000,                             # => Timeout per task (milliseconds)
                                             # => Default: 5000
                                             # => Prevents hanging

  ordered: false,                            # => Results in completion order
                                             # => Default: true (input order)
                                             # => false improves performance

  on_timeout: :kill_task                     # => Kill task on timeout
                                             # => Default: :exit (exit stream)
                                             # => :kill_task continues processing
)
# => Returns Stream that yields {:ok, result} or {:exit, reason}
# => Lazy evaluation with backpressure
# => Type: Enumerable.t()
```

Options control concurrency, timeouts, and error handling.

## Production Pattern - Supervised Task Processing

### Task.Supervisor for Production

```elixir
# Production-grade batch processing with supervision
defmodule Finance.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Task.Supervisor, name: Finance.TaskSupervisor}
                                             # => Supervisor for tasks
                                             # => Named for easy access
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
                                             # => Starts application supervisor
                                             # => Returns {:ok, pid}
  end
end

defmodule ZakatProductionService do
  def process_batch_supervised(donations) do
    donations
    |> Task.Supervisor.async_stream(
      Finance.TaskSupervisor,                # => Supervisor name
                                             # => Tasks supervised
      fn donation ->
        # Database operation
        zakat = donation.amount * 0.025

        # Store result
        Finance.Database.insert_zakat(%{
          donation_id: donation.id,
          amount: zakat
        })

        %{id: donation.id, zakat: zakat}
      end,
      max_concurrency: 25,                   # => Conservative limit
                                             # => Protects database
      timeout: 10_000,                       # => 10 second timeout
                                             # => Accounts for DB latency
      on_timeout: :kill_task,                # => Kill hung tasks
                                             # => Continue processing
      ordered: false                         # => Optimize throughput
                                             # => Order not needed
    )
    |> Enum.reduce(%{success: 0, failed: 0}, fn
      {:ok, _result}, acc ->
        %{acc | success: acc.success + 1}    # => Count successes

      {:exit, _reason}, acc ->
        %{acc | failed: acc.failed + 1}      # => Count failures
                                             # => Continue processing
    end)
  end
end

# Usage
donations = load_donations(1000)
result = ZakatProductionService.process_batch_supervised(donations)
# => result: %{success: 998, failed: 2}
# => Supervised execution
# => Bounded concurrency protects database
# => Type: map()
```

Task.Supervisor provides production-grade supervision with bounded concurrency.

## Agent for Simple State

### When to Use Agent

Use Agent for simple state management without complex logic.

```elixir
# Running total of processed Zakat with Agent
defmodule ZakatCounter do
  use Agent

  def start_link(_) do
    Agent.start_link(fn -> %{count: 0, total: 0} end, name: __MODULE__)
                                             # => Initial state: empty counters
                                             # => Named agent
                                             # => Returns {:ok, pid}
  end

  def add(zakat_amount) do
    Agent.update(__MODULE__, fn state ->
      %{
        count: state.count + 1,              # => Increment count
        total: state.total + zakat_amount    # => Add to total
      }
    end)
    # => Updates state atomically
    # => Type: :ok
  end

  def get_stats do
    Agent.get(__MODULE__, fn state -> state end)
                                             # => Returns current state
                                             # => Type: map()
  end
end

# Usage with concurrent processing
{:ok, _pid} = ZakatCounter.start_link([])
# => Starts counter agent

donations = load_donations(100)
donations
|> Task.async_stream(
  fn donation ->
    zakat = donation.amount * 0.025
    ZakatCounter.add(zakat)                  # => Concurrent state update
                                             # => Agent serializes updates
    zakat
  end,
  max_concurrency: 50
)
|> Stream.run()                              # => Execute stream
                                             # => Discards results

stats = ZakatCounter.get_stats()
# => stats: %{count: 100, total: 2500.0}
# => State consistent despite concurrency
# => Type: map()
```

Agent provides simple concurrent state management.

## Decision Matrix

| Pattern               | Supervision | Backpressure | Cleanup   | Use Case                    |
| --------------------- | ----------- | ------------ | --------- | --------------------------- |
| **Raw spawn**         | ❌ Manual   | ❌ Manual    | ❌ Manual | Learning, prototypes        |
| **Task.async**        | ⚠️ Caller   | ❌ Unbounded | ✅ Auto   | Small batches, async-await  |
| **Task.async_stream** | ⚠️ Caller   | ✅ Bounded   | ✅ Auto   | Large batches, bounded load |
| **Task.Supervisor**   | ✅ Full     | ✅ Bounded   | ✅ Auto   | Production workloads        |
| **Agent**             | ✅ Full     | N/A          | ✅ Auto   | Simple concurrent state     |

### Decision Guide

**Use spawn When**:

- Learning BEAM fundamentals
- Absolute minimal overhead
- Not production code

**Use Task.async When**:

- Small number of concurrent operations (<100)
- No resource exhaustion risk
- Simple async-await pattern

**Use Task.async_stream When**:

- Large batches requiring bounded concurrency
- Need backpressure control
- Resource protection (database, API limits)

**Use Task.Supervisor When**:

- Production systems
- Need supervision and restart
- Long-running concurrent operations

**Use Agent When**:

- Simple state management
- Concurrent read/write access
- No complex state transitions

## Best Practices

### 1. Always Bound Concurrency

```elixir
# Good: Bounded concurrency
Task.async_stream(items, &process/1, max_concurrency: 50)
                                             # => Maximum 50 concurrent
                                             # => Automatic backpressure

# Avoid: Unbounded concurrency
Enum.map(items, fn item ->
  Task.async(fn -> process(item) end)        # => Unbounded spawning
end)                                         # => Potential resource exhaustion
```

Bounded concurrency prevents resource exhaustion.

### 2. Use ordered: false for Better Performance

```elixir
# Good: Unordered for performance
Task.async_stream(items, &process/1,
  ordered: false,                            # => Results as they complete
  max_concurrency: 50                        # => Better throughput
)

# Slower: Ordered results
Task.async_stream(items, &process/1,
  ordered: true,                             # => Wait for order
  max_concurrency: 50                        # => Head-of-line blocking
)
```

Use `ordered: false` when result order doesn't matter.

### 3. Set Realistic Timeouts

```elixir
# Good: Timeout matches operation
Task.async_stream(items, &db_operation/1,
  timeout: 10_000,                           # => 10s for database
  max_concurrency: 25                        # => Conservative with DB
)

# Too short: Premature timeout
Task.async_stream(items, &db_operation/1,
  timeout: 1000                              # => 1s insufficient
)
```

Timeout should match operation characteristics.

### 4. Use Task.Supervisor in Production

```elixir
# Good: Supervised tasks
Task.Supervisor.async_stream(
  MyApp.TaskSupervisor,
  items,
  &process/1,
  max_concurrency: 50
)

# Avoid in production: Unsupervised
Task.async_stream(items, &process/1, max_concurrency: 50)
                                             # => No supervision
```

Always use Task.Supervisor for production workloads.

### 5. Choose Right max_concurrency

```elixir
# CPU-bound: Use schedulers count
max_concurrency: System.schedulers_online()  # => Match CPU cores

# I/O-bound: Higher concurrency
max_concurrency: System.schedulers_online() * 4
                                             # => 4x schedulers for I/O

# Database operations: Conservative
max_concurrency: 25                          # => Protect connection pool

# External API: Respect rate limits
max_concurrency: 10                          # => API rate limit
```

Tune concurrency based on operation type.

## Common Pitfalls

### Pitfall 1: Unbounded Concurrency

```elixir
# Wrong: Unbounded Task.async
tasks = Enum.map(1..10_000, fn i ->
  Task.async(fn -> process(i) end)           # => 10k concurrent processes
end)

# Right: Bounded Task.async_stream
1..10_000
|> Task.async_stream(&process/1, max_concurrency: 50)
                                             # => Maximum 50 concurrent
```

### Pitfall 2: No Timeout

```elixir
# Wrong: No timeout
Task.async_stream(items, &process/1)         # => Default 5s may be wrong

# Right: Explicit timeout
Task.async_stream(items, &process/1, timeout: 30_000)
                                             # => Explicit 30s timeout
```

### Pitfall 3: Ignoring Errors

```elixir
# Wrong: Ignore {:exit, reason}
items
|> Task.async_stream(&process/1)
|> Enum.map(fn {:ok, result} -> result end)  # => Pattern match fails on error
                                             # => Crashes entire pipeline

# Right: Handle both success and failure
items
|> Task.async_stream(&process/1)
|> Enum.reduce(%{ok: [], error: []}, fn
  {:ok, result}, acc ->
    %{acc | ok: [result | acc.ok]}           # => Collect successes
  {:exit, reason}, acc ->
    %{acc | error: [reason | acc.error]}     # => Collect failures
end)
```

### Pitfall 4: Using Agent for Complex State

```elixir
# Wrong: Agent for complex logic
Agent.update(pid, fn state ->
  # Complex state transition logic
  # Multiple validation steps
  # Error handling
end)                                         # => Agent not designed for this

# Right: GenServer for complex state
GenServer.call(pid, {:update, data})         # => GenServer handles complexity
```

Agent is for simple state. Use GenServer for complex transitions.

## Further Reading

**Related concurrency topics**:

- [Processes and Message Passing](/en/learn/software-engineering/programming-languages/elixir/in-the-field/processes-and-message-passing) - BEAM process fundamentals
- [GenServer Patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/genserver-patterns) - State management patterns

**Production patterns**:

- [Best Practices](/en/learn/software-engineering/programming-languages/elixir/in-the-field/best-practices) - Production OTP patterns
- [Performance Optimization](/en/learn/software-engineering/programming-languages/elixir/in-the-field/performance-optimization) - Concurrency optimization

## Summary

Concurrency patterns in Elixir follow clear progression:

1. **Raw spawn** - BEAM primitives with manual coordination
2. **Limitations** - No supervision, unbounded concurrency, no backpressure
3. **Task Module** - Task.async for small batches, Task.async_stream for bounded concurrency
4. **Production** - Task.Supervisor with bounded concurrency and proper error handling

**Use Task.async_stream** for production batch processing with bounded concurrency and automatic backpressure.

**Use Task.Supervisor** to add supervision and automatic restart capabilities.

**Use Agent** for simple concurrent state management without complex logic.

Key insight: **Bounded concurrency with backpressure prevents resource exhaustion** while maximizing throughput.
