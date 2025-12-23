---
title: "Work with Processes and Message Passing"
date: 2025-12-21T10:00:00+07:00
draft: false
description: "Master Elixir processes, message passing, spawn, send/receive, process linking, and concurrent programming fundamentals"
weight: 1000004
tags: ["elixir", "processes", "concurrency", "message-passing", "spawn"]
---

**Need to handle concurrent operations?** This guide teaches Elixir's lightweight processes, message passing, spawn patterns, process linking, and building concurrent systems with the actor model.

## Problem

Traditional threading is complex and error-prone:

- Shared mutable state causes race conditions
- Locks lead to deadlocks
- Thread pools are resource-intensive
- Difficult to reason about concurrent code

Elixir processes are lightweight (thousands per core), isolated (no shared memory), and communicate via messages.

## Prerequisites

- [Beginner Tutorial - Processes](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#processes)
- [Quick Start Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/quick-start)

## Solution Overview

Elixir process model:

1. **Spawning**: Create new processes
2. **Message Passing**: `send` and `receive`
3. **Process Linking**: Monitor process health
4. **State Management**: Recursive loops

## Creating Processes

### Basic Spawn

```elixir
defmodule ProcessExamples do
  # Spawn anonymous function
  def spawn_basic do
    pid = spawn(fn ->
      IO.puts("Hello from process #{inspect(self())}")
    end)

    IO.puts("Spawned process: #{inspect(pid)}")
    pid
  end

  # Spawn named function
  def spawn_named do
    spawn(__MODULE__, :worker_function, ["argument"])
  end

  def worker_function(arg) do
    IO.puts("Worker received: #{arg}")
  end

  # Spawn and wait
  def spawn_and_wait do
    parent = self()

    child = spawn(fn ->
      :timer.sleep(1000)
      send(parent, {:done, "result"})
    end)

    receive do
      {:done, result} ->
        IO.puts("Received: #{result}")
    after
      2000 ->
        IO.puts("Timeout waiting for child")
    end

    child
  end
end

# Usage
ProcessExamples.spawn_basic()
# => Spawned process: #PID<0.123.0>
# => Hello from process #PID<0.123.0>

ProcessExamples.spawn_and_wait()
# After 1 second: "Received: result"
```

**How It Works**: `spawn/1` creates new process executing given function. Returns PID (process identifier). Process runs concurrently with parent.

### Spawn vs Spawn Link

```elixir
defmodule SpawnComparison do
  # Regular spawn - processes independent
  def spawn_independent do
    spawn(fn ->
      :timer.sleep(100)
      raise "Crash!"
    end)

    :timer.sleep(200)
    IO.puts("Parent still alive")
  end

  # Spawn link - crash propagates
  def spawn_linked do
    spawn_link(fn ->
      :timer.sleep(100)
      raise "Crash!"
    end)

    :timer.sleep(200)
    IO.puts("Parent still alive (you won't see this)")
  end

  # Trap exits to handle crashes
  def spawn_link_trapped do
    Process.flag(:trap_exit, true)

    pid = spawn_link(fn ->
      :timer.sleep(100)
      raise "Crash!"
    end)

    receive do
      {:EXIT, ^pid, reason} ->
        IO.puts("Child crashed: #{inspect(reason)}")
    end

    IO.puts("Parent handled crash and continues")
  end
end

# Usage
SpawnComparison.spawn_independent()
# Parent still alive

# SpawnComparison.spawn_linked()
# ** (EXIT) Crash! - parent also crashes

SpawnComparison.spawn_link_trapped()
# Child crashed: ...
# Parent handled crash and continues
```

## Message Passing

### Send and Receive

```elixir
defmodule Messenger do
  def ping_pong do
    parent = self()

    # Spawn child process
    child = spawn(fn ->
      receive do
        {:ping, from} ->
          IO.puts("Child: received ping")
          send(from, {:pong, self()})
      end
    end)

    # Send message to child
    send(child, {:ping, parent})

    # Wait for response
    receive do
      {:pong, from} ->
        IO.puts("Parent: received pong from #{inspect(from)}")
    end
  end

  # Multiple message patterns
  def message_patterns do
    receive do
      {:hello, name} ->
        IO.puts("Hello, #{name}!")

      {:goodbye, name} ->
        IO.puts("Goodbye, #{name}!")

      :shutdown ->
        IO.puts("Shutting down")
        :shutdown

      other ->
        IO.puts("Unknown message: #{inspect(other)}")
    after
      5000 ->
        IO.puts("No message received in 5 seconds")
    end
  end
end

# Usage
Messenger.ping_pong()
# Child: received ping
# Parent: received pong from #PID<0.124.0>

pid = spawn(Messenger, :message_patterns, [])
send(pid, {:hello, "Alice"})
# => Hello, Alice!
```

### Mailbox and Message Queue

```elixir
defmodule MailboxExamples do
  # Messages queue up in mailbox
  def queue_messages do
    pid = spawn(fn ->
      :timer.sleep(2000)  # Delay before processing

      receive do
        msg1 -> IO.puts("Received 1: #{inspect(msg1)}")
      end

      receive do
        msg2 -> IO.puts("Received 2: #{inspect(msg2)}")
      end

      receive do
        msg3 -> IO.puts("Received 3: #{inspect(msg3)}")
      end
    end)

    # Send messages immediately
    send(pid, "First")
    send(pid, "Second")
    send(pid, "Third")

    :timer.sleep(3000)
    pid
  end

  # Selective receive
  def selective_receive do
    spawn(fn ->
      send(self(), {:low, "unimportant"})
      send(self(), {:high, "urgent"})
      send(self(), {:low, "also unimportant"})

      # Process high priority first
      receive do
        {:high, msg} -> IO.puts("Priority: #{msg}")
      end

      # Then process remaining
      receive do
        {:low, msg} -> IO.puts("Normal: #{msg}")
      end

      receive do
        {:low, msg} -> IO.puts("Normal: #{msg}")
      end
    end)
  end
end
```

## Stateful Processes

### Recursive Loop Pattern

```elixir
defmodule Counter do
  # Server loop
  def start(initial_value \\ 0) do
    spawn(fn -> loop(initial_value) end)
  end

  defp loop(current_value) do
    receive do
      {:increment, from} ->
        new_value = current_value + 1
        send(from, {:value, new_value})
        loop(new_value)

      {:get, from} ->
        send(from, {:value, current_value})
        loop(current_value)

      :stop ->
        IO.puts("Counter stopping with value: #{current_value}")
        # Don't recurse - process terminates
    end
  end

  # Client functions
  def increment(pid) do
    send(pid, {:increment, self()})

    receive do
      {:value, value} -> value
    after
      1000 -> {:error, :timeout}
    end
  end

  def get(pid) do
    send(pid, {:get, self()})

    receive do
      {:value, value} -> value
    after
      1000 -> {:error, :timeout}
    end
  end

  def stop(pid) do
    send(pid, :stop)
  end
end

# Usage
counter = Counter.start(0)

Counter.increment(counter)  # => 1
Counter.increment(counter)  # => 2
Counter.get(counter)        # => 2
Counter.increment(counter)  # => 3

Counter.stop(counter)
# => Counter stopping with value: 3
```

### Key-Value Store

```elixir
defmodule KVStore do
  def start do
    spawn(fn -> loop(%{}) end)
  end

  defp loop(state) do
    receive do
      {:put, key, value, from} ->
        new_state = Map.put(state, key, value)
        send(from, :ok)
        loop(new_state)

      {:get, key, from} ->
        value = Map.get(state, key)
        send(from, {:ok, value})
        loop(state)

      {:delete, key, from} ->
        new_state = Map.delete(state, key)
        send(from, :ok)
        loop(new_state)

      {:keys, from} ->
        keys = Map.keys(state)
        send(from, {:ok, keys})
        loop(state)

      :stop ->
        :ok  # Terminate
    end
  end

  # Client API
  def put(pid, key, value) do
    send(pid, {:put, key, value, self()})

    receive do
      :ok -> :ok
    after
      1000 -> {:error, :timeout}
    end
  end

  def get(pid, key) do
    send(pid, {:get, key, self()})

    receive do
      {:ok, value} -> {:ok, value}
    after
      1000 -> {:error, :timeout}
    end
  end

  def delete(pid, key) do
    send(pid, {:delete, key, self()})

    receive do
      :ok -> :ok
    after
      1000 -> {:error, :timeout}
    end
  end

  def keys(pid) do
    send(pid, {:keys, self()})

    receive do
      {:ok, keys} -> {:ok, keys}
    after
      1000 -> {:error, :timeout}
    end
  end

  def stop(pid) do
    send(pid, :stop)
  end
end

# Usage
store = KVStore.start()

KVStore.put(store, :name, "Alice")
KVStore.put(store, :age, 30)

KVStore.get(store, :name)  # => {:ok, "Alice"}
KVStore.keys(store)        # => {:ok, [:name, :age]}
KVStore.delete(store, :age)
KVStore.keys(store)        # => {:ok, [:name]}
```

## Process Monitoring

### Monitor vs Link

```elixir
defmodule ProcessMonitoring do
  # Monitoring (one-way)
  def monitor_example do
    pid = spawn(fn ->
      :timer.sleep(1000)
      raise "Crash!"
    end)

    ref = Process.monitor(pid)

    receive do
      {:DOWN, ^ref, :process, ^pid, reason} ->
        IO.puts("Process #{inspect(pid)} crashed: #{inspect(reason)}")
    end

    IO.puts("Monitor continues running")
  end

  # Check if process is alive
  def check_alive do
    pid = spawn(fn -> :timer.sleep(5000) end)

    IO.puts("Process alive? #{Process.alive?(pid)}")
    :timer.sleep(6000)
    IO.puts("Process alive? #{Process.alive?(pid)}")
  end

  # Named processes
  def named_process do
    # Register process with name
    pid = spawn(fn ->
      receive do
        msg -> IO.puts("Named process received: #{inspect(msg)}")
      end
    end)

    Process.register(pid, :my_worker)

    # Send to named process
    send(:my_worker, "Hello!")

    # Find PID by name
    Process.whereis(:my_worker)
  end
end

# Usage
ProcessMonitoring.monitor_example()
# After 1 second:
# Process #PID<...> crashed: ...
# Monitor continues running
```

## Concurrent Patterns

### Parallel Map

```elixir
defmodule ParallelProcessing do
  def parallel_map(collection, fun) do
    parent = self()

    # Spawn process for each item
    processes = Enum.map(collection, fn item ->
      spawn(fn ->
        result = fun.(item)
        send(parent, {self(), result})
      end)
    end)

    # Collect results
    Enum.map(processes, fn pid ->
      receive do
        {^pid, result} -> result
      end
    end)
  end

  # Example usage
  def expensive_computation(n) do
    :timer.sleep(1000)  # Simulate work
    n * n
  end
end

# Usage
# Sequential: 5 seconds
Enum.map([1, 2, 3, 4, 5], &ParallelProcessing.expensive_computation/1)

# Parallel: ~1 second (if 5+ cores)
ParallelProcessing.parallel_map([1, 2, 3, 4, 5],
  &ParallelProcessing.expensive_computation/1)
# => [1, 4, 9, 16, 25]
```

### Worker Pool

```elixir
defmodule WorkerPool do
  def start(num_workers) do
    parent = self()

    workers = Enum.map(1..num_workers, fn id ->
      spawn(fn -> worker_loop(id, parent) end)
    end)

    coordinator(workers, [])
  end

  defp worker_loop(id, coordinator) do
    send(coordinator, {:ready, self()})

    receive do
      {:work, work_id, fun} ->
        IO.puts("Worker #{id} processing work #{work_id}")
        result = fun.()
        send(coordinator, {:done, self(), work_id, result})
        worker_loop(id, coordinator)

      :stop ->
        IO.puts("Worker #{id} stopping")
        :ok
    end
  end

  defp coordinator(workers, ready_workers) do
    receive do
      {:ready, pid} ->
        coordinator(workers, [pid | ready_workers])

      {:schedule, work_id, fun, from} ->
        case ready_workers do
          [worker | rest] ->
            send(worker, {:work, work_id, fun})
            send(from, {:scheduled, work_id})
            coordinator(workers, rest)

          [] ->
            send(from, {:error, :no_workers_available})
            coordinator(workers, ready_workers)
        end

      {:done, pid, work_id, result} ->
        IO.puts("Work #{work_id} completed: #{inspect(result)}")
        coordinator(workers, [pid | ready_workers])

      :stop ->
        Enum.each(workers, fn pid -> send(pid, :stop) end)
        :ok
    end
  end
end
```

## Best Practices

### Do: Keep Processes Small and Focused

```elixir
# Good: Single responsibility
defmodule Cache do
  def start, do: spawn(fn -> loop(%{}) end)
  defp loop(state), do: # ... cache operations
end

defmodule Logger do
  def start, do: spawn(fn -> loop([]) end)
  defp loop(logs), do: # ... logging operations
end
```

### Do: Use Timeout on Receive

```elixir
# Good: Prevents infinite wait
receive do
  msg -> handle(msg)
after
  5000 -> {:error, :timeout}
end

# Bad: Blocks forever if no message
receive do
  msg -> handle(msg)
end
```

### Do: Match Specific Messages

```elixir
# Good: Explicit patterns
receive do
  {:ok, value} -> handle_success(value)
  {:error, reason} -> handle_error(reason)
  :shutdown -> :ok
after
  1000 -> :timeout
end

# Bad: Catch-all hides errors
receive do
  msg -> handle(msg)
end
```

## See Also

- [Cookbook - Concurrency Recipes](/en/learn/software-engineering/programming-language/elixir/how-to/cookbook#concurrency)
- [Beginner Tutorial - Processes](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner#processes)
- [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver)
- [Task and Agent Guide](/en/learn/software-engineering/programming-language/elixir/how-to/task-agent)
