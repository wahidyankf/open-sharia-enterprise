---
title: "Genserver"
date: 2025-12-21T17:00:00+07:00
draft: false
description: "Build robust stateful processes in Elixir using GenServer with synchronous calls, asynchronous casts, state management patterns, and proper error handling."
weight: 1000004
tags:
  ["elixir", "genserver", "otp", "state-management", "concurrency", "how-to"]
---

**Need to manage state in concurrent Elixir applications?** This guide teaches you GenServer patterns for building robust stateful processes with synchronous calls, asynchronous casts, and proper error handling.

## Prerequisites

- Understanding of processes and message passing
- Basic OTP concepts
- Completed [Intermediate Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate) or equivalent

## Problem

You need to maintain state across multiple requests in a concurrent system. Direct process management with `spawn` and `send`/`receive` is error-prone and lacks standardization.

**Challenges:**

- Managing process lifecycle (start, stop, crash recovery)
- Handling synchronous vs asynchronous requests
- Implementing timeouts and error handling
- Maintaining consistent state updates
- Testing stateful processes

## Solution

Use **GenServer** (Generic Server) behavior - OTP's standardized abstraction for stateful processes. GenServer handles common process patterns and integrates with supervision trees.

### Key Components

1. **Callbacks** - `init/1`, `handle_call/3`, `handle_cast/2`, `handle_info/2`, `terminate/2`
2. **Client API** - Public functions wrapping `GenServer.call/2` and `GenServer.cast/2`
3. **State Management** - Returning updated state from callbacks
4. **Error Handling** - Return tuples (`{:ok, state}`, `{:error, reason}`)

## How It Works

### 1. Basic GenServer Structure

```elixir
defmodule Counter do
  use GenServer

  ## Client API

  def start_link(initial_value \\ 0) do
    GenServer.start_link(__MODULE__, initial_value, name: __MODULE__)
  end

  def increment do
    GenServer.call(__MODULE__, :increment)
  end

  def get_value do
    GenServer.call(__MODULE__, :get_value)
  end

  def reset do
    GenServer.cast(__MODULE__, :reset)
  end

  ## Server Callbacks

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

  @impl true
  def handle_cast(:reset, _state) do
    {:noreply, 0}
  end
end

{:ok, _pid} = Counter.start_link(10)
Counter.increment()        # 11
Counter.increment()        # 12
Counter.get_value()        # 12
Counter.reset()            # :ok
Counter.get_value()        # 0
```

**Anatomy:**

- `start_link/1` - Start GenServer process
- `handle_call/3` - Synchronous requests (blocks caller until reply)
- `handle_cast/2` - Asynchronous requests (fire and forget)
- State flows through callbacks (last element of return tuple)

### 2. Stack Example (Complete GenServer)

```elixir
defmodule Stack do
  use GenServer

  ## Client API

  def start_link(initial_stack) do
    GenServer.start_link(__MODULE__, initial_stack)
  end

  def push(pid, item) do
    GenServer.cast(pid, {:push, item})
  end

  def pop(pid) do
    GenServer.call(pid, :pop)
  end

  def peek(pid) do
    GenServer.call(pid, :peek)
  end

  def size(pid) do
    GenServer.call(pid, :size)
  end

  ## Server Callbacks

  @impl true
  def init(stack) when is_list(stack) do
    {:ok, stack}
  end

  @impl true
  def handle_call(:pop, _from, []) do
    {:reply, {:error, :empty}, []}
  end

  @impl true
  def handle_call(:pop, _from, [head | tail]) do
    {:reply, {:ok, head}, tail}
  end

  @impl true
  def handle_call(:peek, _from, []) do
    {:reply, {:error, :empty}, []}
  end

  @impl true
  def handle_call(:peek, _from, [head | _tail] = stack) do
    {:reply, {:ok, head}, stack}
  end

  @impl true
  def handle_call(:size, _from, stack) do
    {:reply, length(stack), stack}
  end

  @impl true
  def handle_cast({:push, item}, stack) do
    {:noreply, [item | stack]}
  end
end

{:ok, pid} = Stack.start_link([1, 2, 3])
Stack.push(pid, 4)
Stack.peek(pid)           # {:ok, 4}
Stack.pop(pid)            # {:ok, 4}
Stack.pop(pid)            # {:ok, 3}
Stack.size(pid)           # 2
```

### 3. Key-Value Store with Error Handling

```elixir
defmodule KeyValueStore do
  use GenServer

  ## Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def put(pid, key, value) do
    GenServer.cast(pid, {:put, key, value})
  end

  def get(pid, key) do
    GenServer.call(pid, {:get, key})
  end

  def delete(pid, key) do
    GenServer.call(pid, {:delete, key})
  end

  def keys(pid) do
    GenServer.call(pid, :keys)
  end

  def clear(pid) do
    GenServer.cast(pid, :clear)
  end

  ## Server Callbacks

  @impl true
  def init(:ok) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:get, key}, _from, state) do
    case Map.fetch(state, key) do
      {:ok, value} -> {:reply, {:ok, value}, state}
      :error -> {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call({:delete, key}, _from, state) do
    case Map.pop(state, key) do
      {nil, _state} -> {:reply, {:error, :not_found}, state}
      {value, new_state} -> {:reply, {:ok, value}, new_state}
    end
  end

  @impl true
  def handle_call(:keys, _from, state) do
    {:reply, Map.keys(state), state}
  end

  @impl true
  def handle_cast({:put, key, value}, state) do
    {:noreply, Map.put(state, key, value)}
  end

  @impl true
  def handle_cast(:clear, _state) do
    {:noreply, %{}}
  end
end

{:ok, pid} = KeyValueStore.start_link()
KeyValueStore.put(pid, :name, "Alice")
KeyValueStore.put(pid, :age, 30)
KeyValueStore.get(pid, :name)              # {:ok, "Alice"}
KeyValueStore.get(pid, :missing)           # {:error, :not_found}
KeyValueStore.keys(pid)                    # [:name, :age]
KeyValueStore.delete(pid, :age)            # {:ok, 30}
KeyValueStore.clear(pid)
```

### 4. Timeouts and Monitoring

```elixir
defmodule SessionStore do
  use GenServer

  @session_timeout 60_000  # 60 seconds

  ## Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def create_session(user_id) do
    GenServer.call(__MODULE__, {:create_session, user_id})
  end

  def get_session(session_id) do
    GenServer.call(__MODULE__, {:get_session, session_id})
  end

  def refresh_session(session_id) do
    GenServer.cast(__MODULE__, {:refresh, session_id})
  end

  ## Server Callbacks

  @impl true
  def init(:ok) do
    # Schedule periodic cleanup
    schedule_cleanup()
    {:ok, %{}}
  end

  @impl true
  def handle_call({:create_session, user_id}, _from, state) do
    session_id = generate_session_id()
    session = %{
      user_id: user_id,
      created_at: System.system_time(:second),
      last_accessed: System.system_time(:second)
    }
    new_state = Map.put(state, session_id, session)
    {:reply, {:ok, session_id}, new_state}
  end

  @impl true
  def handle_call({:get_session, session_id}, _from, state) do
    case Map.get(state, session_id) do
      nil -> {:reply, {:error, :not_found}, state}
      session ->
        if session_expired?(session) do
          new_state = Map.delete(state, session_id)
          {:reply, {:error, :expired}, new_state}
        else
          {:reply, {:ok, session}, state}
        end
    end
  end

  @impl true
  def handle_cast({:refresh, session_id}, state) do
    case Map.get(state, session_id) do
      nil -> {:noreply, state}
      session ->
        updated_session = %{session | last_accessed: System.system_time(:second)}
        {:noreply, Map.put(state, session_id, updated_session)}
    end
  end

  @impl true
  def handle_info(:cleanup_expired, state) do
    now = System.system_time(:second)
    new_state = Enum.reject(state, fn {_id, session} ->
      session_expired?(session, now)
    end) |> Enum.into(%{})

    schedule_cleanup()
    {:noreply, new_state}
  end

  ## Private Functions

  defp generate_session_id do
    :crypto.strong_rand_bytes(16) |> Base.encode64()
  end

  defp session_expired?(session, now \\ System.system_time(:second)) do
    now - session.last_accessed > div(@session_timeout, 1000)
  end

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup_expired, @session_timeout)
  end
end
```

### 5. State Initialization with Arguments

```elixir
defmodule ConfigurableCache do
  use GenServer

  defstruct [:max_size, :ttl, :data, :access_times]

  ## Client API

  def start_link(opts) do
    max_size = Keyword.get(opts, :max_size, 100)
    ttl = Keyword.get(opts, :ttl, 3600)
    GenServer.start_link(__MODULE__, {max_size, ttl}, name: __MODULE__)
  end

  def put(key, value) do
    GenServer.cast(__MODULE__, {:put, key, value})
  end

  def get(key) do
    GenServer.call(__MODULE__, {:get, key})
  end

  def stats do
    GenServer.call(__MODULE__, :stats)
  end

  ## Server Callbacks

  @impl true
  def init({max_size, ttl}) do
    state = %__MODULE__{
      max_size: max_size,
      ttl: ttl,
      data: %{},
      access_times: %{}
    }
    {:ok, state}
  end

  @impl true
  def handle_call({:get, key}, _from, state) do
    now = System.system_time(:second)

    case Map.get(state.data, key) do
      nil ->
        {:reply, {:error, :not_found}, state}

      value ->
        case Map.get(state.access_times, key) do
          nil -> {:reply, {:error, :not_found}, state}
          timestamp when now - timestamp > state.ttl ->
            # Expired
            new_state = remove_entry(state, key)
            {:reply, {:error, :expired}, new_state}
          _timestamp ->
            # Update access time
            new_access_times = Map.put(state.access_times, key, now)
            new_state = %{state | access_times: new_access_times}
            {:reply, {:ok, value}, new_state}
        end
    end
  end

  @impl true
  def handle_call(:stats, _from, state) do
    stats = %{
      size: map_size(state.data),
      max_size: state.max_size,
      ttl: state.ttl
    }
    {:reply, stats, state}
  end

  @impl true
  def handle_cast({:put, key, value}, state) do
    now = System.system_time(:second)

    # Evict if at capacity
    state = if map_size(state.data) >= state.max_size do
      evict_oldest(state)
    else
      state
    end

    new_data = Map.put(state.data, key, value)
    new_access_times = Map.put(state.access_times, key, now)

    new_state = %{state | data: new_data, access_times: new_access_times}
    {:noreply, new_state}
  end

  ## Private Functions

  defp remove_entry(state, key) do
    %{state |
      data: Map.delete(state.data, key),
      access_times: Map.delete(state.access_times, key)
    }
  end

  defp evict_oldest(state) do
    {oldest_key, _time} = Enum.min_by(state.access_times, fn {_k, v} -> v end)
    remove_entry(state, oldest_key)
  end
end

{:ok, _pid} = ConfigurableCache.start_link(max_size: 3, ttl: 10)
ConfigurableCache.put(:a, 1)
ConfigurableCache.put(:b, 2)
ConfigurableCache.get(:a)        # {:ok, 1}
ConfigurableCache.stats()        # %{size: 2, max_size: 3, ttl: 10}
```

### 6. Synchronous vs Asynchronous

**Use `call` (synchronous) when:**

- Need return value
- Require confirmation
- Client must wait for operation

```elixir
def handle_call(:get_balance, _from, state) do
  {:reply, state.balance, state}
end
```

**Use `cast` (asynchronous) when:**

- Fire-and-forget
- Don't need return value
- Performance critical (no blocking)

```elixir
def handle_cast({:log_event, event}, state) do
  new_events = [event | state.events]
  {:noreply, %{state | events: new_events}}
end
```

**Use `handle_info` for messages not from GenServer API:**

```elixir
def handle_info({:DOWN, _ref, :process, _pid, _reason}, state) do
  # Handle monitored process crash
  {:noreply, state}
end

def handle_info(:tick, state) do
  # Periodic timer message
  schedule_tick()
  {:noreply, update_state(state)}
end
```

## Variations

### Named vs Unnamed GenServers

```elixir
GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
GenServer.call(__MODULE__, :get_value)

{:ok, pid} = GenServer.start_link(__MODULE__, :ok)
GenServer.call(pid, :get_value)

GenServer.start_link(__MODULE__, :ok, name: {:via, Registry, {MyRegistry, key}})
```

### Reply Formats

```elixir
{:reply, reply_value, new_state}

{:reply, reply_value, new_state, :hibernate}

{:stop, reason, reply_value, new_state}

{:noreply, new_state}
```

### Early Return and Async Reply

```elixir
def handle_call(:slow_operation, from, state) do
  Task.start(fn ->
    result = perform_slow_work()
    GenServer.reply(from, result)
  end)

  {:noreply, state}  # Don't block GenServer
end
```

## Pitfalls

### Blocking the GenServer

```elixir
def handle_call(:slow_fetch, _from, state) do
  result = HTTPoison.get!("http://slow-api.com")  # Blocks!
  {:reply, result, state}
end

def handle_call(:slow_fetch, from, state) do
  Task.start(fn ->
    result = HTTPoison.get!("http://slow-api.com")
    GenServer.reply(from, result)
  end)
  {:noreply, state}
end
```

### Forgetting to Update State

```elixir
def handle_cast({:increment, amount}, state) do
  state.counter + amount  # Oops! Just computes, doesn't return
  {:noreply, state}  # Old state returned
end

def handle_cast({:increment, amount}, state) do
  new_counter = state.counter + amount
  {:noreply, %{state | counter: new_counter}}
end
```

### Using `call` When `cast` Would Work

```elixir
def log_event(event) do
  GenServer.call(__MODULE__, {:log, event})  # Blocks caller
end

def log_event(event) do
  GenServer.cast(__MODULE__, {:log, event})
end
```

### Not Handling Crashes Properly

```elixir
def handle_call({:divide, a, b}, _from, state) do
  result = div(a, b)  # Crashes on b = 0!
  {:reply, result, state}
end

def handle_call({:divide, a, b}, _from, state) do
  case b do
    0 -> {:reply, {:error, :division_by_zero}, state}
    _ -> {:reply, {:ok, div(a, b)}, state}
  end
end
```

## Use Cases

**State Management:**

- Configuration stores
- Caches
- Session management
- Connection pools

**Coordination:**

- Rate limiters
- Circuit breakers
- Resource allocation
- Job queues

**Aggregation:**

- Metrics collection
- Event logging
- Statistics tracking

**Singleton Services:**

- Application-wide services
- Hardware interface controllers
- External API clients

## Related Resources

- [Processes and Message Passing](/en/learn/software-engineering/programming-language/elixir/how-to/processes-message-passing) - Foundation concepts
- [Supervision Trees](/en/learn/software-engineering/programming-language/elixir/how-to/supervision) - Fault tolerance with GenServer
- [Intermediate Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate) - GenServer fundamentals
- [Cookbook](/en/learn/software-engineering/programming-language/elixir/how-to/cookbook) - GenServer recipes

## Next Steps

1. Build a stateful cache with TTL and LRU eviction
2. Implement rate limiter using GenServer
3. Study Supervisor integration for fault tolerance
4. Learn GenServer testing with concurrent test cases
5. Explore GenStage for backpressure-aware pipelines
