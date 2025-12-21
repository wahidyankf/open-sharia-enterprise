---
title: "Concurrent Programming with Task and Agent"
date: 2025-12-21T17:10:00+07:00
draft: false
description: "Master lightweight concurrency in Elixir using Task for async operations and Agent for simple state management without full GenServer complexity."
weight: 1000006
tags: ["elixir", "task", "agent", "concurrency", "async", "how-to"]
---

# Concurrent Programming with Task and Agent

**Need lightweight concurrency without GenServer complexity?** This guide teaches you Task for async operations and Agent for simple state containers with practical patterns for parallel processing and state management.

## Prerequisites

- Understanding of processes
- Basic GenServer knowledge
- Completed [Intermediate Tutorial](/learn/swe/prog-lang/elixir/tutorials/intermediate)

## Problem

GenServer is powerful but heavyweight for simple async operations or basic state storage. You need lighter abstractions for common concurrency patterns like running parallel computations, managing simple state without callback boilerplate, handling async timeouts, and coordinating concurrent tasks.

**Challenges:**

- Running concurrent operations and collecting results
- Simple state without callback boilerplate
- Handling async timeouts and failures
- Managing task supervision
- Coordinating multiple tasks
- Updating shared state safely

## Solution

Use **Task** for concurrent computations with built-in supervision and **Agent** for simple state containers without the overhead of GenServer callbacks.

## How It Works

### Task Patterns

#### 1. Basic Async/Await

```elixir
# Start async task
task = Task.async(fn ->
  :timer.sleep(1000)
  "Result"
end)

# Do other work...
IO.puts "Working on other things..."

# Wait for result (default 5s timeout)
result = Task.await(task)  # "Result"
```

Real-world example:

```elixir
defmodule UserLoader do
  def load_user_with_details(user_id) do
    # Load user and their data concurrently
    user_task = Task.async(fn -> Repo.get(User, user_id) end)
    posts_task = Task.async(fn -> Repo.get_posts_by_user(user_id) end)
    comments_task = Task.async(fn -> Repo.get_comments_by_user(user_id) end)

    # Wait for all results
    user = Task.await(user_task)
    posts = Task.await(posts_task)
    comments = Task.await(comments_task)

    %{user: user, posts: posts, comments: comments}
  end
end
```

#### 2. Multiple Concurrent Tasks

```elixir
defmodule ParallelProcessor do
  def fetch_all_users(user_ids) do
    tasks = Enum.map(user_ids, fn id ->
      Task.async(fn -> fetch_user(id) end)
    end)

    # Wait for all tasks (default 5s timeout each)
    Task.await_many(tasks)
  end

  defp fetch_user(id) do
    # Simulate API call
    :timer.sleep(100)
    %{id: id, name: "User #{id}"}
  end
end

# Usage
ParallelProcessor.fetch_all_users([1, 2, 3, 4, 5])
```

With custom timeout:

```elixir
def fetch_with_timeout(user_ids) do
  tasks = Enum.map(user_ids, fn id ->
    Task.async(fn -> slow_api_call(id) end)
  end)

  # 10 second timeout for each task
  Task.await_many(tasks, 10_000)
end
```

#### 3. Supervised Tasks

Add to application supervisor:

```elixir
# lib/my_app/application.ex
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Task.Supervisor, name: MyApp.TaskSupervisor}
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

Using supervised tasks:

```elixir
defmodule SafeProcessor do
  def process_risky_operation(data) do
    task = Task.Supervisor.async(MyApp.TaskSupervisor, fn ->
      risky_operation(data)
    end)

    # If task crashes, it's supervised and won't take down caller
    case Task.await(task, 5000) do
      result -> {:ok, result}
    catch
      :exit, _ -> {:error, :task_failed}
    end
  end

  defp risky_operation(data) do
    # Might raise or crash
    external_api_call(data)
  end
end
```

#### 4. Fire-and-Forget

Unsupervised (use with caution):

```elixir
# Task completes in background, no result needed
Task.start(fn ->
  send_email(user)
end)

# Continue without waiting
:ok
```

Supervised (recommended):

```elixir
Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
  cleanup_old_data()
end)

# If task crashes, supervisor handles restart
```

#### 5. Task.async_stream for Enumerable Processing

Process items concurrently with back-pressure:

```elixir
defmodule BatchProcessor do
  def process_urls(urls) do
    urls
    |> Task.async_stream(&fetch_url/1, max_concurrency: 10, timeout: 5000)
    |> Enum.map(fn
      {:ok, result} -> result
      {:exit, reason} -> {:error, reason}
    end)
  end

  defp fetch_url(url) do
    HTTPoison.get!(url)
    |> Map.get(:body)
  end
end

# Processes up to 10 URLs concurrently
BatchProcessor.process_urls([
  "https://example.com/1",
  "https://example.com/2",
  # ... 100 more URLs
])
```

Advanced async_stream options:

```elixir
urls
|> Task.async_stream(
  &fetch_url/1,
  max_concurrency: 20,       # Max parallel tasks
  timeout: 10_000,           # Per-task timeout
  on_timeout: :kill_task,    # Kill tasks that timeout
  ordered: false             # Results can be out of order (faster)
)
|> Stream.filter(fn
  {:ok, _} -> true
  {:exit, _} -> false       # Filter out failures
end)
|> Enum.to_list()
```

#### 6. Task.yield for Manual Control

```elixir
defmodule FlexibleProcessor do
  def process_with_manual_control(data) do
    task = Task.async(fn -> expensive_operation(data) end)

    # Do other work while task runs
    other_result = quick_operation()

    # Check if task completed (1 second timeout)
    case Task.yield(task, 1000) do
      {:ok, result} ->
        # Task completed within timeout
        {:ok, result, other_result}

      nil ->
        # Task still running, shut it down
        Task.shutdown(task, :brutal_kill)
        {:error, :timeout, other_result}
    end
  end
end
```

### Agent Patterns

#### 1. Simple Counter

```elixir
defmodule Counter do
  use Agent

  def start_link(initial_value \\ 0) do
    Agent.start_link(fn -> initial_value end, name: __MODULE__)
  end

  def increment do
    Agent.update(__MODULE__, fn count -> count + 1 end)
  end

  def decrement do
    Agent.update(__MODULE__, fn count -> count - 1 end)
  end

  def get do
    Agent.get(__MODULE__, fn count -> count end)
  end

  def reset do
    Agent.update(__MODULE__, fn _ -> 0 end)
  end
end

# Usage
{:ok, _pid} = Counter.start_link(10)
Counter.increment()  # 11
Counter.increment()  # 12
Counter.get()        # 12
Counter.reset()      # 0
```

#### 2. Key-Value Store

```elixir
defmodule KVStore do
  use Agent

  def start_link(_opts) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def get(key) do
    Agent.get(__MODULE__, fn map -> Map.get(map, key) end)
  end

  def put(key, value) do
    Agent.update(__MODULE__, fn map -> Map.put(map, key, value) end)
  end

  def delete(key) do
    Agent.update(__MODULE__, fn map -> Map.delete(map, key) end)
  end

  def get_all do
    Agent.get(__MODULE__, fn map -> map end)
  end

  def keys do
    Agent.get(__MODULE__, fn map -> Map.keys(map) end)
  end
end
```

#### 3. Configuration Store

```elixir
defmodule Config do
  use Agent

  def start_link(initial_config) do
    Agent.start_link(fn -> initial_config end, name: __MODULE__)
  end

  def get(key, default \\ nil) do
    Agent.get(__MODULE__, fn config ->
      get_in(config, List.wrap(key)) || default
    end)
  end

  def put(key, value) do
    Agent.update(__MODULE__, fn config ->
      put_in(config, List.wrap(key), value)
    end)
  end

  def update(key, fun) do
    Agent.get_and_update(__MODULE__, fn config ->
      current_value = get_in(config, List.wrap(key))
      new_value = fun.(current_value)
      new_config = put_in(config, List.wrap(key), new_value)
      {new_value, new_config}
    end)
  end
end

# Usage
Config.start_link(%{
  database: %{host: "localhost", port: 5432},
  cache: %{ttl: 3600}
})

Config.get([:database, :host])  # "localhost"
Config.put([:database, :port], 5433)
Config.update([:cache, :ttl], &(&1 * 2))  # Double the TTL
```

#### 4. Agent with Complex State

```elixir
defmodule GameState do
  use Agent

  defstruct players: [], score: %{}, round: 1

  def start_link(_opts) do
    Agent.start_link(fn -> %__MODULE__{} end, name: __MODULE__)
  end

  def add_player(player_name) do
    Agent.update(__MODULE__, fn state ->
      %{state |
        players: [player_name | state.players],
        score: Map.put(state.score, player_name, 0)
      }
    end)
  end

  def update_score(player_name, points) do
    Agent.update(__MODULE__, fn state ->
      %{state | score: Map.update(state.score, player_name, points, &(&1 + points))}
    end)
  end

  def next_round do
    Agent.update(__MODULE__, fn state ->
      %{state | round: state.round + 1}
    end)
  end

  def get_state do
    Agent.get(__MODULE__, fn state -> state end)
  end

  def leaderboard do
    Agent.get(__MODULE__, fn state ->
      state.score
      |> Enum.sort_by(fn {_name, score} -> score end, :desc)
    end)
  end
end
```

#### 5. Agent with Timeout and Cleanup

```elixir
defmodule SessionStore do
  use Agent

  def start_link(_opts) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def create_session(user_id) do
    session_id = generate_session_id()
    expires_at = System.system_time(:second) + 3600  # 1 hour

    Agent.update(__MODULE__, fn sessions ->
      Map.put(sessions, session_id, %{
        user_id: user_id,
        expires_at: expires_at,
        created_at: DateTime.utc_now()
      })
    end)

    session_id
  end

  def get_session(session_id) do
    Agent.get(__MODULE__, fn sessions ->
      case Map.get(sessions, session_id) do
        nil -> {:error, :not_found}
        session ->
          if session.expires_at > System.system_time(:second) do
            {:ok, session}
          else
            {:error, :expired}
          end
      end
    end)
  end

  def delete_session(session_id) do
    Agent.update(__MODULE__, fn sessions ->
      Map.delete(sessions, session_id)
    end)
  end

  def cleanup_expired do
    now = System.system_time(:second)

    Agent.update(__MODULE__, fn sessions ->
      Enum.reject(sessions, fn {_id, session} ->
        session.expires_at <= now
      end)
      |> Map.new()
    end)
  end

  defp generate_session_id do
    :crypto.strong_rand_bytes(32) |> Base.encode64()
  end
end
```

## Variations

### Task with Custom Timeout

```elixir
task = Task.async(fn -> slow_operation() end)
Task.await(task, 10_000)  # 10 second timeout
```

### Agent with Complex State Updates

```elixir
defmodule Analytics do
  use Agent

  def start_link(_opts) do
    Agent.start_link(fn ->
      %{
        page_views: %{},
        unique_visitors: MapSet.new(),
        total_requests: 0
      }
    end, name: __MODULE__)
  end

  def track_page_view(page, visitor_id) do
    Agent.update(__MODULE__, fn state ->
      %{state |
        page_views: Map.update(state.page_views, page, 1, &(&1 + 1)),
        unique_visitors: MapSet.put(state.unique_visitors, visitor_id),
        total_requests: state.total_requests + 1
      }
    end)
  end

  def get_stats do
    Agent.get(__MODULE__, fn state ->
      %{
        total_page_views: Enum.sum(Map.values(state.page_views)),
        unique_visitors: MapSet.size(state.unique_visitors),
        total_requests: state.total_requests,
        popular_pages: Enum.sort_by(state.page_views, fn {_k, v} -> v end, :desc)
                       |> Enum.take(5)
      }
    end)
  end
end
```

### Coordinating Tasks

```elixir
defmodule Pipeline do
  def process_data(input) do
    # Stage 1: Parse
    parse_task = Task.async(fn -> parse(input) end)
    parsed = Task.await(parse_task)

    # Stage 2: Transform (parallel)
    transform_tasks = Enum.map(parsed, fn item ->
      Task.async(fn -> transform(item) end)
    end)
    transformed = Task.await_many(transform_tasks)

    # Stage 3: Aggregate
    aggregate_task = Task.async(fn -> aggregate(transformed) end)
    Task.await(aggregate_task)
  end

  defp parse(input), do: String.split(input, ",")
  defp transform(item), do: String.upcase(item)
  defp aggregate(items), do: Enum.join(items, " | ")
end
```

## Advanced Patterns

### 1. Task Pool Pattern

```elixir
defmodule TaskPool do
  def parallel_map(collection, fun, opts \\ []) do
    max_concurrency = Keyword.get(opts, :max_concurrency, System.schedulers_online() * 2)

    collection
    |> Task.async_stream(fun, max_concurrency: max_concurrency)
    |> Enum.map(fn {:ok, result} -> result end)
  end
end

# Usage - Process 1000 items with max 20 concurrent tasks
TaskPool.parallel_map(1..1000, fn n ->
  expensive_operation(n)
end, max_concurrency: 20)
```

### 2. Agent with ETS for Performance

```elixir
defmodule HybridCache do
  use Agent

  def start_link(_opts) do
    :ets.new(:cache, [:named_table, :public, :set])
    Agent.start_link(fn -> :ok end, name: __MODULE__)
  end

  def put(key, value) do
    :ets.insert(:cache, {key, value})
  end

  def get(key) do
    case :ets.lookup(:cache, key) do
      [{^key, value}] -> {:ok, value}
      [] -> {:error, :not_found}
    end
  end

  # Agent only for coordination, ETS for fast reads
end
```

### 3. Task Retry Pattern

```elixir
defmodule RetryTask do
  def async_with_retry(fun, retries \\ 3) do
    Task.async(fn ->
      retry(fun, retries)
    end)
  end

  defp retry(fun, retries) when retries > 0 do
    try do
      fun.()
    rescue
      error ->
        if retries > 1 do
          :timer.sleep(1000)  # Wait before retry
          retry(fun, retries - 1)
        else
          reraise error, __STACKTRACE__
        end
    end
  end
end

# Usage
task = RetryTask.async_with_retry(fn ->
  unreliable_api_call()
end, 5)

Task.await(task)
```

### 4. Distributed Agent Pattern

```elixir
defmodule DistributedCounter do
  use Agent

  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    Agent.start_link(fn -> 0 end, name: {:global, name})
  end

  def increment(name \\ __MODULE__) do
    Agent.update({:global, name}, fn count -> count + 1 end)
  end

  def get(name \\ __MODULE__) do
    Agent.get({:global, name}, fn count -> count end)
  end
end

# Works across connected Elixir nodes
```

## Use Cases

**Task:**

- Parallel API calls
- Background jobs
- CPU-intensive computations
- Batch processing
- Database queries in parallel
- File processing pipelines

**Agent:**

- Application configuration
- Simple caches
- Counters/metrics
- Temporary state
- Session storage
- Feature flags

**Combined:**

- Fan-out/fan-in patterns
- Parallel processing with shared state
- Worker pools
- Rate limiting
- Circuit breakers

## Best Practices

1. **Use Task.Supervisor for production:**

   ```elixir
   # Don't
   Task.async(fn -> risky_operation() end)

   # Do
   Task.Supervisor.async(MyApp.TaskSupervisor, fn -> risky_operation() end)
   ```

2. **Always set timeouts:**

   ```elixir
   Task.await(task, 5_000)  # Explicit timeout
   ```

3. **Handle task failures:**

   ```elixir
   try do
     Task.await(task)
   catch
     :exit, _ -> handle_failure()
   end
   ```

4. **Keep Agent updates fast:**

   ```elixir
   # Don't - slow operation in Agent
   Agent.update(MyAgent, fn state ->
     slow_computation(state)
   end)

   # Do - compute outside, update quickly
   new_value = slow_computation(old_value)
   Agent.update(MyAgent, fn _ -> new_value end)
   ```

5. **Use get_and_update for atomic operations:**
   ```elixir
   Agent.get_and_update(MyAgent, fn state ->
     {state.value, %{state | value: new_value}}
   end)
   ```

## Common Pitfalls

1. **Not using supervision:** Tasks crash and bring down caller
2. **Missing timeouts:** Tasks hang forever
3. **Blocking Agent calls:** Slow operations block all access
4. **Too many concurrent tasks:** Memory exhaustion
5. **Not handling failures:** Errors propagate unexpectedly
6. **Race conditions:** Multiple Agents instead of one GenServer

## Troubleshooting

### Task Timeout

```elixir
# Increase timeout
Task.await(task, 30_000)

# Or shutdown gracefully
Task.shutdown(task, 5_000)
```

### Agent Bottleneck

```elixir
# If Agent is slow, use ETS or GenServer instead
:ets.new(:my_table, [:named_table, :public])
:ets.insert(:my_table, {:key, :value})
:ets.lookup(:my_table, :key)
```

### Memory Leaks in Agents

```elixir
# Regularly cleanup stale data
def cleanup_old_entries do
  Agent.update(__MODULE__, fn state ->
    Enum.filter(state, &is_recent?/1)
  end)
end
```

## Related Resources

- [GenServer Guide](/learn/swe/prog-lang/elixir/how-to/genserver)
- [Processes Guide](/learn/swe/prog-lang/elixir/how-to/processes-message-passing)
- [Supervision Guide](/learn/swe/prog-lang/elixir/how-to/supervision)
- [Task Documentation](https://hexdocs.pm/elixir/Task.html)
- [Agent Documentation](https://hexdocs.pm/elixir/Agent.html)
- [Task.Supervisor Documentation](https://hexdocs.pm/elixir/Task.Supervisor.html)
