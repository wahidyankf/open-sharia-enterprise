---
title: "Caching"
date: 2025-12-21T18:25:00+07:00
draft: false
description: "Implement efficient caching in Elixir using ETS, Agent, GenServer, and external stores like Redis for performance optimization."
weight: 1000021
tags: ["elixir", "caching", "ets", "performance", "optimization", "how-to"]
---

**Need to cache data for performance?** Elixir offers multiple caching strategies from in-memory ETS tables to external stores like Redis, with built-in concurrency support.

## Prerequisites

- Understanding of GenServer and processes
- Basic performance optimization concepts
- Completed [Intermediate Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate)

## Problem

Improving application performance requires caching frequently accessed data, avoiding expensive database queries, reducing API calls, and managing cache invalidation. You need strategies for different cache types (application-level, process-local, distributed), TTL management, cache warming, and eviction policies.

**Challenges:**

- Choosing appropriate caching strategy (ETS, Agent, GenServer, Redis)
- Managing cache expiration and TTL
- Implementing cache invalidation and updates
- Handling cache misses and race conditions
- Preventing cache stampedes
- Distributed caching across nodes

## Solution

Use **ETS** for fast in-memory caching, **Agent/GenServer** for stateful caching with custom logic, **Cachex** for feature-rich local caching, and **Redis** for distributed caching.

## How It Works

### 1. ETS Cache (Fastest)

Basic ETS cache:

```elixir
defmodule ETSCache do
  @table_name :my_cache

  def start_link do
    :ets.new(@table_name, [:set, :public, :named_table])
    :ok
  end

  def put(key, value, ttl_seconds \\ 3600) do
    expires_at = System.system_time(:second) + ttl_seconds
    :ets.insert(@table_name, {key, value, expires_at})
  end

  def get(key) do
    case :ets.lookup(@table_name, key) do
      [{^key, value, expires_at}] ->
        if System.system_time(:second) < expires_at do
          {:ok, value}
        else
          :ets.delete(@table_name, key)
          {:error, :expired}
        end

      [] ->
        {:error, :not_found}
    end
  end

  def delete(key) do
    :ets.delete(@table_name, key)
  end

  def clear do
    :ets.delete_all_objects(@table_name)
  end

  def size do
    :ets.info(@table_name, :size)
  end
end
```

ETS with background cleanup:

```elixir
defmodule ETSCache.Server do
  use GenServer

  @table_name :my_cache
  @cleanup_interval 60_000  # 1 minute

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def put(key, value, ttl_seconds \\ 3600) do
    expires_at = System.system_time(:second) + ttl_seconds
    :ets.insert(@table_name, {key, value, expires_at})
  end

  def get(key) do
    case :ets.lookup(@table_name, key) do
      [{^key, value, expires_at}] ->
        if System.system_time(:second) < expires_at do
          {:ok, value}
        else
          :ets.delete(@table_name, key)
          {:error, :expired}
        end

      [] ->
        {:error, :not_found}
    end
  end

  ## Callbacks

  def init(_opts) do
    :ets.new(@table_name, [:set, :public, :named_table])
    schedule_cleanup()
    {:ok, %{}}
  end

  def handle_info(:cleanup, state) do
    cleanup_expired()
    schedule_cleanup()
    {:noreply, state}
  end

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup, @cleanup_interval)
  end

  defp cleanup_expired do
    now = System.system_time(:second)

    :ets.select_delete(@table_name, [
      {{:"$1", :"$2", :"$3"}, [{:<, :"$3", now}], [true]}
    ])
  end
end
```

### 2. Agent Cache (Simple State)

```elixir
defmodule AgentCache do
  use Agent

  def start_link(_opts) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def put(key, value) do
    Agent.update(__MODULE__, fn cache ->
      Map.put(cache, key, {value, System.system_time(:second)})
    end)
  end

  def get(key, ttl_seconds \\ 3600) do
    Agent.get(__MODULE__, fn cache ->
      case Map.get(cache, key) do
        {value, timestamp} ->
          if System.system_time(:second) - timestamp < ttl_seconds do
            {:ok, value}
          else
            {:error, :expired}
          end

        nil ->
          {:error, :not_found}
      end
    end)
  end

  def delete(key) do
    Agent.update(__MODULE__, fn cache ->
      Map.delete(cache, key)
    end)
  end

  def clear do
    Agent.update(__MODULE__, fn _cache -> %{} end)
  end
end
```

### 3. GenServer Cache with LRU Eviction

```elixir
defmodule LRUCache do
  use GenServer

  defstruct data: %{}, access_order: [], max_size: 100

  ## Client API

  def start_link(opts \\ []) do
    max_size = Keyword.get(opts, :max_size, 100)
    GenServer.start_link(__MODULE__, max_size, name: __MODULE__)
  end

  def put(key, value) do
    GenServer.cast(__MODULE__, {:put, key, value})
  end

  def get(key) do
    GenServer.call(__MODULE__, {:get, key})
  end

  def delete(key) do
    GenServer.cast(__MODULE__, {:delete, key})
  end

  def stats do
    GenServer.call(__MODULE__, :stats)
  end

  ## Server Callbacks

  def init(max_size) do
    {:ok, %__MODULE__{max_size: max_size}}
  end

  def handle_call({:get, key}, _from, state) do
    case Map.get(state.data, key) do
      nil ->
        {:reply, {:error, :not_found}, state}

      value ->
        # Update access order (move to front)
        access_order = [key | List.delete(state.access_order, key)]
        {:reply, {:ok, value}, %{state | access_order: access_order}}
    end
  end

  def handle_call(:stats, _from, state) do
    stats = %{
      size: map_size(state.data),
      max_size: state.max_size,
      utilization: map_size(state.data) / state.max_size
    }
    {:reply, stats, state}
  end

  def handle_cast({:put, key, value}, state) do
    # Check if we need to evict
    state = if map_size(state.data) >= state.max_size and not Map.has_key?(state.data, key) do
      evict_lru(state)
    else
      state
    end

    # Add or update entry
    new_data = Map.put(state.data, key, value)
    new_access_order = [key | List.delete(state.access_order, key)]

    {:noreply, %{state | data: new_data, access_order: new_access_order}}
  end

  def handle_cast({:delete, key}, state) do
    new_data = Map.delete(state.data, key)
    new_access_order = List.delete(state.access_order, key)

    {:noreply, %{state | data: new_data, access_order: new_access_order}}
  end

  defp evict_lru(state) do
    [lru_key | rest_access] = Enum.reverse(state.access_order)
    %{state | data: Map.delete(state.data, lru_key), access_order: rest_access}
  end
end
```

### 4. Memoization with Function Caching

```elixir
defmodule MemoCache do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @doc """
  Memoize a function call. If the result is cached, return it.
  Otherwise, execute the function and cache the result.
  """
  def memoize(key, fun) when is_function(fun, 0) do
    case GenServer.call(__MODULE__, {:get, key}) do
      {:ok, value} ->
        value

      {:error, :not_found} ->
        value = fun.()
        GenServer.cast(__MODULE__, {:put, key, value})
        value
    end
  end

  def memoize(module, function, args) do
    key = {module, function, args}
    memoize(key, fn -> apply(module, function, args) end)
  end

  ## Callbacks

  def init(_) do
    {:ok, %{}}
  end

  def handle_call({:get, key}, _from, cache) do
    case Map.get(cache, key) do
      nil -> {:reply, {:error, :not_found}, cache}
      value -> {:reply, {:ok, value}, cache}
    end
  end

  def handle_cast({:put, key, value}, cache) do
    {:noreply, Map.put(cache, key, value)}
  end
end

# Usage
defmodule ExpensiveCalculations do
  def fibonacci(n) do
    MemoCache.memoize({:fib, n}, fn ->
      cond do
        n <= 1 -> n
        true -> fibonacci(n - 1) + fibonacci(n - 2)
      end
    end)
  end
end
```

### 5. Cachex (Feature-Rich Library)

Add dependency:

```elixir
{:cachex, "~> 3.6"}
```

Basic usage:

```elixir
defmodule MyCachex do
  def start_link do
    Cachex.start_link(:my_cache, [
      limit: 1000,               # Max entries
      expiration: [
        default: :timer.minutes(5)  # Default TTL
      ],
      stats: true                # Enable statistics
    ])
  end

  def put(key, value, ttl \\ :timer.minutes(5)) do
    Cachex.put(:my_cache, key, value, ttl: ttl)
  end

  def get(key) do
    case Cachex.get(:my_cache, key) do
      {:ok, nil} -> {:error, :not_found}
      {:ok, value} -> {:ok, value}
      error -> error
    end
  end

  def fetch(key, fallback_fun) do
    Cachex.fetch(:my_cache, key, fn _key ->
      {:commit, fallback_fun.()}
    end)
  end

  def delete(key) do
    Cachex.del(:my_cache, key)
  end

  def stats do
    Cachex.stats(:my_cache)
  end
end

# Usage with automatic fallback
MyCachex.fetch("user:123", fn ->
  Repo.get(User, 123)
end)
```

Advanced Cachex features:

```elixir
# Warming cache on startup
defmodule CacheWarmer do
  def warm_cache do
    users = Repo.all(User)

    Enum.each(users, fn user ->
      Cachex.put(:my_cache, "user:#{user.id}", user)
    end)
  end
end

# Hooks for logging
Cachex.start_link(:my_cache, [
  hooks: [
    %Cachex.Hook{
      module: CacheLogger,
      type: :post,
      actions: [:get, :put, :del]
    }
  ]
end)

defmodule CacheLogger do
  use Cachex.Hook

  def handle_notify({action, [key | _rest]}, _results) do
    IO.puts "Cache #{action}: #{inspect(key)}"
    {:ok, :continue}
  end
end
```

### 6. Redis Cache (Distributed)

Add dependency:

```elixir
{:redix, "~> 1.2"}
```

Redis cache module:

```elixir
defmodule RedisCache do
  def start_link(opts \\ []) do
    redis_url = Keyword.get(opts, :redis_url, "redis://localhost:6379")
    Redix.start_link(redis_url, name: __MODULE__)
  end

  def put(key, value, ttl_seconds \\ 3600) do
    encoded = Jason.encode!(value)

    case Redix.command(__MODULE__, ["SET", key, encoded, "EX", ttl_seconds]) do
      {:ok, "OK"} -> :ok
      error -> error
    end
  end

  def get(key) do
    case Redix.command(__MODULE__, ["GET", key]) do
      {:ok, nil} -> {:error, :not_found}
      {:ok, value} -> {:ok, Jason.decode!(value)}
      error -> error
    end
  end

  def delete(key) do
    Redix.command(__MODULE__, ["DEL", key])
  end

  def exists?(key) do
    case Redix.command(__MODULE__, ["EXISTS", key]) do
      {:ok, 1} -> true
      {:ok, 0} -> false
      _ -> false
    end
  end

  def ttl(key) do
    Redix.command(__MODULE__, ["TTL", key])
  end

  def increment(key, amount \\ 1) do
    Redix.command(__MODULE__, ["INCRBY", key, amount])
  end
end
```

## Variations

### Cache-Aside Pattern

```elixir
defmodule UserCache do
  def get_user(user_id) do
    key = "user:#{user_id}"

    case Cache.get(key) do
      {:ok, user} ->
        # Cache hit
        user

      {:error, :not_found} ->
        # Cache miss - fetch from database
        user = Repo.get(User, user_id)

        if user do
          Cache.put(key, user, ttl: :timer.minutes(10))
        end

        user
    end
  end

  def update_user(user_id, attrs) do
    case Repo.update_user(user_id, attrs) do
      {:ok, user} ->
        # Invalidate cache on update
        Cache.delete("user:#{user_id}")
        {:ok, user}

      error ->
        error
    end
  end
end
```

### Write-Through Cache

```elixir
defmodule WriteThroughCache do
  def put_user(user) do
    # Write to database first
    case Repo.insert_or_update(user) do
      {:ok, user} ->
        # Then write to cache
        Cache.put("user:#{user.id}", user)
        {:ok, user}

      error ->
        error
    end
  end
end
```

### Read-Through Cache

```elixir
defmodule ReadThroughCache do
  def get_user(user_id) do
    Cache.fetch("user:#{user_id}", fn ->
      # Automatically fetch from DB if not in cache
      case Repo.get(User, user_id) do
        nil -> {:ignore, nil}
        user -> {:commit, user}
      end
    end)
  end
end
```

### Cache Stampede Prevention

```elixir
defmodule StampedeProtection do
  use GenServer

  defstruct loading: %{}

  def start_link(_) do
    GenServer.start_link(__MODULE__, %__MODULE__{}, name: __MODULE__)
  end

  def fetch(key, loader_fun) do
    case Cache.get(key) do
      {:ok, value} ->
        value

      {:error, :not_found} ->
        GenServer.call(__MODULE__, {:fetch, key, loader_fun})
    end
  end

  ## Callbacks

  def init(state) do
    {:ok, state}
  end

  def handle_call({:fetch, key, loader_fun}, from, state) do
    case state.loading do
      %{^key => waiters} ->
        # Already loading, add to waiters
        {:noreply, %{state | loading: Map.put(state.loading, key, [from | waiters])}}

      _ ->
        # Start loading
        Task.start(fn ->
          value = loader_fun.()
          Cache.put(key, value)
          GenServer.cast(__MODULE__, {:loaded, key, value})
        end)

        {:noreply, %{state | loading: Map.put(state.loading, key, [from])}}
    end
  end

  def handle_cast({:loaded, key, value}, state) do
    waiters = Map.get(state.loading, key, [])

    Enum.each(waiters, fn from ->
      GenServer.reply(from, value)
    end)

    {:noreply, %{state | loading: Map.delete(state.loading, key)}}
  end
end
```

## Advanced Patterns

### 1. Multi-Level Cache

```elixir
defmodule MultiLevelCache do
  @doc """
  Check L1 (ETS) -> L2 (Cachex) -> Database
  """
  def get(key) do
    with {:error, :not_found} <- get_l1(key),
         {:error, :not_found} <- get_l2(key) do
      get_from_db(key)
    else
      {:ok, value} -> {:ok, value}
    end
  end

  defp get_l1(key) do
    ETSCache.get(key)
  end

  defp get_l2(key) do
    case Cachex.get(:l2_cache, key) do
      {:ok, nil} -> {:error, :not_found}
      {:ok, value} ->
        # Promote to L1
        ETSCache.put(key, value)
        {:ok, value}
      error -> error
    end
  end

  defp get_from_db(key) do
    # Fetch from database
    value = Database.fetch(key)

    if value do
      # Store in both levels
      ETSCache.put(key, value)
      Cachex.put(:l2_cache, key, value)
      {:ok, value}
    else
      {:error, :not_found}
    end
  end
end
```

### 2. Tag-Based Invalidation

```elixir
defmodule TaggedCache do
  def put(key, value, tags \\ []) do
    Cachex.put(:my_cache, key, value)

    # Store reverse mapping: tag -> keys
    Enum.each(tags, fn tag ->
      Cachex.get_and_update(:my_cache, {:tag, tag}, fn
        nil -> {nil, MapSet.new([key])}
        keys -> {nil, MapSet.put(keys, key)}
      end)
    end)
  end

  def invalidate_tag(tag) do
    case Cachex.get(:my_cache, {:tag, tag}) do
      {:ok, nil} -> :ok
      {:ok, keys} ->
        Enum.each(keys, fn key ->
          Cachex.del(:my_cache, key)
        end)
        Cachex.del(:my_cache, {:tag, tag})
    end
  end
end

# Usage
TaggedCache.put("post:1", post, tags: [:posts, "user:#{post.user_id}"])
TaggedCache.invalidate_tag("user:123")  # Invalidates all posts by user 123
```

### 3. Probabilistic Early Expiration (PER)

```elixir
defmodule PERCache do
  @doc """
  Probabilistic early expiration to prevent cache stampedes.
  Refreshes cache before expiration with increasing probability.
  """
  def get(key, ttl, refresh_fun) do
    case Cache.get(key) do
      {:ok, {value, set_at}} ->
        age = System.system_time(:second) - set_at

        # Probability of refresh increases as TTL approaches
        refresh_probability = age / ttl

        if :rand.uniform() < refresh_probability do
          # Refresh in background
          Task.start(fn ->
            new_value = refresh_fun.()
            Cache.put(key, {new_value, System.system_time(:second)}, ttl: ttl)
          end)
        end

        {:ok, value}

      {:error, :not_found} ->
        value = refresh_fun.()
        Cache.put(key, {value, System.system_time(:second)}, ttl: ttl)
        {:ok, value}
    end
  end
end
```

## Use Cases

**Performance Optimization:**

- Database query results
- API responses
- Computed values (fibonacci, statistics)
- Session data
- Rate limiting counters

**Distributed Systems:**

- Shared state across nodes
- Distributed locks
- Feature flags
- Configuration data

**Real-Time Applications:**

- Leaderboards
- User presence
- Chat room members
- Live statistics

## Best Practices

1. **Choose appropriate cache type:**
   - ETS: Single node, ultra-fast, ephemeral
   - Cachex: Single node, feature-rich
   - Redis: Distributed, persistent

2. **Set appropriate TTL:**

   ```elixir
   # Frequently changing data
   Cache.put(key, value, ttl: :timer.minutes(1))

   # Relatively stable data
   Cache.put(key, value, ttl: :timer.hours(1))
   ```

3. **Handle cache misses gracefully:**
   Always have a fallback to the source of truth

4. **Invalidate on writes:**
   Delete or update cache when underlying data changes

5. **Monitor cache hit rates:**

   ```elixir
   Cachex.stats(:my_cache)
   ```

6. **Warm critical caches on startup:**
   Preload frequently accessed data

## Common Pitfalls

1. **Over-caching:** Caching everything reduces memory, use TTL
2. **Stale data:** Not invalidating cache on updates
3. **Cache stampede:** Many requests loading same data simultaneously
4. **No eviction policy:** Memory exhaustion
5. **Ignoring failures:** Cache errors should not break application
6. **Serialization overhead:** Redis requires encoding/decoding

## Troubleshooting

### Memory Usage

```elixir
# Check ETS memory
:ets.info(:my_cache, :memory) * :erlang.system_info(:wordsize)

# Implement size limits
Cachex.start_link(:my_cache, limit: 1000)
```

### Cache Not Expiring

```elixir
# Verify TTL
Cachex.ttl(:my_cache, key)

# Enable background cleanup for ETS
# (See ETSCache.Server example above)
```

### Serialization Errors

```elixir
# Ensure data is JSON-serializable for Redis
defimpl Jason.Encoder, for: MyStruct do
  def encode(value, opts) do
    Jason.Encode.map(Map.from_struct(value), opts)
  end
end
```

## Related Resources

- [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver)
- [Performance Guide](/en/learn/software-engineering/programming-language/elixir/how-to/performance)
- [ETS Documentation](https://www.erlang.org/doc/man/ets.html)
- [Cachex Documentation](https://hexdocs.pm/cachex/)
- [Redix Documentation](https://hexdocs.pm/redix/)
