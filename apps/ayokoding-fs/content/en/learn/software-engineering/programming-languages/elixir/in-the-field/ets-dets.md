---
title: "Ets Dets"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000011
description: "In-memory and disk-based table storage for high-performance data access patterns"
tags: ["elixir", "ets", "dets", "storage", "concurrency", "performance"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/pattern-matching-production"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/persistent-term"
---

## When Standard Maps Fall Short

Standard Elixir maps provide excellent in-memory storage, but face limitations in concurrent, high-performance scenarios.

```elixir
# Campaign cache using Map
defmodule CampaignCache do
  # => Map stored in module attribute
  # => Problem: No concurrent writes
  @campaigns %{
    "ramadan_2026" => %{goal: 100_000_000, raised: 45_000_000}
  }

  def get(id), do: Map.get(@campaigns, id)
  # => Read-only access
  # => Cannot update at runtime
end
```

**Map Limitations**:

- **No concurrent writes** - Module attributes are immutable
- **No persistence** - Data lost on application restart
- **Process-bound state** - Cannot share across processes efficiently
- **Memory duplication** - Each process needs full copy

## ETS: Erlang Term Storage

ETS provides mutable, concurrent in-memory tables accessible across processes.

### Creating ETS Tables

```elixir
# Create public ETS table
table = :ets.new(:campaign_cache, [
  # => Table name: :campaign_cache
  :set,
  # => Type: set (unique keys)
  :public,
  # => Access: any process can read/write
  :named_table
  # => Named table: access by name instead of reference
])
# => Returns table reference (or name if :named_table)

# Insert data
:ets.insert(:campaign_cache, {"ramadan_2026", %{
  goal: 100_000_000,
  # => Goal: 100 million IDR
  raised: 45_000_000
  # => Current: 45 million IDR
}})
# => Returns true (success)

# Read data
{id, data} = :ets.lookup(:campaign_cache, "ramadan_2026") |> hd()
# => lookup returns list of tuples
# => hd() gets first element
# => id is "ramadan_2026", data is map
IO.puts("#{data.raised} / #{data.goal}")
# => Output: 45000000 / 100000000
```

### Table Types

**Four table types for different access patterns**:

```elixir
# :set - One value per key (default)
:ets.new(:unique_donors, [:set, :public, :named_table])
# => Each donor_id has one record
:ets.insert(:unique_donors, {"donor123", %{name: "Ahmad"}})
:ets.insert(:unique_donors, {"donor123", %{name: "Fatimah"}})
# => Second insert replaces first

# :ordered_set - Sorted by key
:ets.new(:campaigns_by_date, [:ordered_set, :public, :named_table])
# => Maintained in key order
:ets.insert(:campaigns_by_date, {~D[2026-03-15], "campaign_1"})
:ets.insert(:campaigns_by_date, {~D[2026-01-10], "campaign_2"})
# => Internally sorted by date

# :bag - Multiple values per key (duplicates allowed)
:ets.new(:donations_by_campaign, [:bag, :public, :named_table])
# => One campaign can have multiple donations
:ets.insert(:donations_by_campaign, {"ramadan_2026", {100_000, "Ahmad"}})
:ets.insert(:donations_by_campaign, {"ramadan_2026", {250_000, "Fatimah"}})
# => Both stored

# :duplicate_bag - Like :bag but allows identical entries
:ets.new(:audit_log, [:duplicate_bag, :public, :named_table])
# => Allows exact duplicates
:ets.insert(:audit_log, {"event", "login"})
:ets.insert(:audit_log, {"event", "login"})
# => Both identical entries stored
```

### Concurrency Options

**Optimize for read-heavy or write-heavy workloads**:

```elixir
# Read-optimized table
:ets.new(:campaign_cache, [
  :set,
  :public,
  :named_table,
  {:read_concurrency, true}
  # => Optimizes concurrent reads
  # => Use for read-heavy workloads
])
# => Multiple processes can read simultaneously without blocking

# Benchmark: Read-heavy pattern
defmodule CacheReader do
  # => Spawns 1000 readers
  def benchmark do
    Enum.each(1..1000, fn _ ->
      # => Each iteration spawns process
      spawn(fn ->
        :ets.lookup(:campaign_cache, "ramadan_2026")
        # => Concurrent read
        # => No lock contention with read_concurrency
      end)
    end)
  end
end

# Write-optimized table
:ets.new(:donation_counter, [
  :set,
  :public,
  :named_table,
  {:write_concurrency, true}
  # => Optimizes concurrent writes
  # => Reduces lock granularity
])
# => Multiple processes can write to different keys concurrently

# Update from multiple processes
defmodule DonationUpdater do
  # => Concurrent donation processing
  def record_donation(campaign_id, amount) do
    :ets.update_counter(
      :donation_counter,
      campaign_id,
      {2, amount}
      # => Atomic counter update
      # => Position 2 in tuple, increment by amount
    )
    # => Thread-safe, no race conditions
  end
end
```

### Access Control

**Three access levels**:

```elixir
# :public - Any process can read/write
:ets.new(:public_cache, [:set, :public, :named_table])
# => Any process: full access

# :protected - Owner writes, others read (default)
table = :ets.new(:protected_cache, [:set, :protected])
# => Owner process: read + write
# => Other processes: read only
:ets.insert(table, {"key", "value"})
# => Success if called by owner
# => Error if called by other process

# :private - Owner only
table = :ets.new(:private_cache, [:set, :private])
# => Only owner process can access
# => Other processes: no access at all
```

### Production Pattern: Donation Cache

```elixir
defmodule DonationCache do
  # => GenServer managing ETS table
  use GenServer

  @table_name :donation_cache
  # => Named table for easy access

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
    # => Registered process
  end

  @impl true
  def init(_) do
    # => Create table on init
    table = :ets.new(@table_name, [
      :set,
      # => Unique campaign IDs
      :public,
      # => Allow direct access from any process
      :named_table,
      # => Access by name
      {:read_concurrency, true}
      # => Optimize for frequent reads
    ])
    # => Returns table reference
    {:ok, %{table: table}}
    # => Store reference in state
  end

  # Client API - Direct ETS access (no GenServer call)
  def get(campaign_id) do
    # => Direct read from any process
    case :ets.lookup(@table_name, campaign_id) do
      [{^campaign_id, data}] -> {:ok, data}
      # => Pattern match: id matches, extract data
      [] -> {:error, :not_found}
      # => Empty list: key not found
    end
  end

  def put(campaign_id, data) do
    # => Direct write from any process
    :ets.insert(@table_name, {campaign_id, data})
    # => Returns true
    :ok
  end

  def increment_raised(campaign_id, amount) do
    # => Atomic counter update
    :ets.update_counter(
      @table_name,
      campaign_id,
      {2, amount}
      # => Tuple position 2 (assuming {id, count, ...})
      # => Increment by amount
    )
  end
end

# Usage - No GenServer bottleneck
{:ok, _pid} = DonationCache.start_link([])
# => Start GenServer (creates table)

DonationCache.put("ramadan_2026", %{goal: 100_000_000, raised: 0})
# => Direct ETS insert, no GenServer call

# 1000 concurrent readers
Enum.each(1..1000, fn _ ->
  # => Spawn 1000 processes
  spawn(fn ->
    DonationCache.get("ramadan_2026")
    # => Direct ETS read
    # => No GenServer bottleneck
  end)
end)

# 100 concurrent writers
Enum.each(1..100, fn i ->
  # => Spawn 100 processes
  spawn(fn ->
    DonationCache.increment_raised("ramadan_2026", i * 10_000)
    # => Atomic counter increment
    # => Thread-safe
  end)
end)
```

## DETS: Disk-Based ETS

DETS provides disk-backed persistence with ETS-like API.

### When to Use DETS

**Use DETS when**:

- Data must survive application restarts
- Dataset too large for memory
- Acceptable performance trade-off (slower than ETS)

**Don't use DETS when**:

- Need high write throughput (use database)
- Need complex queries (use database)
- Need ACID transactions (use database)

```elixir
# Open DETS table
{:ok, table} = :dets.open_file(:donation_history, [
  type: :set,
  # => Unique keys like ETS :set
  file: 'donation_history.dets'
  # => Charlist filename (note single quotes)
  # => Persisted to disk
])
# => Returns {:ok, table_reference}

# Insert data (persisted to disk)
:dets.insert(table, {"donation_123", %{
  # => Donation ID as key
  campaign: "ramadan_2026",
  amount: 500_000,
  # => 500k IDR
  donor: "Ahmad",
  timestamp: DateTime.utc_now()
  # => Current UTC time
}})
# => Returns :ok
# => Data written to disk

# Read data (from disk)
[{id, data}] = :dets.lookup(table, "donation_123")
# => Reads from disk
# => Returns list of tuples
IO.inspect(data.amount)
# => Output: 500000

# Close table (flush to disk)
:dets.close(table)
# => Ensures all writes flushed
# => Returns :ok

# Reopen after restart
{:ok, table} = :dets.open_file(:donation_history, [type: :set])
# => Data persisted from previous session
[{_id, data}] = :dets.lookup(table, "donation_123")
# => Still available after restart
```

### DETS Limitations

```elixir
# File size limit: 2GB
# => DETS cannot exceed 2GB file size
# => Use database for larger datasets

# No concurrency optimization
# => No :read_concurrency or :write_concurrency options
# => Single-file locking

# Limited types
# => Only :set and :bag (no :ordered_set or :duplicate_bag)

# Sync required for durability
:dets.sync(table)
# => Force flush to disk
# => Without sync, recent writes may be lost on crash
```

### Production Pattern: Audit Log

```elixir
defmodule AuditLog do
  # => DETS-backed audit logging
  use GenServer

  @table_name :audit_log
  @file_path 'data/audit_log.dets'
  # => Charlist path

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
    # => Registered GenServer
  end

  @impl true
  def init(_) do
    # => Open DETS file on init
    File.mkdir_p!("data")
    # => Ensure directory exists

    {:ok, table} = :dets.open_file(@table_name, [
      type: :bag,
      # => Multiple events per timestamp
      file: @file_path
      # => Persisted to disk
    ])

    {:ok, %{table: table}}
    # => Store reference in state
  end

  @impl true
  def terminate(_reason, %{table: table}) do
    # => Cleanup on shutdown
    :dets.sync(table)
    # => Flush pending writes
    :dets.close(table)
    # => Close file
    :ok
  end

  # Client API
  def log_event(event_type, metadata) do
    # => Async call to avoid blocking
    GenServer.cast(__MODULE__, {:log_event, event_type, metadata})
  end

  def get_events(event_type) do
    # => Sync call to retrieve events
    GenServer.call(__MODULE__, {:get_events, event_type})
  end

  # Server Callbacks
  @impl true
  def handle_cast({:log_event, event_type, metadata}, %{table: table} = state) do
    # => Async event logging
    :dets.insert(table, {event_type, %{
      metadata: metadata,
      timestamp: DateTime.utc_now()
      # => Log timestamp
    }})
    # => Written to disk

    # Periodic sync (every 100 events)
    if :rand.uniform(100) == 1 do
      # => Random 1% chance
      :dets.sync(table)
      # => Flush to disk
    end

    {:noreply, state}
    # => Continue
  end

  @impl true
  def handle_call({:get_events, event_type}, _from, %{table: table} = state) do
    # => Sync event retrieval
    events = :dets.lookup(table, event_type)
    # => Returns list of {event_type, data} tuples
    # => All events with matching type

    {:reply, events, state}
    # => Return events to caller
  end
end

# Usage
{:ok, _pid} = AuditLog.start_link([])
# => Start GenServer (opens DETS)

AuditLog.log_event(:donation_received, %{
  campaign: "ramadan_2026",
  amount: 1_000_000,
  donor: "Fatimah"
})
# => Async log (persisted to disk)

AuditLog.log_event(:donation_received, %{
  campaign: "education_2026",
  amount: 500_000,
  donor: "Ahmad"
})
# => Another donation logged

events = AuditLog.get_events(:donation_received)
# => Retrieve all donation events
# => Returns list: [{:donation_received, %{metadata: ..., timestamp: ...}}, ...]
IO.inspect(length(events))
# => Output: 2 (both donations)
```

## When to Use Mnesia

Mnesia provides distributed ETS with transactions.

**Use Mnesia when**:

- Need distributed tables across nodes
- Need ACID transactions
- Need complex queries
- ETS + replication required

```elixir
# Simple Mnesia example (not production-ready)
# => Start Mnesia
:mnesia.start()
# => Starts Mnesia application

# Create distributed table
:mnesia.create_table(:campaigns, [
  attributes: [:id, :goal, :raised],
  # => Table columns
  disc_copies: [node()],
  # => Disk + memory on this node
  type: :set
  # => Unique keys
])
# => Returns {:atomic, :ok}

# Transaction for consistency
:mnesia.transaction(fn ->
  # => ACID transaction
  :mnesia.write({:campaigns, "ramadan_2026", 100_000_000, 45_000_000})
  # => Write operation
  # => Tuple: {table, key, field1, field2, ...}
end)
# => Returns {:atomic, :ok} on success

# Read in transaction
{:atomic, [campaign]} = :mnesia.transaction(fn ->
  # => Read operation
  :mnesia.read({:campaigns, "ramadan_2026"})
  # => Returns list of records
end)
# => campaign is {:campaigns, "ramadan_2026", 100000000, 45000000}
```

## Choosing Storage Solution

```elixir
# Decision tree
defmodule StorageDecision do
  # => Helper for choosing storage

  def choose(requirements) do
    cond do
      # => Conditional decision logic

      requirements.distributed? ->
        # => Need data on multiple nodes?
        :mnesia
        # => Use Mnesia for distribution

      requirements.persistent? and requirements.simple? ->
        # => Need disk persistence with simple access?
        :dets
        # => Use DETS for simple persistence

      requirements.persistent? and requirements.complex? ->
        # => Need disk persistence with complex queries?
        :database
        # => Use PostgreSQL/MySQL

      requirements.high_concurrency? and requirements.in_memory? ->
        # => Need high-speed concurrent access?
        :ets
        # => Use ETS for in-memory speed

      true ->
        # => Default simple case
        :process_state
        # => Use GenServer state
    end
  end
end

# Example decisions
StorageDecision.choose(%{
  distributed?: false,
  persistent?: false,
  high_concurrency?: true,
  in_memory?: true
})
# => Returns :ets
# => Campaign cache scenario

StorageDecision.choose(%{
  distributed?: false,
  persistent?: true,
  simple?: true,
  complex?: false
})
# => Returns :dets
# => Audit log scenario

StorageDecision.choose(%{
  distributed?: true,
  persistent?: true,
  simple?: false
})
# => Returns :mnesia
# => Multi-node campaign replication
```

## Summary

**Maps**: Process-local, immutable, simple state

**ETS**: In-memory, concurrent, mutable, cross-process

**DETS**: Disk-backed ETS, persistence, 2GB limit

**Mnesia**: Distributed ETS, transactions, complex queries

**Databases**: Large datasets, complex queries, ACID

**Use ETS for**: Read-heavy caches, concurrent counters, session storage

**Use DETS for**: Simple audit logs, small persistent datasets

**Use Mnesia for**: Distributed state, transactional consistency

**Choose based on**: Persistence needs, data size, query complexity, distribution requirements
