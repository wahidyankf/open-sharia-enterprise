# Memory Management

Elixir's memory management is fundamentally different from most languages due to the **BEAM VM's** per-process garbage collection model. Understanding how memory works in Elixir is crucial for building high-performance, scalable applications that can handle millions of concurrent processes efficiently.

**Quick Reference**:

- [BEAM Memory Model](#beam-memory-model)
  - [Process Heap](#process-heap)
  - [Binary Heap](#binary-heap)
  - [ETS Tables](#ets-tables)
  - [Atom Table](#atom-table)
- [Garbage Collection](#garbage-collection)
  - [Generational GC](#generational-gc)
  - [Per-Process GC](#per-process-gc)
  - [GC Triggers](#gc-triggers)
  - [Hibernation](#hibernation)
- [Memory Profiling](#memory-profiling)
  - [Observer](#observer)
  - [Recon](#recon)
  - [Memory Metrics](#memory-metrics)
- [Binary Handling](#binary-handling)
  - [Small vs Large Binaries](#small-vs-large-binaries)
  - [Reference-Counted Binaries](#reference-counted-binaries)
  - [String vs Binary](#string-vs-binary)
- [ETS for Shared State](#ets-for-shared-state)
  - [Table Types](#table-types)
  - [Memory Characteristics](#memory-characteristics)
  - [Performance Trade-offs](#performance-trade-offs)
- [Memory Optimization Patterns](#memory-optimization-patterns)
- [Financial Domain Examples](#financial-domain-examples)
- [Memory Anti-patterns](#memory-anti-patterns)
- [Memory Monitoring](#memory-monitoring)
- [Best Practices](#best-practices)
- [Related Topics](#related-topics)
- [Sources](#sources)

## BEAM Memory Model

The BEAM VM uses a unique memory architecture designed for massive concurrency:

### Process Heap

Each Elixir process has its own **private heap**:

```elixir
# Each process has isolated memory
spawn(fn ->
  # This data lives in this process's heap
  large_list = Enum.to_list(1..1_000_000)
  # When process exits, heap is instantly freed
end)

# Check process memory
pid = spawn(fn ->
  donations = Enum.map(1..10_000, fn i ->
    %{id: i, amount: Money.new(10000, :IDR), donor: "donor_#{i}"}
  end)
  :timer.sleep(:infinity)
end)

# Process info shows memory usage in words (1 word = 8 bytes on 64-bit)
{:memory, words} = Process.info(pid, :memory)
IO.puts("Process using #{words * 8} bytes (#{Float.round(words * 8 / 1024 / 1024, 2)} MB)")
```

Characteristics:

- **Isolated**: No shared memory between processes
- **Fast allocation**: Simple bump-pointer allocation
- **Fast deallocation**: Entire heap freed when process dies
- **Size varies**: Starts small, grows as needed
- **No fragmentation**: Compacted during GC

### Binary Heap

Binaries larger than 64 bytes use a **shared binary heap**:

```elixir
# Small binary (< 64 bytes) - stored in process heap
small_binary = "Hello, World!"
byte_size(small_binary)  # => 13 bytes (process heap)

# Large binary (>= 64 bytes) - stored in shared binary heap
large_binary = String.duplicate("Financial Data ", 100)
byte_size(large_binary)  # => 1500 bytes (shared heap, reference counted)

# Processes share reference, not copy
defmodule BinaryDemo do
  def demonstrate_sharing do
    # Large binary in shared heap
    financial_data = File.read!("large_financial_report.csv")

    # Spawning processes that reference the same binary
    tasks = Enum.map(1..10, fn i ->
      Task.async(fn ->
        # Each process holds a reference, not a copy
        # Memory efficient!
        process_chunk(financial_data, i)
      end)
    end)

    Task.await_many(tasks)
  end

  defp process_chunk(data, chunk_id) do
    # Work with the binary reference
    byte_size(data)
  end
end
```

### ETS Tables

**ETS (Erlang Term Storage)** provides shared memory across processes:

```elixir
# Create ETS table for shared state
:ets.new(:donation_cache, [:named_table, :set, :public, read_concurrency: true])

# Store data (lives outside process heaps)
:ets.insert(:donation_cache, {"donor_123", %{total: Money.new(500_000, :IDR), count: 25}})

# Any process can read
donation_stats = :ets.lookup(:donation_cache, "donor_123")

# Memory is not per-process
:ets.info(:donation_cache, :memory)  # Returns words used
```

### Atom Table

Atoms are stored in a **global atom table**:

```elixir
# ⚠️ WARNING: Atoms are never garbage collected!
# Never create atoms dynamically from user input

# ❌ DANGEROUS - can exhaust atom table (default limit: ~1 million)
defmodule AtomLeak do
  def dangerous(user_input) do
    String.to_atom(user_input)  # DON'T DO THIS!
  end
end

# ✅ SAFE - use existing atoms or strings
defmodule SafeAtoms do
  def safe(user_input) do
    # Use String.to_existing_atom with error handling
    case user_input do
      "pending" -> :pending
      "completed" -> :completed
      _ -> :unknown
    end
  end
end

# Check atom count
:erlang.system_info(:atom_count)  # Current number of atoms
:erlang.system_info(:atom_limit)   # Maximum atoms (default: 1,048,576)
```

## Garbage Collection

### Generational GC

BEAM uses **generational garbage collection** per process:

```elixir
# Young generation (nursery)
defmodule GCDemo do
  def young_generation do
    # Most objects die young
    temp_data = Enum.map(1..1000, & &1 * 2)  # Likely GC'd soon
    result = Enum.sum(temp_data)
    result  # temp_data becomes garbage after this function
  end

  def old_generation do
    # Long-lived objects promoted to old generation
    pid = spawn(fn ->
      # This data survives multiple GC cycles
      persistent_cache = %{
        donations: load_donations(),
        campaigns: load_campaigns()
      }
      maintain_cache(persistent_cache)
    end)
    pid
  end

  defp load_donations, do: []
  defp load_campaigns, do: []
  defp maintain_cache(_cache), do: :timer.sleep(:infinity)
end
```

GC has two generations:

- **Young heap**: New allocations, frequently collected
- **Old heap**: Survived objects, collected less often

### Per-Process GC

Each process is garbage collected **independently**:

```elixir
# Process A doing GC doesn't affect Process B
defmodule IndependentGC do
  def demonstrate do
    # Process A - heavy allocation
    process_a = spawn(fn ->
      Enum.each(1..100, fn _ ->
        _large_data = Enum.to_list(1..100_000)
        :timer.sleep(10)
      end)
    end)

    # Process B - continues uninterrupted
    process_b = spawn(fn ->
      Enum.each(1..100, fn i ->
        IO.puts("Process B: #{i}")
        :timer.sleep(50)
      end)
    end)

    # Process A's GC pauses don't affect Process B
    # This is the beauty of per-process GC!
  end
end
```

Benefits:

- **No stop-the-world GC**: One process GC doesn't pause others
- **Short GC pauses**: Small heaps = fast GC (microseconds)
- **Predictable latency**: No global GC spikes
- **Scales with cores**: More cores = more parallel GC

### GC Triggers

Garbage collection is triggered by:

1. **Heap exhaustion**: Process heap fills up
2. **Manual trigger**: `:erlang.garbage_collect/0`
3. **Message queue size**: Large message queues
4. **Hibernation**: Process hibernates

```elixir
# Manual GC trigger
:erlang.garbage_collect()  # GC current process

# GC specific process
:erlang.garbage_collect(pid)

# Force full GC (both generations)
:erlang.garbage_collect(self(), [:major])

# Get GC info
Process.info(self(), :garbage_collection)
# => {:garbage_collection,
#      [minor_gcs: 5,
#       fullsweep_after: 65535,
#       min_bin_vheap_size: 46422,
#       min_heap_size: 233,
#       max_heap_size: 0]}
```

### Hibernation

**Hibernation** compacts process memory and triggers full GC:

```elixir
defmodule HibernationDemo do
  def start_worker do
    spawn(fn -> worker_loop([]) end)
  end

  defp worker_loop(state) do
    receive do
      {:work, data} ->
        new_state = process_work(data, state)
        worker_loop(new_state)

      :hibernate ->
        # Compact memory and enter hibernation
        # Useful for long-idle processes
        :proc_lib.hibernate(__MODULE__, :worker_loop, [state])
    after
      60_000 ->
        # Auto-hibernate after 1 minute of inactivity
        :proc_lib.hibernate(__MODULE__, :worker_loop, [state])
    end
  end

  defp process_work(data, state), do: [data | state]
end

# GenServer hibernation
defmodule IdleWorker do
  use GenServer

  def init(state) do
    # Return :hibernate to immediately hibernate
    {:ok, state, :hibernate}
  end

  def handle_call(:work, _from, state) do
    # Do work, then hibernate
    {:reply, :ok, state, :hibernate}
  end

  def handle_info(:timeout, state) do
    # Hibernate on timeout
    {:noreply, state, :hibernate}
  end
end
```

Benefits of hibernation:

- **Reduced memory**: Full GC + compaction
- **Smaller heap**: Heap shrinks to minimum
- **Lower cost**: Idle processes use less memory
- **Wake up cost**: Small overhead to wake from hibernation

## Memory Profiling

### Observer

`:observer` is the built-in GUI tool for memory monitoring:

```elixir
# Start observer
:observer.start()

# Or from command line
iex> :observer.start()

# Navigate to:
# 1. System tab - Overall memory usage
# 2. Memory Allocators - Detailed allocation info
# 3. Applications - Per-app memory
# 4. Processes - Sort by memory usage
```

Observer provides:

- **System overview**: Total memory, CPU, processes
- **Process list**: Sort by memory, reductions, message queue
- **Memory allocators**: Detailed breakdown
- **ETS tables**: Table sizes and memory

### Recon

**Recon** library for production debugging:

```elixir
# Add to mix.exs
{:recon, "~> 2.5"}

# Find memory-hungry processes
:recon.proc_count(:memory, 10)
# => [{{:memory, 50000}, #PID<0.200.0>}, ...]

# Find processes with large message queues
:recon.proc_count(:message_queue_len, 10)

# Memory allocation info
:recon_alloc.memory(:allocated)
:recon_alloc.memory(:used)

# Detailed process info
:recon.info(pid)
```

Financial domain example:

```elixir
defmodule FinancialDomain.MemoryMonitor do
  @moduledoc """
  Monitors memory usage in financial processing systems.
  """

  def check_donation_processor_memory do
    # Find donation processing workers
    processes = :recon.proc_count(:memory, 20)

    high_memory = Enum.filter(processes, fn {{:memory, bytes}, _pid} ->
      bytes > 100_000_000  # 100 MB
    end)

    if length(high_memory) > 0 do
      IO.puts("⚠️ High memory processes found:")
      Enum.each(high_memory, fn {{:memory, bytes}, pid} ->
        info = :recon.info(pid)
        IO.puts("  PID: #{inspect(pid)}, Memory: #{bytes} bytes")
        IO.puts("  Info: #{inspect(info)}")
      end)
    end
  end

  def memory_report do
    %{
      total: :erlang.memory(:total),
      processes: :erlang.memory(:processes),
      atom: :erlang.memory(:atom),
      binary: :erlang.memory(:binary),
      ets: :erlang.memory(:ets),
      system: :erlang.memory(:system)
    }
  end
end

# Use in production
FinancialDomain.MemoryMonitor.check_donation_processor_memory()
FinancialDomain.MemoryMonitor.memory_report()
```

### Memory Metrics

Built-in memory functions:

```elixir
# Total memory (all types)
:erlang.memory()
# => [total: 50000000, processes: 20000000, atom: 1000000, ...]

# Specific memory types
:erlang.memory(:total)      # Total allocated
:erlang.memory(:processes)  # Process heaps
:erlang.memory(:system)     # BEAM overhead
:erlang.memory(:atom)       # Atom table
:erlang.memory(:binary)     # Binary heap
:erlang.memory(:ets)        # ETS tables

# Process-specific memory
Process.info(self(), :memory)  # Current process memory (words)
Process.info(pid, :memory)     # Specific process memory

# Memory per process (in bytes)
processes_memory = Enum.map(Process.list(), fn pid ->
  {pid, Process.info(pid, :memory)}
end)
|> Enum.sort_by(fn {_pid, {:memory, bytes}} -> bytes end, :desc)
|> Enum.take(10)
```

## Binary Handling

### Small vs Large Binaries

Binary size determines storage location:

```elixir
# Small binaries (< 64 bytes) - stored in process heap
small = "Short string"
byte_size(small)  # => 12 bytes, stored in heap

# Large binaries (>= 64 bytes) - stored in shared binary heap
large = String.duplicate("Data", 20)
byte_size(large)  # => 80 bytes, reference-counted in shared heap

# Demo: Memory behavior
defmodule BinaryMemory do
  def small_binary_demo do
    # Creates copy in each process heap
    data = "Small"

    tasks = Enum.map(1..1000, fn _ ->
      Task.async(fn ->
        # Each process has own copy (12 bytes each)
        String.length(data)
      end)
    end)

    Task.await_many(tasks)
    # Total memory: ~12KB (1000 copies)
  end

  def large_binary_demo do
    # Shared reference, no copies
    data = String.duplicate("Large", 20)

    tasks = Enum.map(1..1000, fn _ ->
      Task.async(fn ->
        # Each process holds reference only (~8 bytes)
        String.length(data)
      end)
    end)

    Task.await_many(tasks)
    # Total memory: ~80 bytes (data) + 8KB (references)
  end
end
```

### Reference-Counted Binaries

Large binaries use reference counting:

```elixir
defmodule BinarySharing do
  def demonstrate_refcounting do
    # Create large binary (shared heap)
    financial_report = generate_report()  # Assume >64 bytes

    # Process 1 holds reference
    pid1 = spawn(fn ->
      process_report(financial_report)
    end)

    # Process 2 also holds reference (not a copy!)
    pid2 = spawn(fn ->
      validate_report(financial_report)
    end)

    # Binary is ref-counted:
    # - ref_count starts at 2
    # - When pid1 dies, ref_count decrements
    # - When pid2 dies and ref_count reaches 0, binary is freed
  end

  defp generate_report do
    # Generate CSV report
    headers = "ID,Amount,Date,Status\n"
    rows = Enum.map_join(1..1000, "\n", fn i ->
      "#{i},100000,2025-01-23,completed"
    end)
    headers <> rows
  end

  defp process_report(_report), do: :timer.sleep(1000)
  defp validate_report(_report), do: :timer.sleep(1000)
end
```

### String vs Binary

Strings in Elixir are UTF-8 binaries:

```elixir
# String is a binary
string = "Financial Domain"
is_binary(string)  # => true

# But not all binaries are valid strings
binary = <<200, 201, 202>>
String.valid?(binary)  # => false (invalid UTF-8)

# String operations create new binaries
str1 = "Donation"
str2 = str1 <> " Processing"  # New binary created

# Binary pattern matching (efficient, no copy)
defmodule CsvParser do
  def parse_line(<<id::binary-size(10), ",", amount::binary-size(15), ",", rest::binary>>) do
    %{
      id: String.trim(id),
      amount: String.trim(amount) |> String.to_integer(),
      rest: rest
    }
  end

  def parse_line(_), do: {:error, :invalid_format}
end

# Efficient binary building with iolist
defmodule EfficientBuilder do
  def build_csv(donations) do
    # Build iolist (nested list of binaries)
    iolist = [
      "ID,Amount,Donor\n",
      Enum.map(donations, fn d ->
        [d.id, ",", to_string(d.amount), ",", d.donor_id, "\n"]
      end)
    ]

    # Convert to binary once at the end
    IO.iodata_to_binary(iolist)
  end

  # ❌ Bad - creates many intermediate binaries
  def build_csv_slow(donations) do
    Enum.reduce(donations, "ID,Amount,Donor\n", fn d, acc ->
      acc <> d.id <> "," <> to_string(d.amount) <> "," <> d.donor_id <> "\n"
    end)
  end
end
```

## ETS for Shared State

### Table Types

ETS provides four table types:

```elixir
# 1. :set - Key-value pairs, one value per key (fast lookups)
:ets.new(:donation_cache, [:set, :public, :named_table])
:ets.insert(:donation_cache, {"donor_123", %{total: 50000}})

# 2. :ordered_set - Sorted by key (range queries)
:ets.new(:time_series, [:ordered_set, :public, :named_table])
:ets.insert(:time_series, {~U[2025-01-23 10:00:00Z], 10000})
:ets.insert(:time_series, {~U[2025-01-23 11:00:00Z], 15000})

# 3. :bag - Multiple values per key (duplicates with different values)
:ets.new(:donor_donations, [:bag, :public, :named_table])
:ets.insert(:donor_donations, {"donor_123", "don_001"})
:ets.insert(:donor_donations, {"donor_123", "don_002"})

# 4. :duplicate_bag - Multiple values per key (allows full duplicates)
:ets.new(:event_log, [:duplicate_bag, :public, :named_table])
:ets.insert(:event_log, {"donation", :completed})
:ets.insert(:event_log, {"donation", :completed})  # Allowed
```

### Memory Characteristics

ETS tables live outside process heaps:

```elixir
defmodule EtsMemory do
  def demonstrate do
    # Create table
    table = :ets.new(:demo, [:set, :public])

    # Insert data
    Enum.each(1..100_000, fn i ->
      :ets.insert(table, {i, %{
        id: "don_#{i}",
        amount: Money.new(:rand.uniform(100_000), :IDR),
        timestamp: DateTime.utc_now()
      }})
    end)

    # Check table memory
    memory_words = :ets.info(table, :memory)
    memory_mb = memory_words * 8 / 1024 / 1024

    IO.puts("ETS table memory: #{Float.round(memory_mb, 2)} MB")
    IO.puts("Number of objects: #{:ets.info(table, :size)}")

    # Memory is shared, not in process heap
    Process.info(self(), :memory)  # Process heap is small

    # Cleanup
    :ets.delete(table)
  end
end
```

### Performance Trade-offs

ETS configuration affects performance and concurrency:

```elixir
# Read-heavy workload
:ets.new(:read_heavy, [
  :set,
  :public,
  :named_table,
  read_concurrency: true,    # Optimize for concurrent reads
  write_concurrency: false   # Sequential writes
])

# Write-heavy workload
:ets.new(:write_heavy, [
  :set,
  :public,
  :named_table,
  read_concurrency: false,
  write_concurrency: true    # Optimize for concurrent writes
])

# Balanced workload
:ets.new(:balanced, [
  :set,
  :public,
  :named_table,
  read_concurrency: true,
  write_concurrency: true,
  decentralized_counters: true  # Better for high concurrency
])
```

Example: Donation cache with ETS

```elixir
defmodule FinancialDomain.DonationCache do
  @table :donation_cache

  def start_link do
    :ets.new(@table, [
      :set,
      :public,
      :named_table,
      read_concurrency: true,
      write_concurrency: true
    ])
    {:ok, self()}
  end

  def put(donation_id, donation) do
    :ets.insert(@table, {donation_id, donation, System.monotonic_time()})
  end

  def get(donation_id) do
    case :ets.lookup(@table, donation_id) do
      [{^donation_id, donation, _timestamp}] -> {:ok, donation}
      [] -> {:error, :not_found}
    end
  end

  def evict_old(max_age_seconds) do
    now = System.monotonic_time()
    cutoff = now - (max_age_seconds * 1_000_000_000)

    # Select and delete old entries
    :ets.select_delete(@table, [
      {{:_, :_, :"$1"}, [{:<, :"$1", cutoff}], [true]}
    ])
  end

  def stats do
    %{
      size: :ets.info(@table, :size),
      memory_bytes: :ets.info(@table, :memory) * 8,
      type: :ets.info(@table, :type)
    }
  end
end
```

## Memory Optimization Patterns

### 1. Process Pooling

Limit concurrent processes to control memory:

```elixir
defmodule FinancialDomain.DonationProcessor do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    # Use Task.Supervisor for bounded concurrency
    {:ok, supervisor} = Task.Supervisor.start_link(max_children: 100)
    {:ok, %{supervisor: supervisor}}
  end

  def process_batch(donations) do
    GenServer.call(__MODULE__, {:process_batch, donations})
  end

  def handle_call({:process_batch, donations}, _from, state) do
    # Process with bounded concurrency
    results = donations
    |> Enum.chunk_every(100)
    |> Enum.flat_map(fn chunk ->
      chunk
      |> Task.async_stream(
        &process_donation/1,
        max_concurrency: 50,  # Control memory
        timeout: 30_000
      )
      |> Enum.map(fn
        {:ok, result} -> result
        {:exit, reason} -> {:error, reason}
      end)
    end)

    {:reply, results, state}
  end

  defp process_donation(donation) do
    # Process single donation
    {:ok, donation}
  end
end
```

### 2. Streaming Large Data

Use streams to process large datasets without loading into memory:

```elixir
defmodule FinancialDomain.ReportGenerator do
  def generate_csv(output_path) do
    # Stream from database, transform, write - constant memory
    File.stream!(output_path)
    |> Stream.into(
      FinancialDomain.Repo.stream_donations()
      |> Stream.map(&format_donation/1)
    )
    |> Stream.run()
  end

  defp format_donation(donation) do
    "#{donation.id},#{Money.to_string(donation.amount)},#{donation.donor_id}\n"
  end
end

defmodule FinancialDomain.Repo do
  def stream_donations do
    # Stream from database (constant memory)
    Repo.stream(Donation)
    |> Stream.chunk_every(1000)
    |> Stream.flat_map(& &1)
  end
end
```

### 3. Binary Building with IOList

Use iolists for efficient binary concatenation:

```elixir
defmodule FinancialDomain.CsvExporter do
  # ✅ Efficient - builds iolist, converts once
  def export_efficient(donations) do
    iolist = [
      "ID,Amount,Donor,Campaign,Date\n",
      Enum.map(donations, fn d ->
        [
          d.id, ",",
          to_string(d.amount), ",",
          d.donor_id, ",",
          d.campaign_id, ",",
          DateTime.to_iso8601(d.timestamp), "\n"
        ]
      end)
    ]

    IO.iodata_to_binary(iolist)
  end

  # ❌ Inefficient - creates many intermediate binaries
  def export_inefficient(donations) do
    Enum.reduce(donations, "ID,Amount,Donor,Campaign,Date\n", fn d, acc ->
      acc <>
        d.id <> "," <>
        to_string(d.amount) <> "," <>
        d.donor_id <> "," <>
        d.campaign_id <> "," <>
        DateTime.to_iso8601(d.timestamp) <> "\n"
    end)
  end
end
```

## Financial Domain Examples

Complete example: Memory-efficient batch processor

```elixir
defmodule FinancialDomain.BatchProcessor do
  @moduledoc """
  Memory-efficient batch processing for financial transactions.

  Uses:
  - Streaming for large datasets
  - ETS for temporary state
  - Process pooling for controlled concurrency
  - Hibernation for idle workers
  """

  use GenServer

  defstruct [:table, :supervisor, :config]

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(opts) do
    # Create ETS table for job tracking
    table = :ets.new(:batch_jobs, [
      :set,
      :public,
      read_concurrency: true,
      write_concurrency: true
    ])

    # Start task supervisor
    {:ok, supervisor} = Task.Supervisor.start_link(max_children: opts[:max_workers] || 50)

    config = %{
      max_workers: opts[:max_workers] || 50,
      chunk_size: opts[:chunk_size] || 100,
      hibernate_after: opts[:hibernate_after] || 60_000
    }

    state = %__MODULE__{
      table: table,
      supervisor: supervisor,
      config: config
    }

    {:ok, state, config.hibernate_after}
  end

  def process_donations(donation_ids) do
    GenServer.call(__MODULE__, {:process_donations, donation_ids}, :infinity)
  end

  def handle_call({:process_donations, donation_ids}, _from, state) do
    job_id = generate_job_id()

    # Track job in ETS
    :ets.insert(state.table, {job_id, :running, 0, length(donation_ids)})

    # Process in chunks with bounded concurrency
    results = donation_ids
    |> Stream.chunk_every(state.config.chunk_size)
    |> Stream.with_index()
    |> Enum.flat_map(fn {chunk, idx} ->
      process_chunk(chunk, job_id, idx, state)
    end)

    # Update job status
    :ets.insert(state.table, {job_id, :completed, length(results), length(donation_ids)})

    {:reply, {:ok, job_id, results}, state, state.config.hibernate_after}
  end

  def handle_info(:timeout, state) do
    # Hibernate after timeout
    {:noreply, state, :hibernate}
  end

  defp process_chunk(donation_ids, job_id, chunk_idx, state) do
    donation_ids
    |> Task.async_stream(
      fn id -> process_single_donation(id, job_id, chunk_idx) end,
      max_concurrency: state.config.max_workers,
      timeout: 30_000,
      on_timeout: :kill_task
    )
    |> Enum.map(fn
      {:ok, result} ->
        # Update progress in ETS
        :ets.update_counter(state.table, job_id, {3, 1})
        result

      {:exit, reason} ->
        {:error, reason}
    end)
  end

  defp process_single_donation(donation_id, _job_id, _chunk_idx) do
    # Fetch from database (streaming, not loading all)
    case FinancialDomain.Repo.get_donation(donation_id) do
      nil ->
        {:error, :not_found}

      donation ->
        # Process donation
        validate_and_process(donation)
    end
  end

  defp validate_and_process(donation) do
    with :ok <- validate_donation(donation),
         {:ok, _result} <- process_payment(donation),
         {:ok, updated} <- update_donation_status(donation) do
      {:ok, updated}
    end
  end

  defp validate_donation(_donation), do: :ok
  defp process_payment(_donation), do: {:ok, %{}}
  defp update_donation_status(donation), do: {:ok, donation}

  defp generate_job_id do
    "job_#{System.unique_integer([:positive, :monotonic])}"
  end

  # Public API for job status
  def job_status(job_id) do
    case :ets.lookup(:batch_jobs, job_id) do
      [{^job_id, status, processed, total}] ->
        {:ok, %{status: status, processed: processed, total: total}}

      [] ->
        {:error, :not_found}
    end
  end

  # Cleanup old jobs
  def cleanup_old_jobs(max_age_seconds) do
    # Implementation for cleanup
    :ok
  end
end

# Usage
{:ok, _pid} = FinancialDomain.BatchProcessor.start_link(
  max_workers: 100,
  chunk_size: 50,
  hibernate_after: 120_000
)

donation_ids = Enum.to_list(1..10_000) |> Enum.map(&"don_#{&1}")
{:ok, job_id, results} = FinancialDomain.BatchProcessor.process_donations(donation_ids)

# Check progress
{:ok, status} = FinancialDomain.BatchProcessor.job_status(job_id)
```

## Memory Anti-patterns

### 1. Atom Creation from User Input

```elixir
# ❌ DANGEROUS - can exhaust atom table
defmodule DangerousCode do
  def process_status(user_input) do
    String.to_atom(user_input)  # NEVER DO THIS!
  end
end

# ✅ SAFE - use existing atoms
defmodule SafeCode do
  @valid_statuses [:pending, :completed, :failed, :cancelled]

  def process_status(user_input) do
    case user_input do
      "pending" -> {:ok, :pending}
      "completed" -> {:ok, :completed}
      "failed" -> {:ok, :failed}
      "cancelled" -> {:ok, :cancelled}
      _ -> {:error, :invalid_status}
    end
  end
end
```

### 2. Process Leaks

```elixir
# ❌ BAD - spawning without supervision
defmodule LeakyCode do
  def process_donations(donations) do
    Enum.each(donations, fn d ->
      spawn(fn -> process_donation(d) end)  # Can accumulate!
    end)
  end

  defp process_donation(_d), do: :timer.sleep(1000)
end

# ✅ GOOD - bounded concurrency
defmodule SafeCode do
  def process_donations(donations) do
    donations
    |> Task.async_stream(&process_donation/1, max_concurrency: 50)
    |> Enum.to_list()
  end

  defp process_donation(_d), do: :timer.sleep(1000)
end
```

### 3. Large Message Queues

```elixir
# ❌ BAD - sending faster than processing
defmodule SlowConsumer do
  use GenServer

  def init(_), do: {:ok, []}

  def handle_cast({:process, data}, state) do
    # Slow processing
    :timer.sleep(1000)
    process_data(data)
    {:noreply, state}
  end

  defp process_data(_data), do: :ok
end

# Sending too fast causes message queue buildup
Enum.each(1..10_000, fn i ->
  GenServer.cast(SlowConsumer, {:process, i})
end)

# ✅ GOOD - backpressure with call
defmodule FastConsumer do
  use GenServer

  def init(_), do: {:ok, []}

  def handle_call({:process, data}, _from, state) do
    # Caller waits, providing natural backpressure
    process_data(data)
    {:reply, :ok, state}
  end

  defp process_data(_data), do: :timer.sleep(1000)
end
```

## Memory Monitoring

Production monitoring example:

```elixir
defmodule FinancialDomain.Telemetry do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    # Schedule periodic memory checks
    schedule_check()
    {:ok, %{}}
  end

  def handle_info(:check_memory, state) do
    report_memory_stats()
    schedule_check()
    {:noreply, state}
  end

  defp schedule_check do
    Process.send_after(self(), :check_memory, :timer.minutes(5))
  end

  defp report_memory_stats do
    stats = %{
      total: :erlang.memory(:total),
      processes: :erlang.memory(:processes),
      binary: :erlang.memory(:binary),
      ets: :erlang.memory(:ets),
      atom: :erlang.memory(:atom),
      system: :erlang.memory(:system),
      process_count: length(Process.list()),
      ets_count: length(:ets.all()),
      atom_count: :erlang.system_info(:atom_count)
    }

    # Log or send to monitoring service
    Logger.info("Memory stats: #{inspect(stats)}")

    # Alert if memory is high
    if stats.total > 1_000_000_000 do  # 1 GB
      Logger.warn("High memory usage: #{stats.total} bytes")
    end

    stats
  end
end
```

## Best Practices

1. **Use streams for large datasets** - Constant memory usage
2. **Leverage ETS for shared state** - Avoid copying data between processes
3. **Monitor process memory** - Use `:recon` and `:observer` in production
4. **Never create atoms from user input** - Atoms are never GC'd
5. **Use bounded concurrency** - Control memory with process pools
6. **Hibernate idle processes** - Reduce memory for long-lived idle workers
7. **Use iolists for binary building** - Avoid intermediate binary copies
8. **Profile before optimizing** - Measure to find real bottlenecks

## Related Topics

- [Concurrency and Parallelism](ex-so-stla-el__concurrency-and-parallelism.md) - Process-based concurrency
- [Performance](ex-so-stla-el__performance.md) - Performance optimization techniques
- [Error Handling](ex-so-stla-el__error-handling.md) - Supervision and fault tolerance
- [OTP: GenServer](ex-so-stla-el__otp-genserver.md) - Process memory patterns
- [Best Practices](ex-so-stla-el__best-practices.md) - Memory-efficient patterns

## Sources

- [BEAM Memory Architecture](http://erlang.org/doc/efficiency_guide/memory.html)
- [Erlang Garbage Collection](http://erlang.org/doc/efficiency_guide/gc.html)
- [Recon Library](https://github.com/ferd/recon)
- [ETS Documentation](https://www.erlang.org/doc/man/ets.html)
- [Efficiency Guide](http://erlang.org/doc/efficiency_guide/users_guide.html)
- [Garbage Collection in BEAM - Lukas Larsson](https://www.erlang-solutions.com/blog/erlang-garbage-collection.html)
- [Understanding BEAM Memory - Fred Hebert](https://ferd.ca/erlang-s-tail-recursion-is-not-a-silver-bullet.html)

---

**Last Updated**: 2025-01-23

**Next Steps**: Explore [Dependencies](ex-so-stla-el__dependencies.md) for managing project dependencies with Mix and Hex, or see [Performance](ex-so-stla-el__performance.md) for optimization techniques beyond memory management.
