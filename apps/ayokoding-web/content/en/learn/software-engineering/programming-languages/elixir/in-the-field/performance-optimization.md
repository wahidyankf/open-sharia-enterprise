---
title: "Performance Optimization"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000030
description: "Production performance optimization through profiling, benchmarking, and strategic caching in Elixir"
tags: ["elixir", "performance", "optimization", "benchee", "profiling", "ets"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/error-handling-resilience"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/hot-code-upgrades"
---

**Building high-performance Elixir systems?** This guide teaches production optimization through measurement-first approaches: profile to identify bottlenecks, benchmark alternatives, and apply targeted optimizations.

## Why Performance Optimization Matters

Performance optimization is a late-stage activity. **Premature optimization wastes time** on irrelevant bottlenecks. Production optimization follows this sequence:

1. **Measure first** - Profile to identify actual bottlenecks (not guesses)
2. **Benchmark alternatives** - Measure multiple approaches with real workloads
3. **Optimize strategically** - Apply fixes to measured bottlenecks only
4. **Verify impact** - Re-measure to confirm improvement

**Critical principle**: Never optimize without measurements. The BEAM VM's behavior often contradicts intuition - measure everything.

## Financial Domain Examples

Examples use Shariah-compliant financial operations:

- **Zakat batch calculations** - Processing thousands of donation calculations
- **Donation query performance** - Optimizing database queries with Ecto preloading
- **Transaction caching** - Using ETS for read-heavy scenarios

These domains demonstrate optimization patterns with real business impact.

## Benchmarking with Benchee

### Pattern 1: Micro-Benchmarks for Algorithm Selection

Use Benchee to compare algorithm implementations with real workloads.

**Example**: Comparing zakat calculation approaches.

```elixir
# Install Benchee
# mix.exs
defp deps do
  [
    {:benchee, "~> 1.3", only: :dev}             # => Benchmarking library
                                                 # => only: :dev - Not in production
  ]
end

# Benchmark script: bench/zakat_calculation.exs
defmodule ZakatBenchmark do
  # Approach 1: Direct calculation
  def calculate_direct(wealth) do
    wealth * 0.025                               # => Simple multiplication
                                                 # => Returns float
  end

  # Approach 2: Using Decimal for precision
  def calculate_decimal(wealth) do
    wealth
    |> Decimal.new()                             # => Convert to Decimal
    |> Decimal.mult(Decimal.new("0.025"))        # => Precise multiplication
    |> Decimal.to_float()                        # => Convert back to float
  end

  # Approach 3: Integer arithmetic (pennies)
  def calculate_integer(wealth_cents) do
    div(wealth_cents * 25, 1000)                 # => Integer division
                                                 # => wealth_cents in pennies
                                                 # => Returns integer pennies
  end
end

# Run benchmarks
Benchee.run(
  %{
    "direct calculation" => fn ->
      ZakatBenchmark.calculate_direct(100_000)   # => Test with 100,000 units
    end,
    "decimal calculation" => fn ->
      ZakatBenchmark.calculate_decimal(100_000)
    end,
    "integer calculation" => fn ->
      ZakatBenchmark.calculate_integer(10_000_000)
                                                 # => 100,000 * 100 pennies
    end
  },
  time: 5,                                       # => Run for 5 seconds per benchmark
  memory_time: 2                                 # => Measure memory for 2 seconds
)
# => Output shows:
# => - Iterations per second (throughput)
# => - Average execution time
# => - Standard deviation
# => - Memory usage
```

**Run benchmark**:

```bash
mix run bench/zakat_calculation.exs             # => Executes benchmark
# => Output example:
# => direct calculation      1000000  1.2 μs/op   ±5%
# => integer calculation      950000  1.3 μs/op   ±4%
# => decimal calculation      100000  10.5 μs/op  ±8%
```

**Interpretation**:

- **Direct calculation fastest** - 1.2 μs per operation
- **Integer calculation close second** - 1.3 μs, better for money precision
- **Decimal calculation 10x slower** - Only use when precision critical

**Best practice**: Benchmark with production-scale data (1000s of records, not toy examples).

### Pattern 2: Comparing Stream vs Enum

Benchmark lazy evaluation trade-offs.

```elixir
defmodule StreamBenchmark do
  # Generate sample data: 10,000 donation records
  def generate_donations(count) do
    for i <- 1..count do
      %{user_id: i, amount: :rand.uniform(1000), timestamp: DateTime.utc_now()}
    end                                          # => List of maps
  end

  # Approach 1: Eager evaluation with Enum
  def process_with_enum(donations) do
    donations
    |> Enum.filter(fn d -> d.amount > 100 end)  # => Builds intermediate list
    |> Enum.map(fn d -> d.amount * 0.025 end)   # => Builds second intermediate list
    |> Enum.sum()                                # => Sums all values
                                                 # => Total: 3 passes through data
  end

  # Approach 2: Lazy evaluation with Stream
  def process_with_stream(donations) do
    donations
    |> Stream.filter(fn d -> d.amount > 100 end)
                                                 # => Lazy - no intermediate list
    |> Stream.map(fn d -> d.amount * 0.025 end) # => Lazy - no intermediate list
    |> Enum.sum()                                # => Single pass through data
  end
end

# Benchmark
donations = StreamBenchmark.generate_donations(10_000)
                                                 # => 10,000 donation records

Benchee.run(
  %{
    "Enum pipeline" => fn ->
      StreamBenchmark.process_with_enum(donations)
    end,
    "Stream pipeline" => fn ->
      StreamBenchmark.process_with_stream(donations)
    end
  },
  memory_time: 2
)
# => Output shows memory usage difference
# => Enum: Higher memory (intermediate lists)
# => Stream: Lower memory (single pass)
```

**When to use each**:

| Scenario                  | Use    | Reason                     |
| ------------------------- | ------ | -------------------------- |
| Small datasets (< 1000)   | Enum   | Overhead not worth it      |
| Large datasets (> 10,000) | Stream | Memory savings significant |
| Multiple transformations  | Stream | Avoid intermediate lists   |
| Single operation          | Enum   | Simpler, no lazy overhead  |
| Need entire result        | Enum   | Will build list anyway     |
| Process incrementally     | Stream | Memory-efficient           |

**Best practice**: Default to Enum for simplicity. Switch to Stream when profiling shows memory pressure.

## Profiling Tools

### Pattern 3: Development Profiling with :fprof

Use :fprof for call-count profiling (development only, high overhead).

```elixir
defmodule ProfilingExample do
  # Function to profile: batch zakat calculation
  def batch_calculate_zakat(donations) do
    donations
    |> Enum.map(&calculate_single_zakat/1)       # => Calculate zakat per donation
    |> Enum.sum()                                # => Sum all zakat amounts
  end

  defp calculate_single_zakat(donation) do
    # Simulate complex calculation
    Process.sleep(1)                             # => 1ms delay per calculation
    donation.amount * 0.025                      # => 2.5% zakat rate
  end
end

# Profile with :fprof
donations = for i <- 1..100, do: %{user_id: i, amount: 1000}
                                                 # => 100 donation records

:fprof.trace([:start])                           # => Start tracing
result = ProfilingExample.batch_calculate_zakat(donations)
                                                 # => Execute function
:fprof.trace([:stop])                            # => Stop tracing
:fprof.profile()                                 # => Analyze trace data
:fprof.analyse(dest: 'fprof_output.txt')        # => Write analysis to file
                                                 # => Shows:
                                                 # => - Function call counts
                                                 # => - Time per function
                                                 # => - Call hierarchy

# Read output
File.read!('fprof_output.txt')
# => Output shows:
# => ProfilingExample.batch_calculate_zakat/1  100ms  1 call
# => ProfilingExample.calculate_single_zakat/1  95ms  100 calls
# => Process.sleep/1                            90ms  100 calls
```

**:fprof characteristics**:

- **High overhead** (10-100x slower) - Development only
- **Call-count profiling** - Shows which functions called most
- **Time per function** - Identifies slow functions
- **Not for production** - Too expensive

**Best practice**: Use :fprof to identify hot code paths, then optimize those functions.

### Pattern 4: Production-Safe Profiling with :eprof

Use :eprof for lower-overhead profiling (still development-preferred).

```elixir
# Profile with :eprof
:eprof.start()                                   # => Start profiler
:eprof.start_profiling([self()])                 # => Profile current process
result = ProfilingExample.batch_calculate_zakat(donations)
                                                 # => Execute function
:eprof.stop_profiling()                          # => Stop profiling
:eprof.analyze()                                 # => Print analysis
                                                 # => Shows:
                                                 # => - Total time per function
                                                 # => - Call count
                                                 # => - Time per call

# => Output:
# => FUNCTION                         CALLS    %  TIME  [uS / call]
# => ProfilingExample.batch_calculate   1      1   5000  [5000.00]
# => ProfilingExample.calculate_single 100    99  95000  [950.00]
```

**:eprof vs :fprof**:

| Tool   | Overhead  | Detail Level  | Use Case                     |
| ------ | --------- | ------------- | ---------------------------- |
| :fprof | Very high | Call tree     | Deep analysis, small data    |
| :eprof | Moderate  | Function time | Quick profiling, larger data |

**Best practice**: Start with :eprof for quick profiling, use :fprof for deep investigation.

## Memory Profiling

### Pattern 5: Memory Profiling with :recon

Use :recon for production memory analysis (install recon library).

```elixir
# mix.exs
defp deps do
  [
    {:recon, "~> 2.5"}                           # => Production-safe observability
  ]
end

# Memory profiling
:recon.proc_count(:memory, 10)                   # => Top 10 processes by memory
# => Returns:
# => [
# =>   {<0.123.0>, 1_000_000, [...]},           # => PID, bytes, process info
# =>   {<0.456.0>, 800_000, [...]},
# =>   ...
# => ]

:recon.proc_window(:memory, 10, 5000)            # => Monitor top 10 for 5 seconds
# => Shows memory changes over time
# => Identifies memory leaks

# Process-specific memory
pid = Process.whereis(Finance.ZakatCalculator)   # => Get process PID
:recon.info(pid, :memory)                        # => Memory usage for process
# => {:memory, 123456}                          # => Bytes allocated
```

**Memory monitoring best practices**:

1. **Identify top consumers** - Use :recon.proc_count(:memory, 10)
2. **Monitor over time** - Use :recon.proc_window for leak detection
3. **Profile GenServer state** - Large state = memory bottleneck
4. **Track message queues** - Use :recon.proc_count(:message_queue_len, 10)

### Pattern 6: Observer GUI for Development

Use Observer for visual profiling (development only).

```elixir
# Start Observer
:observer.start()                                # => Opens GUI
# => Shows:
# => - System tab: CPU, memory, processes
# => - Load Charts tab: Historical graphs
# => - Applications tab: Supervision trees
# => - Processes tab: All processes with state
# => - Table Viewer tab: ETS/DETS tables
```

**Observer use cases**:

- **Visual supervision tree** - See process hierarchy
- **Memory trends** - Historical memory graphs
- **Process inspection** - Click process for details
- **ETS table browser** - Inspect cache contents

**Best practice**: Use Observer during development to understand system behavior visually.

## Strategic Caching

### Pattern 7: ETS for Read-Heavy Scenarios

Use ETS (Erlang Term Storage) for in-memory caching.

```elixir
defmodule Finance.ZakatCache do
  use GenServer

  # Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def get_nisab do
    # Read from ETS (concurrent reads, no GenServer involved)
    case :ets.lookup(:zakat_cache, :nisab) do
      [{:nisab, value}] -> {:ok, value}          # => Cache hit
      [] -> {:error, :not_found}                 # => Cache miss
    end
  end

  def set_nisab(value) do
    # Write through GenServer (serialized writes)
    GenServer.call(__MODULE__, {:set_nisab, value})
  end

  # Server callbacks
  def init(:ok) do
    # Create ETS table
    table = :ets.new(:zakat_cache, [
      :set,                                      # => Key-value store
      :named_table,                              # => Access by name :zakat_cache
      :public,                                   # => All processes can read
      read_concurrency: true                     # => Optimize for concurrent reads
    ])                                           # => Returns table reference

    # Set initial nisab
    :ets.insert(:zakat_cache, {:nisab, 5000})    # => Insert initial value
    {:ok, %{table: table}}                       # => Store table ref in state
  end

  def handle_call({:set_nisab, value}, _from, state) do
    :ets.insert(:zakat_cache, {:nisab, value})   # => Update cache
    {:reply, :ok, state}
  end

  def terminate(_reason, state) do
    :ets.delete(state.table)                     # => Clean up ETS table
    :ok
  end
end
```

**ETS performance characteristics**:

- **Concurrent reads** - No locking, all processes read simultaneously
- **O(1) lookup** - Constant-time key lookups
- **In-memory** - Fast access, but lost on restart
- **Write serialization** - Use GenServer to coordinate writes

**When to use ETS**:

- **Read-heavy workloads** (90%+ reads) - Nisab lookups, configuration
- **Shared data** - Multiple processes need same data
- **Fast lookup** - Millisecond response requirements
- **Temporary data** - Cache, session storage

**Best practice**: Benchmark ETS vs GenServer state. ETS wins when 10+ processes read concurrently.

### Pattern 8: :persistent_term for Global Config

Use :persistent_term for read-only global configuration.

```elixir
defmodule Finance.ConfigLoader do
  # Set configuration at startup
  def load_config do
    config = %{
      nisab: 5000,                               # => Minimum wealth threshold
      zakat_rate: 0.025,                         # => 2.5% zakat rate
      currency: "USD"                            # => Currency
    }
    :persistent_term.put(:finance_config, config)
                                                 # => Store globally
                                                 # => Optimized for reads
                                                 # => Warning: Updates expensive
  end

  # Read configuration (zero-cost abstraction)
  def get_config do
    :persistent_term.get(:finance_config)        # => Instant read
                                                 # => No copying, pointer only
  end

  def get_nisab do
    config = :persistent_term.get(:finance_config)
    config.nisab                                 # => Extract nisab
  end
end

# Usage in application
def start(_type, _args) do
  Finance.ConfigLoader.load_config()             # => Load at startup
  # ... start supervision tree
end

# In worker processes
nisab = Finance.ConfigLoader.get_nisab()         # => Zero-cost read
```

**:persistent_term vs ETS vs GenServer**:

| Storage          | Read Speed | Write Cost | Use Case         |
| ---------------- | ---------- | ---------- | ---------------- |
| :persistent_term | Fastest    | Very high  | Read-only config |
| ETS              | Fast       | Moderate   | Read-heavy cache |
| GenServer        | Slow       | Low        | Mutable state    |

**Critical**: :persistent_term writes trigger garbage collection across **ALL processes**. Only use for truly static data.

## Ecto Query Optimization

### Pattern 9: N+1 Query Prevention with Preload

Prevent N+1 queries using Ecto preload.

```elixir
defmodule Finance.DonationQuery do
  import Ecto.Query

  # ❌ N+1 PROBLEM: Query per user
  def get_donations_with_users_slow do
    donations = Repo.all(Donation)               # => 1 query: get all donations
    Enum.map(donations, fn donation ->
      user = Repo.get(User, donation.user_id)    # => N queries: one per user
      %{donation: donation, user: user}
    end)                                         # => Total: 1 + N queries
  end

  # ✅ SOLUTION: Single query with JOIN
  def get_donations_with_users_fast do
    Donation
    |> preload(:user)                            # => Load user association
    |> Repo.all()                                # => Single query with JOIN
                                                 # => SQL: SELECT d.*, u.*
                                                 # =>      FROM donations d
                                                 # =>      JOIN users u ON d.user_id = u.id
  end

  # Multiple associations
  def get_donations_with_all_data do
    Donation
    |> preload([:user, :zakat_calculation, :receipt])
                                                 # => Multiple JOINs
    |> Repo.all()                                # => Single query, all data
  end

  # Conditional preload
  def get_donations_with_users_if_needed(include_users?) do
    query = from d in Donation

    query =
      if include_users? do
        query |> preload(:user)                  # => Add preload conditionally
      else
        query
      end

    Repo.all(query)
  end
end
```

**Preload strategies**:

```elixir
# Preload with separate query (when association large)
Donation
|> preload([:user])
|> Repo.all()
# => Query 1: SELECT * FROM donations
# => Query 2: SELECT * FROM users WHERE id IN (...)
# => Total: 2 queries (better than 1 + N)

# Preload with JOIN (when association small)
Donation
|> join(:inner, [d], u in assoc(d, :user))
|> preload([d, u], [user: u])
|> Repo.all()
# => Single query with JOIN
```

**Best practice**: Always preload associations when accessing them. Monitor query counts in logs.

### Pattern 10: Query Result Caching

Cache expensive query results with ETS.

```elixir
defmodule Finance.DonationStats do
  use GenServer

  # Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def get_total_donations do
    # Try cache first
    case :ets.lookup(:donation_stats, :total) do
      [{:total, value, timestamp}] ->
        if fresh?(timestamp) do
          {:ok, value}                           # => Cache hit (fresh)
        else
          fetch_and_cache_total()                # => Cache stale, refresh
        end
      [] ->
        fetch_and_cache_total()                  # => Cache miss
    end
  end

  def invalidate_cache do
    GenServer.cast(__MODULE__, :invalidate)      # => Clear cache
  end

  # Server callbacks
  def init(:ok) do
    :ets.new(:donation_stats, [
      :set,
      :named_table,
      :public,
      read_concurrency: true
    ])
    {:ok, %{}}
  end

  def handle_cast(:invalidate, state) do
    :ets.delete_all_objects(:donation_stats)     # => Clear all cached data
    {:noreply, state}
  end

  # Helpers
  defp fresh?(timestamp) do
    # Consider fresh if < 5 minutes old
    DateTime.diff(DateTime.utc_now(), timestamp) < 300
                                                 # => 300 seconds = 5 minutes
  end

  defp fetch_and_cache_total do
    # Expensive query
    total = Repo.aggregate(Donation, :sum, :amount)
                                                 # => SQL: SELECT SUM(amount) FROM donations
    timestamp = DateTime.utc_now()
    :ets.insert(:donation_stats, {:total, total, timestamp})
                                                 # => Cache with timestamp
    {:ok, total}
  end
end
```

**Caching best practices**:

1. **Set TTL** - Cache expiration prevents stale data
2. **Invalidate on writes** - Clear cache when data changes
3. **Monitor hit rate** - Low hit rate = cache not useful
4. **Measure query cost** - Only cache expensive queries (> 50ms)

## Production Optimization Checklist

Before optimizing production systems:

- [ ] **Profile first** - Use :eprof/:fprof to identify bottlenecks
- [ ] **Benchmark alternatives** - Use Benchee with production-scale data
- [ ] **Measure memory** - Use :recon to identify memory leaks
- [ ] **Monitor query counts** - Check for N+1 queries in logs
- [ ] **Apply targeted fixes** - Optimize measured bottlenecks only
- [ ] **Use ETS for read-heavy data** - Configuration, lookup tables
- [ ] **Prefer Stream for large datasets** - When memory pressure detected
- [ ] **Preload Ecto associations** - Prevent N+1 queries
- [ ] **Cache expensive queries** - With TTL and invalidation
- [ ] **Re-measure impact** - Verify optimization worked

## Trade-Offs: Optimization Approaches

| Approach         | Setup Cost | Runtime Gain | Maintenance | Use Case          |
| ---------------- | ---------- | ------------ | ----------- | ----------------- |
| ETS caching      | Moderate   | High         | Moderate    | Read-heavy data   |
| :persistent_term | Low        | Highest      | Low         | Static config     |
| Stream vs Enum   | Low        | Moderate     | Low         | Large datasets    |
| Ecto preload     | Low        | High         | Low         | Associated data   |
| Query caching    | High       | High         | High        | Expensive queries |

**Recommendation**: Start with low-cost optimizations (Ecto preload, Stream). Add caching only when profiling proves necessity.

## Next Steps

- **[Logging Observability](/en/learn/software-engineering/programming-languages/elixir/in-the-field/logging-observability)** - Monitor performance in production
- **[Error Handling Resilience](/en/learn/software-engineering/programming-languages/elixir/in-the-field/error-handling-resilience)** - Build fault-tolerant systems
- **[Ecto Patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/ecto-patterns)** - Advanced database optimization
- **[Concurrency Patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/concurrency-patterns)** - Optimize concurrent processing

## References

- [Elixir Benchee Documentation](https://hexdocs.pm/benchee/)
- [Erlang Efficiency Guide](https://www.erlang.org/doc/efficiency_guide/users_guide)
- [ETS Documentation](https://www.erlang.org/doc/man/ets.html)
- [Ecto Performance Guide](https://hexdocs.pm/ecto/Ecto.Query.html#module-query-performance)
