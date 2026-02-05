---
title: "Immutability Patterns"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000013
description: "Functional data transformation patterns and efficient immutable updates in Elixir"
tags: ["elixir", "immutability", "functional-programming", "pipe-operator", "data-transformation"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/persistent-term"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/structs-protocols"
---

**Working with immutable data in Elixir?** This guide teaches functional data transformation patterns following OTP-first principles, showing how immutability enables reliable concurrent systems on the BEAM.

## Why Immutability Matters

Elixir enforces immutability at the language level, preventing data mutation after creation. This design choice powers BEAM's concurrent and fault-tolerant architecture:

- **Process isolation** - Each process owns immutable copies, preventing shared state bugs
- **Safe concurrency** - No race conditions from simultaneous mutations
- **Predictable transformations** - Functions always return new data without side effects
- **Message passing reliability** - Messages are immutable copies, ensuring data integrity
- **Simplified reasoning** - No hidden mutations to track mentally

**Production impact**: Immutable data prevents entire classes of concurrent bugs common in mutable languages. GenServer processes can safely share data through messages without synchronization primitives.

Understanding these patterns is crucial before adopting advanced frameworks like Ecto (database queries) or Phoenix (request transformations), which leverage immutability for safe concurrent operations.

## Financial Domain Examples

Examples use Shariah-compliant financial operations:

- **Donation list transformations** - Processing multiple charitable contributions
- **Zakat calculations** - Computing charity percentages on donation lists
- **Transaction history** - Building audit trails from immutable records

These domains demonstrate production-ready immutable data patterns.

## Pipe Operator for Transformations

### Pattern 1: Sequential Transformations

The pipe operator (`|>`) chains immutable transformations, creating clear data flow.

**OTP Primitive**: Pipe operator with Enum module.

```elixir
# Calculate total zakat from donation list
defmodule ZakatCalculator do
  # => Module for Shariah-compliant charity calculations

  def calculate_total_zakat(donations) do
    # => donations: List of %{amount: integer(), eligible: boolean()}
    # => Returns: Total zakat amount (2.5% of eligible donations)

    donations
    # => Input: List of donation maps

    |> Enum.filter(& &1.eligible)
    # => Filters only zakat-eligible donations
    # => Result: Subset of donations where eligible == true

    |> Enum.map(& &1.amount)
    # => Extracts amount from each donation
    # => Result: List of integers [1000, 2000, 500]

    |> Enum.sum()
    # => Sums all amounts
    # => Result: Total eligible amount (integer)

    |> Kernel.*(0.025)
    # => Applies 2.5% zakat rate
    # => Result: Total zakat owed (float)

    |> Float.round(2)
    # => Rounds to 2 decimal places
    # => Type: float()
  end
end

# Usage
donations = [
  # => List of donation maps
  %{donor: "Ahmad", amount: 10000, eligible: true},
  # => Eligible: 10000 * 0.025 = 250.0
  %{donor: "Fatima", amount: 5000, eligible: false},
  # => Not eligible (excluded from calculation)
  %{donor: "Hassan", amount: 8000, eligible: true}
  # => Eligible: 8000 * 0.025 = 200.0
]

zakat = ZakatCalculator.calculate_total_zakat(donations)
# => zakat = 450.0 (10000 + 8000 = 18000 * 0.025)
# => Type: float()
```

**Why pipe operator?**

- **Readability** - Data flow reads top to bottom
- **Immutability** - Each step produces new data without mutating original
- **Composability** - Easy to add/remove transformation steps

**Trade-off**: Pipe operator syntax vs nested function calls.

| Aspect            | Pipe Operator                  | Nested Calls                             |
| ----------------- | ------------------------------ | ---------------------------------------- |
| Readability       | Top-to-bottom flow             | Inside-out reading                       |
| Debugging         | Easy to inspect intermediate   | Must unwrap nested structure             |
| Immutability      | Clear transformation chain     | Same immutability, harder to see         |
| Performance       | Identical (syntax sugar)       | Identical                                |
| Learning curve    | Elixir-specific operator       | Universal function composition           |
| Production choice | **Recommended for chains > 2** | Use for single or simple transformations |

### Pattern 2: Conditional Transformations

Pipe operator with conditional logic for complex business rules.

```elixir
# Apply zakat calculation with exemption threshold
defmodule ConditionalZakat do
  @nisab 85_000
  # => Nisab: Minimum wealth threshold for zakat obligation
  # => 85,000 currency units (Shariah standard)

  def calculate_with_exemption(donations) do
    # => Returns: Zakat amount or :exempt atom

    total =
      donations
      # => Input: List of donation maps

      |> Enum.filter(& &1.eligible)
      # => Filter zakat-eligible donations

      |> Enum.map(& &1.amount)
      # => Extract amounts

      |> Enum.sum()
      # => Sum total eligible wealth
      # => Type: integer()

    if total >= @nisab do
      # => Check if meets nisab threshold

      total
      # => Total eligible amount

      |> Kernel.*(0.025)
      # => Apply 2.5% zakat rate

      |> Float.round(2)
      # => Round to 2 decimal places
      # => Type: float()
    else
      :exempt
      # => Below nisab threshold
      # => Type: :exempt atom
    end
  end
end

# Usage
small_donations = [%{amount: 20000, eligible: true}]
# => Total: 20000 (below nisab)

large_donations = [%{amount: 100000, eligible: true}]
# => Total: 100000 (above nisab)

result1 = ConditionalZakat.calculate_with_exemption(small_donations)
# => result1 = :exempt (20000 < 85000)

result2 = ConditionalZakat.calculate_with_exemption(large_donations)
# => result2 = 2500.0 (100000 * 0.025)
# => Type: float() | :exempt
```

## Immutable Update Patterns

### Pattern 3: Map Updates with Map.update

`Map.update/4` safely transforms map values without mutation.

```elixir
# Update donation status after processing
defmodule DonationProcessor do
  def mark_processed(donation) do
    # => donation: Map with :status key
    # => Returns: New map with :status updated

    Map.update(
      donation,
      # => Original map (unchanged)

      :status,
      # => Key to update

      :pending,
      # => Default value if key missing

      fn _old -> :processed end
      # => Update function (ignores old value)
      # => Returns :processed atom
    )
    # => Returns: New map with updated status
    # => Original donation unchanged
  end

  def increment_retry_count(donation) do
    # => Increment retry counter for failed donations

    Map.update(
      donation,
      :retry_count,
      # => Key to increment

      1,
      # => Default: 1 if key missing (first retry)

      fn count -> count + 1 end
      # => Increment existing count
      # => Type: integer()
    )
    # => Returns new map with incremented counter
  end
end

# Usage
donation = %{id: 123, amount: 5000, status: :pending, retry_count: 0}
# => Original donation map

processed = DonationProcessor.mark_processed(donation)
# => processed = %{id: 123, amount: 5000, status: :processed, retry_count: 0}
# => Original donation unchanged

retried = DonationProcessor.increment_retry_count(donation)
# => retried = %{id: 123, amount: 5000, status: :pending, retry_count: 1}
# => Original donation still unchanged
```

**Why Map.update?**

- **Safe defaults** - Handles missing keys gracefully
- **Functional transformation** - Uses function to compute new value
- **Clear intent** - Explicit about what's being updated

### Pattern 4: Nested Map Updates

Update deeply nested maps with `put_in`, `update_in`, `get_and_update_in`.

```elixir
# Update nested financial records
defmodule FinancialRecords do
  def update_donor_total(records, donor_id, new_total) do
    # => records: %{donors: %{id => %{name, total}}}
    # => Returns: New records map with updated total

    put_in(
      records,
      [:donors, donor_id, :total],
      # => Path to nested value

      new_total
      # => New value to set
    )
    # => Returns: New map with updated nested value
    # => Original records unchanged
  end

  def increment_donation_count(records, donor_id) do
    # => Increment donation count for specific donor

    update_in(
      records,
      [:donors, donor_id, :count],
      # => Path to value to increment

      fn count -> count + 1 end
      # => Transformation function
      # => Type: integer()
    )
    # => Returns: New map with incremented count
  end

  def get_and_reset_total(records, donor_id) do
    # => Get current total and reset to zero

    get_and_update_in(
      records,
      [:donors, donor_id, :total],
      # => Path to nested value

      fn current_total ->
        {current_total, 0}
        # => Returns {old_value, new_value} tuple
        # => old_value returned, new_value stored
      end
    )
    # => Returns: {old_total, updated_records}
    # => Type: {integer(), map()}
  end
end

# Usage
records = %{
  donors: %{
    1 => %{name: "Ahmad", total: 50000, count: 5},
    # => Donor 1 data
    2 => %{name: "Fatima", total: 30000, count: 3}
    # => Donor 2 data
  }
}

updated = FinancialRecords.update_donor_total(records, 1, 75000)
# => updated.donors[1].total = 75000
# => Original records.donors[1].total still 50000

incremented = FinancialRecords.increment_donation_count(records, 2)
# => incremented.donors[2].count = 4
# => Original records.donors[2].count still 3

{old_total, reset_records} = FinancialRecords.get_and_reset_total(records, 1)
# => old_total = 50000
# => reset_records.donors[1].total = 0
# => Original records unchanged
```

## Efficient Immutable Operations

### Pattern 5: MapSet for Unique Collections

MapSets provide efficient set operations with immutability.

```elixir
# Track unique donors efficiently
defmodule UniqueDonors do
  def collect_donors(donations) do
    # => donations: List of %{donor: string(), amount: integer()}
    # => Returns: MapSet of unique donor names

    donations
    # => Input list

    |> Enum.map(& &1.donor)
    # => Extract donor names
    # => Result: List of strings (may have duplicates)

    |> MapSet.new()
    # => Convert to MapSet (removes duplicates)
    # => Type: MapSet.t(String.t())
  end

  def add_donor(donor_set, new_donor) do
    # => Add donor to set (idempotent)

    MapSet.put(donor_set, new_donor)
    # => Returns new MapSet with donor added
    # => If donor exists, returns unchanged set
    # => Type: MapSet.t(String.t())
  end

  def check_eligibility(donor_set, donor_name) do
    # => Check if donor in eligible set

    MapSet.member?(donor_set, donor_name)
    # => Returns: boolean()
  end
end

# Usage
donations = [
  %{donor: "Ahmad", amount: 5000},
  # => First Ahmad donation
  %{donor: "Fatima", amount: 3000},
  %{donor: "Ahmad", amount: 2000},
  # => Second Ahmad donation (duplicate)
  %{donor: "Hassan", amount: 4000}
]

unique_donors = UniqueDonors.collect_donors(donations)
# => unique_donors = MapSet.new(["Ahmad", "Fatima", "Hassan"])
# => Size: 3 (duplicates removed)

updated_set = UniqueDonors.add_donor(unique_donors, "Aisha")
# => updated_set size: 4
# => Original unique_donors still size 3

is_eligible = UniqueDonors.check_eligibility(unique_donors, "Ahmad")
# => is_eligible = true

not_found = UniqueDonors.check_eligibility(unique_donors, "Unknown")
# => not_found = false
```

**Why MapSet?**

- **O(1) lookup** - Fast membership checking
- **Automatic deduplication** - No manual duplicate handling
- **Set operations** - Union, intersection, difference built-in

### Pattern 6: Stream for Lazy Evaluation

Streams defer computation until needed, efficient for large datasets.

```elixir
# Process large donation datasets efficiently
defmodule LargeDonationProcessor do
  def calculate_zakat_lazy(donations_stream) do
    # => donations_stream: Stream or Enumerable
    # => Returns: Stream (lazy, not yet computed)

    donations_stream
    # => Input: Lazy stream

    |> Stream.filter(& &1.eligible)
    # => Lazy filter: Not executed until enumeration

    |> Stream.map(& &1.amount)
    # => Lazy map: Not executed yet

    |> Stream.map(&(&1 * 0.025))
    # => Lazy zakat calculation: Not executed yet

    |> Stream.map(&Float.round(&1, 2))
    # => Lazy rounding: Still not executed
    # => Returns: Stream (no computation yet)
  end

  def take_first_n_zakat(donations, n) do
    # => Calculate zakat only for first n donations

    donations
    |> Stream.filter(& &1.eligible)
    |> Stream.map(& &1.amount * 0.025)
    # => Stream transformations (lazy)

    |> Enum.take(n)
    # => Force evaluation: Only processes n items
    # => Type: list(float())
  end
end

# Usage
large_donations = 1..1_000_000
# => Range: 1 million donations (not yet materialized)

|> Stream.map(fn id ->
     %{id: id, amount: :rand.uniform(10000), eligible: rem(id, 2) == 0}
   end)
# => Lazy stream: Generates donation maps on demand
# => Not yet computed

zakat_stream = LargeDonationProcessor.calculate_zakat_lazy(large_donations)
# => zakat_stream: Still lazy stream
# => No computation performed yet

first_10 = LargeDonationProcessor.take_first_n_zakat(large_donations, 10)
# => first_10: List of 10 float values
# => Only processed 10 donations from 1 million
# => Efficient: Didn't compute all 1 million
```

**Why Stream?**

- **Memory efficiency** - Processes one item at a time
- **Lazy evaluation** - Computation deferred until needed
- **Early termination** - Stop processing when condition met

**Trade-off**: Stream vs Enum.

| Aspect            | Stream (Lazy)                 | Enum (Eager)                     |
| ----------------- | ----------------------------- | -------------------------------- |
| Memory usage      | Constant (one item at a time) | Linear (entire list in memory)   |
| Performance       | Better for large datasets     | Better for small datasets        |
| Composability     | Excellent (chains lazily)     | Good (materializes each step)    |
| Debugging         | Harder (lazy evaluation)      | Easier (immediate results)       |
| When to use       | Large datasets, early exit    | Small datasets, need all results |
| Production choice | **Large/infinite data**       | **Small known datasets**         |

## Best Practices

### ✅ DO: Chain Transformations with Pipe

```elixir
# Good: Clear data flow
donations
|> Enum.filter(& &1.eligible)
|> Enum.map(& &1.amount)
|> Enum.sum()
|> Kernel.*(0.025)
```

```elixir
# Bad: Nested calls (hard to read)
Kernel.*(Enum.sum(Enum.map(Enum.filter(donations, & &1.eligible), & &1.amount)), 0.025)
```

### ✅ DO: Use Appropriate Collection Type

```elixir
# Good: MapSet for uniqueness
unique_donors =
  donations
  |> Enum.map(& &1.donor)
  |> MapSet.new()
```

```elixir
# Bad: List with manual deduplication
unique_donors =
  donations
  |> Enum.map(& &1.donor)
  |> Enum.uniq()  # Less efficient for membership checking
```

### ✅ DO: Use Stream for Large Datasets

```elixir
# Good: Stream for million records
File.stream!("donations.csv")
|> Stream.map(&parse_donation/1)
|> Stream.filter(& &1.eligible)
|> Enum.take(100)  # Only processes until 100 found
```

```elixir
# Bad: Enum loads entire file
File.read!("donations.csv")
|> String.split("\n")
|> Enum.map(&parse_donation/1)
|> Enum.filter(& &1.eligible)
|> Enum.take(100)  # Processed entire file unnecessarily
```

### ✅ DO: Use Map.update for Safe Updates

```elixir
# Good: Map.update with default
Map.update(donation, :retry_count, 1, &(&1 + 1))
```

```elixir
# Bad: Manual nil handling
retry_count = donation[:retry_count] || 0
Map.put(donation, :retry_count, retry_count + 1)
```

### ✅ DO: Use put_in/update_in for Nested Updates

```elixir
# Good: put_in for nested structure
put_in(records, [:donors, id, :total], new_total)
```

```elixir
# Bad: Manual nested map updates
%{records |
  donors: %{records.donors |
    id => %{records.donors[id] | total: new_total}}}
```

## Common Mistakes

### ❌ Mistake 1: Treating Immutable Data as Mutable

```elixir
# Wrong: Expecting mutation
list = [1, 2, 3]
List.delete(list, 2)  # Returns [1, 3]
IO.inspect(list)      # Still [1, 2, 3] - unchanged!
```

```elixir
# Correct: Capture returned value
list = [1, 2, 3]
new_list = List.delete(list, 2)  # Returns [1, 3]
IO.inspect(new_list)              # [1, 3]
```

### ❌ Mistake 2: Using Enum for Large Datasets

```elixir
# Wrong: Eager evaluation for large file
File.read!("huge.csv")
|> String.split("\n")
|> Enum.map(&process/1)
|> Enum.take(10)  # Processed entire file
```

```elixir
# Correct: Stream for lazy evaluation
File.stream!("huge.csv")
|> Stream.map(&process/1)
|> Enum.take(10)  # Only processes 10 lines
```

### ❌ Mistake 3: Over-nesting Pipe Operators

```elixir
# Wrong: Too complex in single pipe
donations
|> Enum.filter(&(&1.eligible and &1.amount > 1000 and not is_nil(&1.donor)))
|> Enum.group_by(& &1.category)
|> Enum.map(fn {cat, items} -> {cat, calculate_stats(items)} end)
|> Enum.into(%{})
```

```elixir
# Correct: Break into helper functions
donations
|> filter_eligible_donations()
|> group_by_category()
|> calculate_category_stats()
```

### ❌ Mistake 4: Using List.concat in Loops

```elixir
# Wrong: Quadratic complexity
Enum.reduce(1..1000, [], fn i, acc ->
  acc ++ [i * 2]  # O(n) for each iteration = O(n²)
end)
```

```elixir
# Correct: Prepend and reverse
Enum.reduce(1..1000, [], fn i, acc ->
  [i * 2 | acc]  # O(1) prepend
end)
|> Enum.reverse()  # O(n) once at end
```

## Production Integration

These immutability patterns integrate with OTP processes:

**GenServer state transformations**:

```elixir
def handle_call(:add_donation, _from, state) do
  new_state = Map.update(state, :total, 0, &(&1 + amount))
  {:reply, :ok, new_state}
end
```

**Process message passing**:

```elixir
# Immutable message ensures no shared state
send(pid, {:donation, %{id: 1, amount: 5000}})
```

**Supervisor restart safety**:

```elixir
# State rebuilt from immutable data
def init(_) do
  state = load_donations() |> build_initial_state()
  {:ok, state}
end
```

## Framework Adoption

**When OTP primitives sufficient**:

- Simple data transformations
- In-memory processing
- Small to medium datasets

**When to adopt frameworks**:

- **Ecto** - Database query transformations with Ecto.Changeset
- **Phoenix** - HTTP request/response transformations with Plug.Conn
- **Flow** - Parallel stream processing for compute-intensive transformations

## References

**OTP Documentation**:

- [Enum module](https://hexdocs.pm/elixir/Enum.html) - Eager collection operations
- [Stream module](https://hexdocs.pm/elixir/Stream.html) - Lazy collection operations
- [Map module](https://hexdocs.pm/elixir/Map.html) - Map transformations
- [MapSet module](https://hexdocs.pm/elixir/MapSet.html) - Set operations

**Related Guides**:

- [Best Practices](/en/learn/software-engineering/programming-languages/elixir/in-the-field/best-practices) - Production patterns
- [GenServer Patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/genserver-patterns) - State management with immutable data
- [Concurrency Patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/concurrency-patterns) - Parallel transformations

**Production Resources**:

- [Elixir School - Collections](https://elixirschool.com/en/lessons/basics/collections) - Collection fundamentals
- [Elixir Getting Started - Enumerables and Streams](https://elixir-lang.org/getting-started/enumerables-and-streams.html) - Official guide
