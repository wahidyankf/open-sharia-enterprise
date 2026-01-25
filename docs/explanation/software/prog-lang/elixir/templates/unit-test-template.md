# Unit Test Template

**Category**: Testing Template
**Complexity**: ⭐⭐ (Medium)
**Prerequisites**: ExUnit, basic Elixir testing

## Overview

ExUnit is Elixir's built-in testing framework providing powerful test organization with `describe` blocks, async execution, doctests, and property-based testing via StreamData. This template demonstrates comprehensive unit testing patterns for domain logic.

## When to Use

✅ **Use Unit Tests when**:

- Testing pure functions in isolation
- Validating business logic without external dependencies
- Fast feedback loop needed (milliseconds)
- Testing edge cases and boundary conditions
- Property-based testing for mathematical invariants

❌ **Don't use Unit Tests for**:

- Database integration (use integration tests)
- External API calls (use integration tests)
- Full request/response cycles (use integration tests)
- Browser interactions (use feature tests)

## Template Code

### Basic Unit Test: ZakatCalculator

```elixir
defmodule ZakatCalculator do
  @moduledoc """
  Calculates Zakat (2.5% Islamic wealth tax) for eligible wealth.
  Wealth must exceed nisab threshold and be held for one lunar year (haul).
  """

  @zakat_rate Decimal.new("0.025")  # 2.5%

  @doc """
  Calculates zakat amount if wealth exceeds nisab.

  ## Examples

      iex> ZakatCalculator.calculate(Money.new(100_000, :IDR), Money.new(85_000, :IDR))
      {:ok, Money.new(2_500, :IDR)}

      iex> ZakatCalculator.calculate(Money.new(50_000, :IDR), Money.new(85_000, :IDR))
      {:error, :below_nisab}
  """
  @spec calculate(Money.t(), Money.t()) :: {:ok, Money.t()} | {:error, atom()}
  def calculate(%Money{} = wealth, %Money{} = nisab) do
    cond do
      not same_currency?(wealth, nisab) ->
        {:error, :currency_mismatch}

      Money.compare(wealth, nisab) in [:lt, :eq] ->
        {:error, :below_nisab}

      true ->
        zakat_amount = Money.multiply(wealth, Decimal.to_float(@zakat_rate))
        {:ok, zakat_amount}
    end
  end

  @doc """
  Calculates zakat for multiple wealth sources.
  All sources must be in the same currency.
  """
  @spec calculate_multiple([Money.t()], Money.t()) :: {:ok, Money.t()} | {:error, atom()}
  def calculate_multiple([], _nisab), do: {:error, :empty_wealth_list}

  def calculate_multiple([first | _rest] = wealth_items, nisab) do
    # Validate all same currency
    if Enum.all?(wealth_items, fn w -> w.currency == first.currency end) do
      total_wealth = Enum.reduce(wealth_items, fn item, acc ->
        Money.add(acc, item)
      end)

      calculate(total_wealth, nisab)
    else
      {:error, :mixed_currencies}
    end
  end

  @doc """
  Calculates net zakat after deducting debts.
  """
  @spec calculate_net(Money.t(), Money.t(), Money.t()) :: {:ok, Money.t()} | {:error, atom()}
  def calculate_net(%Money{} = wealth, %Money{} = debts, %Money{} = nisab) do
    cond do
      not same_currency?(wealth, debts) ->
        {:error, :currency_mismatch}

      not same_currency?(wealth, nisab) ->
        {:error, :currency_mismatch}

      Money.compare(debts, wealth) == :gt ->
        {:error, :debts_exceed_wealth}

      true ->
        net_wealth = Money.subtract(wealth, debts)
        calculate(net_wealth, nisab)
    end
  end

  @doc """
  Validates if haul (one lunar year) has passed.
  Returns true if wealth held >= 354 days (lunar year).
  """
  @spec haul_completed?(Date.t(), Date.t()) :: boolean()
  def haul_completed?(acquisition_date, calculation_date) do
    days_held = Date.diff(calculation_date, acquisition_date)
    days_held >= 354
  end

  # Private helpers

  defp same_currency?(%Money{currency: c1}, %Money{currency: c2}), do: c1 == c2
end
```

### ExUnit Test Suite

```elixir
defmodule ZakatCalculatorTest do
  use ExUnit.Case, async: true
  doctest ZakatCalculator

  alias ZakatCalculator

  # Test data setup
  @nisab_idr Money.new(85_000_000, :IDR)  # ~85g gold in IDR
  @nisab_usd Money.new(5_000, :USD)       # ~85g gold in USD

  describe "calculate/2" do
    test "calculates 2.5% zakat when wealth exceeds nisab" do
      wealth = Money.new(100_000_000, :IDR)

      assert {:ok, zakat} = ZakatCalculator.calculate(wealth, @nisab_idr)
      assert Money.equal?(zakat, Money.new(2_500_000, :IDR))
    end

    test "returns below_nisab error when wealth equals nisab" do
      wealth = Money.new(85_000_000, :IDR)

      assert {:error, :below_nisab} = ZakatCalculator.calculate(wealth, @nisab_idr)
    end

    test "returns below_nisab error when wealth below nisab" do
      wealth = Money.new(50_000_000, :IDR)

      assert {:error, :below_nisab} = ZakatCalculator.calculate(wealth, @nisab_idr)
    end

    test "returns currency_mismatch error when currencies differ" do
      wealth = Money.new(100_000_000, :IDR)
      nisab = Money.new(5_000, :USD)

      assert {:error, :currency_mismatch} = ZakatCalculator.calculate(wealth, nisab)
    end

    test "handles zero wealth" do
      wealth = Money.new(0, :IDR)

      assert {:error, :below_nisab} = ZakatCalculator.calculate(wealth, @nisab_idr)
    end

    test "calculates correct zakat for exact multiples" do
      wealth = Money.new(200_000_000, :IDR)

      assert {:ok, zakat} = ZakatCalculator.calculate(wealth, @nisab_idr)
      assert Money.equal?(zakat, Money.new(5_000_000, :IDR))
    end

    test "calculates correct zakat with decimal precision" do
      wealth = Money.new(123_456_789, :IDR)

      assert {:ok, zakat} = ZakatCalculator.calculate(wealth, @nisab_idr)
      # 2.5% of 123,456,789 = 3,086,419.725
      assert zakat.amount == Decimal.new("3086419.725")
    end
  end

  describe "calculate_multiple/2" do
    test "sums multiple wealth sources and calculates zakat" do
      wealth_items = [
        Money.new(50_000_000, :IDR),
        Money.new(30_000_000, :IDR),
        Money.new(20_000_000, :IDR)
      ]

      assert {:ok, zakat} = ZakatCalculator.calculate_multiple(wealth_items, @nisab_idr)
      # Total: 100M, Zakat: 2.5M
      assert Money.equal?(zakat, Money.new(2_500_000, :IDR))
    end

    test "returns error when total below nisab" do
      wealth_items = [
        Money.new(30_000_000, :IDR),
        Money.new(20_000_000, :IDR),
        Money.new(10_000_000, :IDR)
      ]

      assert {:error, :below_nisab} = ZakatCalculator.calculate_multiple(wealth_items, @nisab_idr)
    end

    test "returns error for empty wealth list" do
      assert {:error, :empty_wealth_list} = ZakatCalculator.calculate_multiple([], @nisab_idr)
    end

    test "returns error when wealth items have mixed currencies" do
      wealth_items = [
        Money.new(50_000_000, :IDR),
        Money.new(5_000, :USD),  # Different currency
        Money.new(20_000_000, :IDR)
      ]

      assert {:error, :mixed_currencies} = ZakatCalculator.calculate_multiple(wealth_items, @nisab_idr)
    end

    test "handles single wealth item" do
      wealth_items = [Money.new(100_000_000, :IDR)]

      assert {:ok, zakat} = ZakatCalculator.calculate_multiple(wealth_items, @nisab_idr)
      assert Money.equal?(zakat, Money.new(2_500_000, :IDR))
    end
  end

  describe "calculate_net/3" do
    test "deducts debts before calculating zakat" do
      wealth = Money.new(100_000_000, :IDR)
      debts = Money.new(10_000_000, :IDR)

      assert {:ok, zakat} = ZakatCalculator.calculate_net(wealth, debts, @nisab_idr)
      # Net wealth: 90M, Zakat: 2.25M
      assert Money.equal?(zakat, Money.new(2_250_000, :IDR))
    end

    test "returns below_nisab when net wealth below nisab" do
      wealth = Money.new(90_000_000, :IDR)
      debts = Money.new(10_000_000, :IDR)

      assert {:error, :below_nisab} = ZakatCalculator.calculate_net(wealth, debts, @nisab_idr)
    end

    test "returns error when debts exceed wealth" do
      wealth = Money.new(50_000_000, :IDR)
      debts = Money.new(60_000_000, :IDR)

      assert {:error, :debts_exceed_wealth} = ZakatCalculator.calculate_net(wealth, debts, @nisab_idr)
    end

    test "returns error when wealth and debts have different currencies" do
      wealth = Money.new(100_000_000, :IDR)
      debts = Money.new(1_000, :USD)

      assert {:error, :currency_mismatch} = ZakatCalculator.calculate_net(wealth, debts, @nisab_idr)
    end

    test "returns error when nisab has different currency" do
      wealth = Money.new(100_000_000, :IDR)
      debts = Money.new(10_000_000, :IDR)
      nisab = Money.new(5_000, :USD)

      assert {:error, :currency_mismatch} = ZakatCalculator.calculate_net(wealth, debts, nisab)
    end

    test "handles zero debts" do
      wealth = Money.new(100_000_000, :IDR)
      debts = Money.new(0, :IDR)

      assert {:ok, zakat} = ZakatCalculator.calculate_net(wealth, debts, @nisab_idr)
      assert Money.equal?(zakat, Money.new(2_500_000, :IDR))
    end
  end

  describe "haul_completed?/2" do
    test "returns true when 354 days have passed (lunar year)" do
      acquisition_date = ~D[2024-01-01]
      calculation_date = ~D[2024-12-20]  # 354 days later

      assert ZakatCalculator.haul_completed?(acquisition_date, calculation_date)
    end

    test "returns true when more than 354 days have passed" do
      acquisition_date = ~D[2024-01-01]
      calculation_date = ~D[2025-01-01]  # 366 days later

      assert ZakatCalculator.haul_completed?(acquisition_date, calculation_date)
    end

    test "returns false when less than 354 days have passed" do
      acquisition_date = ~D[2024-01-01]
      calculation_date = ~D[2024-12-19]  # 353 days later

      refute ZakatCalculator.haul_completed?(acquisition_date, calculation_date)
    end

    test "returns false on acquisition date (0 days)" do
      acquisition_date = ~D[2024-01-01]
      calculation_date = ~D[2024-01-01]

      refute ZakatCalculator.haul_completed?(acquisition_date, calculation_date)
    end

    test "returns false when calculation date before acquisition date" do
      acquisition_date = ~D[2024-12-01]
      calculation_date = ~D[2024-01-01]

      refute ZakatCalculator.haul_completed?(acquisition_date, calculation_date)
    end
  end
end
```

### Property-Based Testing with StreamData

```elixir
defmodule ZakatCalculatorPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias ZakatCalculator

  describe "property: zakat rate is always 2.5%" do
    property "zakat amount is exactly 2.5% of wealth above nisab" do
      check all wealth_amount <- positive_integer(min: 100_000_000, max: 1_000_000_000),
                nisab_amount <- positive_integer(min: 85_000_000, max: 99_999_999),
                wealth_amount > nisab_amount do

        wealth = Money.new(wealth_amount, :IDR)
        nisab = Money.new(nisab_amount, :IDR)

        assert {:ok, zakat} = ZakatCalculator.calculate(wealth, nisab)

        # Zakat should be exactly 2.5% of wealth
        expected_amount = Decimal.mult(Decimal.new(wealth_amount), Decimal.new("0.025"))
        assert Decimal.equal?(zakat.amount, expected_amount)
      end
    end
  end

  describe "property: zakat is always non-negative" do
    property "calculated zakat is never negative" do
      check all wealth_amount <- positive_integer(min: 1, max: 1_000_000_000),
                nisab_amount <- positive_integer(min: 1, max: 1_000_000_000) do

        wealth = Money.new(wealth_amount, :IDR)
        nisab = Money.new(nisab_amount, :IDR)

        case ZakatCalculator.calculate(wealth, nisab) do
          {:ok, zakat} ->
            assert Decimal.compare(zakat.amount, Decimal.new(0)) in [:eq, :gt]

          {:error, :below_nisab} ->
            assert wealth_amount <= nisab_amount
        end
      end
    end
  end

  describe "property: multiple wealth sources commutative" do
    property "order of wealth items does not affect total zakat" do
      check all amounts <- list_of(positive_integer(min: 10_000_000, max: 50_000_000), min_length: 2, max_length: 5) do
        nisab = Money.new(85_000_000, :IDR)

        # Create wealth items in original order
        wealth_items = Enum.map(amounts, &Money.new(&1, :IDR))
        {:ok, zakat1} = ZakatCalculator.calculate_multiple(wealth_items, nisab)

        # Create wealth items in reversed order
        wealth_items_reversed = Enum.reverse(wealth_items)
        {:ok, zakat2} = ZakatCalculator.calculate_multiple(wealth_items_reversed, nisab)

        # Results should be identical
        assert Money.equal?(zakat1, zakat2)
      end
    end
  end

  describe "property: net zakat invariants" do
    property "net wealth is always wealth minus debts" do
      check all wealth_amount <- positive_integer(min: 100_000_000, max: 1_000_000_000),
                debt_percentage <- integer(min: 0, max: 50),
                debt_amount = div(wealth_amount * debt_percentage, 100) do

        wealth = Money.new(wealth_amount, :IDR)
        debts = Money.new(debt_amount, :IDR)
        nisab = Money.new(85_000_000, :IDR)

        net_wealth_amount = wealth_amount - debt_amount

        case ZakatCalculator.calculate_net(wealth, debts, nisab) do
          {:ok, zakat} ->
            # Net zakat should equal zakat on net wealth
            expected_zakat = Money.new(Decimal.to_float(Decimal.mult(Decimal.new(net_wealth_amount), Decimal.new("0.025"))), :IDR)
            assert Money.equal?(zakat, expected_zakat)

          {:error, :below_nisab} ->
            assert net_wealth_amount <= 85_000_000
        end
      end
    end
  end
end
```

## Key Patterns

### 1. Async Test Execution

```elixir
# Enable async execution for independent tests
use ExUnit.Case, async: true
```

### 2. Describe Blocks for Organization

```elixir
describe "calculate/2" do
  test "success case" do
    # ...
  end

  test "error case" do
    # ...
  end
end
```

### 3. Module Attributes for Test Data

```elixir
@nisab_idr Money.new(85_000_000, :IDR)

test "uses nisab from module attribute" do
  assert {:ok, _} = ZakatCalculator.calculate(wealth, @nisab_idr)
end
```

### 4. Doctests for Examples

```elixir
@doc """
## Examples

    iex> ZakatCalculator.calculate(Money.new(100_000, :IDR), Money.new(85_000, :IDR))
    {:ok, Money.new(2_500, :IDR)}
"""
```

### 5. Property-Based Testing

```elixir
use ExUnitProperties

property "invariant description" do
  check all input <- generator() do
    # Assert invariant holds
  end
end
```

## Best Practices

1. **Use `async: true`**: Enable parallel test execution when tests are independent
2. **Organize with `describe`**: Group related tests logically
3. **Use doctests**: Validate documentation examples automatically
4. **Test edge cases**: Zero, negative, boundary values
5. **Property-based tests**: Verify invariants across input ranges
6. **Clear assertions**: Use pattern matching and `assert` macros
7. **Setup callbacks**: Use `setup` for shared test data

## Common Mistakes

### ❌ Mistake 1: Missing `async: true`

```elixir
# Slow: Sequential execution
use ExUnit.Case

# Fast: Parallel execution
use ExUnit.Case, async: true
```

### ❌ Mistake 2: Not testing error cases

```elixir
# Incomplete: Only happy path
test "calculates zakat" do
  assert {:ok, _} = calculate(wealth, nisab)
end

# Complete: Error cases too
test "returns error below nisab" do
  assert {:error, :below_nisab} = calculate(low_wealth, nisab)
end
```

### ❌ Mistake 3: Ignoring doctests

```elixir
# Missing: No doctest directive
defmodule MyTest do
  use ExUnit.Case
end

# Correct: Doctests enabled
defmodule MyTest do
  use ExUnit.Case
  doctest MyModule
end
```

## Resources

- [Back to Templates README](README.md)
- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [StreamData Property Testing](https://hexdocs.pm/stream_data/StreamData.html)
- [Elixir Testing Guide](https://hexdocs.pm/elixir/testing.html)

---

**Last Updated**: 2025-01-23
**Complexity**: Medium
**Production Ready**: ✅ Yes
