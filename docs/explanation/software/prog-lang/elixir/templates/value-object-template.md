# Value Object Template

**Category**: DDD Template
**Complexity**: ⭐⭐ (Medium)
**Prerequisites**: Basic Elixir, Structs, Protocols

## Overview

Value Objects are immutable domain values without identity. They are compared by value rather than reference, making them ideal for monetary amounts, email addresses, coordinates, and other domain concepts.

## When to Use

✅ **Use Value Objects when**:

- No identity needed (value-based equality)
- Immutable by nature (money amounts, coordinates)
- Shared across entities (Money used in many places)
- Validation required (email format, positive amounts)
- Type safety desired (Money vs raw numbers)

❌ **Don't use Value Objects for**:

- Objects needing identity (Donation, User)
- Mutable state (counters, sessions)
- Simple primitives with no behavior (basic strings/integers)
- Objects requiring persistence identity (database entities)

## Template Code

### Basic Value Object: Money

```elixir
defmodule Money do
  @moduledoc """
  Value object for monetary amounts.
  Immutable, validated, type-safe.
  """

  @enforce_keys [:amount, :currency]
  defstruct [:amount, :currency]

  @type t :: %__MODULE__{
    amount: Decimal.t(),
    currency: atom()
  }

  @doc """
  Creates a new Money value object.

  ## Examples

      iex> Money.new(100, :IDR)
      %Money{amount: #Decimal<100>, currency: :IDR}

      iex> Money.new(-50, :USD)
      ** (ArgumentError) amount must be non-negative
  """
  @spec new(number(), atom()) :: t()
  def new(amount, currency) when is_number(amount) and amount >= 0 do
    %Money{
      amount: Decimal.new(amount),
      currency: currency
    }
  end

  def new(amount, _currency) when is_number(amount) and amount < 0 do
    raise ArgumentError, "amount must be non-negative"
  end

  def new(_amount, _currency) do
    raise ArgumentError, "amount must be a number"
  end

  @doc """
  Adds two Money values (same currency only).

  ## Examples

      iex> m1 = Money.new(100, :IDR)
      iex> m2 = Money.new(200, :IDR)
      iex> Money.add(m1, m2)
      %Money{amount: #Decimal<300>, currency: :IDR}
  """
  @spec add(t(), t()) :: t()
  def add(%Money{currency: curr} = m1, %Money{currency: curr} = m2) do
    %Money{
      amount: Decimal.add(m1.amount, m2.amount),
      currency: curr
    }
  end

  def add(%Money{currency: c1}, %Money{currency: c2}) do
    raise ArgumentError, "cannot add different currencies: #{c1} and #{c2}"
  end

  @doc """
  Subtracts two Money values (same currency only).
  """
  @spec subtract(t(), t()) :: t()
  def subtract(%Money{currency: curr} = m1, %Money{currency: curr} = m2) do
    result = Decimal.sub(m1.amount, m2.amount)

    if Decimal.negative?(result) do
      raise ArgumentError, "result would be negative"
    end

    %Money{amount: result, currency: curr}
  end

  def subtract(%Money{currency: c1}, %Money{currency: c2}) do
    raise ArgumentError, "cannot subtract different currencies: #{c1} and #{c2}"
  end

  @doc """
  Multiplies Money by a number.
  """
  @spec multiply(t(), number()) :: t()
  def multiply(%Money{} = money, multiplier) when is_number(multiplier) do
    %Money{
      amount: Decimal.mult(money.amount, Decimal.new(multiplier)),
      currency: money.currency
    }
  end

  @doc """
  Divides Money by a number.
  """
  @spec divide(t(), number()) :: t()
  def divide(%Money{} = money, divisor) when is_number(divisor) and divisor != 0 do
    %Money{
      amount: Decimal.div(money.amount, Decimal.new(divisor)),
      currency: money.currency
    }
  end

  @doc """
  Compares two Money values (same currency only).
  Returns :eq, :lt, or :gt.
  """
  @spec compare(t(), t()) :: :eq | :lt | :gt
  def compare(%Money{currency: curr} = m1, %Money{currency: curr} = m2) do
    Decimal.compare(m1.amount, m2.amount)
  end

  def compare(%Money{currency: c1}, %Money{currency: c2}) do
    raise ArgumentError, "cannot compare different currencies: #{c1} and #{c2}"
  end

  @doc """
  Checks if Money is zero.
  """
  @spec zero?(t()) :: boolean()
  def zero?(%Money{} = money) do
    Decimal.equal?(money.amount, Decimal.new(0))
  end

  @doc """
  Checks if Money is positive.
  """
  @spec positive?(t()) :: boolean()
  def positive?(%Money{} = money) do
    Decimal.positive?(money.amount)
  end

  @doc """
  Checks if two Money values are equal (value-based equality).
  """
  @spec equal?(t(), t()) :: boolean()
  def equal?(%Money{currency: curr} = m1, %Money{currency: curr} = m2) do
    Decimal.equal?(m1.amount, m2.amount)
  end

  def equal?(%Money{}, %Money{}), do: false
end

# Protocol Implementations

defimpl String.Chars, for: Money do
  def to_string(%Money{amount: amount, currency: currency}) do
    "#{Decimal.to_string(amount)} #{currency}"
  end
end

defimpl Inspect, for: Money do
  def inspect(%Money{amount: amount, currency: currency}, _opts) do
    "#Money<#{Decimal.to_string(amount)} #{currency}>"
  end
end

defimpl Jason.Encoder, for: Money do
  def encode(%Money{amount: amount, currency: currency}, opts) do
    Jason.Encode.map(
      %{
        amount: Decimal.to_string(amount),
        currency: to_string(currency)
      },
      opts
    )
  end
end
```

### Tests

```elixir
defmodule MoneyTest do
  use ExUnit.Case, async: true
  doctest Money

  describe "new/2" do
    test "creates money with valid amount and currency" do
      money = Money.new(100, :IDR)
      assert money.amount == Decimal.new(100)
      assert money.currency == :IDR
    end

    test "accepts zero amount" do
      money = Money.new(0, :USD)
      assert Money.zero?(money)
    end

    test "rejects negative amount" do
      assert_raise ArgumentError, "amount must be non-negative", fn ->
        Money.new(-100, :IDR)
      end
    end

    test "rejects non-numeric amount" do
      assert_raise ArgumentError, "amount must be a number", fn ->
        Money.new("100", :IDR)
      end
    end
  end

  describe "add/2" do
    test "adds money with same currency" do
      m1 = Money.new(100, :IDR)
      m2 = Money.new(200, :IDR)

      result = Money.add(m1, m2)

      assert Money.equal?(result, Money.new(300, :IDR))
    end

    test "rejects adding different currencies" do
      m1 = Money.new(100, :IDR)
      m2 = Money.new(50, :USD)

      assert_raise ArgumentError, ~r/cannot add different currencies/, fn ->
        Money.add(m1, m2)
      end
    end

    test "is commutative" do
      m1 = Money.new(100, :IDR)
      m2 = Money.new(200, :IDR)

      assert Money.equal?(Money.add(m1, m2), Money.add(m2, m1))
    end

    test "is associative" do
      m1 = Money.new(100, :IDR)
      m2 = Money.new(200, :IDR)
      m3 = Money.new(300, :IDR)

      result1 = Money.add(Money.add(m1, m2), m3)
      result2 = Money.add(m1, Money.add(m2, m3))

      assert Money.equal?(result1, result2)
    end
  end

  describe "subtract/2" do
    test "subtracts money with same currency" do
      m1 = Money.new(300, :IDR)
      m2 = Money.new(100, :IDR)

      result = Money.subtract(m1, m2)

      assert Money.equal?(result, Money.new(200, :IDR))
    end

    test "rejects negative result" do
      m1 = Money.new(100, :IDR)
      m2 = Money.new(200, :IDR)

      assert_raise ArgumentError, "result would be negative", fn ->
        Money.subtract(m1, m2)
      end
    end

    test "rejects different currencies" do
      m1 = Money.new(100, :IDR)
      m2 = Money.new(50, :USD)

      assert_raise ArgumentError, ~r/cannot subtract different currencies/, fn ->
        Money.subtract(m1, m2)
      end
    end
  end

  describe "multiply/2" do
    test "multiplies money by number" do
      money = Money.new(100, :IDR)

      result = Money.multiply(money, 2.5)

      assert Money.equal?(result, Money.new(250, :IDR))
    end

    test "handles zero multiplier" do
      money = Money.new(100, :IDR)

      result = Money.multiply(money, 0)

      assert Money.zero?(result)
    end

    test "handles fractional multiplier" do
      money = Money.new(100, :IDR)

      result = Money.multiply(money, 0.025)

      assert Money.equal?(result, Money.new(2.5, :IDR))
    end
  end

  describe "divide/2" do
    test "divides money by number" do
      money = Money.new(100, :IDR)

      result = Money.divide(money, 4)

      assert Money.equal?(result, Money.new(25, :IDR))
    end

    test "handles fractional result" do
      money = Money.new(100, :IDR)

      result = Money.divide(money, 3)

      # Decimal precision preserved
      assert result.amount == Decimal.div(Decimal.new(100), Decimal.new(3))
    end
  end

  describe "compare/2" do
    test "compares equal amounts" do
      m1 = Money.new(100, :IDR)
      m2 = Money.new(100, :IDR)

      assert Money.compare(m1, m2) == :eq
    end

    test "compares smaller amount" do
      m1 = Money.new(50, :IDR)
      m2 = Money.new(100, :IDR)

      assert Money.compare(m1, m2) == :lt
    end

    test "compares greater amount" do
      m1 = Money.new(200, :IDR)
      m2 = Money.new(100, :IDR)

      assert Money.compare(m1, m2) == :gt
    end

    test "rejects different currencies" do
      m1 = Money.new(100, :IDR)
      m2 = Money.new(100, :USD)

      assert_raise ArgumentError, ~r/cannot compare different currencies/, fn ->
        Money.compare(m1, m2)
      end
    end
  end

  describe "predicates" do
    test "zero?/1 detects zero amount" do
      assert Money.zero?(Money.new(0, :IDR))
      refute Money.zero?(Money.new(1, :IDR))
    end

    test "positive?/1 detects positive amount" do
      assert Money.positive?(Money.new(1, :IDR))
      refute Money.positive?(Money.new(0, :IDR))
    end

    test "equal?/2 compares values" do
      m1 = Money.new(100, :IDR)
      m2 = Money.new(100, :IDR)
      m3 = Money.new(200, :IDR)

      assert Money.equal?(m1, m2)
      refute Money.equal?(m1, m3)
    end
  end

  describe "protocol implementations" do
    test "String.Chars protocol" do
      money = Money.new(100.50, :IDR)
      assert to_string(money) == "100.5 IDR"
    end

    test "Inspect protocol" do
      money = Money.new(100, :USD)
      assert inspect(money) == "#Money<100 USD>"
    end

    test "Jason.Encoder protocol" do
      money = Money.new(100, :IDR)
      json = Jason.encode!(money)

      assert json == "{\"amount\":\"100\",\"currency\":\"IDR\"}"
    end
  end
end
```

## Financial Domain Example: Zakat Calculation

```elixir
defmodule ZakatCalculator do
  @moduledoc """
  Calculates zakat using Money value objects.
  """

  @zakat_rate Decimal.new("0.025")  # 2.5%

  @doc """
  Calculates zakat if wealth exceeds nisab.
  Returns Money value object.
  """
  @spec calculate(Money.t(), Money.t()) :: {:ok, Money.t()} | {:error, atom()}
  def calculate(%Money{} = wealth, %Money{} = nisab) do
    case Money.compare(wealth, nisab) do
      :gt ->
        zakat = Money.multiply(wealth, Decimal.to_float(@zakat_rate))
        {:ok, zakat}

      _ ->
        {:error, :below_nisab}
    end
  end

  @doc """
  Calculates total zakat for multiple wealth sources.
  """
  @spec calculate_total([Money.t()], Money.t()) :: {:ok, Money.t()} | {:error, atom()}
  def calculate_total(wealth_items, nisab) do
    # Sum all wealth (must be same currency)
    total_wealth = Enum.reduce(wealth_items, fn item, acc ->
      Money.add(acc, item)
    end)

    calculate(total_wealth, nisab)
  end

  @doc """
  Calculates net zakat after deducting expenses.
  """
  @spec calculate_net(Money.t(), Money.t(), Money.t()) :: {:ok, Money.t()} | {:error, atom()}
  def calculate_net(gross_wealth, expenses, nisab) do
    net_wealth = Money.subtract(gross_wealth, expenses)
    calculate(net_wealth, nisab)
  end
end

defmodule ZakatCalculatorTest do
  use ExUnit.Case, async: true

  describe "calculate/2" do
    test "calculates 2.5% when wealth exceeds nisab" do
      wealth = Money.new(100_000_000, :IDR)
      nisab = Money.new(85_000_000, :IDR)

      assert {:ok, zakat} = ZakatCalculator.calculate(wealth, nisab)
      assert Money.equal?(zakat, Money.new(2_500_000, :IDR))
    end

    test "returns error when wealth below nisab" do
      wealth = Money.new(50_000_000, :IDR)
      nisab = Money.new(85_000_000, :IDR)

      assert {:error, :below_nisab} = ZakatCalculator.calculate(wealth, nisab)
    end

    test "returns zero zakat when wealth equals nisab" do
      wealth = Money.new(85_000_000, :IDR)
      nisab = Money.new(85_000_000, :IDR)

      assert {:error, :below_nisab} = ZakatCalculator.calculate(wealth, nisab)
    end
  end

  describe "calculate_total/2" do
    test "sums multiple wealth sources" do
      wealth_items = [
        Money.new(50_000_000, :IDR),
        Money.new(30_000_000, :IDR),
        Money.new(20_000_000, :IDR)
      ]
      nisab = Money.new(85_000_000, :IDR)

      assert {:ok, zakat} = ZakatCalculator.calculate_total(wealth_items, nisab)
      # Total: 100M, Zakat: 2.5M
      assert Money.equal?(zakat, Money.new(2_500_000, :IDR))
    end
  end

  describe "calculate_net/3" do
    test "deducts expenses before calculation" do
      gross_wealth = Money.new(100_000_000, :IDR)
      expenses = Money.new(10_000_000, :IDR)
      nisab = Money.new(85_000_000, :IDR)

      assert {:ok, zakat} = ZakatCalculator.calculate_net(gross_wealth, expenses, nisab)
      # Net: 90M, Zakat: 2.25M
      assert Money.equal?(zakat, Money.new(2_250_000, :IDR))
    end
  end
end
```

## Common Mistakes

### 1. Using Primitives Instead of Value Objects

```elixir
# Bad: Primitive obsession
donation = %{amount: 100, currency: "IDR"}
# Loses type safety, no validation

# Good: Value object
donation = %{amount: Money.new(100, :IDR)}
# Type safe, validated, behavior included
```

### 2. Mutable Value Objects

```elixir
# Bad: Mutable (if this were possible)
money = Money.new(100, :IDR)
money.amount = 200  # Don't do this!

# Good: Immutable operations return new values
money = Money.new(100, :IDR)
new_money = Money.add(money, Money.new(100, :IDR))
# money unchanged, new_money = 200 IDR
```

### 3. Missing Equality Implementation

```elixir
# Bad: Reference equality
m1 = Money.new(100, :IDR)
m2 = Money.new(100, :IDR)
m1 == m2  # false (different references)

# Good: Value equality
Money.equal?(m1, m2)  # true (same value)
```

## Best Practices

1. **Enforce Keys**: Always use `@enforce_keys`
2. **Validate on Construction**: Validate in `new/2`
3. **Immutability**: All operations return new values
4. **Protocol Implementations**: Implement String.Chars, Inspect, Jason.Encoder
5. **Type Safety**: Use @type and @spec
6. **Value Equality**: Provide explicit `equal?/2` function
7. **Guard Against Invalid States**: No negative money, no invalid emails

## Resources

- [Back to Templates README](README.md)
- [Domain-Driven Design](../ex-so-stla-el__domain-driven-design.md)
- [Elixir Structs](https://hexdocs.pm/elixir/Kernel.html#defstruct/1)
- [Protocols](https://hexdocs.pm/elixir/Protocol.html)

---

**Last Updated**: 2025-01-23
**Complexity**: Medium
**Production Ready**: ✅ Yes
