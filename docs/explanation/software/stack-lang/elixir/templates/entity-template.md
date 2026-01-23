# Entity Template

**Category**: DDD Template
**Complexity**: ⭐⭐⭐ (Medium-High)
**Prerequisites**: Ecto, Value Objects, Database basics

## Overview

Entities are domain objects with persistent identity. Unlike value objects which are compared by value, entities are compared by ID. They typically have mutable state and are persisted to databases.

## When to Use

✅ **Use Entities when**:

- Object needs persistent identity (ID that survives state changes)
- Lifecycle tracking required (created_at, updated_at)
- Mutable state (status changes, updates)
- Database persistence needed
- Object referenced by ID

❌ **Don't use Entities for**:

- Immutable values (Money, Email) → Use Value Objects
- Temporary objects (calculations, DTOs)
- Objects without identity (coordinates, measurements)

## Template Code

### Basic Entity: Donation

```elixir
defmodule Donation do
  @moduledoc """
  Entity representing a donation with persistent identity.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{
    id: Ecto.UUID.t() | nil,
    amount: Decimal.t(),
    currency: String.t(),
    donor_id: Ecto.UUID.t(),
    campaign_id: Ecto.UUID.t(),
    status: String.t(),
    inserted_at: DateTime.t() | nil,
    updated_at: DateTime.t() | nil
  }

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "donations" do
    field :amount, :decimal
    field :currency, :string
    field :donor_id, :binary_id
    field :campaign_id, :binary_id
    field :status, :string, default: "pending"
    field :notes, :string

    timestamps()
  end

  @doc """
  Changeset for creating a donation.
  """
  def changeset(donation, attrs) do
    donation
    |> cast(attrs, [:amount, :currency, :donor_id, :campaign_id, :status, :notes])
    |> validate_required([:amount, :currency, :donor_id, :campaign_id])
    |> validate_number(:amount, greater_than: 0)
    |> validate_inclusion(:currency, ["IDR", "USD", "EUR", "SAR"])
    |> validate_inclusion(:status, ["pending", "completed", "failed", "cancelled"])
    |> foreign_key_constraint(:donor_id)
    |> foreign_key_constraint(:campaign_id)
  end

  @doc """
  Changeset for updating donation status.
  """
  def status_changeset(donation, new_status) do
    donation
    |> change(status: new_status)
    |> validate_inclusion(:status, ["pending", "completed", "failed", "cancelled"])
    |> validate_status_transition(donation.status, new_status)
  end

  @doc """
  Converts database entity to domain model with value objects.
  """
  def to_domain(%__MODULE__{} = donation) do
    %{
      id: donation.id,
      amount: Money.new(Decimal.to_float(donation.amount), String.to_atom(donation.currency)),
      donor_id: donation.donor_id,
      campaign_id: donation.campaign_id,
      status: String.to_atom(donation.status),
      notes: donation.notes,
      created_at: donation.inserted_at,
      updated_at: donation.updated_at
    }
  end

  # Private

  defp validate_status_transition(changeset, "pending", "completed"), do: changeset
  defp validate_status_transition(changeset, "pending", "failed"), do: changeset
  defp validate_status_transition(changeset, "pending", "cancelled"), do: changeset

  defp validate_status_transition(changeset, from, to) do
    add_error(changeset, :status, "invalid transition from #{from} to #{to}")
  end
end
```

### Advanced Entity with Lifecycle

```elixir
defmodule Campaign do
  use Ecto.Schema
  import Ecto.Changeset

  @type status :: :draft | :active | :completed | :cancelled

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "campaigns" do
    field :name, :string
    field :description, :string
    field :goal_amount, :decimal
    field :goal_currency, :string
    field :current_amount, :decimal, default: Decimal.new(0)
    field :status, :string, default: "draft"
    field :start_date, :date
    field :end_date, :date

    timestamps()
  end

  # Changesets

  def create_changeset(campaign, attrs) do
    campaign
    |> cast(attrs, [:name, :description, :goal_amount, :goal_currency, :start_date, :end_date])
    |> validate_required([:name, :goal_amount, :goal_currency, :end_date])
    |> validate_number(:goal_amount, greater_than: 0)
    |> validate_dates()
    |> put_status(:draft)
  end

  def activate_changeset(campaign) do
    campaign
    |> change()
    |> validate_can_activate()
    |> put_status(:active)
  end

  def update_amount_changeset(campaign, amount_change) do
    new_amount = Decimal.add(campaign.current_amount, amount_change)

    campaign
    |> change(current_amount: new_amount)
    |> check_completion()
  end

  def complete_changeset(campaign) do
    campaign
    |> change()
    |> put_status(:completed)
  end

  # Validations

  defp validate_dates(changeset) do
    start_date = get_field(changeset, :start_date)
    end_date = get_field(changeset, :end_date)

    cond do
      is_nil(end_date) ->
        changeset

      not is_nil(start_date) and Date.compare(end_date, start_date) != :gt ->
        add_error(changeset, :end_date, "must be after start date")

      Date.compare(end_date, Date.utc_today()) != :gt ->
        add_error(changeset, :end_date, "must be in the future")

      true ->
        changeset
    end
  end

  defp validate_can_activate(changeset) do
    status = get_field(changeset, :status)

    if status == "draft" do
      changeset
    else
      add_error(changeset, :status, "can only activate draft campaigns")
    end
  end

  defp check_completion(changeset) do
    current = get_field(changeset, :current_amount)
    goal = get_field(changeset, :goal_amount)

    if Decimal.compare(current, goal) != :lt do
      put_status(changeset, :completed)
    else
      changeset
    end
  end

  defp put_status(changeset, status) do
    put_change(changeset, :status, to_string(status))
  end
end
```

### Tests

```elixir
defmodule DonationTest do
  use ExUnit.Case, async: true
  alias Donation

  describe "changeset/2" do
    test "valid donation" do
      attrs = %{
        amount: Decimal.new(100),
        currency: "IDR",
        donor_id: Ecto.UUID.generate(),
        campaign_id: Ecto.UUID.generate()
      }

      changeset = Donation.changeset(%Donation{}, attrs)

      assert changeset.valid?
    end

    test "requires amount" do
      attrs = %{currency: "IDR"}
      changeset = Donation.changeset(%Donation{}, attrs)

      refute changeset.valid?
      assert "can't be blank" in errors_on(changeset).amount
    end

    test "amount must be positive" do
      attrs = %{amount: Decimal.new(-100), currency: "IDR"}
      changeset = Donation.changeset(%Donation{}, attrs)

      refute changeset.valid?
      assert "must be greater than 0" in errors_on(changeset).amount
    end

    test "validates currency" do
      attrs = %{amount: Decimal.new(100), currency: "INVALID"}
      changeset = Donation.changeset(%Donation{}, attrs)

      refute changeset.valid?
      assert "is invalid" in errors_on(changeset).currency
    end
  end

  describe "status_changeset/2" do
    test "allows pending → completed" do
      donation = %Donation{status: "pending"}
      changeset = Donation.status_changeset(donation, "completed")

      assert changeset.valid?
    end

    test "rejects completed → pending" do
      donation = %Donation{status: "completed"}
      changeset = Donation.status_changeset(donation, "pending")

      refute changeset.valid?
      assert "invalid transition" in errors_on(changeset).status
    end
  end

  describe "to_domain/1" do
    test "converts to domain model with value objects" do
      donation = %Donation{
        id: Ecto.UUID.generate(),
        amount: Decimal.new(100),
        currency: "IDR",
        status: "pending",
        donor_id: Ecto.UUID.generate(),
        campaign_id: Ecto.UUID.generate(),
        inserted_at: ~U[2025-01-23 10:00:00Z],
        updated_at: ~U[2025-01-23 10:00:00Z]
      }

      domain = Donation.to_domain(donation)

      assert domain.id == donation.id
      assert Money.equal?(domain.amount, Money.new(100, :IDR))
      assert domain.status == :pending
    end
  end

  defp errors_on(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, _opts} -> msg end)
  end
end
```

## Financial Domain Example: Zakat Payment Entity

```elixir
defmodule ZakatPayment do
  @moduledoc """
  Entity representing a zakat payment with full lifecycle.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "zakat_payments" do
    field :taxpayer_id, :binary_id
    field :wealth_amount, :decimal
    field :nisab_amount, :decimal
    field :zakat_amount, :decimal
    field :currency, :string
    field :payment_year, :integer
    field :status, :string, default: "calculated"
    field :paid_at, :utc_datetime
    field :payment_reference, :string

    timestamps()
  end

  def calculate_changeset(attrs) do
    %__MODULE__{}
    |> cast(attrs, [:taxpayer_id, :wealth_amount, :nisab_amount, :currency, :payment_year])
    |> validate_required([:taxpayer_id, :wealth_amount, :nisab_amount, :currency, :payment_year])
    |> validate_number(:wealth_amount, greater_than: 0)
    |> validate_number(:nisab_amount, greater_than: 0)
    |> put_zakat_amount()
    |> put_status(:calculated)
  end

  def record_payment_changeset(zakat_payment, payment_ref) do
    zakat_payment
    |> change()
    |> validate_status(:calculated)
    |> put_change(:status, "paid")
    |> put_change(:paid_at, DateTime.utc_now())
    |> put_change(:payment_reference, payment_ref)
  end

  defp put_zakat_amount(changeset) do
    wealth = get_field(changeset, :wealth_amount)
    nisab = get_field(changeset, :nisab_amount)

    if wealth && nisab && Decimal.compare(wealth, nisab) == :gt do
      zakat = Decimal.mult(wealth, Decimal.new("0.025"))
      put_change(changeset, :zakat_amount, zakat)
    else
      add_error(changeset, :wealth_amount, "must exceed nisab")
    end
  end

  defp validate_status(changeset, expected_status) do
    current_status = get_field(changeset, :status)

    if current_status == to_string(expected_status) do
      changeset
    else
      add_error(changeset, :status, "must be #{expected_status}")
    end
  end

  defp put_status(changeset, status) do
    put_change(changeset, :status, to_string(status))
  end
end

defmodule ZakatPaymentTest do
  use ExUnit.Case, async: true

  describe "calculate_changeset/1" do
    test "calculates zakat when wealth exceeds nisab" do
      attrs = %{
        taxpayer_id: Ecto.UUID.generate(),
        wealth_amount: Decimal.new(100_000_000),
        nisab_amount: Decimal.new(85_000_000),
        currency: "IDR",
        payment_year: 2025
      }

      changeset = ZakatPayment.calculate_changeset(attrs)

      assert changeset.valid?
      assert Ecto.Changeset.get_field(changeset, :zakat_amount) == Decimal.new(2_500_000)
      assert Ecto.Changeset.get_field(changeset, :status) == "calculated"
    end

    test "rejects when wealth below nisab" do
      attrs = %{
        taxpayer_id: Ecto.UUID.generate(),
        wealth_amount: Decimal.new(50_000_000),
        nisab_amount: Decimal.new(85_000_000),
        currency: "IDR",
        payment_year: 2025
      }

      changeset = ZakatPayment.calculate_changeset(attrs)

      refute changeset.valid?
    end
  end

  describe "record_payment_changeset/2" do
    test "records payment for calculated zakat" do
      zakat = %ZakatPayment{
        id: Ecto.UUID.generate(),
        zakat_amount: Decimal.new(2_500_000),
        status: "calculated"
      }

      changeset = ZakatPayment.record_payment_changeset(zakat, "PAY-123")

      assert changeset.valid?
      assert Ecto.Changeset.get_change(changeset, :status) == "paid"
      assert Ecto.Changeset.get_change(changeset, :payment_reference) == "PAY-123"
      assert Ecto.Changeset.get_change(changeset, :paid_at)
    end

    test "rejects payment for non-calculated zakat" do
      zakat = %ZakatPayment{status: "paid"}

      changeset = ZakatPayment.record_payment_changeset(zakat, "PAY-123")

      refute changeset.valid?
    end
  end
end
```

## Common Mistakes

### 1. Entity vs Value Object Confusion

```elixir
# Bad: Using entity for value
defmodule Money do
  use Ecto.Schema  # NO! Money is a value object
  schema "money" do
    field :amount, :decimal
  end
end

# Good: Entity for identity, value object for values
defmodule Donation do
  use Ecto.Schema
  schema "donations" do
    field :amount, :decimal  # Store value
  end

  def to_domain(donation) do
    %{amount: Money.new(donation.amount, :IDR)}  # Convert to value object
  end
end
```

### 2. Missing Validation

```elixir
# Bad: No validation
def changeset(donation, attrs) do
  cast(donation, attrs, [:amount])
end

# Good: Complete validation
def changeset(donation, attrs) do
  donation
  |> cast(attrs, [:amount, :currency])
  |> validate_required([:amount, :currency])
  |> validate_number(:amount, greater_than: 0)
end
```

### 3. Ignoring State Transitions

```elixir
# Bad: Allow any status change
def update_status(donation, new_status) do
  change(donation, status: new_status)
end

# Good: Validate transitions
defp validate_status_transition(changeset, "pending", "completed"), do: changeset
defp validate_status_transition(changeset, from, to) do
  add_error(changeset, :status, "invalid transition from #{from} to #{to}")
end
```

## Best Practices

1. **UUID Primary Keys**: Use `:binary_id` for distributed systems
2. **Timestamps**: Always include `timestamps()`
3. **Changesets for Everything**: Separate changesets for different operations
4. **Status Transitions**: Validate state machine transitions
5. **Domain Conversion**: Provide `to_domain/1` to convert to rich domain models
6. **Soft Deletes**: Consider `deleted_at` instead of actual deletion
7. **Optimistic Locking**: Use `version` field for concurrent updates

## Resources

- [Back to Templates README](README.md)
- [Ecto Schema](https://hexdocs.pm/ecto/Ecto.Schema.html)
- [Ecto Changeset](https://hexdocs.pm/ecto/Ecto.Changeset.html)
- [Value Object Template](value-object-template.md)

---

**Last Updated**: 2025-01-23
**Complexity**: Medium-High
**Production Ready**: ✅ Yes
