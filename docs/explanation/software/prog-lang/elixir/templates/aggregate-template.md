# Aggregate Template

**Category**: DDD Template
**Complexity**: ⭐⭐⭐⭐ (High)
**Prerequisites**: Entity, Value Object, Domain Events

## Overview

Aggregates define consistency boundaries with an aggregate root that controls access to child entities. They enforce invariants and emit domain events.

## When to Use

✅ **Use Aggregates when**:

- Need transactional consistency
- Enforcing complex business rules
- Coordinating multiple entities
- Protecting invariants

❌ **Don't use Aggregates for**:

- Single entities without relationships
- Read-only data
- Simple CRUD

## Template Code

```elixir
defmodule Campaign do
  use Ecto.Schema
  import Ecto.Changeset

  schema "campaigns" do
    field :name, :string
    field :goal_amount, :decimal
    field :current_amount, :decimal, default: Decimal.new(0)
    field :status, :string, default: "draft"

    has_many :donations, Donation
    timestamps()
  end

  # Aggregate Root Operations

  def add_donation(%__MODULE__{} = campaign, donation_attrs) do
    with :ok <- validate_active(campaign),
         {:ok, donation} <- create_donation(campaign, donation_attrs),
         {:ok, updated} <- update_total(campaign, donation.amount),
         events <- emit_events(updated, donation) do
      {:ok, updated, events}
    end
  end

  defp validate_active(%{status: "active"}), do: :ok
  defp validate_active(_), do: {:error, :campaign_not_active}

  defp create_donation(campaign, attrs) do
    %Donation{campaign_id: campaign.id}
    |> Donation.changeset(attrs)
    |> Repo.insert()
  end

  defp update_total(campaign, amount) do
    new_amount = Decimal.add(campaign.current_amount, amount)

    campaign
    |> change(current_amount: new_amount)
    |> check_completion()
    |> Repo.update()
  end

  defp check_completion(changeset) do
    current = get_field(changeset, :current_amount)
    goal = get_field(changeset, :goal_amount)

    if Decimal.compare(current, goal) != :lt do
      put_change(changeset, :status, "completed")
    else
      changeset
    end
  end

  defp emit_events(campaign, donation) do
    events = [
      DonationReceived.new(donation.id, donation.amount, campaign.id, donation.donor_id)
    ]

    if campaign.status == "completed" do
      [CampaignCompleted.new(campaign.id, campaign.current_amount, campaign.goal_amount, 0) | events]
    else
      events
    end
  end
end
```

## Financial Example: Zakat Aggregate

```elixir
defmodule ZakatAggregate do
  use Ecto.Schema

  schema "zakat_accounts" do
    field :taxpayer_id, :binary_id
    field :year, :integer
    field :total_wealth, :decimal
    field :total_zakat, :decimal
    field :status, :string

    has_many :zakat_payments, ZakatPayment
    timestamps()
  end

  def record_wealth(%__MODULE__{} = account, wealth_source, amount) do
    with :ok <- validate_open(account),
         {:ok, updated} <- add_wealth(account, amount),
         event <- WealthRecorded.new(account.id, wealth_source, amount) do
      {:ok, updated, [event]}
    end
  end

  def calculate_zakat(%__MODULE__{} = account, nisab) do
    if Decimal.compare(account.total_wealth, nisab) == :gt do
      zakat = Decimal.mult(account.total_wealth, Decimal.new("0.025"))

      account
      |> change(total_zakat: zakat)
      |> Repo.update()
    else
      {:error, :below_nisab}
    end
  end
end
```

## Best Practices

1. Keep aggregates small
2. Enforce invariants at root
3. Emit domain events
4. One aggregate per transaction
5. Load complete aggregate

## Resources

- [Back to Templates README](README.md)
- [Entity Template](entity-template.md)

---

**Last Updated**: 2025-01-23
