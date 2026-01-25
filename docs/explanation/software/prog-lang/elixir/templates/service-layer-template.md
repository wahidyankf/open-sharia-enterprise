# Service Layer Template

**Category**: DDD Template
**Complexity**: ⭐⭐⭐⭐ (High)
**Prerequisites**: Repository, Aggregate, Events

## Overview

Service Layer (Phoenix Contexts) orchestrates domain logic, manages transactions, and coordinates between aggregates.

## When to Use

✅ **Use Service Layer when**:

- Coordinating multiple aggregates
- Managing transactions
- Implementing use cases
- Orchestrating workflows

## Template Code

```elixir
defmodule DonationContext do
  alias Financial.Repo
  alias Financial.{Donation, Campaign, DonationRepository}

  def create_donation(attrs) do
    Repo.transaction(fn ->
      with {:ok, donation} <- validate_and_create(attrs),
           {:ok, campaign} <- update_campaign(donation),
           :ok <- publish_events(donation, campaign) do
        donation
      else
        {:error, reason} -> Repo.rollback(reason)
      end
    end)
  end

  defp validate_and_create(attrs) do
    %Donation{}
    |> Donation.changeset(attrs)
    |> Repo.insert()
  end

  defp update_campaign(donation) do
    campaign = Repo.get!(Campaign, donation.campaign_id)

    case Campaign.add_donation(campaign, donation) do
      {:ok, updated, events} -> {:ok, {updated, events}}
      error -> error
    end
  end

  defp publish_events(donation, {campaign, events}) do
    Enum.each(events, &EventPublisher.publish/1)
    :ok
  end

  def list_donations(filters \\ %{}) do
    DonationRepository.list(filters)
  end

  def get_donation(id) do
    case DonationRepository.find(id) do
      nil -> {:error, :not_found}
      donation -> {:ok, donation}
    end
  end

  def process_donation(donation_id) do
    Repo.transaction(fn ->
      donation = Repo.get!(Donation, donation_id)

      with {:ok, validated} <- validate_payment(donation),
           {:ok, processed} <- process_payment(validated),
           {:ok, updated} <- mark_completed(processed) do
        updated
      else
        error -> Repo.rollback(error)
      end
    end)
  end

  defp validate_payment(donation), do: {:ok, donation}
  defp process_payment(donation), do: {:ok, donation}

  defp mark_completed(donation) do
    donation
    |> Donation.status_changeset("completed")
    |> Repo.update()
  end
end
```

## Financial Example: Zakat Context

```elixir
defmodule ZakatContext do
  alias Financial.Repo

  def calculate_zakat(taxpayer_id, wealth_items, nisab) do
    Repo.transaction(fn ->
      with {:ok, account} <- get_or_create_account(taxpayer_id),
           {:ok, updated} <- record_wealth_items(account, wealth_items),
           {:ok, calculated} <- ZakatAggregate.calculate_zakat(updated, nisab),
           :ok <- publish_calculation_event(calculated) do
        calculated
      else
        error -> Repo.rollback(error)
      end
    end)
  end

  defp get_or_create_account(taxpayer_id) do
    # Implementation
    {:ok, %ZakatAggregate{}}
  end

  defp record_wealth_items(account, wealth_items) do
    Enum.reduce_while(wealth_items, {:ok, account}, fn item, {:ok, acc} ->
      case ZakatAggregate.record_wealth(acc, item.source, item.amount) do
        {:ok, updated, _events} -> {:cont, {:ok, updated}}
        error -> {:halt, error}
      end
    end)
  end

  defp publish_calculation_event(account) do
    event = ZakatCalculated.new(account.id, account.total_wealth, account.total_zakat)
    EventPublisher.publish(event)
    :ok
  end
end
```

## Best Practices

1. Transaction boundaries at service layer
2. Coordinate aggregates
3. Publish events after commit
4. Handle all error cases
5. Keep services focused

## Resources

- [Back to Templates README](README.md)
- [Phoenix Contexts](https://hexdocs.pm/phoenix/contexts.html)

---

**Last Updated**: 2025-01-23
