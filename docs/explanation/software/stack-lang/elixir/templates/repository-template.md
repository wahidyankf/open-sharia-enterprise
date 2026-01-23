# Repository Template

**Category**: DDD Template
**Complexity**: ⭐⭐ (Medium)
**Prerequisites**: Ecto, Ecto.Query

## Overview

Repositories abstract data persistence, providing a collection-like interface to domain objects. They separate domain logic from database concerns.

## When to Use

✅ **Use Repositories when**:

- Need to abstract persistence layer
- Building testable domain logic
- Multiple data sources possible
- Complex query logic needed

❌ **Don't use Repositories for**:

- Simple CRUD with no domain logic
- Direct Ecto access is sufficient

## Template Code

```elixir
defmodule DonationRepository do
  import Ecto.Query
  alias Financial.Repo
  alias Financial.Donation

  def find(id), do: Repo.get(Donation, id)

  def find_by_campaign(campaign_id) do
    from(d in Donation, where: d.campaign_id == ^campaign_id)
    |> Repo.all()
  end

  def find_by_donor(donor_id) do
    from(d in Donation, where: d.donor_id == ^donor_id, order_by: [desc: d.inserted_at])
    |> Repo.all()
  end

  def find_recent(limit \\ 10) do
    from(d in Donation, order_by: [desc: d.inserted_at], limit: ^limit)
    |> Repo.all()
  end

  def save(donation), do: Repo.insert(donation)

  def update(donation, attrs) do
    donation
    |> Donation.changeset(attrs)
    |> Repo.update()
  end

  def total_by_campaign(campaign_id) do
    from(d in Donation,
      where: d.campaign_id == ^campaign_id and d.status == "completed",
      select: sum(d.amount)
    )
    |> Repo.one()
    |> case do
      nil -> Decimal.new(0)
      amount -> amount
    end
  end

  def count_by_status(status) do
    from(d in Donation, where: d.status == ^status, select: count(d.id))
    |> Repo.one()
  end
end
```

## Tests

```elixir
defmodule DonationRepositoryTest do
  use Financial.DataCase, async: true

  describe "find_by_campaign/1" do
    test "returns all donations for campaign" do
      campaign_id = insert(:campaign).id
      d1 = insert(:donation, campaign_id: campaign_id)
      d2 = insert(:donation, campaign_id: campaign_id)
      insert(:donation)  # Different campaign

      donations = DonationRepository.find_by_campaign(campaign_id)

      assert length(donations) == 2
      assert Enum.any?(donations, fn d -> d.id == d1.id end)
      assert Enum.any?(donations, fn d -> d.id == d2.id end)
    end
  end
end
```

## Best Practices

1. Business-focused queries
2. Transaction management in service layer
3. Return domain objects, not Ecto structs
4. Query composition for flexibility

## Resources

- [Back to Templates README](README.md)
- [Ecto.Repo](https://hexdocs.pm/ecto/Ecto.Repo.html)

---

**Last Updated**: 2025-01-23
