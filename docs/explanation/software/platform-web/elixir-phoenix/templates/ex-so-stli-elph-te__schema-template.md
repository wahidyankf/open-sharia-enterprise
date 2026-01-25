---
title: Phoenix Schema Template
description: Production-ready template for Ecto schemas and changesets
category: explanation
subcategory: stack-libs
tags:
  - phoenix
  - elixir
  - template
  - schema
  - ecto
related:
  - ../ex-so-stli-elph__data-access.md
last_updated: 2026-01-25
---

# Phoenix Schema Template

## Template

```elixir
defmodule Ose.Donations.Donation do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "donations" do
    field :amount, :decimal
    field :donor_name, :string

    belongs_to :campaign, Ose.Donations.Campaign

    timestamps(type: :utc_datetime)
  end

  def changeset(donation, attrs) do
    donation
    |> cast(attrs, [:amount, :donor_name, :campaign_id])
    |> validate_required([:amount, :campaign_id])
    |> validate_number(:amount, greater_than: 0)
  end
end
```
