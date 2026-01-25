---
title: Phoenix Context Template
description: Production-ready template for Phoenix contexts with Islamic finance examples
category: explanation
subcategory: stack-libs
tags:
  - phoenix
  - elixir
  - template
  - context
  - boilerplate
related:
  - ../ex-so-stli-elph__contexts.md
  - ex-so-stli-elph-te__schema-template.md
last_updated: 2026-01-25
---

# Phoenix Context Template

## Overview

Template for creating well-structured Phoenix contexts.

## Template

```elixir
defmodule Ose.Donations do
  @moduledoc """
  Context for managing donations and campaigns.
  """

  import Ecto.Query
  alias Ose.Repo
  alias Ose.Donations.{Campaign, Donation}

  ## Campaigns

  def list_campaigns(criteria \\ []) do
    Campaign
    |> apply_filters(criteria)
    |> Repo.all()
  end

  def get_campaign!(id), do: Repo.get!(Campaign, id)

  def create_campaign(attrs) do
    %Campaign{}
    |> Campaign.changeset(attrs)
    |> Repo.insert()
  end

  defp apply_filters(query, criteria) do
    Enum.reduce(criteria, query, &apply_filter/2)
  end

  defp apply_filter({:status, status}, query) do
    where(query, [c], c.status == ^status)
  end

  defp apply_filter(_, query), do: query
end
```

See [Contexts Guide](../ex-so-stli-elph__contexts.md) for complete patterns.
