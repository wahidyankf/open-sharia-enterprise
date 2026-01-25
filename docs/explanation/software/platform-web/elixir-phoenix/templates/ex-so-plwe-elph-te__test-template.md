---
title: Phoenix Test Template
description: Production-ready templates for Phoenix tests
category: explanation
subcategory: platform-web
tags:
  - phoenix
  - elixir
  - template
  - testing
related:
  - ../ex-so-plwe-elph__testing.md
last_updated: 2026-01-25
---

# Phoenix Test Template

## Context Test Template

```elixir
defmodule Ose.DonationsTest do
  use Ose.DataCase, async: true

  alias Ose.Donations

  describe "create_donation/1" do
    test "creates donation with valid data" do
      campaign = insert(:campaign)
      attrs = %{campaign_id: campaign.id, amount: "100"}

      assert {:ok, donation} = Donations.create_donation(attrs)
      assert Decimal.eq?(donation.amount, Decimal.new("100"))
    end
  end
end
```

## LiveView Test Template

```elixir
defmodule OseWeb.ZakatCalculatorLiveTest do
  use OseWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  test "calculates zakat", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/zakat/calculator")

    view
    |> form("form", assets: %{gold_grams: "100"})
    |> render_submit()

    assert render(view) =~ "Zakat Due"
  end
end
```
