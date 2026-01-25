---
title: Phoenix Controller Template
description: Production-ready template for Phoenix REST controllers
category: explanation
subcategory: platform-web
tags:
  - phoenix
  - elixir
  - template
  - controller
  - rest
related:
  - ../ex-so-plwe-elph__rest-apis.md
last_updated: 2026-01-25
---

# Phoenix Controller Template

## Template

```elixir
defmodule OseWeb.DonationController do
  use OseWeb, :controller

  alias Ose.Donations

  def index(conn, params) do
    donations = Donations.list_donations(params)
    render(conn, :index, donations: donations)
  end

  def create(conn, %{"donation" => donation_params}) do
    case Donations.create_donation(donation_params) do
      {:ok, donation} ->
        conn
        |> put_status(:created)
        |> render(:show, donation: donation)

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render(:error, changeset: changeset)
    end
  end
end
```
