---
title: Phoenix Channel Template
description: Production-ready template for Phoenix channels
category: explanation
subcategory: stack-libs
tags:
  - phoenix
  - elixir
  - template
  - channel
  - websocket
related:
  - ../ex-so-stli-elph__channels.md
last_updated: 2026-01-25
---

# Phoenix Channel Template

## Template

```elixir
defmodule OseWeb.DonationChannel do
  use Phoenix.Channel

  @impl true
  def join("donations:" <> _campaign_id, _payload, socket) do
    {:ok, socket}
  end

  @impl true
  def handle_in("new_donation", params, socket) do
    case create_donation(params, socket) do
      {:ok, donation} ->
        broadcast!(socket, "donation_created", %{donation: donation})
        {:reply, {:ok, %{id: donation.id}}, socket}

      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end

  defp create_donation(params, socket) do
    # Implementation
    {:ok, %{}}
  end
end
```
