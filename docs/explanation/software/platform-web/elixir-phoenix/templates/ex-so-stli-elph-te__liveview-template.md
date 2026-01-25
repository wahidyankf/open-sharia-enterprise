---
title: Phoenix LiveView Template
description: Production-ready template for Phoenix LiveView
category: explanation
subcategory: stack-libs
tags:
  - phoenix
  - elixir
  - template
  - liveview
related:
  - ../ex-so-stli-elph__liveview.md
last_updated: 2026-01-25
---

# Phoenix LiveView Template

## Template

```elixir
defmodule OseWeb.ZakatCalculatorLive do
  use OseWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :result, nil)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.form for={@form} phx-submit="calculate">
        <.input field={@form[:amount]} label="Amount" />
        <.button>Calculate</.button>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("calculate", params, socket) do
    {:noreply, socket}
  end
end
```
