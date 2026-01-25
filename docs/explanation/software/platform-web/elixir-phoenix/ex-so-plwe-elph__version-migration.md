---
title: Phoenix Version Migration Guide
description: Guide for migrating Phoenix applications from 1.6 to 1.7+ including breaking changes and new features
category: explanation
subcategory: platform-web
tags:
  - phoenix
  - elixir
  - migration
  - upgrade
  - version
related:
  - ex-so-plwe-elph__best-practices.md
  - ex-so-plwe-elph__liveview.md
last_updated: 2026-01-25
---

# Phoenix Version Migration Guide

## Quick Reference

**Navigation**: [Stack Libraries](../README.md) > [Elixir Phoenix](./README.md) > Version Migration

### At a Glance

| Change Area    | Phoenix 1.6                       | Phoenix 1.7+            | Impact   |
| -------------- | --------------------------------- | ----------------------- | -------- |
| **Routes**     | `Routes.page_path(@conn, :index)` | `~p"/page"`             | Breaking |
| **Templates**  | `.eex` files                      | `.heex` files           | Breaking |
| **LiveView**   | 0.17                              | 0.18+                   | Breaking |
| **Components** | `render_component/2`              | Function components     | Breaking |
| **Layouts**    | `@inner_content`                  | `<%= @inner_content %>` | Breaking |
| **Auth**       | Manual implementation             | `mix phx.gen.auth`      | Optional |

## Overview

Phoenix 1.7 introduced significant improvements to developer experience with verified routes, HEEx templates, and streamlined component APIs. This guide covers migrating from Phoenix 1.6 to 1.7+.

**Target Audience**: Developers upgrading existing Phoenix 1.6 applications to Phoenix 1.7+.

**Versions**: Phoenix 1.6 → 1.7+, LiveView 0.17 → 0.18+, Elixir 1.14+

**Migration Complexity**: Medium - Requires code changes but automated tools help.

## Pre-Migration Checklist

Before starting the migration:

- ✅ Ensure all tests pass on Phoenix 1.6
- ✅ Commit all changes to version control
- ✅ Create a migration branch: `git checkout -b upgrade-phoenix-1.7`
- ✅ Review Phoenix 1.7 changelog: <https://hexdocs.pm/phoenix/1.7.0/changelog.html>
- ✅ Backup production database
- ✅ Plan for downtime if needed

## Step 1: Update Dependencies

Update `mix.exs` dependencies:

```elixir
# mix.exs
defp deps do
  [
    # Phoenix 1.6 → 1.7
    {:phoenix, "~> 1.7.0"},
    {:phoenix_ecto, "~> 4.4"},
    {:phoenix_html, "~> 3.3"},
    {:phoenix_live_reload, "~> 1.4", only: :dev},
    {:phoenix_live_view, "~> 0.18.0"},
    {:phoenix_live_dashboard, "~> 0.7.0"},

    # Other dependencies
    {:ecto_sql, "~> 3.10"},
    {:postgrex, ">= 0.0.0"},
    {:telemetry_metrics, "~> 0.6"},
    {:telemetry_poller, "~> 1.0"},
    {:gettext, "~> 0.20"},
    {:jason, "~> 1.2"},
    {:plug_cowboy, "~> 2.5"}
  ]
end
```

Install dependencies:

```bash
mix deps.get
mix deps.compile
```

## Step 2: Verified Routes

Phoenix 1.7 introduces verified routes with the `~p` sigil, replacing old route helpers.

### Old Style (Phoenix 1.6)

```elixir
# lib/ose_platform_web/controllers/zakat_controller.ex
defmodule OsePlatformWeb.ZakatController do
  use OsePlatformWeb, :controller

  alias OsePlatformWeb.Router.Helpers, as: Routes

  def create(conn, params) do
    # Old route helpers
    redirect(conn, to: Routes.zakat_path(conn, :index))
  end

  def show(conn, %{"id" => id}) do
    # Old route helpers with params
    redirect(conn, to: Routes.zakat_path(conn, :show, id))
  end
end
```

### New Style (Phoenix 1.7+)

```elixir
# lib/ose_platform_web/controllers/zakat_controller.ex
defmodule OsePlatformWeb.ZakatController do
  use OsePlatformWeb, :controller

  def create(conn, params) do
    # ✅ NEW: Verified routes with ~p sigil
    redirect(conn, to: ~p"/zakat")
  end

  def show(conn, %{"id" => id}) do
    # ✅ NEW: Interpolation in verified routes
    redirect(conn, to: ~p"/zakat/#{id}")
  end
end
```

### Migration Pattern for Routes

**Find and replace** in all files:

```bash
# Find old route helpers
grep -r "Routes\." lib/

# Common replacements:
# Routes.page_path(@conn, :index) → ~p"/"
# Routes.user_path(@conn, :show, @user) → ~p"/users/#{@user}"
# Routes.live_path(@socket, MyLive, param: value) → ~p"/my-live?#{[param: value]}"
```

### Route Helper in Templates

**Before (Phoenix 1.6)**:

```heex
<!-- Old route helpers in templates -->
<%= link "View Zakat", to: Routes.zakat_path(@conn, :show, @calculation) %>
<%= link "Donations", to: Routes.donation_path(@conn, :index) %>
```

**After (Phoenix 1.7+)**:

```heex
<!-- ✅ NEW: Verified routes -->
<.link navigate={~p"/zakat/#{@calculation}"}>View Zakat</.link>
<.link navigate={~p"/donations"}>Donations</.link>
```

## Step 3: HEEx Template Migration

Phoenix 1.7 uses HEEx (HTML+EEx) templates with `.heex` extension.

### Rename Template Files

```bash
# Rename all .eex files to .heex
find lib/ose_platform_web -name "*.html.eex" -exec bash -c 'mv "$0" "${0%.html.eex}.html.heex"' {} \;
```

### Update Template Syntax

**Before (Phoenix 1.6 .eex)**:

```eex
<!-- lib/ose_platform_web/templates/zakat/index.html.eex -->
<h1>Zakat Calculations</h1>

<table>
  <thead>
    <tr>
      <th>Date</th>
      <th>Amount</th>
      <th>Status</th>
      <th></th>
    </tr>
  </thead>
  <tbody>
    <%= for calculation <- @calculations do %>
      <tr>
        <td><%= calculation.inserted_at %></td>
        <td>$<%= calculation.amount %></td>
        <td><%= calculation.status %></td>
        <td>
          <%= link "Show", to: Routes.zakat_path(@conn, :show, calculation) %>
          <%= link "Delete", to: Routes.zakat_path(@conn, :delete, calculation), method: :delete %>
        </td>
      </tr>
    <% end %>
  </tbody>
</table>

<%= link "New Calculation", to: Routes.zakat_path(@conn, :new) %>
```

**After (Phoenix 1.7+ .heex)**:

```heex
<!-- lib/ose_platform_web/controllers/zakat_html/index.html.heex -->
<.header>
  Zakat Calculations
  <:actions>
    <.link navigate={~p"/zakat/new"}>
      <.button>New Calculation</.button>
    </.link>
  </:actions>
</.header>

<.table id="calculations" rows={@calculations}>
  <:col :let={calculation} label="Date">
    <%= calculation.inserted_at %>
  </:col>

  <:col :let={calculation} label="Amount">
    $<%= calculation.amount %>
  </:col>

  <:col :let={calculation} label="Status">
    <.badge color={status_color(calculation.status)}>
      <%= calculation.status %>
    </.badge>
  </:col>

  <:action :let={calculation}>
    <.link navigate={~p"/zakat/#{calculation}"}>Show</.link>
  </:action>

  <:action :let={calculation}>
    <.link
      phx-click={JS.push("delete", value: %{id: calculation.id})}
      data-confirm="Are you sure?"
    >
      Delete
    </.link>
  </:action>
</.table>
```

### Core Components

Phoenix 1.7 generates core components in `core_components.ex`:

```elixir
# lib/ose_platform_web/components/core_components.ex
defmodule OsePlatformWeb.CoreComponents do
  use Phoenix.Component

  alias Phoenix.LiveView.JS

  # Core components:
  # - <.button>
  # - <.input>
  # - <.table>
  # - <.list>
  # - <.header>
  # - <.modal>
  # - <.link>
  # - <.icon>
  # - <.error>

  # Generate with: mix phx.gen.html ...
end
```

## Step 4: LiveView 0.18+ Migration

LiveView 0.18 introduced function components and new lifecycle.

### Component Definition

**Before (LiveView 0.17)**:

```elixir
defmodule OsePlatformWeb.ZakatComponents do
  use Phoenix.Component

  # Old-style component
  def calculation_card(assigns) do
    ~H"""
    <div class="card">
      <h3><%= @calculation.type %></h3>
      <p>Amount: $<%= @calculation.amount %></p>
    </div>
    """
  end
end
```

**After (LiveView 0.18+)**:

```elixir
defmodule OsePlatformWeb.ZakatComponents do
  use Phoenix.Component

  # ✅ NEW: Function component with attr definitions
  attr :calculation, :map, required: true
  attr :class, :string, default: nil

  def calculation_card(assigns) do
    ~H"""
    <div class={["card", @class]}>
      <h3><%= @calculation.type %></h3>
      <p>Amount: $<%= @calculation.amount %></p>
    </div>
    """
  end
end
```

### Slot Definitions

**Before (LiveView 0.17)**:

```elixir
def modal(assigns) do
  assigns = assign_new(assigns, :show, fn -> false end)

  ~H"""
  <div :if={@show}>
    <%= render_slot(@inner_block) %>
  </div>
  """
end
```

**After (LiveView 0.18+)**:

```elixir
# ✅ NEW: Explicit slot definitions
attr :show, :boolean, default: false
slot :inner_block, required: true

def modal(assigns) do
  ~H"""
  <div :if={@show}>
    <%= render_slot(@inner_block) %>
  </div>
  """
end
```

### LiveView Mount Changes

**Before (LiveView 0.17)**:

```elixir
defmodule OsePlatformWeb.ZakatLive.Index do
  use OsePlatformWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    calculations = list_calculations()

    socket =
      socket
      |> assign(:calculations, calculations)
      |> assign(:page_title, "Zakat Calculations")

    {:ok, socket}
  end
end
```

**After (LiveView 0.18+)**:

```elixir
defmodule OsePlatformWeb.ZakatLive.Index do
  use OsePlatformWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    # ✅ NEW: Stream for efficient list updates
    socket =
      socket
      |> assign(:page_title, "Zakat Calculations")
      |> stream(:calculations, list_calculations())

    {:ok, socket}
  end

  # ✅ NEW: Handle stream updates efficiently
  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
    calculation = get_calculation!(id)
    {:ok, _} = delete_calculation(calculation)

    {:noreply, stream_delete(socket, :calculations, calculation)}
  end
end
```

### Template Streams

**Before (LiveView 0.17)**:

```heex
<!-- Old: Manual iteration -->
<div id="calculations">
  <%= for calculation <- @calculations do %>
    <div id={"calculation-#{calculation.id}"}>
      <%= calculation.amount %>
    </div>
  <% end %>
</div>
```

**After (LiveView 0.18+)**:

```heex
<!-- ✅ NEW: Stream with automatic DOM patching -->
<div id="calculations" phx-update="stream">
  <div
    :for={{dom_id, calculation} <- @streams.calculations}
    id={dom_id}
  >
    <%= calculation.amount %>
  </div>
</div>
```

## Step 5: Layout Migration

Phoenix 1.7 changes how layouts work.

### Old Layout (Phoenix 1.6)

```heex
<!-- lib/ose_platform_web/templates/layout/app.html.eex -->
<!DOCTYPE html>
<html>
  <head>
    <title>OSE Platform</title>
  </head>
  <body>
    <header>
      <nav>
        <%= link "Home", to: Routes.page_path(@conn, :index) %>
        <%= link "Zakat", to: Routes.zakat_path(@conn, :index) %>
      </nav>
    </header>

    <main>
      <%= @inner_content %>
    </main>
  </body>
</html>
```

### New Layout (Phoenix 1.7+)

```heex
<!-- lib/ose_platform_web/components/layouts/app.html.heex -->
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title><%= assigns[:page_title] || "OSE Platform" %></title>
    <.live_title_tag>
      <%= assigns[:page_title] || "OSE Platform" %>
    </.live_title_tag>
    <link phx-track-static rel="stylesheet" href={~p"/assets/app.css"} />
    <script defer phx-track-static type="text/javascript" src={~p"/assets/app.js"}>
    </script>
  </head>
  <body>
    <header>
      <nav>
        <.link navigate={~p"/"}>Home</.link>
        <.link navigate={~p"/zakat"}>Zakat</.link>
        <.link navigate={~p"/donations"}>Donations</.link>
      </nav>
    </header>

    <main>
      <%= @inner_content %>
    </main>
  </body>
</html>
```

Layout module:

```elixir
# lib/ose_platform_web/components/layouts.ex
defmodule OsePlatformWeb.Layouts do
  use OsePlatformWeb, :html

  embed_templates "layouts/*"
end
```

## Step 6: Controller View Migration

Phoenix 1.7 replaces views with HTML modules.

### Old View (Phoenix 1.6)

```elixir
# lib/ose_platform_web/views/zakat_view.ex
defmodule OsePlatformWeb.ZakatView do
  use OsePlatformWeb, :view

  def status_badge(status) do
    case status do
      :pending -> "badge-warning"
      :completed -> "badge-success"
      :failed -> "badge-danger"
    end
  end
end
```

### New HTML Module (Phoenix 1.7+)

```elixir
# lib/ose_platform_web/controllers/zakat_html.ex
defmodule OsePlatformWeb.ZakatHTML do
  use OsePlatformWeb, :html

  # ✅ NEW: Embed templates from directory
  embed_templates "zakat_html/*"

  # ✅ Helper functions still work
  def status_badge(status) do
    case status do
      :pending -> "badge-warning"
      :completed -> "badge-success"
      :failed -> "badge-danger"
    end
  end
end
```

Directory structure:

```
lib/ose_platform_web/
└── controllers/
    ├── zakat_controller.ex
    ├── zakat_html.ex
    └── zakat_html/
        ├── index.html.heex
        ├── show.html.heex
        ├── new.html.heex
        └── form.html.heex
```

## Step 7: Form Helpers

Phoenix 1.7 introduces new form component.

### Old Forms (Phoenix 1.6)

```heex
<%= form_for @changeset, Routes.zakat_path(@conn, :create), fn f -> %>
  <%= label f, :amount %>
  <%= text_input f, :amount %>
  <%= error_tag f, :amount %>

  <%= label f, :calculation_type %>
  <%= select f, :calculation_type, ["Wealth", "Business", "Gold"] %>

  <%= submit "Calculate" %>
<% end %>
```

### New Forms (Phoenix 1.7+)

```heex
<.simple_form :let={f} for={@changeset} action={~p"/zakat"}>
  <.input field={f[:amount]} type="text" label="Amount" />
  <.input
    field={f[:calculation_type]}
    type="select"
    label="Calculation Type"
    options={["Wealth", "Business", "Gold"]}
  />

  <:actions>
    <.button>Calculate Zakat</.button>
  </:actions>
</.simple_form>
```

## Step 8: JavaScript Interop

Phoenix 1.7 improves JS commands.

### Old JS (Phoenix 1.6)

```elixir
# In LiveView
def handle_event("show_modal", _, socket) do
  {:noreply,
    socket
    |> push_event("js-exec", %{
      to: "#modal",
      attr: "data-show",
      value: "true"
    })
  }
end
```

### New JS (Phoenix 1.7+)

```elixir
# ✅ NEW: Phoenix.LiveView.JS module
alias Phoenix.LiveView.JS

def handle_event("show_modal", _, socket) do
  {:noreply,
    socket
    |> push_event("show-modal", %{})
  }
end

# In template
<.button phx-click={
  JS.show(to: "#modal")
  |> JS.add_class("active", to: "#modal")
}>
  Show Modal
</.button>

<.button phx-click={
  JS.hide(to: "#modal")
  |> JS.remove_class("active", to: "#modal")
}>
  Hide Modal
</.button>
```

## Step 9: Authentication with phx.gen.auth

Phoenix 1.7 includes built-in auth generator.

### Generate Authentication

```bash
# Generate complete auth system
mix phx.gen.auth Accounts User users

# This creates:
# - User schema and migration
# - UserToken for sessions
# - Authentication contexts
# - Login/registration pages
# - Password reset flow
# - Email confirmation
# - Session management
```

### Using Generated Auth

```elixir
# lib/ose_platform_web/router.ex
defmodule OsePlatformWeb.Router do
  use OsePlatformWeb, :router

  import OsePlatformWeb.UserAuth

  # ... other pipelines

  pipeline :require_authenticated_user do
    plug :fetch_current_user
    plug :require_authenticated_user
  end

  scope "/", OsePlatformWeb do
    pipe_through [:browser, :require_authenticated_user]

    # Protected routes
    resources "/zakat", ZakatController
    resources "/donations", DonationController
  end

  # Auth routes
  scope "/", OsePlatformWeb do
    pipe_through [:browser, :redirect_if_user_is_authenticated]

    get "/users/register", UserRegistrationController, :new
    post "/users/register", UserRegistrationController, :create
    get "/users/log_in", UserSessionController, :new
    post "/users/log_in", UserSessionController, :create
  end
end
```

## Step 10: Configuration Changes

Update configuration files:

### endpoint.ex

```elixir
# lib/ose_platform_web/endpoint.ex
defmodule OsePlatformWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :ose_platform

  # ✅ NEW: Session configuration with signing salt
  @session_options [
    store: :cookie,
    key: "_ose_platform_key",
    signing_salt: "your-signing-salt",
    same_site: "Lax"
  ]

  # ✅ NEW: Static plug with cache headers
  plug Plug.Static,
    at: "/",
    from: :ose_platform,
    gzip: false,
    only: OsePlatformWeb.static_paths()

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Session, @session_options
  plug OsePlatformWeb.Router
end
```

## Breaking Changes Summary

### Components

| Phoenix 1.6          | Phoenix 1.7+        | Notes                   |
| -------------------- | ------------------- | ----------------------- |
| `render_component/2` | Function components | Use `attr` and `slot`   |
| Manual assigns       | `attr :name, :type` | Type-checked attributes |
| `@inner_content`     | `@inner_block`      | Renamed in components   |

### Routes

| Phoenix 1.6                            | Phoenix 1.7+              | Notes           |
| -------------------------------------- | ------------------------- | --------------- |
| `Routes.page_path(@conn, :index)`      | `~p"/"`                   | Verified routes |
| `Routes.user_path(@conn, :show, user)` | `~p"/users/#{user}"`      | Interpolation   |
| `link "Text", to: path`                | `<.link navigate={path}>` | Component       |

### Templates

| Phoenix 1.6                  | Phoenix 1.7+     | Notes       |
| ---------------------------- | ---------------- | ----------- |
| `.html.eex`                  | `.html.heex`     | HTML-aware  |
| `<%= content_tag :div do %>` | `<div>`          | Native HTML |
| Manual forms                 | `<.simple_form>` | Component   |

## Islamic Finance Migration Example

### Before (Phoenix 1.6)

```elixir
# Controller
defmodule OsePlatformWeb.MurabahaController do
  use OsePlatformWeb, :controller
  alias OsePlatformWeb.Router.Helpers, as: Routes

  def create(conn, %{"contract" => params}) do
    case create_contract(params) do
      {:ok, contract} ->
        conn
        |> put_flash(:info, "Murabaha contract created")
        |> redirect(to: Routes.murabaha_path(conn, :show, contract))
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end
end

# Template: lib/ose_platform_web/templates/murabaha/index.html.eex
<h1>Murabaha Contracts</h1>
<table>
  <%= for contract <- @contracts do %>
    <tr>
      <td><%= contract.principal %></td>
      <td><%= contract.profit_rate %>%</td>
      <td>
        <%= link "View", to: Routes.murabaha_path(@conn, :show, contract) %>
      </td>
    </tr>
  <% end %>
</table>
```

### After (Phoenix 1.7+)

```elixir
# Controller (minimal changes)
defmodule OsePlatformWeb.MurabahaController do
  use OsePlatformWeb, :controller

  def create(conn, %{"contract" => params}) do
    case create_contract(params) do
      {:ok, contract} ->
        conn
        |> put_flash(:info, "Murabaha contract created")
        |> redirect(to: ~p"/murabaha/#{contract}")  # ✅ Verified route
      {:error, changeset} ->
        render(conn, :new, changeset: changeset)  # ✅ Atom template name
    end
  end
end

# HTML module
defmodule OsePlatformWeb.MurabahaHTML do
  use OsePlatformWeb, :html
  embed_templates "murabaha_html/*"
end

# Template: lib/ose_platform_web/controllers/murabaha_html/index.html.heex
<.header>
  Murabaha Contracts
  <:actions>
    <.link navigate={~p"/murabaha/new"}>
      <.button>New Contract</.button>
    </.link>
  </:actions>
</.header>

<.table id="contracts" rows={@contracts}>
  <:col :let={contract} label="Principal">
    $<%= contract.principal %>
  </:col>

  <:col :let={contract} label="Profit Rate">
    <%= contract.profit_rate %>%
  </:col>

  <:action :let={contract}>
    <.link navigate={~p"/murabaha/#{contract}"}>View</.link>
  </:action>
</.table>
```

## Testing Migration

Update test files:

### Before

```elixir
# test/ose_platform_web/controllers/zakat_controller_test.exs
test "creates zakat calculation", %{conn: conn} do
  conn = post(conn, Routes.zakat_path(conn, :create), calculation: @valid_attrs)
  assert redirected_to(conn) == Routes.zakat_path(conn, :index)
end
```

### After

```elixir
# test/ose_platform_web/controllers/zakat_controller_test.exs
test "creates zakat calculation", %{conn: conn} do
  conn = post(conn, ~p"/zakat", calculation: @valid_attrs)
  assert redirected_to(conn) == ~p"/zakat"
end
```

## Post-Migration Checklist

After completing migration:

- ✅ All tests pass: `mix test`
- ✅ No compilation warnings: `mix compile --warnings-as-errors`
- ✅ Format code: `mix format`
- ✅ Check for deprecated calls: `mix phx.routes`
- ✅ Update documentation
- ✅ Test in development: `mix phx.server`
- ✅ Test in production-like environment
- ✅ Update CI/CD pipelines
- ✅ Deploy to staging
- ✅ Monitor for issues

## Common Migration Issues

### Issue 1: Routes Not Found

**Error**: `undefined function ~p/1`

**Solution**: Ensure `use OsePlatformWeb, :verified_routes` in modules:

```elixir
# lib/ose_platform_web.ex
def verified_routes do
  quote do
    use Phoenix.VerifiedRoutes,
      endpoint: OsePlatformWeb.Endpoint,
      router: OsePlatformWeb.Router
  end
end
```

### Issue 2: Template Compilation Errors

**Error**: `undefined function render_slot/1`

**Solution**: Update to HEEx syntax and use `Phoenix.Component`:

```elixir
use Phoenix.Component
import Phoenix.HTML
```

### Issue 3: LiveView Streams Not Working

**Error**: `undefined function stream/3`

**Solution**: Ensure LiveView 0.18+:

```elixir
{:phoenix_live_view, "~> 0.18.0"}
```

## Related Documentation

- **[Best Practices](./ex-so-plwe-elph__best-practices.md)** - Modern Phoenix patterns
- **[LiveView](./ex-so-plwe-elph__liveview.md)** - Latest LiveView features
- **[Components](./ex-so-plwe-elph__best-practices.md#components)** - Component patterns
- **[Testing](./ex-so-plwe-elph__testing.md)** - Testing migrated code
