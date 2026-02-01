---
title: "Initial Setup"
date: 2026-02-01T00:00:00+07:00
draft: false
weight: 100001
description: "Add Phoenix LiveView to existing Phoenix app - dependencies, configuration, router setup, and your first LiveView"
tags: ["phoenix", "liveview", "setup", "elixir", "installation", "beginner"]
---

**Want to add real-time features to your Phoenix app?** This initial setup guide adds Phoenix LiveView to an existing Phoenix application in minutes. By the end, you'll have LiveView configured and will create your first interactive page.

This tutorial provides 0-5% coverage—just enough to get LiveView working in your Phoenix app. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/platform-web/elixir-phoenix-liveview/quick-start) (5-30% coverage).

## Prerequisites

Before adding LiveView, you need:

- Phoenix framework installed and working (Phoenix 1.7+)
- Elixir 1.14+ and Erlang/OTP 25+ installed
- Existing Phoenix application (or create new one)
- Basic Phoenix knowledge (routing, controllers, views) - see [Phoenix by Example](/en/learn/software-engineering/platform-web/elixir-phoenix/by-example)
- Basic Elixir knowledge - see [Elixir by Example](/en/learn/software-engineering/programming-languages/elixir/by-example)

**Important**: Phoenix LiveView requires Phoenix 1.5+. Phoenix 1.7+ includes LiveView by default in new projects.

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Add** LiveView dependencies to existing Phoenix app
2. **Configure** LiveView in application setup
3. **Update** router for LiveView routes
4. **Create** your first LiveView module
5. **Render** LiveView in browser
6. **Handle** basic user interactions

## Check Phoenix Version

Verify you have Phoenix 1.7+ (includes LiveView by default):

```bash
mix phx.new --version
```

Expected output:

```
Phoenix installer v1.7.10
```

If you see a lower version, update Phoenix:

```bash
mix archive.uninstall phx_new
mix archive.install hex phx_new
```

## Option 1: New Phoenix Project (Recommended)

Create a new Phoenix project with LiveView included:

```bash
# Create new Phoenix app with LiveView
mix phx.new my_app --live

# Navigate to project
cd my_app

# Install dependencies
mix deps.get

# Create database
mix ecto.create

# Start server
mix phx.server
```

Visit `http://localhost:4000` - LiveView is ready!

**What gets configured**:

- LiveView dependencies in `mix.exs`
- LiveView socket configuration in `endpoint.ex`
- LiveView signing salt in `config.exs`
- Example LiveView templates
- Asset build configuration for LiveView JavaScript

## Option 2: Add to Existing Phoenix App

If you have an existing Phoenix 1.5+ app without LiveView:

### Step 1: Add Dependencies

Edit `mix.exs` and add LiveView dependencies:

```elixir
defp deps do
  [
    # ... existing dependencies
    {:phoenix_live_view, "~> 0.20.0"},
    {:floki, ">= 0.30.0", only: :test}
  ]
end
```

Install dependencies:

```bash
mix deps.get
```

### Step 2: Configure LiveView Socket

Edit `lib/my_app_web/endpoint.ex` and add LiveView socket configuration:

```elixir
defmodule MyAppWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :my_app

  # Add LiveView socket
  socket "/live", Phoenix.LiveView.Socket,
    websocket: [connect_info: [session: @session_options]]

  # ... rest of endpoint configuration
end
```

### Step 3: Add Signing Salt

Edit `config/config.exs` and add LiveView signing salt:

```elixir
config :my_app, MyAppWeb.Endpoint,
  live_view: [signing_salt: "YOUR_SECRET_SIGNING_SALT"]
```

Generate a signing salt:

```bash
mix phx.gen.secret 32
```

Use the generated string as `signing_salt`.

### Step 4: Update Router Configuration

Edit `lib/my_app_web/router.ex` and import LiveView helpers:

```elixir
defmodule MyAppWeb.Router do
  use MyAppWeb, :router
  import Phoenix.LiveView.Router  # Add this line

  # ... rest of router configuration
end
```

### Step 5: Add LiveView JavaScript

Edit `assets/js/app.js` and add LiveView hooks:

```javascript
// Import Phoenix LiveView
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";

// Set up LiveView socket
let csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content");
let liveSocket = new LiveSocket("/live", Socket, { params: { _csrf_token: csrfToken } });

// Connect LiveView
liveSocket.connect();

// Expose for debugging (optional)
window.liveSocket = liveSocket;
```

Install LiveView npm package:

```bash
cd assets
npm install phoenix_live_view --save
cd ..
```

### Step 6: Add CSRF Meta Tag

Edit your layout template (e.g., `lib/my_app_web/templates/layout/root.html.heex`) and add CSRF token meta tag in `<head>`:

```heex
<meta name="csrf-token" content={get_csrf_token()} />
```

### Step 7: Recompile

```bash
mix compile
```

## Creating Your First LiveView

Create a simple counter LiveView to verify setup.

### Step 1: Create LiveView Module

Create `lib/my_app_web/live/counter_live.ex`:

```elixir
defmodule MyAppWeb.CounterLive do
  use MyAppWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="counter">
      <h1>Counter: <%= @count %></h1>
      <button phx-click="increment">Increment</button>
      <button phx-click="decrement">Decrement</button>
    </div>
    """
  end

  @impl true
  def handle_event("increment", _params, socket) do
    {:noreply, update(socket, :count, &(&1 + 1))}
  end

  @impl true
  def handle_event("decrement", _params, socket) do
    {:noreply, update(socket, :count, &(&1 - 1))}
  end
end
```

**What this does**:

- `mount/3`: Initialize state (count = 0)
- `render/1`: Render HTML template with HEEx
- `handle_event/3`: Handle button clicks (increment/decrement)

### Step 2: Add LiveView Route

Edit `lib/my_app_web/router.ex` and add LiveView route:

```elixir
scope "/", MyAppWeb do
  pipe_through :browser

  # Add LiveView route
  live "/counter", CounterLive

  # ... existing routes
end
```

### Step 3: Start Server

```bash
mix phx.server
```

Visit `http://localhost:4000/counter`

**What you should see**:

- Counter displaying "Counter: 0"
- Two buttons (Increment, Decrement)
- Clicking buttons updates count in real-time (no page reload!)

## Verifying LiveView Works

Test the counter:

1. Click "Increment" - count goes to 1
2. Click "Increment" again - count goes to 2
3. Click "Decrement" - count goes to 1
4. Open browser DevTools → Network tab
5. Click button - you'll see WebSocket frames, not HTTP requests

**This confirms**: LiveView is connected via WebSocket and updating DOM in real-time!

## Project Structure

After adding LiveView, your structure includes:

```
my_app/
├── lib/
│   └── my_app_web/
│       ├── live/              # LiveView modules
│       │   └── counter_live.ex
│       ├── router.ex          # Routes (live "/path", Module)
│       ├── endpoint.ex        # WebSocket configuration
│       └── ...
├── assets/
│   └── js/
│       └── app.js            # LiveView JavaScript setup
├── config/
│   └── config.exs            # LiveView signing salt
└── mix.exs                   # LiveView dependencies
```

**LiveView file organization**:

- Simple LiveViews: Single file with `mount/3`, `render/1`, `handle_event/3`
- Complex LiveViews: Separate template file (`.html.heex`)
- LiveComponents: Reusable components in `lib/my_app_web/live/components/`

## Common Issues

### "Cannot connect to WebSocket"

**Cause**: LiveView socket not configured in `endpoint.ex`

**Fix**: Add socket configuration:

```elixir
socket "/live", Phoenix.LiveView.Socket,
  websocket: [connect_info: [session: @session_options]]
```

### "Invalid signing salt"

**Cause**: Missing or incorrect signing salt in `config.exs`

**Fix**: Generate and add signing salt:

```bash
# Generate salt
mix phx.gen.secret 32

# Add to config/config.exs
config :my_app, MyAppWeb.Endpoint,
  live_view: [signing_salt: "GENERATED_SALT_HERE"]
```

### "CSRF token not found"

**Cause**: Missing CSRF meta tag in layout

**Fix**: Add to `root.html.heex`:

```heex
<meta name="csrf-token" content={get_csrf_token()} />
```

### LiveView doesn't update on button click

**Cause**: LiveView JavaScript not loaded

**Fix**: Verify `app.js` includes LiveView setup:

```javascript
import {LiveSocket} from "phoenix_live_view"
let liveSocket = new LiveSocket("/live", Socket, ...)
liveSocket.connect()
```

### Compilation error: "undefined function ~H"

**Cause**: Using Phoenix < 1.7 (HEEx not available)

**Fix**: Upgrade Phoenix or use older LiveView syntax:

```elixir
# Phoenix 1.7+ (HEEx)
~H"""
<div><%= @count %></div>
"""

# Phoenix < 1.7 (EEx)
~L"""
<div><%= @count %></div>
"""
```

## LiveView Lifecycle

Understanding the LiveView lifecycle helps debug issues:

1. **HTTP Request**: User navigates to `/counter` (regular HTTP)
2. **Static Render**: Server renders initial HTML (count = 0)
3. **WebSocket Connect**: Browser establishes WebSocket connection
4. **mount/3**: Server initializes LiveView state
5. **Stateful Render**: LiveView ready for interactions
6. **User Event**: User clicks button (phx-click event)
7. **handle_event/3**: Server processes event, updates state
8. **Patch DOM**: Server sends HTML diff, browser patches DOM

**Key insight**: First render is static HTML (fast initial load), then WebSocket for interactivity.

## Next Steps

Now that LiveView is working:

1. **[Quick Start](/en/learn/software-engineering/platform-web/elixir-phoenix-liveview/quick-start)** - Build a complete LiveView feature with forms and validation
2. **[By Example](/en/learn/software-engineering/platform-web/elixir-phoenix-liveview/by-example)** - Learn through 85 annotated code examples
3. **Experiment** - Add more buttons, display different data, try different events

**Recommended workflow**: Quick Start → practice with your own ideas → use By Example as reference.

## Summary

You now have:

- ✅ LiveView dependencies installed
- ✅ WebSocket socket configured
- ✅ Signing salt generated and configured
- ✅ LiveView JavaScript connected
- ✅ First working LiveView (counter)
- ✅ LiveView route in router

**Total setup time**: ~10-15 minutes (new project) or ~30 minutes (existing project)

**Next**: Try the [Quick Start](/en/learn/software-engineering/platform-web/elixir-phoenix-liveview/quick-start) tutorial to build a real-world LiveView feature with forms, validation, and PubSub.
