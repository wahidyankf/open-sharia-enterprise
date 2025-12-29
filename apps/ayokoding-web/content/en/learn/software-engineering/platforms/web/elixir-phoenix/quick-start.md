---
title: "Quick Start"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100002
description: "Learn core Phoenix concepts and web development patterns"
tags:
  - elixir
  - phoenix
  - web-framework
  - quick-start
---

Learn core Phoenix concepts and web development patterns. This Quick Start teaches essential Phoenix skills.

## 🎯 What You'll Learn

By the end of this tutorial, you'll understand:

- Routing and controllers
- Templates and views
- Contexts and schemas
- LiveView for real-time features

## 📋 Prerequisites

- Phoenix installed (see [Initial Setup](/en/learn/software-engineering/platforms/web/elixir-phoenix/initial-setup))

## 🛣️ Routing and Controllers

Edit `lib/myapp_web/router.ex`:

```elixir
scope "/", MyAppWeb do
  pipe_through :browser

  get "/", PageController, :index
  get "/about", PageController, :about
  resources "/posts", PostController
end
```

Create controller `lib/myapp_web/controllers/page_controller.ex`:

```elixir
defmodule MyAppWeb.PageController do
  use MyAppWeb, :controller

  def index(conn, _params) do
    render(conn, :index)
  end

  def about(conn, _params) do
    render(conn, :about)
  end
end
```

## 📄 Templates and Views

Create `lib/myapp_web/controllers/page_html/about.html.heex`:

```heex
<h1>About Us</h1>
<p>Welcome to our Phoenix application!</p>
```

## 📊 Contexts and Schemas

Generate a context:

```bash
mix phx.gen.html Blog Post posts title:string body:text
```

This creates:

- Context: `lib/myapp/blog.ex`
- Schema: `lib/myapp/blog/post.ex`
- Controller, views, templates

Add to router:

```elixir
resources "/posts", PostController
```

Run migrations:

```bash
mix ecto.migrate
```

## ⚡ LiveView

Generate LiveView:

```bash
mix phx.gen.live Counter Count counts value:integer
```

Add to router:

```elixir
live "/counter", CountLive.Index, :index
```

## ✅ Next Steps

You now understand Phoenix essentials!

1. **Try the examples**: Build routes, controllers, and LiveViews
2. **Explore By Example**: [Elixir Phoenix By Example](/en/learn/software-engineering/platforms/web/elixir-phoenix/by-example)

## 🎯 Self-Assessment

After completing this Quick Start, you should be able to:

- [ ] Define routes and controllers
- [ ] Create templates and views
- [ ] Use contexts and schemas
- [ ] Build LiveView components
