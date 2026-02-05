---
title: "Phoenix Framework"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000016
description: "From Plug HTTP primitives to Phoenix framework with Controllers, LiveView, and bounded context patterns"
tags: ["elixir", "phoenix", "web", "plug", "liveview", "mvc"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/type-specifications"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/phoenix-channels"
---

**How do you build production web applications in Elixir?** This guide teaches the progression from Plug HTTP primitives through Phoenix framework to LiveView-first modern web applications, using bounded context organization patterns introduced in Phoenix 1.7.

## Why It Matters

Web frameworks determine development velocity, maintainability, and production capabilities. Production web applications need:

- **Routing conventions** - Map URLs to handlers with pattern matching
- **Request lifecycle** - Middleware chains, authentication, CSRF protection
- **Real-time capabilities** - WebSocket channels, server-sent events
- **LiveView interactivity** - Server-side rendering with real-time updates
- **Bounded context organization** - Clear business domain boundaries

Real-world scenarios requiring production web frameworks:

- **Donation platforms** - Campaign management, payment processing, real-time updates
- **E-commerce systems** - Product catalogs, shopping carts, order processing
- **SaaS applications** - Multi-tenant systems, user dashboards, billing
- **Content management** - Blog platforms, documentation sites, admin interfaces
- **Internal tools** - Admin dashboards, monitoring interfaces, analytics

Production question: Should you use Plug primitives, build custom framework, or adopt Phoenix? The answer depends on your routing complexity and real-time requirements.

## Standard Library - Plug HTTP Primitives

Plug provides HTTP abstractions with Plug.Conn for request/response handling and Plug.Router for basic routing.

### Plug.Conn - HTTP Connection

```elixir
# HTTP request/response abstraction
defmodule MyPlug do
  import Plug.Conn                           # => Import Conn functions
                                             # => send_resp/3, put_resp_header/3, etc.

  def init(opts), do: opts                   # => Plug initialization
                                             # => Returns options unchanged
                                             # => Type: term() -> term()

  def call(conn, _opts) do
    conn                                     # => Plug.Conn struct
                                             # => Contains request data
    |> put_resp_content_type("text/plain")   # => Set content type header
                                             # => Type: Plug.Conn.t()
    |> send_resp(200, "Hello, World!")       # => Send HTTP 200 response
                                             # => Returns updated conn
  end
end

# Start HTTP server with Plug.Cowboy
{:ok, _} = Plug.Cowboy.http(MyPlug, [])      # => Starts Cowboy HTTP server
                                             # => Listens on port 4000 by default
                                             # => Type: {:ok, pid()}
```

Plug.Conn provides HTTP abstraction, but no routing or lifecycle conventions.

### Plug.Router - Basic Routing

```elixir
# Simple router
defmodule MyRouter do
  use Plug.Router                            # => Import router DSL
                                             # => Provides get, post, match, etc.

  plug :match                                # => Match routes
  plug :dispatch                             # => Dispatch to handlers

  get "/hello" do
    send_resp(conn, 200, "Hello!")           # => Handle GET /hello
                                             # => conn: Current connection
  end                                        # => Type: Plug.Conn.t()

  post "/api/users" do
    send_resp(conn, 201, "User created")     # => Handle POST /api/users
  end

  match _ do
    send_resp(conn, 404, "Not found")        # => Catch-all route
  end
end
```

Basic routing works, but lacks nested routes, resource conventions, parameter validation.

### Complete Example - Donation API with Plug

```elixir
# Donation campaign REST API using Plug
defmodule DonationAPI do
  use Plug.Router                            # => Router DSL
  import Plug.Conn                           # => Connection functions

  plug Plug.Logger                           # => Request logging
  plug :match
  plug :dispatch

  # List campaigns
  get "/api/campaigns" do
    campaigns = [
      %{id: 1, name: "Education Fund", goal: 10000, raised: 5500},
      %{id: 2, name: "Medical Aid", goal: 15000, raised: 12000}
    ]                                        # => Hardcoded campaign data
                                             # => Type: [map()]

    json = Jason.encode!(campaigns)          # => Encode to JSON
                                             # => Type: String.t()

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, json)
  end

  # Get single campaign
  get "/api/campaigns/:id" do
    id = String.to_integer(id)               # => Path parameter from router
                                             # => Type: integer()

    campaign = %{
      id: id,
      name: "Education Fund",
      goal: 10000,
      raised: 5500
    }                                        # => Mock campaign lookup
                                             # => Type: map()

    json = Jason.encode!(campaign)

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, json)
  end

  # Create donation
  post "/api/campaigns/:id/donations" do
    {:ok, body, conn} = Plug.Conn.read_body(conn)
                                             # => Read request body
                                             # => Type: {:ok, binary(), Plug.Conn.t()}

    params = Jason.decode!(body)             # => Parse JSON body
                                             # => Type: map()

    donation = %{
      campaign_id: String.to_integer(id),
      amount: params["amount"],
      donor: params["donor"]
    }                                        # => Create donation record
                                             # => Type: map()

    # Save to database (mock)
    # Repo.insert(donation)

    json = Jason.encode!(donation)

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(201, json)                  # => HTTP 201 Created
  end

  match _ do
    send_resp(conn, 404, "Not found")
  end
end

# Start server
{:ok, _} = Plug.Cowboy.http(DonationAPI, [], port: 4000)
                                             # => Starts on port 4000
                                             # => No supervision tree
                                             # => Manual request handling
```

Works for simple APIs, but lacks validation, database integration, error handling, authentication.

## Limitations of Plug Primitives

### No Routing Conventions

Manual route definition without RESTful conventions:

```elixir
# Problem: Manual route patterns
get "/api/campaigns" do                      # => List
  # Handler code
end

get "/api/campaigns/:id" do                  # => Show
  # Handler code
end

post "/api/campaigns" do                     # => Create
  # Handler code
end

put "/api/campaigns/:id" do                  # => Update
  # Handler code
end

delete "/api/campaigns/:id" do               # => Delete
  # Handler code
end
                                             # => Repetitive CRUD patterns
                                             # => No resource helpers
                                             # => Manual parameter extraction
```

Phoenix provides `resources/4` macro for standard RESTful routes.

### No Request Lifecycle

No structured middleware chain or lifecycle hooks:

```elixir
# Problem: Manual middleware composition
defmodule MyRouter do
  use Plug.Router

  plug :authenticate                         # => Manual authentication
  plug :check_csrf                           # => Manual CSRF protection
  plug :load_user                            # => Manual user loading
  plug :match
  plug :dispatch

  # Must implement all middleware
  def authenticate(conn, _opts) do
    # Custom auth logic
  end

  def check_csrf(conn, _opts) do
    # Custom CSRF logic
  end

  def load_user(conn, _opts) do
    # Custom user loading
  end
end
                                             # => No standardized patterns
                                             # => Error-prone implementations
                                             # => Fragile ordering
```

Phoenix provides structured pipeline system with built-in plugs.

### No Real-Time Support

No built-in WebSocket or real-time capabilities:

```elixir
# Problem: Manual WebSocket handling
# Must implement WebSocket protocol manually
# No pub/sub infrastructure
# No presence tracking
# Complex state synchronization
```

Phoenix Channels provide production-ready real-time infrastructure.

### No LiveView Paradigm

No server-rendered interactivity without JavaScript:

```elixir
# Problem: Full JavaScript SPA or full page reloads
# Either write React/Vue frontend + JSON API
# Or use traditional server rendering with full page reloads
# No middle ground for simple interactivity
```

Phoenix LiveView enables real-time interactivity with minimal JavaScript.

## Production Framework - Phoenix

Phoenix provides full-featured web framework with routing, controllers, real-time channels, and LiveView.

### mix phx.new - Project Generation

```bash
# Create new Phoenix project
mix phx.new donation_platform --no-ecto     # => Generate Phoenix app
                                             # => --no-ecto: Skip database
                                             # => Creates directory structure

cd donation_platform
mix deps.get                                 # => Install dependencies
                                             # => Phoenix, Plug, Cowboy, etc.

mix phx.server                               # => Start development server
                                             # => Runs on http://localhost:4000
                                             # => Hot code reloading enabled
```

Phoenix generates complete project structure with routing, templates, assets.

### Router - RESTful Routing

```elixir
# lib/donation_platform_web/router.ex
defmodule DonationPlatformWeb.Router do
  use DonationPlatformWeb, :router          # => Import Phoenix router

  pipeline :api do
    plug :accepts, ["json"]                  # => Accept JSON only
                                             # => Type: [String.t()]
  end

  scope "/api", DonationPlatformWeb do
    pipe_through :api                        # => Apply API pipeline

    resources "/campaigns", CampaignController, only: [:index, :show, :create]
                                             # => Generate routes:
                                             # => GET    /api/campaigns
                                             # => GET    /api/campaigns/:id
                                             # => POST   /api/campaigns
                                             # => Type: routes list

    resources "/campaigns", CampaignController do
      resources "/donations", DonationController, only: [:create]
    end                                      # => Nested route:
                                             # => POST /api/campaigns/:campaign_id/donations
  end
end
```

Phoenix generates standard RESTful routes with single `resources/4` call.

### Controller - Request Handling

```elixir
# lib/donation_platform_web/controllers/campaign_controller.ex
defmodule DonationPlatformWeb.CampaignController do
  use DonationPlatformWeb, :controller       # => Import controller functions
                                             # => json/2, render/3, etc.

  # List campaigns
  def index(conn, _params) do
    campaigns = [
      %{id: 1, name: "Education Fund", goal: 10000, raised: 5500},
      %{id: 2, name: "Medical Aid", goal: 15000, raised: 12000}
    ]                                        # => Mock campaign list
                                             # => Type: [map()]

    json(conn, campaigns)                    # => Render JSON response
                                             # => Automatically sets content-type
                                             # => Type: Plug.Conn.t()
  end

  # Show single campaign
  def show(conn, %{"id" => id}) do
    campaign = %{
      id: String.to_integer(id),
      name: "Education Fund",
      goal: 10000,
      raised: 5500,
      donations: [
        %{donor: "Ahmad", amount: 1000},
        %{donor: "Fatima", amount: 2000}
      ]
    }                                        # => Mock campaign lookup
                                             # => Type: map()

    json(conn, campaign)
  end

  # Create campaign
  def create(conn, params) do
    campaign = %{
      id: :rand.uniform(1000),
      name: params["name"],
      goal: params["goal"],
      raised: 0
    }                                        # => Mock campaign creation
                                             # => Type: map()

    conn
    |> put_status(:created)                  # => HTTP 201 status
    |> json(campaign)
  end
end
```

Controller actions receive `conn` and `params`, return JSON responses.

### Phoenix 1.7 Context Pattern

Phoenix 1.7 emphasizes bounded contexts for business logic organization:

```elixir
# lib/donation_platform/campaigns.ex - Campaigns context
defmodule DonationPlatform.Campaigns do
  @moduledoc """
  Campaign management context.
  Handles campaign CRUD operations.
  """

  alias DonationPlatform.Campaigns.Campaign  # => Campaign schema
                                             # => Type: module()

  # Public API
  def list_campaigns do
    # Query logic (mock)
    [
      %Campaign{id: 1, name: "Education Fund", goal: 10000, raised: 5500},
      %Campaign{id: 2, name: "Medical Aid", goal: 15000, raised: 12000}
    ]                                        # => Type: [Campaign.t()]
  end

  def get_campaign(id) do
    # Lookup logic (mock)
    {:ok, %Campaign{id: id, name: "Education Fund", goal: 10000, raised: 5500}}
                                             # => Type: {:ok, Campaign.t()} | {:error, :not_found}
  end

  def create_campaign(attrs) do
    # Validation and creation logic
    campaign = %Campaign{
      id: :rand.uniform(1000),
      name: attrs["name"],
      goal: attrs["goal"],
      raised: 0
    }
    {:ok, campaign}                          # => Type: {:ok, Campaign.t()} | {:error, changeset}
  end
end

# lib/donation_platform/campaigns/campaign.ex - Schema
defmodule DonationPlatform.Campaigns.Campaign do
  @enforce_keys [:id, :name, :goal, :raised]
  defstruct [:id, :name, :goal, :raised]

  @type t :: %__MODULE__{
    id: integer(),
    name: String.t(),
    goal: integer(),
    raised: integer()
  }
end
```

Context modules encapsulate business logic, controllers delegate to contexts.

### Updated Controller with Context

```elixir
# lib/donation_platform_web/controllers/campaign_controller.ex
defmodule DonationPlatformWeb.CampaignController do
  use DonationPlatformWeb, :controller

  alias DonationPlatform.Campaigns           # => Import context
                                             # => Type: module()

  def index(conn, _params) do
    campaigns = Campaigns.list_campaigns()   # => Delegate to context
                                             # => Type: [Campaign.t()]
    json(conn, campaigns)
  end

  def show(conn, %{"id" => id}) do
    case Campaigns.get_campaign(id) do
      {:ok, campaign} ->
        json(conn, campaign)
      {:error, :not_found} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Campaign not found"})
    end
  end

  def create(conn, params) do
    case Campaigns.create_campaign(params) do
      {:ok, campaign} ->
        conn
        |> put_status(:created)
        |> json(campaign)
      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: changeset})
    end
  end
end
```

Controller focuses on HTTP handling, context handles business logic.

### Verified Routes (Phoenix 1.7+)

Phoenix 1.7 introduces compile-time route verification:

```elixir
# Traditional string routes (error-prone)
redirect(conn, to: "/api/campaigns/#{campaign.id}")
                                             # => String interpolation
                                             # => No compile-time checking
                                             # => Breaks silently if route changes

# Verified routes (compile-time safety)
use DonationPlatformWeb, :verified_routes   # => Import verified routes

redirect(conn, to: ~p"/api/campaigns/#{campaign.id}")
                                             # => ~p sigil for verified routes
                                             # => Compile error if route invalid
                                             # => Automatic parameter encoding
```

Verified routes catch routing errors at compile time, not runtime.

### Complete Example - Donation Platform API

```elixir
# Full Phoenix API with context pattern

# Router
defmodule DonationPlatformWeb.Router do
  use DonationPlatformWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", DonationPlatformWeb do
    pipe_through :api

    resources "/campaigns", CampaignController, only: [:index, :show, :create] do
      post "/donations", DonationController, :create
    end
  end
end

# Campaigns context
defmodule DonationPlatform.Campaigns do
  alias DonationPlatform.Campaigns.Campaign

  def list_campaigns do
    # Mock data
    [
      %Campaign{id: 1, name: "Education Fund", goal: 10000, raised: 5500},
      %Campaign{id: 2, name: "Medical Aid", goal: 15000, raised: 12000}
    ]
  end

  def get_campaign(id) when is_integer(id) do
    campaign = %Campaign{id: id, name: "Education Fund", goal: 10000, raised: 5500}
    {:ok, campaign}
  end
  def get_campaign(_), do: {:error, :not_found}

  def create_campaign(%{"name" => name, "goal" => goal}) when is_binary(name) and is_integer(goal) do
    campaign = %Campaign{
      id: :rand.uniform(1000),
      name: name,
      goal: goal,
      raised: 0
    }
    {:ok, campaign}
  end
  def create_campaign(_), do: {:error, :invalid_params}

  def add_donation(campaign_id, amount) when is_integer(campaign_id) and is_integer(amount) do
    # Update campaign raised amount
    {:ok, %{campaign_id: campaign_id, new_raised: 5500 + amount}}
  end
end

# Campaign controller
defmodule DonationPlatformWeb.CampaignController do
  use DonationPlatformWeb, :controller
  alias DonationPlatform.Campaigns

  def index(conn, _params) do
    campaigns = Campaigns.list_campaigns()
    json(conn, campaigns)
  end

  def show(conn, %{"id" => id}) do
    case Campaigns.get_campaign(String.to_integer(id)) do
      {:ok, campaign} ->
        json(conn, campaign)
      {:error, :not_found} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Campaign not found"})
    end
  end

  def create(conn, params) do
    case Campaigns.create_campaign(params) do
      {:ok, campaign} ->
        conn
        |> put_status(:created)
        |> json(campaign)
      {:error, :invalid_params} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: "Invalid parameters"})
    end
  end
end

# Donation controller
defmodule DonationPlatformWeb.DonationController do
  use DonationPlatformWeb, :controller
  alias DonationPlatform.Campaigns

  def create(conn, %{"campaign_id" => campaign_id, "amount" => amount}) do
    case Campaigns.add_donation(String.to_integer(campaign_id), amount) do
      {:ok, result} ->
        conn
        |> put_status(:created)
        |> json(result)
      {:error, reason} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: reason})
    end
  end
end

# Start server: mix phx.server
# GET    /api/campaigns              # => List all campaigns
# GET    /api/campaigns/1            # => Show campaign 1
# POST   /api/campaigns              # => Create campaign
# POST   /api/campaigns/1/donations  # => Add donation to campaign 1
```

Full REST API with routing, controllers, contexts, and verified routes.

## Trade-offs

| Approach          | Complexity | Features   | Learning Curve | Use Case                    |
| ----------------- | ---------- | ---------- | -------------- | --------------------------- |
| Plug primitives   | Low        | Basic HTTP | Low            | Simple APIs, microservices  |
| Custom framework  | High       | Custom     | High           | Specialized requirements    |
| Phoenix framework | Medium     | Full-stack | Medium         | Production web applications |

**Plug primitives**: Minimal abstraction, maximum control, no conventions.

**Custom framework**: Build exactly what you need, but high maintenance cost.

**Phoenix framework**: Batteries-included, established patterns, vibrant ecosystem.

## Best Practices

### Use Context Boundaries

Organize business logic into bounded contexts:

```elixir
# Good: Clear context boundaries
DonationPlatform.Campaigns             # => Campaign management
DonationPlatform.Payments              # => Payment processing
DonationPlatform.Notifications         # => Email/SMS notifications
DonationPlatform.Accounts              # => User accounts

# Bad: No context separation
DonationPlatform.get_campaign()        # => Mixed responsibilities
DonationPlatform.create_payment()      # => No clear boundaries
DonationPlatform.send_email()
```

Contexts prevent tight coupling, enable independent evolution.

### Keep Controllers Thin

Controllers handle HTTP, contexts handle business logic:

```elixir
# Good: Thin controller
def create(conn, params) do
  case Campaigns.create_campaign(params) do  # => Delegate to context
    {:ok, campaign} ->
      conn
      |> put_status(:created)
      |> json(campaign)
    {:error, changeset} ->
      conn
      |> put_status(:unprocessable_entity)
      |> json(%{errors: changeset})
  end
end

# Bad: Fat controller
def create(conn, params) do
  # Validation logic
  # Database queries
  # Business rules
  # Error handling
  # All mixed in controller
end
```

Thin controllers enable testing business logic without HTTP.

### Use Verified Routes

Phoenix 1.7+ verified routes catch errors at compile time:

```elixir
# Good: Verified routes
use DonationPlatformWeb, :verified_routes

redirect(conn, to: ~p"/campaigns/#{campaign.id}")
                                             # => Compile-time verification
                                             # => Automatic encoding

# Bad: String interpolation
redirect(conn, to: "/campaigns/#{campaign.id}")
                                             # => Runtime errors
                                             # => Manual encoding
```

Verified routes prevent routing bugs in production.

### Structure Pipelines Clearly

Organize pipelines by authentication requirements:

```elixir
# Router with clear pipelines
pipeline :api do
  plug :accepts, ["json"]
end

pipeline :api_authenticated do
  plug :accepts, ["json"]
  plug :authenticate_api_token
end

scope "/api", DonationPlatformWeb do
  pipe_through :api

  get "/campaigns", CampaignController, :index  # => Public
  get "/campaigns/:id", CampaignController, :show
end

scope "/api", DonationPlatformWeb do
  pipe_through :api_authenticated

  post "/campaigns", CampaignController, :create  # => Authenticated
  post "/campaigns/:id/donations", DonationController, :create
end
```

Clear pipeline boundaries improve security and maintainability.

### Follow Phoenix 1.7 Conventions

Phoenix 1.7 emphasizes contexts and verified routes:

```
lib/
├── donation_platform/                 # Core application
│   ├── campaigns/                     # Campaigns context
│   │   ├── campaign.ex                # Schema
│   │   └── donation.ex
│   ├── campaigns.ex                   # Context API
│   └── application.ex
└── donation_platform_web/             # Web interface
    ├── controllers/
    │   ├── campaign_controller.ex
    │   └── donation_controller.ex
    └── router.ex
```

Separate core domain (lib/donation_platform) from web interface (lib/donation_platform_web).

## References

**Phoenix Documentation**:

- [Phoenix Framework](https://hexdocs.pm/phoenix) - Official documentation
- [Phoenix Guides](https://hexdocs.pm/phoenix/overview.html) - Getting started guides
- [Contexts Guide](https://hexdocs.pm/phoenix/contexts.html) - Bounded context patterns

**Plug Documentation**:

- [Plug](https://hexdocs.pm/plug) - Plug specification
- [Plug.Conn](https://hexdocs.pm/plug/Plug.Conn.html) - Connection struct
- [Plug.Router](https://hexdocs.pm/plug/Plug.Router.html) - Router DSL

**Phoenix 1.7**:

- [Phoenix 1.7 Release](https://www.phoenixframework.org/blog/phoenix-1.7-final-released) - New conventions
- [Verified Routes](https://hexdocs.pm/phoenix/Phoenix.VerifiedRoutes.html) - Compile-time route verification
