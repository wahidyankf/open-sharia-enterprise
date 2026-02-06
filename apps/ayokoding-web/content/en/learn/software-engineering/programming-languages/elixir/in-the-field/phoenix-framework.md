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

  def init(opts), do: opts                   # => Plug initialization callback
                                             # => Called once at compile time
                                             # => opts: Options passed to plug
                                             # => Returns options unchanged
                                             # => Stored and passed to call/2
                                             # => Type: term() -> term()

  def call(conn, _opts) do                   # => Request handling callback
                                             # => Called for every HTTP request
                                             # => conn: Connection struct with request data
                                             # => _opts: Options from init/1 (unused)
    conn                                     # => Plug.Conn struct
                                             # => Contains: method, path, headers, params
                                             # => Mutable through pipeline transformations
    |> put_resp_content_type("text/plain")   # => Set Content-Type response header
                                             # => "text/plain": MIME type for plain text
                                             # => Updates conn.resp_headers
                                             # => Type: Plug.Conn.t()
    |> send_resp(200, "Hello, World!")       # => Send HTTP response to client
                                             # => 200: HTTP OK status code
                                             # => "Hello, World!": Response body
                                             # => Marks conn as sent
                                             # => Returns updated conn
                                             # => Type: Plug.Conn.t()
  end
end

# Start HTTP server with Plug.Cowboy
{:ok, _} = Plug.Cowboy.http(MyPlug, [])      # => Starts Cowboy HTTP server
                                             # => MyPlug: Module implementing Plug behavior
                                             # => []: Empty options list (use defaults)
                                             # => Listens on port 4000 by default
                                             # => Spawns supervised child processes
                                             # => Returns process ID of server
                                             # => Type: {:ok, pid()}
```

Plug.Conn provides HTTP abstraction, but no routing or lifecycle conventions.

### Plug.Router - Basic Routing

```elixir
# Simple router
defmodule MyRouter do
  use Plug.Router                            # => Import router DSL
                                             # => Provides get, post, match, etc.
                                             # => Compiles route matching logic

  plug :match                                # => Match incoming request to route
                                             # => Examines conn.method and conn.path_info
                                             # => Sets conn.private.plug_route if matched
                                             # => First plug in pipeline
                                             # => Must run before :dispatch
  plug :dispatch                             # => Execute matched route handler
                                             # => Calls function for matched route
                                             # => Halts if no match found
                                             # => Second plug in pipeline
                                             # => Requires :match to run first

  get "/hello" do                            # => Define GET route handler
                                             # => Matches: GET requests to /hello
                                             # => do block: Handler implementation
    send_resp(conn, 200, "Hello!")           # => Send response to client
                                             # => conn: Current connection struct
                                             # => 200: HTTP OK status code
                                             # => "Hello!": Response body text
                                             # => Returns updated conn
                                             # => Type: Plug.Conn.t()
  end

  post "/api/users" do                       # => Define POST route handler
                                             # => Matches: POST requests to /api/users
                                             # => Typically for resource creation
    send_resp(conn, 201, "User created")     # => Send creation response
                                             # => 201: HTTP Created status
                                             # => Indicates new resource created
                                             # => Plain text response body
                                             # => Type: Plug.Conn.t()
  end

  match _ do                                 # => Catch-all route handler
                                             # => _ : Matches any method, any path
                                             # => Runs if no other route matched
    send_resp(conn, 404, "Not found")        # => Send not found response
                                             # => 404: HTTP Not Found status
                                             # => Standard error for missing resources
                                             # => Type: Plug.Conn.t()
  end
end
```

Basic routing works, but lacks nested routes, resource conventions, parameter validation.

### Complete Example - Donation API with Plug

```elixir
# Donation campaign REST API using Plug
defmodule DonationAPI do
  use Plug.Router                            # => Import Router DSL
                                             # => Provides get, post, match macros
  import Plug.Conn                           # => Import Connection functions
                                             # => send_resp, put_resp_content_type, etc.

  plug Plug.Logger                           # => Log all requests
                                             # => Shows method, path, status, duration
  plug :match                                # => Match route to handler
  plug :dispatch                             # => Execute matched handler

  # List campaigns
  get "/api/campaigns" do
                                             # => Matches: GET /api/campaigns
                                             # => conn: Current connection
    campaigns = [
      %{id: 1, name: "Education Fund", goal: 10000, raised: 5500},
                                             # => First campaign map
                                             # => Fields: id, name, goal, raised
      %{id: 2, name: "Medical Aid", goal: 15000, raised: 12000}
                                             # => Second campaign map
    ]                                        # => Hardcoded campaign data
                                             # => Type: [map()]

    json = Jason.encode!(campaigns)          # => Encode list to JSON string
                                             # => Jason: JSON library
                                             # => Type: String.t()

    conn
    |> put_resp_content_type("application/json")
                                             # => Set Content-Type header
                                             # => Client knows response is JSON
    |> send_resp(200, json)                  # => Send HTTP 200 with JSON body
                                             # => Returns: Plug.Conn.t()
  end

  # Get single campaign
  get "/api/campaigns/:id" do
                                             # => Matches: GET /api/campaigns/123
                                             # => :id captured as "id" variable
    id = String.to_integer(id)               # => Path parameter from router
                                             # => Convert "123" string to 123 integer
                                             # => Type: integer()

    campaign = %{
      id: id,                                # => Use converted integer ID
      name: "Education Fund",                # => Hardcoded campaign name
      goal: 10000,                           # => Fundraising goal in currency units
      raised: 5500                           # => Amount raised so far
    }                                        # => Mock campaign lookup
                                             # => Production: Repo.get(Campaign, id)
                                             # => Type: map()

    json = Jason.encode!(campaign)           # => Encode campaign map to JSON
                                             # => Type: String.t()

    conn
    |> put_resp_content_type("application/json")
                                             # => Set JSON content type
    |> send_resp(200, json)                  # => Send HTTP 200 OK with campaign
  end

  # Create donation
  post "/api/campaigns/:id/donations" do
                                             # => Matches: POST /api/campaigns/1/donations
                                             # => id: Campaign ID from URL path
    {:ok, body, conn} = Plug.Conn.read_body(conn)
                                             # => Read raw request body
                                             # => body: Binary JSON data
                                             # => conn: Updated connection
                                             # => Type: {:ok, binary(), Plug.Conn.t()}

    params = Jason.decode!(body)             # => Parse JSON body to Elixir map
                                             # => Expects {"amount": 100, "donor": "Alice"}
                                             # => Type: map()

    donation = %{
      campaign_id: String.to_integer(id),    # => Convert campaign ID to integer
                                             # => Type: integer()
      amount: params["amount"],              # => Extract donation amount
                                             # => Type: integer() or float()
      donor: params["donor"]                 # => Extract donor name
                                             # => Type: String.t()
    }                                        # => Create donation record
                                             # => Type: map()

    # Save to database (mock)
    # Repo.insert(donation)                 # => Production: Save to database
                                             # => Returns {:ok, donation} or {:error, changeset}

    json = Jason.encode!(donation)           # => Encode donation to JSON
                                             # => Type: String.t()

    conn
    |> put_resp_content_type("application/json")
                                             # => Set JSON content type
    |> send_resp(201, json)                  # => HTTP 201 Created
                                             # => 201: Resource created successfully
  end

  match _ do
                                             # => Catch-all for unmatched routes
                                             # => Handles any method, any path
    send_resp(conn, 404, "Not found")        # => HTTP 404 Not Found
                                             # => Plain text response
  end
end

# Start server
{:ok, _} = Plug.Cowboy.http(DonationAPI, [], port: 4000)
                                             # => Start Cowboy HTTP server
                                             # => Module: DonationAPI
                                             # => Options: [] (empty list)
                                             # => Port: 4000 (localhost:4000)
                                             # => Returns: {:ok, pid}
                                             # => No supervision tree
                                             # => Manual request handling
```

Works for simple APIs, but lacks validation, database integration, error handling, authentication.

## Limitations of Plug Primitives

### No Routing Conventions

Manual route definition without RESTful conventions:

```elixir
# Problem: Manual route patterns
get "/api/campaigns" do                      # => List all campaigns (index action)
                                             # => Must manually define route
                                             # => No automatic route helper function
                                             # => No convention-based naming
  # Handler code
end

get "/api/campaigns/:id" do                  # => Show single campaign (show action)
                                             # => :id is path parameter
                                             # => Must manually extract and validate
                                             # => Must manually convert string to integer
                                             # => No automatic 404 if not found
  # Handler code
end

post "/api/campaigns" do                     # => Create new campaign (create action)
                                             # => Must manually parse request body JSON
                                             # => Must manually read_body from conn
                                             # => No automatic validation or changesets
                                             # => No automatic error handling
  # Handler code
end

put "/api/campaigns/:id" do                  # => Update existing campaign (update action)
                                             # => Must manually handle both :id and body
                                             # => Must manually merge params
                                             # => No partial update support
                                             # => No automatic conflict detection
  # Handler code
end

delete "/api/campaigns/:id" do               # => Delete campaign (delete action)
                                             # => Must manually verify :id exists
                                             # => Must manually check foreign key constraints
                                             # => No cascade delete handling
                                             # => No soft delete support
  # Handler code
end
                                             # => Repetitive CRUD patterns for every resource
                                             # => Every resource needs 5 manual routes
                                             # => No resource helpers or path conventions
                                             # => Manual parameter extraction every time
                                             # => No automatic route naming (campaigns_path, etc.)
                                             # => No nested resource support
```

Phoenix provides `resources/4` macro for standard RESTful routes.

### No Request Lifecycle

No structured middleware chain or lifecycle hooks:

```elixir
# Problem: Manual middleware composition
defmodule MyRouter do
  use Plug.Router

  plug :authenticate                         # => Manual authentication plug
                                             # => Must implement from scratch
  plug :check_csrf                           # => Manual CSRF protection
                                             # => Must handle tokens, validation
  plug :load_user                            # => Manual user loading
                                             # => Must query database, handle sessions
  plug :match                                # => Route matching
  plug :dispatch                             # => Handler dispatch

  # Must implement all middleware functions
  def authenticate(conn, _opts) do
    # Custom auth logic
                                             # => Check session or JWT token
                                             # => Verify credentials
                                             # => Handle unauthorized access
                                             # => Must return conn or halt pipeline
  end

  def check_csrf(conn, _opts) do
    # Custom CSRF logic
                                             # => Validate CSRF token from request
                                             # => Compare with session token
                                             # => Reject if mismatch
                                             # => Must handle GET vs POST differently
  end

  def load_user(conn, _opts) do
    # Custom user loading
                                             # => Extract user ID from session
                                             # => Query database for user record
                                             # => Assign to conn.assigns
                                             # => Handle user not found
  end
end
                                             # => No standardized patterns or best practices
                                             # => Error-prone implementations (security bugs)
                                             # => Fragile ordering (must run in correct sequence)
                                             # => No testing helpers for plug pipelines
```

Phoenix provides structured pipeline system with built-in plugs.

### No Real-Time Support

No built-in WebSocket or real-time capabilities:

```elixir
# Problem: Manual WebSocket handling
# Must implement WebSocket protocol manually
                                             # => Write handshake logic from scratch
                                             # => Handle frame parsing manually
                                             # => Manage connection lifecycle
                                             # => No automatic reconnection

# No pub/sub infrastructure
                                             # => Must build message broadcasting system
                                             # => Implement topic subscriptions manually
                                             # => Handle race conditions in message delivery
                                             # => No distributed pub/sub across nodes

# No presence tracking
                                             # => Cannot track "who's online" efficiently
                                             # => Must implement custom presence logic
                                             # => Handle network partitions manually
                                             # => No conflict resolution for presence

# Complex state synchronization
                                             # => Client and server state drift easily
                                             # => Must manually handle reconnection state
                                             # => No automatic state reconciliation
                                             # => Race conditions in concurrent updates
```

Phoenix Channels provide production-ready real-time infrastructure.

### No LiveView Paradigm

No server-rendered interactivity without JavaScript:

```elixir
# Problem: Full JavaScript SPA or full page reloads
# Either write React/Vue frontend + JSON API
                                             # => Requires separate frontend codebase
                                             # => Duplicate validation logic
                                             # => Complex build pipeline
                                             # => API versioning challenges

# Or use traditional server rendering with full page reloads
                                             # => Every interaction reloads entire page
                                             # => No real-time updates
                                             # => Poor user experience
                                             # => High bandwidth usage

# No middle ground for simple interactivity
                                             # => Cannot easily add real-time features
                                             # => Simple updates require full SPA
                                             # => Or accept page reload UX penalty
                                             # => No server-side state management for UI
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
  use DonationPlatformWeb, :router          # => Import Phoenix router macros
                                             # => Provides pipeline, scope, resources
                                             # => Sets up routing DSL

  pipeline :api do
    plug :accepts, ["json"]                  # => Accept JSON only
                                             # => Rejects non-JSON requests with 406
                                             # => Type: [String.t()]
  end

  scope "/api", DonationPlatformWeb do
    pipe_through :api                        # => Apply API pipeline to all routes
                                             # => Runs :accepts plug

    resources "/campaigns", CampaignController, only: [:index, :show, :create]
                                             # => Generate standard RESTful routes:
                                             # => GET    /api/campaigns (index)
                                             # => GET    /api/campaigns/:id (show)
                                             # => POST   /api/campaigns (create)
                                             # => only: [:index, :show, :create] - Limit actions
                                             # => Type: routes list
                                             # => Generates route helpers automatically

    resources "/campaigns", CampaignController do
      resources "/donations", DonationController, only: [:create]
    end                                      # => Nested resource route:
                                             # => POST /api/campaigns/:campaign_id/donations
                                             # => :campaign_id available in params
                                             # => Represents parent resource
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
                                             # => Pattern match params map
                                             # => id: String from URL path
    campaign = %{
      id: String.to_integer(id),             # => Convert string ID to integer
                                             # => Type: integer()
      name: "Education Fund",                # => Campaign name
      goal: 10000,                           # => Fundraising goal
      raised: 5500,                          # => Amount raised so far
      donations: [                           # => List of donations
        %{donor: "Ahmad", amount: 1000},     # => First donation
        %{donor: "Fatima", amount: 2000}     # => Second donation
      ]
    }                                        # => Mock campaign lookup
                                             # => Production: Campaigns.get_campaign(id)
                                             # => Type: map()

    json(conn, campaign)                     # => Render JSON response
                                             # => Sets Content-Type: application/json
  end

  # Create campaign
  def create(conn, params) do
                                             # => params: Request parameters map
                                             # => Contains name, goal from body
    campaign = %{
      id: :rand.uniform(1000),               # => Generate random ID
                                             # => Production: Use database auto-increment
      name: params["name"],                  # => Extract name from params
                                             # => Type: String.t()
      goal: params["goal"],                  # => Extract goal from params
                                             # => Type: integer()
      raised: 0                              # => Initialize raised to zero
    }                                        # => Mock campaign creation
                                             # => Production: Campaigns.create_campaign(params)
                                             # => Type: map()

    conn
    |> put_status(:created)                  # => HTTP 201 Created status
                                             # => Indicates new resource created
    |> json(campaign)                        # => Render created campaign as JSON
                                             # => Returns Plug.Conn.t()
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

  def create_campaign(attrs) do              # => Create new campaign
                                             # => attrs: Map with campaign attributes
                                             # => Type: map() -> result tuple
    # Validation and creation logic         # => Production: Would use Ecto changeset
                                             # => Validate name, goal before insert
    campaign = %Campaign{                    # => Create Campaign struct
                                             # => All fields required (@enforce_keys)
      id: :rand.uniform(1000),               # => Generate random ID (mock)
                                             # => Production: Database auto-increment
                                             # => Type: integer()
      name: attrs["name"],                   # => Extract name from attributes
                                             # => Type: String.t()
      goal: attrs["goal"],                   # => Extract fundraising goal
                                             # => Type: integer()
      raised: 0                              # => Initialize raised amount to zero
                                             # => New campaigns start with 0 donations
    }                                        # => campaign: Complete Campaign struct
                                             # => Type: Campaign.t()
    {:ok, campaign}                          # => Return success tuple
                                             # => Production: Save to database first
                                             # => Type: {:ok, Campaign.t()} | {:error, changeset}
  end
end

# lib/donation_platform/campaigns/campaign.ex - Schema
defmodule DonationPlatform.Campaigns.Campaign do
                                             # => Campaign schema definition
                                             # => Represents campaign data structure
  @enforce_keys [:id, :name, :goal, :raised] # => Compiler-enforced required keys
                                             # => Compilation error if any missing
                                             # => Must provide all 4 when creating struct
  defstruct [:id, :name, :goal, :raised]    # => Define struct with 4 fields
                                             # => Creates %Campaign{} type
                                             # => All fields default to nil if not set

  @type t :: %__MODULE__{                    # => Type specification for Campaign struct
                                             # => __MODULE__: Current module (Campaign)
                                             # => Dialyzer uses this for type checking
    id: integer(),                           # => id field type: integer
    name: String.t(),                        # => name field type: String
    goal: integer(),                         # => goal field type: integer (fundraising target)
    raised: integer()                        # => raised field type: integer (amount collected)
  }                                          # => Type: Campaign.t()
                                             # => Used in function specs
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

  def index(conn, _params) do                # => List all campaigns (GET /api/campaigns)
                                             # => conn: Connection struct
                                             # => _params: Query params (unused)
    campaigns = Campaigns.list_campaigns()   # => Delegate to context module
                                             # => Business logic in context layer
                                             # => Type: [Campaign.t()]
    json(conn, campaigns)                    # => Render campaigns as JSON
                                             # => Sets Content-Type: application/json
                                             # => HTTP 200 OK (default)
                                             # => Type: Plug.Conn.t()
  end

  def show(conn, %{"id" => id}) do           # => Show single campaign (GET /api/campaigns/:id)
                                             # => conn: Connection struct
                                             # => Pattern match extracts "id" from params
    case Campaigns.get_campaign(id) do       # => Attempt to fetch campaign
                                             # => id: String from URL path
                                             # => Returns: {:ok, campaign} | {:error, reason}
      {:ok, campaign} ->                     # => Success branch: Campaign found
                                             # => campaign: Campaign.t() struct
        json(conn, campaign)                 # => Render campaign as JSON
                                             # => HTTP 200 OK
      {:error, :not_found} ->                # => Error branch: Campaign not found
                                             # => :not_found: Error reason atom
        conn
        |> put_status(:not_found)            # => Set HTTP 404 Not Found status
                                             # => Updates conn.status
        |> json(%{error: "Campaign not found"})
                                             # => Render error message as JSON
                                             # => %{error: ...}: Error map structure
    end
  end

  def create(conn, params) do                # => Create campaign (POST /api/campaigns)
                                             # => conn: Connection struct
                                             # => params: Request body parsed as map
    case Campaigns.create_campaign(params) do # => Delegate creation to context
                                             # => Context handles validation
                                             # => Returns: {:ok, campaign} | {:error, changeset}
      {:ok, campaign} ->                     # => Success branch: Campaign created
                                             # => campaign: Newly created Campaign.t()
        conn
        |> put_status(:created)              # => Set HTTP 201 Created status
                                             # => Indicates new resource created
        |> json(campaign)                    # => Render created campaign as JSON
                                             # => Returns campaign to client
      {:error, changeset} ->                 # => Error branch: Validation failed
                                             # => changeset: Ecto.Changeset with errors
        conn
        |> put_status(:unprocessable_entity) # => Set HTTP 422 Unprocessable Entity
                                             # => Indicates invalid input data
        |> json(%{errors: changeset})        # => Render validation errors as JSON
                                             # => Client can display errors to user
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
# Demonstrates router, contexts, controllers working together

# Router
defmodule DonationPlatformWeb.Router do
  use DonationPlatformWeb, :router          # => Import Phoenix router macros
                                            # => Provides pipeline, scope, resources

  pipeline :api do
    plug :accepts, ["json"]                 # => Accept only JSON content-type
                                            # => Rejects HTML, XML, etc.
  end

  scope "/api", DonationPlatformWeb do
    pipe_through :api                       # => Apply API pipeline to routes
                                            # => All routes get JSON filtering

    resources "/campaigns", CampaignController, only: [:index, :show, :create] do
                                            # => Generates 3 RESTful routes
                                            # => index: GET /api/campaigns
                                            # => show: GET /api/campaigns/:id
                                            # => create: POST /api/campaigns
      post "/donations", DonationController, :create
                                            # => Nested route inside campaigns resource
                                            # => POST /api/campaigns/:campaign_id/donations
    end
  end
end

# Campaigns context
defmodule DonationPlatform.Campaigns do
  alias DonationPlatform.Campaigns.Campaign # => Import Campaign schema
                                            # => Type: module()

  def list_campaigns do
    # Mock data
    [
      %Campaign{id: 1, name: "Education Fund", goal: 10000, raised: 5500},
                                            # => Campaign struct with 4 fields
                                            # => Type: Campaign.t()
      %Campaign{id: 2, name: "Medical Aid", goal: 15000, raised: 12000}
                                            # => Second campaign
    ]                                       # => Returns list of campaigns
                                            # => Type: [Campaign.t()]
  end

  def get_campaign(id) when is_integer(id) do
                                            # => Guard: id must be integer
                                            # => Prevents invalid lookups
    campaign = %Campaign{id: id, name: "Education Fund", goal: 10000, raised: 5500}
                                            # => Mock campaign lookup
                                            # => Production: Repo.get(Campaign, id)
    {:ok, campaign}                         # => Success tuple
                                            # => Type: {:ok, Campaign.t()}
  end
  def get_campaign(_), do: {:error, :not_found}
                                            # => Catch-all for non-integer ids
                                            # => Type: {:error, :not_found}

  def create_campaign(%{"name" => name, "goal" => goal}) when is_binary(name) and is_integer(goal) do
                                            # => Pattern match + guards validate input
                                            # => name must be string, goal must be integer
    campaign = %Campaign{
      id: :rand.uniform(1000),              # => Generate random ID
                                            # => Production: Auto-incremented by DB
      name: name,                           # => Use validated name
      goal: goal,                           # => Use validated goal
      raised: 0                             # => New campaigns start at 0 raised
    }                                       # => Type: Campaign.t()
    {:ok, campaign}                         # => Success tuple
                                            # => Type: {:ok, Campaign.t()}
  end
  def create_campaign(_), do: {:error, :invalid_params}
                                            # => Catch-all for invalid params
                                            # => Type: {:error, :invalid_params}

  def add_donation(campaign_id, amount) when is_integer(campaign_id) and is_integer(amount) do
                                            # => Guards validate both params are integers
    # Update campaign raised amount
    {:ok, %{campaign_id: campaign_id, new_raised: 5500 + amount}}
                                            # => Mock calculation
                                            # => Production: Update DB and return new balance
                                            # => Type: {:ok, map()}
  end
end

# Campaign controller
defmodule DonationPlatformWeb.CampaignController do
  use DonationPlatformWeb, :controller     # => Import controller functions
                                            # => json/2, put_status/2, etc.
  alias DonationPlatform.Campaigns          # => Import Campaigns context
                                            # => Type: module()

  def index(conn, _params) do
                                            # => conn: Connection struct
                                            # => _params: Unused query params
    campaigns = Campaigns.list_campaigns()  # => Delegate to context
                                            # => Type: [Campaign.t()]
    json(conn, campaigns)                   # => Render JSON response
                                            # => Automatically sets Content-Type
                                            # => Returns: Plug.Conn.t()
  end

  def show(conn, %{"id" => id}) do
                                            # => Pattern match extracts id from params
                                            # => id: String from URL path
    case Campaigns.get_campaign(String.to_integer(id)) do
                                            # => Convert string ID to integer
                                            # => Pass to context function
      {:ok, campaign} ->
                                            # => Success case: campaign found
        json(conn, campaign)                # => Render campaign as JSON
                                            # => HTTP 200 OK
      {:error, :not_found} ->
                                            # => Error case: no matching campaign
        conn
        |> put_status(:not_found)           # => Set HTTP 404 status
        |> json(%{error: "Campaign not found"})
                                            # => Return error message as JSON
    end
  end

  def create(conn, params) do
                                            # => params: Request body parsed as map
                                            # => Contains "name", "goal" keys
    case Campaigns.create_campaign(params) do
                                            # => Delegate creation to context
                                            # => Context validates params
      {:ok, campaign} ->
                                            # => Success: campaign created
        conn
        |> put_status(:created)             # => Set HTTP 201 Created
        |> json(campaign)                   # => Return created campaign as JSON
      {:error, :invalid_params} ->
                                            # => Error: validation failed
        conn
        |> put_status(:unprocessable_entity)
                                            # => Set HTTP 422 Unprocessable Entity
        |> json(%{error: "Invalid parameters"})
                                            # => Return error message
    end
  end
end

# Donation controller
defmodule DonationPlatformWeb.DonationController do
  use DonationPlatformWeb, :controller     # => Import controller functions
  alias DonationPlatform.Campaigns          # => Import Campaigns context

  def create(conn, %{"campaign_id" => campaign_id, "amount" => amount}) do
                                            # => Pattern match extracts campaign_id, amount
                                            # => campaign_id: String from URL
                                            # => amount: Integer from request body
    case Campaigns.add_donation(String.to_integer(campaign_id), amount) do
                                            # => Convert campaign_id to integer
                                            # => Pass both to context
      {:ok, result} ->
                                            # => Success: donation added
        conn
        |> put_status(:created)             # => HTTP 201 Created
        |> json(result)                     # => Return result with new balance
      {:error, reason} ->
                                            # => Error: donation failed
                                            # => reason: Error atom from context
        conn
        |> put_status(:unprocessable_entity)
                                            # => HTTP 422
        |> json(%{error: reason})           # => Return error reason
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
