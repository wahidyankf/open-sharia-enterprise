---
title: "Basics"
date: 2025-03-25T07:17:00+07:00
draft: false
weight: 703
---

This crash course covers the core 85% of Phoenix Framework that you'll use daily, giving you enough foundation to explore the remaining 15% on your own.

## Introduction to Phoenix Framework

Phoenix is a web framework written in Elixir that implements the server-side MVC (Model-View-Controller) pattern. It's built to handle high concurrency, provide fault tolerance, and enable real-time communication.

**Key Benefits:**

- Exceptional performance (millions of WebSocket connections on a single server)
- High developer productivity with concise, maintainable code
- Built-in developer tools (formatter, linter, test framework)
- Real-time capabilities through LiveView without writing JavaScript
- Powerful data handling with Ecto (the database wrapper)

```mermaid
graph TD
    A[Elixir Language] --> B[Phoenix Framework]
    B --> C[Controllers]
    B --> D[Views/Templates]
    B --> E[LiveView]
    B --> F[Contexts]
    B --> G[Ecto ORM]
    B --> H[Plugs]
    B --> I[Channels]
```

## Prerequisites and Installation

Before installing Phoenix, you need:

- Elixir and Erlang installed
- A PostgreSQL database (default, though other databases are supported)
- Node.js (for asset compilation)

### Installation Steps

1. Install Hex (Elixir's package manager):

```bash
mix local.hex
```

2. Install the Phoenix application generator:

```bash
mix archive.install hex phx_new
```

3. Create a new Phoenix project:

```bash
mix phx.new my_app
# Common flags:
# --no-ecto (skip database)
# --no-html (API-only)
# --live (include LiveView)
```

4. Set up the database:

```bash
# Configure database in config/dev.exs
mix ecto.create
```

5. Start the server:

```bash
cd my_app
mix phx.server
# Visit http://localhost:4000
```

## Project Structure

Phoenix follows a well-defined project structure that separates business logic from the web interface:

```
my_app/
├── lib/
│   ├── my_app/           # Business Logic
│   └── my_app_web/       # Web Interface
├── assets/               # CSS, JS, etc.
├── config/               # Configuration
├── priv/                 # Assets and DB
└── test/                 # Test Files
```

**Key Directories and Files:**

- `lib/my_app/`: Business logic and contexts
- `lib/my_app_web/`: Web interface (controllers, views, templates)
- `lib/my_app_web/router.ex`: Defines routes
- `lib/my_app_web/endpoint.ex`: Entry point for HTTP requests
- `assets/`: Frontend assets (CSS, JavaScript)
- `config/`: Environment-specific configuration
- `priv/`: Database migrations and static assets
- `test/`: Test files

## The MVC Pattern in Phoenix

Phoenix implements the Model-View-Controller pattern with some Elixir-specific adaptations:

```mermaid
graph TD
    A[Browser/Client] -->|Request| B[Router]
    B -->|Routes to| C[Controller]
    C -->|Gets data from| D[Context/Model]
    D -->|Returns data to| C
    C -->|Renders| E[View/Template]
    E -->|Response| A
```

### Controllers

Controllers handle incoming HTTP requests, interact with business logic, and prepare data for views:

```elixir
defmodule MyAppWeb.ProductController do
  use MyAppWeb, :controller

  alias MyApp.Store  # Context module

  def index(conn, _params) do
    # Get data from context
    products = Store.list_products()
    # Render view with data
    render(conn, :index, products: products)
  end

  def show(conn, %{"id" => id}) do
    # Pattern matching on params
    product = Store.get_product!(id)
    render(conn, :show, product: product)
  end

  def new(conn, _params) do
    # Create empty changeset for form
    changeset = Store.change_product(%Store.Product{})
    render(conn, :new, changeset: changeset)
  end

  def create(conn, %{"product" => product_params}) do
    case Store.create_product(product_params) do
      {:ok, product} ->
        conn
        |> put_flash(:info, "Product created successfully.")
        |> redirect(to: ~p"/products/#{product}")

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, :new, changeset: changeset)
    end
  end
end
```

### Views and Templates

Modern Phoenix uses HTML modules for views. Templates are embedded Elixir (HEEx) files that define the HTML structure:

```elixir
# View module (lib/my_app_web/controllers/product_html.ex)
defmodule MyAppWeb.ProductHTML do
  use MyAppWeb, :html

  # Automatically embeds templates from this directory
  embed_templates "product_html/*"

  # Helper function available in templates
  def product_price(product) do
    "$#{Decimal.to_string(product.price)}"
  end
end
```

```heex
# Template (lib/my_app_web/controllers/product_html/index.html.heex)
<h1>Products</h1>

<div class="products-list">
  <%= for product <- @products do %>
    <div class="product-card">
      <h3><%= product.name %></h3>
      <p><%= product_price(product) %></p>
      <.link href={~p"/products/#{product}"}>View Details</.link>
    </div>
  <% end %>
</div>
```

## Routing in Phoenix

Routes map HTTP requests to controller actions. Phoenix's router provides a powerful DSL for defining routes:

```elixir
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  # Define pipelines (middleware groups)
  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {MyAppWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  # Browser routes
  scope "/", MyAppWeb do
    pipe_through :browser

    get "/", PageController, :index
    # RESTful resources (creates 7 standard routes)
    resources "/products", ProductController
    # LiveView route
    live "/dashboard", DashboardLive
  end

  # API routes
  scope "/api", MyAppWeb do
    pipe_through :api

    resources "/products", ProductAPIController, except: [:new, :edit]
  end
end
```

Common route types:

- Simple routes: `get "/about", PageController, :about`
- Resource routes: `resources "/users", UserController`
- Live routes: `live "/dashboard", DashboardLive`
- Nested resources: `resources "/users", UserController do resources "/posts", PostController end`

## Contexts: Organizing Business Logic

Contexts are modules that encapsulate and group related functionality, separating business logic from the web layer:

```mermaid
graph TD
    A[Web Layer] -->|Uses| B[Contexts]
    B -->|Manages| C[Schemas]
    B -->|Talks to| D[External Services]
    B -->|Contains| E[Business Logic]
```

Example context:

```elixir
defmodule MyApp.Store do
  import Ecto.Query

  alias MyApp.Repo
  alias MyApp.Store.Product

  # List all products
  def list_products do
    Repo.all(Product)
  end

  # Get a product by ID
  def get_product!(id) do
    Repo.get!(Product, id)
  end

  # Create a product
  def create_product(attrs \\ %{}) do
    %Product{}
    |> Product.changeset(attrs)
    |> Repo.insert()
  end

  # Update a product
  def update_product(%Product{} = product, attrs) do
    product
    |> Product.changeset(attrs)
    |> Repo.update()
  end

  # Delete a product
  def delete_product(%Product{} = product) do
    Repo.delete(product)
  end

  # Create empty changeset for forms
  def change_product(%Product{} = product, attrs \\ %{}) do
    Product.changeset(product, attrs)
  end
end
```

## Database Integration with Ecto

Ecto is Phoenix's database wrapper that provides schema definitions, changesets for validation, and a query DSL.

### Schemas and Changesets

Schemas define the structure of your database tables:

```elixir
defmodule MyApp.Store.Product do
  use Ecto.Schema
  import Ecto.Changeset

  schema "products" do
    field :name, :string
    field :description, :string
    field :price, :decimal
    field :slug, :string

    belongs_to :category, MyApp.Store.Category
    has_many :reviews, MyApp.Store.Review

    timestamps()  # Adds inserted_at and updated_at
  end

  # Changeset handles validation and type casting
  def changeset(product, attrs) do
    product
    |> cast(attrs, [:name, :description, :price, :slug, :category_id])
    |> validate_required([:name, :price])
    |> validate_length(:name, min: 2, max: 100)
    |> validate_number(:price, greater_than: 0)
    |> unique_constraint(:slug)
    |> generate_slug()
  end

  # Custom changeset function to generate slug
  defp generate_slug(changeset) do
    case get_change(changeset, :name) do
      nil -> changeset
      name ->
        slug = name |> String.downcase() |> String.replace(~r/[^a-z0-9]+/, "-")
        put_change(changeset, :slug, slug)
    end
  end
end
```

### Migrations

Migrations define database schema changes over time:

```elixir
defmodule MyApp.Repo.Migrations.CreateProducts do
  use Ecto.Migration

  def change do
    create table(:products) do
      add :name, :string, null: false
      add :description, :text
      add :price, :decimal, null: false, precision: 10, scale: 2
      add :slug, :string
      add :category_id, references(:categories, on_delete: :nothing)

      timestamps()
    end

    create unique_index(:products, [:slug])
    create index(:products, [:category_id])
  end
end
```

Generate and run migrations:

```bash
# Generate a migration
mix ecto.gen.migration create_products

# Run migrations
mix ecto.migrate
```

### Seeding the Database

Create seed files in `priv/repo/seeds.exs`:

```elixir
# priv/repo/seeds.exs

# Categories
alias MyApp.Repo
alias MyApp.Store.Category
alias MyApp.Store.Product

# Clear existing data (optional)
Repo.delete_all(Product)
Repo.delete_all(Category)

# Insert categories
{:ok, electronics} = Repo.insert(%Category{name: "Electronics"})
{:ok, books} = Repo.insert(%Category{name: "Books"})
{:ok, clothing} = Repo.insert(%Category{name: "Clothing"})

# Insert products
Repo.insert!(%Product{
  name: "Laptop",
  description: "High-performance laptop",
  price: Decimal.new("999.99"),
  category_id: electronics.id
})

Repo.insert!(%Product{
  name: "Phoenix Programming",
  description: "Learn Phoenix Framework",
  price: Decimal.new("39.99"),
  category_id: books.id
})

Repo.insert!(%Product{
  name: "T-shirt",
  description: "Comfortable cotton t-shirt",
  price: Decimal.new("19.99"),
  category_id: clothing.id
})

IO.puts "Seed data inserted successfully!"
```

Run seeds:

```bash
mix run priv/repo/seeds.exs
```

## Authentication

Phoenix doesn't include authentication out of the box, but you can add it using `phx_gen_auth`:

```bash
# Add phx_gen_auth to your dependencies
# mix.exs
{:phx_gen_auth, "~> 0.7.0", only: [:dev], runtime: false}

# Generate authentication system
mix phx.gen.auth Accounts User users
```

This generates:

- User schema and context
- Registration and login forms
- Password hashing with BCrypt
- Session management
- Account confirmation and reset functions

## Deployment

Here's a basic deployment workflow for Phoenix applications:

```mermaid
graph TD
    A[Local Development] --> B[Configure Production Settings]
    B --> C[Compile Assets]
    C --> D[Build Release]
    D --> E[Deploy to Server]
    E --> F[Run Migrations]
    F --> G[Start Phoenix Server]
```

### Basic Deployment Steps

1. Configure production settings in `config/runtime.exs`:

```elixir
# config/runtime.exs
import Config

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
      raise "DATABASE_URL environment variable is missing"

  config :my_app, MyApp.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")

  # Other production settings...
end
```

2. Build a release:

```bash
MIX_ENV=prod mix release
```

3. Deploy the release to your server:

```bash
# Example using scp (secure copy)
scp -r _build/prod/rel/my_app user@your-server:/app
```

4. On the server, run:

```bash
# Set environment variables
export DATABASE_URL=postgres://username:password@hostname/database_name
export SECRET_KEY_BASE=$(mix phx.gen.secret)
export PHX_HOST=your-domain.com
export PORT=4000

# Run migrations
/app/my_app/bin/my_app eval "MyApp.Release.migrate"

# Start the application
/app/my_app/bin/my_app start
```

### Docker Deployment

Create a `Dockerfile`:

```dockerfile
FROM elixir:1.15-alpine AS build

# Install build dependencies
RUN apk add --no-cache build-base npm git

WORKDIR /app

# Copy config files
COPY config ./config
COPY mix.exs mix.lock ./

# Install dependencies
RUN mix deps.get --only prod
RUN mix deps.compile

# Copy assets
COPY assets ./assets
COPY priv ./priv

# Compile assets
RUN mix assets.deploy

# Copy application code
COPY lib ./lib

# Compile and build release
RUN mix compile
RUN mix release

# Start a new stage for the final image
FROM alpine:3.18 AS app

RUN apk add --no-cache libstdc++ openssl ncurses-libs

WORKDIR /app

# Copy the release from the build stage
COPY --from=build /app/_build/prod/rel/my_app ./

# Run the application
CMD ["bin/my_app", "start"]
```

## The Remaining 15%: Advanced Topics to Explore

These advanced topics represent the remaining 15% that you can explore as needed:

1. **OTP (Open Telecom Platform)**
   - GenServers for stateful processes
   - Supervisors for fault tolerance
   - Applications and supervision trees
   - Process communication patterns

2. **LiveView**
   - Real-time UI updates without JavaScript
   - Event handling and state management
   - Live components and hooks
   - Form handling and validation

3. **Channels**
   - WebSocket communication
   - Custom protocol implementations
   - Client libraries for JavaScript, iOS, Android

4. **Phoenix PubSub**
   - Distributed real-time communication
   - Broadcasting messages across a cluster
   - Implementing presence features

5. **Advanced Ecto Features**
   - Complex queries and associations
   - Database functions and fragments
   - Multi-database setups
   - Custom types and validations

6. **Performance Optimization**
   - Profiling and benchmarking
   - Database query optimization
   - Cache strategies
   - Distributed systems

7. **Custom Plugs**
   - Building authentication systems
   - Request transformation
   - Response composition
   - Building middleware pipelines

8. **API Development**
   - JSON API best practices
   - API versioning
   - Rate limiting
   - GraphQL with Absinthe

9. **Testing Strategies**
   - Property-based testing
   - Integration testing
   - End-to-end testing
   - Load testing
   - Mocking external services

## Summary

This crash course covered the core 85% of Phoenix Framework that you'll use daily:

- Project setup and structure
- MVC architecture implementation
- Routing configuration
- Context organization
- Database integration with Ecto
- Authentication and deployment

Phoenix's excellent documentation (https://hexdocs.pm/phoenix) and growing community make it easy to explore the advanced topics as your needs evolve. The framework's focus on developer productivity and runtime performance makes it an excellent choice for building modern web applications.
