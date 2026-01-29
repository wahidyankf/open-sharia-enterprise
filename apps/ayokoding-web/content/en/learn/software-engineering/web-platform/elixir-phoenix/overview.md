---
title: Overview
weight: 100000
date: 2025-12-30T00:00:00+07:00
draft: false
description: Build real-time, fault-tolerant web applications with Phoenix Framework on the Elixir platform
---

Phoenix is a web framework built with Elixir that leverages the power of the Erlang VM (BEAM) to deliver high-performance, real-time web applications with exceptional fault tolerance and concurrency.

## What You'll Learn

- **Phoenix Framework Basics** - MVC architecture, routing, controllers, and views
- **LiveView** - Building real-time, interactive UIs without JavaScript
- **Ecto Database Layer** - Schema definitions, queries, migrations, and changesets
- **Channels & PubSub** - Real-time bidirectional communication
- **Deployment** - Production deployment strategies for Phoenix applications
- **OTP Principles** - Supervision trees, GenServers, and fault tolerance

## Platform Characteristics

### Real-Time by Default

Phoenix includes first-class support for real-time features through Channels and LiveView. Build interactive, low-latency applications that push updates to clients instantly without complex JavaScript frameworks.

### Fault-Tolerant Architecture

Built on the Erlang VM, Phoenix applications inherit decades of battle-tested fault tolerance patterns. Processes crash and restart automatically, ensuring your application stays available even under failure conditions.

### Scalability & Performance

The BEAM VM's lightweight processes and efficient scheduling enable Phoenix to handle millions of concurrent connections on a single server. Horizontal scaling is straightforward with distributed Erlang clustering.

### Developer Productivity

Phoenix provides a productive development experience with code reloading, comprehensive testing tools, and a clear MVC structure. LiveView enables building rich UIs with minimal JavaScript.

## Getting Started

Before diving into Phoenix development, get up and running:

1. **[Initial Setup](/en/learn/software-engineering/web-platform/elixir-phoenix/initial-setup)** - Install Elixir, Phoenix, PostgreSQL, Node.js, verify your setup
2. **[Quick Start](/en/learn/software-engineering/web-platform/elixir-phoenix/quick-start)** - Your first Phoenix app, basic routing, essential patterns

These foundational tutorials (0-30% coverage) prepare you for comprehensive Phoenix learning.

Phoenix development follows this typical progression:

1. **Environment Setup** - Install Elixir, Phoenix, and PostgreSQL
2. **Core Concepts** - Routes, controllers, templates, and contexts
3. **Database Integration** - Ecto schemas, migrations, and queries
4. **Real-Time Features** - Channels for WebSocket communication
5. **LiveView Applications** - Server-rendered interactive UIs
6. **Production Deployment** - Release management and deployment

## Common Use Cases

- **Real-Time Dashboards** - Live data visualization and monitoring
- **Chat Applications** - Instant messaging and collaboration tools
- **Trading Platforms** - Low-latency financial applications
- **IoT Backends** - Managing thousands of connected devices
- **Gaming Servers** - Multiplayer game backends
- **Collaborative Tools** - Real-time document editing and collaboration

## Why Phoenix

### When to Choose Phoenix

Phoenix excels in scenarios requiring:

- **Real-time applications** - Chat, collaboration tools, live dashboards, gaming with sub-100ms latency
- **Concurrent workloads** - Thousands to millions of simultaneous users with minimal resource usage
- **Fault tolerance requirements** - Always-on systems with process isolation and automatic recovery
- **Functional programming preference** - Immutable data, pattern matching, pipeline operators
- **LiveView for interactive UIs** - Rich user interfaces without heavy JavaScript frameworks
- **Low operational overhead** - Single server handles what Node.js requires clusters for

### Phoenix vs Other Frameworks

- **vs Ruby on Rails** - Phoenix offers better concurrency and real-time features; Rails provides faster initial development and larger ecosystem
- **vs Django (Python)** - Phoenix delivers superior performance and fault tolerance; Django offers simpler learning curve and broader library support
- **vs Node.js/Express** - Phoenix handles concurrency more efficiently with lightweight processes; Node.js has larger JavaScript ecosystem
- **vs Spring Boot (Java)** - Phoenix excels at real-time features and resource efficiency; Spring Boot suits complex enterprise business logic
- **vs Laravel (PHP)** - Phoenix provides functional programming and better concurrency; Laravel offers simpler deployment and shared hosting support

## Phoenix Versions & Compatibility

### Version Requirements

- **Phoenix 1.7+** (Current) - Requires Elixir 1.14+, Erlang/OTP 24+, introduces verified routes (~p sigil), function components, unified HEEx templates
- **Phoenix 1.6** (Legacy) - Supports Elixir 1.12+, Erlang/OTP 22+, uses traditional view layer and template helpers
- **Migration Path** - Upgrading 1.6 to 1.7 requires refactoring views to function components, updating route helpers to verified routes
- **Long-Term Support** - Phoenix 1.7 recommended for new projects; 1.6 receives security patches but limited feature development

### Component Version Compatibility

- **Elixir Version** - Phoenix 1.7 requires Elixir 1.14 minimum (Elixir 1.15+ recommended for latest language features)
- **Erlang/OTP** - OTP 24+ required for Phoenix 1.7; OTP 26+ recommended for performance improvements
- **Phoenix LiveView** - Phoenix 1.7 works with LiveView 0.18+; LiveView 0.20+ adds declarative form bindings and async result handling
- **Ecto** - Ecto 3.10+ for Phoenix 1.7; Ecto 3.11+ recommended for improved query performance
- **Phoenix PubSub** - Phoenix PubSub 2.1+ for distributed messaging
- **Database Adapters** - Postgrex 0.16+ for PostgreSQL, MyXQL 0.6+ for MySQL

### Project Templates

Phoenix provides project generator options for different application types:

- **Default (Full)** - `mix phx.new my_app` - Complete Phoenix with Ecto, LiveView, HTML templates, asset pipeline
- **API Only** - `mix phx.new my_app --no-html --no-assets` - JSON API backend without frontend
- **No Ecto** - `mix phx.new my_app --no-ecto` - Phoenix without database layer (for microservices or external datastores)
- **No LiveView** - `mix phx.new my_app --no-live` - Traditional request-response without LiveView (rare, LiveView recommended)
- **Binary IDs** - `mix phx.new my_app --binary-id` - Use UUIDs instead of integer primary keys for distributed systems
- **Database Choice** - `--database postgres|mysql|mssql|sqlite` - PostgreSQL default, MySQL/MSSQL/SQLite alternatives

## Prerequisites

### For Elixir Developers New to Phoenix

- **Elixir 1.14+ installed** - Phoenix 1.7 requires Elixir 1.14 minimum (install via asdf, Homebrew, or distribution packages)
- **Erlang/OTP 24+** - BEAM VM runtime (typically installed automatically with Elixir)
- **PostgreSQL** - Default database for Phoenix (PostgreSQL 12+ recommended); alternatives include MySQL, MSSQL, SQLite
- **Node.js** - Asset compilation with esbuild (Node.js 14+ required for development)
- **Elixir fundamentals** - Pattern matching, pipe operator, modules, functions, processes, basic OTP (GenServer, Supervisor)
- **HTTP and REST** - HTTP methods, status codes, JSON serialization for API development
- **SQL basics** - SELECT, INSERT, UPDATE, DELETE, JOINs for Ecto examples

### For Ruby/Rails Developers Switching to Phoenix

- **Functional programming mindset** - Immutable data structures vs Rails' mutable ActiveRecord objects
- **Explicit over implicit** - Phoenix contexts explicitly define boundaries vs Rails' magic associations
- **Process-based architecture** - Lightweight BEAM processes vs Rails' thread-per-request model
- **Pattern matching** - Elixir's pattern matching replaces conditional logic common in Ruby
- **Ecto vs ActiveRecord** - Ecto separates schemas from queries; ActiveRecord combines them
- **Asset pipeline differences** - Phoenix uses esbuild/Tailwind vs Rails' Webpacker/Sprockets
- **Learning curve** - Expect 2-3 weeks for Elixir syntax; functional paradigm shift takes longer

### For Python/Django Developers Switching to Phoenix

- **Functional programming** - Elixir's immutable data and pattern matching vs Python's multi-paradigm approach
- **Concurrency model** - BEAM's lightweight processes vs Django's threading/async views
- **Context boundaries** - Phoenix contexts similar to Django apps but more explicit
- **Ecto vs Django ORM** - Ecto uses changesets for validation; Django uses model forms
- **Template differences** - HEEx templates vs Django templates; similar syntax but different rendering
- **Admin interface** - Phoenix lacks Django's built-in admin; use ExAdmin or custom LiveView dashboards
- **Deployment model** - Mix releases vs WSGI servers; Phoenix compiles to single binary

### For Node.js Developers Switching to Phoenix

- **Functional vs imperative** - Elixir's functional approach vs JavaScript's imperative/functional hybrid
- **Static typing optional** - Elixir uses Dialyzer for type checking; TypeScript-like but less enforced
- **Process concurrency** - BEAM's lightweight processes vs Node.js event loop and worker threads
- **LiveView vs React/Vue** - Server-rendered interactivity vs client-side JavaScript frameworks
- **Ecto vs Sequelize/Prisma** - Different query building; Ecto more composable
- **Async patterns** - Phoenix LiveView handles real-time without callbacks/promises complexity
- **Package management** - Hex (similar to npm) with Mix as task runner

### For Complete Framework Beginners

- **Start with Elixir fundamentals** - Complete Elixir basics before Phoenix (1-2 weeks minimum)
- **Understand functional programming** - Immutability, recursion, pattern matching, higher-order functions
- **Learn OTP basics** - GenServer, Supervisor, process communication for fault tolerance understanding
- **Follow beginner examples** - Start with Example 1 and progress through all 25 beginner examples
- **Build projects** - Create small Phoenix apps (blog, todo list, chat) for hands-on practice
- **Use Phoenix generators** - Run `mix phx.gen.html` and `mix phx.gen.live` to see idiomatic code structure

## Community and Resources

- [Official Phoenix Documentation](https://hexdocs.pm/phoenix/overview.html)
- [Phoenix LiveView Documentation](https://hexdocs.pm/phoenix_live_view/)
- [Elixir Forum](https://elixirforum.com/) - Active community discussion board
- [Phoenix GitHub](https://github.com/phoenixframework/phoenix) - Source code and issue tracking
- [Elixir Slack](https://elixir-slackin.herokuapp.com/) - Real-time community chat
- [Awesome Phoenix](https://github.com/droptheplot/awesome-phoenix) - Curated list of Phoenix resources
- [Phoenix Blog](https://www.phoenixframework.org/blog) - Official announcements and updates
- [ElixirConf Talks](https://www.youtube.com/c/ElixirConf) - Conference presentations and Phoenix content

## Next Steps

Explore the tutorials section to start building with Phoenix, from initial setup through advanced real-time features and production deployment strategies.
