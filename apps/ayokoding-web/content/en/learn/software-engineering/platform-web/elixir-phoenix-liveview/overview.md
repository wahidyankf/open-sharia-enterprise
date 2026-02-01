---
title: "Overview"
date: 2026-02-01T00:00:00+07:00
draft: false
weight: 10000000
description: "Build real-time web applications with server-rendered HTML over WebSockets—no JavaScript framework required"
tags: ["phoenix", "liveview", "elixir", "real-time", "websocket"]
---

**Want real-time web apps without JavaScript complexity?** Phoenix LiveView enables building rich, interactive web applications entirely in Elixir—server-rendered HTML that updates in real-time over WebSockets.

## What Is Phoenix LiveView?

Phoenix LiveView is a **server-side rendering library** for the Phoenix framework that enables building real-time, interactive web applications without writing JavaScript. Updates happen over WebSocket connections, with HTML patches sent from the server to update only the changed parts of the page.

**Key capabilities**:

- **Server-rendered**: All logic runs on the server in Elixir (no client-side JavaScript needed)
- **Real-time updates**: WebSocket connection keeps UI synchronized with server state
- **Minimal payload**: Only HTML diffs sent over the wire (efficient bandwidth usage)
- **Built-in latency compensation**: Optimistic UI updates while waiting for server response
- **File uploads**: Built-in chunked upload support with progress tracking
- **LiveComponents**: Reusable, stateful UI components

Unlike React or Vue which require client-side JavaScript and API coordination, LiveView keeps all application logic on the server while providing the same interactive user experience.

## Why Phoenix LiveView?

**Simplified architecture**:

- **No frontend/backend split**: Single Elixir codebase handles both UI and business logic
- **No API layer**: Direct function calls instead of REST/GraphQL endpoints
- **No state synchronization**: Server state is the source of truth (no client-side state management)
- **No build tools**: No webpack, babel, or npm dependencies for basic LiveView apps

**Developer experience**:

- **Functional programming**: Leverage Elixir's pattern matching and immutability
- **OTP supervision**: Processes crash and restart gracefully (fault tolerance built-in)
- **Hot code reloading**: Update code without restarting the server
- **Unified testing**: Test UI and business logic with same tools (ExUnit)

**Performance**:

- **Minimal JavaScript**: ~35KB compressed JavaScript for LiveView runtime (vs MB for React/Vue)
- **Efficient updates**: Only changed HTML sent over WebSocket (not full re-renders)
- **BEAM concurrency**: Millions of concurrent connections on single server
- **Presence tracking**: Built-in distributed user tracking across servers

**Real-time by default**:

- **PubSub integration**: Built-in support for Phoenix.PubSub (multi-user synchronization)
- **Live navigation**: Navigate between pages without full page reload
- **Form validation**: Real-time validation as users type

## When to Use Phoenix LiveView

**Ideal for**:

- Real-time dashboards and analytics
- Collaborative tools (multi-user editing, chat)
- Forms with complex validation and dynamic behavior
- Admin interfaces and internal tools
- Applications where Elixir team expertise exists
- Projects prioritizing developer productivity over edge performance

**Not ideal for**:

- Offline-first applications (LiveView requires server connection)
- Applications needing heavy client-side computation (3D graphics, complex calculations)
- Sites requiring SEO for dynamic content (static pages SEO-friendly, dynamic content not crawlable)
- Mobile apps (LiveView is web-focused, not native mobile)
- Teams with no Elixir experience (learning curve)

**Phoenix LiveView vs. React/Vue**: LiveView eliminates API layer and client-side state management but requires server connection. React/Vue can work offline and perform client-side computation but require more infrastructure (API server, state management, build tools).

**Phoenix LiveView vs. HTMX**: Both server-render HTML, but LiveView uses WebSockets for persistent connection (real-time updates, multi-user sync) while HTMX uses HTTP requests (simpler, no persistent connection). LiveView for real-time apps, HTMX for traditional request/response patterns.

## Learning Paths

**Multiple ways to learn Phoenix LiveView**:

1. **[Initial Setup](/en/learn/software-engineering/platform-web/elixir-phoenix-liveview/initial-setup)** - Add LiveView to Phoenix app, create first LiveView
2. **[Quick Start](/en/learn/software-engineering/platform-web/elixir-phoenix-liveview/quick-start)** - Complete counter example with step-by-step walkthrough (5-30% coverage)
3. **[By Example](/en/learn/software-engineering/platform-web/elixir-phoenix-liveview/by-example)** - 85 annotated code examples covering 95% of LiveView features

**Recommended path for experienced Elixir developers**: Initial Setup → Quick Start → By Example for comprehensive learning.

**Recommended path for Elixir beginners**: Learn Elixir first → Learn Phoenix basics → Initial Setup → Quick Start → By Example.

## Prerequisites

**Required**:

- Elixir fundamentals (pattern matching, processes, OTP basics) - see [Elixir by Example](/en/learn/software-engineering/programming-languages/elixir/by-example)
- Phoenix framework basics (routing, controllers, views) - see [Phoenix by Example](/en/learn/software-engineering/platform-web/elixir-phoenix/by-example)
- Understanding of web fundamentals (HTML, CSS, HTTP)
- Basic understanding of WebSockets concept

**Recommended (helpful but not required)**:

- Familiarity with Ecto (Phoenix's database library)
- Understanding of PubSub patterns
- Experience with functional programming

**No JavaScript required** - LiveView handles client-side updates automatically. JavaScript knowledge helps for client hooks (advanced feature) but isn't necessary for most LiveView applications.

## Key Features

### Server-Rendered HTML Over WebSockets

LiveView establishes a WebSocket connection and sends only HTML diffs:

1. **Initial render**: Server renders full HTML page (standard HTTP request)
2. **WebSocket upgrade**: JavaScript establishes persistent WebSocket connection
3. **Event handling**: User interactions sent to server over WebSocket
4. **State update**: Server updates state and re-renders affected components
5. **Patch DOM**: Only changed HTML sent to client, DOM patched efficiently

This architecture means all business logic stays on the server while providing reactive UI updates.

### LiveComponents for Reusability

LiveComponents are stateful, reusable UI components:

- **Isolated state**: Each component manages its own assigns
- **Event handling**: Components handle their own events
- **Lifecycle callbacks**: mount/3, update/2 for initialization and updates
- **Send updates**: External processes can update components via send_update/3

Components enable building complex UIs from smaller, testable pieces.

### File Uploads

Built-in support for chunked file uploads with progress tracking:

- **Client-side validation**: File type and size validation before upload
- **Progress tracking**: Real-time upload progress updates
- **Direct upload**: Option to upload directly to S3/GCS (bypass server)
- **Auto-cleanup**: Temporary files cleaned up automatically

File upload handling that would require complex JavaScript in traditional apps is built-in.

### Real-Time Multi-User Sync

Integration with Phoenix.PubSub enables multi-user real-time features:

- **Subscribe to topics**: LiveViews subscribe to PubSub topics
- **Broadcast updates**: Changes broadcast to all subscribed clients
- **Presence tracking**: Track which users are currently online
- **Distributed**: Works across multiple servers (distributed Erlang)

Build collaborative features (shared whiteboards, live editing) without external services.

### Testing

Test LiveViews with same tools as regular Phoenix code:

```elixir
test "counter increments", %{conn: conn} do
  {:ok, view, _html} = live(conn, "/counter")

  # Simulate button click
  assert view |> element("button", "Increment") |> render_click() =~ "Count: 1"

  # Verify state
  assert render(view) =~ "Count: 1"
end
```

Unified testing approach means no separate frontend test infrastructure needed.

## How LiveView Works

**Simplified request lifecycle**:

1. User navigates to `/live-page` (HTTP GET request)
2. Server renders initial HTML with LiveView JavaScript
3. Browser loads page, LiveView JS establishes WebSocket
4. User clicks button (event sent over WebSocket)
5. Server's handle_event/3 callback processes click
6. State updated (assigns modified)
7. Template re-rendered with new state
8. HTML diff computed and sent over WebSocket
9. Browser patches DOM with changes

**Key insight**: State lives on the server. Client is just a rendering target that sends events and receives HTML patches.

## Relationship to Phoenix

Phoenix LiveView is **built on top of Phoenix framework**:

- **Requires Phoenix**: LiveView is a Phoenix library, not standalone
- **Uses Phoenix routing**: LiveView routes defined in Phoenix router
- **Leverages channels**: Built on Phoenix Channels (WebSocket abstraction)
- **Integrates with Ecto**: Use Ecto changesets for form validation
- **Shares conventions**: Follows Phoenix conventions (templates, layouts)

**You must understand Phoenix basics before learning LiveView**. See [Phoenix by Example](/en/learn/software-engineering/platform-web/elixir-phoenix/by-example) for Phoenix fundamentals.

## Next Steps

Choose your learning path:

- **[Initial Setup](/en/learn/software-engineering/platform-web/elixir-phoenix-liveview/initial-setup)** - Add LiveView to existing Phoenix app
- **[Quick Start](/en/learn/software-engineering/platform-web/elixir-phoenix-liveview/quick-start)** - Build your first LiveView (counter)
- **[By Example](/en/learn/software-engineering/platform-web/elixir-phoenix-liveview/by-example)** - Learn through 85 annotated examples

For production use, review the official Phoenix LiveView documentation for best practices, deployment, and performance optimization.
