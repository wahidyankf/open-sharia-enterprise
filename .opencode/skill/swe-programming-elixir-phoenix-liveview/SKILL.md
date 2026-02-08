---
name: swe-programming-elixir-phoenix-liveview
description: Phoenix LiveView coding standards from authoritative docs/explanation/software-engineering/platform-web/tools/elixir-phoenix/ex-soen-plwe-to-elph__liveview.md documentation
---

# Phoenix LiveView Coding Standards

## Purpose

Progressive disclosure of Phoenix LiveView standards for agents writing real-time interactive applications.

**Authoritative Source**: [docs/explanation/software-engineering/platform-web/tools/elixir-phoenix/ex-soen-plwe-to-elph\_\_liveview.md](../../../docs/explanation/software-engineering/platform-web/tools/elixir-phoenix/ex-soen-plwe-to-elph__liveview.md)

**Usage**: Auto-loaded for agents when writing Phoenix LiveView code. Provides quick reference to LiveView lifecycle, events, components, and real-time patterns.

**Foundation**: Requires Phoenix Framework skill (swe-programming-elixir-phoenix). Phoenix LiveView builds on Phoenix Framework with real-time server-rendered capabilities.

## Prerequisite Knowledge

**CRITICAL**: This skill provides **OSE Platform Phoenix LiveView standards**, not LiveView tutorials.

**You MUST complete Elixir, Phoenix Framework, AND Phoenix LiveView learning paths IN ORDER**:

**1. Elixir Foundation** (prerequisite for Phoenix):

- [Elixir Learning Path](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/elixir/) - Initial setup, language overview (0-95% coverage)
- [Elixir By Example](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/elixir/by-example/) - 75+ annotated code examples
- [Elixir In the Field](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/elixir/in-the-field/) - Production patterns
- [Elixir Release Highlights](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/elixir/release-highlights/) - Elixir 1.12-1.18 features

**2. Phoenix Framework Foundation** (prerequisite for LiveView):

- Complete all Phoenix Framework learning content (see swe-programming-elixir-phoenix skill)
- Understand contexts, controllers, channels before using LiveView

**3. Phoenix LiveView Learning Path**:

- [Phoenix LiveView Initial Setup](../../../apps/ayokoding-web/content/en/learn/software-engineering/platform-web/tools/elixir-phoenix-liveview/initial-setup.md) - Environment setup
- [Phoenix LiveView Overview](../../../apps/ayokoding-web/content/en/learn/software-engineering/platform-web/tools/elixir-phoenix-liveview/overview.md) - Lifecycle, events, state
- [Phoenix LiveView By Example](../../../apps/ayokoding-web/content/en/learn/software-engineering/platform-web/tools/elixir-phoenix-liveview/by-example/) - 85+ examples
- [Phoenix LiveView In-the-Field](../../../apps/ayokoding-web/content/en/learn/software-engineering/platform-web/tools/elixir-phoenix-liveview/in-the-field/) - 37 production guides

**Documentation Separation**:

- **AyoKoding Phoenix LiveView** - "How to use Phoenix LiveView" (educational, universal patterns)
- **docs/explanation/elixir-phoenix/liveview** - "How to use LiveView in OSE Platform" (repository conventions)

**What this skill covers**: OSE Platform LiveView configuration, lifecycle management, event handling, component architecture, real-time update patterns, testing strategies.

**What this skill does NOT cover**: LiveView basics, Phoenix fundamentals (those are in ayokoding-web).

## Quick Standards Reference

### LiveView Lifecycle

- `mount/3` - Initialize socket state on first load
- `handle_params/3` - Handle URL parameter changes
- `handle_event/3` - Process client events
- `handle_info/2` - Handle process messages
- `render/1` - Generate HTML template

### State Management

- Use `assign/3` to update socket state
- Keep state minimal and normalized
- Use temporary assigns for one-time data
- Apply `Phoenix.Component.update/2` for derived state

### Event Handling

- Use `phx-click`, `phx-submit`, `phx-change` for user events
- Implement debouncing with `phx-debounce`
- Apply throttling with `phx-throttle`
- Use `phx-hook` for JavaScript interop

### Components

- Use function components for stateless UI
- Apply LiveComponent for stateful isolated components
- Keep components focused and reusable
- Pass assigns explicitly, avoid global state

### Real-time Updates

- Use `Phoenix.PubSub` for broadcasting updates
- Subscribe in `mount/3`, unsubscribe automatically
- Apply `push_event/3` for client-side JavaScript
- Use streams for efficient list updates

### Form Handling

- Use `Phoenix.Component.form/1` for forms
- Apply Ecto changesets for validation
- Implement inline validation with `phx-change`
- Handle file uploads with `allow_upload/3`

### Performance

- Use `temporary_assigns` for large datasets
- Apply pagination for long lists
- Implement lazy loading for images
- Use `Phoenix.LiveView.JS` for client-side DOM manipulation

### Testing

- Use `Phoenix.LiveViewTest` for integration tests
- Test lifecycle callbacks explicitly
- Verify events trigger expected state changes
- Test component rendering and interactions

## Comprehensive Documentation

For detailed guidance, refer to the comprehensive LiveView standards file:

**LiveView Standards**:

- [Phoenix LiveView](../../../docs/explanation/software-engineering/platform-web/tools/elixir-phoenix/ex-soen-plwe-to-elph__liveview.md) - Complete LiveView patterns, lifecycle, components, testing (1387 lines)

**Related Phoenix Standards** (from Phoenix Framework skill):

- [Phoenix Idioms](../../../docs/explanation/software-engineering/platform-web/tools/elixir-phoenix/ex-soen-plwe-to-elph__idioms.md)
- [Phoenix Best Practices](../../../docs/explanation/software-engineering/platform-web/tools/elixir-phoenix/ex-soen-plwe-to-elph__best-practices.md)
- [Phoenix Testing](../../../docs/explanation/software-engineering/platform-web/tools/elixir-phoenix/ex-soen-plwe-to-elph__testing.md)

## Related Skills

- swe-programming-elixir - Elixir language fundamentals (foundation)
- swe-programming-elixir-phoenix - Phoenix Framework patterns (prerequisite)
- docs-applying-content-quality - Content quality standards
- repo-practicing-trunk-based-development - Git workflow

## References

- [Phoenix LiveView Documentation](../../../docs/explanation/software-engineering/platform-web/tools/elixir-phoenix/ex-soen-plwe-to-elph__liveview.md)
- [Phoenix Framework README](../../../docs/explanation/software-engineering/platform-web/tools/elixir-phoenix/README.md)
- [Elixir README](../../../docs/explanation/software-engineering/programming-languages/elixir/README.md)
- [Functional Programming](../../../governance/development/pattern/functional-programming.md)
