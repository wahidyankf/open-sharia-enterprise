# AyoKoding — Product Overview

**Audience**: Engineers, Technical Product/Project Managers (SWE-background TPMs).

## What AyoKoding is

A multilingual educational website serving programming, AI, and security tutorials in
English (primary) and Indonesian. Content is rolling-release on `main` (Trunk Based
Development); scope grows incrementally rather than landing in numbered releases.

## Personas

- **Learners** — readers visiting from desktop, tablet, or mobile to read tutorials.
  Primary interaction: navigate the sidebar, read articles, search for topics.
- **Content Authors** — write markdown files with YAML frontmatter under `content/en/`
  and `content/id/`. Frontmatter governs ordering (`weight`), titles, dates, descriptions,
  tags, and draft state.

## Primary user flows

1. **Browse** — land on `/<locale>/`, navigate the sidebar tree to find a topic, read the
   article, follow prev/next or breadcrumb links between siblings.
2. **Search** — open the search dialog (button in header), type a query, see locale-scoped
   results, click a result to navigate to the page.
3. **Switch locale** — click the language switcher in the header to toggle between English
   and Indonesian; the same `Slug` resolves to the equivalent page under the new locale.

## In scope today

- Multilingual content (English + Indonesian) served as static-generated pages with
  per-locale routing via Next.js middleware.
- Full-text search via FlexSearch with per-locale indexes.
- Hierarchical sidebar navigation derived from the content directory's filesystem layout.
- WCAG AA accessibility compliance (skip-to-content, keyboard nav, focus rings, contrast).
- Dark/light theme toggle persisted in `localStorage`.
- Responsive layout from desktop to mobile (hamburger menu on mobile breakpoint).

## Deferred / out of scope

- Authentication / user accounts.
- Comments / discussion.
- Search result analytics.
- Additional locales beyond English + Indonesian.
- Any backend service beyond the in-process tRPC API in the `web` container.

## Architecture summary

One deployable container (`web` — Next.js 16) with two API perspectives (`web` UI-semantic
and `api` tRPC HTTP-semantic). Six bounded contexts: `app-shell`, `content`, `search`,
`i18n`, `navigation`, `health`. See [`../README.md`](../README.md) for the full BC table
and [`../containers/container.md`](../containers/container.md) for the slug-vs-container
distinction.

## Related

- [`../README.md`](../README.md) — full ayokoding specs index
- [`../system-context/context.md`](../system-context/context.md) — C4 L1 system context
- [`../containers/container.md`](../containers/container.md) — C4 L2 single deployable container
- [`../behavior/`](../behavior/README.md) — Gherkin scenarios that exercise the product
