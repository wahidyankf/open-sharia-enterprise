# OSE Platform Web — Product Overview

**Status**: Placeholder — PM-first overview to be authored when product framing stabilizes.

## What it is

`oseplatform-web` is the marketing and updates site for the Open Sharia Enterprise platform.
It showcases the platform vision, publishes development updates, and provides a searchable
content library.

## Who uses it

- **Visitors** — potential contributors, adopters, and community members reading about the
  platform's progress.
- **Content authors** — the platform maintainer publishing markdown update posts.

## What ships today

- Landing page with hero block and social links.
- Update post listing at `/updates/` and individual update articles.
- Full-text search via FlexSearch.
- RSS feed at `/feed.xml`.
- SEO: sitemap, robots.txt, per-route metadata.
- Health probe at `health.check` tRPC procedure.

## What is deferred

- Authenticated contributor portal.
- Dynamic content management (CMS integration).
- Multi-language support (English-only today).
