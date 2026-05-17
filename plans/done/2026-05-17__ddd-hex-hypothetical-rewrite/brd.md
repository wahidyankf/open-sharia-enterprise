---
title: "BRD — Rewrite DDD + Hexagonal in Practice with hypothetical domain"
date: 2026-05-17
---

# Business Rationale

## Why now

The current tutorial reads as an apprenticeship guide for `apps/ose-app-be` and `apps/organiclever-be`. Every time those codebases change layout, rename a file, or migrate a package, the tutorial silently goes wrong. The mirror-mode contract (`Source:` lines pointing at real files) makes the breakage invisible to the reader — they see a snippet, they trust it, the link is dead or the file has moved. The checker can validate the file exists but cannot validate the snippet still represents the seam being taught.

The tutorial's stated goal is "how the pieces wire together in production". That goal is best served by a stable hypothetical domain, not by a moving real one. A reader learning hexagonal wiring does not need to know that the regulatory-source context lives at a particular package path in a particular real service; they need to see _the wiring shape_ and trust it will still be correct in six months.

## Who benefits

- **Readers** — snippets stay correct over time; no broken `Source:` links; no need to context-switch into a real OSE codebase to follow along.
- **Maintainers** — tutorial no longer tracks two moving codebases. Changes to `ose-app-be` or `organiclever-be` no longer trigger tutorial drift.
- **Future tutorial authors** — same domain model is reusable for other in-practice tutorials (CQRS in practice, sagas in practice, etc.).

## Non-goals

- Not removing the by-example prerequisites (DDD by example, Hexagonal by example). Those remain required reading.
- Not splitting into a third tutorial. Same two-track shape (FP, OOP) preserved.
- Not collapsing tiers. `overview → beginner → intermediate → advanced → production` preserved.
- Not changing the in-the-field tutorial convention (annotation density, guide count, diagram count, production-grade code). Those checker rules still apply.
