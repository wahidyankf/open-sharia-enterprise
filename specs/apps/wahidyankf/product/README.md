# wahidyankf-web — Product

Audience: Engineers, Technical Product/Project Managers

Product-level specifications for wahidyankf-web — what the application is for, who uses it,
and how the user experience is shaped. This is C4 Level 0: the product framing that sits
above system context. Read this first if you are new to the portfolio site.

## What it is

`wahidyankf-web` is a personal portfolio and CV site for Wahidyan Kresna Fridayoka. It
serves two audiences: technical recruiters and hiring managers scanning a professional
profile, and collaborators looking for contact and project links. The site is read-only,
fully static (no backend, no auth, no database), and deploys from `prod-wahidyankf-web` to
Vercel at [www.wahidyankf.com](https://www.wahidyankf.com).

## Planned children

- `overview.md` — PM-first product summary covering personas, primary user flows, current
  scope vs deferred items.

## Related

- [`../system-context/`](../system-context/README.md) — C4 L1 system context (actors and external systems)
- [`../containers/`](../containers/README.md) — C4 L2 containers (single `web` container)
- [`../components/`](../components/README.md) — C4 L3 components (per-BC internals)
- [`../behavior/`](../behavior/README.md) — Gherkin scenarios that exercise the product
