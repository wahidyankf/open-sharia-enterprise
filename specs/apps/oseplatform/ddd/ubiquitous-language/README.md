# OSE Platform Web — Ubiquitous Language

Audience: Engineers, Technical Product/Project Managers

Per-bounded-context glossaries for `oseplatform-web`. Every term used in a Gherkin feature
file or in production code SHOULD appear in exactly one of these glossaries, scoped to the
context that owns it.

## Glossaries (one per bounded context)

| Bounded Context | File                           | Layers                                        |
| --------------- | ------------------------------ | --------------------------------------------- |
| app-shell       | [app-shell.md](./app-shell.md) | `[presentation]`                              |
| landing         | [landing.md](./landing.md)     | `[presentation]`                              |
| content         | [content.md](./content.md)     | `[application, infrastructure, presentation]` |
| search          | [search.md](./search.md)       | `[application, infrastructure, presentation]` |
| rss-feed        | [rss-feed.md](./rss-feed.md)   | `[application, infrastructure]`               |
| seo             | [seo.md](./seo.md)             | `[application, presentation]`                 |
| health          | [health.md](./health.md)       | `[application, presentation]`                 |

## Glossary anatomy

Every glossary file carries:

- **Frontmatter** — `Bounded context`, `Maintainer`, `Last reviewed`.
- **One-line summary** — what the bounded context owns, in one sentence.
- **Term index table** — Term · Code identifier(s) · Used in features.
- **Terms in detail** — one section per term, with code identifier, features, forbidden
  synonyms, and related terms.

## Validation

`rhino-cli ddd ul oseplatform` checks each glossary:

- Required frontmatter keys present.
- Terms table header matches canonical columns.
- Backticked code identifiers in the table exist somewhere under the BC's declared `code:`
  path in `bounded-contexts.yaml`.
- Feature file references resolve to real `.feature` files under the declared `gherkin:`
  path.
- Same term in two glossaries → both must declare mutual `Forbidden synonyms` cross-links.

## Adding or updating a term

1. Locate the bounded context that owns the term — if uncertain, default to the BC whose
   code defines the canonical type.
2. Add a row to the term index table (Term · Code identifier(s) · Used in features).
3. Add a "Terms in detail" section with the code identifier(s), used-in-features list,
   forbidden synonyms, and related terms.
4. If the term existed in another glossary, move it (don't duplicate).
5. Run `rhino-cli ddd ul oseplatform` to confirm validity before committing.

## Related

- [`../bounded-contexts.yaml`](../bounded-contexts.yaml) — registry
- [`../bounded-context-map.md`](../bounded-context-map.md) — relationship diagram
- [`../README.md`](../README.md) — DDD artifacts overview
