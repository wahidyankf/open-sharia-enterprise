# Ubiquitous Language — wahidyankf-web

This folder is the **platform-agnostic glossary** of wahidyankf-web's bounded contexts. It
sits inside `specs/apps/wahidyankf/ddd/` alongside
[`bounded-contexts.yaml`](../bounded-contexts.yaml) because both are DDD artifacts read by
`rhino-cli ddd ul` and `rhino-cli ddd bc`. The same vocabulary governs the frontend and any
future surface.

## What lives here

One markdown file per bounded context, plus this index. Each file lists the terms that
context owns, the code identifiers carrying those terms, and the forbidden synonyms that
would belong to a neighbouring context.

| Context             | Glossary                                       |
| ------------------- | ---------------------------------------------- |
| `app-shell`         | [app-shell.md](./app-shell.md)                 |
| `cv`                | [cv.md](./cv.md)                               |
| `home`              | [home.md](./home.md)                           |
| `personal-projects` | [personal-projects.md](./personal-projects.md) |
| `search`            | [search.md](./search.md)                       |

The bounded-context map and the strategic-pattern relationships between these contexts live
in [`ddd/bounded-context-map.md`](../bounded-context-map.md).

## Authoring rules

1. **One file per bounded context.** Never co-locate terms from two contexts in one file.
2. **Glossary updates ride with the change that introduces them.** No separate catch-up PRs.
3. **Gherkin steps use only glossary terms.** Synonyms outside the glossary fail review.
4. **Code identifiers match the `Code identifier(s)` column verbatim.**
5. **Forbidden synonyms are explicit.** Each glossary lists synonyms used by a neighbouring
   context with a different meaning.
6. **Per-term H3 detail is required.** Each file MUST contain a `## Term index` table and a
   `## Terms in detail` section with one `### Term: <name>` H3 per row.

## How this folder is consumed

- **`wahidyankf-web` Gherkin features** under
  [`behavior/web/gherkin/`](../../behavior/web/gherkin/README.md) — every term in scenario
  titles and step text comes from here.
- **`wahidyankf-web` source** under
  [`apps/wahidyankf-web/src/`](../../../../../apps/wahidyankf-web/src/) — type names and
  function names match the `Code identifier(s)` column.

## Related

- [Bounded-context map](../bounded-context-map.md)
- [DDD Standards (platform-wide)](../../../../../docs/explanation/software-engineering/architecture/domain-driven-design-ddd/README.md)
- [wahidyankf specs README](../../README.md)
