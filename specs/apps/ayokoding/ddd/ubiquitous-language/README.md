# Ubiquitous Language — AyoKoding

This folder is the **platform-agnostic glossary** of AyoKoding's bounded contexts. It sits
inside `specs/apps/ayokoding/ddd/` alongside [`bounded-contexts.yaml`](../bounded-contexts.yaml)
because both are DDD artifacts read by `rhino-cli ddd ul` and `rhino-cli ddd bc`. The same
vocabulary governs the `web` container (today) and any future surface (a separate API
container, future CLI integrations) without a folder rename.

## What lives here

One markdown file per bounded context, plus this index. Each file lists the terms that
context owns, the code identifiers carrying those terms, and the forbidden synonyms that
would belong to a neighbouring context.

| Context      | Glossary                         |
| ------------ | -------------------------------- |
| `app-shell`  | [app-shell.md](./app-shell.md)   |
| `content`    | [content.md](./content.md)       |
| `search`     | [search.md](./search.md)         |
| `i18n`       | [i18n.md](./i18n.md)             |
| `navigation` | [navigation.md](./navigation.md) |
| `health`     | [health.md](./health.md)         |

The bounded-context map and the strategic-pattern relationships between these contexts
live in [`ddd/bounded-context-map.md`](../bounded-context-map.md).

## Authoring rules

1. **One file per bounded context.** Never co-locate terms from two contexts in one file.
   If a term spans two contexts, the homonym is a forbidden synonym in one and an owned
   term in the other.
2. **Glossary updates ride with the change that introduces them.** A PR adding a new tRPC
   procedure, a new DTO field, or a new component MUST update the relevant glossary file
   in the same commit. No separate "glossary catch-up" PRs — that practice rots the
   glossary.
3. **Gherkin steps use only glossary terms.** Step definitions, scenario titles, and
   `Background` clauses pick vocabulary from this folder. Synonyms from outside the
   glossary fail review.
4. **Code identifiers match the `Code identifier(s)` column verbatim.** A term documented
   as `Locale` is the TypeScript `Locale` type, not `Language` or `Lang`.
5. **Forbidden synonyms are explicit.** Each glossary lists synonyms used by a neighbouring
   context with a different meaning. Reviewers reject any usage of a forbidden synonym
   inside the wrong context's source, tests, or specs.
6. **Per-term H3 detail is required.** Each glossary file MUST contain a `## Term index`
   table (Term, Code identifier(s), Used in features columns) and a `## Terms in detail`
   section with one `### Term: <name>` H3 per row in the index. Each H3 MUST include:
   definition paragraph, `**Code identifier(s)**:` (with verified file path),
   `**Used in features**:`, and `**Forbidden synonyms in this context**:` (when
   applicable) and `**Related**:`.

## How this folder is consumed

- **`ayokoding-web` Gherkin features** under
  [`behavior/web/gherkin/`](../../behavior/web/gherkin/README.md) and
  [`behavior/api/gherkin/`](../../behavior/api/gherkin/README.md) — every term in scenario
  titles and step text comes from here.
- **`ayokoding-web` source** under
  [`apps/ayokoding-web/src/contexts/<bc>/`](../../../../../apps/ayokoding-web/) — type
  names, function names, component names match the `Code identifier(s)` column.
- **C4 component diagrams** under `components/` — labels match owned-term names.

## Glossary parity check

`rhino-cli ddd ul ayokoding` validates that every backtick-wrapped code identifier in each
glossary's `Code identifier(s)` column resolves to an actual symbol under the BC's `code:`
path. Stale identifiers from renamed types or deleted functions are caught here.

## Related

- [Bounded-context map](../bounded-context-map.md)
- [DDD Standards (platform-wide)](../../../../../docs/explanation/software-engineering/architecture/domain-driven-design-ddd/README.md)
- [BDD with DDD Standards](../../../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/bdd-with-ddd-standards.md)
- [ayokoding specs README](../../README.md)
- [ayokoding web component specs](../../components/web/README.md)
