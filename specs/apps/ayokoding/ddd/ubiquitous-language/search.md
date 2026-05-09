# Ubiquitous Language — search

**Bounded context**: `search`
**Maintainer**: ayokoding-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Search backend (tRPC) + UI. Application interprets queries and scores results;
infrastructure owns the FlexSearch index; presentation owns the search dialog and
results dropdown.

## Term index

| Term            | Code identifier(s)                                                                          | Used in features        |
| --------------- | ------------------------------------------------------------------------------------------- | ----------------------- |
| `Query`         | `query` string parameter on `query` procedure                                               | `search/*.feature`      |
| `Search index`  | FlexSearch index in infrastructure (apps/ayokoding-web/src/contexts/search/infrastructure/) | `search/*.feature`      |
| `query`         | `query` procedure (apps/ayokoding-web/src/contexts/search/application/)                     | `search/*.feature`      |
| `Search dialog` | `SearchDialog` component (apps/ayokoding-web/src/contexts/search/presentation/)             | `search/search.feature` |
| `Result`        | Per-hit DTO returned by `query` — title + slug + snippet                                    | `search/*.feature`      |

## Terms in detail

### Term: `Query`

The user-supplied search string passed to the `query` procedure. Trimmed and length-checked
in the application layer; an empty trimmed query throws `BAD_REQUEST`.

**Code identifier(s)**:
`query` — Zod-validated string parameter on the `query` procedure
(`apps/ayokoding-web/src/contexts/search/application/`).

**Used in features**: `search/*.feature`

**Forbidden synonyms in this context**: "Search term" (use `Query`); "Keyword" (too
SEO-flavoured); "Search input" (refers to the UI control, not the value).

**Related**: `query`, `Result`

---

### Term: `Search index`

The in-memory FlexSearch index built per-locale from all content metadata at startup.
Indexes title and stripped-markdown body. Owned by the search BC's infrastructure layer.
The application's `query` procedure consumes the index through a port.

**Code identifier(s)**:
FlexSearch index implementation in
`apps/ayokoding-web/src/contexts/search/infrastructure/`.

**Used in features**: `search/*.feature`

**Forbidden synonyms in this context**: "Index" alone (overloaded — qualify as `Search
index`); "Search store" (FlexSearch is an index, not a store).

**Related**: `query`

---

### Term: `query`

The tRPC procedure that runs a `Query` against the locale-scoped `Search index` and returns
ranked `Result`s. Owned by the search BC's application layer.

**Code identifier(s)**:
`query` procedure in
`apps/ayokoding-web/src/contexts/search/application/`.

**Used in features**: `search/search-api.feature`

**Forbidden synonyms in this context**: "Search procedure" (use the procedure name);
"Find" (too generic).

**Related**: `Query`, `Result`

---

### Term: `Search dialog`

The presentation-layer React component — a modal overlay containing the search input and
results dropdown. Triggered from the `Header`'s search button. Calls the `query` procedure
on input.

**Code identifier(s)**:
`SearchDialog` component in
`apps/ayokoding-web/src/contexts/search/presentation/`.

**Used in features**: `search/search.feature`

**Forbidden synonyms in this context**: "Search modal" (the BC term is `Search dialog`);
"Search overlay".

**Related**: `Result`

---

### Term: `Result`

A single hit returned by the `query` procedure — `title`, `slug`, and a short
context-aware snippet excerpted from the indexed body. Rendered in the `Search dialog`'s
results list as a clickable link to the page's `Slug`.

**Code identifier(s)**:
Per-hit DTO returned by the `query` procedure
(`apps/ayokoding-web/src/contexts/search/application/`).

**Used in features**: `search/*.feature`

**Forbidden synonyms in this context**: "Hit" (informal — use `Result`); "Match".

**Related**: `Query`, `query`

---

## Forbidden synonyms

- "Search term" / "Keyword" — use `Query`.
- "Index" alone — use `Search index`.
- "Hit" / "Match" — use `Result`.
