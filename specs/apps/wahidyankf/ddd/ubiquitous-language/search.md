# Ubiquitous Language — search

**Bounded context**: `search`
**Maintainer**: wahidyankf-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

The search context owns the generic text-filtering capability used across the site,
the representation of the user's current query string, and the matched result set
returned after filtering. It provides a reusable filter function consumed by other
contexts; it holds no persistent state and fetches no external data.

## Term index

| Term           | Code identifier(s)                                      | Used in features        |
| -------------- | ------------------------------------------------------- | ----------------------- |
| `SearchTerm`   | `SearchTerm` (TypeScript type, application/search.ts)   | `search/search.feature` |
| `filterItems`  | `filterItems` (function, application/search.ts)         | `search/search.feature` |
| `SearchResult` | `SearchResult` (TypeScript type, application/search.ts) | `search/search.feature` |

## Terms in detail

### Term: `SearchTerm`

The string value entered by the user to filter a list of items. A `SearchTerm` is a
plain, unstructured string — it carries no metadata about which fields to match against
or how to weight results. An empty `SearchTerm` means no filter is active and all items
pass through. The `search` context treats `SearchTerm` as an immutable value; each
keystroke produces a new `SearchTerm` rather than mutating the previous one.

**Code identifier(s)**:
`SearchTerm` — the TypeScript type alias for the user's current query string
(`apps/wahidyankf-web/src/contexts/search/application/search.ts`).

**Used in features**: `search/search.feature`

**Forbidden synonyms in this context**: "Query" (overloaded in database contexts — the
domain term is `SearchTerm`); "Input" (refers to the HTML element, not the domain value);
"Filter string" (informal — use `SearchTerm`); "Keyword" (implies word-boundary matching
semantics that are not guaranteed by this context's implementation).

**Related**: `filterItems`, `SearchResult`

---

### Term: `filterItems`

The generic pure function that applies a `SearchTerm` to an array of items and returns
the subset whose text representation contains the term (case-insensitive substring match).
`filterItems` is parameterised over the item type — it accepts a list of any type `T`
and a string-extraction function so it can operate on `CVEntry` records, `Project`
records, or any other collection without coupling to those types. When the `SearchTerm`
is empty, `filterItems` returns the full input array unchanged.

**Code identifier(s)**:
`filterItems` — the generic filter function
(`apps/wahidyankf-web/src/contexts/search/application/search.ts`).

**Used in features**: `search/search.feature`

**Forbidden synonyms in this context**: "Search function" (too vague — the domain term
is `filterItems`); "Filter" alone (collides with the `ProjectFilter` concept in the
`personal-projects` context — always qualify as `filterItems` in cross-context
discussion).

**Related**: `SearchTerm`, `SearchResult`

---

### Term: `SearchResult`

The ordered array of items returned by `filterItems` after applying a `SearchTerm`.
A `SearchResult` is a value — it is computed fresh each time the `SearchTerm` changes
and is never cached or stored. When the `SearchTerm` is empty, the `SearchResult`
equals the full input collection. The `search` context does not rank or score results;
the original order of the input array is preserved in the output.

**Code identifier(s)**:
`SearchResult` — the TypeScript type representing the array of items that matched the
`SearchTerm`
(`apps/wahidyankf-web/src/contexts/search/application/search.ts`).

**Used in features**: `search/search.feature`

**Forbidden synonyms in this context**: "Hits" (implies a relevance-scoring engine —
this context performs simple substring matching with no scoring); "Matches" (informal —
the domain term is `SearchResult`); "Filtered list" (implementation description — use
`SearchResult`).

**Related**: `SearchTerm`, `filterItems`

---

## Forbidden synonyms

- "Query" — overloaded in database contexts. Use `SearchTerm`.
- "Input" — refers to the HTML element. Use `SearchTerm` for the domain value.
- "Filter string" or "Keyword" — informal or semantically loaded. Use `SearchTerm`.
- "Search function" — too vague. Use `filterItems`.
- "Filter" alone in cross-context discussion — collides with `ProjectFilter` in the
  `personal-projects` context. Always say `filterItems`.
- "Hits" — implies relevance scoring. Use `SearchResult`.
- "Matches" or "Filtered list" — informal or implementation-level. Use `SearchResult`.
