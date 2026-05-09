# Ubiquitous Language — search

**Bounded context**: `search`
**Maintainer**: oseplatform-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Full-text search over indexed content. Application interprets the query; infrastructure owns
the FlexSearch index; presentation renders the dialog. Customer of `content`.

## Term index

| Term           | Code identifier(s)                  | Used in features        |
| -------------- | ----------------------------------- | ----------------------- |
| `Query`        | `searchQuerySchema`, `searchRouter` | `search/search.feature` |
| `Search index` | `SearchService`                     | `search/search.feature` |
| `Result entry` | `SearchResult`                      | `search/search.feature` |
| `Score`        | `SearchService`                     | `search/search.feature` |
| `Snippet`      | `SearchResult`                      | `search/search.feature` |

## Terms in detail

### Term: `Query`

Free-text input plus optional `limit`. Validated via `searchQuerySchema`.

**Code identifier(s)**: `searchQuerySchema` (`src/contexts/search/application/schemas.ts`);
`searchRouter` (`src/contexts/search/application/router.ts`).

**Used in features**: `search/search.feature`

**Forbidden synonyms in this context**: "find term"; "lookup string".

**Related**: `Search index`, `Result entry`

---

### Term: `Search index`

In-memory FlexSearch document index built at startup. Owned by `SearchService`.

**Code identifier(s)**: `SearchService` (`src/contexts/search/application/service.ts`).

**Used in features**: `search/search.feature`

**Forbidden synonyms in this context**: "DB"; "store".

**Related**: `Query`, `Result entry`

---

### Term: `Result entry`

One match returned by the search query — title, slug, and excerpt.

**Code identifier(s)**: `SearchResult` (`src/contexts/content/application/types.ts`).

**Used in features**: `search/search.feature`

**Forbidden synonyms in this context**: "hit"; "match".

**Related**: `Query`, `Score`, `Snippet`

---

### Term: `Score`

The relative ranking of one `Result entry`. Today implicit via FlexSearch ordering.

**Code identifier(s)**: `SearchService` (ordering in search results;
`src/contexts/search/application/service.ts`).

**Used in features**: `search/search.feature`

**Forbidden synonyms in this context**: "ranking"; "weight".

**Related**: `Result entry`

---

### Term: `Snippet`

Short window of plain text around the match, shown in the results dropdown.

**Code identifier(s)**: `SearchResult` (`excerpt` field;
`src/contexts/content/application/types.ts`).

**Used in features**: `search/search.feature`

**Forbidden synonyms in this context**: "preview"; "summary".

**Related**: `Result entry`
