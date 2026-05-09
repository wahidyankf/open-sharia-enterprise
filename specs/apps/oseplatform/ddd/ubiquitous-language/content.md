# Ubiquitous Language — content

**Bounded context**: `content`
**Maintainer**: oseplatform-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Content retrieval and rendering — markdown source files, tRPC procedures, DTOs, and rendering
components. Source of truth for `search`, `rss-feed`, and `seo`.

## Term index

| Term              | Code identifier(s)                                                              | Used in features                    |
| ----------------- | ------------------------------------------------------------------------------- | ----------------------------------- |
| `Article`         | `ContentMeta`, `Heading`, `PageLink`                                            | `content/content-retrieval.feature` |
| `Content source`  | `ContentRepository`, `FileSystemContentRepository`, `InMemoryContentRepository` | `content/content-retrieval.feature` |
| `Slug`            | `slug`, `getBySlug`                                                             | `content/content-retrieval.feature` |
| `Frontmatter`     | `ContentMeta`, `parseMarkdown`                                                  | `content/content-retrieval.feature` |
| `Render pipeline` | `parseMarkdown`, `stripMarkdown`                                                | `content/content-retrieval.feature` |
| `Update`          | `listUpdates`, `ContentMeta`                                                    | `content/content-retrieval.feature` |

## Terms in detail

### Term: `Article`

The canonical retrieval shape — metadata plus rendered HTML, headings, and optional prev/next
navigation links.

**Code identifier(s)**: `ContentMeta`, `Heading`, `PageLink`
(`src/contexts/content/application/types.ts`).

**Used in features**: `content/content-retrieval.feature`

**Forbidden synonyms in this context**: "post"; "page".

**Related**: `Slug`, `Frontmatter`, `Update`

---

### Term: `Content source`

The abstraction for reading raw markdown — a port with filesystem and in-memory adapters.

**Code identifier(s)**: `ContentRepository`, `FileSystemContentRepository`,
`InMemoryContentRepository` (`src/contexts/content/infrastructure/`).

**Used in features**: `content/content-retrieval.feature`

**Forbidden synonyms in this context**: "data source"; "repo".

**Related**: `Article`, `Frontmatter`

---

### Term: `Slug`

The URL-safe identifier for one `Article`. Input parameter to `getBySlug`.

**Code identifier(s)**: `slug` (field on `ContentMeta`); `getBySlug` (procedure in
`src/contexts/content/application/router.ts`).

**Used in features**: `content/content-retrieval.feature`

**Forbidden synonyms in this context**: "id"; "key".

**Related**: `Article`, `Update`

---

### Term: `Frontmatter`

YAML metadata block at the top of every markdown file. Parsed into `ContentMeta` fields.

**Code identifier(s)**: `ContentMeta` (`src/contexts/content/application/types.ts`);
`parseMarkdown` (`src/contexts/content/infrastructure/parser.ts`).

**Used in features**: `content/content-retrieval.feature`

**Forbidden synonyms in this context**: "metadata" (used by `seo`); "header".

**Related**: `Article`, `Slug`

---

### Term: `Render pipeline`

Unified-based markdown → HTML transformation: gray-matter → remark → rehype → shiki → headings.

**Code identifier(s)**: `parseMarkdown` (`src/contexts/content/infrastructure/parser.ts`);
`stripMarkdown` (`src/contexts/content/infrastructure/reader.ts`).

**Used in features**: `content/content-retrieval.feature`

**Forbidden synonyms in this context**: "renderer"; "compiler".

**Related**: `Article`

---

### Term: `Update`

A subset of `Article` where `category` equals `"updates"`. Feeds RSS and sitemap.

**Code identifier(s)**: `listUpdates` (procedure in
`src/contexts/content/application/router.ts`); `ContentMeta`
(`src/contexts/content/application/types.ts`).

**Used in features**: `content/content-retrieval.feature`

**Forbidden synonyms in this context**: "post"; "release note".

**Related**: `Article`, `Slug`
