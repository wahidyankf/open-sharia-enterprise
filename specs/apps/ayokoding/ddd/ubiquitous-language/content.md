# Ubiquitous Language — content

**Bounded context**: `content`
**Maintainer**: ayokoding-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Content rendering + tRPC content procedures (`getBySlug`, `listChildren`). Application owns
procedures + DTOs; infrastructure owns filesystem + frontmatter adapters + markdown
pipeline; presentation owns article and content-list rendering.

## Term index

| Term                | Code identifier(s)                                                                   | Used in features                    |
| ------------------- | ------------------------------------------------------------------------------------ | ----------------------------------- |
| `Page`              | Frontmatter + body returned by `getBySlug`                                           | `content/*.feature`                 |
| `Slug`              | `slug` string parameter on procedures                                                | `content/*.feature`                 |
| `Frontmatter`       | YAML metadata block — `title`, `weight`, `date`, `description`, `tags`, `draft`      | `content/*.feature`                 |
| `getBySlug`         | `getBySlug` procedure (apps/ayokoding-web/src/contexts/content/application/)         | `content/*.feature`                 |
| `listChildren`      | `listChildren` procedure (apps/ayokoding-web/src/contexts/content/application/)      | `content/*.feature`                 |
| `Content reader`    | filesystem reader (apps/ayokoding-web/src/contexts/content/infrastructure/)          | `content/*.feature`                 |
| `Markdown renderer` | `MarkdownRenderer` component (apps/ayokoding-web/src/contexts/content/presentation/) | `content/content-rendering.feature` |

## Terms in detail

### Term: `Page`

A single content document — frontmatter + rendered HTML body. Returned by `getBySlug`. The
canonical aggregate root in this BC. Identity is the `Slug` (locale-prefixed path under the
content directory).

**Code identifier(s)**:
The DTO returned by `getBySlug` — defined in
`apps/ayokoding-web/src/contexts/content/application/`.

**Used in features**: `content/*.feature`

**Forbidden synonyms in this context**: "Article" (informal — `Page` is the BC term;
"Article" is reserved for human-facing UI labels in the `presentation` layer);
"Document" (too generic).

**Related**: `Slug`, `Frontmatter`, `getBySlug`

---

### Term: `Slug`

The unique identifier for a `Page` — a locale-prefixed path matching the file location
under the content directory (e.g. `en/programming/golang/getting-started`). Used by
`getBySlug` and `listChildren` to address pages and sections.

**Code identifier(s)**:
`slug` — Zod-validated string parameter on the content procedures
(`apps/ayokoding-web/src/contexts/content/application/`).

**Used in features**: `content/*.feature`

**Forbidden synonyms in this context**: "URL" (the slug is a path, not a URL); "Path"
(too generic — the BC term is `Slug`); "Page ID" (the slug IS the ID — using a separate
"page ID" term invents a synonym).

**Related**: `Page`, `getBySlug`, `listChildren`

---

### Term: `Frontmatter`

The YAML metadata block at the top of each markdown file. Required keys:
`title`, `weight`, `date`, `description`. Optional: `tags`, `draft`. Parsed by
`gray-matter` in the `Content reader`. Returned alongside the rendered HTML body in
`getBySlug`'s response.

**Code identifier(s)**:
Frontmatter Zod schema in
`apps/ayokoding-web/src/contexts/content/application/`.

**Used in features**: `content/*.feature`

**Forbidden synonyms in this context**: "Metadata" (too generic — the BC term is
`Frontmatter`); "Front-matter" (with hyphen — use unhyphenated form).

**Related**: `Page`

---

### Term: `getBySlug`

The tRPC procedure that fetches a single `Page` by its `Slug` and returns frontmatter +
rendered HTML. Throws `NOT_FOUND` if the slug does not resolve to a file. Owned by the
content BC's application layer.

**Code identifier(s)**:
`getBySlug` procedure in
`apps/ayokoding-web/src/contexts/content/application/`.

**Used in features**: `content/content-api.feature`

**Forbidden synonyms in this context**: "Get page" (informal — use the procedure name
verbatim); "Fetch by slug".

**Related**: `Slug`, `Page`

---

### Term: `listChildren`

The tRPC procedure that lists direct children of a section, sorted by weight ascending.
Used by sidebar navigation and section index pages.

**Code identifier(s)**:
`listChildren` procedure in
`apps/ayokoding-web/src/contexts/content/application/`.

**Used in features**: `content/content-api.feature`

**Forbidden synonyms in this context**: "List subpages" (use the procedure name);
"Get children".

**Related**: `Slug`

---

### Term: `Content reader`

The filesystem adapter that reads markdown files, parses frontmatter via `gray-matter`,
and renders the body via the `unified` (remark/rehype) pipeline with `shiki` syntax
highlighting. Lives in the content BC's infrastructure layer. The application procedures
consume the reader through a port.

**Code identifier(s)**:
filesystem reader implementations in
`apps/ayokoding-web/src/contexts/content/infrastructure/`.

**Used in features**: `content/*.feature`

**Forbidden synonyms in this context**: "Filesystem service" (too generic);
"Markdown loader" (the reader does more than load — it parses + renders).

**Related**: `Page`, `Frontmatter`

---

### Term: `Markdown renderer`

The presentation-layer React component that displays a rendered `Page`'s HTML body with
syntax-highlighted code blocks, callouts, mermaid diagrams, and other shortcodes. Pure
presentation; no fetching.

**Code identifier(s)**:
`MarkdownRenderer` component in
`apps/ayokoding-web/src/contexts/content/presentation/`.

**Used in features**: `content/content-rendering.feature`

**Forbidden synonyms in this context**: "Article view" (informal — the BC term is
`Markdown renderer`); "MDX renderer" (the project does not use MDX).

**Related**: `Page`

---

## Forbidden synonyms

- "Article" — reserved for human-facing UI labels; the BC term is `Page`.
- "Metadata" — use `Frontmatter`.
- "URL" / "Path" — use `Slug` for the canonical identifier.
