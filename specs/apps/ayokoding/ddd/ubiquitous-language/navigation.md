# Ubiquitous Language — navigation

**Bounded context**: `navigation`
**Maintainer**: ayokoding-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Top-level navigation + tRPC navigation tree procedure. Application produces the nav tree
projection from content's filesystem hierarchy; presentation owns the sidebar, breadcrumb,
prev/next, and mobile drawer components.

## Term index

| Term         | Code identifier(s)                                                                          | Used in features                |
| ------------ | ------------------------------------------------------------------------------------------- | ------------------------------- |
| `Nav tree`   | `TreeNode` recursive type (apps/ayokoding-web/src/contexts/navigation/application/)         | `navigation/*.feature`          |
| `getTree`    | `getTree` procedure (apps/ayokoding-web/src/contexts/navigation/application/)               | `navigation/*.feature`          |
| `Sidebar`    | `Sidebar` component (apps/ayokoding-web/src/contexts/navigation/presentation/)              | `navigation/navigation.feature` |
| `Breadcrumb` | `Breadcrumb` component (apps/ayokoding-web/src/contexts/navigation/presentation/)           | `navigation/navigation.feature` |
| `Prev/next`  | `PrevNext` component (apps/ayokoding-web/src/contexts/navigation/presentation/)             | `navigation/navigation.feature` |
| `Section`    | `isSection` boolean on `TreeNode` (apps/ayokoding-web/src/contexts/navigation/application/) | `navigation/*.feature`          |

## Terms in detail

### Term: `Nav tree`

The hierarchical projection of content's filesystem layout — sections containing pages
or sub-sections. Each `TreeNode` carries `title`, `slug`, `weight`, `isSection`,
`children`. Returned by `getTree`. Children are sorted by `weight` ascending.

**Code identifier(s)**:
`TreeNode` recursive type and projection logic in
`apps/ayokoding-web/src/contexts/navigation/application/`.

**Used in features**: `navigation/*.feature`

**Forbidden synonyms in this context**: "Site map" (the BC term is `Nav tree`); "Menu
tree" (informal).

**Related**: `getTree`, `Section`

---

### Term: `getTree`

The tRPC procedure that returns the `Nav tree` for a `Locale`. Optionally scoped to a
`rootSlug` to return only that subtree. Owned by the navigation BC's application layer.

**Code identifier(s)**:
`getTree` procedure in
`apps/ayokoding-web/src/contexts/navigation/application/`.

**Used in features**: `navigation/navigation-api.feature`

**Forbidden synonyms in this context**: "Get nav tree" (use the procedure name verbatim).

**Related**: `Nav tree`

---

### Term: `Sidebar`

The presentation-layer React component that renders the `Nav tree` as a hierarchical
collapsible sidebar. Visible at desktop and laptop breakpoints; hidden behind the
`Hamburger menu` on mobile.

**Code identifier(s)**:
`Sidebar` (composed of `SidebarTree`) component in
`apps/ayokoding-web/src/contexts/navigation/presentation/`.

**Used in features**: `navigation/navigation.feature`

**Forbidden synonyms in this context**: "Side nav" (use `Sidebar`); "Tree view"
(too generic).

**Related**: `Nav tree`

---

### Term: `Breadcrumb`

The presentation-layer React component rendered above each page showing the path from
root to current `Page`. Each segment is a clickable link. Derived from the page's `Slug`
hierarchy in the `Nav tree`.

**Code identifier(s)**:
`Breadcrumb` component in
`apps/ayokoding-web/src/contexts/navigation/presentation/`.

**Used in features**: `navigation/navigation.feature`

**Forbidden synonyms in this context**: "Breadcrumbs" alone (use the singular UL term);
"Crumb trail" (informal).

**Related**: `Nav tree`

---

### Term: `Prev/next`

The presentation-layer React component rendered below each page providing prev and next
navigation links to siblings within the same section. Derived from the `Nav tree`.

**Code identifier(s)**:
`PrevNext` component in
`apps/ayokoding-web/src/contexts/navigation/presentation/`.

**Used in features**: `navigation/navigation.feature`

**Forbidden synonyms in this context**: "Pagination" (refers to numbered page lists, not
sibling navigation); "Pager".

**Related**: `Nav tree`

---

### Term: `Section`

A `TreeNode` with `isSection: true` — represents a folder under the content directory
(typically backed by an `_index.md` file). Sections contain children; non-section nodes
are leaf pages.

**Code identifier(s)**:
`isSection` boolean on `TreeNode` (in `apps/ayokoding-web/src/contexts/navigation/application/`).

**Used in features**: `navigation/*.feature`

**Forbidden synonyms in this context**: "Folder" (filesystem term — UL term is
`Section`); "Category" (too domain-flavoured for a docs site).

**Related**: `Nav tree`

---

## Forbidden synonyms

- "Site map" / "Menu tree" — use `Nav tree`.
- "Side nav" — use `Sidebar`.
- "Folder" — use `Section`.
