# Ubiquitous Language — personal-projects

**Bounded context**: `personal-projects`
**Maintainer**: wahidyankf-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

The personal-projects context owns the catalogue of the portfolio owner's side projects,
the filtering logic that narrows that catalogue by technology tag, and the page component
that presents the filtered results. It holds no data fetched from external services —
all project records are statically declared.

## Term index

| Term                      | Code identifier(s)                                                                      | Used in features                              |
| ------------------------- | --------------------------------------------------------------------------------------- | --------------------------------------------- |
| `Project`                 | `Project` (TypeScript type, application/projects.ts)                                    | `personal-projects/personal-projects.feature` |
| `ProjectFilter`           | `ProjectFilter` (TypeScript type), `filterProjects` (function, application/projects.ts) | `personal-projects/personal-projects.feature` |
| `PersonalProjectsContent` | `PersonalProjectsContent` (React component, presentation/PersonalProjectsContent.tsx)   | `personal-projects/personal-projects.feature` |

## Terms in detail

### Term: `Project`

A value object describing one personal side project. A `Project` record includes the
project name, a short description, the URL where the project can be found, and an ordered
list of technology tags (languages, frameworks, or tools) used to build it. Two `Project`
records with identical fields are considered equal; there is no mutable identity. All
`Project` records are declared statically in the application layer.

**Code identifier(s)**:
`Project` — the TypeScript type defining the shape of one personal project record
(`apps/wahidyankf-web/src/contexts/personal-projects/application/projects.ts`).

**Used in features**: `personal-projects/personal-projects.feature`

**Forbidden synonyms in this context**: "App" (ambiguous — a `Project` may be a library,
CLI tool, or website, not necessarily a user-facing app); "Work" (overloaded with
professional work history owned by the `cv` context — use `Project` for side projects);
"Portfolio item" (used informally — the domain term is `Project`).

**Related**: `ProjectFilter`, `PersonalProjectsContent`

---

### Term: `ProjectFilter`

A value representing the currently active technology-tag filter applied to the project
catalogue. A `ProjectFilter` is either the sentinel value `"all"` (meaning no filter is
active and every `Project` is shown) or a specific tag string that narrows the list to
projects whose tags include that value. The `filterProjects` function applies a
`ProjectFilter` to a list of `Project` records and returns the matching subset.

**Code identifier(s)**:
`ProjectFilter` — the TypeScript type for the active filter value, a string union of `"all"`
and any valid tag
(`apps/wahidyankf-web/src/contexts/personal-projects/application/projects.ts`).
`filterProjects` — the pure function that applies a `ProjectFilter` to a `Project` array
and returns the filtered result (same file).

**Used in features**: `personal-projects/personal-projects.feature`

**Forbidden synonyms in this context**: "Tag filter" (informal — the domain term is
`ProjectFilter`); "Category" (implies a strict taxonomy — tags are free-form strings, and
`ProjectFilter` selects by tag, not by hierarchical category); "Search" (the `search`
context owns text-based search; `ProjectFilter` is tag-based selection).

**Related**: `Project`, `PersonalProjectsContent`

---

### Term: `PersonalProjectsContent`

The root React component for the personal-projects page. It renders the full project
catalogue, exposes tag-based `ProjectFilter` controls so the user can narrow the visible
set, and displays the filtered list of `Project` cards. `PersonalProjectsContent` is a
presentational component; it delegates filtering logic to `filterProjects` and receives
all project data as props or from a static import.

**Code identifier(s)**:
`PersonalProjectsContent` — the root React component for the personal-projects page
(`apps/wahidyankf-web/src/contexts/personal-projects/presentation/PersonalProjectsContent.tsx`).

**Used in features**: `personal-projects/personal-projects.feature`

**Forbidden synonyms in this context**: "Projects page" (a routing concept — the domain
term for the component is `PersonalProjectsContent`); "Portfolio page" (overloaded with
the `home` context's `Portfolio` concept); "Side-projects list" (informal).

**Related**: `Project`, `ProjectFilter`

---

## Forbidden synonyms

- "App" for a `Project` — a project may be a library, CLI, or website. Use `Project`.
- "Work" — reserved for professional history in the `cv` context. Use `Project` for side
  projects.
- "Portfolio item" — informal. Use `Project`.
- "Tag filter" — informal. Use `ProjectFilter`.
- "Category" — implies a strict hierarchy. Tags are free-form; use `ProjectFilter`.
- "Search" in this context — text-based search belongs to the `search` context.
  `ProjectFilter` is tag-based selection only.
- "Projects page" or "Portfolio page" — routing labels. Use `PersonalProjectsContent` for
  the component concept.
