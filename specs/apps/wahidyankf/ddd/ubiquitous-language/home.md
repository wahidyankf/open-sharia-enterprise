# Ubiquitous Language — home

**Bounded context**: `home`
**Maintainer**: wahidyankf-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

The landing page context that introduces the portfolio owner, surfaces a curated set of
featured skills, and provides entry points into the CV and Personal Projects sections.
Owns no persistent data — content is statically derived from the `cv` context's data layer.

## Term index

| Term            | Code identifier(s)                                              | Used in features    |
| --------------- | --------------------------------------------------------------- | ------------------- |
| `HomeContent`   | `HomeContent` (React component, presentation/HomeContent.tsx)   | `home/home.feature` |
| `Portfolio`     | `Portfolio` (TypeScript type, presentation/HomeContent.tsx)     | `home/home.feature` |
| `FeaturedSkill` | `FeaturedSkill` (TypeScript type, presentation/HomeContent.tsx) | `home/home.feature` |

## Terms in detail

### Term: `HomeContent`

The root React component for the landing page. Renders the portfolio owner's introduction,
a curated list of `FeaturedSkill` items derived from the `cv` context's top-skills query,
and call-to-action links to the CV and Personal Projects sections. `HomeContent` is a
presentational component — it receives all data as props and performs no data fetching
itself.

**Code identifier(s)**:
`HomeContent` — the root React component for the home page presentation layer
(`apps/wahidyankf-web/src/contexts/home/presentation/HomeContent.tsx`).

**Used in features**: `home/home.feature`

**Forbidden synonyms in this context**: "Landing page" (a marketing term — the domain term
is `HomeContent`); "Hero" (refers to a visual sub-section, not the full page component);
"Index page" (a file-system concept, not a domain concept).

**Related**: `Portfolio`, `FeaturedSkill`

---

### Term: `Portfolio`

The aggregate value representing the portfolio owner's public professional identity as
surfaced on the home page. A `Portfolio` is a read-only snapshot composed from the owner's
name, a short bio, and a ranked list of `FeaturedSkill` items. It is not stored
independently — it is derived on demand from `cv` data.

**Code identifier(s)**:
`Portfolio` — the TypeScript type describing the home-page portfolio snapshot
(`apps/wahidyankf-web/src/contexts/home/application/portfolio.ts`).

**Used in features**: `home/home.feature`

**Forbidden synonyms in this context**: "Profile" (used generically across many domains —
`Portfolio` is specific to the public professional presentation on the home page);
"Resume" (a synonym for CV in the `cv` context — do not use in `home`); "About"
(a navigation label, not a domain concept).

**Related**: `HomeContent`, `FeaturedSkill`

---

### Term: `FeaturedSkill`

A single skill highlighted on the home page landing section, consisting of a skill name and
an accumulated usage duration in months. `FeaturedSkill` items are the top-N skills ranked
by total months of usage, derived by calling the `cv` context's
`getTopSkillsLastFiveYears` query. The `home` context does not define the ranking logic —
it consumes the ranked list.

**Code identifier(s)**:
`FeaturedSkill` — the TypeScript type for a single highlighted skill entry on the home page
(`apps/wahidyankf-web/src/contexts/home/application/portfolio.ts`).

**Used in features**: `home/home.feature`

**Forbidden synonyms in this context**: "Top skill" (informal label — the domain term is
`FeaturedSkill`); "Skill badge" (a visual presentation detail, not a domain concept);
"Highlight" (too vague).

**Related**: `Portfolio`, `HomeContent`

---

## Forbidden synonyms

- "Landing page" — marketing term. Use `HomeContent` for the component and `Portfolio` for
  the data aggregate.
- "Hero" — refers to a visual sub-section, not the full page. Do not use as a domain term.
- "Index page" — a file-system concept. Use `HomeContent`.
- "Profile" — too generic. Use `Portfolio` for the public professional snapshot on the home
  page.
- "Resume" — a `cv` context synonym; forbidden in the `home` context to avoid confusion.
- "Top skill" — informal. Use `FeaturedSkill`.
- "Skill badge" — a presentation implementation detail. Use `FeaturedSkill`.
