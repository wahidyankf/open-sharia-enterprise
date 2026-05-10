# Ubiquitous Language — cv

**Bounded context**: `cv`
**Maintainer**: wahidyankf-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

The CV context owns the canonical work-history data, skill-duration aggregations, and
markdown rendering utilities. It is the authoritative source of professional experience
entries consumed by both the CV page and the home page's featured-skills section.

## Term index

| Term            | Code identifier(s)                                                                                                                                                              | Used in features |
| --------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------- |
| `CVEntry`       | `CVEntry` (TypeScript type, application/data.ts)                                                                                                                                | `cv/cv.feature`  |
| `WorkHistory`   | `cvData` (constant array of `CVEntry`, application/data.ts)                                                                                                                     | `cv/cv.feature`  |
| `SkillDuration` | `getTopSkillsLastFiveYears` (function), `getTopLanguagesLastFiveYears` (function), `getTopFrameworksLastFiveYears` (function), `formatDuration` (function, application/data.ts) | `cv/cv.feature`  |

## Terms in detail

### Term: `CVEntry`

A single work-history record describing one role or engagement. A `CVEntry` captures the
employer name, job title, start and end dates, a list of responsibilities written in
markdown, and the set of skills, languages, and frameworks used during that engagement.
`CVEntry` is a value object — two entries with identical field values are considered equal;
there is no mutable identity.

**Code identifier(s)**:
`CVEntry` — the TypeScript type defining the shape of one work-history record
(`apps/wahidyankf-web/src/contexts/cv/application/data.ts`).

**Used in features**: `cv/cv.feature`

**Forbidden synonyms in this context**: "Job" (ambiguous — a `CVEntry` may represent a
contract engagement, not just a permanent job); "Experience item" (informal — the domain
term is `CVEntry`); "Resume entry" (use `CVEntry`; "resume" is a synonym for CV only in
some locales and creates confusion).

**Related**: `WorkHistory`, `SkillDuration`

---

### Term: `WorkHistory`

The complete ordered collection of `CVEntry` records representing the portfolio owner's
professional timeline. `WorkHistory` is the canonical data set from which all CV-page
views and the home page's featured-skills aggregations are derived. It is a static
constant defined once and never mutated at runtime.

**Code identifier(s)**:
`cvData` — the exported constant array of `CVEntry` records constituting the full work
history (`apps/wahidyankf-web/src/contexts/cv/application/data.ts`).

**Used in features**: `cv/cv.feature`

**Forbidden synonyms in this context**: "Job list" (informal — the domain term is
`WorkHistory`); "Entries" alone (too vague without the `Work` qualifier); "Data" (the
variable is named `cvData` for implementation reasons, but the domain concept is
`WorkHistory`).

**Related**: `CVEntry`, `SkillDuration`

---

### Term: `SkillDuration`

A derived value pairing a skill (or language or framework) name with its accumulated usage
duration in months, calculated by summing the overlapping months across all `CVEntry`
records in which that skill appears within the last five years. `SkillDuration` values are
produced by the three query functions (`getTopSkillsLastFiveYears`,
`getTopLanguagesLastFiveYears`, `getTopFrameworksLastFiveYears`) and formatted for display
by `formatDuration`. The `home` context consumes these results as `FeaturedSkill` items.

**Code identifier(s)**:
`getTopSkillsLastFiveYears` — returns ranked `SkillDuration` values for general skills
(`apps/wahidyankf-web/src/contexts/cv/application/data.ts`).
`getTopLanguagesLastFiveYears` — returns ranked `SkillDuration` values for programming
languages (same file).
`getTopFrameworksLastFiveYears` — returns ranked `SkillDuration` values for frameworks
(same file).
`formatDuration` — formats a duration in months into a human-readable string (same file).
`parseMarkdownLinks` — parses markdown link syntax in responsibility text into structured
link objects (`apps/wahidyankf-web/src/contexts/cv/application/markdown.tsx`).

**Used in features**: `cv/cv.feature`

**Forbidden synonyms in this context**: "Skill count" (counts occurrences of entries, not
accumulated months — `SkillDuration` is time-based); "Experience level" (a subjective
assessment, not a derived duration); "Proficiency" (no proficiency model exists in this
context).

**Related**: `CVEntry`, `WorkHistory`

---

## Forbidden synonyms

- "Job" for a `CVEntry` — a `CVEntry` covers contracts and engagements, not only permanent
  jobs.
- "Resume entry" — use `CVEntry`. "Resume" is locale-dependent and conflicts with the
  canonical term CV.
- "Job list" or "Entries" alone — use `WorkHistory` for the full collection.
- "Data" as a domain term — `cvData` is the implementation identifier; `WorkHistory` is
  the domain concept.
- "Skill count" — use `SkillDuration`. Counts are not durations.
- "Experience level" or "Proficiency" — no such model exists in the `cv` context.
