# Ubiquitous Language — settings

**Bounded context**: `settings`
**Maintainer**: organiclever-web team
**Last reviewed**: 2026-05-09
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

User-local preferences (theme, locale, rest-timer duration) persisted in PGlite. Owns
the invariant "exactly one preferences row per user" and the dark-mode toggle.

## Term index

| Term            | Code identifier(s)                                                           | Used in features     |
| --------------- | ---------------------------------------------------------------------------- | -------------------- |
| `Preferences`   | `AppSettings` (TS type), `getSettings` / `saveSettings` (use-case fns)       | `settings/*.feature` |
| `Theme`         | `darkMode: boolean` field on `AppSettings`                                   | `settings/*.feature` |
| `Locale`        | `lang: Lang` field on `AppSettings`, `Lang` (values: "en" or "id") (TS type) | `settings/*.feature` |
| `Units`         | (not yet implemented — placeholder for future weight/length units)           | `settings/*.feature` |
| `Settings page` | `SettingsScreen` (component), `app/settings` (route segment)                 | `settings/*.feature` |
| `Reset data`    | (use-case fn, not yet implemented)                                           | `settings/*.feature` |
| `Export data`   | (use-case fn, not yet implemented)                                           | `settings/*.feature` |

## Terms in detail

### Term: `Preferences`

The singleton aggregate holding all user-local settings. One row per user in the PGlite
`app_settings` table. Created on first load via the `seedIfEmpty` bootstrap (owned by
`app-shell`). Updated via `saveSettings` on any user change. Read by `getSettings` on
app startup and by the `workout-session` context (which needs `restSeconds` to calculate
rest durations).

**Code identifier(s)**:
`AppSettings` — the aggregate interface: `{ name: string; restSeconds: RestSeconds;
darkMode: boolean; lang: Lang }`
(`apps/organiclever-web/src/contexts/settings/domain/types.ts`).
`getSettings` — Effect-based use-case returning `AppSettings`
(`apps/organiclever-web/src/contexts/settings/application/index.ts`).
`saveSettings` — Effect-based use-case persisting a full `AppSettings` record (same
file).

**Persisted as**: One row in the PGlite `app_settings` table.

**Used in features**: `settings/*.feature`

**Forbidden synonyms in this context**: "Configuration" (used by `app-shell` to mean
runtime configuration such as i18n keys and design tokens — inside `settings`, prefer
"preferences"); "Profile" (implies identity, not preferences).

**Related**: `Theme`, `Locale`, `Settings page`

---

### Term: `Theme`

The user's chosen visual theme. Stored as `darkMode: boolean` on `AppSettings`: `true`
= dark mode, `false` = light mode. Applied by the `app-shell` bootstrap by setting
`data-theme="dark"` on `<html>`. No "system follows OS" mode today; the user toggles
explicitly.

**Code identifier(s)**:
`darkMode: boolean` — the field on `AppSettings`
(`apps/organiclever-web/src/contexts/settings/domain/types.ts`).

**Used in features**: `settings/*.feature`

**Forbidden synonyms in this context**: "Theme" inside `app-shell` (the shell consumes
the resolved theme as a CSS attribute; only `settings` owns the user-chosen value);
"Color scheme" (a browser/OS concept — the domain word is `Theme`).

**Related**: `Preferences`, `Settings page`

---

### Term: `Locale`

The user's chosen language for the app UI. A BCP-47 language tag, constrained to the
`Lang` enum: `"en"` (English, default) or `"id"` (Bahasa Indonesia). Governs i18n key
resolution in `app-shell`. Stored as `lang: Lang` on `AppSettings`.

**Code identifier(s)**:
`Lang` — the TypeScript union type `"en" | "id"`
(`apps/organiclever-web/src/contexts/settings/domain/types.ts`).
`lang: Lang` — the field on `AppSettings` (same file).

**Used in features**: `settings/*.feature`

**Forbidden synonyms in this context**: "Language" (informal — the domain term is
`Locale`); "i18n setting" (an implementation detail, not a domain term).

**Related**: `Preferences`

---

### Term: `Units`

The measurement units applied to weights and lengths in the UI (e.g., `kg`/`lb`,
`cm`/`in`). **Not yet implemented.** Placeholder in the glossary and `AppSettings`
design for a future iteration. When implemented, it will be a value-typed enum on
`AppSettings` and consumed by journal and stats presentation layers.

**Code identifier(s)**:
Not yet implemented — no type or field in current codebase.

**Used in features**: `settings/*.feature`

**Forbidden synonyms in this context**: "Format" (too broad — `Units` specifically means
measurement units, not number format or date format).

**Related**: `Preferences`

---

### Term: `Settings page`

The route `/app/settings` rendering the preferences form. Lets the user change `Theme`,
`Locale`, and rest-timer duration. Changes write via `saveSettings` immediately (no
separate "save" button in the current UI — form fields update preferences on change).

**Code identifier(s)**:
`SettingsScreen` — the React component
(`apps/organiclever-web/src/contexts/settings/presentation/components/settings-screen.tsx`).
`app/settings` — the Next.js route segment
(`apps/organiclever-web/src/app/app/settings/page.tsx`).

**Used in features**: `settings/*.feature`

**Forbidden synonyms in this context**: "Preferences page" (acceptable informally, but
Gherkin steps use `Settings page` to match the route name).

**Related**: `Preferences`, `Reset data`, `Export data`

---

### Term: `Reset data`

The user action that wipes all PGlite stores (journal entries, routines, preferences) and
returns the app to the empty state — as if the user opened it for the first time.
**Not yet implemented.** When implemented, it will call a cross-context use-case
that clears all three stores and triggers a re-seed via `app-shell`.

**Code identifier(s)**:
Not yet implemented — no function in current codebase.

**Used in features**: `settings/*.feature`

**Forbidden synonyms in this context**: "Clear data" (informal variant — the UL term is
`Reset data`); "Factory reset" (hardware connotation).

**Related**: `Settings page`, `Export data`

---

### Term: `Export data`

The action that serializes all PGlite stores into a downloadable JSON file, letting the
user back up or migrate their data. **Not yet implemented.** When implemented, it
will be a pure domain use-case reading all `JournalEvent`s, `Routine`s, and
`AppSettings` into a versioned export envelope.

**Code identifier(s)**:
Not yet implemented — no function in current codebase.

**Used in features**: `settings/*.feature`

**Forbidden synonyms in this context**: "Download data" (describes the mechanism, not
the intent — the UL term is `Export data`); "Backup" (implies scheduled/automatic,
while `Export data` is an explicit one-shot user action).

**Related**: `Settings page`, `Reset data`

---

## Forbidden synonyms

- "Configuration" — used by `app-shell` to mean runtime configuration (i18n keys, design
  tokens). Inside `settings`, prefer "preferences".
- "Theme" inside `app-shell` — `app-shell` consumes the resolved theme as a CSS class;
  only `settings` owns the user-chosen value.
