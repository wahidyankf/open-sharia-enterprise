# Ubiquitous Language — landing

**Bounded context**: `landing`
**Maintainer**: organiclever-web team
**Last reviewed**: 2026-05-09
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Marketing landing surface at `/` — hero, features, principles, rhythm demo, footer. No
domain logic; pure presentational content rendered as a Next.js Server Component.

## Term index

| Term                 | Code identifier(s)                                                                               | Used in features    |
| -------------------- | ------------------------------------------------------------------------------------------------ | ------------------- |
| `Landing page`       | `LandingPage` (component), `/` (Next.js route)                                                   | `landing/*.feature` |
| `Hero`               | `LandingHero` (component)                                                                        | `landing/*.feature` |
| `Features section`   | `LandingFeatures` (component)                                                                    | `landing/*.feature` |
| `Principles section` | `LandingPrinciples` (component)                                                                  | `landing/*.feature` |
| `Rhythm demo`        | `LandingRhythmDemo` (component)                                                                  | `landing/*.feature` |
| `Landing nav`        | `LandingNav` (component)                                                                         | `landing/*.feature` |
| `Landing footer`     | `LandingFooter` (component)                                                                      | `landing/*.feature` |
| `CTA`                | Button with label "Open the app" linking "/" to "/app/home" (string literal, no code identifier) | `landing/*.feature` |

## Terms in detail

### Term: `Landing page`

The route `/` rendering the OrganicLever marketing surface. Has no `/app` chrome (no
`TabBar`, no `SideNav`, no PGlite runtime). Rendered as a Next.js Server Component —
no client-side JavaScript required for the initial render. The only interactive element
is the `CTA` button linking to `/app/home`.

**Code identifier(s)**:
`LandingPage` — the root React component
(`apps/organiclever-web/src/contexts/landing/presentation/components/landing-page.tsx`).
`/` — the Next.js App Router route
(`apps/organiclever-web/src/app/page.tsx`).

**Used in features**: `landing/*.feature`

**Forbidden synonyms in this context**: "Home" (used inside `/app/home` for the
post-login dashboard, owned by `journal` + `app-shell` — the `Landing page` is the
marketing surface, not the app home); "Index page" (implementation term — the UL term
is `Landing page`).

**Related**: `Hero`, `CTA`, `Landing nav`

---

### Term: `Hero`

The primary above-the-fold block on the `Landing page`. Contains the main headline, a
supporting tagline, and the `CTA` button. Occupies the top viewport on first load.

**Code identifier(s)**:
`LandingHero` — the React component
(`apps/organiclever-web/src/contexts/landing/presentation/components/landing-hero.tsx`).

**Used in features**: `landing/*.feature`

**Forbidden synonyms in this context**: "Banner" (implies an announcement strip, not the
primary marketing block); "Splash" (native-app term for loading screens, not a web
marketing section).

**Related**: `Landing page`, `CTA`

---

### Term: `Features section`

The block on the `Landing page` listing OrganicLever's product capabilities. Presents
features (workout logging, journal, progress tracking) in a scannable grid or list
format.

**Code identifier(s)**:
`LandingFeatures` — the React component
(`apps/organiclever-web/src/contexts/landing/presentation/components/landing-features.tsx`).

**Used in features**: `landing/*.feature`

**Forbidden synonyms in this context**: "Benefits section" (marketing copy variant — the
UL term is `Features section`); "Highlights" (informal).

**Related**: `Landing page`, `Principles section`

---

### Term: `Principles section`

The block describing the OrganicLever approach: functional (no fluff), local-first (data
stays on device), and measured (progress over perfection). Communicates product values
to the visitor.

**Code identifier(s)**:
`LandingPrinciples` — the React component
(`apps/organiclever-web/src/contexts/landing/presentation/components/landing-principles.tsx`).

**Used in features**: `landing/*.feature`

**Forbidden synonyms in this context**: "Values section" (acceptable in copy but the UL
term is `Principles section`); "Philosophy" (too abstract for Gherkin step text).

**Related**: `Landing page`, `Features section`

---

### Term: `Rhythm demo`

An animated tile on the `Landing page` illustrating the daily rhythm metaphor — a
representative widget showing what a logged workout week looks like. Purely decorative /
illustrative; reads no live PGlite data.

**Code identifier(s)**:
`LandingRhythmDemo` — the React component
(`apps/organiclever-web/src/contexts/landing/presentation/components/landing-rhythm-demo.tsx`).

**Used in features**: `landing/*.feature`

**Forbidden synonyms in this context**: "Demo widget" (too generic — the UL term is
`Rhythm demo`); "Preview" (implies live data; this is a static illustration).

**Related**: `Landing page`

---

### Term: `Landing nav`

The top navigation bar visible on the `Landing page` at `/`. Distinct from the
`app-shell` `SideNav`/`TabBar` inside `/app/**`. Contains the brand mark and a link to
the app.

**Code identifier(s)**:
`LandingNav` — the React component
(`apps/organiclever-web/src/contexts/landing/presentation/components/landing-nav.tsx`).

**Used in features**: `landing/*.feature`

**Forbidden synonyms in this context**: "Header" alone (ambiguous with the Next.js HTML
`<header>` element — always qualify as `LandingNav` to keep it distinct from
`app-shell`'s chrome).

**Related**: `Landing page`, `Landing footer`

---

### Term: `Landing footer`

The closing section of the `Landing page` with copyright, links, and metadata.

**Code identifier(s)**:
`LandingFooter` — the React component
(`apps/organiclever-web/src/contexts/landing/presentation/components/landing-footer.tsx`).

**Used in features**: `landing/*.feature`

**Forbidden synonyms in this context**: "Footer" alone (qualify as `LandingFooter` to
distinguish from any footer chrome inside `/app/**`).

**Related**: `Landing page`, `Landing nav`

---

### Term: `CTA`

The "call to action" — the primary `Open the app` button on the `Landing page` that
links the visitor from `/` to `/app/home`. The `CTA` is the entry point from the
marketing surface into the OrganicLever life journal. Its label is the literal string
"Open the app".

**Code identifier(s)**:
Button with label `"Open the app"` rendered inside `LandingHero`
(`apps/organiclever-web/src/contexts/landing/presentation/components/landing-hero.tsx`).

**Used in features**: `landing/*.feature`

**Forbidden synonyms in this context**: "Sign up button" (no auth today — it links
directly to the app); "Get started" (the button label is specifically "Open the app",
not "Get started").

**Related**: `Hero`, `Landing page`

---

## Forbidden synonyms

- "App" — `app-shell` owns the chrome inside `/app/**`. The landing page is not the app;
  prefer "landing page".
- "Home" — used inside `/app/home` for the post-login dashboard, owned by `journal` +
  `app-shell`. The landing page is the marketing surface, not "home".
- "Header" / "footer" alone — qualify as `LandingNav`/`LandingFooter` to keep them
  distinct from `app-shell`'s header chrome.
