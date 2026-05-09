# Ubiquitous Language — landing

**Bounded context**: `landing`
**Maintainer**: oseplatform-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Marketing landing surface at `/` — hero block, social/author identity, calls-to-action.
Pure presentation; renders inside `app-shell` chrome.

## Term index

| Term            | Code identifier(s) | Used in features               |
| --------------- | ------------------ | ------------------------------ |
| `Hero`          | `Hero`             | `landing/landing-page.feature` |
| `Section block` | `Hero`             | `landing/landing-page.feature` |
| `CTA`           | `SocialIcons`      | `landing/landing-page.feature` |
| `Social icons`  | `SocialIcons`      | `landing/landing-page.feature` |

## Terms in detail

### Term: `Hero`

The top-of-page block below the `Header`. Carries headline, sub-headline, and primary CTA.

**Code identifier(s)**: `Hero` (`src/contexts/landing/presentation/hero.tsx`).

**Used in features**: `landing/landing-page.feature`

**Forbidden synonyms in this context**: "banner"; "splash".

**Related**: `Section block`, `CTA`

---

### Term: `Section block`

A vertically stacked content region of the landing page. `Hero` is one section block.

**Code identifier(s)**: `Hero` (`src/contexts/landing/presentation/hero.tsx`).

**Used in features**: `landing/landing-page.feature`

**Forbidden synonyms in this context**: "panel"; "card".

**Related**: `Hero`, `CTA`

---

### Term: `CTA`

A call-to-action anchor — the link the visitor is invited to follow.

**Code identifier(s)**: `SocialIcons` (anchor elements; `src/contexts/landing/presentation/social-icons.tsx`).

**Used in features**: `landing/landing-page.feature`

**Forbidden synonyms in this context**: "button" (these are anchors); "link".

**Related**: `Hero`, `Social icons`

---

### Term: `Social icons`

Row of external-platform identity icons, each a labeled link with `aria-label`.

**Code identifier(s)**: `SocialIcons` (`src/contexts/landing/presentation/social-icons.tsx`).

**Used in features**: `landing/landing-page.feature`

**Forbidden synonyms in this context**: "social links"; "follow buttons".

**Related**: `CTA`
