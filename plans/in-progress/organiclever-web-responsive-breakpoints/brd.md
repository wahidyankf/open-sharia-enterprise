# BRD — Responsive Breakpoints

## Problem Statement

OrganicLever's `/app/*` shell currently has one responsive decision point at
768px. Below it, users get a mobile layout with a bottom TabBar. At and above
it, users get a 220px sidebar framing a **480px-wide content column** — a phone
inside a browser. On a 1280px monitor roughly 65% of the screen is dead space.
On a 768px iPad the sidebar and the narrow column fight for the same space.

This is the dominant UI pattern for early productivity apps that start mobile-
first and never revisit the wider breakpoints. It creates three compounding
problems:

1. **Wasted real estate**: tablet and desktop users can't see more information
   than a phone user despite having 2–4× the pixel area.
2. **Missed interaction patterns**: side panels, persistent entry lists, and
   sticky filter columns are natural on wider screens but unavailable.
3. **Perceived quality gap**: Pre-Alpha status is already disclosed; a layout
   that looks unfinished on desktop compounds the impression.

## Opportunity

A genuine 3-breakpoint shell gives each device class the layout it expects:

- **Mobile** — thumb-first, single column, FAB at center-bottom. Unchanged.
- **Tablet** — icon rail + content split. Home shows week-strip alongside
  entry list simultaneously. History shows chart beside session cards.
  Matches iPad multitasking conventions.
- **Desktop** — labeled sidebar + uncapped 2-column content. Analytics cards
  display in a grid. Settings form uses a 2-column card layout.

No back-end changes. No new data model. Purely presentation layer.

## Success Criteria

- At 768px (representative tablet), the rail nav is visible and content fills
  two columns on Home, History, and Progress.
- At 1280px (representative desktop), the labeled sidebar is visible, content
  max-width is uncapped (≥ 800px usable), and all four main screens show
  multi-column layouts where content density warrants it.
- At 375px (representative mobile), behavior is identical to the current
  production layout.
- Existing unit and E2E test suites pass without skips.
- Dark mode functions correctly at all three breakpoints.

## Out of Scope

- Any changes to PGlite data layer, application logic, or routing.
- New features (workout timer, social sharing, export).
- Landing page (`/`) — responsive already, untouched.
- Storybook stories for new components (nice-to-have, not required).
