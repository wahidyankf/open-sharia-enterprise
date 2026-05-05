# PRD — Responsive Breakpoints

## Breakpoint Definitions

| Name      | Range        | Detection                            |
| --------- | ------------ | ------------------------------------ |
| `mobile`  | `< 640px`    | `window.innerWidth < 640`            |
| `tablet`  | `640–1023px` | `window.innerWidth >= 640 && < 1024` |
| `desktop` | `≥ 1024px`   | `window.innerWidth >= 1024`          |

## Chrome (Navigation Shell)

### Mobile

- Bottom TabBar: 5 slots — Home · Progress · FAB(+) · History · Settings.
- FAB: 52×52 teal rounded square, `+` icon, centered.
- Behavior: unchanged from today.

### Tablet — RailNav

- Left vertical rail: **64px wide**, `var(--color-card)` background, right border.
- **Logo mark** (top): 36×36 teal rounded square with zap icon; no wordmark.
- **Log entry FAB** (below logo): 48×48 circle, teal, `+` icon, centered horizontally.
- **Nav items** (below FAB): Home · History · Progress · Settings.
  - Each: 44×44 tappable, icon centered (20px Icon).
  - Active state: teal-wash background, 10px border-radius, teal-ink icon.
  - No text labels.
- Rail does not collapse; always visible on tablet.

### Desktop — SideNav

- Left sidebar: **220px wide** (unchanged design).
- Logo section: zap icon mark + "OrganicLever" wordmark + "Life journal" sub.
- "Log entry" full-width teal button below logo.
- 4 labeled nav items with active highlight.
- Content area: **no 480px max-width cap**. Content fills remaining viewport up
  to 1280px max-width container (centered with auto margins).

## Screen Layouts

### Home — `/app/home`

**Mobile**: single column — header → week rhythm card → filter chips →
workout module → entry list. (Unchanged.)

**Tablet**: two columns, row fills available width.

```
┌──────────────────────────────────────────────────────┐
│  Header (day · date · Good morning)                  │
├────────────────────────────┬─────────────────────────┤
│ Week rhythm card           │ Recent entries panel    │
│ Filter chips               │  (scrollable, sticky    │
│ Workout module             │   to viewport height)   │
└────────────────────────────┴─────────────────────────┘
Left col: 56% width. Right col: 44% width.
Right panel: independent scroll, full height, `position: sticky top: 0`.
```

**Desktop**: same two-column structure as tablet but wider; workout routine
cards display in a 2-col grid inside the left column.

### History — `/app/history`

**Mobile**: heading → bar chart → session list. (Unchanged.)

**Tablet**: two columns.

```
┌─────────────────┬──────────────────────────────────┐
│ Heading         │ Session list (scrollable)        │
│ Bar chart       │ — reverse-chrono cards           │
│ (sticky top)    │ — 1-col on tablet                │
└─────────────────┴──────────────────────────────────┘
Left col: 42%. Right col: 58%.
Left scrolls with page; chart anchors at top of left column.
```

**Desktop**: same two columns but session cards display in a **2-col grid**
inside the right column when there are ≥ 4 sessions.

### Progress — `/app/progress`

**Mobile**: heading → module tabs → range picker → content. (Unchanged.)

**Tablet**: two columns.

```
┌────────────────────┬─────────────────────────────────┐
│ Heading            │ Content area (scrollable)       │
│ Module pill tabs   │ — exercise cards (1-col)        │
│ Range picker       │ — activity bars                 │
│ (sticky)           │                                 │
└────────────────────┴─────────────────────────────────┘
Left col: 36% (sticky, stops scrolling when content is shorter).
Right col: 64%.
```

**Desktop**: exercise/activity cards in right column display in a
**2-col grid** when `groupBy === "exercise"` and there are ≥ 2 cards.

### Settings — `/app/settings`

**Mobile**: single column. (Unchanged.)

**Tablet**: single column, max-width 560px, centered.

**Desktop**: **two-column card grid**.

```
Left column:                  Right column:
  Profile card                  Language card
  Workout defaults card         Appearance card
                                Data card
```

## Functional Requirements

- `FR-1` All existing functionality (logging, workout timer, routines, dark
  mode, language) works identically at every breakpoint.
- `FR-2` Breakpoint detection is `resize`-event driven; layout updates on
  viewport change without page reload.
- `FR-3` The "Log entry" action (FAB or button) is reachable at all breakpoints.
- `FR-4` Active nav item is visually distinct at all breakpoints.
- `FR-5` Dark mode applies correct token values at all breakpoints.
- `FR-6` Safe-area insets applied on mobile TabBar (unchanged).
- `FR-7` All screens pass existing Vitest unit tests after prop signature updates.

## Gherkin Acceptance Criteria

```gherkin
Feature: Distinct responsive layouts for mobile, tablet, and desktop

  Background:
    Given the user has opened the OrganicLever app at "/app/home"

  # --- Chrome ---

  Scenario: Mobile viewport renders bottom TabBar
    Given the viewport width is 375px
    Then the bottom TabBar is visible
    And the RailNav is not rendered
    And the SideNav is not rendered

  Scenario: Tablet viewport renders icon-only RailNav
    Given the viewport width is 768px
    Then the RailNav is visible
    And the RailNav shows only icons with no text labels
    And the bottom TabBar is not rendered
    And the SideNav is not rendered

  Scenario: Desktop viewport renders labeled SideNav
    Given the viewport width is 1280px
    Then the SideNav is visible with nav item labels
    And the bottom TabBar is not rendered
    And the RailNav is not rendered

  # --- Log entry access ---

  Scenario Outline: Log entry trigger is reachable at every breakpoint
    Given the viewport width is <width>
    When the user activates the log-entry control
    Then the "Add Entry" sheet opens
    Examples:
      | width  |
      | 375px  |
      | 768px  |
      | 1280px |

  # --- Content layouts ---

  Scenario: Home screen is single column on mobile
    Given the viewport width is 375px
    Then the home screen renders in a single column
    And the week rhythm strip is above the entry list

  Scenario: Home screen splits into two columns on tablet
    Given the viewport width is 768px
    Then the home screen renders a left column containing the week rhythm strip
    And the home screen renders a right column containing the recent entries panel

  Scenario: Home screen splits into two columns on desktop
    Given the viewport width is 1280px
    Then the home screen renders a left column containing the week rhythm strip
    And the home screen renders a right column containing the recent entries panel

  Scenario: History screen is single column on mobile
    Given the viewport width is 375px
    Then the history screen renders in a single column
    And the bar chart is above the session list

  Scenario: History screen splits into two columns on tablet
    Given the viewport width is 768px
    Then the history screen renders the bar chart in the left column
    And the history screen renders the session list in the right column

  Scenario: Progress screen is single column on mobile
    Given the viewport width is 375px
    Then the progress screen renders module tabs above the content area

  Scenario: Progress screen splits into two columns on tablet
    Given the viewport width is 768px
    Then the progress screen renders the module and range pickers in the left column
    And the progress screen renders the chart content in the right column

  Scenario: Settings screen is single column on mobile
    Given the viewport width is 375px
    Then the settings screen renders all cards in a single column

  Scenario: Settings screen is single column centered on tablet
    Given the viewport width is 768px
    Then the settings screen renders all cards in a single column with max width

  Scenario: Settings screen uses two-column layout on desktop
    Given the viewport width is 1280px
    Then the settings screen renders Profile and Workout cards in the left column
    And the settings screen renders Language, Appearance, and Data cards in the right column

  # --- Navigation ---

  Scenario Outline: Active nav item is highlighted at every breakpoint
    Given the viewport width is <width>
    And the user is on the "/app/history" page
    Then the History nav item has an active visual state
    Examples:
      | width  |
      | 375px  |
      | 768px  |
      | 1280px |

  # --- Dark mode ---

  Scenario Outline: Dark mode applies correct tokens at every breakpoint
    Given dark mode is enabled
    And the viewport width is <width>
    Then the background uses the dark-mode background token
    And the card surfaces use the dark-mode card token
    Examples:
      | width  |
      | 375px  |
      | 768px  |
      | 1280px |

  # --- Resize ---

  Scenario: Layout updates when viewport crosses a breakpoint boundary
    Given the viewport width is 375px
    And the bottom TabBar is visible
    When the viewport is resized to 768px
    Then the RailNav becomes visible
    And the bottom TabBar is no longer visible
```
