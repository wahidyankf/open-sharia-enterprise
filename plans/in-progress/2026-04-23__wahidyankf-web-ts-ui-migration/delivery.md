# Delivery Checklist — wahidyankf-web Component Migration to ts-ui

## Environment Setup

- [ ] Confirm working directory is `ose-public` subrepo root
- [ ] Run `npm install` in `ose-public/` to install dependencies
- [ ] Run `npm run doctor -- --fix` to converge the full polyglot toolchain (required — the
      `postinstall` hook runs `doctor || true` and silently tolerates drift; see
      [Worktree Toolchain Initialization](../../../governance/development/workflow/worktree-setup.md))
- [ ] Verify dev server starts: `nx dev wahidyankf-web`
- [ ] Run existing tests to establish baseline:
  - [ ] `npx nx run wahidyankf-web:test:quick`
  - [ ] `npx nx run ts-ui:test:quick`
- [ ] Note any preexisting failures before touching any files

---

## Phase 1 — Add ts-ui Workspace Dependency to wahidyankf-web

- [ ] Open `apps/wahidyankf-web/package.json`
- [ ] Add `"@open-sharia-enterprise/ts-ui": "*"` to the `dependencies` block, inserted
      alphabetically between `@next/third-parties` and `class-variance-authority`:

  ```json
  "@open-sharia-enterprise/ts-ui": "*",
  ```

- [ ] Verify no duplicate entries exist in `dependencies` after the edit
- [ ] Commit: `chore(wahidyankf-web): add @open-sharia-enterprise/ts-ui workspace dependency`

---

## Phase 2 — Migrate HighlightText

- [ ] Create directory `libs/ts-ui/src/components/highlight-text/`
- [ ] Copy `apps/wahidyankf-web/src/components/HighlightText.tsx` to
      `libs/ts-ui/src/components/highlight-text/highlight-text.tsx` and refactor to accept
      flexible props (e.g. `highlightClassName` for the mark element styles) with current
      hardcoded values as defaults
- [ ] Copy `apps/wahidyankf-web/src/components/HighlightText.unit.test.tsx` to
      `libs/ts-ui/src/components/highlight-text/highlight-text.unit.test.tsx`
- [ ] In `libs/ts-ui/src/components/highlight-text/highlight-text.unit.test.tsx`, update line 4:

  Before:

  ```ts
  import { HighlightText, highlightText } from "./HighlightText";
  ```

  After:

  ```ts
  import { HighlightText, highlightText } from "./highlight-text";
  ```

- [ ] Append to `libs/ts-ui/src/index.ts`:

  ```ts
  export { HighlightText, highlightText } from "./components/highlight-text/highlight-text";
  ```

- [ ] Update `apps/wahidyankf-web/src/app/page.tsx` — replace lines 16–17:

  Before:

  ```ts
  import { SearchComponent } from "@/components/SearchComponent";
  import { HighlightText } from "@/components/HighlightText";
  ```

  After (merge into one import; `SearchComponent` will be updated again in Phase 4 — add
  `HighlightText` to the existing SearchComponent import line for now, or replace both at once
  in Phase 4):

  ```ts
  import { HighlightText } from "@open-sharia-enterprise/ts-ui";
  import { SearchComponent } from "@/components/SearchComponent";
  ```

  > **Note:** `SearchComponent` still references the local path here and will be fully resolved
  > in Phase 4. It is acceptable to update both on this file in Phase 4 in one pass. The
  > important constraint is that by the end of Phase 4, `page.tsx` reads:
  >
  > ```ts
  > import { SearchComponent, HighlightText } from "@open-sharia-enterprise/ts-ui";
  > ```

- [ ] Update `apps/wahidyankf-web/src/app/cv/page.tsx` — replace lines 35–36:

  Before:

  ```ts
  import { SearchComponent } from "@/components/SearchComponent";
  import { HighlightText } from "@/components/HighlightText";
  ```

  After (same two-pass note applies as page.tsx above):

  ```ts
  import { HighlightText } from "@open-sharia-enterprise/ts-ui";
  import { SearchComponent } from "@/components/SearchComponent";
  ```

- [ ] Update `apps/wahidyankf-web/src/app/personal-projects/page.tsx` — replace lines 8–9:

  Before:

  ```ts
  import { SearchComponent } from "@/components/SearchComponent";
  import { HighlightText } from "@/components/HighlightText";
  ```

  After:

  ```ts
  import { HighlightText } from "@open-sharia-enterprise/ts-ui";
  import { SearchComponent } from "@/components/SearchComponent";
  ```

- [ ] Update `apps/wahidyankf-web/src/utils/markdown.tsx` — replace line 2:

  Before:

  ```ts
  import { HighlightText } from "@/components/HighlightText";
  ```

  After:

  ```ts
  import { HighlightText } from "@open-sharia-enterprise/ts-ui";
  ```

- [ ] Delete `apps/wahidyankf-web/src/components/HighlightText.tsx`
- [ ] Delete `apps/wahidyankf-web/src/components/HighlightText.unit.test.tsx`

---

## Phase 3 — Migrate ScrollToTop

- [ ] Create directory `libs/ts-ui/src/components/scroll-to-top/`
- [ ] Copy `apps/wahidyankf-web/src/components/ScrollToTop.tsx` to
      `libs/ts-ui/src/components/scroll-to-top/scroll-to-top.tsx` and refactor to accept
      flexible props (e.g. `threshold`, `className`, `buttonClassName`) with current hardcoded
      values as defaults
      (the `"use client";` directive at line 1 remains in place)
- [ ] Copy `apps/wahidyankf-web/src/components/ScrollToTop.unit.test.tsx` to
      `libs/ts-ui/src/components/scroll-to-top/scroll-to-top.unit.test.tsx`
- [ ] In `libs/ts-ui/src/components/scroll-to-top/scroll-to-top.unit.test.tsx`, update line 4:

  Before:

  ```ts
  import ScrollToTop from "./ScrollToTop";
  ```

  After:

  ```ts
  import ScrollToTop from "./scroll-to-top";
  ```

- [ ] Append to `libs/ts-ui/src/index.ts`:

  ```ts
  export { default as ScrollToTop } from "./components/scroll-to-top/scroll-to-top";
  ```

- [ ] Update `apps/wahidyankf-web/src/app/layout.tsx` — replace lines 4–5:

  Before:

  ```ts
  import ScrollToTop from "@/components/ScrollToTop";
  import ThemeToggle from "@/components/ThemeToggle";
  ```

  After (add `ScrollToTop` from ts-ui; `ThemeToggle` still references local path and will be
  resolved in Phase 5 — it is acceptable to update both on this file in Phase 5 in one pass):

  ```ts
  import { ScrollToTop } from "@open-sharia-enterprise/ts-ui";
  import ThemeToggle from "@/components/ThemeToggle";
  ```

  > **Note:** By end of Phase 5, `layout.tsx` must read:
  >
  > ```ts
  > import { ScrollToTop, ThemeToggle } from "@open-sharia-enterprise/ts-ui";
  > ```

- [ ] Delete `apps/wahidyankf-web/src/components/ScrollToTop.tsx`
- [ ] Delete `apps/wahidyankf-web/src/components/ScrollToTop.unit.test.tsx`

---

## Phase 4 — Migrate SearchComponent

- [ ] Create directory `libs/ts-ui/src/components/search-component/`
- [ ] Copy `apps/wahidyankf-web/src/components/SearchComponent.tsx` to
      `libs/ts-ui/src/components/search-component/search-component.tsx` and refactor to accept
      flexible props (e.g. `className`, `inputClassName`, `clearButtonClassName`) with current
      hardcoded values as defaults
- [ ] Copy `apps/wahidyankf-web/src/components/SearchComponent.unit.test.tsx` to
      `libs/ts-ui/src/components/search-component/search-component.unit.test.tsx`
- [ ] In `libs/ts-ui/src/components/search-component/search-component.unit.test.tsx`, update
      line 4:

  Before:

  ```ts
  import { SearchComponent } from "./SearchComponent";
  ```

  After:

  ```ts
  import { SearchComponent } from "./search-component";
  ```

- [ ] Append to `libs/ts-ui/src/index.ts`:

  ```ts
  export { SearchComponent } from "./components/search-component/search-component";
  ```

- [ ] Update `apps/wahidyankf-web/src/app/page.tsx` — consolidate to single ts-ui import:

  Final state of the two import lines:

  ```ts
  import { SearchComponent, HighlightText } from "@open-sharia-enterprise/ts-ui";
  ```

  (Remove the temporary split from Phase 2 if done in two passes, or apply directly if doing
  both components in one edit.)

- [ ] Update `apps/wahidyankf-web/src/app/cv/page.tsx` — same consolidation:

  ```ts
  import { SearchComponent, HighlightText } from "@open-sharia-enterprise/ts-ui";
  ```

- [ ] Update `apps/wahidyankf-web/src/app/personal-projects/page.tsx` — same consolidation:

  ```ts
  import { SearchComponent, HighlightText } from "@open-sharia-enterprise/ts-ui";
  ```

- [ ] Delete `apps/wahidyankf-web/src/components/SearchComponent.tsx`
- [ ] Delete `apps/wahidyankf-web/src/components/SearchComponent.unit.test.tsx`

---

## Phase 5 — Migrate ThemeToggle

- [ ] Create directory `libs/ts-ui/src/components/theme-toggle/`
- [ ] Copy `apps/wahidyankf-web/src/components/ThemeToggle.tsx` to
      `libs/ts-ui/src/components/theme-toggle/theme-toggle.tsx` and refactor to accept
      flexible props (e.g. `className`) with current hardcoded values as defaults
      (the `"use client";` directive at line 1 remains in place)
- [ ] Copy `apps/wahidyankf-web/src/components/ThemeToggle.unit.test.tsx` to
      `libs/ts-ui/src/components/theme-toggle/theme-toggle.unit.test.tsx`
- [ ] In `libs/ts-ui/src/components/theme-toggle/theme-toggle.unit.test.tsx`, update line 3:

  Before:

  ```ts
  import ThemeToggle from "./ThemeToggle";
  ```

  After:

  ```ts
  import ThemeToggle from "./theme-toggle";
  ```

- [ ] Append to `libs/ts-ui/src/index.ts`:

  ```ts
  export { default as ThemeToggle } from "./components/theme-toggle/theme-toggle";
  ```

- [ ] Update `apps/wahidyankf-web/src/app/layout.tsx` — consolidate to single ts-ui import:

  Final state:

  ```ts
  import { ScrollToTop, ThemeToggle } from "@open-sharia-enterprise/ts-ui";
  ```

- [ ] Delete `apps/wahidyankf-web/src/components/ThemeToggle.tsx`
- [ ] Delete `apps/wahidyankf-web/src/components/ThemeToggle.unit.test.tsx`

- [ ] Commit after Phases 2–5:
      `feat(ts-ui): migrate HighlightText, ScrollToTop, SearchComponent, ThemeToggle from wahidyankf-web`

---

## Phase 6 — Verify Remaining components/

- [ ] Confirm `apps/wahidyankf-web/src/components/` contains exactly two files:
  - `Navigation.tsx`
  - `Navigation.unit.test.tsx`
- [ ] Confirm no stale imports to deleted files remain:

  ```bash
  grep -r "@/components/HighlightText\|@/components/ScrollToTop\|@/components/SearchComponent\|@/components/ThemeToggle" apps/wahidyankf-web/src/
  ```

  Expected output: zero matches.

- [ ] Confirm ts-ui index.ts now contains all four new export lines:

  ```bash
  grep -E "highlight-text|scroll-to-top|search-component|theme-toggle" libs/ts-ui/src/index.ts
  ```

  Expected: four lines, one per component.

---

## Local Quality Gates (Before Push)

- [ ] Run typecheck for affected projects:

  ```bash
  npx nx affected -t typecheck
  ```

- [ ] Run linting for affected projects:

  ```bash
  npx nx affected -t lint
  ```

- [ ] Run quick tests for affected projects:

  ```bash
  npx nx affected -t test:quick
  ```

- [ ] Run spec-coverage for affected projects:

  ```bash
  npx nx affected -t spec-coverage
  ```

- [ ] Fix ALL failures — including preexisting issues not caused by your changes
- [ ] Re-run failing checks to confirm resolution
- [ ] Verify zero failures before pushing

> **Important**: Fix ALL failures found during quality gates, not just those caused by your
> changes. This follows the root cause orientation principle — proactively fix preexisting errors
> encountered during work. Do not defer or skip existing issues. Commit preexisting fixes
> separately with appropriate conventional commit messages.

---

## Commit Guidelines

- [ ] Commit changes thematically — group related changes into logically cohesive commits
- [ ] Follow Conventional Commits format: `<type>(<scope>): <description>`
- [ ] Split different domains/concerns into separate commits
- [ ] Preexisting fixes get their own commits, separate from plan work
- [ ] Do NOT bundle unrelated changes into a single commit

Suggested commit sequence:

1. `chore(wahidyankf-web): add @open-sharia-enterprise/ts-ui workspace dependency`
2. `feat(ts-ui): migrate HighlightText, ScrollToTop, SearchComponent, ThemeToggle from wahidyankf-web`
3. Any preexisting fix commits (separate, labelled accordingly)

---

## Manual UI Verification (Playwright MCP)

- [ ] Start dev server: `nx dev wahidyankf-web`
- [ ] Navigate to the home page via `browser_navigate` (`http://localhost:3201`)
- [ ] Inspect DOM via `browser_snapshot` — verify search bar and highlight functionality render
- [ ] Navigate to the CV page (`http://localhost:3201/cv`) — verify search bar renders and
      highlight text works on all CV entries
- [ ] Navigate to the personal-projects page (`http://localhost:3201/personal-projects`) —
      verify search bar renders and highlight text works on project entries
- [ ] Interact with `ThemeToggle` via `browser_click` — verify dark/light toggle switches
      correctly
- [ ] Scroll down on any page and verify the `ScrollToTop` button appears; click it and verify
      scroll-to-top behaviour
- [ ] Enter text in the search bar on any listing page — verify highlighted matches appear
      using `HighlightText`
- [ ] Check for JS errors via `browser_console_messages` — must be zero errors
- [ ] Take screenshots via `browser_take_screenshot` for visual reference
- [ ] Document verification results in this checklist

---

## Post-Push CI Verification

- [ ] Push changes to `main`
- [ ] Monitor ALL GitHub Actions workflows triggered by the push
- [ ] Verify ALL CI checks pass — no exceptions
- [ ] If any CI check fails, fix immediately and push a follow-up commit
- [ ] Repeat until ALL GitHub Actions pass with zero failures
- [ ] Do NOT proceed to plan archival until CI is fully green

---

## Plan Archival

- [ ] Verify ALL delivery checklist items are ticked
- [ ] Verify ALL quality gates pass (local + CI)
- [ ] Verify ALL manual assertions pass (Playwright MCP)
- [ ] Move plan folder from `plans/in-progress/` to `plans/done/` via `git mv`
- [ ] Update `plans/in-progress/README.md` — remove this plan entry
- [ ] Update `plans/done/README.md` — add this plan entry with completion date
- [ ] Update any other READMEs that reference this plan
- [ ] Commit the archival: `chore(plans): move wahidyankf-web-ts-ui-migration to done`
