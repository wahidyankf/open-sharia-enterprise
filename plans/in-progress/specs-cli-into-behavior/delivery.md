# Delivery: Consolidate CLI Specs Under `behavior/`

## Phase 0 — Setup

- [ ] 0.1 Create `ose-public` worktree: `cd ose-public && claude --worktree specs-cli-into-behavior`
- [ ] 0.2 Run `npm install && npm run doctor -- --fix` in worktree root

---

## Phase 1 — oseplatform

- [ ] 1.1 Move spec directory: `git mv specs/apps/oseplatform/cli specs/apps/oseplatform/behavior/cli`
- [ ] 1.2 Update `specs/apps/oseplatform/behavior/cli/gherkin/README.md` — fix self-referencing parent link (now one level deeper under `behavior/`)
- [ ] 1.3 Update `specs/apps/oseplatform/behavior/README.md` — add `cli/` child entry to Children section and Perspectives table
- [ ] 1.4 Update `apps/oseplatform-cli/cmd/links_check_test.go` line 19: `cli/gherkin` → `behavior/cli/gherkin`
- [ ] 1.5 Update `apps/oseplatform-cli/cmd/links-check.integration_test.go` line 19: same change
- [ ] 1.6 Update `apps/oseplatform-cli/project.json` `spec-coverage` command string: `specs/apps/oseplatform/cli/gherkin` → `specs/apps/oseplatform/behavior/cli/gherkin`
- [ ] 1.7 Update `apps/oseplatform-cli/project.json` `spec-coverage` inputs glob: same path change
- [ ] 1.8 Update `apps/oseplatform-cli/README.md` — fix spec path reference
- [ ] 1.9 Run `nx run oseplatform-cli:test:quick` — must pass
- [ ] 1.10 Run `nx run oseplatform-cli:test:integration` — must pass
- [ ] 1.11 Run `nx run oseplatform-cli:spec-coverage` — must pass

---

## Phase 2 — ayokoding

- [ ] 2.1 Move spec directory: `git mv specs/apps/ayokoding/cli specs/apps/ayokoding/behavior/cli`
- [ ] 2.2 Update `specs/apps/ayokoding/behavior/cli/gherkin/README.md` — fix self-referencing parent link
- [ ] 2.3 Update `specs/apps/ayokoding/behavior/README.md` — add `cli/` child entry to Children section and Perspectives table
- [ ] 2.4 Update `apps/ayokoding-cli/cmd/links_check_test.go` line 19: `cli/gherkin` → `behavior/cli/gherkin`
- [ ] 2.5 Update `apps/ayokoding-cli/cmd/links-check.integration_test.go` line 19: same change
- [ ] 2.6 Update `apps/ayokoding-cli/project.json` `spec-coverage` command string: `specs/apps/ayokoding/cli/gherkin` → `specs/apps/ayokoding/behavior/cli/gherkin`
- [ ] 2.7 Update `apps/ayokoding-cli/project.json` `spec-coverage` inputs glob: same path change
- [ ] 2.8 Update `apps/ayokoding-cli/README.md` — fix spec path reference
- [ ] 2.9 Run `nx run ayokoding-cli:test:quick` — must pass
- [ ] 2.10 Run `nx run ayokoding-cli:test:integration` — must pass
- [ ] 2.11 Run `nx run ayokoding-cli:spec-coverage` — must pass

---

## Phase 3 — rhino

- [ ] 3.1 Move 18 feature files from `specs/apps/rhino/cli/gherkin/` to `specs/apps/rhino/behavior/cli/gherkin/` using `git mv` for each file (preserves git history)
- [ ] 3.2 Replace `specs/apps/rhino/behavior/cli/gherkin/README.md` with comprehensive version: carry the full 18-command feature table from `specs/apps/rhino/cli/gherkin/README.md`, add `specs/` subfolder section documenting the 4 planned validate-\* features, preserve conventions and related links
- [ ] 3.3 Remove old `specs/apps/rhino/cli/gherkin/README.md` (superseded by merged file)
- [ ] 3.4 Remove empty `specs/apps/rhino/cli/` directory: `git rm -r specs/apps/rhino/cli`
- [ ] 3.5 Bulk-update all 36 rhino-cli Go test files — path string at line 19:

  ```bash
  sed -i '' 's|specs/apps/rhino/cli/gherkin|specs/apps/rhino/behavior/cli/gherkin|g' \
    apps/rhino-cli/cmd/*_test.go apps/rhino-cli/cmd/*.integration_test.go
  ```

- [ ] 3.6 Verify sed replaced exactly 36 files and no unintended files changed: `git diff --name-only apps/rhino-cli/cmd/`
- [ ] 3.7 Update `apps/rhino-cli/project.json` `spec-coverage` command string: `specs/apps/rhino/cli/gherkin` → `specs/apps/rhino/behavior/cli/gherkin`
- [ ] 3.8 Update `apps/rhino-cli/project.json` `spec-coverage` inputs glob: same path change
- [ ] 3.9 Update `specs/apps/rhino/README.md` — update structure diagram to reflect `behavior/cli/` instead of top-level `cli/`
- [ ] 3.10 Update `governance/development/infra/bdd-spec-test-mapping.md` — update example path referencing `specs/apps/rhino/cli/gherkin`
- [ ] 3.11 Run `nx run rhino-cli:test:quick` — must pass
- [ ] 3.12 Run `nx run rhino-cli:test:integration` — must pass
- [ ] 3.13 Run `nx run rhino-cli:spec-coverage` — check output carefully:
  - If it **passes**: done, continue.
  - If it **fails on `validate-*.feature`** files in `specs/`: the tool recurses into the `specs/` subfolder. Resolve: implement stub test files for those 4 planned specs (preferred) OR adjust spec-coverage scope. Do not mark 3.13 complete until green.

---

## Phase 4 — Final Validation

- [ ] 4.1 Verify no `specs/apps/*/cli/` directories remain: `ls specs/apps/oseplatform/ specs/apps/ayokoding/ specs/apps/rhino/`
- [ ] 4.2 Run `nx affected -t test:quick` — all affected projects green
- [ ] 4.3 Run `nx affected -t test:integration` — all affected projects green
- [ ] 4.4 Run `nx affected -t spec-coverage` — all affected projects green
- [ ] 4.5 Run `npm run lint:md` — no markdown violations
- [ ] 4.6 Verify `specs/apps/oseplatform/behavior/README.md` documents `cli/` perspective
- [ ] 4.7 Verify `specs/apps/ayokoding/behavior/README.md` documents `cli/` perspective
- [ ] 4.8 Verify `specs/apps/rhino/behavior/cli/gherkin/README.md` covers all 18 commands + `specs/` subfolder

---

## Phase 5 — Commit and Push

- [ ] 5.1 Stage all changes and commit:

  ```
  refactor(specs): consolidate cli/ specs under behavior/cli/ for all apps

  Move specs/apps/{oseplatform,ayokoding,rhino}/cli/ into their respective
  behavior/cli/ directories. Unifies all Gherkin behavioral contracts under
  behavior/, organized by interface type (web/, api/, cli/).

  Updates all Go test path strings, project.json spec-coverage targets,
  behavior/README.md files, app READMEs, and governance docs.
  ```

- [ ] 5.2 Push to `origin main` (direct-to-main per Trunk Based Development)
- [ ] 5.3 Monitor CI — verify `nx affected` targets pass in GitHub Actions
- [ ] 5.4 Archive plan: `git mv plans/in-progress/specs-cli-into-behavior plans/done/2026-MM-DD__specs-cli-into-behavior`
