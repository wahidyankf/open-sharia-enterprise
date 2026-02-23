# Delivery

## Implementation Order

Changes are ordered from highest impact to lowest. Complete each phase before the next.

---

## Phase 1: Workspace Defaults (nx.json)

Update workspace-level caching and target defaults first so newly added targets inherit correct
caching behavior.

- [ ] **1.1** Update `nx.json` `targetDefaults`: add `typecheck`, `test:quick`, `test:unit`,
      `test:integration`, `test:e2e`; remove non-standard `test` entry
- [ ] **1.2** Remove `tasksRunnerOptions` block from `nx.json` entirely — redundant legacy
      config; caching is fully handled by `cache: true/false` in `targetDefaults`

**Verify**: `cat nx.json` — confirm `targetDefaults` has the 7 canonical targets; `test` and
`tasksRunnerOptions` are gone.

---

## Phase 2: Critical Missing test:quick (Highest Safety Risk)

These two apps are **excluded from the pre-push hook and PR merge gate** until fixed.

- [ ] **2.1** `oseplatform-web/project.json`: Add `test:quick` (runs `bash build.sh` with outputs)
- [ ] **2.2** `oseplatform-web/project.json`: Fix `clean` to include `.hugo_build.lock`
- [ ] **2.3** `organiclever-web/package.json`: Add vitest devDependencies — `vitest`,
      `@vitejs/plugin-react`, `jsdom`, `@testing-library/react`, `vite-tsconfig-paths`
- [ ] **2.4** Create `apps/organiclever-web/vitest.workspace.ts` with `unit` and `integration`
      named projects (see tech-docs.md for full content)
- [ ] **2.5** `organiclever-web/project.json`: Update `lint` to `npx oxlint@latest .` (replaces
      `next lint`)
- [ ] **2.6** `organiclever-web/project.json`: Add `typecheck` (runs `tsc --noEmit`)
- [ ] **2.7** `organiclever-web/project.json`: Add `test:quick` (`npx vitest run --project unit`)
- [ ] **2.8** `organiclever-web/project.json`: Add `test:unit` (`npx vitest run --project unit`)
- [ ] **2.9** `organiclever-web/project.json`: Add `test:integration`
      (`npx vitest run --project integration`)

**Verify**: `nx run oseplatform-web:test:quick`, `nx run organiclever-web:test:quick`,
`nx run organiclever-web:test:unit`, `nx run organiclever-web:test:integration`, and
`nx run organiclever-web:lint` all return exit code 0.

---

## Phase 3: Missing lint on Hugo Sites and Go CLIs

These four apps cannot participate in `nx affected -t lint` until fixed.

- [ ] **3.1** `ayokoding-cli/project.json`: Add `lint` (`golangci-lint run ./...`)
- [ ] **3.2** `rhino-cli/project.json`: Add `lint` (`CGO_ENABLED=0 golangci-lint run ./...`)
- [ ] **3.3** `ayokoding-web/project.json`: Add `lint` (`markdownlint-cli2 "content/**/*.md"`)
- [ ] **3.4** `oseplatform-web/project.json`: Add `lint` (`markdownlint-cli2 "content/**/*.md"`)

**Verify**: `nx run ayokoding-cli:lint`, `nx run rhino-cli:lint`, `nx run ayokoding-web:lint`,
`nx run oseplatform-web:lint` — all exit 0.

---

## Phase 4: Spring Boot Standardization

Multiple renames and additions — apply all together.

- [ ] **4.1** `organiclever-be/project.json`: Rename `serve` → `dev` (same command)
- [ ] **4.2** `organiclever-be/project.json`: Rename `test` → `test:unit` (same command)
- [ ] **4.3** `organiclever-be/project.json`: Add `test:quick` (`mvn test`)
- [ ] **4.4** `organiclever-be/project.json`: Add `start` (`java -jar target/organiclever-be-1.0.0.jar`)
- [ ] **4.5** `organiclever-be/project.json`: Add `outputs: ["{projectRoot}/target"]` to `build`

**Verify**:

- `nx run organiclever-be:dev` — starts Spring Boot dev server
- `nx run organiclever-be:test:quick` — runs Maven tests
- `nx run organiclever-be:test:unit` — runs Maven tests (same result)
- The old `serve` and `test` targets no longer exist in `project.json`

---

## Phase 5: Flutter Standardization

- [ ] **5.1** `organiclever-app/project.json`: Rename `test` → `test:unit` (keep `dependsOn: ["install"]`)
- [ ] **5.2** `organiclever-app/project.json`: Add `typecheck` (`flutter analyze`)
- [ ] **5.3** `organiclever-app/project.json`: Add `dependsOn: ["install"]` to `test:quick`
- [ ] **5.4** `organiclever-app/project.json`: Remove `lint` — redundant with `typecheck` (same
      `flutter analyze` command; running both doubles execution per push with zero additional coverage)

**Verify**:

- `nx run organiclever-app:test:unit` — runs Flutter tests (install runs first)
- `nx run organiclever-app:test:quick` — runs Flutter tests (install runs first)
- `nx run organiclever-app:typecheck` — runs `flutter analyze`
- The old `test` target no longer exists
- The `lint` target no longer exists in `organiclever-app/project.json`

---

## Phase 6: Playwright E2E Standardization (3 projects)

Apply the same set of changes to all three E2E projects. Use the full updated files from
tech-docs.md to replace each project.json entirely.

- [ ] **6.1** `organiclever-web-e2e/project.json`: Rename `e2e` → `test:e2e`
- [ ] **6.2** `organiclever-web-e2e/project.json`: Rename `e2e:ui` → `test:e2e:ui`
- [ ] **6.3** `organiclever-web-e2e/project.json`: Rename `e2e:report` → `test:e2e:report`
- [ ] **6.4** `organiclever-web-e2e/project.json`: Add `lint` (`npx oxlint@latest .`)
- [ ] **6.5** `organiclever-web-e2e/project.json`: Add `test:quick` (`npx oxlint@latest .`)
- [ ] **6.6** `organiclever-be-e2e/project.json`: Same 5 changes
- [ ] **6.7** `organiclever-app-web-e2e/project.json`: Same 5 changes

**Verify**:

- `nx run organiclever-web-e2e:lint` — exits 0
- `nx run organiclever-web-e2e:test:quick` — exits 0
- `nx run organiclever-be-e2e:test:quick` — exits 0
- `nx run organiclever-app-web-e2e:test:quick` — exits 0
- Grep confirms `"e2e"` as a top-level target key is gone from all 3 project.json files

---

## Phase 7: Pre-push Hook

Update the hook after all project.json targets are in place so the three gates have full coverage.

- [ ] **7.1** Replace `.husky/pre-push` content with the three-target sequence (see tech-docs.md)

**Verify**:

- `cat .husky/pre-push` — confirms all three `nx affected` commands are present in order
- Make a trivial change to any file, stage it, and run `git push --dry-run` — hook fires and
  runs typecheck, lint, and test:quick without error

---

## Final Validation

- [ ] **V1** `nx affected -t test:quick --all` — all 10 apps produce a result
- [ ] **V2** `nx affected -t lint --all` — 9 apps produce a result (Flutter skipped by design — no `lint` target)
- [ ] **V3** `nx affected -t typecheck --all` — all statically typed apps produce a result
- [ ] **V4** Verify no non-standard target names remain:

  ```bash
  grep -r '"serve"\|"test"' apps/*/project.json
  # Expected: no matches for "serve" or bare "test" as target keys

  grep -r '"e2e"' apps/*/project.json
  # Expected: only appears inside strings (command values), not as target keys
  ```

- [ ] **V5** Verify E2E projects have canonical names:

  ```bash
  grep '"test:e2e"' apps/organiclever-web-e2e/project.json apps/organiclever-be-e2e/project.json apps/organiclever-app-web-e2e/project.json
  # Expected: 3 matches (one per file)
  ```

- [ ] **V6** Verify `nx.json` has no `"test"` in targetDefaults:

  ```bash
  grep '"test"' nx.json
  # Expected: no match (no "test" targetDefault, no "tasksRunnerOptions")
  ```

- [ ] **V7** Verify pre-push hook runs all three gates:

  ```bash
  cat .husky/pre-push
  # Expected: typecheck, lint, and test:quick all present
  ```

- [ ] **V8** Verify organiclever-web vitest workspace is in place:

  ```bash
  nx run organiclever-web:test:unit        # exits 0
  nx run organiclever-web:test:integration # exits 0
  grep '"name"' apps/organiclever-web/vitest.workspace.ts
  # Expected: shows "unit" and "integration"
  ```

---

## Post-Completion

Move this plan folder from `plans/in-progress/` to `plans/done/` and update both index README
files when all checklist items and acceptance criteria are verified.

---

## Future Work (Out of Scope)

These improvements are noted but not part of this plan:

- Separate Maven Surefire (unit) from Failsafe (integration) in `organiclever-be` to give
  `test:quick` a genuine subset scope
- Add `test:unit` to Hugo sites if a content validation test suite is introduced
