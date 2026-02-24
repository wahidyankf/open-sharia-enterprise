# Delivery Plan

**Status**: Not Started

## Approach

Updates are grouped into nine phases ordered by risk (lowest first). Each phase produces one or
more commits. High-risk major upgrades (Next.js, AGP/Gradle, TailwindCSS) are isolated in their
own phases with explicit go/no-go decision gates so they can be deferred without blocking safe
updates.

## Implementation Phases

### Phase 1: Audit and Baseline

Produce a comprehensive audit report before touching any file.

- [ ] Run `npm outdated` in root workspace; capture output to `generated-reports/`
- [ ] Run `go list -u -m all` in each Go module root (`ayokoding-cli`, `rhino-cli`)
- [ ] Run `hugo mod graph` in `ayokoding-web` and `oseplatform-web`
- [ ] Run `flutter pub outdated` in `apps/organiclever-app/`
- [ ] Run `mvn versions:display-dependency-updates` and `mvn versions:display-parent-updates`
      in `apps/organiclever-be/`
- [ ] Research latest Gradle and AGP version compatibility matrix
- [ ] Compile report: write `generated-reports/dep-audit__YYYY-MM-DD.md` with current vs latest
      for every dependency, classified as P / m / M
- [ ] Review report; make go/no-go decision for each major upgrade
  - [ ] Decision: Next.js 14 → 15 → 16 (document decision and rationale)
  - [ ] Decision: TailwindCSS v3 → v4 (document decision and rationale)
  - [ ] Decision: AGP 8 → 9 + Gradle 8 → 9 (document decision and rationale)

### Phase 2: Node.js Volta Pin + NPM Patch/Minor Updates

Low-risk. Affects root workspace and all NPM-based projects.

- [ ] Update Volta Node.js pin: `24.11.1` → `24.13.1` in root `package.json`
- [ ] Update Volta npm pin: run `volta pin npm@latest` in the repo root, then verify the updated
      version appears in `package.json` under `"volta": { "npm": "..." }`
- [ ] Run `npm update` in root workspace to apply all safe patch/minor bumps
- [ ] Verify `package-lock.json` is updated (no merge conflicts, lockfileVersion unchanged)
- [ ] Update `@playwright/test` to `^1.58.2` across `organiclever-web-e2e`,
      `organiclever-be-e2e`, `organiclever-app-web-e2e`
- [ ] Run `npx playwright install` in each e2e app to refresh browser binaries
- [ ] Run `nx affected -t lint` — all pass
- [ ] Run `nx affected -t test:quick` — all pass
- [ ] Run `nx build organiclever-web` — successful production build
- [ ] Commit: `chore(deps): update node.js volta pin and npm patch/minor deps`

### Phase 3: Go Module Normalization and Updates

Low-risk. Normalizes Go toolchain version and updates transitive deps.

- [ ] Update `ayokoding-cli/go.mod`: change `go 1.24.2` → `go 1.26`
- [ ] Update `rhino-cli/go.mod`: change `go 1.24.2` → `go 1.26`
- [ ] Update `ayokoding-web/go.mod`: change `go 1.25` → `go 1.26`
- [ ] Update `oseplatform-web/go.mod`: change `go 1.25` → `go 1.26`
- [ ] Run `go mod tidy` in `apps/ayokoding-cli/` — verify `go.sum` updated
- [ ] Run `go mod tidy` in `apps/rhino-cli/` — verify `go.sum` updated
- [ ] Run `hugo mod tidy` in `apps/ayokoding-web/` — verify Hugo go.sum updated
- [ ] Run `hugo mod tidy` in `apps/oseplatform-web/` — verify Hugo go.sum updated
- [ ] Run `go build ./...` in `apps/ayokoding-cli/` — succeeds
- [ ] Run `go build ./...` in `apps/rhino-cli/` — succeeds
- [ ] Run `nx build ayokoding-web` — successful Hugo build
- [ ] Run `nx build oseplatform-web` — successful Hugo build
- [ ] Commit: `chore(deps): normalize go toolchain to 1.26 across all go modules`

### Phase 4: Hugo Theme Updates

Low-risk. Themes are additive; unlikely to break content rendering.

- [ ] Read Hextra v0.12.0 release notes for breaking changes (layout, shortcodes, config)
- [ ] Update Hextra in `ayokoding-web`:
      `cd apps/ayokoding-web && hugo mod get github.com/imfing/hextra@v0.12.0`
- [ ] Run `hugo mod tidy` in `apps/ayokoding-web/`
- [ ] Run `nx build ayokoding-web` — no layout errors, no shortcode failures
- [ ] Visually spot-check rendered pages locally: `nx dev ayokoding-web`
- [ ] Update PaperMod to latest commit in `oseplatform-web`:
      `cd apps/oseplatform-web && hugo mod get -u`
- [ ] Run `hugo mod tidy` in `apps/oseplatform-web/`
- [ ] Run `nx build oseplatform-web` — succeeds
- [ ] Visually spot-check: `nx dev oseplatform-web`
- [ ] Update Hugo binary version reference in `CLAUDE.md` and any CI config if Hugo version
      changed to `0.156.0 Extended`
- [ ] Commit: `chore(deps): update hugo themes (hextra v0.12.0, papermod latest)`

### Phase 5: Maven / Spring Boot Patch Update

Low-risk. Single-line version bump; all dependencies managed by BOM.

- [ ] Edit `apps/organiclever-be/pom.xml`: bump `spring-boot-starter-parent` from `4.0.2` to
      `4.0.3`
- [ ] Run `mvn dependency:resolve` to pull updated BOM — verify no conflicts
- [ ] Run `mvn verify` — all tests pass
- [ ] Smoke-test actuator: `mvn spring-boot:run &` then
      `sleep 20 && curl -sf http://localhost:8080/actuator/health && kill %1`
      — verify HTTP 200 response before killing the process
- [ ] Commit: `chore(deps): bump spring boot from 4.0.2 to 4.0.3`

### Phase 6: Flutter / Dart Package Updates

Medium-risk. Resolve any pub constraint conflicts manually.

- [ ] Run `flutter pub upgrade` in `apps/organiclever-app/`
- [ ] Review `flutter pub outdated` output for packages that could not be auto-upgraded
- [ ] For each constrained package: check upstream changelog, update constraint in
      `pubspec.yaml` if safe
- [ ] Re-run `flutter pub upgrade --major-versions` if needed for remaining outdated packages
- [ ] Verify `pubspec.lock` is regenerated cleanly
- [ ] Run `flutter analyze` — no new errors
- [ ] Run `flutter test` — all unit tests pass
- [ ] Run `flutter build web` — successful web build
- [ ] Run `flutter build apk` — Android APK builds (validates pub deps don't break Android)
- [ ] Commit: `chore(deps): upgrade flutter pub packages`

---

> **Decision Gate**: Phases 7 and 8 are high-risk major upgrades. Review the audit report and
> the go/no-go decisions captured in Phase 1 before proceeding. Either phase may be deferred to
> a separate plan if the risk is deemed too high for the current project phase.

---

### Phase 7: Android / Gradle Major Upgrade (AGP 8 → 9, Gradle 8 → 9)

High-risk. Follow the prescribed order strictly.

- [ ] Read AGP 9.0 release notes in full:
      <https://developer.android.com/build/releases/agp-9-0-0-release-notes>
- [ ] Read Kotlin blog update guide for AGP 9:
      <https://blog.jetbrains.com/kotlin/2026/01/update-your-projects-for-agp9/>
- [ ] **Step 7a**: Update Kotlin Gradle plugin `2.2.20` → `2.3.0` in
      `apps/organiclever-app/android/settings.gradle.kts`
  - Run `./gradlew assembleDebug` — verify clean build
  - If deprecation warnings appear, fix before moving to 7b
- [ ] **Step 7b**: Update AGP `8.11.1` → `9.0.1` in
      `apps/organiclever-app/android/settings.gradle.kts`
  - Run `./gradlew assembleDebug` — observe and document any build failures
  - Fix all compilation errors from removed AGP APIs
  - Re-run until green
- [ ] **Step 7c**: Update Gradle wrapper `8.14` → `9.3.1`
  - Edit `apps/organiclever-app/android/gradle/wrapper/gradle-wrapper.properties`
  - Run `./gradlew assembleDebug` — verify with new Gradle version
  - Fix any Gradle API removals
- [ ] Run `flutter build apk` — successful APK build
- [ ] Run `nx run organiclever-app-web-e2e:test:e2e` — e2e tests pass (where applicable)
- [ ] Commit: `chore(deps): upgrade android gradle plugin to 9.0.1 and gradle wrapper to 9.3.1`

### Phase 8: Next.js Major Upgrade (14 → 15 → 16)

High-risk. Two discrete sub-phases — commit after each major.

#### Phase 8a: Next.js 14 → 15

- [ ] Read [Next.js 15 upgrade guide](https://nextjs.org/docs/app/guides/upgrading/version-15)
- [ ] Run official codemod: `cd apps/organiclever-web && npx @next/codemod@latest upgrade`
- [ ] Manually review codemod diff — check `cookies()`, `headers()`, `params`, `searchParams`
      async API changes
- [ ] Update `next` and `eslint-config-next` to `15.x` in `apps/organiclever-web/package.json`
- [ ] Run `npm install` in root workspace
- [ ] Run `nx build organiclever-web` — successful production build
- [ ] Run `nx dev organiclever-web` — application boots, all routes load
- [ ] Run `nx run organiclever-web-e2e:test:e2e` — all e2e tests pass
- [ ] Commit: `chore(deps): upgrade next.js from 14 to 15`

#### Phase 8b: Next.js 15 → 16

- [ ] Read Next.js 16 upgrade guide
- [ ] Assess whether Next.js 16 requires React 19
  - [ ] If React 19 required: verify `@radix-ui/*` packages are compatible with React 19
  - [ ] If incompatible: freeze at Next.js 15.x and defer 16 to a follow-up plan
- [ ] Run official codemod if available: `npx @next/codemod@16 upgrade`
- [ ] Update `next` and `eslint-config-next` to `16.x`
- [ ] Update `react` and `react-dom` to React 19 if required
- [ ] Run `npm install` in root workspace
- [ ] Run `nx build organiclever-web` — successful production build
- [ ] Run `nx dev organiclever-web` — application boots, all routes load
- [ ] Run `nx run organiclever-web-e2e:test:e2e` — all e2e tests pass
- [ ] Commit: `chore(deps): upgrade next.js from 15 to 16`

### Phase 9: TailwindCSS v4 Evaluation

High-risk. TailwindCSS v4 replaces `tailwind.config.js` with CSS-first configuration.

- [ ] Read [TailwindCSS v4 upgrade guide](https://tailwindcss.com/docs/upgrade-guide)
- [ ] Run official upgrade tool (verify exact command at <https://tailwindcss.com/docs/upgrade-guide>
      before running — currently documented as `npx @tailwindcss/upgrade`)
- [ ] Review generated diff — assess scope of configuration migration
- [ ] If migration is feasible (< 1 day of work): complete the migration
  - [ ] Update `tailwindcss`, `postcss`, `tailwind-merge`, `tailwindcss-animate` versions
  - [ ] Migrate `tailwind.config.js` to CSS `@import "tailwindcss"` approach
  - [ ] Visual regression check: `nx dev organiclever-web`, compare key pages
  - [ ] Run `nx build organiclever-web` — production build succeeds
  - [ ] Commit: `chore(deps): upgrade tailwindcss from v3 to v4`
- [ ] If migration is infeasible: pin `tailwindcss` to `^3.4` explicitly; create new backlog
      plan `tailwindcss-v4-migration` with full migration scope

---

## Validation Checklist

Run after all phases are complete to confirm nothing regressed.

- [ ] `npm run lint:md` — all markdown files pass
- [ ] `nx affected -t lint` (all projects) — no lint errors
- [ ] `nx affected -t test:quick` (all projects) — all pass
- [ ] `nx build ayokoding-web` — Hugo build succeeds
- [ ] `nx build oseplatform-web` — Hugo build succeeds
- [ ] `nx build organiclever-web` — Next.js production build succeeds
- [ ] `go build ./...` in `apps/ayokoding-cli/` — CLI compiles
- [ ] `go build ./...` in `apps/rhino-cli/` — CLI compiles
- [ ] `mvn verify` in `apps/organiclever-be/` — all Maven tests pass
- [ ] `flutter build web` in `apps/organiclever-app/` — web target builds
- [ ] `flutter build apk` in `apps/organiclever-app/` — Android APK builds
- [ ] `npm audit` — no critical or high severity CVEs in NPM dependency tree
- [ ] All lock files committed: `package-lock.json`, `go.sum` (×4), `pubspec.lock`
- [ ] No `package.json` or manifest file left with un-committed changes

## Completion Criteria

All acceptance criteria in [requirements.md](./requirements.md) are satisfied.
The audit report produced in Phase 1 is stored in `generated-reports/`.
Every phase checkbox is ticked or explicitly noted as deferred with a reason.
