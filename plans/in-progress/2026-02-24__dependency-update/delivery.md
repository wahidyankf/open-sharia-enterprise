# Delivery Plan

**Status**: In Progress

## Approach

Updates are grouped into nine phases ordered by risk (lowest first). Each phase produces one or
more commits. High-risk major upgrades (Next.js, TailwindCSS) are isolated in their own phases
with explicit go/no-go decision gates so they can be deferred without blocking safe updates.
AGP 9 and Gradle 9 are pre-decided as **DEFERRED** — Flutter officially blocks AGP 9 upgrades
until issue #181383 is resolved; Phase 7 executes only the Kotlin minor update.

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
  - ~~Decision: AGP 8 → 9 + Gradle 8 → 9~~ — **pre-decided DEFERRED** (Flutter blocks AGP 9;
    see Phase 7 and `tech-docs.md` Android/Gradle section)

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

> **Decision Gate**: Phase 7 (AGP/Gradle) is pre-decided: AGP 9 and Gradle 9 are **DEFERRED**
> due to Flutter's official incompatibility block. Only the Kotlin minor update is executed.
> Phase 8 (Next.js) remains high-risk — review the audit report and Phase 8b decision criteria
> (React 19 / Radix UI compatibility) before proceeding. Phase 8 may be deferred to a separate
> plan if the ecosystem is not yet ready.

---

### Phase 7: Android / Gradle — Kotlin Minor Update (AGP/Gradle DEFERRED)

> 🚫 **AGP 8 → 9 and Gradle 8 → 9 upgrades are DEFERRED** for this plan.
>
> The Flutter team officially advises against upgrading Flutter apps to AGP 9 at this time.
> Flutter apps using plugins are incompatible with AGP 9.0.0 (issue #181383). The Flutter team
> has paused AGP 9 support pending a backwards-compatibility audit.
>
> Reference: <https://docs.flutter.dev/release/breaking-changes/migrate-to-agp-9>
> Issue tracker: <https://github.com/flutter/flutter/issues/181383>
>
> **Gradle 9.3.1** is also deferred — it is only required for AGP 9; AGP 8.x runs on Gradle 8.x.
>
> When Flutter resolves issue #181383 and publishes its AGP 9 migration guide, create a new
> plan `android-gradle-agp9-upgrade` to execute the full major upgrade.

**What is executed in this phase** — Kotlin minor update only:

- [ ] Update Kotlin Gradle plugin `2.2.20` → `2.3.0` in
      `apps/organiclever-app/android/settings.gradle.kts`
- [ ] Run `./gradlew assembleDebug` in `apps/organiclever-app/android/` — verify clean build
- [ ] If deprecation warnings appear, fix them (Kotlin 2.3 improved deprecation diagnostics)
- [ ] Run `flutter build apk` — APK builds successfully with updated Kotlin plugin
- [ ] Commit: `chore(deps): upgrade kotlin gradle plugin from 2.2.20 to 2.3.0`

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

> ⚠️ **Known React 19 / Radix UI incompatibilities (as of 2026-02-25)**. Before proceeding,
> check whether these issues have been resolved upstream:
>
> - `@radix-ui/react-icons`: peer dependency declared as `react@^18`; causes install conflicts
>   under React 19.
> - `@radix-ui` Primitives: `useComposedRefs` has a known infinite loop regression with React 19
>   strict mode.
> - `Slot` TypeScript component: breaking type changes under React 19's narrowing.
>
> If any of these remain unresolved when you reach this step, freeze at Next.js 15.x and defer
> Phase 8b to a follow-up plan `nextjs-16-react-19-upgrade`.

- [ ] Read Next.js 16 upgrade guide
- [ ] Assess whether Next.js 16 requires React 19
  - [ ] If React 19 required: verify all `@radix-ui/*` packages (icons, primitives, shadcn
        components) are fully compatible with React 19 — check the changelog for each installed
        package and confirm the known issues above are resolved
  - [ ] If any Radix UI issue is unresolved: stop here; freeze at Next.js 15.x and defer 16
        to a follow-up plan
- [ ] Run official codemod if available: `npx @next/codemod@16 upgrade`
- [ ] Update `next` and `eslint-config-next` to `16.x`
- [ ] Update `react` and `react-dom` to React 19 if required
- [ ] Run `npm install` in root workspace
- [ ] Run `nx build organiclever-web` — successful production build
- [ ] Run `nx dev organiclever-web` — application boots, all routes load
- [ ] Run `nx run organiclever-web-e2e:test:e2e` — all e2e tests pass
- [ ] Commit: `chore(deps): upgrade next.js from 15 to 16`

### Phase 9: TailwindCSS v4 Evaluation

Medium-risk. TailwindCSS v4 replaces `tailwind.config.js` with CSS-first configuration.
shadcn-ui now officially supports Tailwind v4 and provides a migration guide, reducing risk
compared to earlier assessments.

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
