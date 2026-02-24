# Technical Documentation

## Version Audit Snapshot

The table below captures the **current pinned / declared version** against the **latest stable
release** as of 2026-02-25. Classification:

- **P** = Patch (x.y.Z) — safe to apply immediately
- **m** = Minor (x.Y.z) — apply with review of changelog
- **M** = Major (X.y.z) — requires migration guide, dedicated testing phase
- **✓** = Already at latest

### Node.js / NPM Ecosystem

| Package                           | Current       | Latest Stable      | Type    | Notes                                 |
| --------------------------------- | ------------- | ------------------ | ------- | ------------------------------------- |
| Node.js (Volta pin)               | `24.11.1`     | `24.13.1`          | P       | Update Volta pin in `package.json`    |
| npm (Volta pin)                   | `11.6.3`      | verify at runtime  | P       | `npm --version` after Node update     |
| `nx`                              | `22.5.2`      | `22.5.2`           | ✓       | Already latest                        |
| `next` (organiclever-web)         | `14.2.14`     | `16.1.6`           | **M×2** | Two major versions; see below         |
| `eslint-config-next`              | `14.2.14`     | `16.1.6`           | **M×2** | Must track `next` version             |
| `react`                           | `^18`         | React 19 available | m       | Evaluate if Next 16 requires React 19 |
| `react-dom`                       | `^18`         | React 19 available | m       | Track `react`                         |
| `@types/react`                    | `^18`         | track React        | m       | Track `react`                         |
| `@types/react-dom`                | `^18`         | track React        | m       | Track `react-dom`                     |
| `@playwright/test`                | `^1.55.1`     | `1.58.2`           | m       | All three `*-e2e` apps                |
| `@commitlint/cli`                 | `^20.1.0`     | verify             | m       | Check npm registry                    |
| `@commitlint/config-conventional` | `^20.0.0`     | verify             | m       | Check npm registry                    |
| `husky`                           | `^9.1.7`      | verify             | P/m     | Check npm registry                    |
| `lint-staged`                     | `^16.2.6`     | verify             | P/m     | Check npm registry                    |
| `markdownlint-cli2`               | `^0.20.0`     | verify             | P/m     | Check npm registry                    |
| `prettier`                        | `^3.6.2`      | verify             | P/m     | Check npm registry                    |
| `tsx`                             | `^4.20.6`     | verify             | P/m     | Check npm registry                    |
| `tailwindcss`                     | `^3.4.1`      | v4.x available     | **M**   | TailwindCSS v4 is a rewrite           |
| `vitest`                          | `^4.0.18`     | verify             | P/m     | Check npm registry                    |
| `@testing-library/react`          | `^16.3.2`     | verify             | P/m     | Check npm registry                    |
| `lucide-react`                    | `^0.447.0`    | verify             | m       | Check npm registry                    |
| `@radix-ui/*`                     | `^1.x / ^2.x` | verify             | m       | Check npm registry per package        |
| `class-variance-authority`        | `^0.7.0`      | verify             | P/m     | Check npm registry                    |
| `clsx`                            | `^2.1.1`      | verify             | P       | Check npm registry                    |
| `tailwind-merge`                  | `^2.5.3`      | verify             | P/m     | Check npm registry                    |
| `tailwindcss-animate`             | `^1.0.7`      | verify             | P/m     | Check npm registry                    |
| `cookie`                          | `^1.0.0`      | verify             | P       | Check npm registry                    |
| `@types/cookie`                   | `^0.6.0`      | verify             | P       | Check npm registry                    |
| `@types/node`                     | `^20`         | verify             | m       | Check npm registry; Node 24 types     |
| `@vitejs/plugin-react`            | `^5.1.4`      | verify             | P/m     | Check npm registry                    |
| `vite-tsconfig-paths`             | `^6.1.1`      | verify             | P/m     | Check npm registry                    |
| `jsdom`                           | `^28.1.0`     | verify             | m       | Check npm registry                    |
| `postcss`                         | `^8`          | verify             | P/m     | Check npm registry                    |

### Go Modules

| Module / App             | Current `go` directive | Latest Go | Type | Notes                             |
| ------------------------ | ---------------------- | --------- | ---- | --------------------------------- |
| `ayokoding-cli` go.mod   | `1.24.2`               | `1.26`    | m    | Normalize with other modules      |
| `rhino-cli` go.mod       | `1.24.2`               | `1.26`    | m    | Normalize with other modules      |
| `ayokoding-web` go.mod   | `1.25`                 | `1.26`    | m    | Already more recent than CLI apps |
| `oseplatform-web` go.mod | `1.25`                 | `1.26`    | m    | Already more recent than CLI apps |
| `github.com/spf13/cobra` | `v1.10.2`              | `v1.10.2` | ✓    | Already latest                    |
| `gopkg.in/yaml.v3`       | `v3.0.1`               | `v3.0.1`  | ✓    | Stable, no newer release          |

### Hugo Themes (Go modules)

| Theme                                                      | Current                       | Latest             | Type | Notes                                  |
| ---------------------------------------------------------- | ----------------------------- | ------------------ | ---- | -------------------------------------- |
| `github.com/imfing/hextra` (ayokoding-web)                 | `v0.11.1`                     | `v0.12.0`          | m    | WCAG 2.2 AA improvements, new features |
| `github.com/adityatelange/hugo-PaperMod` (oseplatform-web) | commit `3bb0ca2` (2026-01-25) | latest HEAD        | P    | Rolling release; pin to latest commit  |
| Hugo binary                                                | `0.155.2 Extended`            | `0.156.0 Extended` | m    | Update docs and any CI runners         |

### Flutter / Dart

| Package               | Current                          | Latest Stable | Type | Notes                            |
| --------------------- | -------------------------------- | ------------- | ---- | -------------------------------- |
| Dart SDK (constraint) | `^3.11.0`                        | `3.11.0`      | ✓    | Dart 3.11 is the Feb 2026 stable |
| Flutter SDK           | latest compatible with `^3.11.0` | `3.41.2`      | m    | Install latest stable channel    |
| `cupertino_icons`     | `^1.0.8`                         | verify        | P    | Check pub.dev                    |
| `provider`            | `^6.1.2`                         | verify        | P/m  | Check pub.dev                    |
| `http`                | `^1.2.0`                         | verify        | P/m  | Check pub.dev                    |
| `json_annotation`     | `^4.9.0`                         | verify        | P    | Check pub.dev                    |
| `flutter_lints`       | `^6.0.0`                         | verify        | m    | Check pub.dev                    |
| `build_runner`        | `^2.4.9`                         | verify        | P/m  | Check pub.dev                    |
| `json_serializable`   | `^6.8.0`                         | verify        | P    | Check pub.dev                    |
| `mockito`             | `^5.4.4`                         | verify        | P/m  | Check pub.dev                    |

### Java / Maven

| Artifact                       | Current | Latest Stable | Type | Notes                             |
| ------------------------------ | ------- | ------------- | ---- | --------------------------------- |
| Spring Boot parent             | `4.0.2` | `4.0.3`       | P    | Released 2026-02-19               |
| Java (source/target)           | `25`    | `25`          | ✓    | Java 25 is the configured version |
| `spring-boot-starter-web`      | via BOM | via BOM       | —    | Managed by Spring Boot BOM        |
| `spring-boot-starter-actuator` | via BOM | via BOM       | —    | Managed by Spring Boot BOM        |
| `spring-boot-starter-test`     | via BOM | via BOM       | —    | Managed by Spring Boot BOM        |

### Android / Gradle

| Tool                        | Current  | Latest Stable           | Type  | Notes                                    |
| --------------------------- | -------- | ----------------------- | ----- | ---------------------------------------- |
| Gradle wrapper              | `8.14`   | `9.3.1`                 | **M** | Major breaking changes; see below        |
| Android Gradle Plugin (AGP) | `8.11.1` | `9.0.1`                 | **M** | Jan 2026; requires Kotlin plugin update  |
| Kotlin Gradle plugin        | `2.2.20` | `2.3.0`                 | m     | Blog post: "Update projects for AGP 9.0" |
| Java (JVM target)           | `17`     | `17`                    | ✓     | No change required                       |
| Flutter Gradle plugin       | `1.0.0`  | embedded in Flutter SDK | —     | Managed by Flutter toolchain             |

---

## Per-Ecosystem Update Strategy

### NPM — Patch and Minor Updates

Run `npm outdated` first to identify exact installed vs available versions. Apply updates in two
passes:

1. **Pass 1 — patch/minor (safe)**: `npm update` in root. Verify `package-lock.json` is
   updated. Run `nx affected -t lint` and `nx affected -t test:quick`.
2. **Pass 2 — TailwindCSS v4 (major)**: Tailwind v4 is a ground-up rewrite with a different
   configuration model. Evaluate feasibility for `organiclever-web`. If v4 migration is complex,
   pin to `^3.4` and defer to a dedicated plan.
3. **Pass 3 — Next.js 14 → 16 (major × 2)**: This is the highest-risk NPM change (see below).

### NPM — Next.js Major Upgrade (14 → 15 → 16)

> ⚠️ This is a two-step major upgrade. Never jump directly from 14 to 16 without following each
> major's migration guide.

**Step A: 14 → 15**

- Follow the [Next.js 15 Upgrade Guide](https://nextjs.org/docs/app/guides/upgrading/version-15).
- Key changes: caching semantics changed (fetch is no longer cached by default), `cookies()`/
  `headers()` are now async, `params` and `searchParams` props are now Promises.
- Run codemod: `npx @next/codemod@latest upgrade`.
- Validate: `nx build organiclever-web`, `nx run organiclever-web-e2e:test:e2e`.

**Step B: 15 → 16**

- Follow the Next.js 16 upgrade guide when available.
- Reassess React compatibility (Next.js 16 may require React 19).
- Validate same targets.

**Decision gate**: If Next.js 16 requires React 19 and any `@radix-ui` packages do not support
React 19, freeze Next.js at 15.x until the ecosystem catches up.

### Go — Module Normalization + Updates

All four Go modules (`ayokoding-cli`, `rhino-cli`, `ayokoding-web`, `oseplatform-web`) currently
declare different Go toolchain versions (1.24.2 and 1.25). They must all target **Go 1.26**
(released 2026-02-10), which is the current stable release.

```bash
# In each Go module root:
go get -u ./...     # Update all direct and transitive deps
go mod tidy         # Remove unused deps, clean go.sum
```

For Hugo sites, run `hugo mod get -u` instead, which updates Hugo modules properly.

### Hugo Themes

Hugo themes are Go modules fetched via `hugo mod get`. Update each site:

```bash
# ayokoding-web — Hextra 0.11.1 → 0.12.0
cd apps/ayokoding-web && hugo mod get github.com/imfing/hextra@v0.12.0

# oseplatform-web — PaperMod rolling
cd apps/oseplatform-web && hugo mod get -u
```

Review Hextra 0.12.0 changelog for any breaking template or shortcode changes before applying.
The WCAG 2.2 improvements in 0.12.0 are additive; page layout should not regress.

### Flutter / Dart

Flutter dependencies are managed by `pub`. Run:

```bash
cd apps/organiclever-app
flutter pub upgrade         # Upgrade all packages within constraint bounds
flutter pub outdated        # Review what could not be upgraded due to constraints
flutter pub upgrade --major-versions  # Only if major bumps are needed and safe
```

After upgrade, run:

```bash
flutter analyze
flutter test
flutter build web
```

### Maven / Spring Boot

Spring Boot dependencies are managed through the parent BOM. Updating the parent version cascades
to all BOM-managed transitive dependencies automatically.

```bash
cd apps/organiclever-be
# Update spring-boot-starter-parent version in pom.xml to 4.0.3
mvn verify
mvn spring-boot:run   # Smoke test actuator endpoints
```

No code changes are expected for a patch bump 4.0.2 → 4.0.3.

### Android / Gradle (Major Upgrade 8 → 9)

> ⚠️ This is the highest-risk change in the entire plan. Gradle 9 and AGP 9 both carry breaking
> API changes. Approach incrementally.

**Pre-upgrade checklist:**

- Read [Android Gradle Plugin 9.0 release notes](https://developer.android.com/build/releases/agp-9-0-0-release-notes).
- Read [Kotlin blog: "Update your Kotlin projects for Android Gradle Plugin 9.0"](https://blog.jetbrains.com/kotlin/2026/01/update-your-projects-for-agp9/).
- Confirm target Flutter SDK version is compatible with AGP 9.

**Upgrade order:**

1. Update Kotlin Gradle plugin: `2.2.20` → `2.3.0` (stable, May 2025). Test build.
2. Update AGP: `8.11.1` → `9.0.1`. Apply deprecation-warning fixes first if AGP 8 prints any.
3. Update Gradle wrapper to `9.3.1` (matches AGP 9 requirements).
4. Fix any API removals flagged by the Android build system.
5. Verify `./gradlew assembleDebug` succeeds.
6. Verify `flutter build apk` succeeds end-to-end.

---

## Go Version Inconsistency

The current repo has a version mismatch in `go` directives:

| File                          | Current `go` directive |
| ----------------------------- | ---------------------- |
| `apps/ayokoding-cli/go.mod`   | `1.24.2`               |
| `apps/rhino-cli/go.mod`       | `1.24.2`               |
| `apps/ayokoding-web/go.mod`   | `1.25`                 |
| `apps/oseplatform-web/go.mod` | `1.25`                 |

**Resolution**: Normalize all four to `go 1.26` (the current stable). This changes only the
toolchain declaration, not the API surface, so no code changes are required.

---

## Risk Classification

| Change                          | Risk   | Reason                                      |
| ------------------------------- | ------ | ------------------------------------------- |
| Node.js 24.11.1 → 24.13.1       | Low    | Patch update, no API changes                |
| Spring Boot 4.0.2 → 4.0.3       | Low    | Patch update, BOM-managed                   |
| Go directive 1.24.2/1.25 → 1.26 | Low    | Toolchain only, no API removal              |
| Cobra already latest            | None   | No action needed                            |
| Hextra v0.11.1 → v0.12.0        | Low    | Additive theme features                     |
| PaperMod latest commit          | Low    | Additive theme updates                      |
| Playwright 1.55.1 → 1.58.2      | Low    | Minor, backwards-compatible test API        |
| Kotlin 2.2.20 → 2.3.0           | Medium | Minor version, check deprecations           |
| Flutter pub minor updates       | Medium | Resolve constraint conflicts                |
| TailwindCSS v3 → v4             | High   | Complete config model rewrite               |
| Next.js 14 → 15 → 16            | High   | Multiple breaking API changes, async params |
| AGP 8.11.1 → 9.0.1              | High   | Major breaking API changes in build system  |
| Gradle 8.14 → 9.3.1             | High   | Major breaking changes in build API         |

---

## Audit Commands Reference

Run these at the start of Phase 1 to produce the baseline audit report.

```bash
# NPM workspace
npm outdated

# Go modules (per module root)
go list -u -m all        # in apps/ayokoding-cli/
go list -u -m all        # in apps/rhino-cli/
hugo mod get -u --dry-run # not supported — use hugo mod graph instead

# Hugo themes
cd apps/ayokoding-web && hugo mod graph
cd apps/oseplatform-web && hugo mod graph

# Flutter
cd apps/organiclever-app && flutter pub outdated

# Maven
cd apps/organiclever-be && mvn versions:display-dependency-updates
cd apps/organiclever-be && mvn versions:display-parent-updates
cd apps/organiclever-be && mvn versions:display-plugin-updates

# Android (Gradle)
cd apps/organiclever-app/android && ./gradlew dependencyUpdates  # requires com.github.ben-manes.versions plugin
# Alternative: check release notes manually for AGP and Gradle wrapper
```
