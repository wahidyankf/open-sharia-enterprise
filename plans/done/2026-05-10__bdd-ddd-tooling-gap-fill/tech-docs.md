# Tech Docs — BDD + DDD Tooling Gap-Fill

## Allowlist constant

New file: `apps/rhino-cli/internal/allowlist/allowlist.go`

```go
package allowlist

// AppsWithDDD lists every app that adopts full DDD + new specs format and
// is therefore subject to specs validate-adoption + validate-tree gates.
//
// CLI apps adopt BDD only (spec-coverage) and are excluded by design.
var AppsWithDDD = []string{
    "organiclever",
    "wahidyankf",
    "oseplatform",
    "ayokoding",
}
```

Both new Nx targets (`validate:specs-adoption` and `validate:specs-tree`) loop over this list inside their command. The list is also the default value of the new `--apps` flag.

## Fix #1 + #2 — Centralized allowlist gates

### Code change

`apps/rhino-cli/cmd/specs_validate_adoption.go` and `specs_validate_tree.go` accept an optional `--apps` flag (StringSlice). When the flag is absent, they iterate over `allowlist.AppsWithDDD` rather than requiring a positional arg. The existing positional-arg behaviour is preserved for backward-compat:

- `rhino-cli specs validate-adoption organiclever` — single app, today's behavior
- `rhino-cli specs validate-adoption --apps organiclever,wahidyankf` — multi-app
- `rhino-cli specs validate-adoption` (no positional, no flag) — defaults to `--apps=<allowlist>`

### Nx target

`apps/rhino-cli/project.json` adds two targets:

```json
"validate:specs-adoption": {
  "command": "CGO_ENABLED=0 go run -C apps/rhino-cli main.go specs validate-adoption",
  "cache": true,
  "inputs": [
    "{projectRoot}/**/*.go",
    "{workspaceRoot}/specs/apps/**/behavior/**/*.feature",
    "{workspaceRoot}/specs/apps/**/ddd/bounded-contexts.yaml"
  ],
  "outputs": []
},
"validate:specs-tree": {
  "command": "CGO_ENABLED=0 go run -C apps/rhino-cli main.go specs validate-tree",
  "cache": true,
  "inputs": [
    "{projectRoot}/**/*.go",
    "{workspaceRoot}/specs/apps/**/README.md",
    "{workspaceRoot}/specs/apps/**/{product,system-context,containers,components,behavior}/**"
  ],
  "outputs": []
}
```

### Pre-push wiring

The two new Nx targets are added to the existing `nx affected -t ...` line in `.husky/pre-push`. Single source of truth: the allowlist lives only in `apps/rhino-cli/internal/allowlist/allowlist.go`; the pre-push hook makes no per-app routing decision.

`.husky/pre-push` line becomes:

```sh
npx nx affected -t typecheck lint test:quick spec-coverage validate:specs-adoption validate:specs-tree --parallel="$PARALLEL"
```

Both new targets are cacheable and declare their `inputs` to include `specs/apps/**/behavior/**/*.feature` and `specs/apps/**/ddd/bounded-contexts.yaml`, so `nx affected` only re-runs them when an allowlisted app's specs (or the rhino-cli source) actually change. Cache hits are near-zero cost on pushes that don't touch specs.

Removing the legacy regex avoids duplicating the allowlist between Go code and shell script; adding a new app to `allowlist.AppsWithDDD` is the single point of change.

## Fix #3 — `organiclever-be:test:quick` DDD wiring

`apps/organiclever-be/project.json` `test:quick.options.commands` becomes:

```json
"commands": [
  "(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go ddd bc organiclever)",
  "(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go ddd ul organiclever)",
  "<existing commands: dotnet tool restore, build, altcover, runner, test-coverage validate>"
],
"parallel": false
```

`test:quick.inputs` adds:

```json
"{workspaceRoot}/specs/apps/organiclever/ddd/bounded-contexts.yaml",
"{workspaceRoot}/specs/apps/organiclever/ddd/ubiquitous-language/**/*.md"
```

## Fix #4 — Per-BC `code_lang:` field

### Schema bump

Schema version stays at `2` because the new field is **additive** with a default. `bcregistry/types.go` adds:

```go
type Context struct {
    Name           string         `yaml:"name"`
    Summary        string         `yaml:"summary"`
    Layers         []string       `yaml:"layers"`
    Code           []string       `yaml:"code"`
    CodeLang       []string       `yaml:"code_lang"` // NEW
    Glossary       string         `yaml:"glossary"`
    Gherkin        string         `yaml:"gherkin"`
    Relationships  []Relationship `yaml:"relationships"`
}
```

### Loader default

`bcregistry/loader.go` `Load()` post-decode applies the default:

```go
for i := range reg.Contexts {
    if len(reg.Contexts[i].CodeLang) == 0 {
        reg.Contexts[i].CodeLang = []string{"ts", "tsx"}
    }
    if err := validateCodeLang(reg.Contexts[i].CodeLang); err != nil {
        return nil, fmt.Errorf("registry context %q: %w", reg.Contexts[i].Name, err)
    }
}
```

`validateCodeLang` rejects anything outside the supported set.

### Supported language → glob mapping

Single source of truth in `bcregistry/types.go`:

```go
var SupportedLangGlobs = map[string][]string{
    "ts":   {"*.ts"},
    "tsx":  {"*.tsx"},
    "fs":   {"*.fs"},
    "go":   {"*.go"},
    "py":   {"*.py"},
    "java": {"*.java"},
    "kt":   {"*.kt"},
    "rs":   {"*.rs"},
    "ex":   {"*.ex", "*.exs"},
    "exs":  {"*.exs"},
    "cs":   {"*.cs"},
    "clj":  {"*.clj", "*.cljc"},
    "dart": {"*.dart"},
}
```

### Validator change

`glossary/validator.go:116, 161` replaces hardcoded `[]string{"*.ts", "*.tsx"}` with a per-BC computed glob list:

```go
exts := []string{}
for _, lang := range ctx.CodeLang {
    exts = append(exts, bcregistry.SupportedLangGlobs[lang]...)
}
// then: count += grepFn(id, codePath, exts)
```

### Registry update

After fix lands, plans 2 + 3 + organiclever-be inclusion update their registries to declare `code_lang:` per BC. Today only TS-only contexts exist; this fix enables F# (organiclever-be), and prepares for any future polyglot adoption.

## Fix #5 — Multi-parent orphan-root walks

`bcregistry/validator.go:208, 212` already detect the multi-context case for code paths (the audit traced this fix to the post-`code: []string` change for org's plan). For glossary + gherkin parents, change to:

```go
glossaryRoots := map[string]bool{}
gherkinRoots := map[string]bool{}
for _, ctx := range reg.Contexts {
    glossaryRoots[filepath.Join(repoRoot, filepath.Dir(ctx.Glossary))] = true
    gherkinRoots[filepath.Join(repoRoot, filepath.Dir(ctx.Gherkin))] = true
}

sortedGlossaryRoots := /* sorted slice from map */
for _, root := range sortedGlossaryRoots {
    findings = append(findings, detectOrphanFiles(root, registeredGlossary, ...)...)
}
sortedGherkinRoots := /* same for gherkin */
for _, root := range sortedGherkinRoots {
    findings = append(findings, detectOrphanDirs(root, registeredGherkin, ...)...)
}
```

## Fix #6 — Multi-file scenario matching

`internal/speccoverage/checker.go` `findMatchingTestFile()` becomes `findAllMatchingTestFiles()` returning `[]string`. Caller (the `checkOneToOne` scenario loop) iterates the slice and unions the `scenarioTitles` across all matches before checking.

```go
func findAllMatchingTestFiles(appDir, stem string) ([]string, error) {
    var matches []string
    err := filepath.Walk(appDir, func(path string, info os.FileInfo, err error) error {
        if err != nil { return err }
        if info.IsDir() {
            if skipDirs[info.Name()] { return filepath.SkipDir }
            return nil
        }
        base := filepath.Base(path)
        if matchesStem(base, stem) && isTestFile(path) {
            matches = append(matches, path)
        }
        return nil
    })
    return matches, err
}
```

The `--shared-steps` mode is unaffected (it doesn't use `findMatchingTestFile`).

## Fix #7 — Hide drift-\* placeholders

Three approaches considered; chosen approach: **delete entirely**.

- **Delete entirely** (chosen): `cmd/specs_drift_*.go` files removed. If implementation is later needed, a new plan adds them back. Rationale: the placeholder approach was a reservation pattern that has not produced implementations in 5+ months; removing the false-functionality is more honest than maintaining a stub.
- Move to `_stub_/` (rejected): adds dead code that future maintainers must distinguish from live code.
- Hide via cobra `Hidden: true` (rejected): the `Run` function still exists and prints "Not yet implemented" — same false-functionality.

Governance docs that reference the drift commands (`repo-governance/conventions/structure/specs-directory-structure.md` line 211 area) get a one-line update: "Drift detection commands are not currently implemented; track via the [tooling backlog]" with link to a backlog-item heading in the same governance doc.

## Fix #8 — Severity reconciliation

`cmd/specs_validate_counts.go` line 48 changes:

```go
_, _ = fmt.Fprintf(cmd.OutOrStdout(), "%s: MEDIUM: %s\n", f.File, f.Evidence)
```

into per-finding severity:

```go
_, _ = fmt.Fprintf(cmd.OutOrStdout(), "%s: %s: %s\n", f.File, f.Criticality, f.Evidence)
```

`validateSpecCounts()` body sets `Criticality: "HIGH"` for missing folder findings, keeps `"MEDIUM"` for empty-folder findings. The existing `SpecFinding.Criticality` field already supports this. Two changes are required: (1) set `Criticality: "HIGH"` in the missing-folder `SpecFinding{}` struct literal in `validateSpecCounts()` (currently set to `"MEDIUM"` at the sub-folder loop level); (2) change the print format on line 48 to use `f.Criticality` instead of the hardcoded string `"MEDIUM"`.

## Fix #9 — Severity audit log + env var rename

In `cmd/ddd_bc.go:84-91` and `cmd/ddd_ul.go:82-89`:

```go
func resolveBcSeverity(flagVal string) string {
    if flagVal != "" {
        return normaliseSeverity(flagVal)
    }
    if env := os.Getenv("OSE_RHINO_DDD_SEVERITY"); env != "" {
        sev := normaliseSeverity(env)
        if sev == "warn" {
            fmt.Fprintln(os.Stderr, `WARN: severity downgraded to "warn" via OSE_RHINO_DDD_SEVERITY env var`)
        }
        return sev
    }
    if env := os.Getenv("ORGANICLEVER_RHINO_DDD_SEVERITY"); env != "" {
        fmt.Fprintln(os.Stderr, "WARN: ORGANICLEVER_RHINO_DDD_SEVERITY is deprecated; use OSE_RHINO_DDD_SEVERITY")
        sev := normaliseSeverity(env)
        if sev == "warn" {
            fmt.Fprintln(os.Stderr, `WARN: severity downgraded to "warn" via ORGANICLEVER_RHINO_DDD_SEVERITY env var`)
        }
        return sev
    }
    return "error"
}
```

The legacy env var is supported for one minor rev with the deprecation notice. After that minor rev, the legacy lookup is removed.

## Fix #10 — Symmetry whitelist expansion

`bcregistry/validator.go:269-272`:

```go
asymmetricKinds := map[string]bool{
    "customer-supplier": true,
    "conformist":        true,
    "partnership":       true, // NEW — must be reciprocal
    "shared-kernel":     true, // NEW — must be reciprocal
    // anticorruption-layer and open-host-service are intentionally one-way; not in this map
}
```

The two intentionally-one-way kinds (`anticorruption-layer`, `open-host-service`) are NOT added — declaring an ACL or OHS is a one-side statement that the other side need not acknowledge.

`relationships:` entries with kinds outside both `asymmetricKinds` and the one-way set should produce a "unknown relationship kind" finding (currently they're silently allowed). The new validator pass:

```go
knownKinds := map[string]bool{
    "customer-supplier":    true,
    "conformist":           true,
    "partnership":          true,
    "shared-kernel":        true,
    "anticorruption-layer": true,
    "open-host-service":    true,
}
for _, ctx := range reg.Contexts {
    for _, rel := range ctx.Relationships {
        if !knownKinds[rel.Kind] {
            findings = append(findings, Finding{
                File:     "specs/apps/" + reg.App + "/ddd/bounded-contexts.yaml",
                Message:  fmt.Sprintf("unknown relationship kind %q in %q → %q", rel.Kind, ctx.Name, rel.To),
                Severity: severity,
            })
        }
    }
}
```

## Fix #11 — `gherkin: []string` schema extension

### Schema change

Schema version stays at `2` because the change is **additive with auto-conversion** (analogous to how `code:` was elevated). `bcregistry/types.go` redefines:

```go
type Context struct {
    Name           string         `yaml:"name"`
    Summary        string         `yaml:"summary"`
    Layers         []string       `yaml:"layers"`
    Code           []string       `yaml:"code"`
    CodeLang       []string       `yaml:"code_lang"`
    Glossary       string         `yaml:"glossary"`
    Gherkin        GherkinPaths   `yaml:"gherkin"`  // CHANGED — was string
    Relationships  []Relationship `yaml:"relationships"`
}

// GherkinPaths is a list with a custom UnmarshalYAML so a single string
// in the YAML form auto-converts to a one-element slice. This preserves
// every existing v2 registry without edit.
type GherkinPaths []string

func (g *GherkinPaths) UnmarshalYAML(value *yaml.Node) error {
    if value.Kind == yaml.ScalarNode {
        *g = GherkinPaths{value.Value}
        return nil
    }
    var list []string
    if err := value.Decode(&list); err != nil {
        return err
    }
    *g = GherkinPaths(list)
    return nil
}
```

### Loader

`bcregistry/loader.go` post-decode loop adds:

```go
for i := range reg.Contexts {
    if len(reg.Contexts[i].Gherkin) == 0 {
        return nil, fmt.Errorf("registry context %q has empty gherkin list", reg.Contexts[i].Name)
    }
}
```

### Validator changes

`bcregistry/validator.go` `checkGherkin()` becomes per-path:

```go
func checkGherkin(repoRoot string, ctx Context, severity string) []Finding {
    var findings []Finding
    for _, gh := range ctx.Gherkin {
        gherkinPath := filepath.Join(repoRoot, gh)
        if _, err := osStatFn(gherkinPath); err != nil {
            findings = append(findings, Finding{
                File:     gh,
                Message:  fmt.Sprintf("missing gherkin directory for context %q", ctx.Name),
                Severity: severity,
            })
            continue
        }
        // ≥1 .feature check per path
        entries, err := osReadDirFn(gherkinPath)
        if err != nil {
            findings = append(findings, Finding{
                File:     gh,
                Message:  fmt.Sprintf("cannot read gherkin directory for context %q: %v", ctx.Name, err),
                Severity: severity,
            })
            continue
        }
        hasFeature := false
        for _, e := range entries {
            if !e.IsDir() && strings.HasSuffix(e.Name(), ".feature") {
                hasFeature = true
                break
            }
        }
        if !hasFeature {
            findings = append(findings, Finding{
                File:     gh,
                Message:  fmt.Sprintf("no feature files found in gherkin directory for context %q", ctx.Name),
                Severity: severity,
            })
        }
    }
    return findings
}
```

### `registeredGherkin` map population

In `validate()`, the registered set construction loops the list:

```go
for i := range reg.Contexts {
    ctx := &reg.Contexts[i]
    contextByName[ctx.Name] = ctx
    for _, c := range ctx.Code {
        registeredCode[filepath.Join(repoRoot, c)] = true
    }
    registeredGlossary[filepath.Join(repoRoot, ctx.Glossary)] = true
    for _, gh := range ctx.Gherkin {
        registeredGherkin[filepath.Join(repoRoot, gh)] = true
    }
}
```

Combined with fix #5 (multi-parent orphan walks), this catches orphans on every perspective parent regardless of which side a BC declared first.

### `glossary/validator.go` impact

The glossary validator's `checkTerms` uses `ctx.Gherkin` to build feature paths. With `GherkinPaths` it iterates:

```go
for _, gh := range ctx.Gherkin {
    gherkinPath := filepath.Join(opts.RepoRoot, gh)
    findings = append(findings, checkTermsAgainstGherkin(ctx.Glossary, g, codePaths, gherkinPath, sev)...)
}
```

A feature reference in a glossary table that resolves under any one of the BC's gherkin paths counts as found. (Glossary feature references already use the prefix `<bc>/<feature>.feature` per organiclever convention; the validator joins this against each gherkin parent in turn.)

### Plans 2 + 3 follow-up

Once fixes #5 + #11 land, plans 2 and 3's registries can be updated:

- Plan 2 oseplatform — no change required today (no BC has both perspectives day-one).
- Plan 3 ayokoding — change `gherkin: behavior/web/gherkin/content` to `gherkin: [behavior/web/gherkin/content, behavior/api/gherkin/content]` for `content`, `search`, `i18n`, `navigation`. Same yaml file edited in one commit.

This migration commit is **out of scope of plan 4** itself — plan 4 ships the schema, plans 2/3 (or a small follow-up) consume it.

## Fix #12 — Wire `specs validate-counts` into pre-push

### Code change

`apps/rhino-cli/cmd/specs_validate_counts.go` accepts an optional `--apps` StringSlice flag with the same semantics as `validate-adoption` and `validate-tree`:

- `rhino-cli specs validate-counts specs/apps/organiclever` — single positional folder, today's behavior preserved.
- `rhino-cli specs validate-counts --apps organiclever,wahidyankf` — multi-app, expands each name to `specs/apps/<app>` internally.
- `rhino-cli specs validate-counts` (no positional, no flag) — defaults to `--apps=<allowlist>` from `internal/allowlist/allowlist.go` (single source of truth shared with fixes #1 and #2).

The existing positional path argument is preserved unchanged so manual invocations and any future per-app project.json wiring keep working.

### Nx target

`apps/rhino-cli/project.json` adds:

```json
"validate:specs-counts": {
  "command": "CGO_ENABLED=0 go run -C apps/rhino-cli main.go specs validate-counts",
  "cache": true,
  "inputs": [
    "{projectRoot}/**/*.go",
    "{workspaceRoot}/specs/apps/**/README.md",
    "{workspaceRoot}/specs/apps/**/{product,system-context,containers,components,behavior}/**"
  ],
  "outputs": []
}
```

`inputs` mirror `validate:specs-tree` because both validators react to folder structure changes, not file content. Cache hits on pushes that don't touch spec layout are near-zero cost.

### Pre-push wiring

`.husky/pre-push` line extends from:

```sh
npx nx affected -t typecheck lint test:quick spec-coverage validate:specs-adoption validate:specs-tree --parallel="$PARALLEL"
```

to:

```sh
npx nx affected -t typecheck lint test:quick spec-coverage validate:specs-adoption validate:specs-tree validate:specs-counts --parallel="$PARALLEL"
```

The allowlist still lives only in `apps/rhino-cli/internal/allowlist/allowlist.go`; the pre-push hook performs no per-app routing.

### Severity coupling with Fix #8

Fix #12 must ship after Fix #8. Otherwise the new pre-push gate would fire MEDIUM-only findings on missing required folders, which is inconsistent with `validate-tree` and weakens the gate. Phase ordering enforces this — Phase 7 (Fix #8) precedes Phase 7B (Fix #12).

## Fix #13 — Wire `specs validate-links` into pre-push

### Code change

`apps/rhino-cli/cmd/specs_validate_links.go` accepts the same `--apps` StringSlice flag with allowlist default:

- `rhino-cli specs validate-links specs/apps/organiclever` — single folder, today's behavior preserved.
- `rhino-cli specs validate-links --apps organiclever,wahidyankf` — multi-app, expands each to `specs/apps/<app>` internally.
- `rhino-cli specs validate-links` (no positional, no flag) — defaults to `--apps=<allowlist>`.

### Nx target

`apps/rhino-cli/project.json` adds:

```json
"validate:specs-links": {
  "command": "CGO_ENABLED=0 go run -C apps/rhino-cli main.go specs validate-links",
  "cache": true,
  "inputs": [
    "{projectRoot}/**/*.go",
    "{workspaceRoot}/specs/apps/**/*.md"
  ],
  "outputs": []
}
```

`inputs` cover all markdown under `specs/apps/` because `validate-links` parses every `.md` file's link references. Spec folder restructures still trigger because the markdown move/rename invalidates the cache.

### Pre-push wiring

The pre-push line becomes (after Fix #12 lands):

```sh
npx nx affected -t typecheck lint test:quick spec-coverage validate:specs-adoption validate:specs-tree validate:specs-counts validate:specs-links --parallel="$PARALLEL"
```

### Out-of-scope: `docs validate-links`

`rhino-cli` also ships `docs validate-links`, which scans the entire repo's markdown (including `repo-governance/`, `docs/`, `apps/*/README.md`). It is currently ungated. This plan does not wire it because:

- The `docs *` command tree is broader than the specs/BDD/DDD enforcement scope this plan owns.
- A separate plan covering validator unification with `lint:md` and `markdown-link-check` is the natural home for that work.

The result of this plan is that **every `specs *` and `ddd *` and `spec-coverage *` command** is gated; `docs validate-links` is the only related ungated rhino-cli command, and it lives outside the user-stated scope.

## Fix #14 — Wire `validate:specs-*` into PR gate + main-CI deploy workflows

### Surfaces touched

Three `.github/workflows/*.yml` files. After this fix, all four `validate:specs-*` targets run on every gating surface:

| Surface                                                     | File                                               | Trigger                  | Aggregator that gains `specs-gate` in `needs:` |
| ----------------------------------------------------------- | -------------------------------------------------- | ------------------------ | ---------------------------------------------- |
| Pre-push (already in plan)                                  | `.husky/pre-push`                                  | every developer push     | n/a (single line)                              |
| PR quality gate                                             | `pr-quality-gate.yml`                              | every PR                 | `quality-gate`                                 |
| Reusable test-and-deploy (ayokoding/oseplatform/wahidyankf) | `_reusable-test-and-deploy.yml`                    | called by 3 cron deploys | `deploy`                                       |
| OrganicLever development deploy                             | `test-and-deploy-organiclever-web-development.yml` | cron on `main`           | `deploy`                                       |

### Why a dedicated `specs-gate` job

`validate:specs-*` are workspace-level Nx targets owned by `rhino-cli`. The PR gate's existing language jobs (`golang`, `typescript`, …) condition on `needs.detect.outputs.has-<lang>` and use either `nx affected` or `nx run-many --projects='tag:lang:<lang>'`. Appending `validate:specs-*` to the `golang` job conflates spec validation with Go-affected detection — when no Go file changes but a spec changes, the job would skip. A dedicated job that always runs (no `if:` guard) and uses `--projects=rhino-cli` is the simplest correct shape and produces a clear PR check name in the status UI.

### YAML shape (PR gate)

`pr-quality-gate.yml` adds, near the existing `naming` job:

```yaml
specs-gate:
  name: Specs gate (BDD+DDD validators)
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - uses: ./.github/actions/setup-node
    - uses: ./.github/actions/setup-golang
    - run: npx nx run-many -t validate:specs-adoption validate:specs-tree validate:specs-counts validate:specs-links --projects=rhino-cli
```

The `quality-gate` aggregator's `needs:` list is extended:

```yaml
quality-gate:
  name: Quality gate
  needs:
    [detect, format, typescript, golang, jvm, dotnet, python, rust, elixir, clojure, dart, markdown, naming, specs-gate]
```

For documentation consistency, optionally extend the inert `for job in …` loop in the aggregator to include `specs-gate` (the actual failure detection uses `contains(needs.*.result, 'failure')`, which automatically covers every entry in `needs:` and therefore automatically covers `specs-gate` the moment it is added to the `needs:` list).

### YAML shape (`_reusable-test-and-deploy.yml`)

A new job appended after `e2e:`:

```yaml
specs-gate:
  name: Specs gate (BDD+DDD validators)
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - uses: ./.github/actions/setup-node
    - uses: ./.github/actions/setup-golang
    - run: npx nx run-many -t validate:specs-adoption validate:specs-tree validate:specs-counts validate:specs-links --projects=rhino-cli
```

The `deploy` job's `needs:` list is extended:

```yaml
deploy:
  name: Deploy to production
  needs: [lint, unit, integration, e2e, detect-changes, specs-gate]
```

This ensures the daily cron deploys for `ayokoding-web`, `oseplatform-web`, and `wahidyankf-web` all block on spec validators.

### YAML shape (`test-and-deploy-organiclever-web-development.yml`)

A new job appended after `e2e:`:

```yaml
specs-gate:
  name: Specs gate (BDD+DDD validators)
  runs-on: ubuntu-latest
  timeout-minutes: 10
  steps:
    - uses: actions/checkout@v4
    - uses: ./.github/actions/setup-node
    - uses: ./.github/actions/setup-golang
    - run: npx nx run-many -t validate:specs-adoption validate:specs-tree validate:specs-counts validate:specs-links --projects=rhino-cli
```

The `deploy` job's `needs:` list is extended:

```yaml
deploy:
  name: Deploy to staging
  needs: [spec-coverage, fe-lint, be-integration, fe-integration, e2e, specs-gate]
```

### Caching note

The four `validate:specs-*` targets are `cache: true` (Phase 1.3 + Phase 7B.3). On CI, the Nx cloud cache is not configured for this repo (local cache only), so each run re-executes. The four targets together are I/O-bound (read 4 web app spec trees + run `rhino-cli`); per-run cost is well under 30 seconds and dominated by node + go setup actions. Negligible cron cost.

### Out-of-scope CI surfaces

- `pr-validate-links.yml` already runs `rhino-cli docs validate-links` (a different command tree). The plan deliberately leaves that workflow alone — `docs validate-links` is out of scope per Fix #13's "Out-of-scope" note.
- `test-organiclever-web-staging.yml` and `deploy-organiclever-web-to-production.yml` are post-merge deploy promotion workflows (no testing/validation logic). Adding spec gates there would duplicate `test-and-deploy-organiclever-web-development.yml`'s coverage with no incremental signal.

## Fix #15 — Reverse-direction step orphan check

### `stepMatcher` origin tracking

Today `internal/speccoverage/checker.go` `stepMatcher` is `{exact map[string]bool, patterns []*regexp.Regexp}`. Reverse-direction reporting needs origin per matcher entry. Replace with:

```go
type stepMatcherEntry struct {
    Kind        string         // "exact" or "pattern"
    ExactText   string         // populated when Kind == "exact"
    Pattern     *regexp.Regexp // populated when Kind == "pattern"
    PatternText string         // raw regex source for reporting (Kind == "pattern")
    File        string         // origin file (relative to repo root)
}

type stepMatcher struct {
    entries []stepMatcherEntry
    // Forward-direction lookup index (rebuilt from entries):
    exactIndex map[string]int // exact text → entries[i]
}

func (sm *stepMatcher) matches(stepText string) bool {
    normalized := normalizeWS(stepText)
    if _, ok := sm.exactIndex[normalized]; ok {
        return true
    }
    for _, e := range sm.entries {
        if e.Kind == "pattern" && e.Pattern.MatchString(normalized) {
            return true
        }
    }
    return false
}
```

Each `addStepToMatcher` and `extract*StepTexts` call site is updated to pass the origin file path through the helper chain. The forward-direction `matches` API is preserved verbatim (no behavior change to existing forward direction).

### Reverse-direction loop

After `checkOneToOne` and `checkSharedSteps` finish their forward loops, a single reverse-direction loop runs against the same matcher pool:

```go
// Build set of all Gherkin step texts seen during forward pass.
// In checkOneToOne: collected during the existing scenario walk.
// In checkSharedSteps: same — gathered as we walk specFiles.
allGherkinSteps := []string{} // populated during forward pass; unique not required

// Reverse-direction check.
var orphans []OrphanStepImpl
for _, e := range allStepTexts.entries {
    matched := false
    switch e.Kind {
    case "exact":
        for _, gs := range allGherkinSteps {
            if normalizeWS(gs) == e.ExactText {
                matched = true
                break
            }
        }
    case "pattern":
        for _, gs := range allGherkinSteps {
            if e.Pattern.MatchString(normalizeWS(gs)) {
                matched = true
                break
            }
        }
    }
    if !matched {
        orphans = append(orphans, OrphanStepImpl{
            File:        e.File,
            MatcherKind: e.Kind,
            MatcherText: ifElse(e.Kind == "exact", e.ExactText, e.PatternText),
        })
    }
}
```

Naive O(N×M) loop is fine — typical project has under 500 step impls and under 1000 Gherkin steps. If profiling later surfaces a hotspot, rebuild as `map[string]bool` for exacts and a single regex union pass.

### `OrphanStepImpl` finding type

`internal/speccoverage/types.go` adds:

```go
// OrphanStepImpl is a step implementation in source code with no Gherkin step matching it.
type OrphanStepImpl struct {
    File        string // relative path from repo root
    MatcherKind string // "exact" or "pattern"
    MatcherText string // exact step text or raw regex source
}
```

`CheckResult` gains:

```go
type CheckResult struct {
    TotalSpecs       int
    TotalScenarios   int
    TotalSteps       int
    Gaps             []CoverageGap
    ScenarioGaps     []ScenarioGap
    StepGaps         []StepGap
    OrphanStepImpls  []OrphanStepImpl // NEW
    Duration         time.Duration
}
```

### Exit logic

`cmd/spec_coverage_validate.go` `runValidateSpecCoverage` extends the `hasGaps` check:

```go
hasGaps := len(result.Gaps) > 0 ||
    len(result.ScenarioGaps) > 0 ||
    len(result.StepGaps) > 0 ||
    len(result.OrphanStepImpls) > 0 // NEW
if hasGaps {
    if !quiet && output == "text" {
        // existing prints …
        if len(result.OrphanStepImpls) > 0 {
            _, _ = fmt.Fprintf(cmd.OutOrStderr(),
                "❌ Found %d orphan step implementation(s) (no Gherkin step matches them)\n",
                len(result.OrphanStepImpls))
        }
    }
    return fmt.Errorf("spec coverage gaps found: %d file gap(s), %d scenario gap(s), %d step gap(s), %d orphan step impl(s)",
        len(result.Gaps), len(result.ScenarioGaps), len(result.StepGaps), len(result.OrphanStepImpls))
}
```

### Output formatters

`reporter.go` text/markdown/json formatters render the new finding type:

```go
// FormatText (excerpt):
if len(r.OrphanStepImpls) > 0 {
    fmt.Fprintf(&buf, "\nOrphan step implementations (no Gherkin step matches):\n")
    for _, o := range r.OrphanStepImpls {
        fmt.Fprintf(&buf, "  %s: [%s] %s\n", o.File, o.MatcherKind, o.MatcherText)
    }
}

// FormatJSON: include "orphan_step_impls": [...] alongside existing keys.
// FormatMarkdown: render as a section "## Orphan step implementations" with a table.
```

### Pre-flight orphan audit (Phase 5B.4)

Before merging the plan, the new check runs against all 15 spec-coverage-wired projects. Audit script:

```bash
# Run from repo root inside the worktree.
projects=(
  ayokoding-cli ayokoding-web ayokoding-web-be-e2e ayokoding-web-fe-e2e
  organiclever-be organiclever-be-e2e organiclever-web organiclever-web-e2e
  oseplatform-cli oseplatform-web oseplatform-web-be-e2e oseplatform-web-fe-e2e
  rhino-cli wahidyankf-web wahidyankf-web-fe-e2e
)
fail=0
for p in "${projects[@]}"; do
  echo "=== $p ==="
  if ! npx nx run "$p:spec-coverage" 2>&1 | tee "/tmp/sc-$p.log"; then
    fail=1
    grep -E "Orphan step implementation" "/tmp/sc-$p.log" || true
  fi
done
exit $fail
```

For each project that fails, the orphan(s) are either:

1. **Real orphans** — delete the orphan step impl in the same plan (or rename/rework if the underlying scenario was renamed).
2. **False positives from regex extraction limitations** — add a targeted matcher tweak in `checker.go` (e.g. handle a previously-missing string-quote style). Track each tweak as its own commit.

No project receives an opt-out flag; the audit is the gate.

### Mode coverage

The reverse-direction check runs in **both** modes:

- **Default mode** (`checkOneToOne`): The matcher pool is built once from `extractAllStepTexts(opts.AppDir)` regardless of which test files match which feature stems. Reverse-direction validates the entire pool against the union of all Gherkin step texts seen across all `.feature` files in the spec tree. Identical pool, identical Gherkin set as `--shared-steps` mode.
- **`--shared-steps` mode** (`checkSharedSteps`): same pool, same Gherkin set; the reverse-direction call is identical.

Implementation: extract reverse-direction logic into `checkOrphanStepImpls(allStepTexts, allGherkinSteps)` and call it from both `checkOneToOne` and `checkSharedSteps` after the forward loop completes.

### No escape hatch

Per scope decision (2026-05-10): no `--allow-orphan-steps` flag, no env var. Rationale: any orphan is either a real bug or a regex-extraction false positive — both are surface-able and fixable in this plan. Adding an escape hatch would replicate the `OSE_RHINO_DDD_SEVERITY` story and create another silent-suppression channel.

## Test plan

Each fix has its own Gherkin scenarios in `prd.md`. Tests live in `apps/rhino-cli/cmd/<file>_test.go` (godog unit) and `apps/rhino-cli/cmd/<file>.integration_test.go` (godog integration with real fs). Aggregate: ~61 new test scenarios across the 15 fixes (Fix #14 contributes 4 acceptance scenarios verified via grep + workflow-run smoke tests rather than Go unit tests, since the artifact is YAML, not Go code; Fix #15 contributes 7 acceptance scenarios — 6 Go-side unit/integration tests plus 1 cross-project pre-flight audit verified via shell script in Phase 5B.4). Breakdown: 45 Gherkin scenarios from fixes 1-13 in `prd.md` + 4 Gherkin scenarios from Fix #14 + 7 Gherkin scenarios from Fix #15 + ~5 unit/integration test layer variants where the same Gherkin scenario maps to both a unit-level godog test and an integration-level godog test (primarily fixes #4, #5, #6 which touch core validator logic exercised at both test layers).

Coverage gate: ≥90% on `apps/rhino-cli/` (existing). Each fix's TDD red-step writes the failing test first; green-step implements; refactor cleans up.

## Migration considerations

- **Plans 1, 2, 3 ship without `code_lang:`** — they default to `[ts, tsx]`, which is correct for all three since their bounded contexts live entirely in TS. Fix #4 lets them later add `code_lang: [ts, fs]` when a multi-surface BC is introduced.
- **organiclever-be DDD wiring (fix #3)** is independent of fix #4. organiclever's 9 bounded contexts all live under `apps/organiclever-web/src/contexts/<bc>` (TypeScript) today; organiclever-be carries no domain code (health-check only). Fix #3 wires the same TS-against-TS validators that organiclever-web already runs, with cache invalidation triggered when the registry or glossaries change. No `code_lang: [fs]` migration is required for fix #3 to land; that becomes relevant only if a future plan introduces an organiclever-be BC with F# domain code.
- **Pre-push allowlist gate (fixes #1, #2)** must wait until all 4 web apps are on the new format. If plan 4 is delivered before any of plans 1-3, the new pre-push gate would fail on day one. Hard dependency — flagged in `delivery.md` Phase 0.
- **Plan 3 multi-perspective registry (fix #11)** — plan 3 today registers `gherkin: behavior/web/gherkin/<bc>` for multi-perspective BCs (workaround). After fix #11 + fix #5 land, plan 3's registry can be migrated to `gherkin: [behavior/web/gherkin/<bc>, behavior/api/gherkin/<bc>]` per multi-perspective BC. Migration is a separate small follow-up commit, not part of plan 4 itself, since it only edits one yaml file.

## Risk and rollback

- Each fix is its own commit. Rollback per-fix via `git revert <sha>`.
- Pre-push gate addition (`.husky/pre-push`) lands LAST so a partial-state revert doesn't lock developers out of pushing.
- env var rename (#9) is intentionally backward-compat for one rev; the cutover commit explicitly notes the deprecation window.

## Out of scope (revisit later)

- AST-based step extraction (audit LOW priority).
- Drift command implementations.
- DDD-aware `nx affected` graph.
- Validator unification with `lint:md` / `docs validate-links`.
