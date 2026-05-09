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

Governance docs that reference the drift commands (`governance/conventions/structure/specs-directory-structure.md` line 211 area) get a one-line update: "Drift detection commands are not currently implemented; track via the [tooling backlog]" with link to a backlog-item heading in the same governance doc.

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

## Test plan

Each fix has its own Gherkin scenarios in `prd.md`. Tests live in `apps/rhino-cli/cmd/<file>_test.go` (godog unit) and `apps/rhino-cli/cmd/<file>.integration_test.go` (godog integration with real fs). Aggregate: ~40 new test scenarios across the 10 fixes.

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
- Reverse-direction step orphan check in `--shared-steps`.
- Drift command implementations.
- DDD-aware `nx affected` graph.
- Validator unification with `lint:md` / `docs validate-links`.
