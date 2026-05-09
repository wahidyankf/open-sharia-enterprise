package bcregistry

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// osStatFn and osReadDirFn are injectable for unit tests.
var (
	osStatFn    = os.Stat
	osReadDirFn = os.ReadDir
)

// ValidateAll loads the registry and performs all structural parity checks.
func ValidateAll(opts ValidateOptions) ([]Finding, error) {
	sev := opts.Severity
	if sev == "" {
		sev = "error"
	}

	reg, err := Load(opts.RepoRoot, opts.App)
	if err != nil {
		return nil, err
	}
	return validate(opts.RepoRoot, reg, sev), nil
}

func validate(repoRoot string, reg *Registry, severity string) []Finding {
	var findings []Finding

	registeredCode := make(map[string]bool)
	registeredGlossary := make(map[string]bool)
	registeredGherkin := make(map[string]bool)
	contextByName := make(map[string]*Context)

	for i := range reg.Contexts {
		ctx := &reg.Contexts[i]
		contextByName[ctx.Name] = ctx
		for _, c := range ctx.Code {
			registeredCode[filepath.Join(repoRoot, c)] = true
		}
		registeredGlossary[filepath.Join(repoRoot, ctx.Glossary)] = true
		registeredGherkin[filepath.Join(repoRoot, ctx.Gherkin)] = true
	}

	// Check each registered context.
	for _, ctx := range reg.Contexts {
		findings = append(findings, checkContext(repoRoot, ctx, severity)...)
	}

	// Detect orphans (only when contexts exist to infer roots).
	if len(reg.Contexts) > 0 {
		findings = append(findings, detectOrphans(repoRoot, reg, registeredCode, registeredGlossary, registeredGherkin, severity)...)
	}

	// Check relationship symmetry.
	findings = append(findings, checkRelationshipSymmetry(reg, contextByName, severity)...)

	sort.SliceStable(findings, func(i, j int) bool {
		return findings[i].File < findings[j].File
	})
	return findings
}

func checkContext(repoRoot string, ctx Context, severity string) []Finding {
	var findings []Finding

	// Each declared code path must independently satisfy the layer structure.
	for _, codeRel := range ctx.Code {
		codePath := filepath.Join(repoRoot, codeRel)
		if _, err := osStatFn(codePath); err != nil {
			findings = append(findings, Finding{
				File:     codeRel,
				Message:  fmt.Sprintf("missing code directory for context %q", ctx.Name),
				Severity: severity,
			})
			continue // can't check layers for this path if dir missing; other paths still checked
		}
		findings = append(findings, checkLayersAtPath(repoRoot, ctx, codeRel, severity)...)
	}

	// Glossary file must exist.
	glossaryPath := filepath.Join(repoRoot, ctx.Glossary)
	if _, err := osStatFn(glossaryPath); err != nil {
		findings = append(findings, Finding{
			File:     ctx.Glossary,
			Message:  fmt.Sprintf("missing glossary for context %q", ctx.Name),
			Severity: severity,
		})
	}

	// Gherkin directory must exist with ≥1 .feature file.
	findings = append(findings, checkGherkin(repoRoot, ctx, severity)...)

	return findings
}

// checkLayersAtPath enforces declared layer subfolders against a single code path.
// Per-path independent: every code path must contain ALL declared layers.
func checkLayersAtPath(repoRoot string, ctx Context, codeRel, severity string) []Finding {
	var findings []Finding
	codePath := filepath.Join(repoRoot, codeRel)

	entries, err := osReadDirFn(codePath)
	if err != nil {
		return []Finding{{
			File:     codeRel,
			Message:  fmt.Sprintf("cannot read code directory for context %q: %v", ctx.Name, err),
			Severity: severity,
		}}
	}

	actual := map[string]bool{}
	for _, e := range entries {
		if e.IsDir() {
			actual[e.Name()] = true
		}
	}

	declared := map[string]bool{}
	for _, l := range ctx.Layers {
		declared[l] = true
	}

	for _, l := range ctx.Layers {
		if !actual[l] {
			findings = append(findings, Finding{
				File:     filepath.Join(codeRel, l),
				Message:  fmt.Sprintf("missing layer %q for context %q", l, ctx.Name),
				Severity: severity,
			})
		}
	}

	for name := range actual {
		if !declared[name] {
			findings = append(findings, Finding{
				File:     filepath.Join(codeRel, name),
				Message:  fmt.Sprintf("extra layer %q found on filesystem but not declared in registry for context %q", name, ctx.Name),
				Severity: severity,
			})
		}
	}

	return findings
}

func checkGherkin(repoRoot string, ctx Context, severity string) []Finding {
	gherkinPath := filepath.Join(repoRoot, ctx.Gherkin)
	if _, err := osStatFn(gherkinPath); err != nil {
		return []Finding{{
			File:     ctx.Gherkin,
			Message:  fmt.Sprintf("missing gherkin directory for context %q", ctx.Name),
			Severity: severity,
		}}
	}

	entries, err := osReadDirFn(gherkinPath)
	if err != nil {
		return []Finding{{
			File:     ctx.Gherkin,
			Message:  fmt.Sprintf("cannot read gherkin directory for context %q: %v", ctx.Name, err),
			Severity: severity,
		}}
	}

	hasFeature := false
	for _, e := range entries {
		if !e.IsDir() && strings.HasSuffix(e.Name(), ".feature") {
			hasFeature = true
			break
		}
	}
	if !hasFeature {
		return []Finding{{
			File:     ctx.Gherkin,
			Message:  fmt.Sprintf("no feature files found in gherkin directory for context %q", ctx.Name),
			Severity: severity,
		}}
	}
	return nil
}

func detectOrphans(repoRoot string, reg *Registry, registeredCode, registeredGlossary, registeredGherkin map[string]bool, severity string) []Finding {
	var findings []Finding

	// Code roots = union of parents of every code path across every context.
	// Multi-path contexts (e.g., FE + BE) yield multiple roots.
	codeRoots := map[string]bool{}
	for _, ctx := range reg.Contexts {
		for _, c := range ctx.Code {
			codeRoots[filepath.Join(repoRoot, filepath.Dir(c))] = true
		}
	}
	sortedCodeRoots := make([]string, 0, len(codeRoots))
	for r := range codeRoots {
		sortedCodeRoots = append(sortedCodeRoots, r)
	}
	sort.Strings(sortedCodeRoots)
	for _, root := range sortedCodeRoots {
		findings = append(findings, detectOrphanDirs(root, registeredCode, "orphan code directory", "registered in bounded-contexts.yaml", severity)...)
	}

	// Glossary root = parent of first context's glossary path.
	glossaryRoot := filepath.Join(repoRoot, filepath.Dir(reg.Contexts[0].Glossary))
	findings = append(findings, detectOrphanFiles(glossaryRoot, registeredGlossary, "orphan glossary file", "registered in bounded-contexts.yaml", severity)...)

	// Gherkin root = parent of first context's gherkin path.
	gherkinRoot := filepath.Join(repoRoot, filepath.Dir(reg.Contexts[0].Gherkin))
	findings = append(findings, detectOrphanDirs(gherkinRoot, registeredGherkin, "orphan gherkin directory", "registered in bounded-contexts.yaml", severity)...)

	return findings
}

func detectOrphanDirs(root string, registered map[string]bool, kind, notReason, severity string) []Finding {
	entries, err := osReadDirFn(root)
	if err != nil {
		return nil
	}
	var findings []Finding
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		fullPath := filepath.Join(root, e.Name())
		if !registered[fullPath] {
			findings = append(findings, Finding{
				File:     fullPath,
				Message:  fmt.Sprintf(`%s %q not %s`, kind, e.Name(), notReason),
				Severity: severity,
			})
		}
	}
	return findings
}

func detectOrphanFiles(root string, registered map[string]bool, kind, notReason, severity string) []Finding {
	entries, err := osReadDirFn(root)
	if err != nil {
		return nil
	}
	var findings []Finding
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".md") {
			continue
		}
		if e.Name() == "README.md" {
			continue
		}
		fullPath := filepath.Join(root, e.Name())
		if !registered[fullPath] {
			findings = append(findings, Finding{
				File:     fullPath,
				Message:  fmt.Sprintf(`%s %q not %s`, kind, e.Name(), notReason),
				Severity: severity,
			})
		}
	}
	return findings
}

func checkRelationshipSymmetry(reg *Registry, contextByName map[string]*Context, severity string) []Finding {
	var findings []Finding

	// asymmetricKinds require a reciprocal entry.
	asymmetricKinds := map[string]bool{
		"customer-supplier": true,
		"conformist":        true,
	}

	for _, ctx := range reg.Contexts {
		for _, rel := range ctx.Relationships {
			if !asymmetricKinds[rel.Kind] {
				continue
			}
			target, ok := contextByName[rel.To]
			if !ok {
				findings = append(findings, Finding{
					File:     "specs/apps/" + reg.App + "/ddd/bounded-contexts.yaml",
					Message:  fmt.Sprintf("relationship target %q declared by %q does not exist in registry", rel.To, ctx.Name),
					Severity: severity,
				})
				continue
			}
			if !hasReciprocal(target, ctx.Name, rel.Kind) {
				findings = append(findings, Finding{
					File:     "specs/apps/" + reg.App + "/ddd/bounded-contexts.yaml",
					Message:  fmt.Sprintf(`relationship asymmetry: %q → %q (%s) but %q has no reciprocal entry`, ctx.Name, rel.To, rel.Kind, rel.To),
					Severity: severity,
				})
			}
		}
	}
	return findings
}

func hasReciprocal(ctx *Context, sourceName, kind string) bool {
	for _, rel := range ctx.Relationships {
		if rel.To == sourceName && rel.Kind == kind {
			return true
		}
	}
	return false
}
