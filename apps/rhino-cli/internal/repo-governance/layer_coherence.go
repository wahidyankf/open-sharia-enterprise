package repogovernance

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
)

// LayerCoherenceFinding describes a single layer-coherence violation between
// the two governance index documents.
//
// File is the absolute (or repo-relative) filesystem path of the offending
// file when the finding pertains to a single file (e.g. an intra-file name
// disagreement); for cross-file findings that span both governance docs it
// holds a stable composite key (e.g. "repo-governance/README.md+repo-governance/repository-governance-architecture.md").
// Severity is one of "fail" or "warn" (currently always "fail"). Kind is a
// stable machine-readable identifier. Message is the human-readable summary.
type LayerCoherenceFinding struct {
	// File identifies the offending file (or composite path for cross-file findings).
	File string
	// Severity is one of "fail" or "warn".
	Severity string
	// Kind is the stable machine-readable finding category.
	Kind string
	// Message is the human-readable description of the violation.
	Message string
}

// Layer-coherence finding kind constants.
const (
	// LayerCoherenceIntraFileNameConflict indicates a single file declares
	// the same layer number with two different names.
	LayerCoherenceIntraFileNameConflict = "intra-file-name-conflict"
	// LayerCoherenceCrossFileNumberMismatch indicates the two docs declare
	// non-identical sets of layer numbers.
	LayerCoherenceCrossFileNumberMismatch = "cross-file-number-mismatch"
	// LayerCoherenceCrossFileNameMismatch indicates the two docs agree on a
	// layer number but disagree on its name.
	LayerCoherenceCrossFileNameMismatch = "cross-file-name-mismatch"
	// LayerCoherenceNumberingGap indicates the union of declared layer
	// numbers is not contiguous (i.e. there is a hole between 0 and max).
	LayerCoherenceNumberingGap = "numbering-gap"
	// LayerCoherenceMissingDoc indicates one of the two required governance
	// documents could not be read.
	LayerCoherenceMissingDoc = "missing-doc"
)

// layerCoherenceArchPath is the repo-relative path of the architecture
// document audited by the layer-coherence audit.
const layerCoherenceArchPath = "repo-governance/repository-governance-architecture.md"

// layerCoherenceReadmePath is the repo-relative path of the README index
// audited by the layer-coherence audit.
const layerCoherenceReadmePath = "repo-governance/README.md"

// Two regexes extract layer declarations from the two supported markdown
// forms used across the governance corpus:
//
//   - Bold-span form, used in repo-governance/README.md and similar quick-
//     reference index pages: `**Layer N: Name**`
//   - H2 heading form, used in repo-governance/repository-governance-architecture.md
//     as section anchors: `## Layer N: Name (...)` (the parenthetical
//     descriptor is required and is discarded).
//
// Both forms capture the same two pieces of information: the integer layer
// number and the human-readable name. Captured names accept letters, digits,
// spaces, and hyphens and are trimmed lazily up to the closing delimiter.
var (
	layerCoherenceBoldRe = regexp.MustCompile(`\*\*Layer (\d+):\s*([A-Za-z][A-Za-z0-9 -]+?)\*\*`)
	layerCoherenceHeadRe = regexp.MustCompile(`(?m)^##\s+Layer (\d+):\s*([A-Za-z][A-Za-z0-9 -]+?)\s*\(`)
)

// AuditLayerCoherence reads the two governance index documents under repoRoot,
// extracts every `**Layer N: Name**` (or `## Layer N: Name (...)`) declaration
// from each, and returns every coherence violation. The audit enforces three
// invariants:
//
//  1. Within a single file, every occurrence of a given layer number must
//     agree on its name (intra-file conflict).
//  2. The two files must declare the same set of layer numbers
//     (cross-file number mismatch).
//  3. For every shared layer number, the two files must agree on its name
//     (cross-file name mismatch).
//  4. The union of declared layer numbers must be contiguous from 0 to max
//     (numbering gap).
//
// A missing or unreadable document produces a single "missing-doc" finding
// per absent file. Findings are sorted by (File, Kind) for deterministic
// output.
func AuditLayerCoherence(repoRoot string) ([]LayerCoherenceFinding, error) {
	archPath := filepath.Join(repoRoot, layerCoherenceArchPath)
	readmePath := filepath.Join(repoRoot, layerCoherenceReadmePath)

	var findings []LayerCoherenceFinding

	archMap, archFindings, archErr := readLayerMap(archPath)
	if archErr != nil {
		return nil, archErr
	}
	findings = append(findings, archFindings...)

	readmeMap, readmeFindings, readmeErr := readLayerMap(readmePath)
	if readmeErr != nil {
		return nil, readmeErr
	}
	findings = append(findings, readmeFindings...)

	// Both files unreadable => the only signal is the per-file missing-doc
	// findings already collected. Skip cross-file checks because there is
	// nothing to compare against.
	if archMap != nil && readmeMap != nil {
		findings = append(findings, compareLayerMaps(archMap, readmeMap, archPath, readmePath)...)
		findings = append(findings, checkNumberingGap(archMap, readmeMap, archPath, readmePath)...)
	}

	sort.SliceStable(findings, func(i, j int) bool {
		if findings[i].File == findings[j].File {
			return findings[i].Kind < findings[j].Kind
		}
		return findings[i].File < findings[j].File
	})
	return findings, nil
}

// readLayerMap reads path, applies both layer-declaration regexes, and
// returns the deduplicated number→name mapping plus any intra-file
// conflicts. When path does not exist, readLayerMap returns a nil map
// alongside a single "missing-doc" finding so the caller can skip cross-file
// checks; any other read error is propagated. Within a single file, two
// occurrences of the same number with the same name collapse to a single
// entry; two occurrences with different names produce one
// intra-file-name-conflict finding and the first-encountered name is
// retained.
func readLayerMap(path string) (map[int]string, []LayerCoherenceFinding, error) {
	data, err := os.ReadFile(path) //nolint:gosec // trusted governance doc path
	if err != nil {
		if os.IsNotExist(err) {
			return nil, []LayerCoherenceFinding{{
				File:     path,
				Severity: "fail",
				Kind:     LayerCoherenceMissingDoc,
				Message:  fmt.Sprintf("governance doc %q does not exist", path),
			}}, nil
		}
		return nil, nil, fmt.Errorf("read %s: %w", path, err)
	}

	layers := make(map[int]string)
	var findings []LayerCoherenceFinding
	addMatch := func(numStr, name string) {
		var num int
		if _, perr := fmt.Sscanf(numStr, "%d", &num); perr != nil {
			return
		}
		trimmed := name
		if existing, ok := layers[num]; ok {
			if existing != trimmed {
				findings = append(findings, LayerCoherenceFinding{
					File:     path,
					Severity: "fail",
					Kind:     LayerCoherenceIntraFileNameConflict,
					Message: fmt.Sprintf(
						"file declares Layer %d with two different names: %q and %q",
						num, existing, trimmed,
					),
				})
			}
			return
		}
		layers[num] = trimmed
	}

	for _, m := range layerCoherenceBoldRe.FindAllSubmatch(data, -1) {
		addMatch(string(m[1]), string(m[2]))
	}
	for _, m := range layerCoherenceHeadRe.FindAllSubmatch(data, -1) {
		addMatch(string(m[1]), string(m[2]))
	}

	return layers, findings, nil
}

// compareLayerMaps emits one cross-file-number-mismatch finding per layer
// number present in exactly one map, plus one cross-file-name-mismatch
// finding per shared layer number whose name differs between the two maps.
// The composite File field uses the form "archPath+readmePath" so cross-file
// findings sort stably and stay distinguishable from per-file findings.
func compareLayerMaps(arch, readme map[int]string, archPath, readmePath string) []LayerCoherenceFinding {
	var findings []LayerCoherenceFinding
	composite := archPath + "+" + readmePath

	// Collect the union of numbers for deterministic iteration.
	seen := make(map[int]struct{})
	for n := range arch {
		seen[n] = struct{}{}
	}
	for n := range readme {
		seen[n] = struct{}{}
	}
	numbers := make([]int, 0, len(seen))
	for n := range seen {
		numbers = append(numbers, n)
	}
	sort.Ints(numbers)

	for _, n := range numbers {
		archName, archOk := arch[n]
		readmeName, readmeOk := readme[n]
		switch {
		case archOk && !readmeOk:
			findings = append(findings, LayerCoherenceFinding{
				File:     composite,
				Severity: "fail",
				Kind:     LayerCoherenceCrossFileNumberMismatch,
				Message: fmt.Sprintf(
					"Layer %d (%q) is declared in %s but missing from %s",
					n, archName, archPath, readmePath,
				),
			})
		case !archOk && readmeOk:
			findings = append(findings, LayerCoherenceFinding{
				File:     composite,
				Severity: "fail",
				Kind:     LayerCoherenceCrossFileNumberMismatch,
				Message: fmt.Sprintf(
					"Layer %d (%q) is declared in %s but missing from %s",
					n, readmeName, readmePath, archPath,
				),
			})
		case archOk && readmeOk && archName != readmeName:
			findings = append(findings, LayerCoherenceFinding{
				File:     composite,
				Severity: "fail",
				Kind:     LayerCoherenceCrossFileNameMismatch,
				Message: fmt.Sprintf(
					"Layer %d named %q in %s but %q in %s",
					n, archName, archPath, readmeName, readmePath,
				),
			})
		}
	}
	return findings
}

// checkNumberingGap emits one numbering-gap finding per integer missing from
// the contiguous 0..max range covered by the union of the two maps. Returns
// no findings when the union is empty (both files declared no layers — that
// is already surfaced via missing-doc or by absence of any layer findings).
func checkNumberingGap(arch, readme map[int]string, archPath, readmePath string) []LayerCoherenceFinding {
	composite := archPath + "+" + readmePath
	seen := make(map[int]struct{})
	for n := range arch {
		seen[n] = struct{}{}
	}
	for n := range readme {
		seen[n] = struct{}{}
	}
	if len(seen) == 0 {
		return nil
	}
	max := -1
	for n := range seen {
		if n > max {
			max = n
		}
	}
	var findings []LayerCoherenceFinding
	for i := 0; i <= max; i++ {
		if _, ok := seen[i]; !ok {
			findings = append(findings, LayerCoherenceFinding{
				File:     composite,
				Severity: "fail",
				Kind:     LayerCoherenceNumberingGap,
				Message: fmt.Sprintf(
					"layer numbering is not contiguous: Layer %d is missing between 0 and %d",
					i, max,
				),
			})
		}
	}
	return findings
}
