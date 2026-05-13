// Package allowlist defines the canonical set of apps subject to the
// BDD + DDD validation gates (specs validate-adoption, validate-tree,
// validate-counts, validate-links).
//
// Single source of truth: changes to the allowlist happen only here.
// Pre-push hooks, CI workflows, and rhino-cli commands all consume this
// list rather than maintaining parallel copies in shell scripts or YAML.
package allowlist

// AppsWithDDD lists every app that adopts full DDD + new specs format and
// is therefore subject to specs validate-adoption + validate-tree gates.
//
// CLI apps adopt BDD only (spec-coverage) and are excluded by design.
var AppsWithDDD = []string{
	"organiclever",
	"wahidyankf",
	"ose-platform",
	"ayokoding",
}
