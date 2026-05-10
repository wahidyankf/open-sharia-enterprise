package bcregistry

import (
	"fmt"
	"os"
	"path/filepath"

	"gopkg.in/yaml.v3"
)

// osReadFileFn is injectable for unit tests.
var osReadFileFn = func(path string) ([]byte, error) { return os.ReadFile(path) } //nolint:gosec

// Load reads and parses the bounded-context registry at
// specs/apps/<app>/ddd/bounded-contexts.yaml relative to repoRoot.
// Rejects registries whose Version field does not match SchemaVersion.
func Load(repoRoot, app string) (*Registry, error) {
	path := filepath.Join(repoRoot, "specs", "apps", app, "ddd", "bounded-contexts.yaml")
	data, err := osReadFileFn(path)
	if err != nil {
		return nil, fmt.Errorf("registry not found for app %q at %s: %w", app, path, err)
	}
	var reg Registry
	if err := yaml.Unmarshal(data, &reg); err != nil {
		return nil, fmt.Errorf("failed to parse registry for app %q: %w", app, err)
	}
	if reg.Version != SchemaVersion {
		return nil, fmt.Errorf("registry for app %q has unsupported version %d (expected %d) at %s", app, reg.Version, SchemaVersion, path)
	}
	for i := range reg.Contexts {
		if len(reg.Contexts[i].Code) == 0 {
			return nil, fmt.Errorf("registry for app %q context %q has empty code list at %s", app, reg.Contexts[i].Name, path)
		}
		if len(reg.Contexts[i].Gherkin) == 0 {
			return nil, fmt.Errorf("registry for app %q context %q has empty gherkin list at %s", app, reg.Contexts[i].Name, path)
		}
		if len(reg.Contexts[i].CodeLang) == 0 {
			reg.Contexts[i].CodeLang = []string{"ts", "tsx"}
		}
		if err := validateCodeLang(reg.Contexts[i].CodeLang); err != nil {
			return nil, fmt.Errorf("registry context %q: %w", reg.Contexts[i].Name, err)
		}
	}
	return &reg, nil
}
