// Package bcregistry loads and validates the DDD bounded-context registry
// (specs/apps/<app>/ddd/bounded-contexts.yaml) for the given application.
package bcregistry

// SchemaVersion is the registry schema version this loader accepts.
// Version 2 introduced the list-typed Context.Code field (was string in v1).
const SchemaVersion = 2

// Registry is the in-memory form of specs/apps/<app>/ddd/bounded-contexts.yaml.
type Registry struct {
	Version  int       `yaml:"version"`
	App      string    `yaml:"app"`
	Contexts []Context `yaml:"contexts"`
}

// Context describes one bounded context and its expected filesystem artefacts.
// Code is a list to permit multi-surface implementations (e.g., FE + BE);
// each path must independently satisfy the declared Layers structure.
type Context struct {
	Name          string         `yaml:"name"`
	Summary       string         `yaml:"summary"`
	Layers        []string       `yaml:"layers"`
	Code          []string       `yaml:"code"`
	Glossary      string         `yaml:"glossary"`
	Gherkin       string         `yaml:"gherkin"`
	Relationships []Relationship `yaml:"relationships"`
}

// Relationship describes an inter-context dependency.
type Relationship struct {
	To   string `yaml:"to"`
	Kind string `yaml:"kind"`
	Role string `yaml:"role"`
}

// Finding is one validation finding produced by Validate.
type Finding struct {
	File     string
	Message  string
	Severity string // "error" or "warning"
}

// ValidateOptions configures a ValidateAll call.
type ValidateOptions struct {
	RepoRoot string
	App      string
	Severity string // "error" or "warning"; empty defaults to "error"
}
