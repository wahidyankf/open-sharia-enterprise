// Package bcregistry loads and validates the DDD bounded-context registry
// (specs/apps/<app>/ddd/bounded-contexts.yaml) for the given application.
package bcregistry

import (
	"fmt"

	"gopkg.in/yaml.v3"

	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/severity"
)

// SchemaVersion is the registry schema version this loader accepts.
// Version 2 introduced the list-typed Context.Code field (was string in v1).
// The CodeLang field added on top of v2 is additive with a default, so the
// schema version is unchanged.
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
//
// CodeLang is the list of programming-language tags whose source files the
// glossary code-identifier checker should grep when validating this BC. It is
// optional in YAML; when absent the loader defaults it to ["ts", "tsx"] to
// preserve historical TS-only behaviour. Supported values are the keys of
// SupportedLangGlobs.
type Context struct {
	Name          string         `yaml:"name"`
	Summary       string         `yaml:"summary"`
	Layers        []string       `yaml:"layers"`
	Code          []string       `yaml:"code"`
	CodeLang      []string       `yaml:"code_lang"`
	Glossary      string         `yaml:"glossary"`
	Gherkin       GherkinPaths   `yaml:"gherkin"`
	Relationships []Relationship `yaml:"relationships"`
}

// GherkinPaths is a list of repo-relative gherkin directory paths for one BC.
// A scalar form in YAML auto-converts to a one-element slice via UnmarshalYAML
// so existing single-perspective registries continue to validate without edit.
// Multi-perspective BCs (e.g., a content area declared on both web/ and api/)
// declare a list form `[behavior/web/gherkin/x, behavior/api/gherkin/x]`.
type GherkinPaths []string

// UnmarshalYAML accepts both scalar (single string) and sequence (list) YAML
// shapes for the `gherkin:` field. Schema version stays at 2 because the change
// is additive with auto-conversion.
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

// Relationship describes an inter-context dependency.
type Relationship struct {
	To   string `yaml:"to"`
	Kind string `yaml:"kind"`
	Role string `yaml:"role"`
}

// KindValue parses the Kind string into a typed RelationshipKind enum.
// Returns an error when the Kind string is not a recognized value.
func (r Relationship) KindValue() (RelationshipKind, error) {
	return ParseRelationshipKind(r.Kind)
}

// RoleValue parses the Role string into a typed RelationshipRole enum.
// Returns an error when the Role string is not a recognized value.
func (r Relationship) RoleValue() (RelationshipRole, error) {
	return ParseRelationshipRole(r.Role)
}

// Finding is one validation finding produced by Validate.
type Finding struct {
	File     string
	Message  string
	Severity severity.Severity
}

// ValidateOptions configures a ValidateAll call.
type ValidateOptions struct {
	RepoRoot string
	App      string
	Severity severity.Severity
}

// SupportedLangGlobs maps each supported code_lang tag to the file-extension
// globs the glossary code-identifier checker should walk for that language.
// This is the single source of truth for the lang-tag → glob translation; the
// glossary validator computes its grep glob list as the union of these slices
// across the BC's declared CodeLang values.
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

// validateCodeLang returns nil when every entry in langs is a supported
// language tag (a key of SupportedLangGlobs); otherwise it returns an error
// naming the first unsupported tag.
func validateCodeLang(langs []string) error {
	for _, lang := range langs {
		if _, ok := SupportedLangGlobs[lang]; !ok {
			return fmt.Errorf("unsupported code_lang %q (supported: ts, tsx, fs, go, py, java, kt, rs, ex, exs, cs, clj, dart)", lang)
		}
	}
	return nil
}
