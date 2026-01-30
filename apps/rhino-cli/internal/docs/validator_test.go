package docs

import (
	"testing"
)

func TestIsKebabCase(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected bool
	}{
		// Valid kebab-case
		{"simple word", "getting", true},
		{"single letter", "a", true},
		{"single number", "1", true},
		{"two words", "getting-started", true},
		{"three words", "foo-bar-baz", true},
		{"with numbers", "step-1-intro", true},
		{"numbers at end", "part-123", true},
		{"all numbers", "123", true},
		{"number and word", "1-intro", true},
		{"version number", "release-1.12", true},
		{"semver style", "version-1.2.3", true},
		{"dotted number", "go-1.21", true},

		// Invalid cases
		{"empty string", "", false},
		{"uppercase", "UPPERCASE", false},
		{"camelCase", "camelCase", false},
		{"PascalCase", "PascalCase", false},
		{"snake_case", "snake_case", false},
		{"with spaces", "with spaces", false},
		{"starts with hyphen", "-starts-hyphen", false},
		{"ends with hyphen", "ends-hyphen-", false},
		{"double hyphen", "double--hyphen", false},
		{"mixed case", "Mixed-Case", false},
		{"uppercase in middle", "foo-BAR-baz", false},
		{"special chars", "foo@bar", false},
		{"underscore", "foo_bar", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := isKebabCase(tt.input)
			if got != tt.expected {
				t.Errorf("isKebabCase(%q) = %v, want %v", tt.input, got, tt.expected)
			}
		})
	}
}

func TestValidateFileName(t *testing.T) {
	tests := []struct {
		name          string
		path          string
		expectValid   bool
		violationType ViolationType
	}{
		// Valid files
		{"valid tutorial", "docs/tutorials/tu__getting-started.md", true, ""},
		{"valid how-to", "docs/how-to/hoto__deploy-docker.md", true, ""},
		{"valid reference", "docs/reference/re__api-reference.md", true, ""},
		{"valid deep explanation", "docs/explanation/software/prog-lang/python/ex-so-prla-py__basics.md", true, ""},

		// Exempt files
		{"README exempt", "docs/tutorials/README.md", true, ""},
		{"metadata exempt", "docs/metadata/cache.yaml", true, ""},

		// Missing separator
		{"missing separator", "docs/tutorials/missing-separator.md", false, ViolationMissingSeparator},

		// Wrong prefix
		{"wrong prefix", "docs/tutorials/wrong__prefix.md", false, ViolationWrongPrefix},
		{"wrong prefix deep", "docs/explanation/software/bad-prefix__file.md", false, ViolationWrongPrefix},

		// Bad case
		{"uppercase content", "docs/tutorials/tu__UPPERCASE.md", false, ViolationBadCase},
		{"camelCase content", "docs/tutorials/tu__camelCase.md", false, ViolationBadCase},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			violations := validateFileName(tt.path)
			if tt.expectValid {
				if len(violations) > 0 {
					t.Errorf("validateFileName(%q) found %d violations, expected 0", tt.path, len(violations))
					for _, v := range violations {
						t.Logf("  - %s: %s", v.ViolationType, v.Message)
					}
				}
			} else {
				if len(violations) == 0 {
					t.Errorf("validateFileName(%q) found 0 violations, expected at least 1", tt.path)
				} else {
					found := false
					for _, v := range violations {
						if v.ViolationType == tt.violationType {
							found = true
							break
						}
					}
					if !found {
						t.Errorf("validateFileName(%q) expected violation type %q, got %v", tt.path, tt.violationType, violations[0].ViolationType)
					}
				}
			}
		})
	}
}

func TestValidateFileNameWithNumbers(t *testing.T) {
	tests := []struct {
		name        string
		path        string
		expectValid bool
	}{
		// Sequential numbering patterns
		{"numbered file", "docs/tutorials/tu__00-introduction.md", true},
		{"numbered step", "docs/tutorials/tu__01-setup-environment.md", true},
		{"larger number", "docs/tutorials/tu__10-advanced-concepts.md", true},
		{"date in filename", "docs/how-to/hoto__release-process-2025-11.md", true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			violations := validateFileName(tt.path)
			if tt.expectValid && len(violations) > 0 {
				t.Errorf("validateFileName(%q) found %d violations, expected 0", tt.path, len(violations))
				for _, v := range violations {
					t.Logf("  - %s: %s", v.ViolationType, v.Message)
				}
			}
			if !tt.expectValid && len(violations) == 0 {
				t.Errorf("validateFileName(%q) found 0 violations, expected at least 1", tt.path)
			}
		})
	}
}
