package titles

import (
	"testing"
)

func TestGenerateTitle(t *testing.T) {
	// Create test config
	config := &Config{
		Overrides: map[string]string{
			"javascript":       "JavaScript",
			"typescript":       "TypeScript",
			"cliftonstrengths": "CliftonStrengths",
			"api":              "API",
			"nodejs":           "Node.js",
		},
		LowercaseWords: []string{"and", "or", "the", "of", "in", "on", "to", "for", "with"},
	}

	tests := []struct {
		name     string
		filePath string
		expected string
	}{
		// Basic title case
		{
			name:     "simple kebab-case",
			filePath: "/path/to/programming-language.md",
			expected: "Programming Language",
		},
		{
			name:     "multiple words",
			filePath: "/path/to/software-engineering-basics.md",
			expected: "Software Engineering Basics",
		},

		// Exact filename override
		{
			name:     "exact override - cliftonstrengths",
			filePath: "/path/to/cliftonstrengths.md",
			expected: "CliftonStrengths",
		},
		{
			name:     "exact override - case insensitive",
			filePath: "/path/to/CliftonStrengths.md",
			expected: "CliftonStrengths",
		},

		// Per-word override
		{
			name:     "per-word override - javascript",
			filePath: "/path/to/javascript-basics.md",
			expected: "JavaScript Basics",
		},
		{
			name:     "per-word override - typescript",
			filePath: "/path/to/typescript-tutorial.md",
			expected: "TypeScript Tutorial",
		},
		{
			name:     "per-word override - api",
			filePath: "/path/to/rest-api-design.md",
			expected: "Rest API Design",
		},
		{
			name:     "per-word override with special chars - nodejs",
			filePath: "/path/to/nodejs-guide.md",
			expected: "Node.js Guide",
		},

		// Lowercase words (not first word)
		{
			name:     "lowercase 'and' - middle",
			filePath: "/path/to/terms-and-conditions.md",
			expected: "Terms and Conditions",
		},
		{
			name:     "lowercase 'and' - first word (should capitalize)",
			filePath: "/path/to/and-more.md",
			expected: "And More",
		},
		{
			name:     "lowercase 'or' - middle",
			filePath: "/path/to/python-or-ruby.md",
			expected: "Python or Ruby",
		},
		{
			name:     "lowercase 'the' - middle",
			filePath: "/path/to/understanding-the-basics.md",
			expected: "Understanding the Basics",
		},
		{
			name:     "multiple lowercase words",
			filePath: "/path/to/guide-to-the-framework.md",
			expected: "Guide to the Framework",
		},

		// Edge cases
		{
			name:     "underscore separator",
			filePath: "/path/to/hello_world.md",
			expected: "Hello World",
		},
		{
			name:     "mixed separators",
			filePath: "/path/to/hello-world_test.md",
			expected: "Hello World Test",
		},
		{
			name:     "leading underscore (_index)",
			filePath: "/path/to/_index.md",
			expected: "Index",
		},
		{
			name:     "multiple consecutive hyphens",
			filePath: "/path/to/hello--world.md",
			expected: "Hello World",
		},
		{
			name:     "trailing hyphen",
			filePath: "/path/to/hello-world-.md",
			expected: "Hello World",
		},
		{
			name:     "leading hyphen",
			filePath: "/path/to/-hello-world.md",
			expected: "Hello World",
		},
		{
			name:     "single word",
			filePath: "/path/to/python.md",
			expected: "Python",
		},
		{
			name:     "number in filename",
			filePath: "/path/to/module-1-basics.md",
			expected: "Module 1 Basics",
		},

		// Combined cases
		{
			name:     "override + lowercase words",
			filePath: "/path/to/javascript-and-typescript.md",
			expected: "JavaScript and TypeScript",
		},
		{
			name:     "override + title case + lowercase",
			filePath: "/path/to/building-rest-api-with-nodejs.md",
			expected: "Building Rest API with Node.js",
		},

		// Directory path (uses basename)
		{
			name:     "full path",
			filePath: "/apps/ayokoding-web/content/en/learn/software-engineering.md",
			expected: "Software Engineering",
		},
		{
			name:     "nested directory",
			filePath: "/very/long/path/to/some-content.md",
			expected: "Some Content",
		},

		// Real-world examples from ayokoding-web
		{
			name:     "corporate finance",
			filePath: "/path/to/corporate-finance.md",
			expected: "Corporate Finance",
		},
		{
			name:     "about ayokoding",
			filePath: "/path/to/about-ayokoding.md",
			expected: "About Ayokoding",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := GenerateTitle(tt.filePath, config)
			if result != tt.expected {
				t.Errorf("GenerateTitle(%q) = %q, want %q", tt.filePath, result, tt.expected)
			}
		})
	}
}

func TestGenerateTitle_EmptyConfig(t *testing.T) {
	// Config with no overrides or lowercase words
	config := &Config{
		Overrides:      map[string]string{},
		LowercaseWords: []string{},
	}

	tests := []struct {
		name     string
		filePath string
		expected string
	}{
		{
			name:     "simple case",
			filePath: "/path/to/hello-world.md",
			expected: "Hello World",
		},
		{
			name:     "with 'and' (no lowercase rule)",
			filePath: "/path/to/terms-and-conditions.md",
			expected: "Terms And Conditions",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := GenerateTitle(tt.filePath, config)
			if result != tt.expected {
				t.Errorf("GenerateTitle(%q) = %q, want %q", tt.filePath, result, tt.expected)
			}
		})
	}
}

func TestCapitalizeFirst(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"hello", "Hello"},
		{"HELLO", "Hello"},
		{"Hello", "Hello"},
		{"hELLO", "Hello"},
		{"h", "H"},
		{"", ""},
		{"123abc", "123Abc"},
		{"_hello", "_Hello"},
		{"-hello", "-Hello"},
		{"$test", "$Test"},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result := capitalizeFirst(tt.input)
			if result != tt.expected {
				t.Errorf("capitalizeFirst(%q) = %q, want %q", tt.input, result, tt.expected)
			}
		})
	}
}
