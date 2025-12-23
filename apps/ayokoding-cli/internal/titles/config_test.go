package titles

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLoadConfig(t *testing.T) {
	tests := []struct {
		name               string
		configContent      string
		expectError        bool
		expectedOverrides  map[string]string
		expectedLowercase  []string
	}{
		{
			name: "valid config with overrides and lowercase words",
			configContent: `overrides:
  javascript: "JavaScript"
  typescript: "TypeScript"
  cliftonstrengths: "CliftonStrengths"

lowercase_words:
  - and
  - or
  - the`,
			expectError: false,
			expectedOverrides: map[string]string{
				"javascript":       "JavaScript",
				"typescript":       "TypeScript",
				"cliftonstrengths": "CliftonStrengths",
			},
			expectedLowercase: []string{"and", "or", "the"},
		},
		{
			name: "config with uppercase keys (should normalize)",
			configContent: `overrides:
  JavaScript: "JavaScript"
  TYPESCRIPT: "TypeScript"

lowercase_words:
  - AND
  - OR`,
			expectError: false,
			expectedOverrides: map[string]string{
				"javascript": "JavaScript",
				"typescript": "TypeScript",
			},
			expectedLowercase: []string{"and", "or"},
		},
		{
			name: "empty config",
			configContent: `overrides: {}
lowercase_words: []`,
			expectError:       false,
			expectedOverrides: map[string]string{},
			expectedLowercase: []string{},
		},
		{
			name: "config without lowercase_words",
			configContent: `overrides:
  api: "API"`,
			expectError: false,
			expectedOverrides: map[string]string{
				"api": "API",
			},
			expectedLowercase: []string{},
		},
		{
			name: "config without overrides",
			configContent: `lowercase_words:
  - and
  - or`,
			expectError:       false,
			expectedOverrides: map[string]string{},
			expectedLowercase: []string{"and", "or"},
		},
		{
			name:          "invalid YAML",
			configContent: `this is not: valid: yaml: content`,
			expectError:   true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create temporary config file
			tmpDir := t.TempDir()
			configPath := filepath.Join(tmpDir, "config.yaml")

			if err := os.WriteFile(configPath, []byte(tt.configContent), 0644); err != nil {
				t.Fatalf("Failed to create temp config: %v", err)
			}

			// Load config
			config, err := LoadConfig(configPath)

			if tt.expectError {
				if err == nil {
					t.Error("Expected error but got none")
				}
				return
			}

			if err != nil {
				t.Fatalf("Unexpected error: %v", err)
			}

			// Verify overrides (normalized to lowercase keys)
			if len(config.Overrides) != len(tt.expectedOverrides) {
				t.Errorf("Override count mismatch: got %d, want %d", len(config.Overrides), len(tt.expectedOverrides))
			}

			for key, expectedValue := range tt.expectedOverrides {
				if actualValue, exists := config.Overrides[key]; !exists {
					t.Errorf("Missing override for key %q", key)
				} else if actualValue != expectedValue {
					t.Errorf("Override mismatch for %q: got %q, want %q", key, actualValue, expectedValue)
				}
			}

			// Verify lowercase words
			if len(config.LowercaseWords) != len(tt.expectedLowercase) {
				t.Errorf("Lowercase words count mismatch: got %d, want %d", len(config.LowercaseWords), len(tt.expectedLowercase))
			}

			for i, expected := range tt.expectedLowercase {
				if i >= len(config.LowercaseWords) {
					break
				}
				if config.LowercaseWords[i] != expected {
					t.Errorf("Lowercase word[%d] mismatch: got %q, want %q", i, config.LowercaseWords[i], expected)
				}
			}
		})
	}
}

func TestLoadConfig_NonExistentFile(t *testing.T) {
	// Should return empty config, not error
	config, err := LoadConfig("/nonexistent/config.yaml")
	if err != nil {
		t.Errorf("Expected no error for non-existent file, got: %v", err)
	}

	if config == nil {
		t.Fatal("Expected config to be initialized")
	}

	if len(config.Overrides) != 0 {
		t.Error("Expected empty overrides map")
	}

	if len(config.LowercaseWords) != 0 {
		t.Error("Expected empty lowercase words slice")
	}
}

func TestConfig_HasOverride(t *testing.T) {
	config := &Config{
		Overrides: map[string]string{
			"javascript": "JavaScript",
			"typescript": "TypeScript",
		},
	}

	tests := []struct {
		word     string
		expected bool
	}{
		{"javascript", true},
		{"JavaScript", true}, // Case-insensitive
		{"JAVASCRIPT", true},
		{"typescript", true},
		{"python", false},
		{"", false},
	}

	for _, tt := range tests {
		t.Run(tt.word, func(t *testing.T) {
			result := config.HasOverride(tt.word)
			if result != tt.expected {
				t.Errorf("HasOverride(%q) = %v, want %v", tt.word, result, tt.expected)
			}
		})
	}
}

func TestConfig_GetOverride(t *testing.T) {
	config := &Config{
		Overrides: map[string]string{
			"javascript": "JavaScript",
			"api":        "API",
		},
	}

	tests := []struct {
		word     string
		expected string
	}{
		{"javascript", "JavaScript"},
		{"JavaScript", "JavaScript"}, // Case-insensitive
		{"JAVASCRIPT", "JavaScript"},
		{"api", "API"},
		{"Api", "API"},
		{"python", ""}, // Not found returns empty
	}

	for _, tt := range tests {
		t.Run(tt.word, func(t *testing.T) {
			result := config.GetOverride(tt.word)
			if result != tt.expected {
				t.Errorf("GetOverride(%q) = %q, want %q", tt.word, result, tt.expected)
			}
		})
	}
}

func TestConfig_IsLowercaseWord(t *testing.T) {
	config := &Config{
		LowercaseWords: []string{"and", "or", "the", "of"},
	}

	tests := []struct {
		word     string
		expected bool
	}{
		{"and", true},
		{"And", true}, // Case-insensitive
		{"AND", true},
		{"or", true},
		{"the", true},
		{"of", true},
		{"for", false},
		{"", false},
	}

	for _, tt := range tests {
		t.Run(tt.word, func(t *testing.T) {
			result := config.IsLowercaseWord(tt.word)
			if result != tt.expected {
				t.Errorf("IsLowercaseWord(%q) = %v, want %v", tt.word, result, tt.expected)
			}
		})
	}
}
