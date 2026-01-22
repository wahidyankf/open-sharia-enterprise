package sync

import (
	"os"
	"path/filepath"
	"testing"

	"gopkg.in/yaml.v3"
)

func TestExtractFrontmatter(t *testing.T) {
	tests := []struct {
		name          string
		content       string
		wantErr       bool
		expectedFront string
		expectedBody  string
	}{
		{
			name: "valid frontmatter",
			content: `---
name: test-agent
description: Test description
---

# Agent Body

This is the body content.`,
			wantErr:       false,
			expectedFront: "name: test-agent\ndescription: Test description",
			expectedBody:  "\n# Agent Body\n\nThis is the body content.",
		},
		{
			name:     "no frontmatter",
			content:  "Just content without frontmatter",
			wantErr:  true,
		},
		{
			name: "no closing marker",
			content: `---
name: test-agent
description: Test description

No closing marker`,
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			front, body, err := ExtractFrontmatter([]byte(tt.content))

			if (err != nil) != tt.wantErr {
				t.Errorf("ExtractFrontmatter() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if !tt.wantErr {
				if string(front) != tt.expectedFront {
					t.Errorf("ExtractFrontmatter() frontmatter = %q, want %q", string(front), tt.expectedFront)
				}
				if string(body) != tt.expectedBody {
					t.Errorf("ExtractFrontmatter() body = %q, want %q", string(body), tt.expectedBody)
				}
			}
		})
	}
}

func TestParseClaudeTools(t *testing.T) {
	tests := []struct {
		name     string
		input    interface{}
		expected []string
	}{
		{
			name:     "comma-separated string",
			input:    "Read, Write, Edit, Glob, Grep",
			expected: []string{"Read", "Write", "Edit", "Glob", "Grep"},
		},
		{
			name:     "array of strings",
			input:    []interface{}{"Read", "Write", "Edit"},
			expected: []string{"Read", "Write", "Edit"},
		},
		{
			name:     "comma-separated with extra spaces",
			input:    "Read,  Write  ,Edit",
			expected: []string{"Read", "Write", "Edit"},
		},
		{
			name:     "empty string",
			input:    "",
			expected: []string{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ParseClaudeTools(tt.input)

			if len(result) != len(tt.expected) {
				t.Errorf("ParseClaudeTools() length = %d, want %d", len(result), len(tt.expected))
				return
			}

			for i, tool := range result {
				if tool != tt.expected[i] {
					t.Errorf("ParseClaudeTools()[%d] = %q, want %q", i, tool, tt.expected[i])
				}
			}
		})
	}
}

func TestConvertTools(t *testing.T) {
	tests := []struct {
		name     string
		input    []string
		expected map[string]bool
	}{
		{
			name:  "standard tools",
			input: []string{"Read", "Write", "Edit", "Glob", "Grep"},
			expected: map[string]bool{
				"read":  true,
				"write": true,
				"edit":  true,
				"glob":  true,
				"grep":  true,
			},
		},
		{
			name:  "mixed case",
			input: []string{"READ", "write", "Edit"},
			expected: map[string]bool{
				"read":  true,
				"write": true,
				"edit":  true,
			},
		},
		{
			name:     "empty array",
			input:    []string{},
			expected: map[string]bool{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ConvertTools(tt.input)

			if len(result) != len(tt.expected) {
				t.Errorf("ConvertTools() length = %d, want %d", len(result), len(tt.expected))
				return
			}

			for key, value := range tt.expected {
				if result[key] != value {
					t.Errorf("ConvertTools()[%q] = %v, want %v", key, result[key], value)
				}
			}
		})
	}
}

func TestConvertModel(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{name: "sonnet", input: "sonnet", expected: "zai/glm-4.7"},
		{name: "opus", input: "opus", expected: "zai/glm-4.7"},
		{name: "haiku", input: "haiku", expected: "zai/glm-4.5-air"},
		{name: "empty", input: "", expected: "inherit"},
		{name: "whitespace", input: "  ", expected: "inherit"},
		{name: "unknown", input: "unknown-model", expected: "inherit"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ConvertModel(tt.input)
			if result != tt.expected {
				t.Errorf("ConvertModel(%q) = %q, want %q", tt.input, result, tt.expected)
			}
		})
	}
}

func TestConvertAgent(t *testing.T) {
	// Create temp directory for test
	tmpDir := t.TempDir()

	// Create input file
	inputPath := filepath.Join(tmpDir, "test-agent.md")
	inputContent := `---
name: test-agent
description: Test agent for unit testing
tools: Read, Write, Edit
model: sonnet
color: blue
skills:
  - skill-1
  - skill-2
---

# Test Agent

This is the agent body content.
`

	if err := os.WriteFile(inputPath, []byte(inputContent), 0644); err != nil {
		t.Fatalf("Failed to create test input file: %v", err)
	}

	// Convert agent
	outputPath := filepath.Join(tmpDir, "output.md")
	if err := ConvertAgent(inputPath, outputPath, false); err != nil {
		t.Fatalf("ConvertAgent() failed: %v", err)
	}

	// Read output file
	outputContent, err := os.ReadFile(outputPath)
	if err != nil {
		t.Fatalf("Failed to read output file: %v", err)
	}

	// Extract and parse frontmatter
	front, body, err := ExtractFrontmatter(outputContent)
	if err != nil {
		t.Fatalf("Failed to extract frontmatter from output: %v", err)
	}

	var agent OpenCodeAgent
	if err := yaml.Unmarshal(front, &agent); err != nil {
		t.Fatalf("Failed to parse output YAML: %v", err)
	}

	// Verify conversion
	if agent.Description != "Test agent for unit testing" {
		t.Errorf("Description = %q, want %q", agent.Description, "Test agent for unit testing")
	}

	if agent.Model != "zai/glm-4.7" {
		t.Errorf("Model = %q, want %q", agent.Model, "zai/glm-4.7")
	}

	expectedTools := map[string]bool{"read": true, "write": true, "edit": true}
	if len(agent.Tools) != len(expectedTools) {
		t.Errorf("Tools length = %d, want %d", len(agent.Tools), len(expectedTools))
	}

	for key, value := range expectedTools {
		if agent.Tools[key] != value {
			t.Errorf("Tools[%q] = %v, want %v", key, agent.Tools[key], value)
		}
	}

	if len(agent.Skills) != 2 {
		t.Errorf("Skills length = %d, want 2", len(agent.Skills))
	}

	// Verify body is preserved
	expectedBody := "\n# Test Agent\n\nThis is the agent body content.\n"
	if string(body) != expectedBody {
		t.Errorf("Body = %q, want %q", string(body), expectedBody)
	}
}

func TestConvertAgentDryRun(t *testing.T) {
	// Create temp directory for test
	tmpDir := t.TempDir()

	// Create input file
	inputPath := filepath.Join(tmpDir, "test-agent.md")
	inputContent := `---
name: test-agent
description: Test agent
tools: Read, Write
---

Body content.
`

	if err := os.WriteFile(inputPath, []byte(inputContent), 0644); err != nil {
		t.Fatalf("Failed to create test input file: %v", err)
	}

	// Convert agent with dry run
	outputPath := filepath.Join(tmpDir, "output.md")
	if err := ConvertAgent(inputPath, outputPath, true); err != nil {
		t.Fatalf("ConvertAgent() failed: %v", err)
	}

	// Verify output file was NOT created
	if _, err := os.Stat(outputPath); err == nil {
		t.Error("Output file should not exist in dry run mode")
	}
}

func TestConvertAgentWithEmptyModel(t *testing.T) {
	// Create temp directory for test
	tmpDir := t.TempDir()

	// Create input file with empty model
	inputPath := filepath.Join(tmpDir, "test-agent.md")
	inputContent := `---
name: test-agent
description: Test agent
tools: Read, Write
model:
---

Body content.
`

	if err := os.WriteFile(inputPath, []byte(inputContent), 0644); err != nil {
		t.Fatalf("Failed to create test input file: %v", err)
	}

	// Convert agent
	outputPath := filepath.Join(tmpDir, "output.md")
	if err := ConvertAgent(inputPath, outputPath, false); err != nil {
		t.Fatalf("ConvertAgent() failed: %v", err)
	}

	// Read and verify output
	outputContent, err := os.ReadFile(outputPath)
	if err != nil {
		t.Fatalf("Failed to read output file: %v", err)
	}

	front, _, err := ExtractFrontmatter(outputContent)
	if err != nil {
		t.Fatalf("Failed to extract frontmatter: %v", err)
	}

	var agent OpenCodeAgent
	if err := yaml.Unmarshal(front, &agent); err != nil {
		t.Fatalf("Failed to parse YAML: %v", err)
	}

	// Empty model should convert to "inherit"
	if agent.Model != "inherit" {
		t.Errorf("Model = %q, want %q", agent.Model, "inherit")
	}
}
