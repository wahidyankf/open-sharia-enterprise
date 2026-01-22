package sync

import (
	"os"
	"path/filepath"
	"testing"
)

func TestValidateAgentCount(t *testing.T) {
	tmpDir := t.TempDir()

	// Create matching counts
	claudeDir := filepath.Join(tmpDir, ".claude", "agents")
	opencodeDir := filepath.Join(tmpDir, ".opencode", "agent")

	if err := os.MkdirAll(claudeDir, 0755); err != nil {
		t.Fatalf("Failed to create claude dir: %v", err)
	}
	if err := os.MkdirAll(opencodeDir, 0755); err != nil {
		t.Fatalf("Failed to create opencode dir: %v", err)
	}

	// Create agent files
	for i := 1; i <= 3; i++ {
		filename := filepath.Join(claudeDir, "agent-"+string(rune('0'+i))+".md")
		if err := os.WriteFile(filename, []byte("test"), 0644); err != nil {
			t.Fatalf("Failed to create claude agent: %v", err)
		}

		filename = filepath.Join(opencodeDir, "agent-"+string(rune('0'+i))+".md")
		if err := os.WriteFile(filename, []byte("test"), 0644); err != nil {
			t.Fatalf("Failed to create opencode agent: %v", err)
		}
	}

	// Create README.md (should be ignored)
	if err := os.WriteFile(filepath.Join(claudeDir, "README.md"), []byte("readme"), 0644); err != nil {
		t.Fatalf("Failed to create README: %v", err)
	}

	check := validateAgentCount(tmpDir)

	if check.Status != "passed" {
		t.Errorf("Expected status 'passed', got '%s'", check.Status)
	}
}

func TestValidateAgentCountMismatch(t *testing.T) {
	tmpDir := t.TempDir()

	// Create mismatched counts
	claudeDir := filepath.Join(tmpDir, ".claude", "agents")
	opencodeDir := filepath.Join(tmpDir, ".opencode", "agent")

	if err := os.MkdirAll(claudeDir, 0755); err != nil {
		t.Fatalf("Failed to create claude dir: %v", err)
	}
	if err := os.MkdirAll(opencodeDir, 0755); err != nil {
		t.Fatalf("Failed to create opencode dir: %v", err)
	}

	// Claude has 3 agents
	for i := 1; i <= 3; i++ {
		filename := filepath.Join(claudeDir, "agent-"+string(rune('0'+i))+".md")
		if err := os.WriteFile(filename, []byte("test"), 0644); err != nil {
			t.Fatalf("Failed to create claude agent: %v", err)
		}
	}

	// OpenCode has 2 agents
	for i := 1; i <= 2; i++ {
		filename := filepath.Join(opencodeDir, "agent-"+string(rune('0'+i))+".md")
		if err := os.WriteFile(filename, []byte("test"), 0644); err != nil {
			t.Fatalf("Failed to create opencode agent: %v", err)
		}
	}

	check := validateAgentCount(tmpDir)

	if check.Status != "failed" {
		t.Errorf("Expected status 'failed', got '%s'", check.Status)
	}
}

func TestValidateSkillCount(t *testing.T) {
	tmpDir := t.TempDir()

	// Create skill directories
	claudeSkillsDir := filepath.Join(tmpDir, ".claude", "skills")
	opencodeSkillDir := filepath.Join(tmpDir, ".opencode", "skill")

	skill1Dir := filepath.Join(claudeSkillsDir, "skill-1")
	skill2Dir := filepath.Join(claudeSkillsDir, "skill-2")

	if err := os.MkdirAll(skill1Dir, 0755); err != nil {
		t.Fatalf("Failed to create skill1 dir: %v", err)
	}
	if err := os.MkdirAll(skill2Dir, 0755); err != nil {
		t.Fatalf("Failed to create skill2 dir: %v", err)
	}
	if err := os.MkdirAll(opencodeSkillDir, 0755); err != nil {
		t.Fatalf("Failed to create opencode skill dir: %v", err)
	}

	// Create SKILL.md files in Claude
	if err := os.WriteFile(filepath.Join(skill1Dir, "SKILL.md"), []byte("skill1"), 0644); err != nil {
		t.Fatalf("Failed to create skill1 SKILL.md: %v", err)
	}
	if err := os.WriteFile(filepath.Join(skill2Dir, "SKILL.md"), []byte("skill2"), 0644); err != nil {
		t.Fatalf("Failed to create skill2 SKILL.md: %v", err)
	}

	// Create corresponding files in OpenCode
	if err := os.WriteFile(filepath.Join(opencodeSkillDir, "skill-1.md"), []byte("skill1"), 0644); err != nil {
		t.Fatalf("Failed to create opencode skill-1.md: %v", err)
	}
	if err := os.WriteFile(filepath.Join(opencodeSkillDir, "skill-2.md"), []byte("skill2"), 0644); err != nil {
		t.Fatalf("Failed to create opencode skill-2.md: %v", err)
	}

	check := validateSkillCount(tmpDir)

	if check.Status != "passed" {
		t.Errorf("Expected status 'passed', got '%s'", check.Status)
	}
}

func TestValidateSkillFile(t *testing.T) {
	tmpDir := t.TempDir()

	// Create identical skill files
	skillContent := "# Test Skill\n\nThis is a test skill."

	claudePath := filepath.Join(tmpDir, "claude-skill.md")
	opencodePath := filepath.Join(tmpDir, "opencode-skill.md")

	if err := os.WriteFile(claudePath, []byte(skillContent), 0644); err != nil {
		t.Fatalf("Failed to create claude skill: %v", err)
	}
	if err := os.WriteFile(opencodePath, []byte(skillContent), 0644); err != nil {
		t.Fatalf("Failed to create opencode skill: %v", err)
	}

	check := validateSkillFile("test-skill", claudePath, opencodePath)

	if check.Status != "passed" {
		t.Errorf("Expected status 'passed', got '%s'", check.Status)
	}
}

func TestValidateSkillFileMismatch(t *testing.T) {
	tmpDir := t.TempDir()

	// Create different skill files
	claudePath := filepath.Join(tmpDir, "claude-skill.md")
	opencodePath := filepath.Join(tmpDir, "opencode-skill.md")

	if err := os.WriteFile(claudePath, []byte("Claude content"), 0644); err != nil {
		t.Fatalf("Failed to create claude skill: %v", err)
	}
	if err := os.WriteFile(opencodePath, []byte("OpenCode content"), 0644); err != nil {
		t.Fatalf("Failed to create opencode skill: %v", err)
	}

	check := validateSkillFile("test-skill", claudePath, opencodePath)

	if check.Status != "failed" {
		t.Errorf("Expected status 'failed', got '%s'", check.Status)
	}
}

func TestToolsMatch(t *testing.T) {
	tests := []struct {
		name     string
		a        map[string]bool
		b        map[string]bool
		expected bool
	}{
		{
			name:     "matching tools",
			a:        map[string]bool{"read": true, "write": true},
			b:        map[string]bool{"read": true, "write": true},
			expected: true,
		},
		{
			name:     "different tools",
			a:        map[string]bool{"read": true, "write": true},
			b:        map[string]bool{"read": true, "edit": true},
			expected: false,
		},
		{
			name:     "different lengths",
			a:        map[string]bool{"read": true},
			b:        map[string]bool{"read": true, "write": true},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := toolsMatch(tt.a, tt.b)
			if result != tt.expected {
				t.Errorf("toolsMatch() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestSkillsMatch(t *testing.T) {
	tests := []struct {
		name     string
		a        []string
		b        []string
		expected bool
	}{
		{
			name:     "matching skills",
			a:        []string{"skill-1", "skill-2"},
			b:        []string{"skill-1", "skill-2"},
			expected: true,
		},
		{
			name:     "different skills",
			a:        []string{"skill-1", "skill-2"},
			b:        []string{"skill-1", "skill-3"},
			expected: false,
		},
		{
			name:     "different lengths",
			a:        []string{"skill-1"},
			b:        []string{"skill-1", "skill-2"},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := skillsMatch(tt.a, tt.b)
			if result != tt.expected {
				t.Errorf("skillsMatch() = %v, want %v", result, tt.expected)
			}
		})
	}
}

func TestCountMarkdownFiles(t *testing.T) {
	tmpDir := t.TempDir()

	// Create test files
	if err := os.WriteFile(filepath.Join(tmpDir, "file1.md"), []byte("test"), 0644); err != nil {
		t.Fatalf("Failed to create file1.md: %v", err)
	}
	if err := os.WriteFile(filepath.Join(tmpDir, "file2.md"), []byte("test"), 0644); err != nil {
		t.Fatalf("Failed to create file2.md: %v", err)
	}
	if err := os.WriteFile(filepath.Join(tmpDir, "README.md"), []byte("readme"), 0644); err != nil {
		t.Fatalf("Failed to create README.md: %v", err)
	}
	if err := os.WriteFile(filepath.Join(tmpDir, "other.txt"), []byte("test"), 0644); err != nil {
		t.Fatalf("Failed to create other.txt: %v", err)
	}

	count := countMarkdownFiles(tmpDir)

	// Should count file1.md and file2.md, but not README.md or other.txt
	if count != 2 {
		t.Errorf("Expected count 2, got %d", count)
	}
}
