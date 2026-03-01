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

	// Create corresponding files in OpenCode (directory structure: {name}/SKILL.md)
	if err := os.MkdirAll(filepath.Join(opencodeSkillDir, "skill-1"), 0755); err != nil {
		t.Fatalf("Failed to create opencode skill-1 dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(opencodeSkillDir, "skill-1", "SKILL.md"), []byte("skill1"), 0644); err != nil {
		t.Fatalf("Failed to create opencode skill-1/SKILL.md: %v", err)
	}
	if err := os.MkdirAll(filepath.Join(opencodeSkillDir, "skill-2"), 0755); err != nil {
		t.Fatalf("Failed to create opencode skill-2 dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(opencodeSkillDir, "skill-2", "SKILL.md"), []byte("skill2"), 0644); err != nil {
		t.Fatalf("Failed to create opencode skill-2/SKILL.md: %v", err)
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

func TestSortedKeys(t *testing.T) {
	m := map[string]bool{
		"write": true,
		"read":  true,
		"bash":  true,
	}
	keys := sortedKeys(m)
	expected := []string{"bash", "read", "write"}
	if len(keys) != len(expected) {
		t.Fatalf("expected %d keys, got %d: %v", len(expected), len(keys), keys)
	}
	for i, k := range keys {
		if k != expected[i] {
			t.Errorf("keys[%d] = %q, want %q", i, k, expected[i])
		}
	}
}

func TestValidateAgentEquivalence_EmptyDir(t *testing.T) {
	tmpDir := t.TempDir()
	claudeDir := filepath.Join(tmpDir, ".claude", "agents")
	if err := os.MkdirAll(claudeDir, 0755); err != nil {
		t.Fatalf("failed to create claude dir: %v", err)
	}
	checks := validateAgentEquivalence(tmpDir)
	if len(checks) != 0 {
		t.Errorf("expected 0 checks for empty dir, got %d", len(checks))
	}
}

func TestValidateAgentEquivalence_InvalidClaudeDir(t *testing.T) {
	tmpDir := t.TempDir()
	// Don't create .claude/agents — should return one failed check
	checks := validateAgentEquivalence(tmpDir)
	if len(checks) != 1 || checks[0].Status != "failed" {
		t.Errorf("expected 1 failed check for missing dir, got %v", checks)
	}
}

func TestValidateAgentFile_Success(t *testing.T) {
	tmpDir := t.TempDir()

	claudeAgentPath := filepath.Join(tmpDir, "test-agent.md")
	claudeContent := "---\nname: test-agent\ndescription: A test agent\ntools:\n  - Read\n  - Write\nmodel: sonnet\n---\n\nBody.\n"
	if err := os.WriteFile(claudeAgentPath, []byte(claudeContent), 0644); err != nil {
		t.Fatalf("failed to create claude agent: %v", err)
	}

	opencodeAgentPath := filepath.Join(tmpDir, "test-agent-opencode.md")
	if err := ConvertAgent(claudeAgentPath, opencodeAgentPath, false); err != nil {
		t.Fatalf("ConvertAgent() failed: %v", err)
	}

	check := validateAgentFile("test-agent.md", claudeAgentPath, opencodeAgentPath)
	if check.Status != "passed" {
		t.Errorf("expected 'passed', got %q: %s", check.Status, check.Message)
	}
}

func TestValidateAgentFile_MissingOpenCode(t *testing.T) {
	tmpDir := t.TempDir()

	claudeAgentPath := filepath.Join(tmpDir, "test-agent.md")
	claudeContent := "---\nname: test\ndescription: Test\ntools:\n  - Read\nmodel: sonnet\n---\n\nBody.\n"
	if err := os.WriteFile(claudeAgentPath, []byte(claudeContent), 0644); err != nil {
		t.Fatalf("failed to create claude agent: %v", err)
	}

	check := validateAgentFile("test-agent.md", claudeAgentPath, filepath.Join(tmpDir, "nonexistent.md"))
	if check.Status != "failed" {
		t.Errorf("expected 'failed' for missing opencode file, got %q", check.Status)
	}
}

func TestValidateAgentFile_MissingClaude(t *testing.T) {
	tmpDir := t.TempDir()
	check := validateAgentFile("test.md", filepath.Join(tmpDir, "nonexistent.md"), filepath.Join(tmpDir, "nonexistent2.md"))
	if check.Status != "failed" {
		t.Errorf("expected 'failed' for missing claude file, got %q", check.Status)
	}
}

func TestValidateAgentFile_InvalidFrontmatter(t *testing.T) {
	tmpDir := t.TempDir()

	claudePath := filepath.Join(tmpDir, "bad.md")
	opencodePath := filepath.Join(tmpDir, "bad-opencode.md")
	if err := os.WriteFile(claudePath, []byte("no frontmatter here"), 0644); err != nil {
		t.Fatalf("failed to create file: %v", err)
	}
	if err := os.WriteFile(opencodePath, []byte("no frontmatter here"), 0644); err != nil {
		t.Fatalf("failed to create file: %v", err)
	}

	check := validateAgentFile("bad.md", claudePath, opencodePath)
	if check.Status != "failed" {
		t.Errorf("expected 'failed' for invalid frontmatter, got %q", check.Status)
	}
}

func TestValidateSkillIdentity_Success(t *testing.T) {
	tmpDir := t.TempDir()

	claudeSkillsDir := filepath.Join(tmpDir, ".claude", "skills")
	opencodeSkillDir := filepath.Join(tmpDir, ".opencode", "skill")
	skill1Claude := filepath.Join(claudeSkillsDir, "my-skill")
	skill1Opencode := filepath.Join(opencodeSkillDir, "my-skill")

	for _, d := range []string{skill1Claude, skill1Opencode} {
		if err := os.MkdirAll(d, 0755); err != nil {
			t.Fatalf("failed to create dir: %v", err)
		}
	}

	skillContent := "# My Skill\n\nSkill content."
	if err := os.WriteFile(filepath.Join(skill1Claude, "SKILL.md"), []byte(skillContent), 0644); err != nil {
		t.Fatalf("failed to create claude skill: %v", err)
	}
	if err := os.WriteFile(filepath.Join(skill1Opencode, "SKILL.md"), []byte(skillContent), 0644); err != nil {
		t.Fatalf("failed to create opencode skill: %v", err)
	}

	checks := validateSkillIdentity(tmpDir)
	if len(checks) != 1 || checks[0].Status != "passed" {
		t.Errorf("expected 1 passed check, got %v", checks)
	}
}

func TestValidateSkillIdentity_InvalidClaudeDir(t *testing.T) {
	tmpDir := t.TempDir()
	checks := validateSkillIdentity(tmpDir)
	if len(checks) != 1 || checks[0].Status != "failed" {
		t.Errorf("expected 1 failed check for missing dir, got %v", checks)
	}
}

func TestValidateSync_AllPass(t *testing.T) {
	tmpDir := t.TempDir()

	claudeAgentsDir := filepath.Join(tmpDir, ".claude", "agents")
	opencodeAgentDir := filepath.Join(tmpDir, ".opencode", "agent")
	skill1Claude := filepath.Join(tmpDir, ".claude", "skills", "skill-1")
	skill1Opencode := filepath.Join(tmpDir, ".opencode", "skill", "skill-1")

	for _, d := range []string{claudeAgentsDir, opencodeAgentDir, skill1Claude, skill1Opencode} {
		if err := os.MkdirAll(d, 0755); err != nil {
			t.Fatalf("failed to create dir: %v", err)
		}
	}

	// Create and convert agent
	claudeContent := "---\nname: test\ndescription: Test\ntools:\n  - Read\nmodel: sonnet\n---\n\nBody.\n"
	claudeAgentPath := filepath.Join(claudeAgentsDir, "test.md")
	if err := os.WriteFile(claudeAgentPath, []byte(claudeContent), 0644); err != nil {
		t.Fatalf("failed to create claude agent: %v", err)
	}
	opencodeAgentPath := filepath.Join(opencodeAgentDir, "test.md")
	if err := ConvertAgent(claudeAgentPath, opencodeAgentPath, false); err != nil {
		t.Fatalf("failed to convert agent: %v", err)
	}

	// Create matching skills
	skillContent := "# Skill"
	if err := os.WriteFile(filepath.Join(skill1Claude, "SKILL.md"), []byte(skillContent), 0644); err != nil {
		t.Fatalf("failed to create claude skill: %v", err)
	}
	if err := os.WriteFile(filepath.Join(skill1Opencode, "SKILL.md"), []byte(skillContent), 0644); err != nil {
		t.Fatalf("failed to create opencode skill: %v", err)
	}

	result, err := ValidateSync(tmpDir)
	if err != nil {
		t.Fatalf("ValidateSync() error: %v", err)
	}
	if result.TotalChecks == 0 {
		t.Error("expected non-zero total checks")
	}
	for _, check := range result.Checks {
		if check.Status == "failed" {
			t.Errorf("unexpected failed check: %s - %s", check.Name, check.Message)
		}
	}
}

func TestValidateSync_EmptyRepo(t *testing.T) {
	tmpDir := t.TempDir()

	// Create empty .claude and .opencode dirs
	for _, d := range []string{
		filepath.Join(tmpDir, ".claude", "agents"),
		filepath.Join(tmpDir, ".opencode", "agent"),
		filepath.Join(tmpDir, ".claude", "skills"),
		filepath.Join(tmpDir, ".opencode", "skill"),
	} {
		if err := os.MkdirAll(d, 0755); err != nil {
			t.Fatalf("failed to create dir: %v", err)
		}
	}

	result, err := ValidateSync(tmpDir)
	if err != nil {
		t.Fatalf("ValidateSync() error: %v", err)
	}
	// Should succeed with all empty (0 agents, 0 skills)
	if result.FailedChecks > 0 {
		for _, check := range result.Checks {
			if check.Status == "failed" {
				t.Logf("failed check: %s - %s", check.Name, check.Message)
			}
		}
		t.Errorf("expected 0 failed checks for empty repo, got %d", result.FailedChecks)
	}
}
