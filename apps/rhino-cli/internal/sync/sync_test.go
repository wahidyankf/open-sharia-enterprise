package sync

import (
	"os"
	"path/filepath"
	"testing"
)

func TestSyncAll_AgentsAndSkills(t *testing.T) {
	tmpDir := t.TempDir()

	claudeAgentsDir := filepath.Join(tmpDir, ".claude", "agents")
	skill1Dir := filepath.Join(tmpDir, ".claude", "skills", "skill-1")
	for _, d := range []string{claudeAgentsDir, skill1Dir} {
		if err := os.MkdirAll(d, 0755); err != nil {
			t.Fatalf("failed to create dir: %v", err)
		}
	}

	content := "---\nname: test-agent\ndescription: Test agent\ntools:\n  - Read\nmodel: sonnet\n---\n\nBody.\n"
	if err := os.WriteFile(filepath.Join(claudeAgentsDir, "test-agent.md"), []byte(content), 0644); err != nil {
		t.Fatalf("failed to create agent: %v", err)
	}
	if err := os.WriteFile(filepath.Join(skill1Dir, "SKILL.md"), []byte("# Skill 1"), 0644); err != nil {
		t.Fatalf("failed to create skill: %v", err)
	}

	result, err := SyncAll(SyncOptions{RepoRoot: tmpDir})
	if err != nil {
		t.Fatalf("SyncAll() error: %v", err)
	}
	if result.AgentsConverted != 1 {
		t.Errorf("expected 1 agent converted, got %d", result.AgentsConverted)
	}
	if result.SkillsCopied != 1 {
		t.Errorf("expected 1 skill copied, got %d", result.SkillsCopied)
	}
}

func TestSyncAll_AgentsOnly(t *testing.T) {
	tmpDir := t.TempDir()

	claudeAgentsDir := filepath.Join(tmpDir, ".claude", "agents")
	if err := os.MkdirAll(claudeAgentsDir, 0755); err != nil {
		t.Fatalf("failed to create dir: %v", err)
	}

	content := "---\nname: agent1\ndescription: Agent 1\ntools:\n  - Read\nmodel: haiku\n---\n\nBody.\n"
	if err := os.WriteFile(filepath.Join(claudeAgentsDir, "agent1.md"), []byte(content), 0644); err != nil {
		t.Fatalf("failed to create agent: %v", err)
	}

	result, err := SyncAll(SyncOptions{RepoRoot: tmpDir, AgentsOnly: true})
	if err != nil {
		t.Fatalf("SyncAll() error: %v", err)
	}
	if result.AgentsConverted != 1 {
		t.Errorf("expected 1 agent converted, got %d", result.AgentsConverted)
	}
	if result.SkillsCopied != 0 {
		t.Errorf("expected 0 skills in AgentsOnly mode, got %d", result.SkillsCopied)
	}
}

func TestSyncAll_SkillsOnly(t *testing.T) {
	tmpDir := t.TempDir()

	skill1Dir := filepath.Join(tmpDir, ".claude", "skills", "skill-1")
	if err := os.MkdirAll(skill1Dir, 0755); err != nil {
		t.Fatalf("failed to create dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(skill1Dir, "SKILL.md"), []byte("# Skill 1"), 0644); err != nil {
		t.Fatalf("failed to create skill: %v", err)
	}

	result, err := SyncAll(SyncOptions{RepoRoot: tmpDir, SkillsOnly: true})
	if err != nil {
		t.Fatalf("SyncAll() error: %v", err)
	}
	if result.SkillsCopied != 1 {
		t.Errorf("expected 1 skill copied, got %d", result.SkillsCopied)
	}
	if result.AgentsConverted != 0 {
		t.Errorf("expected 0 agents in SkillsOnly mode, got %d", result.AgentsConverted)
	}
}

func TestSyncAll_DryRun(t *testing.T) {
	tmpDir := t.TempDir()

	claudeAgentsDir := filepath.Join(tmpDir, ".claude", "agents")
	if err := os.MkdirAll(claudeAgentsDir, 0755); err != nil {
		t.Fatalf("failed to create dir: %v", err)
	}

	content := "---\nname: dry-agent\ndescription: Dry run agent\ntools:\n  - Read\nmodel: sonnet\n---\n\nBody.\n"
	if err := os.WriteFile(filepath.Join(claudeAgentsDir, "dry-agent.md"), []byte(content), 0644); err != nil {
		t.Fatalf("failed to create agent: %v", err)
	}

	result, err := SyncAll(SyncOptions{RepoRoot: tmpDir, AgentsOnly: true, DryRun: true})
	if err != nil {
		t.Fatalf("SyncAll() error: %v", err)
	}
	if result.AgentsConverted != 1 {
		t.Errorf("expected 1 agent in dry run, got %d", result.AgentsConverted)
	}

	// Output directory should NOT be created in dry run mode
	opencodeDir := filepath.Join(tmpDir, ".opencode", "agent")
	if _, err := os.Stat(opencodeDir); err == nil {
		t.Error("output directory should not exist in dry run mode")
	}
}

func TestSyncAll_InvalidAgentFile(t *testing.T) {
	tmpDir := t.TempDir()

	claudeAgentsDir := filepath.Join(tmpDir, ".claude", "agents")
	if err := os.MkdirAll(claudeAgentsDir, 0755); err != nil {
		t.Fatalf("failed to create dir: %v", err)
	}
	// Invalid agent: no frontmatter
	if err := os.WriteFile(filepath.Join(claudeAgentsDir, "bad-agent.md"), []byte("no frontmatter"), 0644); err != nil {
		t.Fatalf("failed to create agent: %v", err)
	}

	result, err := SyncAll(SyncOptions{RepoRoot: tmpDir, AgentsOnly: true})
	if err != nil {
		t.Fatalf("SyncAll() error: %v", err)
	}
	if result.AgentsFailed != 1 {
		t.Errorf("expected 1 failed agent, got %d", result.AgentsFailed)
	}
	if len(result.FailedFiles) != 1 {
		t.Errorf("expected 1 failed file, got %d: %v", len(result.FailedFiles), result.FailedFiles)
	}
}

func TestSyncAll_DurationSet(t *testing.T) {
	tmpDir := t.TempDir()

	claudeAgentsDir := filepath.Join(tmpDir, ".claude", "agents")
	if err := os.MkdirAll(claudeAgentsDir, 0755); err != nil {
		t.Fatalf("failed to create dir: %v", err)
	}

	result, err := SyncAll(SyncOptions{RepoRoot: tmpDir, AgentsOnly: true})
	if err != nil {
		t.Fatalf("SyncAll() error: %v", err)
	}
	if result.Duration <= 0 {
		t.Error("expected positive duration")
	}
}
