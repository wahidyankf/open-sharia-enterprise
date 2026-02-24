package cmd

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// Helper function to create a valid agent file
func createTestAgent(t *testing.T, dir, name string) {
	t.Helper()
	content := `---
name: ` + name + `
description: Test agent description
tools: Read, Write
model: sonnet
color: blue
skills:
---
Test agent body`

	if err := os.WriteFile(filepath.Join(dir, name+".md"), []byte(content), 0644); err != nil {
		t.Fatalf("Failed to create agent: %v", err)
	}
}

// Helper function to create a valid skill file
func createTestSkill(t *testing.T, dir, name string) {
	t.Helper()
	skillDir := filepath.Join(dir, name)
	if err := os.MkdirAll(skillDir, 0755); err != nil {
		t.Fatalf("Failed to create skill dir: %v", err)
	}

	content := `---
name: ` + name + `
description: Test skill description
---
Skill content`

	if err := os.WriteFile(filepath.Join(skillDir, "SKILL.md"), []byte(content), 0644); err != nil {
		t.Fatalf("Failed to create SKILL.md: %v", err)
	}
}

func TestSyncAgentsCommand_AllValid(t *testing.T) {
	// Save original working directory
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	// Create temporary test repository
	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	// Create .git directory to simulate a git repository
	gitDir := filepath.Join(tmpDir, ".git")
	if err := os.MkdirAll(gitDir, 0755); err != nil {
		t.Fatalf("Failed to create .git dir: %v", err)
	}

	// Create .claude structure
	agentsDir := filepath.Join(tmpDir, ".claude", "agents")
	skillsDir := filepath.Join(tmpDir, ".claude", "skills")
	if err := os.MkdirAll(agentsDir, 0755); err != nil {
		t.Fatalf("Failed to create agents dir: %v", err)
	}
	if err := os.MkdirAll(skillsDir, 0755); err != nil {
		t.Fatalf("Failed to create skills dir: %v", err)
	}

	// Create .opencode structure
	opencodeAgentDir := filepath.Join(tmpDir, ".opencode", "agent")
	opencodeSkillDir := filepath.Join(tmpDir, ".opencode", "skill")
	if err := os.MkdirAll(opencodeAgentDir, 0755); err != nil {
		t.Fatalf("Failed to create opencode agent dir: %v", err)
	}
	if err := os.MkdirAll(opencodeSkillDir, 0755); err != nil {
		t.Fatalf("Failed to create opencode skill dir: %v", err)
	}

	// Create test agents and skills
	createTestAgent(t, agentsDir, "test-agent-1")
	createTestAgent(t, agentsDir, "test-agent-2")
	createTestSkill(t, skillsDir, "test-skill-1")
	createTestSkill(t, skillsDir, "test-skill-2")

	// Test command execution
	cmd := syncAgentsCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	// Reset flags
	syncDryRun = false
	syncAgentsOnly = false
	syncSkillsOnly = false
	output = "text"
	verbose = false
	quiet = false

	// Execute command
	err = cmd.RunE(cmd, []string{})
	if err != nil {
		t.Errorf("Command failed: %v", err)
	}

	outputStr := buf.String()
	t.Logf("Command output:\n%s", outputStr)

	// Verify success message
	if !strings.Contains(outputStr, "✓ SUCCESS") {
		t.Error("Expected output to contain success message")
	}
	if !strings.Contains(outputStr, "Agents: 2 converted") {
		t.Error("Expected output to show 2 agents converted")
	}
	if !strings.Contains(outputStr, "Skills: 2 copied") {
		t.Error("Expected output to show 2 skills copied")
	}

	// Verify files were created in .opencode/
	if _, err := os.Stat(filepath.Join(opencodeAgentDir, "test-agent-1.md")); os.IsNotExist(err) {
		t.Error("Expected test-agent-1.md to be created in .opencode/agent/")
	}
	if _, err := os.Stat(filepath.Join(opencodeAgentDir, "test-agent-2.md")); os.IsNotExist(err) {
		t.Error("Expected test-agent-2.md to be created in .opencode/agent/")
	}
	if _, err := os.Stat(filepath.Join(opencodeSkillDir, "test-skill-1", "SKILL.md")); os.IsNotExist(err) {
		t.Error("Expected test-skill-1/SKILL.md to be created in .opencode/skill/")
	}
	if _, err := os.Stat(filepath.Join(opencodeSkillDir, "test-skill-2", "SKILL.md")); os.IsNotExist(err) {
		t.Error("Expected test-skill-2/SKILL.md to be created in .opencode/skill/")
	}
}

func TestSyncAgentsCommand_DryRun(t *testing.T) {
	// Save original working directory
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	// Create temporary test repository
	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	// Create .git directory
	gitDir := filepath.Join(tmpDir, ".git")
	if err := os.MkdirAll(gitDir, 0755); err != nil {
		t.Fatalf("Failed to create .git dir: %v", err)
	}

	// Create .claude structure
	agentsDir := filepath.Join(tmpDir, ".claude", "agents")
	skillsDir := filepath.Join(tmpDir, ".claude", "skills")
	if err := os.MkdirAll(agentsDir, 0755); err != nil {
		t.Fatalf("Failed to create agents dir: %v", err)
	}
	if err := os.MkdirAll(skillsDir, 0755); err != nil {
		t.Fatalf("Failed to create skills dir: %v", err)
	}

	// Create .opencode structure (but should NOT be modified in dry-run)
	opencodeAgentDir := filepath.Join(tmpDir, ".opencode", "agent")
	if err := os.MkdirAll(opencodeAgentDir, 0755); err != nil {
		t.Fatalf("Failed to create opencode agent dir: %v", err)
	}

	// Create test agent
	createTestAgent(t, agentsDir, "test-agent")

	// Test dry-run command
	cmd := syncAgentsCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	// Set dry-run flag
	syncDryRun = true
	syncAgentsOnly = false
	syncSkillsOnly = false
	output = "text"
	verbose = false
	quiet = false

	// Execute command
	err = cmd.RunE(cmd, []string{})
	if err != nil {
		t.Errorf("Command failed: %v", err)
	}

	outputStr := buf.String()
	t.Logf("Dry-run output:\n%s", outputStr)

	// Verify files were NOT created in dry-run mode (this is the key verification)
	if _, err := os.Stat(filepath.Join(opencodeAgentDir, "test-agent.md")); !os.IsNotExist(err) {
		t.Error("Expected test-agent.md to NOT be created in dry-run mode")
	}

	// Verify command executed successfully (dry-run should not cause errors)
	if !strings.Contains(outputStr, "SUCCESS") {
		t.Error("Expected dry-run to complete successfully")
	}
}

func TestSyncAgentsCommand_AgentsOnly(t *testing.T) {
	// Save original working directory
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	// Create temporary test repository
	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	// Create .git directory
	gitDir := filepath.Join(tmpDir, ".git")
	if err := os.MkdirAll(gitDir, 0755); err != nil {
		t.Fatalf("Failed to create .git dir: %v", err)
	}

	// Create .claude structure
	agentsDir := filepath.Join(tmpDir, ".claude", "agents")
	skillsDir := filepath.Join(tmpDir, ".claude", "skills")
	if err := os.MkdirAll(agentsDir, 0755); err != nil {
		t.Fatalf("Failed to create agents dir: %v", err)
	}
	if err := os.MkdirAll(skillsDir, 0755); err != nil {
		t.Fatalf("Failed to create skills dir: %v", err)
	}

	// Create .opencode structure
	opencodeAgentDir := filepath.Join(tmpDir, ".opencode", "agent")
	opencodeSkillDir := filepath.Join(tmpDir, ".opencode", "skill")
	if err := os.MkdirAll(opencodeAgentDir, 0755); err != nil {
		t.Fatalf("Failed to create opencode agent dir: %v", err)
	}
	if err := os.MkdirAll(opencodeSkillDir, 0755); err != nil {
		t.Fatalf("Failed to create opencode skill dir: %v", err)
	}

	// Create test agents and skills
	createTestAgent(t, agentsDir, "test-agent")
	createTestSkill(t, skillsDir, "test-skill")

	// Test agents-only command
	cmd := syncAgentsCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	// Set agents-only flag
	syncDryRun = false
	syncAgentsOnly = true
	syncSkillsOnly = false
	output = "text"
	verbose = false
	quiet = false

	// Execute command
	err = cmd.RunE(cmd, []string{})
	if err != nil {
		t.Errorf("Command failed: %v", err)
	}

	outputStr := buf.String()
	t.Logf("Agents-only output:\n%s", outputStr)

	// Verify only agents were synced
	if !strings.Contains(outputStr, "Agents: 1 converted") {
		t.Error("Expected output to show 1 agent converted")
	}
	if !strings.Contains(outputStr, "Skills: 0 copied") {
		t.Error("Expected output to show 0 skills copied (agents-only mode)")
	}

	// Verify agent was created but skill was not
	if _, err := os.Stat(filepath.Join(opencodeAgentDir, "test-agent.md")); os.IsNotExist(err) {
		t.Error("Expected test-agent.md to be created")
	}
	if _, err := os.Stat(filepath.Join(opencodeSkillDir, "test-skill.md")); !os.IsNotExist(err) {
		t.Error("Expected test-skill.md to NOT be created in agents-only mode")
	}
}

func TestSyncAgentsCommand_SkillsOnly(t *testing.T) {
	// Save original working directory
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	// Create temporary test repository
	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	// Create .git directory
	gitDir := filepath.Join(tmpDir, ".git")
	if err := os.MkdirAll(gitDir, 0755); err != nil {
		t.Fatalf("Failed to create .git dir: %v", err)
	}

	// Create .claude structure
	agentsDir := filepath.Join(tmpDir, ".claude", "agents")
	skillsDir := filepath.Join(tmpDir, ".claude", "skills")
	if err := os.MkdirAll(agentsDir, 0755); err != nil {
		t.Fatalf("Failed to create agents dir: %v", err)
	}
	if err := os.MkdirAll(skillsDir, 0755); err != nil {
		t.Fatalf("Failed to create skills dir: %v", err)
	}

	// Create .opencode structure
	opencodeAgentDir := filepath.Join(tmpDir, ".opencode", "agent")
	opencodeSkillDir := filepath.Join(tmpDir, ".opencode", "skill")
	if err := os.MkdirAll(opencodeAgentDir, 0755); err != nil {
		t.Fatalf("Failed to create opencode agent dir: %v", err)
	}
	if err := os.MkdirAll(opencodeSkillDir, 0755); err != nil {
		t.Fatalf("Failed to create opencode skill dir: %v", err)
	}

	// Create test agents and skills
	createTestAgent(t, agentsDir, "test-agent")
	createTestSkill(t, skillsDir, "test-skill")

	// Test skills-only command
	cmd := syncAgentsCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	// Set skills-only flag
	syncDryRun = false
	syncAgentsOnly = false
	syncSkillsOnly = true
	output = "text"
	verbose = false
	quiet = false

	// Execute command
	err = cmd.RunE(cmd, []string{})
	if err != nil {
		t.Errorf("Command failed: %v", err)
	}

	outputStr := buf.String()
	t.Logf("Skills-only output:\n%s", outputStr)

	// Verify only skills were synced
	if !strings.Contains(outputStr, "Agents: 0 converted") {
		t.Error("Expected output to show 0 agents converted (skills-only mode)")
	}
	if !strings.Contains(outputStr, "Skills: 1 copied") {
		t.Error("Expected output to show 1 skill copied")
	}

	// Verify skill was created but agent was not
	if _, err := os.Stat(filepath.Join(opencodeAgentDir, "test-agent.md")); !os.IsNotExist(err) {
		t.Error("Expected test-agent.md to NOT be created in skills-only mode")
	}
	if _, err := os.Stat(filepath.Join(opencodeSkillDir, "test-skill", "SKILL.md")); os.IsNotExist(err) {
		t.Error("Expected test-skill/SKILL.md to be created")
	}
}

func TestSyncAgentsCommand_ConflictingFlags(t *testing.T) {
	// Save original working directory
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	// Create temporary test repository
	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	// Create .git directory
	gitDir := filepath.Join(tmpDir, ".git")
	if err := os.MkdirAll(gitDir, 0755); err != nil {
		t.Fatalf("Failed to create .git dir: %v", err)
	}

	// Test command with conflicting flags
	cmd := syncAgentsCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	// Set conflicting flags
	syncDryRun = false
	syncAgentsOnly = true
	syncSkillsOnly = true // Conflict!
	output = "text"
	verbose = false
	quiet = false

	// Execute command (should fail)
	err = cmd.RunE(cmd, []string{})
	if err == nil {
		t.Error("Expected command to fail with conflicting flags")
	}

	if !strings.Contains(err.Error(), "cannot use both") {
		t.Errorf("Expected error message about conflicting flags, got: %v", err)
	}
}

func TestSyncAgentsCommand_JSONOutput(t *testing.T) {
	// Save original working directory
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	// Create temporary test repository
	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	// Create .git directory
	gitDir := filepath.Join(tmpDir, ".git")
	if err := os.MkdirAll(gitDir, 0755); err != nil {
		t.Fatalf("Failed to create .git dir: %v", err)
	}

	// Create .claude structure
	agentsDir := filepath.Join(tmpDir, ".claude", "agents")
	skillsDir := filepath.Join(tmpDir, ".claude", "skills")
	if err := os.MkdirAll(agentsDir, 0755); err != nil {
		t.Fatalf("Failed to create agents dir: %v", err)
	}
	if err := os.MkdirAll(skillsDir, 0755); err != nil {
		t.Fatalf("Failed to create skills dir: %v", err)
	}

	// Create .opencode structure
	opencodeAgentDir := filepath.Join(tmpDir, ".opencode", "agent")
	opencodeSkillDir := filepath.Join(tmpDir, ".opencode", "skill")
	if err := os.MkdirAll(opencodeAgentDir, 0755); err != nil {
		t.Fatalf("Failed to create opencode agent dir: %v", err)
	}
	if err := os.MkdirAll(opencodeSkillDir, 0755); err != nil {
		t.Fatalf("Failed to create opencode skill dir: %v", err)
	}

	// Create test agent
	createTestAgent(t, agentsDir, "test-agent")
	createTestSkill(t, skillsDir, "test-skill")

	// Test JSON output
	cmd := syncAgentsCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	// Set JSON output flag
	syncDryRun = false
	syncAgentsOnly = false
	syncSkillsOnly = false
	output = "json"
	verbose = false
	quiet = false

	// Execute command
	err = cmd.RunE(cmd, []string{})
	if err != nil {
		t.Errorf("Command failed: %v", err)
	}

	jsonOutput := buf.String()
	t.Logf("JSON output:\n%s", jsonOutput)

	// Verify JSON structure (uses CamelCase field names)
	if !strings.Contains(jsonOutput, `"AgentsConverted"`) {
		t.Error("Expected JSON output to contain 'AgentsConverted' field")
	}
	if !strings.Contains(jsonOutput, `"SkillsCopied"`) {
		t.Error("Expected JSON output to contain 'SkillsCopied' field")
	}
	if !strings.Contains(jsonOutput, `"FailedFiles"`) {
		t.Error("Expected JSON output to contain 'FailedFiles' field")
	}
}
