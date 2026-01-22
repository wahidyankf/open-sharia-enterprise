package sync

import (
	"testing"
	"time"
)

func TestClaudeAgent(t *testing.T) {
	agent := ClaudeAgent{
		Name:        "test-agent",
		Description: "Test agent description",
		Tools:       []string{"Read", "Write", "Edit"},
		Model:       "sonnet",
		Color:       "blue",
		Skills:      []string{"skill-1", "skill-2"},
	}

	if agent.Name != "test-agent" {
		t.Errorf("Expected name 'test-agent', got '%s'", agent.Name)
	}
	if len(agent.Tools) != 3 {
		t.Errorf("Expected 3 tools, got %d", len(agent.Tools))
	}
	if len(agent.Skills) != 2 {
		t.Errorf("Expected 2 skills, got %d", len(agent.Skills))
	}
}

func TestOpenCodeAgent(t *testing.T) {
	tools := map[string]bool{
		"read":  true,
		"write": true,
		"edit":  true,
	}

	agent := OpenCodeAgent{
		Description: "Test agent description",
		Model:       "inherit",
		Tools:       tools,
		Skills:      []string{"skill-1"},
	}

	if agent.Description != "Test agent description" {
		t.Errorf("Expected description 'Test agent description', got '%s'", agent.Description)
	}
	if agent.Model != "inherit" {
		t.Errorf("Expected model 'inherit', got '%s'", agent.Model)
	}
	if len(agent.Tools) != 3 {
		t.Errorf("Expected 3 tools, got %d", len(agent.Tools))
	}
	if !agent.Tools["read"] {
		t.Error("Expected 'read' tool to be true")
	}
}

func TestSyncOptions(t *testing.T) {
	opts := SyncOptions{
		RepoRoot:   "/path/to/repo",
		DryRun:     true,
		AgentsOnly: false,
		SkillsOnly: false,
		Verbose:    true,
		Quiet:      false,
	}

	if opts.RepoRoot != "/path/to/repo" {
		t.Errorf("Expected RepoRoot '/path/to/repo', got '%s'", opts.RepoRoot)
	}
	if !opts.DryRun {
		t.Error("Expected DryRun to be true")
	}
	if !opts.Verbose {
		t.Error("Expected Verbose to be true")
	}
}

func TestSyncResult(t *testing.T) {
	result := SyncResult{
		AgentsConverted: 45,
		AgentsFailed:    0,
		SkillsCopied:    23,
		SkillsFailed:    0,
		FailedFiles:     []string{},
		Duration:        100 * time.Millisecond,
	}

	if result.AgentsConverted != 45 {
		t.Errorf("Expected 45 agents converted, got %d", result.AgentsConverted)
	}
	if result.SkillsCopied != 23 {
		t.Errorf("Expected 23 skills copied, got %d", result.SkillsCopied)
	}
	if len(result.FailedFiles) != 0 {
		t.Errorf("Expected 0 failed files, got %d", len(result.FailedFiles))
	}
}

func TestSyncResultWithFailures(t *testing.T) {
	result := SyncResult{
		AgentsConverted: 43,
		AgentsFailed:    2,
		SkillsCopied:    22,
		SkillsFailed:    1,
		FailedFiles:     []string{"agent1.md", "skill1.md"},
		Duration:        150 * time.Millisecond,
	}

	if result.AgentsFailed != 2 {
		t.Errorf("Expected 2 failed agents, got %d", result.AgentsFailed)
	}
	if result.SkillsFailed != 1 {
		t.Errorf("Expected 1 failed skill, got %d", result.SkillsFailed)
	}
	if len(result.FailedFiles) != 2 {
		t.Errorf("Expected 2 failed files, got %d", len(result.FailedFiles))
	}
}

func TestValidationResult(t *testing.T) {
	checks := []ValidationCheck{
		{Name: "Count check", Status: "passed", Expected: "45", Actual: "45", Message: "Agent count matches"},
		{Name: "Format check", Status: "passed", Expected: "valid", Actual: "valid", Message: "Format valid"},
	}

	result := ValidationResult{
		TotalChecks:  2,
		PassedChecks: 2,
		FailedChecks: 0,
		Checks:       checks,
		Duration:     50 * time.Millisecond,
	}

	if result.TotalChecks != 2 {
		t.Errorf("Expected 2 total checks, got %d", result.TotalChecks)
	}
	if result.PassedChecks != 2 {
		t.Errorf("Expected 2 passed checks, got %d", result.PassedChecks)
	}
	if result.FailedChecks != 0 {
		t.Errorf("Expected 0 failed checks, got %d", result.FailedChecks)
	}
}

func TestValidationCheck(t *testing.T) {
	check := ValidationCheck{
		Name:     "Test check",
		Status:   "passed",
		Expected: "expected value",
		Actual:   "expected value",
		Message:  "Values match",
	}

	if check.Status != "passed" {
		t.Errorf("Expected status 'passed', got '%s'", check.Status)
	}
	if check.Expected != check.Actual {
		t.Errorf("Expected and Actual should match: '%s' != '%s'", check.Expected, check.Actual)
	}
}

func TestValidationCheckFailed(t *testing.T) {
	check := ValidationCheck{
		Name:     "Test check",
		Status:   "failed",
		Expected: "expected value",
		Actual:   "different value",
		Message:  "Values do not match",
	}

	if check.Status != "failed" {
		t.Errorf("Expected status 'failed', got '%s'", check.Status)
	}
	if check.Expected == check.Actual {
		t.Error("Expected and Actual should be different for failed check")
	}
}
