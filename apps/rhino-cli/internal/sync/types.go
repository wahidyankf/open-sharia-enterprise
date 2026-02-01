package sync

import (
	"time"
)

// ClaudeAgent represents Claude Code format agent configuration
type ClaudeAgent struct {
	Name        string   `yaml:"name"`
	Description string   `yaml:"description"`
	Tools       []string `yaml:"tools"` // Can be array or comma-separated
	Model       string   `yaml:"model,omitempty"`
	Color       string   `yaml:"color,omitempty"`
	Skills      []string `yaml:"skills,omitempty"`
}

// OpenCodeAgent represents OpenCode format agent configuration
type OpenCodeAgent struct {
	Description string          `yaml:"description"`
	Model       string          `yaml:"model"` // "inherit" | "zai/glm-4.7" | "zai/glm-4.5-air"
	Tools       map[string]bool `yaml:"tools"` // read: true, write: true, etc.
	Skills      []string        `yaml:"skills,omitempty"`
}

// SyncOptions configures sync behavior
type SyncOptions struct {
	RepoRoot   string
	DryRun     bool
	AgentsOnly bool
	SkillsOnly bool
	Verbose    bool
	Quiet      bool
}

// SyncResult contains operation results
type SyncResult struct {
	AgentsConverted int
	AgentsFailed    int
	SkillsCopied    int
	SkillsFailed    int
	FailedFiles     []string
	Duration        time.Duration
}

// ValidationResult contains validation results
type ValidationResult struct {
	TotalChecks  int
	PassedChecks int
	FailedChecks int
	Checks       []ValidationCheck
	Duration     time.Duration
}

// ValidationCheck represents a single validation check
type ValidationCheck struct {
	Name     string
	Status   string // "passed" | "failed"
	Expected string
	Actual   string
	Message  string
}
