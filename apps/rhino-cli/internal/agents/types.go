package agents

import (
	"regexp"
	"time"
)

// ---- Sync types (from former internal/sync) ----

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

// ---- Claude validation types (from former internal/claude) ----

// ClaudeAgentFull represents a complete Claude Code agent with all required fields
type ClaudeAgentFull struct {
	Name        string   `yaml:"name"`
	Description string   `yaml:"description"`
	Tools       string   `yaml:"tools"` // Comma-separated string in Claude format
	Model       string   `yaml:"model"` // Can be empty, "sonnet", "opus", or "haiku"
	Color       string   `yaml:"color"` // blue, green, yellow, or purple
	Skills      []string `yaml:"skills,omitempty"`
}

// ClaudeSkill represents a Claude Code skill configuration
type ClaudeSkill struct {
	Name        string `yaml:"name"`
	Description string `yaml:"description"`
}

// ValidationError represents a validation error with detailed information
type ValidationError struct {
	AgentName string
	SkillName string
	Rule      string
	Message   string
	Expected  string
	Actual    string
}

// ValidateClaudeOptions configures validation behavior
type ValidateClaudeOptions struct {
	RepoRoot   string
	AgentsOnly bool
	SkillsOnly bool
}

// ValidTools lists all recognized tool names in Claude Code agent frontmatter.
var ValidTools = map[string]bool{
	"Read":      true,
	"Write":     true,
	"Edit":      true,
	"Glob":      true,
	"Grep":      true,
	"Bash":      true,
	"TodoWrite": true,
	"WebFetch":  true,
	"WebSearch": true,
}

// ValidModels lists all recognized model values in Claude Code agent frontmatter.
var ValidModels = map[string]bool{
	"":       true, // Empty is valid (inherits)
	"sonnet": true,
	"opus":   true,
	"haiku":  true,
}

// ValidColors lists all recognized color values in Claude Code agent frontmatter.
var ValidColors = map[string]bool{
	"blue":   true,
	"green":  true,
	"yellow": true,
	"purple": true,
}

// ValidSkillNamePattern validates skill name format
// Lowercase letters, numbers, hyphens only, max 64 characters
var ValidSkillNamePattern = regexp.MustCompile(`^[a-z0-9-]{1,64}$`)

// RequiredFieldOrder defines the mandatory ordering of fields in agent frontmatter.
var RequiredFieldOrder = []string{
	"name",
	"description",
	"tools",
	"model",
	"color",
	"skills",
}
