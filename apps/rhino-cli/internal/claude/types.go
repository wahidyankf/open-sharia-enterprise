package claude

import "regexp"

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
