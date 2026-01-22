package claude

// ClaudeAgentFull represents a complete Claude Code agent with all required fields
type ClaudeAgentFull struct {
	Name        string   `yaml:"name"`
	Description string   `yaml:"description"`
	Tools       string   `yaml:"tools"`        // Comma-separated string in Claude format
	Model       string   `yaml:"model"`        // Can be empty, "sonnet", "opus", or "haiku"
	Color       string   `yaml:"color"`        // blue, green, yellow, or purple
	Skills      []string `yaml:"skills,omitempty"`
}

// ClaudeSkill represents a Claude Code skill configuration
type ClaudeSkill struct {
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

// Valid tools in Claude Code format
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

// Valid models in Claude Code format
var ValidModels = map[string]bool{
	"":       true, // Empty is valid (inherits)
	"sonnet": true,
	"opus":   true,
	"haiku":  true,
}

// Valid colors in Claude Code format
var ValidColors = map[string]bool{
	"blue":   true,
	"green":  true,
	"yellow": true,
	"purple": true,
}

// Required field order for agent frontmatter
var RequiredFieldOrder = []string{
	"name",
	"description",
	"tools",
	"model",
	"color",
	"skills",
}
