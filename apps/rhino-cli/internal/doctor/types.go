// Package doctor provides functionality for checking required tool versions.
package doctor

import "time"

// ToolStatus represents the health status of a tool check.
type ToolStatus string

const (
	// StatusOK indicates the tool is installed with the correct version.
	StatusOK ToolStatus = "ok"
	// StatusWarning indicates the tool is installed but the version doesn't match.
	StatusWarning ToolStatus = "warning"
	// StatusMissing indicates the tool is not found in PATH.
	StatusMissing ToolStatus = "missing"
)

// ToolCheck holds the result of checking a single tool.
type ToolCheck struct {
	Name             string
	Binary           string
	Status           ToolStatus
	InstalledVersion string
	RequiredVersion  string
	Source           string
	Note             string
}

// Scope controls which tools doctor checks.
type Scope string

const (
	// ScopeFull checks all tools (default).
	ScopeFull Scope = "full"
	// ScopeMinimal checks only core tools required for basic development.
	ScopeMinimal Scope = "minimal"
)

// MinimalTools lists the tool names included in the minimal scope.
var MinimalTools = map[string]bool{
	"git": true, "volta": true, "node": true, "npm": true,
	"golang": true, "docker": true, "jq": true,
}

// DoctorResult holds the aggregated results of all tool checks.
type DoctorResult struct {
	Checks       []ToolCheck
	OKCount      int
	WarnCount    int
	MissingCount int
	Duration     time.Duration
	Scope        Scope
}

// CheckOptions configures how the doctor check should be performed.
type CheckOptions struct {
	RepoRoot string
	Runner   CommandRunner // nil = use real subprocess runner
	Scope    Scope         // "" or "full" = all tools; "minimal" = core tools only
}

// CommandRunner is an injectable function for executing external commands.
// Tests supply fake runners; production uses the real os/exec-based runner.
type CommandRunner func(name string, args ...string) (stdout, stderr string, exitCode int, err error)
