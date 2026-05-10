// Package doctor provides functionality for checking required tool versions.
package doctor

import "time"

// ToolStatus is a sealed interface for the health status of a tool check.
// Use StatusOK{}, StatusWarning{}, or StatusMissing{}.
//
//sumtype:decl
type ToolStatus interface {
	isToolStatus()
	Code() string
}

// StatusOK indicates the tool is installed with the correct version.
type StatusOK struct{}

func (StatusOK) isToolStatus() {}

// Code implements ToolStatus.
func (StatusOK) Code() string { return "ok" }

// StatusWarning indicates the tool is installed but the version doesn't match.
type StatusWarning struct{}

func (StatusWarning) isToolStatus() {}

// Code implements ToolStatus.
func (StatusWarning) Code() string { return "warning" }

// StatusMissing indicates the tool is not found in PATH.
type StatusMissing struct{}

func (StatusMissing) isToolStatus() {}

// Code implements ToolStatus.
func (StatusMissing) Code() string { return "missing" }

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

// Scope is a sealed interface controlling which tools doctor checks.
// Use ScopeFull{} or ScopeMinimal{} — type switches are enforced exhaustive
// by gochecksumtype when //sumtype:decl is present.
//
//sumtype:decl
type Scope interface {
	isScope()
	Code() string
}

// ScopeFull checks all tools (default).
type ScopeFull struct{}

func (ScopeFull) isScope() {}

// Code implements Scope.
func (ScopeFull) Code() string { return "full" }

// ScopeMinimal checks only core tools required for basic development.
type ScopeMinimal struct{}

func (ScopeMinimal) isScope() {}

// Code implements Scope.
func (ScopeMinimal) Code() string { return "minimal" }

// ParseScope converts a CLI string to a Scope variant.
// Empty string defaults to ScopeFull. Returns false for unknown values.
func ParseScope(s string) (Scope, bool) {
	switch s {
	case "", "full":
		return ScopeFull{}, true
	case "minimal":
		return ScopeMinimal{}, true
	default:
		return nil, false
	}
}

// scopeCode returns the Code() of a Scope, or "full" for a nil Scope.
func scopeCode(s Scope) string {
	if s == nil {
		return "full"
	}
	return s.Code()
}

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
