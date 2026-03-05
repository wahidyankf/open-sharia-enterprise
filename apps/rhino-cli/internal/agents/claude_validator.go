package agents

import (
	"time"
)

// ValidateClaude validates .claude/ directory format
func ValidateClaude(opts ValidateClaudeOptions) (*ValidationResult, error) {
	startTime := time.Now()
	result := &ValidationResult{
		Checks: []ValidationCheck{},
	}

	var skillNames map[string]bool

	// Validate skills first (needed for agent validation)
	if !opts.AgentsOnly {
		skillChecks, names := validateAllSkills(opts.RepoRoot)
		skillNames = names
		for _, check := range skillChecks {
			result.Checks = append(result.Checks, check)
			if check.Status == "passed" {
				result.PassedChecks++
			} else {
				result.FailedChecks++
			}
			result.TotalChecks++
		}
	} else {
		// If agents-only, still need to build skill names for validation
		_, skillNames = validateAllSkills(opts.RepoRoot)
	}

	// Validate agents
	if !opts.SkillsOnly {
		agentChecks := validateAllAgents(opts.RepoRoot, skillNames)
		for _, check := range agentChecks {
			result.Checks = append(result.Checks, check)
			if check.Status == "passed" {
				result.PassedChecks++
			} else {
				result.FailedChecks++
			}
			result.TotalChecks++
		}
	}

	result.Duration = time.Since(startTime)

	return result, nil
}
