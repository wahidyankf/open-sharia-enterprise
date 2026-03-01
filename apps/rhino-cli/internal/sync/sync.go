// Package sync provides validation and synchronisation between Claude Code and OpenCode agent configurations.
package sync

import (
	"time"
)

// SyncAll performs the complete sync operation
func SyncAll(opts SyncOptions) (*SyncResult, error) {
	startTime := time.Now()
	result := &SyncResult{
		FailedFiles: []string{},
	}

	// Sync agents (unless skills-only)
	if !opts.SkillsOnly {
		agentsConverted, agentsFailed, agentFailedFiles, err := ConvertAllAgents(opts.RepoRoot, opts.DryRun)
		if err != nil {
			return nil, err
		}
		result.AgentsConverted = agentsConverted
		result.AgentsFailed = agentsFailed
		result.FailedFiles = append(result.FailedFiles, agentFailedFiles...)
	}

	// Sync skills (unless agents-only)
	if !opts.AgentsOnly {
		skillsCopied, skillsFailed, skillFailedFiles, err := CopyAllSkills(opts.RepoRoot, opts.DryRun)
		if err != nil {
			return nil, err
		}
		result.SkillsCopied = skillsCopied
		result.SkillsFailed = skillsFailed
		result.FailedFiles = append(result.FailedFiles, skillFailedFiles...)
	}

	result.Duration = time.Since(startTime)

	return result, nil
}
