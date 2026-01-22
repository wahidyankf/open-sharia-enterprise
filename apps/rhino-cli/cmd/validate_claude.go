package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/open-sharia-enterprise/apps/rhino-cli/internal/claude"
	"github.com/wahidyankf/open-sharia-enterprise/apps/rhino-cli/internal/sync"
)

var (
	agentsOnly bool
	skillsOnly bool
)

var validateClaudeCmd = &cobra.Command{
	Use:   "validate-claude",
	Short: "Validate Claude Code agent and skill format in .claude/ directory",
	Long: `Validate that .claude/ directory contains valid Claude Code format.

This command performs the following validations:

Agents (.claude/agents/):
- YAML frontmatter syntax
- Required fields: name, description, tools, model, color, skills
- Field order (exact sequence required)
- Valid tool names (Read, Write, Edit, Glob, Grep, Bash, TodoWrite, WebFetch, WebSearch)
- Valid model names (empty, sonnet, opus, haiku)
- Valid colors (blue, green, yellow, purple)
- Filename matches name field
- Agent name uniqueness
- Skills references exist
- No YAML comments
- Special rules (e.g., generated-reports/ tools)

Skills (.claude/skills/):
- SKILL.md file exists
- Required field: description
- YAML syntax validity`,
	Example: `  # Validate all agents and skills
  rhino-cli validate-claude

  # Output as JSON
  rhino-cli validate-claude -o json

  # Verbose mode (show all checks)
  rhino-cli validate-claude -v

  # Validate only agents
  rhino-cli validate-claude --agents-only

  # Validate only skills
  rhino-cli validate-claude --skills-only`,
	SilenceErrors: true,
	RunE:          runValidateClaude,
}

func init() {
	rootCmd.AddCommand(validateClaudeCmd)
	validateClaudeCmd.Flags().BoolVar(&agentsOnly, "agents-only", false, "validate only agents")
	validateClaudeCmd.Flags().BoolVar(&skillsOnly, "skills-only", false, "validate only skills")
}

func runValidateClaude(cmd *cobra.Command, args []string) error {
	// Validate flags
	if agentsOnly && skillsOnly {
		return fmt.Errorf("cannot use --agents-only and --skills-only together")
	}

	// Find git repository root
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	// Perform validation
	result, err := claude.ValidateClaude(claude.ValidateClaudeOptions{
		RepoRoot:   repoRoot,
		AgentsOnly: agentsOnly,
		SkillsOnly: skillsOnly,
	})
	if err != nil {
		return fmt.Errorf("validation failed: %w", err)
	}

	// Format and print output
	formattedOutput := sync.FormatValidationResult(result, output, verbose, quiet)
	fmt.Fprint(cmd.OutOrStdout(), formattedOutput)

	// Return error if validation failed
	if result.FailedChecks > 0 {
		return fmt.Errorf("validation failed: %d checks failed", result.FailedChecks)
	}

	return nil
}
