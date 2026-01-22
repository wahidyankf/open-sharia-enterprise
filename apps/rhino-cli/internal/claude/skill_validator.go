package claude

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/wahidyankf/open-sharia-enterprise/apps/rhino-cli/internal/sync"
	"gopkg.in/yaml.v3"
)

// validateSkill performs all 3 validation rules for a single skill
func validateSkill(skillPath string, skillName string) []sync.ValidationCheck {
	var checks []sync.ValidationCheck

	// Rule 1: SKILL.md file exists in subdirectory
	skillFile := filepath.Join(skillPath, "SKILL.md")
	if _, err := os.Stat(skillFile); os.IsNotExist(err) {
		checks = append(checks, sync.ValidationCheck{
			Name:     fmt.Sprintf("Skill: %s - SKILL.md Exists", skillName),
			Status:   "failed",
			Expected: "SKILL.md file present",
			Actual:   "SKILL.md file not found",
			Message:  "SKILL.md file missing",
		})
		return checks
	}
	checks = append(checks, sync.ValidationCheck{
		Name:    fmt.Sprintf("Skill: %s - SKILL.md Exists", skillName),
		Status:  "passed",
		Message: "SKILL.md file exists",
	})

	// Read SKILL.md file
	content, err := os.ReadFile(skillFile)
	if err != nil {
		checks = append(checks, sync.ValidationCheck{
			Name:    fmt.Sprintf("Skill: %s - Read SKILL.md", skillName),
			Status:  "failed",
			Message: fmt.Sprintf("Failed to read SKILL.md: %v", err),
		})
		return checks
	}

	// Rule 3: YAML syntax validity
	frontmatter, _, err := sync.ExtractFrontmatter(content)
	if err != nil {
		checks = append(checks, sync.ValidationCheck{
			Name:    fmt.Sprintf("Skill: %s - YAML Syntax", skillName),
			Status:  "failed",
			Message: fmt.Sprintf("Invalid frontmatter: %v", err),
		})
		return checks
	}
	checks = append(checks, sync.ValidationCheck{
		Name:    fmt.Sprintf("Skill: %s - YAML Syntax", skillName),
		Status:  "passed",
		Message: "Valid YAML frontmatter",
	})

	// Parse YAML into ClaudeSkill
	var skill ClaudeSkill
	if err := yaml.Unmarshal(frontmatter, &skill); err != nil {
		checks = append(checks, sync.ValidationCheck{
			Name:    fmt.Sprintf("Skill: %s - YAML Parse", skillName),
			Status:  "failed",
			Message: fmt.Sprintf("Failed to parse YAML: %v", err),
		})
		return checks
	}

	// Rule 2: Required frontmatter field: description
	if skill.Description == "" {
		checks = append(checks, sync.ValidationCheck{
			Name:     fmt.Sprintf("Skill: %s - Required Fields", skillName),
			Status:   "failed",
			Expected: "description field present",
			Actual:   "description field missing or empty",
			Message:  "Required description field missing",
		})
		return checks
	}
	checks = append(checks, sync.ValidationCheck{
		Name:    fmt.Sprintf("Skill: %s - Required Fields", skillName),
		Status:  "passed",
		Message: "Required description field present",
	})

	return checks
}

// validateAllSkills validates all skills and returns skill names map
func validateAllSkills(repoRoot string) ([]sync.ValidationCheck, map[string]bool) {
	skillsDir := filepath.Join(repoRoot, ".claude", "skills")

	entries, err := os.ReadDir(skillsDir)
	if err != nil {
		return []sync.ValidationCheck{{
			Name:    "Read Skills Directory",
			Status:  "failed",
			Message: fmt.Sprintf("Failed to read skills directory: %v", err),
		}}, make(map[string]bool)
	}

	skillNames := make(map[string]bool)
	var allChecks []sync.ValidationCheck

	// Validate each skill
	for _, entry := range entries {
		if !entry.IsDir() || strings.HasPrefix(entry.Name(), ".") {
			continue
		}

		skillPath := filepath.Join(skillsDir, entry.Name())
		checks := validateSkill(skillPath, entry.Name())
		allChecks = append(allChecks, checks...)

		// Add to skill names if validation passed
		allPassed := true
		for _, check := range checks {
			if check.Status == "failed" {
				allPassed = false
				break
			}
		}
		if allPassed {
			skillNames[entry.Name()] = true
		}
	}

	return allChecks, skillNames
}
