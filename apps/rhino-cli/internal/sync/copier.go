package sync

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
)

// CopySkill copies a skill file from .claude/skills to .opencode/skill
func CopySkill(inputPath, outputPath string, dryRun bool) error {
	// Read source file
	content, err := os.ReadFile(inputPath)
	if err != nil {
		return fmt.Errorf("failed to read skill file: %w", err)
	}

	// Write to destination (if not dry run)
	if !dryRun {
		// Ensure output directory exists
		if err := os.MkdirAll(filepath.Dir(outputPath), 0755); err != nil {
			return fmt.Errorf("failed to create output directory: %w", err)
		}

		if err := os.WriteFile(outputPath, content, 0644); err != nil {
			return fmt.Errorf("failed to write skill file: %w", err)
		}
	}

	return nil
}

// CopyAllSkills copies all skills from .claude/skills to .opencode/skill
func CopyAllSkills(repoRoot string, dryRun bool) (copied int, failed int, failedFiles []string, err error) {
	claudeSkillsDir := filepath.Join(repoRoot, ".claude", "skills")
	opencodeSkillDir := filepath.Join(repoRoot, ".opencode", "skill")

	// Check if skills directory exists
	if _, err := os.Stat(claudeSkillsDir); os.IsNotExist(err) {
		return 0, 0, nil, fmt.Errorf("skills directory not found: %s", claudeSkillsDir)
	}

	// Read all skill directories
	entries, err := os.ReadDir(claudeSkillsDir)
	if err != nil {
		return 0, 0, nil, fmt.Errorf("failed to read .claude/skills directory: %w", err)
	}

	for _, entry := range entries {
		// Skip README.md in the skills directory root
		if !entry.IsDir() && entry.Name() == "README.md" {
			continue
		}

		// Process skill directories
		if entry.IsDir() {
			skillDir := filepath.Join(claudeSkillsDir, entry.Name())

			// Each skill directory should have a SKILL.md file
			skillFile := filepath.Join(skillDir, "SKILL.md")
			if _, err := os.Stat(skillFile); os.IsNotExist(err) {
				// Skip directories without SKILL.md
				continue
			}

			// Copy SKILL.md to .opencode/skill/{skill-name}.md
			outputPath := filepath.Join(opencodeSkillDir, entry.Name()+".md")

			if err := CopySkill(skillFile, outputPath, dryRun); err != nil {
				failed++
				failedFiles = append(failedFiles, entry.Name())
			} else {
				copied++
			}
		} else if strings.HasSuffix(entry.Name(), ".md") && entry.Name() != "README.md" {
			// Handle individual .md files in skills root (if any)
			inputPath := filepath.Join(claudeSkillsDir, entry.Name())
			outputPath := filepath.Join(opencodeSkillDir, entry.Name())

			if err := CopySkill(inputPath, outputPath, dryRun); err != nil {
				failed++
				failedFiles = append(failedFiles, entry.Name())
			} else {
				copied++
			}
		}
	}

	return copied, failed, failedFiles, nil
}

// CopyFile is a helper function for direct file copying
func CopyFile(src, dst string) error {
	sourceFile, err := os.Open(src)
	if err != nil {
		return fmt.Errorf("failed to open source file: %w", err)
	}
	defer sourceFile.Close()

	// Ensure destination directory exists
	if err := os.MkdirAll(filepath.Dir(dst), 0755); err != nil {
		return fmt.Errorf("failed to create destination directory: %w", err)
	}

	destFile, err := os.Create(dst)
	if err != nil {
		return fmt.Errorf("failed to create destination file: %w", err)
	}
	defer destFile.Close()

	if _, err := io.Copy(destFile, sourceFile); err != nil {
		return fmt.Errorf("failed to copy file: %w", err)
	}

	return nil
}
