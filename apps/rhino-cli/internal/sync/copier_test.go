package sync

import (
	"os"
	"path/filepath"
	"testing"
)

func TestCopySkill(t *testing.T) {
	tmpDir := t.TempDir()

	// Create source skill file
	sourceContent := `---
name: test-skill
description: Test skill description
context: inline
---

# Test Skill

This is a test skill.
`

	sourcePath := filepath.Join(tmpDir, "source.md")
	if err := os.WriteFile(sourcePath, []byte(sourceContent), 0644); err != nil {
		t.Fatalf("Failed to create source file: %v", err)
	}

	// Copy skill
	destPath := filepath.Join(tmpDir, "dest.md")
	if err := CopySkill(sourcePath, destPath, false); err != nil {
		t.Fatalf("CopySkill() failed: %v", err)
	}

	// Verify destination file exists and matches source
	destContent, err := os.ReadFile(destPath)
	if err != nil {
		t.Fatalf("Failed to read destination file: %v", err)
	}

	if string(destContent) != sourceContent {
		t.Errorf("Content mismatch:\nGot: %s\nWant: %s", string(destContent), sourceContent)
	}
}

func TestCopySkillDryRun(t *testing.T) {
	tmpDir := t.TempDir()

	// Create source skill file
	sourcePath := filepath.Join(tmpDir, "source.md")
	if err := os.WriteFile(sourcePath, []byte("test content"), 0644); err != nil {
		t.Fatalf("Failed to create source file: %v", err)
	}

	// Copy skill with dry run
	destPath := filepath.Join(tmpDir, "dest.md")
	if err := CopySkill(sourcePath, destPath, true); err != nil {
		t.Fatalf("CopySkill() failed: %v", err)
	}

	// Verify destination file was NOT created
	if _, err := os.Stat(destPath); err == nil {
		t.Error("Destination file should not exist in dry run mode")
	}
}

func TestCopyAllSkills(t *testing.T) {
	tmpDir := t.TempDir()

	// Create .claude/skills directory structure
	claudeSkillsDir := filepath.Join(tmpDir, ".claude", "skills")
	skill1Dir := filepath.Join(claudeSkillsDir, "skill-1")
	skill2Dir := filepath.Join(claudeSkillsDir, "skill-2")

	if err := os.MkdirAll(skill1Dir, 0755); err != nil {
		t.Fatalf("Failed to create skill1 directory: %v", err)
	}
	if err := os.MkdirAll(skill2Dir, 0755); err != nil {
		t.Fatalf("Failed to create skill2 directory: %v", err)
	}

	// Create SKILL.md files
	skill1Content := "# Skill 1\n\nThis is skill 1."
	skill2Content := "# Skill 2\n\nThis is skill 2."

	if err := os.WriteFile(filepath.Join(skill1Dir, "SKILL.md"), []byte(skill1Content), 0644); err != nil {
		t.Fatalf("Failed to create skill1 SKILL.md: %v", err)
	}
	if err := os.WriteFile(filepath.Join(skill2Dir, "SKILL.md"), []byte(skill2Content), 0644); err != nil {
		t.Fatalf("Failed to create skill2 SKILL.md: %v", err)
	}

	// Create README.md (should be skipped)
	if err := os.WriteFile(filepath.Join(claudeSkillsDir, "README.md"), []byte("readme"), 0644); err != nil {
		t.Fatalf("Failed to create README.md: %v", err)
	}

	// Copy all skills
	copied, failed, failedFiles, err := CopyAllSkills(tmpDir, false)
	if err != nil {
		t.Fatalf("CopyAllSkills() failed: %v", err)
	}

	if copied != 2 {
		t.Errorf("Expected 2 skills copied, got %d", copied)
	}
	if failed != 0 {
		t.Errorf("Expected 0 failures, got %d", failed)
	}
	if len(failedFiles) != 0 {
		t.Errorf("Expected 0 failed files, got %v", failedFiles)
	}

	// Verify output files exist (directory structure: {name}/SKILL.md)
	opencodeSkillDir := filepath.Join(tmpDir, ".opencode", "skill")
	skill1Output := filepath.Join(opencodeSkillDir, "skill-1", "SKILL.md")
	skill2Output := filepath.Join(opencodeSkillDir, "skill-2", "SKILL.md")

	content1, err := os.ReadFile(skill1Output)
	if err != nil {
		t.Errorf("Failed to read skill1 output: %v", err)
	}
	if string(content1) != skill1Content {
		t.Errorf("Skill1 content mismatch")
	}

	content2, err := os.ReadFile(skill2Output)
	if err != nil {
		t.Errorf("Failed to read skill2 output: %v", err)
	}
	if string(content2) != skill2Content {
		t.Errorf("Skill2 content mismatch")
	}

	// Verify README.md was not copied
	readmePath := filepath.Join(opencodeSkillDir, "README.md")
	if _, err := os.Stat(readmePath); err == nil {
		t.Error("README.md should not be copied")
	}
}

func TestCopyFile(t *testing.T) {
	tmpDir := t.TempDir()

	// Create source file
	sourceContent := "test file content"
	sourcePath := filepath.Join(tmpDir, "source.txt")
	if err := os.WriteFile(sourcePath, []byte(sourceContent), 0644); err != nil {
		t.Fatalf("Failed to create source file: %v", err)
	}

	// Copy file
	destPath := filepath.Join(tmpDir, "subdir", "dest.txt")
	if err := CopyFile(sourcePath, destPath); err != nil {
		t.Fatalf("CopyFile() failed: %v", err)
	}

	// Verify destination
	destContent, err := os.ReadFile(destPath)
	if err != nil {
		t.Fatalf("Failed to read destination file: %v", err)
	}

	if string(destContent) != sourceContent {
		t.Errorf("Content mismatch: got %s, want %s", string(destContent), sourceContent)
	}
}
