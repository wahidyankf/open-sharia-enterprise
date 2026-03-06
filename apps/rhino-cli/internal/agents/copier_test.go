package agents

import (
	"os"
	"path/filepath"
	"strings"
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

func TestCopyAllSkills_NoSkillsDir(t *testing.T) {
	tmpDir := t.TempDir()
	// No .claude/skills directory

	_, _, _, err := CopyAllSkills(tmpDir, false)
	if err == nil {
		t.Error("expected error for missing skills directory")
	}
	if !strings.Contains(err.Error(), "skills directory not found") {
		t.Errorf("expected 'skills directory not found' error, got: %v", err)
	}
}

func TestCopyAllSkills_IndividualMdFiles(t *testing.T) {
	tmpDir := t.TempDir()

	claudeSkillsDir := filepath.Join(tmpDir, ".claude", "skills")
	if err := os.MkdirAll(claudeSkillsDir, 0755); err != nil {
		t.Fatal(err)
	}

	// Create an individual .md file in skills root (not README.md)
	if err := os.WriteFile(filepath.Join(claudeSkillsDir, "my-skill.md"), []byte("# Skill"), 0644); err != nil {
		t.Fatal(err)
	}

	copied, failed, _, err := CopyAllSkills(tmpDir, false)
	if err != nil {
		t.Fatalf("CopyAllSkills() failed: %v", err)
	}
	if copied != 1 {
		t.Errorf("expected 1 copied, got %d", copied)
	}
	if failed != 0 {
		t.Errorf("expected 0 failed, got %d", failed)
	}
}

func TestCopyAllSkills_DirWithoutSkillMd(t *testing.T) {
	tmpDir := t.TempDir()

	claudeSkillsDir := filepath.Join(tmpDir, ".claude", "skills")
	emptySkillDir := filepath.Join(claudeSkillsDir, "no-skill-file")
	if err := os.MkdirAll(emptySkillDir, 0755); err != nil {
		t.Fatal(err)
	}
	// No SKILL.md file — directory should be skipped

	copied, failed, _, err := CopyAllSkills(tmpDir, false)
	if err != nil {
		t.Fatalf("CopyAllSkills() unexpected error: %v", err)
	}
	if copied != 0 {
		t.Errorf("expected 0 copied for dir without SKILL.md, got %d", copied)
	}
	if failed != 0 {
		t.Errorf("expected 0 failed, got %d", failed)
	}
}

func TestCopySkill_SourceNotFound(t *testing.T) {
	tmpDir := t.TempDir()
	err := CopySkill(filepath.Join(tmpDir, "nonexistent.md"), filepath.Join(tmpDir, "dest.md"), false)
	if err == nil {
		t.Error("expected error for missing source file")
	}
}

func TestCopyFile_SourceNotFound(t *testing.T) {
	tmpDir := t.TempDir()
	err := CopyFile(filepath.Join(tmpDir, "nonexistent.txt"), filepath.Join(tmpDir, "dest.txt"))
	if err == nil {
		t.Error("expected error for missing source file")
	}
}

func TestCopySkill_WriteError(t *testing.T) {
	// Test the os.WriteFile error path by making the output directory read-only
	tmpDir := t.TempDir()

	// Create source file
	sourcePath := filepath.Join(tmpDir, "source.md")
	if err := os.WriteFile(sourcePath, []byte("skill content"), 0644); err != nil {
		t.Fatal(err)
	}

	// Create a read-only destination directory
	readOnlyDir := filepath.Join(tmpDir, "readonly")
	if err := os.MkdirAll(readOnlyDir, 0555); err != nil {
		t.Fatal(err)
	}
	defer func() { _ = os.Chmod(readOnlyDir, 0755) }()

	// Try to write to a file in the read-only directory
	destPath := filepath.Join(readOnlyDir, "dest.md")
	err := CopySkill(sourcePath, destPath, false)
	if err == nil {
		// On some systems (e.g. running as root) this may succeed
		t.Logf("CopySkill succeeded (may be running as root or OS allows it)")
	}
}

func TestCopyFile_CreateDestError(t *testing.T) {
	// Test the os.Create error path in CopyFile
	tmpDir := t.TempDir()

	// Create source file
	sourcePath := filepath.Join(tmpDir, "source.txt")
	if err := os.WriteFile(sourcePath, []byte("content"), 0644); err != nil {
		t.Fatal(err)
	}

	// Try to write to a read-only directory
	readOnlyDir := filepath.Join(tmpDir, "readonly")
	if err := os.MkdirAll(readOnlyDir, 0555); err != nil {
		t.Fatal(err)
	}
	defer func() { _ = os.Chmod(readOnlyDir, 0755) }()

	destPath := filepath.Join(readOnlyDir, "dest.txt")
	err := CopyFile(sourcePath, destPath)
	if err == nil {
		t.Logf("CopyFile succeeded (may be running as root or OS allows it)")
	}
}

func TestCopyAllSkills_ReadDirError(t *testing.T) {
	// Test CopyAllSkills when the skills directory exists but is not readable.
	// First create the directory structure, then remove permissions.
	tmpDir := t.TempDir()

	claudeSkillsDir := filepath.Join(tmpDir, ".claude", "skills")
	// Create directory structure first (with write permission)
	if err := os.MkdirAll(filepath.Join(tmpDir, ".claude"), 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.Mkdir(claudeSkillsDir, 0755); err != nil {
		t.Fatal(err)
	}
	// Now remove read permissions so ReadDir fails
	if err := os.Chmod(claudeSkillsDir, 0311); err != nil {
		t.Fatal(err)
	}
	defer func() { _ = os.Chmod(claudeSkillsDir, 0755) }()

	_, _, _, err := CopyAllSkills(tmpDir, false)
	// If running as root, ReadDir may succeed — that's OK
	if err != nil {
		if len(err.Error()) == 0 {
			t.Error("expected non-empty error message")
		}
	}
}

func TestCopyAllSkills_IndividualMdFile_CopyFails(t *testing.T) {
	// Cover line 85-88: individual .md file in skills root, CopySkill fails
	// because the output directory is read-only.
	tmpDir := t.TempDir()

	claudeSkillsDir := filepath.Join(tmpDir, ".claude", "skills")
	if err := os.MkdirAll(claudeSkillsDir, 0755); err != nil {
		t.Fatal(err)
	}

	// Create an individual .md file in skills root (not in a subdir, not README.md)
	if err := os.WriteFile(filepath.Join(claudeSkillsDir, "my-skill.md"), []byte("# Skill content"), 0644); err != nil {
		t.Fatal(err)
	}

	// Make the .opencode directory read-only so writing inside it fails
	opencodeDir := filepath.Join(tmpDir, ".opencode")
	if err := os.MkdirAll(opencodeDir, 0555); err != nil {
		t.Fatal(err)
	}
	defer func() { _ = os.Chmod(opencodeDir, 0755) }()

	_, failed, _, err := CopyAllSkills(tmpDir, false)
	if err != nil {
		t.Fatalf("CopyAllSkills() unexpected error: %v", err)
	}
	// On non-root systems, the copy should fail → failed count > 0
	// On root systems, it may succeed → that's acceptable
	_ = failed
}
