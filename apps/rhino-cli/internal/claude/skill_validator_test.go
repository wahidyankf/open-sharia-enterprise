package claude

import (
	"os"
	"path/filepath"
	"testing"
)

func TestValidateSkill_ValidSkill(t *testing.T) {
	tmpDir := t.TempDir()
	skillDir := filepath.Join(tmpDir, "test-skill")

	if err := os.MkdirAll(skillDir, 0755); err != nil {
		t.Fatalf("Failed to create skill dir: %v", err)
	}

	content := `---
description:Test skill description
---
Skill content here`

	if err := os.WriteFile(filepath.Join(skillDir, "SKILL.md"), []byte(content), 0644); err != nil {
		t.Fatalf("Failed to create SKILL.md: %v", err)
	}

	checks := validateSkill(skillDir, "test-skill")

	// Should have 3 checks: file exists, YAML syntax, required fields
	if len(checks) != 3 {
		t.Errorf("Expected 3 checks, got %d", len(checks))
	}

	for _, check := range checks {
		if check.Status != "passed" {
			t.Errorf("Check '%s' failed: %s", check.Name, check.Message)
		}
	}
}

func TestValidateSkill_MissingSkillFile(t *testing.T) {
	tmpDir := t.TempDir()
	skillDir := filepath.Join(tmpDir, "test-skill")

	if err := os.MkdirAll(skillDir, 0755); err != nil {
		t.Fatalf("Failed to create skill dir: %v", err)
	}

	// Don't create SKILL.md

	checks := validateSkill(skillDir, "test-skill")

	// Should return early with 1 failed check
	if len(checks) != 1 {
		t.Errorf("Expected 1 check, got %d", len(checks))
	}

	if checks[0].Status != "failed" {
		t.Errorf("Expected status 'failed', got '%s'", checks[0].Status)
	}

	if checks[0].Expected != "SKILL.md file present" {
		t.Errorf("Expected SKILL.md message, got '%s'", checks[0].Expected)
	}
}

func TestValidateSkill_InvalidYAML(t *testing.T) {
	tmpDir := t.TempDir()
	skillDir := filepath.Join(tmpDir, "test-skill")

	if err := os.MkdirAll(skillDir, 0755); err != nil {
		t.Fatalf("Failed to create skill dir: %v", err)
	}

	// Invalid YAML (no closing ---)
	content := `---
description:Test skill
This is invalid`

	if err := os.WriteFile(filepath.Join(skillDir, "SKILL.md"), []byte(content), 0644); err != nil {
		t.Fatalf("Failed to create SKILL.md: %v", err)
	}

	checks := validateSkill(skillDir, "test-skill")

	// Should have file exists (passed), YAML syntax (failed)
	if len(checks) < 2 {
		t.Errorf("Expected at least 2 checks, got %d", len(checks))
	}

	foundFailure := false
	for _, check := range checks {
		if check.Status == "failed" {
			foundFailure = true
			break
		}
	}

	if !foundFailure {
		t.Error("Expected at least one failed check")
	}
}

func TestValidateSkill_MissingDescription(t *testing.T) {
	tmpDir := t.TempDir()
	skillDir := filepath.Join(tmpDir, "test-skill")

	if err := os.MkdirAll(skillDir, 0755); err != nil {
		t.Fatalf("Failed to create skill dir: %v", err)
	}

	// Missing description field
	content := `---
---
Skill content here`

	if err := os.WriteFile(filepath.Join(skillDir, "SKILL.md"), []byte(content), 0644); err != nil {
		t.Fatalf("Failed to create SKILL.md: %v", err)
	}

	checks := validateSkill(skillDir, "test-skill")

	// Should have 4 checks: exists (pass), YAML syntax (pass), parse (pass), required fields (fail)
	if len(checks) < 3 {
		t.Errorf("Expected at least 3 checks, got %d", len(checks))
	}

	foundMissingDescription := false
	for _, check := range checks {
		if check.Status == "failed" && check.Expected == "description field present" {
			foundMissingDescription = true
			break
		}
	}

	if !foundMissingDescription {
		t.Error("Expected check for missing description field")
	}
}

func TestValidateSkill_EmptyDescription(t *testing.T) {
	tmpDir := t.TempDir()
	skillDir := filepath.Join(tmpDir, "test-skill")

	if err := os.MkdirAll(skillDir, 0755); err != nil {
		t.Fatalf("Failed to create skill dir: %v", err)
	}

	// Empty description
	content := `---
description:
---
Skill content here`

	if err := os.WriteFile(filepath.Join(skillDir, "SKILL.md"), []byte(content), 0644); err != nil {
		t.Fatalf("Failed to create SKILL.md: %v", err)
	}

	checks := validateSkill(skillDir, "test-skill")

	foundMissingDescription := false
	for _, check := range checks {
		if check.Status == "failed" && check.Expected == "description field present" {
			foundMissingDescription = true
			break
		}
	}

	if !foundMissingDescription {
		t.Error("Expected check for empty description field")
	}
}

func TestValidateSkill_LongDescription(t *testing.T) {
	tmpDir := t.TempDir()
	skillDir := filepath.Join(tmpDir, "test-skill")

	if err := os.MkdirAll(skillDir, 0755); err != nil {
		t.Fatalf("Failed to create skill dir: %v", err)
	}

	// Very long description (should be valid)
	longDesc := "This is a very long description that spans multiple concepts and ideas. "
	longDesc += "It includes detailed information about the skill's purpose, usage, and benefits. "
	longDesc += "The description can be as long as needed to fully explain the skill."

	content := `---
description:` + longDesc + `
---
Skill content here`

	if err := os.WriteFile(filepath.Join(skillDir, "SKILL.md"), []byte(content), 0644); err != nil {
		t.Fatalf("Failed to create SKILL.md: %v", err)
	}

	checks := validateSkill(skillDir, "test-skill")

	for _, check := range checks {
		if check.Status != "passed" {
			t.Errorf("Check '%s' failed: %s", check.Name, check.Message)
		}
	}
}

func TestValidateAllSkills_EmptyDirectory(t *testing.T) {
	tmpDir := t.TempDir()
	skillsDir := filepath.Join(tmpDir, ".claude", "skills")

	if err := os.MkdirAll(skillsDir, 0755); err != nil {
		t.Fatalf("Failed to create skills dir: %v", err)
	}

	checks, skillNames := validateAllSkills(tmpDir)

	if len(checks) != 0 {
		t.Errorf("Expected 0 checks for empty directory, got %d", len(checks))
	}

	if len(skillNames) != 0 {
		t.Errorf("Expected 0 skill names, got %d", len(skillNames))
	}
}

func TestValidateAllSkills_MultipleSkills(t *testing.T) {
	tmpDir := t.TempDir()
	skillsDir := filepath.Join(tmpDir, ".claude", "skills")

	if err := os.MkdirAll(skillsDir, 0755); err != nil {
		t.Fatalf("Failed to create skills dir: %v", err)
	}

	// Create 3 valid skills
	for i := 1; i <= 3; i++ {
		skillName := "skill-" + string(rune('0'+i))
		createSkill(t, skillsDir, skillName)
	}

	checks, skillNames := validateAllSkills(tmpDir)

	// 3 skills × 3 checks each = 9 checks
	if len(checks) != 9 {
		t.Errorf("Expected 9 checks (3 skills × 3), got %d", len(checks))
	}

	if len(skillNames) != 3 {
		t.Errorf("Expected 3 skill names, got %d", len(skillNames))
	}

	// Verify all skill names are registered
	for i := 1; i <= 3; i++ {
		skillName := "skill-" + string(rune('0'+i))
		if !skillNames[skillName] {
			t.Errorf("Expected skill '%s' to be registered", skillName)
		}
	}

	passedCount := 0
	for _, check := range checks {
		if check.Status == "passed" {
			passedCount++
		}
	}

	if passedCount != 9 {
		t.Errorf("Expected all 9 checks to pass, got %d passed", passedCount)
	}
}

func TestValidateAllSkills_MixedValidInvalid(t *testing.T) {
	tmpDir := t.TempDir()
	skillsDir := filepath.Join(tmpDir, ".claude", "skills")

	if err := os.MkdirAll(skillsDir, 0755); err != nil {
		t.Fatalf("Failed to create skills dir: %v", err)
	}

	// Create 1 valid skill
	createSkill(t, skillsDir, "valid-skill")

	// Create 1 invalid skill (missing SKILL.md)
	invalidSkillDir := filepath.Join(skillsDir, "invalid-skill")
	if err := os.MkdirAll(invalidSkillDir, 0755); err != nil {
		t.Fatalf("Failed to create invalid skill dir: %v", err)
	}

	checks, skillNames := validateAllSkills(tmpDir)

	// Should have checks for both skills
	if len(checks) < 4 {
		t.Errorf("Expected at least 4 checks, got %d", len(checks))
	}

	// Only valid skill should be registered
	if len(skillNames) != 1 {
		t.Errorf("Expected 1 skill name, got %d", len(skillNames))
	}

	if !skillNames["valid-skill"] {
		t.Error("Expected valid-skill to be registered")
	}

	if skillNames["invalid-skill"] {
		t.Error("Did not expect invalid-skill to be registered")
	}
}

func TestValidateAllSkills_IgnoresDotDirectories(t *testing.T) {
	tmpDir := t.TempDir()
	skillsDir := filepath.Join(tmpDir, ".claude", "skills")

	if err := os.MkdirAll(skillsDir, 0755); err != nil {
		t.Fatalf("Failed to create skills dir: %v", err)
	}

	// Create a valid skill
	createSkill(t, skillsDir, "valid-skill")

	// Create a .hidden directory (should be ignored)
	hiddenDir := filepath.Join(skillsDir, ".hidden")
	if err := os.MkdirAll(hiddenDir, 0755); err != nil {
		t.Fatalf("Failed to create hidden dir: %v", err)
	}

	checks, skillNames := validateAllSkills(tmpDir)

	// Should only have checks for valid-skill (3 checks)
	if len(checks) != 3 {
		t.Errorf("Expected 3 checks (hidden dir ignored), got %d", len(checks))
	}

	if len(skillNames) != 1 {
		t.Errorf("Expected 1 skill name, got %d", len(skillNames))
	}
}

func TestValidateAllSkills_DirectoryNotFound(t *testing.T) {
	tmpDir := t.TempDir()
	// Don't create the skills directory

	checks, skillNames := validateAllSkills(tmpDir)

	if len(checks) != 1 {
		t.Errorf("Expected 1 error check, got %d", len(checks))
	}

	if checks[0].Status != "failed" {
		t.Errorf("Expected failed status, got '%s'", checks[0].Status)
	}

	if len(skillNames) != 0 {
		t.Errorf("Expected 0 skill names for missing directory, got %d", len(skillNames))
	}
}

func TestValidateAllSkills_IgnoresFiles(t *testing.T) {
	tmpDir := t.TempDir()
	skillsDir := filepath.Join(tmpDir, ".claude", "skills")

	if err := os.MkdirAll(skillsDir, 0755); err != nil {
		t.Fatalf("Failed to create skills dir: %v", err)
	}

	// Create a valid skill
	createSkill(t, skillsDir, "valid-skill")

	// Create a file in skills dir (should be ignored)
	if err := os.WriteFile(filepath.Join(skillsDir, "README.md"), []byte("readme"), 0644); err != nil {
		t.Fatalf("Failed to create file: %v", err)
	}

	checks, skillNames := validateAllSkills(tmpDir)

	// Should only have checks for valid-skill (3 checks)
	if len(checks) != 3 {
		t.Errorf("Expected 3 checks (file ignored), got %d", len(checks))
	}

	if len(skillNames) != 1 {
		t.Errorf("Expected 1 skill name, got %d", len(skillNames))
	}
}
