package docs

import (
	"os"
	"path/filepath"
	"testing"
)

func TestGenerateRenameOperations(t *testing.T) {
	tests := []struct {
		name       string
		violations []NamingViolation
		wantCount  int
		wantFirst  *RenameOperation // nil means skip first check
	}{
		{
			name:       "empty violations",
			violations: []NamingViolation{},
			wantCount:  0,
		},
		{
			name: "wrong prefix violation",
			violations: []NamingViolation{
				{
					FilePath:       "docs/tutorials/wrong__getting-started.md",
					FileName:       "wrong__getting-started.md",
					ViolationType:  ViolationWrongPrefix,
					ExpectedPrefix: "tu__",
					ActualPrefix:   "wrong__",
				},
			},
			wantCount: 1,
			wantFirst: &RenameOperation{
				OldPath: "docs/tutorials/wrong__getting-started.md",
				NewPath: "docs/tutorials/tu__getting-started.md",
				OldName: "wrong__getting-started.md",
				NewName: "tu__getting-started.md",
			},
		},
		{
			name: "missing separator violation",
			violations: []NamingViolation{
				{
					FilePath:       "docs/tutorials/getting-started.md",
					FileName:       "getting-started.md",
					ViolationType:  ViolationMissingSeparator,
					ExpectedPrefix: "tu__",
				},
			},
			wantCount: 1,
			wantFirst: &RenameOperation{
				OldPath: "docs/tutorials/getting-started.md",
				NewPath: "docs/tutorials/tu__getting-started.md",
				OldName: "getting-started.md",
				NewName: "tu__getting-started.md",
			},
		},
		{
			name: "bad case violation - not fixable",
			violations: []NamingViolation{
				{
					FilePath:       "docs/tutorials/tu__GettingStarted.md",
					FileName:       "tu__GettingStarted.md",
					ViolationType:  ViolationBadCase,
					ExpectedPrefix: "tu__",
				},
			},
			wantCount: 0, // bad_case is not auto-fixable
		},
		{
			name: "missing prefix violation - not fixable",
			violations: []NamingViolation{
				{
					FilePath:       "docs/tutorials/some-file.md",
					FileName:       "some-file.md",
					ViolationType:  ViolationMissingPrefix,
					ExpectedPrefix: "tu__",
				},
			},
			wantCount: 0, // missing_prefix is not auto-fixable
		},
		{
			name: "mixed violations",
			violations: []NamingViolation{
				{
					FilePath:       "docs/tutorials/wrong__file1.md",
					FileName:       "wrong__file1.md",
					ViolationType:  ViolationWrongPrefix,
					ExpectedPrefix: "tu__",
				},
				{
					FilePath:       "docs/how-to/file2.md",
					FileName:       "file2.md",
					ViolationType:  ViolationMissingSeparator,
					ExpectedPrefix: "hoto__",
				},
				{
					FilePath:       "docs/reference/re__BadCase.md",
					FileName:       "re__BadCase.md",
					ViolationType:  ViolationBadCase,
					ExpectedPrefix: "re__",
				},
			},
			wantCount: 2, // only wrong_prefix and missing_separator are fixable
		},
		{
			name: "violation without expected prefix",
			violations: []NamingViolation{
				{
					FilePath:       "docs/tutorials/wrong__file.md",
					FileName:       "wrong__file.md",
					ViolationType:  ViolationWrongPrefix,
					ExpectedPrefix: "", // no expected prefix
				},
			},
			wantCount: 0, // skip if no expected prefix
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ops := GenerateRenameOperations(tt.violations)

			if len(ops) != tt.wantCount {
				t.Errorf("GenerateRenameOperations() got %d operations, want %d", len(ops), tt.wantCount)
			}

			if tt.wantFirst != nil && len(ops) > 0 {
				if ops[0].OldPath != tt.wantFirst.OldPath {
					t.Errorf("OldPath = %q, want %q", ops[0].OldPath, tt.wantFirst.OldPath)
				}
				if ops[0].NewPath != tt.wantFirst.NewPath {
					t.Errorf("NewPath = %q, want %q", ops[0].NewPath, tt.wantFirst.NewPath)
				}
				if ops[0].OldName != tt.wantFirst.OldName {
					t.Errorf("OldName = %q, want %q", ops[0].OldName, tt.wantFirst.OldName)
				}
				if ops[0].NewName != tt.wantFirst.NewName {
					t.Errorf("NewName = %q, want %q", ops[0].NewName, tt.wantFirst.NewName)
				}
			}
		})
	}
}

func TestGenerateNewFilename(t *testing.T) {
	tests := []struct {
		name      string
		violation NamingViolation
		want      string
	}{
		{
			name: "wrong prefix - replace prefix",
			violation: NamingViolation{
				FileName:       "wrong__content-name.md",
				ViolationType:  ViolationWrongPrefix,
				ExpectedPrefix: "tu__",
			},
			want: "tu__content-name.md",
		},
		{
			name: "missing separator - add prefix",
			violation: NamingViolation{
				FileName:       "content-name.md",
				ViolationType:  ViolationMissingSeparator,
				ExpectedPrefix: "tu__",
			},
			want: "tu__content-name.md",
		},
		{
			name: "wrong prefix with deep path encoding",
			violation: NamingViolation{
				FileName:       "ex-so__file.md",
				ViolationType:  ViolationWrongPrefix,
				ExpectedPrefix: "ex-so-prla-py__",
			},
			want: "ex-so-prla-py__file.md",
		},
		{
			name: "bad case - not fixable",
			violation: NamingViolation{
				FileName:       "tu__BadCase.md",
				ViolationType:  ViolationBadCase,
				ExpectedPrefix: "tu__",
			},
			want: "", // empty means not fixable
		},
		{
			name: "wrong prefix without separator in filename",
			violation: NamingViolation{
				FileName:       "noseparator.md",
				ViolationType:  ViolationWrongPrefix,
				ExpectedPrefix: "tu__",
			},
			want: "", // can't fix wrong_prefix if no __ exists
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := generateNewFilename(tt.violation)
			if got != tt.want {
				t.Errorf("generateNewFilename() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestValidateRenameOperations(t *testing.T) {
	// Create a temp directory for testing file existence checks
	tempDir := t.TempDir()

	// Create a file that already exists
	existingFile := filepath.Join(tempDir, "docs", "tutorials", "tu__existing.md")
	if err := os.MkdirAll(filepath.Dir(existingFile), 0755); err != nil {
		t.Fatalf("failed to create test directory: %v", err)
	}
	if err := os.WriteFile(existingFile, []byte("test"), 0644); err != nil {
		t.Fatalf("failed to create test file: %v", err)
	}

	tests := []struct {
		name      string
		ops       []RenameOperation
		repoRoot  string
		wantError bool
		errorMsg  string
	}{
		{
			name:      "empty operations",
			ops:       []RenameOperation{},
			repoRoot:  tempDir,
			wantError: false,
		},
		{
			name: "single valid operation",
			ops: []RenameOperation{
				{
					OldPath: "docs/tutorials/wrong__file.md",
					NewPath: "docs/tutorials/tu__file.md",
				},
			},
			repoRoot:  tempDir,
			wantError: false,
		},
		{
			name: "duplicate target paths",
			ops: []RenameOperation{
				{
					OldPath: "docs/tutorials/file1.md",
					NewPath: "docs/tutorials/tu__same-name.md",
				},
				{
					OldPath: "docs/tutorials/file2.md",
					NewPath: "docs/tutorials/tu__same-name.md",
				},
			},
			repoRoot:  tempDir,
			wantError: true,
			errorMsg:  "conflict",
		},
		{
			name: "target file already exists",
			ops: []RenameOperation{
				{
					OldPath: "docs/tutorials/wrong__new.md",
					NewPath: "docs/tutorials/tu__existing.md", // this file exists
				},
			},
			repoRoot:  tempDir,
			wantError: true,
			errorMsg:  "already exists",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := ValidateRenameOperations(tt.ops, tt.repoRoot)

			if tt.wantError {
				if err == nil {
					t.Error("ValidateRenameOperations() expected error, got nil")
				} else if tt.errorMsg != "" && !contains(err.Error(), tt.errorMsg) {
					t.Errorf("error = %q, want to contain %q", err.Error(), tt.errorMsg)
				}
			} else {
				if err != nil {
					t.Errorf("ValidateRenameOperations() unexpected error: %v", err)
				}
			}
		})
	}
}

func TestFix_DryRun(t *testing.T) {
	// Test the Fix function in dry-run mode (no actual file operations)
	tempDir := t.TempDir()

	// Create test directory structure
	docsDir := filepath.Join(tempDir, "docs", "tutorials")
	if err := os.MkdirAll(docsDir, 0755); err != nil {
		t.Fatalf("failed to create test directory: %v", err)
	}

	// Create a test file with wrong prefix
	testFile := filepath.Join(docsDir, "wrong__getting-started.md")
	if err := os.WriteFile(testFile, []byte("# Test\n"), 0644); err != nil {
		t.Fatalf("failed to create test file: %v", err)
	}

	// Create validation result with a fixable violation
	validationResult := &ValidationResult{
		TotalFiles:     1,
		ValidFiles:     0,
		ViolationCount: 1,
		Violations: []NamingViolation{
			{
				FilePath:       "docs/tutorials/wrong__getting-started.md",
				FileName:       "wrong__getting-started.md",
				ViolationType:  ViolationWrongPrefix,
				ExpectedPrefix: "tu__",
				ActualPrefix:   "wrong__",
			},
		},
		ViolationsByType: map[ViolationType][]NamingViolation{
			ViolationWrongPrefix: {
				{
					FilePath:       "docs/tutorials/wrong__getting-started.md",
					FileName:       "wrong__getting-started.md",
					ViolationType:  ViolationWrongPrefix,
					ExpectedPrefix: "tu__",
					ActualPrefix:   "wrong__",
				},
			},
		},
	}

	opts := FixOptions{
		RepoRoot:    tempDir,
		DryRun:      true,
		UpdateLinks: true,
		Verbose:     false,
	}

	result, err := Fix(validationResult, opts)
	if err != nil {
		t.Fatalf("Fix() error: %v", err)
	}

	// Verify dry-run results
	if !result.DryRun {
		t.Error("expected DryRun = true")
	}

	if len(result.RenameOperations) != 1 {
		t.Errorf("expected 1 rename operation, got %d", len(result.RenameOperations))
	}

	if result.RenamesApplied != 0 {
		t.Errorf("expected 0 renames applied in dry-run, got %d", result.RenamesApplied)
	}

	// Verify original file still exists (dry-run shouldn't modify)
	if _, err := os.Stat(testFile); os.IsNotExist(err) {
		t.Error("dry-run should not have deleted the original file")
	}

	// Verify expected rename operation
	if len(result.RenameOperations) > 0 {
		op := result.RenameOperations[0]
		if op.NewName != "tu__getting-started.md" {
			t.Errorf("expected new name 'tu__getting-started.md', got %q", op.NewName)
		}
	}
}

func TestFix_NoViolations(t *testing.T) {
	tempDir := t.TempDir()

	validationResult := &ValidationResult{
		TotalFiles:       5,
		ValidFiles:       5,
		ViolationCount:   0,
		Violations:       []NamingViolation{},
		ViolationsByType: map[ViolationType][]NamingViolation{},
	}

	opts := FixOptions{
		RepoRoot:    tempDir,
		DryRun:      true,
		UpdateLinks: true,
	}

	result, err := Fix(validationResult, opts)
	if err != nil {
		t.Fatalf("Fix() error: %v", err)
	}

	if len(result.RenameOperations) != 0 {
		t.Errorf("expected 0 rename operations for no violations, got %d", len(result.RenameOperations))
	}
}

func TestFix_OnlyNonFixableViolations(t *testing.T) {
	tempDir := t.TempDir()

	// Only bad_case violations which are not auto-fixable
	validationResult := &ValidationResult{
		TotalFiles:     1,
		ValidFiles:     0,
		ViolationCount: 1,
		Violations: []NamingViolation{
			{
				FilePath:       "docs/tutorials/tu__BadCase.md",
				FileName:       "tu__BadCase.md",
				ViolationType:  ViolationBadCase,
				ExpectedPrefix: "tu__",
			},
		},
		ViolationsByType: map[ViolationType][]NamingViolation{
			ViolationBadCase: {
				{
					FilePath:       "docs/tutorials/tu__BadCase.md",
					FileName:       "tu__BadCase.md",
					ViolationType:  ViolationBadCase,
					ExpectedPrefix: "tu__",
				},
			},
		},
	}

	opts := FixOptions{
		RepoRoot:    tempDir,
		DryRun:      true,
		UpdateLinks: true,
	}

	result, err := Fix(validationResult, opts)
	if err != nil {
		t.Fatalf("Fix() error: %v", err)
	}

	if len(result.RenameOperations) != 0 {
		t.Errorf("expected 0 rename operations for non-fixable violations, got %d", len(result.RenameOperations))
	}
}

// contains checks if s contains substr
func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(substr) == 0 ||
		(len(s) > 0 && len(substr) > 0 && searchString(s, substr)))
}

func searchString(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
