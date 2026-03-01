package docs

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// GenerateRenameOperations creates rename operations from validation violations.
// Only processes violations that can be automatically fixed (wrong_prefix, missing_separator).
func GenerateRenameOperations(violations []NamingViolation) []RenameOperation {
	var ops []RenameOperation

	for _, v := range violations {
		// Only fix prefix-related violations
		if v.ViolationType != ViolationWrongPrefix && v.ViolationType != ViolationMissingSeparator {
			continue
		}

		// Skip if we don't have an expected prefix
		if v.ExpectedPrefix == "" {
			continue
		}

		newName := generateNewFilename(v)
		if newName == "" || newName == v.FileName {
			continue
		}

		dir := filepath.Dir(v.FilePath)
		newPath := filepath.Join(dir, newName)

		ops = append(ops, RenameOperation{
			OldPath: v.FilePath,
			NewPath: newPath,
			OldName: v.FileName,
			NewName: newName,
		})
	}

	return ops
}

// generateNewFilename creates the corrected filename for a violation.
func generateNewFilename(v NamingViolation) string {
	filename := v.FileName
	expectedPrefix := v.ExpectedPrefix

	switch v.ViolationType {
	case ViolationWrongPrefix:
		// Replace the wrong prefix with the correct one
		// Find where __ appears
		idx := strings.Index(filename, "__")
		if idx == -1 {
			return ""
		}
		// Get the part after __ (content identifier + extension)
		contentPart := filename[idx+2:]
		return expectedPrefix + contentPart

	case ViolationMissingSeparator:
		// Add the expected prefix before the filename
		// The filename doesn't have __, so we need to add prefix__
		ext := filepath.Ext(filename)
		nameWithoutExt := strings.TrimSuffix(filename, ext)
		return expectedPrefix + nameWithoutExt + ext

	case ViolationBadCase, ViolationMissingPrefix:
		// Cannot be auto-fixed: requires human judgment to determine correct name
		return ""

	default:
		return ""
	}
}

// ValidateRenameOperations checks for conflicts in rename operations.
// Returns an error if two files would be renamed to the same target.
func ValidateRenameOperations(ops []RenameOperation, repoRoot string) error {
	// Check for duplicate targets
	targets := make(map[string]string) // newPath -> oldPath
	for _, op := range ops {
		if existing, ok := targets[op.NewPath]; ok {
			return fmt.Errorf("conflict: both '%s' and '%s' would be renamed to '%s'",
				existing, op.OldPath, op.NewPath)
		}
		targets[op.NewPath] = op.OldPath
	}

	// Check that target files don't already exist
	for _, op := range ops {
		targetPath := filepath.Join(repoRoot, op.NewPath)
		if _, err := os.Stat(targetPath); err == nil {
			// Target exists - check if it's the same as source (shouldn't happen)
			sourcePath := filepath.Join(repoRoot, op.OldPath)
			if targetPath != sourcePath {
				return fmt.Errorf("target file already exists: '%s'", op.NewPath)
			}
		}
	}

	return nil
}

// ApplyRenames executes the rename operations.
// Uses git mv to preserve history.
func ApplyRenames(ops []RenameOperation, repoRoot string) (int, []string) {
	var applied int
	var errors []string

	for _, op := range ops {
		err := renameFileWithGit(op, repoRoot)
		if err != nil {
			errors = append(errors, fmt.Sprintf("failed to rename '%s': %v", op.OldPath, err))
			// Continue with other renames
		} else {
			applied++
		}
	}

	return applied, errors
}

// renameFileWithGit renames a file using git mv.
func renameFileWithGit(op RenameOperation, repoRoot string) error {
	// First try git mv
	cmd := exec.Command("git", "mv", op.OldPath, op.NewPath)
	cmd.Dir = repoRoot
	output, err := cmd.CombinedOutput()
	if err != nil {
		// If git mv fails, try regular rename
		oldAbs := filepath.Join(repoRoot, op.OldPath)
		newAbs := filepath.Join(repoRoot, op.NewPath)
		if renameErr := os.Rename(oldAbs, newAbs); renameErr != nil {
			return fmt.Errorf("git mv failed: %v, os.Rename failed: %v",
				string(output), renameErr)
		}
	}
	return nil
}

// Fix performs the complete fix operation.
func Fix(validationResult *ValidationResult, opts FixOptions) (*FixResult, error) {
	result := &FixResult{
		DryRun: opts.DryRun,
	}

	// Generate rename operations from violations
	result.RenameOperations = GenerateRenameOperations(validationResult.Violations)

	if len(result.RenameOperations) == 0 {
		return result, nil
	}

	// Validate no conflicts
	if err := ValidateRenameOperations(result.RenameOperations, opts.RepoRoot); err != nil {
		return nil, fmt.Errorf("validation failed: %w", err)
	}

	// Find links that need updating
	if opts.UpdateLinks {
		linkUpdates, err := FindLinksToUpdate(result.RenameOperations, opts.RepoRoot)
		if err != nil {
			result.Errors = append(result.Errors, fmt.Sprintf("error finding links: %v", err))
		} else {
			result.LinkUpdates = linkUpdates
		}
	}

	// If dry run, stop here
	if opts.DryRun {
		return result, nil
	}

	// Apply link updates first (before files are renamed)
	if opts.UpdateLinks && len(result.LinkUpdates) > 0 {
		updated, errs := ApplyLinkUpdates(result.LinkUpdates, opts.RepoRoot)
		result.LinksUpdated = updated
		result.Errors = append(result.Errors, errs...)
	}

	// Apply renames
	applied, errs := ApplyRenames(result.RenameOperations, opts.RepoRoot)
	result.RenamesApplied = applied
	result.Errors = append(result.Errors, errs...)

	return result, nil
}
