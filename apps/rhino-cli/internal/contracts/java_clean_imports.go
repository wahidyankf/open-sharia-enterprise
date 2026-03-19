package contracts

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

var (
	osRename  = os.Rename
	writeFile = os.WriteFile
	readFile  = os.ReadFile
)

// CleanJavaImports removes unused and same-package imports from all .java files in opts.Dir.
// It uses a two-pass approach per file: first to gather context (package name + body),
// then to filter imports (removing unused, same-package, and duplicate imports).
func CleanJavaImports(opts JavaCleanImportsOptions) (*JavaCleanImportsResult, error) {
	result := &JavaCleanImportsResult{
		Modified: []string{},
	}

	walkErr := filepath.WalkDir(opts.Dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}

		if d.IsDir() || !strings.HasSuffix(path, ".java") {
			return nil
		}

		result.TotalFiles++

		modified, processErr := processJavaFile(path)
		if processErr != nil {
			return processErr
		}

		if modified {
			result.ModifiedFiles++
			rel, relErr := filepath.Rel(opts.Dir, path)
			if relErr != nil {
				rel = path
			}
			result.Modified = append(result.Modified, rel)
		}

		return nil
	})

	if walkErr != nil {
		return nil, fmt.Errorf("walking directory %s: %w", opts.Dir, walkErr)
	}

	return result, nil
}

// processJavaFile processes a single .java file, returning true if the file was modified.
func processJavaFile(path string) (bool, error) {
	data, err := readFile(path)
	if err != nil {
		return false, fmt.Errorf("reading file %s: %w", path, err)
	}

	original := string(data)
	lines := strings.Split(original, "\n")

	// Pass 1: gather context — package name and body text (non-import lines joined).
	pkgName := ""
	var bodyParts []string

	for _, line := range lines {
		if pkg, found := strings.CutPrefix(line, "package "); found {
			pkg = strings.TrimSuffix(pkg, ";")
			pkgName = strings.TrimSpace(pkg)
		} else if !strings.HasPrefix(line, "import ") {
			bodyParts = append(bodyParts, line)
		}
	}

	body := strings.Join(bodyParts, "\n")

	// Pass 2: filter imports.
	seen := make(map[string]bool)
	var kept []string

	for _, line := range lines {
		if !strings.HasPrefix(line, "import ") {
			kept = append(kept, line)
			continue
		}

		// Strip the import statement down to the fully-qualified name.
		fqn := strings.TrimPrefix(line, "import ")
		fqn = strings.TrimSuffix(fqn, ";")
		fqn = strings.TrimSpace(fqn)

		// Handle static imports.
		fqn = strings.TrimPrefix(fqn, "static ")
		fqn = strings.TrimSpace(fqn)

		// Split into package + class name.
		parts := strings.Split(fqn, ".")
		if len(parts) < 2 {
			// Malformed import — keep as-is.
			kept = append(kept, line)
			continue
		}

		className := parts[len(parts)-1]
		importPkg := strings.Join(parts[:len(parts)-1], ".")

		// Skip same-package imports.
		if importPkg == pkgName {
			continue
		}

		// Skip if class name not used in body.
		if !strings.Contains(body, className) {
			continue
		}

		// Skip duplicates.
		if seen[line] {
			continue
		}

		seen[line] = true
		kept = append(kept, line)
	}

	cleaned := strings.TrimRight(strings.Join(kept, "\n"), "\n") + "\n"
	originalNorm := strings.TrimRight(original, "\n") + "\n"

	if cleaned == originalNorm {
		return false, nil
	}

	// Write atomically via a temp file.
	tmpPath := path + ".tmp"

	if err := writeFile(tmpPath, []byte(cleaned), 0644); err != nil {
		return false, fmt.Errorf("writing temp file %s: %w", tmpPath, err)
	}

	if err := osRename(tmpPath, path); err != nil {
		return false, fmt.Errorf("renaming %s to %s: %w", tmpPath, path, err)
	}

	return true, nil
}
