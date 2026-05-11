package repogovernance

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

// ReadmeIndexFinding describes a single mismatch between a README.md index and
// the markdown files actually present alongside it.
//
// Kind is one of "orphan" (file exists on disk but is not linked from the
// neighbouring README.md) or "ghost" (README.md links a file that is not
// present on disk).
type ReadmeIndexFinding struct {
	// File is the path to the offending markdown file. For orphans this is the
	// sibling .md file (or sibling subdirectory README.md) that the README
	// fails to link. For ghosts this is the missing target referenced by the
	// README.
	File string
	// Severity is the finding severity (currently always "high").
	Severity string
	// Kind is "orphan" or "ghost".
	Kind string
	// Message is the human-readable description of the finding.
	Message string
}

// readmeLinkRe matches the link target portion of a markdown link whose
// destination contains a .md file. The capture group is the raw URL fragment
// (everything between the parentheses up to but excluding the closing one).
// The pattern accepts trailing fragments (#…) and query strings (?…) after
// the .md token, which are stripped during extraction.
var readmeLinkRe = regexp.MustCompile(`\[[^\]]+\]\(([^)]*\.md(?:[#?][^)]*)?)\)`)

// readmeIndexSkipDirs is the set of directory names that are skipped entirely
// when walking a path. Hidden directories (names starting with ".") are
// skipped in addition to these.
var readmeIndexSkipDirs = map[string]bool{
	"node_modules": true,
	"target":       true,
	"dist":         true,
	"build":        true,
	".next":        true,
	".git":         true,
}

// AuditReadmeIndex walks each path in paths and, for every README.md it finds,
// compares the README's link listings against the actual markdown files in the
// same directory and the immediate-child subdirectories that themselves
// contain a README.md. Findings are returned sorted by (File, Kind) for
// deterministic output.
//
// excludes is a list of glob patterns matched with filepath.Match against the
// path relative to the walked root. Matching files and directories are
// skipped entirely — they are neither audited as README hosts nor treated as
// sibling files to compare against.
func AuditReadmeIndex(paths []string, excludes []string) ([]ReadmeIndexFinding, error) {
	if len(paths) == 0 {
		return nil, errors.New("at least one path is required")
	}

	var findings []ReadmeIndexFinding
	for _, root := range paths {
		readmes, err := findReadmes(root, excludes)
		if err != nil {
			return nil, err
		}
		for _, readme := range readmes {
			more, err := auditOneReadme(readme, root, excludes)
			if err != nil {
				return nil, err
			}
			findings = append(findings, more...)
		}
	}

	sort.SliceStable(findings, func(i, j int) bool {
		if findings[i].File == findings[j].File {
			return findings[i].Kind < findings[j].Kind
		}
		return findings[i].File < findings[j].File
	})
	return findings, nil
}

// findReadmes walks root recursively and returns every README.md path found,
// honouring the hidden-directory and reserved-directory skip rules and the
// caller-supplied excludes list.
func findReadmes(root string, excludes []string) ([]string, error) {
	var readmes []string
	walkErr := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			if os.IsNotExist(err) {
				return filepath.SkipAll
			}
			return err
		}
		rel, _ := filepath.Rel(root, path)
		if d.IsDir() {
			name := d.Name()
			if path != root && (strings.HasPrefix(name, ".") || readmeIndexSkipDirs[name]) {
				return filepath.SkipDir
			}
			if path != root && matchesAnyGlob(rel, excludes) {
				return filepath.SkipDir
			}
			return nil
		}
		if d.Name() != "README.md" {
			return nil
		}
		if matchesAnyGlob(rel, excludes) {
			return nil
		}
		readmes = append(readmes, path)
		return nil
	})
	if walkErr != nil && !os.IsNotExist(walkErr) {
		return nil, fmt.Errorf("walk %s: %w", root, walkErr)
	}
	sort.Strings(readmes)
	return readmes, nil
}

// auditOneReadme produces findings for a single README.md by comparing its
// .md link targets against the actual sibling markdown files and immediate
// subdirectory READMEs. root is the walk root used to resolve excludes.
func auditOneReadme(readmePath, root string, excludes []string) ([]ReadmeIndexFinding, error) {
	dir := filepath.Dir(readmePath)
	data, err := os.ReadFile(readmePath) //nolint:gosec // trusted repo path
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", readmePath, err)
	}

	linked := extractReadmeLinks(string(data))
	actual, err := listSiblingTargets(dir, root, excludes)
	if err != nil {
		return nil, err
	}

	var findings []ReadmeIndexFinding

	// Orphans: file present on disk but not linked from the README.
	for _, name := range actual.sortedNames() {
		if !linked[name] {
			full := filepath.Join(dir, name)
			findings = append(findings, ReadmeIndexFinding{
				File:     full,
				Severity: "high",
				Kind:     "orphan",
				Message:  fmt.Sprintf("orphan: %s exists but is not linked from %s", name, readmePath),
			})
		}
	}

	// Ghosts: README references a target that is not present on disk.
	for _, link := range sortedKeys(linked) {
		if !actual.present(link) {
			full := filepath.Join(dir, link)
			findings = append(findings, ReadmeIndexFinding{
				File:     full,
				Severity: "high",
				Kind:     "ghost",
				Message:  fmt.Sprintf("ghost: %s references %s but the target does not exist", readmePath, link),
			})
		}
	}

	return findings, nil
}

// extractReadmeLinks parses content for relative markdown link targets whose
// destination is a .md file and returns them as a set keyed by the relative
// path. Absolute paths and URLs that include a scheme are skipped because
// they cannot identify a sibling file. Fragments (#…) and query strings (?…)
// are stripped before storage.
func extractReadmeLinks(content string) map[string]bool {
	out := make(map[string]bool)
	for _, m := range readmeLinkRe.FindAllStringSubmatch(content, -1) {
		raw := strings.TrimSpace(m[1])
		if raw == "" {
			continue
		}
		// Strip ./ prefix, fragments, and query strings.
		raw = strings.TrimPrefix(raw, "./")
		if i := strings.IndexAny(raw, "#?"); i >= 0 {
			raw = raw[:i]
		}
		// Skip absolute paths, URLs (including scheme-only forms like
		// "mailto:..."), and parent-directory traversals — none can refer to
		// a sibling file. We treat any leading scheme followed by ":" before
		// the first "/" as a URL.
		if raw == "" || strings.HasPrefix(raw, "/") || strings.HasPrefix(raw, "..") {
			continue
		}
		if i := strings.Index(raw, ":"); i > 0 {
			slash := strings.Index(raw, "/")
			if slash < 0 || i < slash {
				continue
			}
		}
		out[filepath.ToSlash(raw)] = true
	}
	return out
}

// siblingTargets holds the set of sibling files (relative to a README's
// directory) that should appear in the README's index — namely the immediate
// *.md files plus any immediate subdirectory containing its own README.md.
type siblingTargets struct {
	files   map[string]bool // e.g. "convention-foo.md"
	subDirs map[string]bool // e.g. "structure/README.md"
}

func newSiblingTargets() siblingTargets {
	return siblingTargets{files: map[string]bool{}, subDirs: map[string]bool{}}
}

func (s siblingTargets) sortedNames() []string {
	names := make([]string, 0, len(s.files)+len(s.subDirs))
	for k := range s.files {
		names = append(names, k)
	}
	for k := range s.subDirs {
		names = append(names, k)
	}
	sort.Strings(names)
	return names
}

// present reports whether link (normalized) refers to a known sibling file or
// subdirectory README. Both `subdir/README.md` and `subdir/` are accepted for
// subdirectory targets.
func (s siblingTargets) present(link string) bool {
	link = strings.TrimSuffix(filepath.ToSlash(link), "/")
	if s.files[link] {
		return true
	}
	if s.subDirs[link] {
		return true
	}
	// Allow bare-directory style: "structure" → "structure/README.md".
	if s.subDirs[link+"/README.md"] {
		return true
	}
	return false
}

// listSiblingTargets returns the immediate sibling .md files (excluding the
// README itself) and the immediate subdirectories that contain their own
// README.md. root and excludes are used to filter out excluded paths.
func listSiblingTargets(dir, root string, excludes []string) (siblingTargets, error) {
	out := newSiblingTargets()
	entries, err := os.ReadDir(dir)
	if err != nil {
		return out, fmt.Errorf("read dir %s: %w", dir, err)
	}
	for _, e := range entries {
		name := e.Name()
		full := filepath.Join(dir, name)
		rel, _ := filepath.Rel(root, full)
		if matchesAnyGlob(rel, excludes) {
			continue
		}
		if e.IsDir() {
			if strings.HasPrefix(name, ".") || readmeIndexSkipDirs[name] {
				continue
			}
			subReadme := filepath.Join(full, "README.md")
			if _, err := os.Stat(subReadme); err == nil {
				out.subDirs[filepath.ToSlash(filepath.Join(name, "README.md"))] = true
			}
			continue
		}
		if strings.HasPrefix(name, ".") {
			continue
		}
		if name == "README.md" {
			continue
		}
		if !strings.HasSuffix(name, ".md") {
			continue
		}
		out.files[name] = true
	}
	return out, nil
}

// sortedKeys returns the keys of m in lexicographic order.
func sortedKeys(m map[string]bool) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

// matchesAnyGlob reports whether rel matches any of the supplied glob patterns
// per filepath.Match semantics. Both forward-slash and OS-native separators in
// rel are accepted by normalising to slash form before matching. A pattern is
// tested against (a) the full normalised path, (b) the basename, and (c) any
// individual path component — so a pattern of "node_modules" matches both the
// path "node_modules" and the path "node_modules/foo".
func matchesAnyGlob(rel string, patterns []string) bool {
	if rel == "" || rel == "." {
		return false
	}
	slashed := filepath.ToSlash(rel)
	components := strings.Split(slashed, "/")
	for _, p := range patterns {
		if p == "" {
			continue
		}
		if ok, _ := filepath.Match(p, slashed); ok {
			return true
		}
		if ok, _ := filepath.Match(p, filepath.Base(slashed)); ok {
			return true
		}
		for _, c := range components {
			if ok, _ := filepath.Match(p, c); ok {
				return true
			}
		}
	}
	return false
}
