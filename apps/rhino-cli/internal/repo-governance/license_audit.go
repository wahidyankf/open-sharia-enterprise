package repogovernance

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// LicenseFinding describes a single license-audit violation.
//
// Kind is one of:
//   - "missing-license": the LICENSE file is absent in a required directory.
//   - "spdx-mismatch": the LICENSING-NOTICE.md row's license does not match
//     the on-disk LICENSE file's identified SPDX.
//   - "unreadable-license": the LICENSE file exists but cannot be parsed.
type LicenseFinding struct {
	Path    string `json:"path"`
	Kind    string `json:"kind"`
	Message string `json:"message"`
}

// licenseExemptApps enumerates apps/ subdirectories that are exempt from the
// per-directory LICENSE requirement per
// repo-governance/conventions/structure/licensing.md (internal CLI tool).
// Anything matching the *-e2e suffix is also exempt and handled separately.
var licenseExemptApps = map[string]struct{}{
	"rhino-cli": {},
}

// AuditLicense walks repoRoot and verifies that every required product app,
// library, and specs directory carries an MIT LICENSE file matching any claim
// in LICENSING-NOTICE.md. Required directories are:
//
//   - apps/<name>/ where <name> is not in licenseExemptApps and does not end
//     in "-e2e" (the exemption list mirrors the Licensing Convention)
//   - libs/<name>/ (non-recursive)
//   - specs/
//
// LICENSING-NOTICE.md is parsed for any markdown table whose header includes
// "Path" or "Directory" together with "License". Each row's path is normalised
// (backticks stripped, leading/trailing whitespace removed) and its claimed
// license compared against the SPDX identifier extracted from the first
// non-empty line of the corresponding LICENSE file.
//
// Findings are returned in stable, deterministic order: by Path then by Kind.
// A missing repoRoot or missing LICENSING-NOTICE.md is not an error; in that
// case the LICENSING-NOTICE comparison phase is skipped and the directory
// presence check still runs.
func AuditLicense(repoRoot string) ([]LicenseFinding, error) {
	var findings []LicenseFinding

	dirs, err := requiredLicenseDirs(repoRoot)
	if err != nil {
		return nil, err
	}

	// Map of relative directory path -> identified SPDX (or empty if missing).
	licenseByDir := make(map[string]string, len(dirs))

	for _, rel := range dirs {
		licensePath := filepath.Join(repoRoot, rel, "LICENSE")
		spdx, err := extractSPDX(licensePath)
		if err != nil {
			if os.IsNotExist(err) {
				findings = append(findings, LicenseFinding{
					Path:    filepath.ToSlash(rel),
					Kind:    "missing-license",
					Message: fmt.Sprintf("required directory %q has no LICENSE file", filepath.ToSlash(rel)),
				})
				continue
			}
			findings = append(findings, LicenseFinding{
				Path:    filepath.ToSlash(rel),
				Kind:    "unreadable-license",
				Message: fmt.Sprintf("read LICENSE in %q: %v", filepath.ToSlash(rel), err),
			})
			continue
		}
		licenseByDir[filepath.ToSlash(rel)] = spdx
	}

	// Parse LICENSING-NOTICE.md for any rows claiming a license for a path.
	noticePath := filepath.Join(repoRoot, "LICENSING-NOTICE.md")
	claims, err := parseLicensingNotice(noticePath)
	if err != nil && !os.IsNotExist(err) {
		return nil, err
	}

	for _, claim := range claims {
		// Skip third-party paths the audit does not own (anything outside
		// apps/, libs/, and specs/ — e.g. archived/). The convention treats
		// these as informational only.
		normalised := normaliseClaimPath(claim.Path)
		if !ownedByLicenseAudit(normalised) {
			continue
		}
		identified, present := licenseByDir[normalised]
		if !present {
			// LICENSING-NOTICE claims a directory we did not collect — most
			// commonly because the LICENSE file is missing. The
			// missing-license finding already covers this case, so do not
			// add a duplicate finding.
			continue
		}
		if !licensesEqual(identified, claim.License) {
			findings = append(findings, LicenseFinding{
				Path: normalised,
				Kind: "spdx-mismatch",
				Message: fmt.Sprintf(
					"LICENSING-NOTICE.md claims %q for %q but LICENSE identifies %q",
					claim.License, normalised, identified,
				),
			})
		}
	}

	sort.SliceStable(findings, func(i, j int) bool {
		if findings[i].Path == findings[j].Path {
			return findings[i].Kind < findings[j].Kind
		}
		return findings[i].Path < findings[j].Path
	})

	return findings, nil
}

// requiredLicenseDirs returns the relative paths (slash-separated) of every
// directory that the audit requires to carry a LICENSE file.
func requiredLicenseDirs(repoRoot string) ([]string, error) {
	var dirs []string

	appsDir := filepath.Join(repoRoot, "apps")
	appEntries, err := readNonHiddenDirs(appsDir)
	if err != nil {
		return nil, err
	}
	for _, name := range appEntries {
		if _, exempt := licenseExemptApps[name]; exempt {
			continue
		}
		if strings.HasSuffix(name, "-e2e") {
			continue
		}
		dirs = append(dirs, filepath.ToSlash(filepath.Join("apps", name)))
	}

	libsDir := filepath.Join(repoRoot, "libs")
	libEntries, err := readNonHiddenDirs(libsDir)
	if err != nil {
		return nil, err
	}
	for _, name := range libEntries {
		dirs = append(dirs, filepath.ToSlash(filepath.Join("libs", name)))
	}

	specsDir := filepath.Join(repoRoot, "specs")
	if info, err := os.Stat(specsDir); err == nil && info.IsDir() {
		dirs = append(dirs, "specs")
	} else if err != nil && !os.IsNotExist(err) {
		return nil, fmt.Errorf("stat %s: %w", specsDir, err)
	}

	sort.Strings(dirs)
	return dirs, nil
}

// readNonHiddenDirs returns the names of immediate subdirectories of dir.
// A missing dir yields a nil slice with no error.
func readNonHiddenDirs(dir string) ([]string, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, fmt.Errorf("read %s: %w", dir, err)
	}
	var names []string
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		if strings.HasPrefix(name, ".") {
			continue
		}
		names = append(names, name)
	}
	sort.Strings(names)
	return names, nil
}

// extractSPDX reads the LICENSE file at path and returns the SPDX identifier
// extracted from its first non-empty line. Recognised forms:
//
//   - "MIT License"            -> "MIT"
//   - "Apache License 2.0"     -> "Apache-2.0"
//   - "BSD 3-Clause License"   -> "BSD-3-Clause"
//   - "BSD 2-Clause License"   -> "BSD-2-Clause"
//   - "SPDX-License-Identifier: <id>" -> "<id>"
//
// If the line cannot be classified, the trimmed first non-empty line is
// returned verbatim as the identifier (callers compare with licensesEqual).
func extractSPDX(path string) (string, error) {
	f, err := os.Open(path) //nolint:gosec // trusted repo path
	if err != nil {
		return "", err
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		return classifyLicenseLine(line), nil
	}
	if err := scanner.Err(); err != nil {
		return "", fmt.Errorf("scan %s: %w", path, err)
	}
	return "", fmt.Errorf("LICENSE file %q is empty", path)
}

// classifyLicenseLine converts a LICENSE file's header line into an SPDX
// identifier. The mapping is deliberately conservative and covers the licenses
// currently in use in this repository plus a handful of common forms.
func classifyLicenseLine(line string) string {
	// Explicit SPDX header.
	if rest, ok := stripPrefixFold(line, "SPDX-License-Identifier:"); ok {
		return strings.TrimSpace(rest)
	}
	lower := strings.ToLower(line)
	switch {
	case strings.Contains(lower, "mit license"), lower == "mit":
		return "MIT"
	case strings.Contains(lower, "apache license, version 2.0"),
		strings.Contains(lower, "apache license 2.0"),
		strings.Contains(lower, "apache-2.0"):
		return "Apache-2.0"
	case strings.Contains(lower, "bsd 3-clause"), strings.Contains(lower, "bsd-3-clause"):
		return "BSD-3-Clause"
	case strings.Contains(lower, "bsd 2-clause"), strings.Contains(lower, "bsd-2-clause"):
		return "BSD-2-Clause"
	case strings.Contains(lower, "mozilla public license"), strings.Contains(lower, "mpl-2.0"):
		return "MPL-2.0"
	case strings.Contains(lower, "gnu general public license"):
		return "GPL"
	}
	return line
}

// stripPrefixFold returns the suffix of s after a case-insensitive match of
// prefix, together with ok=true. If s does not start with prefix (ignoring
// case) it returns ("", false).
func stripPrefixFold(s, prefix string) (string, bool) {
	if len(s) < len(prefix) {
		return "", false
	}
	if !strings.EqualFold(s[:len(prefix)], prefix) {
		return "", false
	}
	return s[len(prefix):], true
}

// licenseClaim is one row parsed out of LICENSING-NOTICE.md.
type licenseClaim struct {
	Path    string
	License string
}

// parseLicensingNotice extracts every "path | license [| ...]" row from
// markdown tables in LICENSING-NOTICE.md. Header detection is tolerant:
// any pipe table whose header contains both a "Path" (or "Directory") column
// and a "License" column is consumed.
func parseLicensingNotice(path string) ([]licenseClaim, error) {
	data, err := os.ReadFile(path) //nolint:gosec // trusted repo path
	if err != nil {
		return nil, err
	}

	var claims []licenseClaim
	lines := strings.Split(string(data), "\n")

	pathCol, licenseCol := -1, -1
	inTable := false

	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if !strings.HasPrefix(line, "|") {
			pathCol, licenseCol, inTable = -1, -1, false
			continue
		}
		cells := splitMarkdownRow(line)

		// Detect a header row by looking at the next line for the
		// "---" separator. A header row resets table state.
		if !inTable {
			if i+1 >= len(lines) {
				continue
			}
			sep := strings.TrimSpace(lines[i+1])
			if !isMarkdownTableSeparator(sep) {
				continue
			}
			pathCol, licenseCol = findColumns(cells)
			if pathCol >= 0 && licenseCol >= 0 {
				inTable = true
			}
			// Skip past the separator row.
			i++
			continue
		}

		if pathCol >= len(cells) || licenseCol >= len(cells) {
			continue
		}
		rawPath := strings.TrimSpace(cells[pathCol])
		rawLicense := strings.TrimSpace(cells[licenseCol])
		if rawPath == "" || rawLicense == "" {
			continue
		}
		claims = append(claims, licenseClaim{
			Path:    rawPath,
			License: rawLicense,
		})
	}

	return claims, nil
}

// splitMarkdownRow splits a "| a | b | c |" pipe row into ["a", "b", "c"],
// honouring escaped pipes as literal pipes. Leading and trailing pipes are
// dropped.
func splitMarkdownRow(line string) []string {
	trimmed := strings.TrimSpace(line)
	trimmed = strings.TrimPrefix(trimmed, "|")
	trimmed = strings.TrimSuffix(trimmed, "|")

	var cells []string
	var current strings.Builder
	escaped := false
	for _, r := range trimmed {
		if escaped {
			current.WriteRune(r)
			escaped = false
			continue
		}
		if r == '\\' {
			escaped = true
			continue
		}
		if r == '|' {
			cells = append(cells, current.String())
			current.Reset()
			continue
		}
		current.WriteRune(r)
	}
	cells = append(cells, current.String())
	return cells
}

// isMarkdownTableSeparator reports whether the given line is a markdown
// table separator (a row of |---|---|--- patterns).
func isMarkdownTableSeparator(line string) bool {
	if !strings.HasPrefix(line, "|") {
		return false
	}
	cells := splitMarkdownRow(line)
	if len(cells) == 0 {
		return false
	}
	for _, c := range cells {
		c = strings.TrimSpace(c)
		c = strings.Trim(c, ":")
		if c == "" {
			return false
		}
		for _, r := range c {
			if r != '-' {
				return false
			}
		}
	}
	return true
}

// findColumns scans header cells and returns (pathCol, licenseCol). A column
// counts as a "path" column when its lowercased header is one of "path" or
// "directory"; a "license" column when its header equals "license".
func findColumns(cells []string) (int, int) {
	pathCol, licenseCol := -1, -1
	for i, c := range cells {
		h := strings.ToLower(strings.TrimSpace(c))
		switch h {
		case "path", "directory":
			if pathCol == -1 {
				pathCol = i
			}
		case "license":
			if licenseCol == -1 {
				licenseCol = i
			}
		}
	}
	return pathCol, licenseCol
}

// normaliseClaimPath strips formatting decoration from a LICENSING-NOTICE row
// path cell (backticks, surrounding whitespace, leading "./", trailing slash)
// and returns the slash-separated relative path.
func normaliseClaimPath(raw string) string {
	s := strings.TrimSpace(raw)
	s = strings.Trim(s, "`")
	s = strings.TrimSpace(s)
	s = strings.TrimPrefix(s, "./")
	s = strings.TrimSuffix(s, "/")
	return filepath.ToSlash(s)
}

// ownedByLicenseAudit reports whether the given normalised path falls under
// one of the directory categories this audit owns: apps/<name>, libs/<name>,
// or specs (exact match).
func ownedByLicenseAudit(p string) bool {
	if p == "specs" {
		return true
	}
	if strings.HasPrefix(p, "apps/") || strings.HasPrefix(p, "libs/") {
		// Only own immediate children — apps/foo, libs/bar.
		// Nested paths (apps/foo/src) are out of scope.
		rest := p
		if after, ok := strings.CutPrefix(rest, "apps/"); ok {
			rest = after
		} else if after, ok := strings.CutPrefix(rest, "libs/"); ok {
			rest = after
		}
		if rest == "" || strings.Contains(rest, "/") {
			return false
		}
		return true
	}
	return false
}

// licensesEqual compares an SPDX identifier from a LICENSE file with a claim
// from LICENSING-NOTICE.md case-insensitively, treating common aliases as
// equivalent (e.g. "MIT" and "MIT License").
func licensesEqual(identified, claim string) bool {
	if strings.EqualFold(identified, claim) {
		return true
	}
	normIdentified := classifyLicenseLine(identified)
	normClaim := classifyLicenseLine(claim)
	return strings.EqualFold(normIdentified, normClaim)
}
