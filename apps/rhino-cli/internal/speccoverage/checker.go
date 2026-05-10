package speccoverage

import (
	"bufio"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"
)

var (
	// scenarioDefRe matches Scenario("title", or Scenario('title',
	// Handles escape sequences inside quoted strings (e.g. \' inside single-quoted strings).
	scenarioDefRe = regexp.MustCompile(`Scenario\s*\(\s*(?:"((?:[^"\\]|\\.)*)"|'((?:[^'\\]|\\.)*)')\s*,`)
	// stepDefRe matches Given/When/Then/And/But("text", or ('text',
	// Uses (?s) dotall for multi-line: Then(\n  "text",\n  fn)
	// Handles escape sequences inside quoted strings (e.g. \' inside single-quoted strings).
	stepDefRe = regexp.MustCompile(`(?s)(?:Given|When|Then|And|But)\s*\(\s*(?:"((?:[^"\\]|\\.)*)"|'((?:[^'\\]|\\.)*)')\s*,`)
	// goStepRe matches a godog-style call: ".Step(" then backtick, regex, backtick.
	// Example call shape (using <BT> as backtick stand-in to avoid the extractor
	// catching this comment as a step impl): sc.Step(<BT>^step text here$<BT>, fn).
	// Raw string cannot be used here because the pattern itself contains backtick characters.
	goStepRe = regexp.MustCompile("\\.Step\\(\\x60([^\\x60]+)\\x60") //nolint:staticcheck
	// goScenarioCommentRe matches: // Scenario: Title Here
	goScenarioCommentRe = regexp.MustCompile(`//\s*Scenario:\s*(.+?)\s*$`)
	// tsRegexStepRe matches Given(/^pattern$/, fn) or When(/^pattern$/, fn) in TS/JS.
	// Uses (?s) dotall for multi-line: When(\n  /^pattern$/,\n  fn)
	tsRegexStepRe = regexp.MustCompile(`(?s)(?:Given|When|Then|And|But)\s*\(\s*/\^?(.*?)\$?\s*/\s*,`)
)

// stepMatcherEntry is a single step extraction record, either an exact-text
// match or a compiled regex pattern, with the originating source file path
// for reverse-direction reporting (Phase 5B / Fix #15).
type stepMatcherEntry struct {
	Kind        string         // "exact" or "pattern"
	ExactText   string         // populated when Kind == "exact" (whitespace-normalized)
	Pattern     *regexp.Regexp // populated when Kind == "pattern"
	PatternText string         // raw regex source for reporting (Kind == "pattern")
	File        string         // origin file (absolute path; reporter resolves to repo-relative)
}

// stepMatcher holds extracted step definitions from source code.
//
// Phase 5B (Fix #15) refactor: the canonical store is `entries`, a slice of
// stepMatcherEntry that records origin file per matcher. `exactIndex` is a
// rebuild-on-append fast lookup used by the forward-direction matches() API.
//
// Legacy fields `exact` and `patterns` are retained as write-through views so
// existing language-specific extractors (rust_steps.go, etc.) and unit tests
// that probe the matcher's internal shape continue to compile while the
// extraction layer is migrated. Both legacy fields and `entries` stay in sync
// through addStepToMatcherWithOrigin (and addPattern shim used by raw regex
// appenders such as F#, Elixir, Go-godog).
type stepMatcher struct {
	entries    []stepMatcherEntry
	exactIndex map[string]int   // exact text → index into entries
	exact      map[string]bool  // legacy: derived view, write-through from entries
	patterns   []*regexp.Regexp // legacy: derived view, write-through from entries
}

// newStepMatcher constructs an empty matcher with all internal maps initialized.
func newStepMatcher() *stepMatcher {
	return &stepMatcher{
		entries:    nil,
		exactIndex: map[string]int{},
		exact:      map[string]bool{},
		patterns:   nil,
	}
}

// matches returns true if the given step text matches either an exact entry or
// a compiled regex pattern. Behavior is preserved verbatim from the pre-refactor
// implementation: O(1) exact lookup, then linear scan over patterns.
//
// The legacy `patterns` slice is consulted directly (rather than scanning
// `entries` for Kind=="pattern") because some unit tests synthesize stepMatcher
// values by appending directly to `patterns`. addPatternWithOrigin keeps both
// `entries` and `patterns` in lockstep for the production extraction path.
// stepCovered returns true when either the unexpanded step text matches an
// impl OR every Scenario Outline expansion matches an impl. The OR semantics
// keep TS-with-vitest-cucumber impls (literal `<placeholder>` matchers) and
// playwright-bdd impls (regex matchers binding expanded values) both valid.
func stepCovered(sm *stepMatcher, step ParsedStep) bool {
	if sm.matches(step.Text) {
		return true
	}
	if len(step.Variants) == 0 {
		return false
	}
	for _, v := range step.Variants {
		if !sm.matches(v) {
			return false
		}
	}
	return true
}

func (sm *stepMatcher) matches(stepText string) bool {
	normalized := normalizeWS(stepText)
	if sm.exact[normalized] {
		return true
	}
	for _, re := range sm.patterns {
		if re.MatchString(normalized) {
			return true
		}
	}
	return false
}

// addStepToMatcherWithOrigin records an exact-text matcher entry with origin
// file. Used by the central addStepToMatcher / addPythonStepToMatcher helpers
// after they classify the input as a literal exact match.
func (sm *stepMatcher) addExactWithOrigin(text, file string) {
	if sm.exactIndex == nil {
		sm.exactIndex = map[string]int{}
	}
	if sm.exact == nil {
		sm.exact = map[string]bool{}
	}
	if _, ok := sm.exactIndex[text]; ok {
		return
	}
	sm.entries = append(sm.entries, stepMatcherEntry{
		Kind:      "exact",
		ExactText: text,
		File:      file,
	})
	sm.exactIndex[text] = len(sm.entries) - 1
	sm.exact[text] = true
}

// addPatternWithOrigin records a compiled regex pattern matcher entry with
// origin file. PatternText is the raw regex source string used in reverse-
// direction reporting.
func (sm *stepMatcher) addPatternWithOrigin(re *regexp.Regexp, patternText, file string) {
	sm.entries = append(sm.entries, stepMatcherEntry{
		Kind:        "pattern",
		Pattern:     re,
		PatternText: patternText,
		File:        file,
	})
	sm.patterns = append(sm.patterns, re)
}

// skipDirs are directories to skip when walking app source files.
var skipDirs = map[string]bool{
	"node_modules":        true,
	".next":               true,
	"build":               true,
	"dist":                true,
	"storybook-static":    true,
	"coverage":            true,
	".git":                true,
	"target":              true,
	"_build":              true,
	"deps":                true,
	"bin":                 true,
	"obj":                 true,
	"__pycache__":         true,
	".pytest_cache":       true,
	".venv":               true,
	"generated-contracts": true,
	"generated_contracts": true,
	".dart_tool":          true,
	".features-gen":       true,
}

// CheckAll walks every spec tree in SpecsDirs (falling back to SpecsDir for
// backward compatibility) for .feature files and validates coverage.
// In default mode: 1:1 file matching + scenario + step validation.
// In --shared-steps mode: step-only validation across ALL source files.
func CheckAll(opts ScanOptions) (*CheckResult, error) {
	if len(opts.SpecsDirs) == 0 && opts.SpecsDir != "" {
		opts.SpecsDirs = []string{opts.SpecsDir}
	}
	if opts.SharedSteps {
		return checkSharedSteps(opts)
	}
	return checkOneToOne(opts)
}

// collectFeatureFiles walks every configured spec tree and returns the union.
// Falls back to opts.SpecsDir when SpecsDirs is empty for backward compatibility
// with callers that supply a single tree.
func collectFeatureFiles(opts ScanOptions) ([]string, error) {
	dirs := opts.SpecsDirs
	if len(dirs) == 0 && opts.SpecsDir != "" {
		dirs = []string{opts.SpecsDir}
	}
	var all []string
	for _, dir := range dirs {
		files, err := walkFeatureFiles(dir, opts.ExcludeDirs...)
		if err != nil {
			return nil, err
		}
		all = append(all, files...)
	}
	return all, nil
}

// checkSharedSteps validates step-level coverage only (no file/scenario matching).
// Used for E2E projects where step files are shared across features.
func checkSharedSteps(opts ScanOptions) (*CheckResult, error) {
	start := time.Now()

	specFiles, err := collectFeatureFiles(opts)
	if err != nil {
		return nil, err
	}

	allStepTexts, err := extractAllStepTexts(opts.AppDir)
	if err != nil {
		return nil, err
	}

	var stepGaps []StepGap
	var allGherkinSteps []string
	totalScenarios := 0
	totalSteps := 0

	for _, specFile := range specFiles {
		relSpec, err := filepath.Rel(opts.RepoRoot, specFile)
		if err != nil {
			relSpec = specFile
		}

		scenarios, err := ParseFeatureFile(specFile)
		if err != nil {
			return nil, err
		}

		for _, sc := range scenarios {
			totalScenarios++
			for _, step := range sc.Steps {
				totalSteps++
				allGherkinSteps = append(allGherkinSteps, step.Text)
				allGherkinSteps = append(allGherkinSteps, step.Variants...)
				if !stepCovered(allStepTexts, step) {
					stepGaps = append(stepGaps, StepGap{
						SpecFile:      relSpec,
						ScenarioTitle: sc.Title,
						StepKeyword:   step.Keyword,
						StepText:      step.Text,
					})
				}
			}
		}
	}

	return &CheckResult{
		TotalSpecs:      len(specFiles),
		TotalScenarios:  totalScenarios,
		TotalSteps:      totalSteps,
		StepGaps:        stepGaps,
		OrphanStepImpls: checkOrphanStepImpls(allStepTexts, allGherkinSteps, opts.RepoRoot),
		Duration:        time.Since(start),
	}, nil
}

// checkOneToOne performs the original 1:1 file matching + scenario + step validation.
func checkOneToOne(opts ScanOptions) (*CheckResult, error) {
	start := time.Now()

	specFiles, err := collectFeatureFiles(opts)
	if err != nil {
		return nil, err
	}

	// Collect all step texts from all TS/JS files once before the loop.
	allStepTexts, err := extractAllStepTexts(opts.AppDir)
	if err != nil {
		return nil, err
	}

	var gaps []CoverageGap
	var scenarioGaps []ScenarioGap
	var stepGaps []StepGap
	var allGherkinSteps []string
	totalScenarios := 0
	totalSteps := 0

	for _, specFile := range specFiles {
		stem := strings.TrimSuffix(filepath.Base(specFile), ".feature")

		testFilePaths, err := findAllMatchingTestFiles(opts.AppDir, stem)
		if err != nil {
			return nil, err
		}

		if len(testFilePaths) == 0 {
			relPath, err := filepath.Rel(opts.RepoRoot, specFile)
			if err != nil {
				relPath = specFile
			}
			gaps = append(gaps, CoverageGap{
				SpecFile: relPath,
				Stem:     stem,
			})
			// Phase 5B (Fix #15): the reverse-direction check still wants every
			// Gherkin step text from the entire spec tree, including files with
			// no matching test impl. Collect them, then skip the forward
			// scenario/step gap check below.
			scenarios, parseErr := ParseFeatureFile(specFile)
			if parseErr != nil {
				return nil, parseErr
			}
			for _, sc := range scenarios {
				for _, step := range sc.Steps {
					allGherkinSteps = append(allGherkinSteps, step.Text)
				}
			}
			continue // skip scenario/step check — no test file to check against
		}

		relSpec, err := filepath.Rel(opts.RepoRoot, specFile)
		if err != nil {
			relSpec = specFile
		}

		scenarios, err := ParseFeatureFile(specFile)
		if err != nil {
			return nil, err
		}

		// Union scenario titles across ALL matching test files. Per Fix #6, a
		// feature's scenarios may legitimately be split across sibling test files
		// (e.g. feature.test.tsx + feature.extra.test.tsx) for readability or
		// test-suite organisation; consulting only the first match would falsely
		// report scenarios defined in the other files as gaps.
		scenarioTitles := map[string]bool{}
		for _, testFilePath := range testFilePaths {
			titles, err := extractScenarioTitles(testFilePath)
			if err != nil {
				return nil, err
			}
			for title := range titles {
				scenarioTitles[title] = true
			}
		}

		for _, sc := range scenarios {
			totalScenarios++
			normalizedTitle := normalizeWS(sc.Title)
			if !scenarioTitles[normalizedTitle] {
				scenarioGaps = append(scenarioGaps, ScenarioGap{
					SpecFile:      relSpec,
					ScenarioTitle: sc.Title,
				})
			}

			for _, step := range sc.Steps {
				totalSteps++
				allGherkinSteps = append(allGherkinSteps, step.Text)
				allGherkinSteps = append(allGherkinSteps, step.Variants...)
				if !stepCovered(allStepTexts, step) {
					stepGaps = append(stepGaps, StepGap{
						SpecFile:      relSpec,
						ScenarioTitle: sc.Title,
						StepKeyword:   step.Keyword,
						StepText:      step.Text,
					})
				}
			}
		}
	}

	return &CheckResult{
		TotalSpecs:      len(specFiles),
		TotalScenarios:  totalScenarios,
		TotalSteps:      totalSteps,
		Gaps:            gaps,
		ScenarioGaps:    scenarioGaps,
		StepGaps:        stepGaps,
		OrphanStepImpls: checkOrphanStepImpls(allStepTexts, allGherkinSteps, opts.RepoRoot),
		Duration:        time.Since(start),
	}, nil
}

// checkOrphanStepImpls runs the reverse-direction check (Phase 5B / Fix #15):
// for each step impl entry in the matcher pool, search the union of all
// Gherkin step texts for any match. An entry with no Gherkin step matching it
// is an "orphan" — a step implementation that no scenario references.
//
// repoRoot, when non-empty, is used to convert each entry's absolute origin
// file path into a repo-relative path; if the conversion fails, the absolute
// path is reported instead.
//
// Naive O(N×M) loop. Typical project has under 500 step impls and under 1000
// Gherkin steps; if profiling later surfaces a hotspot, switch exacts to a
// map[string]bool and patterns to a single regex union pass.
func checkOrphanStepImpls(sm *stepMatcher, allGherkinSteps []string, repoRoot string) []OrphanStepImpl {
	if sm == nil || len(sm.entries) == 0 {
		return nil
	}

	// Pre-normalize Gherkin step texts once so the inner loops compare apples
	// to apples without re-normalizing each candidate.
	normalized := make([]string, len(allGherkinSteps))
	for i, gs := range allGherkinSteps {
		normalized[i] = normalizeWS(gs)
	}

	var orphans []OrphanStepImpl
	for _, e := range sm.entries {
		matched := false
		switch e.Kind {
		case "exact":
			for _, gs := range normalized {
				if gs == e.ExactText {
					matched = true
					break
				}
			}
		case "pattern":
			for _, gs := range normalized {
				if e.Pattern.MatchString(gs) {
					matched = true
					break
				}
			}
		}
		if matched {
			continue
		}
		text := e.ExactText
		if e.Kind == "pattern" {
			text = e.PatternText
		}
		filePath := e.File
		if repoRoot != "" && filepath.IsAbs(filePath) {
			if rel, err := filepath.Rel(repoRoot, filePath); err == nil {
				filePath = rel
			}
		}
		orphans = append(orphans, OrphanStepImpl{
			File:        filePath,
			MatcherKind: e.Kind,
			MatcherText: text,
		})
	}
	return orphans
}

// normalizeWS collapses internal whitespace so matching is whitespace-insensitive.
func normalizeWS(s string) string {
	return strings.Join(strings.Fields(s), " ")
}

// walkFeatureFiles returns all .feature files under dir recursively,
// excluding directories whose names appear in excludeDirs.
func walkFeatureFiles(dir string, excludeDirs ...string) ([]string, error) {
	var files []string

	if _, err := os.Stat(dir); os.IsNotExist(err) {
		return nil, nil
	}

	excludeSet := make(map[string]bool, len(excludeDirs))
	for _, d := range excludeDirs {
		excludeSet[d] = true
	}

	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() && excludeSet[info.Name()] {
			return filepath.SkipDir
		}
		if !info.IsDir() && strings.HasSuffix(path, ".feature") {
			files = append(files, path)
		}
		return nil
	})
	if err != nil {
		return nil, err
	}

	return files, nil
}

// toPascalCase converts a kebab-case stem to PascalCase.
// e.g., "health-check" → "HealthCheck"
func toPascalCase(stem string) string {
	parts := strings.Split(stem, "-")
	var b strings.Builder
	for _, p := range parts {
		if p == "" {
			continue
		}
		b.WriteString(strings.ToUpper(p[:1]) + p[1:])
	}
	return b.String()
}

// matchesStem checks if a file's base name matches a feature file stem.
// Supports kebab-case, snake_case, PascalCase, and test_ prefix matching.
func matchesStem(base, stem string) bool {
	snake := strings.ReplaceAll(stem, "-", "_")
	pascal := toPascalCase(stem)
	testSnake := "test_" + snake

	for _, prefix := range []string{
		stem + ".", stem + "_",
		snake + ".", snake + "_",
		pascal,
		testSnake + ".", testSnake + "_",
	} {
		if strings.HasPrefix(base, prefix) {
			return true
		}
	}
	return base == stem || base == snake
}

// isTestFile checks if a file is a test file based on its path and extension.
func isTestFile(path string) bool {
	base := filepath.Base(path)
	ext := filepath.Ext(base)

	switch ext {
	case "":
		// Files with no extension are accepted (backward compat for exact stem match)
		return true
	case ".go":
		return strings.HasSuffix(base, "_test.go")
	case ".ts", ".tsx", ".js", ".jsx":
		return strings.Contains(base, ".test.") ||
			strings.Contains(base, ".spec.") ||
			strings.Contains(base, ".steps.") ||
			strings.Contains(base, ".integration.") ||
			strings.Contains(base, "_test.")
	case ".java", ".kt":
		return isInTestDir(path)
	case ".py":
		return strings.HasPrefix(base, "test_") ||
			strings.HasSuffix(base, "_test.py") ||
			isInTestDir(path)
	case ".exs":
		return strings.HasSuffix(base, "_test.exs") || strings.HasSuffix(base, "_steps.exs")
	case ".rs":
		return strings.HasSuffix(base, "_test.rs") || isInTestDir(path)
	case ".fs", ".cs":
		return isInTestDir(path) ||
			strings.HasSuffix(base, "Steps.cs") ||
			strings.HasSuffix(base, "Tests.cs") ||
			strings.HasSuffix(base, "Steps.fs") ||
			strings.HasSuffix(base, "Tests.fs")
	case ".clj":
		return strings.HasSuffix(base, "_test.clj") || strings.HasSuffix(base, "_steps.clj")
	case ".dart":
		return strings.HasSuffix(base, "_test.dart") || isInTestDir(path)
	}
	return false
}

// isInTestDir checks if a file is inside a test/ or tests/ or Tests/ directory.
func isInTestDir(path string) bool {
	parts := strings.Split(filepath.ToSlash(path), "/")
	for _, p := range parts {
		if p == "test" || p == "tests" || p == "Tests" {
			return true
		}
	}
	return false
}

// findAllMatchingTestFiles returns the paths of all test files under appDir
// whose base name matches the feature file stem (per matchesStem) and whose
// extension/path satisfies isTestFile. Used by checkOneToOne to union scenario
// titles across sibling test files (Fix #6) — a feature's scenarios may
// legitimately be split across multiple files (e.g. feature.test.tsx and
// feature.extra.test.tsx) for readability or test-suite organisation.
func findAllMatchingTestFiles(appDir, stem string) ([]string, error) {
	if _, err := os.Stat(appDir); os.IsNotExist(err) {
		return nil, nil
	}

	var matches []string

	err := filepath.Walk(appDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			if skipDirs[info.Name()] {
				return filepath.SkipDir
			}
			return nil
		}

		base := filepath.Base(path)
		if matchesStem(base, stem) && isTestFile(path) {
			matches = append(matches, path)
		}
		return nil
	})
	if err != nil {
		return nil, err
	}

	return matches, nil
}

// findMatchingTestFile returns the path of the first test file under appDir
// matching the feature file stem, or "" if none found. Thin wrapper over
// findAllMatchingTestFiles preserved for backward compatibility with
// hasMatchingTestFile and existing unit tests; checkOneToOne calls
// findAllMatchingTestFiles directly.
func findMatchingTestFile(appDir, stem string) (string, error) {
	matches, err := findAllMatchingTestFiles(appDir, stem)
	if err != nil {
		return "", err
	}
	if len(matches) == 0 {
		return "", nil
	}
	return matches[0], nil
}

// hasMatchingTestFile returns true if any file under appDir has a base name
// that starts with stem+"." or equals stem exactly.
// Kept for backward compat with existing tests.
func hasMatchingTestFile(appDir, stem string) (bool, error) {
	path, err := findMatchingTestFile(appDir, stem)
	return path != "", err
}

// extractScenarioTitles reads ONLY the matching test file and returns
// all scenario titles found, whitespace-normalised.
// Dispatches by file extension to the appropriate extractor.
func extractScenarioTitles(testFilePath string) (map[string]bool, error) {
	ext := filepath.Ext(testFilePath)
	switch ext {
	case ".go":
		return extractGoScenarioTitles(testFilePath)
	case ".java", ".kt", ".cs", ".rs":
		// Uses // Scenario: comment pattern (same as Go)
		return extractGoScenarioTitles(testFilePath)
	case ".py":
		return extractPythonScenarioTitles(testFilePath)
	case ".exs", ".fs", ".clj":
		// Auto-bind frameworks — skip scenario extraction
		return map[string]bool{}, nil
	case ".dart":
		// Use // Scenario: comment pattern
		return extractGoScenarioTitles(testFilePath)
	default:
		return extractTSScenarioTitles(testFilePath)
	}
}

// extractTSScenarioTitles reads a TS/JS test file and returns all scenario
// titles found in Scenario("...", ...) calls (whitespace-normalised).
func extractTSScenarioTitles(testFilePath string) (map[string]bool, error) {
	result := map[string]bool{}

	f, err := os.Open(testFilePath)
	if err != nil {
		return nil, err
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		matches := scenarioDefRe.FindAllStringSubmatch(line, -1)
		for _, m := range matches {
			title := unescapeString(firstNonEmpty(m[1], m[2]))
			result[normalizeWS(title)] = true
		}
	}

	return result, scanner.Err()
}

// extractGoScenarioTitles reads a Go test file and returns all scenario titles
// found in // Scenario: ... comments (whitespace-normalised).
func extractGoScenarioTitles(testFilePath string) (map[string]bool, error) {
	result := map[string]bool{}

	f, err := os.Open(testFilePath)
	if err != nil {
		return nil, err
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		m := goScenarioCommentRe.FindStringSubmatch(line)
		if m != nil {
			result[normalizeWS(m[1])] = true
		}
	}

	return result, scanner.Err()
}

// extractAllStepTexts walks ALL source files under appDir (skipping build
// artifact directories) and returns a stepMatcher holding extracted step
// definitions from every supported language. Each entry carries the origin
// file path for reverse-direction orphan reporting (Phase 5B / Fix #15).
func extractAllStepTexts(appDir string) (*stepMatcher, error) {
	sm := newStepMatcher()

	if _, err := os.Stat(appDir); os.IsNotExist(err) {
		return sm, nil
	}

	err := filepath.Walk(appDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			if skipDirs[info.Name()] {
				return filepath.SkipDir
			}
			return nil
		}

		ext := filepath.Ext(path)
		switch ext {
		case ".ts", ".tsx", ".js", ".jsx":
			return extractTSStepTexts(path, sm)
		case ".go":
			return extractGoStepTexts(path, sm)
		case ".java", ".kt":
			return extractJVMStepTexts(path, sm)
		case ".py":
			return extractPythonStepTexts(path, sm)
		case ".ex", ".exs":
			return extractElixirStepTexts(path, sm)
		case ".rs":
			return extractRustStepTexts(path, sm)
		case ".cs":
			return extractCSharpStepTexts(path, sm)
		case ".fs":
			return extractFSharpStepTexts(path, sm)
		case ".clj":
			return extractClojureStepTexts(path, sm)
		case ".dart":
			return extractDartStepTexts(path, sm)
		}
		return nil
	})

	return sm, err
}

// extractTSStepTexts reads a TS/JS file and adds all step texts found in
// Given/When/Then/And/But("...", ...) calls and regex literals Given(/^pattern$/, ...) into the matcher.
// Reads entire file content to handle multi-line step definitions like Then(\n  "text",\n  fn).
// Comments (// and /* */) are stripped before regex matching so commented-out
// placeholder step definitions are not extracted as real impls.
func extractTSStepTexts(path string, sm *stepMatcher) error {
	content, err := os.ReadFile(path)
	if err != nil {
		return err
	}

	src := stripJSComments(string(content))

	// String-style step definitions: Given("text", ...)
	matches := stepDefRe.FindAllStringSubmatch(src, -1)
	for _, m := range matches {
		text := unescapeString(firstNonEmpty(m[1], m[2]))
		addStepToMatcherWithOrigin(sm, text, path)
	}

	// Regex-literal step definitions: Given(/^pattern$/, ...)
	regexMatches := tsRegexStepRe.FindAllStringSubmatch(src, -1)
	for _, m := range regexMatches {
		pattern := m[1]
		re, err := regexp.Compile(pattern)
		if err != nil {
			continue
		}
		sm.addPatternWithOrigin(re, pattern, path)
	}

	return nil
}

// stripJSComments removes JavaScript/TypeScript comments from source so
// commented-out step definitions are not extracted as orphan impls. The
// scanner handles:
//   - Block comments `/* ... */` anywhere (multi-line aware)
//   - Line comments `// ...` only when they appear after only whitespace on
//     the line — restricting line-comment stripping prevents collisions with
//     regex literals like `Given(/foo\/bar/, ...)` whose first slash would
//     otherwise be misread as a comment start.
//   - Preserves string and template literals so quoted `//` inside strings is
//     retained verbatim.
func stripJSComments(src string) string {
	var out strings.Builder
	out.Grow(len(src))
	n := len(src)
	i := 0
	atLineStart := true // becomes false after non-whitespace on the line
	for i < n {
		c := src[i]
		if c == '\n' {
			out.WriteByte('\n')
			i++
			atLineStart = true
			continue
		}
		// Block comment — anywhere.
		if c == '/' && i+1 < n && src[i+1] == '*' {
			j := i + 2
			for j+1 < n && (src[j] != '*' || src[j+1] != '/') {
				if src[j] == '\n' {
					out.WriteByte('\n')
				}
				j++
			}
			i = j + 2
			continue
		}
		// Line comment — only at line start (after optional whitespace).
		if atLineStart && c == '/' && i+1 < n && src[i+1] == '/' {
			j := i + 2
			for j < n && src[j] != '\n' {
				j++
			}
			i = j
			continue
		}
		// String literals — copy through verbatim.
		if c == '"' || c == '\'' || c == '`' {
			quote := c
			out.WriteByte(c)
			i++
			for i < n {
				if src[i] == '\\' && i+1 < n {
					out.WriteByte(src[i])
					out.WriteByte(src[i+1])
					i += 2
					continue
				}
				out.WriteByte(src[i])
				if src[i] == quote {
					i++
					break
				}
				i++
			}
			atLineStart = false
			continue
		}
		out.WriteByte(c)
		if c != ' ' && c != '\t' {
			atLineStart = false
		}
		i++
	}
	return out.String()
}

// extractGoStepTexts reads a Go file and adds compiled regex patterns found in
// sc.Step(`...`, fn) calls. Invalid regex patterns are skipped.
func extractGoStepTexts(path string, sm *stepMatcher) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		matches := goStepRe.FindAllStringSubmatch(line, -1)
		for _, m := range matches {
			pattern := m[1]
			re, err := regexp.Compile(pattern)
			if err != nil {
				// Skip invalid patterns gracefully.
				continue
			}
			sm.addPatternWithOrigin(re, pattern, path)
		}
	}
	return scanner.Err()
}

// firstNonEmpty returns the first non-empty string from the arguments.
func firstNonEmpty(a, b string) string {
	if a != "" {
		return a
	}
	return b
}

// unescapeString processes common JavaScript/TypeScript string escape sequences
// so that extracted step texts match the runtime string values in feature files.
// Handles: \' \" \\ \/ \n \t \r
func unescapeString(s string) string {
	var buf strings.Builder
	buf.Grow(len(s))
	i := 0
	for i < len(s) {
		if s[i] == '\\' && i+1 < len(s) {
			switch s[i+1] {
			case '\'':
				buf.WriteByte('\'')
			case '"':
				buf.WriteByte('"')
			case '\\':
				buf.WriteByte('\\')
			case 'n':
				buf.WriteByte('\n')
			case 't':
				buf.WriteByte('\t')
			case 'r':
				buf.WriteByte('\r')
			case '/':
				buf.WriteByte('/')
			default:
				buf.WriteByte(s[i])
				buf.WriteByte(s[i+1])
			}
			i += 2
		} else {
			buf.WriteByte(s[i])
			i++
		}
	}
	return buf.String()
}
