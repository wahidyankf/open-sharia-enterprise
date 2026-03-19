package speccoverage

import (
	"bufio"
	"os"
	"regexp"
)

// rsStepLiteralRe matches #[given("text")] — plain text step
var rsStepLiteralRe = regexp.MustCompile(`#\[(?:given|when|then)\s*\(\s*"((?:[^"\\]|\\.)*)"\s*\)\s*\]`)

// rsStepExprRe matches #[given(expr = "text")] — expression step (treated as Cucumber expression)
var rsStepExprRe = regexp.MustCompile(`#\[(?:given|when|then)\s*\(\s*expr\s*=\s*"((?:[^"\\]|\\.)*)"\s*\)\s*\]`)

// rsStepRegexRe matches #[given(regex = r#"pattern"#)] — regex step
var rsStepRegexRe = regexp.MustCompile(`#\[(?:given|when|then)\s*\(\s*regex\s*=\s*r#"(.*?)"#\s*\)\s*\]`)

// extractRustStepTexts reads a Rust file and adds step texts/patterns.
// Literal and expr forms are added as exact text.
// Regex forms are compiled as regex patterns.
func extractRustStepTexts(path string, sm *stepMatcher) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()

		// Check regex form first (most specific)
		for _, m := range rsStepRegexRe.FindAllStringSubmatch(line, -1) {
			pattern := m[1]
			re, compErr := regexp.Compile(pattern)
			if compErr != nil {
				continue
			}
			sm.patterns = append(sm.patterns, re)
		}

		// Check expr form
		for _, m := range rsStepExprRe.FindAllStringSubmatch(line, -1) {
			sm.exact[normalizeWS(m[1])] = true
		}

		// Check literal form
		for _, m := range rsStepLiteralRe.FindAllStringSubmatch(line, -1) {
			sm.exact[normalizeWS(m[1])] = true
		}
	}
	return scanner.Err()
}
