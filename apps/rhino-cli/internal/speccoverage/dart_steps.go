package speccoverage

import (
	"os"
	"regexp"
)

// dartStepRe matches s.given/when/then/and/but("text", ...) in Dart BDD test files.
// Both double-quoted and single-quoted strings are supported.
// Uses (?s) dotall to handle multi-line calls like s.and(\n  "text",\n  fn).
var dartStepRe = regexp.MustCompile(
	`(?s)\b(?:s|scenario)\.(?:given|when|then|and|but)\s*\(\s*(?:"((?:[^"\\]|\\.)*)"|'((?:[^'\\]|\\.)*)')\s*,`,
)

// extractDartStepTexts reads a Dart file and adds step texts to the stepMatcher.
// Uses addStepToMatcher to handle Cucumber expressions and regex patterns.
// Reads entire file content to handle multi-line step definitions.
func extractDartStepTexts(path string, sm *stepMatcher) error {
	content, err := os.ReadFile(path)
	if err != nil {
		return err
	}

	src := string(content)
	matches := dartStepRe.FindAllStringSubmatch(src, -1)
	for _, m := range matches {
		text := unescapeString(firstNonEmpty(m[1], m[2]))
		addStepToMatcher(sm, text)
	}
	return nil
}
