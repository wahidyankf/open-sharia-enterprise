package speccoverage

import (
	"bufio"
	"os"
	"regexp"
)

// pyStepRe matches @given("text"), @when("text"), @then("text"), @step("text")
var pyStepRe = regexp.MustCompile(`@(?:given|when|then|step)\s*\(\s*"((?:[^"\\]|\\.)*)"\s*\)`)

// pyScenarioRe matches @scenario("feature.feature", "Title")
var pyScenarioRe = regexp.MustCompile(`@scenario\s*\(\s*"[^"]*"\s*,\s*"((?:[^"\\]|\\.)*)"\s*\)`)

// extractPythonStepTexts reads a Python file and adds step texts to sm.exact.
func extractPythonStepTexts(path string, sm *stepMatcher) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		matches := pyStepRe.FindAllStringSubmatch(line, -1)
		for _, m := range matches {
			sm.exact[normalizeWS(m[1])] = true
		}
	}
	return scanner.Err()
}

// extractPythonScenarioTitles extracts titles from @scenario decorators.
func extractPythonScenarioTitles(testFilePath string) (map[string]bool, error) {
	result := map[string]bool{}

	f, err := os.Open(testFilePath)
	if err != nil {
		return nil, err
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		matches := pyScenarioRe.FindAllStringSubmatch(line, -1)
		for _, m := range matches {
			result[normalizeWS(m[1])] = true
		}
	}

	return result, scanner.Err()
}
