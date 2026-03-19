package speccoverage

import (
	"bufio"
	"os"
	"regexp"
)

// jvmStepRe matches @Given("text"), @When("text"), @Then("text"), @And("text"), @But("text")
var jvmStepRe = regexp.MustCompile(`@(?:Given|When|Then|And|But)\s*\(\s*"((?:[^"\\]|\\.)*)"\s*\)`)

// extractJVMStepTexts reads a Java/Kotlin file and adds step texts to sm.exact.
func extractJVMStepTexts(path string, sm *stepMatcher) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		matches := jvmStepRe.FindAllStringSubmatch(line, -1)
		for _, m := range matches {
			sm.exact[normalizeWS(m[1])] = true
		}
	}
	return scanner.Err()
}
