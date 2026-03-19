package speccoverage

import (
	"bufio"
	"os"
	"regexp"
)

// csStepRe matches [Given("text")], [Given(@"^regex$")] — handles optional @ verbatim string prefix
var csStepRe = regexp.MustCompile(`\[(?:Given|When|Then|And|But)\s*\(\s*@?"((?:[^"\\]|\\.)*)"\s*\)\s*\]`)

// fsStepRe matches let [<Given>] ``text`` () — allows optional whitespace between ] and backticks
var fsStepRe = regexp.MustCompile("\\[<(?:Given|When|Then)>\\]\\s*``((?:[^`]|`[^`])*)``")

// extractCSharpStepTexts reads a C# file and adds step texts to sm.exact.
func extractCSharpStepTexts(path string, sm *stepMatcher) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		matches := csStepRe.FindAllStringSubmatch(line, -1)
		for _, m := range matches {
			sm.exact[normalizeWS(m[1])] = true
		}
	}
	return scanner.Err()
}

// extractFSharpStepTexts reads an F# file and adds step texts from backtick-quoted methods.
func extractFSharpStepTexts(path string, sm *stepMatcher) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		matches := fsStepRe.FindAllStringSubmatch(line, -1)
		for _, m := range matches {
			sm.exact[normalizeWS(m[1])] = true
		}
	}
	return scanner.Err()
}
