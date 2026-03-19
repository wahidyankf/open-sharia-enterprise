package speccoverage

import (
	"bufio"
	"os"
	"regexp"
)

// exStepRe matches defgiven/defwhen/defthen ~r/^text$/
var exStepRe = regexp.MustCompile(`def(?:given|when|then|and_|but_)\s+~r/\^?(.*?)\$?/`)

// extractElixirStepTexts reads an Elixir file and adds step regex patterns to sm.patterns.
func extractElixirStepTexts(path string, sm *stepMatcher) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		matches := exStepRe.FindAllStringSubmatch(line, -1)
		for _, m := range matches {
			pattern := m[1]
			re, err := regexp.Compile(pattern)
			if err != nil {
				continue
			}
			sm.patterns = append(sm.patterns, re)
		}
	}
	return scanner.Err()
}
