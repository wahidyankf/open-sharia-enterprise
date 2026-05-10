package speccoverage

import (
	"bufio"
	"os"
	"strings"
)

// ParsedStep is a single step line from a Gherkin scenario.
type ParsedStep struct {
	Keyword string // Given/When/Then/And/But (Title case)
	Text    string // trimmed step text (unexpanded for outline steps)
	// Variants holds expanded step texts when the step belongs to a Scenario
	// Outline with an Examples table. Empty for plain scenarios. Forward-
	// direction coverage is satisfied if Text OR all Variants match an impl.
	Variants []string
}

// ParsedScenario is a Gherkin Scenario block with its steps.
type ParsedScenario struct {
	Title string
	Steps []ParsedStep
}

var stepKeywords = []string{"Given ", "When ", "Then ", "And ", "But "}

// ParseFeatureFile reads a .feature file and returns all scenarios and their
// steps. Background steps are emitted as a synthetic "(Background)" scenario
// so that spec-coverage validates Background step definitions too.
//
// Scenario Outline steps are emitted with `<placeholder>` tokens intact and
// also carry their expanded variants on `ParsedStep.Variants`. Forward-
// direction coverage treats a step as covered if EITHER the unexpanded text
// OR every variant matches an impl. Reverse-direction reachability is
// expanded via `ExpandedOutlineStepTexts`.
func ParseFeatureFile(path string) ([]ParsedScenario, error) {
	scenarios, _, err := parseFeatureFile(path)
	return scenarios, err
}

// ExpandedOutlineStepTexts returns every expanded Scenario Outline step text
// in the given .feature file (one entry per Examples row × outline step). The
// reverse-direction orphan check uses this to validate impls whose regex
// matchers bind to expanded outline values.
func ExpandedOutlineStepTexts(path string) ([]string, error) {
	_, expanded, err := parseFeatureFile(path)
	return expanded, err
}

func parseFeatureFile(path string) ([]ParsedScenario, []string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, nil, err
	}
	defer func() { _ = f.Close() }()

	var scenarios []ParsedScenario
	var expandedSteps []string
	var current *ParsedScenario
	var bgSteps []ParsedStep
	inBackground := false

	// Outline tracking — we store indices of outline steps within current.Steps
	// so we can populate their Variants[] when Examples rows arrive.
	type outlineState struct {
		stepIndices []int
	}
	var pendingOutline *outlineState
	inExamples := false
	var exHeaders []string

	parseRow := func(line string) []string {
		s := strings.TrimSpace(line)
		s = strings.Trim(s, "|")
		parts := strings.Split(s, "|")
		out := make([]string, 0, len(parts))
		for _, p := range parts {
			out = append(out, strings.TrimSpace(p))
		}
		return out
	}

	expandStep := func(text string, headers, row []string) string {
		out := text
		for i, h := range headers {
			if i >= len(row) {
				break
			}
			out = strings.ReplaceAll(out, "<"+h+">", row[i])
		}
		return out
	}

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())

		if strings.HasPrefix(line, "Background:") {
			inExamples = false
			exHeaders = nil
			pendingOutline = nil
			inBackground = true
			current = nil
			continue
		}

		if rest, ok := strings.CutPrefix(line, "Scenario Outline:"); ok {
			inExamples = false
			exHeaders = nil
			inBackground = false
			title := strings.TrimSpace(rest)
			scenarios = append(scenarios, ParsedScenario{Title: title})
			current = &scenarios[len(scenarios)-1]
			pendingOutline = &outlineState{}
			continue
		}
		if rest, ok := strings.CutPrefix(line, "Scenario:"); ok {
			inExamples = false
			exHeaders = nil
			inBackground = false
			title := strings.TrimSpace(rest)
			scenarios = append(scenarios, ParsedScenario{Title: title})
			current = &scenarios[len(scenarios)-1]
			pendingOutline = nil
			continue
		}

		if strings.HasPrefix(line, "Examples:") {
			inExamples = true
			exHeaders = nil
			continue
		}

		if inExamples && strings.HasPrefix(line, "|") {
			row := parseRow(line)
			if exHeaders == nil {
				exHeaders = row
				continue
			}
			if pendingOutline != nil && current != nil {
				for _, idx := range pendingOutline.stepIndices {
					exp := expandStep(current.Steps[idx].Text, exHeaders, row)
					current.Steps[idx].Variants = append(current.Steps[idx].Variants, exp)
					expandedSteps = append(expandedSteps, exp)
				}
			}
			continue
		}

		for _, kw := range stepKeywords {
			if rest, ok := strings.CutPrefix(line, kw); ok {
				text := strings.TrimSpace(rest)
				step := ParsedStep{
					Keyword: strings.TrimSpace(kw),
					Text:    text,
				}
				if inBackground {
					bgSteps = append(bgSteps, step)
				} else if current != nil {
					current.Steps = append(current.Steps, step)
					if pendingOutline != nil {
						pendingOutline.stepIndices = append(pendingOutline.stepIndices, len(current.Steps)-1)
					}
				}
				break
			}
		}
	}

	if len(bgSteps) > 0 {
		bg := ParsedScenario{Title: "(Background)", Steps: bgSteps}
		scenarios = append([]ParsedScenario{bg}, scenarios...)
	}

	return scenarios, expandedSteps, scanner.Err()
}
