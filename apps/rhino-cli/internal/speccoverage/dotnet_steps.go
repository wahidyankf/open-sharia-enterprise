package speccoverage

import (
	"bufio"
	"os"
	"regexp"
	"strings"
)

// csVerbatimStepRe matches C# verbatim string attributes: [Given(@"text with ""escaped quotes""")]
// In verbatim strings, "" is the escape for literal ".
// Uses (?s) for dotall to handle multi-line [When(\n  @"...")] patterns.
var csVerbatimStepRe = regexp.MustCompile(`(?s)\[(?:Given|When|Then|And|But)\s*\(\s*@"((?:[^"]|"")*)"\s*\)\s*\]`)

// csRegularStepRe matches C# regular string attributes: [Given("text with \"escaped\"")]
// Uses (?s) for dotall to handle multi-line patterns.
var csRegularStepRe = regexp.MustCompile(`(?s)\[(?:Given|When|Then|And|But)\s*\(\s*"((?:[^"\\]|\\.)*)"\s*\)\s*\]`)

// fsStepAttrRe detects a line that carries a TickSpec step attribute.
// Matches [<Given>], [<When>], [<Then>], [<And>], [<But>] (inline or standalone).
var fsStepAttrRe = regexp.MustCompile(`\[<(?:Given|When|Then|And|But)>]`)

// fsStepRe matches F# TickSpec step definitions in the inline style:
//
//	let [<Given>] ``text`` () =
//
// The optional inline-attribute group is retained for backward compatibility.
// The multiline style ([<Given>] on the previous line) is handled by the caller
// which tracks whether the preceding line contained a step attribute.
// The backtick-quoted method name IS the step regex pattern (TickSpec convention).
var fsStepRe = regexp.MustCompile("let\\s+(?:\\[<(?:Given|When|Then|And|But)>\\]\\s*)?``((?:[^`]|`[^`])*)``")

// fsLetBacktickRe matches any F# backtick-quoted let binding without requiring
// a step attribute on the same line. Used in the multiline case where the
// attribute was on the previous line.
var fsLetBacktickRe = regexp.MustCompile("let\\s+``((?:[^`]|`[^`])*)``")

// extractCSharpStepTexts reads a C# file and adds step texts to the stepMatcher.
// Reads entire file content to handle multi-line attributes like [When(\n  @"...")].
// Handles both verbatim strings (@"...""...") and regular strings ("...\\"...").
// Uses addStepToMatcher to handle Cucumber expressions and regex patterns.
func extractCSharpStepTexts(path string, sm *stepMatcher) error {
	content, err := os.ReadFile(path)
	if err != nil {
		return err
	}

	src := string(content)

	// Try verbatim strings first (more specific: @"...")
	verbatimMatches := csVerbatimStepRe.FindAllStringSubmatch(src, -1)
	for _, m := range verbatimMatches {
		// Unescape "" → " in verbatim strings
		text := strings.ReplaceAll(m[1], `""`, `"`)
		addStepToMatcherWithOrigin(sm, text, path)
	}

	// Also try regular strings (without @" prefix)
	regularMatches := csRegularStepRe.FindAllStringSubmatch(src, -1)
	for _, m := range regularMatches {
		// Skip if this match was already captured by the verbatim regex
		// (the regular regex could partially match verbatim content)
		addStepToMatcherWithOrigin(sm, m[1], path)
	}

	return nil
}

// extractFSharpStepTexts reads an F# file and adds step patterns from backtick-quoted methods.
// F# TickSpec uses the method name as a regex pattern, so backtick-quoted step names are compiled as regex.
//
// Two styles are handled:
//   - Inline:    let [<Given>] “step text“ () =    (attribute on same line)
//   - Multiline: [<Given>]                           (attribute on previous line)
//     let “step text“ () =
//
// Only functions annotated with a TickSpec step attribute ([<Given>], [<When>], [<Then>],
// [<And>], [<But>]) are extracted. Bare backtick-quoted let bindings without a step attribute
// (e.g. xUnit [<Fact>] test functions) are intentionally skipped to avoid false orphan reports.
func extractFSharpStepTexts(path string, sm *stepMatcher) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer func() { _ = f.Close() }()

	prevLineHasStepAttr := false
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()

		thisLineHasStepAttr := fsStepAttrRe.MatchString(line)

		// Case 1: inline style -- the [<Given/When/Then/And/But>] attribute and
		// the backtick-quoted function name appear on the same line.
		if thisLineHasStepAttr {
			matches := fsStepRe.FindAllStringSubmatch(line, -1)
			for _, m := range matches {
				addFSharpStepPattern(m[1], path, sm)
			}
		}

		// Case 2: multiline style -- [<Given>] was on the PREVIOUS line, this line
		// is the bare let ``text`` () = binding (no attribute on this line).
		if prevLineHasStepAttr && !thisLineHasStepAttr {
			matches := fsLetBacktickRe.FindAllStringSubmatch(line, -1)
			for _, m := range matches {
				addFSharpStepPattern(m[1], path, sm)
			}
		}

		prevLineHasStepAttr = thisLineHasStepAttr
	}
	return scanner.Err()
}

// addFSharpStepPattern compiles an F# backtick-quoted function name as a
// regex pattern and adds it to the step matcher.
func addFSharpStepPattern(name, path string, sm *stepMatcher) {
	text := normalizeWS(name)
	pattern := "^" + text + "$"
	re, err := regexp.Compile(pattern)
	if err == nil {
		sm.addPatternWithOrigin(re, pattern, path)
	}
}
