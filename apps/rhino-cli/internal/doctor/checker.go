package doctor

import (
	"bytes"
	"encoding/json"
	"encoding/xml"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// packageJSONVolta holds the parts of package.json we care about.
type packageJSONVolta struct {
	Volta struct {
		Node string `json:"node"`
		NPM  string `json:"npm"`
	} `json:"volta"`
}

// pomProject holds the minimal pom.xml structure we need.
type pomProject struct {
	XMLName    xml.Name `xml:"project"`
	Properties struct {
		JavaVersion string `xml:"java.version"`
	} `xml:"properties"`
}

// readNodeVersion reads the required Node.js version from package.json.
func readNodeVersion(packageJSONPath string) (string, error) {
	data, err := os.ReadFile(packageJSONPath)
	if err != nil {
		return "", err
	}
	var pkg packageJSONVolta
	if err := json.Unmarshal(data, &pkg); err != nil {
		return "", err
	}
	return pkg.Volta.Node, nil
}

// readNpmVersion reads the required npm version from package.json.
func readNpmVersion(packageJSONPath string) (string, error) {
	data, err := os.ReadFile(packageJSONPath)
	if err != nil {
		return "", err
	}
	var pkg packageJSONVolta
	if err := json.Unmarshal(data, &pkg); err != nil {
		return "", err
	}
	return pkg.Volta.NPM, nil
}

// readJavaVersion reads the required Java version from pom.xml.
func readJavaVersion(pomXMLPath string) (string, error) {
	data, err := os.ReadFile(pomXMLPath)
	if err != nil {
		return "", err
	}
	var pom pomProject
	if err := xml.Unmarshal(data, &pom); err != nil {
		return "", err
	}
	return pom.Properties.JavaVersion, nil
}

// readGoVersion reads the required Go version from the "go X.Y" directive in go.mod.
func readGoVersion(goModPath string) (string, error) {
	data, err := os.ReadFile(goModPath)
	if err != nil {
		return "", err
	}
	for _, line := range strings.Split(string(data), "\n") {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "go ") {
			parts := strings.Fields(line)
			if len(parts) >= 2 {
				return parts[1], nil
			}
		}
	}
	return "", fmt.Errorf("go directive not found in go.mod")
}

// normalizeSimpleVersion strips a leading "v" from a version string.
func normalizeSimpleVersion(s string) string {
	return strings.TrimPrefix(s, "v")
}

// parseJavaVersion extracts the Java major version from java -version stderr output.
// Handles both old-style ("1.8.0_292") and new-style ("21.0.1" or "25") versioning.
func parseJavaVersion(stderr string) string {
	for _, line := range strings.Split(stderr, "\n") {
		if strings.Contains(line, "version") {
			start := strings.Index(line, "\"")
			end := strings.LastIndex(line, "\"")
			if start != -1 && end != -1 && start != end {
				version := line[start+1 : end]
				parts := strings.Split(version, ".")
				if len(parts) > 0 && parts[0] != "" {
					if parts[0] == "1" && len(parts) > 1 {
						// Old Java naming: 1.8 = Java 8
						return parts[1]
					}
					return parts[0]
				}
			}
		}
	}
	return ""
}

// parseMavenVersion extracts the Maven version from mvn --version stdout output.
func parseMavenVersion(stdout string) string {
	for _, line := range strings.Split(stdout, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "Apache Maven ") {
			parts := strings.Fields(trimmed)
			for i, part := range parts {
				if part == "Maven" && i+1 < len(parts) {
					return parts[i+1]
				}
			}
		}
	}
	return ""
}

// parseGoVersion extracts the Go version from go version stdout output.
func parseGoVersion(stdout string) string {
	for _, line := range strings.Split(stdout, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "go version go") {
			parts := strings.Fields(trimmed)
			if len(parts) >= 3 {
				return strings.TrimPrefix(parts[2], "go")
			}
		}
	}
	return ""
}

// compareExact compares installed vs required versions exactly (after normalization).
// Empty required means no requirement — always OK.
func compareExact(installed, required string) (ToolStatus, string) {
	if required == "" {
		return StatusOK, "no version requirement"
	}
	inst := normalizeSimpleVersion(installed)
	req := normalizeSimpleVersion(required)
	if inst == req {
		return StatusOK, fmt.Sprintf("required: %s", required)
	}
	return StatusWarning, fmt.Sprintf("required: %s, version mismatch", required)
}

// compareMajor compares only the major version component (used for Java).
// Empty required means no requirement — always OK.
func compareMajor(installed, required string) (ToolStatus, string) {
	if required == "" {
		return StatusOK, "no version requirement"
	}
	inst := normalizeSimpleVersion(installed)
	req := normalizeSimpleVersion(required)
	instMajor := strings.SplitN(inst, ".", 2)[0]
	reqMajor := strings.SplitN(req, ".", 2)[0]
	if instMajor != "" && instMajor == reqMajor {
		return StatusOK, fmt.Sprintf("required: %s", required)
	}
	return StatusWarning, fmt.Sprintf("required: %s, version mismatch", required)
}

// parseVersionParts splits a version string into [major, minor, patch] integers.
// Missing components default to 0. Returns ok=false if any part is non-numeric.
func parseVersionParts(s string) (major, minor, patch int, ok bool) {
	s = normalizeSimpleVersion(s)
	parts := strings.SplitN(s, ".", 3)
	var nums [3]int
	for i, p := range parts {
		n, err := strconv.Atoi(p)
		if err != nil {
			return 0, 0, 0, false
		}
		nums[i] = n
	}
	return nums[0], nums[1], nums[2], true
}

// compareGTE checks that installed >= required (used for backward-compatible tools like Go).
// Empty required means no requirement — always OK.
func compareGTE(installed, required string) (ToolStatus, string) {
	if required == "" {
		return StatusOK, "no version requirement"
	}
	iMaj, iMin, iPat, iOk := parseVersionParts(installed)
	rMaj, rMin, rPat, rOk := parseVersionParts(required)
	if !iOk || !rOk {
		// Fall back to exact comparison if parsing fails
		return compareExact(installed, required)
	}
	if iMaj > rMaj ||
		(iMaj == rMaj && iMin > rMin) ||
		(iMaj == rMaj && iMin == rMin && iPat >= rPat) {
		return StatusOK, fmt.Sprintf("required: \u2265%s", required)
	}
	return StatusWarning, fmt.Sprintf("required: \u2265%s, version too old", required)
}

// toolDef describes how to check a single tool: what to run, how to parse the output,
// how to compare versions, and where to read the required version from.
type toolDef struct {
	name      string
	binary    string
	source    string
	args      []string
	useStderr bool // true when the version info is on stderr (e.g. java -version)
	parseVer  func(output string) string
	compare   func(installed, required string) (ToolStatus, string)
	readReq   func() string // returns "" when there is no requirement
}

// parseTrimVersion normalizes output where the version string is the whole output
// (e.g. volta --version → "2.0.2\n", node --version → "v24.11.1\n").
func parseTrimVersion(s string) string {
	return normalizeSimpleVersion(strings.TrimSpace(s))
}

// buildToolDefs returns the ordered list of tools to check for the given repo root.
// Adding a new tool only requires a new entry here.
func buildToolDefs(repoRoot string) []toolDef {
	packageJSONPath := filepath.Join(repoRoot, "package.json")
	pomXMLPath := filepath.Join(repoRoot, "apps", "organiclever-be", "pom.xml")
	goModPath := filepath.Join(repoRoot, "apps", "rhino-cli", "go.mod")

	noReq := func() string { return "" }

	return []toolDef{
		{
			name:     "volta",
			binary:   "volta",
			source:   "(no config file)",
			args:     []string{"--version"},
			parseVer: parseTrimVersion,
			compare:  compareExact,
			readReq:  noReq,
		},
		{
			name:     "node",
			binary:   "node",
			source:   "package.json → volta.node",
			args:     []string{"--version"},
			parseVer: parseTrimVersion,
			compare:  compareExact,
			readReq:  func() string { v, _ := readNodeVersion(packageJSONPath); return v },
		},
		{
			name:     "npm",
			binary:   "npm",
			source:   "package.json → volta.npm",
			args:     []string{"--version"},
			parseVer: parseTrimVersion,
			compare:  compareExact,
			readReq:  func() string { v, _ := readNpmVersion(packageJSONPath); return v },
		},
		{
			name:      "java",
			binary:    "java",
			source:    "apps/organiclever-be/pom.xml → <java.version>",
			args:      []string{"-version"},
			useStderr: true, // java -version writes to stderr, not stdout
			parseVer:  parseJavaVersion,
			compare:   compareMajor,
			readReq:   func() string { v, _ := readJavaVersion(pomXMLPath); return v },
		},
		{
			name:     "maven",
			binary:   "mvn",
			source:   "(no config file)",
			args:     []string{"--version"},
			parseVer: parseMavenVersion,
			compare:  compareExact,
			readReq:  noReq,
		},
		{
			name:     "golang",
			binary:   "go",
			source:   "apps/rhino-cli/go.mod → go directive",
			args:     []string{"version"},
			parseVer: parseGoVersion,
			compare:  compareGTE,
			readReq:  func() string { v, _ := readGoVersion(goModPath); return v },
		},
	}
}

// runOneDef executes a single tool check definition using the provided runner.
func runOneDef(runner CommandRunner, def toolDef) ToolCheck {
	check := ToolCheck{
		Name:            def.name,
		Binary:          def.binary,
		Source:          def.source,
		RequiredVersion: def.readReq(),
	}
	stdout, stderr, _, err := runner(def.binary, def.args...)
	if err != nil {
		check.Status = StatusMissing
		check.Note = "not found in PATH"
		return check
	}
	output := stdout
	if def.useStderr {
		output = stderr
	}
	check.InstalledVersion = def.parseVer(output)
	check.Status, check.Note = def.compare(check.InstalledVersion, check.RequiredVersion)
	return check
}

// realRunner executes a command using os/exec, separating exec errors from non-zero exit codes.
// Returns an error only if the binary is not found in PATH.
func realRunner(name string, args ...string) (stdout, stderr string, exitCode int, err error) {
	if _, lookErr := exec.LookPath(name); lookErr != nil {
		return "", "", -1, fmt.Errorf("binary not found in PATH: %s", name)
	}
	var stdoutBuf, stderrBuf bytes.Buffer
	cmd := exec.Command(name, args...)
	cmd.Stdout = &stdoutBuf
	cmd.Stderr = &stderrBuf
	runErr := cmd.Run()
	stdout = stdoutBuf.String()
	stderr = stderrBuf.String()
	if runErr != nil {
		if exitErr, ok := runErr.(*exec.ExitError); ok {
			// Non-zero exit is not an error — we still have the output
			return stdout, stderr, exitErr.ExitCode(), nil
		}
		return stdout, stderr, -1, runErr
	}
	return stdout, stderr, 0, nil
}

// CheckAll runs all tool checks and returns aggregated results.
// If opts.Runner is nil, the real subprocess runner is used.
func CheckAll(opts CheckOptions) (*DoctorResult, error) {
	start := time.Now()

	runner := opts.Runner
	if runner == nil {
		runner = realRunner
	}

	defs := buildToolDefs(opts.RepoRoot)
	checks := make([]ToolCheck, 0, len(defs))
	for _, def := range defs {
		checks = append(checks, runOneDef(runner, def))
	}

	result := &DoctorResult{
		Checks:   checks,
		Duration: time.Since(start),
	}

	for _, c := range checks {
		switch c.Status {
		case StatusOK:
			result.OKCount++
		case StatusWarning:
			result.WarnCount++
		case StatusMissing:
			result.MissingCount++
		}
	}

	return result, nil
}
