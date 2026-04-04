package doctor

import (
	"bytes"
	"encoding/json"
	"encoding/xml"
	"fmt"
	"os"
	"os/exec"
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

// parseLineWord returns the wordIdx-th space-separated token from the first
// line that starts with linePrefix (after trimming whitespace). If tokenPrefix
// is non-empty, it is stripped from the matched token. Returns "" when no
// matching line exists or the line has fewer tokens than wordIdx+1.
func parseLineWord(output, linePrefix string, wordIdx int, tokenPrefix string) string {
	for _, line := range strings.Split(output, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, linePrefix) {
			parts := strings.Fields(trimmed)
			if wordIdx < len(parts) {
				return strings.TrimPrefix(parts[wordIdx], tokenPrefix)
			}
		}
	}
	return ""
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

// compareMajorGTE checks that the major version of installed >= major version of required.
// Used for tools where only major version compatibility matters (e.g., erlang, dotnet with rollForward).
// Empty required means no requirement — always OK.
func compareMajorGTE(installed, required string) (ToolStatus, string) {
	if required == "" {
		return StatusOK, "no version requirement"
	}
	inst := normalizeSimpleVersion(installed)
	req := normalizeSimpleVersion(required)
	instMajor := strings.SplitN(inst, ".", 2)[0]
	reqMajor := strings.SplitN(req, ".", 2)[0]
	iMaj, iErr := strconv.Atoi(instMajor)
	rMaj, rErr := strconv.Atoi(reqMajor)
	if iErr != nil || rErr != nil {
		return compareExact(installed, required)
	}
	if iMaj >= rMaj {
		return StatusOK, fmt.Sprintf("required: \u2265%s (major)", required)
	}
	return StatusWarning, fmt.Sprintf("required: \u2265%s (major), version too old", required)
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

// --- Reader functions for new tool version sources ---

// readPythonVersion reads the required Python version from a .python-version file.
func readPythonVersion(pythonVersionPath string) (string, error) {
	data, err := os.ReadFile(pythonVersionPath)
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(data)), nil
}

// readToolVersionsEntry reads a specific tool version from a .tool-versions file.
func readToolVersionsEntry(toolVersionsPath, toolName string) (string, error) {
	data, err := os.ReadFile(toolVersionsPath)
	if err != nil {
		return "", err
	}
	for _, line := range strings.Split(string(data), "\n") {
		line = strings.TrimSpace(line)
		parts := strings.Fields(line)
		if len(parts) >= 2 && parts[0] == toolName {
			return parts[1], nil
		}
	}
	return "", fmt.Errorf("%s not found in .tool-versions", toolName)
}

// globalJSON holds the parts of global.json we care about for .NET SDK version.
type globalJSON struct {
	SDK struct {
		Version string `json:"version"`
	} `json:"sdk"`
}

// readDotnetVersion reads the required .NET SDK version from global.json.
func readDotnetVersion(globalJSONPath string) (string, error) {
	data, err := os.ReadFile(globalJSONPath)
	if err != nil {
		return "", err
	}
	var g globalJSON
	if err := json.Unmarshal(data, &g); err != nil {
		return "", err
	}
	return g.SDK.Version, nil
}

// readDartSDKVersion reads the Dart SDK version from pubspec.yaml environment.sdk field.
// Strips leading "^" or ">=" constraint prefixes.
func readDartSDKVersion(pubspecPath string) (string, error) {
	data, err := os.ReadFile(pubspecPath)
	if err != nil {
		return "", err
	}
	inEnv := false
	for _, line := range strings.Split(string(data), "\n") {
		trimmed := strings.TrimSpace(line)
		if trimmed == "environment:" {
			inEnv = true
			continue
		}
		if inEnv {
			if !strings.HasPrefix(line, " ") && !strings.HasPrefix(line, "\t") && trimmed != "" {
				break // Left the environment block
			}
			if strings.HasPrefix(trimmed, "sdk:") {
				ver := strings.TrimSpace(strings.TrimPrefix(trimmed, "sdk:"))
				ver = strings.TrimPrefix(ver, "^")
				ver = strings.TrimPrefix(ver, ">=")
				return strings.TrimSpace(ver), nil
			}
		}
	}
	return "", fmt.Errorf("environment.sdk not found in pubspec.yaml")
}

// --- Parser functions for new tool version outputs ---

// parsePythonVersion extracts the version from "Python 3.13.1" output.
func parsePythonVersion(output string) string {
	return parseLineWord(output, "Python ", 1, "")
}

// parseRustVersion extracts the version from "rustc 1.94.0 (hash date)" output.
func parseRustVersion(output string) string {
	return parseLineWord(output, "rustc ", 1, "")
}

// parseCargoLlvmCov extracts the version from "cargo-llvm-cov 0.8.5" output.
func parseCargoLlvmCov(output string) string {
	return parseLineWord(output, "cargo-llvm-cov ", 1, "")
}

// parseElixirVersion extracts the version from elixir --version multiline output.
// Input: "Erlang/OTP 27 ...\n\nElixir 1.19.5 (compiled with Erlang/OTP 27)"
func parseElixirVersion(output string) string {
	return parseLineWord(output, "Elixir ", 1, "")
}

// parseErlangVersion extracts the OTP major version from erl eval output.
// Input is just the version string like "27".
func parseErlangVersion(output string) string {
	return strings.TrimSpace(output)
}

// parseDotnetVersion extracts version from "dotnet --version" output (e.g., "8.0.401\n").
func parseDotnetVersion(output string) string {
	return strings.TrimSpace(output)
}

// parseClojureVersion extracts version from "Clojure CLI version 1.12.4.1582".
func parseClojureVersion(output string) string {
	return parseLineWord(output, "Clojure CLI version ", 3, "")
}

// parseDartVersion extracts version from "Dart SDK version: 3.11.3 (stable) ...".
func parseDartVersion(output string) string {
	for _, line := range strings.Split(output, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "Dart SDK version:") {
			ver := strings.TrimPrefix(trimmed, "Dart SDK version:")
			ver = strings.TrimSpace(ver)
			// Take the first space-delimited token
			fields := strings.Fields(ver)
			if len(fields) > 0 {
				return fields[0]
			}
		}
	}
	return ""
}

// parseFlutterVersion extracts version from "Flutter 3.41.5 • channel stable ...".
func parseFlutterVersion(output string) string {
	return parseLineWord(output, "Flutter ", 1, "")
}

// parseDockerVersion extracts version from "Docker version 29.2.1, build ...".
func parseDockerVersion(output string) string {
	for _, line := range strings.Split(output, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "Docker version") {
			fields := strings.Fields(trimmed)
			if len(fields) >= 3 {
				return strings.TrimSuffix(fields[2], ",")
			}
		}
	}
	return ""
}

// parseJqVersion extracts version from "jq-1.8.1" output.
func parseJqVersion(output string) string {
	trimmed := strings.TrimSpace(output)
	return strings.TrimPrefix(trimmed, "jq-")
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
