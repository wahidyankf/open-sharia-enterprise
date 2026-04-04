//go:build integration

package cmd

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/cucumber/godog"
)

var specsDoctorDir = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/cli/gherkin")
}()

// Scenario: All required tools are installed and versions match
// Scenario: A required tool is missing from the environment
// Scenario: A tool is installed but its version does not match the requirement
// Scenario: JSON output lists all tool check results

type doctorSteps struct {
	originalWd   string
	tmpDir       string
	originalPath string
	cmdErr       error
	cmdOutput    string
}

func (s *doctorSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "doctor-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0755)
	s.originalPath = os.Getenv("PATH")
	verbose = false
	quiet = false
	output = "text"
	scope = "full"
	fix = false
	dryRun = false
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *doctorSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	if s.originalPath != "" {
		_ = os.Setenv("PATH", s.originalPath)
	}
	return context.Background(), nil
}

// runToolVersion runs a binary with args and returns trimmed stdout or stderr output.
// Returns empty string if the binary is not found.
func runToolVersion(binary string, args ...string) string {
	cmd := exec.Command(binary, args...)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		if _, lookErr := exec.LookPath(binary); lookErr != nil {
			return ""
		}
	}
	return strings.TrimSpace(out.String())
}

// detectNodeVersion returns the installed Node.js version (without the leading "v").
func detectNodeVersion() string {
	raw := runToolVersion("node", "--version")
	return strings.TrimPrefix(raw, "v")
}

// detectNpmVersion returns the installed npm version.
func detectNpmVersion() string {
	return runToolVersion("npm", "--version")
}

// detectJavaMajorVersion returns the installed Java major version (e.g. "21").
func detectJavaMajorVersion() string {
	// java -version writes to stderr
	cmd := exec.Command("java", "-version")
	var errBuf bytes.Buffer
	cmd.Stderr = &errBuf
	_ = cmd.Run()
	raw := errBuf.String()
	for _, line := range strings.Split(raw, "\n") {
		if strings.Contains(line, "version") {
			start := strings.Index(line, "\"")
			end := strings.LastIndex(line, "\"")
			if start != -1 && end != -1 && start != end {
				ver := line[start+1 : end]
				parts := strings.Split(ver, ".")
				if len(parts) > 0 && parts[0] != "" {
					if parts[0] == "1" && len(parts) > 1 {
						return parts[1]
					}
					return parts[0]
				}
			}
		}
	}
	return ""
}

// detectGoVersion returns the installed Go version (e.g. "1.24.2").
func detectGoVersion() string {
	raw := runToolVersion("go", "version")
	// "go version go1.24.2 linux/amd64"
	fields := strings.Fields(raw)
	for _, f := range fields {
		if strings.HasPrefix(f, "go") && len(f) > 2 {
			return strings.TrimPrefix(f, "go")
		}
	}
	return ""
}

// writeDoctorConfigFiles writes all config files into tmpDir using the provided version strings.
func writeDoctorConfigFiles(tmpDir, nodeVer, npmVer, javaMajor, goVer string) error {
	dirs := []string{
		"apps/organiclever-be-jasb",
		"apps/rhino-cli",
		"apps/a-demo-be-python-fastapi",
		"apps/a-demo-be-fsharp-giraffe",
		"apps/a-demo-fe-dart-flutterweb",
		"apps/a-demo-be-rust-axum",
	}
	for _, dir := range dirs {
		if err := os.MkdirAll(filepath.Join(tmpDir, dir), 0755); err != nil {
			return fmt.Errorf("mkdir %s: %w", dir, err)
		}
	}

	files := map[string]string{
		"package.json":                      fmt.Sprintf(`{"name":"test","volta":{"node":%q,"npm":%q}}`, nodeVer, npmVer),
		"apps/organiclever-be-jasb/pom.xml": fmt.Sprintf(`<project><properties><java.version>%s</java.version></properties></project>`, javaMajor),
		"apps/rhino-cli/go.mod":             fmt.Sprintf("module foo\n\ngo %s\n", goVer),
		// Tool config files — use sensible defaults that match common installed versions
		"apps/a-demo-be-python-fastapi/.python-version": "3.13\n",
		".tool-versions": "erlang 27.3\nelixir 1.19.5-otp-27\n",
		"apps/a-demo-be-fsharp-giraffe/global.json":   `{"sdk":{"version":"10.0.103","rollForward":"latestMinor"}}`,
		"apps/a-demo-fe-dart-flutterweb/pubspec.yaml": "name: demo\n\nenvironment:\n  sdk: ^3.11.1\n  flutter: \">=3.41.0\"\n",
		"apps/a-demo-be-rust-axum/Cargo.toml":         "[package]\nname = \"test\"\nrust-version = \"1.80\"\n",
	}
	for relPath, content := range files {
		if err := os.WriteFile(filepath.Join(tmpDir, relPath), []byte(content), 0644); err != nil {
			return fmt.Errorf("write %s: %w", relPath, err)
		}
	}

	return nil
}

func (s *doctorSteps) allRequiredDevelopmentToolsArePresentWithMatchingVersions(t *testing.T) error {
	nodeVer := detectNodeVersion()
	npmVer := detectNpmVersion()
	javaMajor := detectJavaMajorVersion()
	goVer := detectGoVersion()

	if nodeVer == "" || npmVer == "" || javaMajor == "" || goVer == "" {
		t.Skip("one or more required tools not available in PATH — skipping scenario")
	}

	return writeDoctorConfigFiles(s.tmpDir, nodeVer, npmVer, javaMajor, goVer)
}

func (s *doctorSteps) aRequiredDevelopmentToolIsNotFoundInTheSystemPATH() error {
	// Create an empty bin dir so PATH resolution finds nothing
	emptyBin, err := os.MkdirTemp("", "empty-bin-*")
	if err != nil {
		return fmt.Errorf("create empty bin dir: %w", err)
	}
	_ = os.Setenv("PATH", emptyBin)

	// Write config files with dummy versions (tools won't be found anyway)
	return writeDoctorConfigFiles(s.tmpDir, "24.0.0", "11.0.0", "21", "1.24.0")
}

func (s *doctorSteps) aRequiredDevelopmentToolIsInstalledWithANonMatchingVersion() error {
	npmVer := detectNpmVersion()
	if npmVer == "" {
		npmVer = "11.0.0"
	}
	javaMajor := detectJavaMajorVersion()
	if javaMajor == "" {
		javaMajor = "21"
	}
	goVer := detectGoVersion()
	if goVer == "" {
		goVer = "1.24.0"
	}

	// Write all config files first using standard helper (creates dirs too)
	if err := writeDoctorConfigFiles(s.tmpDir, "1.0.0", npmVer, javaMajor, goVer); err != nil {
		return err
	}

	// Override node requirement to "1.0.0" is already done above — guaranteed mismatch
	return nil
}

func (s *doctorSteps) theDeveloperRunsTheDoctorCommand() error {
	buf := new(bytes.Buffer)
	doctorCmd.SetOut(buf)
	doctorCmd.SetErr(buf)
	s.cmdErr = doctorCmd.RunE(doctorCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *doctorSteps) theDeveloperRunsTheDoctorCommandWithJSONOutput() error {
	output = "json"
	buf := new(bytes.Buffer)
	doctorCmd.SetOut(buf)
	doctorCmd.SetErr(buf)
	s.cmdErr = doctorCmd.RunE(doctorCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *doctorSteps) theCommandDoctorExitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected command to succeed but got error: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *doctorSteps) theCommandDoctorExitsWithAFailureCode() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected command to fail but it succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *doctorSteps) theOutputReportsEachToolAsPassing() error {
	if !strings.Contains(s.cmdOutput, "Doctor Report") {
		return fmt.Errorf("expected output to contain 'Doctor Report' but got: %s", s.cmdOutput)
	}
	if strings.Contains(s.cmdOutput, "✗") {
		return fmt.Errorf("expected no missing tools (✗) in output but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *doctorSteps) theOutputIdentifiesTheMissingTool() error {
	if !strings.Contains(s.cmdOutput, "✗") && !strings.Contains(strings.ToLower(s.cmdOutput), "missing") {
		return fmt.Errorf("expected output to identify missing tool (✗ or 'missing') but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *doctorSteps) theOutputReportsTheToolAsAWarningRatherThanAFailure() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected command to exit successfully (warnings don't fail) but got: %v", s.cmdErr)
	}
	if !strings.Contains(s.cmdOutput, "⚠") && !strings.Contains(strings.ToLower(s.cmdOutput), "warning") {
		return fmt.Errorf("expected output to contain warning indicator (⚠ or 'warning') but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *doctorSteps) theOutputIsValidDoctorJSON() error {
	var parsed map[string]any
	if err := json.Unmarshal([]byte(s.cmdOutput), &parsed); err != nil {
		return fmt.Errorf("output is not valid JSON: %v\nOutput: %s", err, s.cmdOutput)
	}
	return nil
}

func (s *doctorSteps) theJSONListsEveryCheckedToolWithItsStatus() error {
	var parsed map[string]any
	if err := json.Unmarshal([]byte(s.cmdOutput), &parsed); err != nil {
		return fmt.Errorf("output is not valid JSON: %v\nOutput: %s", err, s.cmdOutput)
	}
	tools, ok := parsed["tools"].([]any)
	if !ok {
		return fmt.Errorf("expected 'tools' array in JSON but got: %s", s.cmdOutput)
	}
	if len(tools) != 19 {
		return fmt.Errorf("expected 19 tools in JSON output, got %d\nOutput: %s", len(tools), s.cmdOutput)
	}
	return nil
}

// Integration step stubs for new scenarios — these features are tested in depth
// by the unit tests; integration tests only verify the command runs end-to-end.

func (s *doctorSteps) theDeveloperRunsDoctorWithMinimalScope() error {
	scope = "minimal"
	return s.theDeveloperRunsTheDoctorCommand()
}

func (s *doctorSteps) theOutputChecksOnlyTheMinimalToolSet() error {
	if !strings.Contains(s.cmdOutput, "scope: minimal") {
		return fmt.Errorf("expected '(scope: minimal)' in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *doctorSteps) theDeveloperRunsDoctorWithFix() error {
	fix = true
	return s.theDeveloperRunsTheDoctorCommand()
}

func (s *doctorSteps) theDeveloperRunsDoctorWithFixDryRun() error {
	fix = true
	dryRun = true
	return s.theDeveloperRunsTheDoctorCommand()
}

func (s *doctorSteps) theOutputContainsFixProgress() error {
	if !strings.Contains(s.cmdOutput, "Fix summary") && !strings.Contains(s.cmdOutput, "Skip:") && !strings.Contains(s.cmdOutput, "Would install") {
		return fmt.Errorf("expected fix progress in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *doctorSteps) theOutputContainsDryRunPreview() error {
	if !strings.Contains(s.cmdOutput, "Would install") && !strings.Contains(s.cmdOutput, "Skip:") {
		return fmt.Errorf("expected dry-run preview in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *doctorSteps) theOutputReportsNothingToFix() error {
	if !strings.Contains(s.cmdOutput, "Nothing to fix") {
		return fmt.Errorf("expected 'Nothing to fix' in output, got: %s", s.cmdOutput)
	}
	return nil
}

// registerCommonDoctorSteps registers all step definitions shared between
// InitializeDoctorScenario and TestIntegrationDoctor.
func registerCommonDoctorSteps(sc *godog.ScenarioContext, s *doctorSteps) {
	sc.Step(`^a required development tool is not found in the system PATH$`, s.aRequiredDevelopmentToolIsNotFoundInTheSystemPATH)
	sc.Step(`^a required development tool is installed with a non-matching version$`, s.aRequiredDevelopmentToolIsInstalledWithANonMatchingVersion)
	sc.Step(`^the developer runs the doctor command$`, s.theDeveloperRunsTheDoctorCommand)
	sc.Step(`^the developer runs the doctor command with JSON output$`, s.theDeveloperRunsTheDoctorCommandWithJSONOutput)
	sc.Step(`^the developer runs the doctor command with minimal scope$`, s.theDeveloperRunsDoctorWithMinimalScope)
	sc.Step(`^the developer runs the doctor command with the fix flag$`, s.theDeveloperRunsDoctorWithFix)
	sc.Step(`^the developer runs the doctor command with fix and dry-run flags$`, s.theDeveloperRunsDoctorWithFixDryRun)
	sc.Step(`^the command exits successfully$`, s.theCommandDoctorExitsSuccessfully)
	sc.Step(`^the command exits with a failure code$`, s.theCommandDoctorExitsWithAFailureCode)
	sc.Step(`^the output reports each tool as passing$`, s.theOutputReportsEachToolAsPassing)
	sc.Step(`^the output identifies the missing tool$`, s.theOutputIdentifiesTheMissingTool)
	sc.Step(`^the output reports the tool as a warning rather than a failure$`, s.theOutputReportsTheToolAsAWarningRatherThanAFailure)
	sc.Step(`^the output is valid JSON$`, s.theOutputIsValidDoctorJSON)
	sc.Step(`^the JSON lists every checked tool with its status$`, s.theJSONListsEveryCheckedToolWithItsStatus)
	sc.Step(`^the output checks only the minimal tool set$`, s.theOutputChecksOnlyTheMinimalToolSet)
	sc.Step(`^the output contains fix progress$`, s.theOutputContainsFixProgress)
	sc.Step(`^the output contains a dry-run preview$`, s.theOutputContainsDryRunPreview)
	sc.Step(`^the output reports nothing to fix$`, s.theOutputReportsNothingToFix)
}

// InitializeDoctorScenario registers all step definitions.
func InitializeDoctorScenario(sc *godog.ScenarioContext) {
	s := &doctorSteps{}
	sc.Before(s.before)
	sc.After(s.after)

	sc.Step(`^all required development tools are present with matching versions$`,
		func(ctx context.Context) (context.Context, error) {
			return ctx, nil
		})
	registerCommonDoctorSteps(sc, s)
}

func TestIntegrationDoctor(t *testing.T) {
	s := &doctorSteps{}

	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)

			sc.Step(`^all required development tools are present with matching versions$`,
				func() error {
					return s.allRequiredDevelopmentToolsArePresentWithMatchingVersions(t)
				})
			registerCommonDoctorSteps(sc, s)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDoctorDir},
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run feature tests")
	}
}
