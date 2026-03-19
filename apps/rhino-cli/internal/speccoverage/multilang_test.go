package speccoverage

import (
	"os"
	"path/filepath"
	"testing"
)

func TestMatchesStem(t *testing.T) {
	tests := []struct {
		base, stem string
		want       bool
	}{
		{"health-check.test.ts", "health-check", true},
		{"health_check_test.go", "health-check", true},
		{"HealthCheck.java", "health-check", true},
		{"HealthCheckSteps.java", "health-check", true},
		{"test_health_check.py", "health-check", true},
		{"health_check_test.py", "health-check", true},
		{"other.test.ts", "health-check", false},
		{"health-check", "health-check", true},
		{"health_check", "health-check", true},
	}
	for _, tt := range tests {
		got := matchesStem(tt.base, tt.stem)
		if got != tt.want {
			t.Errorf("matchesStem(%q, %q) = %v, want %v", tt.base, tt.stem, got, tt.want)
		}
	}
}

func TestToPascalCase(t *testing.T) {
	tests := []struct{ in, want string }{
		{"health-check", "HealthCheck"},
		{"user-login", "UserLogin"},
		{"simple", "Simple"},
	}
	for _, tt := range tests {
		got := toPascalCase(tt.in)
		if got != tt.want {
			t.Errorf("toPascalCase(%q) = %q, want %q", tt.in, got, tt.want)
		}
	}
}

func TestIsTestFile(t *testing.T) {
	tests := []struct {
		path string
		want bool
	}{
		{"src/foo_test.go", true},
		{"src/foo.go", false},
		{"src/foo.test.ts", true},
		{"src/foo.ts", false},
		{"src/test/FooTest.java", true},
		{"src/main/Foo.java", false},
		{"src/test_foo.py", true},
		{"src/foo.py", false},
		{"src/foo_test.exs", true},
		{"src/foo.exs", false},
		{"src/foo_test.rs", true},
		{"src/tests/foo.rs", true},
		{"src/FooSteps.cs", true},
		{"src/Foo.cs", false},
		{"src/foo_test.clj", true},
		{"src/foo.clj", false},
		{"src/foo_test.dart", true},
		{"src/test/foo.dart", true},
	}
	for _, tt := range tests {
		got := isTestFile(tt.path)
		if got != tt.want {
			t.Errorf("isTestFile(%q) = %v, want %v", tt.path, got, tt.want)
		}
	}
}

func TestExtractJVMStepTexts(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "Steps.java")
	content := `@Given("the user is logged in")
@When("they click logout")
@Then("they are redirected to login page")`
	os.WriteFile(path, []byte(content), 0644)

	sm := &stepMatcher{exact: map[string]bool{}}
	err := extractJVMStepTexts(path, sm)
	if err != nil {
		t.Fatal(err)
	}
	if !sm.exact["the user is logged in"] {
		t.Error("expected 'the user is logged in' in exact matches")
	}
	if len(sm.exact) != 3 {
		t.Errorf("expected 3 exact matches, got %d", len(sm.exact))
	}
}

func TestExtractPythonStepTexts(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "steps.py")
	content := `@given("the user exists")
@when("they submit the form")
@then("a success message is shown")`
	os.WriteFile(path, []byte(content), 0644)

	sm := &stepMatcher{exact: map[string]bool{}}
	err := extractPythonStepTexts(path, sm)
	if err != nil {
		t.Fatal(err)
	}
	if len(sm.exact) != 3 {
		t.Errorf("expected 3 exact matches, got %d", len(sm.exact))
	}
}

func TestExtractElixirStepTexts(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "steps.exs")
	content := `defgiven ~r/^the system is running$/
defwhen ~r/^the user sends a request$/
defthen ~r/^a response is returned$/`
	os.WriteFile(path, []byte(content), 0644)

	sm := &stepMatcher{exact: map[string]bool{}}
	err := extractElixirStepTexts(path, sm)
	if err != nil {
		t.Fatal(err)
	}
	if len(sm.patterns) != 3 {
		t.Errorf("expected 3 patterns, got %d", len(sm.patterns))
	}
	if !sm.matches("the system is running") {
		t.Error("expected pattern to match 'the system is running'")
	}
}

func TestExtractRustStepTexts(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "steps.rs")
	content := `#[given("the user is logged in")]
fn given_logged_in(world: &mut World) {}

#[when(expr = "they click {string}")]
fn when_click(world: &mut World, button: String) {}

#[then(regex = r#"the response contains "([^"]+)""#)]
fn then_response(world: &mut World) {}
`
	os.WriteFile(path, []byte(content), 0644)

	sm := &stepMatcher{exact: map[string]bool{}}
	err := extractRustStepTexts(path, sm)
	if err != nil {
		t.Fatal(err)
	}
	// 2 exact (literal + expr) + 1 pattern (regex)
	if len(sm.exact) != 2 {
		t.Errorf("expected 2 exact matches, got %d: %v", len(sm.exact), sm.exact)
	}
	if len(sm.patterns) != 1 {
		t.Errorf("expected 1 pattern, got %d", len(sm.patterns))
	}
	if !sm.exact["the user is logged in"] {
		t.Error("expected 'the user is logged in' in exact matches")
	}
	if !sm.exact["they click {string}"] {
		t.Error("expected 'they click {string}' in exact matches")
	}
	if len(sm.patterns) > 0 && !sm.patterns[0].MatchString(`the response contains "hello"`) {
		t.Error("expected regex pattern to match")
	}
}

func TestExtractCSharpStepTexts(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "Steps.cs")
	content := `[Given("the app is started")]
[When(@"^the user clicks (.*)$")]
[Then("the result is shown")]`
	os.WriteFile(path, []byte(content), 0644)

	sm := &stepMatcher{exact: map[string]bool{}}
	err := extractCSharpStepTexts(path, sm)
	if err != nil {
		t.Fatal(err)
	}
	if len(sm.exact) != 3 {
		t.Errorf("expected 3 exact matches, got %d", len(sm.exact))
	}
}

func TestExtractClojureStepTexts(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "steps.clj")
	content := `(Given "the server is ready"
  (fn [state] state))
(When "a request is sent"
  (fn [state] state))`
	os.WriteFile(path, []byte(content), 0644)

	sm := &stepMatcher{exact: map[string]bool{}}
	err := extractClojureStepTexts(path, sm)
	if err != nil {
		t.Fatal(err)
	}
	if len(sm.exact) != 2 {
		t.Errorf("expected 2 exact matches, got %d", len(sm.exact))
	}
}

func TestCheckSharedSteps(t *testing.T) {
	root := t.TempDir()

	// Create feature file
	specDir := filepath.Join(root, "specs")
	os.MkdirAll(specDir, 0755)
	featureContent := `Feature: Test
  Scenario: Login
    Given the user is logged in
    When they click logout
    Then they are redirected`
	os.WriteFile(filepath.Join(specDir, "test.feature"), []byte(featureContent), 0644)

	// Create step file matching 2 of 3 steps
	appDir := filepath.Join(root, "app")
	os.MkdirAll(appDir, 0755)
	stepContent := `Given("the user is logged in", async () => {});
When("they click logout", async () => {});`
	os.WriteFile(filepath.Join(appDir, "common.steps.ts"), []byte(stepContent), 0644)

	result, err := checkSharedSteps(ScanOptions{
		RepoRoot:    root,
		SpecsDir:    specDir,
		AppDir:      appDir,
		SharedSteps: true,
	})
	if err != nil {
		t.Fatal(err)
	}
	if result.TotalSteps != 3 {
		t.Errorf("expected 3 total steps, got %d", result.TotalSteps)
	}
	if len(result.StepGaps) != 1 {
		t.Errorf("expected 1 step gap, got %d", len(result.StepGaps))
	}
	if len(result.Gaps) != 0 {
		t.Errorf("expected 0 file gaps in shared-steps mode, got %d", len(result.Gaps))
	}
}

func TestPythonScenarioExtraction(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "test_login.py")
	content := `@scenario("login.feature", "User logs in")
def test_user_logs_in():
    pass

@scenario("login.feature", "User fails login")
def test_user_fails_login():
    pass`
	os.WriteFile(path, []byte(content), 0644)

	titles, err := extractPythonScenarioTitles(path)
	if err != nil {
		t.Fatal(err)
	}
	if len(titles) != 2 {
		t.Errorf("expected 2 scenarios, got %d", len(titles))
	}
	if !titles["User logs in"] {
		t.Error("expected 'User logs in' in titles")
	}
}
