package speccoverage

import (
	"os"
	"path/filepath"
	"testing"
)

func writeFeatureFile(t *testing.T, dir, name, content string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("WriteFile %s: %v", path, err)
	}
	return path
}

func TestParseFeatureFile_MultipleScenarios(t *testing.T) {
	dir := t.TempDir()
	path := writeFeatureFile(t, dir, "login.feature", `
Feature: User Login

  Scenario: Successful login
    Given a registered user
    When the user submits credentials
    Then the user should be on the dashboard

  Scenario: Failed login
    Given a visitor on the login page
    When the visitor submits wrong credentials
    Then an error is displayed
`)

	scenarios, err := ParseFeatureFile(path)
	if err != nil {
		t.Fatalf("ParseFeatureFile() error = %v", err)
	}

	if len(scenarios) != 2 {
		t.Fatalf("expected 2 scenarios, got %d", len(scenarios))
	}

	if scenarios[0].Title != "Successful login" {
		t.Errorf("scenarios[0].Title = %q, want %q", scenarios[0].Title, "Successful login")
	}
	if len(scenarios[0].Steps) != 3 {
		t.Errorf("scenarios[0].Steps count = %d, want 3", len(scenarios[0].Steps))
	}
	if scenarios[0].Steps[0].Keyword != "Given" {
		t.Errorf("step keyword = %q, want %q", scenarios[0].Steps[0].Keyword, "Given")
	}
	if scenarios[0].Steps[0].Text != "a registered user" {
		t.Errorf("step text = %q, want %q", scenarios[0].Steps[0].Text, "a registered user")
	}

	if scenarios[1].Title != "Failed login" {
		t.Errorf("scenarios[1].Title = %q, want %q", scenarios[1].Title, "Failed login")
	}
	if len(scenarios[1].Steps) != 3 {
		t.Errorf("scenarios[1].Steps count = %d, want 3", len(scenarios[1].Steps))
	}
}

func TestParseFeatureFile_AllStepKeywords(t *testing.T) {
	dir := t.TempDir()
	path := writeFeatureFile(t, dir, "steps.feature", `
Feature: Step Keywords

  Scenario: All keywords
    Given an initial state
    When an action occurs
    Then a result is observed
    And an extra condition
    But not this other thing
`)

	scenarios, err := ParseFeatureFile(path)
	if err != nil {
		t.Fatalf("ParseFeatureFile() error = %v", err)
	}

	if len(scenarios) != 1 {
		t.Fatalf("expected 1 scenario, got %d", len(scenarios))
	}

	steps := scenarios[0].Steps
	if len(steps) != 5 {
		t.Fatalf("expected 5 steps, got %d", len(steps))
	}

	expected := []struct{ kw, text string }{
		{"Given", "an initial state"},
		{"When", "an action occurs"},
		{"Then", "a result is observed"},
		{"And", "an extra condition"},
		{"But", "not this other thing"},
	}
	for i, e := range expected {
		if steps[i].Keyword != e.kw {
			t.Errorf("steps[%d].Keyword = %q, want %q", i, steps[i].Keyword, e.kw)
		}
		if steps[i].Text != e.text {
			t.Errorf("steps[%d].Text = %q, want %q", i, steps[i].Text, e.text)
		}
	}
}

func TestParseFeatureFile_TagsAndNarrativeIgnored(t *testing.T) {
	dir := t.TempDir()
	path := writeFeatureFile(t, dir, "tagged.feature", `
@auth @smoke
Feature: Auth

  As a user
  I want to log in
  So that I can access the app

  @login
  Scenario: Login
    Given I am on the login page
    When I submit valid credentials
    Then I should be redirected
`)

	scenarios, err := ParseFeatureFile(path)
	if err != nil {
		t.Fatalf("ParseFeatureFile() error = %v", err)
	}

	if len(scenarios) != 1 {
		t.Fatalf("expected 1 scenario, got %d", len(scenarios))
	}
	if scenarios[0].Title != "Login" {
		t.Errorf("Title = %q, want %q", scenarios[0].Title, "Login")
	}
	if len(scenarios[0].Steps) != 3 {
		t.Errorf("Steps count = %d, want 3", len(scenarios[0].Steps))
	}
}

func TestParseFeatureFile_EmptyFile(t *testing.T) {
	dir := t.TempDir()
	path := writeFeatureFile(t, dir, "empty.feature", "")

	scenarios, err := ParseFeatureFile(path)
	if err != nil {
		t.Fatalf("ParseFeatureFile() error = %v", err)
	}

	if len(scenarios) != 0 {
		t.Errorf("expected 0 scenarios, got %d", len(scenarios))
	}
}

func TestParseFeatureFile_NoScenarios(t *testing.T) {
	dir := t.TempDir()
	path := writeFeatureFile(t, dir, "nosc.feature", `
Feature: Just a feature

  As a user
  I want nothing
`)

	scenarios, err := ParseFeatureFile(path)
	if err != nil {
		t.Fatalf("ParseFeatureFile() error = %v", err)
	}

	if len(scenarios) != 0 {
		t.Errorf("expected 0 scenarios, got %d", len(scenarios))
	}
}

func TestParseFeatureFile_NonExistentFile(t *testing.T) {
	_, err := ParseFeatureFile("/nonexistent/path/file.feature")
	if err == nil {
		t.Error("expected error for non-existent file, got nil")
	}
}

func TestParseFeatureFile_StepWithEmbeddedQuotes(t *testing.T) {
	dir := t.TempDir()
	path := writeFeatureFile(t, dir, "quotes.feature", `
Feature: Embedded quotes

  Scenario: Login with credentials
    Given a registered user with email "user@example.com" and password "pass123"
    When the user submits the form
    Then the error "Invalid email" should be shown
`)

	scenarios, err := ParseFeatureFile(path)
	if err != nil {
		t.Fatalf("ParseFeatureFile() error = %v", err)
	}

	if len(scenarios) != 1 {
		t.Fatalf("expected 1 scenario, got %d", len(scenarios))
	}
	if len(scenarios[0].Steps) != 3 {
		t.Fatalf("expected 3 steps, got %d", len(scenarios[0].Steps))
	}
	want := `a registered user with email "user@example.com" and password "pass123"`
	if scenarios[0].Steps[0].Text != want {
		t.Errorf("step text = %q, want %q", scenarios[0].Steps[0].Text, want)
	}
}

func TestParseFeatureFile_BackgroundSteps(t *testing.T) {
	dir := t.TempDir()
	path := writeFeatureFile(t, dir, "bg.feature", `
Feature: Background Test

  Background:
    Given the API is running
    And a user "alice" is registered

  Scenario: First scenario
    When alice sends GET /api/v1/users/me
    Then the response status code should be 200

  Scenario: Second scenario
    When alice sends POST /api/v1/logout
    Then the response status code should be 200
`)

	scenarios, err := ParseFeatureFile(path)
	if err != nil {
		t.Fatalf("ParseFeatureFile() error = %v", err)
	}

	// Background creates a synthetic "(Background)" scenario + 2 real scenarios = 3
	if len(scenarios) != 3 {
		t.Fatalf("expected 3 scenarios (1 background + 2 real), got %d", len(scenarios))
	}

	// First should be the synthetic Background scenario
	if scenarios[0].Title != "(Background)" {
		t.Errorf("scenarios[0].Title = %q, want %q", scenarios[0].Title, "(Background)")
	}
	if len(scenarios[0].Steps) != 2 {
		t.Errorf("Background steps count = %d, want 2", len(scenarios[0].Steps))
	}
	if scenarios[0].Steps[0].Text != "the API is running" {
		t.Errorf("Background step[0] = %q, want %q", scenarios[0].Steps[0].Text, "the API is running")
	}

	// Real scenarios follow
	if scenarios[1].Title != "First scenario" {
		t.Errorf("scenarios[1].Title = %q, want %q", scenarios[1].Title, "First scenario")
	}
	if len(scenarios[1].Steps) != 2 {
		t.Errorf("scenarios[1] steps count = %d, want 2", len(scenarios[1].Steps))
	}
}

func TestParseFeatureFile_NoBackground(t *testing.T) {
	dir := t.TempDir()
	path := writeFeatureFile(t, dir, "nobg.feature", `
Feature: No Background

  Scenario: Only scenario
    Given a user exists
    When the user logs in
    Then the user is on the dashboard
`)

	scenarios, err := ParseFeatureFile(path)
	if err != nil {
		t.Fatalf("ParseFeatureFile() error = %v", err)
	}

	// No Background — should NOT have a synthetic scenario
	if len(scenarios) != 1 {
		t.Fatalf("expected 1 scenario (no background), got %d", len(scenarios))
	}
	if scenarios[0].Title != "Only scenario" {
		t.Errorf("Title = %q, want %q", scenarios[0].Title, "Only scenario")
	}
}

// TestParseFeatureFile_ScenarioOutline_BasicSupport — Fix #15 follow-on:
// Scenario Outline headers are recognized and their unexpanded steps appear
// in the returned scenarios.
func TestParseFeatureFile_ScenarioOutline_BasicSupport(t *testing.T) {
	dir := t.TempDir()
	path := writeFeatureFile(t, dir, "outline.feature", `
Feature: Routes

  Scenario Outline: Each tab is reachable
    When the user navigates to "<path>"
    Then the "<screen>" screen is visible

    Examples:
      | path        | screen   |
      | /app/home   | Home     |
      | /app/history | History |
`)

	scenarios, err := ParseFeatureFile(path)
	if err != nil {
		t.Fatalf("ParseFeatureFile() error = %v", err)
	}

	// Outline scenario emitted with `<placeholder>` tokens intact.
	if len(scenarios) != 1 {
		t.Fatalf("expected 1 outline scenario, got %d: %+v", len(scenarios), scenarios)
	}
	if scenarios[0].Title != "Each tab is reachable" {
		t.Errorf("title = %q, want %q", scenarios[0].Title, "Each tab is reachable")
	}
	if len(scenarios[0].Steps) != 2 {
		t.Fatalf("expected 2 steps, got %d", len(scenarios[0].Steps))
	}
	wantStepText := `the user navigates to "<path>"`
	if scenarios[0].Steps[0].Text != wantStepText {
		t.Errorf("step[0].Text = %q, want %q", scenarios[0].Steps[0].Text, wantStepText)
	}
	// Variants populated with one entry per Examples row × outline step.
	if len(scenarios[0].Steps[0].Variants) != 2 {
		t.Errorf("step[0].Variants count = %d, want 2 (one per Examples row)",
			len(scenarios[0].Steps[0].Variants))
	}
}

// TestParseFeatureFile_ScenarioOutline_VariantsExpanded — Fix #15 follow-on:
// each Examples row expands to a Variants entry per outline step.
func TestParseFeatureFile_ScenarioOutline_VariantsExpanded(t *testing.T) {
	dir := t.TempDir()
	path := writeFeatureFile(t, dir, "outline.feature", `
Feature: Disabled

  Scenario Outline: Disabled routes 404
    When a visitor requests <method> <path>
    Then the response status is 404

    Examples:
      | method | path     |
      | GET    | /login   |
      | GET    | /profile |
`)

	scenarios, err := ParseFeatureFile(path)
	if err != nil {
		t.Fatalf("ParseFeatureFile() error = %v", err)
	}
	if len(scenarios) != 1 {
		t.Fatalf("expected 1 outline scenario, got %d", len(scenarios))
	}
	step := scenarios[0].Steps[0]
	wantUnexpanded := `a visitor requests <method> <path>`
	if step.Text != wantUnexpanded {
		t.Errorf("step.Text = %q, want %q", step.Text, wantUnexpanded)
	}
	wantVariants := []string{
		"a visitor requests GET /login",
		"a visitor requests GET /profile",
	}
	if len(step.Variants) != len(wantVariants) {
		t.Fatalf("variants count = %d, want %d", len(step.Variants), len(wantVariants))
	}
	for i, want := range wantVariants {
		if step.Variants[i] != want {
			t.Errorf("variants[%d] = %q, want %q", i, step.Variants[i], want)
		}
	}
}

// TestExpandedOutlineStepTexts_ReturnsExpandedOnly verifies the helper that
// feeds reverse-direction orphan checks: it returns expanded forms only,
// never the unexpanded outline templates.
func TestExpandedOutlineStepTexts_ReturnsExpandedOnly(t *testing.T) {
	dir := t.TempDir()
	path := writeFeatureFile(t, dir, "outline.feature", `
Feature: Outline

  Scenario Outline: Tab is reachable
    Given the user is on "<path>"

    Examples:
      | path      |
      | /app/home |
      | /app/cv   |
`)

	expanded, err := ExpandedOutlineStepTexts(path)
	if err != nil {
		t.Fatalf("ExpandedOutlineStepTexts() error = %v", err)
	}
	want := []string{
		`the user is on "/app/home"`,
		`the user is on "/app/cv"`,
	}
	if len(expanded) != len(want) {
		t.Fatalf("expanded count = %d, want %d: %+v", len(expanded), len(want), expanded)
	}
	for i, w := range want {
		if expanded[i] != w {
			t.Errorf("expanded[%d] = %q, want %q", i, expanded[i], w)
		}
	}
}

// TestParseFeatureFile_ScenarioOutline_NoExamples emits the unexpanded outline
// scenario but no Variants when no Examples table is present.
func TestParseFeatureFile_ScenarioOutline_NoExamples(t *testing.T) {
	dir := t.TempDir()
	path := writeFeatureFile(t, dir, "outline.feature", `
Feature: Outline without examples

  Scenario Outline: Placeholder
    Given a "<x>" exists
`)

	scenarios, err := ParseFeatureFile(path)
	if err != nil {
		t.Fatalf("error: %v", err)
	}
	if len(scenarios) != 1 {
		t.Fatalf("expected 1 scenario, got %d", len(scenarios))
	}
	if len(scenarios[0].Steps[0].Variants) != 0 {
		t.Errorf("variants should be empty when no Examples table; got %v",
			scenarios[0].Steps[0].Variants)
	}
}
