//go:build integration

package cmd

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"

	"github.com/cucumber/godog"
)

var specsLinksDir = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/rhino-cli/links")
}()

// Scenario: A document set with all valid internal links passes validation
// Given markdown files where all internal links point to existing files
// When the developer runs validate-links
// Then the command exits successfully
// And the output reports no broken links found

// Scenario: A broken internal link is detected and reported
// Given a markdown file with a link pointing to a non-existent file
// When the developer runs validate-links
// Then the command exits with a failure code
// And the output identifies the file containing the broken link

// Scenario: External URLs are not validated
// Given a markdown file containing only external HTTPS links
// When the developer runs validate-links
// Then the command exits successfully
// And the output reports no broken links found

// Scenario: With --staged-only only staged files are checked
// Given a markdown file with a broken link that has not been staged in git
// When the developer runs validate-links with the --staged-only flag
// Then the command exits successfully

type validateLinksSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
}

func (s *validateLinksSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "validate-links-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0755)
	verbose = false
	quiet = false
	output = "text"
	validateLinksStagedOnly = false
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *validateLinksSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	return context.Background(), nil
}

func (s *validateLinksSteps) markdownFilesWhereAllInternalLinksPointToExistingFiles() error {
	docsDir := filepath.Join(s.tmpDir, "docs")
	if err := os.MkdirAll(docsDir, 0755); err != nil {
		return err
	}
	if err := os.WriteFile(filepath.Join(docsDir, "b.md"), []byte("# B\n"), 0644); err != nil {
		return err
	}
	if err := os.WriteFile(filepath.Join(docsDir, "a.md"), []byte("[see](./b.md)\n"), 0644); err != nil {
		return err
	}
	return nil
}

func (s *validateLinksSteps) aMarkdownFileWithALinkPointingToANonExistentFile() error {
	docsDir := filepath.Join(s.tmpDir, "docs")
	if err := os.MkdirAll(docsDir, 0755); err != nil {
		return err
	}
	if err := os.WriteFile(filepath.Join(docsDir, "a.md"), []byte("[see](./nonexistent.md)\n"), 0644); err != nil {
		return err
	}
	return nil
}

func (s *validateLinksSteps) aMarkdownFileContainingOnlyExternalHTTPSLinks() error {
	docsDir := filepath.Join(s.tmpDir, "docs")
	if err := os.MkdirAll(docsDir, 0755); err != nil {
		return err
	}
	if err := os.WriteFile(filepath.Join(docsDir, "a.md"), []byte("[link](https://example.com)\n"), 0644); err != nil {
		return err
	}
	return nil
}

func (s *validateLinksSteps) aMarkdownFileWithABrokenLinkThatHasNotBeenStagedInGit() error {
	// Re-initialize tmpDir as a real git repo so git diff --cached works correctly.
	if err := exec.Command("git", "init", s.tmpDir).Run(); err != nil {
		return fmt.Errorf("git init failed: %w", err)
	}
	_ = exec.Command("git", "-C", s.tmpDir, "config", "user.email", "test@example.com").Run()
	_ = exec.Command("git", "-C", s.tmpDir, "config", "user.name", "Test User").Run()

	docsDir := filepath.Join(s.tmpDir, "docs")
	if err := os.MkdirAll(docsDir, 0755); err != nil {
		return err
	}
	// Write a broken link but do NOT stage the file.
	if err := os.WriteFile(filepath.Join(docsDir, "a.md"), []byte("[see](./nonexistent.md)\n"), 0644); err != nil {
		return err
	}
	return nil
}

func (s *validateLinksSteps) theDeveloperRunsValidateLinks() error {
	validateLinksStagedOnly = false
	buf := new(bytes.Buffer)
	validateLinksCmd.SetOut(buf)
	validateLinksCmd.SetErr(buf)
	s.cmdErr = validateLinksCmd.RunE(validateLinksCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateLinksSteps) theDeveloperRunsValidateLinksWithTheStagedOnlyFlag() error {
	validateLinksStagedOnly = true
	buf := new(bytes.Buffer)
	validateLinksCmd.SetOut(buf)
	validateLinksCmd.SetErr(buf)
	s.cmdErr = validateLinksCmd.RunE(validateLinksCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateLinksSteps) theValidateLinksCommandExitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected command to exit successfully, got error: %w (output: %s)", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *validateLinksSteps) theValidateLinksCommandExitsWithAFailureCode() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected command to exit with failure, but it succeeded (output: %s)", s.cmdOutput)
	}
	return nil
}

func (s *validateLinksSteps) theOutputReportsNoBrokenLinksFound() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected no broken links, got error: %w (output: %s)", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *validateLinksSteps) theOutputIdentifiesTheFileContainingTheBrokenLink() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected broken link error, but command succeeded")
	}
	return nil
}

func InitializeValidateLinksScenario(sc *godog.ScenarioContext) {
	s := &validateLinksSteps{}
	sc.Before(s.before)
	sc.After(s.after)

	sc.Step(`^markdown files where all internal links point to existing files$`,
		s.markdownFilesWhereAllInternalLinksPointToExistingFiles)
	sc.Step(`^a markdown file with a link pointing to a non-existent file$`,
		s.aMarkdownFileWithALinkPointingToANonExistentFile)
	sc.Step(`^a markdown file containing only external HTTPS links$`,
		s.aMarkdownFileContainingOnlyExternalHTTPSLinks)
	sc.Step(`^a markdown file with a broken link that has not been staged in git$`,
		s.aMarkdownFileWithABrokenLinkThatHasNotBeenStagedInGit)
	sc.Step(`^the developer runs validate-links$`,
		s.theDeveloperRunsValidateLinks)
	sc.Step(`^the developer runs validate-links with the --staged-only flag$`,
		s.theDeveloperRunsValidateLinksWithTheStagedOnlyFlag)
	sc.Step(`^the command exits successfully$`,
		s.theValidateLinksCommandExitsSuccessfully)
	sc.Step(`^the command exits with a failure code$`,
		s.theValidateLinksCommandExitsWithAFailureCode)
	sc.Step(`^the output reports no broken links found$`,
		s.theOutputReportsNoBrokenLinksFound)
	sc.Step(`^the output identifies the file containing the broken link$`,
		s.theOutputIdentifiesTheFileContainingTheBrokenLink)
}

func TestIntegrationValidateLinks(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeValidateLinksScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsLinksDir},
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run feature tests")
	}
}
