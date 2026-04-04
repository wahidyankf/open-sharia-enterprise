package cmd

import (
	"bytes"
	"context"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/cucumber/godog"
)

var specsDirUnitEnvInit = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/cli/gherkin")
}()

type envInitUnitSteps struct {
	cmdErr    error
	cmdOutput string

	// Mocked filesystem state.
	examples     map[string][]byte // path -> content for .env.example files
	existingEnvs map[string]bool   // paths of existing .env files
	writtenFiles map[string][]byte // files written by envInitWriteFile
}

func (s *envInitUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	envInitForce = false
	s.cmdErr = nil
	s.cmdOutput = ""
	s.examples = make(map[string][]byte)
	s.existingEnvs = make(map[string]bool)
	s.writtenFiles = make(map[string][]byte)

	// Mock findGitRoot via osGetwd/osStat — default to /mock-repo.
	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}

	// Default: no examples found.
	envInitWalkDir = func(_ string, _ fs.WalkDirFunc) error {
		return nil
	}
	envInitReadFile = func(path string) ([]byte, error) {
		if data, ok := s.examples[path]; ok {
			return data, nil
		}
		return nil, fmt.Errorf("file not found: %s", path)
	}
	envInitWriteFile = func(path string, data []byte, _ os.FileMode) error {
		s.writtenFiles[path] = data
		return nil
	}
	envInitStat = func(path string) (os.FileInfo, error) {
		if s.existingEnvs[path] {
			return &mockFileInfo{name: filepath.Base(path)}, nil
		}
		return nil, os.ErrNotExist
	}

	return context.Background(), nil
}

func (s *envInitUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	osGetwd = os.Getwd
	osStat = os.Stat
	envInitWalkDir = filepath.WalkDir
	envInitReadFile = os.ReadFile
	envInitWriteFile = os.WriteFile
	envInitStat = os.Stat
	envInitForce = false
	return context.Background(), nil
}

// --- mock WalkDir helper ---

// mockDirEntry implements fs.DirEntry for test use.
type mockDirEntry struct {
	name  string
	isDir bool
}

func (d *mockDirEntry) Name() string               { return d.name }
func (d *mockDirEntry) IsDir() bool                { return d.isDir }
func (d *mockDirEntry) Type() fs.FileMode          { return 0 }
func (d *mockDirEntry) Info() (fs.FileInfo, error) { return nil, nil }

func (s *envInitUnitSteps) setupWalkDir() {
	envInitWalkDir = func(root string, fn fs.WalkDirFunc) error {
		// Walk root directory entry first.
		if err := fn(root, &mockDirEntry{name: filepath.Base(root), isDir: true}, nil); err != nil {
			return err
		}
		// Walk each example in sorted order.
		for path := range s.examples {
			dir := filepath.Dir(path)
			// Walk the directory entry.
			if err := fn(dir, &mockDirEntry{name: filepath.Base(dir), isDir: true}, nil); err != nil {
				return err
			}
			// Walk the .env.example file entry.
			if err := fn(path, &mockDirEntry{name: ".env.example", isDir: false}, nil); err != nil {
				return err
			}
		}
		return nil
	}
}

// --- Given steps ---

func (s *envInitUnitSteps) envExamplesExistButNoEnvFiles() error {
	s.examples = map[string][]byte{
		"/mock-repo/infra/dev/app-one/.env.example": []byte("KEY1=value1\n"),
		"/mock-repo/infra/dev/app-two/.env.example": []byte("KEY2=value2\n"),
	}
	s.existingEnvs = make(map[string]bool)
	s.setupWalkDir()
	return nil
}

func (s *envInitUnitSteps) envExamplesAndSomeEnvFilesExist() error {
	s.examples = map[string][]byte{
		"/mock-repo/infra/dev/app-one/.env.example": []byte("KEY1=value1\n"),
		"/mock-repo/infra/dev/app-two/.env.example": []byte("KEY2=value2\n"),
	}
	// app-one already has a .env file.
	s.existingEnvs = map[string]bool{
		"/mock-repo/infra/dev/app-one/.env": true,
	}
	s.setupWalkDir()
	return nil
}

func (s *envInitUnitSteps) noEnvExamplesExist() error {
	s.examples = make(map[string][]byte)
	// WalkDir returns nothing (default mock already does this).
	envInitWalkDir = func(_ string, fn fs.WalkDirFunc) error {
		return fn("/mock-repo/infra/dev", &mockDirEntry{name: "dev", isDir: true}, nil)
	}
	return nil
}

// --- When steps ---

func (s *envInitUnitSteps) runEnvInitCmd() error {
	buf := new(bytes.Buffer)
	envInitCmd.SetOut(buf)
	envInitCmd.SetErr(buf)
	s.cmdErr = envInitCmd.RunE(envInitCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *envInitUnitSteps) theDeveloperRunsEnvInit() error {
	return s.runEnvInitCmd()
}

func (s *envInitUnitSteps) theDeveloperRunsEnvInitWithForce() error {
	envInitForce = true
	return s.runEnvInitCmd()
}

// --- Then steps ---

func (s *envInitUnitSteps) theCommandExitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected no error, got: %v", s.cmdErr)
	}
	return nil
}

func (s *envInitUnitSteps) theCommandExitsWithAFailureCode() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected an error, got nil")
	}
	return nil
}

func (s *envInitUnitSteps) envFilesCreatedFromExamples() error {
	for exPath, content := range s.examples {
		envPath := filepath.Join(filepath.Dir(exPath), ".env")
		written, ok := s.writtenFiles[envPath]
		if !ok {
			return fmt.Errorf("expected %s to be written, but it was not", envPath)
		}
		if string(written) != string(content) {
			return fmt.Errorf("expected content %q at %s, got %q", string(content), envPath, string(written))
		}
	}
	return nil
}

func (s *envInitUnitSteps) outputListsEachCreatedFile() error {
	if !strings.Contains(s.cmdOutput, "Created:") {
		return fmt.Errorf("expected 'Created:' in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *envInitUnitSteps) existingEnvFilesNotOverwritten() error {
	for envPath := range s.existingEnvs {
		if _, ok := s.writtenFiles[envPath]; ok {
			return fmt.Errorf("expected %s to NOT be overwritten, but it was", envPath)
		}
	}
	return nil
}

func (s *envInitUnitSteps) outputShowsSkippedFiles() error {
	if !strings.Contains(s.cmdOutput, "Skipped:") {
		return fmt.Errorf("expected 'Skipped:' in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *envInitUnitSteps) allEnvFilesCreatedOrOverwritten() error {
	for exPath := range s.examples {
		envPath := filepath.Join(filepath.Dir(exPath), ".env")
		if _, ok := s.writtenFiles[envPath]; !ok {
			return fmt.Errorf("expected %s to be written (force mode), but it was not", envPath)
		}
	}
	return nil
}

func (s *envInitUnitSteps) outputReportsZeroFilesCreated() error {
	if !strings.Contains(s.cmdOutput, "0 created") {
		return fmt.Errorf("expected '0 created' in output, got: %s", s.cmdOutput)
	}
	return nil
}

// --- Test suite ---

func TestUnitEnvInit(t *testing.T) {
	s := &envInitUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)

			// Given steps.
			sc.Step(stepEnvExamplesExistButNoEnvFiles, s.envExamplesExistButNoEnvFiles)
			sc.Step(stepEnvExamplesAndSomeEnvFilesExist, s.envExamplesAndSomeEnvFilesExist)
			sc.Step(stepNoEnvExamplesExist, s.noEnvExamplesExist)

			// When steps.
			sc.Step(stepDeveloperRunsEnvInit, s.theDeveloperRunsEnvInit)
			sc.Step(stepDeveloperRunsEnvInitWithForce, s.theDeveloperRunsEnvInitWithForce)

			// Then steps.
			sc.Step(stepExitsSuccessfully, s.theCommandExitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.theCommandExitsWithAFailureCode)
			sc.Step(stepEnvFilesCreatedFromExamples, s.envFilesCreatedFromExamples)
			sc.Step(stepOutputListsEachCreatedFile, s.outputListsEachCreatedFile)
			sc.Step(stepExistingEnvFilesNotOverwritten, s.existingEnvFilesNotOverwritten)
			sc.Step(stepOutputShowsSkippedFiles, s.outputShowsSkippedFiles)
			sc.Step(stepAllEnvFilesCreatedOrOverwritten, s.allEnvFilesCreatedOrOverwritten)
			sc.Step(stepOutputReportsZeroFilesCreated, s.outputReportsZeroFilesCreated)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitEnvInit},
			Tags:     "@env-init",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestEnvInitCmd_Initialization verifies the command metadata.
func TestEnvInitCmd_Initialization(t *testing.T) {
	if envInitCmd.Use != "init" {
		t.Errorf("expected Use == %q, got %q", "init", envInitCmd.Use)
	}
	if !strings.Contains(strings.ToLower(envInitCmd.Short), "env") {
		t.Errorf("expected Short to mention env, got %q", envInitCmd.Short)
	}
}

// TestEnvInitCmd_NoArgs verifies the command accepts no positional arguments.
func TestEnvInitCmd_NoArgs(t *testing.T) {
	if envInitCmd.Args == nil {
		return // cobra.NoArgs is nil-able default; acceptable
	}
	if err := envInitCmd.Args(envInitCmd, []string{"unexpected"}); err == nil {
		t.Error("expected error when positional args provided, got nil")
	}
}

// TestEnvInitCmd_ForceFlag verifies the --force flag is wired.
func TestEnvInitCmd_ForceFlag(t *testing.T) {
	f := envInitCmd.Flags().Lookup("force")
	if f == nil {
		t.Fatal("expected --force flag to be registered")
	}
}
