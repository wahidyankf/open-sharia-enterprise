//go:build integration

package cmd

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/cucumber/godog"
)

type dartScaffoldSteps struct {
	tmpDir     string
	srcDir     string
	cmdErr     error
	cmdOutput  string
	originalWd string
}

func (s *dartScaffoldSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "dart-scaffold-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0755)
	verbose = false
	quiet = false
	output = "text"
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *dartScaffoldSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	return context.Background(), nil
}

func (s *dartScaffoldSteps) aGeneratedContractsDirWithModelDartFiles() error {
	s.srcDir = filepath.Join(s.tmpDir, "generated-contracts")
	modelDir := filepath.Join(s.srcDir, "lib", "model")
	if err := os.MkdirAll(modelDir, 0755); err != nil {
		return err
	}
	if err := os.WriteFile(filepath.Join(modelDir, "foo_model.dart"), []byte("// model"), 0644); err != nil {
		return err
	}
	return os.WriteFile(filepath.Join(modelDir, "bar_model.dart"), []byte("// model"), 0644)
}

func (s *dartScaffoldSteps) aGeneratedContractsDirWithNoModelFiles() error {
	s.srcDir = filepath.Join(s.tmpDir, "generated-contracts")
	return os.MkdirAll(filepath.Join(s.srcDir, "lib"), 0755)
}

func (s *dartScaffoldSteps) anExistingGeneratedContractsDirWithOldScaffoldFiles() error {
	s.srcDir = filepath.Join(s.tmpDir, "generated-contracts")
	libDir := filepath.Join(s.srcDir, "lib")
	if err := os.MkdirAll(libDir, 0755); err != nil {
		return err
	}
	if err := os.WriteFile(filepath.Join(s.srcDir, "pubspec.yaml"), []byte("name: old_name\n"), 0644); err != nil {
		return err
	}
	return os.WriteFile(filepath.Join(libDir, "a_demo_contracts.dart"), []byte("// old barrel\n"), 0644)
}

func (s *dartScaffoldSteps) theDeveloperRunsContractsDartScaffold() error {
	buf := new(bytes.Buffer)
	contractsDartScaffoldCmd.SetOut(buf)
	contractsDartScaffoldCmd.SetErr(buf)
	s.cmdErr = contractsDartScaffoldCmd.RunE(contractsDartScaffoldCmd, []string{s.srcDir})
	s.cmdOutput = buf.String()
	return nil
}

func (s *dartScaffoldSteps) theCommandExitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected command to exit successfully, got error: %w", s.cmdErr)
	}
	return nil
}

func (s *dartScaffoldSteps) pubspecYamlIsCreatedWithCorrectContent() error {
	data, err := os.ReadFile(filepath.Join(s.srcDir, "pubspec.yaml"))
	if err != nil {
		return fmt.Errorf("reading pubspec.yaml: %w", err)
	}
	if !strings.Contains(string(data), "name: a_demo_contracts") {
		return fmt.Errorf("expected pubspec.yaml to contain 'name: a_demo_contracts', got: %s", string(data))
	}
	return nil
}

func (s *dartScaffoldSteps) theBarrelLibraryIsCreatedWithPartDirectivesForEachModel() error {
	data, err := os.ReadFile(filepath.Join(s.srcDir, "lib", "a_demo_contracts.dart"))
	if err != nil {
		return fmt.Errorf("reading barrel library: %w", err)
	}
	content := string(data)
	if !strings.Contains(content, "part 'model/foo_model.dart';") {
		return fmt.Errorf("expected barrel to contain foo_model.dart part directive, got: %s", content)
	}
	if !strings.Contains(content, "part 'model/bar_model.dart';") {
		return fmt.Errorf("expected barrel to contain bar_model.dart part directive, got: %s", content)
	}
	return nil
}

func (s *dartScaffoldSteps) pubspecYamlIsCreated() error {
	if _, err := os.Stat(filepath.Join(s.srcDir, "pubspec.yaml")); err != nil {
		return fmt.Errorf("expected pubspec.yaml to exist: %w", err)
	}
	return nil
}

func (s *dartScaffoldSteps) theBarrelLibraryIsCreatedWithoutPartDirectives() error {
	data, err := os.ReadFile(filepath.Join(s.srcDir, "lib", "a_demo_contracts.dart"))
	if err != nil {
		return fmt.Errorf("reading barrel library: %w", err)
	}
	if strings.Contains(string(data), "part '") {
		return fmt.Errorf("expected barrel to have no part directives, but found some in: %s", string(data))
	}
	return nil
}

func (s *dartScaffoldSteps) theExistingFilesAreOverwrittenWithFreshScaffold() error {
	// pubspec.yaml should have been overwritten with fresh content
	data, err := os.ReadFile(filepath.Join(s.srcDir, "pubspec.yaml"))
	if err != nil {
		return fmt.Errorf("reading pubspec.yaml: %w", err)
	}
	if !strings.Contains(string(data), "name: a_demo_contracts") {
		return fmt.Errorf("expected pubspec.yaml to be overwritten with 'name: a_demo_contracts', got: %s", string(data))
	}

	// barrel should have been overwritten
	barrelData, err := os.ReadFile(filepath.Join(s.srcDir, "lib", "a_demo_contracts.dart"))
	if err != nil {
		return fmt.Errorf("reading barrel library: %w", err)
	}
	if strings.Contains(string(barrelData), "// old barrel") {
		return fmt.Errorf("expected barrel to be overwritten, but still contains old content")
	}
	return nil
}

func InitializeDartScaffoldScenario(sc *godog.ScenarioContext) {
	s := &dartScaffoldSteps{}
	sc.Before(s.before)
	sc.After(s.after)

	sc.Step(`^a generated-contracts directory with model Dart files$`,
		s.aGeneratedContractsDirWithModelDartFiles)
	sc.Step(`^a generated-contracts directory with no model files$`,
		s.aGeneratedContractsDirWithNoModelFiles)
	sc.Step(`^an existing generated-contracts directory with old scaffold files$`,
		s.anExistingGeneratedContractsDirWithOldScaffoldFiles)
	sc.Step(`^the developer runs contracts dart-scaffold on the directory$`,
		s.theDeveloperRunsContractsDartScaffold)
	sc.Step(`^the command exits successfully$`,
		s.theCommandExitsSuccessfully)
	sc.Step(`^pubspec\.yaml is created with correct content$`,
		s.pubspecYamlIsCreatedWithCorrectContent)
	sc.Step(`^the barrel library is created with part directives for each model$`,
		s.theBarrelLibraryIsCreatedWithPartDirectivesForEachModel)
	sc.Step(`^pubspec\.yaml is created$`,
		s.pubspecYamlIsCreated)
	sc.Step(`^the barrel library is created without part directives$`,
		s.theBarrelLibraryIsCreatedWithoutPartDirectives)
	sc.Step(`^the existing files are overwritten with fresh scaffold$`,
		s.theExistingFilesAreOverwrittenWithFreshScaffold)
}

func TestIntegrationContractsDartScaffold(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeDartScaffoldScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsContractsDir},
			TestingT: t,
			Tags:     "@contracts-dart-scaffold",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run feature tests")
	}
}
