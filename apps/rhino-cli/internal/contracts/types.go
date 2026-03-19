// Package contracts provides codegen post-processing commands for generated API contracts.
package contracts

// JavaCleanImportsOptions configures the java-clean-imports command.
type JavaCleanImportsOptions struct {
	Dir string // Absolute path to generated-contracts directory
}

// JavaCleanImportsResult contains the results of cleaning Java imports.
type JavaCleanImportsResult struct {
	TotalFiles    int      // Number of .java files found
	ModifiedFiles int      // Number of files that were modified
	Modified      []string // Relative paths of modified files
}

// DartScaffoldOptions configures the dart-scaffold command.
type DartScaffoldOptions struct {
	Dir string // Absolute path to generated-contracts directory
}

// DartScaffoldResult contains the results of Dart scaffolding.
type DartScaffoldResult struct {
	PubspecCreated bool     // Whether pubspec.yaml was written
	BarrelCreated  bool     // Whether barrel library was written
	ModelFiles     []string // Basenames of model files found
}
