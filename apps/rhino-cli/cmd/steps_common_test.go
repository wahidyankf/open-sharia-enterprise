package cmd

// Common step patterns shared across multiple commands.
const (
	stepExitsSuccessfully = `^the command exits successfully$`
	stepExitsWithFailure  = `^the command exits with a failure code$`
	stepOutputIsValidJSON = `^the output is valid JSON$`
)

// Doctor-specific step patterns.
const (
	stepAllToolsPresentWithMatchingVersions   = `^all required development tools are present with matching versions$`
	stepARequiredToolNotFoundInPATH           = `^a required development tool is not found in the system PATH$`
	stepARequiredToolInstalledWithNonMatching = `^a required development tool is installed with a non-matching version$`
	stepDeveloperRunsDoctorCommand            = `^the developer runs the doctor command$`
	stepDeveloperRunsDoctorCommandWithJSON    = `^the developer runs the doctor command with JSON output$`
	stepOutputReportsEachToolAsPassing        = `^the output reports each tool as passing$`
	stepOutputIdentifiesMissingTool           = `^the output identifies the missing tool$`
	stepOutputReportsToolAsWarning            = `^the output reports the tool as a warning rather than a failure$`
	stepJSONListsEveryCheckedToolWithStatus   = `^the JSON lists every checked tool with its status$`
)

// Test-coverage validate step patterns.
const (
	stepGoCoverageFile90Pct                        = `^a Go coverage file recording 90% line coverage$`
	stepGoCoverageFile70Pct                        = `^a Go coverage file recording 70% line coverage$`
	stepGoCoverageFile85Pct                        = `^a Go coverage file recording 85% line coverage$`
	stepLCOVCoverageFile90Pct                      = `^an LCOV coverage file recording 90% line coverage$`
	stepLCOVCoverageFileMultipleSourceFiles        = `^an LCOV coverage file with multiple source files$`
	stepDeveloperRunsValidateCoverage85WithPerFile = `^the developer runs test-coverage validate with an 85% threshold and per-file flag$`
	stepOutputContainsPerFileCoverageBreakdown     = `^the output contains per-file coverage breakdown$`
	stepDeveloperRunsValidateCoverageWithExclusion = `^the developer runs test-coverage validate with exclusion of a source file$`
	stepOutputDoesNotContainExcludedFile           = `^the output does not contain the excluded file$`
	stepCoberturaXMLCoverageFile90Pct              = `^a Cobertura XML coverage file recording 90% line coverage$`
	stepCoberturaXMLCoverageFileWithPartialBranch  = `^a Cobertura XML coverage file with partial branch coverage$`
	stepNoCoverageFileExistsAtPath                 = `^no coverage file exists at the specified path$`
	stepDeveloperRunsValidateCoverage85            = `^the developer runs test-coverage validate with an 85% threshold$`
	stepDeveloperRunsValidateCoverage85WithJSON    = `^the developer runs test-coverage validate with an 85% threshold requesting JSON output$`
	stepOutputReportsMeasuredCoveragePct           = `^the output reports the measured coverage percentage$`
	stepOutputIndicatesCoveragePasses              = `^the output indicates the coverage passes the threshold$`
	stepOutputIndicatesCoverageFails               = `^the output indicates the coverage fails the threshold$`
	stepJSONIncludesCoveragePctAndPassFail         = `^the JSON includes the coverage percentage and pass/fail status$`
	stepOutputDescribesMissingFile                 = `^the output describes the missing file$`
)

// Test-coverage merge step patterns.
const (
	stepTwoLCOVFilesWithDifferentSourceFiles = `^two LCOV coverage files with different source files$`
	stepTwoLCOVFilesWithHighCoverage         = `^two LCOV coverage files with high coverage$`
	stepTwoLCOVFilesWithLowCoverage          = `^two LCOV coverage files with low coverage$`
	stepDeveloperRunsMergeWithOutputFile     = `^the developer runs test-coverage merge with an output file$`
	stepDeveloperRunsMergeWithValidation80   = `^the developer runs test-coverage merge with validation at 80% threshold$`
	stepDeveloperRunsMergeWithValidation95   = `^the developer runs test-coverage merge with validation at 95% threshold$`
	stepMergedOutputFileExistsInLCOVFormat   = `^the merged output file exists in LCOV format$`
)

// Agents sync step patterns.
const (
	stepClaudeDirWithValidAgentsAndSkills         = `^a \.claude/ directory with valid agents and skills$`
	stepClaudeDirWithAgentsAndSkillsToConvert     = `^a \.claude/ directory with agents and skills to convert$`
	stepClaudeDirWithBothAgentsAndSkills          = `^a \.claude/ directory with both agents and skills$`
	stepClaudeAgentConfiguredWithSonnetModel      = `^a \.claude/ agent configured with the "sonnet" model$`
	stepDeveloperRunsSyncAgents                   = `^the developer runs agents sync$`
	stepDeveloperRunsSyncAgentsWithDryRunFlag     = `^the developer runs agents sync with the --dry-run flag$`
	stepDeveloperRunsSyncAgentsWithAgentsOnlyFlag = `^the developer runs agents sync with the --agents-only flag$`
	stepOpenCodeDirContainsConvertedConfig        = `^the \.opencode/ directory contains the converted configuration$`
	stepOutputDescribesPlannedOperations          = `^the output describes the planned operations$`
	stepNoFilesWrittenToOpenCodeDir               = `^no files are written to the \.opencode/ directory$`
	stepOnlyAgentFilesWrittenToOpenCodeDir        = `^only agent files are written to the \.opencode/ directory$`
	stepCorrespondingOpenCodeAgentUsesZaiGlmModel = `^the corresponding \.opencode/ agent uses the "zai/glm-4\.7" model identifier$`
)

// Agents validate-claude step patterns.
const (
	stepClaudeDirWhereAllAgentsAndSkillsValid         = `^a \.claude/ directory where all agents and skills are valid$`
	stepClaudeDirWhereOneAgentMissingToolsField       = `^a \.claude/ directory where one agent is missing the required "tools" field$`
	stepClaudeDirWithTwoAgentsSameName                = `^a \.claude/ directory containing two agent files declaring the same name$`
	stepClaudeDirAgentsValidButSkillsHaveIssues       = `^a \.claude/ directory where agents are valid but skills have issues$`
	stepClaudeDirSkillsValidButAgentsHaveIssues       = `^a \.claude/ directory where skills are valid but agents have issues$`
	stepDeveloperRunsValidateClaude                   = `^the developer runs agents validate-claude$`
	stepDeveloperRunsValidateClaudeWithAgentsOnlyFlag = `^the developer runs agents validate-claude with the --agents-only flag$`
	stepDeveloperRunsValidateClaudeWithSkillsOnlyFlag = `^the developer runs agents validate-claude with the --skills-only flag$`
	stepOutputReportsAllChecksAsPassing               = `^the output reports all checks as passing$`
	stepOutputIdentifiesAgentAndMissingField          = `^the output identifies the agent and the missing field$`
	stepOutputReportsDuplicateAgentName               = `^the output reports the duplicate agent name$`
)

// Agents validate-sync step patterns.
const (
	stepClaudeAndOpenCodeConfigsFullySynchronised      = `^\.claude/ and \.opencode/ configurations that are fully synchronised$`
	stepAgentInClaudeWithDescriptionMismatch           = `^an agent in \.claude/ whose description differs from its \.opencode/ counterpart$`
	stepClaudeContainingMoreAgentsThanOpenCode         = `^\.claude/ containing more agents than \.opencode/$`
	stepDeveloperRunsValidateSync                      = `^the developer runs agents validate-sync$`
	stepOutputReportsAllSyncChecksAsPassing            = `^the output reports all sync checks as passing$`
	stepOutputIdentifiesAgentWithMismatchedDescription = `^the output identifies the agent with the mismatched description$`
	stepOutputReportsAgentCountMismatch                = `^the output reports the agent count mismatch$`
)

// Docs validate-links step patterns.
const (
	stepMarkdownFilesAllInternalLinksValid       = `^markdown files where all internal links point to existing files$`
	stepMarkdownFileWithLinkToNonExistentFile    = `^a markdown file with a link pointing to a non-existent file$`
	stepMarkdownFileContainingOnlyExternalLinks  = `^a markdown file containing only external HTTPS links$`
	stepMarkdownFileWithBrokenLinkNotStaged      = `^a markdown file with a broken link that has not been staged in git$`
	stepDeveloperRunsValidateDocsLinks           = `^the developer runs docs validate-links$`
	stepDeveloperRunsValidateDocsLinksWithStaged = `^the developer runs docs validate-links with the --staged-only flag$`
	stepOutputReportsNoBrokenLinksFound          = `^the output reports no broken links found$`
	stepOutputIdentifiesFileContainingBrokenLink = `^the output identifies the file containing the broken link$`
)

// Docs validate-naming step patterns.
const (
	stepDocsDirWhereEveryFileFollowsNamingConvention   = `^a docs directory where every file follows the naming convention$`
	stepDocsDirWithFileWithoutDoubleDashSeparator      = `^a docs directory containing a file without the double-underscore prefix separator$`
	stepDocsDirWithFilePrefixNotMatchingDirPath        = `^a docs directory containing a file whose prefix does not match its directory path$`
	stepDocsDirWithNamingViolations                    = `^a docs directory containing files with naming violations$`
	stepDeveloperRunsValidateDocsNaming                = `^the developer runs docs validate-naming$`
	stepDeveloperRunsValidateDocsNamingWithFix         = `^the developer runs docs validate-naming with the --fix flag$`
	stepDeveloperRunsValidateDocsNamingWithFixAndApply = `^the developer runs docs validate-naming with --fix and --apply flags$`
	stepOutputReportsZeroViolations                    = `^the output reports zero violations$`
	stepOutputIdentifiesFileWithNamingViolation        = `^the output identifies the file with the naming violation$`
	stepOutputReportsExpectedPrefixAlongsideFilename   = `^the output reports the expected prefix alongside the actual filename$`
	stepOutputShowsPlannedRenames                      = `^the output shows the planned renames$`
	stepNoFilesRenamedOnDisk                           = `^no files are renamed on disk$`
	stepFilesRenamedToFollowNamingConvention           = `^the files are renamed to follow the naming convention$`
)

// Contracts java-clean-imports step patterns.
const (
	stepGeneratedContractsDirWithUnusedImports       = `^a generated-contracts directory with Java files containing unused imports$`
	stepGeneratedContractsDirWithSamePackageImports  = `^a generated-contracts directory with Java files containing same-package imports$`
	stepGeneratedContractsDirWithDuplicateImports    = `^a generated-contracts directory with Java files containing duplicate imports$`
	stepGeneratedContractsDirWithOnlyRequiredImports = `^a generated-contracts directory with Java files having only required imports$`
	stepEmptyGeneratedContractsDir                   = `^an empty generated-contracts directory$`
	stepDeveloperRunsContractsJavaCleanImports       = `^the developer runs contracts java-clean-imports on the directory$`
	stepUnusedImportsRemovedFromJavaFiles            = `^unused imports are removed from the Java files$`
	stepSamePackageImportsRemovedFromJavaFiles       = `^same-package imports are removed from the Java files$`
	stepOnlyOneCopyOfEachImportRemains               = `^only one copy of each import remains$`
	stepJavaFilesAreUnchanged                        = `^the Java files are unchanged$`
	stepCommandReportsNoFilesModified                = `^the command reports no files modified$`
)

// Contracts dart-scaffold step patterns.
const (
	stepGeneratedContractsDirWithModelDartFiles      = `^a generated-contracts directory with model Dart files$`
	stepGeneratedContractsDirWithNoModelFiles        = `^a generated-contracts directory with no model files$`
	stepExistingGeneratedContractsDirWithOldScaffold = `^an existing generated-contracts directory with old scaffold files$`
	stepDeveloperRunsContractsDartScaffold           = `^the developer runs contracts dart-scaffold on the directory$`
	stepPubspecYamlCreatedWithCorrectContent         = `^pubspec\.yaml is created with correct content$`
	stepBarrelLibraryCreatedWithPartDirectives       = `^the barrel library is created with part directives for each model$`
	stepPubspecYamlCreated                           = `^pubspec\.yaml is created$`
	stepBarrelLibraryCreatedWithoutPartDirectives    = `^the barrel library is created without part directives$`
	stepExistingFilesOverwrittenWithFreshScaffold    = `^the existing files are overwritten with fresh scaffold$`
)

// Java validate-annotations step patterns.
const (
	stepJavaSourceTreeAllPackagesNullMarked          = `^a Java source tree where every package has a @NullMarked-annotated package-info\.java$`
	stepJavaSourceTreeOnePackageNoPackageInfo        = `^a Java source tree where one package has no package-info\.java file$`
	stepJavaSourceTreeOnePackageWithoutNullMarked    = `^a Java source tree where one package has a package-info\.java without @NullMarked$`
	stepJavaSourceTreeAllPackagesNonNull             = `^a Java source tree where every package has a @NonNull-annotated package-info\.java$`
	stepDeveloperRunsJavaValidateAnnotationsOnRoot   = `^the developer runs java validate-annotations on the source root$`
	stepDeveloperRunsJavaValidateAnnotationsNonNull  = `^the developer runs java validate-annotations with --annotation NonNull$`
	stepOutputReportsZeroJavaViolations              = `^the output reports zero violations$`
	stepOutputIdentifiesPackageMissingPackageInfo    = `^the output identifies the package missing package-info\.java$`
	stepOutputIdentifiesPackageWithMissingAnnotation = `^the output identifies the package with the missing annotation$`
)

// Spec-coverage validate step patterns.
const (
	stepSpecsDirEveryFeatureFileHasTest              = `^a specs directory where every feature file has a corresponding test file$`
	stepSpecsDirContainsFeatureFileWithNoTest        = `^a specs directory containing a feature file with no corresponding test file$`
	stepFeatureFileWithScenarioNotInAnyTestFile      = `^a feature file with a scenario whose title does not appear in any test file$`
	stepFeatureFileWithStepTextNotInAnyTestFile      = `^a feature file with a step text that does not appear in any test file$`
	stepDeveloperRunsValidateSpecCoverage            = `^the developer runs spec-coverage validate on the specs and app directories$`
	stepOutputReportsAllSpecsAsCovered               = `^the output reports all specs as covered$`
	stepOutputIdentifiesFeatureFileAsUncoveredSpec   = `^the output identifies the feature file as an uncovered spec$`
	stepOutputIdentifiesScenarioAsUnimplemented      = `^the output identifies the scenario as an unimplemented scenario$`
	stepOutputIdentifiesStepAsUndefined              = `^the output identifies the step as an undefined step$`
	stepFeatureFilesWithStepsInSharedStepFiles       = `^feature files with steps implemented in shared step files$`
	stepDeveloperRunsValidateSpecCoverageSharedSteps = `^the developer runs spec-coverage validate with shared-steps flag$`
	stepCommandValidatesStepsAcrossAllSourceFiles    = `^the command validates steps across all source files without file matching$`
	stepFeatureFilesWithTestsInMultipleLanguages     = `^feature files with test implementations in multiple languages$`
	stepTestFilesMatchedUsingLanguageConventions     = `^test files are matched using language-specific conventions$`
)

// Test-coverage diff step patterns.
const (
	stepCoverageFileAndNoGitChanges                    = `^a coverage file and no git changes$`
	stepCoverageFileAllChangedLinesCovered             = `^a coverage file where all changed lines are covered$`
	stepCoverageFileSomeChangedLinesMissed             = `^a coverage file where some changed lines are missed$`
	stepCoverageFileAndChangesInExcludedFiles          = `^a coverage file and changes in excluded files$`
	stepDeveloperRunsTestCoverageDiff                  = `^the developer runs test-coverage diff$`
	stepDeveloperRunsTestCoverageDiffWithThreshold     = `^the developer runs test-coverage diff with a threshold$`
	stepDeveloperRunsTestCoverageDiffWithHighThreshold = `^the developer runs test-coverage diff with a high threshold$`
	stepDeveloperRunsTestCoverageDiffWithExclusion     = `^the developer runs test-coverage diff with exclusion$`
	stepOutputReports100PercentCoverage                = `^the output reports 100% coverage$`
	stepExcludedFilesDoNotAffectDiffResult             = `^the excluded files do not affect the diff coverage result$`
)

// Git pre-commit step patterns.
const (
	stepDeveloperIsOutsideGitRepository     = `^the developer is outside a git repository$`
	stepDeveloperRunsGitPreCommit           = `^the developer runs rhino-cli git pre-commit$`
	stepOutputMentionsGitRepositoryNotFound = `^the output mentions that a git repository was not found$`
)
