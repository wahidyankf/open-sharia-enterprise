package doctor

import (
	"path/filepath"
	"strings"
)

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
// To add a new tool, add a new entry to the slice below — no other file needs to change.
func buildToolDefs(repoRoot string) []toolDef {
	packageJSONPath := filepath.Join(repoRoot, "package.json")
	pomXMLPath := filepath.Join(repoRoot, "apps", "organiclever-be-jasb", "pom.xml")
	goModPath := filepath.Join(repoRoot, "apps", "rhino-cli", "go.mod")
	vercelJSONPath := filepath.Join(repoRoot, "apps", "oseplatform-web", "vercel.json")
	pythonVersionPath := filepath.Join(repoRoot, "apps", "demo-be-python-fastapi", ".python-version")
	toolVersionsPath := filepath.Join(repoRoot, ".tool-versions")
	globalJSONPath := filepath.Join(repoRoot, "apps", "demo-be-fsharp-giraffe", "global.json")
	pubspecPath := filepath.Join(repoRoot, "apps", "demo-fe-dart-flutterweb", "pubspec.yaml")

	noReq := func() string { return "" }

	return []toolDef{
		// --- Core tools ---
		{
			name:     "git",
			binary:   "git",
			source:   "(no config file)",
			args:     []string{"--version"},
			parseVer: func(s string) string { return parseLineWord(s, "git version ", 2, "") },
			compare:  compareExact,
			readReq:  noReq,
		},
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
			source:    "apps/organiclever-be-jasb/pom.xml → <java.version>",
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
			parseVer: func(s string) string { return parseLineWord(s, "Apache Maven ", 2, "") },
			compare:  compareExact,
			readReq:  noReq,
		},
		{
			name:     "golang",
			binary:   "go",
			source:   "apps/rhino-cli/go.mod → go directive",
			args:     []string{"version"},
			parseVer: func(s string) string { return parseLineWord(s, "go version ", 2, "go") },
			compare:  compareGTE,
			readReq:  func() string { v, _ := readGoVersion(goModPath); return v },
		},
		// --- Hugo ---
		{
			name:     "hugo",
			binary:   "hugo",
			source:   "apps/oseplatform-web/vercel.json → HUGO_VERSION",
			args:     []string{"version"},
			parseVer: parseHugoVersion,
			compare:  compareGTE,
			readReq:  func() string { v, _ := readHugoVersion(vercelJSONPath); return v },
		},
		// --- Python ---
		{
			name:     "python",
			binary:   "python3",
			source:   "apps/demo-be-python-fastapi/.python-version",
			args:     []string{"--version"},
			parseVer: parsePythonVersion,
			compare:  compareGTE,
			readReq:  func() string { v, _ := readPythonVersion(pythonVersionPath); return v },
		},
		// --- Rust ---
		{
			name:     "rust",
			binary:   "rustc",
			source:   "(no config file)",
			args:     []string{"--version"},
			parseVer: parseRustVersion,
			compare:  compareExact,
			readReq:  noReq,
		},
		{
			name:     "cargo-llvm-cov",
			binary:   "cargo",
			source:   "(no config file)",
			args:     []string{"llvm-cov", "--version"},
			parseVer: parseCargoLlvmCov,
			compare:  compareExact,
			readReq:  noReq,
		},
		// --- Elixir/Erlang ---
		{
			name:     "elixir",
			binary:   "elixir",
			source:   ".tool-versions → elixir",
			args:     []string{"--version"},
			parseVer: parseElixirVersion,
			compare:  compareGTE,
			readReq: func() string {
				v, _ := readToolVersionsEntry(toolVersionsPath, "elixir")
				// Strip -otp-XX suffix: "1.19.5-otp-27" → "1.19.5"
				if idx := strings.Index(v, "-otp-"); idx != -1 {
					return v[:idx]
				}
				return v
			},
		},
		{
			name:     "erlang",
			binary:   "erl",
			source:   ".tool-versions → erlang",
			args:     []string{"-noshell", "-eval", `io:format("~s",[erlang:system_info(otp_release)]),halt().`},
			parseVer: parseErlangVersion,
			compare:  compareMajorGTE,
			readReq: func() string {
				v, _ := readToolVersionsEntry(toolVersionsPath, "erlang")
				return v
			},
		},
		// --- .NET ---
		{
			name:     "dotnet",
			binary:   "dotnet",
			source:   "apps/demo-be-fsharp-giraffe/global.json → sdk.version",
			args:     []string{"--version"},
			parseVer: parseDotnetVersion,
			compare:  compareMajorGTE,
			readReq:  func() string { v, _ := readDotnetVersion(globalJSONPath); return v },
		},
		// --- Clojure ---
		{
			name:      "clojure",
			binary:    "clj",
			source:    "(no config file)",
			args:      []string{"--version"},
			useStderr: false,
			parseVer:  parseClojureVersion,
			compare:   compareExact,
			readReq:   noReq,
		},
		// --- Dart/Flutter ---
		{
			name:     "dart",
			binary:   "dart",
			source:   "apps/demo-fe-dart-flutterweb/pubspec.yaml → environment.sdk",
			args:     []string{"--version"},
			parseVer: parseDartVersion,
			compare:  compareGTE,
			readReq:  func() string { v, _ := readDartSDKVersion(pubspecPath); return v },
		},
		{
			name:     "flutter",
			binary:   "flutter",
			source:   "(no config file)",
			args:     []string{"--version"},
			parseVer: parseFlutterVersion,
			compare:  compareExact,
			readReq:  noReq,
		},
		// --- Infrastructure ---
		{
			name:     "docker",
			binary:   "docker",
			source:   "(no config file)",
			args:     []string{"--version"},
			parseVer: parseDockerVersion,
			compare:  compareExact,
			readReq:  noReq,
		},
		{
			name:     "jq",
			binary:   "jq",
			source:   "(no config file)",
			args:     []string{"--version"},
			parseVer: parseJqVersion,
			compare:  compareExact,
			readReq:  noReq,
		},
	}
}
