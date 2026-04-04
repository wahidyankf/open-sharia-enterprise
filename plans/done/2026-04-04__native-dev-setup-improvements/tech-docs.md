# Technical Documentation: Native Dev Setup Improvements

## Architecture Overview

All improvements target `apps/rhino-cli/` (Go) and repository root config files. No new
applications or libraries are introduced. The changes extend the existing `doctor` package and
`env` command.

```
apps/rhino-cli/
├── cmd/
│   ├── doctor.go           # CLI command — add --fix, --scope flags
│   └── env.go              # CLI command — add "init" subcommand
├── internal/
│   └── doctor/
│       ├── tools.go        # Tool definitions — remove Hugo, add Playwright, add install commands
│       ├── checker.go      # Check logic — add scope filtering, fix runner
│       ├── checker_test.go # Tests
│       ├── fixer.go        # NEW — auto-install logic
│       ├── fixer_test.go   # NEW — fixer tests
│       ├── reporter.go     # Reporter — update summary for scope
│       └── reporter_test.go
```

## Improvement 1: `doctor --fix`

### Design

Add an `installCmd` field to `toolDef` that returns the shell command(s) to install the tool on
the current platform. The `--fix` flag triggers a second pass after `CheckAll`: for each tool with
`StatusMissing`, execute its `installCmd`. Platform detection uses `runtime.GOOS`.

```go
type toolDef struct {
    // ... existing fields ...
    installCmd func(required string, platform string) []installStep // nil = cannot auto-install
}

type installStep struct {
    description string   // "Install Go via Homebrew"
    command     string   // "brew"
    args        []string // ["install", "go"]
}
```

### Install commands per tool (macOS + Ubuntu)

| Tool           | macOS                                        | Ubuntu/Linux                                                                                                                                           |
| -------------- | -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| git            | `xcode-select --install`                     | `sudo apt-get install -y git`                                                                                                                          |
| volta          | `curl https://get.volta.sh \| bash`          | `curl https://get.volta.sh \| bash` (same)                                                                                                             |
| node           | `volta install node@{required}`              | `volta install node@{required}` (same)                                                                                                                 |
| npm            | `volta install npm@{required}`               | `volta install npm@{required}` (same)                                                                                                                  |
| java           | `sdk install java {required}-tem`            | `sdk install java {required}-tem` (same, requires SDKMAN)                                                                                              |
| maven          | `sdk install maven`                          | `sdk install maven` (same)                                                                                                                             |
| golang         | `brew install go`                            | Download tarball from go.dev (`curl -L https://go.dev/dl/go{req}.linux-amd64.tar.gz \| sudo tar -xz -C /usr/local`) — apt `golang-go` is too old       |
| python         | `brew install pyenv && pyenv install {req}`  | Install pyenv deps + `curl https://pyenv.run \| bash && pyenv install {req}`                                                                           |
| rust           | `curl ...rustup.rs \| sh -s -- -y`           | `curl ...rustup.rs \| sh -s -- -y` (same)                                                                                                              |
| cargo-llvm-cov | `cargo install cargo-llvm-cov`               | `cargo install cargo-llvm-cov` (same)                                                                                                                  |
| elixir         | `asdf plugin add elixir && asdf install ...` | `asdf plugin add elixir && asdf install ...` (same)                                                                                                    |
| erlang         | `asdf plugin add erlang && asdf install ...` | Install build deps first + `asdf plugin add erlang && asdf install ...`                                                                                |
| dotnet         | `brew install dotnet`                        | `sudo snap install dotnet-sdk --classic --channel=10.0` (self-contained; apt requires Microsoft APT feed setup)                                        |
| clojure        | `brew install clojure/tools/clojure`         | `curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh && chmod +x linux-install.sh && sudo ./linux-install.sh` |
| dart/flutter   | `brew install --cask flutter`                | `sudo snap install flutter --classic`                                                                                                                  |
| docker         | Print: "Install Docker Desktop"              | `sudo apt-get install -y docker.io docker-compose-v2`                                                                                                  |
| jq             | `brew install jq`                            | `sudo apt-get install -y jq`                                                                                                                           |
| playwright     | `npx playwright install`                     | `npx playwright install && npx playwright install-deps` (system libs needed)                                                                           |

**Ubuntu-specific prerequisites** (install before toolchain setup):

```bash
sudo apt-get update && sudo apt-get install -y \
  build-essential autoconf curl git unzip zip \
  libncurses-dev libssl-dev libreadline-dev \
  libsqlite3-dev libbz2-dev libffi-dev zlib1g-dev
```

These are needed by pyenv (Python compilation), asdf-erlang (Erlang compilation), and other
tools that compile from source on Linux.

### Dependency ordering

Some tools depend on others (e.g., node requires volta, java requires SDKMAN). The fix loop must
process tools in the order defined in `buildToolDefs()` (which is already dependency-ordered:
volta before node, etc.).

**Shell restart caveat**: Volta (`curl | bash`), SDKMAN (`curl | bash`), and rustup all modify
shell profile files (`~/.zshrc`, `~/.bashrc`, or `~/.cargo/env`) to add themselves to `PATH`. After installing any of them,
the fixer must `source` the relevant profile or the subsequent tool installs (node via volta,
java via sdk) will fail because the binary isn't in the current shell's PATH. The fixer should:

1. After installing Volta: `source ~/.zshrc` (or detect shell and source accordingly)
2. After installing SDKMAN: `source "$HOME/.sdkman/bin/sdkman-init.sh"`
3. After installing rustup: `source "$HOME/.cargo/env"`

If sourcing fails or the tool still isn't in PATH, print a message asking the developer to
restart their shell and re-run `doctor --fix`.

### Error handling

- If an install step fails, log the error, mark the tool as `failed`, continue to the next tool
- At the end, print a summary: `Fixed: 3, Failed: 1, Already OK: 15`
- Return exit code 1 if any tool remains missing/failed after fix attempt

### Gherkin specs

rhino-cli enforces `spec-coverage validate` in pre-push. Existing specs:

- `specs/apps/rhino/cli/gherkin/doctor.feature` — 4 scenarios (all-ok, missing, warning, JSON)
- `specs/apps/rhino/cli/gherkin/env-backup.feature` and `env-restore.feature` — existing env specs

New features require new Gherkin scenarios:

- **`doctor --fix`**: Add scenarios to `doctor.feature` (fix missing tool, skip already-installed,
  fix failure handling)
- **`doctor --scope`**: Add scenarios to `doctor.feature` (minimal scope, full scope default)
- **`env init`**: Create `specs/apps/rhino/cli/gherkin/env-init.feature`

### `--dry-run` mode

`doctor --fix --dry-run` prints what would be installed without executing any commands. This is
the Terraform "plan" equivalent — it gives confidence before applying changes.

Implementation: the fix loop checks `opts.DryRun` before executing each install command. When
true, it prints the command and skips execution:

```go
if opts.DryRun {
    fmt.Printf("Would install: %s via %s %s\n", def.name, step.command, strings.Join(step.args, " "))
    continue
}
```

The `--dry-run` flag is only effective with `--fix`. Running `doctor --dry-run` without `--fix`
is equivalent to plain `doctor` (read-only check).

### Architectural decision

The choice to use native package managers instead of Terraform, Ansible, or Docker Dev Containers
is recorded in
[Native-First Toolchain Management](../../../governance/development/workflow/native-first-toolchain.md).
Key insight: package managers already guarantee idempotency, and the installed binaries ARE the
state — no external state file needed.

### Testing strategy

- Unit tests: mock `CommandRunner` to simulate missing tools, verify correct install commands
  are generated
- Integration test: run `doctor --fix` on a system where all tools are already installed, verify
  no install commands are executed (idempotency)

## Improvement 2: Remove Hugo

### Changes

1. **`tools.go`**: Remove the Hugo `toolDef` entry from `buildToolDefs()`
2. **`tools.go`**: Remove `vercelJSONPath` variable (no longer needed)
3. **`checker.go`**: Remove `readHugoVersion`, `parseHugoVersion` (line ~322), `vercelJSON` type
4. **`checker_test.go`**: Remove Hugo-related test cases (`TestParseHugoVersion`, `TestReadHugoVersion`, Hugo mock data)
5. **`reporter_test.go`**: Remove Hugo `ToolCheck` entry, remove "hugo" from name list, update count 19 → 18
   5b. **`cmd/doctor_test.go`**: Remove "hugo" from `makeAllOKChecks()`, update count 19 → 18 in
   `theJSONListsEveryCheckedToolWithItsStatus()`, remove Hugo-specific test scenarios
   5c. **`cmd/doctor.go`**: Remove Hugo from the Long help string tool list (line ~26)
6. **Workflow doc**: Remove Phase 11 (Hugo) from
   `governance/workflows/infra/development-environment-setup.md`
7. **Workflow doc**: Update tool inventory table (remove row 8, renumber)
8. **Workflow doc**: Update minimal scope table if Hugo was referenced

### Risk

None. Hugo is unused. No active project references it.

## Improvement 3: `rhino-cli env init`

### Design

Add `init` subcommand to the existing `env` command group. The command:

1. Walks `infra/dev/` looking for `.env.example` files
2. For each, creates a `.env` file in the same directory (if not exists)
3. Copies the content verbatim

```go
// cmd/env_init.go
var envInitCmd = &cobra.Command{
    Use:   "init",
    Short: "Create .env files from .env.example templates",
    Long:  `Finds all .env.example files in infra/dev/ and copies them to .env
in the same directory. Existing .env files are not overwritten unless --force is used.`,
}
```

### Mapping

The `.env.example` files are co-located with their `.env` targets:

```
infra/dev/a-demo-be-golang-gin/.env.example → infra/dev/a-demo-be-golang-gin/.env
infra/dev/organiclever/.env.example         → infra/dev/organiclever/.env
```

No path transformation needed — just replace `.env.example` with `.env` in the same directory.

### Flags

- `--force`: Overwrite existing `.env` files (default: false)

### Output

```
Created: infra/dev/a-demo-be-golang-gin/.env (from .env.example)
Skipped: infra/dev/organiclever/.env (already exists, use --force to overwrite)
Created: infra/dev/ayokoding-web/.env (from .env.example)
...
Summary: 15 created, 3 skipped
```

## Improvement 4: Playwright in Doctor

### Design

Add a new `toolDef` for Playwright. Unlike other tools, Playwright isn't a CLI binary — it's
browser binaries in a cache directory. The check should:

1. Verify `npx playwright --version` works (Playwright npm package installed)
2. Check if browser binaries exist in the platform-specific Playwright cache directory:
   - macOS: `~/Library/Caches/ms-playwright/`
   - Linux: `~/.cache/ms-playwright/`

### Implementation

```go
{
    name:     "playwright",
    binary:   "npx",
    source:   "node_modules (npx playwright)",
    args:     []string{"playwright", "--version"},
    parseVer: parsePlaywrightVersion, // output is "Version 1.58.2", not bare version
    compare:  comparePlaywright,      // custom: also checks browser cache dir
    readReq:  noReq,                  // version comes from npm, not a config file
}
```

**Version parsing**: `npx playwright --version` outputs `"Version 1.58.2\n"` (with "Version "
prefix), so `parseTrimVersion` won't work. Add a dedicated parser:

```go
func parsePlaywrightVersion(output string) string {
    return parseLineWord(output, "Version ", 1, "")
}
```

The `comparePlaywright` function checks for the existence of browser directories after version
parsing succeeds. If the CLI works but browsers are missing, return `StatusWarning` with a helpful
note.

### Browser cache detection

```go
func checkPlaywrightBrowsers() bool {
    home, _ := os.UserHomeDir()
    // Platform-specific cache directory
    var cacheDir string
    if runtime.GOOS == "darwin" {
        cacheDir = filepath.Join(home, "Library", "Caches", "ms-playwright")
    } else {
        cacheDir = filepath.Join(home, ".cache", "ms-playwright")
    }
    entries, err := os.ReadDir(cacheDir)
    if err != nil {
        return false
    }
    // At least chromium should be present
    for _, e := range entries {
        if strings.HasPrefix(e.Name(), "chromium-") {
            return true
        }
    }
    return false
}
```

### `--fix` integration

The install command for Playwright: `npx playwright install`.

## Improvement 5: Brewfile

### Content

```ruby
# Brewfile — Homebrew dependencies for open-sharia-enterprise
# Run: brew bundle
# Note: This covers Homebrew-installable tools only.
# Tools managed by other installers (Volta, SDKMAN, rustup, asdf, pyenv)
# are handled by rhino-cli doctor --fix.

brew "go"
brew "jq"
brew "dotnet"
brew "pyenv"
brew "asdf"
brew "clojure/tools/clojure"
cask "flutter"  # Flutter is a Homebrew cask, not a formula
```

### Location

Repository root (`Brewfile`). macOS only — Ubuntu does not use Homebrew. The `Brewfile` is
harmless on Linux (no `brew` command, so `brew bundle` simply isn't available).

### Gitignore

`Brewfile.lock.json` should be added to `.gitignore` (Homebrew generates this on `brew bundle`).

## Improvement 6: `doctor --scope minimal`

### Design

Add a `Scope` field to `CheckOptions` in `checker.go` and filter `buildToolDefs()` output
based on scope. All scope logic lives in `checker.go` (no separate `scope.go` file needed — it's
a single type, a constant set, and a filter in `CheckAll`).

```go
type Scope string

const (
    ScopeFull    Scope = "full"
    ScopeMinimal Scope = "minimal"
)

var minimalTools = map[string]bool{
    "git": true, "volta": true, "node": true, "npm": true,
    "golang": true, "docker": true, "jq": true,
}
```

### CLI flag

```go
doctorCmd.Flags().String("scope", "full", "tool scope: full or minimal")
```

### Reporter update

The summary line should include scope when not `full`:

```
Summary: 7/7 tools OK (scope: minimal)
```

## Improvement 7: Fix Postinstall Caching

### Change

In `package.json`:

```diff
- "doctor": "nx run rhino-cli:build --skip-nx-cache && ./apps/rhino-cli/dist/rhino-cli doctor"
+ "doctor": "nx run rhino-cli:build && ./apps/rhino-cli/dist/rhino-cli doctor"
```

### Why `--skip-nx-cache` was there

Likely to ensure doctor always runs the latest rhino-cli code during development. But Nx cache
invalidation is based on source file hashes — if `apps/rhino-cli/` source changes, Nx already
invalidates the cache. The `--skip-nx-cache` is redundant.

### Risk

Low. If the cache is stale (shouldn't happen with Nx's hash-based invalidation), `npm run doctor`
would use the old binary. Developer can always `nx run rhino-cli:build --skip-nx-cache` manually.

## Improvement 8: Pin Rust and Flutter Versions

### Rust

`apps/a-demo-be-rust-axum/rust-toolchain.toml` already exists with `channel = "stable"` but no
MSRV. `Cargo.toml` has `edition = "2021"` but no `rust-version` field.

Add `rust-version` (MSRV) to `Cargo.toml`:

```toml
# In Cargo.toml [package] section
rust-version = "1.80"
```

Update `tools.go`:

```go
{
    name:     "rust",
    binary:   "rustc",
    source:   "apps/a-demo-be-rust-axum/Cargo.toml → rust-version",
    args:     []string{"--version"},
    parseVer: parseRustVersion,
    compare:  compareGTE,  // was: compareExact
    readReq:  func() string { v, _ := readRustVersion(cargoTomlPath); return v },
}
```

### Flutter

`apps/a-demo-fe-dart-flutterweb/pubspec.yaml` currently has `environment.sdk: ^3.11.1` but
no `environment.flutter` constraint. Add one:

```yaml
environment:
  sdk: ^3.11.1
  flutter: ">=3.41.0"
```

Update `tools.go`:

```go
{
    name:     "flutter",
    binary:   "flutter",
    source:   "apps/a-demo-fe-dart-flutterweb/pubspec.yaml → environment.flutter",
    args:     []string{"--version"},
    parseVer: parseFlutterVersion,
    compare:  compareGTE,  // was: compareExact
    readReq:  func() string { v, _ := readFlutterVersion(pubspecPath); return v },
}
```

## Git Worktree Compatibility

All improvements must work correctly from git worktrees. This repo uses worktrees heavily for
AI agent isolation (`.claude/worktrees/`).

### How it works today

`findGitRoot()` in `apps/rhino-cli/cmd/helpers.go` walks up from `cwd` looking for `.git`.
In a worktree, `.git` is a **file** (not a directory) containing
`gitdir: /path/to/main/.git/worktrees/<name>`. `os.Stat` succeeds for both files and
directories, so `findGitRoot()` already works correctly in worktrees.

**Verified**: `npm run doctor` runs successfully from a worktree (19/19 tools OK).

### What each improvement needs for worktree support

| Improvement         | Worktree concern                                              | Resolution                                          |
| ------------------- | ------------------------------------------------------------- | --------------------------------------------------- |
| `doctor --fix`      | None — installs tools globally, not per-worktree              | Works as-is                                         |
| Hugo removal        | None — removes a check, no path changes                       | Works as-is                                         |
| `env init`          | Scans `infra/dev/` relative to repo root from `findGitRoot()` | Works as-is — worktree contains full working tree   |
| Playwright check    | Checks global cache dir (`~/.cache/` or `~/Library/Caches/`)  | Works as-is — not repo-relative                     |
| Brewfile            | Lives at repo root, used manually                             | Works as-is — `brew bundle` runs from any directory |
| `doctor --scope`    | Filters the existing tool list                                | Works as-is                                         |
| Postinstall caching | `npm run doctor` invokes Nx which uses `nx.json` at repo root | Works as-is — Nx resolves workspace root            |
| Version pinning     | Reads config files relative to `findGitRoot()`                | Works as-is — config files exist in worktree        |

### Key invariant

`findGitRoot()` returns the worktree root (the directory containing the `.git` file), which
IS the working tree root. All paths in `buildToolDefs()` are constructed as
`filepath.Join(repoRoot, "apps", ...)` — this resolves correctly in both the main repo and
worktrees because worktrees contain the full file tree.

### Version reader for Rust MSRV

```go
func readRustVersion(cargoTomlPath string) (string, error) {
    data, err := os.ReadFile(cargoTomlPath)
    if err != nil {
        return "", err
    }
    for _, line := range strings.Split(string(data), "\n") {
        trimmed := strings.TrimSpace(line)
        if strings.HasPrefix(trimmed, "rust-version") {
            // rust-version = "1.80"
            parts := strings.SplitN(trimmed, "=", 2)
            if len(parts) == 2 {
                ver := strings.TrimSpace(parts[1])
                ver = strings.Trim(ver, "\"")
                return ver, nil
            }
        }
    }
    return "", nil // no MSRV specified
}
```

### Version reader for Flutter

```go
func readFlutterVersion(pubspecPath string) (string, error) {
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
                break
            }
            if strings.HasPrefix(trimmed, "flutter:") {
                ver := strings.TrimSpace(strings.TrimPrefix(trimmed, "flutter:"))
                ver = strings.TrimPrefix(ver, "^")
                ver = strings.TrimPrefix(ver, ">=")
                return strings.TrimSpace(ver), nil
            }
        }
    }
    return "", nil // no flutter constraint
}
```
