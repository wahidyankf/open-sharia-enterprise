# javaproject-cli

Standalone Go CLI tool that validates Java package null-safety annotations.

Checks that every Java package directory under a given source root contains a
`package-info.java` with the required annotation (default: `@NullMarked`).

## Usage

```sh
javaproject-cli [flags] <source-root>
```

### Flags

| Flag          | Default      | Description                                                     |
| ------------- | ------------ | --------------------------------------------------------------- |
| `-annotation` | `NullMarked` | Annotation name to require in `package-info.java` (without `@`) |
| `-o`          | `text`       | Output format: `text`, `json`, or `markdown`                    |
| `-v`          | `false`      | Verbose output                                                  |
| `-q`          | `false`      | Quiet — suppress the summary line when there are no violations  |

### Exit codes

| Code | Meaning                      |
| ---- | ---------------------------- |
| 0    | All packages valid           |
| 1    | One or more violations found |
| 2    | Usage error or I/O failure   |

## Examples

```sh
# Validate from workspace root (Nx usage)
./apps/javaproject-cli/dist/javaproject-cli apps/organiclever-be/src/main/java

# Use a different annotation
javaproject-cli -annotation NonNullByDefault src/main/java

# Output as JSON
javaproject-cli -o json src/main/java

# Output as markdown report
javaproject-cli -o markdown src/main/java
```

## Violation types

| Type                   | Meaning                                                |
| ---------------------- | ------------------------------------------------------ |
| `missing_package_info` | Directory has `.java` files but no `package-info.java` |
| `missing_annotation`   | `package-info.java` exists but lacks `@<annotation>`   |

## Development

```sh
# Build
nx build javaproject-cli

# Test
nx run javaproject-cli:test:quick

# Lint
nx lint javaproject-cli
```

## Why this exists

`organiclever-be`'s `typecheck` target needs to run the Java null-safety check.
Keeping this as a focused, standalone binary prevents unrelated changes to `rhino-cli`
from triggering a rebuild cascade across all `organiclever-*` projects.
