# Technical Documentation

Exact changes for each file. Changes are copy-paste ready. Only the diffs are shown — surrounding
context is included only where needed to locate the insertion point.

---

## nx.json

**Two changes: replace `targetDefaults`, remove `tasksRunnerOptions`.**

`targetDefaults` with `cache: true/false` is the modern Nx mechanism for declaring caching.
`tasksRunnerOptions.cacheableOperations` is the old pre-Nx-15 equivalent — redundant and legacy.
Since the current `tasksRunnerOptions` uses the default runner (`nx/tasks-runners/default`), the
entire block can be removed with no behavioral change.

Current `targetDefaults`:

```json
"targetDefaults": {
  "build": {
    "dependsOn": ["^build"],
    "outputs": ["{projectRoot}/dist"],
    "cache": true
  },
  "test": {
    "dependsOn": ["build"],
    "cache": true
  },
  "lint": {
    "cache": true
  }
}
```

Target `targetDefaults`:

```json
"targetDefaults": {
  "build": {
    "dependsOn": ["^build"],
    "outputs": ["{projectRoot}/dist"],
    "cache": true
  },
  "typecheck": {
    "cache": true
  },
  "lint": {
    "cache": true
  },
  "test:quick": {
    "cache": true
  },
  "test:unit": {
    "cache": true
  },
  "test:integration": {
    "cache": false
  },
  "test:e2e": {
    "cache": false
  }
}
```

Remove `tasksRunnerOptions` entirely:

```json
// DELETE this entire block from nx.json:
"tasksRunnerOptions": {
  "default": {
    "runner": "nx/tasks-runners/default",
    "options": {
      "cacheableOperations": ["build", "test", "lint"]
    }
  }
}
```

---

## apps/ayokoding-cli/project.json

**Add `lint` target** (after `install`):

```json
"lint": {
  "executor": "nx:run-commands",
  "options": {
    "command": "go vet ./...",
    "cwd": "apps/ayokoding-cli"
  }
}
```

Full updated file:

```json
{
  "name": "ayokoding-cli",
  "sourceRoot": "apps/ayokoding-cli",
  "projectType": "application",
  "targets": {
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "go build -o dist/ayokoding-cli",
        "cwd": "apps/ayokoding-cli"
      },
      "outputs": ["{projectRoot}/dist"]
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "command": "go test ./...",
        "cwd": "apps/ayokoding-cli"
      }
    },
    "run": {
      "executor": "nx:run-commands",
      "options": {
        "command": "go run main.go",
        "cwd": "apps/ayokoding-cli"
      }
    },
    "install": {
      "executor": "nx:run-commands",
      "options": {
        "command": "go mod tidy",
        "cwd": "apps/ayokoding-cli"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "go vet ./...",
        "cwd": "apps/ayokoding-cli"
      }
    }
  }
}
```

---

## apps/rhino-cli/project.json

**Add `lint` target** (after `install`):

```json
"lint": {
  "executor": "nx:run-commands",
  "options": {
    "command": "CGO_ENABLED=0 go vet ./...",
    "cwd": "apps/rhino-cli"
  }
}
```

Full updated file:

```json
{
  "name": "rhino-cli",
  "sourceRoot": "apps/rhino-cli",
  "projectType": "application",
  "targets": {
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "CGO_ENABLED=0 go build -o dist/rhino-cli",
        "cwd": "apps/rhino-cli"
      },
      "outputs": ["{projectRoot}/dist"]
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "command": "CGO_ENABLED=0 go test ./...",
        "cwd": "apps/rhino-cli"
      }
    },
    "run": {
      "executor": "nx:run-commands",
      "options": {
        "command": "go run main.go",
        "cwd": "apps/rhino-cli"
      }
    },
    "install": {
      "executor": "nx:run-commands",
      "options": {
        "command": "go mod tidy",
        "cwd": "apps/rhino-cli"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "CGO_ENABLED=0 go vet ./...",
        "cwd": "apps/rhino-cli"
      }
    }
  }
}
```

---

## apps/ayokoding-web/project.json

**Add `lint` target** (after `test:quick`):

```json
"lint": {
  "executor": "nx:run-commands",
  "options": {
    "command": "markdownlint-cli2 \"content/**/*.md\"",
    "cwd": "apps/ayokoding-web"
  }
}
```

Full updated file:

```json
{
  "name": "ayokoding-web",
  "sourceRoot": "apps/ayokoding-web",
  "projectType": "application",
  "targets": {
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "hugo server -D",
        "cwd": "apps/ayokoding-web"
      }
    },
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "./build.sh",
        "cwd": "apps/ayokoding-web"
      },
      "outputs": ["{projectRoot}/public"]
    },
    "clean": {
      "executor": "nx:run-commands",
      "options": {
        "command": "rm -rf public resources .hugo_build.lock",
        "cwd": "apps/ayokoding-web"
      }
    },
    "run-pre-commit": {
      "executor": "nx:run-commands",
      "options": {
        "commands": [
          "nx build ayokoding-cli",
          "./apps/ayokoding-cli/dist/ayokoding-cli titles update --quiet",
          "./apps/ayokoding-cli/dist/ayokoding-cli nav regen --quiet"
        ],
        "parallel": false
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "command": "./build.sh",
        "cwd": "apps/ayokoding-web"
      },
      "outputs": ["{projectRoot}/public"]
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "markdownlint-cli2 \"content/**/*.md\"",
        "cwd": "apps/ayokoding-web"
      }
    }
  }
}
```

---

## apps/oseplatform-web/project.json

**Add `test:quick`, add `lint`, fix `clean`:**

Full updated file:

```json
{
  "name": "oseplatform-web",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "sourceRoot": "apps/oseplatform-web",
  "projectType": "application",
  "targets": {
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "hugo server --buildDrafts --buildFuture",
        "cwd": "apps/oseplatform-web"
      }
    },
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "bash build.sh",
        "cwd": "apps/oseplatform-web"
      },
      "outputs": ["{projectRoot}/public"]
    },
    "clean": {
      "executor": "nx:run-commands",
      "options": {
        "command": "rm -rf public resources .hugo_build.lock",
        "cwd": "apps/oseplatform-web"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "command": "bash build.sh",
        "cwd": "apps/oseplatform-web"
      },
      "outputs": ["{projectRoot}/public"]
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "markdownlint-cli2 \"content/**/*.md\"",
        "cwd": "apps/oseplatform-web"
      }
    }
  },
  "tags": ["type:app", "platform:hugo"]
}
```

---

## apps/organiclever-web/project.json

**Add `typecheck` and `test:quick`:**

Full updated file:

```json
{
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "name": "organiclever-web",
  "sourceRoot": "apps/organiclever-web/src",
  "projectType": "application",
  "targets": {
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "next dev",
        "cwd": "{projectRoot}"
      }
    },
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "next build",
        "cwd": "{projectRoot}"
      },
      "outputs": ["{projectRoot}/.next"]
    },
    "start": {
      "executor": "nx:run-commands",
      "options": {
        "command": "next start",
        "cwd": "{projectRoot}"
      }
    },
    "typecheck": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc --noEmit",
        "cwd": "{projectRoot}"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc --noEmit",
        "cwd": "{projectRoot}"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "next lint",
        "cwd": "{projectRoot}"
      }
    }
  },
  "tags": ["type:app", "platform:nextjs", "domain:organiclever"]
}
```

**Note**: `test:quick` runs `tsc --noEmit` only because no unit tests exist. When unit tests are
added (Jest or Vitest), update `test:quick` to also run the test suite.

---

## apps/organiclever-be/project.json

**Rename `serve`→`dev`, rename `test`→`test:unit`, add `test:quick`, add `start`, add `outputs`
to `build`:**

Full updated file:

```json
{
  "name": "organiclever-be",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "sourceRoot": "apps/organiclever-be/src",
  "projectType": "application",
  "targets": {
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mvn clean package -DskipTests",
        "cwd": "apps/organiclever-be"
      },
      "outputs": ["{projectRoot}/target"]
    },
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mvn spring-boot:run -Dspring-boot.run.profiles=dev",
        "cwd": "apps/organiclever-be"
      }
    },
    "start": {
      "executor": "nx:run-commands",
      "options": {
        "command": "java -jar target/organiclever-be-1.0.0.jar",
        "cwd": "apps/organiclever-be"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mvn test",
        "cwd": "apps/organiclever-be"
      }
    },
    "test:unit": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mvn test",
        "cwd": "apps/organiclever-be"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mvn checkstyle:check",
        "cwd": "apps/organiclever-be"
      }
    }
  },
  "tags": []
}
```

**Notes**:

- `test:quick` and `test:unit` run the same `mvn test` command. This is acceptable: no
  unit/integration test separation exists yet. When Maven Failsafe is configured to separate unit
  (`Surefire`) and integration tests (`Failsafe`), update `test:quick` to run only Surefire tests.
- The JAR filename `organiclever-be-1.0.0.jar` is derived from `pom.xml`
  (`artifactId=organiclever-be`, `version=1.0.0`). Verify if the version changes before committing.

---

## apps/organiclever-app/project.json

**Rename `test`→`test:unit`, add `typecheck`, update `test:quick` with `dependsOn`:**

Full updated file:

```json
{
  "name": "organiclever-app",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "sourceRoot": "apps/organiclever-app/lib",
  "projectType": "application",
  "targets": {
    "install": {
      "executor": "nx:run-commands",
      "options": {
        "command": "export PATH=\"$PATH:$HOME/flutter/bin\" && flutter pub get",
        "cwd": "apps/organiclever-app"
      }
    },
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "export PATH=\"$PATH:$HOME/flutter/bin\" && flutter run -d web-server --web-port=3100 --dart-define=API_BASE_URL=http://localhost:8100/api/v1",
        "cwd": "apps/organiclever-app"
      },
      "dependsOn": ["install"]
    },
    "build:web": {
      "executor": "nx:run-commands",
      "options": {
        "command": "export PATH=\"$PATH:$HOME/flutter/bin\" && flutter build web --dart-define=API_BASE_URL=https://api.organiclever.com/api/v1",
        "cwd": "apps/organiclever-app"
      },
      "outputs": ["{projectRoot}/build/web"]
    },
    "typecheck": {
      "executor": "nx:run-commands",
      "options": {
        "command": "export PATH=\"$PATH:$HOME/flutter/bin\" && flutter analyze",
        "cwd": "apps/organiclever-app"
      }
    },
    "test:unit": {
      "executor": "nx:run-commands",
      "options": {
        "command": "export PATH=\"$PATH:$HOME/flutter/bin\" && flutter test",
        "cwd": "apps/organiclever-app"
      },
      "dependsOn": ["install"]
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "command": "export PATH=\"$PATH:$HOME/flutter/bin\" && flutter test",
        "cwd": "apps/organiclever-app"
      },
      "dependsOn": ["install"]
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "export PATH=\"$PATH:$HOME/flutter/bin\" && flutter analyze",
        "cwd": "apps/organiclever-app"
      }
    }
  },
  "tags": ["type:app", "platform:flutter", "domain:organiclever"]
}
```

**Flutter typecheck note**: `flutter analyze` is Dart's type analysis tool. It combines type
checking and linting into a single step (unlike TypeScript's `tsc --noEmit` + `eslint`). Having
both `typecheck` and `lint` run `flutter analyze` is intentional: they declare different
_purposes_ to Nx, enabling separate caching and invocation, even though the underlying tool is the
same. `test:quick` runs unit tests only (since `flutter analyze` = lint, and lint is run
separately per the standard — no need to double-execute it in `test:quick`).

---

## apps/organiclever-web-e2e/project.json

**Rename all `e2e*` targets, add `lint` and `test:quick`:**

Full updated file:

```json
{
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "name": "organiclever-web-e2e",
  "sourceRoot": "apps/organiclever-web-e2e/tests",
  "projectType": "application",
  "targets": {
    "install": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npm install",
        "cwd": "apps/organiclever-web-e2e"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc --noEmit",
        "cwd": "apps/organiclever-web-e2e"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc --noEmit",
        "cwd": "apps/organiclever-web-e2e"
      }
    },
    "test:e2e": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx playwright test",
        "cwd": "apps/organiclever-web-e2e"
      }
    },
    "test:e2e:ui": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx playwright test --ui",
        "cwd": "apps/organiclever-web-e2e"
      }
    },
    "test:e2e:report": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx playwright show-report",
        "cwd": "apps/organiclever-web-e2e"
      }
    }
  },
  "tags": ["type:e2e", "platform:playwright", "domain:organiclever"]
}
```

---

## apps/organiclever-be-e2e/project.json

**Same pattern as `organiclever-web-e2e`:**

Full updated file:

```json
{
  "name": "organiclever-be-e2e",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "projectType": "application",
  "sourceRoot": "apps/organiclever-be-e2e/tests",
  "targets": {
    "install": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npm install",
        "cwd": "apps/organiclever-be-e2e"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc --noEmit",
        "cwd": "apps/organiclever-be-e2e"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc --noEmit",
        "cwd": "apps/organiclever-be-e2e"
      }
    },
    "test:e2e": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx playwright test",
        "cwd": "apps/organiclever-be-e2e"
      }
    },
    "test:e2e:ui": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx playwright test --ui",
        "cwd": "apps/organiclever-be-e2e"
      }
    },
    "test:e2e:report": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx playwright show-report",
        "cwd": "apps/organiclever-be-e2e"
      }
    }
  },
  "tags": ["type:e2e", "platform:playwright", "domain:organiclever"]
}
```

---

## apps/organiclever-app-web-e2e/project.json

**Same pattern as `organiclever-web-e2e`:**

Full updated file:

```json
{
  "name": "organiclever-app-web-e2e",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "projectType": "application",
  "sourceRoot": "apps/organiclever-app-web-e2e/tests",
  "targets": {
    "install": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npm install",
        "cwd": "apps/organiclever-app-web-e2e"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc --noEmit",
        "cwd": "apps/organiclever-app-web-e2e"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc --noEmit",
        "cwd": "apps/organiclever-app-web-e2e"
      }
    },
    "test:e2e": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx playwright test",
        "cwd": "apps/organiclever-app-web-e2e"
      }
    },
    "test:e2e:ui": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx playwright test --ui",
        "cwd": "apps/organiclever-app-web-e2e"
      }
    },
    "test:e2e:report": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx playwright show-report",
        "cwd": "apps/organiclever-app-web-e2e"
      }
    }
  },
  "tags": ["type:e2e", "platform:playwright", "domain:organiclever"]
}
```

---

## Design Decisions

### Why `go vet` for Go lint?

`go vet` is the standard Go static analysis tool — it detects suspicious constructs, incorrect
format strings, and other common errors. It ships with the Go toolchain (no extra install). More
powerful linters (e.g., `golangci-lint`) can be added later as a lint enhancement; the standard
requires at least a `lint` target that exits non-zero on violations.

### Why `markdownlint-cli2` for Hugo site lint?

Hugo sites contain markdown content — the "code" that `lint` is responsible for checking. The
workspace already uses `markdownlint-cli2` (v0.20.0) for `npm run lint:md`. Using the same tool
per-project makes the lint target consistent with the workspace-level lint command.

### Why `tsc --noEmit` for Playwright E2E lint?

The E2E projects are TypeScript-only with no ESLint configuration. TypeScript type checking
(`tsc --noEmit`) is the most lightweight lint available without adding a new devDependency. If
ESLint is added to the E2E projects later, update `lint` to run ESLint and keep `test:quick` as
the linter invocation.

### Why `test:quick` = `tsc --noEmit` for organiclever-web?

No unit tests exist. Per the standard: "The target must always exist — even if it only runs the
type checker." TypeScript type checking is fast (seconds) and catches real errors. When unit tests
(Jest/Vitest) are added, update `test:quick` to include them alongside `tsc --noEmit`.

### Why `test:quick` and `test:unit` both run `mvn test` in organiclever-be?

No Maven Failsafe / test tagging exists to separate unit from integration tests. Running `mvn test`
for both is correct today. When tests grow, configure Surefire for unit tests only in `test:quick`
and add Failsafe for integration tests in `test:integration`.

### Flutter typecheck = lint: intentional duplication

Dart's analyzer (`flutter analyze`) is both a type checker and a linter — it cannot be split.
Declaring both `typecheck` and `lint` with the same command satisfies the governance standard
("Run the type checker without emitting artifacts" for statically typed projects) while maintaining
semantic clarity about each target's purpose. Both results are cached independently by Nx.

---

## .husky/pre-push

**Replace the entire file content:**

Current:

```sh
#!/usr/bin/env sh

# Run affected tests
npx nx affected -t test:quick
```

Target:

```sh
#!/usr/bin/env sh

# Run affected quality gates: type checking, linting, and fast tests
npx nx affected -t typecheck
npx nx affected -t lint
npx nx affected -t test:quick
```

**Ordering**: `typecheck` → `lint` → `test:quick`. Static analysis first (fastest feedback, no
side effects), test execution last. If type errors exist, the developer sees them immediately
without waiting for tests.

**Nx skip behavior**: Projects without a `typecheck` target are silently skipped by
`nx affected -t typecheck`. The hook is safe to deploy even before all project.json files have
the target — it simply has no effect for projects missing it.
