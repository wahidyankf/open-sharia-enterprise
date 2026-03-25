# Technical Documentation: ayokoding-web CI and Quality Gate Standardization

## Current State

### Nx Target Configuration (`project.json`)

ayokoding-web declares these targets:

| Target             | Command                                   | Cache           | Inputs                                   |
| ------------------ | ----------------------------------------- | --------------- | ---------------------------------------- |
| `codegen`          | `echo 'No codegen needed...'`             | inherited       | default                                  |
| `dev`              | `next dev --port 3101`                    | no              | —                                        |
| `build`            | `next build`                              | yes             | default                                  |
| `start`            | `next start --port 3101`                  | no              | —                                        |
| `typecheck`        | `tsc --noEmit`                            | yes (inherited) | default                                  |
| `lint`             | `npx oxlint@latest .`                     | yes (inherited) | default                                  |
| `test:unit`        | `npx vitest run`                          | yes             | `default` + Gherkin specs                |
| `test:quick`       | vitest + coverage + link check (parallel) | yes (inherited) | **default only (missing Gherkin specs)** |
| `test:integration` | `npx vitest run --project integration`    | no              | default                                  |

### CI Workflows

**PR Quality Gate** (`pr-quality-gate.yml`):

```yaml
# Current — runs all three gates (correct)
- name: Run typecheck
  run: npx nx affected -t typecheck
- name: Run lint
  run: npx nx affected -t lint
- name: Run test:quick
  run: npx nx affected -t test:quick
```

This is already well-structured with named steps. Improvement opportunity: add markdown linting as a separate named step.

**Scheduled Workflow** (`test-and-deploy-ayokoding-web.yml`):

```
Jobs: unit → e2e → detect-changes → deploy
                                      ↓
                              (needs: unit, e2e)
```

Missing: `integration` job between `unit` and `deploy`.

### Testing Architecture

```
specs/apps/ayokoding-web/**/*.feature
        ↓
┌───────────────────────────────────┐
│ test:unit (vitest)                │
│ ├── unit project (Node.js env)   │  BE/tRPC step tests
│ └── unit-fe project (jsdom env)  │  FE component step tests
│                                   │
│ Coverage: 80% lines (rhino-cli)  │
│ Link check: ayokoding-cli        │
└───────────────────────────────────┘
        ↓ (composed inline in test:quick)

┌───────────────────────────────────┐
│ test:integration (vitest)         │
│ └── integration project           │  MSW / in-process mocks
│     cache: false                  │
└───────────────────────────────────┘
        ↓ (not in any CI workflow)

┌───────────────────────────────────┐
│ test:e2e (Playwright)             │
│ ├── ayokoding-web-be-e2e          │  tRPC API tests
│ └── ayokoding-web-fe-e2e          │  UI tests
│     Runs against Docker compose   │
└───────────────────────────────────┘
```

## Target State

### Change 1: Fix nx-targets.md Tag Table

In `governance/development/infra/nx-targets.md`, update the Current Project Tags table:

```diff
- | `ayokoding-web`           | `["type:app", "platform:hugo", "domain:ayokoding"]`                     |
+ | `ayokoding-web`           | `["type:app", "platform:nextjs", "lang:ts", "domain:ayokoding"]`        |
```

### Change 2: Add test:integration to Scheduled CI

Add a new `integration` job to `test-and-deploy-ayokoding-web.yml`:

```yaml
integration:
  name: Integration Tests
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    -  # Volta + Node setup (same as unit job)
    - run: npm ci
    - name: Run integration tests
      run: npx nx run ayokoding-web:test:integration
```

Update the `deploy` job dependency:

```diff
  deploy:
-   needs: [unit, e2e, detect-changes]
+   needs: [unit, integration, e2e, detect-changes]
```

### Change 3: Add Gherkin Spec Inputs to test:quick

In `apps/ayokoding-web/project.json`, add explicit inputs to `test:quick`:

```diff
  "test:quick": {
    "executor": "nx:run-commands",
+   "inputs": ["default", "{workspaceRoot}/specs/apps/ayokoding-web/**/*.feature"],
    "dependsOn": ["ayokoding-cli:build"],
```

This mirrors the `test:unit` inputs declaration and ensures cache invalidation on spec changes.

### Change 4: Improve PR Quality Gate Step Names (Optional)

The current step names in `pr-quality-gate.yml` are already reasonable. This is a low-priority polish item — verify current names are clear and add `name:` annotations if any steps lack them.

## Risks and Mitigations

| Risk                                 | Likelihood | Impact | Mitigation                                                        |
| ------------------------------------ | ---------- | ------ | ----------------------------------------------------------------- |
| Integration tests add CI time        | Medium     | Low    | Run in parallel with unit and e2e jobs                            |
| Integration tests flaky in CI        | Low        | Medium | ayokoding-web uses MSW (in-process), not real DB — deterministic  |
| Cache input change triggers rebuilds | Certain    | Low    | One-time cache miss, subsequent runs benefit from correct caching |

## Out of Scope

- Changing the 80% coverage threshold (it exceeds the standard — no action needed)
- Adding new test targets or test levels
- Restructuring the vitest project configuration
- Changing the E2E test architecture
- Modifying the pre-push hook (already correct)
