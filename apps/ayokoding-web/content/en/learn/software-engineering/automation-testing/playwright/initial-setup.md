---
title: "Initial Setup"
date: 2026-02-01T00:00:00+07:00
draft: false
weight: 100001
description: "Get Playwright installed and running - Node.js setup, Playwright installation, browser installation, and your first test"
tags: ["playwright", "installation", "setup", "testing", "automation", "beginner"]
---

**Want to start testing with Playwright?** This initial setup guide gets Playwright installed and working on your system in minutes. By the end, you'll have Playwright running with all browsers and will execute your first test.

This tutorial provides 0-5% coverage—just enough to get Playwright working on your machine. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/automation-testing/playwright/quick-start) (5-30% coverage).

## Prerequisites

Before installing Playwright, you need:

- A computer running Windows, macOS, or Linux
- Node.js 18+ installed ([nodejs.org](https://nodejs.org/))
- A terminal/command prompt
- A code editor (VS Code, WebStorm, or any editor)
- Basic command-line navigation skills
- Basic JavaScript or TypeScript knowledge

No prior testing framework experience required—this guide starts from zero.

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Install** Playwright using npm
2. **Download** browser binaries (Chromium, Firefox, WebKit)
3. **Configure** Playwright project structure
4. **Write** your first test
5. **Run** tests from command line
6. **View** test reports

## Node.js Verification

Playwright requires Node.js 18 or later. Verify your version:

```bash
node --version
```

Expected output (version 18 or higher):

```
v18.17.0
```

If you see a lower version or "command not found", install Node.js from [nodejs.org](https://nodejs.org/).

**Recommended**: Use LTS (Long Term Support) version for stability.

## Creating a New Project

### Option 1: New Playwright Project (Recommended for Beginners)

Create a new directory and initialize Playwright with configuration:

```bash
# Create project directory
mkdir my-playwright-tests
cd my-playwright-tests

# Initialize npm project
npm init -y

# Install Playwright with init (includes browser download)
npm init playwright@latest
```

The init command will prompt you for configuration:

```
? Do you want to use TypeScript or JavaScript? › TypeScript
? Where to put your end-to-end tests? › tests
? Add a GitHub Actions workflow? › Yes
? Install Playwright browsers (can be done manually via 'npx playwright install')? › Yes
```

**What gets created**:

- `playwright.config.ts` - Playwright configuration file
- `tests/` - Directory for test files
- `tests/example.spec.ts` - Example test file
- `.github/workflows/playwright.yml` - GitHub Actions CI configuration
- Browser binaries downloaded (~400MB total)

### Option 2: Add to Existing Project

If you have an existing Node.js project:

```bash
# Navigate to your project
cd your-existing-project

# Install Playwright
npm install --save-dev @playwright/test

# Install browsers
npx playwright install
```

Create `playwright.config.ts` manually (see Configuration section).

## Browser Installation

Playwright requires browser binaries. These are NOT your system browsers—Playwright downloads and manages its own versions.

### Automatic Installation (During Init)

If you chose "Yes" during `npm init playwright@latest`, browsers are already installed.

### Manual Installation

Install all browsers:

```bash
npx playwright install
```

Install specific browsers:

```bash
# Install only Chromium
npx playwright install chromium

# Install Chromium and Firefox
npx playwright install chromium firefox

# Install all browsers (Chromium, Firefox, WebKit)
npx playwright install
```

**Browser download sizes** (approximate):

- Chromium: ~150MB
- Firefox: ~80MB
- WebKit: ~50MB (macOS/Linux), ~180MB (Windows)

**Total**: ~400MB for all three browsers

### Verify Installation

Check installed browsers:

```bash
npx playwright --version
```

Expected output:

```
Version 1.40.0
```

## Configuration File

The `playwright.config.ts` file controls test execution:

```typescript
import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  // Test directory
  testDir: "./tests",

  // Run tests in parallel
  fullyParallel: true,

  // Fail on CI if tests left test.only()
  forbidOnly: !!process.env.CI,

  // Retry failed tests (2 retries on CI, 0 locally)
  retries: process.env.CI ? 2 : 0,

  // Number of parallel workers (default: half of CPU cores)
  workers: process.env.CI ? 1 : undefined,

  // Reporter
  reporter: "html",

  // Shared settings for all projects
  use: {
    // Base URL for navigation
    baseURL: "http://localhost:3000",

    // Collect trace on failure for debugging
    trace: "on-first-retry",

    // Screenshot on failure
    screenshot: "only-on-failure",
  },

  // Test against multiple browsers
  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },
    {
      name: "firefox",
      use: { ...devices["Desktop Firefox"] },
    },
    {
      name: "webkit",
      use: { ...devices["Desktop Safari"] },
    },
  ],

  // Run local dev server before tests (optional)
  // webServer: {
  //   command: 'npm run start',
  //   url: 'http://localhost:3000',
  //   reuseExistingServer: !process.env.CI,
  // },
});
```

**Key configuration options**:

- `testDir`: Where test files are located
- `fullyParallel`: Run tests in parallel (faster execution)
- `retries`: Auto-retry flaky tests
- `workers`: Number of parallel test workers
- `reporter`: Test result format (html, list, json, junit)
- `projects`: Browser configurations to test against

## Your First Test

Create `tests/example.spec.ts`:

```typescript
import { test, expect } from "@playwright/test";

test("has title", async ({ page }) => {
  // Navigate to Playwright homepage
  await page.goto("https://playwright.dev/");

  // Assert page title contains "Playwright"
  await expect(page).toHaveTitle(/Playwright/);
});

test("get started link", async ({ page }) => {
  await page.goto("https://playwright.dev/");

  // Click "Get started" link
  await page.getByRole("link", { name: "Get started" }).click();

  // Verify navigation to installation page
  await expect(page).toHaveURL(/.*intro/);
});
```

**Test structure**:

- `test()`: Define a test case
- `{ page }`: Playwright provides page object (auto-cleanup)
- `await`: Playwright actions are async
- `expect()`: Assertions with automatic retries

## Running Tests

### Run All Tests

```bash
npx playwright test
```

Output:

```
Running 6 tests using 3 workers

  ✓ [chromium] › example.spec.ts:3:1 › has title (1.2s)
  ✓ [chromium] › example.spec.ts:9:1 › get started link (0.8s)
  ✓ [firefox] › example.spec.ts:3:1 › has title (1.5s)
  ✓ [firefox] › example.spec.ts:9:1 › get started link (1.1s)
  ✓ [webkit] › example.spec.ts:3:1 › has title (1.3s)
  ✓ [webkit] › example.spec.ts:9:1 › get started link (0.9s)

  6 passed (4.2s)
```

**What happened**: Each test ran in 3 browsers (chromium, firefox, webkit) = 6 total test runs.

### Run Specific Browser

```bash
# Run only in Chromium
npx playwright test --project=chromium

# Run only in Firefox
npx playwright test --project=firefox
```

### Run Specific Test File

```bash
npx playwright test tests/example.spec.ts
```

### Run in Headed Mode (See Browser)

```bash
# Show browser window during test execution
npx playwright test --headed
```

### Debug Mode

```bash
# Run with Playwright Inspector (step-by-step debugging)
npx playwright test --debug
```

## Viewing Test Reports

After tests run, generate HTML report:

```bash
npx playwright show-report
```

This opens an interactive report showing:

- Test results (passed/failed/skipped)
- Execution time for each test
- Screenshots of failures
- Traces for debugging
- Browser/project breakdown

## Project Structure

After setup, your project structure:

```
my-playwright-tests/
├── node_modules/          # Dependencies
├── tests/                 # Test files
│   └── example.spec.ts   # Example test
├── test-results/          # Test artifacts (screenshots, videos, traces)
├── playwright-report/     # HTML report
├── playwright.config.ts   # Playwright configuration
├── package.json           # npm configuration
└── package-lock.json      # Locked dependency versions
```

**Test file naming convention**:

- `*.spec.ts` - Spec files (tests)
- `*.test.ts` - Alternative naming (also works)

## Common Issues

### "Cannot find module '@playwright/test'"

**Cause**: Playwright not installed

**Fix**:

```bash
npm install --save-dev @playwright/test
```

### "Executable doesn't exist at ..."

**Cause**: Browsers not installed

**Fix**:

```bash
npx playwright install
```

### Tests timeout

**Cause**: Default timeout too short for slow network

**Fix**: Increase timeout in `playwright.config.ts`:

```typescript
export default defineConfig({
  timeout: 60000, // 60 seconds (default: 30s)
});
```

### "Address already in use" (port conflict)

**Cause**: Another process using port 3000

**Fix**: Change `baseURL` in config or stop conflicting process

## Editor Integration

### VS Code

Install Playwright Test for VSCode extension:

1. Open VS Code
2. Go to Extensions (Ctrl+Shift+X / Cmd+Shift+X)
3. Search "Playwright Test for VSCode"
4. Click Install

**Features**:

- Run tests from editor
- Debug tests with breakpoints
- View test results inline
- Generate tests by recording actions

### WebStorm

Playwright support built-in (2023.1+):

- Right-click test file → Run 'filename'
- Debug tests with breakpoints
- View test results in tool window

## Next Steps

Now that Playwright is installed:

1. **[Quick Start](/en/learn/software-engineering/automation-testing/playwright/quick-start)** - Build a complete test with step-by-step walkthrough
2. **[By Example](/en/learn/software-engineering/automation-testing/playwright/by-example)** - Learn through 85 annotated code examples
3. **Playwright Documentation** - Official docs for advanced features

**Recommended workflow**: Quick Start → practice with your own projects → use By Example as reference.

## Summary

You now have:

- ✅ Playwright installed via npm
- ✅ Browser binaries downloaded (Chromium, Firefox, WebKit)
- ✅ Configuration file created (`playwright.config.ts`)
- ✅ Example test running across 3 browsers
- ✅ HTML report viewer working

**Total setup time**: ~5-10 minutes (including browser downloads)

**Next**: Try the [Quick Start](/en/learn/software-engineering/automation-testing/playwright/quick-start) tutorial to build a real-world test scenario.
