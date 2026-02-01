---
title: "Quick Start"
date: 2026-02-01T00:00:00+07:00
draft: false
weight: 100002
description: "Rapid tour of Playwright essentials - locators, interactions, assertions, page objects, and debugging in one comprehensive tutorial"
tags: ["playwright", "quick-start", "testing", "automation", "typescript", "e2e"]
---

**Ready to test with Playwright?** This quick start tutorial provides a fast-paced tour through Playwright's core capabilities. By the end, you'll build a complete login test with form validation, error handling, and best practices.

This tutorial provides 5-30% coverage—practical hands-on experience with essential Playwright features. For comprehensive learning, continue to [By Example](/en/learn/software-engineering/automation-testing/playwright/by-example) (95% coverage).

## Prerequisites

Before starting this tutorial, you need:

- Playwright installed (see [Initial Setup](/en/learn/software-engineering/automation-testing/playwright/initial-setup))
- Basic JavaScript or TypeScript knowledge
- Understanding of async/await syntax
- Familiarity with HTML/CSS selectors
- Text editor with TypeScript support

## Learning Objectives

By the end of this tutorial, you will understand:

1. **Locators** - Finding elements with getByRole, getByText, CSS selectors
2. **Interactions** - Clicking, typing, selecting, uploading files
3. **Assertions** - Web-first assertions with automatic retries
4. **Navigation** - Page navigation and URL verification
5. **Forms** - Complete form testing workflow
6. **Debugging** - Using Playwright Inspector and trace viewer
7. **Page Objects** - Organizing tests with page object pattern
8. **Best Practices** - Auto-wait, isolation, and flake prevention

## The Scenario: Login Flow Testing

We'll test a complete login flow including:

- Valid login (happy path)
- Invalid credentials (error handling)
- Form validation (empty fields)
- Password visibility toggle
- Remember me checkbox

This mirrors real-world testing requirements.

## Project Setup

Create a test file `tests/login.spec.ts`:

```bash
# Create test file
touch tests/login.spec.ts
```

We'll build this test incrementally, learning Playwright features as we go.

## Basic Test Structure

Start with the simplest test - navigate and verify title:

```typescript
import { test, expect } from "@playwright/test";

test("login page loads", async ({ page }) => {
  // Navigate to login page
  await page.goto("https://demo.playwright.dev/login");

  // Verify page title
  await expect(page).toHaveTitle(/Login/);
});
```

**Key concepts**:

- `test()`: Define a test case
- `{ page }`: Playwright provides isolated page for each test (auto-cleanup)
- `await`: All Playwright actions are async
- `expect()`: Web-first assertions with automatic retries

Run the test:

```bash
npx playwright test tests/login.spec.ts
```

## Finding Elements: Locators

Playwright recommends role-based locators for accessibility and reliability.

### Role-Based Locators (Recommended)

```typescript
test("find elements by role", async ({ page }) => {
  await page.goto("https://demo.playwright.dev/login");

  // Find by ARIA role and accessible name
  const usernameInput = page.getByRole("textbox", { name: "Username" });
  const passwordInput = page.getByRole("textbox", { name: "Password" });
  const loginButton = page.getByRole("button", { name: "Log in" });

  // Verify elements are visible
  await expect(usernameInput).toBeVisible();
  await expect(passwordInput).toBeVisible();
  await expect(loginButton).toBeVisible();
});
```

**Why role-based locators?**

- Accessibility-first (screen reader compatible)
- Resistant to DOM changes
- No brittle test IDs or CSS classes needed

### Text-Based Locators

```typescript
test("find elements by text", async ({ page }) => {
  await page.goto("https://demo.playwright.dev/login");

  // Find by exact text
  const heading = page.getByText("Welcome Back");

  // Find by partial text (regex)
  const forgotPassword = page.getByText(/forgot.*password/i);

  await expect(heading).toBeVisible();
  await expect(forgotPassword).toBeVisible();
});
```

### CSS Selectors (When Necessary)

```typescript
test("find elements with CSS", async ({ page }) => {
  await page.goto("https://demo.playwright.dev/login");

  // CSS selector as fallback
  const logo = page.locator(".login-logo");
  const form = page.locator("form#login-form");

  await expect(logo).toBeVisible();
  await expect(form).toBeVisible();
});
```

**Locator priority**: Role > Label > Placeholder > Test ID > CSS selector

## Interactions: Filling Forms

Now let's interact with the login form:

```typescript
test("fill login form", async ({ page }) => {
  await page.goto("https://demo.playwright.dev/login");

  // Type into text inputs
  await page.getByRole("textbox", { name: "Username" }).fill("testuser");
  await page.getByRole("textbox", { name: "Password" }).fill("password123");

  // Check "Remember me" checkbox
  await page.getByRole("checkbox", { name: "Remember me" }).check();

  // Click login button
  await page.getByRole("button", { name: "Log in" }).click();

  // Verify successful login (URL change)
  await expect(page).toHaveURL(/dashboard/);
});
```

**Interaction methods**:

- `fill()`: Clear and type text (recommended)
- `type()`: Type character by character (slower, for special cases)
- `click()`: Click element
- `check()` / `uncheck()`: Toggle checkboxes
- `selectOption()`: Select dropdown option

**Auto-waiting**: Playwright waits for elements to be visible, enabled, and stable before acting. No manual waits needed.

## Assertions: Verifying Behavior

Playwright provides web-first assertions that automatically retry:

```typescript
test("verify login success", async ({ page }) => {
  await page.goto("https://demo.playwright.dev/login");

  // Fill and submit form
  await page.getByRole("textbox", { name: "Username" }).fill("testuser");
  await page.getByRole("textbox", { name: "Password" }).fill("password123");
  await page.getByRole("button", { name: "Log in" }).click();

  // Wait for navigation and verify URL
  await expect(page).toHaveURL(/dashboard/);

  // Verify welcome message appears
  await expect(page.getByText(/Welcome, testuser/i)).toBeVisible();

  // Verify logout button exists
  await expect(page.getByRole("button", { name: "Logout" })).toBeVisible();
});
```

**Common assertions**:

- `toBeVisible()`: Element is visible
- `toHaveText()`: Element contains specific text
- `toHaveValue()`: Input has specific value
- `toBeEnabled()` / `toBeDisabled()`: Element state
- `toHaveURL()`: Current URL matches pattern
- `toHaveTitle()`: Page title matches

**Automatic retry**: Assertions retry until timeout (default 5 seconds). This handles dynamic content without manual waits.

## Error Handling: Testing Failure Paths

Test invalid login (error path):

```typescript
test("invalid login shows error", async ({ page }) => {
  await page.goto("https://demo.playwright.dev/login");

  // Attempt login with wrong credentials
  await page.getByRole("textbox", { name: "Username" }).fill("wronguser");
  await page.getByRole("textbox", { name: "Password" }).fill("wrongpassword");
  await page.getByRole("button", { name: "Log in" }).click();

  // Verify error message appears
  await expect(page.getByText(/Invalid username or password/i)).toBeVisible();

  // Verify still on login page
  await expect(page).toHaveURL(/login/);

  // Verify form is still visible (not redirected)
  await expect(page.getByRole("button", { name: "Log in" })).toBeVisible();
});
```

**Why test error paths?**

- Catches missing error handling
- Verifies user feedback
- Ensures graceful failure

## Form Validation: Empty Fields

Test client-side validation:

```typescript
test("empty form shows validation errors", async ({ page }) => {
  await page.goto("https://demo.playwright.dev/login");

  // Click login without filling form
  await page.getByRole("button", { name: "Log in" }).click();

  // Verify HTML5 validation or custom error messages
  const usernameError = page.getByText(/Username is required/i);
  const passwordError = page.getByText(/Password is required/i);

  await expect(usernameError).toBeVisible();
  await expect(passwordError).toBeVisible();
});
```

## Page Object Pattern: Organizing Tests

Create reusable page objects for better maintainability.

Create `pages/LoginPage.ts`:

```typescript
import { Page, Locator } from "@playwright/test";

export class LoginPage {
  readonly page: Page;
  readonly usernameInput: Locator;
  readonly passwordInput: Locator;
  readonly rememberMeCheckbox: Locator;
  readonly loginButton: Locator;
  readonly errorMessage: Locator;

  constructor(page: Page) {
    this.page = page;
    this.usernameInput = page.getByRole("textbox", { name: "Username" });
    this.passwordInput = page.getByRole("textbox", { name: "Password" });
    this.rememberMeCheckbox = page.getByRole("checkbox", { name: "Remember me" });
    this.loginButton = page.getByRole("button", { name: "Log in" });
    this.errorMessage = page.locator(".error-message");
  }

  async goto() {
    await this.page.goto("https://demo.playwright.dev/login");
  }

  async login(username: string, password: string, rememberMe = false) {
    await this.usernameInput.fill(username);
    await this.passwordInput.fill(password);

    if (rememberMe) {
      await this.rememberMeCheckbox.check();
    }

    await this.loginButton.click();
  }

  async getErrorText(): Promise<string> {
    return (await this.errorMessage.textContent()) || "";
  }
}
```

Use page object in tests:

```typescript
import { LoginPage } from "../pages/LoginPage";

test("login with page object", async ({ page }) => {
  const loginPage = new LoginPage(page);

  // Navigate to login page
  await loginPage.goto();

  // Perform login
  await loginPage.login("testuser", "password123", true);

  // Verify successful login
  await expect(page).toHaveURL(/dashboard/);
});

test("invalid login with page object", async ({ page }) => {
  const loginPage = new LoginPage(page);

  await loginPage.goto();
  await loginPage.login("wronguser", "wrongpassword");

  // Verify error appears
  const errorText = await loginPage.getErrorText();
  expect(errorText).toContain("Invalid username or password");
});
```

**Benefits of page objects**:

- Reusable across tests
- Single source of truth for selectors
- Easier to maintain when UI changes
- More readable tests

## Debugging: Playwright Inspector

When tests fail, use Playwright Inspector to debug interactively:

```bash
# Run test in debug mode
npx playwright test tests/login.spec.ts --debug
```

**Playwright Inspector features**:

- Step through test line by line
- Inspect page state at each step
- Try selectors in console
- See screenshots at each action
- Resume or step over

**Pro tip**: Add `await page.pause()` in test to trigger debugger at specific point.

## Trace Viewer: Post-Mortem Debugging

When tests fail in CI, trace viewer shows what happened:

Configure trace collection in `playwright.config.ts`:

```typescript
use: {
  trace: 'on-first-retry', // Collect trace on retry
}
```

View trace after test failure:

```bash
# Run tests
npx playwright test

# Open trace viewer for last run
npx playwright show-trace
```

**Trace viewer shows**:

- Complete timeline of actions
- Screenshots before/after each action
- Network requests and responses
- Console logs
- DOM snapshots

## Complete Example: Full Login Test Suite

Putting it all together:

```typescript
import { test, expect } from "@playwright/test";
import { LoginPage } from "../pages/LoginPage";

test.describe("Login Flow", () => {
  let loginPage: LoginPage;

  test.beforeEach(async ({ page }) => {
    loginPage = new LoginPage(page);
    await loginPage.goto();
  });

  test("successful login with valid credentials", async ({ page }) => {
    await loginPage.login("testuser", "password123");
    await expect(page).toHaveURL(/dashboard/);
    await expect(page.getByText(/Welcome, testuser/i)).toBeVisible();
  });

  test("failed login with invalid credentials", async () => {
    await loginPage.login("wronguser", "wrongpassword");
    const errorText = await loginPage.getErrorText();
    expect(errorText).toContain("Invalid username or password");
  });

  test("remember me checkbox persists session", async ({ page }) => {
    await loginPage.login("testuser", "password123", true);
    await expect(page).toHaveURL(/dashboard/);

    // Close and reopen browser
    await page.context().close();
    // Verify session persisted (would need cookie/storage check)
  });

  test("password visibility toggle", async ({ page }) => {
    const passwordInput = loginPage.passwordInput;
    const toggleButton = page.getByRole("button", { name: "Show password" });

    // Password initially hidden (type="password")
    await expect(passwordInput).toHaveAttribute("type", "password");

    // Click toggle to show password
    await toggleButton.click();
    await expect(passwordInput).toHaveAttribute("type", "text");

    // Click again to hide
    await toggleButton.click();
    await expect(passwordInput).toHaveAttribute("type", "password");
  });

  test("form validation for empty fields", async ({ page }) => {
    await loginPage.loginButton.click();

    // HTML5 validation or custom errors
    await expect(page.getByText(/Username is required/i)).toBeVisible();
    await expect(page.getByText(/Password is required/i)).toBeVisible();
  });
});
```

Run the suite:

```bash
npx playwright test tests/login.spec.ts
```

## What to Try Next

Extend your login tests:

1. **Add password reset flow** - Test "Forgot password" link and email verification
2. **Test social login** - Google/Facebook OAuth flows
3. **Add MFA testing** - Two-factor authentication code entry
4. **Test account lockout** - Multiple failed login attempts
5. **Accessibility testing** - Verify keyboard navigation and screen reader support

## Common Gotchas

### 1. Element Not Found

**Problem**: `Element not found` error

**Cause**: Incorrect locator or element not yet rendered

**Fix**: Use Playwright Inspector to test locator:

```bash
npx playwright test --debug
```

### 2. Flaky Tests

**Problem**: Tests pass sometimes, fail sometimes

**Cause**: Race conditions, missing waits

**Fix**: Use web-first assertions (automatic retry):

```typescript
// ❌ Bad (flaky)
const text = await element.textContent();
expect(text).toBe("Loading complete");

// ✅ Good (auto-retry)
await expect(element).toHaveText("Loading complete");
```

### 3. Timeout Errors

**Problem**: Test times out waiting for element

**Cause**: Element never appears, or takes longer than 30s

**Fix**: Increase timeout or fix underlying issue:

```typescript
// Increase timeout for specific action
await page.getByRole("button").click({ timeout: 60000 }); // 60 seconds

// Or configure globally in playwright.config.ts
export default defineConfig({
  timeout: 60000, // 60 seconds per test
});
```

## Best Practices Summary

1. **Use role-based locators** - Accessibility-first, resistant to changes
2. **Leverage auto-waiting** - No manual waits needed for most cases
3. **Test failure paths** - Error handling is as important as happy paths
4. **Organize with page objects** - Reusability and maintainability
5. **Debug with Inspector** - Step through tests interactively when debugging
6. **Collect traces on failure** - Post-mortem debugging for CI failures
7. **Isolate tests** - Each test should be independent (no shared state)

## Next Steps

Now that you understand Playwright basics:

1. **[By Example](/en/learn/software-engineering/automation-testing/playwright/by-example)** - 85 annotated examples covering 95% of Playwright
2. **Practice with your project** - Apply Playwright to your actual application
3. **Official Documentation** - Advanced features, best practices, CI/CD integration

**Recommended learning path**: Quick Start → practice on real projects → By Example for comprehensive reference.

## Summary

You've learned:

- ✅ Locators (role-based, text-based, CSS selectors)
- ✅ Interactions (fill, click, check, type)
- ✅ Assertions (web-first with auto-retry)
- ✅ Form testing (happy path, errors, validation)
- ✅ Page object pattern (reusable, maintainable)
- ✅ Debugging (Inspector, trace viewer)
- ✅ Best practices (auto-wait, isolation, accessibility)

**Coverage**: 5-30% of Playwright features - practical foundation for real-world testing.

**Next**: Explore [By Example](/en/learn/software-engineering/automation-testing/playwright/by-example) for comprehensive 95% coverage through 85 annotated examples.
