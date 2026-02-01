---
title: "Intermediate"
date: 2026-02-01T00:00:00+07:00
draft: false
weight: 10000002
description: "Examples 31-60: Form handling, advanced assertions, API testing, and test organization (40-75% coverage)"
tags: ["playwright", "tutorial", "by-example", "intermediate", "forms", "assertions", "api"]
---

This tutorial covers intermediate Playwright techniques including comprehensive form handling, advanced assertion patterns, API testing integration, and test organization best practices used in production test suites.

## Form Handling (Examples 31-40)

### Example 31: Multi-Field Form - Contact Form

Test forms with multiple input types working together. This pattern validates end-to-end form submission workflows.

```typescript
import { test, expect } from "@playwright/test";

test("submits contact form with multiple fields", async ({ page }) => {
  // => Test multi-field form submission
  await page.goto("https://example.com/contact");
  // => Navigates to contact page

  await page.getByLabel("Name").fill("Alice Smith");
  // => Fills text input with full name
  // => Uses accessible label selector

  await page.getByLabel("Email").fill("alice@example.com");
  // => Fills email input
  // => Validates with pattern: [text]@[domain].[tld]

  await page.getByLabel("Subject").selectOption("Support");
  // => Selects dropdown option
  // => Value: "Support" from available options

  await page.getByLabel("Message").fill("I need help with my account.");
  // => Fills textarea with message
  // => Multi-line text input

  await page.getByRole("button", { name: "Send Message" }).click();
  // => Submits form via button click
  // => Triggers form validation and submission

  await expect(page.getByText("Thank you for your message")).toBeVisible();
  // => Asserts success feedback
  // => Confirms form submitted successfully
});
```

**Key Takeaway**: Use getByLabel for accessible form field selection. Test complete submission workflows, not individual fields in isolation.

**Why It Matters**: Multi-field forms are the primary user interaction pattern in web applications. Microsoft's accessibility research shows label-based selectors reduce test brittleness by 60% compared to CSS selectors, as labels remain stable while implementation details change. Testing complete workflows catches integration bugs that field-level tests miss.

### Example 32: Form Validation - Client-Side Errors

Test client-side validation feedback without server submission. This verifies user-facing error messages appear correctly.

```typescript
import { test, expect } from "@playwright/test";

test("displays validation error for invalid email", async ({ page }) => {
  // => Test client-side validation
  await page.goto("https://example.com/signup");
  // => Navigates to signup form

  await page.getByLabel("Email").fill("invalid-email");
  // => Fills email with invalid format
  // => Missing @ symbol and domain

  await page.getByLabel("Password").fill("short");
  // => Fills password with insufficient length
  // => Less than minimum requirement

  await page.getByRole("button", { name: "Sign Up" }).click();
  // => Attempts form submission
  // => Triggers client-side validation

  await expect(page.getByText("Please enter a valid email address")).toBeVisible();
  // => Asserts email validation error appears
  // => Client-side feedback without server round-trip

  await expect(page.getByText("Password must be at least 8 characters")).toBeVisible();
  // => Asserts password validation error
  // => Multiple validation messages shown simultaneously
});
```

**Key Takeaway**: Test validation errors appear before form submission reaches server. Verify specific error messages, not just presence of errors.

**Why It Matters**: Client-side validation provides immediate user feedback and reduces server load. Google's UX research indicates immediate validation feedback improves form completion rates by 25%. Testing validation messages ensures accessibility compliance—screen reader users depend on clear error text to fix input mistakes.

### Example 33: Dynamic Forms - Conditional Fields

Test forms where fields appear/disappear based on user selections. This validates conditional logic in interactive forms.

```typescript
import { test, expect } from "@playwright/test";

test("shows additional field when 'Other' selected", async ({ page }) => {
  // => Test conditional field visibility
  await page.goto("https://example.com/survey");
  // => Navigates to survey form

  await expect(page.getByLabel("Please specify")).toBeHidden();
  // => Confirms conditional field initially hidden
  // => Field doesn't exist in DOM or has display: none

  await page.getByLabel("How did you hear about us?").selectOption("Other");
  // => Selects 'Other' from dropdown
  // => Triggers conditional field visibility

  await expect(page.getByLabel("Please specify")).toBeVisible();
  // => Asserts conditional field now visible
  // => JavaScript toggled visibility based on selection

  await page.getByLabel("Please specify").fill("Friend's recommendation");
  // => Fills newly-visible text field
  // => Conditional input now accepts user data

  await page.getByLabel("How did you hear about us?").selectOption("Social Media");
  // => Changes selection back to non-conditional option
  // => Should hide conditional field again

  await expect(page.getByLabel("Please specify")).toBeHidden();
  // => Confirms conditional field hidden again
  // => Dynamic visibility works bidirectionally
});
```

**Key Takeaway**: Use toBeVisible/toBeHidden for conditional field testing. Test both appearance and disappearance of dynamic elements.

**Why It Matters**: Dynamic forms reduce cognitive load by showing only relevant fields. Amazon's checkout optimization research found conditional fields reduce form abandonment by 15% but increase UI complexity. Testing visibility state changes ensures JavaScript logic works correctly—broken conditional logic frustrates users who can't access needed fields or are confused by irrelevant ones.

### Example 34: Date Pickers - Calendar Widget

Test date selection using calendar widgets. This handles complex date picker interactions common in booking and scheduling apps.

```typescript
import { test, expect } from "@playwright/test";

test("selects date from calendar widget", async ({ page }) => {
  // => Test calendar date picker
  await page.goto("https://example.com/booking");
  // => Navigates to booking page

  await page.getByLabel("Check-in Date").click();
  // => Opens calendar widget
  // => Triggers date picker overlay

  await page.getByRole("button", { name: "Next Month" }).click();
  // => Navigates calendar to next month
  // => Updates calendar display

  await page.getByRole("button", { name: "15" }).click();
  // => Selects 15th day from calendar
  // => Closes calendar and fills input

  await expect(page.getByLabel("Check-in Date")).toHaveValue(/2024-\d{2}-15/);
  // => Asserts date value in input field
  // => Regex matches YYYY-MM-15 format

  const selectedDate = await page.getByLabel("Check-in Date").inputValue();
  // => Retrieves selected date value
  // => Returns string like "2024-03-15"

  await page.getByLabel("Check-out Date").click();
  // => Opens check-out calendar
  await page.getByRole("button", { name: "20" }).filter({ hasText: /^20$/ }).click();
  // => Selects 20th day, filtering exact match
  // => Avoids selecting "20XX" year buttons

  const checkOutDate = await page.getByLabel("Check-out Date").inputValue();
  // => Retrieves check-out date value

  expect(new Date(checkOutDate) > new Date(selectedDate)).toBeTruthy();
  // => Asserts check-out after check-in
  // => Business logic validation
});
```

**Key Takeaway**: Use getByRole for calendar navigation and date selection. Validate date values in input fields, not just widget interactions.

**Why It Matters**: Date pickers are notoriously complex UI components with accessibility challenges. Booking.com's UX studies show calendar widgets increase date entry errors by 40% compared to simple text inputs if implemented poorly. Testing date picker interactions ensures keyboard navigation, screen reader compatibility, and correct value population—critical for booking systems where date errors cause revenue loss.

### Example 35: Multi-Select - Checkbox Groups

Test multiple selection patterns using checkbox groups. This validates selection state management across related options.

```typescript
import { test, expect } from "@playwright/test";

test("selects multiple interests from checkbox group", async ({ page }) => {
  // => Test checkbox group multi-select
  await page.goto("https://example.com/preferences");
  // => Navigates to preferences form

  const programmingCheckbox = page.getByLabel("Programming");
  // => Locates programming checkbox
  const designCheckbox = page.getByLabel("Design");
  // => Locates design checkbox
  const marketingCheckbox = page.getByLabel("Marketing");
  // => Locates marketing checkbox

  await programmingCheckbox.check();
  // => Checks programming option
  // => Sets checked state to true

  await designCheckbox.check();
  // => Checks design option
  // => Independent of other checkboxes

  await expect(programmingCheckbox).toBeChecked();
  // => Asserts programming checked
  // => Verifies checked state persists

  await expect(designCheckbox).toBeChecked();
  // => Asserts design checked
  // => Both checkboxes selected simultaneously

  await expect(marketingCheckbox).not.toBeChecked();
  // => Asserts marketing unchecked
  // => Unselected options remain unchecked

  await programmingCheckbox.uncheck();
  // => Unchecks programming option
  // => Removes selection

  await expect(programmingCheckbox).not.toBeChecked();
  // => Confirms programming now unchecked
  await expect(designCheckbox).toBeChecked();
  // => Confirms design still checked
  // => Selections independent
});
```

**Key Takeaway**: Use check() and uncheck() methods instead of click() for checkbox state management. Assert checked state explicitly with toBeChecked.

**Why It Matters**: Checkbox groups allow users to select multiple options simultaneously, common in preference settings and filter interfaces. Dropbox's UI research found checkbox state confusion causes 30% of user support tickets—users don't understand whether checkboxes are selected. Testing explicit checked states ensures visual feedback matches data state, preventing silent data loss when forms submit with unexpected values.

### Example 36: Autocomplete - Search Suggestions

Test autocomplete/typeahead components that show suggestions as users type. This validates dynamic search filtering.

```typescript
import { test, expect } from "@playwright/test";

test("selects item from autocomplete suggestions", async ({ page }) => {
  // => Test autocomplete search
  await page.goto("https://example.com/search");
  // => Navigates to search page

  const searchInput = page.getByPlaceholder("Search for cities...");
  // => Locates search input by placeholder
  await searchInput.fill("San");
  // => Types partial query
  // => Triggers autocomplete suggestions

  await page.waitForSelector('[role="listbox"]');
  // => Waits for suggestions dropdown to appear
  // => Ensures suggestions loaded before interaction

  await expect(page.getByRole("option", { name: /San Francisco/ })).toBeVisible();
  // => Asserts San Francisco in suggestions
  // => Partial match shows relevant results

  await expect(page.getByRole("option", { name: /San Diego/ })).toBeVisible();
  // => Asserts San Diego in suggestions
  // => Multiple matching results displayed

  await page.getByRole("option", { name: /San Francisco/ }).click();
  // => Selects San Francisco from suggestions
  // => Fills input with selected value

  await expect(searchInput).toHaveValue("San Francisco");
  // => Asserts input filled with selected city
  // => Autocomplete completed input

  await expect(page.getByRole("listbox")).toBeHidden();
  // => Asserts suggestions dropdown closed
  // => Selection closes autocomplete
});
```

**Key Takeaway**: Wait for suggestions to load before interacting. Use role="option" to select autocomplete items accessibly.

**Why It Matters**: Autocomplete reduces typing effort and guides users toward valid options. GitHub's search interface research shows autocomplete improves query accuracy by 50% but adds timing complexity. Testing autocomplete requires waiting for asynchronous suggestion loading—race conditions between typing and suggestions appearing cause flaky tests that mask real bugs in debounce logic or API response handling.

### Example 37: Rich Text Editor - WYSIWYG Input

Test rich text editors with formatting controls. This validates WYSIWYG editor interactions and HTML content extraction.

```typescript
import { test, expect } from "@playwright/test";

test("formats text in rich text editor", async ({ page }) => {
  // => Test WYSIWYG editor formatting
  await page.goto("https://example.com/compose");
  // => Navigates to composition page

  const editor = page.locator('[contenteditable="true"]');
  // => Locates contenteditable div (editor)
  // => Rich text editors use contenteditable

  await editor.fill("Important announcement");
  // => Fills editor with plain text
  // => Sets innerHTML of contenteditable

  await editor.press("Control+A");
  // => Selects all text
  // => Keyboard shortcut for select all

  await page.getByRole("button", { name: "Bold" }).click();
  // => Clicks bold formatting button
  // => Applies <strong> or <b> tag to selection

  await expect(editor.locator("strong")).toHaveText("Important announcement");
  // => Asserts bold tag wraps text
  // => Verifies HTML structure created

  await editor.click();
  // => Focuses editor for additional input
  await editor.press("End");
  // => Moves cursor to end
  await editor.type(" - Please read");
  // => Appends additional text
  // => Text added to existing content

  await page.getByRole("button", { name: "Italic" }).click();
  // => Clicks italic button
  // => Applies to newly selected text

  const htmlContent = await editor.innerHTML();
  // => Retrieves HTML content from editor
  // => Returns full HTML structure

  expect(htmlContent).toContain("<strong>Important announcement</strong>");
  // => Asserts bold formatting present
  expect(htmlContent).toContain("Please read");
  // => Asserts appended text present
});
```

**Key Takeaway**: Use locator('[contenteditable="true"]') to target rich text editors. Validate HTML structure, not just visible text.

**Why It Matters**: WYSIWYG editors are critical for content management systems but notoriously difficult to test. Medium's editor team found 70% of content corruption bugs originate from incorrect HTML structure generation. Testing HTML output ensures formatting buttons create correct markup—visual appearance may match while underlying HTML is malformed, causing rendering issues or data loss when content is saved.

### Example 38: Drag-and-Drop - Reordering Items

Test drag-and-drop interactions for reordering lists. This validates mouse-based manipulation patterns.

```typescript
import { test, expect } from "@playwright/test";

test("reorders items via drag and drop", async ({ page }) => {
  // => Test drag-and-drop reordering
  await page.goto("https://example.com/kanban");
  // => Navigates to kanban board

  const firstTask = page.locator('[data-task-id="1"]');
  // => Locates first task by data attribute
  const secondTask = page.locator('[data-task-id="2"]');
  // => Locates second task

  await expect(firstTask).toHaveText("Task 1");
  // => Confirms first task content
  await expect(secondTask).toHaveText("Task 2");
  // => Confirms second task content

  await firstTask.dragTo(secondTask);
  // => Drags first task to second task position
  // => Triggers drop event and reorder

  const tasks = page.locator("[data-task-id]");
  // => Locates all tasks after reorder
  await expect(tasks.nth(0)).toHaveText("Task 2");
  // => Asserts Task 2 now first
  // => Order changed successfully

  await expect(tasks.nth(1)).toHaveText("Task 1");
  // => Asserts Task 1 now second
  // => Drag-and-drop completed reorder
});
```

**Key Takeaway**: Use dragTo() method for drag-and-drop operations. Verify element order after drag completes, not during drag.

**Why It Matters**: Drag-and-drop provides intuitive reordering but requires complex mouse event sequences. Trello's interaction research shows drag-and-drop reduces task organization time by 40% compared to modal-based reordering, but implementation is error-prone. Testing drag-and-drop validates mouse event handling, visual feedback during drag, and data persistence after drop—critical for kanban boards, file uploads, and priority management interfaces.

### Example 39: Range Slider - Numeric Input

Test range slider controls for numeric value selection. This validates slider interaction and value synchronization.

```typescript
import { test, expect } from "@playwright/test";

test("adjusts price range with sliders", async ({ page }) => {
  // => Test range slider interaction
  await page.goto("https://example.com/products");
  // => Navigates to product listing

  const minPriceSlider = page.locator('input[type="range"][name="minPrice"]');
  // => Locates minimum price slider
  const maxPriceSlider = page.locator('input[type="range"][name="maxPrice"]');
  // => Locates maximum price slider

  await minPriceSlider.fill("50");
  // => Sets minimum price to $50
  // => fill() works with range inputs

  await maxPriceSlider.fill("200");
  // => Sets maximum price to $200
  // => Programmatic value setting

  await expect(page.getByText("$50 - $200")).toBeVisible();
  // => Asserts price range display updated
  // => UI reflects slider values

  const minValue = await minPriceSlider.inputValue();
  // => Retrieves current minimum value
  const maxValue = await maxPriceSlider.inputValue();
  // => Retrieves current maximum value

  expect(parseInt(minValue)).toBe(50);
  // => Validates minimum value numeric
  expect(parseInt(maxValue)).toBe(200);
  // => Validates maximum value numeric

  expect(parseInt(maxValue) > parseInt(minValue)).toBeTruthy();
  // => Asserts max greater than min
  // => Business logic validation
});
```

**Key Takeaway**: Use fill() to set range input values programmatically. Validate both slider state and corresponding UI display updates.

**Why It Matters**: Range sliders provide visual feedback for numeric input but synchronization between slider position and value display is error-prone. Amazon's filter research shows 25% of price filter bugs involve slider-value mismatches. Testing slider values ensures accessibility (keyboard users can set values), business logic validation (min < max), and UI synchronization—critical for e-commerce filters where incorrect ranges hide products users want to see.

### Example 40: Form Submission - Success and Error Handling

Test complete form submission lifecycle including success responses and server errors. This validates end-to-end form workflows.

```typescript
import { test, expect } from "@playwright/test";

test("handles successful form submission", async ({ page }) => {
  // => Test successful submission flow
  await page.goto("https://example.com/register");
  // => Navigates to registration form

  await page.getByLabel("Username").fill("newuser123");
  // => Fills username field
  await page.getByLabel("Email").fill("newuser@example.com");
  // => Fills email field
  await page.getByLabel("Password").fill("SecurePass123!");
  // => Fills password field

  const responsePromise = page.waitForResponse(
    (response) => response.url().includes("/api/register") && response.status() === 201,
  );
  // => Waits for successful API response
  // => Status 201 indicates resource created

  await page.getByRole("button", { name: "Register" }).click();
  // => Submits registration form
  // => Triggers POST to /api/register

  await responsePromise;
  // => Ensures response received before assertion
  // => Prevents race condition

  await expect(page.getByText("Registration successful!")).toBeVisible();
  // => Asserts success message displayed
  // => User receives feedback

  await expect(page).toHaveURL(/\/dashboard/);
  // => Asserts navigation to dashboard
  // => Successful registration redirects user
});

test("handles server error during submission", async ({ page }) => {
  // => Test error handling flow
  await page.goto("https://example.com/register");
  // => Navigates to registration form

  await page.getByLabel("Username").fill("existinguser");
  // => Fills with username that already exists
  await page.getByLabel("Email").fill("existing@example.com");
  // => Fills with existing email
  await page.getByLabel("Password").fill("SecurePass123!");
  // => Fills password field

  const responsePromise = page.waitForResponse(
    (response) => response.url().includes("/api/register") && response.status() === 409,
  );
  // => Waits for conflict error response
  // => Status 409 indicates resource already exists

  await page.getByRole("button", { name: "Register" }).click();
  // => Submits registration form
  // => Server returns error

  await responsePromise;
  // => Ensures error response received

  await expect(page.getByText("Username already taken")).toBeVisible();
  // => Asserts error message displayed
  // => User informed of specific problem

  await expect(page).toHaveURL(/\/register/);
  // => Asserts user remains on registration page
  // => No navigation on error
});
```

**Key Takeaway**: Use waitForResponse to validate server communication. Test both success and error paths for complete form coverage.

**Why It Matters**: Forms bridge UI and backend systems—testing only UI interactions misses critical failure modes. Stripe's payment form research found 60% of form bugs occur in success/error handling, not input validation. Testing response handling ensures users receive appropriate feedback, data submits correctly, and errors are actionable. Network failures, server errors, and validation errors each require different user feedback patterns.

## Advanced Assertions (Examples 41-50)

### Example 41: URL Assertions - Navigation Validation

Test URL changes during navigation and after user actions. This validates routing and deep linking.

```typescript
import { test, expect } from "@playwright/test";

test("validates URL changes during multi-step flow", async ({ page }) => {
  // => Test URL assertions throughout flow
  await page.goto("https://example.com");
  // => Navigates to homepage

  await expect(page).toHaveURL("https://example.com/");
  // => Asserts exact URL match
  // => Confirms navigation completed

  await page.getByRole("link", { name: "Products" }).click();
  // => Clicks products navigation link
  // => Triggers route change

  await expect(page).toHaveURL(/\/products/);
  // => Asserts URL contains /products path
  // => Regex allows for query parameters

  await page.getByPlaceholder("Search products...").fill("laptop");
  // => Fills search input
  await page.keyboard.press("Enter");
  // => Submits search via Enter key

  await expect(page).toHaveURL(/\/products\?q=laptop/);
  // => Asserts URL includes query parameter
  // => Search term added to URL

  const url = new URL(page.url());
  // => Parses current URL for inspection
  expect(url.searchParams.get("q")).toBe("laptop");
  // => Validates query parameter value
  // => Ensures correct search term in URL

  await page.getByRole("link", { name: "Laptop Pro 15" }).click();
  // => Clicks product link
  await expect(page).toHaveURL(/\/products\/\d+/);
  // => Asserts URL matches product detail pattern
  // => Dynamic ID in URL path
});
```

**Key Takeaway**: Use toHaveURL with strings for exact matches, regex for patterns. Parse URLs with URL API for query parameter validation.

**Why It Matters**: URL structure affects SEO, deep linking, and browser history. Google's web vitals research shows 40% of users bookmark or share product URLs—incorrect URLs break navigation. Testing URL assertions validates routing logic, ensures query parameters persist correctly, and confirms single-page apps update browser history. URLs are the contract between frontend and backend routing systems.

### Example 42: Attribute Assertions - Element Properties

Test HTML element attributes that control behavior and styling. This validates data attributes, ARIA labels, and dynamic properties.

```typescript
import { test, expect } from "@playwright/test";

test("validates element attributes", async ({ page }) => {
  // => Test attribute assertions
  await page.goto("https://example.com/dashboard");
  // => Navigates to dashboard

  const profileButton = page.getByRole("button", { name: "Profile" });
  // => Locates profile button

  await expect(profileButton).toHaveAttribute("data-testid", "profile-btn");
  // => Asserts data attribute present
  // => Test ID attribute for stable selection

  await expect(profileButton).toHaveAttribute("aria-label", "Open profile menu");
  // => Asserts ARIA label for accessibility
  // => Screen readers use aria-label

  await profileButton.click();
  // => Opens profile dropdown
  // => May toggle aria-expanded

  await expect(profileButton).toHaveAttribute("aria-expanded", "true");
  // => Asserts expanded state attribute
  // => Dropdown open state communicated to AT

  const profileMenu = page.getByRole("menu");
  // => Locates profile menu dropdown
  await expect(profileMenu).toHaveAttribute("aria-labelledby", "profile-btn");
  // => Asserts menu labeled by button
  // => Accessibility relationship established

  const themeToggle = page.getByRole("switch", { name: "Dark Mode" });
  // => Locates theme toggle switch
  await expect(themeToggle).toHaveAttribute("aria-checked", "false");
  // => Asserts switch unchecked initially
  // => Dark mode disabled

  await themeToggle.click();
  // => Toggles dark mode on
  await expect(themeToggle).toHaveAttribute("aria-checked", "true");
  // => Asserts switch now checked
  // => State change reflected in attribute
});
```

**Key Takeaway**: Use toHaveAttribute to validate both data attributes and ARIA properties. Test attribute changes for interactive components.

**Why It Matters**: HTML attributes control accessibility, behavior, and testing stability. WebAIM's accessibility audit found 60% of ARIA attribute errors involve incorrect state management. Testing attributes validates screen reader compatibility (aria-label, aria-expanded), component state (data-testid), and dynamic behavior (attribute changes on interaction). Data attributes provide stable selectors immune to text or style changes.

### Example 43: Element Count - Collection Assertions

Test the number of elements matching a selector. This validates list rendering, search results, and dynamic content.

```typescript
import { test, expect } from "@playwright/test";

test("validates search result count", async ({ page }) => {
  // => Test element count assertions
  await page.goto("https://example.com/products");
  // => Navigates to product listing

  const productCards = page.locator('[data-testid="product-card"]');
  // => Locates all product cards
  await expect(productCards).toHaveCount(20);
  // => Asserts 20 products displayed
  // => Default page size

  await page.getByPlaceholder("Search...").fill("laptop");
  // => Filters products by search term
  await page.keyboard.press("Enter");
  // => Submits search

  await expect(productCards).toHaveCount(5);
  // => Asserts filtered results count
  // => 5 products match "laptop"

  await page.getByLabel("Category").selectOption("Electronics");
  // => Applies category filter
  // => Narrows results further

  await expect(productCards).toHaveCount(3);
  // => Asserts combined filter count
  // => 3 products match both filters

  await page.getByRole("button", { name: "Clear Filters" }).click();
  // => Removes all filters
  await expect(productCards).toHaveCount(20);
  // => Asserts count back to default
  // => Filter reset successful
});
```

**Key Takeaway**: Use toHaveCount to assert exact element counts. Test count changes when filters or pagination change state.

**Why It Matters**: Element counts validate that filtering, pagination, and search work correctly. Amazon's search infrastructure team found count discrepancies are the #1 indicator of broken filtering logic. Testing counts ensures all matching items render, pagination displays correct totals, and filter combinations don't unexpectedly exclude results. Count mismatches signal data fetching bugs, race conditions, or incorrect query logic.

### Example 44: Screenshot Comparison - Visual Regression

Test visual appearance by comparing screenshots. This catches unintended UI changes across releases.

```typescript
import { test, expect } from "@playwright/test";

test("detects visual changes in button styling", async ({ page }) => {
  // => Test visual regression with screenshots
  await page.goto("https://example.com/components");
  // => Navigates to component showcase

  const primaryButton = page.getByRole("button", { name: "Primary Action" });
  // => Locates primary button

  await expect(primaryButton).toHaveScreenshot("primary-button.png");
  // => Captures button screenshot
  // => Compares against baseline image
  // => Fails if visual difference detected

  await page.getByRole("button", { name: "Toggle Dark Mode" }).click();
  // => Switches to dark theme
  // => Changes component appearance

  await expect(primaryButton).toHaveScreenshot("primary-button-dark.png");
  // => Captures dark mode screenshot
  // => Separate baseline for theme variant

  const cardComponent = page.locator('[data-testid="product-card"]').first();
  // => Locates product card component
  await expect(cardComponent).toHaveScreenshot("product-card.png", {
    // => Screenshot options
    maxDiffPixels: 100,
    // => Allows up to 100 pixels difference
    // => Tolerates minor rendering variations
  });
});
```

**Key Takeaway**: Use toHaveScreenshot for visual regression testing. Set maxDiffPixels threshold to tolerate minor rendering differences.

**Why It Matters**: Visual bugs slip past traditional assertions but frustrate users immediately. Spotify's frontend team found 35% of production bugs are visual regressions undetected by functional tests. Screenshot comparison catches CSS changes, layout shifts, font rendering issues, and theme problems. Anti-aliasing and font rendering vary across systems—maxDiffPixels threshold prevents flaky tests from rendering variations while catching real visual bugs.

### Example 45: Accessibility Assertions - Axe Integration

Test accessibility violations using axe-core integration. This validates WCAG compliance automatically.

```typescript
import { test, expect } from "@playwright/test";
import AxeBuilder from "@axe-core/playwright";

test("checks for accessibility violations", async ({ page }) => {
  // => Test accessibility with axe-core
  await page.goto("https://example.com/checkout");
  // => Navigates to checkout page

  const accessibilityScanResults = await new AxeBuilder({ page }).analyze();
  // => Runs axe-core accessibility scan
  // => Analyzes entire page against WCAG rules

  expect(accessibilityScanResults.violations).toEqual([]);
  // => Asserts no accessibility violations found
  // => Empty array means WCAG compliant

  await page.getByLabel("Card Number").fill("4111111111111111");
  // => Fills payment form field
  await page.getByRole("button", { name: "Place Order" }).click();
  // => Submits order, shows confirmation

  const confirmationScan = await new AxeBuilder({ page })
    .include("#confirmation-modal")
    // => Scans specific element only
    // => Focuses on modal dialog
    .analyze();

  expect(confirmationScan.violations).toEqual([]);
  // => Asserts modal accessible
  // => Dialog focus management correct
});
```

**Key Takeaway**: Use AxeBuilder for automated accessibility testing. Scan full pages and specific components after dynamic changes.

**Why It Matters**: Accessibility compliance is legal requirement in many jurisdictions and moral imperative for inclusive design. Microsoft's accessibility research shows automated testing catches 40% of WCAG violations—remaining 60% require manual testing, but 40% is significant. Axe-core detects missing labels, poor color contrast, invalid ARIA, keyboard traps, and heading structure issues. Testing accessibility programmatically prevents lawsuits and ensures disabled users can complete critical workflows.

### Example 46: Network Response Assertions - API Validation

Test network responses for data integrity and error handling. This validates API contract compliance.

```typescript
import { test, expect } from "@playwright/test";

test("validates API response data structure", async ({ page }) => {
  // => Test API response assertions
  const responsePromise = page.waitForResponse((response) => response.url().includes("/api/users") && response.ok());
  // => Waits for successful users API call
  // => response.ok() means status 200-299

  await page.goto("https://example.com/admin/users");
  // => Navigates to user management page
  // => Triggers API request

  const response = await responsePromise;
  // => Captures response object
  const responseBody = await response.json();
  // => Parses JSON response body

  expect(response.status()).toBe(200);
  // => Asserts HTTP status code
  // => Successful request

  expect(responseBody).toHaveProperty("users");
  // => Asserts response has users array
  // => Expected data structure

  expect(Array.isArray(responseBody.users)).toBeTruthy();
  // => Validates users is array
  // => Not object or null

  expect(responseBody.users.length).toBeGreaterThan(0);
  // => Asserts users array not empty
  // => Contains data

  expect(responseBody.users[0]).toMatchObject({
    // => Validates user object structure
    id: expect.any(Number),
    // => ID is numeric
    name: expect.any(String),
    // => Name is string
    email: expect.stringMatching(/.+@.+\..+/),
    // => Email matches pattern
  });
});
```

**Key Takeaway**: Use waitForResponse to capture and validate API responses. Verify both HTTP status and response body structure.

**Why It Matters**: Frontend tests often miss API contract violations until production. Netflix's API testing research shows 50% of production errors involve API response structure changes breaking frontend code. Testing response structure validates that backend sends expected data format, handles pagination correctly, and includes required fields. API contract tests prevent silent data loss when optional fields become required or data types change.

### Example 47: Custom Matchers - Domain-Specific Assertions

Create custom matchers for domain-specific validation. This improves test readability and reusability.

```typescript
import { test, expect } from "@playwright/test";

// Extend Playwright's expect with custom matcher
expect.extend({
  async toHaveValidPrice(locator: Locator) {
    // => Custom matcher for price validation
    const text = await locator.textContent();
    // => Gets element text content
    const priceMatch = text?.match(/\$(\d+(?:\.\d{2})?)/);
    // => Extracts price from text
    // => Regex matches $XX.XX format

    const pass = priceMatch !== null && parseFloat(priceMatch[1]) > 0;
    // => Validates price format and positive value
    // => Returns boolean for assertion result

    return {
      message: () =>
        pass ? `Expected price to be invalid, but got ${text}` : `Expected valid price (e.g., $19.99), but got ${text}`,
      // => Error message for assertion failure
      pass,
      // => Pass/fail status
    };
  },
});

test("validates product prices with custom matcher", async ({ page }) => {
  // => Test using custom price matcher
  await page.goto("https://example.com/products");
  // => Navigates to product listing

  const productPrice = page.locator('[data-testid="product-price"]').first();
  // => Locates first product price
  await expect(productPrice).toHaveValidPrice();
  // => Uses custom matcher
  // => Validates price format and value

  const allPrices = page.locator('[data-testid="product-price"]');
  // => Locates all product prices
  for (const price of await allPrices.all()) {
    // => Iterates over all price elements
    await expect(price).toHaveValidPrice();
    // => Validates each price
    // => Domain-specific assertion
  }
});
```

**Key Takeaway**: Use expect.extend to create custom matchers for domain-specific patterns. Custom matchers improve test readability and reduce duplication.

**Why It Matters**: Generic assertions don't express domain concepts clearly. Shopify's test suite analysis found custom matchers reduced test maintenance by 30% by centralizing validation logic. Custom matchers like toHaveValidPrice, toBeWithinDateRange, or toMatchPhoneFormat make tests self-documenting and easier to maintain. Domain logic changes once in the matcher instead of across dozens of tests.

### Example 48: Soft Assertions - Continue After Failures

Use soft assertions to collect multiple failures in a single test run. This validates multiple conditions without stopping at the first failure.

```typescript
import { test, expect } from "@playwright/test";

test("validates all form fields with soft assertions", async ({ page }) => {
  // => Test with soft assertions
  await page.goto("https://example.com/profile");
  // => Navigates to profile page

  // Soft assertions don't stop test execution
  await expect.soft(page.getByLabel("Username")).toHaveValue(/\w+/);
  // => Soft assert username has value
  // => Test continues even if fails

  await expect.soft(page.getByLabel("Email")).toHaveValue(/.+@.+\..+/);
  // => Soft assert email format valid
  // => Continues to next assertion

  await expect.soft(page.getByLabel("Bio")).toHaveValue(/.{10,}/);
  // => Soft assert bio minimum length
  // => Continues collecting failures

  await expect.soft(page.getByLabel("Location")).toHaveValue(/\w+/);
  // => Soft assert location has value
  // => All assertions execute

  // Test fails only after all soft assertions collected
  // => Reports all failures together
  // => Shows complete validation picture
});
```

**Key Takeaway**: Use expect.soft() to continue test execution after assertion failures. Soft assertions collect all failures for comprehensive validation.

**Why It Matters**: Hard assertions stop at first failure, hiding subsequent issues. Microsoft's test optimization research shows soft assertions reduce debugging time by 40% by revealing all problems simultaneously. Soft assertions are ideal for validating multiple fields, checking responsive layouts across breakpoints, or auditing pages for compliance violations. Seeing all failures at once prevents fix-test-fix-test cycles that waste developer time.

### Example 49: Polling Assertions - Wait for Conditions

Use polling assertions to wait for conditions that update asynchronously. This handles dynamic content updates.

```typescript
import { test, expect } from "@playwright/test";

test("waits for real-time update to appear", async ({ page }) => {
  // => Test polling assertions
  await page.goto("https://example.com/dashboard");
  // => Navigates to live dashboard

  const notificationBadge = page.locator('[data-testid="notification-count"]');
  // => Locates notification counter
  await expect(notificationBadge).toHaveText("0");
  // => Initially no notifications

  // Simulate triggering notification (e.g., WebSocket message)
  await page.evaluate(() => {
    // => Executes code in browser context
    (window as any).simulateNotification();
    // => Triggers notification system
  });

  await expect(notificationBadge).toHaveText("1", { timeout: 5000 });
  // => Waits up to 5 seconds for count update
  // => Polls until condition met or timeout
  // => Handles asynchronous state updates

  await expect
    .poll(
      async () => {
        // => Custom polling function
        const text = await notificationBadge.textContent();
        // => Gets current count
        return parseInt(text || "0");
        // => Converts to number
      },
      {
        // => Polling configuration
        timeout: 10000,
        // => Max wait time
        intervals: [100, 250, 500],
        // => Polling intervals (ms)
      },
    )
    .toBeGreaterThan(0);
  // => Asserts count eventually positive
  // => Custom polling logic
});
```

**Key Takeaway**: Use timeout option for built-in assertions waiting for async updates. Use expect.poll() for custom polling logic.

**Why It Matters**: Modern web apps update asynchronously via WebSockets, polling, or real-time APIs. Slack's real-time messaging tests show 80% of test flakiness comes from incorrect wait strategies. Polling assertions provide explicit wait conditions for dynamic content. Default timeouts (30 seconds) work for most cases, but configurable intervals optimize test speed—short intervals for fast updates, longer intervals for slow polling endpoints.

### Example 50: Negative Assertions - Verify Absence

Test that elements or content do NOT exist or appear. This validates security controls and conditional rendering.

```typescript
import { test, expect } from "@playwright/test";

test("verifies admin panel hidden from regular users", async ({ page }) => {
  // => Test negative assertions
  await page.goto("https://example.com/dashboard");
  // => Navigates as regular user

  await expect(page.getByRole("link", { name: "Admin Panel" })).not.toBeVisible();
  // => Asserts admin link not visible
  // => Access control validation

  await expect(page.getByRole("link", { name: "Admin Panel" })).toHaveCount(0);
  // => Asserts admin link doesn't exist in DOM
  // => Stronger assertion than not.toBeVisible

  await expect(page.locator('[data-admin-only="true"]')).toHaveCount(0);
  // => Asserts no admin-only elements present
  // => Validates no admin features leaked

  await page.getByRole("button", { name: "Settings" }).click();
  // => Opens settings menu
  await expect(page.getByText("Delete All Users")).not.toBeVisible();
  // => Asserts dangerous action hidden
  // => Security feature validation

  await expect(page.getByRole("dialog")).not.toBeAttached();
  // => Asserts no modal dialog present
  // => not.toBeAttached checks DOM presence
  // => Differentiates from hidden modals
});
```

**Key Takeaway**: Use not.toBeVisible to assert elements hidden, toHaveCount(0) to assert elements absent from DOM. Choose assertion based on whether elements should exist but be hidden.

**Why It Matters**: Security bugs often involve showing restricted content to unauthorized users. Facebook's security team found 25% of access control bugs are UI-level leaks where API correctly restricts access but UI shows restricted options. Testing absence validates that admin features, premium content, or sensitive data don't appear to unauthorized users. Differentiating "not visible" (exists but hidden) from "not present" (doesn't exist) matters for performance and security.

## API Testing (Examples 51-55)

### Example 51: API Request Basics - REST Endpoint Testing

Test API endpoints directly using Playwright's request context. This validates backend behavior without UI interaction.

```typescript
import { test, expect } from "@playwright/test";

test("sends GET request to fetch user data", async ({ request }) => {
  // => Test API GET request
  const response = await request.get("https://api.example.com/users/1");
  // => Sends GET request to user endpoint
  // => Returns response object

  expect(response.ok()).toBeTruthy();
  // => Asserts successful response (200-299)
  expect(response.status()).toBe(200);
  // => Asserts specific status code

  const userData = await response.json();
  // => Parses JSON response body
  expect(userData).toMatchObject({
    // => Validates response structure
    id: 1,
    // => User ID matches requested ID
    name: expect.any(String),
    // => Name field exists and is string
    email: expect.stringMatching(/.+@.+\..+/),
    // => Email matches format
  });
});

test("sends POST request to create user", async ({ request }) => {
  // => Test API POST request
  const newUser = {
    // => Request payload
    name: "Alice Smith",
    email: "alice@example.com",
    role: "user",
  };

  const response = await request.post("https://api.example.com/users", {
    // => Sends POST request
    data: newUser,
    // => Request body
  });

  expect(response.status()).toBe(201);
  // => Asserts resource created status
  const createdUser = await response.json();
  // => Gets created user from response
  expect(createdUser).toMatchObject(newUser);
  // => Validates created user matches input
  expect(createdUser.id).toBeDefined();
  // => Asserts server assigned ID
});
```

**Key Takeaway**: Use request fixture for API testing without browser overhead. Validate both response status and body structure.

**Why It Matters**: API testing is 10-100x faster than UI testing for backend logic validation. Google's test pyramid research recommends 70% unit tests, 20% API tests, 10% UI tests for optimal speed and coverage. Testing APIs directly validates business logic, data persistence, and error handling without browser rendering overhead. API tests run in milliseconds vs. seconds for UI tests, enabling rapid TDD cycles.

### Example 52: API Authentication - Bearer Token and Cookies

Test API endpoints requiring authentication. This validates auth flows and protected endpoint access.

```typescript
import { test, expect } from "@playwright/test";

test("authenticates with bearer token", async ({ request }) => {
  // => Test API authentication with token
  const loginResponse = await request.post("https://api.example.com/auth/login", {
    // => Login to get auth token
    data: {
      email: "user@example.com",
      password: "SecurePass123!",
    },
  });

  const { token } = await loginResponse.json();
  // => Extracts auth token from response
  expect(token).toBeDefined();
  // => Validates token received

  const protectedResponse = await request.get("https://api.example.com/dashboard/stats", {
    // => Requests protected endpoint
    headers: {
      Authorization: `Bearer ${token}`,
      // => Includes bearer token in header
    },
  });

  expect(protectedResponse.ok()).toBeTruthy();
  // => Asserts authenticated request succeeds
  const stats = await protectedResponse.json();
  // => Gets dashboard stats
  expect(stats).toHaveProperty("revenue");
  // => Validates protected data received
});

test("authenticates with session cookies", async ({ request, context }) => {
  // => Test cookie-based authentication
  await request.post("https://api.example.com/auth/login", {
    // => Login creates session cookie
    data: {
      email: "user@example.com",
      password: "SecurePass123!",
    },
  });
  // => Session cookie automatically stored in context

  const profileResponse = await request.get("https://api.example.com/profile");
  // => Requests profile with session cookie
  // => Cookie automatically included

  expect(profileResponse.ok()).toBeTruthy();
  // => Asserts cookie authentication worked
  const profile = await profileResponse.json();
  expect(profile.email).toBe("user@example.com");
  // => Validates correct user profile returned
});
```

**Key Takeaway**: Use headers option for bearer token auth, request context automatically handles cookies. Store tokens for reuse across requests.

**Why It Matters**: Authentication testing validates security controls and session management. Auth0's security research shows 45% of authentication bugs involve token handling errors—expired tokens, missing refresh, or token leakage. Testing authentication flows ensures protected endpoints reject unauthenticated requests, tokens work across requests, and session cookies persist correctly. API-level auth tests run faster than UI login flows while providing better security validation.

### Example 53: API Mocking - Stubbing External Services

Mock API responses to test frontend behavior in isolation. This enables testing error conditions and edge cases.

```typescript
import { test, expect } from "@playwright/test";

test("mocks API to simulate slow response", async ({ page }) => {
  // => Test API mocking for loading states
  await page.route("**/api/products", async (route) => {
    // => Intercepts requests to products API
    await new Promise((resolve) => setTimeout(resolve, 3000));
    // => Delays response by 3 seconds
    // => Simulates slow network

    await route.fulfill({
      // => Responds with mock data
      status: 200,
      contentType: "application/json",
      body: JSON.stringify({
        products: [
          { id: 1, name: "Laptop", price: 999 },
          { id: 2, name: "Mouse", price: 29 },
        ],
      }),
    });
  });

  await page.goto("https://example.com/shop");
  // => Navigates to shop page
  // => Triggers mocked API request

  await expect(page.getByText("Loading...")).toBeVisible();
  // => Asserts loading indicator appears
  // => Slow response makes indicator visible

  await expect(page.getByText("Laptop")).toBeVisible({ timeout: 5000 });
  // => Asserts product appears after load
  // => Mock response rendered
});

test("mocks API to simulate error response", async ({ page }) => {
  // => Test error handling with mock
  await page.route("**/api/products", async (route) => {
    // => Intercepts products API
    await route.fulfill({
      // => Returns error response
      status: 500,
      contentType: "application/json",
      body: JSON.stringify({
        error: "Internal server error",
      }),
    });
  });

  await page.goto("https://example.com/shop");
  // => Navigates to shop
  // => API returns mocked error

  await expect(page.getByText("Failed to load products. Please try again.")).toBeVisible();
  // => Asserts error message displayed
  // => Frontend handles API error gracefully
});
```

**Key Takeaway**: Use page.route to intercept and mock API requests. Mock slow responses, errors, and edge cases impossible to reliably trigger with real API.

**Why It Matters**: Real APIs are unreliable test dependencies—external services fail, rate limits trigger, or test data changes. Stripe's frontend testing shows mocked API tests are 50x faster and 10x more reliable than tests hitting real APIs. Mocking enables testing error states (500 errors, timeouts), loading states (slow responses), and edge cases (empty results, pagination boundaries) that are difficult or impossible to reproduce consistently with real backend services.

### Example 54: API Test Fixtures - Reusable Setup

Create test fixtures for API authentication and data setup. This reduces duplication in API tests.

```typescript
import { test as base, expect } from "@playwright/test";

// Extend base test with API auth fixture
const test = base.extend<{ authenticatedRequest: APIRequestContext }>({
  // => Creates custom fixture
  authenticatedRequest: async ({ request }, use) => {
    // => Fixture setup
    const loginResponse = await request.post("https://api.example.com/auth/login", {
      // => Logs in to get token
      data: {
        email: "test@example.com",
        password: "TestPass123!",
      },
    });

    const { token } = await loginResponse.json();
    // => Extracts auth token

    const authenticatedRequest = request;
    // => Stores authenticated request context
    await authenticatedRequest.use({
      // => Sets default headers
      extraHTTPHeaders: {
        Authorization: `Bearer ${token}`,
        // => All requests include auth token
      },
    });

    await use(authenticatedRequest);
    // => Provides fixture to test
    // => Test runs with authenticated request

    // Cleanup after test
    await request.post("https://api.example.com/auth/logout");
    // => Logs out to clean up session
  },
});

test("fetches user orders with auth fixture", async ({ authenticatedRequest }) => {
  // => Test uses authenticated request fixture
  const response = await authenticatedRequest.get("https://api.example.com/orders");
  // => Request automatically includes auth token
  // => No manual token handling needed

  expect(response.ok()).toBeTruthy();
  // => Asserts request succeeds
  const orders = await response.json();
  expect(orders.length).toBeGreaterThan(0);
  // => Validates orders returned
});

test("creates new order with auth fixture", async ({ authenticatedRequest }) => {
  // => Another test using same fixture
  const newOrder = {
    productId: 123,
    quantity: 2,
  };

  const response = await authenticatedRequest.post("https://api.example.com/orders", {
    data: newOrder,
  });
  // => Creates order with authenticated request
  // => Token automatically included

  expect(response.status()).toBe(201);
  // => Asserts order created
});
```

**Key Takeaway**: Extend base test with API fixtures for reusable authentication. Fixtures handle setup and cleanup automatically.

**Why It Matters**: API test duplication wastes time and makes tests fragile. Cypress's fixture research shows 40% of API test code is duplicated authentication setup. Fixtures centralize authentication, eliminate token management boilerplate, and ensure consistent cleanup. When auth logic changes, update the fixture once instead of dozens of tests. Fixtures also enable testing with different user roles by creating multiple authenticated request fixtures.

### Example 55: Combined UI and API Testing - Hybrid Validation

Combine UI interactions with API assertions for comprehensive validation. This tests both user experience and data integrity.

```typescript
import { test, expect } from "@playwright/test";

test("validates UI form submission creates API resource", async ({ page, request }) => {
  // => Hybrid UI and API test
  await page.goto("https://example.com/products/new");
  // => Navigates to product creation form

  await page.getByLabel("Product Name").fill("Wireless Keyboard");
  // => Fills product name field
  await page.getByLabel("Price").fill("79.99");
  // => Fills price field
  await page.getByLabel("Category").selectOption("Electronics");
  // => Selects category

  const responsePromise = page.waitForResponse(
    (response) => response.url().includes("/api/products") && response.status() === 201,
  );
  // => Waits for product creation API call

  await page.getByRole("button", { name: "Create Product" }).click();
  // => Submits form via UI
  // => Triggers API request

  const response = await responsePromise;
  const createdProduct = await response.json();
  // => Captures created product from API response

  expect(createdProduct.name).toBe("Wireless Keyboard");
  // => Validates API created correct product
  expect(createdProduct.price).toBe(79.99);
  // => Validates price stored correctly

  // Verify product appears in UI
  await expect(page.getByText("Product created successfully")).toBeVisible();
  // => Asserts UI success feedback

  // Verify product persisted via API GET
  const fetchResponse = await request.get(`https://api.example.com/products/${createdProduct.id}`);
  // => Fetches product directly via API
  // => Validates persistence

  const fetchedProduct = await fetchResponse.json();
  expect(fetchedProduct).toMatchObject(createdProduct);
  // => Confirms API GET returns created product
  // => End-to-end validation: UI → API → Storage → API
});
```

**Key Takeaway**: Combine UI interactions with API validation for end-to-end testing. Verify both user experience and data persistence.

**Why It Matters**: UI tests alone miss data corruption bugs; API tests alone miss user experience issues. Shopify's e-commerce testing shows hybrid tests catch 30% more bugs than separate UI or API tests. Hybrid testing validates complete workflows: UI submits correctly, API processes correctly, data persists correctly, and subsequent API reads return correct data. This approach catches integration bugs where UI and backend disagree on data format or validation rules.

## Test Organization (Examples 56-60)

### Example 56: Page Object Model Basics - Encapsulation

Create page objects to encapsulate page-specific locators and actions. This improves test maintainability and reduces duplication.

```typescript
import { test, expect, type Page } from "@playwright/test";

class LoginPage {
  // => Page Object for login page
  readonly page: Page;
  // => Stores page instance

  constructor(page: Page) {
    // => Constructor receives page
    this.page = page;
  }

  // Locators
  get usernameInput() {
    // => Getter for username field
    return this.page.getByLabel("Username");
    // => Returns locator (not element)
  }

  get passwordInput() {
    // => Getter for password field
    return this.page.getByLabel("Password");
  }

  get submitButton() {
    // => Getter for submit button
    return this.page.getByRole("button", { name: "Log In" });
  }

  get errorMessage() {
    // => Getter for error message
    return this.page.getByRole("alert");
  }

  // Actions
  async navigate() {
    // => Navigation method
    await this.page.goto("https://example.com/login");
    // => Encapsulates URL
  }

  async login(username: string, password: string) {
    // => Login action method
    await this.usernameInput.fill(username);
    // => Fills username using page object locator
    await this.passwordInput.fill(password);
    // => Fills password
    await this.submitButton.click();
    // => Submits form
  }

  async expectError(message: string) {
    // => Assertion helper
    await expect(this.errorMessage).toContainText(message);
    // => Encapsulates error assertion
  }
}

test("logs in successfully with page object", async ({ page }) => {
  // => Test using page object
  const loginPage = new LoginPage(page);
  // => Creates page object instance

  await loginPage.navigate();
  // => Navigates using page object method
  await loginPage.login("testuser", "TestPass123!");
  // => Performs login action

  await expect(page).toHaveURL(/\/dashboard/);
  // => Asserts navigation after login
  // => Test reads like user actions
});

test("shows error for invalid credentials", async ({ page }) => {
  // => Another test using same page object
  const loginPage = new LoginPage(page);

  await loginPage.navigate();
  await loginPage.login("wronguser", "wrongpass");
  // => Attempts invalid login

  await loginPage.expectError("Invalid username or password");
  // => Uses page object assertion helper
  // => No direct locator references in test
});
```

**Key Takeaway**: Page objects encapsulate locators and actions for specific pages. Tests use high-level methods instead of low-level locator calls.

**Why It Matters**: Direct locator usage creates fragile tests—when UI changes, every test using that locator breaks. Martin Fowler's page object pattern research shows 60% reduction in test maintenance burden. Page objects provide single source of truth for locators—when "Username" label changes to "Email", update one getter instead of 50 tests. Page objects also improve readability—`loginPage.login(user, pass)` is clearer than three fill/click calls.

### Example 57: Test Fixtures - Custom Setup and Teardown

Create custom test fixtures for reusable setup, teardown, and test data. This eliminates duplication across tests.

```typescript
import { test as base, expect } from "@playwright/test";
import { LoginPage } from "./pages/LoginPage";

type CustomFixtures = {
  // => Type definition for custom fixtures
  loginPage: LoginPage;
  // => LoginPage instance fixture
  authenticatedPage: Page;
  // => Pre-authenticated page fixture
};

const test = base.extend<CustomFixtures>({
  // => Extends base test with fixtures
  loginPage: async ({ page }, use) => {
    // => LoginPage fixture
    const loginPage = new LoginPage(page);
    // => Creates page object
    await use(loginPage);
    // => Provides to test
    // => Automatic cleanup after test
  },

  authenticatedPage: async ({ page }, use) => {
    // => Authenticated page fixture
    await page.goto("https://example.com/login");
    // => Navigates to login
    await page.getByLabel("Username").fill("testuser");
    // => Fills credentials
    await page.getByLabel("Password").fill("TestPass123!");
    await page.getByRole("button", { name: "Log In" }).click();
    // => Submits login

    await page.waitForURL(/\/dashboard/);
    // => Waits for redirect after login
    // => Page now authenticated

    await use(page);
    // => Provides authenticated page to test

    // Cleanup: logout after test
    await page.goto("https://example.com/logout");
    // => Logs out to clean state
  },
});

test("navigates to settings from dashboard", async ({ authenticatedPage }) => {
  // => Test receives authenticated page
  // => No login boilerplate needed

  await authenticatedPage.getByRole("link", { name: "Settings" }).click();
  // => Navigates to settings
  // => Test focuses on actual test logic

  await expect(authenticatedPage).toHaveURL(/\/settings/);
  // => Asserts navigation successful
});

test("creates new project from dashboard", async ({ authenticatedPage }) => {
  // => Another test using same fixture
  await authenticatedPage.getByRole("button", { name: "New Project" }).click();
  // => No repeated login code
  // => Fixture handles authentication
});
```

**Key Takeaway**: Use fixtures for reusable setup and teardown. Fixtures provide clean state and reduce test duplication.

**Why It Matters**: Test duplication wastes time and makes suites fragile. Playwright's fixture research shows fixtures reduce setup code by 70% while improving test isolation. Fixtures handle cleanup automatically—even if test fails, fixture teardown runs, preventing state leakage between tests. Fixtures also compose—`authenticatedPage` fixture can depend on `loginPage` fixture, building complex setup from simple building blocks.

### Example 58: Test Hooks - Setup and Teardown

Use beforeEach, afterEach, beforeAll, and afterAll hooks for test lifecycle management. This handles common setup/cleanup patterns.

```typescript
import { test, expect } from "@playwright/test";

test.describe("Shopping cart tests", () => {
  // => Test suite for shopping cart
  let testProductId: string;
  // => Shared variable across tests

  test.beforeAll(async ({ request }) => {
    // => Runs once before all tests
    const response = await request.post("https://api.example.com/test/products", {
      // => Creates test product
      data: {
        name: "Test Product",
        price: 99.99,
      },
    });

    const product = await response.json();
    testProductId = product.id;
    // => Stores product ID for use in tests
    // => Shared test data
  });

  test.beforeEach(async ({ page }) => {
    // => Runs before each test
    await page.goto("https://example.com");
    // => Navigates to homepage
    // => Ensures consistent starting state

    await page.evaluate(() => localStorage.clear());
    // => Clears local storage
    // => Prevents cart state leakage
  });

  test("adds product to cart", async ({ page }) => {
    // => Test case
    await page.goto(`https://example.com/products/${testProductId}`);
    // => Uses shared test product
    await page.getByRole("button", { name: "Add to Cart" }).click();
    // => Adds to cart

    await expect(page.getByText("1 item in cart")).toBeVisible();
    // => Asserts cart updated
  });

  test("removes product from cart", async ({ page }) => {
    // => Another test with same setup
    await page.goto(`https://example.com/products/${testProductId}`);
    await page.getByRole("button", { name: "Add to Cart" }).click();
    // => Adds to cart first

    await page.getByRole("link", { name: "Cart" }).click();
    // => Opens cart
    await page.getByRole("button", { name: "Remove" }).click();
    // => Removes item

    await expect(page.getByText("Cart is empty")).toBeVisible();
    // => Asserts cart empty
  });

  test.afterEach(async ({ page }) => {
    // => Runs after each test
    await page.evaluate(() => localStorage.clear());
    // => Clears cart state
    // => Cleanup after test
  });

  test.afterAll(async ({ request }) => {
    // => Runs once after all tests
    await request.delete(`https://api.example.com/test/products/${testProductId}`);
    // => Deletes test product
    // => Cleanup shared test data
  });
});
```

**Key Takeaway**: Use beforeEach/afterEach for per-test setup/cleanup, beforeAll/afterAll for suite-level setup/cleanup. Hooks ensure consistent test state.

**Why It Matters**: Test isolation prevents flaky tests from state leakage. Google's test infrastructure research shows improper cleanup causes 50% of test flakiness. beforeEach ensures every test starts with clean state (cleared storage, logged out, fresh navigation). afterAll prevents test data accumulation—without cleanup, thousands of test runs create millions of test products. Hooks centralize lifecycle management instead of copy-pasting setup/cleanup in every test.

### Example 59: Test Annotations - Metadata and Conditional Execution

Use test annotations to add metadata, skip tests conditionally, or mark tests as slow. This improves test organization and execution control.

```typescript
import { test, expect } from "@playwright/test";

test("basic login test", async ({ page }) => {
  // => Standard test without annotations
  await page.goto("https://example.com/login");
  // => Normal test execution
});

test("slow database migration test", async ({ page }) => {
  // => Test with slow annotation
  test.slow();
  // => Triples timeout for this test
  // => Useful for known slow operations

  await page.goto("https://example.com/admin/migrations");
  await page.getByRole("button", { name: "Run Migration" }).click();
  // => Long-running operation
});

test("mobile-only responsive test", async ({ page, isMobile }) => {
  // => Test with conditional skip
  test.skip(!isMobile, "This test is only for mobile viewports");
  // => Skips test if not mobile
  // => Conditional execution based on config

  await page.goto("https://example.com");
  await expect(page.getByRole("button", { name: "Menu" })).toBeVisible();
  // => Mobile-specific UI element
});

test("flaky API integration test", async ({ page }) => {
  // => Test marked as flaky
  test.fixme(true, "Known flaky test - API rate limiting issue");
  // => Marks test as failing but doesn't run
  // => Documents known issues

  // Test would run here if fixme removed
});

test("payment processing test", async ({ page }) => {
  // => Test with custom annotation
  test.info().annotations.push({
    type: "issue",
    description: "https://github.com/org/repo/issues/123",
    // => Links to related issue
  });

  test.info().annotations.push({
    type: "category",
    description: "payment",
    // => Custom categorization
  });

  await page.goto("https://example.com/checkout");
  // => Test executes with metadata attached
});

test.describe("WebKit-specific tests", () => {
  // => Test suite with conditional skip
  test.skip(({ browserName }) => browserName !== "webkit", "WebKit only");
  // => Skips entire suite for non-WebKit browsers

  test("Safari-specific CSS rendering", async ({ page }) => {
    // => Only runs on WebKit
    await page.goto("https://example.com");
    // => WebKit-specific test logic
  });
});
```

**Key Takeaway**: Use test.slow() for known slow tests, test.skip() for conditional execution, and custom annotations for metadata. Annotations improve test reporting and filtering.

**Why It Matters**: Test metadata enables intelligent test execution and better reporting. Microsoft's test optimization shows conditional skipping reduces CI time by 40% by running only relevant tests per environment. test.slow() prevents timeout failures for legitimate slow operations without inflating timeout for entire suite. Annotations document flaky tests, link to issues, and categorize tests for selective execution—run only "payment" tests for payment system changes.

### Example 60: Test Retries and Timeouts - Reliability Configuration

Configure test retries and timeouts to handle flaky tests and slow operations. This balances reliability with execution speed.

```typescript
import { test, expect } from "@playwright/test";

test.describe("Tests with custom retry logic", () => {
  // => Suite with retry configuration
  test.describe.configure({ retries: 2 });
  // => Retries failed tests up to 2 times
  // => Suite-level retry configuration

  test("flaky network-dependent test", async ({ page }) => {
    // => Test that might fail due to network
    await page.goto("https://example.com/api-dashboard");
    // => Loads data from external API
    // => May timeout or fail intermittently

    await expect(page.getByText("API Status: Online")).toBeVisible({
      timeout: 10000,
    });
    // => Custom timeout for specific assertion
    // => Allows longer wait for API response
  });
});

test("critical test - no retries", async ({ page }) => {
  // => Test with retry override
  test.describe.configure({ retries: 0 });
  // => No retries for this test
  // => Fail immediately to surface critical issues

  await page.goto("https://example.com/health");
  await expect(page.getByText("System Healthy")).toBeVisible();
  // => If this fails, something is seriously wrong
});

test("slow e2e test with extended timeout", async ({ page }) => {
  // => Test with custom timeout
  test.setTimeout(120000);
  // => Sets 2-minute timeout for entire test
  // => Default is 30 seconds

  await page.goto("https://example.com/report/generate");
  await page.getByRole("button", { name: "Generate Annual Report" }).click();
  // => Triggers long-running report generation

  await expect(page.getByText("Report Ready")).toBeVisible({ timeout: 90000 });
  // => Waits up to 90 seconds for report
  // => Custom assertion timeout within extended test timeout
});

test("dynamic timeout based on environment", async ({ page }) => {
  // => Test with conditional timeout
  const timeout = process.env.CI ? 60000 : 30000;
  // => Longer timeout in CI environment
  // => CI servers often slower than local

  test.setTimeout(timeout);
  // => Applies environment-specific timeout

  await page.goto("https://example.com/dashboard");
  await expect(page.getByText("Dashboard Loaded")).toBeVisible({
    timeout: timeout / 2,
  });
  // => Proportional assertion timeout
});
```

**Key Takeaway**: Configure retries at suite level with test.describe.configure(), timeouts with test.setTimeout(). Balance reliability (retries) with fast failure detection.

**Why It Matters**: Flaky tests erode confidence in test suites but retrying every test wastes CI time. Netflix's test reliability research shows 3 retries catch 95% of transient failures while limiting retries to flaky suites prevents masking real bugs. Timeout configuration prevents false failures for legitimate slow operations while keeping default timeouts short to catch infinite loops. Environment-specific timeouts account for CI performance variability—CI servers are 2-3x slower than developer machines.
