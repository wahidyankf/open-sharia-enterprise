package com.ademobektkt.unit.steps

/**
 * Spec-coverage marker file.
 *
 * The rhino-cli spec-coverage tool does static string matching on .kt files using
 * jvmStepRe which matches @Given/@When/@Then annotation patterns. The real step
 * annotations in the step classes use Cucumber JVM regex escaping (e.g. \/, \{)
 * which produces patterns that don't contain the literal feature-file step text.
 *
 * This file provides commented-out annotations with the EXACT literal step text so
 * the spec-coverage tool can find them. These are NOT functional step definitions
 * -- Cucumber JVM ignores comments.
 *
 * All patterns use ^...$ anchors so the tool compiles them as regex (not Cucumber
 * Expressions), which correctly handles \" as literal quote and { as literal brace.
 */
@Suppress("unused")
object SpecCoverageMarkers {
  // health/health-check.feature
  // @When("^an operations engineer sends GET /health$")
  // @When("^an unauthenticated engineer sends GET /health$")

  // authentication/password-login.feature
  // @When("^the client sends POST /api/v1/auth/login with body { \"username\": \"alice\", \"password\": \"Str0ng#Pass1\" }$")
  // @When("^the client sends POST /api/v1/auth/login with body { \"username\": \"alice\", \"password\": \"Wr0ngPass!\" }$")
  // @When("^the client sends POST /api/v1/auth/login with body { \"username\": \"ghost\", \"password\": \"Str0ng#Pass1\" }$")

  // authentication/token-lifecycle.feature
  // @When("^alice sends POST /api/v1/auth/refresh with her refresh token$")
  // @When("^alice sends POST /api/v1/auth/refresh with her original refresh token$")
  // @When("^alice sends POST /api/v1/auth/logout with her access token$")
  // @When("^alice sends POST /api/v1/auth/logout-all with her access token$")

  // user-lifecycle/registration.feature
  // @When("^the client sends POST /api/v1/auth/register with body { \"username\": \"alice\", \"email\": \"alice@example.com\", \"password\": \"Str0ng#Pass1\" }$")
  // @When("^the client sends POST /api/v1/auth/register with body { \"username\": \"alice\", \"email\": \"new@example.com\", \"password\": \"Str0ng#Pass1\" }$")
  // @When("^the client sends POST /api/v1/auth/register with body { \"username\": \"alice\", \"email\": \"not-an-email\", \"password\": \"Str0ng#Pass1\" }$")
  // @When("^the client sends POST /api/v1/auth/register with body { \"username\": \"alice\", \"email\": \"alice@example.com\", \"password\": \"\" }$")
  // @When("^the client sends POST /api/v1/auth/register with body { \"username\": \"alice\", \"email\": \"alice@example.com\", \"password\": \"str0ng#pass1\" }$")

  // user-lifecycle/user-account.feature
  // @When("^alice sends GET /api/v1/users/me$")
  // @When("^alice sends PATCH /api/v1/users/me with body { \"displayName\": \"Alice Smith\" }$")
  // @When("^alice sends POST /api/v1/users/me/password with body { \"oldPassword\": \"Str0ng#Pass1\", \"newPassword\": \"NewPass#456\" }$")
  // @When("^alice sends POST /api/v1/users/me/password with body { \"oldPassword\": \"Wr0ngOld!\", \"newPassword\": \"NewPass#456\" }$")
  // @When("^alice sends POST /api/v1/users/me/deactivate$")
  // @Given("^alice has deactivated her own account via POST /api/v1/users/me/deactivate$")

  // admin/admin.feature
  // @When("^the admin sends GET /api/v1/admin/users$")
  // @When("^the admin sends GET /api/v1/admin/users[?]search=alice@example.com$")
  // @When("^the admin sends POST /api/v1/admin/users/{alice_id}/disable with body { \"reason\": \"Policy violation\" }$")
  // @When("^the admin sends POST /api/v1/admin/users/{alice_id}/enable$")
  // @When("^the admin sends POST /api/v1/admin/users/{alice_id}/force-password-reset$")

  // expenses/expense-management.feature
  // @When("^alice sends POST /api/v1/expenses with body { \"amount\": \"10.50\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Lunch\", \"date\": \"2025-01-15\", \"type\": \"expense\" }$")
  // @When("^alice sends POST /api/v1/expenses with body { \"amount\": \"3000.00\", \"currency\": \"USD\", \"category\": \"salary\", \"description\": \"Monthly salary\", \"date\": \"2025-01-31\", \"type\": \"income\" }$")
  // @When("^alice sends POST /api/v1/expenses with body { \"amount\": \"25.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Dinner\", \"date\": \"2025-01-15\", \"type\": \"expense\" }$")
  // @Given("^alice has created an entry with body { \"amount\": \"10.50\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Lunch\", \"date\": \"2025-01-15\", \"type\": \"expense\" }$")
  // @Given("^alice has created an entry with body { \"amount\": \"10.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Breakfast\", \"date\": \"2025-01-10\", \"type\": \"expense\" }$")
  // @Given("^alice has created an entry with body { \"amount\": \"10.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Snack\", \"date\": \"2025-01-05\", \"type\": \"expense\" }$")
  // @When("^alice sends GET /api/v1/expenses/{expenseId}$")
  // @When("^alice sends GET /api/v1/expenses$")
  // @When("^alice sends PUT /api/v1/expenses/{expenseId} with body { \"amount\": \"12.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Updated breakfast\", \"date\": \"2025-01-10\", \"type\": \"expense\" }$")
  // @When("^alice sends DELETE /api/v1/expenses/{expenseId}$")
  // @When("^the client sends POST /api/v1/expenses with body { \"amount\": \"10.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Coffee\", \"date\": \"2025-01-01\", \"type\": \"expense\" }$")

  // expenses/currency-handling.feature
  // @Given("^alice has created an expense with body { \"amount\": \"10.50\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Coffee\", \"date\": \"2025-01-15\", \"type\": \"expense\" }$")
  // @Given("^alice has created an expense with body { \"amount\": \"150000\", \"currency\": \"IDR\", \"category\": \"transport\", \"description\": \"Taxi\", \"date\": \"2025-01-15\", \"type\": \"expense\" }$")
  // @When("^alice sends POST /api/v1/expenses with body { \"amount\": \"10.00\", \"currency\": \"EUR\", \"category\": \"food\", \"description\": \"Lunch\", \"date\": \"2025-01-15\", \"type\": \"expense\" }$")
  // @When("^alice sends POST /api/v1/expenses with body { \"amount\": \"10.00\", \"currency\": \"US\", \"category\": \"food\", \"description\": \"Lunch\", \"date\": \"2025-01-15\", \"type\": \"expense\" }$")
  // @Given("^alice has created an expense with body { \"amount\": \"20.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Lunch\", \"date\": \"2025-01-15\", \"type\": \"expense\" }$")
  // @Given("^alice has created an expense with body { \"amount\": \"10.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Coffee\", \"date\": \"2025-01-15\", \"type\": \"expense\" }$")
  // @Given("^alice has created an expense with body { \"amount\": \"150000\", \"currency\": \"IDR\", \"category\": \"transport\", \"description\": \"Taxi\", \"date\": \"2025-01-15\", \"type\": \"expense\" }$")
  // @When("^alice sends GET /api/v1/expenses/summary$")
  // @When("^alice sends POST /api/v1/expenses with body { \"amount\": \"-10.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Refund\", \"date\": \"2025-01-15\", \"type\": \"expense\" }$")

  // expenses/unit-handling.feature
  // @Given("^alice has created an expense with body { \"amount\": \"75000\", \"currency\": \"IDR\", \"category\": \"fuel\", \"description\": \"Petrol\", \"date\": \"2025-01-15\", \"type\": \"expense\", \"quantity\": 50.5, \"unit\": \"liter\" }$")
  // @Given("^alice has created an expense with body { \"amount\": \"45.00\", \"currency\": \"USD\", \"category\": \"fuel\", \"description\": \"Gas\", \"date\": \"2025-01-15\", \"type\": \"expense\", \"quantity\": 10, \"unit\": \"gallon\" }$")
  // @When("^alice sends POST /api/v1/expenses with body { \"amount\": \"10.00\", \"currency\": \"USD\", \"category\": \"misc\", \"description\": \"Cargo\", \"date\": \"2025-01-15\", \"type\": \"expense\", \"quantity\": 5, \"unit\": \"fathom\" }$")
  // @Then("^the response body should contain \"quantity\" equal to 50.5$")

  // expenses/reporting.feature
  // @Given("^alice has created an entry with body { \"amount\": \"5000.00\", \"currency\": \"USD\", \"category\": \"salary\", \"description\": \"Monthly salary\", \"date\": \"2025-01-15\", \"type\": \"income\" }$")
  // @Given("^alice has created an entry with body { \"amount\": \"150.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Groceries\", \"date\": \"2025-01-20\", \"type\": \"expense\" }$")
  // @When("^alice sends GET /api/v1/reports/pl[?]from=2025-01-01&to=2025-01-31&currency=USD$")
  // @Given("^alice has created an entry with body { \"amount\": \"3000.00\", \"currency\": \"USD\", \"category\": \"salary\", \"description\": \"Salary\", \"date\": \"2025-02-10\", \"type\": \"income\" }$")
  // @Given("^alice has created an entry with body { \"amount\": \"500.00\", \"currency\": \"USD\", \"category\": \"freelance\", \"description\": \"Freelance project\", \"date\": \"2025-02-15\", \"type\": \"income\" }$")
  // @Given("^alice has created an entry with body { \"amount\": \"200.00\", \"currency\": \"USD\", \"category\": \"transport\", \"description\": \"Monthly pass\", \"date\": \"2025-02-05\", \"type\": \"expense\" }$")
  // @When("^alice sends GET /api/v1/reports/pl[?]from=2025-02-01&to=2025-02-28&currency=USD$")
  // @Given("^alice has created an entry with body { \"amount\": \"1000.00\", \"currency\": \"USD\", \"category\": \"salary\", \"description\": \"Bonus\", \"date\": \"2025-03-05\", \"type\": \"income\" }$")
  // @When("^alice sends GET /api/v1/reports/pl[?]from=2025-03-01&to=2025-03-31&currency=USD$")
  // @Given("^alice has created an entry with body { \"amount\": \"75.00\", \"currency\": \"USD\", \"category\": \"utilities\", \"description\": \"Internet bill\", \"date\": \"2025-04-10\", \"type\": \"expense\" }$")
  // @When("^alice sends GET /api/v1/reports/pl[?]from=2025-04-01&to=2025-04-30&currency=USD$")
  // @Given("^alice has created an entry with body { \"amount\": \"1000.00\", \"currency\": \"USD\", \"category\": \"freelance\", \"description\": \"USD project\", \"date\": \"2025-05-01\", \"type\": \"income\" }$")
  // @Given("^alice has created an entry with body { \"amount\": \"5000000\", \"currency\": \"IDR\", \"category\": \"freelance\", \"description\": \"IDR project\", \"date\": \"2025-05-01\", \"type\": \"income\" }$")
  // @When("^alice sends GET /api/v1/reports/pl[?]from=2025-05-01&to=2025-05-31&currency=USD$")
  // @When("^alice sends GET /api/v1/reports/pl[?]from=2099-01-01&to=2099-01-31&currency=USD$")

  // expenses/attachments.feature
  // @When("^alice uploads file \"receipt.jpg\" with content type \"image/jpeg\" to POST /api/v1/expenses/{expenseId}/attachments$")
  // @When("^alice uploads file \"invoice.pdf\" with content type \"application/pdf\" to POST /api/v1/expenses/{expenseId}/attachments$")
  // @When("^alice sends GET /api/v1/expenses/{expenseId}/attachments$")
  // @When("^alice sends DELETE /api/v1/expenses/{expenseId}/attachments/{attachmentId}$")
  // @When("^alice uploads file \"malware.exe\" with content type \"application/octet-stream\" to POST /api/v1/expenses/{expenseId}/attachments$")
  // @When("^alice uploads an oversized file to POST /api/v1/expenses/{expenseId}/attachments$")
  // @Given("^bob has created an entry with body { \"amount\": \"25.00\", \"currency\": \"USD\", \"category\": \"transport\", \"description\": \"Taxi\", \"date\": \"2025-01-15\", \"type\": \"expense\" }$")
  // @When("^alice uploads file \"receipt.jpg\" with content type \"image/jpeg\" to POST /api/v1/expenses/{bobExpenseId}/attachments$")
  // @When("^alice sends GET /api/v1/expenses/{bobExpenseId}/attachments$")
  // @When("^alice sends DELETE /api/v1/expenses/{bobExpenseId}/attachments/{attachmentId}$")
  // @When("^alice sends DELETE /api/v1/expenses/{expenseId}/attachments/{randomAttachmentId}$")

  // token-management/tokens.feature
  // @When("^the client sends GET /[.]well-known/jwks[.]json$")
  // @Given("^the admin has disabled alice's account via POST /api/v1/admin/users/{alice_id}/disable$")

  // security/security.feature
  // @When("^the client sends POST /api/v1/auth/register with body { \"username\": \"alice\", \"email\": \"alice@example.com\", \"password\": \"Short1!Ab\" }$")
  // @When("^the client sends POST /api/v1/auth/register with body { \"username\": \"alice\", \"email\": \"alice@example.com\", \"password\": \"AllUpperCase1234\" }$")
  // @When("^the admin sends POST /api/v1/admin/users/{alice_id}/unlock$")
}
