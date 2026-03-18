package com.demobektkt.unit.steps

import io.cucumber.java.en.Given
import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertTrue

class UnitExpenseSteps {

  private fun createExpenseFromFields(
    username: String,
    amount: String,
    currency: String,
    category: String,
    description: String,
    date: String,
    type: String,
    quantity: Double? = null,
    unit: String? = null,
  ): String {
    val token = UnitTestWorld.accessTokens[username] ?: error("$username has no access token")
    val (status, respBody) =
      UnitServiceDispatcher.createExpense(
        token,
        amount,
        currency,
        category,
        description,
        date,
        type,
        quantity,
        unit,
      )
    assertTrue(status == 201, "Expected 201 creating expense, got $status. Body: $respBody")
    return UnitJsonHelper.getString(respBody, "id") ?: error("No id in expense response: $respBody")
  }

  private fun createExpenseFromKV(username: String, vararg kvPairs: Pair<String, String>): String {
    val map = kvPairs.toMap()
    return createExpenseFromFields(
      username,
      map["amount"] ?: error("missing amount"),
      map["currency"] ?: error("missing currency"),
      map["category"] ?: error("missing category"),
      map["description"] ?: error("missing description"),
      map["date"] ?: error("missing date"),
      map["type"] ?: error("missing type"),
      map["quantity"]?.toDoubleOrNull(),
      map["unit"],
    )
  }

  @Given(
    "alice has created an entry with body \\{ {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string} \\}"
  )
  fun aliceHasCreatedEntryWithBody(
    k1: String,
    v1: String,
    k2: String,
    v2: String,
    k3: String,
    v3: String,
    k4: String,
    v4: String,
    k5: String,
    v5: String,
    k6: String,
    v6: String,
  ) {
    val id =
      createExpenseFromKV("alice", k1 to v1, k2 to v2, k3 to v3, k4 to v4, k5 to v5, k6 to v6)
    UnitTestWorld.expenseIds["alice:last"] = id
  }

  @Given(
    "alice has created an expense with body \\{ {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string} \\}"
  )
  fun aliceHasCreatedExpenseWithSixFields(
    k1: String,
    v1: String,
    k2: String,
    v2: String,
    k3: String,
    v3: String,
    k4: String,
    v4: String,
    k5: String,
    v5: String,
    k6: String,
    v6: String,
  ) {
    val id =
      createExpenseFromKV("alice", k1 to v1, k2 to v2, k3 to v3, k4 to v4, k5 to v5, k6 to v6)
    UnitTestWorld.expenseIds["alice:last"] = id
  }

  @Given(
    "^alice has created an entry with body \\{ \"amount\": \"([^\"]+)\", \"currency\": \"([^\"]+)\", \"category\": \"([^\"]+)\", \"description\": \"([^\"]+)\", \"date\": \"([^\"]+)\", \"type\": \"([^\"]+)\", \"quantity\": ([0-9.]+), \"unit\": \"([^\"]+)\" \\}$"
  )
  fun aliceHasCreatedEntryWithQuantityUnit(
    amount: String,
    currency: String,
    category: String,
    description: String,
    date: String,
    type: String,
    quantity: String,
    unit: String,
  ) {
    val id =
      createExpenseFromFields(
        "alice",
        amount,
        currency,
        category,
        description,
        date,
        type,
        quantity.toDouble(),
        unit,
      )
    UnitTestWorld.expenseIds["alice:last"] = id
  }

  @Given(
    "^alice has created an expense with body \\{ \"amount\": \"([^\"]+)\", \"currency\": \"([^\"]+)\", \"category\": \"([^\"]+)\", \"description\": \"([^\"]+)\", \"date\": \"([^\"]+)\", \"type\": \"([^\"]+)\", \"quantity\": ([0-9.]+), \"unit\": \"([^\"]+)\" \\}$"
  )
  fun aliceHasCreatedExpenseWithQuantityUnit(
    amount: String,
    currency: String,
    category: String,
    description: String,
    date: String,
    type: String,
    quantity: String,
    unit: String,
  ) {
    val id =
      createExpenseFromFields(
        "alice",
        amount,
        currency,
        category,
        description,
        date,
        type,
        quantity.toDouble(),
        unit,
      )
    UnitTestWorld.expenseIds["alice:last"] = id
  }

  @Given("alice has created 3 entries")
  fun aliceHasCreated3Entries() {
    createExpenseFromFields("alice", "10.00", "USD", "food", "Entry1", "2025-01-01", "expense")
    createExpenseFromFields("alice", "20.00", "USD", "food", "Entry2", "2025-01-02", "expense")
    createExpenseFromFields("alice", "30.00", "USD", "food", "Entry3", "2025-01-03", "expense")
  }

  @When("alice sends GET \\/api\\/v1\\/expenses\\/\\{expenseId\\}")
  fun aliceSendsGetExpenseById() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = UnitTestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
    val (status, body) = UnitServiceDispatcher.getExpenseById(token, expenseId)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When("alice sends GET \\/api\\/v1\\/expenses")
  fun aliceSendsGetExpenses() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = UnitServiceDispatcher.listExpenses(token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When("alice sends GET \\/api\\/v1\\/expenses\\/summary")
  fun aliceSendsGetExpensesSummary() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = UnitServiceDispatcher.expenseSummary(token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When(
    "^alice sends PUT /api/v1/expenses/\\{expenseId\\} with body \\{ \"amount\": \"([^\"]+)\", \"currency\": \"([^\"]+)\", \"category\": \"([^\"]+)\", \"description\": \"([^\"]+)\", \"date\": \"([^\"]+)\", \"type\": \"([^\"]+)\" \\}$"
  )
  fun aliceSendsPutExpenseWithBody(
    amount: String,
    currency: String,
    category: String,
    description: String,
    date: String,
    type: String,
  ) {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = UnitTestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
    val (status, respBody) =
      UnitServiceDispatcher.updateExpense(
        token,
        expenseId,
        amount,
        currency,
        category,
        description,
        date,
        type,
      )
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }

  @When("alice sends DELETE \\/api\\/v1\\/expenses\\/\\{expenseId\\}")
  fun aliceSendsDeleteExpense() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = UnitTestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
    val (status, body) = UnitServiceDispatcher.deleteExpense(token, expenseId)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When(
    "^alice sends POST /api/v1/expenses with body \\{ \"amount\": \"([^\"]+)\", \"currency\": \"([^\"]+)\", \"category\": \"([^\"]+)\", \"description\": \"([^\"]+)\", \"date\": \"([^\"]+)\", \"type\": \"([^\"]+)\" \\}$"
  )
  fun aliceSendsPostExpensesWithBody(
    amount: String,
    currency: String,
    category: String,
    description: String,
    date: String,
    type: String,
  ) {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, respBody) =
      UnitServiceDispatcher.createExpense(
        token,
        amount,
        currency,
        category,
        description,
        date,
        type,
      )
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
    if (status == 201) {
      UnitJsonHelper.getString(respBody, "id")?.let { UnitTestWorld.expenseIds["alice:last"] = it }
    }
  }

  @When(
    "^alice sends POST /api/v1/expenses with body \\{ \"amount\": \"([^\"]+)\", \"currency\": \"([^\"]+)\", \"category\": \"([^\"]+)\", \"description\": \"([^\"]+)\", \"date\": \"([^\"]+)\", \"type\": \"([^\"]+)\", \"quantity\": ([0-9.]+), \"unit\": \"([^\"]+)\" \\}$"
  )
  fun aliceSendsPostExpensesWithBodyAndUnit(
    amount: String,
    currency: String,
    category: String,
    description: String,
    date: String,
    type: String,
    quantity: String,
    unit: String,
  ) {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, respBody) =
      UnitServiceDispatcher.createExpense(
        token,
        amount,
        currency,
        category,
        description,
        date,
        type,
        quantity.toDouble(),
        unit,
      )
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
    if (status == 201) {
      UnitJsonHelper.getString(respBody, "id")?.let { UnitTestWorld.expenseIds["alice:last"] = it }
    }
  }

  @Then("the response body should contain {string} total equal to {string}")
  fun theResponseBodyShouldContainCurrencyTotalEqualTo(currency: String, total: String) {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(
      body.contains(currency) && body.contains(total),
      "Expected currency '$currency' with total '$total' in: $body",
    )
  }

  @Then("^the response body should contain \"([^\"]+)\" equal to ([0-9]+(?:\\.[0-9]+)?)$")
  fun theResponseBodyShouldContainFieldEqualToNumber(field: String, expected: String) {
    val actual = UnitJsonHelper.getNumber(UnitTestWorld.lastResponseBody, field)
    val expectedDouble = expected.toDouble()
    val actualDouble = actual?.toDoubleOrNull()
    assertTrue(
      actualDouble != null && actualDouble == expectedDouble,
      "Field '$field' expected $expected but got $actual in: ${UnitTestWorld.lastResponseBody}",
    )
  }
}
