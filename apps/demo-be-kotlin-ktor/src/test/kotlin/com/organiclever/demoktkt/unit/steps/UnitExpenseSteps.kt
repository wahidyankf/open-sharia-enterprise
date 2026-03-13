package com.organiclever.demoktkt.unit.steps

import io.cucumber.java.en.Given
import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertTrue

class UnitExpenseSteps {

  private fun createExpense(username: String, body: String): String {
    val token = UnitTestWorld.accessTokens[username] ?: error("$username has no access token")
    val (status, respBody) = UnitHttpHelper.post("/api/v1/expenses", body, token)
    assertTrue(status == 201, "Expected 201 creating expense, got $status. Body: $respBody")
    return UnitJsonHelper.getString(respBody, "id") ?: error("No id in expense response: $respBody")
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
    val body = """{"$k1":"$v1","$k2":"$v2","$k3":"$v3","$k4":"$v4","$k5":"$v5","$k6":"$v6"}"""
    val id = createExpense("alice", body)
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
    val body = """{"$k1":"$v1","$k2":"$v2","$k3":"$v3","$k4":"$v4","$k5":"$v5","$k6":"$v6"}"""
    val id = createExpense("alice", body)
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
    val body =
      """{"amount":"$amount","currency":"$currency","category":"$category","description":"$description","date":"$date","type":"$type","quantity":$quantity,"unit":"$unit"}"""
    val id = createExpense("alice", body)
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
    val body =
      """{"amount":"$amount","currency":"$currency","category":"$category","description":"$description","date":"$date","type":"$type","quantity":$quantity,"unit":"$unit"}"""
    val id = createExpense("alice", body)
    UnitTestWorld.expenseIds["alice:last"] = id
  }

  @Given("alice has created 3 entries")
  fun aliceHasCreated3Entries() {
    val bodies =
      listOf(
        """{"amount":"10.00","currency":"USD","category":"food","description":"Entry1","date":"2025-01-01","type":"expense"}""",
        """{"amount":"20.00","currency":"USD","category":"food","description":"Entry2","date":"2025-01-02","type":"expense"}""",
        """{"amount":"30.00","currency":"USD","category":"food","description":"Entry3","date":"2025-01-03","type":"expense"}""",
      )
    bodies.forEach { createExpense("alice", it) }
  }

  @When("alice sends GET \\/api\\/v1\\/expenses\\/\\{expenseId\\}")
  fun aliceSendsGetExpenseById() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = UnitTestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
    val (status, body) = UnitHttpHelper.get("/api/v1/expenses/$expenseId", token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When("alice sends GET \\/api\\/v1\\/expenses")
  fun aliceSendsGetExpenses() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = UnitHttpHelper.get("/api/v1/expenses", token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When("alice sends GET \\/api\\/v1\\/expenses\\/summary")
  fun aliceSendsGetExpensesSummary() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = UnitHttpHelper.get("/api/v1/expenses/summary", token)
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
    val body =
      """{"amount":"$amount","currency":"$currency","category":"$category","description":"$description","date":"$date","type":"$type"}"""
    val (status, respBody) = UnitHttpHelper.put("/api/v1/expenses/$expenseId", body, token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }

  @When("alice sends DELETE \\/api\\/v1\\/expenses\\/\\{expenseId\\}")
  fun aliceSendsDeleteExpense() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = UnitTestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
    val (status, body) = UnitHttpHelper.delete("/api/v1/expenses/$expenseId", token)
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
    val body =
      """{"amount":"$amount","currency":"$currency","category":"$category","description":"$description","date":"$date","type":"$type"}"""
    val (status, respBody) = UnitHttpHelper.post("/api/v1/expenses", body, token)
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
    val body =
      """{"amount":"$amount","currency":"$currency","category":"$category","description":"$description","date":"$date","type":"$type","quantity":$quantity,"unit":"$unit"}"""
    val (status, respBody) = UnitHttpHelper.post("/api/v1/expenses", body, token)
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
