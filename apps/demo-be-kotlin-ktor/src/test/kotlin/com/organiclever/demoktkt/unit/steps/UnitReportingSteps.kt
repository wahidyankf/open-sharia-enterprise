package com.organiclever.demoktkt.unit.steps

import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertTrue

class UnitReportingSteps {

  @When("^alice sends GET /api/v1/reports/pl\\?from=([^&]+)&to=([^&]+)&currency=([A-Z]+)$")
  fun aliceSendsGetReportsPl(from: String, to: String, currency: String) {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) =
      UnitHttpHelper.get("/api/v1/reports/pl?from=$from&to=$to&currency=$currency", token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @Then("the income breakdown should contain {string} with amount {string}")
  fun theIncomeBreakdownShouldContain(category: String, amount: String) {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(
      body.contains(category) && body.contains(amount),
      "Expected income breakdown to contain '$category' with amount '$amount' in: $body",
    )
  }

  @Then("the expense breakdown should contain {string} with amount {string}")
  fun theExpenseBreakdownShouldContain(category: String, amount: String) {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(
      body.contains(category) && body.contains(amount),
      "Expected expense breakdown to contain '$category' with amount '$amount' in: $body",
    )
  }
}
