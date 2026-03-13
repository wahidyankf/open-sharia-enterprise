package com.organiclever.demoktkt.unit.steps

import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertTrue

class UnitAdminSteps {

  @When("the admin sends GET \\/api\\/v1\\/admin\\/users")
  fun theAdminSendsGetAdminUsers() {
    val token = UnitTestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    val (status, body) = UnitHttpHelper.get("/api/v1/admin/users", token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When("^the admin sends GET /api/v1/admin/users\\?email=([^@]+@[^\\s]+)$")
  fun theAdminSendsGetAdminUsersWithEmailFilter(email: String) {
    val token = UnitTestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    val (status, body) = UnitHttpHelper.get("/api/v1/admin/users?email=$email", token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When(
    "^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/disable with body \\{ \"reason\": \"([^\"]+)\" \\}$"
  )
  fun theAdminSendsPostDisableAliceWithBody(reason: String) {
    val aliceId = UnitTestWorld.userIds["alice"] ?: error("alice id not stored")
    val adminToken = UnitTestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    val body = """{"reason":"$reason"}"""
    val (status, respBody) =
      UnitHttpHelper.post("/api/v1/admin/users/$aliceId/disable", body, adminToken)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }

  @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/enable$")
  fun theAdminSendsPostEnableAlice() {
    val aliceId = UnitTestWorld.userIds["alice"] ?: error("alice id not stored")
    val adminToken = UnitTestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    val (status, body) = UnitHttpHelper.post("/api/v1/admin/users/$aliceId/enable", "", adminToken)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/force-password-reset$")
  fun theAdminSendsPostForcePasswordResetAlice() {
    val aliceId = UnitTestWorld.userIds["alice"] ?: error("alice id not stored")
    val adminToken = UnitTestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    val (status, body) =
      UnitHttpHelper.post("/api/v1/admin/users/$aliceId/force-password-reset", "", adminToken)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @Then("^the response body should contain at least one user with \"email\" equal to \"([^\"]+)\"$")
  fun theResponseBodyShouldContainAtLeastOneUserWithEmail(email: String) {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(body.contains(email), "Expected email '$email' in response: $body")
  }
}
