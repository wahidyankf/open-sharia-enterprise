package com.organiclever.demoktkt.integration.steps

import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertTrue

class AdminSteps {

  @When("the admin sends GET \\/api\\/v1\\/admin\\/users")
  fun theAdminSendsGetAdminUsers() {
    val token = TestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    val (status, body) = HttpHelper.get("/api/v1/admin/users", token)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When("^the admin sends GET /api/v1/admin/users\\?email=([^@]+@[^\\s]+)$")
  fun theAdminSendsGetAdminUsersWithEmailFilter(email: String) {
    val token = TestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    val (status, body) = HttpHelper.get("/api/v1/admin/users?email=$email", token)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When(
    "^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/disable with body \\{ \"reason\": \"([^\"]+)\" \\}$"
  )
  fun theAdminSendsPostDisableAliceWithBody(reason: String) {
    val aliceId = TestWorld.userIds["alice"] ?: error("alice id not stored")
    val adminToken = TestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    val body = """{"reason":"$reason"}"""
    val (status, respBody) =
      HttpHelper.post("/api/v1/admin/users/$aliceId/disable", body, adminToken)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = respBody
  }

  @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/enable$")
  fun theAdminSendsPostEnableAlice() {
    val aliceId = TestWorld.userIds["alice"] ?: error("alice id not stored")
    val adminToken = TestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    val (status, body) = HttpHelper.post("/api/v1/admin/users/$aliceId/enable", "", adminToken)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/force-password-reset$")
  fun theAdminSendsPostForcePasswordResetAlice() {
    val aliceId = TestWorld.userIds["alice"] ?: error("alice id not stored")
    val adminToken = TestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    val (status, body) =
      HttpHelper.post("/api/v1/admin/users/$aliceId/force-password-reset", "", adminToken)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @Then("^the response body should contain at least one user with \"email\" equal to \"([^\"]+)\"$")
  fun theResponseBodyShouldContainAtLeastOneUserWithEmail(email: String) {
    val body = TestWorld.lastResponseBody
    assertTrue(body.contains(email), "Expected email '$email' in response: $body")
  }
}
