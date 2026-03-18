package com.demobektkt.integration.steps

import io.cucumber.java.en.When

class UserAccountSteps {

  @When("alice sends GET \\/api\\/v1\\/users\\/me")
  fun aliceSendsGetUsersMe() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = ServiceDispatcher.getProfile(token)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When("^alice sends PATCH /api/v1/users/me with body \\{ \"displayName\": \"([^\"]+)\" \\}$")
  fun aliceSendsPatchUsersMeWithBody(displayName: String) {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = ServiceDispatcher.updateDisplayName(token, displayName)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When(
    "^alice sends POST /api/v1/users/me/password with body \\{ \"oldPassword\": \"([^\"]+)\", \"newPassword\": \"([^\"]+)\" \\}$"
  )
  fun aliceSendsPostChangePasswordWithBody(oldPassword: String, newPassword: String) {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = ServiceDispatcher.changePassword(token, oldPassword, newPassword)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When("alice sends POST \\/api\\/v1\\/users\\/me\\/deactivate")
  fun aliceSendsPostDeactivate() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = ServiceDispatcher.deactivate(token)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }
}
