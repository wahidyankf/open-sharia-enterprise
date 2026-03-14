package com.demobektkt.unit.steps

import io.cucumber.java.en.When

class UnitUserAccountSteps {

  @When("alice sends GET \\/api\\/v1\\/users\\/me")
  fun aliceSendsGetUsersMe() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = UnitServiceDispatcher.getProfile(token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When("^alice sends PATCH /api/v1/users/me with body \\{ \"displayName\": \"([^\"]+)\" \\}$")
  fun aliceSendsPatchUsersMeWithBody(displayName: String) {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, respBody) = UnitServiceDispatcher.updateDisplayName(token, displayName)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }

  @When(
    "^alice sends POST /api/v1/users/me/password with body \\{ \"oldPassword\": \"([^\"]+)\", \"newPassword\": \"([^\"]+)\" \\}$"
  )
  fun aliceSendsPostChangePasswordWithBody(oldPassword: String, newPassword: String) {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, respBody) = UnitServiceDispatcher.changePassword(token, oldPassword, newPassword)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }

  @When("alice sends POST \\/api\\/v1\\/users\\/me\\/deactivate")
  fun aliceSendsPostDeactivate() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, respBody) = UnitServiceDispatcher.deactivate(token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }
}
