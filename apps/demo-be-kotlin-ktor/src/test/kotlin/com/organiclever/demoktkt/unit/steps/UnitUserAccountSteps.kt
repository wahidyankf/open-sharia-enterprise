package com.organiclever.demoktkt.unit.steps

import io.cucumber.java.en.When

class UnitUserAccountSteps {

  @When("alice sends GET \\/api\\/v1\\/users\\/me")
  fun aliceSendsGetUsersMe() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = UnitHttpHelper.get("/api/v1/users/me", token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When("^alice sends PATCH /api/v1/users/me with body \\{ \"display_name\": \"([^\"]+)\" \\}$")
  fun aliceSendsPatchUsersMeWithBody(displayName: String) {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val body = """{"display_name":"$displayName"}"""
    val (status, respBody) = UnitHttpHelper.patch("/api/v1/users/me", body, token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }

  @When(
    "^alice sends POST /api/v1/users/me/password with body \\{ \"old_password\": \"([^\"]+)\", \"new_password\": \"([^\"]+)\" \\}$"
  )
  fun aliceSendsPostChangePasswordWithBody(oldPassword: String, newPassword: String) {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val body = """{"old_password":"$oldPassword","new_password":"$newPassword"}"""
    val (status, respBody) = UnitHttpHelper.post("/api/v1/users/me/password", body, token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }

  @When("alice sends POST \\/api\\/v1\\/users\\/me\\/deactivate")
  fun aliceSendsPostDeactivate() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, respBody) = UnitHttpHelper.post("/api/v1/users/me/deactivate", "", token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }
}
