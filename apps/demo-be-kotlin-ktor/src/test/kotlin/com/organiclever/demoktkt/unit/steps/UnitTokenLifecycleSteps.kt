package com.organiclever.demoktkt.unit.steps

import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertEquals

class UnitTokenLifecycleSteps {

  @Then("alice's access token should be invalidated")
  fun alicesAccessTokenShouldBeInvalidated() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, _) = UnitHttpHelper.get("/api/v1/users/me", token)
    assertEquals(401, status, "Expected alice's token to be invalidated (401)")
  }

  @When("alice sends POST \\/api\\/v1\\/auth\\/refresh with her original refresh token")
  fun aliceSendsPostRefreshWithOriginalToken() {
    val originalToken =
      UnitTestWorld.refreshTokens["alice"] ?: error("alice has no original refresh token")
    val newAccessToken = UnitTestWorld.accessTokens["alice:new"]
    val body = """{"refresh_token":"$originalToken"}"""
    val (status, respBody) = UnitHttpHelper.post("/api/v1/auth/refresh", body, newAccessToken)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }
}
