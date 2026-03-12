package com.organiclever.demoktkt.integration.steps

import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertEquals

class TokenLifecycleSteps {

  @Then("alice's access token should be invalidated")
  fun alicesAccessTokenShouldBeInvalidated() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, _) = HttpHelper.get("/api/v1/users/me", token)
    assertEquals(401, status, "Expected alice's token to be invalidated (401)")
  }

  @When("alice sends POST \\/api\\/v1\\/auth\\/refresh with her original refresh token")
  fun aliceSendsPostRefreshWithOriginalToken() {
    // The original refresh token was used and is now revoked (rotation)
    val originalToken =
      TestWorld.refreshTokens["alice"] ?: error("alice has no original refresh token")
    val newAccessToken = TestWorld.accessTokens["alice:new"]
    val body = """{"refresh_token":"$originalToken"}"""
    val (status, respBody) = HttpHelper.post("/api/v1/auth/refresh", body, newAccessToken)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = respBody
  }
}
