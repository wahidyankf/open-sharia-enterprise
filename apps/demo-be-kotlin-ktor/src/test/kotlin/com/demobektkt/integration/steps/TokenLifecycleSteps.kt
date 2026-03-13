package com.demobektkt.integration.steps

import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertEquals

class TokenLifecycleSteps {

    @Then("alice's access token should be invalidated")
    fun alicesAccessTokenShouldBeInvalidated() {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val (status, _) = ServiceDispatcher.getProfile(token)
        assertEquals(401, status, "Expected alice's token to be invalidated (401)")
    }

    @When("alice sends POST \\/api\\/v1\\/auth\\/refresh with her original refresh token")
    fun aliceSendsPostRefreshWithOriginalToken() {
        val originalToken =
            TestWorld.refreshTokens["alice"] ?: error("alice has no original refresh token")
        val (status, body) = ServiceDispatcher.refresh(originalToken)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }
}
