package com.organiclever.demoktkt.integration.steps

import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertTrue

class TokenManagementSteps {

    @When("alice decodes her access token payload")
    fun aliceDecodesHerAccessTokenPayload() {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val (status, body) = ServiceDispatcher.tokenClaims(token)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @Then("the token should contain a non-null {string} claim")
    fun theTokenShouldContainNonNullClaim(claim: String) {
        JsonHelper.assertNonNull(TestWorld.lastResponseBody, claim)
    }

    @When("^the client sends GET /\\.well-known/jwks\\.json$")
    fun theClientSendsGetJwks() {
        val (status, body) = ServiceDispatcher.jwks()
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @Then("^the response body should contain at least one key in the \"keys\" array$")
    fun theResponseBodyShouldContainAtLeastOneKeyInKeysArray() {
        val body = TestWorld.lastResponseBody
        assertTrue(body.contains("keys"), "Expected 'keys' in response: $body")
        assertTrue(body.contains("kty"), "Expected at least one key object in: $body")
    }

    @Then("alice's access token should be recorded as revoked")
    fun alicesAccessTokenShouldBeRecordedAsRevoked() {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val decoded = TestWorld.jwtService.decodeTokenUnchecked(token)
        val jti = decoded?.getClaim("jti")?.asString() ?: error("no jti in token")
        val isRevoked = runBlocking { TestWorld.tokenRepo.isRevoked(jti) }
        assertTrue(isRevoked, "Expected alice's token jti=$jti to be revoked")
    }
}
