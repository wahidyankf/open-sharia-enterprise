package com.organiclever.demoktkt.unit.steps

import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertTrue

class UnitTokenManagementSteps {

  @When("alice decodes her access token payload")
  fun aliceDecodesHerAccessTokenPayload() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = UnitHttpHelper.get("/api/v1/tokens/claims", token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @Then("the token should contain a non-null {string} claim")
  fun theTokenShouldContainNonNullClaim(claim: String) {
    UnitJsonHelper.assertNonNull(UnitTestWorld.lastResponseBody, claim)
  }

  @When("^the client sends GET /\\.well-known/jwks\\.json$")
  fun theClientSendsGetJwks() {
    val (status, body) = UnitHttpHelper.get("/.well-known/jwks.json")
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @Then("^the response body should contain at least one key in the \"keys\" array$")
  fun theResponseBodyShouldContainAtLeastOneKeyInKeysArray() {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(body.contains("keys"), "Expected 'keys' in response: $body")
    assertTrue(body.contains("kty"), "Expected at least one key object in: $body")
  }

  @Then("alice's access token should be recorded as revoked")
  fun alicesAccessTokenShouldBeRecordedAsRevoked() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val decoded = UnitTestWorld.jwtService.decodeTokenUnchecked(token)
    val jti = decoded?.getClaim("jti")?.asString() ?: error("no jti in token")
    val isRevoked = runBlocking { UnitTestWorld.tokenRepo.isRevoked(jti) }
    assertTrue(isRevoked, "Expected alice's token jti=$jti to be revoked")
  }
}
