package com.organiclever.be.integration.token_management;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.organiclever.be.auth.repository.RevokedTokenRepository;
import com.organiclever.be.integration.ResponseStore;
import com.organiclever.be.integration.steps.TokenStore;
import com.jayway.jsonpath.JsonPath;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;

@Scope("cucumber-glue")
public class TokenManagementSteps {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private RevokedTokenRepository revokedTokenRepository;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @When("alice decodes her access token payload")
    public void aliceDecodesHerAccessTokenPayload() throws Exception {
        // Decode the JWT token stored in TokenStore
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        // Store the payload - we need to make this available to Then steps
        // We'll use responseStore trick: set a fake result by calling /api/v1/users/me
        // and using the token. But actually the test checks claims in the JWT itself.
        // We'll store the decoded payload as a custom response via a helper endpoint hit.
        // Better: expose the JWT payload via a call to /api/v1/users/me and check the field exists
        // Actually, the test step checks "sub" and "iss" claims in the JWT payload.
        // We need to decode the JWT and check its claims.
        // Store the token payload as JSON response in responseStore via a wrapper.
        // Let's use a simpler approach: decode the JWT base64 middle part.
        String[] parts = token.split("\\.");
        if (parts.length != 3) {
            throw new IllegalArgumentException("Invalid JWT format");
        }
        String payloadJson = new String(
                java.util.Base64.getUrlDecoder().decode(parts[1]),
                java.nio.charset.StandardCharsets.UTF_8);
        // Store as a mock result we can query
        // We need MvcResult... let's use a custom approach.
        // We'll just store the payload in a response via a custom lookup.
        // The simplest: make a GET to /api/v1/users/me (authenticated) and the "sub" and "iss"
        // come from the token not the response. So we create a fake MvcResult that stores
        // the JWT payload as response body.
        responseStore.setResult(
                mockMvc.perform(
                        get("/api/v1/users/me")
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
        // Store the payload for the token claims check
        tokenStore.setToken(token); // Keep the token for claims checking
        // We need to expose the JWT payload - store it in a custom field
        // We'll use a thread-local or just decode in the Then step
        jwtPayloadJson = payloadJson;
    }

    private String jwtPayloadJson = "";

    @Then("the token should contain a non-null {string} claim")
    public void theTokenShouldContainANonNullClaim(final String claim) throws Exception {
        // Parse the stored JWT payload
        if (jwtPayloadJson.isEmpty()) {
            // Re-decode from stored token
            String token = tokenStore.getToken();
            if (token == null) {
                throw new IllegalStateException("No token stored");
            }
            String[] parts = token.split("\\.");
            jwtPayloadJson = new String(
                    java.util.Base64.getUrlDecoder().decode(parts[1]),
                    java.nio.charset.StandardCharsets.UTF_8);
        }
        // Check that the claim exists and is non-null
        Object value = JsonPath.read(jwtPayloadJson, "$." + claim);
        assertThat(value).isNotNull();
    }

    @When("^the client sends GET /.well-known/jwks.json$")
    public void theClientSendsGetJwks() throws Exception {
        responseStore.setResult(
                mockMvc.perform(get("/.well-known/jwks.json"))
                        .andReturn());
    }

    @Then("the response body should contain at least one key in the {string} array")
    public void theResponseBodyShouldContainAtLeastOneKeyInArray(final String field) throws Exception {
        MockMvcResultMatchers.jsonPath("$." + field).isArray()
                .match(responseStore.getResult());
        MockMvcResultMatchers.jsonPath("$." + field + "[0]").exists()
                .match(responseStore.getResult());
    }

    @When("^alice sends POST /api/v1/auth/logout with her access token$")
    public void aliceSendsPostLogoutWithHerAccessToken() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/auth/logout")
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @Then("alice's access token should be recorded as revoked")
    public void alicesAccessTokenShouldBeRecordedAsRevoked() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        assertThat(revokedTokenRepository.existsByToken(token)).isTrue();
    }

    @Given("alice has logged out and her access token is blacklisted")
    public void aliceHasLoggedOutAndTokenIsBlacklisted() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        mockMvc.perform(
                post("/api/v1/auth/logout")
                        .header("Authorization", "Bearer " + token))
                .andReturn();
    }

    @Given("^the admin has disabled alice's account via POST /api/v1/admin/users/\\{alice_id\\}/disable$")
    public void theAdminHasDisabledAlicesAccount() throws Exception {
        String adminToken = tokenStore.getAdminToken();
        UUID aliceId = tokenStore.getAliceId();
        if (adminToken == null || aliceId == null) {
            throw new IllegalStateException("Admin token or alice ID not set");
        }
        mockMvc.perform(
                post("/api/v1/admin/users/" + aliceId + "/disable")
                        .header("Authorization", "Bearer " + adminToken)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(
                                java.util.Map.of("reason", "Test"))))
                .andReturn();
    }
}
