package com.organiclever.be.integration.token_lifecycle;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jayway.jsonpath.JsonPath;
import com.organiclever.be.auth.repository.RefreshTokenRepository;
import com.organiclever.be.auth.repository.UserRepository;
import com.organiclever.be.integration.ResponseStore;
import com.organiclever.be.integration.steps.TokenStore;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;

@Scope("cucumber-glue")
public class TokenLifecycleSteps {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private RefreshTokenRepository refreshTokenRepository;

    @Autowired
    private UserRepository userRepository;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @When("^alice sends POST /api/v1/auth/refresh with her refresh token$")
    public void aliceSendsPostRefreshWithHerRefreshToken() throws Exception {
        String rt = tokenStore.getRefreshToken();
        if (rt == null) {
            throw new IllegalStateException("Refresh token not stored");
        }
        String body = objectMapper.writeValueAsString(Map.of("refresh_token", rt));
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/auth/refresh")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
    }

    @Given("alice's refresh token has expired")
    public void alicesRefreshTokenHasExpired() throws Exception {
        // Expire the refresh token in DB
        String rt = tokenStore.getRefreshToken();
        if (rt == null) {
            return;
        }
        // Hash the token to find it in DB
        String tokenHash = hashToken(rt);
        refreshTokenRepository.findByTokenHash(tokenHash).ifPresent(token -> {
            token.setExpiresAt(java.time.Instant.now().minus(1, java.time.temporal.ChronoUnit.DAYS));
            refreshTokenRepository.save(token);
        });
    }

    @Given("alice has used her refresh token to get a new token pair")
    public void aliceHasUsedHerRefreshTokenToGetANewTokenPair() throws Exception {
        String originalRt = tokenStore.getRefreshToken();
        tokenStore.setOriginalRefreshToken(originalRt);
        // Use the refresh token
        String body = objectMapper.writeValueAsString(Map.of("refresh_token", originalRt));
        MvcResult result = mockMvc.perform(
                post("/api/v1/auth/refresh")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andReturn();
        String responseBody = result.getResponse().getContentAsString();
        String newAccessToken = JsonPath.read(responseBody, "$.access_token");
        String newRefreshToken = JsonPath.read(responseBody, "$.refresh_token");
        tokenStore.setToken(newAccessToken);
        tokenStore.setRefreshToken(newRefreshToken);
    }

    @When("^alice sends POST /api/v1/auth/refresh with her original refresh token$")
    public void aliceSendsPostRefreshWithHerOriginalRefreshToken() throws Exception {
        String originalRt = tokenStore.getOriginalRefreshToken();
        if (originalRt == null) {
            throw new IllegalStateException("Original refresh token not stored");
        }
        String body = objectMapper.writeValueAsString(Map.of("refresh_token", originalRt));
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/auth/refresh")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
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

    @Then("alice's access token should be invalidated")
    public void alicesAccessTokenShouldBeInvalidated() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        MvcResult result = mockMvc.perform(
                get("/api/v1/users/me")
                        .header("Authorization", "Bearer " + token))
                .andReturn();
        assertThat(result.getResponse().getStatus()).isEqualTo(401);
    }

    @When("^alice sends POST /api/v1/auth/logout-all with her access token$")
    public void aliceSendsPostLogoutAllWithHerAccessToken() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/auth/logout-all")
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @Given("alice has already logged out once")
    public void aliceHasAlreadyLoggedOutOnce() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        mockMvc.perform(
                post("/api/v1/auth/logout")
                        .header("Authorization", "Bearer " + token))
                .andReturn();
    }

    private String hashToken(final String token) {
        try {
            java.security.MessageDigest md = java.security.MessageDigest.getInstance("SHA-256");
            byte[] hash = md.digest(token.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder();
            for (byte b : hash) {
                sb.append(String.format("%02x", b));
            }
            return sb.toString();
        } catch (java.security.NoSuchAlgorithmException e) {
            throw new RuntimeException("SHA-256 not available", e);
        }
    }
}
