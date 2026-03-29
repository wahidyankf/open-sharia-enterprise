package com.ademobejasb.integration.token_lifecycle;

import com.ademobejasb.auth.repository.RefreshTokenRepository;
import com.ademobejasb.contracts.AuthTokens;
import com.ademobejasb.auth.service.AccountNotActiveException;
import com.ademobejasb.auth.service.AuthService;
import com.ademobejasb.auth.service.InvalidTokenException;
import com.ademobejasb.auth.service.TokenExpiredException;
import com.ademobejasb.integration.ResponseStore;
import com.ademobejasb.integration.steps.TokenStore;
import com.ademobejasb.security.JwtUtil;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;

import static org.assertj.core.api.Assertions.assertThat;

@Scope("cucumber-glue")
public class TokenLifecycleSteps {

    @Autowired
    private AuthService authService;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private RefreshTokenRepository refreshTokenRepository;

    @Autowired
    private JwtUtil jwtUtil;

    @When("^alice sends POST /api/v1/auth/refresh with her refresh token$")
    public void aliceSendsPostRefreshWithHerRefreshToken() {
        String rt = tokenStore.getRefreshToken();
        if (rt == null) {
            throw new IllegalStateException("Refresh token not stored");
        }
        performRefresh(rt);
    }

    @Given("alice's refresh token has expired")
    public void alicesRefreshTokenHasExpired() {
        String rt = tokenStore.getRefreshToken();
        if (rt == null) {
            return;
        }
        String tokenHash = hashToken(rt);
        refreshTokenRepository.findByTokenHash(tokenHash).ifPresent(token -> {
            token.setExpiresAt(java.time.Instant.now().minus(1, java.time.temporal.ChronoUnit.DAYS));
            refreshTokenRepository.save(token);
        });
    }

    @Given("alice has used her refresh token to get a new token pair")
    public void aliceHasUsedHerRefreshTokenToGetANewTokenPair() {
        String originalRt = tokenStore.getRefreshToken();
        tokenStore.setOriginalRefreshToken(originalRt);
        try {
            AuthTokens response = authService.refresh(originalRt);
            tokenStore.setToken(response.getAccessToken());
            tokenStore.setRefreshToken(response.getRefreshToken());
        } catch (InvalidTokenException | AccountNotActiveException e) {
            throw new RuntimeException("Unexpected refresh failure: " + e.getMessage(), e);
        }
    }

    @When("^alice sends POST /api/v1/auth/refresh with her original refresh token$")
    public void aliceSendsPostRefreshWithHerOriginalRefreshToken() {
        String originalRt = tokenStore.getOriginalRefreshToken();
        if (originalRt == null) {
            throw new IllegalStateException("Original refresh token not stored");
        }
        performRefresh(originalRt);
    }

    @When("^alice sends POST /api/v1/auth/logout with her access token$")
    public void aliceSendsPostLogoutWithHerAccessToken() {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        authService.logout(token);
        responseStore.setResponse(200);
    }

    @Then("alice's access token should be invalidated")
    public void alicesAccessTokenShouldBeInvalidated() {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        // After logout the token is in the revoked list; a GET /users/me should return 401
        boolean revoked = authService.isTokenRevoked(token);
        assertThat(revoked).isTrue();
    }

    @When("^alice sends POST /api/v1/auth/logout-all with her access token$")
    public void aliceSendsPostLogoutAllWithHerAccessToken() {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        String username = jwtUtil.extractUsername(token);
        authService.logoutAll(token, username);
        responseStore.setResponse(200);
    }

    @Given("alice has already logged out once")
    public void aliceHasAlreadyLoggedOutOnce() {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        authService.logout(token);
    }

    // ============================================================
    // Internal helpers
    // ============================================================

    private void performRefresh(final String rawRefreshToken) {
        try {
            AuthTokens resp = authService.refresh(rawRefreshToken);
            responseStore.setResponse(200, resp);
            tokenStore.setToken(resp.getAccessToken());
            tokenStore.setRefreshToken(resp.getRefreshToken());
        } catch (TokenExpiredException e) {
            responseStore.setResponse(401, Map.of("message", "Token has expired"));
        } catch (InvalidTokenException e) {
            responseStore.setResponse(401, Map.of("message", "Invalid token"));
        } catch (AccountNotActiveException e) {
            responseStore.setResponse(401, Map.of("message", "Account is deactivated or not active"));
        }
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
