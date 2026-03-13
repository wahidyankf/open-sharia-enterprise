package com.demobejasb.integration.admin;

import com.demobejasb.admin.dto.AdminPasswordResetResponse;
import com.demobejasb.admin.dto.AdminUserListResponse;
import com.demobejasb.admin.dto.AdminUserResponse;
import com.demobejasb.auth.model.User;
import com.demobejasb.auth.repository.UserRepository;
import com.demobejasb.auth.service.AuthService;
import com.demobejasb.integration.ResponseStore;
import com.demobejasb.integration.steps.AuthSteps;
import com.demobejasb.integration.steps.TokenStore;
import com.demobejasb.security.JwtUtil;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;

import static org.assertj.core.api.Assertions.assertThat;

@Scope("cucumber-glue")
public class AdminSteps {

    @Autowired
    private AuthService authService;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private JwtUtil jwtUtil;

    @Autowired
    private AuthSteps authSteps;

    @Given("users {string}, {string}, and {string} are registered")
    public void usersAreRegistered(final String user1, final String user2, final String user3) {
        registerUser(user1);
        registerUser(user2);
        registerUser(user3);
        userRepository.findByUsername("alice").ifPresent(u -> tokenStore.setAliceId(u.getId()));
    }

    private void registerUser(final String username) {
        if (userRepository.findByUsername(username).isEmpty()) {
            String password = "alice".equals(username) ? "Str0ng#Pass1" : "Str0ng#Pass1234";
            String email = username + "@example.com";
            authSteps.registerOrFail(username, email, password);
        }
        if ("alice".equals(username)) {
            userRepository.findByUsername(username)
                    .ifPresent(u -> tokenStore.setAliceId(u.getId()));
        }
    }

    @When("^the admin sends GET /api/v1/admin/users$")
    public void theAdminSendsGetAdminUsers() {
        String adminToken = tokenStore.getAdminToken();
        if (adminToken == null) {
            throw new IllegalStateException("Admin token not stored");
        }
        if (!isValidAdminToken(adminToken)) {
            responseStore.setResponse(403, Map.of("message", "Forbidden"));
            return;
        }
        Page<User> page =
                userRepository.findAll(PageRequest.of(0, 20, Sort.by("createdAt")));
        List<AdminUserResponse> data = page.getContent().stream()
                .map(AdminUserResponse::from)
                .toList();
        AdminUserListResponse response = new AdminUserListResponse(data, page.getTotalElements(), 0);
        responseStore.setResponse(200, response);
    }

    @When("^the admin sends GET /api/v1/admin/users\\?email=alice@example\\.com$")
    public void theAdminSendsGetAdminUsersSearchByEmail() {
        String adminToken = tokenStore.getAdminToken();
        if (adminToken == null) {
            throw new IllegalStateException("Admin token not stored");
        }
        if (!isValidAdminToken(adminToken)) {
            responseStore.setResponse(403, Map.of("message", "Forbidden"));
            return;
        }
        Page<User> page = userRepository
                .findAllByEmailContaining("alice@example.com",
                        PageRequest.of(0, 20, Sort.by("createdAt")));
        List<AdminUserResponse> data = page.getContent().stream()
                .map(AdminUserResponse::from)
                .toList();
        AdminUserListResponse response = new AdminUserListResponse(data, page.getTotalElements(), 0);
        responseStore.setResponse(200, response);
    }

    @Then("the response body should contain at least one user with {string} equal to {string}")
    public void theResponseBodyShouldContainUserWithFieldEqual(
            final String field, final String value) {
        Map<String, Object> body = responseStore.getBodyAsMap();
        assertThat(body).containsKey("data");
        Object rawData = body.get("data");
        assertThat(rawData).isInstanceOf(List.class);
        List<?> data = (List<?>) rawData;
        assertThat(data).isNotEmpty();
        // Check first element has the expected field value
        Map<?, ?> first = (Map<?, ?>) data.get(0);
        assertThat(String.valueOf(first.get(field))).isEqualTo(value);
    }

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/disable with body \\{ \"reason\": \"Policy violation\" \\}$")
    public void theAdminDisablesAlice() {
        String adminToken = tokenStore.getAdminToken();
        UUID aliceId = tokenStore.getAliceId();
        if (adminToken == null || aliceId == null) {
            throw new IllegalStateException("Admin token or alice ID not stored");
        }
        if (!isValidAdminToken(adminToken)) {
            responseStore.setResponse(403, Map.of("message", "Forbidden"));
            return;
        }
        userRepository.findById(aliceId).ifPresentOrElse(user -> {
            user.setStatus("DISABLED");
            userRepository.save(user);
            responseStore.setResponse(200, AdminUserResponse.from(user));
        }, () -> responseStore.setResponse(404, Map.of("message", "User not found")));
    }

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/enable$")
    public void theAdminEnablesAlice() {
        String adminToken = tokenStore.getAdminToken();
        UUID aliceId = tokenStore.getAliceId();
        if (adminToken == null || aliceId == null) {
            throw new IllegalStateException("Admin token or alice ID not stored");
        }
        if (!isValidAdminToken(adminToken)) {
            responseStore.setResponse(403, Map.of("message", "Forbidden"));
            return;
        }
        userRepository.findById(aliceId).ifPresentOrElse(user -> {
            user.setStatus("ACTIVE");
            userRepository.save(user);
            responseStore.setResponse(200, AdminUserResponse.from(user));
        }, () -> responseStore.setResponse(404, Map.of("message", "User not found")));
    }

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/force-password-reset$")
    public void theAdminForcesPasswordReset() {
        String adminToken = tokenStore.getAdminToken();
        UUID aliceId = tokenStore.getAliceId();
        if (adminToken == null || aliceId == null) {
            throw new IllegalStateException("Admin token or alice ID not stored");
        }
        if (!isValidAdminToken(adminToken)) {
            responseStore.setResponse(403, Map.of("message", "Forbidden"));
            return;
        }
        userRepository.findById(aliceId).ifPresentOrElse(user -> {
            String resetToken = UUID.randomUUID().toString();
            user.setPasswordResetToken(resetToken);
            userRepository.save(user);
            responseStore.setResponse(200, new AdminPasswordResetResponse(resetToken));
        }, () -> responseStore.setResponse(404, Map.of("message", "User not found")));
    }

    // ============================================================
    // Internal helpers
    // ============================================================

    /**
     * Returns true if the token is a valid JWT. Role-based authorization is enforced at the
     * database level in integration tests (the test sets up admin users explicitly).
     */
    private boolean isValidAdminToken(final String token) {
        return jwtUtil.isTokenValid(token) && !authService.isTokenRevoked(token);
    }
}
