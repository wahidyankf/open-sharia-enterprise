package com.demobejasb.unit.steps;

import com.demobejasb.admin.controller.AdminController;
import com.demobejasb.auth.repository.UserRepository;
import com.demobejasb.auth.service.UsernameAlreadyExistsException;
import com.demobejasb.auth.service.AuthService;
import com.demobejasb.contracts.DisableRequest;
import com.demobejasb.contracts.PasswordResetResponse;
import com.demobejasb.contracts.User;
import com.demobejasb.contracts.UserListResponse;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.http.ResponseEntity;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit-level step definitions for admin feature scenarios (list users, disable, enable, unlock,
 * force password reset).
 */
@Scope("cucumber-glue")
public class UnitAdminSteps {

    @Autowired
    private UnitStateStore stateStore;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AuthService authService;

    @Autowired
    private AdminController adminController;

    @Given("users {string}, {string}, and {string} are registered")
    public void usersAreRegistered(
            final String user1, final String user2, final String user3) {
        registerUser(user1);
        registerUser(user2);
        registerUser(user3);
        userRepository.findByUsername("alice")
                .ifPresent(u -> stateStore.setAliceId(u.getId()));
    }

    @When("^the admin sends GET /api/v1/admin/users$")
    public void theAdminSendsGetAdminUsers() {
        ResponseEntity<UserListResponse> resp = adminController.listUsers(null, 0, 20);
        stateStore.setStatusCode(resp.getStatusCode().value());
        stateStore.setResponseBody(resp.getBody());
    }

    @When("^the admin sends GET /api/v1/admin/users\\?search=alice@example\\.com$")
    public void theAdminSendsGetAdminUsersSearchByEmail() {
        ResponseEntity<UserListResponse> resp =
                adminController.listUsers("alice@example.com", 0, 20);
        stateStore.setStatusCode(resp.getStatusCode().value());
        stateStore.setResponseBody(resp.getBody());
    }

    @Then("the response body should contain at least one user with {string} equal to {string}")
    public void theResponseBodyShouldContainUserWithFieldEqual(
            final String field, final String value) {
        Object body = stateStore.getResponseBody();
        assertThat(body).isInstanceOf(UserListResponse.class);
        UserListResponse resp = (UserListResponse) body;
        assertThat(resp.getContent()).isNotEmpty();
        boolean found = resp.getContent().stream().anyMatch(user -> {
            Object fieldValue = switch (field) {
                case "email" -> user.getEmail();
                case "username" -> user.getUsername();
                case "status" -> user.getStatus() != null ? user.getStatus().getValue() : null;
                default -> null;
            };
            return value.equals(fieldValue);
        });
        assertThat(found).isTrue();
    }

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/disable with body \\{ \"reason\": \"Policy violation\" \\}$")
    public void theAdminDisablesAlice() {
        UUID aliceId = stateStore.getAliceId();
        if (aliceId == null) {
            stateStore.setStatusCode(400);
            return;
        }
        try {
            DisableRequest disableReq = new DisableRequest();
            disableReq.setReason("Policy violation");
            ResponseEntity<User> resp = adminController.disableUser(aliceId, disableReq);
            stateStore.setStatusCode(resp.getStatusCode().value());
            stateStore.setResponseBody(resp.getBody());
        } catch (RuntimeException e) {
            stateStore.setStatusCode(404);
            stateStore.setLastException(e);
        }
    }

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/enable$")
    public void theAdminEnablesAlice() {
        UUID aliceId = stateStore.getAliceId();
        if (aliceId == null) {
            stateStore.setStatusCode(400);
            return;
        }
        try {
            ResponseEntity<User> resp = adminController.enableUser(aliceId);
            stateStore.setStatusCode(resp.getStatusCode().value());
            stateStore.setResponseBody(resp.getBody());
        } catch (RuntimeException e) {
            stateStore.setStatusCode(404);
            stateStore.setLastException(e);
        }
    }

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/force-password-reset$")
    public void theAdminForcesPasswordReset() {
        UUID aliceId = stateStore.getAliceId();
        if (aliceId == null) {
            stateStore.setStatusCode(400);
            return;
        }
        try {
            ResponseEntity<PasswordResetResponse> resp =
                    adminController.forcePasswordReset(aliceId);
            stateStore.setStatusCode(resp.getStatusCode().value());
            stateStore.setResponseBody(resp.getBody());
        } catch (RuntimeException e) {
            stateStore.setStatusCode(404);
            stateStore.setLastException(e);
        }
    }

    // ============================================================
    // Helpers
    // ============================================================

    private void registerUser(final String username) {
        if (userRepository.findByUsername(username).isEmpty()) {
            String password = "alice".equals(username) ? "Str0ng#Pass1" : "Str0ng#Pass1234";
            String email = username + "@example.com";
            try {
                com.demobejasb.contracts.RegisterRequest req =
                        new com.demobejasb.contracts.RegisterRequest();
                req.setUsername(username);
                req.setEmail(email);
                req.setPassword(password);
                authService.register(req);
            } catch (UsernameAlreadyExistsException ignored) {
                // Already registered
            }
        }
        if ("alice".equals(username)) {
            userRepository.findByUsername(username)
                    .ifPresent(u -> stateStore.setAliceId(u.getId()));
        }
    }
}
