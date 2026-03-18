package com.demobejasb.unit.steps;

import com.demobejasb.auth.repository.UserRepository;
import com.demobejasb.auth.service.InvalidCredentialsException;
import com.demobejasb.contracts.ChangePasswordRequest;
import com.demobejasb.contracts.UpdateProfileRequest;
import com.demobejasb.user.controller.UserController;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.When;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.http.ResponseEntity;

/**
 * Unit-level step definitions for user account management (profile, password change, deactivation).
 */
@Scope("cucumber-glue")
public class UnitUserAccountSteps {

    @Autowired
    private UnitStateStore stateStore;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private UserController userController;

    @When("^alice sends GET /api/v1/users/me$")
    public void aliceSendsGetUsersMe() {
        String username = resolveUsername();
        ResponseEntity<com.demobejasb.contracts.User> resp = userController.getProfile(
                UnitAuthSteps.userDetails(username));
        stateStore.setStatusCode(resp.getStatusCode().value());
        stateStore.setResponseBody(resp.getBody());
    }

    @When("^alice sends PATCH /api/v1/users/me with body \\{ \"displayName\": \"Alice Smith\" \\}$")
    public void aliceSendsPatchUsersMeWithDisplayName() {
        String username = resolveUsername();
        UpdateProfileRequest req = new UpdateProfileRequest();
        req.setDisplayName("Alice Smith");
        ResponseEntity<com.demobejasb.contracts.User> resp = userController.updateProfile(
                UnitAuthSteps.userDetails(username), req);
        stateStore.setStatusCode(resp.getStatusCode().value());
        stateStore.setResponseBody(resp.getBody());
    }

    @When("^alice sends POST /api/v1/users/me/password with body \\{ \"oldPassword\": \"Str0ng#Pass1\", \"newPassword\": \"NewPass#456\" \\}$")
    public void aliceSendsPostChangePasswordSuccess() {
        performChangePassword("Str0ng#Pass1", "NewPass#456");
    }

    @When("^alice sends POST /api/v1/users/me/password with body \\{ \"oldPassword\": \"Wr0ngOld!\", \"newPassword\": \"NewPass#456\" \\}$")
    public void aliceSendsPostChangePasswordWrongOld() {
        performChangePassword("Wr0ngOld!", "NewPass#456");
    }

    @When("^alice sends POST /api/v1/users/me/deactivate$")
    public void aliceSendsPostSelfDeactivate() {
        String username = resolveUsername();
        ResponseEntity<Void> resp = userController.deactivate(
                UnitAuthSteps.userDetails(username));
        stateStore.setStatusCode(resp.getStatusCode().value());
        stateStore.setResponseBody(java.util.Map.of("message", "Account deactivated"));
    }

    @Given("^alice has deactivated her own account via POST /api/v1/users/me/deactivate$")
    public void aliceHasDeactivatedHerOwnAccount() {
        userRepository.findByUsername("alice").ifPresent(user -> {
            user.setStatus("DISABLED");
            userRepository.save(user);
        });
    }

    // ============================================================
    // Helpers
    // ============================================================

    private String resolveUsername() {
        String raw = stateStore.getCurrentUsername();
        return (raw == null) ? "alice" : raw;
    }

    private void performChangePassword(
            final String oldPassword, final String newPassword) {
        String username = resolveUsername();
        try {
            ChangePasswordRequest req = new ChangePasswordRequest();
            req.setOldPassword(oldPassword);
            req.setNewPassword(newPassword);
            ResponseEntity<Void> resp = userController.changePassword(
                    UnitAuthSteps.userDetails(username), req);
            stateStore.setStatusCode(resp.getStatusCode().value());
            stateStore.setResponseBody(java.util.Map.of("message", "Password changed"));
        } catch (InvalidCredentialsException e) {
            stateStore.setStatusCode(401);
            stateStore.setLastException(e);
        }
    }
}
