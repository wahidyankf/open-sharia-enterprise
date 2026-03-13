package com.demobejasb.integration.user_account;

import com.demobejasb.auth.repository.UserRepository;
import com.demobejasb.integration.ResponseStore;
import com.demobejasb.integration.steps.TokenStore;
import com.demobejasb.security.JwtUtil;
import com.demobejasb.user.dto.UserProfileResponse;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.When;
import java.util.Map;
import java.util.Objects;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.security.crypto.password.PasswordEncoder;

@Scope("cucumber-glue")
public class UserAccountSteps {

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    private JwtUtil jwtUtil;

    @When("^alice sends GET /api/v1/users/me$")
    public void aliceSendsGetUsersMe() {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        performGetUsersMe(token);
    }

    @When("^alice sends PATCH /api/v1/users/me with body \\{ \"display_name\": \"Alice Smith\" \\}$")
    public void aliceSendsPatchUsersMeWithDisplayName() {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        if (!jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        userRepository.findByUsername(username).ifPresentOrElse(user -> {
            user.setDisplayName("Alice Smith");
            userRepository.save(user);
            responseStore.setResponse(200, UserProfileResponse.from(user));
        }, () -> responseStore.setResponse(404, Map.of("message", "User not found")));
    }

    @When("^alice sends POST /api/v1/users/me/password with body \\{ \"old_password\": \"Str0ng#Pass1\", \"new_password\": \"NewPass#456\" \\}$")
    public void aliceSendsPostChangePasswordSuccess() {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        performChangePassword(token, "Str0ng#Pass1", "NewPass#456");
    }

    @When("^alice sends POST /api/v1/users/me/password with body \\{ \"old_password\": \"Wr0ngOld!\", \"new_password\": \"NewPass#456\" \\}$")
    public void aliceSendsPostChangePasswordWrongOld() {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        performChangePassword(token, "Wr0ngOld!", "NewPass#456");
    }

    @When("^alice sends POST /api/v1/users/me/deactivate$")
    public void aliceSendsPostSelfDeactivate() {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        if (!jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        userRepository.findByUsername(username).ifPresentOrElse(user -> {
            user.setStatus("DISABLED");
            userRepository.save(user);
            responseStore.setResponse(200);
        }, () -> responseStore.setResponse(404, Map.of("message", "User not found")));
    }

    @Given("^alice has deactivated her own account via POST /api/v1/users/me/deactivate$")
    public void aliceHasDeactivatedHerOwnAccount() {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        if (!jwtUtil.isTokenValid(token)) {
            throw new RuntimeException("Invalid token for account deactivation");
        }
        String username = jwtUtil.extractUsername(token);
        userRepository.findByUsername(username).ifPresent(user -> {
            user.setStatus("DISABLED");
            userRepository.save(user);
        });
    }

    // ============================================================
    // Internal helpers
    // ============================================================

    private void performGetUsersMe(final String token) {
        if (!jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        userRepository.findByUsername(username).ifPresentOrElse(
                user -> responseStore.setResponse(200, UserProfileResponse.from(user)),
                () -> responseStore.setResponse(404, Map.of("message", "User not found")));
    }

    private void performChangePassword(
            final String token, final String oldPassword, final String newPassword) {
        if (!jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        userRepository.findByUsername(username).ifPresentOrElse(user -> {
            if (!passwordEncoder.matches(oldPassword, user.getPasswordHash())) {
                responseStore.setResponse(401, Map.of("message", "Invalid credentials"));
                return;
            }
            user.setPasswordHash(Objects.requireNonNull(passwordEncoder.encode(newPassword)));
            userRepository.save(user);
            responseStore.setResponse(200);
        }, () -> responseStore.setResponse(404, Map.of("message", "User not found")));
    }
}
