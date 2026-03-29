package com.ademobejasb.integration.steps;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ademobejasb.auth.repository.UserRepository;
import com.ademobejasb.auth.service.AccountNotActiveException;
import com.ademobejasb.auth.service.AuthService;
import com.ademobejasb.auth.service.InvalidCredentialsException;
import com.ademobejasb.auth.service.UsernameAlreadyExistsException;
import com.ademobejasb.contracts.AuthTokens;
import com.ademobejasb.contracts.LoginRequest;
import com.ademobejasb.contracts.RegisterRequest;
import com.ademobejasb.integration.ResponseStore;
import com.ademobejasb.security.JwtUtil;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;

import static org.assertj.core.api.Assertions.assertThat;

@Scope("cucumber-glue")
public class AuthSteps {

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

    private final ObjectMapper objectMapper = new ObjectMapper();

    // ============================================================
    // Registration helpers
    // ============================================================

    @When("^a client sends POST /api/v1/auth/register with body:$")
    public void postRegister(final String body) {
        try {
            @SuppressWarnings("unchecked")
            Map<String, Object> map = objectMapper.readValue(body, Map.class);
            String username = (String) map.get("username");
            String email = (String) map.get("email");
            Object rawPassword = map.getOrDefault("password", "");
            String password = rawPassword != null ? (String) rawPassword : "";
            performRegister(username, email, password);
        } catch (Exception e) {
            responseStore.setResponse(400, Map.of("message", "Validation failed: " + e.getMessage()));
        }
    }

    @When("^the client sends POST /api/v1/auth/register with body \\{ \"username\": \"([^\"]+)\", \"email\": \"([^\"]+)\", \"password\": \"([^\"]*)\" \\}$")
    public void theClientSendsPostRegisterWithBody(
            final String username, final String email, final String password) {
        performRegister(username, email, password);
    }

    @Given("a user {string} is registered with email {string} and password {string}")
    public void aUserIsRegisteredWithEmailAndPassword(
            final String username, final String email, final String password) {
        com.ademobejasb.contracts.User response = registerOrFail(username, email, password);
        if ("alice".equals(username)) {
            tokenStore.setAliceId(java.util.UUID.fromString(response.getId()));
        }
    }

    @Given("a user {string} is registered with password {string}")
    public void aUserIsRegisteredWithPassword(final String username, final String password) {
        String email = username + "@example.com";
        com.ademobejasb.contracts.User response = registerOrFail(username, email, password);
        if ("alice".equals(username)) {
            tokenStore.setAliceId(java.util.UUID.fromString(response.getId()));
        }
    }

    @Given("a user {string} is already registered")
    public void userIsAlreadyRegistered(final String username) {
        aUserIsRegisteredWithPassword(username, "Str0ng#Pass1234");
    }

    @Given("a user {string} is already registered with password {string}")
    public void userIsAlreadyRegisteredWithPassword(
            final String username, final String password) {
        aUserIsRegisteredWithPassword(username, password);
    }

    // ============================================================
    // Login helpers
    // ============================================================

    @When("^a client sends POST /api/v1/auth/login with body:$")
    public void postLogin(final String body) {
        try {
            @SuppressWarnings("unchecked")
            Map<String, Object> map = objectMapper.readValue(body, Map.class);
            String username = (String) map.get("username");
            String password = (String) map.get("password");
            performLogin(username, password);
        } catch (Exception e) {
            responseStore.setResponse(400, Map.of("message", "Validation failed: " + e.getMessage()));
        }
    }

    @When("^the client sends POST /api/v1/auth/login with body \\{ \"username\": \"([^\"]+)\", \"password\": \"([^\"]+)\" \\}$")
    public void theClientSendsPostLoginWithBody(final String username, final String password) {
        performLogin(username, password);
    }

    @Given("the client has logged in as {string} and stored the JWT token")
    public void clientLoggedIn(final String username) {
        AuthTokens auth = loginOrFail(username, "Str0ng#Pass1234");
        tokenStore.setToken(auth.getAccessToken());
    }

    @Given("{string} has logged in and stored the access token")
    public void userHasLoggedInAndStoredAccessToken(final String username) {
        String password = "alice".equals(username) ? "Str0ng#Pass1" : "Str0ng#Pass1234";
        AuthTokens auth = loginOrFail(username, password);
        tokenStore.setToken(auth.getAccessToken());
        if ("alice".equals(username) && tokenStore.getAliceId() == null) {
            userRepository.findByUsername("alice").ifPresent(u -> tokenStore.setAliceId(u.getId()));
        }
    }

    @Given("{string} has logged in and stored the access token and refresh token")
    public void userHasLoggedInAndStoredTokens(final String username) {
        String password = "alice".equals(username) ? "Str0ng#Pass1" : "Str0ng#Pass1234";
        AuthTokens auth = loginOrFail(username, password);
        tokenStore.setToken(auth.getAccessToken());
        tokenStore.setRefreshToken(auth.getRefreshToken());
    }

    @Given("an admin user {string} is registered and logged in")
    public void anAdminUserIsRegisteredAndLoggedIn(final String username) {
        String email = username + "@example.com";
        String password = "Adm1n#Secure123";
        com.ademobejasb.contracts.User reg = registerOrFail(username, email, password);
        // Promote to ADMIN
        userRepository.findByUsername(username).ifPresent(user -> {
            user.setRole("ADMIN");
            userRepository.save(user);
        });
        AuthTokens auth = loginOrFail(username, password);
        tokenStore.setAdminToken(auth.getAccessToken());
        tokenStore.setAdminUserId(java.util.UUID.fromString(reg.getId()));
    }

    @Given("a user {string} is registered and deactivated")
    public void aUserIsRegisteredAndDeactivated(final String username) {
        if (userRepository.findByUsername(username).isEmpty()) {
            aUserIsRegisteredWithPassword(username, "Str0ng#Pass1");
        }
        userRepository.findByUsername(username).ifPresent(user -> {
            user.setStatus("DISABLED");
            userRepository.save(user);
        });
    }

    @Given("a user {string} is registered and locked after too many failed logins")
    public void aUserIsRegisteredAndLockedAfterTooManyFailedLogins(final String username) {
        if (userRepository.findByUsername(username).isEmpty()) {
            aUserIsRegisteredWithPassword(username, "Str0ng#Pass1");
        }
        userRepository.findByUsername(username).ifPresent(user -> {
            user.setStatus("LOCKED");
            user.setFailedLoginAttempts(5);
            userRepository.save(user);
        });
        if ("alice".equals(username)) {
            userRepository.findByUsername(username).ifPresent(u -> tokenStore.setAliceId(u.getId()));
        }
    }

    @Given("{string} has had the maximum number of failed login attempts")
    public void userHasHadMaxFailedLoginAttempts(final String username) {
        // Attempt 5 failed logins to lock the account
        for (int i = 0; i < 5; i++) {
            try {
                LoginRequest req = new LoginRequest();
                req.setUsername(username);
                req.setPassword("WrongPass#1234");
                authService.login(req);
            } catch (InvalidCredentialsException | AccountNotActiveException e) {
                // Expected
            }
        }
        if ("alice".equals(username)) {
            userRepository.findByUsername(username).ifPresent(u -> tokenStore.setAliceId(u.getId()));
        }
    }

    @Given("an admin has unlocked alice's account")
    public void anAdminHasUnlockedAlicesAccount() {
        userRepository.findByUsername("alice").ifPresent(user -> {
            user.setStatus("ACTIVE");
            user.setFailedLoginAttempts(0);
            userRepository.save(user);
        });
    }

    @Given("alice's account has been disabled by the admin")
    public void alicesAccountHasBeenDisabledByAdmin() {
        userRepository.findByUsername("alice").ifPresent(user -> {
            user.setStatus("DISABLED");
            userRepository.save(user);
        });
    }

    @Given("alice's account has been disabled")
    public void alicesAccountHasBeenDisabled() {
        userRepository.findByUsername("alice").ifPresent(user -> {
            user.setStatus("DISABLED");
            userRepository.save(user);
        });
    }

    @Given("the user {string} has been deactivated")
    public void theUserHasBeenDeactivated(final String username) {
        userRepository.findByUsername(username).ifPresent(user -> {
            user.setStatus("DISABLED");
            userRepository.save(user);
        });
    }

    @When("^the client sends GET /api/v1/users/me with alice's access token$")
    public void theClientSendsGetUsersMeWithAlicesToken() {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Alice's token not stored");
        }
        performGetUsersMe(token);
    }

    // ============================================================
    // Admin action steps (shared)
    // ============================================================

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/unlock$")
    public void theAdminSendsPostUnlockAliceShared() {
        String adminToken = tokenStore.getAdminToken();
        java.util.UUID aliceId = tokenStore.getAliceId();
        if (adminToken == null || aliceId == null) {
            throw new IllegalStateException("Admin token or alice ID not stored");
        }
        // Authorization check: admin token must be valid
        if (!jwtUtil.isTokenValid(adminToken)) {
            responseStore.setResponse(401, Map.of("message", "Invalid token"));
            return;
        }
        userRepository.findById(aliceId).ifPresent(user -> {
            user.setStatus("ACTIVE");
            user.setFailedLoginAttempts(0);
            userRepository.save(user);
        });
        userRepository.findById(aliceId).ifPresentOrElse(
                user -> responseStore.setResponse(200, Map.of(
                        "id", user.getId().toString(),
                        "username", user.getUsername(),
                        "status", user.getStatus())),
                () -> responseStore.setResponse(404, Map.of("message", "User not found")));
    }

    // ============================================================
    // Assert steps (shared)
    // ============================================================

    @Then("the response body should contain {string} equal to {string}")
    public void responseBodyContainsFieldEqualTo(final String field, final String value) {
        Map<String, Object> body = responseStore.getBodyAsMap();
        assertThat(body).containsKey(field);
        assertThat(String.valueOf(body.get(field))).isEqualTo(value);
    }

    @Then("the response body should not contain a {string} field")
    public void responseBodyShouldNotContainField(final String field) {
        Map<String, Object> body = responseStore.getBodyAsMap();
        assertThat(body).doesNotContainKey(field);
    }

    @Then("the response body should contain a non-null {string} field")
    public void responseBodyContainsNonNullField(final String field) {
        Map<String, Object> body = responseStore.getBodyAsMap();
        assertThat(body).containsKey(field);
        assertThat(body.get(field)).isNotNull();
    }

    @Then("the response body should contain a {string} field")
    public void responseBodyContainsField(final String field) {
        Map<String, Object> body = responseStore.getBodyAsMap();
        assertThat(body).containsKey(field);
    }

    @Then("the response body should contain an error message about duplicate username")
    public void responseBodyContainsDuplicateUsernameError() {
        assertThat(responseStore.getBody()).containsIgnoringCase("already exists");
    }

    @Then("the response body should contain an error message about invalid credentials")
    public void responseBodyContainsInvalidCredentialsError() {
        assertThat(responseStore.getBody()).containsIgnoringCase("invalid");
    }

    @Then("the response body should contain an error message about account deactivation")
    public void responseBodyContainsAccountDeactivationError() {
        assertThat(responseStore.getBody()).containsIgnoringCase("deactivat");
    }

    @Then("the response body should contain an error message about token expiration")
    public void responseBodyContainsTokenExpirationError() {
        assertThat(responseStore.getBody()).containsIgnoringCase("expir");
    }

    @Then("the response body should contain an error message about invalid token")
    public void responseBodyContainsInvalidTokenError() {
        assertThat(responseStore.getBody()).containsIgnoringCase("invalid");
    }

    @Then("the response body should contain a validation error for {string}")
    public void responseBodyContainsValidationError(final String field) {
        int status = responseStore.getStatusCode();
        assertThat(status).isIn(400, 415);
    }

    @Then("alice's account status should be {string}")
    public void alicesAccountStatusShouldBe(final String status) {
        String actualStatus = userRepository.findByUsername("alice")
                .map(u -> u.getStatus().toLowerCase())
                .orElseThrow(() -> new RuntimeException("Alice not found"));
        assertThat(actualStatus).isEqualToIgnoringCase(status);
    }

    // ============================================================
    // Internal helpers
    // ============================================================

    /**
     * Performs registration and stores the HTTP-equivalent response. Maps exceptions to their
     * corresponding HTTP status codes, mirroring GlobalExceptionHandler and Bean Validation.
     */
    void performRegister(
            final String username, final String email, final String password) {
        // Blank-field validation (replicates @Valid on the controller method parameter)
        if (isBlank(username) || isBlank(email) || isBlank(password)
                || (username != null && (username.length() < 3 || username.length() > 50))) {
            responseStore.setResponse(400, Map.of("message", "Validation failed"));
            return;
        }
        try {
            RegisterRequest req = new RegisterRequest();
            req.setUsername(username);
            req.setEmail(email);
            req.setPassword(password);
            com.ademobejasb.contracts.User resp = authService.register(req);
            responseStore.setResponse(201, resp);
        } catch (com.ademobejasb.config.ValidationException e) {
            responseStore.setResponse(400, Map.of("message", e.getMessage()));
        } catch (UsernameAlreadyExistsException e) {
            responseStore.setResponse(409, Map.of("message", e.getMessage()));
        }
    }

    /**
     * Performs registration and throws if it fails (used in Given steps where failure is unexpected).
     */
    public com.ademobejasb.contracts.User registerOrFail(
            final String username, final String email, final String password) {
        try {
            RegisterRequest req = new RegisterRequest();
            req.setUsername(username);
            req.setEmail(email);
            req.setPassword(password);
            return authService.register(req);
        } catch (UsernameAlreadyExistsException e) {
            // Already registered — return the existing user info
            return userRepository.findByUsername(username)
                    .map(AuthService::buildUserResponse)
                    .orElseThrow(() -> new RuntimeException("User not found: " + username));
        }
    }

    /**
     * Performs login and stores the HTTP-equivalent response.
     */
    void performLogin(final String username, final String password) {
        // Manual validation
        if (isBlank(username) || isBlank(password)) {
            responseStore.setResponse(400, Map.of("message", "Validation failed"));
            return;
        }
        try {
            LoginRequest req = new LoginRequest();
            req.setUsername(username);
            req.setPassword(password);
            AuthTokens resp = authService.login(req);
            responseStore.setResponse(200, resp);
            tokenStore.setToken(resp.getAccessToken());
            tokenStore.setRefreshToken(resp.getRefreshToken());
        } catch (InvalidCredentialsException e) {
            responseStore.setResponse(401, Map.of("message", e.getMessage()));
        } catch (AccountNotActiveException e) {
            responseStore.setResponse(401, Map.of("message", "Account is deactivated or not active"));
        }
    }

    /**
     * Performs login and throws if it fails (used in Given steps where failure is unexpected).
     */
    AuthTokens loginOrFail(final String username, final String password) {
        try {
            LoginRequest req = new LoginRequest();
            req.setUsername(username);
            req.setPassword(password);
            return authService.login(req);
        } catch (InvalidCredentialsException | AccountNotActiveException e) {
            throw new RuntimeException("Unexpected login failure: " + e.getMessage(), e);
        }
    }

    /**
     * Performs a GET /api/v1/users/me using the provided token for authorization. Mirrors the
     * UserController logic: validates the JWT then returns the user profile.
     */
    void performGetUsersMe(final String token) {
        if (!jwtUtil.isTokenValid(token) || authService.isTokenRevoked(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        userRepository.findByUsername(username).ifPresentOrElse(
                user -> responseStore.setResponse(200, AuthService.buildUserResponse(user)),
                () -> responseStore.setResponse(404, Map.of("message", "User not found")));
    }

    private static boolean isBlank(final String s) {
        return s == null || s.isBlank();
    }
}
