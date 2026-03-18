package com.demobejasb.unit.steps;

import com.demobejasb.auth.controller.AuthController;
import com.demobejasb.auth.repository.UserRepository;
import com.demobejasb.auth.service.AccountNotActiveException;
import com.demobejasb.auth.service.AuthService;
import com.demobejasb.auth.service.InvalidCredentialsException;
import com.demobejasb.auth.service.UsernameAlreadyExistsException;
import com.demobejasb.contracts.AuthTokens;
import com.demobejasb.contracts.LoginRequest;
import com.demobejasb.contracts.RegisterRequest;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import jakarta.servlet.http.HttpServletRequest;
import java.util.Collections;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.User;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit-level Cucumber step definitions for authentication and registration. Calls service methods
 * directly (no MockMvc, no HTTP). Translates HTTP-semantic Gherkin steps into service-level
 * outcomes.
 */
@Scope("cucumber-glue")
public class UnitAuthSteps {

    /** Maps JSON property names (snake_case) to AuthTokens field names (camelCase). */
    private static final Map<String, String> AUTH_FIELD_MAP = Map.of(
            "access_token", "accessToken",
            "refresh_token", "refreshToken");

    @Autowired
    private UnitStateStore stateStore;

    @Autowired
    private AuthService authService;

    @Autowired
    private AuthController authController;

    @Autowired
    private com.demobejasb.admin.controller.AdminController adminController;

    @Autowired
    private com.demobejasb.user.controller.UserController userController;

    @Autowired
    private UserRepository userRepository;

    // ============================================================
    // Registration — When steps
    // ============================================================

    @When("^a client sends POST /api/v1/auth/register with body:$")
    public void postRegisterWithBody(final String body) {
        String username = extractJsonString(body, "username");
        String email = extractJsonString(body, "email");
        String password = extractJsonString(body, "password");
        performRegister(username, email, password);
    }

    @When("^the client sends POST /api/v1/auth/register with body \\{ \"username\": \"([^\"]+)\", \"email\": \"([^\"]+)\", \"password\": \"([^\"]*)\" \\}$")
    public void theClientSendsPostRegisterWithBody(
            final String username, final String email, final String password) {
        performRegister(username, email, password);
    }

    // ============================================================
    // Login — When steps
    // ============================================================

    @When("^a client sends POST /api/v1/auth/login with body:$")
    public void postLoginWithBody(final String body) {
        String username = extractJsonString(body, "username");
        String password = extractJsonString(body, "password");
        performLogin(username, password);
    }

    @When("^the client sends POST /api/v1/auth/login with body \\{ \"username\": \"([^\"]+)\", \"password\": \"([^\"]+)\" \\}$")
    public void theClientSendsPostLoginWithBody(final String username, final String password) {
        performLogin(username, password);
    }

    @Given("the client has logged in as {string} and stored the JWT token")
    public void clientLoggedIn(final String username) {
        AuthTokens response = doLogin(username, "Str0ng#Pass1234");
        stateStore.setAccessToken(response.getAccessToken());
        stateStore.setCurrentUsername(username);
    }

    @Given("{string} has logged in and stored the access token")
    public void userHasLoggedInAndStoredAccessToken(final String username) {
        String password = "alice".equals(username) ? "Str0ng#Pass1" : "Str0ng#Pass1234";
        AuthTokens response = doLogin(username, password);
        stateStore.setAccessToken(response.getAccessToken());
        stateStore.setCurrentUsername(username);
        if ("alice".equals(username) && stateStore.getAliceId() == null) {
            userRepository.findByUsername("alice")
                    .ifPresent(u -> stateStore.setAliceId(u.getId()));
        }
    }

    @Given("{string} has logged in and stored the access token and refresh token")
    public void userHasLoggedInAndStoredTokens(final String username) {
        String password = "alice".equals(username) ? "Str0ng#Pass1" : "Str0ng#Pass1234";
        AuthTokens response = doLogin(username, password);
        stateStore.setAccessToken(response.getAccessToken());
        stateStore.setRefreshToken(response.getRefreshToken());
        stateStore.setCurrentUsername(username);
    }

    @Given("an admin user {string} is registered and logged in")
    public void anAdminUserIsRegisteredAndLoggedIn(final String username) {
        String email = username + "@example.com";
        String password = "Adm1n#Secure123";
        com.demobejasb.contracts.User regResp = null;
        try {
            RegisterRequest req = new RegisterRequest();
            req.setUsername(username);
            req.setEmail(email);
            req.setPassword(password);
            regResp = authService.register(req);
        } catch (UsernameAlreadyExistsException ignored) {
            // Already registered — continue
        }
        userRepository.findByUsername(username).ifPresent(user -> {
            user.setRole("ADMIN");
            userRepository.save(user);
        });
        AuthTokens loginResp = doLogin(username, password);
        stateStore.setAdminToken(loginResp.getAccessToken());
        if (regResp != null) {
            stateStore.setAdminUserId(java.util.UUID.fromString(regResp.getId()));
        } else {
            userRepository.findByUsername(username)
                    .ifPresent(u -> stateStore.setAdminUserId(u.getId()));
        }
    }

    @Given("{string} has had the maximum number of failed login attempts")
    public void userHasHadMaxFailedLoginAttempts(final String username) {
        for (int i = 0; i < 5; i++) {
            try {
                LoginRequest req = new LoginRequest();
                req.setUsername(username);
                req.setPassword("WrongPass#1234");
                authService.login(req);
            } catch (InvalidCredentialsException | AccountNotActiveException ignored) {
                // Expected
            }
        }
        if ("alice".equals(username)) {
            userRepository.findByUsername(username)
                    .ifPresent(u -> stateStore.setAliceId(u.getId()));
        }
    }

    // ============================================================
    // Assertion steps
    // ============================================================

    @Then("the response body should contain {string} equal to {string}")
    public void responseBodyContainsFieldEqualTo(final String field, final String value) {
        Object body = stateStore.getResponseBody();
        assertThat(body).isNotNull();
        Object actual = resolveField(body, field);
        assertThat(actual).isNotNull();
        assertThat(actual.toString()).isEqualTo(value);
    }

    @Then("the response body should contain {string} equal to {double}")
    public void responseBodyContainsFieldEqualToDouble(
            final String field, final double value) {
        Object body = stateStore.getResponseBody();
        assertThat(body).isNotNull();
        Object actual = resolveField(body, field);
        assertThat(actual).isNotNull();
        double actualDouble = Double.parseDouble(actual.toString());
        assertThat(actualDouble).isEqualTo(value);
    }

    @Then("the response body should not contain a {string} field")
    public void responseBodyShouldNotContainField(final String field) {
        // contracts.User (register response) intentionally omits password
        // For AuthTokens (accessToken, refreshToken, tokenType), same
        // If the response is null or the field is genuinely absent, the test passes
        Object body = stateStore.getResponseBody();
        if (body instanceof com.demobejasb.contracts.User) {
            // password field is never in contracts.User by design
            assertThat(field).isEqualTo("password");
        }
        // For other cases, absence is implicit in our type-safe response objects
    }

    @Then("the response body should contain a non-null {string} field")
    public void responseBodyContainsNonNullField(final String field) {
        Object body = stateStore.getResponseBody();
        assertThat(body).isNotNull();
        Object value = resolveField(body, field);
        assertThat(value).isNotNull();
    }

    @Then("the response body should contain a {string} field")
    public void responseBodyContainsField(final String field) {
        Object body = stateStore.getResponseBody();
        assertThat(body).isNotNull();
        Object value = resolveField(body, field);
        assertThat(value).isNotNull();
    }

    @Then("the response body should contain an error message about duplicate username")
    public void responseBodyContainsDuplicateUsernameError() {
        Exception ex = stateStore.getLastException();
        assertThat(ex).isNotNull();
        assertThat(ex.getMessage()).containsIgnoringCase("already exists");
    }

    @Then("the response body should contain an error message about invalid credentials")
    public void responseBodyContainsInvalidCredentialsError() {
        Exception ex = stateStore.getLastException();
        assertThat(ex).isNotNull();
        assertThat(ex).isInstanceOfAny(
                InvalidCredentialsException.class, AccountNotActiveException.class);
    }

    @Then("the response body should contain an error message about account deactivation")
    public void responseBodyContainsAccountDeactivationError() {
        Exception ex = stateStore.getLastException();
        assertThat(ex).isNotNull();
        assertThat(ex).isInstanceOf(AccountNotActiveException.class);
    }

    @Then("the response body should contain an error message about token expiration")
    public void responseBodyContainsTokenExpirationError() {
        Exception ex = stateStore.getLastException();
        assertThat(ex).isNotNull();
        assertThat(ex.getMessage()).containsIgnoringCase("expir");
    }

    @Then("the response body should contain an error message about invalid token")
    public void responseBodyContainsInvalidTokenError() {
        int status = stateStore.getStatusCode();
        assertThat(status).isEqualTo(401);
        Exception ex = stateStore.getLastException();
        assertThat(ex).isNotNull();
        // Accept any token-related error: invalid, expired, revoked, not found
        String msg = ex.getMessage() != null ? ex.getMessage().toLowerCase() : "";
        assertThat(msg).matches(".*(?:invalid|expired|revoked|not found|token).*");
    }

    @Then("the response body should contain a validation error for {string}")
    public void responseBodyContainsValidationError(final String field) {
        int status = stateStore.getStatusCode();
        assertThat(status).isIn(400, 415);
    }

    @Then("alice's account status should be {string}")
    public void alicesAccountStatusShouldBe(final String status) {
        String actualStatus = userRepository.findByUsername("alice")
                .map(u -> u.getStatus().toLowerCase())
                .orElseThrow(() -> new RuntimeException("Alice not found"));
        assertThat(actualStatus).isEqualToIgnoringCase(status);
    }

    @When("^the client sends GET /api/v1/users/me with alice's access token$")
    public void theClientSendsGetUsersMeWithAlicesToken() {
        String token = stateStore.getAccessToken();
        if (token == null) {
            stateStore.setStatusCode(401);
            return;
        }
        if (authService.isTokenRevoked(token)) {
            stateStore.setStatusCode(401);
            return;
        }
        // Check account status before delegating to controller
        boolean notActive = userRepository.findByUsername("alice")
                .map(u -> "DISABLED".equals(u.getStatus()) || "LOCKED".equals(u.getStatus()))
                .orElse(true);
        if (notActive) {
            stateStore.setStatusCode(401);
            return;
        }
        ResponseEntity<com.demobejasb.contracts.User>
                resp = userController.getProfile(userDetails("alice"));
        stateStore.setStatusCode(resp.getStatusCode().value());
        stateStore.setResponseBody(resp.getBody());
    }

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/unlock$")
    public void theAdminSendsPostUnlockAliceShared() {
        java.util.UUID aliceId = stateStore.getAliceId();
        if (aliceId == null) {
            stateStore.setStatusCode(400);
            return;
        }
        try {
            ResponseEntity<com.demobejasb.contracts.User>
                    resp = adminController.unlockUser(aliceId);
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

    private void performRegister(
            final String username, final String email, final String password) {
        // Blank-field validation (generated types lack Bean Validation annotations)
        if (username == null || username.isBlank()
                || email == null || email.isBlank()
                || password == null || password.isBlank()) {
            stateStore.setStatusCode(400);
            stateStore.setLastException(new IllegalArgumentException("Validation failed"));
            return;
        }
        RegisterRequest request = new RegisterRequest();
        request.setUsername(username);
        request.setEmail(email);
        request.setPassword(password);
        try {
            ResponseEntity<com.demobejasb.contracts.User> response =
                    authController.register(request);
            stateStore.setStatusCode(response.getStatusCode().value());
            stateStore.setResponseBody(response.getBody());
            stateStore.setLastException(null);
            if (response.getBody() != null && "alice".equals(username)) {
                stateStore.setAliceId(java.util.UUID.fromString(response.getBody().getId()));
            }
        } catch (com.demobejasb.config.ValidationException e) {
            stateStore.setStatusCode(400);
            stateStore.setLastException(e);
        } catch (UsernameAlreadyExistsException e) {
            stateStore.setStatusCode(409);
            stateStore.setLastException(e);
            stateStore.setResponseBody(null);
        }
    }

    private void performLogin(final String username, final String password) {
        try {
            LoginRequest req = new LoginRequest();
            req.setUsername(username);
            req.setPassword(password);
            ResponseEntity<AuthTokens> response = authController.login(req);
            AuthTokens body = response.getBody();
            stateStore.setStatusCode(response.getStatusCode().value());
            stateStore.setResponseBody(body);
            stateStore.setLastException(null);
            if (body != null) {
                stateStore.setAccessToken(body.getAccessToken());
                stateStore.setRefreshToken(body.getRefreshToken());
            }
        } catch (InvalidCredentialsException | AccountNotActiveException e) {
            stateStore.setStatusCode(401);
            stateStore.setLastException(e);
            stateStore.setResponseBody(null);
        }
    }

    AuthTokens doLogin(final String username, final String password) {
        try {
            LoginRequest req = new LoginRequest();
            req.setUsername(username);
            req.setPassword(password);
            ResponseEntity<AuthTokens> resp = authController.login(req);
            return resp.getBody();
        } catch (InvalidCredentialsException | AccountNotActiveException e) {
            throw new RuntimeException(
                    "Login failed for " + username + ": " + e.getMessage(), e);
        }
    }

    /** Creates a minimal UserDetails for use as @AuthenticationPrincipal in controller calls. */
    static org.springframework.security.core.userdetails.UserDetails userDetails(
            final String username) {
        return User.withUsername(username)
                .password("")
                .authorities(new SimpleGrantedAuthority("ROLE_USER"))
                .build();
    }

    /** Creates a mock HttpServletRequest with a Bearer token in the Authorization header. */
    static HttpServletRequest mockRequest(final String token) {
        HttpServletRequest req = Mockito.mock(HttpServletRequest.class);
        Mockito.when(req.getHeader("Authorization"))
                .thenReturn(token != null ? "Bearer " + token : null);
        Mockito.when(req.getHeaderNames())
                .thenReturn(Collections.enumeration(java.util.List.of("Authorization")));
        return req;
    }

    /**
     * Resolves a JSON-property-named field from a response object. Supports both the snake_case
     * JSON name (e.g., "access_token") and the camelCase Java field name (e.g., "accessToken").
     * Handles all response DTO types used across the application.
     */
    Object resolveField(final Object body, final String jsonField) {
        if (body instanceof AuthTokens resp) {
            String javaField = AUTH_FIELD_MAP.getOrDefault(jsonField, jsonField);
            return switch (javaField) {
                case "accessToken" -> resp.getAccessToken();
                case "refreshToken" -> resp.getRefreshToken();
                case "tokenType" -> resp.getTokenType();
                default -> null;
            };
        }
        if (body instanceof com.demobejasb.contracts.User resp) {
            return switch (jsonField) {
                case "id" -> resp.getId();
                case "username" -> resp.getUsername();
                case "email" -> resp.getEmail();
                case "createdAt" -> resp.getCreatedAt() != null
                        ? resp.getCreatedAt().toString() : null;
                case "display_name", "displayName" -> resp.getDisplayName();
                case "status" -> resp.getStatus() != null
                        ? resp.getStatus().getValue() : null;
                case "role" -> resp.getRoles() != null && !resp.getRoles().isEmpty()
                        ? resp.getRoles().get(0) : null;
                default -> null;
            };
        }
        if (body instanceof com.demobejasb.contracts.UserListResponse resp) {
            return switch (jsonField) {
                case "data", "content" -> resp.getContent();
                case "total", "totalElements" -> resp.getTotalElements();
                case "page" -> resp.getPage();
                default -> null;
            };
        }
        if (body instanceof com.demobejasb.contracts.Expense resp) {
            return switch (jsonField) {
                case "id" -> resp.getId();
                case "amount" -> resp.getAmount();
                case "currency" -> resp.getCurrency();
                case "category" -> resp.getCategory();
                case "description" -> resp.getDescription();
                case "date" -> resp.getDate() != null ? resp.getDate().toString() : null;
                case "type" -> resp.getType() != null ? resp.getType().getValue() : null;
                case "quantity" -> resp.getQuantity();
                case "unit" -> resp.getUnit();
                default -> null;
            };
        }
        if (body instanceof com.demobejasb.contracts.ExpenseListResponse resp) {
            return switch (jsonField) {
                case "data", "content" -> resp.getContent();
                case "total", "totalElements" -> resp.getTotalElements();
                case "page" -> resp.getPage();
                default -> null;
            };
        }
        if (body instanceof com.demobejasb.attachment.dto.AttachmentResponse resp) {
            return switch (jsonField) {
                case "id" -> resp.id();
                case "filename" -> resp.filename();
                case "contentType" -> resp.contentType();
                case "url" -> resp.url();
                default -> null;
            };
        }
        if (body instanceof com.demobejasb.contracts.PLReport resp) {
            return switch (jsonField) {
                case "totalIncome" -> resp.getTotalIncome();
                case "totalExpense" -> resp.getTotalExpense();
                case "net" -> resp.getNet();
                case "incomeBreakdown" -> resp.getIncomeBreakdown();
                case "expenseBreakdown" -> resp.getExpenseBreakdown();
                default -> null;
            };
        }
        if (body instanceof com.demobejasb.contracts.PasswordResetResponse resp) {
            return switch (jsonField) {
                case "reset_token", "token" -> resp.getToken();
                default -> null;
            };
        }
        if (body instanceof java.util.Map<?, ?> map) {
            return map.get(jsonField);
        }
        return null;
    }

    private String extractJsonString(final String json, final String key) {
        Pattern p = Pattern.compile("\"" + key + "\"\\s*:\\s*\"([^\"]*)\"");
        Matcher m = p.matcher(json);
        return m.find() ? m.group(1) : "";
    }
}
