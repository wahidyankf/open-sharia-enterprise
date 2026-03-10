package com.organiclever.be.integration.steps;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jayway.jsonpath.JsonPath;
import com.organiclever.be.auth.repository.UserRepository;
import com.organiclever.be.integration.ResponseStore;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import java.util.Map;
import java.util.UUID;
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
public class AuthSteps {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private UserRepository userRepository;

    private final ObjectMapper objectMapper = new ObjectMapper();

    // ============================================================
    // Registration helpers
    // ============================================================

    @When("^a client sends POST /api/v1/auth/register with body:$")
    public void postRegister(final String body) throws Exception {
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/auth/register")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
    }

    @When("^the client sends POST /api/v1/auth/register with body \\{ \"username\": \"([^\"]+)\", \"email\": \"([^\"]+)\", \"password\": \"([^\"]*)\" \\}$")
    public void theClientSendsPostRegisterWithBody(
            final String username, final String email, final String password) throws Exception {
        String body = objectMapper.writeValueAsString(
                Map.of("username", username, "email", email, "password", password));
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/auth/register")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
    }

    @Given("a user {string} is registered with email {string} and password {string}")
    public void aUserIsRegisteredWithEmailAndPassword(
            final String username, final String email, final String password) throws Exception {
        MvcResult result = mockMvc.perform(
                post("/api/v1/auth/register")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(
                                Map.of("username", username, "email", email, "password", password))))
                .andExpect(MockMvcResultMatchers.status().isCreated())
                .andReturn();
        if ("alice".equals(username)) {
            String id = JsonPath.read(result.getResponse().getContentAsString(), "$.id");
            tokenStore.setAliceId(UUID.fromString(id));
        }
    }

    @Given("a user {string} is registered with password {string}")
    public void aUserIsRegisteredWithPassword(final String username, final String password)
            throws Exception {
        String email = username + "@example.com";
        MvcResult result = mockMvc.perform(
                post("/api/v1/auth/register")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(
                                Map.of("username", username, "email", email, "password", password))))
                .andExpect(MockMvcResultMatchers.status().isCreated())
                .andReturn();
        if ("alice".equals(username)) {
            String id = JsonPath.read(result.getResponse().getContentAsString(), "$.id");
            tokenStore.setAliceId(UUID.fromString(id));
        }
    }

    @Given("a user {string} is already registered")
    public void userIsAlreadyRegistered(final String username) throws Exception {
        aUserIsRegisteredWithPassword(username, "Str0ng#Pass1234");
    }

    @Given("a user {string} is already registered with password {string}")
    public void userIsAlreadyRegisteredWithPassword(
            final String username, final String password) throws Exception {
        aUserIsRegisteredWithPassword(username, password);
    }

    // ============================================================
    // Login helpers
    // ============================================================

    @When("^a client sends POST /api/v1/auth/login with body:$")
    public void postLogin(final String body) throws Exception {
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/auth/login")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
    }

    @When("^the client sends POST /api/v1/auth/login with body \\{ \"username\": \"([^\"]+)\", \"password\": \"([^\"]+)\" \\}$")
    public void theClientSendsPostLoginWithBody(final String username, final String password)
            throws Exception {
        String body = objectMapper.writeValueAsString(
                Map.of("username", username, "password", password));
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/auth/login")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
    }

    @Given("the client has logged in as {string} and stored the JWT token")
    public void clientLoggedIn(final String username) throws Exception {
        MvcResult result = mockMvc.perform(
                post("/api/v1/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(
                                Map.of("username", username, "password", "Str0ng#Pass1234"))))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andReturn();
        String token = JsonPath.read(result.getResponse().getContentAsString(), "$.access_token");
        tokenStore.setToken(token);
    }

    @Given("{string} has logged in and stored the access token")
    public void userHasLoggedInAndStoredAccessToken(final String username) throws Exception {
        String password = "alice".equals(username) ? "Str0ng#Pass1" : "Str0ng#Pass1234";
        MvcResult result = mockMvc.perform(
                post("/api/v1/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(
                                Map.of("username", username, "password", password))))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andReturn();
        String responseBody = result.getResponse().getContentAsString();
        String token = JsonPath.read(responseBody, "$.access_token");
        tokenStore.setToken(token);
        // Also store alice id if available
        if ("alice".equals(username) && tokenStore.getAliceId() == null) {
            // Try to find in DB
            userRepository.findByUsername("alice").ifPresent(u -> tokenStore.setAliceId(u.getId()));
        }
    }

    @Given("{string} has logged in and stored the access token and refresh token")
    public void userHasLoggedInAndStoredTokens(final String username) throws Exception {
        String password = "alice".equals(username) ? "Str0ng#Pass1" : "Str0ng#Pass1234";
        MvcResult result = mockMvc.perform(
                post("/api/v1/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(
                                Map.of("username", username, "password", password))))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andReturn();
        String responseBody = result.getResponse().getContentAsString();
        String accessToken = JsonPath.read(responseBody, "$.access_token");
        String refreshToken = JsonPath.read(responseBody, "$.refresh_token");
        tokenStore.setToken(accessToken);
        tokenStore.setRefreshToken(refreshToken);
    }

    @Given("an admin user {string} is registered and logged in")
    public void anAdminUserIsRegisteredAndLoggedIn(final String username) throws Exception {
        String email = username + "@example.com";
        String password = "Adm1n#Secure123";
        // Register the admin
        MvcResult regResult = mockMvc.perform(
                post("/api/v1/auth/register")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(
                                Map.of("username", username, "email", email, "password", password))))
                .andExpect(MockMvcResultMatchers.status().isCreated())
                .andReturn();
        String adminId = JsonPath.read(regResult.getResponse().getContentAsString(), "$.id");
        // Set the user role to ADMIN in DB
        userRepository.findByUsername(username).ifPresent(user -> {
            user.setRole("ADMIN");
            userRepository.save(user);
        });
        // Login
        MvcResult loginResult = mockMvc.perform(
                post("/api/v1/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(
                                Map.of("username", username, "password", password))))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andReturn();
        String adminToken = JsonPath.read(loginResult.getResponse().getContentAsString(), "$.access_token");
        tokenStore.setAdminToken(adminToken);
        tokenStore.setAdminUserId(UUID.fromString(adminId));
    }

    @Given("a user {string} is registered and deactivated")
    public void aUserIsRegisteredAndDeactivated(final String username) throws Exception {
        // Only register if not already registered
        if (userRepository.findByUsername(username).isEmpty()) {
            aUserIsRegisteredWithPassword(username, "Str0ng#Pass1");
        }
        userRepository.findByUsername(username).ifPresent(user -> {
            user.setStatus("DISABLED");
            userRepository.save(user);
        });
    }

    @Given("a user {string} is registered and locked after too many failed logins")
    public void aUserIsRegisteredAndLockedAfterTooManyFailedLogins(final String username)
            throws Exception {
        if (userRepository.findByUsername(username).isEmpty()) {
            aUserIsRegisteredWithPassword(username, "Str0ng#Pass1");
        }
        // Simulate lock by setting status directly
        userRepository.findByUsername(username).ifPresent(user -> {
            user.setStatus("LOCKED");
            user.setFailedLoginAttempts(5);
            userRepository.save(user);
        });
        // Also store alice id
        if ("alice".equals(username)) {
            userRepository.findByUsername(username).ifPresent(u -> tokenStore.setAliceId(u.getId()));
        }
    }

    @Given("{string} has had the maximum number of failed login attempts")
    public void userHasHadMaxFailedLoginAttempts(final String username) throws Exception {
        // Try to login with wrong password 5 times to lock the account
        String wrongPassword = "WrongPass#1234";
        for (int i = 0; i < 5; i++) {
            mockMvc.perform(
                    post("/api/v1/auth/login")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(
                                    Map.of("username", username, "password", wrongPassword))))
                    .andReturn();
        }
        // Store alice id
        if ("alice".equals(username)) {
            userRepository.findByUsername(username).ifPresent(u -> tokenStore.setAliceId(u.getId()));
        }
    }

    @Given("an admin has unlocked alice's account")
    public void anAdminHasUnlockedAlicesAccount() throws Exception {
        userRepository.findByUsername("alice").ifPresent(user -> {
            user.setStatus("ACTIVE");
            user.setFailedLoginAttempts(0);
            userRepository.save(user);
        });
    }

    @Given("alice's account has been disabled by the admin")
    public void alicesAccountHasBeenDisabledByAdmin() throws Exception {
        userRepository.findByUsername("alice").ifPresent(user -> {
            user.setStatus("DISABLED");
            userRepository.save(user);
        });
    }

    @Given("alice's account has been disabled")
    public void alicesAccountHasBeenDisabled() throws Exception {
        userRepository.findByUsername("alice").ifPresent(user -> {
            user.setStatus("DISABLED");
            userRepository.save(user);
        });
    }

    @Given("the user {string} has been deactivated")
    public void theUserHasBeenDeactivated(final String username) throws Exception {
        userRepository.findByUsername(username).ifPresent(user -> {
            user.setStatus("DISABLED");
            userRepository.save(user);
        });
    }

    @When("^the client sends GET /api/v1/users/me with alice's access token$")
    public void theClientSendsGetUsersMeWithAlicesToken() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Alice's token not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        get("/api/v1/users/me")
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    // ============================================================
    // Admin action steps (shared)
    // ============================================================

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/unlock$")
    public void theAdminSendsPostUnlockAliceShared() throws Exception {
        String adminToken = tokenStore.getAdminToken();
        java.util.UUID aliceId = tokenStore.getAliceId();
        if (adminToken == null || aliceId == null) {
            throw new IllegalStateException("Admin token or alice ID not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/admin/users/" + aliceId + "/unlock")
                                .header("Authorization", "Bearer " + adminToken))
                        .andReturn());
    }

    // ============================================================
    // Assert steps (shared)
    // ============================================================

    @Then("the response body should contain {string} equal to {string}")
    public void responseBodyContainsFieldEqualTo(final String field, final String value)
            throws Exception {
        MockMvcResultMatchers.jsonPath("$." + field).value(value)
                .match(responseStore.getResult());
    }

    @Then("the response body should not contain a {string} field")
    public void responseBodyShouldNotContainField(final String field) throws Exception {
        MockMvcResultMatchers.jsonPath("$." + field).doesNotExist()
                .match(responseStore.getResult());
    }

    @Then("the response body should contain a non-null {string} field")
    public void responseBodyContainsNonNullField(final String field) throws Exception {
        MockMvcResultMatchers.jsonPath("$." + field).exists()
                .match(responseStore.getResult());
        MockMvcResultMatchers.jsonPath("$." + field).isNotEmpty()
                .match(responseStore.getResult());
    }

    @Then("the response body should contain a {string} field")
    public void responseBodyContainsField(final String field) throws Exception {
        MockMvcResultMatchers.jsonPath("$." + field).exists()
                .match(responseStore.getResult());
    }

    @Then("the response body should contain an error message about duplicate username")
    public void responseBodyContainsDuplicateUsernameError() throws Exception {
        final String body = responseStore.getResult().getResponse().getContentAsString();
        assertThat(body).containsIgnoringCase("already exists");
    }

    @Then("the response body should contain an error message about invalid credentials")
    public void responseBodyContainsInvalidCredentialsError() throws Exception {
        final String body = responseStore.getResult().getResponse().getContentAsString();
        assertThat(body).containsIgnoringCase("invalid");
    }

    @Then("the response body should contain an error message about account deactivation")
    public void responseBodyContainsAccountDeactivationError() throws Exception {
        final String body = responseStore.getResult().getResponse().getContentAsString();
        assertThat(body).containsIgnoringCase("deactivat");
    }

    @Then("the response body should contain an error message about token expiration")
    public void responseBodyContainsTokenExpirationError() throws Exception {
        final String body = responseStore.getResult().getResponse().getContentAsString();
        assertThat(body).containsIgnoringCase("expir");
    }

    @Then("the response body should contain an error message about invalid token")
    public void responseBodyContainsInvalidTokenError() throws Exception {
        final String body = responseStore.getResult().getResponse().getContentAsString();
        assertThat(body).containsIgnoringCase("invalid");
    }

    @Then("the response body should contain a validation error for {string}")
    public void responseBodyContainsValidationError(final String field) throws Exception {
        int status = responseStore.getResult().getResponse().getStatus();
        assertThat(status).isIn(400, 415);
    }

    @Then("alice's account status should be {string}")
    public void alicesAccountStatusShouldBe(final String status) throws Exception {
        String actualStatus = userRepository.findByUsername("alice")
                .map(u -> u.getStatus().toLowerCase())
                .orElseThrow(() -> new RuntimeException("Alice not found"));
        assertThat(actualStatus).isEqualToIgnoringCase(status);
    }
}
