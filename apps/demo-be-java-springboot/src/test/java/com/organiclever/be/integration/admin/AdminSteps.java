package com.organiclever.be.integration.admin;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.organiclever.be.auth.repository.UserRepository;
import com.organiclever.be.integration.ResponseStore;
import com.organiclever.be.integration.steps.TokenStore;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import java.util.Map;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;

@Scope("cucumber-glue")
public class AdminSteps {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private UserRepository userRepository;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Given("users {string}, {string}, and {string} are registered")
    public void usersAreRegistered(final String user1, final String user2, final String user3)
            throws Exception {
        registerUser(user1);
        registerUser(user2);
        registerUser(user3);
        // Store alice's ID
        userRepository.findByUsername("alice").ifPresent(u -> tokenStore.setAliceId(u.getId()));
    }

    private void registerUser(final String username) throws Exception {
        if (userRepository.findByUsername(username).isEmpty()) {
            String password = "alice".equals(username) ? "Str0ng#Pass1" : "Str0ng#Pass1234";
            mockMvc.perform(
                    post("/api/v1/auth/register")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(
                                    Map.of(
                                            "username", username,
                                            "email", username + "@example.com",
                                            "password", password))))
                    .andReturn();
        }
        if ("alice".equals(username)) {
            userRepository.findByUsername(username)
                    .ifPresent(u -> tokenStore.setAliceId(u.getId()));
        }
    }

    @When("^the admin sends GET /api/v1/admin/users$")
    public void theAdminSendsGetAdminUsers() throws Exception {
        String adminToken = tokenStore.getAdminToken();
        if (adminToken == null) {
            throw new IllegalStateException("Admin token not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        get("/api/v1/admin/users")
                                .header("Authorization", "Bearer " + adminToken))
                        .andReturn());
    }

    @When("^the admin sends GET /api/v1/admin/users\\?email=alice@example\\.com$")
    public void theAdminSendsGetAdminUsersSearchByEmail() throws Exception {
        String adminToken = tokenStore.getAdminToken();
        if (adminToken == null) {
            throw new IllegalStateException("Admin token not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        get("/api/v1/admin/users")
                                .param("email", "alice@example.com")
                                .header("Authorization", "Bearer " + adminToken))
                        .andReturn());
    }

    @Then("the response body should contain at least one user with {string} equal to {string}")
    public void theResponseBodyShouldContainUserWithFieldEqual(
            final String field, final String value) throws Exception {
        // Check that at least one element in $.data has the field equal to value
        MockMvcResultMatchers.jsonPath("$.data").isArray().match(responseStore.getResult());
        MockMvcResultMatchers.jsonPath("$.data[0]." + field).value(value)
                .match(responseStore.getResult());
    }

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/disable with body \\{ \"reason\": \"Policy violation\" \\}$")
    public void theAdminDisablesAlice() throws Exception {
        String adminToken = tokenStore.getAdminToken();
        UUID aliceId = tokenStore.getAliceId();
        if (adminToken == null || aliceId == null) {
            throw new IllegalStateException("Admin token or alice ID not stored");
        }
        String body = objectMapper.writeValueAsString(Map.of("reason", "Policy violation"));
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/admin/users/" + aliceId + "/disable")
                                .header("Authorization", "Bearer " + adminToken)
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
    }

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/enable$")
    public void theAdminEnablesAlice() throws Exception {
        String adminToken = tokenStore.getAdminToken();
        UUID aliceId = tokenStore.getAliceId();
        if (adminToken == null || aliceId == null) {
            throw new IllegalStateException("Admin token or alice ID not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/admin/users/" + aliceId + "/enable")
                                .header("Authorization", "Bearer " + adminToken))
                        .andReturn());
    }

    // "the admin sends POST /api/v1/admin/users/{alice_id}/unlock" is in AuthSteps (shared)

@When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/force-password-reset$")
    public void theAdminForcesPasswordReset() throws Exception {
        String adminToken = tokenStore.getAdminToken();
        UUID aliceId = tokenStore.getAliceId();
        if (adminToken == null || aliceId == null) {
            throw new IllegalStateException("Admin token or alice ID not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/admin/users/" + aliceId + "/force-password-reset")
                                .header("Authorization", "Bearer " + adminToken))
                        .andReturn());
    }
}
