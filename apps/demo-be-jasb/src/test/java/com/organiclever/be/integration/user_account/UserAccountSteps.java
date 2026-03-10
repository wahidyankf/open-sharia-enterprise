package com.organiclever.be.integration.user_account;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.organiclever.be.integration.ResponseStore;
import com.organiclever.be.integration.steps.TokenStore;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.When;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;

@Scope("cucumber-glue")
public class UserAccountSteps {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @When("^alice sends GET /api/v1/users/me$")
    public void aliceSendsGetUsersMe() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        get("/api/v1/users/me")
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @When("^alice sends PATCH /api/v1/users/me with body \\{ \"display_name\": \"Alice Smith\" \\}$")
    public void aliceSendsPatchUsersMeWithDisplayName() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        String body = objectMapper.writeValueAsString(Map.of("display_name", "Alice Smith"));
        responseStore.setResult(
                mockMvc.perform(
                        patch("/api/v1/users/me")
                                .header("Authorization", "Bearer " + token)
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
    }

    @When("^alice sends POST /api/v1/users/me/password with body \\{ \"old_password\": \"Str0ng#Pass1\", \"new_password\": \"NewPass#456\" \\}$")
    public void aliceSendsPostChangePasswordSuccess() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        String body = objectMapper.writeValueAsString(
                Map.of("old_password", "Str0ng#Pass1", "new_password", "NewPass#456"));
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/users/me/password")
                                .header("Authorization", "Bearer " + token)
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
    }

    @When("^alice sends POST /api/v1/users/me/password with body \\{ \"old_password\": \"Wr0ngOld!\", \"new_password\": \"NewPass#456\" \\}$")
    public void aliceSendsPostChangePasswordWrongOld() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        String body = objectMapper.writeValueAsString(
                Map.of("old_password", "Wr0ngOld!", "new_password", "NewPass#456"));
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/users/me/password")
                                .header("Authorization", "Bearer " + token)
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
    }

    @When("^alice sends POST /api/v1/users/me/deactivate$")
    public void aliceSendsPostSelfDeactivate() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/users/me/deactivate")
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @Given("^alice has deactivated her own account via POST /api/v1/users/me/deactivate$")
    public void aliceHasDeactivatedHerOwnAccount() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        mockMvc.perform(
                post("/api/v1/users/me/deactivate")
                        .header("Authorization", "Bearer " + token))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andReturn();
    }
}
