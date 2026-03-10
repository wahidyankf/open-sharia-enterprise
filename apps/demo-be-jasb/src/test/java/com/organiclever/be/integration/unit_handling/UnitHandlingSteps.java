package com.organiclever.be.integration.unit_handling;

import com.jayway.jsonpath.JsonPath;
import com.organiclever.be.integration.ResponseStore;
import com.organiclever.be.integration.steps.TokenStore;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
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
public class UnitHandlingSteps {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Given("^alice has created an expense with body (.*)$")
    public void aliceHasCreatedAnExpenseWithBody(final String body) throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        MvcResult result = mockMvc.perform(
                post("/api/v1/expenses")
                        .header("Authorization", "Bearer " + token)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(MockMvcResultMatchers.status().isCreated())
                .andReturn();
        String id = JsonPath.read(result.getResponse().getContentAsString(), "$.id");
        tokenStore.setExpenseId(UUID.fromString(id));
    }

    @When("^alice sends GET /api/v1/expenses/\\{expenseId\\}$")
    public void aliceSendsGetExpenseById() throws Exception {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        get("/api/v1/expenses/" + expenseId)
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @Then("the response body should contain \"quantity\" equal to {double}")
    public void theResponseBodyShouldContainQuantityEqual(final double quantity) throws Exception {
        MockMvcResultMatchers.jsonPath("$.quantity").value(quantity)
                .match(responseStore.getResult());
    }

    // "the response body should contain "unit" equal to {string}" is handled by AuthSteps

@When("^alice sends POST /api/v1/expenses with body (.*)$")
    public void aliceSendsPostExpensesWithBody(final String body) throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/expenses")
                                .header("Authorization", "Bearer " + token)
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
    }
}
