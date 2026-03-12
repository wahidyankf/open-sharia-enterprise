package com.organiclever.be.integration.reporting;

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

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;

@Scope("cucumber-glue")
public class ReportingSteps {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Given("^alice has created an entry with body (.*)$")
    public void aliceHasCreatedAnEntryWithBody(final String body) throws Exception {
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

    @When("^alice sends GET /api/v1/reports/pl\\?from=2025-01-01&to=2025-01-31&currency=USD$")
    public void aliceSendsGetPLJan() throws Exception {
        performGetPl("2025-01-01", "2025-01-31", "USD");
    }

    @When("^alice sends GET /api/v1/reports/pl\\?from=2025-02-01&to=2025-02-28&currency=USD$")
    public void aliceSendsGetPLFeb() throws Exception {
        performGetPl("2025-02-01", "2025-02-28", "USD");
    }

    @When("^alice sends GET /api/v1/reports/pl\\?from=2025-03-01&to=2025-03-31&currency=USD$")
    public void aliceSendsGetPLMar() throws Exception {
        performGetPl("2025-03-01", "2025-03-31", "USD");
    }

    @When("^alice sends GET /api/v1/reports/pl\\?from=2025-04-01&to=2025-04-30&currency=USD$")
    public void aliceSendsGetPLApr() throws Exception {
        performGetPl("2025-04-01", "2025-04-30", "USD");
    }

    @When("^alice sends GET /api/v1/reports/pl\\?from=2025-05-01&to=2025-05-31&currency=USD$")
    public void aliceSendsGetPLMay() throws Exception {
        performGetPl("2025-05-01", "2025-05-31", "USD");
    }

    @When("^alice sends GET /api/v1/reports/pl\\?from=2099-01-01&to=2099-01-31&currency=USD$")
    public void aliceSendsGetPLFuture() throws Exception {
        performGetPl("2099-01-01", "2099-01-31", "USD");
    }

    // "the response body should contain {string} equal to {string}" is handled by AuthSteps

    @Then("the income breakdown should contain {string} with amount {string}")
    public void theIncomeBreakdownShouldContain(final String category, final String amount)
            throws Exception {
        MockMvcResultMatchers.jsonPath("$.income_breakdown." + category).value(amount)
                .match(responseStore.getResult());
    }

    @Then("the expense breakdown should contain {string} with amount {string}")
    public void theExpenseBreakdownShouldContain(final String category, final String amount)
            throws Exception {
        MockMvcResultMatchers.jsonPath("$.expense_breakdown." + category).value(amount)
                .match(responseStore.getResult());
    }

    private void performGetPl(final String from, final String to, final String currency)
            throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        get("/api/v1/reports/pl")
                                .param("from", from)
                                .param("to", to)
                                .param("currency", currency)
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }
}
