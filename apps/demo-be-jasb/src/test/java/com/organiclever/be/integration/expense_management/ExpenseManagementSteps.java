package com.organiclever.be.integration.expense_management;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jayway.jsonpath.JsonPath;
import com.organiclever.be.integration.ResponseStore;
import com.organiclever.be.integration.steps.TokenStore;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.When;
import java.util.Map;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;

@Scope("cucumber-glue")
public class ExpenseManagementSteps {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @When("^alice sends POST /api/v1/expenses with body \\{ \"amount\": \"10\\.50\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Lunch\", \"date\": \"2025-01-15\", \"type\": \"expense\" \\}$")
    public void aliceCreatesExpenseEntry() throws Exception {
        String body = "{\"amount\":\"10.50\",\"currency\":\"USD\",\"category\":\"food\",\"description\":\"Lunch\",\"date\":\"2025-01-15\",\"type\":\"expense\"}";
        performCreateExpense(body);
    }

    @When("^alice sends POST /api/v1/expenses with body \\{ \"amount\": \"3000\\.00\", \"currency\": \"USD\", \"category\": \"salary\", \"description\": \"Monthly salary\", \"date\": \"2025-01-31\", \"type\": \"income\" \\}$")
    public void aliceCreatesIncomeEntry() throws Exception {
        String body = "{\"amount\":\"3000.00\",\"currency\":\"USD\",\"category\":\"salary\",\"description\":\"Monthly salary\",\"date\":\"2025-01-31\",\"type\":\"income\"}";
        performCreateExpense(body);
    }

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

    @Given("alice has created 3 entries")
    public void aliceHasCreated3Entries() throws Exception {
        String base = "{\"amount\":\"5.00\",\"currency\":\"USD\",\"category\":\"misc\",\"description\":\"Entry\",\"date\":\"2025-01-01\",\"type\":\"expense\"}";
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        for (int i = 0; i < 3; i++) {
            mockMvc.perform(
                    post("/api/v1/expenses")
                            .header("Authorization", "Bearer " + token)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(base))
                    .andReturn();
        }
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

    @When("^alice sends GET /api/v1/expenses$")
    public void aliceSendsGetExpenses() throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        get("/api/v1/expenses")
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @When("^alice sends PUT /api/v1/expenses/\\{expenseId\\} with body \\{ \"amount\": \"12\\.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Updated breakfast\", \"date\": \"2025-01-10\", \"type\": \"expense\" \\}$")
    public void aliceUpdatesExpense() throws Exception {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        String body = "{\"amount\":\"12.00\",\"currency\":\"USD\",\"category\":\"food\",\"description\":\"Updated breakfast\",\"date\":\"2025-01-10\",\"type\":\"expense\"}";
        responseStore.setResult(
                mockMvc.perform(
                        put("/api/v1/expenses/" + expenseId)
                                .header("Authorization", "Bearer " + token)
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{expenseId\\}$")
    public void aliceDeletesExpense() throws Exception {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        delete("/api/v1/expenses/" + expenseId)
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @When("^the client sends POST /api/v1/expenses with body \\{ \"amount\": \"10\\.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Coffee\", \"date\": \"2025-01-01\", \"type\": \"expense\" \\}$")
    public void unauthenticatedClientCreatesExpense() throws Exception {
        String body = "{\"amount\":\"10.00\",\"currency\":\"USD\",\"category\":\"food\",\"description\":\"Coffee\",\"date\":\"2025-01-01\",\"type\":\"expense\"}";
        responseStore.setResult(
                mockMvc.perform(
                        post("/api/v1/expenses")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(body))
                        .andReturn());
    }

    private void performCreateExpense(final String body) throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        MvcResult result = mockMvc.perform(
                post("/api/v1/expenses")
                        .header("Authorization", "Bearer " + token)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andReturn();
        responseStore.setResult(result);
        if (result.getResponse().getStatus() == 201) {
            String id = JsonPath.read(result.getResponse().getContentAsString(), "$.id");
            tokenStore.setExpenseId(UUID.fromString(id));
        }
    }
}
