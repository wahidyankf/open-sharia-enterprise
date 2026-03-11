package com.organiclever.demojavx.integration.steps;

import com.organiclever.demojavx.support.AppFactory;
import com.organiclever.demojavx.support.ScenarioState;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.When;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Assertions;

public class ExpenseSteps {

    private final ScenarioState state;

    public ExpenseSteps(ScenarioState state) {
        this.state = state;
    }

    @Given("^alice has created an entry with body \\{ \"amount\": \"([^\"]*)\", \"currency\": \"([^\"]*)\", \"category\": \"([^\"]*)\", \"description\": \"([^\"]*)\", \"date\": \"([^\"]*)\", \"type\": \"([^\"]*)\" \\}$")
    public void aliceHasCreatedEntry(String amount, String currency, String category,
            String description, String date, String type) throws Exception {
        String token = state.getAccessToken();
        JsonObject body = new JsonObject()
                .put("amount", amount)
                .put("currency", currency)
                .put("category", category)
                .put("description", description)
                .put("date", date)
                .put("type", type);
        HttpResponse<Buffer> resp = AppFactory.getClient()
                .post("/api/v1/expenses")
                .bearerTokenAuthentication(token)
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        Assertions.assertEquals(201, resp.statusCode(),
                "Expected 201 creating entry but got " + resp.statusCode() + ": "
                        + resp.bodyAsString());
        state.setExpenseId(resp.bodyAsJsonObject().getString("id"));
    }

    @Given("^alice has created an expense with body \\{ \"amount\": \"([^\"]*)\", \"currency\": \"([^\"]*)\", \"category\": \"([^\"]*)\", \"description\": \"([^\"]*)\", \"date\": \"([^\"]*)\", \"type\": \"([^\"]*)\" \\}$")
    public void aliceHasCreatedExpense(String amount, String currency, String category,
            String description, String date, String type) throws Exception {
        aliceHasCreatedEntry(amount, currency, category, description, date, type);
    }

    @Given("^alice has created an expense with body \\{ \"amount\": \"([^\"]*)\", \"currency\": \"([^\"]*)\", \"category\": \"([^\"]*)\", \"description\": \"([^\"]*)\", \"date\": \"([^\"]*)\", \"type\": \"([^\"]*)\", \"quantity\": ([0-9.]+), \"unit\": \"([^\"]*)\" \\}$")
    public void aliceHasCreatedExpenseWithUnit(String amount, String currency, String category,
            String description, String date, String type, String quantityStr,
            String unit) throws Exception {
        String token = state.getAccessToken();
        double quantityVal = Double.parseDouble(quantityStr);
        JsonObject body = new JsonObject()
                .put("amount", amount)
                .put("currency", currency)
                .put("category", category)
                .put("description", description)
                .put("date", date)
                .put("type", type)
                .put("quantity", quantityVal)
                .put("unit", unit);
        HttpResponse<Buffer> resp = AppFactory.getClient()
                .post("/api/v1/expenses")
                .bearerTokenAuthentication(token)
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        Assertions.assertEquals(201, resp.statusCode(),
                "Expected 201 creating entry but got " + resp.statusCode() + ": "
                        + resp.bodyAsString());
        state.setExpenseId(resp.bodyAsJsonObject().getString("id"));
    }

    @Given("alice has created {int} entries")
    public void aliceHasCreatedEntries(int count) throws Exception {
        for (int i = 0; i < count; i++) {
            aliceHasCreatedEntry("10.00", "USD", "food", "Entry " + i, "2025-01-0" + (i + 1),
                    "expense");
        }
    }

    @When("^alice sends GET /api/v1/expenses/\\{expenseId\\}$")
    public void aliceSendsGetExpense() throws Exception {
        String id = state.getExpenseId();
        Assertions.assertNotNull(id, "Expense ID must be set");
        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/api/v1/expenses/" + id)
                .bearerTokenAuthentication(state.getAccessToken())
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends GET /api/v1/expenses$")
    public void aliceSendsGetExpenses() throws Exception {
        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/api/v1/expenses")
                .bearerTokenAuthentication(state.getAccessToken())
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends GET /api/v1/expenses/summary$")
    public void aliceSendsGetSummary() throws Exception {
        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/api/v1/expenses/summary")
                .bearerTokenAuthentication(state.getAccessToken())
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends POST /api/v1/expenses with body \\{ \"amount\": \"([^\"]*)\", \"currency\": \"([^\"]*)\", \"category\": \"([^\"]*)\", \"description\": \"([^\"]*)\", \"date\": \"([^\"]*)\", \"type\": \"([^\"]*)\" \\}$")
    public void aliceSendsCreateExpense(String amount, String currency, String category,
            String description, String date, String type) throws Exception {
        JsonObject body = new JsonObject()
                .put("amount", amount)
                .put("currency", currency)
                .put("category", category)
                .put("description", description)
                .put("date", date)
                .put("type", type);
        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/expenses")
                .bearerTokenAuthentication(state.getAccessToken())
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
        if (response.statusCode() == 201) {
            state.setExpenseId(response.bodyAsJsonObject().getString("id"));
        }
    }

    @When("^alice sends POST /api/v1/expenses with body \\{ \"amount\": \"([^\"]*)\", \"currency\": \"([^\"]*)\", \"category\": \"([^\"]*)\", \"description\": \"([^\"]*)\", \"date\": \"([^\"]*)\", \"type\": \"([^\"]*)\", \"quantity\": ([0-9.]+), \"unit\": \"([^\"]*)\" \\}$")
    public void aliceSendsCreateExpenseWithUnit(String amount, String currency, String category,
            String description, String date, String type, String quantityStr,
            String unit) throws Exception {
        double quantityVal = Double.parseDouble(quantityStr);
        JsonObject body = new JsonObject()
                .put("amount", amount)
                .put("currency", currency)
                .put("category", category)
                .put("description", description)
                .put("date", date)
                .put("type", type)
                .put("quantity", quantityVal)
                .put("unit", unit);
        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/expenses")
                .bearerTokenAuthentication(state.getAccessToken())
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
        if (response.statusCode() == 201) {
            state.setExpenseId(response.bodyAsJsonObject().getString("id"));
        }
    }

    @When("^alice sends PUT /api/v1/expenses/\\{expenseId\\} with body \\{ \"amount\": \"([^\"]*)\", \"currency\": \"([^\"]*)\", \"category\": \"([^\"]*)\", \"description\": \"([^\"]*)\", \"date\": \"([^\"]*)\", \"type\": \"([^\"]*)\" \\}$")
    public void aliceSendsUpdateExpense(String amount, String currency, String category,
            String description, String date, String type) throws Exception {
        String id = state.getExpenseId();
        Assertions.assertNotNull(id, "Expense ID must be set");
        JsonObject body = new JsonObject()
                .put("amount", amount)
                .put("currency", currency)
                .put("category", category)
                .put("description", description)
                .put("date", date)
                .put("type", type);
        HttpResponse<Buffer> response = AppFactory.getClient()
                .put("/api/v1/expenses/" + id)
                .bearerTokenAuthentication(state.getAccessToken())
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{expenseId\\}$")
    public void aliceSendsDeleteExpense() throws Exception {
        String id = state.getExpenseId();
        Assertions.assertNotNull(id, "Expense ID must be set");
        HttpResponse<Buffer> response = AppFactory.getClient()
                .delete("/api/v1/expenses/" + id)
                .bearerTokenAuthentication(state.getAccessToken())
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^the client sends POST /api/v1/expenses with body \\{ \"amount\": \"([^\"]*)\", \"currency\": \"([^\"]*)\", \"category\": \"([^\"]*)\", \"description\": \"([^\"]*)\", \"date\": \"([^\"]*)\", \"type\": \"([^\"]*)\" \\}$")
    public void unauthClientSendsCreateExpense(String amount, String currency, String category,
            String description, String date, String type) throws Exception {
        JsonObject body = new JsonObject()
                .put("amount", amount)
                .put("currency", currency)
                .put("category", category)
                .put("description", description)
                .put("date", date)
                .put("type", type);
        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/expenses")
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }
}
