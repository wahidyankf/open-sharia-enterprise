package com.organiclever.demojavx.unit.steps;

import com.organiclever.demojavx.support.AppFactory;
import com.organiclever.demojavx.support.ScenarioState;
import io.cucumber.java.en.When;
import io.cucumber.java.en.Then;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Assertions;

public class UnitReportingSteps {

    private final ScenarioState state;

    public UnitReportingSteps(ScenarioState state) {
        this.state = state;
    }

    @When("^alice sends GET /api/v1/reports/pl\\?from=([^&]+)&to=([^&]+)&currency=([^&]+)$")
    public void aliceSendsGetPlReport(String from, String to, String currency) throws Exception {
        String token = state.getAccessToken();
        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/api/v1/reports/pl?from=" + from + "&to=" + to + "&currency=" + currency)
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @Then("the income breakdown should contain {string} with amount {string}")
    public void incomeBreakdownContains(String category, String amount) {
        checkBreakdown("income_breakdown", category, amount);
    }

    @Then("the expense breakdown should contain {string} with amount {string}")
    public void expenseBreakdownContains(String category, String amount) {
        checkBreakdown("expense_breakdown", category, amount);
    }

    private void checkBreakdown(String field, String category, String amount) {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response);
        JsonObject body = response.bodyAsJsonObject();
        Assertions.assertNotNull(body);
        JsonArray breakdown = body.getJsonArray(field);
        Assertions.assertNotNull(breakdown, "Expected '" + field + "' in response");
        boolean found = false;
        for (int i = 0; i < breakdown.size(); i++) {
            JsonObject entry = breakdown.getJsonObject(i);
            if (category.equals(entry.getString("category"))) {
                Assertions.assertEquals(amount, entry.getString("amount"),
                        "Expected amount " + amount + " for category " + category
                                + " but got " + entry.getString("amount"));
                found = true;
                break;
            }
        }
        Assertions.assertTrue(found,
                "Category '" + category + "' not found in '" + field + "'");
    }
}
