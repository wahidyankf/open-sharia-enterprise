package com.organiclever.demojavx.integration.steps;

import com.organiclever.demojavx.support.AppFactory;
import com.organiclever.demojavx.support.ScenarioState;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import io.vertx.core.buffer.Buffer;
import io.vertx.ext.web.client.HttpResponse;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Assertions;

public class HealthSteps {

    private final ScenarioState state;

    public HealthSteps(ScenarioState state) {
        this.state = state;
    }

    @When("^an operations engineer sends GET /health$")
    public void operationsEngineerSendsGetHealth() throws Exception {
        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/health")
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^an unauthenticated engineer sends GET /health$")
    public void unauthenticatedEngineerSendsGetHealth() throws Exception {
        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/health")
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @Then("the health status should be {string}")
    public void healthStatusShouldBe(String expected) {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response);
        String status = response.bodyAsJsonObject().getString("status");
        Assertions.assertEquals(expected, status);
    }

    @Then("the response should not include detailed component health information")
    public void responseDoesNotIncludeDetailedComponentHealth() {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response);
        io.vertx.core.json.JsonObject body = response.bodyAsJsonObject();
        // Only "status" field should be present — no "components", "db", etc.
        Assertions.assertNull(body.getValue("components"),
                "Response should not include component details");
    }
}
