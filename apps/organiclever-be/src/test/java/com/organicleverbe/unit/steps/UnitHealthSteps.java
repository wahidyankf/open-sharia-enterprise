package com.organicleverbe.unit.steps;

import com.organicleverbe.health.controller.HealthController;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class UnitHealthSteps {
    @Autowired
    private UnitStateStore stateStore;

    @Autowired
    private HealthController healthController;

    @When("^an operations engineer sends GET /health$")
    public void anOperationsEngineerSendsGetHealth() {
        Map<String, String> response = healthController.health();
        stateStore.setStatusCode(200);
        stateStore.setResponseBody(response);
    }

    @When("^an unauthenticated engineer sends GET /health$")
    public void anUnauthenticatedEngineerSendsGetHealth() {
        Map<String, String> response = healthController.health();
        stateStore.setStatusCode(200);
        stateStore.setResponseBody(response);
    }

    @Then("the health status should be {string}")
    public void theHealthStatusShouldBe(String expectedStatus) {
        @SuppressWarnings("unchecked")
        Map<String, String> body = (Map<String, String>) stateStore.getResponseBody();
        assertEquals(expectedStatus, body.get("status"));
    }

    @Then("the response should not include detailed component health information")
    public void theResponseShouldNotIncludeDetailedComponentHealthInformation() {
        assertFalse(stateStore.getResponseBody().toString().contains("components"));
    }
}
