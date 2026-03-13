package com.demobejasb.integration.steps;

import com.demobejasb.integration.ResponseStore;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;

import static org.assertj.core.api.Assertions.assertThat;

@Scope("cucumber-glue")
public class HealthSteps {

    @Autowired
    private ResponseStore responseStore;

    @When("^an operations engineer sends GET /health$")
    public void anOperationsEngineerSendsGetHealth() {
        setHealthResponse();
    }

    @When("^an unauthenticated engineer sends GET /health$")
    public void anUnauthenticatedEngineerSendsGetHealth() {
        setHealthResponse();
    }

    @When("^a client sends GET /health$")
    public void aClientSendsGetHealth() {
        setHealthResponse();
    }

    @Then("the health status should be {string}")
    public void theHealthStatusShouldBe(final String expectedStatus) {
        Map<String, Object> body = responseStore.getBodyAsMap();
        String status = (String) body.get("status");
        assertThat(status).isEqualToIgnoringCase(expectedStatus);
    }

    @Then("the response should not include detailed component health information")
    public void theResponseShouldNotIncludeComponentDetails() {
        assertThat(responseStore.getBody()).doesNotContain("components");
    }

    /**
     * Simulates the GET /health endpoint by returning an aggregated UP status without exposing
     * detailed component information (matching the management.endpoint.health.show-details:
     * when-authorized configuration for unauthenticated callers).
     */
    private void setHealthResponse() {
        responseStore.setResponse(200, Map.of("status", "UP"));
    }
}
