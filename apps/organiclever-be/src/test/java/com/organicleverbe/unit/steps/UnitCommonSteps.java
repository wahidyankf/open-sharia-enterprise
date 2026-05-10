package com.organicleverbe.unit.steps;

import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import org.springframework.beans.factory.annotation.Autowired;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class UnitCommonSteps {
    @Autowired
    private UnitStateStore stateStore;

    @Given("the API is running")
    public void theApiIsRunning() {
        // no-op: Spring context running means API is up
    }

    @Then("the response status code should be {int}")
    public void theResponseStatusCodeShouldBe(int expectedCode) {
        assertEquals(expectedCode, stateStore.getStatusCode());
    }
}
