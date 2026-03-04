package com.organiclever.be.integration.steps;

import com.organiclever.be.integration.ResponseStore;
import io.cucumber.java.Before;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

public class CommonSteps {

    @Autowired
    private ResponseStore responseStore;

    @Before
    public void resetState() {
        responseStore.clear();
    }

    @Given("the OrganicLever API is running")
    public void theOrganicLeverApiIsRunning() {
        // No-op: MockMvc context is always ready when scenarios execute.
    }

    @Then("the response status code should be {int}")
    public void theResponseStatusCodeShouldBe(final int expectedStatusCode) throws Exception {
        MockMvcResultMatchers.status()
            .is(expectedStatusCode)
            .match(responseStore.getResult());
    }
}
