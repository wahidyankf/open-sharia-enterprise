package com.organiclever.be.integration.steps;

import com.organiclever.be.integration.ResponseStore;
import io.cucumber.java.After;
import io.cucumber.java.Before;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@Scope("cucumber-glue")
public class CommonSteps {

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private InMemoryDataStore dataStore;

    @Before
    public void beginScenario() {
        responseStore.clear();
        tokenStore.clear();
        dataStore.reset();
    }

    @After
    public void cleanupScenario() {
        dataStore.reset();
    }

    @Given("the OrganicLever API is running")
    public void theOrganicLeverApiIsRunning() {
        // No-op: MockMvc context is always ready when scenarios execute.
    }

    @Given("the API is running")
    public void theApiIsRunning() {
        // No-op: MockMvc context is always ready when scenarios execute.
    }

    @Then("the response status code should be {int}")
    public void theResponseStatusCodeShouldBe(final int expectedStatusCode) throws Exception {
        MockMvcResultMatchers.status()
            .is(expectedStatusCode)
            .match(responseStore.getResult());
    }
}
